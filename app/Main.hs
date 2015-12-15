--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import Data.Aeson
import           Network.Socket      (withSocketsDo)
import qualified Data.ByteString.Lazy as LBS
import           Data.Text           (Text)
import qualified Data.Text           as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO        as T
import qualified Data.Set as S
import qualified Network.WebSockets  as WS


--------------------------------------------------------------------------------
app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        let msgLBS = LBS.fromStrict (encodeUtf8 msg)
        let eWorld = eitherDecode msgLBS :: Either String World
        liftIO $ T.putStrLn msg

        case eWorld of
          Left err -> liftIO $ print err
          Right world -> do
            liftIO $ print world
            let mDir = getNextMove world
            case mDir of
              Just dir -> do
                sendCommand conn DropBomb
                sendCommand conn (dirToCommand dir)
              Nothing -> return ()

    sendCommand conn (SetName "Santa's Lazy Haskellers")

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            let command = parseCommand line
            sendCommand conn command
            loop
            -- unless (T.null line) $ WS.sendTextData conn line >> loop

    loop
    WS.sendClose conn ("Bye!" :: Text)


--------------------------------------------------------------------------------
main :: IO ()
main = withSocketsDo $ WS.runClient "10.112.155.244" 8080 "/" app


data Command
  = Look
  | DropBomb
  | SetName String
  | MoveNorth
  | MoveSouth
  | MoveEast
  | MoveWest
  deriving Show

instance ToJSON Command where
  toJSON com@(SetName name) = object ["command" .= ["SetName", name]]
  toJSON com = object ["command" .= show com]

parseCommand :: T.Text -> Command
parseCommand "h" = MoveWest
parseCommand "j" = MoveSouth
parseCommand "k" = MoveNorth
parseCommand "l" = MoveEast
parseCommand "b" = DropBomb
parseCommand _ = Look

sendCommand :: WS.Connection -> Command -> IO ()
sendCommand conn command = do
    let jsonCommand = encode command
    LBS.putStrLn jsonCommand
    WS.sendTextData conn jsonCommand


data World = World
  { walls :: [Wall]
  , players :: [Player]
  , bombs :: [Bomb]
  } deriving Show

instance FromJSON World where
  parseJSON (Object v) = World <$> v.: "walls" <*> v .: "players" <*> v .: "bombs"

{-
{"alive":true,"type":"Strong","position":{"x":0,"y":0}
-}
data Wall = Wall
  { wAlive :: Bool
  , wStrength :: WallStrength
  , wPosition :: Position
  } deriving Show

instance FromJSON Wall where
  parseJSON (Object v) = Wall <$> v .: "alive" <*> v.: "type" <*> v.: "position"

data WallStrength
  = Strong
  | Weak deriving Show

instance FromJSON WallStrength where
  parseJSON (String s) = return $ toWallStrength s
  parseJSON e = error $ "wall strength error:  " ++ show e

toWallStrength :: T.Text -> WallStrength
toWallStrength "Strong" = Strong
toWallStrength "Weak" = Weak

data Position = Position
  { posX :: Int
  , posY :: Int
  } deriving Show

instance FromJSON Position where
  parseJSON (Object v) = Position <$> v .: "x" <*> v.: "y"


{-
{"alive":true,"score":-2,"name":"Pythonistas","id":"36242761-d09a-4010-8570-907798ae8945","position":{"x":1,"y":9}}
-}
data Player = Player
  { pAlive :: Bool
  , pScore :: Int
  , pName :: String
  , pID :: String
  , pPosition :: Position
  } deriving Show

instance FromJSON Player where
  parseJSON (Object v) = Player <$> v .: "alive" <*> v .: "score" <*> v .: "name" <*> v .: "id" <*> v .: "position"


{-
{"blast":null,"position":{"x":1,"y":2}}
-}
data Bomb = Bomb
  { bBlast :: Maybe ()
  , bPosition :: Position
  } deriving Show

instance FromJSON Bomb where
  parseJSON (Object v) = Bomb <$> v .: "blast" <*> v .: "position"


getMe :: [Player] -> Maybe Player
getMe (p:ps) = if pName p == "Santa's Lazy Haskellers" then Just p else getMe ps
getMe _ = Nothing

dirToCommand :: Direction -> Command
dirToCommand North = MoveNorth
dirToCommand South = MoveSouth
dirToCommand East = MoveEast
dirToCommand West = MoveWest

data Direction
  = North
  | East
  | South
  | West
  deriving (Eq, Ord, Show)

getNextMove :: World -> Maybe Direction
getNextMove world =
  case getMe (players world) of
    Nothing -> Nothing
    Just player -> Just $ nextMove player (walls world)

nextMove :: Player -> [Wall] -> Direction
nextMove player walls = head (S.toList (openMoves player walls))

openMoves :: Player -> [Wall] -> S.Set Direction
openMoves player walls = S.fromList [North, East, South, West] `S.difference` closedDirs
  where closedDirs = closedMoves player walls

closedMoves :: Player -> [Wall] -> S.Set Direction
closedMoves _ [] = S.empty
closedMoves player walls = go S.empty player walls
  where
    go dirs _ [] = dirs
    go dirs p (w:ws) =
      let newDirs =
            case p `nextTo` w of
              Just dir -> S.insert dir dirs
              Nothing -> dirs
      in go newDirs p ws


nextTo :: Player -> Wall -> Maybe Direction
nextTo player wall =
  let wX = posX (wPosition wall)
      wY = posY (wPosition wall)
      pX = posX (pPosition player)
      pY = posY (pPosition player)
      dX = pX - wX
      dY = pY - wY
  in dir dX dY
  where dir  1  0 = Just East
        dir (-1)  0 = Just West
        dir  0  1 = Just North
        dir  0 (-1) = Just South
        dir  _  _ = Nothing
