-- at the moment, just a proof of concept

module IRC.Client where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forever, when)
import Data.ByteString.Char8 (pack, unpack)
import Data.Either.Utils (fromRight)
import Data.List.Split (splitOn)

import Pipes
import Pipes.Network.TCP (connect, fromSocket, toSocket)
import Pipes.Prelude as P (map)

import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.State.Class as S (get, put)
import Pipes.Lift (evalStateP)

import IRC.Types
import IRC.Parser
import IRC.Commands

import qualified IRC.Fing as F (join)
import IRC.Fing hiding (join)

data PrivMsg = PrivMsg Nickname Target String
data Ping = Ping String

ircmain :: Pipe Message Message IO () -> IO ()
ircmain talk = connect "irc.foonetic.net" "6667" $ \(sock,addr) ->
    runEffect  $  fromSocket sock 4096
              >-> P.map unpack
              >-> decrlfP
              >-> P.map (fromRight . parseMessage)
              >-> recvlog
              >-> talk
              >-> sendlog
              >-> P.map send
              >-> P.map pack
              >-> toSocket sock

decrlfP :: Monad m => Pipe String String m ()
decrlfP = evalStateP "" . forever $ do
    strs <- (splitOn "\r\n" .) . (++) <$> S.get <*> await
    mapM yield $ init strs
    S.put $ last strs

recvlog :: Pipe Message Message IO ()
recvlog = for cat $ (>>) <$> lift . putStrLn . show <*> yield
-- recvlog = for cat $ \str -> do
--     lift . putStrLn . show $ str
--     yield str

sendlog :: Pipe Message Message IO ()
sendlog = for cat $ (>>) <$> lift . putStrLn . (">>> " ++) . show <*> yield
-- sendlog = for cat $ \str -> do
--     lift . putStrLn . (">>> " ++) . show $ str
--     yield str

-- a simple function that takes a handle (i.e. connection to irc server) and
-- parses output, splitting across specialized functions
talk :: Pipe Message Message IO ()
talk = evalStateP defGame $ onConnect >> for cat delegate where
    delegate msg = do
        case msg of
            Message (Just (User nick _ _)) "PRIVMSG" [chan,txt] -> onPrivMsg (PrivMsg nick chan txt)
            Message src "PING" [origin] -> onPing (Ping origin)
            Message _ "001" _ -> onWelcome
            _ -> onMessage msg

onConnect :: Pipe Message Message (StateT Game IO) ()
onConnect = do
    yield $ user "testbot" "testbot"
    yield $ nick "testbot"

onPrivMsg :: PrivMsg -> Producer' Message (StateT Game IO) ()
onPrivMsg (PrivMsg nick me str) = do
    let trg = case head me of
            '#' -> me
            _   -> nick
    case head . words $ str of
        "echo" -> yield $ privmsg trg str
        "quit" -> yield . quit . Just . unwords . tail . words $ str
        "show" -> mapM_ (yield . privmsg trg) =<< lines . showGame <$> S.get
        "join" -> do
            yeh <- lift $ F.join nick
            when yeh $
                yield . privmsg trg $ nick ++ " has joined."
        _      -> do
            yeh <- lift $ play (nick, str)
            when yeh $
                mapM_ (yield . privmsg trg) =<< lines . showGame <$> S.get

onPing :: Monad m => Ping -> Producer' Message m ()
onPing (Ping origin) = yield . pong $ origin

onWelcome :: Monad m => Producer' Message m ()
onWelcome = yield . join $ ["#zook"]

onMessage :: Monad m => Message -> Producer' Message m ()
onMessage _ = return ()

