module IRC.Commands where

import Prelude hiding (concat)
import Data.Foldable (toList, concat)
import Data.List (intercalate)

import IRC.Types

-- Message -> String functions

send :: Message -> String
send (Message pref cmd args) =
    sendPrefix pref ++ cmd ++ " " ++ sendArgs args ++ "\r\n"

sendPrefix :: Maybe Prefix -> String
sendPrefix = concat . fmap ((++ " ") . sendPrefix') where
    sendPrefix' (Server h)   = h
    sendPrefix' (User n u h) = n ++ maybe "" ('!':) u ++ maybe "" ('@':) h

sendArgs :: Args -> String
sendArgs = unwords . mapLast (':' :)

-- util function.  maybe put in its own file later?
mapLast :: (a -> a) -> [a] -> [a]
mapLast f = foldr (once f) [] where
    once f s [] = [f s]
    once _ s ls = s:ls

-- to allow more flexibility with the commands
with :: Message -> String -> Message
(Message pref cmd args) `with` arg = Message pref cmd (args ++ [arg])

-- functions of type [params] -> Message
-- these are all taken from sections 3 and 4 of rfc 2812

-- let's start with a general command function
command :: Command -> Args -> Message
command = Message Nothing

-- Password message (3.1.1)
pass :: String -> Message
pass p = command "PASS" [p]

-- Nick message (3.1.2)
nick :: Nickname -> Message
nick n = command "NICK" [n]

-- User message (3.1.3)
-- 8 is sensible default
user :: Username -> Realname -> Message
user u r = command "USER" [u, "8", "*", r]

-- Oper message (3.1.4)
oper :: Nickname -> Password -> Message
oper n p = command "OPER" [n,p]

-- User mode message (3.1.5)
userMode :: Nickname -> Mode -> Message
userMode n m = command "MODE" [n,m]

-- Channel mode message (3.2.3)
chanMode :: Channel -> Mode -> [String] -> Message
chanMode c m p = command "MODE" $ [c,m] ++ p

-- Quit message (3.1.7)
quit :: Maybe String -> Message
quit = command "QUIT" . toList

-- Join message (3.2.1)
-- three flavours of join
join :: [Channel] -> Message
join cs = command "JOIN" [intercalate "," cs]

joinK :: [(Channel, Key)] -> Message
joinK = command "JOIN" . map (intercalate ",") . square . unzip where
    square (x,y) = [x,y]

-- part all
join0 :: Message
join0 = command "JOIN" ["0"]

-- Part message (3.2.2)
part :: [Channel] -> Maybe String -> Message
part cs msg = command "PART" (intercalate "," cs : toList msg)

-- Topic message (3.2.4)
topic :: Channel -> Maybe String -> Message
topic c msg = command "TOPIC" (c : toList msg)

-- Kick command (3.2.8)
kick :: Channel -> [Nickname] -> Maybe String -> Message
kick c ns msg = command "KICK" (c : intercalate "," ns : toList msg)
-- alt
kickS :: [(Channel,Nickname)] -> Maybe String -> Message
kickS cns msg = command "KICK" (cs : ns : toList msg) where
    (cs,ns) = mapmap (intercalate ",") . unzip $ cns
    mapmap f (x,y) = (f x, f y)

-- Kill message (3.7.1)
kill :: Nickname -> String -> Message
kill n str = command "KILL" [n,str]

-- Ping message (3.7.2)
-- ping (pong) messages are fucking weird.  this is a simplified interface.
ping :: String -> Message
ping origin = command "PING" [origin]

-- Pong message (3.7.3)
pong :: String -> Message
pong origin = command "PONG" [origin]

-- Away (4.1)
away :: Maybe String -> Message
away = command "AWAY" . toList

-- Invite message (3.2.7)
invite :: Nickname -> Channel -> Message
invite n c = command "INVITE" [n,c]

-- Private messages (3.3.1)
privmsg :: String -> String -> Message
privmsg t msg = command "PRIVMSG" [t,msg]

-- Notice (3.3.2)
notice :: String -> String -> Message
notice t msg = command "NOTICE" [t,msg]

-- Names message (3.2.5)
names :: [Channel] -> Message
names cs = command "NAMES" [intercalate "," cs]

-- List message (3.2.6)
list :: [Channel] -> Message
list cs = command "NAMES" [intercalate "," cs]

-- Info command (3.4.10)
info :: Message
info = command "INFO" []

-- Whois query (3.6.2)
whois :: [Nickname] -> Message
whois ns = command "WHOIS" [intercalate "," ns]

-- Whowas (3.6.3)
whowas :: [Nickname] -> Int -> Message
whowas ns i = command "WHOWAS" [intercalate "," ns, show i]

-- Ison message (4.9)
ison :: [Nickname] -> Message
ison ns = command "ISON" ns

