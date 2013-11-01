module Anabel where

import Control.Monad.State hiding (join)
import Control.Applicative ((<$>))
import Debug.Trace

import IRC

{- temporary adhocary -}
data Memory = Mem [String] (Message -> String)

remember :: String -> Memory -> Memory
remember str (Mem strs x) = Mem (str:strs) x

forget :: Memory -> Memory
forget (Mem (s:ss) x) = Mem ss x

unmember :: Memory -> [String]
unmember (Mem strs _) = strs

holdbreath :: Memory -> Memory
holdbreath (Mem strs breath) = Mem strs $ const ""

unholdbreath :: Memory -> Memory
unholdbreath (Mem strs breath) = Mem strs breathe

animus :: Memory -> (Message -> String)
animus (Mem _ breath) = breath
{- end temphocary -}

exist :: String -> State Memory String
exist str = do
    mem <- get
    ((animus mem) msg ++) <$> respond msg where
        Just msg = message str

breathe :: Message -> String
breathe (Message pref cmd args) = case cmd of
    "PING"  -> transmit . pong . Just $ unwords args
    "001"   -> transmit $ join "#words"
    _       -> ""

respond :: Message -> State Memory String
respond (Message pref cmd args) = case cmd of
    "PRIVMSG" -> respondPrivmsg $ last args
    _         -> return ""

respondPrivmsg :: String -> State Memory String
respondPrivmsg msg = case (head $ words $ msg) of
    "remember" -> do
        modify $ remember (unwords . tail . words $ msg)
        return . transmit . privmsg "#words" $ "okay"
    "forget" -> do
        modify $ forget
        return . transmit . privmsg "#words" $ "okay"
    "tell"     -> get >>= return . transmit . privmsg "#words" . show . unmember
    "stop"     -> do
        modify holdbreath
        return . transmit . privmsg "#words" $ "okay"
    "start"    -> do
        modify unholdbreath
        return . transmit . privmsg "#words" $ "okay"
    "don't"    -> return . transmit . privmsg "#words" $ " "
    "quit"     -> return . transmit . quit $ Nothing
    -- "u"        -> return . transmit . quit $ Just "urafoggert"
    _          -> return ""
    
