module IRC.CTCP where

import Control.Monad.State
import Data.Maybe
import Data.Char

import Text.Parsec
import Text.Parsec.String

type LowMessage = String
type MiddleMessage = String
type HighMessage = String

-- quote :: QuoteChar -> Mapping -> String -> String
-- dequote :: QuoteChar -> Mapping -> String -> String

-- SO APPARENTLY NONE OF THIS STUFF IS ACTUALLY IMPLEMENTED IN IRSSI

-- *actual* ctcp shit is just \001ctcp\001 in a privmsg or notice

-- preliminary; contains much badness

(>:) :: a -> [[a]] -> [[a]]
l >: []       = [[l]]
l >: (ls:lss) = (l:ls):lss

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn q = foldr eat [] where
    eat x acc | x == q    = [] : acc
              | otherwise = x >: acc

-- breaks on a lot of input
unquote :: Eq a => a -> [a] -> [a]
unquote c str = str where
    [[],str,[]] = splitOn c str

-- data CTCP = CTCP String String
-- unctcp :: String -> CTCP
-- unctcp = uncurry CTCP . break (== ' ') . extract '\001'

