{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}

module IRC.Fing where

import Control.Applicative ((<$>), (<*>), (<*))
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.Char
import Data.Function
import Network
import Text.Parsec hiding (State)
import Text.Parsec.String
import System.IO

-- data structures

data Hand = Hand
    { _up    :: Int
    , _total :: Int
    } deriving (Eq, Show)

up :: Lens' Hand Int
up = lens _up (\h x -> h { _up = x `mod` _total h })

total :: Lens' Hand Int
total = lens _total (\h t -> h { _total = t , _up = _up h `mod` t } )

data Player = Player
    { _name  :: String
    , _left  :: Hand
    , _right :: Hand
    } deriving (Eq, Show)

makeLenses ''Player

-- functions

(+++) :: String -> String -> String -> String
(+++) a b c = a ++ b ++ c

showHand :: Hand -> String
showHand h = ((+++ ":") `on` show . (h ^.)) up total

showPlayer :: Player -> String
showPlayer p = p ^. name
            ++ " [ " ++ (showHand $ view left  p)
            ++ " | " ++ (showHand $ view right p)
            ++ " ]"

touch :: Hand -> Hand -> Hand
touch = over up . (+) . view up

finger :: Getting Hand Player Hand -> Setting' (->) Player Hand
       -> Player -> Player -> Player
finger hi hj = over hj . touch . view hi

dead :: Player -> Bool
dead p = (dead' $ view left p) && (dead' $ view right p) where
    dead' = (== 0) . view up

-- defaults

defHand = Hand 1 5
defPlayer = Player "" defHand defHand

-- main

data Game = Game
    { _p1 :: Player
    , _p2 :: Player
    -- , _cp :: Lens' Game Player
    }
makeLenses ''Game

showGame :: Game -> String
showGame g = foldr (+++ "\n") ""
           . map (showPlayer . (g ^.))
           $ [p2,p1]

defGame = Game defPlayer defPlayer

swapPs :: Game -> Game
swapPs g = g & p1 .~ p2' & p2 .~ p1' where
    (p1',p2') = (g ^. p1 , g ^. p2)

data Par = L | R deriving Show

readPar :: Parser Par
readPar = try (string "l" >> return L) <|> (string "r" >> return R)

readDir :: Parser Par
readDir = try (string "<-" >> return L) <|> (string "->" >> return R)

input :: Parser (Par,Par)
input = do
    a <- readPar <* spaces
    dir <- readDir <* spaces
    b <- readPar <* spaces
    return $ case dir of
        R -> (a,b)
        L -> (b,a)

parToLens :: Par -> Lens' Player Hand
parToLens L = left
parToLens R = right

-- parseInput :: String -> Maybe (Lens' Player Hand, Lens' Player Hand)
parseInput = fmap (bimap parToLens parToLens)
           . either (const Nothing) Just
           . parse input ""

-- for irc games

join :: String -> StateT Game IO Bool
join n = do
    n1 <- view (p1 . name) <$> get
    if null n1 then do
        p1 . name .= n
        modify swapPs
        return True
    else return False
        

play :: (String, String) -> StateT Game IO Bool
play (n, m) = do
    n1 <- view (p1 . name) <$> get
    if n == n1 then case parseInput m of
        Nothing         -> return False
        Just (from, to) -> do
            p1 <- view p1 <$> get
            p2 %= finger from to p1
            modify swapPs
            return True
    else return False

