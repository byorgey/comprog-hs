-- https://byorgey.wordpress.com/2019/05/22/competitive-programming-in-haskell-scanner/

{-# LANGUAGE LambdaCase #-}

module Scanner where

import           Control.Monad.State

type Scanner = State [String]

runScanner :: Scanner a -> String -> a
runScanner = runScannerWith words

runScannerWith :: (String -> [String]) -> Scanner a -> String -> a
runScannerWith t s = evalState s . t

str :: Scanner String
str = get >>= \case { s:ss -> put ss >> return s }

int :: Scanner Int
int = read <$> str

integer :: Scanner Integer
integer = read <$> str

double :: Scanner Double
double = read <$> str

numberOf :: Scanner a -> Scanner [a]
numberOf s = int >>= flip replicateM s

many :: Scanner a -> Scanner [a]
many s = get >>= \case { [] -> return []; _ -> (:) <$> s <*> many s }

two, three, four :: Scanner a -> Scanner [a]
[two, three, four] = map replicateM [2..4]
