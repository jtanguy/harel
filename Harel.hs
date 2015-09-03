module Main where

import Control.Monad (replicateM)
import qualified Database.Redis as Redis
import System.Random (randomRIO)

import Servant

alphabet = ['a'..'z']++['A'..'Z']++['0'..'9']
chooseFrom alph = (alph !!) <$> randomRIO (0,length alph -1)

genShort = replicateM 7 (chooseFrom alphabet)


main :: IO ()
main = do
  putStrLn "Hello world ! This is Harel"
  putStr "Here is a short id: "
  genShort >>= putStrLn
