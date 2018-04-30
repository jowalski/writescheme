module E1 where

import System.Environment

-- Original
firstArg :: [String] -> String
firstArg = (!! 0)

-- Exercise 1
allArgs :: [String] -> String
allArgs = (>>= (++ " "))

-- Exercise 2
arithOpArgs
  :: (Int -> Int -> Int) -> Int -> [String] -> String
arithOpArgs op x = show . (foldl op x) . (fmap (\c -> read c :: Int))

-- Exercise 3
getLineArgs :: IO [String]
getLineArgs = fmap (\x -> [x]) getLine

main :: IO ()
main =
  do args <- getLineArgs
     putStrLn ("Hello, " ++ allArgs args)