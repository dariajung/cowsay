module Cowsay where

import System.Directory
import Control.Monad
import Data.List

data Options = Options {
    oThought :: String,
    oEyes :: String,
    oTongue :: String,
    oWidth :: Int,
    oFile :: String
} deriving (Show)

defaults :: Options
defaults = Options {
    oThought = "\\",
    oEyes = "oo",
    oTongue = "  ",
    oWidth = 40,
    oFile = "default"
}

-- print all the cows
getCows = do
    files <- getDirectoryContents "cows"
    cows <- mapM readFile (filepaths files)

    mapM putStrLn cows

    where 
        listFiles fs = sort $ filter (isSuffixOf ".cow") fs
        filepaths fs = map (\x -> "cows/" ++ x) (listFiles fs)
