module Cowsay where

import System.Directory
import System.Environment
import Control.Monad
import Data.List
import Data.Char (isSpace)

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

main = do
    cow <- readFile ("cows/default.cow")
    let fragCow = lines cow
        rewritten = rewriteCow defaults fragCow

    mapM_ putStrLn rewritten

getAll = do
    files <- getDirectoryContents "cows"
    mapM_ putStrLn (filepaths files)

    where 
        listFiles fs = sort $ filter (isSuffixOf ".cow") fs
        filepaths fs = map (\x -> "cows/" ++ x) (listFiles fs)

rewriteCow :: Options -> [String] -> [String]
rewriteCow opts cowPs = map (\x -> rewriteLine opts x) (filtered cowPs)
    where
        filtered lns = init $ filter validLine lns

validLine :: String -> Bool
validLine (x:xs)
    | x `notElem` "$#"      = True
    | otherwise             = False

rewriteLine :: Options -> String -> String
rewriteLine opts line =
    case elemIndex '$' line of
        Nothing     -> line
        Just i      -> rewriteLine opts (foldr (\y x -> replaceVar x i y) line substitutions)
    where 
        substitutions = [("thoughts", oThought opts), ("eyes", oEyes opts), ("tongue", oTongue opts)]

replaceVar :: String -> Int -> (String, String) -> String
replaceVar line index (opt, val)
    | opt `isPrefixOf` post     = init pre ++ val ++ (drop (length opt) post) 
    | otherwise                 = line
    where 
        (pre, post) = splitAt (index + 1) line

--buildBubble 

textWrap :: String -> Int -> [String]
textWrap str maxL
    | length str < maxL         = [str]
    | otherwise                 = take maxL str : textWrap (drop maxL str) maxL

