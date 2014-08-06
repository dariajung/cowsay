module Cowsay where

import System.Directory
import System.Environment
import Control.Monad
import Data.List
import Data.List.Split
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
textWrap [] _ = []
textWrap str maxL = drop 1 $ reverse $ genSent zipped 0 [] [[]] maxL
    where
        _words = splitOn " " str
        zipped = zip (map (\x -> length x) _words) _words
        genSent [] _ currSent sentences _ = currSent : sentences 
        genSent ws@(x:xs) currCount currSent sentences _max
            | currCount + fst x < (_max - 2)    = genSent xs (currCount + fst x) (currSent ++ " " ++ snd x) sentences _max
            | otherwise                   = genSent ws 0 [] (currSent : sentences) _max

