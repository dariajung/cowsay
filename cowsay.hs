module Cowsay where

import System.Directory
import System.Environment
import Control.Monad
import Data.List
import Data.List.Split

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
    args <- getArgs
    progName <- getProgName
    case (length args) of 
        0       -> putStrLn $ "Usage: " ++ progName ++ " < message >"
        _       -> runProg defaults (head args)

runProg :: Options -> String -> IO ()
runProg opts thought = do
    cow <- readFile ("cows/default.cow")
    let fragCow = lines cow
        rewritten = rewriteCow defaults fragCow
    putStrLn $ buildBubble thought (oWidth opts)
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

getBorder :: [a] -> Int -> [String]
getBorder lns index
    | length(lns) < 2           = [ "<", ">" ]
    | index == 0                = [ "/", "\\" ]
    | index == (length lns) - 1 = [ "\\", "/" ]
    | otherwise                 = [ "|", "|" ]

buildBubble :: String -> Int -> String
buildBubble str maxL = 
    intercalate "\n" (bubbleTop ++ (init $ helper nLns 0) ++ bubbleBottom)
    where 
        _nLns = (textWrap str maxL)
        bordersize = length $ head _nLns
        nLns = map (\x -> normalize x bordersize) _nLns
        bubbleTop = [" " ++ (concat $ replicate bordersize "_")]
        bubbleBottom = [" " ++ (concat $ replicate bordersize "-")]
        helper lns index = 
            case (index < length lns) of
                True        -> [(border !! 0) ++ (lns !! index) ++ (border !! 1)] ++ (helper lns (index + 1))
                False       -> [[]]
            where
                border = getBorder lns index

normalize :: [Char] -> Int -> [Char]
normalize ln maxL
    | length ln < maxL      = ln ++ (concat $ replicate (maxL - length ln) " ")
    | otherwise             = ln

textWrap :: String -> Int -> [String]
textWrap [] _ = []
textWrap str maxL = drop 1 $ reverse $ genSent zipped 0 [] [[]] maxL
    where
        _words = splitOn " " str
        zipped = zip (map (\x -> length x) _words) _words
        genSent [] _ currSent sentences _ = currSent : sentences 
        genSent ws@(x:xs) currCount currSent sentences _max
            | currCount + fst x < _max      = genSent xs (currCount + fst x + 1) (currSent ++ " " ++ snd x) sentences _max
            | otherwise                     = genSent ws 0 [] (currSent : sentences) _max

