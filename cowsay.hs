module Cowsay where

import System.Directory
import System.Environment
import Control.Monad
import Data.List
import Data.List.Split
import System.Console.GetOpt
import Data.Maybe

data Options = Options 
    { oThought :: String
    , oEyes :: String
    , oTongue :: String
    , oWidth :: Int
    , oFile :: String
    , oList :: Bool } 
    deriving Show

defaults :: Options
defaults = Options 
    { oThought = "\\"
    , oEyes = "oo"
    , oTongue = "  "
    , oWidth = 40
    , oFile = "default" 
    , oList = False }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['l']      ["list"]        (NoArg showListCows)            "Lists available cows"
    , Option ['e']      ["eyes"]        (ReqArg setEyes "EYES")         "Set cow's eyes"
    , Option ['T']      ["tongue"]      (ReqArg setTongue "TONGUE")     "Set cow's tongue"
    , Option ['W']      ["Width"]       (ReqArg setWidth "WIDTH")       "Bubble width"
    , Option ['f']      ["File"]        (ReqArg setFilePath "FILE")     "Set cow file"
    ]
    where
        showListCows opt = opt { oList = True }
        setEyes eyes opt = opt { oEyes = take 2 eyes }
        setTongue t opt = opt { oTongue = take 2 t }
        setWidth w opt = opt { oWidth = (read w :: Int) }
        setFilePath fp opt = opt { oFile = fp }

getOpts :: [Options -> Options] -> Options
getOpts = foldl (flip id) defaults

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName

    case getOpt Permute options args of
        (o, n, [])  -> runProg (getOpts o) (concat n)
        _           -> putStrLn $ usageInfo ("Usage: " ++ progName ++ " < option > < message >") options

runProg :: Options -> String -> IO ()
runProg opts thought
    | oList opts        = listAllCows
    | otherwise         = outputCow opts thought
    
outputCow :: Options -> String -> IO ()
outputCow opts thought = do
    cow <- readFile ("cows/" ++ (oFile opts) ++ ".cow")
    let fragCow = lines cow
        rewritten = rewriteCow opts fragCow
        nullMsg = "Cow wants a message! Mooooo. Use -h for all options."
        _thought = if null thought then nullMsg else thought
    putStrLn $ buildBubble _thought (oWidth opts)
    mapM_ putStrLn rewritten

listAllCows :: IO ()
listAllCows = do
    files <- getDirectoryContents "cows"
    mapM_ putStrLn (filepaths files)

    where 
        listFiles fs = sort $ filter (isSuffixOf ".cow") fs
        filepaths fs = map (\x -> "- " ++ (take (length x - 4) x)) (listFiles fs)

rewriteCow :: Options -> [String] -> [String]
rewriteCow opts cowPs = map (\x -> rewriteLine opts x) (filtered cowPs)
    where
        filtered lns = init $ filter validLine lns

validLine :: String -> Bool
validLine [] = True
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

