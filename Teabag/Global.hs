module Teabag.Global where

import Control.Monad
import System.FilePath

loadFile :: FilePath -> IO [[String]]
loadFile filename = do
	file <- readFile filename
	return . map words $ lines file

getOptions :: Monad m => [[String]] -> String -> m [String]
getOptions [] key = error $ unwords [key, "not a valid option"]
getOptions (opt : opts) key = case opt of
	[] -> getOptions opts key
	(optKey:optVals) -> if optKey == key then return optVals else getOptions opts key

getAllOptions :: Monad m => [[String]] -> String -> m [[String]]
getAllOptions list key = mapM removeHead =<< filterM (matchesKey key) list

matchesKey :: Monad m => String -> [String] -> m Bool
matchesKey _ [] = return False
matchesKey key' (optKey : _) = return $ key' == optKey

removeHead :: Monad m => [a] -> m [a]
removeHead [] = return []
removeHead (_ : t) = return t

createPath :: [String] -> String
createPath [item] = item
createPath (item : items) = item ++ [pathSeparator] ++ createPath items

teaFileExt :: String
teaFileExt = ".tea"

teaDataDir :: String
teaDataDir = "data"

teaMapsDir :: String
teaMapsDir = "maps"

teaMainFile :: String
teaMainFile = createPath [teaDataDir, "main" ++ teaFileExt]

teaMapFile :: String -> String
teaMapFile name = createPath [teaDataDir, teaMapsDir, name ++ teaFileExt]
