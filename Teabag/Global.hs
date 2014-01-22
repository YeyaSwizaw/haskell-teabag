module Teabag.Global where

loadFile :: FilePath -> IO [[String]]
loadFile filename = do
	file <- readFile filename
	return . map words $ lines file

getOptions :: Monad m => [[String]] -> String -> m [String]
getOptions [] key = error $ unwords [key, "not a valid option"]
getOptions (opt:opts) key = case opt of
	[] -> getOptions opts key
	(optKey:optVals) -> if optKey == key then return optVals else getOptions opts key

teaFileExt :: String
teaFileExt = ".tea"

teaDataDir :: String
teaDataDir = "data/"

teaMainFile :: String
teaMainFile = teaDataDir ++ "main" ++ teaFileExt
