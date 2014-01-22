module Teabag.Global where

loadFile :: FilePath -> IO [[String]]
loadFile filename = do
	file <- readFile filename
	return . map words $ lines file

getOptions :: (Eq a, Monad m) => [[a]] -> a -> m [a]
getOptions [] key = return [key]
getOptions (opt:opts) key = case opt of
	[] -> getOptions opts key
	(optKey:optVals) -> if optKey == key then return optVals else getOptions opts key

teaFileExt = ".tea"
teaDataDir = "data/"

teaMainFile = teaDataDir ++ "main" ++ teaFileExt
