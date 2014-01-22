module Teabag.Global where

loadFile :: FilePath -> IO [[String]]
loadFile filename = do
	file <- readFile filename
	return . map words $ lines file
