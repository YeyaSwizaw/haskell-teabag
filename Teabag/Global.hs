module Teabag.Global where

import Control.Monad
import System.FilePath

import SFML.Graphics

for :: (Eq a, Num a) => (a -> a1) -> a -> [a1]
for f 0 = [f 0]
for f n = f n : for f (n - 1)

for' :: (Eq a, Num a, Monad m) => (a -> m a1) -> a -> m [a1]
for' f 0 = do
	item <- f 0
	return [item]
for' f n = do
	item <- f n
	liftM ((:) item) (for' f (n - 1))

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

loadImage :: FilePath -> IO Image
loadImage path = checkImg =<< imageFromFile path

checkImg :: Maybe Image -> IO Image
checkImg img = case img of
	Nothing -> error "Image not loaded successfuly"
	Just i -> return i

loadTexture :: FilePath -> IO Texture
loadTexture path = checkEither =<< textureFromFile path Nothing

checkEither :: (Monad m, Show a) => Either a a1 -> m a1
checkEither (Left ex) = error (show ex)
checkEither (Right tex) = return tex

createPath :: [String] -> String
createPath [item] = item
createPath (item : items) = item ++ [pathSeparator] ++ createPath items

teaFileExt :: String
teaFileExt = ".tea"

teaImgExt :: String
teaImgExt = ".png"

teaDataDir :: String
teaDataDir = "data"

teaMapsDir :: String
teaMapsDir = "maps"

teaTilesDir :: String
teaTilesDir = "tiles"

teaMainFile :: String
teaMainFile = createPath [teaDataDir, "main" ++ teaFileExt]

teaMapFile :: String -> String
teaMapFile name = createPath [teaDataDir, teaMapsDir, name ++ teaFileExt]

teaMapImgFile :: String -> String
teaMapImgFile name = createPath [teaDataDir, teaMapsDir, name ++ teaImgExt]

teaTileFile :: String -> String
teaTileFile name = createPath [teaDataDir, teaTilesDir, name ++ teaImgExt]
