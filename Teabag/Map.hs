module Teabag.Map where

import Teabag.Global

import Control.Monad

import SFML.Graphics
import SFML.System

data Tiledef =
	T_ { name :: String,
		  col :: Color,
          blocking :: Bool,
          collMap :: Bool }
	deriving (Show, Eq)

data Map =
	M_ { tiledefs :: [Tiledef],
		 tiletex :: [(String, Texture)] }

loadMap :: String -> IO Map
loadMap mapname = do
	optsFile <- loadFile $ teaMapFile mapname
	tiles <- getAllOptions optsFile "tile"
	tdefs <- mapM createTileDef tiles
	ttex <- mapM loadTileTexture tdefs
	mapImg <- loadImage $ teaMapImgFile mapname
	(Vec2u mapW' mapH') <- imageSize mapImg
	let mapW = fromIntegral $ mapH' - 1
	let mapH = fromIntegral $ mapW' - 1
	mapColours <- for' (\x -> for' (getPixel mapImg x) mapH) mapW
	mapTiles <- readMap tdefs mapColours
	sprites <- createSprites ttex mapTiles
	return (M_ tdefs ttex)

checkImg :: Maybe Image -> IO Image
checkImg img = case img of
	Nothing -> error "Map image not loaded successfuly"
	Just i -> return i

tileNameFromColor :: [Tiledef] -> Color -> String
tileNameFromColor [] c = error $ "Invalid tile colour in map: " ++ show c
tileNameFromColor (tdef : tdefs) c = if col tdef == c then name tdef else tileNameFromColor tdefs c

readMap :: Monad m => [Tiledef] -> [[Color]] -> m [[String]]
readMap tdefs tileColours = return (map (map $ tileNameFromColor tdefs) tileColours)

createTileDef :: Monad m => [String] -> m Tiledef
createTileDef [r', g', b', name', coll, collMap'] = 
	return (T_ name' (Color (read r') (read g') (read b') 255) (toBool coll) (toBool collMap'))
	where
	toBool :: String -> Bool
	toBool "0" = False
	toBool _ = True

loadTileTexture :: Tiledef -> IO (String, Texture)
loadTileTexture tdef = do
	tex' <- loadTexture $ teaTileFile $ name tdef
	return (name tdef, tex')

createSprites :: [(String, Texture)] -> [[String]] -> IO [Sprite]
createSprites = createSprites' 0

createSprites' :: Int -> [(String, Texture)] -> [[String]] -> IO [Sprite]
createSprites' _ _ [] = return [] 
createSprites' x ttex (row : tiles) = liftM2 (++) (createSpriteRow x ttex row) (createSprites' (x + 1) ttex tiles)

createSpriteRow :: Int -> [(String, Texture)] -> [String] -> IO [Sprite]
createSpriteRow = createSpriteRow' 0

createSpriteRow' :: Int -> Int -> [(String, Texture)] -> [String] -> IO [Sprite]
createSpriteRow' _ _ _ [] = return []
createSpriteRow' x y ttex (tname : row) = case lookup tname ttex of
	Just tex' -> do
		(Vec2u texW' texH') <- textureSize tex'
		spr <- checkEither =<< createSprite
		setTexture spr tex' True
		let texW = fromIntegral texW' :: Float
		let texH = fromIntegral texH' :: Float
		let x' = fromIntegral x :: Float
		let y' = fromIntegral y :: Float
		setPosition spr (Vec2f (texW * x') (texH * y'))
		liftM ((:) spr) (createSpriteRow' x (y + 1) ttex row)
	Nothing -> error $ "Error finding texture for" ++ tname
	
	
