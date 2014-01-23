module Teabag.Map where

import Teabag.Global

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
	(Vec2u mapW mapH) <- imageSize mapImg
	mapColours <- for' (\x -> for' (getPixel mapImg x) (fromIntegral $ mapH - 1)) (fromIntegral $ mapW - 1)
	mapTiles <- readMap tdefs mapColours
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
