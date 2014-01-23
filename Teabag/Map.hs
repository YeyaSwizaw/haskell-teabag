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
		 tiletex :: [(String, Texture)],
		 mapTiles :: [[String]],
		 mapTex :: RenderTexture,
		 mapSpr :: Sprite }

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
	mTiles <- readMap tdefs mapColours
	sprites <- createSprites ttex mTiles mapW mapH
	sprSize <- getGlobalBounds $ head sprites
	let sprW = round (fwidth sprSize) :: Int
	let sprH = round (fheight sprSize) :: Int
	let texW = sprW * fromIntegral mapW'
	let texH = sprH * fromIntegral mapH'
	mRenTex <- checkEither =<< createRenderTexture texW texH False
	clear mRenTex black
	mapM_ (\s -> drawSprite mRenTex s Nothing) sprites
	display mRenTex
	mSpr <- checkEither =<< createSprite
	mTex <- getRenderTexture mRenTex
	setTexture mSpr mTex True
	return $ M_ tdefs ttex mTiles mRenTex mSpr

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

createSprites :: [(String, Texture)] -> [[String]] -> Int -> Int -> IO [Sprite]
createSprites _ _ 0 _ = return []
createSprites ttex (row : tiles) x y = liftM2 (++) (createSpriteRow ttex row x y) (createSprites ttex tiles (x - 1) y)

createSpriteRow :: [(String, Texture)] -> [String] -> Int -> Int -> IO [Sprite]
createSpriteRow _ _ _ 0 = return []
createSpriteRow ttex (tname : row) x y = case lookup tname ttex of
	Just tex' -> do
		(Vec2u texW' texH') <- textureSize tex'
		spr <- checkEither =<< createSprite
		setTexture spr tex' True
		let texW = fromIntegral texW' :: Float
		let texH = fromIntegral texH' :: Float
		let x' = fromIntegral x :: Float
		let y' = fromIntegral y :: Float
		print $ texW * x'
		print $ texH * y'
		print tname
		setPosition spr (Vec2f (texW * x') (texH * y'))
		liftM ((:) spr) (createSpriteRow ttex row x (y - 1))
	Nothing -> error $ "Error finding texture for" ++ tname
	
