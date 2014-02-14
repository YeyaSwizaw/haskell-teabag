module Teabag.Map where

import Teabag.Global

import Control.Monad

import SFML.Graphics
import SFML.System

data Tiledef =
	T_
	{ tname    :: String
	, col      :: Color
	, blocking :: Bool
	, collMap  :: Bool
	} deriving (Show, Eq)

data Entdef =
	E_
	{ ename :: String
	, ex    :: Int
	, ey    :: Int
	} deriving (Show, Eq)

data Map =
	M_
	{ tileDefs :: [Tiledef]
	, tileTex  :: [(String, Texture)]
	, mapTiles :: [[String]]
	, mapTex   :: RenderTexture
	, mapSpr   :: Sprite
	, entDefs  :: [Entdef]
	, entTex   :: [(String, Texture)]
	, entSprs  :: [(String, Sprite)]
	}

loadMap :: String -> IO Map
loadMap mapname = do
	optsFile <- loadFile $ teaMapFile mapname
	tdefs <- mapM createTileDef =<< getAllOptions optsFile "tile"
	edefs <- mapM createEntDef =<< getAllOptions optsFile "entity"
	eTexs <- loadEntityTextures [] edefs
	eSprs <- mapM (createEntity eTexs) edefs
	ttex <- mapM loadTileTexture tdefs
	(mTiles, mapW, mapH) <- readTiles mapname tdefs
	tSprites <- createSprites ttex mTiles mapW mapH
	mRenTex <- renderSprites tSprites mapW mapH
	mSpr <- checkEither =<< createSprite
	mTex <- getRenderTexture mRenTex
	setTexture mSpr mTex True
	return $ M_ tdefs ttex mTiles mRenTex mSpr edefs eTexs eSprs

readTiles :: String -> [Tiledef] -> IO ([[String]], Int, Int)
readTiles mapname tdefs = do
	mapImg <- loadImage $ teaMapImgFile mapname
	(Vec2u mapW' mapH') <- imageSize mapImg
	let mapW = fromIntegral $ mapH' - 1
	let mapH = fromIntegral $ mapW' - 1
	mTiles <- readMap tdefs =<< for' (\x -> for' (getPixel mapImg x) mapH) mapW
	return (mTiles, mapH, mapW)

renderSprites :: [Sprite] -> Int -> Int -> IO RenderTexture
renderSprites sprites mapW mapH = do
	sprSize <- getGlobalBounds $ head sprites
	let sprW = round (fwidth sprSize) :: Int
	let sprH = round (fheight sprSize) :: Int
	let texW = sprW * fromIntegral (mapW + 1)
	let texH = sprH * fromIntegral (mapH + 1)
	mRenTex <- checkEither =<< createRenderTexture texW texH False
	clear mRenTex black
	mapM_ (\s -> drawSprite mRenTex s Nothing) sprites
	display mRenTex
	return mRenTex

checkImg :: Maybe Image -> IO Image
checkImg img = case img of
	Nothing -> error "Map image not loaded successfuly"
	Just i -> return i

tileNameFromColor :: [Tiledef] -> Color -> String
tileNameFromColor [] c = error $ "Invalid tile colour in map: " ++ show c
tileNameFromColor (tdef : tdefs) c = if col tdef == c then tname tdef else tileNameFromColor tdefs c

readMap :: Monad m => [Tiledef] -> [[Color]] -> m [[String]]
readMap tdefs tileColours = return (map (map $ tileNameFromColor tdefs) tileColours)

createTileDef :: Monad m => [String] -> m Tiledef
createTileDef [r', g', b', name', coll, collMap'] = 
	return (T_ name' (Color (read r') (read g') (read b') 255) (toBool coll) (toBool collMap'))
	where
	toBool :: String -> Bool
	toBool "0" = False
	toBool _ = True

createEntDef :: Monad m => [String] -> m Entdef
createEntDef [name', ex', ey'] =
	return (E_ name' (read ex') (read ey'))

createEntity :: [(String, Texture)] -> Entdef -> IO (String, Sprite)
createEntity etexs edef = do
	optsFile <- loadFile $ teaEntFile $ ename edef
	[spritename] <- getOptions optsFile "sprite"
	case lookup spritename etexs of
		Nothing -> error "Some sort of unusual entity texture related failure"
		Just spritetex -> do
			spr <- checkEither =<< createSprite
			setTexture spr spritetex True
 			setPosition spr $ Vec2f (fromIntegral $ ex edef) (fromIntegral $ ey edef)
			return (spritename, spr)

loadEntityTextures :: [(String, Texture)] -> [Entdef] -> IO [(String, Texture)]
loadEntityTextures acc [] = return acc
loadEntityTextures acc (edef : edefs) = do
	optsFile <- loadFile $ teaEntFile $ ename edef
	[spritename] <- getOptions optsFile "sprite"
	case lookup spritename acc of
		Nothing -> do
			spritetex <- loadTexture $ teaSprFile spritename
			loadEntityTextures ((ename edef, spritetex) : acc) edefs
		Just _ -> loadEntityTextures acc edefs
			
loadTileTexture :: Tiledef -> IO (String, Texture)
loadTileTexture tdef = do
	tex' <- loadTexture $ teaTileFile $ tname tdef
	return (tname tdef, tex')

createSprites :: [(String, Texture)] -> [[String]] -> Int -> Int -> IO [Sprite]
createSprites _ [] _ _ = return []
createSprites ttex (row : tiles) x y = liftM2 (++) (createSpriteRow ttex row x y) (createSprites ttex tiles (x - 1) y)

createSpriteRow :: [(String, Texture)] -> [String] -> Int -> Int -> IO [Sprite]
createSpriteRow _ [] _ _ = return []
createSpriteRow ttex (tname' : row) x y = case lookup tname' ttex of
	Just tex' -> do
		(Vec2u texW' texH') <- textureSize tex'
		spr <- checkEither =<< createSprite
		setTexture spr tex' True
		let texW = fromIntegral texW' :: Float
		let texH = fromIntegral texH' :: Float
		let x' = fromIntegral x :: Float
		let y' = fromIntegral y :: Float
		setPosition spr $ Vec2f (texW * x') (texH * y')
		liftM ((:) spr) (createSpriteRow ttex row x (y - 1))
	Nothing -> error $ "Error finding texture for" ++ tname'
	
