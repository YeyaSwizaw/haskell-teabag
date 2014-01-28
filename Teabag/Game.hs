module Teabag.Game (
	EventType(
		TeaClosed,
		TeaResized,
		TeaLostFocus,
		TeaGainedFocus,
		TeaTextEntered,
		TeaKeyPressed,
		TeaKeyReleased,
		TeaMouseWheelMoved,
		TeaMouseButtonPressed,
		TeaMouseButtonReleased,
		TeaMouseMoved,
		TeaMouseEntered,
		TeaMouseLeft,
		TeaJoystickButtonPressed,
		TeaJoystickButtonReleased,
		TeaJoystickMoved,
		TeaJoystickConnected,
		TeaJoystickDisconnected
	),

	Game(G_),
	
	teaInit,
	teaBindEvent,
	teaOnTick,
	teaRun,
	teaResizeView,
	teaMoveEntity,
	teaGetEntityPosition,
	teaStoreValue,
	teaGetValue,
	teaClose,
	teaDestroy
) where

import Teabag.Global
import Teabag.Map

import Control.Monad

import SFML.Window
import SFML.Graphics

import Data.Dynamic

data EventType =
	  TeaClosed 
	| TeaResized
	| TeaLostFocus
	| TeaGainedFocus
	| TeaTextEntered
	| TeaKeyPressed
	| TeaKeyReleased
	| TeaMouseWheelMoved
	| TeaMouseButtonPressed
	| TeaMouseButtonReleased
	| TeaMouseMoved
	| TeaMouseEntered
	| TeaMouseLeft
	| TeaJoystickButtonPressed
	| TeaJoystickButtonReleased
	| TeaJoystickMoved
	| TeaJoystickConnected
	| TeaJoystickDisconnected
	deriving (Eq, Show)

data Game = 
	G_ { wnd :: RenderWindow,
		 evts :: [(EventType, [SFEvent -> Game -> IO Game])],
		 ticks :: [Game -> IO Game],
		 vals :: [(String, Dynamic)],
		 gmap :: Map }

getEvtType :: SFEvent -> EventType
getEvtType e = case e of
	SFEvtClosed -> TeaClosed
	SFEvtResized{} -> TeaResized
	SFEvtLostFocus -> TeaLostFocus
	SFEvtGainedFocus -> TeaGainedFocus
	SFEvtTextEntered{} -> TeaTextEntered
	SFEvtKeyPressed{} -> TeaKeyPressed
	SFEvtKeyReleased{} -> TeaKeyReleased
	SFEvtMouseWheelMoved{} -> TeaMouseWheelMoved
	SFEvtMouseButtonPressed{} -> TeaMouseButtonPressed
	SFEvtMouseButtonReleased{} -> TeaMouseButtonReleased
	SFEvtMouseMoved{} -> TeaMouseMoved
	SFEvtMouseEntered -> TeaMouseEntered
	SFEvtMouseLeft -> TeaMouseLeft
	SFEvtJoystickButtonPressed{} -> TeaJoystickButtonPressed
	SFEvtJoystickButtonReleased{} -> TeaJoystickButtonReleased
	SFEvtJoystickMoved{} -> TeaJoystickMoved
	SFEvtJoystickConnected{} -> TeaJoystickConnected 
	SFEvtJoystickDisconnected{} -> TeaJoystickDisconnected
 
teaInit :: IO Game
teaInit = do
	dataFile <- loadFile teaMainFile
	name' <- liftM unwords $ getOptions dataFile "name"
	[w, h] <- getOptions dataFile "wind"
	[mapName] <- getOptions dataFile "start"
	gmap' <- loadMap mapName
	wnd' <- createRenderWindow (VideoMode (read w) (read h) 32) name' [SFDefaultStyle] Nothing
	setFramerateLimit wnd' 60
	return $ G_ wnd' [] [] [] gmap'

addCallback :: [(EventType, [SFEvent -> Game -> IO Game])] -> EventType -> (SFEvent -> Game -> IO Game) -> [(EventType, [SFEvent -> Game -> IO Game])]
addCallback [] evtType evtCall = [(evtType, [evtCall])]
addCallback ((t, ls) : evts') evtType evtCall = 
	if t == evtType then
		(t, evtCall : ls) : evts'
	else
		(t, ls) : addCallback evts' evtType evtCall

teaBindEvent :: Monad m => EventType -> (SFEvent -> Game -> IO Game) -> Game -> m Game
teaBindEvent evtType evtCall game = return game { evts = addCallback (evts game) evtType evtCall }

teaOnTick :: Monad m => (Game -> IO Game) -> Game -> m Game
teaOnTick tickCall game = return game { ticks = tickCall : ticks game }

callFuncs :: SFEvent -> [SFEvent -> Game -> IO Game] -> Game -> IO Game
callFuncs _ [] game = runLoop game
callFuncs evt (f : fs) game = f evt game >>= callFuncs evt fs

findAndCallFuncs :: EventType -> SFEvent -> Game -> [(EventType, [SFEvent -> Game -> IO Game])] -> IO Game
findAndCallFuncs _ _ game [] = runLoop game
findAndCallFuncs evtType evt game ((t, fs) : evts') = 
	if t == evtType then
		callFuncs evt fs game
	else 
		findAndCallFuncs evtType evt game evts'

renderWindow :: Game -> IO Game
renderWindow game = do
	clearRenderWindow (wnd game) black
	drawSprite (wnd game) (mapSpr (gmap game)) Nothing
	mapM_ (\(_, s) -> drawSprite (wnd game) s Nothing) $ entSprs (gmap game)
	display (wnd game)
	teaRun game

callTicks :: [Game -> IO Game] -> Game -> IO Game
callTicks [] game = renderWindow game
callTicks (f : fs) game = f game >>= callTicks fs

runLoop :: Game -> IO Game
runLoop game = do
	evt <- pollEvent (wnd game)
	case evt of
		Just e -> findAndCallFuncs (getEvtType e) e game (evts game)
		Nothing -> callTicks (ticks game) game

teaResizeView :: Int -> Int -> Game -> IO ()
teaResizeView  w h game = setView (wnd game) =<< viewFromRect (FloatRect 0 0 (fromIntegral w) (fromIntegral h))

teaMoveEntity :: String -> Float -> Float -> Game -> IO ()
teaMoveEntity entname xd yd game = case lookup entname $ entSprs $ gmap game of
	Nothing -> error $ entname ++ " not a valid entity"
	Just spr -> move spr $ Vec2f xd yd

teaGetEntityPosition :: String -> Game -> IO (Float, Float)
teaGetEntityPosition entname game = case lookup entname $ entSprs $ gmap game of
	Nothing -> error $ entname ++ " not a valid entity"
	Just spr -> getPosition spr >>= (\(Vec2f xp yp) -> return (xp, yp))

teaStoreValue :: (Monad m, Typeable a) => String -> a -> Game -> m Game
teaStoreValue valname val game = return game { vals = updateListItem (vals game) valname $ toDyn val }

teaGetValue :: (Typeable a) => String -> Game -> Maybe a
teaGetValue valname game = case lookup valname $ vals game of
	Nothing -> Nothing
	Just val -> fromDynamic val

updateListItem :: (Eq a) => [(a, b)] -> a -> b -> [(a, b)]
updateListItem [] valname val = [(valname, val)]
updateListItem ((k, v) : tl) valname val = if valname == k then (k, val) : tl else (k, v) : updateListItem tl valname val

teaRun :: Game -> IO Game
teaRun game = do
	running <- isWindowOpen (wnd game)
	(if not running then return else runLoop) game
	
teaClose :: Game -> IO ()
teaClose game = close $ wnd game

teaDestroy :: Game -> IO ()
teaDestroy game = destroy $ wnd game
