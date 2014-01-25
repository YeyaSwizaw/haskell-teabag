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
	teaRun,
	teaClose,
	teaDestroy
) where

import Teabag.Global
import Teabag.Map

import Control.Monad

import SFML.Window
import SFML.Graphics

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
		 evts :: [(EventType, [SFEvent -> Game -> IO ()])],
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
	return $ G_ wnd' [] gmap'

addCallback :: [(EventType, [SFEvent -> Game -> IO ()])] -> EventType -> (SFEvent -> Game -> IO ()) -> [(EventType, [SFEvent -> Game -> IO ()])]
addCallback [] evtType evtCall = [(evtType, [evtCall])]
addCallback ((t, ls) : evts') evtType evtCall = 
	if t == evtType then
		(t, evtCall : ls) : evts'
	else
		(t, ls) : addCallback evts' evtType evtCall

teaBindEvent :: Monad m => Game -> EventType -> (SFEvent -> Game -> IO ()) -> m Game
teaBindEvent game evtType evtCall = return game { evts = addCallback (evts game) evtType evtCall }

callFuncs :: SFEvent -> Game -> [SFEvent -> Game -> IO ()] -> IO Game
callFuncs evt game = foldr (\f -> (>>) (f evt game)) (runLoop game) 

findAndCallFuncs :: EventType -> SFEvent -> Game -> [(EventType, [SFEvent -> Game -> IO ()])] -> IO Game
findAndCallFuncs _ _ game [] = runLoop game
findAndCallFuncs evtType evt game ((t, fs) : evts') = 
	if t == evtType then
		callFuncs evt game fs
	else 
		findAndCallFuncs evtType evt game evts'

renderWindow :: Game -> IO Game
renderWindow game = do
	clearRenderWindow (wnd game) black
	drawSprite (wnd game) (mapSpr (gmap game)) Nothing
	mapM_ (\(_, s) -> drawSprite (wnd game) s Nothing) $ entSprs (gmap game)
	display (wnd game)
	teaRun game

runLoop :: Game -> IO Game
runLoop game = do
	evt <- pollEvent (wnd game)
	case evt of
		Just e -> findAndCallFuncs (getEvtType e) e game (evts game)
		Nothing -> renderWindow game

teaRun :: Game -> IO Game
teaRun game = do
	running <- isWindowOpen (wnd game)
	(if not running then return else runLoop) game
	
teaClose :: Game -> IO ()
teaClose game = close $ wnd game

teaDestroy :: Game -> IO ()
teaDestroy game = destroy $ wnd game
