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
	G_ RenderWindow [(EventType, [SFEvent -> Game -> IO ()])]
 
teaInit :: IO Game
teaInit = do
	dataFile <- loadFile teaMainFile
	name <- liftM unwords $ getOptions dataFile "name"
	[w, h] <- getOptions dataFile "wind"
	wnd <- createRenderWindow (VideoMode (read w) (read h) 32) name [SFDefaultStyle] Nothing
	return $ G_ wnd []

addCallback :: [(EventType, [SFEvent -> Game -> IO ()])] -> EventType -> (SFEvent -> Game -> IO ()) -> [(EventType, [SFEvent -> Game -> IO ()])]
addCallback [] evtType evtCall = [(evtType, [evtCall])]
addCallback ((t, ls):evts) evtType evtCall = 
	if t == evtType then
		((t, evtCall:ls):evts)
	else
		((t, ls):(addCallback evts evtType evtCall))

teaBindEvent :: Monad m => Game -> EventType -> (SFEvent -> Game -> IO ()) -> m Game
teaBindEvent (G_ wnd evts) evtType evtCall = do return (G_ wnd (addCallback evts evtType evtCall))

callFuncs :: SFEvent -> Game -> [SFEvent -> Game -> IO ()] -> IO Game
callFuncs evt game [] = runLoop game
callFuncs evt game (f:ls) = do
	f evt game
	callFuncs evt game ls

findAndCallFuncs :: EventType -> SFEvent -> Game -> [(EventType, [SFEvent -> Game -> IO ()])] -> IO Game
findAndCallFuncs evtType evt game [] = runLoop game
findAndCallFuncs evtType evt game ((t,fs):evts) = 
	if t == evtType then
		callFuncs evt game fs
	else 
		findAndCallFuncs evtType evt game evts

renderWindow :: Game -> IO Game
renderWindow game@(G_ wnd evts) = do
	clearRenderWindow wnd black
	display wnd
	teaRun game

runLoop :: Game -> IO Game
runLoop game@(G_ wnd evts) = do
	evt <- pollEvent wnd
	case evt of
		Just e -> case e of
			SFEvtClosed -> findAndCallFuncs TeaClosed e game evts
			SFEvtResized _ _-> findAndCallFuncs TeaResized e game evts
			SFEvtLostFocus -> findAndCallFuncs TeaLostFocus e game evts
			SFEvtGainedFocus -> findAndCallFuncs TeaGainedFocus e game evts
			SFEvtTextEntered _ -> findAndCallFuncs TeaTextEntered e game evts
			SFEvtKeyPressed _ _ _ _ _ -> findAndCallFuncs TeaKeyPressed e game evts
			SFEvtKeyReleased _ _ _ _ _ -> findAndCallFuncs TeaKeyReleased e game evts
			SFEvtMouseWheelMoved _ _ _ -> findAndCallFuncs TeaMouseWheelMoved e game evts
			SFEvtMouseButtonPressed _ _ _ -> findAndCallFuncs TeaMouseButtonPressed e game evts
			SFEvtMouseButtonReleased _ _ _ -> findAndCallFuncs TeaMouseButtonReleased e game evts
			SFEvtMouseMoved _ _ -> findAndCallFuncs TeaMouseMoved e game evts
			SFEvtMouseEntered -> findAndCallFuncs TeaMouseEntered e game evts
			SFEvtMouseLeft -> findAndCallFuncs TeaMouseLeft e game evts
			SFEvtJoystickButtonPressed _ _ -> findAndCallFuncs TeaJoystickButtonPressed e game evts
			SFEvtJoystickButtonReleased _ _ -> findAndCallFuncs TeaJoystickButtonReleased e game evts
			SFEvtJoystickMoved _ _ _ -> findAndCallFuncs TeaJoystickMoved e game evts
			SFEvtJoystickConnected _ -> findAndCallFuncs TeaJoystickConnected e game evts
			SFEvtJoystickDisconnected _ -> findAndCallFuncs TeaJoystickDisconnected e game evts
		Nothing -> renderWindow game

teaRun :: Game -> IO Game
teaRun game@(G_ wnd evts) = do
	running <- isWindowOpen wnd
	(if running == False then return else runLoop) game
	
teaClose :: Game -> IO ()
teaClose (G_ wnd evts) = close wnd

teaDestroy :: Game -> IO ()
teaDestroy (G_ wnd evts) = destroy wnd
