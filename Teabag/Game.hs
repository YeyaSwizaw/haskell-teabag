module Teabag.Game (
	EventType(TeaClosed, TeaKeyPressed),
	Game(G_),
	
	teaInit,
	teaBindEvent,
	teaRun,
	teaClose,
	teaDestroy
) where

import Control.Monad

import SFML.Window
import SFML.Graphics

data EventType =
	TeaClosed     |
	TeaKeyPressed
	deriving (Eq, Show)

data Game = 
	G_ RenderWindow [(EventType, [SFEvent -> Game -> IO ()])]
 
teaInit :: IO Game
teaInit = do
	wnd <- createRenderWindow (VideoMode 800 600 32) "Teabag" [SFDefaultStyle] Nothing
	return (G_ wnd [])

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
renderWindow (G_ wnd evts) = do
	clearRenderWindow wnd black
	display wnd
	teaRun (G_ wnd evts)

runLoop :: Game -> IO Game
runLoop (G_ wnd evts) = do
	evt <- pollEvent wnd
	case evt of
		Just e -> case e of
			SFEvtClosed -> findAndCallFuncs TeaClosed e (G_ wnd evts) evts
			SFEvtKeyPressed _ _ _ _ _ -> findAndCallFuncs TeaKeyPressed e (G_ wnd evts) evts
			_ -> runLoop (G_ wnd evts)
		Nothing -> renderWindow (G_ wnd evts)

teaRun :: Game -> IO Game
teaRun (G_ wnd evts) = do
	running <- isWindowOpen wnd
	if running == False then (return (G_ wnd evts)) else (runLoop (G_ wnd evts))
	
teaClose :: Game -> IO ()
teaClose (G_ wnd evts) = close wnd

teaDestroy :: Game -> IO ()
teaDestroy (G_ wnd evts) = destroy wnd
