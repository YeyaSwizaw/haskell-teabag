import Teabag.Game
import SFML.Window

import Control.Monad

exitGame :: SFEvent -> Game -> IO Game
exitGame _ game = teaClose game >> return game

keyPressed :: SFEvent -> Game -> IO Game
keyPressed evt game = case code evt of
	KeyQ -> teaClose game >> return game
	KeyEscape -> teaClose game >> return game
	_ -> return game

onResize :: SFEvent -> Game -> IO Game
onResize evt game = teaResizeView (width evt) (height evt) game >> return game

onMouseRelease :: SFEvent -> Game -> IO Game
onMouseRelease evt = teaStoreValue "goalpos" (fromIntegral (x evt) :: Float, fromIntegral (y evt) :: Float)

onTick :: Game -> IO Game
onTick game = case (teaGetValue "goalpos" game :: Maybe (Float, Float)) of
	Nothing -> return game
	Just val -> movePlayer val game >> return game

movePlayer :: (Float, Float) -> Game -> IO ()
movePlayer (xg, yg) game = do
	(xp, yp) <- teaGetEntityPosition "player" game
	let xd = if abs (xp - xg) > 2 then xg - xp else 0
	let yd = if abs (yp - yg) > 2 then yg - yp else 0
	unless (xd == 0 && yd == 0) 
		(let mag = sqrt $ (xd ^^ (2 :: Int)) + (yd ^^ (2 :: Int)) in
		teaMoveEntity "player" ((xd / mag) * 2) ((yd / mag) * 2) game)

main :: IO ()
main = teaInit
	>>= teaBindEvent TeaClosed exitGame
	>>= teaBindEvent TeaKeyPressed keyPressed
	>>= teaBindEvent TeaResized onResize
	>>= teaBindEvent TeaMouseButtonReleased onMouseRelease
	>>= teaOnTick onTick
	>>= teaRun
	>>= teaDestroy
