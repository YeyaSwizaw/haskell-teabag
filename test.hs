import Teabag.Game
import SFML.Window

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
onMouseRelease evt = teaStoreValue "goalpos" (x evt, y evt)

onTick :: Game -> IO Game
onTick game = case (teaGetValue "goalpos" game :: Maybe (Int, Int)) of
	Nothing -> print "No stored value for goalpos" >> return game
	Just val -> print val >> return game

main :: IO ()
main = teaInit
	>>= teaBindEvent TeaClosed exitGame
	>>= teaBindEvent TeaKeyPressed keyPressed
	>>= teaBindEvent TeaResized onResize
	>>= teaBindEvent TeaMouseButtonReleased onMouseRelease
	>>= teaOnTick onTick
	>>= teaRun
	>>= teaDestroy
