import Teabag.Game
import SFML.Window

exitGame :: SFEvent -> Game -> IO ()
exitGame _ = teaClose

keyPressed :: SFEvent -> Game -> IO ()
keyPressed evt game = case code evt of
	KeyQ -> teaClose game
	KeyEscape -> teaClose game
	_ -> return ()

onResize :: SFEvent -> Game -> IO ()
onResize evt = teaResizeView (width evt) (height evt)

main :: IO ()
main = teaInit
	>>= teaBindEvent TeaClosed exitGame
	>>= teaBindEvent TeaKeyPressed keyPressed
	>>= teaBindEvent TeaResized onResize
	>>= teaRun
	>>= teaDestroy
