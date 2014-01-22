import Teabag.Game
import SFML.Window

exitGame :: SFEvent -> Game -> IO ()
exitGame _ = teaClose

keyPressed :: SFEvent -> Game -> IO ()
keyPressed evt game = case code evt of
	KeyQ -> teaClose game
	KeyEscape -> teaClose game
	_ -> return ()

main :: IO ()
main = do
	game <- teaInit
	game <- teaBindEvent game TeaClosed exitGame
	game <- teaBindEvent game TeaKeyPressed keyPressed
	game <- teaRun game
	teaDestroy game
