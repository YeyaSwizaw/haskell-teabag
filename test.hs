import Teabag.Game
import SFML.Window

exitGame :: SFEvent -> Game -> IO ()
exitGame evt game = do
	teaClose game

keyPressed :: SFEvent -> Game -> IO ()
keyPressed (SFEvtKeyPressed code _ _ _ _) game = case code of
	KeyQ -> teaClose game
	KeyEscape -> teaClose game
	_ -> return ()

main = do
	game <- teaInit
	game <- teaBindEvent game TeaClosed exitGame
	game <- teaBindEvent game TeaKeyPressed keyPressed
	game <- teaRun game
	teaDestroy game
