import GameState
import System.IO
import Types
import System.Environment
import Data.Tree.Game_tree.Negascout

main = do
    str <- hGetContents stdin
    args <- getArgs
    let lookAhead = read (args!!0)
    let startPos:moveStrs = lines str
    let currentGameState = buildGameState (read startPos) (map read moveStrs)
    let (states,value) = negascout currentGameState lookAhead
    mapM_ (putStrLn . displayGameState) states
    putStrLn $ show value
    putStrLn $ displayGameState currentGameState
