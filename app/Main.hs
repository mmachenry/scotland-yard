module Main where

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
    mapM_ (putStrLn . show) states
    putStrLn $ show value
    putStrLn $ show currentGameState

s = buildGameState pos moves
pos = [94, 29, 34, 151, 91]
moves =
    [FugitiveMove Taxi,
     DetectiveMove Purple Taxi 95,
     DetectiveMove Yellow Bus 41,
     DetectiveMove Red Bus 63,
     DetectiveMove Green Bus 154,
     DetectiveMove Blue Taxi 105,
     FugitiveMove Taxi,
     DetectiveMove Purple Bus 77,
     DetectiveMove Yellow Bus 52,
     DetectiveMove Red Bus 100,
     DetectiveMove Green Bus 153,
     DetectiveMove Blue Bus 89,
     Reveal Taxi 157,
     DetectiveMove Purple Bus 124,
     DetectiveMove Yellow Bus 67,
     DetectiveMove Red Taxi 113,
     DetectiveMove Green Underground 140,
     DetectiveMove Blue Taxi 105,
     FugitiveMove Black,
     DetectiveMove Purple Bus 153,
     DetectiveMove Yellow Bus 102,
     DetectiveMove Red Taxi 114,
     DetectiveMove Green Taxi 156,
     DetectiveMove Blue Taxi 118,
     FugitiveMove Taxi,
     DetectiveMove Purple Bus 180,
     DetectiveMove Yellow Bus 127,
     DetectiveMove Red Taxi 126,
     DetectiveMove Green Taxi 140,
     DetectiveMove Blue Bus 135,
     FugitiveMove Taxi,
     DetectiveMove Purple Bus 184,
     DetectiveMove Yellow Taxi 116,
     DetectiveMove Red Taxi 127,
     DetectiveMove Green Taxi 156,
     DetectiveMove Blue Taxi 143,
     FugitiveMove Taxi,
     DetectiveMove Purple Bus 153,
     DetectiveMove Yellow Bus 118,
     DetectiveMove Red Bus 102,
     DetectiveMove Green Taxi 157,
     DetectiveMove Blue Taxi 159,
     Reveal Taxi 165,
     DetectiveMove Purple Bus 124,
     DetectiveMove Yellow Taxi 105,
     DetectiveMove Red Bus 105,
     DetectiveMove Green Bus 185,
     DetectiveMove Blue Underground 140,
     FugitiveMove Taxi,
     DetectiveMove Purple Taxi 123,
     DetectiveMove Yellow Taxi 89,
     DetectiveMove Red Underground 111,
     DetectiveMove Green Taxi 184,
     DetectiveMove Blue Underground 153,
     DoubleMove (FugitiveMove Taxi) (FugitiveMove Taxi)
     ]
