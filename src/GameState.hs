{-# LANGUAGE TypeSynonymInstances #-}

module GameState (
  GameState(..),
  applyMoves,
  DetectiveState(..),
  FugitiveState(..),
  buildGameState,
  displayGameState,
  displayDetectiveState,
  displayFugitiveState,
  heur,
  initGameState,
  detectiveMoves,
  growFugitiveLocations
) where

import Data.List
import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet
import Types
import GameData
import Data.Tree.Game_tree.Game_tree
import FloydWarshall
import Data.Array.Unboxed
import Data.Ord

data GameState = GameState {
    detectives :: [DetectiveState],
    fugitive :: FugitiveState,
    history :: [Move],
    turnsLeft :: Int,
    playerToMove :: Player
} deriving (Eq, Show)

data DetectiveState = DetectiveState {
    detectiveColor :: Detective,
    detectiveTickets :: MultiSet.MultiSet Ticket,
    detectivePosition :: Stop
} deriving (Eq, Show)

data FugitiveState = FugitiveState {
    fugitiveTickets :: MultiSet.MultiSet Ticket,
    fugitiveDoubleMoves :: Int,
    fugitivePositions :: Set.Set Stop
} deriving (Eq, Show)

detectivePositions gs = Set.fromList (map detectivePosition (detectives gs))

instance Game_tree GameState where
    is_terminal gs =
        Set.null (fugitivePositions (fugitive gs)) || turnsLeft gs == 0
    node_value gs =
        (if playerToMove gs == MrX then 1 else -1) * heur gs
    children = nextGameState

legalMoves :: GameState -> [Move]
legalMoves gs@(GameState ds (FugitiveState tickets _ positions) _ _ MrX) =
    if isRevealTurn gs
    then [Reveal ticket stop |
          ticket <- map fst (MultiSet.toOccurList tickets),
          position <- Set.toList positions,
          stop <- Set.toList
                      (Set.difference
                          (movesFromStop position ticket)
                          (Set.fromList (map detectivePosition ds)))]
    else [FugitiveMove ticket |
          ticket <- map fst (MultiSet.toOccurList tickets),
          any (\p->not (Set.null (movesFromStop p ticket)))
              (Set.toList positions)]

isRevealTurn :: GameState -> Bool
isRevealTurn gs = Set.member (24-turnsLeft gs) revealTurns

nextGameState :: GameState -> [GameState]
nextGameState gs@(GameState _ _ _ _ MrX) = map (applyMove gs) (legalMoves gs)
nextGameState gs@(GameState ds f h t Detectives) =
    [applyMoves gs moves |
     p' <- detectiveMoves (head ds),
     y' <- detectiveMoves (ds!!1),
     r' <- detectiveMoves (ds!!2),
     g' <- detectiveMoves (ds!!3),
     b' <- detectiveMoves (ds!!4),
     let moves = [p',y',r',g',b'],
     noCollisions (map detectivePosition ds) (map movePosition moves)]
    where noCollisions :: [Stop] -> [Stop] -> Bool
          noCollisions [] [] = True
          noCollisions orig (d':otherd') =
                 notElem d' otherd'
              && notElem d' orig
              && noCollisions (tail orig) otherd'
          movePosition (DetectiveMove _ _ s) = s
          movePosition (NullMove _ s) = s
 
detectiveMoves :: DetectiveState -> [Move]
detectiveMoves ds@(DetectiveState color tickets position) =
    let moves =
            [DetectiveMove color ticket stop |
             ticket <- map fst (MultiSet.toOccurList tickets),
             stop <- Set.toList (movesFromStop position ticket)]
    in if null moves then [NullMove color position] else moves
 
----------------------------------------------------------------------
-- Initial game state
----------------------------------------------------------------------

initGameState :: [Stop] -> GameState
initGameState ds =
    GameState
        (zipWith initDetective [Purple ..] ds)
        (initFugitive ds)
        []
        24
        MrX
 
initDetective :: Detective -> Stop -> DetectiveState
initDetective color =
    DetectiveState
        color
        (MultiSet.fromOccurList [(Taxi,10),(Bus,8),(Underground,4)])

initFugitive stops =
    FugitiveState 
        (MultiSet.fromOccurList
            [(Taxi,4),(Bus,3),(Underground,3),(Black,5)])
        2
        (Set.difference startPositions (Set.fromList stops))

buildGameState :: [Stop] -> [Move] -> GameState
buildGameState stops = applyMoves (initGameState stops)

----------------------------------------------------------------------
-- Apply move
----------------------------------------------------------------------

applyMoves :: GameState -> [Move] -> GameState
applyMoves = foldl applyMove

applyMove :: GameState -> Move -> GameState
applyMove (GameState ds f h turnsLeft player) move =
    GameState (map (applyMoveDetective move) ds)
              (applyMoveFugitive move f ds)
              (move:h)
              (turnsLeft - numFugitveMoves move)
              (nextPlayer player)

applyMoveDetective (DetectiveMove c t s) d@(DetectiveState color tickets stop)
    | color == c = DetectiveState color (MultiSet.delete t tickets) s
    | otherwise = d
applyMoveDetective _ d = d

applyMoveFugitive move (FugitiveState tickets doubleMoves stops) ds =
    FugitiveState
             (applyMoveFugitiveTickets move tickets)
             (applyMoveDoubles move doubleMoves)
             (findFugitive
                 move
                 stops
                 (Set.fromList (map detectivePosition ds)))

applyMoveFugitiveTickets (DetectiveMove _ t _) ft = MultiSet.insert t ft
applyMoveFugitiveTickets (NullMove _ _) ft = ft
applyMoveFugitiveTickets (FugitiveMove t) ft = MultiSet.delete t ft
applyMoveFugitiveTickets (Reveal t _) ft = MultiSet.delete t ft
applyMoveFugitiveTickets (DoubleMove move1 move2) ft =
    applyMoveFugitiveTickets move2 (applyMoveFugitiveTickets move1 ft)

applyMoveDoubles (DoubleMove m1 m2) dm =
    applyMoveDoubles m2 (applyMoveDoubles m1 (dm-1))
applyMoveDoubles _ dm = dm

--the set of stops MrX may be in after the the given legal move is taken
findFugitive ::  Move -> Set.Set Stop -> Set.Set Stop-> Set.Set Stop
findFugitive (DetectiveMove _ _ stop) fl dl = Set.delete stop fl
findFugitive (NullMove _ _) fl _ = fl
findFugitive (FugitiveMove ticket) fl dl =
    Set.difference (growFugitiveLocations fl ticket) dl
findFugitive (Reveal _ stop) _ dl = Set.singleton stop
findFugitive (DoubleMove m1 m2) fl dl =
    findFugitive m2 (findFugitive m1 fl dl) dl

-- a set of the places a fugitive may be after using the given ticket
growFugitiveLocations :: Set.Set Stop -> Ticket -> Set.Set Stop
growFugitiveLocations fl ticket =
    Set.unions (map (`movesFromStop` ticket) (Set.toList fl))

numFugitveMoves (FugitiveMove _) = 1
numFugitveMoves (Reveal _ _) = 1
numFugitveMoves (DoubleMove m1 m2) = numFugitveMoves m1 + numFugitveMoves m2
numFugitveMoves _ = 0

----------------------------------------------------------------------
-- Display
----------------------------------------------------------------------
displayGameState :: GameState -> String
displayGameState (GameState ds f h _ nextp) =
  intercalate "\n" (map displayDetectiveState ds)
  ++ "\n" ++ displayFugitiveState f ++ 
  "\nLast move: " ++ (if null h then "N/A" else show (head h)) ++
  "\nNext player: " ++ show nextp ++
  "\nMoves: " ++ show (length h) ++ "\n"

displayDetectiveState :: DetectiveState -> String
displayDetectiveState (DetectiveState color tickets position) =
  show color ++ ": " ++ show position ++ " " ++
  show (MultiSet.toOccurList tickets)

displayFugitiveState :: FugitiveState -> String
displayFugitiveState (FugitiveState tickets _ positions) =
  "Mr. X: " ++ show (Set.toList positions) ++ " " ++
  show (MultiSet.toOccurList tickets)

----------------------------------------------------------------------
-- Heuristics
----------------------------------------------------------------------
{-
It is bad for the set of places Mr. X could be to be large.
It is bad for the set of places to be able to grow larger in a move.
It is bad for the set of places to have locations far from one another.
It is bad for the detectives to be far away.
-}

-- This is a really simple unweighted heuristic that says you get a point 
-- against for every element of the potential fugitive positions as well as
-- a point for the distances of the detectives from the positions. This likely
-- should be weighted very differently. Consider also a measurement of how
-- close the fugitive locations are to each other.

heur :: GameState -> Int
heur gs = detectiveDistances gs + cardinality gs

detectiveDistances :: GameState -> Int
detectiveDistances gs =
    let fp = fugitivePositions (fugitive gs)
        dp = map detectivePosition (detectives gs)
        totalDistances = sum (map (totalDistanceTo fp) dp)
    in totalDistances

totalDistanceTo :: Set.Set Stop -> Stop -> Int
totalDistanceTo set pos =
    Set.fold (\p total->total + (distance pos p)) 0 set

cardinality :: GameState -> Int
cardinality gs = Set.size (fugitivePositions (fugitive gs))

