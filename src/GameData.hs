module GameData (
  movesFromStop,
  startPositions,
  revealTurns,
  nextPlayer
  ) where

import Types
import Data.Array.Unboxed
import qualified Data.Set as Set
import TheBoard

-- the positions that players may start from on the game board
startPositions :: Set.Set Stop
startPositions = Set.fromList
  [13,26,29,34,50,53,91,94,103,112,117,132,138,141,155,174,197,198]

-- the turns on which MrX must reveal himself to the detectives
revealTurns :: Set.Set Stop
revealTurns = Set.fromList [3,8,13,18,24]

-- the stops a player may go to from the given stop with the given ticket
movesFromStop :: Stop -> Ticket -> Set.Set Stop
movesFromStop stop Taxi = theBoard!stop!TaxiRoute
movesFromStop stop Bus = theBoard!stop!BusRoute
movesFromStop stop Underground = theBoard!stop!UndergroundRoute
movesFromStop stop Black =
  Set.unions $ map (theBoard!stop!) [minBound..maxBound]

nextPlayer :: Player -> Player
nextPlayer MrX = Detectives
nextPlayer Detectives = MrX

