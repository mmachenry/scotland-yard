module Types (
    Board,
    Stop,
    Move(..),
    Player(..),
    Detective(..),
    Ticket(..),
    Route(..)
) where

import Data.Array.Unboxed
import qualified Data.Set as Set

type Board = Array Stop (Array Route (Set.Set Stop))
data Route = TaxiRoute | BusRoute | UndergroundRoute | FerrieRoute
  deriving (Show,Enum,Ix,Eq,Ord)

type Stop = Int

data Move = 
      DetectiveMove Detective Ticket Stop
    | NullMove Detective Stop
    | FugitiveMove Ticket
    | Reveal Ticket Stop
    | DoubleMove Move Move
    deriving (Show, Read, Eq)

data Player = MrX | Detectives deriving (Show,Eq)

data Detective = Purple | Yellow | Red | Green | Blue
  deriving (Show,Read,Eq,Enum)

data Ticket = Taxi | Bus | Underground | Black
  deriving (Enum,Show,Ix,Eq,Ord, Read)

