module GameStateSpec where

import GameState
import Types
import Test.Hspec
import qualified Data.MultiSet as MS
import qualified Data.Set as Set

s = buildGameState startingPositions moves
startingPositions = [94, 29, 34, 151, 91]
moves = [
     FugitiveMove Taxi,
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

result = GameState [
  DetectiveState Purple
    (MS.fromAscOccurList [(Taxi,8),(Bus,1),(Underground,4)]) 123,
  DetectiveState Yellow
    (MS.fromAscOccurList [(Taxi,7),(Bus,2),(Underground,4)]) 89,
  DetectiveState Red
    (MS.fromAscOccurList [(Taxi,6),(Bus,4),(Underground,3)]) 111,
  DetectiveState Green
    (MS.fromAscOccurList [(Taxi,5),(Bus,5),(Underground,3)]) 184,
  DetectiveState Blue
    (MS.fromAscOccurList [(Taxi,5),(Bus,6),(Underground,2)]) 153
  ]
  (FugitiveState
    (MS.fromAscOccurList [(Taxi,13),(Bus,25),(Underground,7),(Black,4)])
    1
    (Set.fromList [138,147,148,149,151,164,166,178,179,
                   180,181,182,183,190,192,193,194] ))
  (reverse moves)
  13 
  Detectives

spec :: Spec
spec = do
  describe "apply moves" $ do
    it "updates an example game state" $
      s `shouldBe` result
