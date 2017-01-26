module TheBoard (theBoard) where

import qualified Data.Set as Set
import Data.Array.Unboxed
import Types

--type Board = Array Stop (Array Route (Set Stop))
--data Route = TaxiRoute | BusRoute | UndergroundRoute | FerrieRoute

-- convert the board from using lists to sets
convertToSet :: [(Route,[Stop])] -> [(Route,(Set.Set Stop))]
convertToSet [] = []
convertToSet ((r,stops):rest) = (r,Set.fromList stops) : (convertToSet rest)

theBoard :: Board
theBoard =
  array (1,200)
    (zipWith (\a b->(a, array (TaxiRoute,FerrieRoute) (convertToSet b)))
             [1 .. 200]
             [[(TaxiRoute,[8,9]),(BusRoute,[58,46]),(UndergroundRoute,[46]),(FerrieRoute,[])],
              [(TaxiRoute,[10,20]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[4,11,12]),(BusRoute,[22,23]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[3,13]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[15,16]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[7,29]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[6,17]),(BusRoute,[42]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[1,18,19]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[1,19,20]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
  {-10-}      [(TaxiRoute,[2,11,21,34]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[3,10,22]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[3,23]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[4,14,23,24]),(BusRoute,[14,23,52]),(UndergroundRoute,[46,67,89]),(FerrieRoute,[])],
              [(TaxiRoute,[13,15,25]),(BusRoute,[13,15]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[5,14,16,26,28]),(BusRoute,[14,41]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[5,15,28,29]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[7,29,30]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[8,31,43]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[8,9,32]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
  {-20-}      [(TaxiRoute,[2,9,33]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[10,33]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[11,23,34,35]),(BusRoute,[3,23,34,65]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[12,13,22,37]),(BusRoute,[3,13,22,67]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[13,37,38]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[14,38,39]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[15,27,39]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[26,28,40]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[15,16,27,41]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[6,16,17,41,42]),(BusRoute,[41,42,55]),(UndergroundRoute,[]),(FerrieRoute,[])],
  {-30-}      [(TaxiRoute,[17,42]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[18,43,44]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[19,33,44,45]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[20,21,32,46]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[10,22,47,48]),(BusRoute,[22,46,63]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[22,36,48,65]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[35,37,49]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[23,24,36,50]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[24,25,50,51]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[25,26,51,52]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
  {-40-}      [(TaxiRoute,[27,41,52,53]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[28,29,40,54]),(BusRoute,[15,29,52,87]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[29,30,56,72]),(BusRoute,[7,29,72]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[18,31,57]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[31,32,58]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[32,46,58,59,60]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[33,45,47,61]),(BusRoute,[1,34,58,78]),(UndergroundRoute,[1,13,74,79]),(FerrieRoute,[])],
              [(TaxiRoute,[34,46,62]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[34,35,62,63]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[36,50,66]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
  {-50-}      [(TaxiRoute,[37,38,49]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[38,39,52,67,68]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[39,40,51,69]),(BusRoute,[13,67,41,86]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[40,54,69]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[41,53,55,70]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[54,71]),(BusRoute,[29,89]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[42,91]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[43,58,73]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[44,45,57,59,74,75]),(BusRoute,[1,46,74,77]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[45,58,75,76]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
  {-60-}      [(TaxiRoute,[45,61,76]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[46,60,62,76,78]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[47,48,61,79]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[48,64,79,80]),(BusRoute,[34,65,79,100]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[63,65,81]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[35,64,66,82]),(BusRoute,[22,63,67,82]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[49,65,67,82]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[51,66,68,84]),(BusRoute,[23,52,65,82,102]),(UndergroundRoute,[13,79,89,111]),(FerrieRoute,[])],
              [(TaxiRoute,[51,67,69,85]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[52,53,68,86]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
  {-70-}      [(TaxiRoute,[54,71,87]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[55,70,72,89]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[42,71,90,91]),(BusRoute,[42,105,107]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[57,74,92]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[58,73,75,92]),(BusRoute,[58,94]),(UndergroundRoute,[46]),(FerrieRoute,[])],
              [(TaxiRoute,[58,59,74,94]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[59,60,61,77]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[76,78,95,96]),(BusRoute,[58,78,94,124]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[61,77,79,97]),(BusRoute,[46,77,79]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[62,63,78,98]),(BusRoute,[78,63]),(UndergroundRoute,[46,67,93,111]),(FerrieRoute,[])],
  {-80-}      [(TaxiRoute,[63,99,100]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[64,82,100]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[65,66,81,101]),(BusRoute,[65,67,100,140]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[101,102]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[67,85]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[68,84,103]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[69,103,104]),(BusRoute,[52,87,102,116]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[70,88]),(BusRoute,[41,86,105]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[87,89,117]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[71,88,105]),(BusRoute,[55,105]),(UndergroundRoute,[13,67,140,159]),(FerrieRoute,[])],
  {-90-}      [(TaxiRoute,[72,91,105]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[56,72,90,105,107]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[73,74,93]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[92,94]),(BusRoute,[94]),(UndergroundRoute,[79]),(FerrieRoute,[])],
              [(TaxiRoute,[75,93,95]),(BusRoute,[74,77,93]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[77,94,122]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[77,97,109]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[78,96,98,109]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[79,97,99,110]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[80,98,110,112]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
 {-100-}      [(TaxiRoute,[80,81,101,112,113]),(BusRoute,[63,82,111]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[82,83,100,114]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[83,103,115]),(BusRoute,[67,86,127]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[85,86,102]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[86,116]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[89,90,91,106,118]),(BusRoute,[89,72,87,107,118]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[105,107]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[91,106,119]),(BusRoute,[72,105,161]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[96,97,110,124]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
 {-110-}      [(TaxiRoute,[98,99,109,111]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[110,112,124]),(BusRoute,[100,124]),(UndergroundRoute,[67,79,153,163]),(FerrieRoute,[])],
              [(TaxiRoute,[99,100,111,125]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[100,114,125]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[101,113,115,126,131,132]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[102,114,126,127]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[118,157])],
              [(TaxiRoute,[104,117,127,128]),(BusRoute,[86,118,127,142]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[88,116,118,129]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[105,117,119]),(BusRoute,[105,116,135]),(UndergroundRoute,[]),(FerrieRoute,[115])],
              [(TaxiRoute,[107,118,136]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
 {-120-}      [(TaxiRoute,[121,144]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[120,122,145]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[95,121,123,146]),(BusRoute,[123,144]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[122,124,137,148,149]),(BusRoute,[122,124,144,165]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[109,111,123,130,138]),(BusRoute,[77,111,123,153]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[112,113,131]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[114,115,127,140]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[115,116,126,133,134]),(BusRoute,[102,116,133]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[116,129,134,142]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[117,128,135,142,143]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
 {-130-}      [(TaxiRoute,[124,131,139]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[114,125,130]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[114,140]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[127,140,141]),(BusRoute,[127,140,157]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[127,128,141,142]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[129,136,143,161]),(BusRoute,[118,159,161]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[119,135,162]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[123,147]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[124,150,152]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[130,140,153,154]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
 {-140-}      [(TaxiRoute,[126,132,133,139,154,156]),(BusRoute,[82,133,154,156]),(UndergroundRoute,[89,153,159]),(FerrieRoute,[])],
              [(TaxiRoute,[133,134,142,158]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[128,129,134,141,143,158,159]),(BusRoute,[116,157,159]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[129,135,142,159,160]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[120,145,177]),(BusRoute,[122,123,163]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[121,144,146]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[122,145,147,163]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[137,146,164]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[123,149,164]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[123,148,150,165]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
 {-150-}      [(TaxiRoute,[138,149,151]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[150,152,165,166]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[138,151,153]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[139,152,154,166,167]),(BusRoute,[124,154,180,184]),(UndergroundRoute,[111,140,163,185]),(FerrieRoute,[])],
              [(TaxiRoute,[139,140,153,155]),(BusRoute,[140,153,156]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[154,156,167,168]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[140,155,157,169]),(BusRoute,[140,154,157,184]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[156,158,170]),(BusRoute,[133,142,156,185]),(UndergroundRoute,[]),(FerrieRoute,[115,194])],
              [(TaxiRoute,[141,142,157,171]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[142,143,160,172,188]),(BusRoute,[135,142,161,187,199]),(UndergroundRoute,[89,140,185]),(FerrieRoute,[])],
 {-160-}      [(TaxiRoute,[143,159,161,173]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[135,160,174]),(BusRoute,[107,135,159,199]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[136,175]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[146,177]),(BusRoute,[144,176,191]),(UndergroundRoute,[111,153]),(FerrieRoute,[])],
              [(TaxiRoute,[147,148,178,179]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[149,151,179,180]),(BusRoute,[123,180,191]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[151,153,181,183]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[153,155,183,168]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[155,167,184]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[156,184]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
 {-170-}      [(TaxiRoute,[157,171,185]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[158,170,172,186,198]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[159,171,187]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[160,174,188,200]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[161,173,175]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[162,174,200]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[177,189]),(BusRoute,[163,190]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[144,163,176]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[164,189,191]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[164,165,191]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
 {-180-}      [(TaxiRoute,[165,181,193]),(BusRoute,[153,165,184,190]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[166,180,182,193]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[181,183,195]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[166,167,182,196]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[168,169,185,196,197]),(BusRoute,[153,156,180,185]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[170,184,186]),(BusRoute,[157,184,187,199]),(UndergroundRoute,[153,159]),(FerrieRoute,[])],
              [(TaxiRoute,[171,185,198]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[172,188,198]),(BusRoute,[159,185,199]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[159,173,187,199]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[176,178,190]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
 {-190-}      [(TaxiRoute,[189,191,192]),(BusRoute,[176,180,191]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[178,179,190,192]),(BusRoute,[163,165,190]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[190,191,194]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[180,181,194]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[192,193,195]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[157])],
              [(TaxiRoute,[182,194,197]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[183,184,197]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[184,195,196]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[171,186,187]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])],
              [(TaxiRoute,[188,200]),(BusRoute,[159,161,185,187]),(UndergroundRoute,[]),(FerrieRoute,[])],
 {-200-}      [(TaxiRoute,[173,175,199]),(BusRoute,[]),(UndergroundRoute,[]),(FerrieRoute,[])]])
