import Data.Map
import Data.List
import Data.Maybe
import DataTypes

beginString = "X"
crossMapping = Data.Map.fromList [("X", "FXXK"), ("F", "FF")]
functionMapping = Data.Map.fromList[("X", (+)), ("F", (-))]

charToString c = [c]

-- get l-system generation n with start string b
getGeneration n b = (!! n) $ iterate nextGen b
nextGen s = concat $ Data.List.map (transform . charToString) s
--if the string does not have a mapping we just use it again
transform s = fromMaybe s $ Data.Map.lookup s crossMapping
getFunction s = fromMaybe (\a b-> a) $ Data.Map.lookup s functionMapping
