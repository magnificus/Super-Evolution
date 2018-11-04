output = "input.txt"

func3 (x, y, z) = x+y+z
func2 (x,y) = x+y
func1 x = x

numsPrint = 100

--valNames = ["x","y","z"]
valNames = "x"

--toCorrectFormat (ins, res) =  (zip valNames ins) ++ [("res", res)]


input = [1..numsPrint]

mergeIntoLists [] [] = []
mergeIntoLists [] (p:ps) = [p] : (mergeIntoLists [] ps)
mergeIntoLists (l:ls) (p:ps) = (l++[p]) : (mergeIntoLists ls ps)

formatItem (s, n) = s ++ (" = ") ++ (show n) 

main = do
    let results = map (\r -> ("res", r)) $ map func1 input
    let inputCombined = zip (repeat valNames) input
    let outputList = mergeIntoLists (mergeIntoLists [] inputCombined) results
    let outputFormatted = map (map formatItem) outputList
    --let formatted = zip inputCombined results
    --let formatted = --map (\a -> a ++ [("res", res)]) results
    --let combined = map (\r -> (zip valNames input) ++ [("res", r)]) results
    --let formatted = map toCorrectFormat combined
    return outputFormatted
