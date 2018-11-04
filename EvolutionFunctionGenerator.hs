output = "input.txt"

func3 (x, y, z) = x+y+z
func2 (x,y) = x+y
func1 x = x^x + 4

numsPrint = 20

--valNames = ["x","y","z"]
valNames = "x"

input = [1..numsPrint]
input2 = [[1..numsPrint], [1..numsPrint]]

mergeIntoLists [] [] = []
mergeIntoLists [] (p:ps) = [p] : (mergeIntoLists [] ps)
mergeIntoLists (l:ls) (p:ps) = (l++[p]) : (mergeIntoLists ls ps)

formatItem (s, n) = s ++ (" = ") ++ (show n) 
formatLine = foldr1 (\a b -> a ++ "," ++ b)
formatLines = foldr1 (\a b -> a ++ "\n" ++b)


main = do
    let results = map (\r -> ("res", r)) $ map func1 input
    let inputCombined = zip (repeat valNames) input
    let outputList = mergeIntoLists (mergeIntoLists [] inputCombined) results
    let outputFormatted = formatLines $ map formatLine $ map (map formatItem) outputList
    writeFile output outputFormatted
    return outputFormatted
