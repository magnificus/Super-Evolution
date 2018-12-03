output = "input.txt"

func3 (x, y, z) = x**1.25 + 0.7*y - z
func2 (x,y) = x + y**(5.5)
func1 :: Double -> Double
func1 x = x**2 + x**x - 1

numsPrint = 5

valNames2 = ("x", "y")
valNames3 = ("x", "y", "z")
valNames1 = "x"

input = take numsPrint [1.0,5.0..]
vars2 = [(i,j) | i <- input, j <- input]
vars3 = [(i,j,k) | i <- input, j <- input, k <- input]

mergeIntoLists [] [] = []
mergeIntoLists [] (p:ps) = [p] : (mergeIntoLists [] ps)
mergeIntoLists (l:ls) (p:ps) = (l++[p]) : (mergeIntoLists ls ps)

formatItem :: (String, Double) -> String
formatItem (s, n) = s ++ (" = ") ++ (show n) 
formatLine :: [String] -> String
formatLine = foldr1 (\a b -> a ++ "," ++ b)
formatLines :: [String] -> String
formatLines = foldr1 (\a b -> a ++ "\n" ++b)


output1 = mergeIntoLists [[(valNames1, val)] | val <- input] results
    where results = [("res", r) | r <- map func1 input]

output2 = mergeIntoLists inputCombined results
    where (val1,val2) = valNames2
          inputCombined = [[(val1,v1),(val2,v2)] | (v1,v2) <- vars2]
          results = [("res", r) | r <- map func2 vars2]

output3 = mergeIntoLists inputCombined results
    where (val1,val2,val3) = valNames3
          inputCombined = [[(val1,v1),(val2,v2),(val3,v3)] | (v1,v2,v3) <- vars3]
          results = [("res", r) | r <- map func3 vars3]

main = do
    let outputList = output3
    let outputFormatted = formatLines $ map formatLine $ map (map formatItem) outputList
    writeFile output outputFormatted
    return outputFormatted

