main :: IO ()
main = do
    input <- readFile "2.txt"
    print $ solve 12 2 $ parse input
    print $ solve2 [(nouns, verbs) | nouns <- [0..99], verbs <- [0..99]] (parse input) [0]

solve :: Int -> Int -> [Int] -> Int
solve first second x = head $ runOps 0 (setAtIndex 1 first (setAtIndex 2 second x))

solve2 :: [(Int, Int)] -> [Int] -> [Int] -> Int
solve2 combinations input output
    | head output == 19690720 = 100 * output !! 1 + output !! 2
    | otherwise = solve2 (tail combinations) input $ runOps 0 (setAtIndex 1 noun (setAtIndex 2 verb input))
    where
        noun = fst h
        verb = snd h
        h = head combinations


runOps :: Int -> [Int] -> [Int]
runOps opLoc s
    | s !! opLoc == 1 = runOps (opLoc + 4) (setAtIndex iOutput (input1 + input2) s)
    | s !! opLoc == 2 = runOps (opLoc + 4) (setAtIndex iOutput (input1 * input2) s)
    | otherwise = s
    where
        input1 =  s !! (s !! (opLoc + 1))
        input2 =  s !! (s !! (opLoc + 2))
        iOutput =  s !! (opLoc + 3)

parse :: String -> [Int]
parse s = map read $ words $ replace ',' ' ' s


-- Utils, could import these from some libraries, but chose to build these functions myself for the sake of learning
replace :: Char -> Char -> String -> String
replace from to = map $ \c -> if c == from then to else c

setAtIndex :: Int -> Int -> [Int] -> [Int]
setAtIndex i r s = xs ++ [r] ++ tail ys
    where (xs, ys) = splitAt i s
