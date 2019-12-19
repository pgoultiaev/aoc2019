import           Data.Set                      as Set
import           Data.List                     as List
import           Data.Maybe                    as Maybe

main :: IO ()
main = do
    input <- readFile "3.txt"
    let a = runInstr $ Prelude.map parseWire $ lines input
    print $ solve a
    print $ solve2 a

solve :: [[(Int, Int)]] -> Int
solve []       = -1
solve (x : xs) = elemAt 1 $ Set.map manhattanDistanceToOrigin $ intersection
    (fromList x)
    (fromList $ head xs)

solve2 :: [[(Int, Int)]] -> Int
solve2 []       = -1
solve2 (a : xs) = elemAt 1 $ Set.map (traverseDistanceAB a b) intersections
  where
    intersections = intersection (fromList a) (fromList b)
    b             = head xs

traverseDistanceAB :: [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> Int
traverseDistanceAB a b loc =
    fromMaybe 0 (List.elemIndex loc a) + fromMaybe 0 (List.elemIndex loc b)


manhattanDistanceToOrigin :: (Int, Int) -> Int
manhattanDistanceToOrigin (0, 0) = 0
manhattanDistanceToOrigin a      = abs (fst a) + abs (snd a)

runInstr :: [[String]] -> [[(Int, Int)]]
runInstr = Prelude.map (\x -> traverseGrid x (0, 0))

traverseGrid :: [String] -> (Int, Int) -> [(Int, Int)]
traverseGrid []   currLoc = [currLoc]
traverseGrid wire currLoc = currLoc : traverseGrid (tail wire) nextLoc
    where nextLoc = move currLoc (head wire)

move :: (Int, Int) -> String -> (Int, Int)
move (x, y) "R" = (x + 1, y)
move (x, y) "D" = (x, y - 1)
move (x, y) "L" = (x - 1, y)
move (x, y) "U" = (x, y + 1)
move (x, y) _   = (x, y)

parseWire :: String -> [String]
parseWire s = concatMap parseInstruction $ words $ replace ',' ' ' s

parseInstruction :: String -> [String]
parseInstruction []       = []
parseInstruction (x : xs) = replicate (read xs :: Int) [x]


-- Utils
replace :: Char -> Char -> String -> String
replace from to = Prelude.map $ \c -> if c == from then to else c
