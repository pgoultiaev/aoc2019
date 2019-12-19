import           Data.Char                      ( digitToInt )
import           Data.List                      ( group )

main :: IO ()
main = do
    print $ solve [240298 .. 784956]
    print $ solve2 [240298 .. 784956]

solve :: [Integer] -> Int
solve [] = 0
solve x  = length $ filter (\ds -> duplicate ds && successor ds) ps
  where
    ps    = map (\p -> zip p $ tail p) xlist
    xlist = map digits x

solve2 :: [Integer] -> Int
solve2 [] = 0
solve2 x  = length $ filter successor ps
  where
    ps            = map (\p -> zip p $ tail p) xlist
    xlist         = map digits xMinOne2Group
    xMinOne2Group = filter hasTwoGroup x

successor :: [(Int, Int)] -> Bool
successor = all (uncurry (<=))

duplicate :: [(Int, Int)] -> Bool
duplicate = any (uncurry (==))

hasTwoGroup :: Integer -> Bool
hasTwoGroup = elem 2 . map length . group . show

digits :: Integer -> [Int]
digits = map digitToInt . show
