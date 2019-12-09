main :: IO ()
main = do
    input <- readFile "1.txt"
    print $ solve input fuelcalc
    print $ solve input fuelcalcrec

solve :: String -> (Int -> Int) -> Int
solve s f = sum . map (f . readString) . words $ s

fuelcalc :: Int -> Int
fuelcalc x = x `div` 3 - 2

fuelcalcrec :: Int -> Int
fuelcalcrec x
 | x < 7 = 0
 | otherwise = fuelcalc x + fuelcalcrec (fuelcalc x)

readString :: String -> Int
readString s = read s :: Int