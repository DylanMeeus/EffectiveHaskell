

main :: IO()
main = putStrLn $ example [5..15]

mapApply :: [a -> b] -> [a] -> [b]
mapApply toApply = concatMap (\i -> map ($ i) toApply) 

example :: [Int] -> String
example i = map(\x -> lookupLetter x) $ mapApply offsets i
    where
        letters :: [Char]
        letters = ['a'..'z']

        lookupLetter :: Int -> Char
        lookupLetter n = letters !! n

        offsets :: [Int -> Int]
        offsets = [rot13, swap10, mixupVowels]

        rot13 :: Int -> Int
        rot13 n = (n + 13)  `rem` 26

        swap10 :: Int -> Int
        swap10 n 
            | n <= 10 = n + 10
            | n <= 20 = n - 10
            | otherwise = n

        mixupVowels :: Int -> Int
        mixupVowels n = 
            case n of 
                0 -> 8
                4 -> 14
                8 -> 20
                14 -> 0
                20 -> 4
                n' -> n'
