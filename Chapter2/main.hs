module Main where


(%) x y = rem x y 

factors num = 
    factors' num 2
    where
        factors' num fact
            | num == 1 = []
            | num % fact == 0 = fact : factors'  (num `div` fact) fact
            | otherwise = factors' num (fact + 1)


main = putStrLn "hello world"





isBalanced s = 
    0 == isBalanced' 0 s
        where 
            isBalanced' count s 
                | null s = count
                | head s == "(" = isBalanced' (count + 1) (tail s)
                | head s == ")" = isBalanced' (count - 1) (tail s)
                | otherwise = isBalanced' count (tail s)


--doubler :: [Int] -> [Int]
myMap f l
    | [] == l = []
    | otherwise = f (head l) : myMap f (tail l)
