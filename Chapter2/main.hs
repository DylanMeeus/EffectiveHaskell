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



checkGuestList guestList name = elem name guestList

foodCosts = [("Ren", 10.00), ("George", 4.00), ("Porter", 27.50)]

partyBudget isAttending = foldr1 (+) . map snd . filter (isAttending . fst)


-- map the list to the foodCosts
-- sum for the second value

partyBudget' attendees =  foldr1 (+) $ map snd $ filter (\x -> elem (fst x) attendees) foodCosts


pairs as bs = 
    let as' = filter (`elem` bs) as
        bs' = filter odd bs
        mkPairs a = map (\b -> (a,b)) bs'
    in map mkPairs as'


combineLists as bs 
    | [] == as = []
    | [] == bs = []
    | otherwise = (head as, head bs) : combineLists (tail as) (tail bs)


pairwiseSum xs ys =
    let sumElems pairs = 
            let a = fst pairs
                b = snd pairs
            in a + b
    in map sumElems $ zip xs ys


pairwiseSum' xs ys 
    | [] == xs = []
    | [] == ys = []
    | otherwise = (head xs + head ys) : pairwiseSum' (tail xs) (tail ys)
