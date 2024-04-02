

reverseFold lst = foldl (\x y -> y : x) [] lst



--helper a [] = a : []
--helper a b = b : a

--reverseFoldr lst = foldr helper (head lst) (tail lst)


-- (1:(2:(3:[])))
-- foldr replaces each : with (f x)

-- create a custom foldr function (helper)
-- if last element -> wrap it in a list
-- else -> cons without wrapping..


-- implement zipWith, with and without list comprehension
-- zipW == zipWith without list comprehension
zipW f lst1 lst2  
    | lst1 == [] = []
    | lst2 == [] = []
    | otherwise = 
        let a = head lst1
            b = head lst2
        in (f a b) : zipW f (tail lst1) (tail lst2)


-- zipW' is zipWith using list comprehension
zipW' f lst1 lst2 = [f a b | (a, b) <- zip lst1 lst2] 


-- implement concatMap using foldr/foldl
cMap lst = foldr1 (\x y -> x <> y) lst
cMap' lst = foldl1 (\x y -> x <>y) lst


