module Main where

salutation = "hello"
person = "George"

makeGreeting salutation person = salutation <> " " <> person

greetPerson = makeGreeting "Hello"



extendedGreeting person = 
    let joinWithNewlines a b = a <> "\n" <> b 
        helloAndGoodbye hello goodbye = 
            let hello' = makeGreeting hello person
                goodbye' = makeGreeting goodbye person
            in joinWithNewlines hello' goodbye'
        in joinWithNewlines "hello" "goodbye"


printSmallNumber num =
    print msg
    where 
        msg = if num < 10 
            then show num
            else "too big"


guardSize num 
    | num < 3 = 
        let size = "positive"
        in exclaim size
    | num < 10 = "that's a medium number"
    | num < 100 = "that's a big number"
    | num < 1000 = "that's a giant number"
    | otherwise = "that's unfathomably big"
    where
        exclaim msg = "that's a " <> msg <> " number!"

-- define my own modulo function first
(%) a b = a `rem` b 

fizzBuzzFor number
    | 0 == number % 15 = "fizzbuzz"
    | 0 == number % 5 = "buzz"
    | 0 == number % 3 = "fizz"
    | otherwise = show number


fizzBuzzCount n = [fizzBuzzFor x | x <- [0..n]]


-- excercises of Chapter 1
factorial n 
    | n <= 1 = n
    | otherwise = n * factorial (n - 1)

fib n 
    | n <= 1 = n
    | otherwise = fib (n-1) + fib (n-2)


uncurriedAddition nums = 
    let 
        a = fst nums
        b = snd nums
    in a + b

-- mimic the curry function
untuple f x y = f (x,y)


-- mimic the uncurry function
tuple f x = f (fst x) (snd x)

main = print $ greetPerson "George"
