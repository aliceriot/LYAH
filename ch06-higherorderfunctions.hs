-- learning about currying!
multThree
    :: (Num a)
    => a -> a -> a -> a
multThree x y z = x * y * z

-- basically we can call a function with fewer than it's full set of 
-- parameters, and this is essentially another function
-- so we could do something like
--
-- let multTwowithNine = multThree 9
--
-- cool!
compareWithHundred
    :: (Num a, Ord a)
    => a -> Ordering
compareWithHundred x = compare 100 x

-- cool
-- can also do with infix notation, by sectioning:
divideByTen
    :: (Floating a)
    => a -> a
divideByTen = (/ 10)

-- this will return a function which divides a number by ten
isUpper :: Char -> Bool
isUpper = (`elem` ['A' .. 'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where
    g x y = f y x

-- the above can be written more simply as
flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

-- a new way to do the quicksort! filter and map are very much
-- like list comprehension, and when you want to use one over the others
-- depends on things like code readability and so on
quicksort
    :: (Ord a)
    => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<= x) xs)
        biggerSorted = quicksort (filter (> x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

-- quick little function which finds the biggest number
-- divisible by 3829 less that 100000
biggestNum
    :: (Integral a)
    => a
biggestNum =
    head
        [ x
        | x <- [100000,99999 ..] 
        , mod x 3829 == 0 ]

-- or with filter
biggestNum2
    :: (Integral a)
    => a
biggestNum2 = head (filter p [100000,99999 ..])
  where
    p x = mod x 3829 == 0

-- sum of all odd squares <10000
sumSquaresSmaller
    :: (Integral a)
    => a
sumSquaresSmaller = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))

-- collatz sequence
collatzChain
    :: (Integral a)
    => a -> [a]
collatzChain 1 = [1]
collatzChain x
    | even x = x : collatzChain (div x 2)
    | odd x = x : collatzChain (1 + (x * 3))

-- find all collatz chaings for [1..100] which are longer than 15
collatzCount :: Int
collatzCount =
    length
        [ x
        | x <- map collatzChain [1 .. 100] 
        , length x > 15 ]

-- book's way
collatzCount2 :: Int
collatzCount2 = length (filter isLong (map collatzChain [1 .. 100]))
  where
    isLong xs = length xs > 15

-- lambda! we write this in haskell using \
numLongChains :: Int
numLongChains =
    length (filter (\xs -> length xs > 15) (map collatzChain [1 .. 100]))

-- we make a function in our filter predicate using lambda above
-- we can do cool things with it like
-- zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
-- folding is something cool for working with lists!
sum'
    :: (Num a)
    => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- here you should read acc as accumulator
elem'
    :: (Eq a)
    => a -> [a] -> Bool
elem' x xs =
    foldl
        (\acc y ->
              if y == x
                  then True
                  else acc)
        False
        xs

-- fold is pretty cool! there are both right and left folds, which 
-- basically describe which direction the fold takes when it's chopping
-- up the list
map2 :: (a -> b) -> [a] -> [b]
map2 f xs = foldr (\x acc -> f x : acc) [] xs

-- or with a left fold!
map3 :: (a -> b) -> [a] -> [b]
map3 f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- apparently left fold is less suitable here, b/c the ++ operation is
-- more expensive than using the : list syntax
-- dunno why
-- also only right folds work on infinite lists!
-- folds are useful! here are versions of a bunch of standard library functions
-- implemented with folds
maximum'
    :: (Ord a)
    => [a] -> a
maximum' =
    foldr1
        (\x acc ->
              if x > acc
                  then x
                  else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product'
    :: (Num a)
    => [a] -> a
product' = foldr1 (*)

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p =
    foldr
        (\x acc ->
              if p x
                  then x : acc
                  else acc)
        []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- how many elements does it take for the sum of their roots to exceed 1000?
sqrtSum :: Int
-- sqrtSum = length [x | x <- scanl1 (+) (map sqrt [1..]), x <1000]
sqrtSum =
    length
        [ x
        | x <- (takeWhile (< 1000) (scanl1 (+) (map sqrt [1 ..]))) ] +
    1

-- sqrtSum = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
-- I'm not sure I exactly understand what the difference between the version with
-- takeWhile and the version using a condition on a list comprehension, but the 
-- list comprehension one hangs at the biggest sum below 1000, whereas the takeWhile
-- just exits cleanly. ???
-- takeWhile is made to work properly with infinite lists!
--
--
-- $ operator! super handy
-- makes sum (map sqrt [1..131])
-- equivalent to
-- sum $ map sqrt [1..131]
-- basically syntactic sugar for the parentheses 
-- dot notation for composition! can define equivalently:
fn1 x = ceiling (negate (tan (cos (max 50 x))))

fn = ceiling . negate . tan . cos . max 50

-- the above functions work the same way!
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (< 10000) (filter odd $ map (^ 2) [1 ..]))

-- or, with dot composition
oddSquaresum2 :: Integer
oddSquaresum2 = sum . takeWhile (< 10000) . filter odd . map (^ 2) $ [1 ..]

-- or, more readably,
oddSquareSum3 :: Integer
oddSquareSum3 =
    let oddSquares = filter odd $ map (^ 2) [1 ..]
        belowLimit = takeWhile (< 10000) oddSquares
    in sum belowLimit
-- I think the second one is the best combination of readability and succinctness, but w/e
