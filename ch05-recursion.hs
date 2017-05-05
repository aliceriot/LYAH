-- cool way to define a maximum function!
maximum'
    :: (Ord a)
    => [a] -> a
maximum' [] = error "empty list!"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
  where
    maxTail = maximum' xs

-- haskell takes some getting used to...
-- like, how does it even figure out what to do with this?
--
-- we can also rewrite this as:
maximum2
    :: (Ord a)
    => [a] -> a
maximum2 [] = error "empty you butt!"
maximum2 [x] = x
maximum2 (x:xs) = max x (maximum2 xs)

-- lets use our newfound knowledge of recursion and pattern 
-- matching in haskell to write some functions!
sum'
    :: (Num a)
    => [a] -> a
sum' [] = error "empty list!"
sum' [x] = x
sum' (x:xs) = x + sum xs

-- What are we saying with the type restriction here?
replicate'
    :: (Num i, Ord i)
    => i -> a -> [a]
-- i must be both Num and Ord - note that Ord is NOT a subclass
-- of Num, so we can't just specify that
-- a stands for any class, which means we can replicate anything with this
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n - 1) x

-- type declarations are difficult!
-- take, a function which takes the first n items!
take'
    :: (Num i, Ord i)
    => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

-- remember, with pattern matching you don't have to be totally explicit!
-- eg take' n _ , we can make a condition on that and if it doesn't meet
-- that it won't run into problems
-- this will work as long as you don't use an otherwise
-- the otherwise will always execute if a supplied guard condition is not
-- met
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- haskell は難しいですねえ！
elem'
    :: (Eq a)
    => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys)
    | x == y = True
    | otherwise = elem' x ys

-- sort! I think this is going to be trick
quicksort
    :: (Ord a)
    => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted =
            quicksort
                [ a
                | a <- xs 
                , a <= x ]
        biggerSorted =
            quicksort
                [ a
                | a <- xs 
                , a > x ]
    in smallerSorted ++ [x] ++ biggerSorted
