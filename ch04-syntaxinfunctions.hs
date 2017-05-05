-- pattern matching
--
-- a pretty nifty way to define a function
-- sort of 'piece-wise' or whatever they called it in high school
lucky
    :: (Integral a)
    => a -> String
lucky 7 = "LUCKY NUMBER 7!"
lucky x = "Sorry, nope"

sayMe :: Int -> String -- so you don't need to use type variable here
sayMe 1 = "One!"
sayMe x = "Not one..."

factorial
    :: (Integral a)
    => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors
    :: (Num a)
    => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- functions for a three members tuple (triple)
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, x, _) = x

third :: (a, b, c) -> c
third (_, _, x) = x

-- our own version of head (car)
head' :: [a] -> a
head' [] = error "Empty list!"
head' (x:_) = x

tell
    :: (Show a)
    => [a] -> String
tell [] = "Empty list!"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "Two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) =
    "Long list! First two elements are: " ++ show x ++ " and " ++ show y

-- our own version of length!
length'
    :: (Num b)
    => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- nice recursion! this syntax is sooo much better than scheme
sum'
    :: (Num a)
    => [a] -> a
sum' [] = 0
sum' (n:rest) = n + sum' rest

-- patterns! a nice way to keep a specific 'match' pattern around
captial :: String -> String
capital "" = error "Empty String!"

captial all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- BMI berating function (introduces guards)
bmiTell
    :: (RealFloat a)
    => a -> String
bmiTell bmi
    | bmi <= 18.5 = "a"
    | bmi <= 25.0 = "b"
    | bmi <= 30.0 = "c"
    | otherwise = "d"

max'
    :: (Ord a)
    => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

-- something that might be handy to know
myCompare
    :: (Ord a)
    => a -> a -> Ordering
a `myCompare` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

-- it's possible to write a function definition using the backtick 
-- notation, I don't see myself using this
-- rewrite of the BMITELL
bmiTell2
    :: (RealFloat a)
    => a -> a -> String
bmiTell2 weight height
    | bmi <= 18.5 = "a"
    | bmi <= 25.0 = "b"
    | bmi <= 30.0 = "c"
    | otherwise = "huh?"
  where
    bmi = weight / height ^ 2

-- could also do
bmiTell3
    :: (RealFloat a)
    => a -> a -> String
bmiTell3 weight height
    | bmi <= skinny = "a"
    | bmi <= normal = "b"
    | bmi <= fat = "c"
    | otherwise = "huh?"
  where
    bmi = weight / height ^ 2
    skinny = 18.5
    normal = 25.0
    fat = 30.0

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f:_) = firstname
    (l:_) = lastname

calcBmis
    :: (RealFloat a)
    => [(a, a)] -> [a]
calcBmis xs =
    [ bmi w h
    | (w, h) <- xs ]
  where
    bmi weight height = weight / height ^ 2

-- let binding, similar to the where binding
-- apparently a common way to write haskell code is to use where for
-- any helper functions a function might need
-- (cleans up the main body of the function defnition?)
cylinder
    :: (RealFloat a)
    => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

-- let is cool! like letrec in scheme
calcBmis2
    :: (RealFloat a)
    => [(a, a)] -> [a]
calcBmis2 xs =
    [ bmi
    | (w, h) <- xs 
    , let bmi = w / h ^ 2 
    , bmi >= 25 ]

-- we can put let in a list comprehension!
-- nifty! hehehehe
-- pattern matching in function definitions is basically just
-- syntactic sugar for the haskell case syntax
-- we can rewrite
head' :: [a] -> a
head' [] = error "empty list..."
head' (x:_) = x

-- as
head2' :: [a] -> a
head2' xs =
    case xs of
        [] -> error "empty list..."
        (x:_) -> x
-- I think this is pretty cool!
