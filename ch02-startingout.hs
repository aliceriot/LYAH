import Prelude

--starting LYAH
doubleMe x = x + x

doubleUs x y = x * 2 + y * 2

--doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x =
    if x > 100
        then x
        else x * 2

-- we have something like cons
consit x y = x : y

boomBangs xs =
    [ if x < 10
         then "Boom!"
         else "Bang!"
    | x <- xs 
    , odd x ]

fizzBuzz
    :: (Integral a, Show a)
    => a -> [String]
fizzBuzz n =
    [ fizz n
    | n <- [1 .. n] ]
  where
    fizz n
        | ((mod n 5) == 0) && ((mod n 3) == 0) = "fizzbuzz"
        | ((mod n 3) == 0) = "fizz"
        | ((mod n 5) == 0) = "buzz"
        | otherwise = show n

cartesian :: [a] -> [a] -> [[a]]
cartesian [] _ = []
cartesian _ [] = []
cartesian xs ys =
    [ [x, y]
    | x <- xs 
    , y <- ys ]

-- list comprehension is really cool in haskell!
-- better syntax and more powah compared with python, I think
-- we can use it to define length
length' xs =
    sum
        [ 1
        | _ <- xs ]

removeNonUppercase string =
    [ c
    | c <- string 
    , elem c ['A' .. 'Z'] ]

removeOdds biglist =
    [ [ x
      | x <- lat 
      , even x ]
    | lat <- biglist ]

triangles :: Integer -> [(Integer, Integer, Integer)]
triangles n =
    [ (a, b, c)
    | c <- [1 .. n] 
    , b <- [1 .. n] 
    , a <- [1 .. n] 
    , right a b c ]
  where
    right a b c = a ^ 2 + b ^ 2 == c ^ 2
