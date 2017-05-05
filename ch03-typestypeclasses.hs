-- we like to worry about types, because it makes our code more explicit
-- and less likely to be incorrect
-- functions can have types as well!
removeNonUppercase :: String -> String
removeNonUppercase st =
    [ c
    | c <- st 
    , c `elem` ['A' .. 'Z'] ]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1 .. n]

circumference :: Float -> Float
circumference r = 2 * pi * r
-- lovely stuff!
-- examining the type signatures of functions with :t is super helpful!
-- really a cool way to think about computation
-- I like static typing
-- python can leave
