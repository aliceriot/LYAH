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
