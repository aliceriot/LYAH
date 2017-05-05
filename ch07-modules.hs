import Data.List
import qualified Data.Map as M
import Data.Char
import Data.Function
import qualified Data.Set as Set
import Geometry

-- we can use something found in the module above to get the number 
-- of unique elements in a list
numUniques
    :: (Eq a)
    => [a] -> Int
numUniques = length . nub

-- lots of handy builtins in the Data.List module
-- and it turns out that these are all imported automatically as part of the prelude
-- sum of all third powers under 10000
thirdUnder :: Integer
thirdUnder = sum $ takeWhile (< 10000) $ map (^ 3) [1 ..]

search
    :: (Eq a)
    => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in foldl
           (\acc x ->
                 if take nlen x == needle
                     then True
                     else acc)
           False
           (tails haystack)

joinLists :: [a] -> [a] -> [a]
joinLists [] [] = []
joinLists x [] = x
joinLists [] x = x
joinLists (x:xs) (y:ys) = x : y : joinLists xs ys

-- Caesar cipher
encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in map chr shifted

-- or, a bit more succinctly
encode2 :: Int -> String -> String
encode2 shift msg = map chr $ map (+ shift) $ map ord msg

unencode :: Int -> String -> String
unencode shift msg = encode (negate shift) msg

-- dictionaries/association lists
phoneBook =
    [ ("ben", "555-5555")
    , ("ben", "554-8555")
    , ("bill", "567-4324")
    , ("patsy", "567-9887")
    , ("patsy", "543-4545")
    ]

findkey
    :: (Eq a)
    => a -> [(a, b)] -> b
findkey key dic = snd . head . filter (\(k, v) -> k == key) $ dic

-- a safer version
findkey2
    :: (Eq a)
    => a -> [(a, b)] -> Maybe b
findkey2 _ [] = Nothing
findkey2 key ((k, v):xs) =
    if key == k
        then Just v
        else findkey2 key xs

findkey3
    :: (Eq a)
    => a -> [(a, b)] -> Maybe b
findkey3 key =
    foldr
        (\(k, v) acc ->
              if key == k
                  then Just v
                  else acc)
        Nothing

-- our version of fromList
fromList'
    :: (Ord k)
    => [(k, v)] -> M.Map k v
fromList' = foldr (\(k, v) acc -> M.insert k v acc) M.empty

phoneBookToMap
    :: (Ord k)
    => [(k, String)] -> M.Map k String
phoneBookToMap xs =
    M.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"

text2 =
    "The old man left his garbage can out and now his trash is all over my lawn!"
