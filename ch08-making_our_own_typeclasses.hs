import qualified Data.Map as Map

data Shape1
  = Circle1 Float
            Float
            Float
  | Rectangle1 Float
               Float
               Float
               Float
  deriving (Show)

-- (above is first version)
data Point =
  Point Float
        Float
  deriving (Show)

data Shape
  = Circle Point
           Float
  | Rectangle Point
              Point
  deriving (Show)

-- the above type is a nice one for holding a shape! or, well, just
-- two different kinds of shapes
surface :: Shape1 -> Float
surface (Circle1 _ _ r) = pi * r ^ 2
surface (Rectangle1 x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

surface2 :: Shape -> Float
surface2 (Circle _ r) = pi * r ^ 2
surface2 (Rectangle (Point x1 y1) (Point x2 y2)) =
  (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
  Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

data Person =
  Person String
         String
         Int
         Float
         String
         String
  deriving (Show)

-- the above is way yucky!
data Person2 = Person2
  { firstName :: String
  , lastName :: String
  , age :: Int
  , height :: Float
  , phoneNumber :: String
  , flavor :: String
  } deriving (Show)

-- this sort of type syntax is called record syntax
data Car = Car
  { company :: String
  , model :: String
  , year :: Int
  } deriving (Show)

-- this car class shows us the utility of using this record syntax for certain kinds of types
displayCar :: Car -> String
displayCar (Car {company = c
                ,model = m
                ,year = y}) =
  "It's a " ++ c ++ " " ++ m ++ " from " ++ show y ++ "!"

data Vector a =
  Vector a
         a
         a
  deriving (Show)

vplus
  :: (Num t)
  => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vectMult
  :: (Num t)
  => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i * m) (j * m) (k * m)

scalarMult
  :: (Num t)
  => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i * l + j * m + k * n

data Person3 = Person3
  { firstName3 :: String
  , lastName3 :: String
  , age3 :: Int
  } deriving (Eq, Show, Read)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- we had a phonebook earlier, with type phoneBook :: [(String,String)]
-- we can define that like:
type PhoneNumber = String

type Name = String

type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook =
  [ ("betty", "555-2938")
  , ("bonnie", "452-2928")
  , ("patsy", "493-2928")
  , ("lucille", "205-2928")
  , ("wendy", "939-8282")
  , ("penny", "853-2492")
  ]

-- we can use this to make the type declarations for our datatype much easier to read
-- this also increases the readability of functions dealing with this data type
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

data LockerState
  = Taken
  | Free
  deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) ->
      if state /= Taken
        then Right code
        else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers =
  Map.fromList
    [ (100, (Taken, "ZD39I"))
    , (101, (Free, "JAH3I"))
    , (103, (Free, "IQSA9"))
    , (105, (Free, "QOTSA"))
    , (109, (Taken, "893JJ"))
    , (110, (Taken, "99292"))
    ]

-- our own implementation of lists!
infixr 5 :-:

data List a
  = Empty
  | (:-:) a
          (List a)
  deriving (Show, Read, Eq, Ord)

infixr 5 .++

(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

-- implementing a B-tree!
-- no pointers anywhere!
data Tree a
  = EmptyTree
  | Node a
         (Tree a)
         (Tree a)
  deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert
  :: (Ord a)
  => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem
  :: (Ord a)
  => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

-- nice! I think this makes it a bit more clear how functional
-- data structures can work
-- defining typeclasses, and then making types as well
data TrafficLight
  = Red
  | Yellow
  | Green

instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

-- interesting thing: we only have to define how our trafficlight
-- type handles '==' because '/=' is defined in terms of '==' for 
-- the typeclass Eq, so we get that for free!
instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

-- implementing java-style typing
class YesNo a  where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

yesnoif
  :: YesNo a
  => a -> t -> t -> t
yesnoif yesnoVal yesResult noResult =
  if yesno yesnoVal
    then yesResult
    else noResult

-- type-fu
class Tofu t  where
  tofu :: j a -> t a j

data Frank a b = Frank
  { frankField :: b a
  } deriving (Show)

instance Tofu Frank where
  tofu x = Frank x
