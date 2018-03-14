module Task3 where

import Data.List
import Data.Maybe
import Data.List.NonEmpty (NonEmpty (..), fromList, (<|))

-- task1 days
data Day = MON | TUE | WED | THU | FRI | SAT | SUN
    deriving (Eq, Enum, Show)

day :: [Day]
day = [MON, TUE, WED, THU, FRI, SAT, SUN]

nextDay :: Day -> Day
nextDay d 
    | d == SUN = MON
    | otherwise = succ d

afterDays :: Day -> Int -> Day
afterDays d n = iterate nextDay d !! (mod n 7)

isWeekend :: Day -> Bool
isWeekend SAT = True
isWeekend SUN = True
isWeekend _   = False

daysToParty :: Day -> Int
daysToParty FRI = 0
daysToParty d   = 1 + (daysToParty $ nextDay d)

-- task 2 castles

newtype Citizen = Citizen
    { cName :: String} deriving (Eq, Show)

data Bible = Library | Church deriving (Show)

data Family = One | Two | Three | Four
    deriving (Enum, Show)

newtype House = House Family deriving (Show)

newtype Walls = Walls
    { wLen :: Int} deriving (Show)

newtype Castle = Castle
    { cLord :: Maybe Citizen} deriving (Show)

data Protection = Protection
    { pCastle :: Castle
    , pWalls  :: Maybe Walls
    } deriving (Show)

data City = City
    { cProtection :: Maybe Protection
    , cBible      :: Maybe Bible
    , cHouses     :: NonEmpty House
    } deriving (Show)

countPeople :: City -> Int
countPeople (City _ _ houses) = foldl (\x (House family) -> x + fromEnum family + 1) 0 houses

buildCastle :: City -> Castle -> (City, Bool)
buildCastle (City Nothing bible houses) castle = (City
                                                    (Just
                                                        (Protection castle Nothing))
                                                    bible houses,
                                                 True)
buildCastle x _                                = (x, False)

buildChurchOrLibrary :: City -> Bible -> (City, Bool)
buildChurchOrLibrary (City protection Nothing houses) bible =
                                    (City protection (Just bible) houses, True)
buildChurchOrLibrary x _                                    = (x, False)

buildNewHouse :: City -> Family -> City
buildNewHouse (City protection bible houses) family = City protection bible (House family<|houses)

data NewLordError = NoCastle | HaveAlready deriving (Eq, Show)

meetNewLord :: City -> Citizen -> (City, Maybe NewLordError)
meetNewLord city@(City Nothing _ _) _  = (city, Just NoCastle)
meetNewLord city@(City (Just (Protection (Castle lord) walls)) bible houses) newLord
            | isNothing lord =
                  (City
                     (Just (Protection
                            (Castle (Just newLord)) walls))
                     bible houses,
                   Nothing)
            | otherwise = (city, Just HaveAlready)

buildWalls :: City -> Walls -> (City, Bool)
buildWalls city@(City (Just (Protection (Castle (Just lord)) Nothing)) bible houses) walls
            | countPeople city >= 10 =
                     (City
                        (Just (Protection
                                    (Castle (Just lord)) (Just walls)))
                        bible houses,
                     True)
            | otherwise              = (city, False)
buildWalls city _ = (city, False)



-- task 3 nat
-- lite version
data Nat = Z | S Nat
    deriving (Show, Eq)

natFromInt :: Int -> Nat
natFromInt 0 = Z
natFromInt n = S (natFromInt (n - 1))

intFromNat :: Nat -> Int
intFromNat Z     = 0
intFromNat (S n) = 1 + (intFromNat n)

add :: Nat -> Nat -> Nat
add Z b     = b
add a Z     = a
add a (S b) = S (add a b)

sub :: Nat -> Nat -> Nat
sub a Z         = a
sub (S a) (S b) = sub a b

mul :: Nat -> Nat -> Nat
mul a Z     = Z
mul Z b     = Z
mul a (S b) = add a (mul a b)

instance Ord Nat where
    compare Z Z         = EQ
    compare (S _) Z     = GT
    compare Z (S _)     = LT
    compare (S a) (S b) = compare a b

-- hard version

isPrime :: Nat -> Bool
isPrime Z         = True
isPrime (S Z)     = False
isPrime (S (S a)) = isPrime a

divNat :: Nat -> Nat -> Nat
divNat _ Z      = error "Division by Zero"
divNat Z _      = Z
divNat a b
    | a == b    = (S Z)
    | a < b     = Z
    | otherwise = (S (divNat (sub a b) b))

modDivNat :: Nat -> Nat -> Nat
modDivNat _ Z = error "Division by Zero"
modDivNat Z a = a
modDivNat a b = sub a (mul (divNat a b) b)

-- task4 search tree

data Tree a = Nil | Node [a] (Tree a) (Tree a) deriving (Eq, Show)

empty :: Tree a -> Bool
empty Nil = True
empty _   = False

size :: (Ord a) => Tree a -> Int
size (Node list left right) = size left + length list + size right
size Nil                    = 0

contains :: (Ord a) => Tree a -> a -> Bool
contains Nil _ = False
contains (Node (x:_) left right) v
                             | x > v = contains left v
                             | x < v = contains right v
                             | x == v = True
contains (Node [] _ _) _ = False

insert :: (Ord a) => Tree a -> a -> Tree a
insert Nil x = Node [x] Nil Nil
insert (Node list@(x:_) left right) v
                             | x > v = Node list (Task3.insert left v) right
                             | x < v = Node list left (Task3.insert right v)
                             | x == v = Node (v:list) left right

remove :: (Ord a) => Tree a -> a -> Tree a
remove Nil _ = Nil
remove (Node list@(x:xs) left right) v
                             | x > v = Node list (remove left v) right
                             | x < v = Node list left (remove right v)
                             | x == v && length list > 1 = Node xs left right
                             | x == v && right == Nil && left == Nil = Nil
                             | x == v && right == Nil = left
                             | x == v = Node list2 left new_right
                                            where
                                              (list2, new_right) = delete_left right
                                              delete_left :: (Ord a) => Tree a -> ([a], Tree a)
                                              delete_left (Node list Nil right) = (list, right)
                                              delete_left (Node l left r) =
                                                let (ret, new_left) = delete_left left in
                                                    (ret, Node l new_left r)

fromList :: (Ord a) => [a] -> Tree a
fromList = foldl Task3.insert Nil

-- tests

city1 = City (Just
              (Protection
                 (Castle Nothing) Nothing))
              (Just Library)
              (House Two :| [House Four, House Four])

city2 = City Nothing
             Nothing
             (House One :| [])

testCity :: Bool
testCity = testCity1 && testCity2
         where
           testCity1 = countPeople city1 == 10 &&
                       not (snd $ buildCastle city1 (Castle Nothing)) &&
                       not (snd $ buildChurchOrLibrary city1 Library) &&
                       let newCity1 = fst $ meetNewLord city1 (Citizen "abacaba") in
                       snd (buildWalls newCity1 (Walls 10))
           testCity2 = countPeople city2 == 1 &&
                       (snd (meetNewLord city2 (Citizen "abacaba")) == Just NoCastle) &&
                       not (snd $ buildWalls city2 (Walls 10)) &&
                       let newCity1 = fst $ buildCastle city2 (Castle Nothing)
                           newCity2 = fst $ meetNewLord newCity1 (Citizen "abacaba")
                           newCity3 = buildNewHouse (
                                            buildNewHouse (
                                                buildNewHouse newCity2 Four
                                                ) Four
                                            ) Four
                       in
                       snd $ buildWalls newCity3 (Walls 10)

testNat :: Bool
testNat = intFromNat ((mul (natFromInt 10) (natFromInt 5) `add` divNat (natFromInt 8) (natFromInt 2) `sub` natFromInt 4)) == intFromNat (natFromInt 50)

testTree :: Bool
testTree = let tree = Task3.fromList [5, 3, 1, 4, 2, 7, 6, 9, 8]
           in
           contains tree 4 && contains tree 9 &&
           contains (Task3.insert tree 15) 15 &&
           not (contains (remove tree 4) 4)

testTask3 :: [Bool]
testTask3 = [testCity, testNat, testTree]

checkTask3 :: [Int]
checkTask3 = map fst $ filter (not . snd) $ zip [1..] testTask3


