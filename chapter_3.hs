--import Prelude(Eq,Show,Double,String,Int,Num)
import Data.List

data BookInfo = Book Int String [String]
                deriving(Show)
data MagzineInfo =  Magzine Int String [String]
                    deriving(Show)

myInfo = Book 12345678 "real world haskell" ["a","b","c"]

data BookReview = BookReview BookInfo CustomerID String

type CustomerID = Int

type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo,BookReview)

-- data Bool = False | True

type CardHolder = String

type CardNumber = String

type Address = [String]

data BillingInfo    = CreditCard CardNumber CardHolder Address
                    | CashOnDelivery
                    | Invoice CustomerID
                    deriving(Show)

a = ("123","456")

b = ("888","999")

data Catacean   = Catacean String String
                deriving(Show)

data Furniture  = Furniture String String
                deriving(Show)

c = Catacean "123" "456"

d = Furniture "345" "765"

data Cartesian2D    = Cartesian2D Double Double
                    deriving(Eq,Show)

data Polar2D    = Polar2D Double Double
                deriving(Eq,Show)

data Roygbiv    = Red
                | Orange
                | Yellow
                | Green
                | Blue
                | Indigo
                | Voilet
                  deriving(Eq,Show)

type Vector = (Double,Double)

data Shape  = Circle Vector Double
            | Poly [Vector]
              deriving(Show)

myNot True = False
myNot False = True              

sumList (x:xs)  = x + sumList xs
sumList []      = 0

third(a,b,c) =c

complicated(True,a,x:xs,5) = (a,xs)

bookID      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors

nicerID         (Book id _     _      ) = id
nicerTitle      (Book _  title _      ) = title
nicerAuthors    (Book _  _     authors) = authors

badExample (x:xs) = x + badExample xs

goodExample (x:xs)  = x + goodExample xs
goodExample _       = 0

data Customer = Customer {
        customerID      ::  CustomerID
    ,   customerName    ::  String
    ,   customerAddress ::  Address
}   deriving(Show)

customer1 = Customer 271828 "J.R. Hacker"
            ["255 Syntax Ct",
             "Milpitas, CA 95134",
             "USA"]

customer2   = Customer{
        customerID  =   271828
    ,   customerAddress =   [   "1",
                                "2",    
                                "3"]
    ,   customerName    =   "???"
}

someBool = Just True
someString = Just "some"

wrapped = Just (Just "wrapped")

data List a = Cons a (List a)
            | Nil
              deriving(Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

data Tree a     = Node a (Tree a) (Tree a)
                | Empty
                  deriving(Show)

simpleTree  =   Node "parent"   (Node "left child" Empty Empty)
                                (Node "right child" Empty Empty)

toList (Cons x xs)  = (x:toList xs)
toList Nil          = []

data MaybeTree a =  MaybeTree (Maybe (a,(MaybeTree a),(MaybeTree a)))
                    deriving(Show)

mySecond :: [a] -> a
mySecond xs =   if null (tail xs)
                then error "123"
                else head (tail xs)

safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))

tinySecond (_:x:_) = Just x
tinySecond _ = Nothing

lend amount balance =   let reserve = 100
                            newbalance = balance - amount
                        in  if balance < reserve
                            then Nothing
                            else Just newbalance

foo =   let a = 1
        in  let b = 2
            in a+b   

bar =   let x = 1
        in ((let x = "foo" in x),x)

quux a  =   let a = "foo"
            in a ++ "bar"

lend2 amount balance =  if amount < reserve * 0.5
                        then Just newbalance
                        else Nothing
    where   reserve   = 100
            newbalance = balance - amount

pluralise :: String->[Int]->[String]
pluralise word counts = map plural counts
    where   plural 0    = "no "++ word ++ "s"
            plural 1    = "one "++ word
            plural n    = show n ++ " " ++ word ++ "s"

itemName = "Weighted Companion Cube"

fromMaybe defval wrapped = 
    case wrapped of
        Nothing     -> defval
        Just value  -> value

nodesAreSame (Node a _ _)(Node b _ _)
        | a==b  = Just a
nodesAreSame _ _ = Nothing

lend3 amount balance
        | amount <= 0               = Nothing
        | amount > reserve * 0.5    = Nothing
        | otherwise                 = Just newbalance
    where   reserve     = 100
            newbalance  = balance - amount

niceDrop n xs | n <= 0  = xs
niceDrop _ []           = []
niceDrop n (_:xs)       = niceDrop (n-1) xs

myLength :: [a]->Int

myLength []       = 0
myLength (x:xs)   = 1 + myLength(xs)

avg [] = 0
avg xs = (sum xs) / fromIntegral (length (xs))
        where   sum []      = 0
                sum (x:xs)  = x + sum xs

huiwen xs =   xs ++ reverse xs
        where   reserve [] = []
                reserve (x:xs)  = reserve xs ++ x

ishuiwen xs | length xs >= 2   
                =   if head xs == last xs
                    then ishuiwen (init (tail xs))
                    else False
ishuiwen []     = True
ishuiwen (x:[]) = True 



ddsort xs = sortBy sortHelp xs
    where sortHelp a b 
            | length a == length b  = EQ
            | length a > length b   = GT
            | otherwise             = LT  

