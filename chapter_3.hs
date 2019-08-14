--import Prelude(Eq,Show,Double,String,Int,Num)


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