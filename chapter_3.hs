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

data Bool = False | True

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

