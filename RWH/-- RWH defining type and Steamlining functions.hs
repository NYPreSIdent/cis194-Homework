-- RWH Defining Types and Steamlining Functions

data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

myInfo = Book 98132917438432 "Algebra of programming"
         ["Richard Bird", "Oege de Moor"]

type CustomerID = Int
type ReviewBody = String
data BookReview = BookReview BookInfo CustomerID String

type BookRecord = (BookInfo, BookReview)

data Bool = False | True

type CardHolder = String
type CardNumber = String
type Address    = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

-- Using tuple to represent BillingInfo
type BookInfoT = (Int, String, [String])

-- x and y coordinates or lengths
data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show)

data Polar2D = Polar2D Double Double
               deriving (Eq, Show)

-- emun type
data Roygbiv = Red 
             | Orange
             | Yellow
             | Green
             | Blue
             | Indigo
             | Violet
               deriving (Eq, Show)

-- like C union
type Vector = (Double, Double)

data Shape = Circle Vector Double
           | Poly [Vector]

-- pattern matching: about taking apart a value by finding out which constructor 
-- it was bulit with.
sumList (x:xs) = x + sumList xs
sumList []     = 0 -- ordering is important.

third (a, b ,c) = c

complicated (True, a, x:xs, 5) = (a, xs)

-- wild card
nicierID     (Book id _     _      ) = id
nicerTitle   (Book _  title _      ) = title
nicerAuthors (Book _  _     authors) = authors

-- Exhaustive patterns and wild cards
badExample (x : xs) = x + badExample xs -- This would cause a bug: badExample []

goodExample (x : xs) = x + goodExample xs
goodExample _        = 0

-- Record Syntax
data Customer = Customer {
	 customerID      :: CustomerID
   , customerName    :: String
   , customerAddress :: Address
   } deriving (Show)

customer1 = Customer {
	  customerID      = 287432
	, customerAddress = ["233412", "Disc", "USA"]
	, customerName    = "Jane Q. Citizen"
}

-- CanlendarTime is a good example of Record Syntax.

-- Recursive type: defined in terms of itself.
data List a = cons a (List a)
            | Nil
              deriving (Show)
