import Control.Applicative
import Data.Monoid

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Show)

-- Make the list a Functor
instance Functor List where  
    fmap f (Value x tail) = Value (f x) (fmap f tail)
    fmap f Empty = Empty

-- Write a function which appends one list on to another
combineLists :: List a -> List a -> List a
combineLists Empty b = b
combineLists (Value head tail) b = Value head (combineLists tail b)

-- Make our list a Monoid
instance Monoid (List a) where  
    mempty = Empty  
    mappend first second = combineLists first second

-- Make our list an Applicative
instance Applicative List where  
    pure a = Value a Empty
    Empty <*> _ = Empty
    _ <*> Empty = Empty  
    Value head tail <*> g = combineLists (fmap head g) (tail <*> g)

-- Make sure that the List obeys the laws for Applicative and Monoid

-- Create some lists of numbers of different lengths such as:
twoValueList = Value 10 $ Value 20 Empty

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+2)
plusTwo <$> twoValueList 

-- Use <$> and <*> on the lists with a binary function

-- Create some lists of binary functions

-- Use <*> on the binary functions list and the number lists
