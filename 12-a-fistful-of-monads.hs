import Control.Monad
{-
 - Create a type called Validation
 - The type constructor takes one parameter
 - There are two Values: 
 -   Success, which takes that parameter and
 -   Fail String, which represents a failure, and a reason for that failure
 -}
data Validation a = Failure String | Success a deriving (Eq, Show)

-- Make the Validation a Monad
instance Monad Validation where  
    return x = Success x
    Failure x >>= f = Failure x
    Success x >>= f = f x

monadLeftIdentity :: (Monad m, Eq (m y)) => (x -> m y) -> x -> ((m y), (m y), Bool)
monadLeftIdentity f m = (left, right, left == right) where 
	left = return m >>= f
	right = f m

monadRightIdentity :: (Monad m, Eq (m y)) => m y -> ((m y), (m y), Bool)
monadRightIdentity m = (left, right, left == right) where 
	left = m >>= return
	right = m

monadAssociativity :: (Monad m, Eq (m z)) => (x -> m y) -> (y -> m z) -> m x -> ((m z), (m z), Bool)
monadAssociativity f g m = (left, right, left == right) where 
	left = (m >>= f) >>= g
	right = m >>= (\a -> f a >>= g)


{-
 - Create a function, positiveCheck, which takes a number and returns a successful Validation if it's positive, 
 - and a failed Validation with a String message if not.
 -}
positiveCheck :: (Num a, Ord a) => a -> Validation a
positiveCheck x = if (x >= 0) then Success x else Failure "It's not very positive."

{-
 - Create a function, evenCheck, which returns a successful Validation if it's even,
 - and a failed Validation with a string message if it's odd
 -}
evenCheck :: (Integral a) => a -> Validation a
evenCheck x = if ((rem x 2) == 0) then Success x else Failure "It's a little strange looking."

{-
 - Write a function which uses positiveCheck and evenCheck to make sure a number is both positive and even
 -}
positiveAndEvenCheck :: (Num a, Ord a, Integral a) => a -> Validation a
positiveAndEvenCheck = positiveCheck >=> evenCheck
