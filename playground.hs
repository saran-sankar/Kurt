inc :: Integer -> Integer
inc n = n + 1

leng :: [a] -> Integer
leng [] = 0
leng (xs:t) = 1 + leng t

m :: Integer
m = inc 3

data Color = Red | Blue

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

class Functor f where fmap :: (a -> b) -> (f a -> f b)

data Array a = Nil | Cons a (Array a)

array :: Array Integer
array = Nil

max :: Integer -> Integer -> Integer
max n m = if m>n then m else n

xor True 1 = False
xor False 0 = False
xor x y = True

max_of_2_and = Main.max 2

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

class Bifunctor f where
  bimap :: (a -> a') -> (b -> b') -> (f a b -> f a' b')

instance Bifunctor (,) where
  bimap g h ((,) a b) = (,) (g a) (h b)

square n = n^2
answer = bimap square square (1,2)

main :: IO()
main = print()
