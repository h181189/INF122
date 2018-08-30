-- 1
a1 :: String
a1 = ['a', 'b', 'c']

a2 :: (Char, Char, Char)
a2 = ('a', 'b', 'c')

a3 :: [(Bool, Char)]
a3 = [(False, '0'), (True, '1')]

a4 :: ([Bool], String)
a4 = ([False, True], ['0', '1'])

a5 :: [([a] -> [a])]
a5 = [tail, init, reverse]

-- 2
bools :: [Bool]
bools = [True, True, True]

nums :: [[Int]]
nums = [[1, 1, 1]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy a = (a, a)

apply :: (a -> b) -> a -> b
apply f a = f a

-- 3
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- 5
q1 :: Bool
q1 = False

q2 :: Int
q2 = 5 + 8

q3 :: (Int -> Int)
q3 = (+) 2
