import Test.QuickCheck

-- 1
    -- A
plu :: [Int] -> Int -> [Int]
plu k n = map (+n) k

pali :: String -> Bool
pali x = x == reverse x

pluCheck :: [Int] -> Int -> Bool
pluCheck [] n = [] == plu []     n
pluCheck xs n = map (+n) xs == plu xs n

paliCheck :: String -> Bool
paliCheck xs = if palindrome then pali xs else not (pali xs)
                where
                    palindrome = xs == reverse xs

    -- B 
-- Chapter/3.hs

    -- C
c1 :: Bool
c1 = False

c2 :: Int
c2 = 5 + 8

c3 :: Int -> Int
c3 = (+) 2

c4 :: Int -> Int
c4 = (+2)

c5 :: Int -> Int
c5 = (2+)

c6 :: ([String], Char)
c6 = (["foo", "bar"], 'a')

c7 :: [(Bool, [String])]
c7 = [(True, []), (False, ["a"])]

c8 :: Int -> [a] -> a
c8 = \x y -> y !! x

c9 :: [Int -> [a] -> [a]]
c9 = [take, drop, \x y -> [y !! x]]

    -- D
d1 :: [Bool]
d1 = [False, True, False]

d2 :: [[Int]]
d2 = [[1, 2], [3, 4]]

d3 :: [([Char], Int)]
d3 = [("a", 7)]

d4 :: [(Char, Int)]
d4 = [('a', 7)]

d5 :: Num a => a -> a
d5 x = x * 2

d6 :: (a, b) -> a
d6 (x, y) = x

d7 :: a -> (a, a)
d7 x = (x, x)

    -- E
e1 :: [a] -> a
e1 xs = head xs
e1' :: [a] -> a
e1' xs = last xs

e2 :: [a] -> Int
e2 xs = length xs

    -- F
f1 :: a -> b -> (a, b)
f1 a b = (a, b)

f2 :: a -> b -> (a, b)
f2 a = \b -> (a, b)

f3 :: a -> b -> (a, b)
f3 = \a b -> (a, b)

f4 :: a -> b -> (a, b)
f4 = \a -> \b -> (a, b)

f5 :: a -> b -> (b, a)
f5 = \a -> \b -> (b, a)

f6 :: a -> b -> (a, b)
f6 = \b -> \a -> (b, a)

-- G
f :: Int -> Int -> Int
f x y = x + y

g :: (Int, Int) -> Int
g xy = fst xy + snd xy

fg :: Int -> Int -> Bool
fg x y = f x y == g (x, y)
