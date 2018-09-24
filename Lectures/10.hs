test, test2 :: Int -> Int
test x = x
test2 x = x^2

type Intpair = (Int, Int) -- often used to create shorter alternatives to types
newtype Nat = N Int

data Direction = Ne | Se | Sw | Nw deriving Show
data BinaryTreeInt = Leaf Int | Node Int Int

test3 :: Direction -> String
test3 d = show d
