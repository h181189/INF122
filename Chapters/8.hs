-- 5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- 6
eval :: Expr -> Int
eval e = folde id (+) e

size :: Expr -> Int
size e = folde (\x -> 1) (+) e
