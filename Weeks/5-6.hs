-- 3
data Exp = S | U | Og Exp Exp | El Exp Exp | Ik Exp

folde :: a -> a -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> Exp -> a
folde s u o e i S = s
folde s u o e i U = u
folde s u o e i (Og x y) = o (folde s u o e i x) (folde s u o e i y)
folde s u o e i (El x y) = e (folde s u o e i x) (folde s u o e i y)
folde s u o e i (Ik x) = i (folde s u o e i x)

evb :: Exp -> Bool
evb exp = folde True False (&&) (||) (not) exp
