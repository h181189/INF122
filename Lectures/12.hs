-- E -> + E E | * E E | Int
data Ast = A Ast Ast | M Ast Ast | V Int

parseE :: [String] -> Ast
parseE ("+":xs) = let   (e1, r1) = parseE xs ;
                        (e2, r2) = parseE r1 in (A e1 e2, r2)

parseE ("*":xs)  let    (e1, r1) = parseE xs ;
                        (e2, r2) = parseE r1 in (M e1 e2, r2)

parseE (x:xs)   if (onlyDigits x) let (V(read x :: Int), xs)
                else error "Syntax error: " ++ x

onlyDigits x = takeWhile isDigit x == x
