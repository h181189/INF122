data AST = Add AST AST | Mult AST AST | Dig Int

parse :: String -> AST

parseExpr :: String -> (AST, String)

parseTerm :: String -> (AST, String)
parseTerm str = case parseFactor str of
    (a1, '*':r1) ->
        case parseTerm rest1 of
            (a2, rest2) -> (Mult a1 a2, rest2)
            pair -> pair

            -- let (a2, rest2) = parseTerm rest1
            -- in (Mult a1 a2, rest2)

parseFactor :: String -> (AST, String)
