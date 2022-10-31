import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction (x:y:ys) "/" = (y / x):ys
            foldingFunction (y:ys) "neg1" = (-y):ys
            foldingFunction (y:ys) "raiz2" = (sqrt y):ys
            foldingFunction (y:ys) "condnumero" = (tellingNumber(y)):ys
            foldingFunction (y:ys) "raiz" = (sqrt y):ys
            foldingFunction xs "sum" = [sum xs]
            foldingFunction xs "product" = [product xs]
            foldingFunction xs "promedio"= [sum xs / fromIntegral (length xs)]
            
            foldingFunction xs numberString = read numberString:xs

tellingNumber :: Float  -> Float
tellingNumber 3 = 100
tellingNumber 5 =25
tellingNumber otherwise = 0
