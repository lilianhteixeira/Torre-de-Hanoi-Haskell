{-constTorre :: Integer -> [Integer] -> [Integer] -> [Integer] -> t
constTorre nDiscos esquerda meio direita = do
    constLinha nDiscos (head esquerda) (head meio) (head direita)
    constLinha nDiscos (tail esquerda) (tail meio) (tail direita)
-}

constLinhas :: Integer -> [Integer] -> [Integer] -> [Integer] -> IO()
constLinhas nDiscos [] [] [] = putStrLn("")
constLinhas nDiscos [x] [y] [z] = do
    putStrLn((constColuna nDiscos x) ++ (constColuna nDiscos y) ++ (constColuna nDiscos z))
constLinhas nDiscos esq meio dir = do
    putStrLn((constColuna nDiscos (last esq)) ++ (constColuna nDiscos (last meio)) ++ (constColuna nDiscos (last dir)))
    constLinhas nDiscos (init esq) (init meio) (init dir)

insereQTD :: [Integer] -> String -> String
insereQTD [] y = ""
insereQTD [x] y = y
insereQTD (x:xs) y = y ++ insereQTD xs y

constColuna :: Integer -> Integer -> [Char]
constColuna nDiscos disco = do
    if disco /= 0
    then  insereQTD [0 .. ((nDiscos - disco) - 1)] " " ++ "(" ++ insereQTD [0 .. ((disco * 2) - 1)] "_" ++ ")" ++ insereQTD [0 .. ((nDiscos - disco) - 1)] " "
    else  insereQTD [0 .. (nDiscos - 1)] " " ++ "||" ++ insereQTD [0 .. (nDiscos - 1)] " "
