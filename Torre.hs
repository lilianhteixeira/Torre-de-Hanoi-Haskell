module Torre where

data Torres = Torres {
    torreA :: [Int],
    torreB :: [Int],
    torreC :: [Int],
    numDiscos :: Int
} deriving (Show, Read)

insereDisco :: String -> Torres -> Int -> Torres
insereDisco letra torre disco = atualizaTorre (letra) (torre) (push torreLetra (disco) (topo torreLetra))
            where torreLetra  = getTorre letra torre

push :: [Int] -> Int -> Int -> [Int]
push [] _ _= []
push (x:xs) disco ultimoDisco | x == 0 && ( ultimoDisco == 0 || disco < ultimoDisco) = [disco] ++ xs
                              | otherwise                     = x : (push xs disco ultimoDisco)

removeDisco :: String -> Torres -> [Int]
removeDisco letra torre = pop (torreLetra) (topo torreLetra)
                    where torreLetra = getTorre letra torre

pop :: [Int] -> Int -> [Int]
pop [] _ = []
pop (x:xs) ultimoDisco | x /= 0 && x == ultimoDisco = 0 : xs
                   | otherwise = x : (pop xs ultimoDisco)

getTorre :: String -> Torres -> [Int]
getTorre letra torres | letra == "a" || letra == "A" = torreA torres
                      | letra == "b" || letra == "B" = torreB torres
                      | otherwise                    = torreC torres

tamanhoTorre :: [Int] -> Int
tamanhoTorre []                 = 0 
tamanhoTorre (x:xs) | x /= 0    =  1 + tamanhoTorre xs
                    | otherwise = 0

topo :: [Int] -> Int
topo torre | indice >= 0 = torre !!indice
           | otherwise = torre !!0
            where indice = (tamanhoTorre torre) - 1

ehVazia :: [Int] -> Bool
ehVazia (x : xs) = x == 0
ehVazia _ = False

ehCheia :: [Int] -> Int -> Bool
ehCheia torre numeroDeDiscos = ((tamanhoTorre torre) == numeroDeDiscos)

inicializaTorres :: Int -> Torres
inicializaTorres numeroDeDiscos= Torres (preencheTorre numeroDeDiscos) (criaTorreVazia numeroDeDiscos) (criaTorreVazia numeroDeDiscos) numeroDeDiscos

preencheTorre :: Int -> [Int]
preencheTorre numeroDeDiscos = [numeroDeDiscos,(numeroDeDiscos-1)..1] ++ [0]

criaTorreVazia :: Int -> [Int]
criaTorreVazia numeroDeDiscos = replicate (numeroDeDiscos+1) 0

moveDisco :: String -> String -> Torres -> Torres
moveDisco origem destino torres = atualizaTorre destino primeiraAtualizacao torreDestino
                where discoRemovido = topo(getTorre origem torres)
                      torreOrigem = pop(getTorre origem torres) (discoRemovido)
                      torreDestino = push(getTorre destino torres) (discoRemovido) (topo(getTorre destino torres))
                      primeiraAtualizacao = atualizaTorre origem torres torreOrigem

atualizaTorre :: String -> Torres -> [Int] -> Torres
atualizaTorre letra torres torreAtualizada | letra == "a" || letra == "A" = Torres (torreAtualizada)
                                                                                   (torreB torres)
                                                                                   (torreC torres)
                                                                                   (numDiscos torres)

                                           | letra == "b" || letra == "B" = Torres (torreA torres)
                                                                                   (torreAtualizada)
                                                                                   (torreC torres)
                                                                                   (numDiscos torres)

                                           | otherwise                    = Torres (torreA torres)
                                                                                   (torreB torres)
                                                                                   (torreAtualizada)
                                                                                   (numDiscos torres)

