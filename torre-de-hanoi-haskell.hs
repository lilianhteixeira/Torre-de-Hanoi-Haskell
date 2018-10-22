import Data.List
import System.IO 

main :: IO()
main = do
    putStr "\n\n---Torre De Hanoi---\n Escolha um opcao:\n 1 - Jogar Torre De Hanoi\n 2 - IA Resolve\n 3 - Regras do Jogo\n 4 - Sair\n DIGITE:\n => "
    escolha <- getLine
    inicializar $ read escolha



    

inicializar :: Int -> IO()
inicializar opcao = case opcao of
    1 -> jogarTorre
    2 -> iaResolve
    3 -> regras
    4 -> sair
    _ -> main

jogarTorre :: IO()
jogarTorre = do
    putStrLn "fim"



regras :: IO()
regras = do
    putStrLn  "\n\n -- Torre de Hanoi: --\n   O objetivo deste jogo, consiste em deslocar todos os discos da primeira haste para a ultima haste.\n   Respeitando as seguintes regras:\n\n     1 - Deslocar um disco de cada vez, o qual devera ser o do topo de uma das tres hastes.\n\n     2 - Cada disco nunca podera ser colocado sobre outro de tamanho menor.\n\n"
    main


iaResolve :: IO()
iaResolve = do
    putStrLn "\nDigite o numero de discos(Maior ou igual a 3):\n "
    linha <- getLine
    let numeroDeDiscos = read linha :: Int
    avaliandoNumeroDeDiscos . getNumeroDeDiscos $ numeroDeDiscos
    let esquerda = [1..numeroDeDiscos]
    let meio = criarListaVazia $  numeroDeDiscos 
    let direita = criarListaVazia $  numeroDeDiscos
    putStrLn (constroiTorre esquerda meio direita)
    let discoRemovido = removeDisco esquerda
    let opsEsquerda = corrigeHaste esquerda [] []
    let novoMeio = inserirDisco meio [] discoRemovido  []
    putStrLn (constroiTorre opsEsquerda novoMeio direita)

    putStrLn "fim"


sair :: IO()
sair = do
    putStrLn "\n    --- Terminando o Programa ---"


getNumeroDeDiscos :: Int -> Bool
getNumeroDeDiscos numeroDeDiscos = 
    if(numeroDeDiscos < 3)
        then False
        else True

avaliandoNumeroDeDiscos :: Bool -> IO()
avaliandoNumeroDeDiscos False = erroNumeroDeDiscos
avaliandoNumeroDeDiscos True = continueNumeroDeDiscos

erroNumeroDeDiscos :: IO()
erroNumeroDeDiscos = do
    putStrLn "\nError - Numero De Discos Menor Que 3"
    iaResolve

continueNumeroDeDiscos :: IO()
continueNumeroDeDiscos = do
    putStrLn "\n"   

criarLista :: Int -> [Int]
criarLista numeroDeDiscos = [1..numeroDeDiscos]

criarListaVazia :: Int -> [Int]
criarListaVazia numeroDeDiscos = replicate numeroDeDiscos 0
    
constroiTorre :: [Int] -> [Int] -> [Int] -> String
constroiTorre esquerda meio direita = (show esquerda) ++ "\n" ++ (show meio) ++ "\n" ++ (show direita) ++ "\n"

inserirDisco :: [Int]-> [Int] -> Int -> [Int] -> [Int] 
inserirDisco [] (x:xs) elemento discos =  (xs) ++ [elemento] ++ discos
inserirDisco (x:xs) zeros elemento discos = 
    if(x == 0)
        then inserirDisco xs (zeros ++ [x]) elemento discos
        else inserirDisco xs zeros elemento ([x] ++ discos)


removeDisco :: [Int] -> Int
removeDisco (x:xs) = 
    if(x /= 0)
        then x
        else removeDisco xs

corrigeHaste :: [Int] -> [Int] -> [Int] -> [Int]
corrigeHaste [] zeros (x:xs) = [0] ++ zeros ++ xs
corrigeHaste (x:xs) zeros discos = 
    if(x == 0)
        then corrigeHaste xs (zeros ++ [x]) discos
        else corrigeHaste xs zeros (discos ++ [x])
