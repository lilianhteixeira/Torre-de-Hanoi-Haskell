import Data.List
import System.IO 

main :: IO()
main = do
    putStr "\n\n---Torre De Hanoi---\n Escolha um opcao:\n 1 - Jogar Torre De Hanoi\n 2 - IA Resolve\n 3 - Regras do Jogo\n 4 - Sair\n DIGITE:\n => "
    escolha <- getLine
    inicializar $ read escolha



    

inicializar :: Int -> IO()
inicializar opcao = case opcao of
    2 -> iaResolve
    3 -> regras
    4 -> sair
    _ -> main


regras :: IO()
regras = do
    putStrLn  "\n\n -- Torre de Hanoi: --\n   O objetivo deste jogo, consiste em deslocar todos os discos da primeira haste para a ultima haste.\n   Respeitando as seguintes regras:\n\n     1 - Deslocar um disco de cada vez, o qual devera ser o do topo de uma das tres hastes.\n\n     2 - Cada disco nunca podera ser colocado sobre outro de tamanho menor.\n\n"
    main


iaResolve :: IO()
iaResolve = do
    putStrLn "\nDigite o numero de discos(Maior ou igual a 3):\n "
    numeroDeDiscos <- getLine
    avaliandoNumeroDeDiscos . getNumeroDeDiscos $ (read numeroDeDiscos)
    putStrLn (show numeroDeDiscos)
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
    putStrLn "\nOK"

