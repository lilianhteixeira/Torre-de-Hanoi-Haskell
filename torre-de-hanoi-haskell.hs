import Data.List
import System.IO
import Prelude
import Torre

main :: IO()
main = do
    clearScreen
    putStr "\n\n---Torre De Hanoi---\n Escolha um opcao:\n 1 - Jogar Torre De Hanoi\n 2 - IA Resolve\n 3 - Regras do Jogo\n 4 - Sair\n DIGITE:\n"
    escolha <- getLine
    inicializar $ read escolha

reloadMainWithPause :: IO()
reloadMainWithPause = do
    pause
    main

inicializar :: Int -> IO()
inicializar opcao = case opcao of 
   1 -> escolherDificuldade
   2 -> iaResolve
   3 -> regras
   4 -> sair
   _ -> validaMain
    
    
escolherDificuldade :: IO()
escolherDificuldade = do
    clearScreen
    putStrLn "1 - Fácil\n2 - Médio\n3 - Dificil"
    opc <- getLine
    let opcao = read opc
    if opcao == 1 then recebeJogada 3 (inicializaTorres 3)
    else if opcao == 2 then recebeJogada 4 (inicializaTorres 4)
    else if opcao == 3 then recebeJogada 5 (inicializaTorres 5)
    else  do 
        putStrLn "Digite um número válido.\n"
        pause
        escolherDificuldade

vencedor :: IO()
vencedor = do
    clearScreen
    putStrLn "Parabéns você ganhou !!!"
    putStrLn "\nDeseja jogar novamente?\n1 - Sim\n2 - Não"
    opc <- getLine
    let opcao = read opc
    if (opcao) == 1 then main
    else putStrLn "\n    --- Jogo Finalizado ---"

recebeJogada :: Int -> Torres -> IO()
recebeJogada n torre = do
    clearScreen
    visualizaTorres torre
    if (ganhouJogo n torre) == True then vencedor 
    else do 
        putStrLn "\nEscolha a torre de origem (A, B ou C)"
        origem <- getChar
        getLine
        torresSelecionaveis origem
        destino <- getChar
        getLine
        if  not(jogadaValida torre n origem destino == True) then jogadaInvalida n torre 
        else recebeJogada n (moveDisco [origem] [destino] torre)

torresSelecionaveis :: Char -> IO()
torresSelecionaveis opc | opc == 'a' || opc == 'A' = putStrLn "\nEscolha a torre de destino (B ou C)"
                        | opc == 'b' || opc == 'B' = putStrLn "\nEscolha a torre de destino (A ou C)"
                        | otherwise = putStrLn "\nEscolha a torre de destino (A ou B)"

jogadaInvalida :: Int -> Torres -> IO()
jogadaInvalida n torre = do 
    putStrLn "\nJogada inválida\n" 
    recebeJogada n torre 
  
jogadaValida :: Torres -> Int -> Char -> Char -> Bool
jogadaValida torres n origem destino = topo (getTorre [destino] torres) == 0 || (topo(getTorre [origem]  torres  ) < topo(getTorre [destino] torres))

ganhouJogo :: Int -> Torres -> Bool
ganhouJogo n torres = (getTorre "C" torres ) == [n,n-1..0]

pause :: IO()
pause = do 
    putStrLn "Pressione qualquer tecla para continuar..."
    nada <- getLine
    putStrLn ""

regras :: IO()
regras = do
    clearScreen
    putStrLn  "\n\n -- Torre de Hanoi: --\n   O objetivo deste jogo, consiste em deslocar todos os discos da primeira haste para a ultima haste.\n   Respeitando as seguintes regras:\n\n     1 - Deslocar um disco de cada vez, o qual devera ser o do topo de uma das tres hastes.\n\n     2 - Cada disco nunca podera ser colocado sobre outro de tamanho menor.\n\n"
    reloadMainWithPause

iaResolve :: IO()
iaResolve = do
    clearScreen
    putStrLn "\nDigite o numero de discos(Maior ou igual a 3):\n "
    linha <- getLine
    let numeroDeDiscos = read linha :: Int
    if (validaEntrada numeroDeDiscos) then do
    let torres = inicializaTorres numeroDeDiscos
    (visualizaIA (torres) ((resolucaoIA (torres) ('a') ('b') ('c') (numeroDeDiscos))))
    putStrLn "Fim"
    reloadMainWithPause
    else erroNumeroDeDiscos

resolucaoIA :: Torres -> Char -> Char -> Char -> Int -> [Char]
resolucaoIA torres origem intermediario destino discos | discos == 1 = [origem] ++ [destino]
                                                       | otherwise =
                                                       (resolucaoIA torres origem destino intermediario (discos-1))
                                                                 ++ [origem] ++ [destino] ++
                                                       (resolucaoIA torres intermediario origem destino (discos-1))

visualizaIA :: Torres -> [Char] -> IO()
visualizaIA torres [] = if ((numDiscos torres ) `mod` 2) == 0
                        then visualizaTorres ((moveDisco (['b']) (['c']) (torres)))
                        else visualizaTorres ((moveDisco (['a']) (['c']) (torres)))
visualizaIA torres (x:xs) = do
    visualizaTorres (torres)
    pause
    visualizaIA (moveDisco ([x]) ([head xs]) (torres)) (tail xs)

sair :: IO()
sair = do
    putStrLn "\n    --- Jogo Finalizado ---"


validaEntrada :: Int -> Bool
validaEntrada numeroDeDiscos = 
    if(numeroDeDiscos < 3)
        then False
    else True

erroNumeroDeDiscos :: IO()
erroNumeroDeDiscos = do
    putStrLn "\nError - Numero De Discos Menor Que 3"
    pause
    iaResolve


validaMain :: IO()
validaMain = do
    putStrLn "\nOpção Inválida!"
    pause
    main