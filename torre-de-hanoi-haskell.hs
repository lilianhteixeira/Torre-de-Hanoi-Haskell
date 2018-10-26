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
    if opcao == 1 then recebeJogada 3 (inicializaTorres 3) 0
    else if opcao == 2 then recebeJogada 4 (inicializaTorres 4) 0
    else if opcao == 3 then recebeJogada 5 (inicializaTorres 5) 0
    else  do 
        putStrLn "Digite um número válido.\n"
        pause
        escolherDificuldade

vencedor :: Int -> Int -> IO()
vencedor movimentos numeroDeDiscos = do
    clearScreen
    (verificaDesempenho movimentos numeroDeDiscos)
    putStrLn "\nDeseja jogar novamente?\n1 - Sim\n2 - Não"
    opc <- getLine
    let opcao = read opc
    if (opcao) == 1 then main
    else putStrLn "\n    --- Jogo Finalizado ---"

verificaDesempenho :: Int -> Int -> IO()
verificaDesempenho movimentos numeroDeDiscos | movimentos == ideal = putStrLn ("\nParabéns, voce ganhou e sua pontuacao foi excelente " ++ show(movimentos) ++ " de " ++ show(ideal) ++ ".")
                                             | movimentos > ideal && movimentos < (ideal+5) = putStrLn ("\nParabéns, voce ganhou e sua pontuacao foi abaixo da media " ++ show(movimentos) ++ " de " ++ show(ideal) ++ ".\nNão desista e tente novamente! :)")
                                             | otherwise = putStrLn ("\nParabeéns, voce ganhou e sua pontuacao foi ruim " ++ show(movimentos) ++ " de " ++ show(ideal) ++ ".\nNão desista e tente novamente! :)")
                                             where ideal = (2^numeroDeDiscos) - 1



recebeJogada :: Int -> Torres -> Int -> IO()
recebeJogada n torre movimentos = do
    clearScreen
    visualizaTorres torre
    if (ganhouJogo n torre) == True then (vencedor movimentos (numDiscos torre)) 
    else do 
        putStrLn ("\nMovimentos: " ++ (show movimentos))
        putStrLn "\nEscolha a torre de origem (A, B ou C)"
        origem <- getChar
        getLine
        torresSelecionaveis origem
        destino <- getChar
        getLine
        if  not(jogadaValida torre n origem destino == True) then jogadaInvalida n torre movimentos
        else recebeJogada n (moveDisco [origem] [destino] torre) (movimentos+ 1)

torresSelecionaveis :: Char -> IO()
torresSelecionaveis opc | opc == 'a' || opc == 'A' = putStrLn "\nEscolha a torre de destino (B ou C)"
                        | opc == 'b' || opc == 'B' = putStrLn "\nEscolha a torre de destino (A ou C)"
                        | otherwise = putStrLn "\nEscolha a torre de destino (A ou B)"

jogadaInvalida :: Int -> Torres -> Int -> IO()
jogadaInvalida n torre movimentos = do 
    putStrLn "\nJogada inválida\n"
    pause
    recebeJogada n torre movimentos
  
jogadaValida :: Torres -> Int -> Char -> Char -> Bool
jogadaValida torres n origem destino = do
            if (torresValidas origem && torresValidas destino)
               then topo (getTorre [destino] torres) == 0 || (topo(getTorre [origem]  torres  ) < topo(getTorre [destino] torres))
            else False

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
    (visualizaIA (torres) ((resolucaoIA (torres) ('a') ('b') ('c') (numeroDeDiscos))) (0))
    putStrLn "Fim"
    reloadMainWithPause
    else erroNumeroDeDiscos

resolucaoIA :: Torres -> Char -> Char -> Char -> Int -> [Char]
resolucaoIA torres origem intermediario destino discos | discos == 1 = [origem] ++ [destino]
                                                       | otherwise =
                                                       (resolucaoIA torres origem destino intermediario (discos-1))
                                                                 ++ [origem] ++ [destino] ++
                                                       (resolucaoIA torres intermediario origem destino (discos-1))

visualizaIA :: Torres -> [Char] -> Int -> IO()
visualizaIA torres [] movimentos = do
                        if ((numDiscos torres ) `mod` 2) == 0
                            then visualizaTorres ((moveDisco (['b']) (['c']) (torres)))
                        else visualizaTorres ((moveDisco (['a']) (['c']) (torres)))
                        putStrLn ("\nMovimentos: " ++ show(movimentos) ++ "\n")
visualizaIA torres (x:xs) movimentos = do
    visualizaTorres (torres)
    putStrLn ("\nMovimentos: " ++ show(movimentos))
    pause
    visualizaIA (moveDisco ([x]) ([head xs]) (torres)) (tail xs) (movimentos+1)

sair :: IO()
sair = do
    putStrLn "\n    --- Jogo Finalizado ---"


validaEntrada :: Int -> Bool
validaEntrada numeroDeDiscos = 
    if(numeroDeDiscos < 3)
        then False
    else True

torresValidas :: Char -> Bool
torresValidas letra | letra == 'a' || letra == 'A' = True
                    | letra == 'b' || letra == 'B' = True
                    | letra == 'c' || letra == 'C' = True
                    | otherwise                    = False

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