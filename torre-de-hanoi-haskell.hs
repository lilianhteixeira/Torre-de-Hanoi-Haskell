import Data.List
import System.IO
import System.Process
import System.Info
import Torre

clearScreen = if (System.Info.os) == "mingw32" then system "cls" else system "clear"

main :: IO()
main = do
    putStr "\n\n---Torre De Hanoi---\n Escolha um opcao:\n 1 - Jogar Torre De Hanoi\n 2 - IA Resolve\n 3 - Regras do Jogo\n 4 - Sair\n DIGITE:\n => "
    escolha <- getLine
    inicializar $ read escolha



    

inicializar :: Int -> IO()
inicializar opcao = case opcao of 
   1 -> escolherDificuldade
   2 -> iaResolve
   3 -> regras
   4 -> sair
   _ -> main
    
    
escolherDificuldade :: IO()
escolherDificuldade = do
	putStrLn "1 - Fácil \n 2 - Médio \n 3 - Dificil"
	opc <- getLine
	let opcao = read opc
	if opcao == 1 then recebeJogada 3 (inicializaTorres 3)
	else if opcao == 2 then recebeJogada 4 (inicializaTorres 4)
	else if opcao == 3 then recebeJogada 5 (inicializaTorres 5)
	else  putStrLn "Digite um número válido."
	
			
recebeJogada :: Int -> Torres -> IO()
recebeJogada n torre = do
	visualizaTorres torre
	if (ganhouJogo n torre) == True then putStrLn "Parabéns você ganhou !!!" 
	else do 
		putStrLn " Escolha a torre de origem "
		origem <- getChar
		putStrLn " Escolha a torre de destino "
		destino <- getChar
		if  not(jogadaValida torre n origem destino == True) then jogadaInvalida n torre 
		else recebeJogada n (moveDisco [origem] [destino] torre)
	
jogadaInvalida :: Int -> Torres -> IO()
jogadaInvalida n torre = do 
	putStrLn "Jogada inválida" 
	recebeJogada n torre 
  
jogadaValida :: Torres -> Int -> Char -> Char -> Bool
jogadaValida torres n origem destino = topo (getTorre [destino] torres) == 0 || (topo(getTorre [origem]  torres  ) < topo(getTorre [destino] torres))

ganhouJogo :: Int -> Torres -> Bool
ganhouJogo n torres = (getTorre "C" torres ) == [n,n-1..0]
		

jogada :: Torres -> IO ()
jogada torres = do
    clearScreen
    putStrLn "\tEstado atual"
    visualizaTorres (torres)
    putStrLn "\nTorre de Origem: "
    origem <- getLine
    putStrLn "\nTorre de Destino: "
    destino <- getLine
    jogada (moveDisco origem destino torres)


pause :: IO()
pause = do 
    putStrLn "Pressione qualquer tecla para continuar..."
    nada <- getLine
    putStrLn ""

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
	let torre = inicializaTorres numeroDeDiscos
	let torreb = jogadaAutomatica numeroDeDiscos torre 'a' 'b' 'c'
	putStrLn "fim"
    
jogadaAutomatica :: Int -> Torres -> Char -> Char -> Char -> Torres
jogadaAutomatica n torre origem meio destino = do
	visualizaTorres torre
	if (ganhouJogo n torre) == True then torre
	else do 
		if(n == 1)
			then casoBase torre origem meio destino
		else
		 casoGenerico n torre origem meio destino
		 
casoGenerico :: Int -> Torres -> Char -> Char -> Char -> Torres
casoGenerico n torre origem meio destino = do
			let torre1 = jogadaAutomatica (n-1) torre origem destino meio
			let torre2 = moveDisco [origem] [destino] torre1
			let torre3 = jogadaAutomatica (n-1) torre meio origem destino
			return torre3

casoBase :: Torres -> Char -> Char -> Char -> Torres
casoBase torre origem meio destino = moveDisco [origem] [destino] torre

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
    
constroiTorre :: [Int] -> [Int] -> [Int] -> String
constroiTorre esquerda meio direita = (show esquerda) ++ "\n" ++ (show meio) ++ "\n" ++ (show direita) ++ "\n"

corrigeHaste :: [Int] -> [Int] -> [Int] -> [Int]
corrigeHaste [] zeros (x:xs) = [0] ++ zeros ++ xs
corrigeHaste (x:xs) zeros discos = 
    if(x == 0)
        then corrigeHaste xs (zeros ++ [x]) discos
        else corrigeHaste xs zeros (discos ++ [x])
        
