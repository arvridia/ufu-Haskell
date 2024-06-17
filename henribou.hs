module Main (main) where

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

main :: IO ()
main = do hSetBuffering stdout NoBuffering -- desabilitando bufferização
          putStr "Banco Henrique Braga Alves Pereira \n"
          putStr "Digite uma opcao: \n"
          putStr "1. Saldo \n"
          putStr "2. Extrato \n"
          putStr "3. Deposito \n"
          putStr "4. Saque \n"
          putStr "5. Fim \n"
          putStr "Escolha uma opcao: "
          opcao <- readLn
          putStrLn ""
          case opcao of
            1 -> do putStrLn "opcao 1" 
                    imprime "saldo.txt"
            2 -> do putStrLn "opcao 2"
                    imprime "extrato.txt"
            3 -> do putStrLn "opcao 3"
                    putStrLn "Insira o valor do deposito: "
                    deposito1 <- readLn
                    deposito deposito1
            4 -> do putStrLn "opcao 4"
                    putStrLn "Insira o valor do saque: "
                    saque1 <- readLn
                    saque saque1
            5 -> putStrLn "opcao 5"
            _ -> putStrLn "opcao invalida, tente novamente"
          if not (opcao == 5) then main else putStrLn "Obrigado por utilizar o banco, estamos sempre a sua disposicao, volte sempre!"

imprime :: String -> IO ()
imprime a = do
      conteudo <- readFile a
      putStrLn conteudo

soma :: Float -> Float -> String
soma a b = show (a + b) 

subtracao :: Float -> Float -> String
subtracao a b = show (b - a)

deposito :: Float -> IO ()
deposito a = do
      conteudo <- readFile "saldo.txt"
      putStrLn ("Saldo anterior"++ conteudo)
      writeFile "saldo.txt" (soma a (read conteudo))
      appendFile "extrato.txt" ("/n" ++ "+" ++ show (a))
      putStrLn "Deposito realizado com sucesso!"

saque :: Float -> IO ()
saque a = do
      conteudo <- readFile "saldo.txt"
      putStrLn ("Saldo anterior" ++ conteudo)
      writeFile "saldo.txt" (subtracao a (read conteudo))
      appendFile "extrato.txt" ("\n" ++ "-" ++ show (a))
      putStrLn "Saque realizado com sucesso!"