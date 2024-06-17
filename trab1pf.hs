module Main (main) where

import System.IO
type FilePath = String


imprime :: String -> IO ()
imprime s = do conteudo <- readFile s
               putStrLn "==============="
               putStrLn conteudo
               putStrLn "===============" 
           
--conteudo <- readFile "saldo.txt"
--writeFile "saldo.txt"
--Exception: saldo.txt: withFile: resource busy (file is locked)
--talvez por causa do lazy evaluation dessa funcao "readFile"
--entao utilizei a funcao readFile', que le completamente antes de retornar
           
deposito :: Float -> IO ()
deposito f = do conteudo <- readFile' "saldo.txt"
                writeFile "saldo.txt" (show((read conteudo) + f))
                appendFile "extrato.txt" ("\n"++"+"++(show f)) 
                

saque :: Float -> IO ()
saque f = do conteudo <- readFile' "saldo.txt"
             writeFile "saldo.txt" (show((read conteudo) - f))
             appendFile "extrato.txt" ("\n"++"-"++(show f))

main = do hSetBuffering stdout NoBuffering
          putStrLn " "
          putStrLn "=============================="
          putStrLn "Banco Arthur Resende Santos"
          putStrLn "=============================="
          putStrLn "Opções:"
          putStrLn "1. Saldo"
          putStrLn "2. Extrato"
          putStrLn "3. Depósito"
          putStrLn "4. Saque"
          putStrLn "5. Fim"
          putStrLn "Escolha uma opção:"
          op <- readLn
          putStrLn " "
          case op of
               1 -> do putStrLn "Seu saldo atual eh: "
                       imprime "saldo.txt"
                       putStrLn " "
                       putStrLn "Escolha uma nova opcao ou finalize o programa"
                       main
               2 -> do putStrLn "Extrato: "
                       imprime "extrato.txt"
                       putStrLn " "
                       putStrLn "Escolha uma nova opcao ou finalize o programa"
                       main
               3 -> do putStrLn "Digite o valor que gostaria de inserir na sua conta: "
                       dep <- getLine
                       deposito (read dep :: Float)
                       putStrLn "Valor depositado com sucesso!"
                       putStrLn " "
                       putStrLn "Escolha uma nova opcao ou finalize o programa"
                       main
               4 -> do putStrLn "Digite o valor que gostaria de sacar da sua conta: "
                       saq <- getLine
                       saque (read saq :: Float)
                       putStrLn "Saque efetuado com sucesso!"
                       putStrLn " "
                       putStrLn "Escolha uma nova opcao ou finalize o programa"
                       main
               5 -> do putStrLn " "
                       putStrLn "Obrigado por utilizar nosso banco!"
                       
               _ -> do putStrLn " "
                       putStrLn "Escolha invalida, por favor digite um numero de um a cinco!"
                       main
          
          