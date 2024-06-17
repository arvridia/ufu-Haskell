import System.IO
type FilePath = String

oi = do conteudo <- readFile "saque.txt" 
        putStrLn conteudo