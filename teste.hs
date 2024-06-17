import System.IO
type FilePath = String

oi :: Float -> IO ()
oi f = appendFile "saque.txt" (show f)
