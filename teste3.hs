import System.IO
type FilePath = String

oi :: Float -> IO ()
oi f = writeFile "saque.txt" (show f)