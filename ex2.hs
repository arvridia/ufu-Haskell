import Data.char

acronimo x = [a | a <- x, isUpper a]