inverteCase :: Char -> Char
inverteCase c = if isUpper' then toLower'
                           else toUpper'
                where isUpper'
                       | c == 'A' = True
                       | c == 'B' = True
                       | c == 'C' = True
                       | c == 'D' = True
                       | c == 'E' = True
                       | c == 'F' = True
                       | c == 'G' = True
                       | c == 'H' = True
                       | c == 'I' = True
                       | c == 'J' = True
                       | c == 'K' = True
                       | c == 'L' = True
                       | c == 'M' = True
                       | c == 'N' = True
                       | c == 'O' = True
                       | c == 'P' = True
                       | c == 'Q' = True
                       | c == 'R' = True
                       | c == 'S' = True
                       | c == 'T' = True
                       | c == 'U' = True
                       | c == 'V' = True
                       | c == 'W' = True
                       | c == 'X' = True
                       | c == 'Y' = True
                       | c == 'Z' = True
                       | otherwise = False
                      toLower' 
                       | c == 'A' = 'a'
                       | c == 'B' = 'b'
                       | c == 'C' = 'c'
                       | c == 'D' = 'd'
                       | c == 'E' = 'e'
                       | c == 'F' = 'f'
                       | c == 'G' = 'g'
                       | c == 'H' = 'h'
                       | c == 'I' = 'i'
                       | c == 'J' = 'j'
                       | c == 'K' = 'k'
                       | c == 'L' = 'l'
                       | c == 'M' = 'm'
                       | c == 'N' = 'n'
                       | c == 'O' = 'o'
                       | c == 'P' = 'p'
                       | c == 'Q' = 'q'
                       | c == 'R' = 'r'
                       | c == 'S' = 's'
                       | c == 'T' = 't'
                       | c == 'U' = 'u'
                       | c == 'V' = 'v'
                       | c == 'W' = 'w'
                       | c == 'X' = 'x'
                       | c == 'Y' = 'y'
                       | c == 'Z' = 'z'
                       | otherwise = c
                      toUpper' 
                       | c == 'a' = 'A'
                       | c == 'b' = 'B'
                       | c == 'c' = 'C'
                       | c == 'd' = 'D'
                       | c == 'e' = 'E'
                       | c == 'f' = 'F'
                       | c == 'g' = 'G'
                       | c == 'h' = 'H'
                       | c == 'i' = 'I'
                       | c == 'j' = 'J'
                       | c == 'k' = 'K'
                       | c == 'l' = 'L'
                       | c == 'm' = 'M'
                       | c == 'n' = 'N'
                       | c == 'o' = 'O'
                       | c == 'p' = 'P'
                       | c == 'q' = 'Q'
                       | c == 'r' = 'R'
                       | c == 's' = 'S'
                       | c == 't' = 'T'
                       | c == 'u' = 'U'
                       | c == 'v' = 'V'
                       | c == 'w' = 'W'
                       | c == 'x' = 'X'
                       | c == 'y' = 'Y'
                       | c == 'z' = 'Z'
                       | otherwise = c