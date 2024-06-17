combinacoes 0 _ = [[]]
combinacoes d l = [ (a:bs) | (a:aa) <- tails l, bs <- combinacoes (d-1) aa ]