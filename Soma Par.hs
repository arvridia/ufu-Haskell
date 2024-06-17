soma_par(A,0) :- A < 10,A mod 2 =\= 0,!.
soma_par(A,A) :- A < 10,A mod 2 =:= 0,!.
soma_par(A,B) :- X is A mod 10,
                      X mod 2 =:= 0,
                      C is A//10,soma_par(C,BB),
                      B is CC + X,!;
    				  C is A//10,soma_par(C,B).