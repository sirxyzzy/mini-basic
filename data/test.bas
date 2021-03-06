5   REM SIEVE OF ERATOSTHENES, BY ERATOSTHENES OF CYERENE, 276 BC - 194 BC
6   DATA "Hello sailor!", PIBBLE, 3.12345, 33
7 GOTO 8
10  PRINT "FIND PRIMES FROM 2 TO N (N <= 1000). ENTER N: "
20  INPUT N
30  IF N<2 THEN 10
40  IF N>1000 THEN 10
50  DIM A(1000)
60  LET S=SQR(N)
70  FOR I=2 TO S
80    IF A(I)=1 THEN 130
90    LET D=N/I
100   FOR J=I TO D
110     LET A(I*J)=1
120   NEXT J
130 NEXT I
140 FOR I=2 TO N
150   IF A(I)=1 THEN 170
160   PRINT I
170 NEXT I
171 DIM A(22), B(10,10)
172 DEF FNA = Q + R
173 DEF FNB(I) = I * I
174 GOSUB 20
175 GOTO 20
176 ON I + 1 GOTO 100, 110, 120
177 OPTION BASE 0
178 OPTION BASE 1
179 READ A, A$, C(1,2), D(X+3), B
999 END