10 PRINT "PROGRAM FILE 164: GENERAL USE OF NUMERIC EXPRESSIONS"
20 PRINT "        IN LET-STATEMENT."
30 PRINT "    ANSI STANDARD 6.2, 6.4, 7.2, 7.4, 8.2, 8.4, "
40 PRINT "                  16.2, 16.4"
50 PRINT
60 PRINT "THIS PROGRAM EXERCISES THE FULL RANGE OF USES AND VARIETIES"
70 PRINT "OF NUMERIC EXPRESSIONS IN THE LET-STATEMENT. EXPRESSIONS ARE"
80 PRINT "COMPOSED OF ALL TYPES OF PRIMARIES AND ARE USED AS ARGUMENTS"
90 PRINT "OF FUNCTIONS AND AS SUBSCRIPTS."
100 PRINT
110 LET P=3.14159265
120 PRINT "SECTION 164.1: NUMERIC EXPRESSIONS CONTAINING"
130 PRINT "        SUBSCRIPTED VARIABLES AND FUNCTION REFERENCES."
150 PRINT
160 PRINT "THIS SECTION TESTS WHETHER SUBSCRIPTED VARIABLES AND"
170 PRINT "REFERENCES TO BOTH IMPLEMENTATION-SUPPLIED AND"
180 PRINT "USER-DEFINED FUNCTIONS CAN BE USED AS PRIMARIES IN"
190 PRINT "NUMERIC EXPRESSIONS."
200 GOSUB 7000
270 LET X=10*RND+3/RND-TAN(-.348E-2)
280 LET A=SIN(X)*SIN(X)+COS(X)^(-(-1.3+.5-1.2))
290 LET Y=1
300 GOSUB 9000
310 DIM F(20)
320 REM SET UP FIBONACCI SERIES IN F
330 LET F(1)=1
340 LET F(2)=1
350 FOR I=3 TO 20
360 LET J=I-1
370 LET K=I-2
380 LET F(I)=F(J)+F(K)
390 NEXT I
400 LET A=F(20)/F(19)/((1+SQR(5))/2)
410 LET Y=1
420 GOSUB 9000
430 FOR I=1 TO 7
440 FOR J=1 TO 7
450 LET Q(I,J)=I/J
460 NEXT J
470 NEXT I
480 LET X=P/10
490 LET A=-F(19)/F(20)/(-SIN(X))/Q(2,7)
500 LET Y=7
510 GOSUB 9000
520 REM DEF COMMON LOG
530 DEF FNC(X)=LOG(X)/LOG(10)
540 LET X=P/6
550 LET A=SIN(X)*FNC(1E36)/((F(20)+F(20))/F(19)-1)^FNC(100)*Q(5,6)
560 LET Y=3
570 GOSUB 9000
580 GOSUB 8000
660 PRINT "SECTION 164.2: NUMERIC EXPRESSIONS USED AS FUNCTION"
670 PRINT "        ARGUMENTS AND ARRAY SUBSCRIPTS."
680 PRINT
690 PRINT "THIS SECTION TESTS WHETHER NUMERIC EXPRESSIONS CAN BE"
700 PRINT "USED AS ARGUMENTS TO BOTH IMPLEMENTATION-SUPPLIED AND"
710 PRINT "USER-DEFINED FUNCTIONS AND ALSO AS SUBSCRIPTS."
750 GOSUB 7000
790 REM DEF ARCSIN OF X IN DEGREES
800 DEF FNA(X)=ATN(1/(1/X^2-1)^.5)/P*180
810 LET A=FNA(SQR(3/4))/2/2/3/5
820 LET Y=1
830 GOSUB 9000
840 LET A=Q(1+FNA(Q(F(4),ABS(-6)))/6,INT(SQR(F(+1+2^3))))
850 LET Y=1.2
860 GOSUB 9000
870 LET X=RND
880 LET D=P/180
890 LET F(F(F(6))*Q(2,7)-TAN(P/4))=TAN(ATN(SIN(D*FNA(LOG(EXP(X))))))
900 LET A=F(5)
910 LET Y=X
920 GOSUB 9000
930 LET F(5)=5
1000 GOSUB 8000
1010 PRINT "SECTION 164.3: EXPRESSIONS USED AS SUBSCRIPTS ARE"
1015 PRINT "        ROUNDED TO NEAREST INTEGER."
1020 PRINT
1030 PRINT "THIS SECTION ENSURES THAT WHEN EXPRESSIONS ARE USED AS"
1040 PRINT "SUBSCRIPTS, THEY ARE ROUNDED TO THE NEAREST INTEGER."
1060 GOSUB 7000
1080 DIM V(5,11)
1090 FOR I=0 TO 5
1100 FOR J=0 TO 11
1110 LET V(I,J)=12*I+J
1120 NEXT J
1130 NEXT I
1160 LET A=V(FNC(316)-.1,FNC(ABS(-316))+10E-2)
1170 LET Y=27
1180 GOSUB 9000
1190 LET A=V(Q(3,7),Q(4,7))
1200 LET Y=1
1210 GOSUB 9000
1220 LET A=V(FNC(1E-3)+2.51,13*7/9+1)
1230 LET Y=11
1240 GOSUB 9000
1250 LET A=1+V(LOG(1.0)-.4,.6-EXP(0))
1260 LET Y=1
1270 GOSUB 9000
1280 LET A=V(F(5)+.49,F(7)-Q(5,3))
1290 LET Y=71
1300 GOSUB 9000
1700 GOSUB 8000
6000 PRINT "END PROGRAM 164"
6010 STOP
7000 REM SUBROUTINE TO BEGIN SECTION
7010 PRINT
7020 PRINT "                 BEGIN TEST."
7030 PRINT
7040 PRINT "CASE #","SHOULD BE","ACTUAL","OUTCOME"
7050 PRINT
7060 LET E1=0
7070 LET C1=0
7080 RETURN
8000 REM SUBROUTINE TO END SECTION
8010 PRINT
8020 IF E1=0 THEN 8050
8030 PRINT "*** TEST FAILED IN ";E1;" CASE(S) ***"
8040 GOTO 8060
8050 PRINT "*** TEST PASSED ***"
8060 PRINT
8070 PRINT "                  END TEST."
8080 PRINT
8090 RETURN
9000 REM HANDLE REPORT
9010 REM THIS SUBROUTINE ADOPTS A VERY LOOSE CRITERION OF CORRECTNESS
9020 REM (RELATIVE ERROR < .001). ITS PURPOSE ISN'T TO MEASURE ACCURACY
9030 REM BUT ONLY TO ASSURE THAT THE SEMANTICS OF THE EXPRESSION
9040 REM HAVE BEEN CORRECTLY IMPLEMENTED.
9050 LET C1=C1+1
9060 PRINT C1,Y,A,
9070 LET P$="FAIL"
9080 LET M=0.001
9090 LET T=(A-Y)/Y
9100 IF T>M THEN 9150
9110 LET T=-T
9120 IF T>M THEN 9150
9130 LET P$=" OK "
9140 GOTO 9160
9150 LET E1=E1+1
9160 PRINT P$
9170 RETURN
9180 END
