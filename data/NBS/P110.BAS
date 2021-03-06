5 PRINT "PROGRAM FILE 110: MIXED INPUT OF STRINGS AND NUMBERS."
10 PRINT "    ANSI STANDARD 13.2, 13.4"
20 PRINT
25 PRINT "SECTION 110.1: MIXED INPUT OF STRINGS AND NUMBERS."
30 PRINT
45 PRINT "PLEASE ENTER YOUR INPUT-REPLIES EXACTLY AS REQUESTED"
50 PRINT "BY THE PROMPT, INCLUDING ALL SPACING, COMMAS AND OTHER"
55 PRINT "PUNCTUATION."
60 PRINT
65 PRINT " - EXCEPT - WHEREVER THE PROMPT CONTAINS AN EQUALS (=)"
70 PRINT "SUBSTITUTE A BLANK SPACE, AND WHEREVER THE PROMPT"
75 PRINT "CONTAINS A NUMBER-SIGN (#) SUBSTITUTE A QUOTE."
80 PRINT "THUS, THE PROMPT:"
85 PRINT "             ==#ER#=="
90 PRINT "SHOULD CAUSE YOU TO STRIKE THE KEYS:"
95 PRINT "  1. SPACE"
100 PRINT "  2. SPACE"
105 PRINT "  3. QUOTE"
110 PRINT "  4. LETTER E"
115 PRINT "  5. LETTER R"
120 PRINT "  6. QUOTE"
125 PRINT "  7. SPACE"
130 PRINT "  8. SPACE"
135 PRINT
150 PRINT "THESE CONVENTIONS ARE NECESSARY BECAUSE THE QUOTE IS"
155 PRINT "UNPRINTABLE IN MINIMAL BASIC, AND SPACES ARE DIFFICULT"
160 PRINT "TO COUNT ON A TERMINAL."
165 PRINT
170 PRINT "PLEASE NOTE THAT THIS ROUTINE IS STRONGLY DEPENDENT"
175 PRINT "ON THE CORRECT OPERATION OF THE STRING COMPARISON"
180 PRINT "FEATURE. IF THAT FEATURE IS INCORRECT, THIS ROUTINE"
185 PRINT "WILL LIKELY GIVE INVALID RESULTS."
195 PRINT
200 PRINT "ALL THE REPLIES PROMPTED FOR ARE VALID, THEREFORE "
205 PRINT "IF THE PROCESSOR REJECTS ANY OF THEM,"
210 PRINT "***  TEST FAILED  ***"
215 PRINT
225 PRINT "FOR USER CONVENIENCE, THE STRING TO BE ENTERED IS INDENTED"
230 PRINT "TWO SPACES, SO THAT IF THE INPUT-PROMPT OCCUPIES TWO"
235 PRINT "POSITIONS, AS RECOMMENDED, YOUR INPUT-REPLY WILL BE"
240 PRINT "ALIGNED WITH THE PROMPT STRING."
241 PRINT
242 PRINT "               BEGIN TEST"
245 LET F=0
247 LET Y$="SHOULD BE: ***"
248 LET Z$="ACTUAL   : ***"
250 READ M,C(1),C(2),C(3),P$
255 LET Q$=""
257 LET R$=""
260 IF M<100 THEN 275
265 LET M=M-100
270 READ Q$,R$
275 IF M=0 THEN 850
280 LET A$=""
285 LET B$=""
290 LET C$=""
295 LET I$=""
300 LET J$=""
305 LET K$=""
310 LET A1=0
315 LET B1=0
320 LET C1=0
325 LET I1=0
330 LET J1=0
335 LET K1=0
345 LET Q=0
350 FOR I2=1 TO M
355 LET Q=Q+Q+C(I2)
360 NEXT I2
362 LET Q=Q+1
365 IF M<=1 THEN 380
370 LET Q=Q+2
372 IF M<=2 THEN 380
375 LET Q=Q+4
380 LET Q1=Q
382 IF C(1)=0 THEN 395
385 READ I$
390 GOTO 400
395 READ I1
400 IF M<2 THEN 450
405 IF C(2)=0 THEN 420
410 READ J$
415 GOTO 425
420 READ J1
425 IF M<3 THEN 450
430 IF C(3)=0 THEN 445
435 READ K$
440 GOTO 450
445 READ K1
450 PRINT
455 PRINT
460 PRINT "PLEASE ENTER:"
465 PRINT "  ";P$;Q$;R$
475 ON Q1 GOTO 480,500,520,540,560,580,600,620,640,660,680,700,720,740
480 INPUT A1
485 PRINT Y$;I1;"***"
490 PRINT Z$;A1;"***"
495 GOTO 760
500 INPUT A$
505 PRINT Y$;I$;"***"
510 PRINT Z$;A$;"***"
515 GOTO 760
520 INPUT A1,B1
525 PRINT Y$;I1;J1;"***"
530 PRINT Z$;A1;B1;"***"
535 GOTO 760
540 INPUT A1,B$
545 PRINT Y$;I1;J$;"***"
550 PRINT Z$;A1;B$;"***"
555 GOTO 760
560 INPUT A$,B1
565 PRINT Y$;I$;J1;"***"
570 PRINT Z$;A$;B1;"***"
575 GOTO 760
580 INPUT A$,B$
585 PRINT Y$;I$;J$;"***"
590 PRINT Z$;A$;B$;"***"
595 GOTO 760
600 INPUT A1,B1,C1
605 PRINT Y$;I1;J1;K1;"***"
610 PRINT Z$;A1;B1;C1;"***"
615 GOTO 760
620 INPUT A1,B1,C$
625 PRINT Y$;I1;J1;K$;"***"
630 PRINT Z$;A1;B1;C$;"***"
635 GOTO 760
640 INPUT A1,B$,C1
645 PRINT Y$;I1;J$;K1;"***"
650 PRINT Z$;A1;B$;C1;"***"
655 GOTO 760
660 INPUT A1,B$,C$
665 PRINT Y$;I1;J$;K$;"***"
670 PRINT Z$;A1;B$;C$;"***"
675 GOTO 760
680 INPUT A$,B1,C1
685 PRINT Y$;I$;J1;K1;"***"
690 PRINT Z$;A$;B1;C1;"***"
695 GOTO 760
700 INPUT A$,B1,C$
705 PRINT Y$;I$;J1;K$;"***"
710 PRINT Z$;A$;B1;C$;"***"
715 GOTO 760
720 INPUT A$,B$,C1
725 PRINT Y$;I$;J$;K1;"***"
730 PRINT Z$;A$;B$;C1;"***"
735 GOTO 760
740 INPUT A$,B$,C$
745 PRINT Y$;I$;J$;K$;"***"
750 PRINT Z$;A$;B$;C$;"***"
755 GOTO 760
760 REM RESULTS DISPLAYED
765 IF A$<>I$ THEN 820
770 IF B$<>J$ THEN 820
771 IF C$<>K$ THEN 820
772 LET D9=A1
774 LET E9=I1
776 GOSUB 3000
778 IF F9<>0 THEN 820
782 LET D9=B1
784 LET E9=J1
786 GOSUB 3000
788 IF F9<>0 THEN 820
792 LET D9=C1
794 LET E9=K1
796 GOSUB 3000
798 IF F9<>0 THEN 820
799 PRINT "TEST OK"
800 GOTO 250
820 PRINT "INPUT NOT EQUAL EXPECTED VALUE. RE-TRY (Y OR N)?"
825 INPUT S$
830 IF S$<>"N" THEN 450
835 PRINT "TEST FAILED."
840 LET F=F+1
845 GOTO 250
850 PRINT
855 IF F=0 THEN 870
860 PRINT "***  TEST FAILED: ";F;" CASE(S) HANDLED IMPROPERLY  ***"
865 GOTO 875
870 PRINT "***  TEST PASSED  ***"
875 PRINT
880 PRINT "                   END TEST"
885 PRINT
890 PRINT "END PROGRAM 110"
895 STOP
900 REM FORMAT OF DATA PARMS:
910 REM    1 - # OF DATA ITEMS (+100 IF LONG PROMPT)
920 REM    2-4 - DATA FORMAT, X=0 => NUMERIC
930 REM                       X=1 => ALPHA
940 REM    5 - PROMPT (1ST OF THREE, IF LONG)
950 REM    6-N (OR 8-N) - COMPARAND VALUES
1000 DATA 3,0,0,0,"==1==,==2==,==3==",1,2,3
1010 DATA 103,0,0,0,"==+987999E32==,","==-1.00000E-37==,","==3.E37=="
1020 DATA      9.87999E37,-1E-37,3E37
1030 DATA 103,0,0,0,"===222222",",111111==,","==333333"
1040 DATA       222222,111111,333333
1050 DATA 2,0,0,0,"5,6",5,6
1060 DATA 2,0,0,0,"-05.34,345.567E-11",-5.34,3.45567E-9
1070 DATA 2,0,0,0,"2E2,-3.45",200,-3.45
1080 DATA 102,0,0,0,"-","0000.00012345","6E-11,+1E37",-1.23456E-15,1E37
1090 DATA 103,0,0,0,"-00","0.E-00,","+.000,0E22",0,0,0
1100 DATA 103,0,0,0,"-","999.E-00,","+.999,9E22",-999,+.999,9E22
1110 DATA 3,1,1,1,"ABC,##,#DEF#","ABC","","DEF"
1120 DATA 103,0,1,0,"==4.56789E-11==",",==MIDDLE=ITEM==",",==9=="
1130 DATA            4.56789E-11,"MIDDLE ITEM",9
1140 DATA 103,0,1,0,"==987654===,","===#==MIDDLE=ITE"
1150 DATA       "M==#==,==656565==",987654,"  MIDDLE ITEM  ",656565
1160 DATA 103,1,0,1,"==AN=UNQUOTED=STR","ING===,==3.14159"
1170 DATA      "==,==#EQUALS=PI#=="
1180 DATA       "AN UNQUOTED STRING",3.14159,"EQUALS PI"
1190 DATA 103,0,1,1,"07676760000000E00","00022===,========"
1200 DATA      "==X=========,X"
1210 DATA      7.67676E34,"X","X"
1220 DATA 103,1,0,1,"=====##=====,===5","===,=====THIRD=","ITEM====="
1230 DATA      "",5,"THIRD ITEM"
1240 DATA 103,1,0,1,"==========#=","#==,===0==","==,====##==="," ",0,""
1250 DATA 102,0,1,0,"==+333.","333E-33==,","==+333.333E-33=="
1260 DATA       3.33333E-31,"+333.333E-33"
1270 DATA 103,0,1,0,"1,2","========","========3,4"
1280 DATA      1,"2                3",4
2000 DATA 0,0,0,0,"Q"
3000 REM SUBROUTINE TO CHECK NUMERIC VALUES
3010 REM ONLY A LOOSE CRITERION OF EQUALITY IS USED
3020 LET E7=.999*E9
3030 LET E8=1.001*E9
3040 IF E9>=0 THEN 3100
3050 LET E6=E7
3060 LET E7=E8
3070 LET E8=E6
3100 LET F9=0
3110 IF E7>D9 THEN 3200
3120 IF E8<D9 THEN 3200
3130 RETURN
3200 LET F9=1
3210 RETURN
4000 END
