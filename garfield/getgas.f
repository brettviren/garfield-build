CDECK  ID>, GETGAS.
       SUBROUTINE GETGAS(NGS,Q,QIN,NIN,E,EI,NAME,VIRL,EB,
     -      PEQEL,PEQIN,KEL,KIN,SCRPT)
*-----------------------------------------------------------------------
*   GETGAS - Retrieves gas tables, originally called GASMIX
*   Author: Steve Biagi, modified.
*   (Last changed on 16/ 9/05.)
*-----------------------------------------------------------------------
       implicit none
       CHARACTER*15 NAME
       CHARACTER*30 SCRPT(226)
       DOUBLE PRECISION Q(6,2048),QIN(220,2048),E(6),EI(220),
     -      PEQEL(6,2048),PEQIN(220,2048),VIRL,EB
       INTEGER KIN(220),KEL(6),NGS,NIN
*** Take the desired gas
       IF(NGS.EQ.1)THEN
            CALL GAS1(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.2)THEN
            CALL GAS2(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.3)THEN
            CALL GAS3(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.4)THEN
            CALL GAS4(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.5)THEN
            CALL GAS5(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.6)THEN
            CALL GAS6(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.7)THEN
            CALL GAS7(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.8)THEN
            CALL GAS8(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.9)THEN
            CALL GAS9(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.10)THEN
            CALL GAS10(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.11)THEN
            CALL GAS11(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.12)THEN
            CALL GAS12(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.13)THEN
            CALL GAS13(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.14)THEN
            CALL GAS14(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.15)THEN
            CALL GAS15(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.16)THEN
            CALL GAS16(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.17)THEN
            CALL GAS17(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.18)THEN
            CALL GAS18(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.19)THEN
            CALL GAS19(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.20)THEN
            CALL GAS20(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.21)THEN
            CALL GAS21(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.22)THEN
            CALL GAS22(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.23)THEN
            CALL GAS23(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.24)THEN
            CALL GAS24(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.25)THEN
            CALL GAS25(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.26)THEN
            CALL GAS26(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.27)THEN
            CALL GAS27(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.28)THEN
            CALL GAS28(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.29)THEN
            CALL GAS29(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.30)THEN
            CALL GAS30(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.31)THEN
            CALL GAS31(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.32)THEN
            CALL GAS32(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.33)THEN
            CALL GAS33(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.34)THEN
            CALL GAS34(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.35)THEN
            CALL GAS35(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.36)THEN
            CALL GAS36(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.37)THEN
            CALL GAS37(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.38)THEN
            CALL GAS38(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.39)THEN
            CALL GAS39(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.40)THEN
            CALL GAS40(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.41)THEN
            CALL GAS41(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.42)THEN
            CALL GAS42(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.43)THEN
            CALL GAS43(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.44)THEN
            CALL GAS44(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.45)THEN
            CALL GAS45(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.46)THEN
            CALL GAS46(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.47)THEN
            CALL GAS47(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.48)THEN
            CALL GAS48(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.49)THEN
            CALL GAS49(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.50)THEN
            CALL GAS50(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.51)THEN
            CALL GAS51(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.52)THEN
            CALL GAS52(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.53)THEN
            CALL GAS53(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.54)THEN
            CALL GAS54(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.55)THEN
            CALL GAS55(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.56)THEN
            CALL GAS56(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.57)THEN
            CALL GAS57(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.58)THEN
            CALL GAS58(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.59)THEN
            CALL GAS59(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSEIF(NGS.EQ.60)THEN
            CALL GAS60(Q,QIN,NIN,E,EI,NAME,VIRL,EB,PEQEL,PEQIN,KEL,KIN,
     -           SCRPT)
       ELSE
            PRINT *,' !!!!!! GETGAS WARNING : Received the unknown'//
     -           ' gas identifier ',NGS,'.'
       ENDIF
       END
