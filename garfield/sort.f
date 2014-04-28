CDECK  ID>, SORT.
       SUBROUTINE SORT(I,R2,IE)
*-----------------------------------------------------------------------
*   SORT   - Selects collision type from collision array by binary step
*            sampling reduces sampling range to within 4 positions in
*            array.
*            output =  i   (position within 4 of correct value)
*   (Last changed on 28/ 7/05.)
*-----------------------------------------------------------------------
       implicit none
*   Sometimes IPLAST is called LAST
       DOUBLE PRECISION CF,EIN,TCF,RGAS,WPL
       INTEGER IARRY,IPN,IPLAST,ISIZE
       COMMON/LARGE/CF(2048,512),EIN(512),TCF(2048),IARRY(512),
     -      RGAS(512),IPN(512),WPL(512),IPLAST,ISIZE
       INTEGER I,IE,ISTEP,INCR,K
       DOUBLE PRECISION R2
       ISTEP=ISIZE
       INCR=0
       DO 1 K=1,12
       I=INCR
       IF(ISTEP.EQ.2) RETURN
       I=INCR+ISTEP
       IF(I.LE.IPLAST)THEN
            IF(CF(IE,I).LT.R2) INCR=INCR+ISTEP
       ENDIF
       ISTEP=ISTEP/2
1      CONTINUE
       END
