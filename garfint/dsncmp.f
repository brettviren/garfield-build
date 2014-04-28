CDECK  ID>, DSNCMP.
       LOGICAL FUNCTION DSNCMP(DATE1,DATE2)
*-----------------------------------------------------------------------
*   DSNCMP - Returns .TRUE. if the date DATE2 precedes DATE1.
*   (Last changed on 10/11/09.)
*-----------------------------------------------------------------------
       implicit none
       CHARACTER*8 DATE1,DATE2
       INTEGER IDAY1,IDAY2,IMON1,IMON2,IYEAR1,IYEAR2
*** Decode the date strings.
       READ(DATE1,'(BN,I2,1X,I2,1X,I2)') IDAY1,IMON1,IYEAR1
       IF(IYEAR1.LT.84)THEN
            IYEAR1=IYEAR1+2000
       ELSE
            IYEAR1=IYEAR1+1900
       ENDIF
       READ(DATE2,'(BN,I2,1X,I2,1X,I2)') IDAY2,IMON2,IYEAR2
       IF(IYEAR2.LT.84)THEN
            IYEAR2=IYEAR2+2000
       ELSE
            IYEAR2=IYEAR2+1900
       ENDIF
*** Compare.
       DSNCMP=.TRUE.
       IF(IYEAR1.GT.IYEAR2)RETURN
       IF(IYEAR1.EQ.IYEAR2.AND.IMON1.GT.IMON2)RETURN
       IF(IYEAR1.EQ.IYEAR2.AND.IMON1.EQ.IMON2.AND.IDAY1.GT.IDAY2)RETURN
       DSNCMP=.FALSE.
       END
