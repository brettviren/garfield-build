CDECK  ID>, OUTFM2.
       SUBROUTINE OUTFM2(VAL,STRING)
*-----------------------------------------------------------------------
*   OUTFM2 - Takes care of formatting a real.
*   VARIABLES : VAL         : The number to be formatted.
*               STRING      : Output string, use only first NC chars.
*   (Last changed on 26/ 5/97.)
*-----------------------------------------------------------------------
       implicit none
       CHARACTER*(*) STRING
       CHARACTER*13  AUX
       CHARACTER*7   REST
       CHARACTER*8   FMT
       CHARACTER     SIGN,FIRST
       INTEGER I,J,NOUT,IEXP
       REAL VAL
*** Initialise the string.
       STRING=' '
*** Carry on for reals, first handle the special value 0.
       IF(VAL.EQ.0)THEN
            STRING='0'
*** Integer numbers less than 1E7.
       ELSEIF(ABS(VAL).LT.1.0E7.AND.
     -      ABS(VAL-ANINT(VAL)).LT.1.0E-5*ABS(VAL))THEN
            IF(LEN(STRING).LT.10)THEN
                 STRING='***'
            ELSE
                 WRITE(STRING,'(I10)') NINT(VAL)
            ENDIF
*** Non-integer numbers without exponent, above 1.
        ELSEIF(ABS(VAL).LT.1.0E6.AND.ABS(VAL).GE.1.0)THEN
             IF(LEN(STRING).LT.8)THEN
                  STRING='***'
             ELSE
                  WRITE(FMT,'(''(F8.'',I1,''  )'')')
     -                 5-INT(LOG10(ABS(VAL)))
                  WRITE(STRING,FMT) VAL
                  DO 40 I=8,1,-1
                  IF(STRING(I:I).EQ.'0')THEN
                       STRING(I:I)=' '
                  ELSEIF(STRING(I:I).EQ.'.')THEN
                       STRING(I:I)=' '
                       GOTO 50
                  ELSEIF(STRING(I:I).NE.' ')THEN
                       GOTO 50
                  ENDIF
40                CONTINUE
50                CONTINUE
             ENDIF
*** Non-integer format less than 1.
        ELSEIF(ABS(VAL).LT.1.AND.ABS(VAL).GT.1E-5)THEN
             IF(LEN(STRING).LT.13)THEN
                  STRING='***'
             ELSE
                  WRITE(FMT,'(''(F'',I2,''.'',I2,'')'')')
     -                 8-INT(LOG10(ABS(VAL))),5-INT(LOG10(ABS(VAL)))
                  WRITE(STRING,FMT) VAL
                  DO 60 I=13,1,-1
                  IF(STRING(I:I).EQ.'0')THEN
                       STRING(I:I)=' '
                  ELSEIF(STRING(I:I).EQ.'.')THEN
                       STRING(I:I)=' '
                       GOTO 70
                  ELSEIF(STRING(I:I).NE.' ')THEN
                       GOTO 70
                  ENDIF
60                CONTINUE
70                CONTINUE
             ENDIF
*** Anything else.
       ELSE
            IF(LEN(STRING).LT.13)THEN
                 STRING='***'
            ELSE
                 WRITE(AUX,'(E13.6)') VAL
                 IF(VAL.GE.0)THEN
                      SIGN=' '
                 ELSE
                      SIGN='-'
                 ENDIF
                 IF(INDEX('+-0123456789',AUX(11:11)).EQ.0.OR.
     -                INDEX('0123456789',AUX(12:12)).EQ.0.OR.
     -                INDEX('0123456789',AUX(13:13)).EQ.0)THEN
                      STRING=AUX
                 ELSE
                      READ(AUX,'(3X,A1,A5,1X,I3)') FIRST,REST,IEXP
                      DO 20 I=5,1,-1
                      IF(REST(I:I).NE.'0')GOTO 30
                      IF(REST(I:I).EQ.'0')REST(I:I)=' '
20                    CONTINUE
30                    CONTINUE
                      WRITE(STRING,'(A1,A1,''.'',A5,''E'',I3)')
     -                     SIGN,FIRST,REST,IEXP-1
                      IF(IEXP.EQ.1)STRING(9:)=' '
                      IF(REST.EQ.' ')STRING(3:3)=' '
                 ENDIF
            ENDIF
       ENDIF
*** See whether the expression starts with a dot.
       DO 110 I=1,LEN(STRING)
*   If it does, try to shift all the rest and add a '0'.
       IF(STRING(I:I).EQ.'.')THEN
            DO 120 J=LEN(STRING)-1,I,-1
            STRING(J+1:J+1)=STRING(J:J)
            STRING(J:J)=' '
120         CONTINUE
            IF(STRING(I:I).EQ.' ')STRING(I:I)='0'
            GOTO 130
*   If the string starts with something else, leave search.
       ELSEIF(INDEX(' +-',STRING(I:I)).EQ.0)THEN
            GOTO 130
       ENDIF
110    CONTINUE
130    CONTINUE
*** Remove blanks.
       NOUT=0
       DO 100 I=1,LEN(STRING)
       IF(STRING(I:I).NE.' ')THEN
            IF(NOUT.GE.LEN(STRING))THEN
                 STRING='***'
                 RETURN
            ENDIF
            NOUT=NOUT+1
            IF(NOUT.NE.I)THEN
                 STRING(NOUT:NOUT)=STRING(I:I)
                 STRING(I:I)=' '
            ENDIF
       ENDIF
100    CONTINUE
       END
