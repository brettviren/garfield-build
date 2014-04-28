CDECK  ID>, STRTYP.
       INTEGER FUNCTION STRTYP(STRING,NC)
*-----------------------------------------------------------------------
*   STRTYP - Determines the type of STRING(1:NC), 0=character string,
*            1=integer, 2=real, 3=hex, 4=asterisk, -1=invalid argument.
*   (Last changed on 25/ 3/06.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER IINTEG,IREAL,IHEX,I,NC
       CHARACTER*(*) STRING
*** First handle the case of incorrect arguments.
       IF(NC.GT.LEN(STRING).OR.NC.LT.1)THEN
            STRTYP=-1
            RETURN
       ENDIF
*** Handle case of asterisk.
       IF(NC.EQ.1.AND.STRING(1:NC).EQ.'*')THEN
            STRTYP=4
            RETURN
       ENDIF
*** Initiliase the flag which are 1 for integers, reals and hex.
       IINTEG=1
       IREAL=1
       IHEX=1
*** Loop over the word.
       DO 10 I=1,NC
       IF(INDEX('0123456789ABCDEFabcdef',STRING(I:I)).EQ.0)IHEX=0
       IF(INDEX('.Ee',STRING(I:I)).NE.0)THEN
            IINTEG=0
       ELSEIF(INDEX('01234567890+- ',STRING(I:I)).EQ.0)THEN
            IINTEG=0
            IREAL=0
       ENDIF
10     CONTINUE
*** Determine the type from the value of the flags.
       IF(IINTEG.EQ.0.AND.IREAL.EQ.1)THEN
            STRTYP=2
       ELSEIF(IINTEG.EQ.1)THEN
            STRTYP=1
       ELSEIF(IHEX.EQ.1)THEN
            STRTYP=3
       ELSE
            STRTYP=0
       ENDIF
       END
