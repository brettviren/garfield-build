CDECK  ID>, STRLEN.
       INTEGER FUNCTION STRLEN(STRING)
*-----------------------------------------------------------------------
*   STRLEN - Returns the blank-truncated length of a string.
*   (Last changed on 21/ 2/08.)
*-----------------------------------------------------------------------
       implicit none
       CHARACTER*(*) STRING
       INTEGER I
*** Default.
       STRLEN=1
*** Non-zero, non-empty strings.
       IF(LEN(STRING).GE.1.AND.STRING.NE.' ')THEN
            DO 10 I=LEN(STRING),1,-1
            IF(STRING(I:I).NE.' ')THEN
                 STRLEN=I
                 RETURN
            ENDIF
10          CONTINUE
       ENDIF
       END
