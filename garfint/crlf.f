CDECK  ID>, CRLF.
       SUBROUTINE CRLF(STRING)
*-----------------------------------------------------------------------
*   CRLF   - Replaces CR and LF characters by blanks.
*   (Last changed on 26/11/09.)
*-----------------------------------------------------------------------
       implicit none
       CHARACTER*(*) STRING
       INTEGER I
*** Loop over the string.
       DO 10 I=1,LEN(STRING)
*   Check for LF and CR.
       IF(ICHAR(STRING(I:I)).EQ.10.OR.
     -    ICHAR(STRING(I:I)).EQ.13)STRING(I:I)=' '
10     CONTINUE
       END
