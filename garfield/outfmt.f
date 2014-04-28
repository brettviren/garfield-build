CDECK  ID>, OUTFMT.
       SUBROUTINE OUTFMT(VAL,IFMT,STRING,NC,ALIGN)
*-----------------------------------------------------------------------
*   OUTFMT - Takes care of output formatting.
*   VARIABLES : VAL         : The number to be formatted.
*               IFMT        : Format code, 0=undefined, 1=string,
*                             2=number, 3=logical, 4=histogram.
*               STRING      : Output string, use only first NC chars.
*   (Last changed on  9/ 4/00.)
*-----------------------------------------------------------------------
       implicit none
       CHARACTER*(*) STRING,ALIGN
       INTEGER NC,IFMT,I,IFAIL
       REAL VAL
*** Initialise the string.
       STRING=' '
*** Unitialised variables.
       IF(IFMT.EQ.0)THEN
            IF(LEN(STRING).LT.4)THEN
                 STRING='?'
            ELSE
                 STRING='Nill'
            ENDIF
*** Take care of strings.
       ELSEIF(IFMT.EQ.1)THEN
            CALL STRBUF('READ',NINT(VAL),STRING,NC,IFAIL)
            RETURN
*** Take care of numbers.
       ELSEIF(IFMT.EQ.2)THEN
            CALL OUTFM2(VAL,STRING)
*** Take care of logicals.
       ELSEIF(IFMT.EQ.3)THEN
            IF(LEN(STRING).LT.5)THEN
                 STRING='***'
            ELSEIF(NINT(VAL).EQ.0)THEN
                 STRING='False'
            ELSEIF(NINT(VAL).EQ.1)THEN
                 STRING='True'
            ELSE
                 STRING='???'
            ENDIF
*** Take care of histograms.
       ELSEIF(IFMT.EQ.4)THEN
            STRING='Histogram'
*** Take care of matrices.
       ELSEIF(IFMT.EQ.5)THEN
            CALL OUTFM5(VAL,STRING)
*** Only other format is real (2).
       ELSE
            PRINT *,' ###### OUTFMT ERROR   : Invalid format code'//
     -           ' received: ',IFMT,'; program bug, please report.'
            STRING='???'
            NC=3
            RETURN
       ENDIF
*** Count the length, removing blanks for left alignment.
       IF(ALIGN.EQ.'LEFT')THEN
            NC=0
            DO 10 I=1,LEN(STRING)
            IF(STRING(I:I).NE.' ')THEN
                 NC=NC+1
                 IF(STRING(I:I).EQ.'%')THEN
                      STRING(NC:NC)=' '
                 ELSE
                      STRING(NC:NC)=STRING(I:I)
                 ENDIF
            ENDIF
10          CONTINUE
            IF(NC.LT.LEN(STRING))
     -           STRING(MIN(LEN(STRING),NC+1):LEN(STRING))=' '
*   For right alignment.
       ELSEIF(ALIGN.EQ.'RIGHT')THEN
            NC=0
            DO 80 I=LEN(STRING),1,-1
            IF(STRING(I:I).NE.' ')THEN
                 NC=NC+1
                 IF(STRING(I:I).EQ.'%')THEN
                      STRING(LEN(STRING)-NC+1:LEN(STRING)-NC+1)=' '
                 ELSE
                      STRING(LEN(STRING)-NC+1:LEN(STRING)-NC+1)=
     -                     STRING(I:I)
                 ENDIF
            ENDIF
80          CONTINUE
            IF(NC.LT.LEN(STRING))
     -           STRING(1:MAX(1,LEN(STRING)-NC))=' '
*   Invalid alignment code.
       ELSE
            STRING='???'
            NC=3
            PRINT *,' ###### OUTFMT ERROR   : Received invalid'//
     -           ' alignment code: ',ALIGN,'.'
       ENDIF
       END
