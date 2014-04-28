CDECK  ID>, GRMENUNW.
       SUBROUTINE GRMENU(STRING,SEPAR,XCMIN,YCMIN,XCMAX,YCMAX,
     -      IWKCH,IDEVCH,ICPET,ICHOIC,IFAIL)
*-----------------------------------------------------------------------
*   GRMENU - Builds a menu from the input string. Version for use with
*            any GKS conforming to the final standard.
*-----------------------------------------------------------------------
       PARAMETER(MXITEM=10)
       CHARACTER*(*) STRING
       CHARACTER SEPAR
       CHARACTER*20 ITEM(MXITEM)
       CHARACTER*500 RECORD
       INTEGER NITEM,LENGTH(MXITEM),IARRAY(1)
       REAL RARRAY(1)
*** Assume we won't fail.
       IFAIL=0
*** Scan for separator.
       NITEM=0
       I0=1
       DO 10 I=1,LEN(STRING)
       IF(STRING(I:I).EQ.SEPAR.OR.I.EQ.LEN(STRING))THEN
            IF(NITEM.LT.MXITEM)THEN
                 NITEM=NITEM+1
                 IF(I.EQ.LEN(STRING).AND.STRING(I:I).NE.SEPAR.AND.
     -                I0.LE.I)THEN
                      ITEM(NITEM)=STRING(I0:I)
                      LENGTH(NITEM)=I-I0+1
                 ELSEIF(I0.LE.I-1)THEN
                      ITEM(NITEM)=STRING(I0:I-1)
                      LENGTH(NITEM)=I-I0
                 ELSE
                      ITEM(NITEM)='< not labelled >'
                      LENGTH(NITEM)=16
                 ENDIF
            ELSE
                 IFAIL=1
                 RETURN
            ENDIF
            I0=I+1
       ENDIF
10     CONTINUE
*** Pack the record.
       CALL GPREC(0,IARRAY,0,RARRAY,NITEM,LENGTH,ITEM,LEN(RECORD),
     -      IERR,NCREC,RECORD)
       IF(IERR.NE.0)THEN
            CALL GMSG(IWKCH,'Unable to prepare the menu.')
            IFAIL=1
            RETURN
       ENDIF
*** Check initial default for the choice.
       IF(ICHOIC.LE.0.OR.ICHOIC.GT.NITEM)ICHOIC=1
*** Initialise the CHOICE.
       CALL GINCH(IWKCH,IDEVCH,1,ICHOIC,ICPET,
     -      XCMIN,XCMAX,YCMIN,YCMAX,NCREC,RECORD)
*** Request a choice.
       CALL GMSG(IWKCH,'Please choose an item from the menu.')
100    CONTINUE
       CALL GRQCH(IWKCH,IDEVCH,IERR,ICHOIC)
       IF(IERR.NE.1.OR.ICHOIC.LE.0.OR.ICHOIC.GT.NITEM)THEN
            CALL GMSG(IWKCH,'Not a valid choice, please try again.')
            GOTO 100
       ENDIF
       END
