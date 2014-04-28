CDECK  ID>, INPGLB.
       SUBROUTINE INPGLB
*-----------------------------------------------------------------------
*   INPGLB - Updates the table of global variables.
*   (Last changed on 19/ 1/11.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER MXWIRE,MXSW,MXLIST,MXCHA,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR,
     -         MXPAIR,MXPART,MXFOUR,MXCLUS,
     -         MXLINE,MXEQUT,
     -         MXRECL,MXINCH,MXWORD,MXCHAR,MXNAME,MXLUN,
     -         MXINS,MXREG,MXARG,MXCONS,MXVAR,MXALGE,
     -         MXZERO,MXSTCK,MXFPNT,MXFPAR,MXWKLS,
     -         MXHLEV,MXHLRL,MXSUBT,
     -         MXDLVL,MXILVL,MXDLIN,
     -         MXHIST,MXFRAC,MXBANG,MXBTAB,
     -         MXEXG,MXIOG,MXCSG,
     -         MXORIA,
     -         MXMAT,MXEMAT,MXMDIM,
     -         MXSHOT,MXZPAR,
     -         MXMAP,MXEPS,MXWMAP,MXSOLI,MXSBUF,
     -         MXPLAN,MXPOIN,MXEDGE,
     -         MXMCA
       PARAMETER (MXWIRE=   300,MXSW  =   50)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXHIST=   200, MXCHA = MXLIST/2)
       PARAMETER (MXGRID=    50)
       PARAMETER (MXNAME=   200, MXLUN =    30)
       PARAMETER (MXCLUS=   500, MXPAIR=  2000, MXPART= 10000)
       PARAMETER (MXLINE=   150, MXEQUT=    50)
       PARAMETER (MXFOUR=    16)
       PARAMETER (MXRECL= 10000)
       PARAMETER (MXINCH=  2000, MXWORD=   200, MXCHAR=MXINCH)
       PARAMETER (MXINS =  1000, MXREG =   500, MXCONS=  -500,
     -            MXVAR =   500, MXALGE=   500, MXARG =   100)
       PARAMETER (MXMAT =   500, MXEMAT=200000, MXMDIM=   10)
       PARAMETER (MXZERO=MXWIRE)
       PARAMETER (MXSTCK=     5)
       PARAMETER (MXFPNT= 20000, MXFPAR=    10)
       PARAMETER (MXWKLS=    10)
       PARAMETER (MXHLEV=     9, MXSUBT=   200, MXHLRL=  860)
       PARAMETER (MXDLVL=    10, MXILVL=    20, MXDLIN= 2500)
       PARAMETER (MXFRAC=    13)
       PARAMETER (MXBANG=    20, MXBTAB=    25)
       PARAMETER (MXEXG =    50, MXIOG =    10, MXCSG =  200)
       PARAMETER (MXORIA=  1000)
       PARAMETER (MXSHOT=    10, MXZPAR=4*MXSHOT+2)
       PARAMETER (MXMAP =  5000,MXEPS =   10)
       PARAMETER (MXWMAP=     5)
       PARAMETER (MXSOLI=  1000)
       PARAMETER (MXPLAN= 50000, MXPOIN=100000,MXEDGE=100)
       PARAMETER (MXSBUF= 20000)
       PARAMETER (MXMCA = 50000)
*   The parameter MXNBMC must equal MXGNAM (sequence MAGBPARM) !
       INTEGER MXNBMC
       PARAMETER(MXNBMC=60)
       INTEGER INS(MXINS,4),ALGENT(MXALGE,10),MODREG(MXCONS:MXREG),
     -      ISYNCH,IINS0,ICONS0,ARGREF(MXARG,2),MODARG(MXARG),
     -      NREG,NCONS,NINS,NERR,NRES,NALGE,IENTRL,NAERR(100)
       REAL REG(MXCONS:MXREG),ARG(MXARG),EXPMAX
       PARAMETER(EXPMAX=40.0)
       LOGICAL EXEC(MXINS),LIGUND,LINUND
       COMMON /ALGDAT/ REG,ARG,MODARG,ARGREF,INS,MODREG,ALGENT,
     -      NREG,NCONS,NINS,NERR,NAERR,
     -      NRES,NALGE,IENTRL,ISYNCH,IINS0,ICONS0,EXEC,LIGUND,LINUND
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*(MXINCH) STRING,INDSTR
       CHARACTER*54 VALUE
       CHARACTER*10 MODE
       LOGICAL USE(MXVAR)
       INTEGER MODRES(1),NCIND,NCSTR,NC,NWORD,I,IGLB,IENTNO,IENTRY,
     -      ILAST,IFAIL,IEXTR,NNRES,ITEMP,ISIZ(1)
       REAL RES(1)
*** Check number of arguments.
       CALL INPNUM(NWORD)
*   No arguments, listing required.
       IF(NWORD.EQ.1)THEN
            WRITE(LUNOUT,'(/2X,''GLOBAL VARIABLES CURRENTLY DEFINED''//
     -           2X,''Name        Type        Value''/)')
            DO 40 I=1,NGLB
            IF(GLBMOD(I).EQ.1)THEN
                 MODE='String'
            ELSEIF(GLBMOD(I).EQ.2)THEN
                 MODE='Number'
            ELSEIF(GLBMOD(I).EQ.3)THEN
                 MODE='Logical'
            ELSEIF(GLBMOD(I).EQ.4)THEN
                 MODE='Histogram'
            ELSEIF(GLBMOD(I).EQ.5)THEN
                 MODE='Matrix'
            ELSEIF(GLBMOD(I).EQ.0)THEN
                 MODE='Undefined'
            ELSE
                 MODE='# Unknown'
            ENDIF
            CALL OUTFMT(GLBVAL(I),GLBMOD(I),VALUE,NC,'LEFT')
            IF(I.LE.7)THEN
                 WRITE(LUNOUT,'(2X,A10,2X,A10,2X,A,
     -                ''   (Not user modifiable)'')')
     -                GLBVAR(I),MODE,VALUE(1:NC)
            ELSEIF(I.LE.11)THEN
                 WRITE(LUNOUT,'(2X,A10,2X,A10,2X,A,
     -                ''   (Can be modified by the program)'')')
     -                GLBVAR(I),MODE,VALUE(1:NC)
            ELSE
                 WRITE(LUNOUT,'(2X,A10,2X,A10,2X,A)')
     -                GLBVAR(I),MODE,VALUE(1:NC)
            ENDIF
40          CONTINUE
            RETURN
       ENDIF
*** Pick up the name of the variable.
       CALL INPSTR(2,2,STRING,NC)
*   Find out whether this is a matrix indexing expression.
       IF(INDEX(STRING(1:NC),'[').GT.1.AND.STRING(NC:NC).EQ.']')THEN
            NCSTR=INDEX(STRING(1:NC),'[')-1
            INDSTR=STRING(NCSTR+1:NC)
            NCIND=NC-NCSTR
       ELSE
            NCSTR=NC
            INDSTR=' '
            NCIND=0
       ENDIF
*   Check the name starts with a character.
       IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',STRING(1:1)).EQ.0)THEN
            PRINT *,' !!!!!! INPGLB WARNING : The variable name does'//
     -           ' not start with a character.'
            RETURN
       ENDIF
*   Check for illegal characters.
       DO 30 I=1,NCSTR
       IF(INDEX('+-*/&|<=#>^ ,.:;([{)]}''"`',STRING(I:I)).NE.0)THEN
            PRINT *,' !!!!!! INPGLB WARNING : The variable name'//
     -           ' contains at least 1 illegal character; ignored.'
            RETURN
       ENDIF
30     CONTINUE
*   Make sure the name is not empty.
       IF(STRING.EQ.' '.OR.NCSTR.LT.1)THEN
            PRINT *,' !!!!!! INPGLB WARNING : The variable name'//
     -           ' is empty; definition is ignored.'
            RETURN
       ENDIF
*   Warn if the name is longer than 10 characters.
       IF(NCSTR.GT.10)PRINT *,' !!!!!! INPGLB WARNING : The variable'//
     -      ' name is truncated to the first 10 characters.'
*** Scan the table, add an entry if needed.
       DO 10 I=1,NGLB
       IF(GLBVAR(I).EQ.STRING(1:MAX(1,MIN(10,NCSTR))))THEN
            IF(NCIND.NE.0.AND.GLBMOD(I).NE.5)THEN
                 PRINT *,' !!!!!! INPGLB WARNING : '//STRING(1:NCSTR)//
     -                ' is not of type Matrix; indexing not permitted.'
                 RETURN
            ENDIF
            IGLB=I
            GOTO 20
       ENDIF
10     CONTINUE
*   If a submatrix, the variables must have been defined before.
       IF(NCIND.NE.0)THEN
            PRINT *,' !!!!!! INPGLB WARNING : '//STRING(1:NCSTR)//
     -           ' is not a declared Matrix; indexing not permitted.'
            RETURN
       ELSEIF(NGLB.GE.MXVAR)THEN
            PRINT *,' !!!!!! INPGLB WARNING : No room to add another'//
     -           ' global variable; definition ignored.'
            RETURN
       ENDIF
       NGLB=NGLB+1
       IGLB=NGLB
       GLBVAR(NGLB)=STRING(1:MAX(1,MIN(10,NCSTR)))
       GLBMOD(NGLB)=0
*   Ensure that this variable is not a system variable.
20     CONTINUE
       IF(IGLB.LE.7)THEN
            PRINT *,' !!!!!! INPGLB WARNING : This variable can'//
     -           ' not be redefined by the user.'
            RETURN
       ENDIF
*** Only 2 arguments: reset.
       IF(NWORD.EQ.2)THEN
            IF(NCIND.EQ.0)THEN
                 GLBMOD(IGLB)=0
                 GLBVAL(IGLB)=0
            ELSE
                 PRINT *,' !!!!!! INPGLB WARNING : Partial reset of'//
     -                ' matrices is not permitted ; ignored.'
            ENDIF
            RETURN
       ENDIF
*** Translation of the expression, fetch the string.
       CALL INPSTR(3,NWORD,STRING,NC)
**  Translate for the case with indexing.
       IF(NCIND.NE.0)THEN
*   Translate expression.
            CALL ALGPRE('('//STRING(1:NC)//')'//INDSTR(1:NCIND),
     -           NC+NCIND+2,GLBVAR,NGLB,NNRES,USE,IENTRY,IFAIL)
*   Check validity.
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! INPGLB WARNING : Unable to process'//
     -                ' the indexing expression; global not assigned.'
                 CALL ALGCLR(IENTRY)
                 RETURN
            ELSEIF(NNRES.NE.1)THEN
                 PRINT *,' !!!!!! INPGLB WARNING : Indexing doesn''t'//
     -                ' lead to 1 result; global not assigned.'
                 CALL ALGCLR(IENTRY)
                 RETURN
            ENDIF
*   Locate the entry point number.
            IENTNO=0
            DO 50 I=1,NALGE
            IF(ALGENT(I,1).EQ.IENTRY.AND.ALGENT(I,3).EQ.1)IENTNO=I
50          CONTINUE
            IF(IENTNO.EQ.0)THEN
                 PRINT *,' !!!!!! INPGLB WARNING : No valid indexing'//
     -                ' entry point found; global not assigned.'
                 CALL ALGCLR(IENTRY)
                 RETURN
            ENDIF
*   Locate the final EXTRACT_SUBMATRIX call.
            DO 60 I=ALGENT(IENTNO,5)+ALGENT(IENTNO,6)-1,
     -           ALGENT(IENTNO,5)+2,-1
            IF(INS(I,1).EQ.-80.AND.INS(I,2).EQ.9.AND.
     -           INS(I-1,2).EQ.8.AND.INS(I-2,2).EQ.8)THEN
                 IEXTR=I
                 GOTO 70
            ENDIF
60          CONTINUE
            PRINT *,' !!!!!! INPGLB WARNING : Instruction list'//
     -           ' tail not as expected.'
            CALL ALGCLR(IENTRY)
            RETURN
70          CONTINUE
*   Store the location of the last instruction.
            ILAST=ALGENT(IENTNO,5)+ALGENT(IENTNO,6)-1
*   Store reference to temporary matrix.
            ITEMP=INS(IEXTR-2,3)
*   Replace result and return by DELETE_MATRIX on temporary matrix.
            IF(ITEMP.GT.NGLB)THEN
                 INS(ILAST-1,1)=  0
                 INS(ILAST-1,2)=  8
                 INS(ILAST-1,3)=ITEMP
                 INS(ILAST-1,4)=  1
                 INS(ILAST  ,1)=-86
                 INS(ILAST  ,2)=  9
                 INS(ILAST  ,3)=  1
                 INS(ILAST  ,4)=  0
            ELSE
                 INS(ILAST-1,1)=  0
                 INS(ILAST-1,2)= -1
                 INS(ILAST-1,3)=  0
                 INS(ILAST-1,4)=  0
                 INS(ILAST  ,1)=  0
                 INS(ILAST  ,2)= -1
                 INS(ILAST  ,3)=  0
                 INS(ILAST  ,4)=  0
            ENDIF
*   Replace EXTRACT_SUBMATRIX by STORE_SUBMATRIX.
            INS(IEXTR  ,1)=-81
*   Exchange the in/out matrices, assign to global, fix protections.
            INS(IEXTR-1,1)=  3
            INS(IEXTR-1,3)=INS(IEXTR-2,3)
            INS(IEXTR-2,1)=  0
            INS(IEXTR-2,3)=IGLB
*** In debug mode, print the list.
            IF(LDEBUG)THEN
                 WRITE(LUNOUT,'(''  ++++++ INPGLB DEBUG   : List'',
     -                '' after processing indexing calls:'')')
                 CALL ALGPRT(ALGENT(IENTNO,5),ALGENT(IENTNO,5)+
     -                ALGENT(IENTNO,6)-1)
            ENDIF
**  Translate for the case without indexing.
       ELSE
            CALL ALGPRE(STRING(1:NC),NC,
     -           GLBVAR,NGLB,NNRES,USE,IENTRY,IFAIL)
*   Check validity.
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! INPGLB WARNING : Unable to process'//
     -                ' the expression; global not assigned.'
                 CALL ALGCLR(IENTRY)
                 RETURN
            ELSEIF(NNRES.NE.1)THEN
                 PRINT *,' !!!!!! INPGLB WARNING : Formula doesn''t'//
     -                ' lead to 1 result; global not assigned.'
                 CALL ALGCLR(IENTRY)
                 RETURN
            ENDIF
*   No temporary matrix.
            ITEMP=0
       ENDIF
*** Evaluate.
       CALL TIMEL(GLBVAL(1))
       CALL ALGEXE(IENTRY,GLBVAL,GLBMOD,NGLB,RES,MODRES,1,IFAIL)
*   Error messages ?
       CALL ALGERR
*   If failed, return.
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! INPGLB WARNING : Unable to evaluate'//
     -           ' the expression; definition ignored.'
            ISIZ(1)=1
            IF(NCIND.NE.0.AND.ITEMP.GT.NGLB)
     -           CALL MATADM('DELETE',NINT(REG(ITEMP)),1,ISIZ,2,IFAIL)
            CALL ALGCLR(IENTRY)
            RETURN
       ENDIF
*   Store the reference or the value itself.
       IF(NCIND.EQ.0)THEN
            IF((MODRES(1).EQ.1.OR.MODRES(1).EQ.4.OR.MODRES(1).EQ.5).AND.
     -           MODRES(1).EQ.GLBMOD(IGLB).AND.
     -           NINT(GLBVAL(IGLB)).EQ.NINT(RES(1)))THEN
                 GLBVAL(IGLB)=RES(1)
                 GLBMOD(IGLB)=MODRES(1)
            ELSE
                 CALL ALGREU(NINT(GLBVAL(IGLB)),GLBMOD(IGLB),0)
                 GLBVAL(IGLB)=RES(1)
                 GLBMOD(IGLB)=MODRES(1)
            ENDIF
       ENDIF
*   Remove the entry point.
       CALL ALGCLR(IENTRY)
       END
