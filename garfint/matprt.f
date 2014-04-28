CDECK  ID>, MATPRT.
       SUBROUTINE MATPRT(IREF)
*-----------------------------------------------------------------------
*   MATPRT - Prints a matrix.
*   (Last changed on 25/10/95.)
*-----------------------------------------------------------------------
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
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER IREF,ISLOT,IA(MXMDIM),MATADR
       REAL AUX
       CHARACTER*78 STRING
       CHARACTER*20 STRAUX
       EXTERNAL MATADR
*** Check validity of reference.
       IF(IREF.LE.0)THEN
            PRINT *,' !!!!!! MATPRT WARNING : Non-positive reference'//
     -           ' given; matrix not printed.'
            RETURN
       ENDIF
*** Locate the current matrix.
       DO 10 I=1,MXMAT
       IF(MREF(I).EQ.IREF)THEN
            ISLOT=I
            GOTO 20
       ENDIF
10     CONTINUE
       PRINT *,' !!!!!! MATPRT WARNING : Matrix to be printed has'//
     -      ' not been found.'
       RETURN
20     CONTINUE
*** Special case: null matrices.
       IF(MDIM(ISLOT).LT.1)THEN
            WRITE(LUNOUT,'(''  (Null matrix)''/)')
*** Special case: the 1-dimensional matrix.
       ELSEIF(MDIM(ISLOT).EQ.1)THEN
            NC=0
            STRING=' '
            DO 130 I=1,MSIZ(ISLOT,1)
            CALL OUTFMT(MVEC(MORG(ISLOT)+I),MMOD(ISLOT),
     -           STRAUX,NCAUX,'LEFT')
            IF(NC+NCAUX+1.GT.LEN(STRING))THEN
                 IF(NC.GE.1)WRITE(LUNOUT,'(2X,A)') STRING(1:NC)
                 STRING(1:5)=' '
                 NC=5
            ENDIF
            STRING(NC+1:NC+NCAUX+1)=STRAUX(1:NCAUX)//' '
            NC=NC+NCAUX+1
130         CONTINUE
            WRITE(LUNOUT,'(2X,A/)') STRING(1:NC)
*** Print larger matrices.
       ELSE
*   First establish an initial address vector.
            DO 30 I=1,MDIM(ISLOT)
            IA(I)=1
30          CONTINUE
*   Return here to print a further layer of the matrix.
120         CONTINUE
*   Print a header for the matrix of the last 2 dimensions.
            IF(MDIM(ISLOT).GT.2)THEN
                 STRING(1:1)='['
                 NC=1
                 DO 40 I=1,MDIM(ISLOT)-2
                 AUX=REAL(IA(I))
                 CALL OUTFMT(AUX,2,STRAUX,NCAUX,'LEFT')
                 STRING(NC+1:NC+NCAUX)=STRAUX(1:NCAUX)
                 IF(NC+NCAUX+4.GT.LEN(STRING))THEN
                      STRING(LEN(STRING)-7:LEN(STRING))=' ... ;;]'
                      NC=LEN(STRING)
                      GOTO 50
                 ENDIF
                 NC=NC+NCAUX
                 IF(I.LT.MDIM(ISLOT)-2)THEN
                      STRING(NC+1:NC+1)=';'
                      NC=NC+1
                 ENDIF
40               CONTINUE
                 STRING(NC+1:NC+3)=';;]'
                 NC=NC+3
50               CONTINUE
                 WRITE(LUNOUT,'(2X,A)') STRING(1:NC)
            ENDIF
*   Print the matrix for the last 2 dimensions, find longest element.
            MAXLEN=0
            DO 60 I=1,MSIZ(ISLOT,MDIM(ISLOT)-1)
            DO 70 J=1,MSIZ(ISLOT,MDIM(ISLOT))
            IA(MDIM(ISLOT)-1)=I
            IA(MDIM(ISLOT))=J
            IADDR=MATADR(ISLOT,IA)
            CALL OUTFMT(MVEC(IADDR),MMOD(ISLOT),STRAUX,NCAUX,'LEFT')
            MAXLEN=MAX(MAXLEN,NCAUX)
70          CONTINUE
60          CONTINUE
*   And now print the matrix itself.
            DO 80 I=1,MSIZ(ISLOT,MDIM(ISLOT))
            NC=0
            STRING=' '
            DO 90 J=1,MSIZ(ISLOT,MDIM(ISLOT)-1)
            IA(MDIM(ISLOT))=I
            IA(MDIM(ISLOT)-1)=J
            IADDR=MATADR(ISLOT,IA)
            CALL OUTFMT(MVEC(IADDR),MMOD(ISLOT),STRAUX,NCAUX,'RIGHT')
            IF(NC+MAXLEN+1.GT.LEN(STRING))THEN
                 WRITE(LUNOUT,'(2X,A)') STRING(1:NC)
                 STRING(1:MAXLEN+1)=' '
                 NC=MAXLEN+1
            ENDIF
            STRING(NC+1:NC+MAXLEN+1)=STRAUX(LEN(STRAUX)-MAXLEN+1:)//' '
            NC=NC+MAXLEN+1
90          CONTINUE
            WRITE(LUNOUT,'(2X,A)') STRING(1:NC)
80          CONTINUE
            WRITE(LUNOUT,'('' '')')
*   Increment the address vector.
            DO 100 I=1,MDIM(ISLOT)-2
            IF(IA(I).LT.MSIZ(ISLOT,I))THEN
                 IA(I)=IA(I)+1
                 DO 110 J=1,I-1
                 IA(J)=1
110              CONTINUE
                 GOTO 120
            ENDIF
100         CONTINUE
       ENDIF
       END
