CDECK  ID>, FUNFIT.
       SUBROUTINE FUNFIT(FUN,X,Y,EY,N,LPRINT,IA,IE,NA,IFAIL)
*-----------------------------------------------------------------------
*   FUNFIT - Fits an arbitrary function.
*   (Last changed on  9/ 6/12.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       CHARACTER*(*) FUN
       REAL X(*),Y(*),EY(*)
       DOUBLE PRECISION XX(MXFPNT),YY(MXFPNT),EEY(MXFPNT),
     -      AA(MXVAR),EA(MXVAR),CHI2,YSUM
       INTEGER N,NA,NNA,IFAIL,IFAIL1,IA(*),IE(*),IENTRY,I,IIA,NRES,NDATA
       LOGICAL LPRINT,USE(MXVAR),OK
       COMMON /FFUDAT/ NNA,IENTRY,IIA(MXVAR)
       EXTERNAL FUNFUN
*** Preset the error flag.
       IFAIL=1
       OK=.TRUE.
*** Debugging and identification output.
       IF(LIDENT)PRINT *,' /// ROUTINE FUNFIT ///'
*** Copy the vectors.
       YSUM=0
       NDATA=0
       DO 30 I=1,N
       IF(EY(I).GT.0)THEN
            NDATA=NDATA+1
            IF(NDATA.LE.MXFPNT)THEN
                 XX(NDATA)=DBLE(X(I))
                 YY(NDATA)=DBLE(Y(I))
                 YSUM=YSUM+ABS(YY(I))
                 EEY(NDATA)=DBLE(EY(I))
            ENDIF
       ENDIF
30     CONTINUE
*** Check remaining number of data points.
       IF(NDATA.LT.N)PRINT *,' ------ FUNFIT MESSAGE : Eliminated ',
     -      N-NDATA,' data points for which error <= 0.'
       IF(NDATA.LT.NA)THEN
            PRINT *,' !!!!!! FUNFIT WARNING : The problem is not'//
     -           ' sufficiently constrained; no fit.'
            OK=.FALSE.
       ENDIF
*** Check dimensions.
       IF(NA.GT.MXFPAR.OR.NA.GT.MXVAR.OR.NDATA.GT.MXFPNT)THEN
            PRINT *,' !!!!!! FUNFIT WARNING : Dimensions of the'//
     -           ' problem exceed compilation parameters; no fit.'
            OK=.FALSE.
       ELSEIF(NA.LE.0)THEN
            PRINT *,' !!!!!! FUNFIT WARNING : No parameters to be'//
     -           ' adjusted; no fit.'
            OK=.FALSE.
       ENDIF
*** Convert the function.
       CALL ALGPRE(FUN,LEN(FUN),GLBVAR,NGLB,NRES,USE,IENTRY,IFAIL1)
*   Check error flag.
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! FUNFIT WARNING : Translating the'//
     -           ' function ',FUN,' failed; no fit.'
            RETURN
       ENDIF
*   Check the type of the used globals and copy to a fit vector.
       DO 10 I=1,NGLB
       IF(I.NE.8.AND.USE(I).AND.GLBMOD(I).EQ.0)THEN
            PRINT *,' !!!!!! FUNFIT WARNING : The function uses the'//
     -           ' uninitialised variable '//GLBVAR(I)
            OK=.FALSE.
       ELSEIF(I.NE.8.AND.USE(I).AND.GLBMOD(I).NE.2)THEN
            PRINT *,' !!!!!! FUNFIT WARNING : The function uses the'//
     -           ' non-numeric global '//GLBVAR(I)
            OK=.FALSE.
       ENDIF
10     CONTINUE
*   Check that all variables are in fact used.
       NNA=NA
       DO 20 I=1,NA
       IF(IA(I).LE.0.OR.IA(I).GT.NGLB)THEN
            PRINT *,' !!!!!! FUNFIT WARNING : Incorrect reference'//
     -           ' to a global received; program bug, please report.'
            OK=.FALSE.
       ELSEIF(.NOT.USE(IA(I)))THEN
            PRINT *,' !!!!!! FUNFIT WARNING : The function does not'//
     -           ' depend on the variable '//GLBVAR(IA(I))
            OK=.FALSE.
       ENDIF
       AA(I)=DBLE(GLBVAL(IA(I)))
       IIA(I)=IA(I)
20     CONTINUE
*   Ensure that the function depends on x.
       IF(NA.GT.1.AND..NOT.USE(8))THEN
            PRINT *,' !!!!!! FUNFIT WARNING : The function does not'//
     -           ' depend on X but on more than 1 fit parameter.'
            OK=.FALSE.
       ELSEIF(.NOT.USE(8))THEN
            PRINT *,' ------ FUNFIT MESSAGE : The function does not'//
     -           ' depend on X (acceptable for 1 free parameter).'
       ENDIF
*   Set the mode of global 8 (=X) to 2 and delete anything tied to it.
       CALL ALGREU(NINT(GLBVAL(8)),GLBMOD(8),0)
       GLBMOD(8)=2
*   Check error status.
       IF(.NOT.OK)THEN
            PRINT *,' !!!!!! FUNFIT WARNING : No fit because of the'//
     -           ' above warnings.'
            RETURN
       ENDIF
*** Now carry out the fit.
       CALL LSQFIT(FUNFUN,AA,EA,NA,XX,YY,EEY,NDATA,200,0.01*YSUM/NDATA,
     -      CHI2,1.0D-3,LPRINT,IFAIL1)
*   Print the number of errors.
       CALL ALGERR
*   Check error flag.
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! FUNFIT WARNING : Error fitting the'//
     -           ' function; results not returned.'
            RETURN
       ENDIF
*** Transfer the results back.
       DO 40 I=1,NA
       GLBVAL(IA(I))=REAL(AA(I))
       GLBVAL(IE(I))=REAL(EA(I))
       GLBMOD(IA(I))=2
       GLBMOD(IE(I))=2
40     CONTINUE
*** Things seem to have worked.
       IFAIL=0
       END
