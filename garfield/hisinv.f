CDECK  ID>, HISINV.
       SUBROUTINE HISINV(IREF,EPS,XEPS,IORDER,IFAIL)
*-----------------------------------------------------------------------
*   HISINV - Inverse interpolation to find XEPS such that P(X<XEPS)=EPS.
*   (Last changed on  4/10/00.)
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
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
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
       PARAMETER (MXMAP =350000,MXEPS =   10)
       PARAMETER (MXWMAP=     5)
       PARAMETER (MXSOLI=  1000)
       PARAMETER (MXPLAN= 50000, MXPOIN=100000,MXEDGE=100)
       PARAMETER (MXSBUF= 20000)
       PARAMETER (MXMCA = 50000)
*   The parameter MXNBMC must equal MXGNAM (sequence MAGBPARM) !
       INTEGER MXNBMC
       PARAMETER(MXNBMC=60)
       DOUBLE PRECISION CONTEN(MXHIST,0:MXCHA+1)
       REAL XMIN(MXHIST),XMAX(MXHIST)
       DOUBLE PRECISION SX0(MXHIST),SX1(MXHIST),SX2(MXHIST)
       INTEGER NCHA(MXHIST),NENTRY(MXHIST)
       LOGICAL SET(MXHIST),HISUSE(MXHIST),HISLIN(MXHIST)
       COMMON /HISDAT/ SX0,SX1,SX2,CONTEN,XMIN,XMAX,HISUSE,HISLIN,NCHA,
     -      NENTRY,SET
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL CUMUL(0:MXCHA+1),X(0:MXLIST+1),DIVDIF,SUM,CLAST,
     -      EPS,XEPS,XPL(MXLIST),YPL(MXLIST)
       INTEGER IREF,IORDER,IFAIL,I,ILAST,NOUT
       CHARACTER*40 COMM,STATUS
       EXTERNAL DIVDIF
*** Assume that things will work.
       IFAIL=0
       XEPS=0.0
       STATUS='Final status: OK'
*** Check reference number.
       IF(IREF.LE.0.OR.IREF.GT.MXHIST)THEN
            PRINT *,' !!!!!! HISINV WARNING : Histogram reference'//
     -           ' not valid; no inverse interpolation.'
            IFAIL=1
            RETURN
       ENDIF
*** Produce a cumulative histogram, for checking purposes only.
       CUMUL(0)=CONTEN(IREF,0)
       DO 40 I=1,NCHA(IREF)+1
       CUMUL(I)=CONTEN(IREF,I)+CUMUL(I-1)
40     CONTINUE
*** Check whether our points is out of bounds or not.
       SUM=CUMUL(NCHA(IREF)+1)
       IF(SUM.LE.0.OR.SUM*EPS.LE.CUMUL(0).OR.
     -      SUM*EPS.GT.CUMUL(NCHA(IREF)))THEN
            IFAIL=1
            STATUS='Status: Failed, input out of bounds.'
            IF(LDEBUG)PRINT *,' ++++++ HISINV DEBUG   : Point ',SUM*EPS,
     -           ' located out of bounds: ',CUMUL(0),CUMUL(NCHA(IREF))
            IF(SUM.LE.0)RETURN
       ELSE
            IFAIL=0
       ENDIF
*** Make a stair case and check at the same time for jumps.
       CLAST=CONTEN(IREF,0)/SUM
       ILAST=0
       DO 50 I=1,NCHA(IREF)
       XPL(2*I-1)=XMIN(IREF)+REAL(I-1)*(XMAX(IREF)-XMIN(IREF))/
     -      REAL(NCHA(IREF))
       XPL(2*I  )=XMIN(IREF)+REAL( I )*(XMAX(IREF)-XMIN(IREF))/
     -      REAL(NCHA(IREF))
       CUMUL(I)=CUMUL(I)/SUM
       YPL(2*I-1)=CUMUL(I)
       YPL(2*I  )=CUMUL(I)
       IF((CUMUL(I)-EPS)*(EPS-CLAST).GE.0.0.AND.ILAST.NE.I-1)THEN
            IF(LDEBUG)PRINT *,' ++++++ HISINV DEBUG   : Point located'//
     -           ' on jump between bins ',ILAST,' and ',I,'.'
            STATUS='Status: Failed, input falls in a jump.'
            IFAIL=1
       ENDIF
       IF(CONTEN(IREF,I).GT.0)THEN
            CLAST=CUMUL(I)
            ILAST=I
       ENDIF
50     CONTINUE
*** Make a normalised cumulative distribution.
       CUMUL(0)=CONTEN(IREF,0)
       NOUT=0
       DO 10 I=1,NCHA(IREF)
       IF(CONTEN(IREF,I).NE.0)THEN
            NOUT=NOUT+1
            X(NOUT)=XMIN(IREF)+(REAL(I)-0.5)*
     -           (XMAX(IREF)-XMIN(IREF))/REAL(NCHA(IREF))
            CUMUL(NOUT)=CONTEN(IREF,I)+CUMUL(NOUT-1)
       ELSE
       ENDIF
10     CONTINUE
       DO 20 I=1,NOUT
       CUMUL(I)=CUMUL(I)/SUM
20     CONTINUE
*** Verify that NOUT is larger than 1.
       IF(NOUT.LE.1)THEN
            PRINT *,' !!!!!! HISINV WARNING : Less than 2 bins filled'//
     -           ' inverse interpolation not possible; 0 returned.'
            IFAIL=1
            XEPS=0.0
            RETURN
       ENDIF
       IF(EPS.LT.CUMUL(1).OR.EPS.GT.CUMUL(NOUT))THEN
            IFAIL=1
            STATUS='Status: Failed, input out of bounds.'
            IF(LDEBUG)PRINT *,' ++++++ HISINV DEBUG   : Point ',SUM*EPS,
     -           ' located out of bounds: ',CUMUL(0),CUMUL(NCHA(IREF))
            IF(XEPS.LT.CUMUL(1))THEN
                XEPS=X(1)
            ELSE
                XEPS=X(NOUT)
            ENDIF
            RETURN
       ENDIF
*** Plot a few things.
       IF(LDEBUG)THEN
            CALL GRCART(XMIN(IREF),-0.05,XMAX(IREF),1.05,
     -           'x0','P(x < x0)','Inverse interpolation check')
            CALL GRATTS('FUNCTION-1','POLYLINE')
            IF(NCHA(IREF).GT.0)CALL GRLINE(2*NCHA(IREF),XPL,YPL)
            CALL GRATTS('FUNCTION-2','POLYLINE')
            CALL GRLINE(NOUT,X(1),CUMUL(1))
       ENDIF
*** Perform the inverse interpolation.
       XEPS=DIVDIF(X(1),CUMUL(1),NOUT,EPS,IORDER)
       IF(XEPS.LT.XMIN(IREF).OR.XEPS.GT.XMAX(IREF))THEN
            IFAIL=1
            STATUS='Status: Failed, result out of bounds.'
            XEPS=MIN(XMAX(IREF),MAX(XMIN(IREF),XEPS))
       ENDIF
*** Debugging output, if requested.
       IF(LDEBUG)THEN
            XPL(1)=XMIN(IREF)
            YPL(1)=EPS
            XPL(2)=XEPS
            YPL(2)=EPS
            XPL(3)=XEPS
            YPL(3)=-0.05
            CALL GRATTS('COMMENT','POLYLINE')
            CALL GRLINE(3,XPL,YPL)
            WRITE(COMM,'(''Epsilon = '',E12.5)') EPS
            CALL GRCOMM(1,COMM)
            WRITE(COMM,'(''x0 found= '',E12.5)') XEPS
            CALL GRCOMM(2,COMM)
            WRITE(COMM,'(''Interpolation order= '',I2)') IORDER
            CALL GRCOMM(3,COMM)
            CALL GRCOMM(4,STATUS)
            CALL GRNEXT
            CALL GRALOG('Inverse interpolation check.            ')
       ENDIF
       END
