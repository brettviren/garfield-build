CDECK  ID>, BEMCHK.
       SUBROUTINE BEMCHK(NPL,XPL,YPL,ZPL,IFAIL)
*-----------------------------------------------------------------------
*   BEMCHK - Removes duplicate branches from a curve.
*   (Last changed on 16/ 4/10.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER NPL,I,J,IFAIL,NNEW,JCUT
       DOUBLE PRECISION XPL(NPL),YPL(NPL),ZPL(NPL),EPS,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX
       LOGICAL MARK(MXEDGE)
*** Initial settings.
       IFAIL=1
*** Check number of points.
       IF(NPL.GT.MXEDGE)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ BEMCHK DEBUG   :'',
     -           '' Rejected - Too many points.'')')
            RETURN
       ELSEIF(NPL.LT.3)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ BEMCHK DEBUG   :'',
     -           '' Rejected - Too few points.'')')
            RETURN
       ENDIF
*** Set tolerances.
       XMIN=XPL(1)
       XMAX=XPL(1)
       YMIN=YPL(1)
       YMAX=YPL(1)
       ZMIN=ZPL(1)
       ZMAX=ZPL(1)
       DO 10 I=2,NPL
       XMIN=MIN(XMIN,XPL(I))
       XMAX=MAX(XMAX,XPL(I))
       YMIN=MIN(YMIN,YPL(I))
       YMAX=MAX(YMAX,YPL(I))
       ZMIN=MIN(ZMIN,ZPL(I))
       ZMAX=MAX(ZMAX,ZPL(I))
10     CONTINUE
*   Set epsilons accordingly.
       EPS=1.0D-6*(1+ABS(XMAX-XMIN)+ABS(YMAX-YMIN)+ABS(ZMAX-ZMIN))
*** Make a first marker list.
1010   CONTINUE
       DO 20 I=1,NPL
       MARK(I)=.FALSE.
20     CONTINUE
*** Find a point that is surrounded on both side by equal points.
       DO 30 I=1,NPL
       JCUT=0
       DO 40 J=1,NPL/2
       IF((XPL(1+MOD(I+J-1,NPL))-XPL(1+MOD(I-J-1+NPL,NPL)))**2+
     -    (YPL(1+MOD(I+J-1,NPL))-YPL(1+MOD(I-J-1+NPL,NPL)))**2+
     -    (ZPL(1+MOD(I+J-1,NPL))-ZPL(1+MOD(I-J-1+NPL,NPL)))**2.GT.
     -    EPS**2)GOTO 50
       JCUT=J
40     CONTINUE
50     CONTINUE
*   See whether we found one.
       IF(JCUT.GT.0)THEN
      print *,' Cutting a tail of ',JCUT,' points.'
            DO 60 J=I-JCUT+1,I+JCUT
            MARK(1+MOD(J-1+NPL,NPL))=.TRUE.
60          CONTINUE
            GOTO 1000
       ENDIF
30     CONTINUE
*** Check for successive identical points.
       DO 70 I=1,NPL
       IF(  (XPL(I)-XPL(1+MOD(I,NPL)))**2+
     -      (YPL(I)-YPL(1+MOD(I,NPL)))**2+
     -      (ZPL(I)-ZPL(1+MOD(I,NPL)))**2.LT.
     -      EPS**2)THEN
      print *,' Found identical points.'
            MARK(I)=.TRUE.
            GOTO 1000
       ENDIF
70     CONTINUE
*** Done.
       IFAIL=1
       IF(NPL.GT.2)IFAIL=0
       RETURN
*** Eliminate the piece.
1000   CONTINUE
       NNEW=0
       DO 80 I=1,NPL
       IF(MARK(I))GOTO 80
       NNEW=NNEW+1
       XPL(NNEW)=XPL(I)
       YPL(NNEW)=YPL(I)
       ZPL(NNEW)=ZPL(I)
80     CONTINUE
       NPL=NNEW
       GOTO 1010
*** Seems OK
       IFAIL=0
       END
