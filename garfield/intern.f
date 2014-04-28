CDECK  ID>, INTERN.
       SUBROUTINE INTERN(NPL,XPL,YPL,X,Y,INSIDE,EDGE)
*-----------------------------------------------------------------------
*   INTERN - Determines whether the point (X,Y) is located inside of the
*            polygon (XPL,YPL).
*   (Last changed on  6/10/00.)
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
       DOUBLE PRECISION WGT,FPRMAT,
     -      FPROJ,FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX,GYBOX,GZBOX
       REAL PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM
       INTEGER NLINED,NGRIDX,NGRIDY,ITRTYP,NTRLIN,NTRSAM,INDPOS,NCTRW,
     -      NTRFLX,NINORD,
     -      NCPNAM,NCXLAB,NCYLAB,NCFPRO,IPRMAT,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,ITFSRM,NTRERR
       LOGICAL LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG,LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       COMMON /PARMS / WGT(MXLIST),FPRMAT(3,3),
     -      FPROJ(3,3),FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX(12),GYBOX(12),GZBOX(12),
     -      PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM,
     -      INDPOS(11000),IPRMAT(3),NCTRW,NCPNAM,
     -      ITRTYP,NTRLIN,NTRSAM,NTRFLX,ITFSRM,NTRERR(10),
     -      NLINED,NINORD,NGRIDX,NGRIDY,NCXLAB,NCYLAB,NCFPRO,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,
     -      LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG(10),LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       CHARACTER*80 PARTID,PXLAB,PYLAB,PROLAB
       CHARACTER*10 PNAME
       CHARACTER*5  PRVIEW
       CHARACTER*(MXCHAR) FCNTRW
       COMMON /PARCHR/ PARTID,FCNTRW,PNAME,PXLAB,PYLAB,PROLAB,PRVIEW
       INTEGER NPL,NITER,I,J,NCROSS
       REAL XPL(NPL),YPL(NPL),X,Y,XINF,YINF,XMAX,YMAX,XMIN,YMIN,RNDUNI,
     -      EPSX,EPSY
       LOGICAL CROSS,ONLINE,INSIDE,EDGE
       EXTERNAL CROSS,ONLINE,RNDUNI
*** Initial settings.
       INSIDE=.FALSE.
       EDGE=.FALSE.
*** Special treatment for few points.
       IF(NPL.LT.2)THEN
            RETURN
       ELSEIF(NPL.EQ.2)THEN
            EDGE=ONLINE(XPL(1),YPL(1),XPL(2),YPL(2),X,Y)
            RETURN
       ENDIF
*** Determine the range of the data.
       XMIN=XPL(1)
       YMIN=YPL(1)
       XMAX=XPL(1)
       YMAX=YPL(1)
       DO 10 I=2,NPL
       XMIN=MIN(XMIN,XPL(I))
       YMIN=MIN(YMIN,YPL(I))
       XMAX=MAX(XMAX,XPL(I))
       YMAX=MAX(YMAX,YPL(I))
10     CONTINUE
*** Set tolerances.
       IF(LEPSG)THEN
            EPSX=EPSGX
            EPSY=EPSGY
       ELSE
            EPSX=1.0E-5*MAX(ABS(XMIN),ABS(XMAX))
            EPSY=1.0E-5*MAX(ABS(YMIN),ABS(YMAX))
            IF(EPSX.LE.0)EPSX=1.0E-5
            IF(EPSY.LE.0)EPSY=1.0E-5
       ENDIF
*** Ensure that we have a range.
       IF(ABS(XMAX-XMIN).LE.EPSX)THEN
            IF(Y.GE.YMIN-EPSY.AND.Y.LE.YMAX+EPSY.AND.
     -           ABS(XMAX+XMIN-2*X).LE.EPSX)THEN
                 EDGE=.TRUE.
            ELSE
                 EDGE=.FALSE.
            ENDIF
            RETURN
       ELSEIF(ABS(YMAX-YMIN).LE.EPSY)THEN
            IF(X.GE.XMIN-EPSX.AND.X.LE.XMAX+EPSX.AND.
     -           ABS(YMAX+YMIN-2*Y).LE.EPSY)THEN
                 EDGE=.TRUE.
            ELSE
                 EDGE=.FALSE.
            ENDIF
            RETURN
       ENDIF
*** Choose a point at "infinity".
       XINF=XMIN-ABS(XMAX-XMIN)
       YINF=YMIN-ABS(YMAX-YMIN)
*** Loop over the edges counting intersections.
       NITER=0
20     CONTINUE
       NCROSS=0
       DO 30 J=1,NPL
*   Flag points located on one of the edges.
       IF(ONLINE(XPL(1+MOD(J-1,NPL)),YPL(1+MOD(J-1,NPL)),
     -      XPL(1+MOD(J,NPL)),YPL(1+MOD(J,NPL)),X,Y))THEN
            EDGE=.TRUE.
            RETURN
       ENDIF
*   Count mid-line intersects.
       IF(CROSS(X,Y,XINF,YINF,
     -      XPL(1+MOD(J-1,NPL)),YPL(1+MOD(J-1,NPL)),
     -      XPL(1+MOD(J  ,NPL)),YPL(1+MOD(J  ,NPL))))NCROSS=NCROSS+1
*   Ensure that the testing line doesn't cross a corner.
       IF(ONLINE(X,Y,XINF,YINF,XPL(J),YPL(J)))THEN
            XINF=XMIN-RNDUNI(1.0)*ABS(XMAX-XINF)
            YINF=YMIN+RNDUNI(-1.0)*ABS(YMAX-YINF)
            NITER=NITER+1
            IF(NITER.LT.100)GOTO 20
            PRINT *,' !!!!!! INTERN WARNING : Unable to verify'//
     -           ' whether a point is internal; setting to "edge".'
            INSIDE=.FALSE.
            EDGE=.TRUE.
*   Produce a dump if requested.
            IF(LGSTOP)THEN
                 OPEN(UNIT=12,FILE='intern.dat',STATUS='UNKNOWN')
                 WRITE(12,*) EPSGX,EPSGY,EPSGZ,LEPSG
                 WRITE(12,*) X,Y
                 WRITE(12,*) NPL
                 DO 40 I=1,NPL
                 WRITE(12,*) I,XPL(I),YPL(I)
40               CONTINUE
                 CLOSE(12)
                 PRINT *,' ------ INTERN MESSAGE : Dump produced;'//
     -                ' terminating program execution.'
                 CALL QUIT
            ENDIF
            RETURN
       ENDIF
30     CONTINUE
*** Set the INSIDE flag.
       IF(NCROSS.NE.2*(NCROSS/2))INSIDE=.TRUE.
       END
