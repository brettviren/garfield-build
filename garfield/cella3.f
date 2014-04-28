CDECK  ID>, CELLA3.
       SUBROUTINE CELLA3
*-----------------------------------------------------------------------
*   CELLA3 - This routine draws all elements of the cell inside the
*            box (PPXMIN,PPYMIN,PPZMIN) to (PPXMAX,PPYMAX,PPZMAX),
*            taking care of periodicities etc, on the plot being made.
*            Version used for 3D impressions of space.
*   VARIABLES : NXMIN,NXMAX: Numbers of resp first and last x-period.
*               NYMIN,NYMAX:    "    "   "     "    "   "   y   "
*               XPL,YPL    : Used for plotting of lines.
*   (Last changed on  1/12/00.)
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
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL,
     -      BEMSET
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL,BEMSET
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,MEV2KG,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      MEV2KG = 1.782661845E-30,
     -      BOLTZ=1.380658E-23)
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
       DOUBLE PRECISION XPL(5),YPL(5),WW,XPMIN,YPMIN,XPMAX,YPMAX,
     -      X1,Y1,X2,Y2,XX1,YY1,XX2,YY2,SMIN,SMAX
       INTEGER NX,NXMIN,NXMAX,NY,NYMIN,NYMAX,I,ICOL,IFAIL1,NMAX
       PARAMETER(NMAX=100)
*** Determine the number of periods present in the cell.
       NXMIN=0
       NXMAX=0
       NYMIN=0
       NYMAX=0
       IF(PERX)THEN
            NXMIN=INT(GXMIN/SX)-1
            NXMAX=INT(GXMAX/SX)+1
       ENDIF
       IF(PERY)THEN
            NYMIN=INT(GYMIN/SY)-1
            NYMAX=INT(GYMAX/SY)+1
       ENDIF
*** Draw the illuminated x and y-planes, set the representations.
       CALL GRATTS('PLANES','AREA')
       CALL GRATTS('PLANES','POLYLINE')
*   Generate the colour table (shared with the tube).
       IF((YNPLAN(1).OR.YNPLAN(2).OR.YNPLAN(3).OR.YNPLAN(4)).AND.
     -      ICOLPL.EQ.0)THEN
            ICOLPL=ICOL0
            CALL COLSHD(ICOLPL)
            ICOL0=ICOL0+NPRCOL
       ENDIF
*   Ensure the planes do not hide each other.
       IF(YNPLAN(1))THEN
            XPMIN=COPLAN(1)
       ELSE
            XPMIN=GXMIN
       ENDIF
       IF(YNPLAN(2))THEN
            XPMAX=COPLAN(2)
       ELSE
            XPMAX=GXMAX
       ENDIF
       IF(YNPLAN(3))THEN
            YPMIN=COPLAN(3)
       ELSE
            YPMIN=GYMIN
       ENDIF
       IF(YNPLAN(4))THEN
            YPMAX=COPLAN(4)
       ELSE
            YPMAX=GYMAX
       ENDIF
*   The x-planes.
       DO 10 NX=NXMIN,NXMAX
       IF(YNPLAN(1).AND.COPLAN(1)+NX*SX.GE.GXMIN.AND.
     -      COPLAN(1)+NX*SX.LE.GXMAX.AND.FPROJA.GT.0)THEN
            CALL PLACOO(DBLE(COPLAN(1)+NX*SX),YPMIN,GZMIN,XPL(1),YPL(1))
            CALL PLACOO(DBLE(COPLAN(1)+NX*SX),YPMIN,GZMAX,XPL(2),YPL(2))
            CALL PLACOO(DBLE(COPLAN(1)+NX*SX),YPMAX,GZMAX,XPL(3),YPL(3))
            CALL PLACOO(DBLE(COPLAN(1)+NX*SX),YPMAX,GZMIN,XPL(4),YPL(4))
            XPL(5)=XPL(1)
            YPL(5)=YPL(1)
            CALL COLWGT(+1.0D0,0.0D0,0.0D0,WW)
            IF(WW.GE.0)THEN
                 ICOL=ICOLPL+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
            ELSE
                 PRINT *,' !!!!!! CELLA3 WARNING : Request to plot'//
     -                ' a plane seen from the back (program bug).'
                 ICOL=ICOLPL
            ENDIF
            CALL GSFACI(ICOL)
            CALL GFA2(5,XPL,YPL)
            CALL GPL2(5,XPL,YPL)
       ENDIF
       IF(YNPLAN(2).AND.COPLAN(2)+NX*SX.GE.GXMIN.AND.
     -      COPLAN(2)+NX*SX.LE.GXMAX.AND.FPROJA.LT.0)THEN
            CALL PLACOO(DBLE(COPLAN(2)+NX*SX),YPMIN,GZMIN,XPL(1),YPL(1))
            CALL PLACOO(DBLE(COPLAN(2)+NX*SX),YPMIN,GZMAX,XPL(2),YPL(2))
            CALL PLACOO(DBLE(COPLAN(2)+NX*SX),YPMAX,GZMAX,XPL(3),YPL(3))
            CALL PLACOO(DBLE(COPLAN(2)+NX*SX),YPMAX,GZMIN,XPL(4),YPL(4))
            XPL(5)=XPL(1)
            YPL(5)=YPL(1)
            CALL COLWGT(-1.0D0,0.0D0,0.0D0,WW)
            IF(WW.GE.0)THEN
                 ICOL=ICOLPL+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
            ELSE
                 PRINT *,' !!!!!! CELLA3 WARNING : Request to plot'//
     -                ' a plane seen from the back (program bug).'
                 ICOL=ICOLPL
            ENDIF
            CALL GSFACI(ICOL)
            CALL GFA2(5,XPL,YPL)
            CALL GPL2(5,XPL,YPL)
       ENDIF
10     CONTINUE
       DO 20 NY=NYMIN,NYMAX
       IF(YNPLAN(3).AND.COPLAN(3)+NY*SY.GE.GYMIN.AND.
     -      COPLAN(3)+NY*SY.LE.GYMAX.AND.FPROJB.GT.0)THEN
            CALL PLACOO(XPMIN,DBLE(COPLAN(3)+NY*SY),GZMIN,XPL(1),YPL(1))
            CALL PLACOO(XPMIN,DBLE(COPLAN(3)+NY*SY),GZMAX,XPL(2),YPL(2))
            CALL PLACOO(XPMAX,DBLE(COPLAN(3)+NY*SY),GZMAX,XPL(3),YPL(3))
            CALL PLACOO(XPMAX,DBLE(COPLAN(3)+NY*SY),GZMIN,XPL(4),YPL(4))
            XPL(5)=XPL(1)
            YPL(5)=YPL(1)
            CALL COLWGT(0.0D0,+1.0D0,0.0D0,WW)
            IF(WW.GE.0)THEN
                 ICOL=ICOLPL+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
            ELSE
                 PRINT *,' !!!!!! CELLA3 WARNING : Request to plot'//
     -                ' a plane seen from the back (program bug).'
                 ICOL=ICOLPL
            ENDIF
            CALL GSFACI(ICOL)
            CALL GFA2(5,XPL,YPL)
            CALL GPL2(5,XPL,YPL)
       ENDIF
       IF(YNPLAN(4).AND.COPLAN(4)+NY*SY.GE.GYMIN.AND.
     -      COPLAN(4)+NY*SY.LE.GYMAX.AND.FPROJB.LT.0)THEN
            CALL PLACOO(XPMIN,DBLE(COPLAN(4)+NY*SY),GZMIN,XPL(1),YPL(1))
            CALL PLACOO(XPMIN,DBLE(COPLAN(4)+NY*SY),GZMAX,XPL(2),YPL(2))
            CALL PLACOO(XPMAX,DBLE(COPLAN(4)+NY*SY),GZMAX,XPL(3),YPL(3))
            CALL PLACOO(XPMAX,DBLE(COPLAN(4)+NY*SY),GZMIN,XPL(4),YPL(4))
            XPL(5)=XPL(1)
            YPL(5)=YPL(1)
            CALL COLWGT(0.0D0,-1.0D0,0.0D0,WW)
            IF(WW.GE.0)THEN
                 ICOL=ICOLPL+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
            ELSE
                 PRINT *,' !!!!!! CELLA3 WARNING : Request to plot'//
     -                ' a plane seen from the back (program bug).'
                 ICOL=ICOLPL
            ENDIF
            CALL GSFACI(ICOL)
            CALL GFA2(5,XPL,YPL)
            CALL GPL2(5,XPL,YPL)
       ENDIF
20     CONTINUE
*** Draw the illuminated x and y-plane strips, set the representations.
       CALL GRATTS('STRIPS','AREA')
       CALL GRATTS('STRIPS','POLYLINE')
*   Generate the colour table (shared with the tube).
       IF((YNPLAN(1).OR.YNPLAN(2).OR.YNPLAN(3).OR.YNPLAN(4)).AND.
     -      ICOLST.EQ.0)THEN
            ICOLST=ICOL0
            CALL COLSHD(ICOLST)
            ICOL0=ICOL0+NPRCOL
       ENDIF
*   Ensure the planes do not hide each other.
       IF(YNPLAN(1))THEN
            XPMIN=COPLAN(1)
       ELSE
            XPMIN=GXMIN
       ENDIF
       IF(YNPLAN(2))THEN
            XPMAX=COPLAN(2)
       ELSE
            XPMAX=GXMAX
       ENDIF
       IF(YNPLAN(3))THEN
            YPMIN=COPLAN(3)
       ELSE
            YPMIN=GYMIN
       ENDIF
       IF(YNPLAN(4))THEN
            YPMAX=COPLAN(4)
       ELSE
            YPMAX=GYMAX
       ENDIF
*   The x-planes.
       DO 110 NX=NXMIN,NXMAX
       IF(YNPLAN(1).AND.COPLAN(1)+NX*SX.GE.GXMIN.AND.
     -      COPLAN(1)+NX*SX.LE.GXMAX.AND.FPROJA.GT.0)THEN
            DO 130 I=1,NPSTR1(1)
            SMIN=DBLE(PLSTR1(1,I,1))
            SMAX=DBLE(PLSTR1(1,I,2))
            IF(SMAX.LT.GYMIN.OR.SMIN.GT.GYMAX)GOTO 130
            CALL PLACOO(DBLE(COPLAN(1)+NX*SX),SMIN,GZMIN,XPL(1),YPL(1))
            CALL PLACOO(DBLE(COPLAN(1)+NX*SX),SMIN,GZMAX,XPL(2),YPL(2))
            CALL PLACOO(DBLE(COPLAN(1)+NX*SX),SMAX,GZMAX,XPL(3),YPL(3))
            CALL PLACOO(DBLE(COPLAN(1)+NX*SX),SMAX,GZMIN,XPL(4),YPL(4))
            XPL(5)=XPL(1)
            YPL(5)=YPL(1)
            CALL COLWGT(+1.0D0,0.0D0,0.0D0,WW)
            IF(WW.GE.0)THEN
                 ICOL=ICOLST+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
            ELSE
                 PRINT *,' !!!!!! CELLA3 WARNING : Request to plot'//
     -                ' a plane seen from the back (program bug).'
                 ICOL=ICOLST
            ENDIF
            CALL GSFACI(ICOL)
            CALL GFA2(5,XPL,YPL)
            CALL GPL2(5,XPL,YPL)
130         CONTINUE
            DO 140 I=1,NPSTR2(1)
            SMIN=DBLE(PLSTR2(1,I,1))
            SMAX=DBLE(PLSTR2(1,I,2))
            IF(SMAX.LT.GZMIN.OR.SMIN.GT.GZMAX)GOTO 140
            CALL PLACOO(DBLE(COPLAN(1)+NX*SX),YPMIN,SMIN,XPL(1),YPL(1))
            CALL PLACOO(DBLE(COPLAN(1)+NX*SX),YPMIN,SMAX,XPL(2),YPL(2))
            CALL PLACOO(DBLE(COPLAN(1)+NX*SX),YPMAX,SMAX,XPL(3),YPL(3))
            CALL PLACOO(DBLE(COPLAN(1)+NX*SX),YPMAX,SMIN,XPL(4),YPL(4))
            XPL(5)=XPL(1)
            YPL(5)=YPL(1)
            CALL COLWGT(+1.0D0,0.0D0,0.0D0,WW)
            IF(WW.GE.0)THEN
                 ICOL=ICOLST+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
            ELSE
                 PRINT *,' !!!!!! CELLA3 WARNING : Request to plot'//
     -                ' a plane seen from the back (program bug).'
                 ICOL=ICOLST
            ENDIF
            CALL GSFACI(ICOL)
            CALL GFA2(5,XPL,YPL)
            CALL GPL2(5,XPL,YPL)
140         CONTINUE
       ENDIF
       IF(YNPLAN(2).AND.COPLAN(2)+NX*SX.GE.GXMIN.AND.
     -      COPLAN(2)+NX*SX.LE.GXMAX.AND.FPROJA.LT.0)THEN
            DO 150 I=1,NPSTR1(2)
            SMIN=DBLE(PLSTR1(2,I,1))
            SMAX=DBLE(PLSTR1(2,I,2))
            IF(SMAX.LT.GYMIN.OR.SMIN.GT.GYMAX)GOTO 150
            CALL PLACOO(DBLE(COPLAN(2)+NX*SX),SMIN,GZMIN,XPL(1),YPL(1))
            CALL PLACOO(DBLE(COPLAN(2)+NX*SX),SMIN,GZMAX,XPL(2),YPL(2))
            CALL PLACOO(DBLE(COPLAN(2)+NX*SX),SMAX,GZMAX,XPL(3),YPL(3))
            CALL PLACOO(DBLE(COPLAN(2)+NX*SX),SMAX,GZMIN,XPL(4),YPL(4))
            XPL(5)=XPL(1)
            YPL(5)=YPL(1)
            CALL COLWGT(-1.0D0,0.0D0,0.0D0,WW)
            IF(WW.GE.0)THEN
                 ICOL=ICOLST+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
            ELSE
                 PRINT *,' !!!!!! CELLA3 WARNING : Request to plot'//
     -                ' a plane seen from the back (program bug).'
                 ICOL=ICOLST
            ENDIF
            CALL GSFACI(ICOL)
            CALL GFA2(5,XPL,YPL)
            CALL GPL2(5,XPL,YPL)
150         CONTINUE
            DO 160 I=1,NPSTR2(2)
            SMIN=DBLE(PLSTR2(2,I,1))
            SMAX=DBLE(PLSTR2(2,I,2))
            IF(SMAX.LT.GZMIN.OR.SMIN.GT.GZMAX)GOTO 160
            CALL PLACOO(DBLE(COPLAN(2)+NX*SX),YPMIN,SMIN,XPL(1),YPL(1))
            CALL PLACOO(DBLE(COPLAN(2)+NX*SX),YPMIN,SMAX,XPL(2),YPL(2))
            CALL PLACOO(DBLE(COPLAN(2)+NX*SX),YPMAX,SMAX,XPL(3),YPL(3))
            CALL PLACOO(DBLE(COPLAN(2)+NX*SX),YPMAX,SMIN,XPL(4),YPL(4))
            XPL(5)=XPL(1)
            YPL(5)=YPL(1)
            CALL COLWGT(-1.0D0,0.0D0,0.0D0,WW)
            IF(WW.GE.0)THEN
                 ICOL=ICOLST+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
            ELSE
                 PRINT *,' !!!!!! CELLA3 WARNING : Request to plot'//
     -                ' a plane seen from the back (program bug).'
                 ICOL=ICOLST
            ENDIF
            CALL GSFACI(ICOL)
            CALL GFA2(5,XPL,YPL)
            CALL GPL2(5,XPL,YPL)
160         CONTINUE
       ENDIF
110    CONTINUE
       DO 120 NY=NYMIN,NYMAX
       IF(YNPLAN(3).AND.COPLAN(3)+NY*SY.GE.GYMIN.AND.
     -      COPLAN(3)+NY*SY.LE.GYMAX.AND.FPROJB.GT.0)THEN
            DO 170 I=1,NPSTR1(3)
            SMIN=DBLE(PLSTR1(3,I,1))
            SMAX=DBLE(PLSTR1(3,I,2))
            IF(SMAX.LT.GXMIN.OR.SMIN.GT.GXMAX)GOTO 170
            CALL PLACOO(SMIN,DBLE(COPLAN(3)+NY*SY),GZMIN,XPL(1),YPL(1))
            CALL PLACOO(SMIN,DBLE(COPLAN(3)+NY*SY),GZMAX,XPL(2),YPL(2))
            CALL PLACOO(SMAX,DBLE(COPLAN(3)+NY*SY),GZMAX,XPL(3),YPL(3))
            CALL PLACOO(SMAX,DBLE(COPLAN(3)+NY*SY),GZMIN,XPL(4),YPL(4))
            XPL(5)=XPL(1)
            YPL(5)=YPL(1)
            CALL COLWGT(0.0D0,+1.0D0,0.0D0,WW)
            IF(WW.GE.0)THEN
                 ICOL=ICOLST+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
            ELSE
                 PRINT *,' !!!!!! CELLA3 WARNING : Request to plot'//
     -                ' a plane seen from the back (program bug).'
                 ICOL=ICOLST
            ENDIF
            CALL GSFACI(ICOL)
            CALL GFA2(5,XPL,YPL)
            CALL GPL2(5,XPL,YPL)
170         CONTINUE
            DO 180 I=1,NPSTR2(3)
            SMIN=DBLE(PLSTR2(3,I,1))
            SMAX=DBLE(PLSTR2(3,I,2))
            IF(SMAX.LT.GZMIN.OR.SMIN.GT.GZMAX)GOTO 180
            CALL PLACOO(XPMIN,DBLE(COPLAN(3)+NY*SY),SMIN,XPL(1),YPL(1))
            CALL PLACOO(XPMIN,DBLE(COPLAN(3)+NY*SY),SMAX,XPL(2),YPL(2))
            CALL PLACOO(XPMAX,DBLE(COPLAN(3)+NY*SY),SMAX,XPL(3),YPL(3))
            CALL PLACOO(XPMAX,DBLE(COPLAN(3)+NY*SY),SMIN,XPL(4),YPL(4))
            XPL(5)=XPL(1)
            YPL(5)=YPL(1)
            CALL COLWGT(0.0D0,+1.0D0,0.0D0,WW)
            IF(WW.GE.0)THEN
                 ICOL=ICOLST+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
            ELSE
                 PRINT *,' !!!!!! CELLA3 WARNING : Request to plot'//
     -                ' a plane seen from the back (program bug).'
                 ICOL=ICOLST
            ENDIF
            CALL GSFACI(ICOL)
            CALL GFA2(5,XPL,YPL)
            CALL GPL2(5,XPL,YPL)
180         CONTINUE
       ENDIF
       IF(YNPLAN(4).AND.COPLAN(4)+NY*SY.GE.GYMIN.AND.
     -      COPLAN(4)+NY*SY.LE.GYMAX.AND.FPROJB.LT.0)THEN
            DO 190 I=1,NPSTR1(4)
            SMIN=DBLE(PLSTR1(4,I,1))
            SMAX=DBLE(PLSTR1(4,I,2))
            IF(SMAX.LT.GXMIN.OR.SMIN.GT.GXMAX)GOTO 190
            CALL PLACOO(SMIN,DBLE(COPLAN(4)+NY*SY),GZMIN,XPL(1),YPL(1))
            CALL PLACOO(SMIN,DBLE(COPLAN(4)+NY*SY),GZMAX,XPL(2),YPL(2))
            CALL PLACOO(SMAX,DBLE(COPLAN(4)+NY*SY),GZMAX,XPL(3),YPL(3))
            CALL PLACOO(SMAX,DBLE(COPLAN(4)+NY*SY),GZMIN,XPL(4),YPL(4))
            XPL(5)=XPL(1)
            YPL(5)=YPL(1)
            CALL COLWGT(0.0D0,-1.0D0,0.0D0,WW)
            IF(WW.GE.0)THEN
                 ICOL=ICOLST+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
            ELSE
                 PRINT *,' !!!!!! CELLA3 WARNING : Request to plot'//
     -                ' a plane seen from the back (program bug).'
                 ICOL=ICOLST
            ENDIF
            CALL GSFACI(ICOL)
            CALL GFA2(5,XPL,YPL)
            CALL GPL2(5,XPL,YPL)
190         CONTINUE
            DO 200 I=1,NPSTR2(4)
            SMIN=DBLE(PLSTR2(4,I,1))
            SMAX=DBLE(PLSTR2(4,I,2))
            IF(SMAX.LT.GZMIN.OR.SMIN.GT.GZMAX)GOTO 200
            CALL PLACOO(XPMIN,DBLE(COPLAN(4)+NY*SY),SMIN,XPL(1),YPL(1))
            CALL PLACOO(XPMIN,DBLE(COPLAN(4)+NY*SY),SMAX,XPL(2),YPL(2))
            CALL PLACOO(XPMAX,DBLE(COPLAN(4)+NY*SY),SMAX,XPL(3),YPL(3))
            CALL PLACOO(XPMAX,DBLE(COPLAN(4)+NY*SY),SMIN,XPL(4),YPL(4))
            XPL(5)=XPL(1)
            YPL(5)=YPL(1)
            CALL COLWGT(0.0D0,-1.0D0,0.0D0,WW)
            IF(WW.GE.0)THEN
                 ICOL=ICOLST+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
            ELSE
                 PRINT *,' !!!!!! CELLA3 WARNING : Request to plot'//
     -                ' a plane seen from the back (program bug).'
                 ICOL=ICOLST
            ENDIF
            CALL GSFACI(ICOL)
            CALL GFA2(5,XPL,YPL)
            CALL GPL2(5,XPL,YPL)
200         CONTINUE
       ENDIF
120    CONTINUE
*** Draw the illuminated parts of the tube.
       IF(TUBE)THEN
*   Set the representations.
            CALL GRATTS('TUBE','POLYLINE')
            CALL GRATTS('TUBE','AREA')
*   Generate the colour table (shared with the planes).
            IF(ICOLPL.EQ.0)THEN
                 ICOLPL=ICOL0
                 CALL COLSHD(ICOLPL)
                 ICOL0=ICOL0+NPRCOL
            ENDIF
*   Case of a polygon.
            IF(NTUBE.GT.0)THEN
                 X1=COTUBE*COS(2.0D0*PI*DBLE(0)/DBLE(NTUBE))
                 Y1=COTUBE*SIN(2.0D0*PI*DBLE(0)/DBLE(NTUBE))
                 DO 50 I=1,NTUBE
                 X2=COTUBE*COS(2.0D0*PI*DBLE(I)/DBLE(NTUBE))
                 Y2=COTUBE*SIN(2.0D0*PI*DBLE(I)/DBLE(NTUBE))
                 XX1=X1
                 YY1=Y1
                 XX2=X2
                 YY2=Y2
                 CALL CLIP2D(XX1,YY1,XX2,YY2,GXMIN,GYMIN,
     -                GXMAX,GYMAX,IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      X1=X2
                      Y1=Y2
                      GOTO 50
                 ENDIF
                 CALL PLACOO(XX1,YY1,GZMIN,XPL(1),YPL(1))
                 CALL PLACOO(XX1,YY1,GZMAX,XPL(2),YPL(2))
                 CALL PLACOO(XX2,YY2,GZMAX,XPL(3),YPL(3))
                 CALL PLACOO(XX2,YY2,GZMIN,XPL(4),YPL(4))
                 XPL(5)=XPL(1)
                 YPL(5)=YPL(1)
                 CALL COLWGT(-X1-X2,-Y1-Y2,0.0D0,WW)
                 IF(WW.GE.0)THEN
                      ICOL=ICOLPL+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
                      CALL GSFACI(ICOL)
                      CALL GFA2(5,XPL,YPL)
                      CALL GPL2(5,XPL,YPL)
                 ENDIF
                 X1=X2
                 Y1=Y2
50               CONTINUE
*   Case of a cylinder.
            ELSE
                 X1=COTUBE*COS(2.0D0*PI*DBLE(0)/DBLE(NMAX))
                 Y1=COTUBE*SIN(2.0D0*PI*DBLE(0)/DBLE(NMAX))
                 DO 70 I=1,NMAX
                 X2=COTUBE*COS(2.0D0*PI*DBLE(I)/DBLE(NMAX))
                 Y2=COTUBE*SIN(2.0D0*PI*DBLE(I)/DBLE(NMAX))
                 XX1=X1
                 YY1=Y1
                 XX2=X2
                 YY2=Y2
                 CALL CLIP2D(XX1,YY1,XX2,YY2,GXMIN,GYMIN,
     -                GXMAX,GYMAX,IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      X1=X2
                      Y1=Y2
                      GOTO 70
                 ENDIF
                 CALL PLACOO(XX1,YY1,GZMIN,XPL(1),YPL(1))
                 CALL PLACOO(XX1,YY1,GZMAX,XPL(2),YPL(2))
                 CALL PLACOO(XX2,YY2,GZMAX,XPL(3),YPL(3))
                 CALL PLACOO(XX2,YY2,GZMIN,XPL(4),YPL(4))
                 XPL(5)=XPL(1)
                 YPL(5)=YPL(1)
                 CALL COLWGT(-X1-X2,-Y1-Y2,0.0D0,WW)
                 IF(WW.GE.0)THEN
                      ICOL=ICOLPL+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
                      CALL GSFACI(ICOL)
                      CALL GFA2(5,XPL,YPL)
                 ENDIF
                 CALL GPL2(2,XPL(2),YPL(2))
                 CALL GPL2(2,XPL(4),YPL(4))
                 X1=X2
                 Y1=Y2
70               CONTINUE
            ENDIF
       ENDIF
*** Plot the solids.
       CALL PLAPLT
*** Draw the parts of the tube seen from the outside.
       IF(TUBE.AND.LFULLT)THEN
*   Set the representations.
            CALL GRATTS('TUBE','POLYLINE')
*   Case of a polygon.
            IF(NTUBE.GT.0)THEN
                 X1=COTUBE*COS(2.0D0*PI*DBLE(0)/DBLE(NTUBE))
                 Y1=COTUBE*SIN(2.0D0*PI*DBLE(0)/DBLE(NTUBE))
                 DO 60 I=1,NTUBE
                 X2=COTUBE*COS(2.0D0*PI*DBLE(I)/DBLE(NTUBE))
                 Y2=COTUBE*SIN(2.0D0*PI*DBLE(I)/DBLE(NTUBE))
                 XX1=X1
                 YY1=Y1
                 XX2=X2
                 YY2=Y2
                 CALL CLIP2D(XX1,YY1,XX2,YY2,GXMIN,GYMIN,
     -                GXMAX,GYMAX,IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      X1=X2
                      Y1=Y2
                      GOTO 60
                 ENDIF
                 CALL PLACOO(XX1,YY1,GZMIN,XPL(1),YPL(1))
                 CALL PLACOO(XX1,YY1,GZMAX,XPL(2),YPL(2))
                 CALL PLACOO(XX2,YY2,GZMAX,XPL(3),YPL(3))
                 CALL PLACOO(XX2,YY2,GZMIN,XPL(4),YPL(4))
                 XPL(5)=XPL(1)
                 YPL(5)=YPL(1)
                 CALL COLWGT(-X1-X2,-Y1-Y2,0.0D0,WW)
                 IF(WW.LT.0)CALL GPL2(5,XPL,YPL)
                 X1=X2
                 Y1=Y2
60               CONTINUE
*   Case of a cylinder.
            ELSE
                 X1=COTUBE*COS(2.0D0*PI*DBLE(0)/DBLE(NMAX))
                 Y1=COTUBE*SIN(2.0D0*PI*DBLE(0)/DBLE(NMAX))
                 DO 80 I=1,NMAX
                 X2=COTUBE*COS(2.0D0*PI*DBLE(I)/DBLE(NMAX))
                 Y2=COTUBE*SIN(2.0D0*PI*DBLE(I)/DBLE(NMAX))
                 XX1=X1
                 YY1=Y1
                 XX2=X2
                 YY2=Y2
                 CALL CLIP2D(XX1,YY1,XX2,YY2,GXMIN,GYMIN,
     -                GXMAX,GYMAX,IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      X1=X2
                      Y1=Y2
                      GOTO 80
                 ENDIF
                 CALL COLWGT(-X1-X2,-Y1-Y2,0.0D0,WW)
                 IF(WW.LT.0)THEN
                      CALL PLACOO(XX1,YY1,GZMIN,XPL(1),YPL(1))
                      CALL PLACOO(XX2,YY2,GZMIN,XPL(2),YPL(2))
                      CALL GPL2(2,XPL,YPL)
                      CALL PLACOO(XX1,YY1,GZMAX,XPL(1),YPL(1))
                      CALL PLACOO(XX2,YY2,GZMAX,XPL(2),YPL(2))
                      CALL GPL2(2,XPL,YPL)
                 ENDIF
                 X1=X2
                 Y1=Y2
80               CONTINUE
            ENDIF
       ENDIF
*** Second pass of the x and y-planes, set the representations.
       IF(LFULLP)THEN
*   Set the representation.
            CALL GRATTS('PLANES','POLYLINE')
*   The x-planes.
            DO 30 NX=NXMIN,NXMAX
            IF(YNPLAN(1).AND.COPLAN(1)+NX*SX.GE.GXMIN.AND.
     -           COPLAN(1)+NX*SX.LE.GXMAX.AND.FPROJA.LE.0)THEN
                 CALL PLACOO(DBLE(COPLAN(1)+NX*SX),YPMIN,GZMIN,
     -                XPL(1),YPL(1))
                 CALL PLACOO(DBLE(COPLAN(1)+NX*SX),YPMIN,GZMAX,
     -                XPL(2),YPL(2))
                 CALL PLACOO(DBLE(COPLAN(1)+NX*SX),YPMAX,GZMAX,
     -                XPL(3),YPL(3))
                 CALL PLACOO(DBLE(COPLAN(1)+NX*SX),YPMAX,GZMIN,
     -                XPL(4),YPL(4))
                 XPL(5)=XPL(1)
                 YPL(5)=YPL(1)
                 CALL GPL2(5,XPL,YPL)
            ENDIF
            IF(YNPLAN(2).AND.COPLAN(2)+NX*SX.GE.GXMIN.AND.
     -           COPLAN(2)+NX*SX.LE.GXMAX.AND.FPROJA.GE.0)THEN
                 CALL PLACOO(DBLE(COPLAN(2)+NX*SX),YPMIN,GZMIN,
     -                XPL(1),YPL(1))
                 CALL PLACOO(DBLE(COPLAN(2)+NX*SX),YPMIN,GZMAX,
     -                XPL(2),YPL(2))
                 CALL PLACOO(DBLE(COPLAN(2)+NX*SX),YPMAX,GZMAX,
     -                XPL(3),YPL(3))
                 CALL PLACOO(DBLE(COPLAN(2)+NX*SX),YPMAX,GZMIN,
     -                XPL(4),YPL(4))
                 XPL(5)=XPL(1)
                 YPL(5)=YPL(1)
                 CALL GPL2(5,XPL,YPL)
            ENDIF
30          CONTINUE
            DO 40 NY=NYMIN,NYMAX
            IF(YNPLAN(3).AND.COPLAN(3)+NY*SY.GE.GYMIN.AND.
     -           COPLAN(3)+NY*SY.LE.GYMAX.AND.FPROJB.LE.0)THEN
                 CALL PLACOO(XPMIN,DBLE(COPLAN(3)+NY*SY),GZMIN,
     -                XPL(1),YPL(1))
                 CALL PLACOO(XPMIN,DBLE(COPLAN(3)+NY*SY),GZMAX,
     -                XPL(2),YPL(2))
                 CALL PLACOO(XPMAX,DBLE(COPLAN(3)+NY*SY),GZMAX,
     -                XPL(3),YPL(3))
                 CALL PLACOO(XPMAX,DBLE(COPLAN(3)+NY*SY),GZMIN,
     -                XPL(4),YPL(4))
                 XPL(5)=XPL(1)
                 YPL(5)=YPL(1)
                 CALL GPL2(5,XPL,YPL)
            ENDIF
            IF(YNPLAN(4).AND.COPLAN(4)+NY*SY.GE.GYMIN.AND.
     -           COPLAN(4)+NY*SY.LE.GYMAX.AND.FPROJB.GE.0)THEN
                 CALL PLACOO(XPMIN,DBLE(COPLAN(4)+NY*SY),GZMIN,
     -                XPL(1),YPL(1))
                 CALL PLACOO(XPMIN,DBLE(COPLAN(4)+NY*SY),GZMAX,
     -                XPL(2),YPL(2))
                 CALL PLACOO(XPMAX,DBLE(COPLAN(4)+NY*SY),GZMAX,
     -                XPL(3),YPL(3))
                 CALL PLACOO(XPMAX,DBLE(COPLAN(4)+NY*SY),GZMIN,
     -                XPL(4),YPL(4))
                 XPL(5)=XPL(1)
                 YPL(5)=YPL(1)
                 CALL GPL2(5,XPL,YPL)
            ENDIF
40          CONTINUE
**  Plot the strips, set the representation.
            CALL GRATTS('STRIPS','POLYLINE')
*   The x-planes.
            DO 210 NX=NXMIN,NXMAX
            IF(YNPLAN(1).AND.COPLAN(1)+NX*SX.GE.GXMIN.AND.
     -           COPLAN(1)+NX*SX.LE.GXMAX.AND.FPROJA.LE.0)THEN
                 DO 230 I=1,NPSTR1(1)
                 SMIN=DBLE(PLSTR1(1,I,1))
                 SMAX=DBLE(PLSTR1(1,I,2))
                 IF(SMAX.LT.GYMIN.OR.SMIN.GT.GYMAX)GOTO 230
                 CALL PLACOO(DBLE(COPLAN(1)+NX*SX),SMIN,GZMIN,
     -                XPL(1),YPL(1))
                 CALL PLACOO(DBLE(COPLAN(1)+NX*SX),SMIN,GZMAX,
     -                XPL(2),YPL(2))
                 CALL PLACOO(DBLE(COPLAN(1)+NX*SX),SMAX,GZMAX,
     -                XPL(3),YPL(3))
                 CALL PLACOO(DBLE(COPLAN(1)+NX*SX),SMAX,GZMIN,
     -                XPL(4),YPL(4))
                 XPL(5)=XPL(1)
                 YPL(5)=YPL(1)
                 CALL GPL2(5,XPL,YPL)
230              CONTINUE
                 DO 240 I=1,NPSTR2(1)
                 SMIN=DBLE(PLSTR2(1,I,1))
                 SMAX=DBLE(PLSTR2(1,I,2))
                 IF(SMAX.LT.GZMIN.OR.SMIN.GT.GZMAX)GOTO 240
                 CALL PLACOO(DBLE(COPLAN(1)+NX*SX),YPMIN,SMIN,
     -                XPL(1),YPL(1))
                 CALL PLACOO(DBLE(COPLAN(1)+NX*SX),YPMIN,SMAX,
     -                XPL(2),YPL(2))
                 CALL PLACOO(DBLE(COPLAN(1)+NX*SX),YPMAX,SMAX,
     -                XPL(3),YPL(3))
                 CALL PLACOO(DBLE(COPLAN(1)+NX*SX),YPMAX,SMIN,
     -                XPL(4),YPL(4))
                 XPL(5)=XPL(1)
                 YPL(5)=YPL(1)
                 CALL GPL2(5,XPL,YPL)
240              CONTINUE
            ENDIF
            IF(YNPLAN(2).AND.COPLAN(2)+NX*SX.GE.GXMIN.AND.
     -           COPLAN(2)+NX*SX.LE.GXMAX.AND.FPROJA.GE.0)THEN
                 DO 250 I=1,NPSTR1(2)
                 SMIN=DBLE(PLSTR1(2,I,1))
                 SMAX=DBLE(PLSTR1(2,I,2))
                 IF(SMAX.LT.GYMIN.OR.SMIN.GT.GYMAX)GOTO 250
                 CALL PLACOO(DBLE(COPLAN(2)+NX*SX),SMIN,GZMIN,
     -                XPL(1),YPL(1))
                 CALL PLACOO(DBLE(COPLAN(2)+NX*SX),SMIN,GZMAX,
     -                XPL(2),YPL(2))
                 CALL PLACOO(DBLE(COPLAN(2)+NX*SX),SMAX,GZMAX,
     -                XPL(3),YPL(3))
                 CALL PLACOO(DBLE(COPLAN(2)+NX*SX),SMAX,GZMIN,
     -                XPL(4),YPL(4))
                 XPL(5)=XPL(1)
                 YPL(5)=YPL(1)
                 CALL GPL2(5,XPL,YPL)
250              CONTINUE
                 DO 260 I=1,NPSTR2(2)
                 SMIN=DBLE(PLSTR2(2,I,1))
                 SMAX=DBLE(PLSTR2(2,I,2))
                 IF(SMAX.LT.GZMIN.OR.SMIN.GT.GZMAX)GOTO 260
                 CALL PLACOO(DBLE(COPLAN(2)+NX*SX),YPMIN,SMIN,
     -                XPL(1),YPL(1))
                 CALL PLACOO(DBLE(COPLAN(2)+NX*SX),YPMIN,SMAX,
     -                XPL(2),YPL(2))
                 CALL PLACOO(DBLE(COPLAN(2)+NX*SX),YPMAX,SMAX,
     -                XPL(3),YPL(3))
                 CALL PLACOO(DBLE(COPLAN(2)+NX*SX),YPMAX,SMIN,
     -                XPL(4),YPL(4))
                 XPL(5)=XPL(1)
                 YPL(5)=YPL(1)
                 CALL GPL2(5,XPL,YPL)
260              CONTINUE
            ENDIF
210         CONTINUE
            DO 220 NY=NYMIN,NYMAX
            IF(YNPLAN(3).AND.COPLAN(3)+NY*SY.GE.GYMIN.AND.
     -           COPLAN(3)+NY*SY.LE.GYMAX.AND.FPROJB.LE.0)THEN
                 DO 270 I=1,NPSTR1(3)
                 SMIN=DBLE(PLSTR1(3,I,1))
                 SMAX=DBLE(PLSTR1(3,I,2))
                 IF(SMAX.LT.GXMIN.OR.SMIN.GT.GXMAX)GOTO 270
                 CALL PLACOO(SMIN,DBLE(COPLAN(3)+NY*SY),GZMIN,
     -                XPL(1),YPL(1))
                 CALL PLACOO(SMIN,DBLE(COPLAN(3)+NY*SY),GZMAX,
     -                XPL(2),YPL(2))
                 CALL PLACOO(SMAX,DBLE(COPLAN(3)+NY*SY),GZMAX,
     -                XPL(3),YPL(3))
                 CALL PLACOO(SMAX,DBLE(COPLAN(3)+NY*SY),GZMIN,
     -                XPL(4),YPL(4))
                 XPL(5)=XPL(1)
                 YPL(5)=YPL(1)
                 CALL GPL2(5,XPL,YPL)
270              CONTINUE
                 DO 280 I=1,NPSTR2(3)
                 SMIN=DBLE(PLSTR2(3,I,1))
                 SMAX=DBLE(PLSTR2(3,I,2))
                 IF(SMAX.LT.GZMIN.OR.SMIN.GT.GZMAX)GOTO 280
                 CALL PLACOO(XPMIN,DBLE(COPLAN(3)+NY*SY),SMIN,
     -                XPL(1),YPL(1))
                 CALL PLACOO(XPMIN,DBLE(COPLAN(3)+NY*SY),SMAX,
     -                XPL(2),YPL(2))
                 CALL PLACOO(XPMAX,DBLE(COPLAN(3)+NY*SY),SMAX,
     -                XPL(3),YPL(3))
                 CALL PLACOO(XPMAX,DBLE(COPLAN(3)+NY*SY),SMIN,
     -                XPL(4),YPL(4))
                 XPL(5)=XPL(1)
                 YPL(5)=YPL(1)
                 CALL GPL2(5,XPL,YPL)
280              CONTINUE
            ENDIF
            IF(YNPLAN(4).AND.COPLAN(4)+NY*SY.GE.GYMIN.AND.
     -           COPLAN(4)+NY*SY.LE.GYMAX.AND.FPROJB.GE.0)THEN
                 DO 290 I=1,NPSTR1(4)
                 SMIN=DBLE(PLSTR1(4,I,1))
                 SMAX=DBLE(PLSTR1(4,I,2))
                 IF(SMAX.LT.GXMIN.OR.SMIN.GT.GXMAX)GOTO 290
                 CALL PLACOO(SMIN,DBLE(COPLAN(4)+NY*SY),GZMIN,
     -                XPL(1),YPL(1))
                 CALL PLACOO(SMIN,DBLE(COPLAN(4)+NY*SY),GZMAX,
     -                XPL(2),YPL(2))
                 CALL PLACOO(SMAX,DBLE(COPLAN(4)+NY*SY),GZMAX,
     -                XPL(3),YPL(3))
                 CALL PLACOO(SMAX,DBLE(COPLAN(4)+NY*SY),GZMIN,
     -                XPL(4),YPL(4))
                 XPL(5)=XPL(1)
                 YPL(5)=YPL(1)
                 CALL GPL2(5,XPL,YPL)
290              CONTINUE
                 DO 300 I=1,NPSTR2(4)
                 SMIN=DBLE(PLSTR2(4,I,1))
                 SMAX=DBLE(PLSTR2(4,I,2))
                 IF(SMAX.LT.GZMIN.OR.SMIN.GT.GZMAX)GOTO 300
                 CALL PLACOO(XPMIN,DBLE(COPLAN(4)+NY*SY),SMIN,
     -                XPL(1),YPL(1))
                 CALL PLACOO(XPMIN,DBLE(COPLAN(4)+NY*SY),SMAX,
     -                XPL(2),YPL(2))
                 CALL PLACOO(XPMAX,DBLE(COPLAN(4)+NY*SY),SMAX,
     -                XPL(3),YPL(3))
                 CALL PLACOO(XPMAX,DBLE(COPLAN(4)+NY*SY),SMIN,
     -                XPL(4),YPL(4))
                 XPL(5)=XPL(1)
                 YPL(5)=YPL(1)
                 CALL GPL2(5,XPL,YPL)
300              CONTINUE
            ENDIF
220         CONTINUE
       ENDIF
       END
