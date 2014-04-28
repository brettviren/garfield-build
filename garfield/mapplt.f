CDECK  ID>, MAPPLT.
       SUBROUTINE MAPPLT(PPXMIN,PPYMIN,PPZMIN,PPXMAX,PPYMAX,PPZMAX)
*-----------------------------------------------------------------------
*   MAPPLT - Plots the materials.
*   (Last changed on 15/ 7/08.)
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
       REAL EXMAP,EYMAP,EZMAP,VMAP,EWXMAP,EWYMAP,EWZMAP,VWMAP,
     -      BXMAP,BYMAP,BZMAP,
     -      XMAP,YMAP,ZMAP,XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,
     -      VMMIN,VMMAX,EPSMAT,EPSSUR,XFMOFF,YFMOFF,ZFMOFF
       INTEGER MATMAP,NMAP,NEPS,MAPORD,MAPTYP,IDRMAT,INDEWS,
     -      NWMAP
       LOGICAL MAPFLG,LMAPPL,SETAX,SETAY,SETAZ,ELMDGN,LSFDER
       CHARACTER EWSTYP
       CHARACTER*10 MATSRC
       COMMON /FLDMAP/ VMAP(MXMAP,10),VWMAP(MXMAP,10,MXWMAP),
     -      EXMAP(MXMAP,10),EYMAP(MXMAP,10),EZMAP(MXMAP,10),
     -      EWXMAP(MXMAP,10,MXWMAP),EWYMAP(MXMAP,10,MXWMAP),
     -      EWZMAP(MXMAP,10,MXWMAP),
     -      BXMAP(MXMAP,10),BYMAP(MXMAP,10),BZMAP(MXMAP,10),
     -      XMAP(MXMAP,10),YMAP(MXMAP,10),ZMAP(MXMAP,10),
     -      XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,VMMIN,VMMAX,
     -      XFMOFF,YFMOFF,ZFMOFF,
     -      EPSMAT(MXEPS),EPSSUR(MXEPS),MATMAP(MXMAP),
     -      NMAP,NEPS,MAPORD,MAPTYP,IDRMAT,INDEWS(MXWMAP),NWMAP,
     -      MAPFLG(10+4*MXWMAP),ELMDGN(MXMAP),
     -      LMAPPL,SETAX,SETAY,SETAZ,LSFDER
       COMMON /FLDCHR/ EWSTYP(MXWMAP),MATSRC
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
       REAL PPXMIN,PPYMIN,PPZMIN,PPXMAX,PPYMAX,PPZMAX,
     -      XPL(20),YPL(20)
       DOUBLE PRECISION X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,X5,Y5,Z5,
     -      X6,Y6,Z6,X7,Y7,Z7,X8,Y8,Z8,XCUT,YCUT
       INTEGER I,NX,NXMIN,NXMAX,NY,NYMIN,NYMAX,NZ,NZMIN,NZMAX,NPL
       LOGICAL CUT,CROSS,IN1,IN2,IN3,IN4,IN5,IN6,IN7,IN8
       EXTERNAL CROSS
*** Don't do anything if the material map is not present.
       IF(.NOT.MAPFLG(9).OR..NOT.LMAPPL)RETURN
*** 2D maps make only sense in a z-projection.
       IF((MAPTYP.EQ.1.OR.MAPTYP.EQ.2.OR.MAPTYP.EQ.3).AND.
     -      ABS(FPROJC).LT.0.999*FPROJN)RETURN
*** Set the tolerances for butterfly elimination.
       CALL EPSSET('SET',1.0D-6*ABS(PPXMAX-PPXMIN),
     -      1.0D-6*ABS(PPYMAX-PPYMIN),1.0D-6*ABS(PPZMAX-PPZMIN))
*** Determine the number of periods present in the cell.
       NXMIN=0
       NXMAX=0
       NYMIN=0
       NYMAX=0
       NZMIN=0
       NZMAX=0
       IF(PERX.OR.PERMX)THEN
            NXMIN=INT(PPXMIN/SX)-1
            NXMAX=INT(PPXMAX/SX)+1
       ENDIF
       IF(PERY.OR.PERMY)THEN
            NYMIN=INT(PPYMIN/SY)-1
            NYMAX=INT(PPYMAX/SY)+1
       ENDIF
       IF(PERZ.OR.PERMZ)THEN
            NZMIN=INT(PPZMIN/SZ)-1
            NZMAX=INT(PPZMAX/SZ)+1
       ENDIF
*** Loop over the elements.
       DO 10 I=1,NMAP
*   Skip the drift medium.
       IF(MATMAP(I).EQ.IDRMAT.OR.MATMAP(I).EQ.-1)GOTO 10
**  Triangular maps.
       IF(MAPTYP.EQ.1.OR.MAPTYP.EQ.2.OR.MAPTYP.EQ.3)THEN
*   Loop over the periods, if present.
            DO 20 NX=NXMIN,NXMAX
            DO 30 NY=NYMIN,NYMAX
*   Determine corners of the triangle.
            IF(PERMX.AND.NX.NE.2*(NX/2))THEN
                 XPL(1)=XMMIN+XMMAX-XMAP(I,1)+NX*SX
                 XPL(2)=XMMIN+XMMAX-XMAP(I,2)+NX*SX
                 XPL(3)=XMMIN+XMMAX-XMAP(I,3)+NX*SX
                 XPL(4)=XMMIN+XMMAX-XMAP(I,1)+NX*SX
            ELSE
                 XPL(1)=XMAP(I,1)+NX*SX
                 XPL(2)=XMAP(I,2)+NX*SX
                 XPL(3)=XMAP(I,3)+NX*SX
                 XPL(4)=XMAP(I,1)+NX*SX
            ENDIF
            IF(PERMY.AND.NY.NE.2*(NY/2))THEN
                 YPL(1)=YMMIN+YMMAX-YMAP(I,1)+NY*SY
                 YPL(2)=YMMIN+YMMAX-YMAP(I,2)+NY*SY
                 YPL(3)=YMMIN+YMMAX-YMAP(I,3)+NY*SY
                 YPL(4)=YMMIN+YMMAX-YMAP(I,1)+NY*SY
            ELSE
                 YPL(1)=YMAP(I,1)+NY*SY
                 YPL(2)=YMAP(I,2)+NY*SY
                 YPL(3)=YMAP(I,3)+NY*SY
                 YPL(4)=YMAP(I,1)+NY*SY
            ENDIF
*   Plot the various media.
            IF(MATMAP(I).EQ.1)THEN
                 CALL GRATTS('MATERIAL-1','AREA')
                 CALL GRCONV(4,XPL,YPL)
            ELSEIF(MATMAP(I).EQ.2)THEN
                 CALL GRATTS('MATERIAL-2','AREA')
                 CALL GRCONV(4,XPL,YPL)
            ELSEIF(MATMAP(I).EQ.3)THEN
                 CALL GRATTS('MATERIAL-3','AREA')
                 CALL GRCONV(4,XPL,YPL)
            ELSEIF(MATMAP(I).EQ.4)THEN
                 CALL GRATTS('MATERIAL-4','AREA')
                 CALL GRCONV(4,XPL,YPL)
            ELSE
                 CALL GRATTS('MATERIAL-5','AREA')
                 CALL GRCONV(4,XPL,YPL)
            ENDIF
*   Next medium.
30          CONTINUE
20          CONTINUE
**  Quadrilateral linear maps.
       ELSEIF(MAPTYP.EQ.4)THEN
*   Loop over the periods, if present.
            DO 40 NX=NXMIN,NXMAX
            DO 50 NY=NYMIN,NYMAX
*   Determine corners of the triangle.
            IF(PERMX.AND.NX.NE.2*(NX/2))THEN
                 XPL(1)=XMMIN+XMMAX-XMAP(I,1)+NX*SX
                 XPL(2)=XMMIN+XMMAX-XMAP(I,2)+NX*SX
                 XPL(3)=XMMIN+XMMAX-XMAP(I,3)+NX*SX
                 XPL(4)=XMMIN+XMMAX-XMAP(I,4)+NX*SX
                 XPL(5)=XMMIN+XMMAX-XMAP(I,1)+NX*SX
            ELSE
                 XPL(1)=XMAP(I,1)+NX*SX
                 XPL(2)=XMAP(I,2)+NX*SX
                 XPL(3)=XMAP(I,3)+NX*SX
                 XPL(4)=XMAP(I,4)+NX*SX
                 XPL(5)=XMAP(I,1)+NX*SX
            ENDIF
            IF(PERMY.AND.NY.NE.2*(NY/2))THEN
                 YPL(1)=YMMIN+YMMAX-YMAP(I,1)+NY*SY
                 YPL(2)=YMMIN+YMMAX-YMAP(I,2)+NY*SY
                 YPL(3)=YMMIN+YMMAX-YMAP(I,3)+NY*SY
                 YPL(4)=YMMIN+YMMAX-YMAP(I,4)+NY*SY
                 YPL(5)=YMMIN+YMMAX-YMAP(I,1)+NY*SY
            ELSE
                 YPL(1)=YMAP(I,1)+NY*SY
                 YPL(2)=YMAP(I,2)+NY*SY
                 YPL(3)=YMAP(I,3)+NY*SY
                 YPL(4)=YMAP(I,4)+NY*SY
                 YPL(5)=YMAP(I,1)+NY*SY
            ENDIF
*   Plot the various media.
            IF(MATMAP(I).EQ.1)THEN
                 CALL GRATTS('MATERIAL-1','AREA')
                 CALL GRCONV(5,XPL,YPL)
            ELSEIF(MATMAP(I).EQ.2)THEN
                 CALL GRATTS('MATERIAL-2','AREA')
                 CALL GRCONV(5,XPL,YPL)
            ELSEIF(MATMAP(I).EQ.3)THEN
                 CALL GRATTS('MATERIAL-3','AREA')
                 CALL GRCONV(5,XPL,YPL)
            ELSEIF(MATMAP(I).EQ.4)THEN
                 CALL GRATTS('MATERIAL-4','AREA')
                 CALL GRCONV(5,XPL,YPL)
            ELSE
                 CALL GRATTS('MATERIAL-5','AREA')
                 CALL GRCONV(5,XPL,YPL)
            ENDIF
*   Next medium.
50          CONTINUE
40          CONTINUE
**  Quadrilateral 8-node "serendipity" maps.
       ELSEIF(MAPTYP.EQ.5)THEN
*   Loop over the periods, if present.
            DO 60 NX=NXMIN,NXMAX
            DO 70 NY=NYMIN,NYMAX
*   Determine corners of the triangle.
            IF(PERMX.AND.NX.NE.2*(NX/2))THEN
                 XPL(1)=XMMIN+XMMAX-XMAP(I,1)+NX*SX
                 XPL(2)=XMMIN+XMMAX-XMAP(I,5)+NX*SX
                 XPL(3)=XMMIN+XMMAX-XMAP(I,2)+NX*SX
                 XPL(4)=XMMIN+XMMAX-XMAP(I,6)+NX*SX
                 XPL(5)=XMMIN+XMMAX-XMAP(I,3)+NX*SX
                 XPL(6)=XMMIN+XMMAX-XMAP(I,7)+NX*SX
                 XPL(7)=XMMIN+XMMAX-XMAP(I,4)+NX*SX
                 XPL(8)=XMMIN+XMMAX-XMAP(I,8)+NX*SX
                 XPL(9)=XMMIN+XMMAX-XMAP(I,1)+NX*SX
            ELSE
                 XPL(1)=XMAP(I,1)+NX*SX
                 XPL(2)=XMAP(I,5)+NX*SX
                 XPL(3)=XMAP(I,2)+NX*SX
                 XPL(4)=XMAP(I,6)+NX*SX
                 XPL(5)=XMAP(I,3)+NX*SX
                 XPL(6)=XMAP(I,7)+NX*SX
                 XPL(7)=XMAP(I,4)+NX*SX
                 XPL(8)=XMAP(I,8)+NX*SX
                 XPL(9)=XMAP(I,1)+NX*SX
            ENDIF
            IF(PERMY.AND.NY.NE.2*(NY/2))THEN
                 YPL(1)=YMMIN+YMMAX-YMAP(I,1)+NY*SY
                 YPL(2)=YMMIN+YMMAX-YMAP(I,5)+NY*SY
                 YPL(3)=YMMIN+YMMAX-YMAP(I,2)+NY*SY
                 YPL(4)=YMMIN+YMMAX-YMAP(I,6)+NY*SY
                 YPL(5)=YMMIN+YMMAX-YMAP(I,3)+NY*SY
                 YPL(6)=YMMIN+YMMAX-YMAP(I,7)+NY*SY
                 YPL(7)=YMMIN+YMMAX-YMAP(I,4)+NY*SY
                 YPL(8)=YMMIN+YMMAX-YMAP(I,8)+NY*SY
                 YPL(9)=YMMIN+YMMAX-YMAP(I,1)+NY*SY
            ELSE
                 YPL(1)=YMAP(I,1)+NY*SY
                 YPL(2)=YMAP(I,5)+NY*SY
                 YPL(3)=YMAP(I,2)+NY*SY
                 YPL(4)=YMAP(I,6)+NY*SY
                 YPL(5)=YMAP(I,3)+NY*SY
                 YPL(6)=YMAP(I,7)+NY*SY
                 YPL(7)=YMAP(I,4)+NY*SY
                 YPL(8)=YMAP(I,8)+NY*SY
                 YPL(9)=YMAP(I,1)+NY*SY
            ENDIF
*   Plot the various media.
            IF(MATMAP(I).EQ.1)THEN
                 CALL GRATTS('MATERIAL-1','AREA')
                 CALL GRCONV(9,XPL,YPL)
            ELSEIF(MATMAP(I).EQ.2)THEN
                 CALL GRATTS('MATERIAL-2','AREA')
                 CALL GRCONV(9,XPL,YPL)
            ELSEIF(MATMAP(I).EQ.3)THEN
                 CALL GRATTS('MATERIAL-3','AREA')
                 CALL GRCONV(9,XPL,YPL)
            ELSEIF(MATMAP(I).EQ.4)THEN
                 CALL GRATTS('MATERIAL-4','AREA')
                 CALL GRCONV(9,XPL,YPL)
            ELSE
                 CALL GRATTS('MATERIAL-5','AREA')
                 CALL GRCONV(9,XPL,YPL)
            ENDIF
*   Next medium.
70          CONTINUE
60          CONTINUE
**  Tetrahedral maps.
       ELSEIF(MAPTYP.EQ.11.OR.MAPTYP.EQ.12.OR.MAPTYP.EQ.13)THEN
*   Loop over the x-periods, determine corners of tetrahedrons.
            DO 120 NX=NXMIN,NXMAX
            IF(PERMX.AND.NX.NE.2*(NX/2))THEN
                 X1=XMMIN+XMMAX-XMAP(I,1)+NX*SX
                 X2=XMMIN+XMMAX-XMAP(I,2)+NX*SX
                 X3=XMMIN+XMMAX-XMAP(I,3)+NX*SX
                 X4=XMMIN+XMMAX-XMAP(I,4)+NX*SX
            ELSE
                 X1=XMAP(I,1)+NX*SX
                 X2=XMAP(I,2)+NX*SX
                 X3=XMAP(I,3)+NX*SX
                 X4=XMAP(I,4)+NX*SX
            ENDIF
*   Loop over the y-periods, determine corners of tetrahedrons.
            DO 130 NY=NYMIN,NYMAX
            IF(PERMY.AND.NY.NE.2*(NY/2))THEN
                 Y1=YMMIN+YMMAX-YMAP(I,1)+NY*SY
                 Y2=YMMIN+YMMAX-YMAP(I,2)+NY*SY
                 Y3=YMMIN+YMMAX-YMAP(I,3)+NY*SY
                 Y4=YMMIN+YMMAX-YMAP(I,4)+NY*SY
            ELSE
                 Y1=YMAP(I,1)+NY*SY
                 Y2=YMAP(I,2)+NY*SY
                 Y3=YMAP(I,3)+NY*SY
                 Y4=YMAP(I,4)+NY*SY
            ENDIF
*   Loop over the z-periods, determine corners of tetrahedrons.
            DO 140 NZ=NZMIN,NZMAX
            IF(PERMZ.AND.NZ.NE.2*(NZ/2))THEN
                 Z1=ZMMIN+ZMMAX-ZMAP(I,1)+NZ*SZ
                 Z2=ZMMIN+ZMMAX-ZMAP(I,2)+NZ*SZ
                 Z3=ZMMIN+ZMMAX-ZMAP(I,3)+NZ*SZ
                 Z4=ZMMIN+ZMMAX-ZMAP(I,4)+NZ*SZ
            ELSE
                 Z1=ZMAP(I,1)+NZ*SZ
                 Z2=ZMAP(I,2)+NZ*SZ
                 Z3=ZMAP(I,3)+NZ*SZ
                 Z4=ZMAP(I,4)+NZ*SZ
            ENDIF
*   See whether the edges are in the plane.
            IN1=.FALSE.
            IN2=.FALSE.
            IN3=.FALSE.
            IN4=.FALSE.
            IF(ABS(FPROJA*X1+FPROJB*Y1+FPROJC*Z1-FPROJD).LT.
     -           1.0E-4*MAX(ABS(X1),ABS(Y1),ABS(Z1),ABS(FPROJA),
     -           ABS(FPROJB),ABS(FPROJC),ABS(FPROJD)))IN1=.TRUE.
            IF(ABS(FPROJA*X2+FPROJB*Y2+FPROJC*Z2-FPROJD).LT.
     -           1.0E-4*MAX(ABS(X2),ABS(Y2),ABS(Z2),ABS(FPROJA),
     -           ABS(FPROJB),ABS(FPROJC),ABS(FPROJD)))IN2=.TRUE.
            IF(ABS(FPROJA*X3+FPROJB*Y3+FPROJC*Z3-FPROJD).LT.
     -           1.0E-4*MAX(ABS(X3),ABS(Y3),ABS(Z3),ABS(FPROJA),
     -           ABS(FPROJB),ABS(FPROJC),ABS(FPROJD)))IN3=.TRUE.
            IF(ABS(FPROJA*X4+FPROJB*Y4+FPROJC*Z4-FPROJD).LT.
     -           1.0E-4*MAX(ABS(X4),ABS(Y4),ABS(Z4),ABS(FPROJA),
     -           ABS(FPROJB),ABS(FPROJC),ABS(FPROJD)))IN4=.TRUE.
*   Add those of the 4 corners that are in the plane.
            NPL=0
            IF(IN1)THEN
                 CALL PLACOO(X1,Y1,Z1,XCUT,YCUT)
                 NPL=NPL+1
                 XPL(NPL)=XCUT
                 YPL(NPL)=YCUT
            ENDIF
            IF(IN2)THEN
                 CALL PLACOO(X2,Y2,Z2,XCUT,YCUT)
                 NPL=NPL+1
                 XPL(NPL)=XCUT
                 YPL(NPL)=YCUT
            ENDIF
            IF(IN3)THEN
                 CALL PLACOO(X3,Y3,Z3,XCUT,YCUT)
                 NPL=NPL+1
                 XPL(NPL)=XCUT
                 YPL(NPL)=YCUT
            ENDIF
            IF(IN4)THEN
                 CALL PLACOO(X4,Y4,Z4,XCUT,YCUT)
                 NPL=NPL+1
                 XPL(NPL)=XCUT
                 YPL(NPL)=YCUT
            ENDIF
*   Cut the 6 edges with the viewing plane.
            IF(.NOT.(IN1.OR.IN2))THEN
                 CALL PLACUT(X1,Y1,Z1,X2,Y2,Z2,XCUT,YCUT,CUT)
                 IF(CUT)THEN
                      NPL=NPL+1
                      XPL(NPL)=XCUT
                      YPL(NPL)=YCUT
                 ENDIF
            ENDIF
            IF(.NOT.(IN1.OR.IN3))THEN
                 CALL PLACUT(X1,Y1,Z1,X3,Y3,Z3,XCUT,YCUT,CUT)
                 IF(CUT)THEN
                      NPL=NPL+1
                      XPL(NPL)=XCUT
                      YPL(NPL)=YCUT
                 ENDIF
            ENDIF
            IF(.NOT.(IN1.OR.IN4))THEN
                 CALL PLACUT(X1,Y1,Z1,X4,Y4,Z4,XCUT,YCUT,CUT)
                 IF(CUT)THEN
                      NPL=NPL+1
                      XPL(NPL)=XCUT
                      YPL(NPL)=YCUT
                 ENDIF
            ENDIF
            IF(.NOT.(IN2.OR.IN3))THEN
                 CALL PLACUT(X2,Y2,Z2,X3,Y3,Z3,XCUT,YCUT,CUT)
                 IF(CUT)THEN
                      NPL=NPL+1
                      XPL(NPL)=XCUT
                      YPL(NPL)=YCUT
                 ENDIF
            ENDIF
            IF(.NOT.(IN2.OR.IN4))THEN
                 CALL PLACUT(X2,Y2,Z2,X4,Y4,Z4,XCUT,YCUT,CUT)
                 IF(CUT)THEN
                      NPL=NPL+1
                      XPL(NPL)=XCUT
                      YPL(NPL)=YCUT
                 ENDIF
            ENDIF
            IF(.NOT.(IN3.OR.IN4))THEN
                 CALL PLACUT(X3,Y3,Z3,X4,Y4,Z4,XCUT,YCUT,CUT)
                 IF(CUT)THEN
                      NPL=NPL+1
                      XPL(NPL)=XCUT
                      YPL(NPL)=YCUT
                 ENDIF
            ENDIF
*   Plot the various media.
            IF(NPL.GE.3)THEN
                 NPL=NPL+1
                 XPL(NPL)=XPL(1)
                 YPL(NPL)=YPL(1)
                 IF(MATMAP(I).EQ.1)THEN
                      CALL GRATTS('MATERIAL-1','AREA')
                      CALL GRCONV(NPL,XPL,YPL)
                 ELSEIF(MATMAP(I).EQ.2)THEN
                      CALL GRATTS('MATERIAL-2','AREA')
                      CALL GRCONV(NPL,XPL,YPL)
                 ELSEIF(MATMAP(I).EQ.3)THEN
                      CALL GRATTS('MATERIAL-3','AREA')
                      CALL GRCONV(NPL,XPL,YPL)
                 ELSEIF(MATMAP(I).EQ.4)THEN
                      CALL GRATTS('MATERIAL-4','AREA')
                      CALL GRCONV(NPL,XPL,YPL)
                 ELSE
                      CALL GRATTS('MATERIAL-5','AREA')
                      CALL GRCONV(NPL,XPL,YPL)
                 ENDIF
            ENDIF
*   Next periods.
140         CONTINUE
130         CONTINUE
120         CONTINUE
**  Hexahedral maps.
       ELSEIF(MAPTYP.EQ.14.OR.MAPTYP.EQ.15.OR.MAPTYP.EQ.16)THEN
*   Loop over the x-periods, determine corners of hexahedrons.
            DO 150 NX=NXMIN,NXMAX
            IF(PERMX.AND.NX.NE.2*(NX/2))THEN
                 X1=XMMIN+XMMAX-XMAP(I,1)+NX*SX
                 X2=X1-(XMAP(I,2)-XMAP(I,1))
                 X4=X1-(XMAP(I,3)-XMAP(I,1))
                 X3=X2+X4-X1
                 X5=X1-(XMAP(I,4)-XMAP(I,1))
                 X6=X2-(XMAP(I,4)-XMAP(I,1))
                 X7=X3-(XMAP(I,4)-XMAP(I,1))
                 X8=X4-(XMAP(I,4)-XMAP(I,1))
            ELSE
                 X1=XMAP(I,1)+NX*SX
                 X2=X1+(XMAP(I,2)-XMAP(I,1))
                 X4=X1+(XMAP(I,3)-XMAP(I,1))
                 X3=X2+X4-X1
                 X5=X1+(XMAP(I,4)-XMAP(I,1))
                 X6=X2+(XMAP(I,4)-XMAP(I,1))
                 X7=X3+(XMAP(I,4)-XMAP(I,1))
                 X8=X4+(XMAP(I,4)-XMAP(I,1))
            ENDIF
*   Loop over the y-periods, determine corners of tetrahedrons.
            DO 160 NY=NYMIN,NYMAX
            IF(PERMY.AND.NY.NE.2*(NY/2))THEN
                 Y1=YMMIN+YMMAX-YMAP(I,1)+NY*SY
                 Y2=Y1-(YMAP(I,2)-YMAP(I,1))
                 Y4=Y1-(YMAP(I,3)-YMAP(I,1))
                 Y3=Y2+Y4-Y1
                 Y5=Y1-(YMAP(I,4)-YMAP(I,1))
                 Y6=Y2-(YMAP(I,4)-YMAP(I,1))
                 Y7=Y3-(YMAP(I,4)-YMAP(I,1))
                 Y8=Y4-(YMAP(I,4)-YMAP(I,1))
            ELSE
                 Y1=YMAP(I,1)+NY*SY
                 Y2=Y1+(YMAP(I,2)-YMAP(I,1))
                 Y4=Y1+(YMAP(I,3)-YMAP(I,1))
                 Y3=Y2+Y4-Y1
                 Y5=Y1+(YMAP(I,4)-YMAP(I,1))
                 Y6=Y2+(YMAP(I,4)-YMAP(I,1))
                 Y7=Y3+(YMAP(I,4)-YMAP(I,1))
                 Y8=Y4+(YMAP(I,4)-YMAP(I,1))
            ENDIF
*   Loop over the z-periods, determine corners of tetrahedrons.
            DO 170 NZ=NZMIN,NZMAX
            IF(PERMZ.AND.NZ.NE.2*(NZ/2))THEN
                 Z1=ZMMIN+ZMMAX-ZMAP(I,1)+NZ*SZ
                 Z2=Z1-(ZMAP(I,2)-ZMAP(I,1))
                 Z4=Z1-(ZMAP(I,3)-ZMAP(I,1))
                 Z3=Z2+Z4-Z1
                 Z5=Z1-(ZMAP(I,4)-ZMAP(I,1))
                 Z6=Z2-(ZMAP(I,4)-ZMAP(I,1))
                 Z7=Z3-(ZMAP(I,4)-ZMAP(I,1))
                 Z8=Z4-(ZMAP(I,4)-ZMAP(I,1))
            ELSE
                 Z1=ZMAP(I,1)+NZ*SZ
                 Z2=Z1+(ZMAP(I,2)-ZMAP(I,1))
                 Z4=Z1+(ZMAP(I,3)-ZMAP(I,1))
                 Z3=Z2+Z4-Z1
                 Z5=Z1+(ZMAP(I,4)-ZMAP(I,1))
                 Z6=Z2+(ZMAP(I,4)-ZMAP(I,1))
                 Z7=Z3+(ZMAP(I,4)-ZMAP(I,1))
                 Z8=Z4+(ZMAP(I,4)-ZMAP(I,1))
            ENDIF
*   See whether the edges are in the plane.
            IN1=.FALSE.
            IN2=.FALSE.
            IN3=.FALSE.
            IN4=.FALSE.
            IN5=.FALSE.
            IN6=.FALSE.
            IN7=.FALSE.
            IN8=.FALSE.
            IF(ABS(FPROJA*X1+FPROJB*Y1+FPROJC*Z1-FPROJD).LT.
     -           1.0E-4*MAX(ABS(X1),ABS(Y1),ABS(Z1),ABS(FPROJA),
     -           ABS(FPROJB),ABS(FPROJC),ABS(FPROJD)))IN1=.TRUE.
            IF(ABS(FPROJA*X2+FPROJB*Y2+FPROJC*Z2-FPROJD).LT.
     -           1.0E-4*MAX(ABS(X2),ABS(Y2),ABS(Z2),ABS(FPROJA),
     -           ABS(FPROJB),ABS(FPROJC),ABS(FPROJD)))IN2=.TRUE.
            IF(ABS(FPROJA*X3+FPROJB*Y3+FPROJC*Z3-FPROJD).LT.
     -           1.0E-4*MAX(ABS(X3),ABS(Y3),ABS(Z3),ABS(FPROJA),
     -           ABS(FPROJB),ABS(FPROJC),ABS(FPROJD)))IN3=.TRUE.
            IF(ABS(FPROJA*X4+FPROJB*Y4+FPROJC*Z4-FPROJD).LT.
     -           1.0E-4*MAX(ABS(X4),ABS(Y4),ABS(Z4),ABS(FPROJA),
     -           ABS(FPROJB),ABS(FPROJC),ABS(FPROJD)))IN4=.TRUE.
            IF(ABS(FPROJA*X5+FPROJB*Y5+FPROJC*Z5-FPROJD).LT.
     -           1.0E-4*MAX(ABS(X5),ABS(Y5),ABS(Z5),ABS(FPROJA),
     -           ABS(FPROJB),ABS(FPROJC),ABS(FPROJD)))IN5=.TRUE.
            IF(ABS(FPROJA*X6+FPROJB*Y6+FPROJC*Z6-FPROJD).LT.
     -           1.0E-4*MAX(ABS(X6),ABS(Y6),ABS(Z6),ABS(FPROJA),
     -           ABS(FPROJB),ABS(FPROJC),ABS(FPROJD)))IN6=.TRUE.
            IF(ABS(FPROJA*X7+FPROJB*Y7+FPROJC*Z7-FPROJD).LT.
     -           1.0E-4*MAX(ABS(X7),ABS(Y7),ABS(Z7),ABS(FPROJA),
     -           ABS(FPROJB),ABS(FPROJC),ABS(FPROJD)))IN7=.TRUE.
            IF(ABS(FPROJA*X8+FPROJB*Y8+FPROJC*Z8-FPROJD).LT.
     -           1.0E-4*MAX(ABS(X8),ABS(Y8),ABS(Z8),ABS(FPROJA),
     -           ABS(FPROJB),ABS(FPROJC),ABS(FPROJD)))IN8=.TRUE.
*   Add those of the 8 corners that are in the plane.
            NPL=0
            IF(IN1)THEN
                 CALL PLACOO(X1,Y1,Z1,XCUT,YCUT)
                 NPL=NPL+1
                 XPL(NPL)=XCUT
                 YPL(NPL)=YCUT
            ENDIF
            IF(IN2)THEN
                 CALL PLACOO(X2,Y2,Z2,XCUT,YCUT)
                 NPL=NPL+1
                 XPL(NPL)=XCUT
                 YPL(NPL)=YCUT
            ENDIF
            IF(IN3)THEN
                 CALL PLACOO(X3,Y3,Z3,XCUT,YCUT)
                 NPL=NPL+1
                 XPL(NPL)=XCUT
                 YPL(NPL)=YCUT
            ENDIF
            IF(IN4)THEN
                 CALL PLACOO(X4,Y4,Z4,XCUT,YCUT)
                 NPL=NPL+1
                 XPL(NPL)=XCUT
                 YPL(NPL)=YCUT
            ENDIF
            IF(IN5)THEN
                 CALL PLACOO(X5,Y5,Z5,XCUT,YCUT)
                 NPL=NPL+1
                 XPL(NPL)=XCUT
                 YPL(NPL)=YCUT
            ENDIF
            IF(IN6)THEN
                 CALL PLACOO(X6,Y6,Z6,XCUT,YCUT)
                 NPL=NPL+1
                 XPL(NPL)=XCUT
                 YPL(NPL)=YCUT
            ENDIF
            IF(IN7)THEN
                 CALL PLACOO(X7,Y7,Z7,XCUT,YCUT)
                 NPL=NPL+1
                 XPL(NPL)=XCUT
                 YPL(NPL)=YCUT
            ENDIF
            IF(IN8)THEN
                 CALL PLACOO(X8,Y8,Z8,XCUT,YCUT)
                 NPL=NPL+1
                 XPL(NPL)=XCUT
                 YPL(NPL)=YCUT
            ENDIF
*   Cut the 12 edges with the viewing plane.
            IF(.NOT.(IN1.OR.IN2))THEN
                 CALL PLACUT(X1,Y1,Z1,X2,Y2,Z2,XCUT,YCUT,CUT)
                 IF(CUT)THEN
                      NPL=NPL+1
                      XPL(NPL)=XCUT
                      YPL(NPL)=YCUT
                 ENDIF
            ENDIF
            IF(.NOT.(IN2.OR.IN3))THEN
                 CALL PLACUT(X2,Y2,Z2,X3,Y3,Z3,XCUT,YCUT,CUT)
                 IF(CUT)THEN
                      NPL=NPL+1
                      XPL(NPL)=XCUT
                      YPL(NPL)=YCUT
                 ENDIF
            ENDIF
            IF(.NOT.(IN3.OR.IN4))THEN
                 CALL PLACUT(X3,Y3,Z3,X4,Y4,Z4,XCUT,YCUT,CUT)
                 IF(CUT)THEN
                      NPL=NPL+1
                      XPL(NPL)=XCUT
                      YPL(NPL)=YCUT
                 ENDIF
            ENDIF
            IF(.NOT.(IN4.OR.IN1))THEN
                 CALL PLACUT(X4,Y4,Z4,X1,Y1,Z1,XCUT,YCUT,CUT)
                 IF(CUT)THEN
                      NPL=NPL+1
                      XPL(NPL)=XCUT
                      YPL(NPL)=YCUT
                 ENDIF
            ENDIF
            IF(.NOT.(IN5.OR.IN6))THEN
                 CALL PLACUT(X5,Y5,Z5,X6,Y6,Z6,XCUT,YCUT,CUT)
                 IF(CUT)THEN
                      NPL=NPL+1
                      XPL(NPL)=XCUT
                      YPL(NPL)=YCUT
                 ENDIF
            ENDIF
            IF(.NOT.(IN6.OR.IN7))THEN
                 CALL PLACUT(X6,Y6,Z6,X7,Y7,Z7,XCUT,YCUT,CUT)
                 IF(CUT)THEN
                      NPL=NPL+1
                      XPL(NPL)=XCUT
                      YPL(NPL)=YCUT
                 ENDIF
            ENDIF
            IF(.NOT.(IN7.OR.IN8))THEN
                 CALL PLACUT(X7,Y7,Z7,X8,Y8,Z8,XCUT,YCUT,CUT)
                 IF(CUT)THEN
                      NPL=NPL+1
                      XPL(NPL)=XCUT
                      YPL(NPL)=YCUT
                 ENDIF
            ENDIF
            IF(.NOT.(IN8.OR.IN5))THEN
                 CALL PLACUT(X8,Y8,Z8,X5,Y5,Z5,XCUT,YCUT,CUT)
                 IF(CUT)THEN
                      NPL=NPL+1
                      XPL(NPL)=XCUT
                      YPL(NPL)=YCUT
                 ENDIF
            ENDIF
            IF(.NOT.(IN1.OR.IN5))THEN
                 CALL PLACUT(X1,Y1,Z1,X5,Y5,Z5,XCUT,YCUT,CUT)
                 IF(CUT)THEN
                      NPL=NPL+1
                      XPL(NPL)=XCUT
                      YPL(NPL)=YCUT
                 ENDIF
            ENDIF
            IF(.NOT.(IN2.OR.IN6))THEN
                 CALL PLACUT(X2,Y2,Z2,X6,Y6,Z6,XCUT,YCUT,CUT)
                 IF(CUT)THEN
                      NPL=NPL+1
                      XPL(NPL)=XCUT
                      YPL(NPL)=YCUT
                 ENDIF
            ENDIF
            IF(.NOT.(IN3.OR.IN7))THEN
                 CALL PLACUT(X3,Y3,Z3,X7,Y7,Z7,XCUT,YCUT,CUT)
                 IF(CUT)THEN
                      NPL=NPL+1
                      XPL(NPL)=XCUT
                      YPL(NPL)=YCUT
                 ENDIF
            ENDIF
            IF(.NOT.(IN4.OR.IN8))THEN
                 CALL PLACUT(X4,Y4,Z4,X8,Y8,Z8,XCUT,YCUT,CUT)
                 IF(CUT)THEN
                      NPL=NPL+1
                      XPL(NPL)=XCUT
                      YPL(NPL)=YCUT
                 ENDIF
            ENDIF
*   Plot the various media.
            IF(NPL.GE.3)THEN
                 NPL=NPL+1
                 XPL(NPL)=XPL(1)
                 YPL(NPL)=YPL(1)
                 IF(MATMAP(I).EQ.1)THEN
                      CALL GRATTS('MATERIAL-1','AREA')
                      CALL GRCONV(NPL,XPL,YPL)
                 ELSEIF(MATMAP(I).EQ.2)THEN
                      CALL GRATTS('MATERIAL-2','AREA')
                      CALL GRCONV(NPL,XPL,YPL)
                 ELSEIF(MATMAP(I).EQ.3)THEN
                      CALL GRATTS('MATERIAL-3','AREA')
                      CALL GRCONV(NPL,XPL,YPL)
                 ELSEIF(MATMAP(I).EQ.4)THEN
                      CALL GRATTS('MATERIAL-4','AREA')
                      CALL GRCONV(NPL,XPL,YPL)
                 ELSE
                      CALL GRATTS('MATERIAL-5','AREA')
                      CALL GRCONV(NPL,XPL,YPL)
                 ENDIF
            ENDIF
*   Next periods.
170         CONTINUE
160         CONTINUE
150         CONTINUE
       ENDIF
*   Next element.
10     CONTINUE
*** Reset the tolerances.
       CALL EPSSET('RESET',0.0D0,0.0D0,0.0D0)
       END
