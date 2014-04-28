CDECK  ID>, DLCSTA.
       SUBROUTINE DLCSTA(Q,ITYPE)
*-----------------------------------------------------------------------
*   DLCSTA - Subroutine returning the status of a drift line. It checks
*            that the particle is not inside or near a wire or a plane.
*            If that is the case however, the drift line is finished and
*            a non-zero status code is returned.
*   VARIABLES : XLAST,YLAST: Last particle position in basic cell.
*               SHIFT      : .TRUE. if we are not in the basic period.
*               XW,YW      : Wire position moved to the particle period.
*               DIST2      : Minimum distance of particle during the
*                            last step to a given wire squared.
*   (Last changed on 16/12/09.)
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
       DOUBLE PRECISION XU,YU,ZU,TU,XTARG,YTARG,TMC,DMC
       REAL DXMIN,DYMIN,DZMIN,DXMAX,DYMAX,DZMAX,DTARG,EPSDFI,EPSTWI,
     -      EPSATI,RDF2,DSCMIN,DSCMAX,DTFACT,
     -      DDXMIN,DDXMAX,DDYMIN,DDYMAX,DDZMIN,DDZMAX,EPSDIF,RTRAP,
     -      STMAX,EQTTHR,EQTASP,EQTCLS,QPCHAR
       INTEGER NU,ISTAT,ITARG,MXDIFS,MXTWNS,MXATTS,MDF2,
     -      ISTAT1,ISTAT2,ISTAT3,ISTAT4,ISTAT5,ISTAT6,NMC,MCMETH,
     -      IPTYPE,IPTECH
       LOGICAL LREPSK,LKINK,LSTMAX,LEQSRT,LEQCRS,LEQMRK,LAVPRO
       COMMON /DRFDAT/ XU(MXLIST),YU(MXLIST),ZU(MXLIST),TU(MXLIST),
     -      XTARG,YTARG,TMC,DMC,DTARG,
     -      DXMIN,DYMIN,DZMIN,DXMAX,DYMAX,DZMAX,
     -      DDXMIN,DDXMAX,DDYMIN,DDYMAX,DDZMIN,DDZMAX,
     -      EQTTHR,EQTASP,EQTCLS,QPCHAR,
     -      RTRAP,STMAX,EPSDIF,EPSDFI,EPSTWI,EPSATI,RDF2,DSCMIN,DSCMAX,
     -      DTFACT,MDF2,
     -      MXDIFS,MXTWNS,MXATTS,
     -      NU,ISTAT,ITARG,
     -      ISTAT1,ISTAT2,ISTAT3,ISTAT4,ISTAT5,ISTAT6,NMC,MCMETH,IPTYPE,
     -      IPTECH,LREPSK,LKINK,LSTMAX,LEQSRT,LEQCRS,LEQMRK,LAVPRO
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       DOUBLE PRECISION XOLD,YOLD,ZOLD,XW,YW,XDIST,YDIST,DIST2,DISMIN
       REAL DCXMIN,DCXMAX,DCYMIN,DCYMAX,Q
       INTEGER IFAIL,I,IOUT,IFLAG,ITYPE,IVOL
       LOGICAL SHIFT
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE DLCSTA ///'
*** Preset ISTAT to 0 (normal situation).
       ISTAT=0
*** Handle the case NU=1 seperately.
       IF(NU.EQ.1)THEN
*   Define the area to be used for checks later on, when NU > 1
            DDXMIN=DXMIN
            DDYMIN=DYMIN
            DDZMIN=DZMIN
            DDXMAX=DXMAX
            DDYMAX=DYMAX
            DDZMAX=DZMAX
*   Check position with respect to the planes.
            ISTAT1=-1
            ISTAT2=-1
            ISTAT3=-1
            ISTAT4=-1
            ISTAT5=-1
            ISTAT6=-1
            IF(YNPLAN(1))THEN
                 DCXMIN=COPLAN(1)
                 IF(PERX)THEN
                      DCXMIN=DCXMIN+AINT((REAL(XU(1))-COPLAN(1))/SX)*SX
                      IF(XU(1).LT.COPLAN(1))DCXMIN=DCXMIN-SX
                 ENDIF
                 IF(DCXMIN.GT.DXMIN)ISTAT1=-11
                 DDXMIN=MAX(DCXMIN,DXMIN)
            ENDIF
            IF(YNPLAN(2))THEN
                 DCXMAX=COPLAN(2)
                 IF(PERX)THEN
                      DCXMAX=DCXMAX+AINT((REAL(XU(1))-COPLAN(2))/SX)*SX
                      IF(XU(1).GT.COPLAN(2))DCXMAX=DCXMAX+SX
                 ENDIF
                 IF(DCXMAX.LT.DXMAX)ISTAT2=-12
                 DDXMAX=MIN(DCXMAX,DXMAX)
            ENDIF
            IF(YNPLAN(3))THEN
                 DCYMIN=COPLAN(3)
                 IF(PERY)THEN
                      DCYMIN=DCYMIN+AINT((REAL(YU(1))-COPLAN(3))/SY)*SY
                      IF(YU(1).LT.COPLAN(3))DCYMIN=DCYMIN-SY
                 ENDIF
                 IF(DCYMIN.GT.DYMIN)ISTAT3=-13
                 DDYMIN=MAX(DCYMIN,DYMIN)
            ENDIF
            IF(YNPLAN(4))THEN
                 DCYMAX=COPLAN(4)
                 IF(PERY)THEN
                      DCYMAX=DCYMAX+AINT((REAL(YU(1))-COPLAN(4))/SY)*SY
                      IF(YU(1).GT.COPLAN(4))DCYMAX=DCYMAX+SY
                 ENDIF
                 IF(DCYMAX.LT.DYMAX)ISTAT4=-14
                 DDYMAX=MIN(DCYMAX,DYMAX)
            ENDIF
*   Check position with respect to the tube, if it exists.
            IF(TUBE)THEN
                 CALL INTUBE(REAL(XU(1)),REAL(YU(1)),COTUBE,NTUBE,IOUT)
                 IF(IOUT.EQ.1)THEN
                      ISTAT=-15
                 ELSEIF(IOUT.NE.0)THEN
                      ISTAT=-3
                 ENDIF
            ENDIF
*   particle outside the drift area right at the start,
            IF(XU(1).LT.DDXMIN)ISTAT=ISTAT1
            IF(XU(1).GT.DDXMAX)ISTAT=ISTAT2
            IF(YU(1).LT.DDYMIN)ISTAT=ISTAT3
            IF(YU(1).GT.DDYMAX)ISTAT=ISTAT4
            IF(ZU(1).LT.DDZMIN)ISTAT=ISTAT5
            IF(ZU(1).GT.DDZMAX)ISTAT=ISTAT6
            IF(ISTAT.NE.0)RETURN
**  Check whether the particle is already very near a wire.
            ITARG=0
            DISMIN=0
            DO 10 I=1,NWIRE
*   Skip wires with the wrong charge.
            IF(LREPSK.AND.Q*E(I).GT.0.0)GOTO 10
*   First find the wire closest to where we are now.
            XW=DBLE(X(I))
            YW=DBLE(Y(I))
            SHIFT=.FALSE.
            IF(PERX)THEN
                 XDIST=XU(1)-DBLE(X(I))
                 XW=DBLE(X(I))+SX*ANINT(XDIST/SX)
                 IF(ABS(XDIST).GT.SX/2)SHIFT=.TRUE.
            ENDIF
            IF(PERY)THEN
                 YDIST=YU(1)-DBLE(Y(I))
                 YW=DBLE(Y(I))+SY*ANINT(YDIST/SY)
                 IF(ABS(YDIST).GT.SY/2)SHIFT=.TRUE.
            ENDIF
            DIST2=(XU(NU)-XW)**2+(YU(NU)-YW)**2
*   Keep track of which one is closest.
            IF(ITARG.EQ.0.OR.DIST2.LT.DISMIN)THEN
                 DISMIN=DIST2
                 IF(SHIFT)THEN
                      ITARG=I+MXWIRE
                 ELSE
                      ITARG=I
                 ENDIF
                 XTARG=XW
                 YTARG=YW
                 DTARG=D(I)
            ENDIF
*   Next find out if we have to make some last step or not.
            IF(DIST2.LE.(0.5*RTRAP*D(I))**2)THEN
                 IF(DIST2.LE.(0.5*D(I))**2)THEN
                      IF(LDEBUG)
     -                     WRITE(LUNOUT,'(''  ++++++ DLCSTA DEBUG   :'',
     -                     '' Particle is inside the wire at NU=1.'')')
                      ISTAT=I
                 ELSE
                      IF(LDEBUG)
     -                     WRITE(LUNOUT,'(''  ++++++ DLCSTA DEBUG   :'',
     -                     '' DLCWIR entered from DLCSTA at NU=1.'')')
                      CALL DLCWIR(0,Q,ITYPE)
                 ENDIF
                 RETURN
            ENDIF
10          CONTINUE
**  Check whether the point is inside a solid
            IF(NSOLID.GT.0)THEN
                 CALL CELVPT(XU(NU),YU(NU),ZU(NU),IVOL)
                 IF(IVOL.NE.0)ISTAT=2*MXWIRE+IVOL
            ENDIF
**  On first step, stop here.
            RETURN
       ENDIF
*** Next handle the case of NU > 1, check crossing of a whole period.
       IF((PERX.AND.ABS(XU(NU)-XU(NU-1)).GE.SX).OR.
     -      (PERY.AND.ABS(YU(NU)-YU(NU-1)).GE.SY))THEN
            PRINT *,' ###### DLCSTA ERROR   : Particle crossed more'//
     -           ' than one period ; calculation is abandoned.'
            ISTAT=-3
            RETURN
       ENDIF
*** Check that the particle is still inside the drift area, clip if not.
       IF(XU(NU).LT.DDXMIN)ISTAT=ISTAT1
       IF(XU(NU).GT.DDXMAX)ISTAT=ISTAT2
       IF(YU(NU).LT.DDYMIN)ISTAT=ISTAT3
       IF(YU(NU).GT.DDYMAX)ISTAT=ISTAT4
       IF(ZU(NU).LT.DDZMIN)ISTAT=ISTAT5
       IF(ZU(NU).GT.DDZMAX)ISTAT=ISTAT6
       IF(ISTAT.NE.0)THEN
            XOLD=XU(NU)
            YOLD=YU(NU)
            ZOLD=ZU(NU)
            CALL CLIP3D(XU(NU-1),    YU(NU-1),    ZU(NU-1),
     -                  XU(NU),      YU(NU),      ZU(NU),
     -                  DBLE(DDXMIN),DBLE(DDYMIN),DBLE(DDZMIN),
     -                  DBLE(DDXMAX),DBLE(DDYMAX),DBLE(DDZMAX),IFAIL)
            IF(IFAIL.NE.0.OR.(XOLD.EQ.XU(NU-1).AND.
     -           YOLD.EQ.YU(NU-1).AND.ZOLD.EQ.ZU(NU-1)))THEN
                 NU=NU-1
            ELSE
                 TU(NU)=TU(NU-1)+(TU(NU)-TU(NU-1))*SQRT(
     -                ((XU(NU)-XU(NU-1))**2+(YU(NU)-YU(NU-1))**2+
     -                 (ZU(NU)-ZU(NU-1))**2)/
     -                ((XOLD-XU(NU-1))**2+(YOLD-YU(NU-1))**2+
     -                 (ZOLD-ZU(NU-1))**2))
            ENDIF
            RETURN
       ENDIF
*** Left the tube ?
       IF(TUBE)THEN
            CALL INTUBE(REAL(XU(NU)),REAL(YU(NU)),COTUBE,NTUBE,IOUT)
            IF(IOUT.NE.0)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCSTA DEBUG   :'',
     -                '' Particle is leaving the tube.'')')
                 CALL DLCTUB(Q,ITYPE)
                 RETURN
            ENDIF
       ENDIF
*** Check whether the point is inside a solid
       IF(NSOLID.GT.0)THEN
            CALL CELVPT(XU(NU),YU(NU),ZU(NU),IVOL)
            IF(IVOL.NE.0)THEN
                 ISTAT=2*MXWIRE+IVOL
                 RETURN
            ENDIF
       ENDIF
*** Find out whether a wire has been hit and remember the nearest wire.
       ITARG=0
       DISMIN=0
       DO 20 I=1,NWIRE
*   Skip wires with the wrong charge.
       IF(LREPSK.AND.Q*E(I).GT.0.0)GOTO 20
*   First find the wire closest to where we are now.
       XW=X(I)
       YW=Y(I)
       SHIFT=.FALSE.
       IF(PERX)THEN
            XDIST=(XU(NU)+XU(NU-1))/2-XW
            XW=XW+SX*ANINT(XDIST/SX)
            IF(ABS(XDIST).GT.SX/2)SHIFT=.TRUE.
       ENDIF
       IF(PERY)THEN
            YDIST=(YU(NU)+YU(NU-1))/2-YW
            YW=YW+SY*ANINT(YDIST/SY)
            IF(ABS(YDIST).GT.SY/2)SHIFT=.TRUE.
       ENDIF
       IF(XW+0.5*D(I).LT.DDXMIN.OR.XW-0.5*D(I).GT.DDXMAX.OR.
     -      YW+0.5*D(I).LT.DDYMIN.OR.YW-0.5*D(I).GT.DDYMAX)GOTO 20
*   Compute distance of the last point to the (replica of) wire I.
       CALL DLCMIN(XW,YW,XU(NU-1),YU(NU-1),XU(NU),YU(NU),DIST2,IFLAG)
*   Keep track of which one is closest.
       IF(ITARG.EQ.0.OR.DIST2.LT.DISMIN)THEN
            DISMIN=DIST2
            IF(SHIFT)THEN
                 ITARG=I+MXWIRE
            ELSE
                 ITARG=I
            ENDIF
            XTARG=XW
            YTARG=YW
            DTARG=D(I)
       ENDIF
*   Next find out if we have to make some last step or not.
       IF(DIST2.LE.(0.5*RTRAP*D(I))**2)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCSTA DEBUG   :'',
     -           '' Particle hit wire '',I5,'' at '',2E15.8/26X,
     -           ''distance from centre is '',E15.8,
     -           '', wire radius is '',E15.8/26X,''DLCWIR called'',
     -           '' from DLCSTA because the wire is hit.'')')
     -           I,XW,YW,SQRT(DIST2),D(I)/2.0
            IF(IPTECH.NE.4.AND.(DIST2.LE.
     -           (0.5*D(I))**2.OR.IFLAG.NE.+1))NU=NU-1
            CALL DLCWIR(0,Q,ITYPE)
            RETURN
       ENDIF
20     CONTINUE
       END
