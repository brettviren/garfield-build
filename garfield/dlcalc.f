CDECK  ID>, DLCALC.
       SUBROUTINE DLCALC(X1,Y1,Z1,Q,ITYPE)
*-----------------------------------------------------------------------
*   DLCALC - Subroutine doing the actual drift line calculations. It
*            communicates with the outside through sequence DRIFTLINE.
*            The calculations are based on a Runge-Kutta-Fehlberg method
*            which has the advantage of controlling the stepsize and the
*            error while needing only relatively few calls to EFIELD.
*            Full details are given in the reference quoted below.
*   VARIABLES : H          : Current stepsize (it is in fact a delta t).
*               HPREV      : Stores the previous value of H (comparison)
*               INITCH     : Used for checking initial stepsize (1 = ok)
*               Other variables such as F0, F1, F2, F3, PHII, PHIII,
*               CI. ,CII. , BETA.. etc   are explained in the reference.
*   REFERENCE : Stoer + Bulirsch, Einfuhrung in die Numerische
*               Mathematic II, chapter 7, page 122, 1978, HTB, Springer.
*   (Last changed on  4/ 4/10.)
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
       DOUBLE PRECISION F0(3),F1(3),F2(3),F3(3),PHII(3),PHIII(3),
     -      X0,Y0,Z0,H,HPREV,CI0,CI1,CI2,CII0,CII2,CII3,
     -      BETA10,BETA20,BETA21,BETA30,BETA31,BETA32,
     -      DIST21,DIST22,DIST23,XST0,YST0,XST1,YST1
       INTEGER IPLANE,IFLAG1,IFLAG2,IFLAG3,ILOC,ILOC1,ILOC2,ILOC3,
     -      INITCH,ITYPE,IOUT,IOUT1,IOUT2,IOUT3
       REAL Q,X1,Y1,Z1,EX,EY,EZ,ETOT,VOLT
*** Initialise the constants appearing in the RKF formulas.
       PARAMETER(CI0   =214.0D0/ 891.0D0,CI1   =   1.0D0/  33.0D0,
     -           CI2   =650.0D0/ 891.0D0,CII0  = 533.0D0/2106.0D0,
     -           CII2  =800.0D0/1053.0D0,CII3  =  -1.0D0/  78.0D0,
     -           BETA10=  1.0D0/   4.0D0,BETA20=-189.0D0/ 800.0D0,
     -           BETA21=729.0D0/ 800.0D0,BETA30= 214.0D0/ 891.0D0,
     -           BETA31=  1.0D0/  33.0D0,BETA32= 650.0D0/ 891.0D0)
*** Use these lines if the compiler rejects the PARAMETER statements.
C+SELF,IF=SAVE.
C       SAVE CI0,CI1,CI2,CII0,CII2,CII3
C       SAVE BETA10,BETA20,BETA21,BETA30,BETA31,BETA32
C+SELF.
C       DATA CI0   ,CI1   ,CI2   /0.240179574, 0.030303030, 0.729517396/
C       DATA CII0  ,CII2  ,CII3  /0.253086420, 0.759734093,-0.012820513/
C       DATA BETA10,BETA20,BETA21/0.25,       -0.23625,     0.91125    /
C       DATA BETA30,BETA31,BETA32/0.240179574, 0.030303030, 0.729517396/
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE DLCALC ///'
*** Initialise the output position and time vectors.
       NU=1
       XU(1)=DBLE(X1)
       YU(1)=DBLE(Y1)
       ZU(1)=DBLE(Z1)
       TU(1)=0.0D0
       ISTAT=0
       IPTYPE=ITYPE
       IPTECH=1
       QPCHAR=Q
*** Check the initial position, setting a status code if appropriate.
       CALL EFIELD(X1,Y1,Z1,EX,EY,EZ,ETOT,VOLT,0,ILOC)
*   In a wire.
       IF(ILOC.GT.0.AND.ILOC.LE.MXWIRE)THEN
            IF((X(ILOC)-X1)**2+(Y(ILOC)-Y1)**2.LE.0.25*D(ILOC)**2)THEN
                 ISTAT=ILOC
            ELSE
                 ISTAT=ILOC+MXWIRE
            ENDIF
*   Outside the planes.
       ELSEIF(ILOC.EQ.-1.OR.ILOC.EQ.-4)THEN
            IF(YNPLAN(1).AND.X1.LE.COPLAN(1))THEN
                 ISTAT=-11
            ELSEIF(YNPLAN(2).AND.X1.GE.COPLAN(2))THEN
                 ISTAT=-12
            ELSEIF(YNPLAN(3).AND.Y1.LE.COPLAN(3))THEN
                 ISTAT=-13
            ELSEIF(YNPLAN(4).AND.Y1.GE.COPLAN(4))THEN
                 ISTAT=-14
            ELSEIF(TUBE)THEN
                 CALL INTUBE(X1,Y1,COTUBE,NTUBE,IOUT)
                 IF(IOUT.EQ.1)THEN
                      ISTAT=-15
                 ELSEIF(IOUT.NE.0)THEN
                      ISTAT=-3
                 ENDIF
            ENDIF
            IF(ISTAT.EQ.0)THEN
                 PRINT *,' !!!!!! DLCALC WARNING : Field location'//
     -                ' code does not match geometry; please report.'
                 ISTAT=-4
            ENDIF
*   In a solid.
       ELSEIF(ILOC.GT.2*MXWIRE.AND.ILOC.LE.2*MXWIRE+MXSOLI)THEN
            ISTAT=ILOC
*   In a material.
       ELSEIF(ILOC.EQ.-5)THEN
            ISTAT=-5
*   Outside the mesh.
       ELSEIF(ILOC.EQ.-6)THEN
            ISTAT=-6
*   Other bizarre codes.
       ELSEIF(ILOC.NE.0)THEN
            PRINT *,' ###### DLCALC ERROR   : Unexpected ILOC=',ILOC,
     -           ' received from EFIELD ; program bug, please report.'
            ISTAT=-3
       ENDIF
*   Always return if location code is non-zero.
       IF(ILOC.NE.0)RETURN
*** Check the initial status, establishing eg the target wire.
       CALL DLCSTA(Q,ITYPE)
       IF(ISTAT.NE.0)RETURN
*** Set the initial step-size, zero drift-field should be exceptional.
       CALL DLCVEL(DBLE(X1),DBLE(Y1),DBLE(Z1),F0,Q,ITYPE,ILOC)
       IF(F0(1)**2+F0(2)**2+F0(3)**2.EQ.0.0)THEN
            PRINT *,' !!!!!! DLCALC WARNING : Drift line starts from'//
     -           ' a zero E-field point.'
            ISTAT=-3
            RETURN
       ELSE
            H=EPSDIF/SQRT(F0(1)**2+F0(2)**2+F0(3)**2)
       ENDIF
*   Allow INITCH cycles to adjust the initial step-size.
       INITCH=3
20     CONTINUE
       NU=1
*   And also store the initial point locally in scalar double precision.
       X0=DBLE(X1)
       Y0=DBLE(Y1)
       Z0=DBLE(Z1)
*** Take steps of size H (adjusted every cycle).
30     CONTINUE
       CALL DLCVEL(
     -      X0+H*BETA10*F0(1),
     -      Y0+H*BETA10*F0(2),
     -      Z0+H*BETA10*F0(3),
     -      F1,Q,ITYPE,ILOC1)
       CALL DLCVEL(
     -      X0+H*(BETA20*F0(1)+BETA21*F1(1)),
     -      Y0+H*(BETA20*F0(2)+BETA21*F1(2)),
     -      Z0+H*(BETA20*F0(3)+BETA21*F1(3)),
     -      F2,Q,ITYPE,ILOC2)
       CALL DLCVEL(
     -      X0+H*(BETA30*F0(1)+BETA31*F1(1)+BETA32*F2(1)),
     -      Y0+H*(BETA30*F0(2)+BETA31*F1(2)+BETA32*F2(2)),
     -      Z0+H*(BETA30*F0(3)+BETA31*F1(3)+BETA32*F2(3)),
     -      F3,Q,ITYPE,ILOC3)
*** Check that the target wire is not crossed while exploring the field.
       IF(ITARG.GT.0)THEN
            CALL DLCMIN(XTARG,YTARG,X0,Y0,
     -           X0+H*BETA10*F0(1),Y0+H*BETA10*F0(2),
     -           DIST21,IFLAG1)
            CALL DLCMIN(XTARG,YTARG,X0,Y0,
     -           X0+H*(BETA20*F0(1)+BETA21*F1(1)),
     -           Y0+H*(BETA20*F0(2)+BETA21*F1(2)),
     -           DIST22,IFLAG2)
            CALL DLCMIN(XTARG,YTARG,X0,Y0,
     -           X0+H*(BETA30*F0(1)+BETA31*F1(1)+BETA32*F2(1)),
     -           Y0+H*(BETA30*F0(2)+BETA31*F1(2)+BETA32*F2(2)),
     -           DIST23,IFLAG3)
*   If it is, quit at this point after terminating via DLCWIR.
            IF(DIST21.LT.0.25*DTARG**2.OR.DIST22.LT.0.25*DTARG**2.OR.
     -           DIST23.LT.0.25*DTARG**2)THEN
                 IF(LDEBUG)PRINT *,' ++++++ DLCALC DEBUG   : DLCWIR',
     -                ' entered from DLCALC.'
                 CALL DLCWIR(1,Q,ITYPE)
                 RETURN
            ENDIF
       ENDIF
*** Check that none of the planes was crossed during this computation.
       IF(YNPLAN(1).OR.YNPLAN(2).OR.YNPLAN(3).OR.YNPLAN(4))THEN
            XST0=MIN(X0+H*BETA10*F0(1),X0+H*(BETA20*F0(1)+BETA21*F1(1)),
     -           X0+H*(BETA30*F0(1)+BETA31*F1(1)+BETA32*F2(1)))
            YST0=MIN(Y0+H*BETA10*F0(2),Y0+H*(BETA20*F0(2)+BETA21*F1(2)),
     -           Y0+H*(BETA30*F0(2)+BETA31*F1(2)+BETA32*F2(2)))
            XST1=MAX(X0+H*BETA10*F0(1),X0+H*(BETA20*F0(1)+BETA21*F1(1)),
     -           X0+H*(BETA30*F0(1)+BETA31*F1(1)+BETA32*F2(1)))
            YST1=MAX(Y0+H*BETA10*F0(2),Y0+H*(BETA20*F0(2)+BETA21*F1(2)),
     -           Y0+H*(BETA30*F0(2)+BETA31*F1(2)+BETA32*F2(2)))
            IPLANE=0
            IF(YNPLAN(1).AND.XST0.LE.COPLAN(1))IPLANE=1
            IF(YNPLAN(2).AND.XST1.GE.COPLAN(2))IPLANE=2
            IF(YNPLAN(3).AND.YST0.LE.COPLAN(3))IPLANE=3
            IF(YNPLAN(4).AND.YST1.GE.COPLAN(4))IPLANE=4
            IF(IPLANE.NE.0)THEN
                 IF(LDEBUG)PRINT *,' ++++++ DLCALC DEBUG   : Plane ',
     -                IPLANE,' was crossed during the last step.'
                 CALL DLCPLA(IPLANE,Q,ITYPE)
                 RETURN
            ENDIF
       ENDIF
*** Check that the tube was not left.
       IF(TUBE)THEN
            CALL INTUBE(
     -           REAL(X0+H*BETA10*F0(1)),
     -           REAL(Y0+H*BETA10*F0(2)),
     -           COTUBE,NTUBE,IOUT1)
            CALL INTUBE(
     -           REAL(X0+H*(BETA20*F0(1)+BETA21*F1(1))),
     -           REAL(Y0+H*(BETA20*F0(2)+BETA21*F1(2))),
     -           COTUBE,NTUBE,IOUT2)
            CALL INTUBE(
     -           REAL(X0+H*(BETA30*F0(1)+BETA31*F1(1)+BETA32*F2(1))),
     -           REAL(Y0+H*(BETA30*F0(2)+BETA31*F1(2)+BETA32*F2(2))),
     -           COTUBE,NTUBE,IOUT3)
            IF(IOUT1.NE.0.OR.IOUT2.NE.0.OR.IOUT3.NE.0)THEN
                 IF(LDEBUG)PRINT *,' ++++++ DLCALC DEBUG   : The tube'//
     -                ' was left during the last step.'
                 CALL DLCTUB(Q,ITYPE)
                 RETURN
            ENDIF
       ENDIF
*** Check that no dielectric was entered nor that the mesh was left.
       IF(ICTYPE.EQ.0.AND.(ILOC1.NE.0.OR.ILOC2.NE.0.OR.ILOC3.NE.0))THEN
            IF(ILOC1.NE.0)THEN
                 CALL DLCFMP(X0,Y0,Z0,
     -                X0+H*BETA10*F0(1),
     -                Y0+H*BETA10*F0(2),
     -                Z0+H*BETA10*F0(3),
     -                H,ILOC1,Q,ITYPE)
            ELSEIF(ILOC2.NE.0)THEN
                 CALL DLCFMP(X0,Y0,Z0,
     -                X0+H*(BETA20*F0(1)+BETA21*F1(1)),
     -                Y0+H*(BETA20*F0(2)+BETA21*F1(2)),
     -                Z0+H*(BETA20*F0(3)+BETA21*F1(3)),
     -                H,ILOC2,Q,ITYPE)
            ELSEIF(ILOC3.NE.0)THEN
                 CALL DLCFMP(X0,Y0,Z0,
     -                X0+H*(BETA30*F0(1)+BETA31*F1(1)+BETA32*F2(1)),
     -                Y0+H*(BETA30*F0(2)+BETA31*F1(2)+BETA32*F2(2)),
     -                Z0+H*(BETA30*F0(3)+BETA31*F1(3)+BETA32*F2(3)),
     -                H,ILOC3,Q,ITYPE)
            ENDIF
            IF(LDEBUG)PRINT *,' ++++++ DLCALC DEBUG   : Drift medium',
     -           ' or mesh left at NU=',NU,' ILOC=',ILOC1,ILOC2,ILOC3
            RETURN
*   Same for solids in neBEM.
       ELSEIF(ICTYPE.EQ.-1.AND.
     -      (ILOC1.NE.0.OR.ILOC2.NE.0.OR.ILOC3.NE.0))THEN
            IF(ILOC1.NE.0)THEN
                 CALL DLCSOL(X0,Y0,Z0,
     -                X0+H*BETA10*F0(1),
     -                Y0+H*BETA10*F0(2),
     -                Z0+H*BETA10*F0(3),
     -                H,ILOC1,Q,ITYPE)
            ELSEIF(ILOC2.NE.0)THEN
                 CALL DLCSOL(X0,Y0,Z0,
     -                X0+H*(BETA20*F0(1)+BETA21*F1(1)),
     -                Y0+H*(BETA20*F0(2)+BETA21*F1(2)),
     -                Z0+H*(BETA20*F0(3)+BETA21*F1(3)),
     -                H,ILOC2,Q,ITYPE)
            ELSEIF(ILOC3.NE.0)THEN
                 CALL DLCSOL(X0,Y0,Z0,
     -                X0+H*(BETA30*F0(1)+BETA31*F1(1)+BETA32*F2(1)),
     -                Y0+H*(BETA30*F0(2)+BETA31*F1(2)+BETA32*F2(2)),
     -                Z0+H*(BETA30*F0(3)+BETA31*F1(3)+BETA32*F2(3)),
     -                H,ILOC3,Q,ITYPE)
            ENDIF
            IF(LDEBUG)PRINT *,' ++++++ DLCALC DEBUG   : Drift medium',
     -           ' or mesh left at NU=',NU,' ILOC=',ILOC1,ILOC2,ILOC3
            RETURN
       ENDIF
*** Now set up the correction for (X0,Y0,Z0).
       PHII(1)=CI0*F0(1)+CI1*F1(1)+CI2*F2(1)
       PHII(2)=CI0*F0(2)+CI1*F1(2)+CI2*F2(2)
       PHII(3)=CI0*F0(3)+CI1*F1(3)+CI2*F2(3)
       PHIII(1)=CII0*F0(1)+CII2*F2(1)+CII3*F3(1)
       PHIII(2)=CII0*F0(2)+CII2*F2(2)+CII3*F3(2)
       PHIII(3)=CII0*F0(3)+CII2*F2(3)+CII3*F3(3)
*** Be sure that the step has non-zero length.
       IF(SQRT(PHII(1)**2+PHII(2)**2+PHII(3)**2).LE.0)THEN
            IF(LDEBUG)PRINT *,' ++++++ DLCALC DEBUG   : Step ',NU,
     -           ' has 0 length; abandoned.'
            ISTAT=-3
            RETURN
*** Check step size.
       ELSEIF(LSTMAX.AND.
     -      H*SQRT(PHII(1)**2+PHII(2)**2+PHII(3)**2).GT.STMAX)THEN
            H=0.5*STMAX/SQRT(PHII(1)**2+PHII(2)**2+PHII(3)**2)
            IF(LDEBUG)PRINT *,' ++++++ DLCALC DEBUG   : Step ',NU,
     -           ' is considered too long; H is reduced.'
            GOTO 30
C*** Don't allow H to become too large in view of the time resolution.
C       ELSEIF(H*ABS(PHII(1)).GT.(DXMAX-DXMIN)/10.0.OR.
C     -      H*ABS(PHII(2)).GT.(DYMAX-DYMIN)/10.0)THEN
C            H=H/2
C            IF(LDEBUG)PRINT *,' ++++++ DLCALC DEBUG   : Step ',NU,
C     -           ' is considered too long; H is divided by 2.'
C            GOTO 30
*** Check bending angle.
       ELSEIF(LKINK.AND.NU.GT.1)THEN
            IF(PHII(1)*(XU(NU)-XU(NU-1))+
     -           PHII(2)*(YU(NU)-YU(NU-1))+
     -           PHII(3)*(ZU(NU)-ZU(NU-1)).LT.0)THEN
                 ISTAT=-8
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCALC DEBUG   :'',
     -                '' Step '',I3,'': bending angle exceeds pi/2.''/
     -                26X,''Proposed step: '',3E15.8/
     -                26X,''Previous step: '',3E15.8/
     -                26X,''Inner product: '',E15.8)')
     -                NU+1,PHII(1),PHII(2),PHII(3),XU(NU)-XU(NU-1),
     -                YU(NU)-YU(NU-1),ZU(NU)-ZU(NU-1),
     -                PHII(1)*(XU(NU)-XU(NU-1))+
     -                PHII(2)*(YU(NU)-YU(NU-1))+
     -                PHII(3)*(ZU(NU)-ZU(NU-1))
                 RETURN
            ENDIF
       ENDIF
*** Redefine X0, Y0 and Z0.
       X0=X0+H*PHII(1)
       Y0=Y0+H*PHII(2)
       Z0=Z0+H*PHII(3)
*** Copy new X0 and Y0 to XU and YU, add new TU.
       NU=NU+1
       XU(NU)=X0
       YU(NU)=Y0
       ZU(NU)=Z0
       TU(NU)=TU(NU-1)+H
*** Check particle position.
       CALL DLCSTA(Q,ITYPE)
       IF(ISTAT.NE.0)RETURN
*** Adjust step size according to the accuracy of the two estimates.
       HPREV=H
       IF(PHII(1).NE.PHIII(1).OR.PHII(2).NE.PHIII(2).OR.
     -      PHII(3).NE.PHIII(3))THEN
            H=SQRT(H*EPSDIF/(ABS(PHII(1)-PHIII(1))+
     -           ABS(PHII(2)-PHIII(2))+ABS(PHII(3)-PHIII(3))))
       ELSE
            IF(LDEBUG)PRINT *,' ++++++ DLCALC DEBUG   : H increased by',
     -           ' a factor of 2 in step ',NU,' (equal estimates).'
            H=H*2.0D0
       ENDIF
*** Make sure that H is different from zero; this should always be ok.
       IF(H.EQ.0.0D0)THEN
            PRINT *,' ###### DLCALC ERROR   : Step ',NU,' step size is',
     -           ' zero (program bug) ; the calculation is abandoned.'
            ISTAT=-3
            RETURN
       ENDIF
*** Check the initial step size.
       IF(INITCH.GT.0.AND.(H.LT.HPREV/5.0))THEN
C           IF(LDEBUG)PRINT *,' ++++++ DLCALC DEBUG   : Stepsize',
C    -           ' reinitialised, current value is ',H
            INITCH=INITCH-1
            GOTO 20
       ENDIF
       INITCH=0
*** Don't allow H to grow too quickly.
       IF(H.GT.10.0*HPREV)THEN
            H=10.0*HPREV
C           IF(LDEBUG)PRINT *,' ++++++ DLCALC DEBUG   : Step ',NU,
C    -           ' H restricted to 10 times HPREV.'
       ENDIF
*** Make sure we haven't got more than MXLIST points already.
       IF(NU.EQ.MXLIST)THEN
            ISTAT=-2
            RETURN
       ENDIF
*** Stop in case H tends to become too small.
       IF(H*(ABS(PHII(1))+ABS(PHII(2))+ABS(PHII(3))).LT.EPSDIF)THEN
            IF(LDEBUG)PRINT *,' ++++++ DLCALC DEBUG   : The step size',
     -           ' has become smaller than EPSDIF; line abandoned.'
            ISTAT=-3
            RETURN
       ENDIF
*** Remember: F0 equals F3 of the previous step.
       F0(1)=F3(1)
       F0(2)=F3(2)
       F0(3)=F3(3)
       GOTO 30
       END
