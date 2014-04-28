CDECK  ID>, DLCVAC.
       SUBROUTINE DLCVAC(X1,Y1,Z1,VX1,VY1,VZ1,Q,PMASS)
*-----------------------------------------------------------------------
*   DLCVAC - Subroutine doing the actual drift line calculations in
*            vacuo. It communicates with the outside through sequence
*            DRIFTLINE. The calculations are based on a Runge Kutta
*            Nystroem method with step size control based on the
*            comparison of a 5th and a 2nd order estimate.
*
* NIST 1 eV = 1.782 661 845 x 10-36 kg
* http://physics.nist.gov/cgi-bin/cuu/Value?evkg
*
*   VARIABLES : H          : Current stepsize (it is in fact a delta t).
*               HPREV      : Stores the previous value of H (comparison)
*               INITCH     : Used for checking initial stepsize (1 = ok)
*               X1,Y1,Z1   : Starting location [cm]
*               VX1,VY1,VZ1: Starting velocity [cm/microsec]
*               Q          : Charge, -1 for e-, +1 for proton
*               PMASS      : Mass in MeV
*   (Last changed on 12/ 7/13.)
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
       DOUBLE PRECISION TIME,VEL(3),POS(3),ACC(3),H,HPREV,
     -      WORK(18),OLDPOS(3),OLDVEL(3),OLDACC(3),TRAPEZ(3)
       INTEGER ILOC,INITCH,ILOCVF,I,IOUT
       REAL Q,PMASS,X1,Y1,Z1,VX1,VY1,VZ1,EX,EY,EZ,ETOT,VOLT,EOVERM
       COMMON /VFUCOM/ EOVERM,ILOCVF
       EXTERNAL DLCVFU
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE DLCVAC ///'
*** Ensure that v < c
       IF(VX1**2+VY1**2+VZ1**2.GT.CLIGHT**2)THEN
            PRINT *,' !!!!!! DLCVAC WARNING : Initial velocity'//
     -           ' exceeds the speed of light; not tracked.'
            ISTAT = -3
            RETURN
       ENDIF
*** Initialise the output position and time vectors.
       NU=1
       XU(1)=DBLE(X1)
       YU(1)=DBLE(Y1)
       ZU(1)=DBLE(Z1)
       TU(1)=0.0D0
       ISTAT=0
*** Set particle type according to mass and technique to vacuum.
       IF(ABS(PMASS*MEV2KG-EMASS).LT.
     -      1E-4*(ABS(EMASS)+ABS(PMASS*MEV2KG)))THEN
            IPTYPE=1
       ELSE
            IPTYPE=2
       ENDIF
       QPCHAR=Q
       IPTECH=3
*** Set the charge over mass ratio.
       EOVERM=ECHARG*Q/(PMASS*MEV2KG)
       print *,' Charge: ',q,echarg*q
       print *,' Mass:   ',pmass,PMASS*MEV2KG
       print *,' Gamma:  ',1/SQRT(1-(Vx1**2+Vy1**2+Vz1**2)/CLIGHT**2)
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
                 PRINT *,' !!!!!! DLCVAC WARNING : Field location'//
     -                ' code does not match geometry; please report.'
                 ISTAT=-4
            ENDIF
*   In a material.
       ELSEIF(ILOC.EQ.-5)THEN
            ISTAT=-5
*   Outside the mesh.
       ELSEIF(ILOC.EQ.-6)THEN
            ISTAT=-6
*   Other bizarre codes.
       ELSEIF(ILOC.NE.0)THEN
            PRINT *,' ###### DLCVAC ERROR   : Unexpected ILOC=',ILOC,
     -           ' received from EFIELD ; program bug, please report.'
            ISTAT=-3
       ENDIF
*   Always return if location code is non-zero.
       IF(ILOC.NE.0)RETURN
*** Check the initial status, establishing eg the target wire.
       CALL DLCSTA(Q,IPTYPE)
       IF(ISTAT.NE.0)RETURN
*** Set the initial step-size, ensure that the particle will move.
       POS(1)=DBLE(X1)
       POS(2)=DBLE(Y1)
       POS(3)=DBLE(Z1)
       VEL(1)=DBLE(VX1)
       VEL(2)=DBLE(VY1)
       VEL(3)=DBLE(VZ1)
       CALL DLCVFU(0.0D0,POS,VEL,ACC)
       IF(ACC(1)**2+ACC(2)**2+ACC(3)**2.GT.0)THEN
            H=100*(-SQRT(VEL(1)**2+VEL(2)**2+VEL(3)**2)+
     -           SQRT(VEL(1)**2+VEL(2)**2+VEL(3)**2+
     -           2*EPSDIF*SQRT(ACC(1)**2+ACC(2)**2+ACC(3)**2)))/
     -           SQRT(ACC(1)**2+ACC(2)**2+ACC(3)**2)
       ELSEIF(VEL(1)**2+VEL(2)**2+VEL(3)**2.GT.0)THEN
            H=100*EPSDIF/SQRT(VEL(1)**2+VEL(2)**2+VEL(3)**2)
       ELSE
            PRINT *,' !!!!!! DLCVAC WARNING : Drift line starts'//
     -           ' with zero velocity and zero acceleration;'//
     -           ' abandoned'
            ISTAT=-3
            RETURN
       ENDIF
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCVAC DEBUG   : Initial'',
     -      '' step size set to '',E10.3)') H
*** Allow INITCH cycles to adjust the initial step-size.
       INITCH=3
20     CONTINUE
       NU=1
*** Set the initial time, position, velocity and acceleration.
       TIME=0
       POS(1)=DBLE(X1)
       POS(2)=DBLE(Y1)
       POS(3)=DBLE(Z1)
       VEL(1)=DBLE(VX1)
       VEL(2)=DBLE(VY1)
       VEL(3)=DBLE(VZ1)
       CALL DLCVFU(0.0D0,POS,VEL,ACC)
*** Next step.
30     CONTINUE
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCVAC DEBUG   : Step '',
     -      I4/26X,''(x,y,z)='',3E12.5,'' t='',E12.5)')
     -      NU,(POS(I),I=1,3),TIME
*   Reset location code.
       ILOCVF=0
*   Save old position.
       DO 40 I=1,3
       OLDPOS(I)=POS(I)
       OLDVEL(I)=VEL(I)
       OLDACC(I)=ACC(I)
40     CONTINUE
*** Take a Runge-Kutta-Nystroem step.
       CALL DRKNYS(3,H,TIME,POS,VEL,DLCVFU,WORK)
*** Make a trapezoid estimate of the same step.
       CALL DLCVFU(0.0D0,POS,VEL,ACC)
       DO 50 I=1,3
       TRAPEZ(I)=OLDPOS(I)+H*(VEL(I)+OLDVEL(I))/2+
     -      H**2*(ACC(I)+OLDACC(I))/8
50     CONTINUE
*** Check that the target wire is not crossed while exploring the field.
       IF(ITARG.GT.0.AND.ILOCVF.GT.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCVAC DEBUG   :'',
     -           '' DLCWIR entered from DLCVAC for ILOCVF='',I5)')
     -           ILOCVF
            CALL DLCWIR(1,Q,IPTYPE)
            RETURN
       ENDIF
*** Check that no dielectric was entered nor that the mesh was left.
       IF(ICTYPE.EQ.0.AND.ILOCVF.NE.0)THEN
            CALL DLCFMP(OLDPOS(1),OLDPOS(2),OLDPOS(3),
     -           POS(1),POS(2),POS(3),H,ILOCVF,Q,IPTYPE)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCVAC DEBUG   :'',
     -           '' Drift medium or mesh left at NU='',I4,
     -           '' ILOC='',I5)') NU,ILOCVF
            RETURN
*   Check for solids.
       ELSEIF(ICTYPE.EQ.-1.AND.ILOCVF.NE.0)THEN
            CALL DLCSOL(OLDPOS(1),OLDPOS(2),OLDPOS(3),
     -           POS(1),POS(2),POS(3),H,ILOCVF,Q,IPTYPE)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCVAC DEBUG   :'',
     -           '' Drift medium or mesh left at NU='',I4,
     -           '' ILOC='',I5)') NU,ILOCVF
            RETURN
       ENDIF
*** Check particle position for other termination conditions.
       CALL DLCSTA(Q,IPTYPE)
       IF(ISTAT.NE.0)RETURN
*** Check bending angle.
       IF(LKINK.AND.NU.GT.1)THEN
            IF(VEL(1)*(XU(NU)-XU(NU-1))+VEL(2)*(YU(NU)-YU(NU-1))+
     -           VEL(3)*(ZU(NU)-ZU(NU-1)).LT.0)THEN
                 ISTAT=-8
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCVAC DEBUG   :'',
     -                '' Step '',I3,'': bending angle exceeds pi/2.''/
     -                26X,''Velocity vector: '',3E15.8/
     -                26X,''Previous step:   '',3E15.8/
     -                26X,''Inner product:   '',E15.8)')
     -                NU+1,VEL(1),VEL(2),VEL(3),XU(NU)-XU(NU-1),
     -                YU(NU)-YU(NU-1),ZU(NU)-ZU(NU-1),
     -                VEL(1)*(XU(NU)-XU(NU-1))+
     -                VEL(2)*(YU(NU)-YU(NU-1))+
     -                VEL(3)*(ZU(NU)-ZU(NU-1))
                 RETURN
            ENDIF
       ENDIF
*** Copy new X0 and Y0 to XU and YU, add new TU.
       NU=NU+1
       XU(NU)=POS(1)
       YU(NU)=POS(2)
       ZU(NU)=POS(3)
       TU(NU)=TIME
*** Adjust step size by comparing trapezoid rule and RKN estimates.
       HPREV=H
       IF(POS(1).NE.TRAPEZ(1).OR.POS(2).NE.TRAPEZ(2).OR.
     -      POS(3).NE.TRAPEZ(3))THEN
            H=H*SQRT(EPSDIF/(ABS(POS(1)-TRAPEZ(1))+
     -           ABS(POS(2)-TRAPEZ(2))+ABS(POS(3)-TRAPEZ(3))))
       ELSE
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCVAC DEBUG   :'',
     -           '' Step size increased by a factor 2 in step '',I4,
     -           '' (1st order = RKN).'')') NU
            H=H*2.0D0
       ENDIF
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCVAC DEBUG   :'',
     -      '' 1st order: '',3E12.5/26X,''RKN:       '',3E12.5/26X,
     -      ''Step size changed by a factor '',E12.5,'' to '',E12.5)')
     -      (TRAPEZ(I),I=1,3),(POS(I),I=1,3),H/HPREV,H
*** Don't allow H to become too large in view of the time resolution.
       IF(H*ABS(VEL(1)).GT.(DXMAX-DXMIN)/10.0.OR.
     -      H*ABS(VEL(2)).GT.(DYMAX-DYMIN)/10.0.OR.
     -      H*ABS(VEL(3)).GT.(DZMAX-DZMIN)/10.0)THEN
            H=H/2
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCVAC DEBUG   :'',
     -           '' Step size reduced by a factor 2 in step '',I4,
     -           '' (step too long).'')') NU
       ENDIF
*** Make sure that H is different from zero; this should always be ok.
       IF(H.EQ.0.0D0)THEN
            PRINT *,' ###### DLCVAC ERROR   : Step ',NU,' step size is',
     -           ' zero (program bug) ; the calculation is abandoned.'
            ISTAT=-3
            RETURN
       ENDIF
*** Check the initial step size.
       IF(INITCH.GT.0.AND.(H.LT.HPREV/5.0))THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCVAC DEBUG   :'',
     -           '' Step size reinitialised, current value is '',
     -           E12.5)') H
            INITCH=INITCH-1
            GOTO 20
       ENDIF
       INITCH=0
*** Don't allow H to grow too quickly.
       IF(H.GT.10.0*HPREV)THEN
            H=10.0*HPREV
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCVAC DEBUG   :'',
     -           '' Step size restricted to 10 x previous in step '',
     -           I4,''.'')') NU
       ENDIF
*** Make sure we haven't got more than MXLIST points already.
       IF(NU.EQ.MXLIST)THEN
            ISTAT=-2
            RETURN
       ENDIF
*** Stop in case H tends to become too small.
       IF(H*(ABS(VEL(1))+ABS(VEL(2))+ABS(VEL(3))).LT.EPSDIF)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCVAC DEBUG   :'',
     -           '' Step size smaller than EPSDIF in step '',I4,
     -           '' ; line abandoned.'')') NU
            ISTAT=-3
            RETURN
       ENDIF
       GOTO 30
       END
