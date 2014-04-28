CDECK  ID>, DLCVEL.
       SUBROUTINE DLCVEL(XPOS,YPOS,ZPOS,VD,Q,ITYPE,ILOC)
*-----------------------------------------------------------------------
*   DLCVEL - Subroutine returning the (vector) speed of an electron or
*            ion, taking the electric (and magnetic) field into account.
*   VARIABLES : V          : Speed of the electron or ion.
*               Q          : Charge of the particle in units of E.
*               ITYPE      : Particle type (1=e- ; else=ion).
*               PMU        : Mobility of the electron/ion in the gas.
*   (Last changed on 18/12/06.)
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
       DOUBLE PRECISION CLSDIS,CLSAVE
       REAL EGAS,VGAS,XGAS,YGAS,DGAS,AGAS,BGAS,HGAS,MGAS,WGAS,OGAS,SGAS,
     -      EXGAS,IOGAS,
     -      CVGAS,CXGAS,CYGAS,CDGAS,CAGAS,CBGAS,CHGAS,CMGAS,CWGAS,COGAS,
     -      CSGAS,CEXGAS,CIOGAS,
     -      VGAS2,XGAS2,YGAS2,DGAS2,AGAS2,BGAS2,HGAS2,MGAS2,WGAS2,OGAS2,
     -      SGAS2,EXGAS2,IOGAS2,
     -      AORIG,AORIG2,PENPRB,PENRMS,PENDT,ENIOG,ENEXG,
     -      BANG,BTAB,
     -      VEXTR1,VEXTR2,VEXTR3,VEXTR4,
     -      XEXTR1,XEXTR2,XEXTR3,XEXTR4,
     -      YEXTR1,YEXTR2,YEXTR3,YEXTR4,
     -      DEXTR1,DEXTR2,DEXTR3,DEXTR4,
     -      AEXTR1,AEXTR2,AEXTR3,AEXTR4,
     -      BEXTR1,BEXTR2,BEXTR3,BEXTR4,
     -      HEXTR1,HEXTR2,HEXTR3,HEXTR4,
     -      MEXTR1,MEXTR2,MEXTR3,MEXTR4,
     -      WEXTR1,WEXTR2,WEXTR3,WEXTR4,
     -      OEXTR1,OEXTR2,OEXTR3,OEXTR4,
     -      SEXTR1,SEXTR2,SEXTR3,SEXTR4,
     -      EEXTR1,EEXTR2,EEXTR3,EEXTR4,
     -      ZEXTR1,ZEXTR2,ZEXTR3,ZEXTR4,
     -      GASRNG,
     -      Z,A,RHO,CMEAN,EMPROB,EPAIR,PGAS,TGAS,GASDEN,
     -      DTION,DLION,GASFRM,ELOSCS
       LOGICAL GASOK,TAB2D,GASOPT,HEEDOK,SRIMOK,TRIMOK,GASSET
       INTEGER NGAS,NCLS,NBANG,NBTAB,NFTAB,NFCLS,
     -      IVMETH,IXMETH,IYMETH,IDMETH,IAMETH,IBMETH,IHMETH,IMMETH,
     -      IWMETH,IOMETH,ISMETH,IEMETH,IZMETH,
     -      IVEXTR,IXEXTR,IYEXTR,IDEXTR,IAEXTR,IBEXTR,IHEXTR,IMEXTR,
     -      IWEXTR,IOEXTR,ISEXTR,IEEXTR,IZEXTR,
     -      JVEXTR,JXEXTR,JYEXTR,JDEXTR,JAEXTR,JBEXTR,JHEXTR,JMEXTR,
     -      JWEXTR,JOEXTR,JSEXTR,JEEXTR,JZEXTR,
     -      IATHR,IBTHR,IHTHR,
     -      NEXGAS,NIOGAS,NCSGAS,ICSTYP
       CHARACTER*80 GASID
       CHARACTER*(MXCHAR) FCNTAB,FCNCLS
       CHARACTER*10 CLSTYP
       CHARACTER*45 DSCEXG(MXEXG),DSCIOG(MXIOG),DSCCSG(MXCSG)
       COMMON /GASDAT/ CLSDIS(MXPAIR),CLSAVE,
     -      EGAS(MXLIST),
     -      VGAS(MXLIST),XGAS(MXLIST),YGAS(MXLIST),WGAS(MXLIST),
     -      DGAS(MXLIST),OGAS(MXLIST),AGAS(MXLIST),BGAS(MXLIST),
     -      HGAS(MXLIST),MGAS(MXLIST),SGAS(MXLIST,6),
     -      EXGAS(MXLIST,MXEXG),IOGAS(MXLIST,MXIOG),
     -      CVGAS(MXLIST),CXGAS(MXLIST),CYGAS(MXLIST),CWGAS(MXLIST),
     -      CDGAS(MXLIST),COGAS(MXLIST),CAGAS(MXLIST),CBGAS(MXLIST),
     -      CHGAS(MXLIST),CMGAS(MXLIST),CSGAS(MXLIST,6),
     -      CEXGAS(MXLIST,MXEXG),CIOGAS(MXLIST,MXIOG),
     -      VGAS2(MXLIST,MXBANG,MXBTAB),WGAS2(MXLIST,MXBANG,MXBTAB),
     -      XGAS2(MXLIST,MXBANG,MXBTAB),YGAS2(MXLIST,MXBANG,MXBTAB),
     -      AGAS2(MXLIST,MXBANG,MXBTAB),BGAS2(MXLIST,MXBANG,MXBTAB),
     -      DGAS2(MXLIST,MXBANG,MXBTAB),OGAS2(MXLIST,MXBANG,MXBTAB),
     -      HGAS2(MXLIST,MXBANG,MXBTAB),MGAS2(MXLIST,MXBANG,MXBTAB),
     -      SGAS2(MXLIST,MXBANG,MXBTAB,6),
     -      EXGAS2(MXLIST,MXBANG,MXBTAB,MXEXG),
     -      IOGAS2(MXLIST,MXBANG,MXBTAB,MXIOG),
     -      AORIG(MXLIST),AORIG2(MXLIST,MXBANG,MXBTAB),
     -      PENPRB(MXEXG),PENRMS(MXEXG),PENDT(MXEXG),
     -      ENIOG(MXIOG),ENEXG(MXEXG),
     -      BANG(MXBANG),BTAB(MXBTAB),
     -      GASRNG(20,2),GASFRM(MXNBMC),ELOSCS(MXCSG),
     -      Z,A,RHO,CMEAN,EMPROB,EPAIR,PGAS,TGAS,GASDEN,
     -      DTION,DLION,
     -      VEXTR1,VEXTR2,VEXTR3,VEXTR4,
     -      XEXTR1,XEXTR2,XEXTR3,XEXTR4,
     -      YEXTR1,YEXTR2,YEXTR3,YEXTR4,
     -      DEXTR1,DEXTR2,DEXTR3,DEXTR4,
     -      AEXTR1,AEXTR2,AEXTR3,AEXTR4,
     -      BEXTR1,BEXTR2,BEXTR3,BEXTR4,
     -      HEXTR1,HEXTR2,HEXTR3,HEXTR4,
     -      MEXTR1,MEXTR2,MEXTR3,MEXTR4,
     -      WEXTR1,WEXTR2,WEXTR3,WEXTR4,
     -      OEXTR1,OEXTR2,OEXTR3,OEXTR4,
     -      SEXTR1(6),SEXTR2(6),SEXTR3(6),SEXTR4(6),
     -      EEXTR1(MXEXG),EEXTR2(MXEXG),EEXTR3(MXEXG),EEXTR4(MXEXG),
     -      ZEXTR1(MXIOG),ZEXTR2(MXIOG),ZEXTR3(MXIOG),ZEXTR4(MXIOG),
     -      IVMETH,IXMETH,IYMETH,IDMETH,IAMETH,IBMETH,IHMETH,IMMETH,
     -      IWMETH,IOMETH,ISMETH,IEMETH,IZMETH,
     -      IVEXTR,IXEXTR,IYEXTR,IDEXTR,IAEXTR,IBEXTR,IHEXTR,IMEXTR,
     -      IWEXTR,IOEXTR,ISEXTR,IEEXTR,IZEXTR,
     -      JVEXTR,JXEXTR,JYEXTR,JDEXTR,JAEXTR,JBEXTR,JHEXTR,JMEXTR,
     -      JWEXTR,JOEXTR,JSEXTR,JEEXTR,JZEXTR,
     -      NGAS,NCLS,NBANG,NBTAB,NFTAB,NFCLS,
     -      IATHR,IBTHR,IHTHR,
     -      NEXGAS,NIOGAS,NCSGAS,ICSTYP(MXCSG),
     -      GASOK(20),GASOPT(20,4),
     -      TAB2D,HEEDOK,SRIMOK,TRIMOK,GASSET
       COMMON /GASCHR/ FCNTAB,FCNCLS,CLSTYP,GASID,DSCEXG,DSCIOG,DSCCSG
       LOGICAL MAGOK
       REAL ALFA,B0X,B0Y,B0Z,SUSWIR,SUSGAS,BSCALE,BFMIN,BFMAX,
     -      BFXMIN,BFYMIN,BFZMIN,BFXMAX,BFYMAX,BFZMAX
       INTEGER MAGSRC,
     -      IBXTYP,IBYTYP,IBZTYP,
     -      IRB0X,IRB0Y,IRB0Z,IRV0X,IRV0Y,IRV0Z,
     -      IENB0X,IENB0Y,IENB0Z,IBXDIR,IBYDIR,IBZDIR,
     -      NCB0X,NCB0Y,NCB0Z
       CHARACTER*(MXCHAR) FUNB0X,FUNB0Y,FUNB0Z
       COMMON /MAGDAT/ ALFA,SUSWIR,SUSGAS,
     -      B0X,B0Y,B0Z,BSCALE,BFMIN,BFMAX,
     -      BFXMIN,BFYMIN,BFZMIN,BFXMAX,BFYMAX,BFZMAX,
     -      MAGSRC,IBXTYP,IBYTYP,IBZTYP,
     -      IRB0X,IRB0Y,IRB0Z,IRV0X,IRV0Y,IRV0Z,
     -      IENB0X,IENB0Y,IENB0Z,IBXDIR,IBYDIR,IBZDIR,
     -      NCB0X,NCB0Y,NCB0Z,
     -      MAGOK
       COMMON /MAGCHR/ FUNB0X,FUNB0Y,FUNB0Z
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
       DOUBLE PRECISION XPOS,YPOS,ZPOS,VD(3),UB(3),UEXB(3),ENORM
       REAL EX,EY,EZ,BX,BY,BZ,VOLT,GASVEL,GASVT1,GASVT2,GASMOB,GASLOR,
     -      PMU,ETOT,BTOT,ANGLE,Q,QS,VE,VB,VEXB
       INTEGER ITYPE,ILOC
       EXTERNAL GASVEL,GASMOB,GASLOR
*** Deal the with special case of vacuum drift - drift velocity unknown.
       IF(ITYPE.EQ.0)THEN
            PRINT *,' !!!!!! DLCVEL WARNING : Drift velocity for'//
     -           ' vacuum drift requested ; not defined, set to 0.'
            VD(1)=0.0D0
            VD(2)=0.0D0
            VD(3)=0.0D0
            ILOC=-11
            RETURN
       ENDIF
*** Calculate the electric field.
       CALL EFIELD(REAL(XPOS),REAL(YPOS),REAL(ZPOS),
     -      EX,EY,EZ,ETOT,VOLT,0,ILOC)
       IF(POLAR)THEN
            EZ=0
            ETOT=SQRT(EX**2+EY**2)
       ENDIF
       IF(ILOC.NE.0.OR.ETOT.LE.0)THEN
            VD(1)=0.0D0
            VD(2)=0.0D0
            VD(3)=0.0D0
            RETURN
       ENDIF
*** Electron without B field.
       IF(ITYPE.EQ.1.AND..NOT.MAGOK)THEN
*   Compute the mobility.
            IF(POLAR)THEN
                 PMU=GASVEL(EX/EXP(REAL(XPOS)),EY/EXP(REAL(XPOS)),EZ,
     -                BX,BY,BZ)/(EXP(XPOS)*ETOT)
            ELSE
                 PMU=GASVEL(EX,EY,EZ,BX,BY,BZ)/ETOT
            ENDIF
*   Store the velocity vector.
            VD(1)=Q*PMU*EX
            VD(2)=Q*PMU*EY
            VD(3)=Q*PMU*EZ
*** Electron (+ or - !) with B field and velocity vector.
       ELSEIF(ITYPE.EQ.1.AND.GASOK(1).AND.GASOK(9).AND.GASOK(10))THEN
*   Compute the B field.
            CALL BFIELD(REAL(XPOS),REAL(YPOS),REAL(ZPOS),BX,BY,BZ,BTOT)
*   Compute unit vectors along Btrans and ExB.
            UEXB(1)=EY*BZ-EZ*BY
            UEXB(2)=EZ*BX-EX*BZ
            UEXB(3)=EX*BY-EY*BX
            ENORM=SQRT(UEXB(1)**2+UEXB(2)**2+UEXB(3)**2)
            IF(ENORM.GT.0)THEN
                 UEXB(1)=UEXB(1)/ENORM
                 UEXB(2)=UEXB(2)/ENORM
                 UEXB(3)=UEXB(3)/ENORM
            ELSE
                 UEXB(1)=EX/ETOT
                 UEXB(2)=EY/ETOT
                 UEXB(3)=EZ/ETOT
            ENDIF
            UB(1)=UEXB(2)*EZ-UEXB(3)*EY
            UB(2)=UEXB(3)*EX-UEXB(1)*EZ
            UB(3)=UEXB(1)*EY-UEXB(2)*EX
            ENORM=SQRT(UB(1)**2+UB(2)**2+UB(3)**2)
            IF(ENORM.GT.0)THEN
                 UB(1)=UB(1)/ENORM
                 UB(2)=UB(2)/ENORM
                 UB(3)=UB(3)/ENORM
            ELSE
                 UB(1)=EX/ETOT
                 UB(2)=EY/ETOT
                 UB(3)=EZ/ETOT
            ENDIF
*   Compute the velocities in all directions.
            IF(POLAR)THEN
                 VE=GASVEL(EX/EXP(REAL(XPOS)),EY/EXP(REAL(XPOS)),EZ,
     -                BX,BY,BZ)/EXP(XPOS)
                 VB=GASVT1(EX/EXP(REAL(XPOS)),EY/EXP(REAL(XPOS)),EZ,
     -                BX,BY,BZ)/EXP(XPOS)
                 VEXB=GASVT2(EX/EXP(REAL(XPOS)),EY/EXP(REAL(XPOS)),EZ,
     -                BX,BY,BZ)/EXP(XPOS)
            ELSE
                 VE=GASVEL(EX,EY,EZ,BX,BY,BZ)
                 VB=GASVT1(EX,EY,EZ,BX,BY,BZ)
                 VEXB=GASVT2(EX,EY,EZ,BX,BY,BZ)
            ENDIF
*   Return the velocity vector.
            QS=SIGN(1.0,Q)
            VD(1)=QS*(VE*EX/ETOT+QS**2*VB*UB(1)+QS*VEXB*UEXB(1))
            VD(2)=QS*(VE*EY/ETOT+QS**2*VB*UB(2)+QS*VEXB*UEXB(2))
            VD(3)=QS*(VE*EZ/ETOT+QS**2*VB*UB(3)+QS*VEXB*UEXB(3))
*** Electron with B field and Lorentz angle.
       ELSEIF(ITYPE.EQ.1.AND.GASOK(7))THEN
*   Compute the B field.
            CALL BFIELD(REAL(XPOS),REAL(YPOS),REAL(ZPOS),BX,BY,BZ,BTOT)
*   Compute a unit vector along ExB.
            UEXB(1)=EY*BZ-EZ*BY
            UEXB(2)=EZ*BX-EX*BZ
            UEXB(3)=EX*BY-EY*BX
            ENORM=SQRT(UEXB(1)**2+UEXB(2)**2+UEXB(3)**2)
            IF(ENORM.GT.0)THEN
                 UEXB(1)=UEXB(1)/ENORM
                 UEXB(2)=UEXB(2)/ENORM
                 UEXB(3)=UEXB(3)/ENORM
            ELSE
                 UEXB(1)=EX/ETOT
                 UEXB(2)=EY/ETOT
                 UEXB(3)=EZ/ETOT
            ENDIF
*   Compute the velocities and the angle.
            IF(POLAR)THEN
                 VE=GASVEL(EX/EXP(REAL(XPOS)),EY/EXP(REAL(XPOS)),EZ,
     -                BX,BY,BZ)/EXP(XPOS)
                 ANGLE=GASLOR(EX/EXP(REAL(XPOS)),EY/EXP(REAL(XPOS)),EZ,
     -                BX,BY,BZ)
            ELSE
                 VE=GASVEL(EX,EY,EZ,BX,BY,BZ)
                 ANGLE=GASLOR(EX,EY,EZ,BX,BY,BZ)
            ENDIF
*   Return the velocity.
            VD(1)=Q*VE*(COS(ANGLE)*EX/ETOT+SIN(ANGLE)*UEXB(1))
            VD(2)=Q*VE*(COS(ANGLE)*EY/ETOT+SIN(ANGLE)*UEXB(2))
            VD(3)=Q*VE*(COS(ANGLE)*EZ/ETOT+SIN(ANGLE)*UEXB(3))
*** Electron with B field and nothing else known.
       ELSEIF(ITYPE.EQ.1)THEN
*   Compute the B field.
            CALL BFIELD(REAL(XPOS),REAL(YPOS),REAL(ZPOS),BX,BY,BZ,BTOT)
*   Compute the velocity.
            IF(POLAR)THEN
                 PMU=Q*GASVEL(EX/EXP(REAL(XPOS)),EY/EXP(REAL(XPOS)),EZ,
     -                BX,BY,BZ)/EXP(XPOS)
            ELSE
                 PMU=Q*GASVEL(EX,EY,EZ,BX,BY,BZ)/ETOT
            ENDIF
*   Return a velocity.
            VD(1)=PMU*(EX+PMU*(EY*BZ-EZ*BY)+
     -           BX*((BX*EX+BY*EY+BZ*EZ)*PMU**2))/(1+(PMU*BTOT)**2)
            VD(2)=PMU*(EY+PMU*(EZ*BX-EX*BZ)+
     -           BY*((BX*EX+BY*EY+BZ*EZ)*PMU**2))/(1+(PMU*BTOT)**2)
            VD(3)=PMU*(EZ+PMU*(EX*BY-EY*BX)+
     -           BZ*((BX*EX+BY*EY+BZ*EZ)*PMU**2))/(1+(PMU*BTOT)**2)
*** Ion without B field.
       ELSEIF(ITYPE.EQ.2.AND..NOT.MAGOK)THEN
*   Compute the mobility.
            IF(POLAR)THEN
                 PMU=GASMOB(EX/EXP(REAL(XPOS)),EY/EXP(REAL(XPOS)),EZ,
     -                BX,BY,BZ)/EXP(2*XPOS)
            ELSE
                 PMU=GASMOB(EX,EY,EZ,BX,BY,BZ)
            ENDIF
*   Store the velocity vector.
            VD(1)=Q*PMU*EX
            VD(2)=Q*PMU*EY
            VD(3)=Q*PMU*EZ
*** Ion with B field.
       ELSEIF(ITYPE.EQ.2)THEN
*   Compute the B field.
            CALL BFIELD(REAL(XPOS),REAL(YPOS),REAL(ZPOS),BX,BY,BZ,BTOT)
*   Compute the velocity.
            IF(POLAR)THEN
                 PMU=Q*GASMOB(EX/EXP(REAL(XPOS)),EY/EXP(REAL(XPOS)),EZ,
     -                BX,BY,BZ)/EXP(2*XPOS)
            ELSE
                 PMU=Q*GASMOB(EX,EY,EZ,BX,BY,BZ)
            ENDIF
*   Return a velocity.
            VD(1)=PMU*(EX+PMU*(EY*BZ-EZ*BY)+
     -           BX*((BX*EX+BY*EY+BZ*EZ)*PMU**2))/(1+(PMU*BTOT)**2)
            VD(2)=PMU*(EY+PMU*(EZ*BX-EX*BZ)+
     -           BY*((BX*EX+BY*EY+BZ*EZ)*PMU**2))/(1+(PMU*BTOT)**2)
            VD(3)=PMU*(EZ+PMU*(EX*BY-EY*BX)+
     -           BZ*((BX*EX+BY*EY+BZ*EZ)*PMU**2))/(1+(PMU*BTOT)**2)
*** Other cases.
       ELSE
            PRINT *,' !!!!!! DLCVEL WARNING : Unable to deal with the'//
     -           ' particle type / field combination; returning 0.'
            VD(1)=0.0D0
            VD(2)=0.0D0
            VD(3)=0.0D0
            RETURN
       ENDIF
       END
