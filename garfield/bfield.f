CDECK  ID>, BFIELD.
       SUBROUTINE BFIELD(XIN,YIN,ZIN,BX,BY,BZ,BTOT)
*-----------------------------------------------------------------------
*   BFIELD - Subroutine returning the magnetic field at (X1,Y1)
*            it calls -depending on the type of periodicity one of the
*            routines MAG00, MAGX0, MAG0Y or MAGXY.
*   VARIABLES : XIN,YIN,ZIN : Point where the B field is requested.
*   (Last changed on  1/ 9/09.)
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
       REAL XIN,YIN,ZIN,BX,BY,BZ,BTOT,XPOS,YPOS,VAR(3),RES(1)
       INTEGER ILOC,MODVAR(3),NVAR,MODRES(1),NREXP,IFAIL,
     -      ISB0X,ISV0X,ISB0Y,ISV0Y,ISB0Z,ISV0Z
*** Source of B field: 0 = none, 1 = COMPONENTS, 2 = field map
       IF(MAGSRC.EQ.0.OR..NOT.MAGOK)THEN
            BX=0
            BY=0
            BZ=0
            BTOT=0
       ELSEIF(MAGSRC.EQ.1)THEN
**  Compute Bx.
            IF(POLAR)THEN
                 BX=0
            ELSEIF(IBXTYP.EQ.0)THEN
                 BX=0
            ELSEIF(IBXTYP.EQ.1)THEN
                 BX=B0X
            ELSEIF(IBXTYP.EQ.2)THEN
                 VAR(1)=XIN
                 VAR(2)=YIN
                 VAR(3)=ZIN
                 MODVAR(1)=2
                 MODVAR(2)=2
                 MODVAR(3)=2
                 NVAR=3
                 NREXP=1
                 CALL AL2EXE(IENB0X,VAR,MODVAR,NVAR,RES,MODRES,NREXP,
     -                IFAIL)
                 IF(IFAIL.EQ.0.AND.MODRES(1).EQ.2)THEN
                      BX=RES(1)
                 ELSE
                      BX=0
                 ENDIF
            ELSEIF(IBXTYP.EQ.3)THEN
                 IF(IBXDIR.EQ.1)THEN
                      VAR(1)=XIN
                 ELSEIF(IBXDIR.EQ.2)THEN
                      VAR(1)=YIN
                 ELSEIF(IBXDIR.EQ.3)THEN
                      VAR(1)=ZIN
                 ENDIF
                 ISB0X=0
                 ISV0X=0
                 CALL MATIN1(IRV0X,IRB0X,1,VAR,RES,ISV0X,ISB0X,2,IFAIL)
                 IF(IFAIL.EQ.0)THEN
                      BX=RES(1)
                 ELSE
                      BX=0
                 ENDIF
            ENDIF
**  Compute By.
            IF(POLAR)THEN
                 BY=0
            ELSEIF(IBYTYP.EQ.0)THEN
                 BY=0
            ELSEIF(IBYTYP.EQ.1)THEN
                 BY=B0Y
            ELSEIF(IBYTYP.EQ.2)THEN
                 VAR(1)=XIN
                 VAR(2)=YIN
                 VAR(3)=ZIN
                 MODVAR(1)=2
                 MODVAR(2)=2
                 MODVAR(3)=2
                 NVAR=3
                 NREXP=1
                 CALL AL2EXE(IENB0Y,VAR,MODVAR,NVAR,RES,MODRES,NREXP,
     -                IFAIL)
                 IF(IFAIL.EQ.0.AND.MODRES(1).EQ.2)THEN
                      BY=RES(1)
                 ELSE
                      BY=0
                 ENDIF
            ELSEIF(IBYTYP.EQ.3)THEN
                 IF(IBYDIR.EQ.1)THEN
                      VAR(1)=XIN
                 ELSEIF(IBYDIR.EQ.2)THEN
                      VAR(1)=YIN
                 ELSEIF(IBYDIR.EQ.3)THEN
                      VAR(1)=ZIN
                 ENDIF
                 ISB0Y=0
                 ISV0Y=0
                 CALL MATIN1(IRV0Y,IRB0Y,1,VAR,RES,ISV0Y,ISB0Y,2,IFAIL)
                 IF(IFAIL.EQ.0)THEN
                      BY=RES(1)
                 ELSE
                      BY=0
                 ENDIF
            ENDIF
**  Compute Bz.
            IF(IBZTYP.EQ.0)THEN
                 BZ=0
            ELSEIF(IBZTYP.EQ.1)THEN
                 BZ=B0Z
            ELSEIF(IBZTYP.EQ.2)THEN
                 VAR(1)=XIN
                 VAR(2)=YIN
                 VAR(3)=ZIN
                 MODVAR(1)=2
                 MODVAR(2)=2
                 MODVAR(3)=2
                 NVAR=3
                 NREXP=1
                 CALL AL2EXE(IENB0Z,VAR,MODVAR,NVAR,RES,MODRES,NREXP,
     -                IFAIL)
                 IF(IFAIL.EQ.0.AND.MODRES(1).EQ.2)THEN
                      BZ=RES(1)
                 ELSE
                      BZ=0
                 ENDIF
            ELSEIF(IBZTYP.EQ.3)THEN
                 IF(IBZDIR.EQ.1)THEN
                      VAR(1)=XIN
                 ELSEIF(IBZDIR.EQ.2)THEN
                      VAR(1)=YIN
                 ELSEIF(IBZDIR.EQ.3)THEN
                      VAR(1)=ZIN
                 ENDIF
                 ISB0Z=0
                 ISV0Z=0
                 CALL MATIN1(IRV0Z,IRB0Z,1,VAR,RES,ISV0Z,ISB0Z,2,IFAIL)
                 IF(IFAIL.EQ.0)THEN
                      BZ=RES(1)
                 ELSE
                      BZ=0
                 ENDIF
            ENDIF
**  Combined treatment if the wire distortion is taken into account.
            IF(IBXTYP.EQ.1.AND.IBYTYP.EQ.1.AND.IBZTYP.EQ.1.AND.
     -           ALFA.NE.0)THEN
*   Reduce the coordinates in case of a periodic cell.
                 IF(PERX)THEN
                      XPOS=XIN-SX*ANINT(XIN/SX)
                 ELSE
                      XPOS=XIN
                 ENDIF
                 IF(PERY)THEN
                      YPOS=YIN-SY*ANINT(YIN/SY)
                 ELSE
                      YPOS=YIN
                 ENDIF
*   Next have the components of the field calculated.
                 IF(.NOT.PERX.AND..NOT.PERY)CALL MAG00(XPOS,YPOS,BX,BY)
                 IF(     PERX.AND..NOT.PERY)CALL MAGX0(XPOS,YPOS,BX,BY)
                 IF(.NOT.PERX.AND.     PERY)CALL MAG0Y(XPOS,YPOS,BX,BY)
                 IF(     PERX.AND.     PERY)CALL MAGXY(XPOS,YPOS,BX,BY)
            ENDIF
*** Field map: interpolation.
       ELSEIF(MAGSRC.EQ.2)THEN
            CALL MAGFMP(XIN,YIN,ZIN,BX,BY,BZ,ILOC)
       ENDIF
*** Scale to V.microsec/cm2.
       BX=BX*BSCALE
       BY=BY*BSCALE
       BZ=BZ*BSCALE
*** Calculate the norm,
       BTOT=SQRT(BX**2+BY**2+BZ**2)
       END
