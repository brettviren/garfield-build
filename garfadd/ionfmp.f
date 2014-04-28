CDECK  ID>, IONFMP.
       SUBROUTINE IONFMP(XIN,YIN,ZIN,EX,EY,EZ,ISW)
*-----------------------------------------------------------------------
*   IONFMP - Interpolates the weighting field map at (XPOS,YPOS,ZPOS).
*   (Last changed on  9/ 1/09.)
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
       REAL XIN,YIN,ZIN,XPOS,YPOS,ZPOS,EX,EY,EZ,XNEW,YNEW,ZNEW,
     -      T1,T2,T3,T4,AUXR,AUXPHI,AROT,XAUX,YAUX,ER,EAXIS,RCOOR,ZCOOR,
     -      DN1DUH,DN2DUH,DN3DUH,DN4DUH,DN5DUH,DN6DUH,DN7DUH,DN8DUH,
     -      DN1DVH,DN2DVH,DN3DVH,DN4DVH,DN5DVH,DN6DVH,DN7DVH,DN8DVH,
     -      DN1DWH,DN2DWH,DN3DWH,DN4DWH,DN5DWH,DN6DWH,DN7DWH,DN8DWH,
     -      DN1DUP,DN2DUP,DN3DUP,DN4DUP,DN5DUP,DN6DUP,
     -      DN1DVP,DN2DVP,DN3DVP,DN4DVP,DN5DVP,DN6DVP,
     -      DN1DWP,DN2DWP,DN3DWP,DN4DWP,DN5DWP,DN6DWP,
     -      DT1DX,DT2DX,DT3DX,DT4DX,
     -      DT1DY,DT2DY,DT3DY,DT4DY,
     -      DT1DZ,DT2DZ,DT3DZ,DT4DZ
       DOUBLE PRECISION JAC(4,4),DET
       INTEGER IMAP,NX,NY,NZ,ISW
       LOGICAL MIRRX,MIRRY,MIRRZ
*** Derivatives of the shape functions for hexahedrals.
       DN1DUH(T1,T2,T3)=-(1 - T2)*(1 - T3)/8
       DN2DUH(T1,T2,T3)= (1 - T2)*(1 - T3)/8
       DN3DUH(T1,T2,T3)= (1 + T2)*(1 - T3)/8
       DN4DUH(T1,T2,T3)=-(1 + T2)*(1 - T3)/8
       DN5DUH(T1,T2,T3)=-(1 - T2)*(1 + T3)/8
       DN6DUH(T1,T2,T3)= (1 - T2)*(1 + T3)/8
       DN7DUH(T1,T2,T3)= (1 + T2)*(1 + T3)/8
       DN8DUH(T1,T2,T3)=-(1 + T2)*(1 + T3)/8
       DN1DVH(T1,T2,T3)=-(1 - T1)*(1 - T3)/8
       DN2DVH(T1,T2,T3)=-(1 + T1)*(1 - T3)/8
       DN3DVH(T1,T2,T3)= (1 + T1)*(1 - T3)/8
       DN4DVH(T1,T2,T3)= (1 - T1)*(1 - T3)/8
       DN5DVH(T1,T2,T3)=-(1 - T1)*(1 + T3)/8
       DN6DVH(T1,T2,T3)=-(1 + T1)*(1 + T3)/8
       DN7DVH(T1,T2,T3)= (1 + T1)*(1 + T3)/8
       DN8DVH(T1,T2,T3)= (1 - T1)*(1 + T3)/8
       DN1DWH(T1,T2,T3)=-(1 - T1)*(1 - T2)/8
       DN2DWH(T1,T2,T3)=-(1 + T1)*(1 - T2)/8
       DN3DWH(T1,T2,T3)=-(1 + T1)*(1 + T2)/8
       DN4DWH(T1,T2,T3)=-(1 - T1)*(1 + T2)/8
       DN5DWH(T1,T2,T3)= (1 - T1)*(1 - T2)/8
       DN6DWH(T1,T2,T3)= (1 + T1)*(1 - T2)/8
       DN7DWH(T1,T2,T3)= (1 + T1)*(1 + T2)/8
       DN8DWH(T1,T2,T3)= (1 - T1)*(1 + T2)/8
*** Derivatives of the shape functions for pentahedrals.
       DN1DUP(T1,T2,T3)=(-1+T3)/2
       DN2DUP(T1,T2,T3)=( 1-T3)/2
       DN3DUP(T1,T2,T3)=0
       DN4DUP(T1,T2,T3)=(-1-T3)/2
       DN5DUP(T1,T2,T3)=( 1+T3)/2
       DN6DUP(T1,T2,T3)=0
       DN1DVP(T1,T2,T3)=(-1+T3)/2
       DN2DVP(T1,T2,T3)=0
       DN3DVP(T1,T2,T3)=( 1-T3)/2
       DN4DVP(T1,T2,T3)=(-1-T3)/2
       DN5DVP(T1,T2,T3)=0
       DN6DVP(T1,T2,T3)=( 1+T3)/2
       DN1DWP(T1,T2,T3)=(-1+T1+T2)/2
       DN2DWP(T1,T2,T3)=-T1/2
       DN3DWP(T1,T2,T3)=-T2/2
       DN4DWP(T1,T2,T3)=( 1-T1-T2)/2
       DN5DWP(T1,T2,T3)= T1/2
       DN6DWP(T1,T2,T3)= T2/2
*** Initial values.
       EX=0
       EY=0
       EZ=0
       XPOS=XIN-XFMOFF
       YPOS=YIN-YFMOFF
       ZPOS=ZIN-ZFMOFF
*** First see whether we at all have a grid.
       IF(.NOT.MAPFLG(1))RETURN
*** If chamber is periodic, reduce to the cell volume.
       MIRRX=.FALSE.
       IF(PERX)THEN
            XPOS=XMMIN+MOD(XPOS-XMMIN,XMMAX-XMMIN)
            IF(XPOS.LT.XMMIN)XPOS=XPOS+(XMMAX-XMMIN)
       ELSEIF(PERMX)THEN
            XNEW=XMMIN+MOD(XPOS-XMMIN,XMMAX-XMMIN)
            IF(XNEW.LT.XMMIN)XNEW=XNEW+(XMMAX-XMMIN)
            NX=NINT((XNEW-XPOS)/(XMMAX-XMMIN))
            IF(NX.NE.2*(NX/2))THEN
                 XNEW=XMMIN+XMMAX-XNEW
                 MIRRX=.TRUE.
            ENDIF
            XPOS=XNEW
       ENDIF
       IF(PERAX.AND.(ZPOS.NE.0.OR.YPOS.NE.0))THEN
            AUXR=SQRT(ZPOS**2+YPOS**2)
            AUXPHI=ATAN2(ZPOS,YPOS)
            AROT=(XAMAX-XAMIN)*ANINT((AUXPHI-0.5*(XAMIN+XAMAX))/
     -           (XAMAX-XAMIN))
            IF(AUXPHI-AROT.LT.XAMIN)AROT=AROT-(XAMAX-XAMIN)
            IF(AUXPHI-AROT.GT.XAMAX)AROT=AROT+(XAMAX-XAMIN)
            AUXPHI=AUXPHI-AROT
            YPOS=AUXR*COS(AUXPHI)
            ZPOS=AUXR*SIN(AUXPHI)
       ENDIF
       MIRRY=.FALSE.
       IF(PERY)THEN
            YPOS=YMMIN+MOD(YPOS-YMMIN,YMMAX-YMMIN)
            IF(YPOS.LT.YMMIN)YPOS=YPOS+(YMMAX-YMMIN)
       ELSEIF(PERMY)THEN
            YNEW=YMMIN+MOD(YPOS-YMMIN,YMMAX-YMMIN)
            IF(YNEW.LT.YMMIN)YNEW=YNEW+(YMMAX-YMMIN)
            NY=NINT((YNEW-YPOS)/(YMMAX-YMMIN))
            IF(NY.NE.2*(NY/2))THEN
                 YNEW=YMMIN+YMMAX-YNEW
                 MIRRY=.TRUE.
            ENDIF
            YPOS=YNEW
       ENDIF
       IF(PERAY.AND.(XPOS.NE.0.OR.ZPOS.NE.0))THEN
            AUXR=SQRT(XPOS**2+ZPOS**2)
            AUXPHI=ATAN2(XPOS,ZPOS)
            AROT=(YAMAX-YAMIN)*ANINT((AUXPHI-0.5*(YAMIN+YAMAX))/
     -           (YAMAX-YAMIN))
            IF(AUXPHI-AROT.LT.YAMIN)AROT=AROT-(YAMAX-YAMIN)
            IF(AUXPHI-AROT.GT.YAMAX)AROT=AROT+(YAMAX-YAMIN)
            AUXPHI=AUXPHI-AROT
            ZPOS=AUXR*COS(AUXPHI)
            XPOS=AUXR*SIN(AUXPHI)
       ENDIF
       MIRRZ=.FALSE.
       IF(PERZ)THEN
            ZPOS=ZMMIN+MOD(ZPOS-ZMMIN,ZMMAX-ZMMIN)
            IF(ZPOS.LT.ZMMIN)ZPOS=ZPOS+(ZMMAX-ZMMIN)
       ELSEIF(PERMZ)THEN
            ZNEW=ZMMIN+MOD(ZPOS-ZMMIN,ZMMAX-ZMMIN)
            IF(ZNEW.LT.ZMMIN)ZNEW=ZNEW+(ZMMAX-ZMMIN)
            NZ=NINT((ZNEW-ZPOS)/(ZMMAX-ZMMIN))
            IF(NZ.NE.2*(NZ/2))THEN
                 ZNEW=ZMMIN+ZMMAX-ZNEW
                 MIRRZ=.TRUE.
            ENDIF
            ZPOS=ZNEW
       ENDIF
       IF(PERAZ.AND.(YPOS.NE.0.OR.XPOS.NE.0))THEN
            AUXR=SQRT(YPOS**2+XPOS**2)
            AUXPHI=ATAN2(YPOS,XPOS)
            AROT=(ZAMAX-ZAMIN)*ANINT((AUXPHI-0.5*(ZAMIN+ZAMAX))/
     -           (ZAMAX-ZAMIN))
            IF(AUXPHI-AROT.LT.ZAMIN)AROT=AROT-(ZAMAX-ZAMIN)
            IF(AUXPHI-AROT.GT.ZAMAX)AROT=AROT+(ZAMAX-ZAMIN)
            AUXPHI=AUXPHI-AROT
            XPOS=AUXR*COS(AUXPHI)
            YPOS=AUXR*SIN(AUXPHI)
       ENDIF
*** If we have a rotationally symmetric field map, store coordinates.
       IF(PERRX)THEN
            RCOOR=SQRT(YPOS**2+ZPOS**2)
            ZCOOR=XPOS
       ELSEIF(PERRY)THEN
            RCOOR=SQRT(XPOS**2+ZPOS**2)
            ZCOOR=YPOS
       ELSEIF(PERRZ)THEN
            RCOOR=SQRT(XPOS**2+YPOS**2)
            ZCOOR=ZPOS
       ENDIF
       IF(PERRX.OR.PERRY.OR.PERRZ)THEN
            XPOS=RCOOR
            YPOS=ZCOOR
            ZPOS=0
       ENDIF
*** Locate the point.
       CALL MAPIND(XPOS,YPOS,ZPOS,T1,T2,T3,T4,JAC,DET,IMAP)
       IF(IMAP.LE.0.OR.IMAP.GT.NMAP)RETURN
*** A 2-dimensional interpolation (triangles), linear ...
       IF((MAPTYP.EQ.1.OR.MAPTYP.EQ.2.OR.MAPTYP.EQ.3).AND.
     -      MAPORD.EQ.1)THEN
            IF(LSFDER)THEN
                 DET=-(XMAP(IMAP,2)*YMAP(IMAP,1)-
     -                 XMAP(IMAP,3)*YMAP(IMAP,1)-
     -                 XMAP(IMAP,1)*YMAP(IMAP,2)+
     -                 XMAP(IMAP,3)*YMAP(IMAP,2)+
     -                 XMAP(IMAP,1)*YMAP(IMAP,3)-
     -                 XMAP(IMAP,2)*YMAP(IMAP,3))
                 DT1DX=(YMAP(IMAP,3) - YMAP(IMAP,2))/DET
                 DT1DY=(XMAP(IMAP,2) - XMAP(IMAP,3))/DET
                 DT2DX=(YMAP(IMAP,1) - YMAP(IMAP,3))/DET
                 DT2DY=(XMAP(IMAP,3) - XMAP(IMAP,1))/DET
                 DT3DX=(YMAP(IMAP,2) - YMAP(IMAP,1))/DET
                 DT3DY=(XMAP(IMAP,1) - XMAP(IMAP,2))/DET
                 EX=VWMAP(IMAP, 1,ISW)*DT1DX+
     -              VWMAP(IMAP, 2,ISW)*DT2DX+
     -              VWMAP(IMAP, 3,ISW)*DT3DX
                 EY=VWMAP(IMAP, 1,ISW)*DT1DY+
     -              VWMAP(IMAP, 2,ISW)*DT2DY+
     -              VWMAP(IMAP, 3,ISW)*DT3DY
                 EZ=0
            ELSE
                 IF(MAPFLG(10+4*ISW-3))EX=
     -                EWXMAP(IMAP,1,ISW)*T1+
     -                EWXMAP(IMAP,2,ISW)*T2+
     -                EWXMAP(IMAP,3,ISW)*T3
                 IF(MAPFLG(11+4*ISW-3))EY=
     -                EWYMAP(IMAP,1,ISW)*T1+
     -                EWYMAP(IMAP,2,ISW)*T2+
     -                EWYMAP(IMAP,3,ISW)*T3
                 IF(MAPFLG(12+4*ISW-3))EZ=
     -                EWZMAP(IMAP,1,ISW)*T1+
     -                EWZMAP(IMAP,2,ISW)*T2+
     -                EWZMAP(IMAP,3,ISW)*T3
            ENDIF
**  or quadratic.
       ELSEIF(MAPTYP.EQ.2.AND.MAPORD.EQ.2)THEN
*   By gradient of the potential
            IF(LSFDER)THEN
                 DET=-(XMAP(IMAP,2)*YMAP(IMAP,1)-
     -                 XMAP(IMAP,3)*YMAP(IMAP,1)-
     -                 XMAP(IMAP,1)*YMAP(IMAP,2)+
     -                 XMAP(IMAP,3)*YMAP(IMAP,2)+
     -                 XMAP(IMAP,1)*YMAP(IMAP,3)-
     -                 XMAP(IMAP,2)*YMAP(IMAP,3))
                 DT1DX=(YMAP(IMAP,3) - YMAP(IMAP,2))/DET
                 DT1DY=(XMAP(IMAP,2) - XMAP(IMAP,3))/DET
                 DT2DX=(YMAP(IMAP,1) - YMAP(IMAP,3))/DET
                 DT2DY=(XMAP(IMAP,3) - XMAP(IMAP,1))/DET
                 DT3DX=(YMAP(IMAP,2) - YMAP(IMAP,1))/DET
                 DT3DY=(XMAP(IMAP,1) - XMAP(IMAP,2))/DET
                 EX=VWMAP(IMAP, 1,ISW)*(4*T1-1)*DT1DX+
     -              VWMAP(IMAP, 2,ISW)*(4*T2-1)*DT2DX+
     -              VWMAP(IMAP, 3,ISW)*(4*T3-1)*DT3DX+
     -              VWMAP(IMAP, 4,ISW)*(4*T2*DT1DX+4*T1*DT2DX)+
     -              VWMAP(IMAP, 5,ISW)*(4*T3*DT1DX+4*T1*DT3DX)+
     -              VWMAP(IMAP, 6,ISW)*(4*T3*DT2DX+4*T2*DT3DX)
                 EY=VWMAP(IMAP, 1,ISW)*(4*T1-1)*DT1DY+
     -              VWMAP(IMAP, 2,ISW)*(4*T2-1)*DT2DY+
     -              VWMAP(IMAP, 3,ISW)*(4*T3-1)*DT3DY+
     -              VWMAP(IMAP, 4,ISW)*(4*T2*DT1DY+4*T1*DT2DY)+
     -              VWMAP(IMAP, 5,ISW)*(4*T3*DT1DY+4*T1*DT3DY)+
     -              VWMAP(IMAP, 6,ISW)*(4*T3*DT2DY+4*T2*DT3DY)
                 EZ=0
*   By interpolation of field maps.
            ELSE
                 IF(MAPFLG(10+4*ISW-3))EX=
     -                EWXMAP(IMAP,1,ISW)*T1*(2*T1-1)+
     -                EWXMAP(IMAP,2,ISW)*T2*(2*T2-1)+
     -                EWXMAP(IMAP,3,ISW)*T3*(2*T3-1)+
     -                4*EWXMAP(IMAP,4,ISW)*T1*T2+
     -                4*EWXMAP(IMAP,5,ISW)*T1*T3+
     -                4*EWXMAP(IMAP,6,ISW)*T2*T3
                 IF(MAPFLG(11+4*ISW-3))EY=
     -                EWYMAP(IMAP,1,ISW)*T1*(2*T1-1)+
     -                EWYMAP(IMAP,2,ISW)*T2*(2*T2-1)+
     -                EWYMAP(IMAP,3,ISW)*T3*(2*T3-1)+
     -                4*EWYMAP(IMAP,4,ISW)*T1*T2+
     -                4*EWYMAP(IMAP,5,ISW)*T1*T3+
     -                4*EWYMAP(IMAP,6,ISW)*T2*T3
                 IF(MAPFLG(12+4*ISW-3))EZ=
     -                EWZMAP(IMAP,1,ISW)*T1*(2*T1-1)+
     -                EWZMAP(IMAP,2,ISW)*T2*(2*T2-1)+
     -                EWZMAP(IMAP,3,ISW)*T3*(2*T3-1)+
     -                4*EWZMAP(IMAP,4,ISW)*T1*T2+
     -                4*EWZMAP(IMAP,5,ISW)*T1*T3+
     -                4*EWZMAP(IMAP,6,ISW)*T2*T3
            ENDIF
**  or quadratic interpolation in curved triangles ...
       ELSEIF((MAPTYP.EQ.3.AND.MAPORD.EQ.2).OR.
     -      (MAPTYP.EQ.5.AND.MAPORD.EQ.2.AND.ELMDGN(IMAP)))THEN
*   By gradient of the potential
            IF(LSFDER)THEN
                 EX=-(VWMAP(IMAP, 1,ISW)*(4*T1-1)*JAC(1,2)+
     -                VWMAP(IMAP, 2,ISW)*(4*T2-1)*JAC(2,2)+
     -                VWMAP(IMAP, 3,ISW)*(4*T3-1)*JAC(3,2)+
     -                VWMAP(IMAP, 4,ISW)*(4*T2*JAC(1,2)+4*T1*JAC(2,2))+
     -                VWMAP(IMAP, 5,ISW)*(4*T3*JAC(1,2)+4*T1*JAC(3,2))+
     -                VWMAP(IMAP, 6,ISW)*(4*T3*JAC(2,2)+4*T2*JAC(3,2)))/
     -                DET
                 EY=-(VWMAP(IMAP, 1,ISW)*(4*T1-1)*JAC(1,3)+
     -                VWMAP(IMAP, 2,ISW)*(4*T2-1)*JAC(2,3)+
     -                VWMAP(IMAP, 3,ISW)*(4*T3-1)*JAC(3,3)+
     -                VWMAP(IMAP, 4,ISW)*(4*T2*JAC(1,3)+4*T1*JAC(2,3))+
     -                VWMAP(IMAP, 5,ISW)*(4*T3*JAC(1,3)+4*T1*JAC(3,3))+
     -                VWMAP(IMAP, 6,ISW)*(4*T3*JAC(2,3)+4*T2*JAC(3,3)))/
     -                DET
                 EZ=0
*   By interpolation of field maps.
            ELSE
                 IF(MAPFLG(2))EX=
     -                EWXMAP(IMAP,1,ISW)*T1*(2*T1-1)+
     -                EWXMAP(IMAP,2,ISW)*T2*(2*T2-1)+
     -                EWXMAP(IMAP,3,ISW)*T3*(2*T3-1)+
     -                4*EWXMAP(IMAP,4,ISW)*T1*T2+
     -                4*EWXMAP(IMAP,5,ISW)*T1*T3+
     -                4*EWXMAP(IMAP,6,ISW)*T2*T3
                 IF(MAPFLG(3))EY=
     -                EWYMAP(IMAP,1,ISW)*T1*(2*T1-1)+
     -                EWYMAP(IMAP,2,ISW)*T2*(2*T2-1)+
     -                EWYMAP(IMAP,3,ISW)*T3*(2*T3-1)+
     -                4*EWYMAP(IMAP,4,ISW)*T1*T2+
     -                4*EWYMAP(IMAP,5,ISW)*T1*T3+
     -                4*EWYMAP(IMAP,6,ISW)*T2*T3
                 IF(MAPFLG(4))EZ=
     -                EWZMAP(IMAP,1,ISW)*T1*(2*T1-1)+
     -                EWZMAP(IMAP,2,ISW)*T2*(2*T2-1)+
     -                EWZMAP(IMAP,3,ISW)*T3*(2*T3-1)+
     -                4*EWZMAP(IMAP,4,ISW)*T1*T2+
     -                4*EWZMAP(IMAP,5,ISW)*T1*T3+
     -                4*EWZMAP(IMAP,6,ISW)*T2*T3
            ENDIF
**  or quadratic in 8-node "serendipity" quadrilaterals.
       ELSEIF(MAPTYP.EQ.5.AND.MAPORD.EQ.2)THEN
            IF(LSFDER.AND.MAPFLG(13+4*ISW-3))THEN
                 EX=-(VWMAP(IMAP, 1,ISW)*((1-T2)*(2*T1+T2)*JAC(1,1)+
     -                                    (1-T1)*(T1+2*T2)*JAC(2,1))/4+
     -                VWMAP(IMAP, 2,ISW)*((1-T2)*(2*T1-T2)*JAC(1,1)-
     -                                    (1+T1)*(T1-2*T2)*JAC(2,1))/4+
     -                VWMAP(IMAP, 3,ISW)*((1+T2)*(2*T1+T2)*JAC(1,1)+
     -                                    (1+T1)*(T1+2*T2)*JAC(2,1))/4+
     -                VWMAP(IMAP, 4,ISW)*((1+T2)*(2*T1-T2)*JAC(1,1)-
     -                                    (1-T1)*(T1-2*T2)*JAC(2,1))/4+
     -                VWMAP(IMAP, 5,ISW)*(T1*(T2-1)       *JAC(1,1)+
     -                                    (T1-1)*(T1+1)   *JAC(2,1)/2)+
     -                VWMAP(IMAP, 6,ISW)*((1-T2)*(1+T2)   *JAC(1,1)/2-
     -                                    (1+T1)*T2       *JAC(2,1))+
     -                VWMAP(IMAP, 7,ISW)*(-T1*(1+T2)      *JAC(1,1)+
     -                                    (1-T1)*(1+T1)   *JAC(2,1)/2)+
     -                VWMAP(IMAP, 8,ISW)*((T2-1)*(T2+1)   *JAC(1,1)/2+
     -                                    (T1-1)*T2       *JAC(2,1)))/
     -                DET
                 EY=-(VWMAP(IMAP, 1,ISW)*((1-T2)*(2*T1+T2)*JAC(1,2)+
     -                                    (1-T1)*(T1+2*T2)*JAC(2,2))/4+
     -                VWMAP(IMAP, 2,ISW)*((1-T2)*(2*T1-T2)*JAC(1,2)-
     -                                    (1+T1)*(T1-2*T2)*JAC(2,2))/4+
     -                VWMAP(IMAP, 3,ISW)*((1+T2)*(2*T1+T2)*JAC(1,2)+
     -                                    (1+T1)*(T1+2*T2)*JAC(2,2))/4+
     -                VWMAP(IMAP, 4,ISW)*((1+T2)*(2*T1-T2)*JAC(1,2)-
     -                                    (1-T1)*(T1-2*T2)*JAC(2,2))/4+
     -                VWMAP(IMAP, 5,ISW)*(T1*(T2-1)       *JAC(1,2)+
     -                                    (T1-1)*(T1+1)   *JAC(2,2)/2)+
     -                VWMAP(IMAP, 6,ISW)*((1-T2)*(1+T2)   *JAC(1,2)/2-
     -                                    (1+T1)*T2       *JAC(2,2))+
     -                VWMAP(IMAP, 7,ISW)*(-T1*(1+T2)      *JAC(1,2)+
     -                                    (1-T1)*(1+T1)   *JAC(2,2)/2)+
     -                VWMAP(IMAP, 8,ISW)*((T2-1)*(T2+1)   *JAC(1,2)/2+
     -                                    (T1-1)*T2       *JAC(2,2)))/
     -                DET
            ELSEIF(.NOT.LSFDER)THEN
                 IF(MAPFLG(10+4*ISW-3))EX=
     -                -EWXMAP(IMAP,1,ISW)*(1-T1)*(1-T2)*(1+T1+T2)/4-
     -                 EWXMAP(IMAP,2,ISW)*(1+T1)*(1-T2)*(1-T1+T2)/4-
     -                 EWXMAP(IMAP,3,ISW)*(1+T1)*(1+T2)*(1-T1-T2)/4-
     -                 EWXMAP(IMAP,4,ISW)*(1-T1)*(1+T2)*(1+T1-T2)/4+
     -                 EWXMAP(IMAP,5,ISW)*(1-T1)*(1+T1)*(1-T2)/2+
     -                 EWXMAP(IMAP,6,ISW)*(1+T1)*(1+T2)*(1-T2)/2+
     -                 EWXMAP(IMAP,7,ISW)*(1-T1)*(1+T1)*(1+T2)/2+
     -                 EWXMAP(IMAP,8,ISW)*(1-T1)*(1+T2)*(1-T2)/2
                 IF(MAPFLG(11+4*ISW-3))EY=
     -                -EWYMAP(IMAP,1,ISW)*(1-T1)*(1-T2)*(1+T1+T2)/4-
     -                 EWYMAP(IMAP,2,ISW)*(1+T1)*(1-T2)*(1-T1+T2)/4-
     -                 EWYMAP(IMAP,3,ISW)*(1+T1)*(1+T2)*(1-T1-T2)/4-
     -                 EWYMAP(IMAP,4,ISW)*(1-T1)*(1+T2)*(1+T1-T2)/4+
     -                 EWYMAP(IMAP,5,ISW)*(1-T1)*(1+T1)*(1-T2)/2+
     -                 EWYMAP(IMAP,6,ISW)*(1+T1)*(1+T2)*(1-T2)/2+
     -                 EWYMAP(IMAP,7,ISW)*(1-T1)*(1+T1)*(1+T2)/2+
     -                 EWYMAP(IMAP,8,ISW)*(1-T1)*(1+T2)*(1-T2)/2
            ENDIF
*** A 3-dimensional interpolation (tetrahedra), linear ...
       ELSEIF((MAPTYP.EQ.11.OR.MAPTYP.EQ.12.OR.MAPTYP.EQ.13).AND.
     -      MAPORD.EQ.1)THEN
            IF(LSFDER)THEN
                 DET = XMAP(IMAP,2)*YMAP(IMAP,3)*ZMAP(IMAP,1)-
     -                 XMAP(IMAP,2)*YMAP(IMAP,4)*ZMAP(IMAP,1)-
     -                 XMAP(IMAP,1)*YMAP(IMAP,3)*ZMAP(IMAP,2)+
     -                 XMAP(IMAP,1)*YMAP(IMAP,4)*ZMAP(IMAP,2)-
     -                 XMAP(IMAP,2)*YMAP(IMAP,1)*ZMAP(IMAP,3)+
     -                 XMAP(IMAP,1)*YMAP(IMAP,2)*ZMAP(IMAP,3)-
     -                 XMAP(IMAP,1)*YMAP(IMAP,4)*ZMAP(IMAP,3)+
     -                 XMAP(IMAP,2)*YMAP(IMAP,4)*ZMAP(IMAP,3)+
     -                 XMAP(IMAP,4)*(YMAP(IMAP,2)*ZMAP(IMAP,1)-
     -                               YMAP(IMAP,3)*ZMAP(IMAP,1)-
     -                               YMAP(IMAP,1)*ZMAP(IMAP,2)+
     -                               YMAP(IMAP,3)*ZMAP(IMAP,2)+
     -                               YMAP(IMAP,1)*ZMAP(IMAP,3)-
     -                               YMAP(IMAP,2)*ZMAP(IMAP,3))+
     -                 XMAP(IMAP,2)*YMAP(IMAP,1)*ZMAP(IMAP,4)-
     -                 XMAP(IMAP,1)*YMAP(IMAP,2)*ZMAP(IMAP,4)+
     -                 XMAP(IMAP,1)*YMAP(IMAP,3)*ZMAP(IMAP,4)-
     -                 XMAP(IMAP,2)*YMAP(IMAP,3)*ZMAP(IMAP,4)+
     -                 XMAP(IMAP,3)*(YMAP(IMAP,4)*ZMAP(IMAP,1)+
     -                               YMAP(IMAP,1)*ZMAP(IMAP,2)-
     -                               YMAP(IMAP,4)*ZMAP(IMAP,2)-
     -                               YMAP(IMAP,1)*ZMAP(IMAP,4)-
     -                               YMAP(IMAP,2)*ZMAP(IMAP,1)+
     -                               YMAP(IMAP,2)*ZMAP(IMAP,4))
                 EX=-(VWMAP(IMAP,1,ISW)*(
     -                     YMAP(IMAP,4)*( ZMAP(IMAP,2)-ZMAP(IMAP,3))+
     -                     YMAP(IMAP,2)*( ZMAP(IMAP,3)-ZMAP(IMAP,4))+
     -                     YMAP(IMAP,3)*(-ZMAP(IMAP,2)+ZMAP(IMAP,4)))+
     -                VWMAP(IMAP,2,ISW)*(
     -                     YMAP(IMAP,4)*(-ZMAP(IMAP,1)+ZMAP(IMAP,3))+
     -                     YMAP(IMAP,3)*( ZMAP(IMAP,1)-ZMAP(IMAP,4))+
     -                     YMAP(IMAP,1)*(-ZMAP(IMAP,3)+ZMAP(IMAP,4)))+
     -                VWMAP(IMAP,3,ISW)*(
     -                     YMAP(IMAP,4)*( ZMAP(IMAP,1)-ZMAP(IMAP,2))+
     -                     YMAP(IMAP,1)*( ZMAP(IMAP,2)-ZMAP(IMAP,4))+
     -                     YMAP(IMAP,2)*(-ZMAP(IMAP,1)+ZMAP(IMAP,4)))+
     -                VWMAP(IMAP,4,ISW)*(
     -                     YMAP(IMAP,3)*(-ZMAP(IMAP,1)+ZMAP(IMAP,2))+
     -                     YMAP(IMAP,2)*( ZMAP(IMAP,1)-ZMAP(IMAP,3))+
     -                     YMAP(IMAP,1)*(-ZMAP(IMAP,2)+ZMAP(IMAP,3))))/
     -                DET
                 EY=-(VWMAP(IMAP,1,ISW)*(
     -                     XMAP(IMAP,4)*(-ZMAP(IMAP,2)+ZMAP(IMAP,3))+
     -                     XMAP(IMAP,3)*( ZMAP(IMAP,2)-ZMAP(IMAP,4))+
     -                     XMAP(IMAP,2)*(-ZMAP(IMAP,3)+ZMAP(IMAP,4)))+
     -                VWMAP(IMAP,2,ISW)*(
     -                     XMAP(IMAP,4)*( ZMAP(IMAP,1)-ZMAP(IMAP,3))+
     -                     XMAP(IMAP,1)*( ZMAP(IMAP,3)-ZMAP(IMAP,4))+
     -                     XMAP(IMAP,3)*(-ZMAP(IMAP,1)+ZMAP(IMAP,4)))+
     -                VWMAP(IMAP,3,ISW)*(
     -                     XMAP(IMAP,4)*(-ZMAP(IMAP,1)+ZMAP(IMAP,2))+
     -                     XMAP(IMAP,2)*( ZMAP(IMAP,1)-ZMAP(IMAP,4))+
     -                     XMAP(IMAP,1)*(-ZMAP(IMAP,2)+ZMAP(IMAP,4)))+
     -                VWMAP(IMAP,4,ISW)*(
     -                     XMAP(IMAP,3)*( ZMAP(IMAP,1)-ZMAP(IMAP,2))+
     -                     XMAP(IMAP,1)*( ZMAP(IMAP,2)-ZMAP(IMAP,3))+
     -                     XMAP(IMAP,2)*(-ZMAP(IMAP,1)+ZMAP(IMAP,3))))/
     -                DET
                 EZ=-(VWMAP(IMAP,1,ISW)*(
     -                     XMAP(IMAP,4)*( YMAP(IMAP,2)-YMAP(IMAP,3))+
     -                     XMAP(IMAP,2)*( YMAP(IMAP,3)-YMAP(IMAP,4))+
     -                     XMAP(IMAP,3)*(-YMAP(IMAP,2)+YMAP(IMAP,4)))+
     -                VWMAP(IMAP,2,ISW)*(
     -                     XMAP(IMAP,4)*(-YMAP(IMAP,1)+YMAP(IMAP,3))+
     -                     XMAP(IMAP,3)*( YMAP(IMAP,1)-YMAP(IMAP,4))+
     -                     XMAP(IMAP,1)*(-YMAP(IMAP,3)+YMAP(IMAP,4)))+
     -                VWMAP(IMAP,3,ISW)*(
     -                     XMAP(IMAP,4)*( YMAP(IMAP,1)-YMAP(IMAP,2))+
     -                     XMAP(IMAP,1)*( YMAP(IMAP,2)-YMAP(IMAP,4))+
     -                     XMAP(IMAP,2)*(-YMAP(IMAP,1)+YMAP(IMAP,4)))+
     -                VWMAP(IMAP,4,ISW)*(
     -                     XMAP(IMAP,3)*(-YMAP(IMAP,1)+YMAP(IMAP,2))+
     -                     XMAP(IMAP,2)*( YMAP(IMAP,1)-YMAP(IMAP,3))+
     -                     XMAP(IMAP,1)*(-YMAP(IMAP,2)+YMAP(IMAP,3))))/
     -                DET
            ELSE
                 IF(MAPFLG(10+4*ISW-3))EX=
     -                EWXMAP(IMAP,1,ISW)*T1+
     -                EWXMAP(IMAP,2,ISW)*T2+
     -                EWXMAP(IMAP,3,ISW)*T3+
     -                EWXMAP(IMAP,4,ISW)*T4
                 IF(MAPFLG(11+4*ISW-3))EY=
     -                EWYMAP(IMAP,1,ISW)*T1+
     -                EWYMAP(IMAP,2,ISW)*T2+
     -                EWYMAP(IMAP,3,ISW)*T3+
     -                EWYMAP(IMAP,4,ISW)*T4
                 IF(MAPFLG(12+4*ISW-3))EZ=
     -                EWZMAP(IMAP,1,ISW)*T1+
     -                EWZMAP(IMAP,2,ISW)*T2+
     -                EWZMAP(IMAP,3,ISW)*T3+
     -                EWZMAP(IMAP,4,ISW)*T4
            ENDIF
**  or quadratic in linear tetrahedra ....
       ELSEIF(MAPTYP.EQ.12.AND.MAPORD.EQ.2)THEN
            IF(LSFDER)THEN
*   Determinant of the Jacobian.
                 DET=-((ZMAP(IMAP,1)-ZMAP(IMAP,2))*
     -                 (XMAP(IMAP,4)*( YMAP(IMAP,2)-YMAP(IMAP,3))+
     -                  XMAP(IMAP,2)*( YMAP(IMAP,3)-YMAP(IMAP,4))+
     -                  XMAP(IMAP,3)*(-YMAP(IMAP,2)+YMAP(IMAP,4)))+
     -                 (XMAP(IMAP,1)-XMAP(IMAP,2))*
     -                 (YMAP(IMAP,4)*( ZMAP(IMAP,2)-ZMAP(IMAP,3))+
     -                  YMAP(IMAP,2)*( ZMAP(IMAP,3)-ZMAP(IMAP,4))+
     -                  YMAP(IMAP,3)*(-ZMAP(IMAP,2)+ZMAP(IMAP,4)))+
     -                 (YMAP(IMAP,1)-YMAP(IMAP,2))*
     -                 (XMAP(IMAP,4)*(-ZMAP(IMAP,2)+ZMAP(IMAP,3))+
     -                  XMAP(IMAP,3)*( ZMAP(IMAP,2)-ZMAP(IMAP,4))+
     -                  XMAP(IMAP,2)*(-ZMAP(IMAP,3)+ZMAP(IMAP,4))))
*   Elements of the Jacobian
                 DT1DX=(YMAP(IMAP,4)*( ZMAP(IMAP,2)-ZMAP(IMAP,3))+
     -                  YMAP(IMAP,2)*( ZMAP(IMAP,3)-ZMAP(IMAP,4))+
     -                  YMAP(IMAP,3)*(-ZMAP(IMAP,2)+ZMAP(IMAP,4)))/DET
                 DT1DY=(XMAP(IMAP,4)*(-ZMAP(IMAP,2)+ZMAP(IMAP,3))+
     -                  XMAP(IMAP,3)*( ZMAP(IMAP,2)-ZMAP(IMAP,4))+
     -                  XMAP(IMAP,2)*(-ZMAP(IMAP,3)+ZMAP(IMAP,4)))/DET
                 DT1DZ=(XMAP(IMAP,4)*( YMAP(IMAP,2)-YMAP(IMAP,3))+
     -                  XMAP(IMAP,2)*( YMAP(IMAP,3)-YMAP(IMAP,4))+
     -                  XMAP(IMAP,3)*(-YMAP(IMAP,2)+YMAP(IMAP,4)))/DET
                 DT2DX=(YMAP(IMAP,4)*(-ZMAP(IMAP,1)+ZMAP(IMAP,3))+
     -                  YMAP(IMAP,3)*( ZMAP(IMAP,1)-ZMAP(IMAP,4))+
     -                  YMAP(IMAP,1)*(-ZMAP(IMAP,3)+ZMAP(IMAP,4)))/DET
                 DT2DY=(XMAP(IMAP,4)*( ZMAP(IMAP,1)-ZMAP(IMAP,3))+
     -                  XMAP(IMAP,1)*( ZMAP(IMAP,3)-ZMAP(IMAP,4))+
     -                  XMAP(IMAP,3)*(-ZMAP(IMAP,1)+ZMAP(IMAP,4)))/DET
                 DT2DZ=(XMAP(IMAP,4)*(-YMAP(IMAP,1)+YMAP(IMAP,3))+
     -                  XMAP(IMAP,3)*( YMAP(IMAP,1)-YMAP(IMAP,4))+
     -                  XMAP(IMAP,1)*(-YMAP(IMAP,3)+YMAP(IMAP,4)))/DET
                 DT3DX=(YMAP(IMAP,4)*( ZMAP(IMAP,1)-ZMAP(IMAP,2))+
     -                  YMAP(IMAP,1)*( ZMAP(IMAP,2)-ZMAP(IMAP,4))+
     -                  YMAP(IMAP,2)*(-ZMAP(IMAP,1)+ZMAP(IMAP,4)))/DET
                 DT3DY=(XMAP(IMAP,4)*(-ZMAP(IMAP,1)+ZMAP(IMAP,2))+
     -                  XMAP(IMAP,2)*( ZMAP(IMAP,1)-ZMAP(IMAP,4))+
     -                  XMAP(IMAP,1)*(-ZMAP(IMAP,2)+ZMAP(IMAP,4)))/DET
                 DT3DZ=(XMAP(IMAP,4)*( YMAP(IMAP,1)-YMAP(IMAP,2))+
     -                  XMAP(IMAP,1)*( YMAP(IMAP,2)-YMAP(IMAP,4))+
     -                  XMAP(IMAP,2)*(-YMAP(IMAP,1)+YMAP(IMAP,4)))/DET
                 DT4DX=(YMAP(IMAP,3)*(-ZMAP(IMAP,1)+ZMAP(IMAP,2))+
     -                  YMAP(IMAP,2)*( ZMAP(IMAP,1)-ZMAP(IMAP,3))+
     -                  YMAP(IMAP,1)*(-ZMAP(IMAP,2)+ZMAP(IMAP,3)))/DET
                 DT4DY=(XMAP(IMAP,3)*( ZMAP(IMAP,1)-ZMAP(IMAP,2))+
     -                  XMAP(IMAP,1)*( ZMAP(IMAP,2)-ZMAP(IMAP,3))+
     -                  XMAP(IMAP,2)*(-ZMAP(IMAP,1)+ZMAP(IMAP,3)))/DET
                 DT4DZ=(XMAP(IMAP,3)*(-YMAP(IMAP,1)+YMAP(IMAP,2))+
     -                  XMAP(IMAP,2)*( YMAP(IMAP,1)-YMAP(IMAP,3))+
     -                  XMAP(IMAP,1)*(-YMAP(IMAP,2)+YMAP(IMAP,3)))/DET
*   Electric field
                 EX=VWMAP(IMAP, 1,ISW)*(4*T1-1)*DT1DX+
     -              VWMAP(IMAP, 2,ISW)*(4*T2-1)*DT2DX+
     -              VWMAP(IMAP, 3,ISW)*(4*T3-1)*DT3DX+
     -              VWMAP(IMAP, 4,ISW)*(4*T4-1)*DT4DX+
     -              VWMAP(IMAP, 5,ISW)*(4*T2*DT1DX+4*T1*DT2DX)+
     -              VWMAP(IMAP, 6,ISW)*(4*T3*DT1DX+4*T1*DT3DX)+
     -              VWMAP(IMAP, 7,ISW)*(4*T4*DT1DX+4*T1*DT4DX)+
     -              VWMAP(IMAP, 8,ISW)*(4*T3*DT2DX+4*T2*DT3DX)+
     -              VWMAP(IMAP, 9,ISW)*(4*T4*DT2DX+4*T2*DT4DX)+
     -              VWMAP(IMAP,10,ISW)*(4*T4*DT3DX+4*T3*DT4DX)
                 EY=VWMAP(IMAP, 1,ISW)*(4*T1-1)*DT1DY+
     -              VWMAP(IMAP, 2,ISW)*(4*T2-1)*DT2DY+
     -              VWMAP(IMAP, 3,ISW)*(4*T3-1)*DT3DY+
     -              VWMAP(IMAP, 4,ISW)*(4*T4-1)*DT4DY+
     -              VWMAP(IMAP, 5,ISW)*(4*T2*DT1DY+4*T1*DT2DY)+
     -              VWMAP(IMAP, 6,ISW)*(4*T3*DT1DY+4*T1*DT3DY)+
     -              VWMAP(IMAP, 7,ISW)*(4*T4*DT1DY+4*T1*DT4DY)+
     -              VWMAP(IMAP, 8,ISW)*(4*T3*DT2DY+4*T2*DT3DY)+
     -              VWMAP(IMAP, 9,ISW)*(4*T4*DT2DY+4*T2*DT4DY)+
     -              VWMAP(IMAP,10,ISW)*(4*T4*DT3DY+4*T3*DT4DY)
                 EZ=VWMAP(IMAP, 1,ISW)*(4*T1-1)*DT1DZ+
     -              VWMAP(IMAP, 2,ISW)*(4*T2-1)*DT2DZ+
     -              VWMAP(IMAP, 3,ISW)*(4*T3-1)*DT3DZ+
     -              VWMAP(IMAP, 4,ISW)*(4*T4-1)*DT4DZ+
     -              VWMAP(IMAP, 5,ISW)*(4*T2*DT1DZ+4*T1*DT2DZ)+
     -              VWMAP(IMAP, 6,ISW)*(4*T3*DT1DZ+4*T1*DT3DZ)+
     -              VWMAP(IMAP, 7,ISW)*(4*T4*DT1DZ+4*T1*DT4DZ)+
     -              VWMAP(IMAP, 8,ISW)*(4*T3*DT2DZ+4*T2*DT3DZ)+
     -              VWMAP(IMAP, 9,ISW)*(4*T4*DT2DZ+4*T2*DT4DZ)+
     -              VWMAP(IMAP,10,ISW)*(4*T4*DT3DZ+4*T3*DT4DZ)
            ELSE
                 IF(MAPFLG(10+4*ISW-3))EX=
     -                EWXMAP(IMAP,1,ISW)*T1*(2*T1-1)+
     -                EWXMAP(IMAP,2,ISW)*T2*(2*T2-1)+
     -                EWXMAP(IMAP,3,ISW)*T3*(2*T3-1)+
     -                EWXMAP(IMAP,4,ISW)*T4*(2*T4-1)+
     -                4*EWXMAP(IMAP,5,ISW)*T1*T2+
     -                4*EWXMAP(IMAP,6,ISW)*T1*T3+
     -                4*EWXMAP(IMAP,7,ISW)*T1*T4+
     -                4*EWXMAP(IMAP,8,ISW)*T2*T3+
     -                4*EWXMAP(IMAP,9,ISW)*T2*T4+
     -                4*EWXMAP(IMAP,10,ISW)*T3*T4
                 IF(MAPFLG(11+4*ISW-3))EY=
     -                EWYMAP(IMAP,1,ISW)*T1*(2*T1-1)+
     -                EWYMAP(IMAP,2,ISW)*T2*(2*T2-1)+
     -                EWYMAP(IMAP,3,ISW)*T3*(2*T3-1)+
     -                EWYMAP(IMAP,4,ISW)*T4*(2*T4-1)+
     -                4*EWYMAP(IMAP,5,ISW)*T1*T2+
     -                4*EWYMAP(IMAP,6,ISW)*T1*T3+
     -                4*EWYMAP(IMAP,7,ISW)*T1*T4+
     -                4*EWYMAP(IMAP,8,ISW)*T2*T3+
     -                4*EWYMAP(IMAP,9,ISW)*T2*T4+
     -                4*EWYMAP(IMAP,10,ISW)*T3*T4
                 IF(MAPFLG(12+4*ISW-3))EZ=
     -                EWZMAP(IMAP,1,ISW)*T1*(2*T1-1)+
     -                EWZMAP(IMAP,2,ISW)*T2*(2*T2-1)+
     -                EWZMAP(IMAP,3,ISW)*T3*(2*T3-1)+
     -                EWZMAP(IMAP,4,ISW)*T4*(2*T4-1)+
     -                4*EWZMAP(IMAP,5,ISW)*T1*T2+
     -                4*EWZMAP(IMAP,6,ISW)*T1*T3+
     -                4*EWZMAP(IMAP,7,ISW)*T1*T4+
     -                4*EWZMAP(IMAP,8,ISW)*T2*T3+
     -                4*EWZMAP(IMAP,9,ISW)*T2*T4+
     -                4*EWZMAP(IMAP,10,ISW)*T3*T4
            ENDIF
**  or quadratic in quadratic tetrahedra ....
       ELSEIF(MAPTYP.EQ.13.AND.MAPORD.EQ.2)THEN
            IF(LSFDER.AND.MAPFLG(13+4*ISW-3))THEN
                 EX=-(VWMAP(IMAP, 1,ISW)*(4*T1-1)*JAC(1,2)+
     -                VWMAP(IMAP, 2,ISW)*(4*T2-1)*JAC(2,2)+
     -                VWMAP(IMAP, 3,ISW)*(4*T3-1)*JAC(3,2)+
     -                VWMAP(IMAP, 4,ISW)*(4*T4-1)*JAC(4,2)+
     -                VWMAP(IMAP, 5,ISW)*(4*T2*JAC(1,2)+4*T1*JAC(2,2))+
     -                VWMAP(IMAP, 6,ISW)*(4*T3*JAC(1,2)+4*T1*JAC(3,2))+
     -                VWMAP(IMAP, 7,ISW)*(4*T4*JAC(1,2)+4*T1*JAC(4,2))+
     -                VWMAP(IMAP, 8,ISW)*(4*T3*JAC(2,2)+4*T2*JAC(3,2))+
     -                VWMAP(IMAP, 9,ISW)*(4*T4*JAC(2,2)+4*T2*JAC(4,2))+
     -                VWMAP(IMAP,10,ISW)*(4*T4*JAC(3,2)+4*T3*JAC(4,2)))/
     -                DET
                 EY=-(VWMAP(IMAP, 1,ISW)*(4*T1-1)*JAC(1,3)+
     -                VWMAP(IMAP, 2,ISW)*(4*T2-1)*JAC(2,3)+
     -                VWMAP(IMAP, 3,ISW)*(4*T3-1)*JAC(3,3)+
     -                VWMAP(IMAP, 4,ISW)*(4*T4-1)*JAC(4,3)+
     -                VWMAP(IMAP, 5,ISW)*(4*T2*JAC(1,3)+4*T1*JAC(2,3))+
     -                VWMAP(IMAP, 6,ISW)*(4*T3*JAC(1,3)+4*T1*JAC(3,3))+
     -                VWMAP(IMAP, 7,ISW)*(4*T4*JAC(1,3)+4*T1*JAC(4,3))+
     -                VWMAP(IMAP, 8,ISW)*(4*T3*JAC(2,3)+4*T2*JAC(3,3))+
     -                VWMAP(IMAP, 9,ISW)*(4*T4*JAC(2,3)+4*T2*JAC(4,3))+
     -                VWMAP(IMAP,10,ISW)*(4*T4*JAC(3,3)+4*T3*JAC(4,3)))/
     -                DET
                 EZ=-(VWMAP(IMAP, 1,ISW)*(4*T1-1)*JAC(1,4)+
     -                VWMAP(IMAP, 2,ISW)*(4*T2-1)*JAC(2,4)+
     -                VWMAP(IMAP, 3,ISW)*(4*T3-1)*JAC(3,4)+
     -                VWMAP(IMAP, 4,ISW)*(4*T4-1)*JAC(4,4)+
     -                VWMAP(IMAP, 5,ISW)*(4*T2*JAC(1,4)+4*T1*JAC(2,4))+
     -                VWMAP(IMAP, 6,ISW)*(4*T3*JAC(1,4)+4*T1*JAC(3,4))+
     -                VWMAP(IMAP, 7,ISW)*(4*T4*JAC(1,4)+4*T1*JAC(4,4))+
     -                VWMAP(IMAP, 8,ISW)*(4*T3*JAC(2,4)+4*T2*JAC(3,4))+
     -                VWMAP(IMAP, 9,ISW)*(4*T4*JAC(2,4)+4*T2*JAC(4,4))+
     -                VWMAP(IMAP,10,ISW)*(4*T4*JAC(3,4)+4*T3*JAC(4,4)))/
     -                DET
C      print *,' At ',t1,t2,t3,t4,' for isw = ',isw,' imap = ',imap,
C     -     ' E = ',ex,ey,ez,' vwmap = ',(vwmap(imap,i,isw),i=1,10)
            ELSEIF(.NOT.LSFDER)THEN
                 IF(MAPFLG(10+4*ISW-3))EX=
     -                EWXMAP(IMAP,1,ISW)*T1*(2*T1-1)+
     -                EWXMAP(IMAP,2,ISW)*T2*(2*T2-1)+
     -                EWXMAP(IMAP,3,ISW)*T3*(2*T3-1)+
     -                EWXMAP(IMAP,4,ISW)*T4*(2*T4-1)+
     -                4*EWXMAP(IMAP,5,ISW)*T1*T2+
     -                4*EWXMAP(IMAP,6,ISW)*T1*T3+
     -                4*EWXMAP(IMAP,7,ISW)*T1*T4+
     -                4*EWXMAP(IMAP,8,ISW)*T2*T3+
     -                4*EWXMAP(IMAP,9,ISW)*T2*T4+
     -                4*EWXMAP(IMAP,10,ISW)*T3*T4
                 IF(MAPFLG(11+4*ISW-3))EY=
     -                EWYMAP(IMAP,1,ISW)*T1*(2*T1-1)+
     -                EWYMAP(IMAP,2,ISW)*T2*(2*T2-1)+
     -                EWYMAP(IMAP,3,ISW)*T3*(2*T3-1)+
     -                EWYMAP(IMAP,4,ISW)*T4*(2*T4-1)+
     -                4*EWYMAP(IMAP,5,ISW)*T1*T2+
     -                4*EWYMAP(IMAP,6,ISW)*T1*T3+
     -                4*EWYMAP(IMAP,7,ISW)*T1*T4+
     -                4*EWYMAP(IMAP,8,ISW)*T2*T3+
     -                4*EWYMAP(IMAP,9,ISW)*T2*T4+
     -                4*EWYMAP(IMAP,10,ISW)*T3*T4
                 IF(MAPFLG(12+4*ISW-3))EZ=
     -                EWZMAP(IMAP,1,ISW)*T1*(2*T1-1)+
     -                EWZMAP(IMAP,2,ISW)*T2*(2*T2-1)+
     -                EWZMAP(IMAP,3,ISW)*T3*(2*T3-1)+
     -                EWZMAP(IMAP,4,ISW)*T4*(2*T4-1)+
     -                4*EWZMAP(IMAP,5,ISW)*T1*T2+
     -                4*EWZMAP(IMAP,6,ISW)*T1*T3+
     -                4*EWZMAP(IMAP,7,ISW)*T1*T4+
     -                4*EWZMAP(IMAP,8,ISW)*T2*T3+
     -                4*EWZMAP(IMAP,9,ISW)*T2*T4+
     -                4*EWZMAP(IMAP,10,ISW)*T3*T4
            ENDIF
*** Or an interpolation on a non-degerate hexahedron, interpolate.
       ELSEIF(MAPTYP.EQ.14.AND..NOT.ELMDGN(IMAP).AND..NOT.LSFDER)THEN
            IF(MAPFLG(10+4*ISW-3))EX=
     -           EWXMAP(IMAP,1,ISW)*(1-T1)*(1-T2)*(1-T3)/8+
     -           EWXMAP(IMAP,2,ISW)*(1+T1)*(1-T2)*(1-T3)/8+
     -           EWXMAP(IMAP,3,ISW)*(1+T1)*(1+T2)*(1-T3)/8+
     -           EWXMAP(IMAP,4,ISW)*(1-T1)*(1+T2)*(1-T3)/8+
     -           EWXMAP(IMAP,5,ISW)*(1-T1)*(1-T2)*(1+T3)/8+
     -           EWXMAP(IMAP,6,ISW)*(1+T1)*(1-T2)*(1+T3)/8+
     -           EWXMAP(IMAP,7,ISW)*(1+T1)*(1+T2)*(1+T3)/8+
     -           EWXMAP(IMAP,8,ISW)*(1-T1)*(1+T2)*(1+T3)/8
            IF(MAPFLG(11+4*ISW-3))EY=
     -           EWYMAP(IMAP,1,ISW)*(1-T1)*(1-T2)*(1-T3)/8+
     -           EWYMAP(IMAP,2,ISW)*(1+T1)*(1-T2)*(1-T3)/8+
     -           EWYMAP(IMAP,3,ISW)*(1+T1)*(1+T2)*(1-T3)/8+
     -           EWYMAP(IMAP,4,ISW)*(1-T1)*(1+T2)*(1-T3)/8+
     -           EWYMAP(IMAP,5,ISW)*(1-T1)*(1-T2)*(1+T3)/8+
     -           EWYMAP(IMAP,6,ISW)*(1+T1)*(1-T2)*(1+T3)/8+
     -           EWYMAP(IMAP,7,ISW)*(1+T1)*(1+T2)*(1+T3)/8+
     -           EWYMAP(IMAP,8,ISW)*(1-T1)*(1+T2)*(1+T3)/8
            IF(MAPFLG(12+4*ISW-3))EZ=
     -           EWZMAP(IMAP,1,ISW)*(1-T1)*(1-T2)*(1-T3)/8+
     -           EWZMAP(IMAP,2,ISW)*(1+T1)*(1-T2)*(1-T3)/8+
     -           EWZMAP(IMAP,3,ISW)*(1+T1)*(1+T2)*(1-T3)/8+
     -           EWZMAP(IMAP,4,ISW)*(1-T1)*(1+T2)*(1-T3)/8+
     -           EWZMAP(IMAP,5,ISW)*(1-T1)*(1-T2)*(1+T3)/8+
     -           EWZMAP(IMAP,6,ISW)*(1+T1)*(1-T2)*(1+T3)/8+
     -           EWZMAP(IMAP,7,ISW)*(1+T1)*(1+T2)*(1+T3)/8+
     -           EWZMAP(IMAP,8,ISW)*(1-T1)*(1+T2)*(1+T3)/8
**  or an interpolation on a non-degerate hexahedron, derivatives.
       ELSEIF(MAPTYP.EQ.14.AND..NOT.ELMDGN(IMAP).AND.LSFDER)THEN
            IF(MAPFLG(13+4*ISW-3))EX=
     -         -(VWMAP(IMAP,1,ISW)*(DN1DUH(T1,T2,T3)*JAC(1,1)+
     -                         DN1DVH(T1,T2,T3)*JAC(2,1)+
     -                         DN1DWH(T1,T2,T3)*JAC(3,1))+
     -           VWMAP(IMAP,2,ISW)*(DN2DUH(T1,T2,T3)*JAC(1,1)+
     -                         DN2DVH(T1,T2,T3)*JAC(2,1)+
     -                         DN2DWH(T1,T2,T3)*JAC(3,1))+
     -           VWMAP(IMAP,3,ISW)*(DN3DUH(T1,T2,T3)*JAC(1,1)+
     -                         DN3DVH(T1,T2,T3)*JAC(2,1)+
     -                         DN3DWH(T1,T2,T3)*JAC(3,1))+
     -           VWMAP(IMAP,4,ISW)*(DN4DUH(T1,T2,T3)*JAC(1,1)+
     -                         DN4DVH(T1,T2,T3)*JAC(2,1)+
     -                         DN4DWH(T1,T2,T3)*JAC(3,1))+
     -           VWMAP(IMAP,5,ISW)*(DN5DUH(T1,T2,T3)*JAC(1,1)+
     -                         DN5DVH(T1,T2,T3)*JAC(2,1)+
     -                         DN5DWH(T1,T2,T3)*JAC(3,1))+
     -           VWMAP(IMAP,6,ISW)*(DN6DUH(T1,T2,T3)*JAC(1,1)+
     -                         DN6DVH(T1,T2,T3)*JAC(2,1)+
     -                         DN6DWH(T1,T2,T3)*JAC(3,1))+
     -           VWMAP(IMAP,7,ISW)*(DN7DUH(T1,T2,T3)*JAC(1,1)+
     -                         DN7DVH(T1,T2,T3)*JAC(2,1)+
     -                         DN7DWH(T1,T2,T3)*JAC(3,1))+
     -           VWMAP(IMAP,8,ISW)*(DN8DUH(T1,T2,T3)*JAC(1,1)+
     -                         DN8DVH(T1,T2,T3)*JAC(2,1)+
     -                         DN8DWH(T1,T2,T3)*JAC(3,1)))/DET
            IF(MAPFLG(13+4*ISW-3))EY=
     -         -(VWMAP(IMAP,1,ISW)*(DN1DUH(T1,T2,T3)*JAC(1,2)+
     -                         DN1DVH(T1,T2,T3)*JAC(2,2)+
     -                         DN1DWH(T1,T2,T3)*JAC(3,2))+
     -           VWMAP(IMAP,2,ISW)*(DN2DUH(T1,T2,T3)*JAC(1,2)+
     -                         DN2DVH(T1,T2,T3)*JAC(2,2)+
     -                         DN2DWH(T1,T2,T3)*JAC(3,2))+
     -           VWMAP(IMAP,3,ISW)*(DN3DUH(T1,T2,T3)*JAC(1,2)+
     -                         DN3DVH(T1,T2,T3)*JAC(2,2)+
     -                         DN3DWH(T1,T2,T3)*JAC(3,2))+
     -           VWMAP(IMAP,4,ISW)*(DN4DUH(T1,T2,T3)*JAC(1,2)+
     -                         DN4DVH(T1,T2,T3)*JAC(2,2)+
     -                         DN4DWH(T1,T2,T3)*JAC(3,2))+
     -           VWMAP(IMAP,5,ISW)*(DN5DUH(T1,T2,T3)*JAC(1,2)+
     -                         DN5DVH(T1,T2,T3)*JAC(2,2)+
     -                         DN5DWH(T1,T2,T3)*JAC(3,2))+
     -           VWMAP(IMAP,6,ISW)*(DN6DUH(T1,T2,T3)*JAC(1,2)+
     -                         DN6DVH(T1,T2,T3)*JAC(2,2)+
     -                         DN6DWH(T1,T2,T3)*JAC(3,2))+
     -           VWMAP(IMAP,7,ISW)*(DN7DUH(T1,T2,T3)*JAC(1,2)+
     -                         DN7DVH(T1,T2,T3)*JAC(2,2)+
     -                         DN7DWH(T1,T2,T3)*JAC(3,2))+
     -           VWMAP(IMAP,8,ISW)*(DN8DUH(T1,T2,T3)*JAC(1,2)+
     -                         DN8DVH(T1,T2,T3)*JAC(2,2)+
     -                         DN8DWH(T1,T2,T3)*JAC(3,2)))/DET
            IF(MAPFLG(13+4*ISW-3))EZ=
     -         -(VWMAP(IMAP,1,ISW)*(DN1DUH(T1,T2,T3)*JAC(1,3)+
     -                         DN1DVH(T1,T2,T3)*JAC(2,3)+
     -                         DN1DWH(T1,T2,T3)*JAC(3,3))+
     -           VWMAP(IMAP,2,ISW)*(DN2DUH(T1,T2,T3)*JAC(1,3)+
     -                         DN2DVH(T1,T2,T3)*JAC(2,3)+
     -                         DN2DWH(T1,T2,T3)*JAC(3,3))+
     -           VWMAP(IMAP,3,ISW)*(DN3DUH(T1,T2,T3)*JAC(1,3)+
     -                         DN3DVH(T1,T2,T3)*JAC(2,3)+
     -                         DN3DWH(T1,T2,T3)*JAC(3,3))+
     -           VWMAP(IMAP,4,ISW)*(DN4DUH(T1,T2,T3)*JAC(1,3)+
     -                         DN4DVH(T1,T2,T3)*JAC(2,3)+
     -                         DN4DWH(T1,T2,T3)*JAC(3,3))+
     -           VWMAP(IMAP,5,ISW)*(DN5DUH(T1,T2,T3)*JAC(1,3)+
     -                         DN5DVH(T1,T2,T3)*JAC(2,3)+
     -                         DN5DWH(T1,T2,T3)*JAC(3,3))+
     -           VWMAP(IMAP,6,ISW)*(DN6DUH(T1,T2,T3)*JAC(1,3)+
     -                         DN6DVH(T1,T2,T3)*JAC(2,3)+
     -                         DN6DWH(T1,T2,T3)*JAC(3,3))+
     -           VWMAP(IMAP,7,ISW)*(DN7DUH(T1,T2,T3)*JAC(1,3)+
     -                         DN7DVH(T1,T2,T3)*JAC(2,3)+
     -                         DN7DWH(T1,T2,T3)*JAC(3,3))+
     -           VWMAP(IMAP,8,ISW)*(DN8DUH(T1,T2,T3)*JAC(1,3)+
     -                         DN8DVH(T1,T2,T3)*JAC(2,3)+
     -                         DN8DWH(T1,T2,T3)*JAC(3,3)))/DET
**  or an interpolation on a degerate hexahedron, interpolate
       ELSEIF(MAPTYP.EQ.14.AND.ELMDGN(IMAP).AND..NOT.LSFDER)THEN
            IF(MAPFLG(10+4*ISW-3))EX=
     -           EWXMAP(IMAP,1,ISW)*(1-T1-T2)*(1-T3)/2+
     -           EWXMAP(IMAP,2,ISW)*      T1 *(1-T3)/2+
     -           EWXMAP(IMAP,3,ISW)*      T2 *(1-T3)/2+
     -           EWXMAP(IMAP,5,ISW)*(1-T1-T2)*(1+T3)/2+
     -           EWXMAP(IMAP,6,ISW)*      T1 *(1+T3)/2+
     -           EWXMAP(IMAP,7,ISW)*      T2 *(1+T3)/2
            IF(MAPFLG(11+4*ISW-3))EY=
     -           EWYMAP(IMAP,1,ISW)*(1-T1-T2)*(1-T3)/2+
     -           EWYMAP(IMAP,2,ISW)*      T1 *(1-T3)/2+
     -           EWYMAP(IMAP,3,ISW)*      T2 *(1-T3)/2+
     -           EWYMAP(IMAP,5,ISW)*(1-T1-T2)*(1+T3)/2+
     -           EWYMAP(IMAP,6,ISW)*      T1 *(1+T3)/2+
     -           EWYMAP(IMAP,7,ISW)*      T2 *(1+T3)/2
            IF(MAPFLG(12+4*ISW-3))EZ=
     -           EWZMAP(IMAP,1,ISW)*(1-T1-T2)*(1-T3)/2+
     -           EWZMAP(IMAP,2,ISW)*      T1 *(1-T3)/2+
     -           EWZMAP(IMAP,3,ISW)*      T2 *(1-T3)/2+
     -           EWZMAP(IMAP,5,ISW)*(1-T1-T2)*(1+T3)/2+
     -           EWZMAP(IMAP,6,ISW)*      T1 *(1+T3)/2+
     -           EWZMAP(IMAP,7,ISW)*      T2 *(1+T3)/2
**  or an interpolation on a degerate hexahedron, derivatives.
       ELSEIF(MAPTYP.EQ.14.AND.ELMDGN(IMAP).AND.LSFDER)THEN
            IF(MAPFLG(13+4*ISW-3))EX=
     -         -(VWMAP(IMAP,1,ISW)*(DN1DUP(T1,T2,T3)*JAC(1,1)+
     -                         DN1DVP(T1,T2,T3)*JAC(2,1)+
     -                         DN1DWP(T1,T2,T3)*JAC(3,1))+
     -           VWMAP(IMAP,2,ISW)*(DN2DUP(T1,T2,T3)*JAC(1,1)+
     -                         DN2DVP(T1,T2,T3)*JAC(2,1)+
     -                         DN2DWP(T1,T2,T3)*JAC(3,1))+
     -           VWMAP(IMAP,3,ISW)*(DN3DUP(T1,T2,T3)*JAC(1,1)+
     -                         DN3DVP(T1,T2,T3)*JAC(2,1)+
     -                         DN3DWP(T1,T2,T3)*JAC(3,1))+
     -           VWMAP(IMAP,5,ISW)*(DN4DUP(T1,T2,T3)*JAC(1,1)+
     -                         DN4DVP(T1,T2,T3)*JAC(2,1)+
     -                         DN4DWP(T1,T2,T3)*JAC(3,1))+
     -           VWMAP(IMAP,6,ISW)*(DN5DUP(T1,T2,T3)*JAC(1,1)+
     -                         DN5DVP(T1,T2,T3)*JAC(2,1)+
     -                         DN5DWP(T1,T2,T3)*JAC(3,1))+
     -           VWMAP(IMAP,7,ISW)*(DN6DUP(T1,T2,T3)*JAC(1,1)+
     -                         DN6DVP(T1,T2,T3)*JAC(2,1)+
     -                         DN6DWP(T1,T2,T3)*JAC(3,1)))/DET
            IF(MAPFLG(13+4*ISW-3))EY=
     -         -(VWMAP(IMAP,1,ISW)*(DN1DUP(T1,T2,T3)*JAC(1,2)+
     -                         DN1DVP(T1,T2,T3)*JAC(2,2)+
     -                         DN1DWP(T1,T2,T3)*JAC(3,2))+
     -           VWMAP(IMAP,2,ISW)*(DN2DUP(T1,T2,T3)*JAC(1,2)+
     -                         DN2DVP(T1,T2,T3)*JAC(2,2)+
     -                         DN2DWP(T1,T2,T3)*JAC(3,2))+
     -           VWMAP(IMAP,3,ISW)*(DN3DUP(T1,T2,T3)*JAC(1,2)+
     -                         DN3DVP(T1,T2,T3)*JAC(2,2)+
     -                         DN3DWP(T1,T2,T3)*JAC(3,2))+
     -           VWMAP(IMAP,5,ISW)*(DN4DUP(T1,T2,T3)*JAC(1,2)+
     -                         DN4DVP(T1,T2,T3)*JAC(2,2)+
     -                         DN4DWP(T1,T2,T3)*JAC(3,2))+
     -           VWMAP(IMAP,6,ISW)*(DN5DUP(T1,T2,T3)*JAC(1,2)+
     -                         DN5DVP(T1,T2,T3)*JAC(2,2)+
     -                         DN5DWP(T1,T2,T3)*JAC(3,2))+
     -           VWMAP(IMAP,7,ISW)*(DN6DUP(T1,T2,T3)*JAC(1,2)+
     -                         DN6DVP(T1,T2,T3)*JAC(2,2)+
     -                         DN6DWP(T1,T2,T3)*JAC(3,2)))/DET
            IF(MAPFLG(13+4*ISW-3))EZ=
     -         -(VWMAP(IMAP,1,ISW)*(DN1DUP(T1,T2,T3)*JAC(1,3)+
     -                         DN1DVP(T1,T2,T3)*JAC(2,3)+
     -                         DN1DWP(T1,T2,T3)*JAC(3,3))+
     -           VWMAP(IMAP,2,ISW)*(DN2DUP(T1,T2,T3)*JAC(1,3)+
     -                         DN2DVP(T1,T2,T3)*JAC(2,3)+
     -                         DN2DWP(T1,T2,T3)*JAC(3,3))+
     -           VWMAP(IMAP,3,ISW)*(DN3DUP(T1,T2,T3)*JAC(1,3)+
     -                         DN3DVP(T1,T2,T3)*JAC(2,3)+
     -                         DN3DWP(T1,T2,T3)*JAC(3,3))+
     -           VWMAP(IMAP,5,ISW)*(DN4DUP(T1,T2,T3)*JAC(1,3)+
     -                         DN4DVP(T1,T2,T3)*JAC(2,3)+
     -                         DN4DWP(T1,T2,T3)*JAC(3,3))+
     -           VWMAP(IMAP,6,ISW)*(DN5DUP(T1,T2,T3)*JAC(1,3)+
     -                         DN5DVP(T1,T2,T3)*JAC(2,3)+
     -                         DN5DWP(T1,T2,T3)*JAC(3,3))+
     -           VWMAP(IMAP,7,ISW)*(DN6DUP(T1,T2,T3)*JAC(1,3)+
     -                         DN6DVP(T1,T2,T3)*JAC(2,3)+
     -                         DN6DWP(T1,T2,T3)*JAC(3,3)))/DET
*   Other elements.
       ELSE
            PRINT *,' !!!!!! IONFMP WARNING : Unknown element, type = ',
     -           MAPTYP,', order = ',MAPORD,', LSFDER = ',LSFDER
            RETURN
       ENDIF
*** Apply mirror imaging.
       IF(MIRRX)EX=-EX
       IF(MIRRY)EY=-EY
       IF(MIRRZ)EZ=-EZ
*** Rotate the field.
       IF(PERAX)THEN
            CALL CFMCTP(EY,EZ,XAUX,YAUX,1)
            YAUX=YAUX+AROT*180/PI
            CALL CFMPTC(XAUX,YAUX,EY,EZ,1)
       ENDIF
       IF(PERAY)THEN
            CALL CFMCTP(EZ,EX,XAUX,YAUX,1)
            YAUX=YAUX+AROT*180/PI
            CALL CFMPTC(XAUX,YAUX,EZ,EX,1)
       ENDIF
       IF(PERAZ)THEN
            CALL CFMCTP(EX,EY,XAUX,YAUX,1)
            YAUX=YAUX+AROT*180/PI
            CALL CFMPTC(XAUX,YAUX,EX,EY,1)
       ENDIF
*** And take care of symmetry.
       ER=EX
       EAXIS=EZ
       IF(PERRX)THEN
            IF(RCOOR.LE.0)THEN
                 EX=EAXIS
                 EY=0
                 EZ=0
            ELSE
                 EX=EAXIS
                 EY=ER*YIN/RCOOR
                 EZ=ER*ZIN/RCOOR
            ENDIF
       ENDIF
       IF(PERRY)THEN
            IF(RCOOR.LE.0)THEN
                 EX=0
                 EY=EAXIS
                 EZ=0
            ELSE
                 EX=ER*XIN/RCOOR
                 EY=EAXIS
                 EZ=ER*ZIN/RCOOR
            ENDIF
       ENDIF
       IF(PERRZ)THEN
            IF(RCOOR.LE.0)THEN
                 EX=0
                 EY=0
                 EZ=EAXIS
            ELSE
                 EX=ER*XIN/RCOOR
                 EY=ER*YIN/RCOOR
                 EZ=EAXIS
            ENDIF
       ENDIF
       END
