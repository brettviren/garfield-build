CDECK  ID>, MAGFMP.
       SUBROUTINE MAGFMP(XIN,YIN,ZIN,BX,BY,BZ,ILOC)
*-----------------------------------------------------------------------
*   MAGFMP - Interpolates the B field map at (XPOS,YPOS,ZPOS).
*   (Last changed on 26/10/07.)
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
       REAL XIN,YIN,ZIN,XPOS,YPOS,ZPOS,BX,BY,BZ,XNEW,YNEW,ZNEW,
     -      T1,T2,T3,T4,AUXPHI,AUXR,AROT,XAUX,YAUX,BR,BAXIS,RCOOR,ZCOOR
       DOUBLE PRECISION JAC(4,4),DET
       INTEGER ILOC,IMAP,NX,NY,NZ
       LOGICAL MIRRX,MIRRY,MIRRZ
*** Initial values.
       BX=0
       BY=0
       BZ=0
       ILOC=0
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
       IF(IMAP.LE.0.OR.IMAP.GT.NMAP)THEN
            ILOC=-6
            RETURN
       ENDIF
*** A 2-dimensional interpolation (triangles), linear ...
       IF((MAPTYP.EQ.1.OR.MAPTYP.EQ.2.OR.MAPTYP.EQ.3).AND.
     -      MAPORD.EQ.1)THEN
                 IF(MAPFLG(6))BX=
     -                BXMAP(IMAP,1)*T1+BXMAP(IMAP,2)*T2+BXMAP(IMAP,3)*T3
                 IF(MAPFLG(7))BY=
     -                BYMAP(IMAP,1)*T1+BYMAP(IMAP,2)*T2+BYMAP(IMAP,3)*T3
                 IF(MAPFLG(8))BZ=
     -                BZMAP(IMAP,1)*T1+BZMAP(IMAP,2)*T2+BZMAP(IMAP,3)*T3
**  or quadratic.
       ELSEIF(MAPTYP.EQ.2.AND.MAPORD.EQ.2)THEN
*   By interpolation of field maps.
                 IF(MAPFLG(6))BX=
     -                BXMAP(IMAP,1)*T1*(2*T1-1)+
     -                BXMAP(IMAP,2)*T2*(2*T2-1)+
     -                BXMAP(IMAP,3)*T3*(2*T3-1)+
     -                4*BXMAP(IMAP,4)*T1*T2+
     -                4*BXMAP(IMAP,5)*T1*T3+
     -                4*BXMAP(IMAP,6)*T2*T3
                 IF(MAPFLG(7))BY=
     -                BYMAP(IMAP,1)*T1*(2*T1-1)+
     -                BYMAP(IMAP,2)*T2*(2*T2-1)+
     -                BYMAP(IMAP,3)*T3*(2*T3-1)+
     -                4*BYMAP(IMAP,4)*T1*T2+
     -                4*BYMAP(IMAP,5)*T1*T3+
     -                4*BYMAP(IMAP,6)*T2*T3
                 IF(MAPFLG(8))BZ=
     -                BZMAP(IMAP,1)*T1*(2*T1-1)+
     -                BZMAP(IMAP,2)*T2*(2*T2-1)+
     -                BZMAP(IMAP,3)*T3*(2*T3-1)+
     -                4*BZMAP(IMAP,4)*T1*T2+
     -                4*BZMAP(IMAP,5)*T1*T3+
     -                4*BZMAP(IMAP,6)*T2*T3
*** A 3-dimensional interpolation (tetrahedra), linear ...
       ELSEIF((MAPTYP.EQ.11.OR.MAPTYP.EQ.12.OR.MAPTYP.EQ.13).AND.
     -      MAPORD.EQ.1)THEN
                 IF(MAPFLG(6))BX=BXMAP(IMAP,1)*T1+BXMAP(IMAP,2)*T2+
     -                BXMAP(IMAP,3)*T3+BXMAP(IMAP,4)*T4
                 IF(MAPFLG(7))BY=BYMAP(IMAP,1)*T1+BYMAP(IMAP,2)*T2+
     -                BYMAP(IMAP,3)*T3+BYMAP(IMAP,4)*T4
                 IF(MAPFLG(8))BZ=BZMAP(IMAP,1)*T1+BZMAP(IMAP,2)*T2+
     -                BZMAP(IMAP,3)*T3+BZMAP(IMAP,4)*T4
**  or quadratic.
       ELSEIF(MAPTYP.EQ.12.AND.MAPORD.EQ.2)THEN
                 IF(MAPFLG(6))BX=
     -                BXMAP(IMAP,1)*T1*(2*T1-1)+
     -                BXMAP(IMAP,2)*T2*(2*T2-1)+
     -                BXMAP(IMAP,3)*T3*(2*T3-1)+
     -                BXMAP(IMAP,4)*T4*(2*T4-1)+
     -                4*BXMAP(IMAP,5)*T1*T2+
     -                4*BXMAP(IMAP,6)*T1*T3+
     -                4*BXMAP(IMAP,7)*T1*T4+
     -                4*BXMAP(IMAP,8)*T2*T3+
     -                4*BXMAP(IMAP,9)*T2*T4+
     -                4*BXMAP(IMAP,10)*T3*T4
                 IF(MAPFLG(7))BY=
     -                BYMAP(IMAP,1)*T1*(2*T1-1)+
     -                BYMAP(IMAP,2)*T2*(2*T2-1)+
     -                BYMAP(IMAP,3)*T3*(2*T3-1)+
     -                BYMAP(IMAP,4)*T4*(2*T4-1)+
     -                4*BYMAP(IMAP,5)*T1*T2+
     -                4*BYMAP(IMAP,6)*T1*T3+
     -                4*BYMAP(IMAP,7)*T1*T4+
     -                4*BYMAP(IMAP,8)*T2*T3+
     -                4*BYMAP(IMAP,9)*T2*T4+
     -                4*BYMAP(IMAP,10)*T3*T4
                 IF(MAPFLG(8))BZ=
     -                BZMAP(IMAP,1)*T1*(2*T1-1)+
     -                BZMAP(IMAP,2)*T2*(2*T2-1)+
     -                BZMAP(IMAP,3)*T3*(2*T3-1)+
     -                BZMAP(IMAP,4)*T4*(2*T4-1)+
     -                4*BZMAP(IMAP,5)*T1*T2+
     -                4*BZMAP(IMAP,6)*T1*T3+
     -                4*BZMAP(IMAP,7)*T1*T4+
     -                4*BZMAP(IMAP,8)*T2*T3+
     -                4*BZMAP(IMAP,9)*T2*T4+
     -                4*BZMAP(IMAP,10)*T3*T4
*** Or an interpolation on a non-degerate hexahedron, interpolate.
       ELSEIF(MAPTYP.EQ.14.AND..NOT.ELMDGN(IMAP))THEN
            IF(MAPFLG(6))BX=
     -           BXMAP(IMAP,1)*(1-T1)*(1-T2)*(1-T3)/8+
     -           BXMAP(IMAP,2)*(1+T1)*(1-T2)*(1-T3)/8+
     -           BXMAP(IMAP,3)*(1+T1)*(1+T2)*(1-T3)/8+
     -           BXMAP(IMAP,4)*(1-T1)*(1+T2)*(1-T3)/8+
     -           BXMAP(IMAP,5)*(1-T1)*(1-T2)*(1+T3)/8+
     -           BXMAP(IMAP,6)*(1+T1)*(1-T2)*(1+T3)/8+
     -           BXMAP(IMAP,7)*(1+T1)*(1+T2)*(1+T3)/8+
     -           BXMAP(IMAP,8)*(1-T1)*(1+T2)*(1+T3)/8
            IF(MAPFLG(7))BY=
     -           BYMAP(IMAP,1)*(1-T1)*(1-T2)*(1-T3)/8+
     -           BYMAP(IMAP,2)*(1+T1)*(1-T2)*(1-T3)/8+
     -           BYMAP(IMAP,3)*(1+T1)*(1+T2)*(1-T3)/8+
     -           BYMAP(IMAP,4)*(1-T1)*(1+T2)*(1-T3)/8+
     -           BYMAP(IMAP,5)*(1-T1)*(1-T2)*(1+T3)/8+
     -           BYMAP(IMAP,6)*(1+T1)*(1-T2)*(1+T3)/8+
     -           BYMAP(IMAP,7)*(1+T1)*(1+T2)*(1+T3)/8+
     -           BYMAP(IMAP,8)*(1-T1)*(1+T2)*(1+T3)/8
            IF(MAPFLG(8))BZ=
     -           BZMAP(IMAP,1)*(1-T1)*(1-T2)*(1-T3)/8+
     -           BZMAP(IMAP,2)*(1+T1)*(1-T2)*(1-T3)/8+
     -           BZMAP(IMAP,3)*(1+T1)*(1+T2)*(1-T3)/8+
     -           BZMAP(IMAP,4)*(1-T1)*(1+T2)*(1-T3)/8+
     -           BZMAP(IMAP,5)*(1-T1)*(1-T2)*(1+T3)/8+
     -           BZMAP(IMAP,6)*(1+T1)*(1-T2)*(1+T3)/8+
     -           BZMAP(IMAP,7)*(1+T1)*(1+T2)*(1+T3)/8+
     -           BZMAP(IMAP,8)*(1-T1)*(1+T2)*(1+T3)/8
**  or an interpolation on a degerate hexahedron, interpolate
       ELSEIF(MAPTYP.EQ.14.AND.ELMDGN(IMAP))THEN
            IF(MAPFLG(6))BX=
     -           BXMAP(IMAP,1)*(1-T1-T2)*(1-T3)/2+
     -           BXMAP(IMAP,2)*      T1 *(1-T3)/2+
     -           BXMAP(IMAP,3)*      T2 *(1-T3)/2+
     -           BXMAP(IMAP,5)*(1-T1-T2)*(1+T3)/2+
     -           BXMAP(IMAP,6)*      T1 *(1+T3)/2+
     -           BXMAP(IMAP,7)*      T2 *(1+T3)/2
            IF(MAPFLG(7))BY=
     -           BYMAP(IMAP,1)*(1-T1-T2)*(1-T3)/2+
     -           BYMAP(IMAP,2)*      T1 *(1-T3)/2+
     -           BYMAP(IMAP,3)*      T2 *(1-T3)/2+
     -           BYMAP(IMAP,5)*(1-T1-T2)*(1+T3)/2+
     -           BYMAP(IMAP,6)*      T1 *(1+T3)/2+
     -           BYMAP(IMAP,7)*      T2 *(1+T3)/2
            IF(MAPFLG(8))BZ=
     -           BZMAP(IMAP,1)*(1-T1-T2)*(1-T3)/2+
     -           BZMAP(IMAP,2)*      T1 *(1-T3)/2+
     -           BZMAP(IMAP,3)*      T2 *(1-T3)/2+
     -           BZMAP(IMAP,5)*(1-T1-T2)*(1+T3)/2+
     -           BZMAP(IMAP,6)*      T1 *(1+T3)/2+
     -           BZMAP(IMAP,7)*      T2 *(1+T3)/2
*** Or an unknown case.
       ELSE
            ILOC=-10
            RETURN
       ENDIF
*** Apply mirror imaging.
       IF(MIRRX)BX=-BX
       IF(MIRRY)BY=-BY
       IF(MIRRZ)BZ=-BZ
*** Rotate the field.
       IF(PERAX)THEN
            CALL CFMCTP(BY,BZ,XAUX,YAUX,1)
            YAUX=YAUX+AROT*180/PI
            CALL CFMPTC(XAUX,YAUX,BY,BZ,1)
       ENDIF
       IF(PERAY)THEN
            CALL CFMCTP(BZ,BX,XAUX,YAUX,1)
            YAUX=YAUX+AROT*180/PI
            CALL CFMPTC(XAUX,YAUX,BZ,BX,1)
       ENDIF
       IF(PERAZ)THEN
            CALL CFMCTP(BX,BY,XAUX,YAUX,1)
            YAUX=YAUX+AROT*180/PI
            CALL CFMPTC(XAUX,YAUX,BX,BY,1)
       ENDIF
*** And take care of symmetry, x is radial component.
       BR=BX
       BAXIS=BZ
       IF(PERRX)THEN
            IF(RCOOR.LE.0)THEN
                 BX=BAXIS
                 BY=0
                 BZ=0
            ELSE
                 BX=BAXIS
                 BY=BR*YIN/RCOOR
                 BZ=BR*ZIN/RCOOR
            ENDIF
       ENDIF
       IF(PERRY)THEN
            IF(RCOOR.LE.0)THEN
                 BX=0
                 BY=BAXIS
                 BZ=0
            ELSE
                 BX=BR*XIN/RCOOR
                 BY=BAXIS
                 BZ=BR*ZIN/RCOOR
            ENDIF
       ENDIF
       IF(PERRZ)THEN
            IF(RCOOR.LE.0)THEN
                 BX=0
                 BY=0
                 BZ=BAXIS
            ELSE
                 BX=BR*XIN/RCOOR
                 BY=BR*YIN/RCOOR
                 BZ=BAXIS
            ENDIF
       ENDIF
*** And store material index.
       IF(MATMAP(IMAP).EQ.IDRMAT.OR..NOT.MAPFLG(9))THEN
            ILOC=0
       ELSE
            ILOC=-5
       ENDIF
       END
