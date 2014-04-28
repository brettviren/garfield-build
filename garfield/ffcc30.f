CDECK  ID>, FFCC30.
       SUBROUTINE FFCC30(XPOS,YPOS,EX,EY)
*-----------------------------------------------------------------------
*   FFCC30 - Routine returning the potential and electric field in a
*            configuration with 2 y and 2 x planes.
*   VARIABLES : see the writeup
*   (Last changed on 27/ 1/96.)
*-----------------------------------------------------------------------
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
       COMPLEX WSUM1,WSUM2,WSUM3,WSUM4,ZSIN,ZCOF,ZU,ZUNEW,
     -      ZTERM1,ZTERM2,ZETA
*** Initial values.
       WSUM1=0
       WSUM2=0
       WSUM3=0
       WSUM4=0
*** Wire loop.
       DO 10 I=1,NWIRE
*   Compute the direct contribution.
       IF(CNALSO(I))THEN
            ZETA=ZMULT*CMPLX(XPOS-X(I),YPOS-Y(I))
            IF(AIMAG(ZETA).GT.+15)THEN
                 WSUM1=WSUM1-E(I)*ICONS
            ELSEIF(AIMAG(ZETA).LT.-15)THEN
                 WSUM1=WSUM1+E(I)*ICONS
            ELSE
                 ZSIN=SIN(ZETA)
                 ZCOF=4*ZSIN**2-2
                 ZU=-P1-ZCOF*P2
                 ZUNEW=1-ZCOF*ZU-P2
                 ZTERM1=(ZUNEW+ZU)*ZSIN
                 ZU=-3*P1-ZCOF*5*P2
                 ZUNEW=1-ZCOF*ZU-5*P2
                 ZTERM2=(ZUNEW-ZU)*COS(ZETA)
                 WSUM1=WSUM1+E(I)*(ZTERM2/ZTERM1)
            ENDIF
       ENDIF
*   Find the plane nearest to the wire.
       CX=COPLAX-SX*ANINT((COPLAX-X(I))/SX)
*   Mirror contribution from the x plane.
       ZETA=ZMULT*CMPLX(2*CX-XPOS-X(I),YPOS-Y(I))
       IF(AIMAG(ZETA).GT.+15)THEN
            WSUM2=WSUM2-E(I)*ICONS
       ELSEIF(AIMAG(ZETA).LT.-15)THEN
            WSUM2=WSUM2+E(I)*ICONS
       ELSE
            ZSIN=SIN(ZETA)
            ZCOF=4*ZSIN**2-2
            ZU=-P1-ZCOF*P2
            ZUNEW=1-ZCOF*ZU-P2
            ZTERM1=(ZUNEW+ZU)*ZSIN
            ZU=-3*P1-ZCOF*5*P2
            ZUNEW=1-ZCOF*ZU-5*P2
            ZTERM2=(ZUNEW-ZU)*COS(ZETA)
            WSUM2=WSUM2+E(I)*(ZTERM2/ZTERM1)
       ENDIF
*   Find the plane nearest to the wire.
       CY=COPLAY-SY*ANINT((COPLAY-Y(I))/SY)
*   Mirror contribution from the y plane.
       ZETA=ZMULT*CMPLX(XPOS-X(I),2*CY-YPOS-Y(I))
       IF(AIMAG(ZETA).GT.+15)THEN
            WSUM3=WSUM3-E(I)*ICONS
       ELSEIF(AIMAG(ZETA).LT.-15)THEN
            WSUM3=WSUM3+E(I)*ICONS
       ELSE
            ZSIN=SIN(ZETA)
            ZCOF=4*ZSIN**2-2
            ZU=-P1-ZCOF*P2
            ZUNEW=1-ZCOF*ZU-P2
            ZTERM1=(ZUNEW+ZU)*ZSIN
            ZU=-3*P1-ZCOF*5*P2
            ZUNEW=1-ZCOF*ZU-5*P2
            ZTERM2=(ZUNEW-ZU)*COS(ZETA)
            WSUM3=WSUM3+E(I)*(ZTERM2/ZTERM1)
       ENDIF
*   Mirror contribution from both the x and the y plane.
       ZETA=ZMULT*CMPLX(2*CX-XPOS-X(I),2*CY-YPOS-Y(I))
       IF(AIMAG(ZETA).GT.+15)THEN
            WSUM4=WSUM4-E(I)*ICONS
       ELSEIF(AIMAG(ZETA).LT.-15)THEN
            WSUM4=WSUM4+E(I)*ICONS
       ELSE
            ZSIN=SIN(ZETA)
            ZCOF=4*ZSIN**2-2
            ZU=-P1-ZCOF*P2
            ZUNEW=1-ZCOF*ZU-P2
            ZTERM1=(ZUNEW+ZU)*ZSIN
            ZU=-3*P1-ZCOF*5*P2
            ZUNEW=1-ZCOF*ZU-5*P2
            ZTERM2=(ZUNEW-ZU)*COS(ZETA)
            WSUM4=WSUM4+E(I)*(ZTERM2/ZTERM1)
       ENDIF
10     CONTINUE
*** Convert the two contributions to a real field.
       EX=+REAL(ZMULT*(WSUM1+WSUM2-WSUM3-WSUM4))
       EY=-AIMAG(ZMULT*(WSUM1-WSUM2+WSUM3-WSUM4))
       END
