CDECK  ID>, CELINT.
       SUBROUTINE CELINT
*-----------------------------------------------------------------------
*   CELINT - Initialises cell data.
*   (Last changed on 15/ 4/12.)
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
       INTEGER NBEM,IREFB1(MXPLAN),NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,
     -      BEMNEW,BEMINV,BEMSLV
       DOUBLE PRECISION BEMQTH,BEMSTH,BEMSSC,BEMTGT,BEMEPA,BEMEPD
       LOGICAL LBDUMP
       COMMON /BEMDAT/ BEMQTH,BEMSSC,BEMSTH,BEMTGT,BEMEPA,BEMEPD,
     -      IREFB1,NBEM,NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,BEMNEW,
     -      BEMINV,BEMSLV,LBDUMP
       INTEGER I,J
       REAL PHI2
*** Overall flag for cell data.
       CELSET   =.FALSE.
*** Coordinate system.
       POLAR    =.FALSE.
       TUBE     =.FALSE.
*** Cell type.
       TYPE     ='A  '
       ICTYPE   =1
*** neBEM structures
       BEMSET   =.FALSE.
*** Identifier.
       CELLID   =' '
*** Wires.
       NWIRE    =0
       NSW      =0
       KAPPA    =0.0
       DO 40 I=1,MXWIRE
*   Location
       X(I)     =0.0
       Y(I)     =0.0
*   Diameter
       D(I)     =0.0
*   Charge
       E(I)     =0.0
*   Potential
       V(I)     =0.0
*   Stretching weight
       W(I)     =50.0
*   Length
       U(I)     =100.0
*   Density
       DENS(I)  =19.3
*   Dipole angle and amplitude
       PHI2     =0.0
       COSPH2(I)=COS(PHI2)
       SINPH2(I)=SIN(PHI2)
       AMP2(I)  =0.0
*   Terms for B2 potentials
       B2SIN(I) =0.0
*   Conformal mapping in polygons
       WMAP(I)  =CMPLX(0.0,0.0)
*   Wire type
       WIRTYP(I)='?'
*   Mirror charges for force calculations
       CNALSO(I)=.TRUE.
40     CONTINUE
*** Dipole terms
       LDIPOL   =.FALSE.
*** 3D charges.
       N3D      =0
       DO 90 I=1,MX3D
       X3D(I)   =0.0
       Y3D(I)   =0.0
       Z3D(I)   =0.0
       E3D(I)   =0.0
90     CONTINUE
       NTERMB   =10
       NTERMP   =100
*** Planes and tube.
       DO 20 I=1,5
*   Coordinates, voltage and existence.
       IF(I.LE.4)THEN
            YNPLAN(I)=.FALSE.
            COPLAN(I)=0.0
            VTPLAN(I)=0.0
       ENDIF
*   Labels and references.
       PLATYP(I)='?'
       INDPLA(I)=0
*   Strips.
       NPSTR1(I)=0
       NPSTR2(I)=0
       DO 100 J=1,MXPSTR
       PLSTR1(I,J,1)=0
       PLSTR1(I,J,2)=0
       PLSTR1(I,J,3)=0
       PLSTR2(I,J,1)=0
       PLSTR2(I,J,2)=0
       PLSTR2(I,J,3)=0
       PSLAB1(I,J)='?'
       PSLAB2(I,J)='?'
       INDST1(I,J)=0
       INDST2(I,J)=0
100    CONTINUE
20     CONTINUE
*   Plane shorthand.
       YNPLAX   =.FALSE.
       YNPLAY   =.FALSE.
       COPLAX   =1.0
       COPLAY   =1.0
*   Tube properties.
       NTUBE    =0
       MTUBE    =0
       COTUBE   =1
       VTTUBE   =0
*** Dielectrica.
       NXMATT   =0
       NYMATT   =0
*** Periodicities.
       PERX     =.FALSE.
       PERY     =.FALSE.
       PERZ     =.FALSE.
       PERMX    =.FALSE.
       PERMY    =.FALSE.
       PERMZ    =.FALSE.
       PERAX    =.FALSE.
       PERAY    =.FALSE.
       PERAZ    =.FALSE.
       PERRX    =.FALSE.
       PERRY    =.FALSE.
       PERRZ    =.FALSE.
       SX       =1.0
       SY       =1.0
       SZ       =1.0
*** Gravity.
       DOWN(1)  =0
       DOWN(2)  =0
       DOWN(3)  =1
*** Initial neBEM parameters.
       BEMSSC=0.001
       BEMQTH=150
       BEMTGT=0.0050
       NBEMMN=1
       NBEMMX=10
       NBEMPX=5
       NBEMPY=5
       NBEMPZ=5
       BEMNEW=1
       BEMINV=0
       BEMEPA=1.0E-6
       BEMEPD=1.0E-6
       LBDUMP=.FALSE.
       BEMSLV=0
       END
