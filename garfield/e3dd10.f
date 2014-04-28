CDECK  ID>, E3DD10.
       SUBROUTINE E3DD10(XXPOS,YYPOS,ZZPOS,EEX,EEY,EEZ,VOLT)
*-----------------------------------------------------------------------
*   E3DD10 - Subroutine adding 3-dimensional charges to tubes with one
*            wire running down the centre.
*            The series expansions for the modified Bessel functions
*            have been taken from Abramowitz and Stegun.
*   VARIABLES : See routine E3DA00 for most of the variables.
*   (Last changed on 25/11/95.)
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
       DOUBLE PRECISION EXSUM,EYSUM,EZSUM,VSUM,
     -      I0S,I1S,K0S,K0L,K1S,K1L,K0R,K1R,
     -      XX,RR,ZZP,ZZN,RR1,RR2,RM1,RM2,ERR,EZZ
       REAL XPOS,YPOS,ZPOS,EX,EY,EZ,VOLT,RCUT
       PARAMETER(RCUT=1.0)
*** Statement functions for the modified Bessel functions:
       I0S(XX)=1
     -      +3.5156229*(XX/3.75)**2
     -      +3.0899424*(XX/3.75)**4
     -      +1.2067492*(XX/3.75)**6
     -      +0.2659732*(XX/3.75)**8
     -      +0.0360768*(XX/3.75)**10
     -      +0.0045813*(XX/3.75)**12
       I1S(XX)=XX*(
     -      +0.5
     -      +0.87890594*(XX/3.75)**2
     -      +0.51498869*(XX/3.75)**4
     -      +0.15084934*(XX/3.75)**6
     -      +0.02658733*(XX/3.75)**8
     -      +0.00301532*(XX/3.75)**10
     -      +0.00032411*(XX/3.75)**12)
       K0S(XX)=-LOG(XX/2)*I0S(XX)
     -      -0.57721566
     -      +0.42278420*(XX/2)**2
     -      +0.23069756*(XX/2)**4
     -      +0.03488590*(XX/2)**6
     -      +0.00262698*(XX/2)**8
     -      +0.00010750*(XX/2)**10
     -      +0.00000740*(XX/2)**12
       K0L(XX)=(EXP(-XX)/SQRT(XX))*(
     -      +1.25331414
     -      -0.07832358*(2/XX)
     -      +0.02189568*(2/XX)**2
     -      -0.01062446*(2/XX)**3
     -      +0.00587872*(2/XX)**4
     -      -0.00251540*(2/XX)**5
     -      +0.00053208*(2/XX)**6)
       K1S(XX)=LOG(XX/2)*I1S(XX)+(1/XX)*(
     -      +1
     -      +0.15443144*(XX/2)**2
     -      -0.67278579*(XX/2)**4
     -      -0.18156897*(XX/2)**6
     -      -0.01919402*(XX/2)**8
     -      -0.00110404*(XX/2)**10
     -      -0.00004686*(XX/2)**12)
       K1L(XX)=(EXP(-XX)/SQRT(XX))*(
     -      +1.25331414
     -      +0.23498619*(2/XX)
     -      -0.03655620*(2/XX)**2
     -      +0.01504268*(2/XX)**3
     -      -0.00780353*(2/XX)**4
     -      +0.00325614*(2/XX)**5
     -      -0.00068245*(2/XX)**6)
*** Initialise the sums for the field components.
       EX=0.0
       EEX=0.0
       EY=0.0
       EEY=0.0
       EZ=0.0
       EEZ=0.0
       VOLT=0.0
*** Ensure that the routine can actually work.
       IF(NWIRE.LT.1)THEN
            PRINT *,' Inappropriate potential function.'
            RETURN
       ENDIF
*** Define a periodicity and one plane in the mapped frame.
       SSX=LOG(2*COTUBE/D(1))
       CPL=LOG(D(1)/2)
*** Transform the coordinates to the mapped frame.
       XPOS=0.5*LOG(XXPOS**2+YYPOS**2)
       YPOS=ATAN2(YYPOS,XXPOS)
       ZPOS=ZZPOS
*** Loop over all point charges.
       DO 10 I=1,N3D
       DO 40 II=-1,1
       XX3D=0.5*LOG(X3D(I)**2+Y3D(I)**2)
       YY3D=ATAN2(Y3D(I),X3D(I))+II*2*PI
       ZZ3D=Z3D(I)
*   Skip wires that are on the charge.
       IF(XPOS.EQ.XX3D.AND.YPOS.EQ.YY3D.AND.ZPOS.EQ.ZZ3D)GOTO 40
*** In the far away zone, sum the modified Bessel function series.
       IF((YPOS-YY3D)**2+(ZPOS-ZZ3D)**2.GT.(RCUT*2*SSX)**2)THEN
*   Initialise the per-wire sum.
            EXSUM=0.0
            EYSUM=0.0
            EZSUM=0.0
            VSUM=0.0
*   Loop over the terms in the series.
            DO 20 J=1,NTERMB
*   Obtain reduced coordinates.
            RR=PI*J*SQRT((YPOS-YY3D)**2+(ZPOS-ZZ3D)**2)/SSX
            ZZP=PI*J*(XPOS-XX3D)/SSX
            ZZN=PI*J*(XPOS+XX3D-2*CPL)/SSX
*   Evaluate the Bessel functions for this R.
            IF(RR.LT.2)THEN
                 K0R=K0S(RR)
                 K1R=K1S(RR)
            ELSE
                 K0R=K0L(RR)
                 K1R=K1L(RR)
            ENDIF
*   Get the field components.
            VSUM=VSUM+(1/SSX)*K0R*(COS(ZZP)-COS(ZZN))
            ERR=(2*J*PI/SSX**2)*K1R*(COS(ZZP)-COS(ZZN))
            EZZ=(2*J*PI/SSX**2)*K0R*(SIN(ZZP)-SIN(ZZN))
            EXSUM=EXSUM+EZZ
            EYSUM=EYSUM+ERR*(YPOS-YY3D)/
     -           SQRT((YPOS-YY3D)**2+(ZPOS-ZZ3D)**2)
            EZSUM=EZSUM+ERR*(ZPOS-ZZ3D)/
     -           SQRT((YPOS-YY3D)**2+(ZPOS-ZZ3D)**2)
20          CONTINUE
*** Direct polynomial summing, obtain reduced coordinates.
       ELSE
*   Loop over the terms.
            DO 30 J=0,NTERMP
*   Simplify the references to the distances.
            RR1=SQRT((XPOS-XX3D+J*2*SSX)**2+(YPOS-YY3D)**2+
     -           (ZPOS-ZZ3D)**2)
            RR2=SQRT((XPOS-XX3D-J*2*SSX)**2+(YPOS-YY3D)**2+
     -           (ZPOS-ZZ3D)**2)
            RM1=SQRT((XPOS+XX3D-J*2*SSX-2*CPL)**2+
     -           (YPOS-YY3D)**2+(ZPOS-ZZ3D)**2)
            RM2=SQRT((XPOS+XX3D+J*2*SSX-2*CPL)**2+
     -           (YPOS-YY3D)**2+(ZPOS-ZZ3D)**2)
*   Initialisation of the sum: only a charge and a mirror charge.
            IF(J.EQ.0)THEN
                 VSUM=1/RR1-1/RM1
                 EXSUM=(XPOS-XX3D)/RR1**3-
     -                (XPOS+XX3D-2*CPL)/RM1**3
                 EYSUM=(YPOS-YY3D)*(1/RR1**3-1/RM1**3)
                 EZSUM=(ZPOS-ZZ3D)*(1/RR1**3-1/RM1**3)
*   Further terms in the series: 2 charges and 2 mirror charges.
            ELSE
                 VSUM=VSUM+1/RR1+1/RR2-1/RM1-1/RM2
                 EXSUM=EXSUM+
     -                (XPOS-XX3D+J*2*SSX)/RR1**3+
     -                (XPOS-XX3D-J*2*SSX)/RR2**3-
     -                (XPOS+XX3D-J*2*SSX-2*CPL)/RM1**3-
     -                (XPOS+XX3D+J*2*SSX-2*CPL)/RM2**3
                 EYSUM=EYSUM+(YPOS-YY3D)*
     -                (1/RR1**3+1/RR2**3-1/RM1**3-1/RM2**3)
                 EZSUM=EZSUM+(ZPOS-ZZ3D)*
     -                (1/RR1**3+1/RR2**3-1/RM1**3-1/RM2**3)
            ENDIF
30          CONTINUE
       ENDIF
*** Convert the double precision sum to single precision.
       EX=EX+E3D(I)*REAL(EXSUM)
       EY=EY+E3D(I)*REAL(EYSUM)
       EZ=EZ+E3D(I)*REAL(EZSUM)
       VOLT=VOLT+E3D(I)*REAL(VSUM)
*** Finish the loop over the charges.
40     CONTINUE
10     CONTINUE
*** Transform the field vectors back to Cartesian coordinates.
       EEX=EXP(-XPOS)*(+EX*COS(YPOS)-EY*SIN(YPOS))
       EEY=EXP(-XPOS)*(+EX*SIN(YPOS)+EY*COS(YPOS))
       EEZ=EZ
       END
