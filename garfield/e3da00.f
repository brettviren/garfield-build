CDECK  ID>, E3DA00.
       SUBROUTINE E3DA00(XPOS,YPOS,ZPOS,EX,EY,EZ,VOLT)
*-----------------------------------------------------------------------
*   E3DA00 - Subroutine adding 3-dimensional charges for A cells.
*            The potential used is 1/2*pi*eps0  1/r
*   VARIABLES : EX, EY     : x,y-component of the electric field.
*               ETOT       : Magnitude of electric field.
*               VOLT       : Potential.
*               EXHELP etc : One term in the series to be summed.
*               (XPOS,YPOS): The position where the field is calculated.
*   (Last changed on  5/12/94.)
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
*** Initialise the potential and the electric field.
       EX=0.0
       EY=0.0
       EZ=0.0
       VOLT=0.0
*** Loop over all charges.
       DO 10 I=1,N3D
*** Calculate the field in case there are no planes.
       R=SQRT((XPOS-X3D(I))**2+(YPOS-Y3D(I))**2+(ZPOS-Z3D(I))**2)
       IF(R.EQ.0)GOTO 10
       EXHELP=-(XPOS-X3D(I))/R**3
       EYHELP=-(YPOS-Y3D(I))/R**3
       EZHELP=-(ZPOS-Z3D(I))/R**3
       VHELP =1/R
*** Take care of a plane at constant x.
       IF(YNPLAX)THEN
            XXMIRR=X3D(I)+(XPOS-2*COPLAX)
            RPLAN=SQRT(XXMIRR**2+(YPOS-Y3D(I))**2)
            IF(RPLAN.EQ.0)GOTO 10
            EXHELP=EXHELP+XXMIRR/RPLAN**3
            EYHELP=EYHELP+(YPOS-Y3D(I))/RPLAN**3
            EZHELP=EZHELP+(ZPOS-Z3D(I))/RPLAN**3
            VHELP =VHELP-1/RPLAN
       ENDIF
*** Take care of a plane at constant y.
       IF(YNPLAY)THEN
            YYMIRR=Y3D(I)+(YPOS-2*COPLAY)
            RPLAN=SQRT((XPOS-X3D(I))**2+YYMIRR**2)
            IF(RPLAN.EQ.0)GOTO 10
            EXHELP=EXHELP+(XPOS-X3D(I))/RPLAN**3
            EYHELP=EYHELP+YYMIRR/RPLAN**3
            EZHELP=EZHELP+(ZPOS-Z3D(I))/RPLAN**3
            VHELP =VHELP-1/RPLAN
       ENDIF
*** Take care of pairs of planes.
       IF(YNPLAX.AND.YNPLAY)THEN
            RPLAN=SQRT(XXMIRR**2+YYMIRR**2)
            IF(RPLAN.EQ.0)GOTO 10
            EXHELP=EXHELP-XXMIRR/RPLAN**3
            EYHELP=EYHELP-YYMIRR/RPLAN**3
            EZHELP=EZHELP-(ZPOS-Z3D(I))/RPLAN**3
            VHELP =VHELP+1/RPLAN
       ENDIF
*** Add the terms to the electric field and the potential.
       EX=EX-E3D(I)*EXHELP
       EY=EY-E3D(I)*EYHELP
       EZ=EZ-E3D(I)*EZHELP
       VOLT=VOLT+E3D(I)*VHELP
*** Finish the loop over the charges.
10     CONTINUE
       END
