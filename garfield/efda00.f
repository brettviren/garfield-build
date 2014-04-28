CDECK  ID>, EFDA00.
       SUBROUTINE EFDA00(XPOS,YPOS,EX,EY,VOLT,IOPT)
*-----------------------------------------------------------------------
*   EFDA00 - Subroutine performing the actual field calculations in case
*            the charges have been prepared by EFQA00.
*   VARIABLES : R2         : Potential before taking -log(sqrt(...))
*               EX, EY     : x,y-component of the electric field.
*               ETOT       : Magnitude of electric field.
*               VOLT       : Potential.
*               EXHELP etc : One term in the series to be summed.
*               (XPOS,YPOS): The position where the field is calculated.
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
       COMMON /TMPA00/ EPSMT1,EPSMT2
*** Initialise the potential and the electric field.
       EX=0.0
       EY=0.0
       VOLT=V0
*** Loop over all wires.
       DO 10 I=1,NWIRE
*** Calculate the field in case there are no planes.
       R2=(XPOS-X(I))**2+(YPOS-Y(I))**2
       EXHELP=(XPOS-X(I))/R2
       EYHELP=(YPOS-Y(I))/R2
*** Take care of a plane at constant x.
       IF(YNPLAX)THEN
            XXMIRR=X(I)+(XPOS-2.0*COPLAX)
            R2PLAN=XXMIRR**2+(YPOS-Y(I))**2
            EXHELP=EXHELP-XXMIRR/R2PLAN
            EYHELP=EYHELP-(YPOS-Y(I))/R2PLAN
            R2=R2/R2PLAN
       ENDIF
*** Take care of a plane at constant y.
       IF(YNPLAY)THEN
            YYMIRR=Y(I)+(YPOS-2.0*COPLAY)
            R2PLAN=(XPOS-X(I))**2+YYMIRR**2
            EXHELP=EXHELP-(XPOS-X(I))/R2PLAN
            EYHELP=EYHELP-YYMIRR/R2PLAN
            R2=R2/R2PLAN
       ENDIF
*** Take care of pairs of planes.
       IF(YNPLAX.AND.YNPLAY)THEN
            R2PLAN=XXMIRR**2+YYMIRR**2
            EXHELP=EXHELP+XXMIRR/R2PLAN
            EYHELP=EYHELP+YYMIRR/R2PLAN
            R2=R2*R2PLAN
       ENDIF
*** Calculate the electric field and the potential.
       IF((YNMATX.AND.((XPOS.LT.COMATX.AND.XMATT(1,3).NE.0).OR.
     -      (XPOS.GT.COMATX.AND.XMATT(1,4).NE.0))).OR.
     -      (YNMATY.AND.((YPOS.LT.COMATY.AND.YMATT(1,3).NE.0).OR.
     -      (YPOS.GT.COMATY.AND.YMATT(1,4).NE.0))))THEN
            IF(IOPT.NE.0)VOLT=VOLT-0.5*E(I)*EPSMT2*LOG(R2)
            EX=EX+E(I)*EPSMT2*EXHELP
            EY=EY+E(I)*EPSMT2*EYHELP
       ELSE
            IF(IOPT.NE.0)VOLT=VOLT-0.5*E(I)*LOG(R2)
            EX=EX+E(I)*EXHELP
            EY=EY+E(I)*EYHELP
       ENDIF
*** Dielectric mediums, no planes.
       IF(YNMATX.AND.((XPOS.GT.COMATX.AND.XMATT(1,3).NE.0).OR.
     -      (XPOS.LT.COMATX.AND.XMATT(1,4).NE.0)))THEN
            IF(IOPT.NE.0)VOLT=VOLT-E(I)*EPSMT1*0.5*
     -           LOG((XPOS+X(I)-2*COMATX)**2+(YPOS-Y(I))**2)
            EX=EX-E(I)*EPSMT1*0.5*(XPOS+X(I)-2*COMATX)/
     -           SQRT((XPOS+X(I)-2*COMATX)**2+(YPOS-Y(I))**2)
            EY=EY-E(I)*EPSMT1*0.5*(YPOS-Y(I))/
     -           SQRT((XPOS+X(I)-2*COMATX)**2+(YPOS-Y(I))**2)
       ENDIF
       IF(YNMATY.AND.((YPOS.GT.COMATY.AND.YMATT(1,3).NE.0).OR.
     -      (YPOS.LT.COMATY.AND.YMATT(1,4).NE.0)))THEN
            IF(IOPT.NE.0)VOLT=VOLT-E(I)*EPSMT1*0.5*
     -           LOG((YPOS+Y(I)-2*COMATY)**2+(XPOS-X(I))**2)
            EX=EX-E(I)*EPSMT1*0.5*(YPOS+Y(I)-2*COMATY)/
     -           SQRT((YPOS+Y(I)-2*COMATY)**2+(XPOS-X(I))**2)
            EY=EY-E(I)*EPSMT1*0.5*(XPOS-X(I))/
     -           SQRT((YPOS+Y(I)-2*COMATY)**2+(XPOS-X(I))**2)
       ENDIF
*** Finish the loop over the wires.
10     CONTINUE
       END
