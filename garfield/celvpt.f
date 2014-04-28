CDECK  ID>, CELVPT.
       SUBROUTINE CELVPT(XPOS,YPOS,ZPOS,IVOL)
*-----------------------------------------------------------------------
*   CELVPT - Returns the volume in which a point is located.
*   (Last changed on 11/10/11.)
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
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
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
       DOUBLE PRECISION XPOS,YPOS,ZPOS,X0,Y0,Z0,DX,DY,DZ
       INTEGER IVOL,I,IREF
       LOGICAL INSIDE
*** Initial volume setting (not inside a solid).
       IVOL=0
*** Loop over the solids.
       DO 10 I=1,NSOLID
**  Deal with periodicities, define the offset.
       IREF=ISTART(I)
*   Find the centre of a cylinder.
       IF(ISOLTP(I).EQ.1)THEN
            X0=CBUF(IREF+3)
            Y0=CBUF(IREF+4)
            Z0=CBUF(IREF+5)
*   Holes.
       ELSEIF(ISOLTP(I).EQ.2)THEN
            X0=CBUF(IREF+6)
            Y0=CBUF(IREF+7)
            Z0=CBUF(IREF+8)
*   Boxes.
       ELSEIF(ISOLTP(I).EQ.3)THEN
            X0=CBUF(IREF+4)
            Y0=CBUF(IREF+5)
            Z0=CBUF(IREF+6)
*   Spheres.
       ELSEIF(ISOLTP(I).EQ.4)THEN
            X0=CBUF(IREF+2)
            Y0=CBUF(IREF+3)
            Z0=CBUF(IREF+4)
*   Toblerone.
       ELSEIF(ISOLTP(I).EQ.5)THEN
            X0=CBUF(IREF+4)
            Y0=CBUF(IREF+5)
            Z0=CBUF(IREF+6)
*   Extrusion.
       ELSEIF(ISOLTP(I).EQ.6)THEN
            X0=CBUF(IREF+3)
            Y0=CBUF(IREF+4)
            Z0=CBUF(IREF+5)
*   Anything else.
       ELSE
            PRINT *,' !!!!!! CELVPT WARNING : Encountered'//
     -           ' an unknown solid type ',ISOLTP(I)
            X0=0
            Y0=0
            Z0=0
       ENDIF
**  Reduce to the period nearest the centre
       DX=XPOS-X0
       DY=YPOS-Y0
       DZ=ZPOS-Z0
       IF(PERX)DX=DX-SX*ANINT(DX/SX)
       IF(PERY)DY=DY-SY*ANINT(DY/SY)
       IF(PERZ)DZ=DZ-SZ*ANINT(DZ/SZ)
**  Check position relative to solid, first cylinders.
       IF(ISOLTP(I).EQ.1)THEN
            CALL PLACYI(I,X0+DX,Y0+DY,Z0+DZ,INSIDE)
            IF(INSIDE)THEN
                 IVOL=I
                 RETURN
            ENDIF
*   Holes.
       ELSEIF(ISOLTP(I).EQ.2)THEN
            CALL PLACHI(I,X0+DX,Y0+DY,Z0+DZ,INSIDE)
            IF(INSIDE)THEN
                 IVOL=I
                 RETURN
            ENDIF
*   Boxes.
       ELSEIF(ISOLTP(I).EQ.3)THEN
            CALL PLABXI(I,X0+DX,Y0+DY,Z0+DZ,INSIDE)
            IF(INSIDE)THEN
                 IVOL=I
                 RETURN
            ENDIF
*   Spheres.
       ELSEIF(ISOLTP(I).EQ.4)THEN
            CALL PLASPI(I,X0+DX,Y0+DY,Z0+DZ,INSIDE)
            IF(INSIDE)THEN
                 IVOL=I
                 RETURN
            ENDIF
*   Toblerone.
       ELSEIF(ISOLTP(I).EQ.5)THEN
            CALL PLATBI(I,X0+DX,Y0+DY,Z0+DZ,INSIDE)
            IF(INSIDE)THEN
                 IVOL=I
                 RETURN
            ENDIF
*   Extrusion.
       ELSEIF(ISOLTP(I).EQ.6)THEN
            CALL PLAEXI(I,X0+DX,Y0+DY,Z0+DZ,INSIDE)
            IF(INSIDE)THEN
                 IVOL=I
                 RETURN
            ENDIF
*   Other things.
       ELSE
            PRINT *,' !!!!!! CELVPT WARNING : Found a solid of'//
     -           ' unknown type; ignored.'
       ENDIF
10     CONTINUE
       END
