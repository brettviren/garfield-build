CDECK  ID>, CELSTR.
       SUBROUTINE CELSTR(IFAIL)
*-----------------------------------------------------------------------
*   CELSTR - Assigns default anode-cathode gaps, if applicable.
*   (Last changed on  7/12/00.)
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
       INTEGER IFAIL,I,J
       REAL GAPDEF(4)
*** Assume this will work.
       IFAIL=0
*** Compute default gaps.
       IF(YNPLAN(1))THEN
            IF(YNPLAN(2))THEN
                 GAPDEF(1)=COPLAN(2)-COPLAN(1)
            ELSEIF(NWIRE.LE.0)THEN
                 GAPDEF(1)=-1
            ELSE
                 GAPDEF(1)=X(1)-COPLAN(1)
                 DO 10 I=2,NWIRE
                 IF(X(I)-COPLAN(1).LT.GAPDEF(1))GAPDEF(1)=X(I)-COPLAN(1)
10               CONTINUE
            ENDIF
       ENDIF
       IF(YNPLAN(2))THEN
            IF(YNPLAN(1))THEN
                 GAPDEF(2)=COPLAN(2)-COPLAN(1)
            ELSEIF(NWIRE.LE.0)THEN
                 GAPDEF(2)=-1
            ELSE
                 GAPDEF(2)=COPLAN(2)-X(1)
                 DO 20 I=2,NWIRE
                 IF(COPLAN(2)-X(I).LT.GAPDEF(2))GAPDEF(2)=COPLAN(2)-X(I)
20               CONTINUE
            ENDIF
       ENDIF
       IF(YNPLAN(3))THEN
            IF(YNPLAN(4))THEN
                 GAPDEF(3)=COPLAN(4)-COPLAN(3)
            ELSEIF(NWIRE.LE.0)THEN
                 GAPDEF(3)=-1
            ELSE
                 GAPDEF(3)=Y(1)-COPLAN(3)
                 DO 30 I=2,NWIRE
                 IF(Y(I)-COPLAN(3).LT.GAPDEF(3))GAPDEF(3)=Y(I)-COPLAN(3)
30               CONTINUE
            ENDIF
       ENDIF
       IF(YNPLAN(4))THEN
            IF(YNPLAN(3))THEN
                 GAPDEF(4)=COPLAN(4)-COPLAN(3)
            ELSEIF(NWIRE.LE.0)THEN
                 GAPDEF(4)=-1
            ELSE
                 GAPDEF(4)=COPLAN(4)-X(1)
                 DO 40 I=2,NWIRE
                 IF(COPLAN(4)-Y(I).LT.GAPDEF(4))GAPDEF(4)=COPLAN(4)-Y(I)
40               CONTINUE
            ENDIF
       ENDIF
*** Assign.
       DO 50 I=1,4
       DO 60 J=1,NPSTR1(I)
       IF(PLSTR1(I,J,3).LT.0)PLSTR1(I,J,3)=GAPDEF(I)
       IF(PLSTR1(I,J,3).LT.0)THEN
            PRINT *,' !!!!!! CELSTR WARNING : Not able to set a'//
     -           ' default anode-cathode gap for x/y-strip ',J,
     -           ' of plane ',I,'.'
            IFAIL=1
       ENDIF
60     CONTINUE
       DO 70 J=1,NPSTR2(I)
       IF(PLSTR2(I,J,3).LT.0)PLSTR2(I,J,3)=GAPDEF(I)
       IF(PLSTR2(I,J,3).LT.0)THEN
            PRINT *,' !!!!!! CELSTR WARNING : Not able to set a'//
     -           ' default anode-cathode gap for z-strip ',J,
     -           ' of plane ',I,'.'
            IFAIL=1
       ENDIF
70     CONTINUE
50     CONTINUE
       END
