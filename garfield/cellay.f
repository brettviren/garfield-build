CDECK  ID>, CELLAY.
       SUBROUTINE CELLAY(PXMIN,PYMIN,PXMAX,PYMAX)
*-----------------------------------------------------------------------
*   CELLAY - This routine draws all elements of the cell inside the
*            rectangle (PXMIN,PYMIN) to (PXMAX,PYMAX), taking care of
*            periodicities etc, on the plot being made.
*   VARIABLES : NXMIN,NXMAX: Numbers of resp first and last x-period.
*               NYMIN,NYMAX:    "    "   "     "    "   "   y   "
*               (XPOS,YPOS): Used for plotting (like XPL and YPL).
*               CHAR       : Used because WIRTYP(I) may start in the
*                            middle of a word.
*               XPL,YPL    : Used for plotting of lines.
*   (Last changed on  1/12/00.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL XPL(101),YPL(101),XPOS(1),YPOS(1),PXMIN,PYMIN,PXMAX,PYMAX
       INTEGER NX,NXMIN,NXMAX,NY,NYMIN,NYMAX,I,J,K
*** Determine the number of periods present in the cell.
       NXMIN=0
       NXMAX=0
       NYMIN=0
       NYMAX=0
       IF(PERX)THEN
            NXMIN=INT(PXMIN/SX)-1
            NXMAX=INT(PXMAX/SX)+1
       ENDIF
       IF(PERY)THEN
            NYMIN=INT(PYMIN/SY)-1
            NYMAX=INT(PYMAX/SY)+1
       ENDIF
*** Draw the field map if present.
       CALL MAPPLT(PXMIN,PYMIN,0.0,PXMAX,PYMAX,0.0)
*** Plot the wires as MARKERS.
       IF(LWRMRK)THEN
*   Loop over the wires.
            DO 130 I=1,NWIRE
*   Loop over the periods.
            DO 140 NX=NXMIN,NXMAX
            DO 150 NY=NYMIN,NYMAX
*   Non-tube shaped cells.
            IF(.NOT.TUBE)THEN
                 XPOS(1)=X(I)+NX*SX
                 IF(XPOS(1)+0.5*D(I).LE.PXMIN.OR.
     -                XPOS(1)-0.5*D(I).GE.PXMAX)GOTO 140
                 YPOS(1)=Y(I)+NY*SY
                 IF(YPOS(1)+0.5*D(I).LE.PYMIN.OR.
     -                YPOS(1)-0.5*D(I).GE.PYMAX)GOTO 150
                 IF(POLAR)CALL CFMRTC(XPOS,YPOS,XPOS,YPOS,1)
*   Tubed shaped cells.
            ELSE
                 CALL CFMCTP(X(I),Y(I),XPOS,YPOS,1)
                 IF(PERY)YPOS(1)=YPOS(1)+REAL(NY*360)/REAL(MTUBE)
                 CALL CFMPTC(XPOS,YPOS,XPOS,YPOS,1)
                 IF(XPOS(1)+0.5*D(I).LE.PXMIN.OR.
     -                XPOS(1)-0.5*D(I).GE.PXMAX.OR.
     -                YPOS(1)+0.5*D(I).LE.PYMIN.OR.
     -                YPOS(1)-0.5*D(I).GE.PYMAX)GOTO 150
            ENDIF
*   Choose the appropriate representation.
            IF(WIRTYP(I).EQ.'S')THEN
                 CALL GRATTS('S-WIRE','POLYMARKER')
            ELSEIF(WIRTYP(I).EQ.'P')THEN
                 CALL GRATTS('P-WIRE','POLYMARKER')
            ELSEIF(WIRTYP(I).EQ.'C')THEN
                 CALL GRATTS('C-WIRE','POLYMARKER')
            ELSE
                 CALL GRATTS('OTHER-WIRE','POLYMARKER')
            ENDIF
            CALL GRMARK(1,XPOS,YPOS)
150         CONTINUE
140         CONTINUE
130         CONTINUE
*** Plot the wires as AREAS.
       ELSE
*   Set fill area style, by default hollow to make GFA look like GPL.
            CALL GRATTS('WIRES','AREA')
*   Open a segment so that we can later on pick out the wires.
            CALL GCRSG(1)
*   Make the wires detectable.
            CALL GSDTEC(1,1)
*   Loop over all wires.
            DO 40 I=1,NWIRE
*   Set a pick identifier for each wire separately.
            CALL GSPKID(I)
*   Loop over the periods.
            DO 30 NX=NXMIN,NXMAX
            DO 20 NY=NYMIN,NYMAX
*   Non-tube shaped cells.
            IF(.NOT.TUBE)THEN
                 XPOS(1)=X(I)+NX*SX
                 IF(XPOS(1)+0.5*D(I).LE.PXMIN.OR.
     -                XPOS(1)-0.5*D(I).GE.PXMAX)GOTO 30
                 YPOS(1)=Y(I)+NY*SY
                 IF(YPOS(1)+0.5*D(I).LE.PYMIN.OR.
     -                YPOS(1)-0.5*D(I).GE.PYMAX)GOTO 20
                 IF(POLAR)CALL CFMRTC(XPOS,YPOS,XPOS,YPOS,1)
*   Tubed shaped cells.
            ELSE
                 CALL CFMCTP(X(I),Y(I),XPOS,YPOS,1)
                 IF(PERY)YPOS(1)=YPOS(1)+REAL(NY*360)/REAL(MTUBE)
                 CALL CFMPTC(XPOS,YPOS,XPOS,YPOS,1)
                 IF(XPOS(1)+0.5*D(I).LE.PXMIN.OR.
     -                XPOS(1)-0.5*D(I).GE.PXMAX.OR.
     -                YPOS(1)+0.5*D(I).LE.PYMIN.OR.
     -                YPOS(1)-0.5*D(I).GE.PYMAX)GOTO 20
            ENDIF
*   Calculate 20 points on each of the wires to make a circle.
            DO 10 J=1,21
            XPL(J)=XPOS(1)+0.5*D(I)*COS(PI*J/10.0)
            YPL(J)=YPOS(1)+0.5*D(I)*SIN(PI*J/10.0)
            IF(XPL(J).LT.PXMIN)XPL(J)=PXMIN
            IF(XPL(J).GT.PXMAX)XPL(J)=PXMAX
            IF(YPL(J).LT.PYMIN)YPL(J)=PYMIN
            IF(YPL(J).GT.PYMAX)YPL(J)=PYMAX
10          CONTINUE
*   Plots as fill areas.
            CALL GRAREA(21,XPL,YPL)
*   Next periods.
20          CONTINUE
30          CONTINUE
*   Next wire.
40          CONTINUE
*   Close the segment for the wires.
            CALL GCLSG
       ENDIF
*** Draw lines at the positions of the x (or r)-planes.
       DO 70 I=1,2
       DO 60 NX=NXMIN,NXMAX
       IF(YNPLAN(I))THEN
            CALL GRATTS('PLANES','POLYLINE')
            XPOS(1)=COPLAN(I)+NX*SX
            IF(XPOS(1).LE.PXMIN.OR.XPOS(1).GE.PXMAX)GOTO 60
            DO 50 J=1,101
            XPL(J)=XPOS(1)
            YPL(J)=PYMIN+(J-1)*(PYMAX-PYMIN)/100
50          CONTINUE
            IF(POLAR)CALL CFMRTC(XPL,YPL,XPL,YPL,101)
            CALL GRLINE(101,XPL,YPL)
            CALL GRATTS('STRIPS','POLYLINE')
            DO 160 J=1,NPSTR1(I)
            DO 170 K=1,101
            XPL(K)=XPOS(1)
            YPL(K)=MAX(PLSTR1(I,J,1),PYMIN)+(K-1)*
     -           (MIN(PLSTR1(I,J,2),PYMAX)-MAX(PLSTR1(I,J,1),PYMIN))/100
170         CONTINUE
            IF(POLAR)CALL CFMRTC(XPL,YPL,XPL,YPL,101)
            CALL GRLINE(101,XPL,YPL)
160         CONTINUE
       ENDIF
60     CONTINUE
70     CONTINUE
*** Draw lines at the positions of the y-planes.
       DO 100 I=3,4
       DO 90 NY=NYMIN,NYMAX
       IF(YNPLAN(I))THEN
            CALL GRATTS('PLANES','POLYLINE')
            YPOS(1)=COPLAN(I)+NY*SY
            IF(YPOS(1).LE.PYMIN.OR.YPOS(1).GE.PYMAX)GOTO 90
            DO 80 J=1,101
            XPL(J)=PXMIN+(J-1)*(PXMAX-PXMIN)/100
            YPL(J)=YPOS(1)
80          CONTINUE
            IF(POLAR)CALL CFMRTC(XPL,YPL,XPL,YPL,101)
            CALL GRLINE(101,XPL,YPL)
            CALL GRATTS('STRIPS','POLYLINE')
            DO 180 J=1,NPSTR1(I)
            DO 190 K=1,101
            XPL(K)=MAX(PLSTR1(I,J,1),PXMIN)+(K-1)*
     -           (MIN(PLSTR1(I,J,2),PXMAX)-MAX(PLSTR1(I,J,1),PXMIN))/100
            YPL(K)=YPOS(1)
190         CONTINUE
            IF(POLAR)CALL CFMRTC(XPL,YPL,XPL,YPL,101)
            CALL GRLINE(101,XPL,YPL)
180         CONTINUE
       ENDIF
90     CONTINUE
100    CONTINUE
*** Draw the dielectrica, first switch to fill are style hatched.
       CALL GRATTS('DIELECTRICA-1','AREA')
       DO 110 I=1,NXMATT
       XPL(1)=PXMIN
       IF(XMATT(I,3).EQ.0)XPL(1)=MIN(PXMAX,MAX(PXMIN,XMATT(I,1)))
       YPL(1)=PYMIN
       XPL(2)=XPL(1)
       YPL(2)=PYMAX
       XPL(3)=PXMAX
       IF(XMATT(I,4).EQ.0)XPL(3)=MIN(PXMAX,MAX(PXMIN,XMATT(I,2)))
       YPL(3)=PYMAX
       XPL(4)=XPL(3)
       YPL(4)=PYMIN
       XPL(5)=XPL(1)
       YPL(5)=YPL(1)
       CALL GRLINE(5,XPL,YPL)
       CALL GRAREA(5,XPL,YPL)
110    CONTINUE
       DO 120 I=1,NYMATT
       XPL(1)=PXMIN
       YPL(1)=PYMIN
       IF(YMATT(I,3).EQ.0)YPL(1)=MIN(PYMAX,MAX(PYMIN,YMATT(I,1)))
       XPL(2)=PXMAX
       YPL(2)=YPL(1)
       XPL(3)=PXMAX
       YPL(3)=PYMAX
       IF(YMATT(I,4).EQ.0)YPL(3)=MIN(PYMAX,MAX(PYMIN,YMATT(I,2)))
       XPL(4)=PXMIN
       YPL(4)=YPL(3)
       XPL(5)=XPL(1)
       YPL(5)=YPL(1)
       CALL GRLINE(5,XPL,YPL)
       CALL GRAREA(5,XPL,YPL)
120    CONTINUE
*** Draw the tube.
       CALL GRATTS('TUBE','POLYLINE')
       IF(TUBE.AND.NTUBE.EQ.0)THEN
            DO 200 I=1,101
            XPL(I)=COTUBE*COS(PI*REAL(I)/50.0)
            YPL(I)=COTUBE*SIN(PI*REAL(I)/50.0)
200         CONTINUE
            CALL GRLINE(101,XPL,YPL)
       ELSEIF(TUBE)THEN
            XPL(1)=COTUBE*COS(2*PI*REAL(0)/REAL(NTUBE))
            YPL(1)=COTUBE*SIN(2*PI*REAL(0)/REAL(NTUBE))
            DO 210 I=1,NTUBE
            XPL(2)=COTUBE*COS(2*PI*REAL(I)/REAL(NTUBE))
            YPL(2)=COTUBE*SIN(2*PI*REAL(I)/REAL(NTUBE))
            CALL GRLINE(2,XPL,YPL)
            XPL(1)=XPL(2)
            YPL(1)=YPL(2)
210         CONTINUE
       ENDIF
       END
