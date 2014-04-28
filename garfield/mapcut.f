CDECK  ID>, MAPCUT.
       SUBROUTINE MAPCUT(WXMIN,WYMIN,WZMIN,WXMAX,WYMAX,WZMAX,WINDOW)
*-----------------------------------------------------------------------
*   MAPCUT - Removes elements outside a window.
*   (Last changed on 15/ 7/08.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       INTEGER I,J,K,NEWMAP,NNODE
       REAL WXMIN,WYMIN,WZMIN,WXMAX,WYMAX,WZMAX,XP,YP,ZP
       LOGICAL WINDOW,LXYZ
*** Ensure we have a field map.
       IF(NMAP.LE.0.OR.MAPTYP.LE.0)THEN
            PRINT *,' !!!!!! MAPCUT WARNING : Currently no map; no'//
     -           ' cuts are applied.'
            RETURN
       ENDIF
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPCUT DEBUG   : Window'',
     -      '' cuts to be applied: '',L1/
     -      26X,E12.5,'' < x < '',E12.5/
     -      26X,E12.5,'' < y < '',E12.5/
     -      26X,E12.5,'' < z < '',E12.5)')
     -      WINDOW,WXMIN,WXMAX,WYMIN,WYMAX,WZMIN,WZMAX
*** Return if there is no cut to be applied
       IF(.NOT.WINDOW)RETURN
*** Set number of nodes
       IF(MAPTYP.EQ.1)THEN
            NNODE=3
            LXYZ=.FALSE.
       ELSEIF(MAPTYP.EQ.2)THEN
            NNODE=6
            LXYZ=.FALSE.
       ELSEIF(MAPTYP.EQ.4)THEN
            NNODE=4
            LXYZ=.FALSE.
       ELSEIF(MAPTYP.EQ.5)THEN
            NNODE=8
            LXYZ=.FALSE.
       ELSEIF(MAPTYP.EQ.11)THEN
            NNODE=4
            LXYZ=.TRUE.
       ELSEIF(MAPTYP.EQ.12)THEN
            NNODE=4
            LXYZ=.TRUE.
       ELSEIF(MAPTYP.EQ.13)THEN
            NNODE=10
            LXYZ=.TRUE.
       ELSEIF(MAPTYP.EQ.14)THEN
            NNODE=8
            LXYZ=.TRUE.
       ELSE
            PRINT *,' !!!!!! MAPCUT WARNING : Unknown map type ',MAPTYP,
     -           '; no cuts applied.'
            RETURN
       ENDIF
*** Initial new number of elements
       NEWMAP=0
*** Preset geometric range,
       XMMIN=0.0
       XMMAX=0.0
       YMMIN=0.0
       YMMAX=0.0
       ZMMIN=0.0
       ZMMAX=0.0
*** Reset axial ranges.
       SETAX=.FALSE.
       SETAY=.FALSE.
       SETAZ=.FALSE.
*** Loop over the elements.
       DO 10 I=1,NMAP
*   Check whether the element is inside the window.
       DO 20 J=1,NNODE
       IF(XMAP(I,J).LT.WXMIN.OR.XMAP(I,J).GT.WXMAX)GOTO 10
       IF(YMAP(I,J).LT.WYMIN.OR.YMAP(I,J).GT.WYMAX)GOTO 10
       IF(LXYZ.AND.(ZMAP(I,J).LT.WZMIN.OR.ZMAP(I,J).GT.WZMAX))GOTO 10
20     CONTINUE
*   New element index.
       NEWMAP=NEWMAP+1
       IF(NEWMAP.EQ.I)GOTO 10
       DO 30 J=1,10
*   Transfer coordinates and compute the range.
       XMAP(NEWMAP,J)=XMAP(I,J)
       XP=XMAP(NEWMAP,J)
       YMAP(NEWMAP,J)=YMAP(I,J)
       YP=YMAP(NEWMAP,J)
       IF(LXYZ)THEN
            ZMAP(NEWMAP,J)=ZMAP(I,J)
            ZP=ZMAP(NEWMAP,J)
       ELSE
            ZMAP(NEWMAP,J)=0.0
            ZP=0.0
       ENDIF
*   Geometric range.
       IF(NEWMAP.EQ.1.AND.J.EQ.1)THEN
            XMMIN=XP
            XMMAX=XP
            YMMIN=YP
            YMMAX=YP
            ZMMIN=ZP
            ZMMAX=ZP
       ELSE
            XMMIN=MIN(XMMIN,XP)
            XMMAX=MAX(XMMAX,XP)
            YMMIN=MIN(YMMIN,YP)
            YMMAX=MAX(YMMAX,YP)
            ZMMIN=MIN(ZMMIN,ZP)
            ZMMAX=MAX(ZMMAX,ZP)
       ENDIF
*   Update angular ranges.
       IF(YP.NE.0.0.OR.ZP.NE.0.0)THEN
            IF(SETAX)THEN
                 XAMIN=MIN(XAMIN,ATAN2(ZP,YP))
                 XAMAX=MAX(XAMAX,ATAN2(ZP,YP))
            ELSE
                 XAMIN=ATAN2(ZP,YP)
                 XAMAX=ATAN2(ZP,YP)
                 SETAX=.TRUE.
            ENDIF
       ENDIF
       IF(ZP.NE.0.0.OR.XP.NE.0.0)THEN
            IF(SETAY)THEN
                 YAMIN=MIN(YAMIN,ATAN2(XP,ZP))
                 YAMAX=MAX(YAMAX,ATAN2(XP,ZP))
            ELSE
                 YAMIN=ATAN2(XP,ZP)
                 YAMAX=ATAN2(XP,ZP)
                 SETAY=.TRUE.
            ENDIF
       ENDIF
       IF(XP.NE.0.0.OR.YP.NE.0.0)THEN
            IF(SETAZ)THEN
                 ZAMIN=MIN(ZAMIN,ATAN2(YP,XP))
                 ZAMAX=MAX(ZAMAX,ATAN2(YP,XP))
            ELSE
                 ZAMIN=ATAN2(YP,XP)
                 ZAMAX=ATAN2(YP,XP)
                 SETAZ=.TRUE.
            ENDIF
       ENDIF
30     CONTINUE
*   Field data
       DO 40 J=1,10
       VMAP(NEWMAP,J)=VMAP(I,J)
       EXMAP(NEWMAP,J)=EXMAP(I,J)
       EYMAP(NEWMAP,J)=EYMAP(I,J)
       EZMAP(NEWMAP,J)=EZMAP(I,J)
       BXMAP(NEWMAP,J)=BXMAP(I,J)
       BYMAP(NEWMAP,J)=BYMAP(I,J)
       BZMAP(NEWMAP,J)=BZMAP(I,J)
*   Range of the potentials
       IF(NEWMAP.EQ.1)THEN
            VMMIN=VMAP(NEWMAP,J)
            VMMAX=VMAP(NEWMAP,J)
       ELSE
            VMMIN=MIN(VMMIN,VMAP(NEWMAP,J))
            VMMAX=MAX(VMMAX,VMAP(NEWMAP,J))
       ENDIF
40     CONTINUE
*   Weighting fields
       DO 50 J=1,10
       DO 60 K=1,MXWMAP
       VWMAP(NEWMAP,J,K)=VWMAP(I,J,K)
       EWXMAP(NEWMAP,J,K)=EWXMAP(I,J,K)
       EWYMAP(NEWMAP,J,K)=EWYMAP(I,J,K)
       EWZMAP(NEWMAP,J,K)=EWZMAP(I,J,K)
60     CONTINUE
50     CONTINUE
*   Material index
       MATMAP(NEWMAP)=MATMAP(I)
*   Degeneracy flag
       ELMDGN(NEWMAP)=ELMDGN(I)
10     CONTINUE
*** Information message.
       PRINT *,' ------ MAPCUT MESSAGE : Eliminated ',NMAP-NEWMAP,
     -      ' elements out of ',NMAP,'.'
*** Store the new number of elements
       NMAP=NEWMAP
*** Set the same limits for the cell.
       XMIN=XMMIN
       XMAX=XMMAX
       YMIN=YMMIN
       YMAX=YMMAX
       ZMIN=ZMMIN
       ZMAX=ZMMAX
       VMIN=VMMIN
       VMAX=VMMAX
       IF(PERX.OR.PERMX)SX=ABS(XMMAX-XMMIN)
       IF(PERY.OR.PERMY)SY=ABS(YMMAX-YMMIN)
       IF(PERZ.OR.PERMZ)SZ=ABS(ZMMAX-ZMMIN)
       IF(PERRX)THEN
            XMIN=YMMIN
            XMAX=YMMAX
            YMIN=-MAX(ABS(XMMIN),ABS(XMMAX))
            YMAX=+MAX(ABS(XMMIN),ABS(XMMAX))
            ZMIN=-MAX(ABS(XMMIN),ABS(XMMAX))
            ZMAX=+MAX(ABS(XMMIN),ABS(XMMAX))
       ELSEIF(PERRY)THEN
            XMIN=-MAX(ABS(XMMIN),ABS(XMMAX))
            XMAX=+MAX(ABS(XMMIN),ABS(XMMAX))
            YMIN=YMMIN
            YMAX=YMMAX
            ZMIN=-MAX(ABS(XMMIN),ABS(XMMAX))
            ZMAX=+MAX(ABS(XMMIN),ABS(XMMAX))
       ELSEIF(PERRZ)THEN
            XMIN=-MAX(ABS(XMMIN),ABS(XMMAX))
            XMAX=+MAX(ABS(XMMIN),ABS(XMMAX))
            YMIN=-MAX(ABS(XMMIN),ABS(XMMAX))
            YMAX=+MAX(ABS(XMMIN),ABS(XMMAX))
            ZMIN=YMMIN
            ZMAX=YMMAX
       ENDIF
       IF(PERAX)THEN
            YMIN=-MAX(ABS(YMMIN),ABS(YMMAX),ABS(ZMMIN),ABS(ZMMAX))
            YMAX=+MAX(ABS(YMMIN),ABS(YMMAX),ABS(ZMMIN),ABS(ZMMAX))
            ZMIN=-MAX(ABS(YMMIN),ABS(YMMAX),ABS(ZMMIN),ABS(ZMMAX))
            ZMAX=+MAX(ABS(YMMIN),ABS(YMMAX),ABS(ZMMIN),ABS(ZMMAX))
       ELSEIF(PERAY)THEN
            XMIN=-MAX(ABS(XMMIN),ABS(XMMAX),ABS(ZMMIN),ABS(ZMMAX))
            XMAX=+MAX(ABS(XMMIN),ABS(XMMAX),ABS(ZMMIN),ABS(ZMMAX))
            ZMIN=-MAX(ABS(XMMIN),ABS(XMMAX),ABS(ZMMIN),ABS(ZMMAX))
            ZMAX=+MAX(ABS(XMMIN),ABS(XMMAX),ABS(ZMMIN),ABS(ZMMAX))
       ELSEIF(PERAZ)THEN
            XMIN=-MAX(ABS(XMMIN),ABS(XMMAX),ABS(YMMIN),ABS(YMMAX))
            XMAX=+MAX(ABS(XMMIN),ABS(XMMAX),ABS(YMMIN),ABS(YMMAX))
            YMIN=-MAX(ABS(XMMIN),ABS(XMMAX),ABS(YMMIN),ABS(YMMAX))
            YMAX=+MAX(ABS(XMMIN),ABS(XMMAX),ABS(YMMIN),ABS(YMMAX))
       ENDIF
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPCUT DEBUG   : Range''/
     -      26X,E12.5,'' < x < '',E12.5/
     -      26X,E12.5,'' < y < '',E12.5/
     -      26X,E12.5,'' < z < '',E12.5/
     -      26X,E12.5,'' < V < '',E12.5)')
     -      XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,VMIN,VMAX
       END
