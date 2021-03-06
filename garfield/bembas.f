CDECK  ID>, BEMBAS.
       SUBROUTINE BEMBAS
*-----------------------------------------------------------------------
*   BEMBAS - Reduces panels to the basic period.
*   (Last changed on 12/ 5/10.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER NPL,IPL,ICOL,IVOL,IFAIL1,IREF,NREF,NX,NY,NZ
       DOUBLE PRECISION XPL(MXPOIN),YPL(MXPOIN),ZPL(MXPOIN),APL,BPL,CPL,
     -      XC,YC,ZC,EPS
       LOGICAL MARK(MXPLAN)
*** Nothing to do if there is no periodicity.
       IF(.NOT.(PERX.OR.PERY.OR.PERZ))RETURN
*** Count panels.
       CALL PLABU1('QUERY',NREF,NPL,XPL,YPL,ZPL,
     -      APL,BPL,CPL,ICOL,IVOL,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! BEMBAS WARNING : Unable to'//
     -           ' count the number of panels.'
            RETURN
       ENDIF
*** Keep track of panels already processed.
       DO 10 IREF=1,MXPLAN
       MARK(IREF)=.FALSE.
10     CONTINUE
*** Loop over the panels.
       DO 20 IREF=1,NREF
       IF(MARK(IREF))GOTO 20
*   Retrieve.
       CALL PLABU1('READ',IREF,NPL,XPL,YPL,ZPL,APL,BPL,CPL,
     -      ICOL,IVOL,IFAIL1)
       IF(IFAIL1.NE.0)GOTO 20
*   Determine centre of gravity.
       XC=0
       YC=0
       ZC=0
       DO 30 IPL=1,NPL
       XC=XC+XPL(IPL)
       YC=YC+YPL(IPL)
       ZC=ZC+ZPL(IPL)
30     CONTINUE
       XC=XC/NPL
       YC=YC/NPL
       ZC=ZC/NPL
*   Any change ?
       EPS=1.0D-6
       IF(PERX)THEN
            NX=ANINT(XC/SX)
            IF(ABS(XC/SX-ANINT(XC/SX)-0.5D0).LT.EPS)NX=NX+1
       ELSE
            NX=0
       ENDIF
       IF(PERY)THEN
            NY=ANINT(YC/SY)
            IF(ABS(YC/SY-ANINT(YC/SY)-0.5D0).LT.EPS)NY=NY+1
       ELSE
            NY=0
       ENDIF
       IF(PERZ)THEN
            NZ=ANINT(ZC/SZ)
            IF(ABS(ZC/SZ-ANINT(ZC/SZ)-0.5D0).LT.EPS)NZ=NZ+1
       ELSE
            NZ=0
       ENDIF
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ BEMBAS DEBUG   : Shifts '',
     -      3(F15.8,1X,I3,1X))') XC/SX,NX,YC/SY,NY,ZC/SZ,NZ
*   Skip if there is no shift.
       IF(NX.EQ.0.AND.NY.EQ.0.AND.NZ.EQ.0)GOTO 20
*   Shift for x-periodicity.
       IF(NX.NE.0)THEN
            DO 40 IPL=1,NPL
            XPL(IPL)=XPL(IPL)-SX*NX
40          CONTINUE
       ENDIF
*   Shift for y-periodicity.
       IF(NY.NE.0)THEN
            DO 50 IPL=1,NPL
            YPL(IPL)=YPL(IPL)-SY*NY
50          CONTINUE
       ENDIF
*   Shift for z-periodicity.
       IF(NZ.NE.0)THEN
            DO 60 IPL=1,NPL
            ZPL(IPL)=ZPL(IPL)-SZ*NZ
60          CONTINUE
       ENDIF
*   Store new and delete old.
       CALL PLABU1('DELETE',IREF,NPL,XPL,YPL,ZPL,APL,BPL,CPL,
     -      ICOL,IVOL,IFAIL1)
       IF(IFAIL1.NE.0)PRINT *,' !!!!!! BEMBAS WARNING : Unable to'//
     -      ' delete old panel.'
       CALL PLABU1('STORE',IREF,NPL,XPL,YPL,ZPL,APL,BPL,CPL,
     -      ICOL,IVOL,IFAIL1)
       IF(IFAIL1.NE.0.OR.IREF.LE.0.OR.IREF.GE.MXPLAN)THEN
            PRINT *,' !!!!!! BEMBAS WARNING : Unable to store panel'//
     -           ' after move to basic period.'
            GOTO 20
       ENDIF
       MARK(IREF)=.TRUE.
*   Next panel.
20     CONTINUE
       END
