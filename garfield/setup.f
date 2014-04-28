CDECK  ID>, SETUP.
       SUBROUTINE SETUP(IFAIL)
*-----------------------------------------------------------------------
*   SETUP  - Routine calling the appropriate setup routine.
*   (Last changed on 19/ 9/07.)
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
       INTEGER IFAIL,IFAIL1
*** Try to obtain the storage used for the capacitance matrix.
       CALL BOOK('BOOK','MATRIX','CELL',IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! SETUP  WARNING : Unable to allocate'//
     -           ' storage for the capacitance matrix; no charges.'
            IF(LDEBUG)CALL BOOK('LIST',' ',' ',IFAIL1)
            RETURN
       ENDIF
*** Set a separate set of plane variables to avoid repeated loops.
       IF(YNPLAN(1))THEN
            COPLAX=COPLAN(1)
            YNPLAX=.TRUE.
       ELSEIF(YNPLAN(2))THEN
            COPLAX=COPLAN(2)
            YNPLAX=.TRUE.
       ELSE
            YNPLAX=.FALSE.
       ENDIF
       IF(YNPLAN(3))THEN
            COPLAY=COPLAN(3)
            YNPLAY=.TRUE.
       ELSEIF(YNPLAN(4))THEN
            COPLAY=COPLAN(4)
            YNPLAY=.TRUE.
       ELSE
            YNPLAY=.FALSE.
       ENDIF
*** Set the correction parameters for the planes.
       IF(TUBE)THEN
            CORVTA=0.0
            CORVTB=0.0
            CORVTC=VTTUBE
       ELSEIF((YNPLAN(1).AND.YNPLAN(2)).AND.
     -      .NOT.(YNPLAN(3).OR.YNPLAN(4)))THEN
            CORVTA=(VTPLAN(1)-VTPLAN(2))/(COPLAN(1)-COPLAN(2))
            CORVTB=0.0
            CORVTC=(VTPLAN(2)*COPLAN(1)-VTPLAN(1)*COPLAN(2))/
     -             (COPLAN(1)-COPLAN(2))
       ELSEIF((YNPLAN(3).AND.YNPLAN(4)).AND.
     -      .NOT.(YNPLAN(1).OR.YNPLAN(2)))THEN
            CORVTA=0.0
            CORVTB=(VTPLAN(3)-VTPLAN(4))/(COPLAN(3)-COPLAN(4))
            CORVTC=(VTPLAN(4)*COPLAN(3)-VTPLAN(3)*COPLAN(4))/
     -             (COPLAN(3)-COPLAN(4))
       ELSE
            CORVTA=0
            CORVTB=0
            CORVTC=0
            IF(YNPLAN(1))CORVTC=VTPLAN(1)
            IF(YNPLAN(2))CORVTC=VTPLAN(2)
            IF(YNPLAN(3))CORVTC=VTPLAN(3)
            IF(YNPLAN(4))CORVTC=VTPLAN(4)
       ENDIF
*** Call the set routine appropriate for the present cell type.
       IF(NWIRE.GT.0)THEN
            IF(TYPE.EQ.'A  '.AND.NXMATT.EQ.0.AND.NYMATT.EQ.0)THEN
                 CALL SETA00(IFAIL)
            ELSEIF(TYPE.EQ.'A  ')THEN
                 CALL EFQA00(IFAIL)
            ENDIF
            IF(TYPE.EQ.'B1X')CALL SETB1X(IFAIL)
            IF(TYPE.EQ.'B1Y')CALL SETB1Y(IFAIL)
            IF(TYPE.EQ.'B2X')CALL SETB2X(IFAIL)
            IF(TYPE.EQ.'B2Y')CALL SETB2Y(IFAIL)
            IF(TYPE.EQ.'C1 ')CALL SETC10(IFAIL)
            IF(TYPE.EQ.'C2X')CALL SETC2X(IFAIL)
            IF(TYPE.EQ.'C2Y')CALL SETC2Y(IFAIL)
            IF(TYPE.EQ.'C3 ')CALL SETC30(IFAIL)
            IF(TYPE.EQ.'D1 ')CALL SETD10(IFAIL)
            IF(TYPE.EQ.'D2 ')CALL SETD20(IFAIL)
            IF(TYPE.EQ.'D3 ')CALL SETD30(IFAIL)
C           IF(TYPE.EQ.'D4 ')CALL SETD40(IFAIL)
*   Add dipole terms if required
            IF(LDIPOL)THEN
                 CALL SETDIP(IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! SETUP WARNING : Computing the'//
     -                     ' dipole moments failed; DIPOLE disabled.'
                      IFAIL=1
                  ENDIF
            ENDIF
*   Check the error condition.
            IF(IFAIL.EQ.1)PRINT *,' ###### SETUP  ERROR   : Preparing'//
     -           ' the cell for field calculations did not succeed.'
*** Do neBEM preparations.
       ELSEIF(TYPE.EQ.'BEM')THEN
            CALL BEMINT(IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! SETUP  WARNING :'//
     -                ' neBEM initialisation failed.'
                 IFAIL=1
            ENDIF
       ENDIF
*** Release the capacitance matrix.
       CALL BOOK('RELEASE','MATRIX','CELL',IFAIL1)
*** Register the amount of CPU time used.
       CALL TIMLOG('Calculating the wire charges:           ')
       END
