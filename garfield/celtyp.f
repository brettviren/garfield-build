CDECK  ID>, CELTYP.
       SUBROUTINE CELTYP(IFAIL)
*-----------------------------------------------------------------------
*   CELTYP - Determines the cell type, see the writeup for explanations.
*   VARIABLES : no local variables.
*   (Last changed on  1/ 3/09.)
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
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
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
       CHARACTER*10 USER
       INTEGER IFAIL,IFAIL1
*** Assume this fails.
       IFAIL=1
*** Identify BEM structures.
       IF(BEMSET.AND.NWIRE.EQ.0.AND.
     -      .NOT.(YNPLAN(1).OR.YNPLAN(2).OR.YNPLAN(3).OR.YNPLAN(4)).AND.
     -      .NOT.TUBE)THEN
            TYPE='BEM'
            GOTO 10
       ENDIF
*** Identify field maps.
       CALL BOOK('INQUIRE','MAP',USER,IFAIL1)
*   Unable to tell: assume this isn't a field map.
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! CELTYP WARNING : Unable to obtain'//
     -           ' field map allocation information ; assumed to'//
     -           ' be a non-field map cell.'
*   Field map chamber: ensure that there are no other elements.
       ELSEIF(USER.EQ.'CELL'.AND.
     -      (MAPFLG(2).OR.MAPFLG(3).OR.MAPFLG(4).OR.MAPFLG(5)))THEN
            TYPE='MAP'
            GOTO 10
       ENDIF
*** Provisionally handle all TUBE type cells via D00.
       IF(TUBE)THEN
            IF(NTUBE.EQ.0)THEN
                 IF(PERY)THEN
                      TYPE='D2 '
                 ELSE
                      TYPE='D1 '
                 ENDIF
            ELSEIF(NTUBE.GE.3.AND.NTUBE.LE.8)THEN
                 IF(PERY)THEN
                      TYPE='D4 '
                 ELSE
                      TYPE='D3 '
                 ENDIF
            ELSE
                 PRINT *,' !!!!!! CELTYP WARNING : Potentials not yet'//
     -                ' available, using a round tube.'
                 NTUBE=0
                 TYPE='D3 '
            ENDIF
            GOTO 10
       ENDIF
*** Find the 'A' type cell.
       IF(.NOT.(PERX.OR.PERY).AND.
     -    .NOT.(YNPLAN(1).AND.YNPLAN(2)).AND.
     -    .NOT.(YNPLAN(3).AND.YNPLAN(4)))THEN
            TYPE='A  '
            GOTO 10
       ENDIF
*** Find the 'B1X' type cell.
       IF(PERX.AND..NOT.PERY.AND.
     -    .NOT.(YNPLAN(1).OR.YNPLAN(2)).AND.
     -    .NOT.(YNPLAN(3).AND.YNPLAN(4)))THEN
            TYPE='B1X'
            GOTO 10
       ENDIF
*** Find the 'B1Y' type cell.
       IF(PERY.AND..NOT.PERX.AND.
     -    .NOT.(YNPLAN(1).AND.YNPLAN(2)).AND.
     -    .NOT.(YNPLAN(3).OR.YNPLAN(4)))THEN
            TYPE='B1Y'
            GOTO 10
       ENDIF
*** Find the 'B2X' type cell.
       IF(PERX.AND..NOT.PERY.AND.
     -    .NOT.(YNPLAN(3).AND.YNPLAN(4)))THEN
            TYPE='B2X'
            GOTO 10
       ENDIF
       IF(.NOT.(PERX.OR.PERY).AND.
     -    .NOT.(YNPLAN(3).AND.YNPLAN(4)).AND.
     -         (YNPLAN(1).AND.YNPLAN(2)))THEN
            SX=ABS(COPLAN(2)-COPLAN(1))
            TYPE='B2X'
            GOTO 10
       ENDIF
*** Find the 'B2Y' type cell.
       IF(PERY.AND..NOT.PERX.AND.
     -    .NOT.(YNPLAN(1).AND.YNPLAN(2)))THEN
            TYPE='B2Y'
            GOTO 10
       ENDIF
       IF(.NOT.(PERX.OR.PERY).AND.
     -    .NOT.(YNPLAN(1).AND.YNPLAN(2)).AND.
     -         (YNPLAN(3).AND.YNPLAN(4)))THEN
            SY=ABS(COPLAN(4)-COPLAN(3))
            TYPE='B2Y'
            GOTO 10
       ENDIF
*** Find the 'C1 ' type cell.
       IF(.NOT.(YNPLAN(1).OR.YNPLAN(2).OR.YNPLAN(3).OR.YNPLAN(4)).AND.
     -     PERX.AND.PERY)THEN
            TYPE='C1 '
            GOTO 10
       ENDIF
*** Find the 'C2X' type cell.
       IF(.NOT.((YNPLAN(3).AND.PERY).OR.(YNPLAN(3).AND.YNPLAN(4))))THEN
            IF(YNPLAN(1).AND.YNPLAN(2))THEN
                 SX=ABS(COPLAN(2)-COPLAN(1))
                 TYPE='C2X'
                 GOTO 10
            ENDIF
            IF(PERX.AND.YNPLAN(1))THEN
                 TYPE='C2X'
                 GOTO 10
            ENDIF
       ENDIF
*** Find the 'C2Y' type cell.
       IF(.NOT.((YNPLAN(1).AND.PERX).OR.(YNPLAN(1).AND.YNPLAN(2))))THEN
            IF(YNPLAN(3).AND.YNPLAN(4))THEN
                 SY=ABS(COPLAN(4)-COPLAN(3))
                 TYPE='C2Y'
                 GOTO 10
            ENDIF
            IF(PERY.AND.YNPLAN(3))THEN
                 TYPE='C2Y'
                 GOTO 10
            ENDIF
       ENDIF
*** Find the 'C3 ' type cell.
       IF(PERX.AND.PERY)THEN
            TYPE='C3 '
            GOTO 10
       ENDIF
       IF(PERX)THEN
            TYPE='C3 '
            SY=ABS(COPLAN(4)-COPLAN(3))
            GOTO 10
       ENDIF
       IF(PERY)THEN
            TYPE='C3 '
            SX=ABS(COPLAN(2)-COPLAN(1))
            GOTO 10
       ENDIF
       IF(YNPLAN(1).AND.YNPLAN(2).AND.YNPLAN(3).AND.YNPLAN(4))THEN
            TYPE='C3 '
            SX=ABS(COPLAN(2)-COPLAN(1))
            SY=ABS(COPLAN(4)-COPLAN(3))
            GOTO 10
       ENDIF
*** Fatal error if the cell is not recognised.
       PRINT *,' !!!!!! CELTYP WARNING : Cell type not recognised;',
     -      ' program error - please send a message.'
       RETURN
10     CONTINUE
*** Make sure the periodicities are positive numbers.
       SX=ABS(SX)
       SY=ABS(SY)
*** Store a numerical code for the cell type for greater efficiency.
       IF(TYPE.EQ.'BEM')ICTYPE=-1
       IF(TYPE.EQ.'MAP')ICTYPE=0
       IF(TYPE.EQ.'A  ')ICTYPE=1
       IF(TYPE.EQ.'B1X')ICTYPE=2
       IF(TYPE.EQ.'B1Y')ICTYPE=3
       IF(TYPE.EQ.'B2X')ICTYPE=4
       IF(TYPE.EQ.'B2Y')ICTYPE=5
       IF(TYPE.EQ.'C1 ')ICTYPE=6
       IF(TYPE.EQ.'C2X')ICTYPE=7
       IF(TYPE.EQ.'C2Y')ICTYPE=8
       IF(TYPE.EQ.'C3 ')ICTYPE=9
       IF(TYPE.EQ.'D1 ')ICTYPE=10
       IF(TYPE.EQ.'D2 ')ICTYPE=11
       IF(TYPE.EQ.'D3 ')ICTYPE=12
       IF(TYPE.EQ.'D4 ')ICTYPE=13
*** Things are OK if we get this far.
       IFAIL=0
*** Store the amount of CPU time used for cell identification.
       CALL TIMLOG('Finding the cell type (A, B1X etc):     ')
       END
