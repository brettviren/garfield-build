CDECK  ID>, ZROFND.
       SUBROUTINE ZROFND(ZXMIN,ZYMIN,ZXMAX,ZYMAX,IFAIL)
*-----------------------------------------------------------------------
*   ZROFND - This routine tries to find all zeros of the driftfield,
*            provided they are located in the rectangle (ZXMIN,ZYMIN),
*            (ZXMAX,ZYMAX). It stores them in the vector XZ,YZ.
*   VARIABLES:  XLST,YDST,XRST,YUST : Rectangle searched for zeros
*               IDIRST      : -1: Rectangle cut into 2 along y-axis,
*                             +1: as -1, but the 2 halves are finished,
*                             -2, +2: as -1 and +1, cut along the x-axis
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
       LOGICAL ZROSET
       REAL XZ,YZ,PZ,DPMIN,DPMAX,DAMIN,DAMAX,EMIN
       INTEGER NZ,NFC
       COMMON /ZRODAT/ XZ(MXZERO),YZ(MXZERO),PZ(MXZERO),NZ,NFC,
     -                 DPMIN,DPMAX,DAMIN,DAMAX,EMIN,ZROSET
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       DIMENSION XLST(MXZERO),XRST(MXZERO),YDST(MXZERO),YUST(MXZERO)
       INTEGER   NZST(MXZERO),IDIRST(MXZERO),ZROCNT
       EXTERNAL  ZROCNT
*** Define some output formats.
*** Identify the routine and start debugging output, if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE ZROFND ///'
       IF(LDEBUG)PRINT *,' ++++++ ZROFND DEBUG   : Start of debug',
     -      ' output'
*** Initialise some global parameters.
       NZ=0
       NFC=0
       EMIN=1.0E-5
       DAMIN=0.10
       DAMAX=0.30
       DPMIN=0.01
       DPMAX=0.20
       ZROSET=.FALSE.
*** Initialise the search stack.
       IST=1
       NZ=0
       XLST(1)=ZXMIN
       XRST(1)=ZXMAX
       YDST(1)=ZYMIN
       YUST(1)=ZYMAX
       IDIRST(1)=-1
       JWARN=0
       IFAIL=0
*** Begin of 'recursive' loop, find no of zeros in the rectangle.
10     CONTINUE
       IF(LDEBUG)WRITE(*,'(/26X,''IST='',I3,'' Area='',4F10.3)')
     -      IST,XLST(IST),YDST(IST),XRST(IST),YUST(IST)
       NZST(IST)=ZROCNT(XLST(IST),YDST(IST),XRST(IST),YUST(IST),IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' ###### ZROFND ERROR   : Search abandoned because',
     -              ' of a zero count error'
            RETURN
       ENDIF
       IF(NZST(IST).LT.0)THEN
            PRINT *,' ###### ZROFND ERROR   : Number of zeros < 0',
     -              ' (program bug) ; search abandoned'
            IFAIL=1
            RETURN
       ENDIF
       IF(LDEBUG)WRITE(*,'(34X,''The area contains '',I3,'' zeros.'')')
     -      NZST(IST)
*** Subtract the number of zeros from the number for the larger area.
       IF(IST.NE.1)NZST(IST-1)=NZST(IST-1)-NZST(IST)
*** 1 zero in the rectangle, check there is space left to store it,
       IF(NZST(IST).EQ.1)THEN
            IF(NZ+1.GT.MXZERO)THEN
                 PRINT *,' !!!!!! ZROFND WARNING : number of',
     -                   ' zeros exceeds MXZERO (=',MXZERO,');',
     -                   ' remaining zeros not considered.'
                 PRINT *,'                         Increase the',
     -                   ' MXZERO parameter to at least ',NZST(1),
     -                   ' and recompile the program.'
                 IFAIL=1
                 RETURN
            ENDIF
*   and try to locate it.
            NZ=NZ+1
            CALL ZROLOC(XZ(NZ),YZ(NZ),XLST(IST),YDST(IST),
     -           XRST(IST),YUST(IST),IFAIL)
            IF(IFAIL.NE.0)NZ=NZ-1
       ENDIF
*** No zeros left, climb in the stack until an unfinished level is found
       IF((NZST(IST).EQ.0.OR.NZST(IST).EQ.1).AND.IFAIL.EQ.0)THEN
20          CONTINUE
            IST=IST-1
            IF(IST.LT.1)GOTO 200
*   warn if negative zero counts are found,
            IF(NZST(IST).LT.0)THEN
                 IF(LDEBUG)WRITE(*,'(26X,''At IST='',I3,'' (flagged '',
     -                I2,'') negative zero count: '',I3,''.'')')
     -                IST,IDIRST(IST),NZST(IST)
                 JWARN=JWARN+1
*   warn for inconsistent counts (flagged finished but zeros left),
            ELSEIF(IDIRST(IST).GT.0.AND.NZST(IST).NE.0)THEN
                 IF(LDEBUG)WRITE(*,'(26X,''At IST='',I3,'' (flagged '',
     -                ''finished) '',I3,'' zeros left.'')')IST,NZST(IST)
                 JWARN=JWARN+1
            ENDIF
*   continue going upwards if the level is finished.
            IF(IDIRST(IST).GT.0.OR.NZST(IST).LE.0)GOTO 20
*   Go one level deeper again setting a new search area.
            IST=IST+1
            IF(IDIRST(IST-1).EQ.-1)THEN
                 IDIRST(IST-1)=+1
                 XLST(IST)=XRST(IST)
                 XRST(IST)=XRST(IST-1)
            ELSEIF(IDIRST(IST-1).EQ.-2)THEN
                 IDIRST(IST-1)=+2
                 YDST(IST)=YUST(IST)
                 YUST(IST)=YUST(IST-1)
            ENDIF
*** Handle the case there is more than one zero.
       ELSEIF(NZST(IST).GT.1.OR.IFAIL.NE.0)THEN
*   Make sure there is room in the stack,
            IF(IST+1.GT.MXZERO)THEN
                 PRINT *,' !!!!!! ZROFND WARNING : Stack exhausted;',
     -                   ' search for zeros abandoned.'
                 PRINT *,'                         Increase the',
     -                   ' MXZERO parameter and recompile the program.'
                 IFAIL=1
                 RETURN
            ENDIF
*   Split the area in 2, flag both halves as unfinished.
            IF(XRST(IST)-XLST(IST).GT.YUST(IST)-YDST(IST))THEN
                 IDIRST(IST)=-1
                 XLST(IST+1)=XLST(IST)
                 XRST(IST+1)=0.5*(XLST(IST)+XRST(IST))
                 YDST(IST+1)=YDST(IST)
                 YUST(IST+1)=YUST(IST)
            ELSE
                 IDIRST(IST)=-2
                 XLST(IST+1)=XLST(IST)
                 XRST(IST+1)=XRST(IST)
                 YDST(IST+1)=YDST(IST)
                 YUST(IST+1)=0.5*(YDST(IST)+YUST(IST))
            ENDIF
            IST=IST+1
       ENDIF
       GOTO 10
*   Normal end of this routine, warn for inconsistent zero counts.
200    CONTINUE
       IF(JWARN.NE.0)WRITE(*,'(/,''  !!!!!! ZROFND WARNING :'',
     -      '' Number of detected inconsistent zero counts='',I3,/,25X,
     -      '' zeros may well be missing and/or counted twice'')') JWARN
       IFAIL=0
       IF(LDEBUG)WRITE(*,'(/26X,''A total of '',I3,'' zeros has been'',
     -      '' located,'',/,26X,''requiring '',I4,'' function calls.''//
     -      ''  ++++++ ZROFND DEBUG   : End of debug output.'')')
     -      NZ,NFC
       END
