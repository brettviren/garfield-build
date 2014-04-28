CDECK  ID>, OPTADD.
       SUBROUTINE OPTADD(CHANGE)
*-----------------------------------------------------------------------
*   OPTADD - This routine adds items to the cell.
*   (Last changed on 29/11/00.)
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
       CHARACTER*(MXCHAR) STRING
       CHARACTER WIRCDE
       INTEGER INPCMP,INPTYP,NWORD,I,J,INEXT,IDIR,ICSET,IXYSET,
     -      IVSET,IFAIL,IFAIL1,IFAIL2,NC
       REAL S,COOR,VOLT,XWIR,YWIR,DWIR,VWIR,UWIR,WWIR,DENWIR
       EXTERNAL INPCMP,INPTYP
       LOGICAL CHANGE
*** Assume no change at first.
       CHANGE=.FALSE.
*** Pick up the number of arguments.
       CALL INPNUM(NWORD)
       IF(NWORD.LE.1)THEN
            PRINT *,' !!!!!! OPTADD WARNING : You must specify which'//
     -           ' items you wish to add; cell not changed.'
            RETURN
       ENDIF
*** Loop over the arguments.
       INEXT=2
       DO 10 I=2,NWORD
       IF(I.LT.INEXT)GOTO 10
**  Add a periodicity.
       IF(INPCMP(I,'PE#RIODICITY').NE.0)THEN
*   Check there are arguments.
            IF(I+2.LT.NWORD)THEN
                 CALL INPMSG(I,'Should have two arguments.    ')
                 GOTO 10
            ENDIF
*   Initialise the direction and length.
            IDIR=0
            S=-1.0
*   Check the coordinate system and reject r periodicities.
            IF(INPCMP(I+1,'X')+INPCMP(I+1,'Y').NE.0.AND.POLAR)THEN
                 CALL INPMSG(I+1,'Only polar elements permitted.')
                 INEXT=I+2
                 GOTO 10
            ELSEIF(INPCMP(I+1,'PHI').NE.0.AND..NOT.POLAR)THEN
                 CALL INPMSG(I+1,'No polar elements permitted.  ')
                 INEXT=I+2
                 GOTO 10
            ELSEIF(INPCMP(I+1,'R').NE.0)THEN
                 CALL INPMSG(I+1,'No radial periods permitted.  ')
                 INEXT=I+3
                 GOTO 10
            ENDIF
*   Read the length.
            IF(INPCMP(I+1,'X')+INPCMP(I+1,'Y')+
     -           INPCMP(I+1,'PHI').NE.0)THEN
                 CALL INPCHK(I+2,2,IFAIL)
                 IF(IFAIL.NE.0)THEN
                      INEXT=I+3
                      GOTO 10
                 ENDIF
                 CALL INPRDR(I+2,S,-1.0)
                 IF(S.LE.0.0)CALL INPMSG(I+2,
     -                'The length must be > 0.       ')
            ENDIF
*   Store the period direction.
            IF(INPCMP(I+1,'X').NE.0)THEN
                 IDIR=1
            ELSEIF(INPCMP(I+1,'Y').NE.0)THEN
                 IDIR=2
            ELSEIF(INPCMP(I+1,'PHI').NE.0)THEN
                 IDIR=4
            ELSEIF(INPCMP(I+1,'PE#RIODICITY')+INPCMP(I+1,'PL#ANE')+
     -           INPCMP(I+1,'W#IRE').NE.0)THEN
                 CALL INPMSG(I,'Should have two arguments.    ')
                 INEXT=I+1
                 GOTO 10
            ELSE
                 CALL INPMSG(I+1,'Not a valid period direction. ')
                 INEXT=I+1
                 GOTO 10
            ENDIF
*   Check the data and update the cell.
            IF(IDIR.EQ.0.OR.S.LE.0.0)THEN
                 CALL INPMSG(I,'Not a valid specification.    ')
            ELSEIF(IDIR.EQ.1)THEN
                 IF(PERX)PRINT *,' !!!!!! OPTADD WARNING :'//
     -                ' Previous x periodicity overridden.'
                 CHANGE=.TRUE.
                 PERX=.TRUE.
                 SX=S
            ELSEIF(IDIR.EQ.2)THEN
                 IF(PERY)PRINT *,' !!!!!! OPTADD WARNING :'//
     -                ' Previous y periodicity overridden.'
                 CHANGE=.TRUE.
                 PERY=.TRUE.
                 SY=S
            ELSEIF(IDIR.EQ.4)THEN
                 IF(PERY)PRINT *,' !!!!!! OPTADD WARNING :'//
     -                ' Previous phi periodicity overridden.'
                 IF(ABS(360.0-S*ANINT(360.0/S)).GT.1.0E-4)PRINT *,
     -                ' !!!!!! OPTADD WARNING : The phi period is'//
     -                ' rounded so that it divides 360.'
                 CHANGE=.TRUE.
                 PERY=.TRUE.
                 SY=2*PI*ANINT(360.0/S)
            ENDIF
*   Skip the words that were read.
            INEXT=I+3
**  Add a plane.
       ELSEIF(INPCMP(I,'PL#ANE').NE.0)THEN
*   Initialise the direction and coordinate.
            IDIR=0
            COOR=0.0
            ICSET=0
            VOLT=0.0
            WIRCDE=' '
*   Read the specified direction and length.
            DO 40 J=I+1,NWORD-1
            IF(J.LT.INEXT)GOTO 40
*   Trivial errors.
            IF(INPCMP(J,'R')+INPCMP(J,'PHI').NE.0.AND..NOT.POLAR)THEN
                 CALL INPMSG(J,'No polar planes are permitted.')
                 INEXT=J+1
                 GOTO 40
            ELSEIF(INPCMP(J,'X')+INPCMP(J,'Y').NE.0.AND.POLAR)THEN
                 CALL INPMSG(J,'Only polar planes permitted.  ')
                 INEXT=J+1
                 GOTO 40
            ENDIF
*   Pick up the direction, if it is one.
            IF(INPCMP(J,'X').NE.0)THEN
                 IDIR=1
            ELSEIF(INPCMP(J,'Y').NE.0)THEN
                 IDIR=2
            ELSEIF(INPCMP(J,'R').NE.0)THEN
                 IDIR=3
            ELSEIF(INPCMP(J,'PHI').NE.0)THEN
                 IDIR=4
            ENDIF
*   Pick up the position or the potential.
            IF(INPCMP(J,'R')+INPCMP(J,'PHI')+
     -           INPCMP(J,'X')+INPCMP(J,'Y').NE.0)THEN
                 CALL INPCHK(J+1,2,IFAIL)
                 CALL INPRDR(J+1,COOR,0.0)
                 INEXT=J+2
                 IF(INPCMP(J,'R').NE.0.AND.COOR.LE.0.0.AND.
     -                IFAIL.EQ.0)THEN
                      CALL INPMSG(J+1,'Radial coordinate must be > 0.')
                      GOTO 10
                 ENDIF
                 ICSET=1
            ELSEIF(INPCMP(J,'V#OLTAGE').NE.0)THEN
                 CALL INPCHK(J+1,2,IFAIL)
                 CALL INPRDR(J+1,VOLT,0.0)
                 INEXT=J+2
*   Labels.
            ELSEIF(INPCMP(J,'LAB#EL').NE.0)THEN
                 CALL INPSTR(J+1,J+1,STRING,NC)
                 WIRCDE=STRING(1:1)
                 IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',WIRCDE).EQ.
     -                0)THEN
                      CALL INPMSG(J+1,'The label must be a letter.')
                      GOTO 10
                 ENDIF
                 INEXT=J+2
*   Other keywords.
            ELSEIF(INPCMP(J,'PE#RIODICITY')+INPCMP(J,'PL#ANE')+
     -           INPCMP(J,'W#IRE').NE.0)THEN
                 GOTO 50
            ELSE
                 CALL INPMSG(J,'Neither a V nor a direction.  ')
                 INEXT=J+1
            ENDIF
40          CONTINUE
*   Check the data and store the plane.
50          CONTINUE
            IF(IDIR.EQ.0.OR.ICSET.EQ.0)THEN
                 CALL INPMSG(I,'Not a valid specification.    ')
            ELSEIF(IDIR.EQ.1.OR.IDIR.EQ.3)THEN
                 IF(IDIR.EQ.3)COOR=LOG(COOR)
                 IF(.NOT.YNPLAN(1))THEN
                      YNPLAN(1)=.TRUE.
                      COPLAN(1)=COOR
                      VTPLAN(1)=VOLT
                      PLATYP(1)=WIRCDE
                      INDPLA(1)=0
                      NPSTR1(1)=0
                      NPSTR2(1)=0
                      CHANGE=.TRUE.
                 ELSEIF(.NOT.YNPLAN(2))THEN
                      YNPLAN(2)=.TRUE.
                      COPLAN(2)=COOR
                      VTPLAN(2)=VOLT
                      PLATYP(2)=WIRCDE
                      INDPLA(2)=0
                      NPSTR1(2)=0
                      NPSTR2(2)=0
                      CHANGE=.TRUE.
                 ELSE
                      CALL INPMSG(I,'No room for further planes.   ')
                 ENDIF
            ELSEIF(IDIR.EQ.2.OR.IDIR.EQ.4)THEN
                 IF(IDIR.EQ.3)COOR=PI*COOR/180.0
                 IF(.NOT.YNPLAN(3))THEN
                      YNPLAN(3)=.TRUE.
                      COPLAN(3)=COOR
                      VTPLAN(3)=VOLT
                      PLATYP(3)=WIRCDE
                      INDPLA(3)=0
                      NPSTR1(3)=0
                      NPSTR2(3)=0
                      CHANGE=.TRUE.
                 ELSEIF(.NOT.YNPLAN(4))THEN
                      YNPLAN(4)=.TRUE.
                      COPLAN(4)=COOR
                      VTPLAN(4)=VOLT
                      PLATYP(4)=WIRCDE
                      INDPLA(4)=0
                      NPSTR1(4)=0
                      NPSTR2(4)=0
                      CHANGE=.TRUE.
                 ELSE
                      CALL INPMSG(I,'No room for further planes.   ')
                 ENDIF
            ENDIF
**   Add a wire.
       ELSEIF(INPCMP(I,'W#IRE').NE.0)THEN
*    Initialise wire-code, diameter, position and potential.
            WIRCDE='?'
            XWIR=0.0
            YWIR=0.0
            IXYSET=0
            VWIR=0.0
            IVSET=0
            DWIR=0.0100
            UWIR=100.0
            WWIR=50.0
            DENWIR=19.3
*    Loop over the keywords.
            DO 70 J=I+1,NWORD
            IF(J.LT.INEXT)GOTO 70
*    Wire position.
            IF(INPCMP(J,'AT').NE.0)THEN
                 IF(J+2.GT.NWORD.OR.INPTYP(J+1).LE.0.OR.
     -                INPTYP(J+2).LE.0)THEN
                      CALL INPMSG(I,'Needs two numeric arguments.  ')
                      IF(INPTYP(J+1).LE.0)THEN
                           INEXT=J+1
                      ELSEIF(INPTYP(J+2).LE.0)THEN
                           INEXT=J+2
                      ELSE
                           INEXT=J+3
                      ENDIF
                      GOTO 70
                 ENDIF
                 CALL INPCHK(J+1,2,IFAIL1)
                 CALL INPCHK(J+2,2,IFAIL2)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0)THEN
                      CALL INPRDR(J+1,XWIR,0.0)
                      CALL INPRDR(J+2,YWIR,0.0)
                      IF(POLAR.AND.XWIR.LE.0.0)THEN
                           CALL INPMSG(J+1,
     -                          'Invalid polar coordinate.     ')
                      ELSE
                           IXYSET=1
                      ENDIF
                 ENDIF
                 INEXT=J+3
*   Wire potential.
            ELSEIF(INPCMP(J,'V#OLTAGE').NE.0)THEN
                 IF(J+1.GT.NWORD.OR.INPTYP(J+1).LE.0)THEN
                      CALL INPMSG(I,'Needs one numeric argument.   ')
                      IF(INPTYP(J+1).LE.0)THEN
                           INEXT=J+1
                      ELSE
                           INEXT=J+2
                      ENDIF
                      GOTO 70
                 ENDIF
                 CALL INPCHK(J+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(J+1,VWIR,0.0)
                      IVSET=1
                 ENDIF
                 INEXT=J+2
*   Wire label.
            ELSEIF(INPCMP(J,'TYP#E')+INPCMP(J,'LAB#EL').NE.0)THEN
                 IF(J+1.GT.NWORD)THEN
                      CALL INPMSG(J,'Has one character as argument.')
                      INEXT=J+1
                      GOTO 70
                 ENDIF
                 CALL INPSTR(J+1,J+1,STRING,NC)
                 IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',
     -                STRING(1:1)).EQ.0)THEN
                      CALL INPMSG(J+1,'Non-alphabetic first character')
                 ELSE
                      WIRCDE=STRING(1:1)
                 ENDIF
                 INEXT=J+2
*    Wire diameter.
            ELSEIF(INPCMP(J,'D#IAMETER').NE.0)THEN
                 IF(J+1.GT.NWORD.OR.INPTYP(J+1).LE.0)THEN
                      CALL INPMSG(I,'Needs one numeric argument.   ')
                      IF(INPTYP(J+1).LE.0)THEN
                           INEXT=J+1
                      ELSE
                           INEXT=J+2
                      ENDIF
                      GOTO 70
                 ENDIF
                 CALL INPCHK(J+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(J+1,DWIR,0.01)
                      IF(DENWIR.LE.0)CALL INPMSG(J+1,
     -                      'The diameter must be > 0.')
                 ENDIF
                 INEXT=J+2
*   Density.
            ELSEIF(INPCMP(J,'DENS#ITY')+INPCMP(J,'MAT#ERIAL').NE.0)THEN
                 IF(J+1.GT.NWORD)THEN
                      CALL INPMSG(J,'Has an argument.')
                      INEXT=J+1
                 ELSEIF(INPCMP(J+1,'CU-BE#RYLLIUM')+
     -                INPCMP(J+1,'C#OPPER-BE#RYLLIUM')+
     -                INPCMP(J+1,'BE#RYLLIUM-#CU')+
     -                INPCMP(J+1,'BE#RYLLIUM-#COPPER').NE.0)THEN
                      DENWIR=8.7
                      INEXT=J+2
                 ELSEIF(INPTYP(J+1).EQ.4.OR.INPCMP(J+1,'W')+
     -                INPCMP(J+1,'TUNG#STEN').NE.0)THEN
                      DENWIR=19.3
                      INEXT=J+2
                 ELSEIF(INPTYP(J+1).EQ.1.OR.INPTYP(J+1).EQ.2)THEN
                      CALL INPCHK(J+1,2,IFAIL1)
                      CALL INPRDR(J+1,DENWIR,19.3)
                      IF(DENWIR.LE.0)CALL INPMSG(J+1,
     -                      'The density must be > 0.')
                      INEXT=J+2
                 ELSE
                      CALL INPMSG(J+1,'Not a valid argument.')
                      INEXT=J+2
                 ENDIF
*   Length.
            ELSEIF(INPCMP(J,'L#ENGTH').NE.0)THEN
                 IF(J+1.GT.NWORD.OR.INPTYP(J+1).LE.0)THEN
                      CALL INPMSG(I,'Needs one numeric argument.   ')
                      IF(INPTYP(J+1).LE.0)THEN
                           INEXT=J+1
                      ELSE
                           INEXT=J+2
                      ENDIF
                      GOTO 70
                 ENDIF
                 CALL INPCHK(J+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(J+1,UWIR,100.0)
                      IF(UWIR.LE.0)CALL INPMSG(J+1,
     -                      'The length must be > 0.')
                 ENDIF
                 INEXT=J+2
*   Weight.
            ELSEIF(INPCMP(J,'W#EIGHT')+INPCMP(J,'TENS#ION').NE.0)THEN
                 IF(J+1.GT.NWORD.OR.INPTYP(J+1).LE.0)THEN
                      CALL INPMSG(I,'Needs one numeric argument.   ')
                      IF(INPTYP(J+1).LE.0)THEN
                           INEXT=J+1
                      ELSE
                           INEXT=J+2
                      ENDIF
                      GOTO 70
                 ENDIF
                 CALL INPCHK(J+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(J+1,WWIR,50.0)
                      IF(WWIR.LE.0)CALL INPMSG(J+1,
     -                      'The weight must be > 0.')
                 ENDIF
                 INEXT=J+2
*   Back to main category.
            ELSEIF(INPCMP(J,'PE#RIODICITY')+INPCMP(J,'PL#ANE')+
     -           INPCMP(J,'W#IRE').NE.0)THEN
                 GOTO 80
*   Unrecognised keyword.
            ELSE
                 CALL INPMSG(J,'Not a valid keyword.')
            ENDIF
70          CONTINUE
*   Check whether sufficient data were provided.
80          CONTINUE
            IF(IXYSET.EQ.0.OR.WIRCDE.EQ.'?')THEN
                 CALL INPMSG(I,'Incompletely specified wire.')
            ELSEIF(DWIR.LE.0.OR.UWIR.LE.0.OR.WWIR.LE.0.OR.
     -           DENWIR.LE.0)THEN
                 CALL INPMSG(I,'Invalid wire specification.')
            ELSEIF(NWIRE.GE.MXWIRE)THEN
                 CALL INPMSG(I,'No room for further wires.')
            ELSE
                 NWIRE=NWIRE+1
                 X(NWIRE)=XWIR
                 Y(NWIRE)=YWIR
                 V(NWIRE)=VWIR
                 D(NWIRE)=DWIR
                 U(NWIRE)=UWIR
                 W(NWIRE)=WWIR
                 DENS(NWIRE)=DENWIR
                 WIRTYP(NWIRE)=WIRCDE
                 INDSW(NWIRE)=0
                 IF(POLAR)THEN
                      D(NWIRE)=D(NWIRE)/X(NWIRE)
                      CALL CFMPTR(X(NWIRE),Y(NWIRE),X(NWIRE),Y(NWIRE),1,
     -                     IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                           CALL INPMSG(I,
     -                          'Invalid polar position.       ')
                           NWIRE=NWIRE-1
                      ENDIF
                 ENDIF
                 CHANGE=.TRUE.
            ENDIF
**   Unrecognised argument.
       ELSE
            CALL INPMSG(I,'Not PERIOD, PLANE or WIRE.    ')
       ENDIF
10     CONTINUE
       CALL INPERR
       END
