CDECK  ID>, CELINP.
       SUBROUTINE CELINP(IGET,IFAIL)
*-----------------------------------------------------------------------
*   CELINP - Subroutine reading the cell data from the input file. It
*            fills the common block wire in part.
*   VARIABLES : DX         : The x-increment in the present row.
*               DY         : The y-increment in the present row.
*               DV         : The voltage increment in the present row.
*               NX,NY      : Number of x,y planes read so far.
*               IGET       : 1 if data comes from dataset, 0 else.
*   (Last changed on 16/ 6/10.)
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
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
       CHARACTER*(MXINCH) STRING,FUNCT
       CHARACTER*10 VARLIS(MXVAR),USER
       CHARACTER DIR,PLATPR,STRTPR,WIRTPR
       REAL VAR(MXVAR),RES(8),DOWNXR,DOWNYR,DOWNZR,DNORM,
     -      DR,SR,UR,VR,WR,XR,YR,VARVAL,EPSR,CMIN,CMAX,S,COOR,VOLT,
     -      RADIUS,SMIN,SMAX,SAUX,GAP,
     -      WXMIN,WYMIN,WZMIN,WXMAX,WYMAX,WZMAX,AUX
       LOGICAL USE(MXVAR),CCART,CPOLAR,CTUBE,STDSTR,OK,DELETE,WINDOW
       INTEGER INPCMP,INPTYP,MODVAR(MXVAR),MODRES(8),NVAR,IENTRY,NRES,
     -      IFAIL1,IFAIL2,IFAIL3,IFAIL4,IFAIL5,IFAIL6,IFAIL7,IFAIL,
     -      IFOUND,MODTMP,IVAR,IDTYPE,
     -      INEXT,NR,NFUNCT,NTUBER,NX,NY,IGET,NWORD,I,J,NC,IPER
       EXTERNAL STDSTR,INPCMP,INPTYP
*** Preset error flag and indicator for file reading.
       IFAIL=0
       IGET=0
*** Initial number of DEFINE variables.
       NVAR=0
*** Initialise number of planes and coordinate system flags.
       NX=0
       NY=2
       CCART=.FALSE.
       CPOLAR=.FALSE.
       CTUBE=.FALSE.
*   Initialise the solids.
       NSOLID=0
       ICCURR=0
       CALL CELSCT('RESET')
*   Field map window
       WXMIN=-1
       WXMAX=+1
       WYMIN=-1
       WYMAX=+1
       WZMIN=-1
       WZMAX=+1
       WINDOW=.FALSE.
*   Release the matrix, will be reallocated by SETUP.
       CALL BOOK('INQUIRE','MATRIX',USER,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! CELINP WARNING : Unable to obtain'//
     -           ' capacitance allocation information ; wire and'//
     -           ' plane based fields probably not possible.'
       ELSEIF(USER.EQ.'CELL')THEN
            CALL BOOK('RELEASE','MATRIX','CELL',IFAIL1)
       ELSEIF(USER.NE.' ')THEN
            CALL BOOK('RELEASE','MATRIX',USER,IFAIL1)
            PRINT *,' ------ CELINP MESSAGE : Capacitance matrix'//
     -           ' was not released by ',USER,'; release forced.'
       ENDIF
*   Release CELL use of the field map.
       CALL BOOK('INQUIRE','MAP',USER,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! CELINP WARNING : Unable to obtain'//
     -           ' field map allocation information ; field map'//
     -           ' probably not useable.'
       ELSEIF(USER.EQ.'CELL')THEN
            CALL MAPINT
            CALL BOOK('RELEASE','MAP','CELL',IFAIL1)
       ELSEIF(USER.EQ.' ')THEN
            CALL MAPINT
       ELSE
            PRINT *,' !!!!!! CELINP WARNING : Field map is in use'//
     -           ' by ',USER,'; field map probably not useable.'
       ENDIF
*** Read a line from input.
       CALL INPPRM('Cell','NEW-PRINT')
10     CONTINUE
       CALL INPWRD(NWORD)
*** Skip this line if there are no words.
       CALL INPSTR(1,MXWORD,STRING,NC)
       IF(NWORD.EQ.0)GOTO 10
*** If an '&' is the first letter of the instr, it is the next section.
       IF(STRING(1:1).EQ.'&')THEN
            GOTO 50
*** If CELL-ID is a keyword: store CELLID.
       ELSEIF(INPCMP(1,'C#ELL-#IDENTIFIER').NE.0)THEN
            IF(NWORD.EQ.1.AND.CELLID.EQ.' ')THEN
                 WRITE(LUNOUT,'(2X/''No cell identification set at'',
     -                '' the moment.''/)')
            ELSEIF(NWORD.EQ.1)THEN
                 WRITE(LUNOUT,'(2X/''The current cell identification'',
     -                '' is: '',A/)') CELLID
            ELSE
                 CALL INPSTR(2,2,STRING,NC)
                 IF(NC.GT.40)PRINT *,' !!!!!! CELINP WARNING : The'//
     -                ' cell identifier is truncated to 40 characters.'
                 CELLID=STRING(1:MIN(NC,80))
            ENDIF
*** Cuts on solids.
       ELSEIF(INPCMP(1,'CUT-#SOLIDS').NE.0)THEN
            CALL CELSCT('STORE')
*** Add a new variable to the list, if DEFINE is a keyword.
       ELSEIF(INPCMP(1,'DEF#INE').NE.0)THEN
*   Display all variables if no arguments have been provided.
            IF(NWORD.EQ.1)THEN
                 IF(NVAR.EQ.0)THEN
                      WRITE(LUNOUT,'(/,
     -                     ''  No variables have been set sofar.''/)')
                 ELSE
                      WRITE(LUNOUT,'(/,
     -                     ''  Variable name            Value'')')
                      DO 13 I=1,NVAR
                      WRITE(LUNOUT,'(2X,A10,5X,F15.5)') VARLIS(I),VAR(I)
13                    CONTINUE
                      WRITE(LUNOUT,'('' '')')
                 ENDIF
*   Display only the value of the variable, if value is omitted.
            ELSEIF(NWORD.EQ.2)THEN
                 CALL INPSTR(2,2,STRING,NC)
                 IF(NC.LT.1)THEN
                      PRINT *,' !!!!!! CELINP WARNING : A variable'//
     -                     ' name must have at least some characters.'
                 ELSE
                      IFOUND=0
                      DO 18 I=1,NVAR
                      IF(VARLIS(I).EQ.STRING(1:NC))THEN
                           IF(IFOUND.EQ.0)WRITE(LUNOUT,'(/,
     -                          ''  Variable name            Value'')')
                           WRITE(LUNOUT,'(2X,A10,5X,F15.5)')
     -                          VARLIS(I),VAR(I)
                           IFOUND=1
                      ENDIF
18                    CONTINUE
                      IF(IFOUND.EQ.1)WRITE(LUNOUT,'('' '')')
                      IF(IFOUND.EQ.0)PRINT *,' !!!!!! CELINP WARNING'//
     -                     ' : The variable '//STRING(1:NC)//' has not',
     -                     ' yet been defined.'
                 ENDIF
*   Apparently a true define request, study it in detail.
            ELSEIF(NWORD.EQ.3)THEN
                 CALL INPSTR(3,3,STRING,NC)
                 CALL ALGPRE(STRING,NC,VARLIS,NVAR,NRES,USE,IENTRY,
     -                IFAIL1)
                 CALL ALGEXE(IENTRY,VAR,MODVAR,NVAR,
     -                RES,MODRES,1,IFAIL2)
                 CALL ALGCLR(IENTRY)
                 IF(IFAIL1+IFAIL2.NE.0)THEN
                      PRINT *,' !!!!!! CELINP WARNING : Variable'//
     -                     ' is not stored (syntax errors).'
                      GOTO 10
                 ENDIF
                 VARVAL=RES(1)
                 MODTMP=MODRES(1)
*   Extract the name of the variable.
                 CALL INPSTR(2,2,STRING,NC)
                 IF(NC.GT.10)THEN
                      PRINT *,' !!!!!! CELINP WARNING : '//STRING(1:NC)
     -                     //' is longer than 10 characters,'
                      PRINT *,'                         shortened to '//
     -                     STRING(1:10)//'.'
                 ENDIF
*   Check that the name is legal.
                 IFAIL1=0
                 DO 15 I=1,NC
                 IF(INDEX('+-*/&|<=#>^ ,.:;([{)]}''"`',
     -                STRING(I:I)).NE.0)THEN
                      PRINT *,' !!!!!! CELINP WARNING : '//STRING(1:NC)
     -                     //' contains illegal characters; not stored.'
                      GOTO 10
                 ENDIF
15               CONTINUE
                 IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',STRING(1:1))
     -                .EQ.0)THEN
                      PRINT *,' !!!!!! CELINP WARNING : The first'//
     -                     ' character of '//STRING(1:NC)//' is not'//
     -                     ' alphabetic; variable not stored.'
                      GOTO 10
                 ENDIF
*   Make sure it is not the reserved loop variable I.
                 IF(STRING(1:NC).EQ.'I'.AND.NC.EQ.1)THEN
                      PRINT *,' !!!!!! CELINP WARNING : I is reserved'//
     -                     ' as the ROW loop variable; not added.'
                      GOTO 10
                 ENDIF
*   See whether it duplicates an older variable,
                 IVAR=NVAR+1
                 DO 16 I=1,NVAR
                 IF(STRING(1:10).EQ.VARLIS(I))IVAR=I
16               CONTINUE
*   check that there is still room for new variables,
                 IF(IVAR+1.GE.MXVAR)THEN
                      PRINT *,' !!!!!! CELINP WARNING : No room for'//
     -                     ' new variables ; increase MXVAR.'
                      GOTO 10
                 ENDIF
*   and store it along with its value.
                 IF(IVAR.EQ.NVAR+1)THEN
                      NVAR=NVAR+1
                      VARLIS(NVAR)=STRING(1:10)
                 ELSE
                      IF(LDEBUG)PRINT *,' !!!!!! CELINP WARNING : '//
     -                     ' variable '//STRING(1:NC)//' is redefined.'
                      IF(LDEBUG)PRINT *,'                         '//
     -                     ' old value=',VAR(IVAR),' new value=',VARVAL
                 ENDIF
                 VAR(IVAR)=VARVAL
                 MODVAR(IVAR)=MODTMP
*   Incorrect number of arguments.
            ELSE
                 PRINT *,' !!!!!! CELINP WARNING : DEFINE needs 2'//
     -                ' arguments ; instruction is ignored.'
            ENDIF
*** Dielectrica.
       ELSEIF(INPCMP(1,'DIEL#ECTRICUM').NE.0)THEN
            PRINT *,' !!!!!! CELINP WARNING : Instruction not released.'
*   Initial values.
            EPSR=-1.0
            CMIN=0.0
            CMAX=0.0
            DIR=' '
            IDTYPE=2
            IFAIL1=1
            IFAIL2=1
            IFAIL3=1
*   Loop over the input string.
            INEXT=2
            DO 19 I=2,NWORD
            IF(I.LT.INEXT)GOTO 19
*   The extent and direction of the dielectricum.
            IF(INPCMP(I,'X-#RANGE')+INPCMP(I,'Y-#RANGE').NE.0)THEN
                 IF(I+2.GT.NWORD)THEN
                      CALL INPMSG(I,'RANGE needs two values.       ')
                 ELSEIF((INPTYP(I+1).LE.0.AND.
     -                INPCMP(I+1,'-INF#INITY')+
     -                INPCMP(I+1,'+INF#INITY')+
     -                INPCMP(I+1,'INF#INITY').EQ.0).OR.
     -                (INPTYP(I+2).LE.0.AND.
     -                INPCMP(I+2,'-INF#INITY')+
     -                INPCMP(I+2,'+INF#INITY')+
     -                INPCMP(I+2,'INF#INITY').EQ.0))THEN
                      CALL INPMSG(I,'Invalid range specification.  ')
                      INEXT=I+3
                 ELSEIF(INPCMP(I+1,'INF#INITY')+
     -                INPCMP(I+1,'+INF#INITY').NE.0)THEN
                      CALL INPMSG(I+1,'Should be -INF or a number.   ')
                      INEXT=I+3
                 ELSEIF(INPCMP(I+2,'-INF#INITY').NE.0)THEN
                      CALL INPMSG(I+2,'Should be +INF or a number.   ')
                      INEXT=I+3
                 ELSEIF(INPCMP(I+1,'-INF#INITY').NE.0.AND.
     -                INPCMP(I+2,'INF#INITY')+
     -                INPCMP(I+2,'+INF#INITY').NE.0)THEN
                      CALL INPMSG(I,'Full coverage is not allowed. ')
                      INEXT=I+3
                 ELSE
                      IF(INPCMP(I+1,'-INF#INITY').NE.0)THEN
                           IFAIL1=0
                           CALL INPCHK(I+2,2,IFAIL2)
                           IF(IFAIL2.EQ.0)CALL INPRDR(I+2,CMAX,0.0)
                           IDTYPE=-1
                      ELSEIF(INPCMP(I+2,'INF#INITY')+
     -                     INPCMP(I+2,'+INF#INITY').NE.0)THEN
                           CALL INPCHK(I+1,2,IFAIL1)
                           IF(IFAIL1.EQ.0)CALL INPRDR(I+1,CMIN,0.0)
                           IFAIL2=0
                           IDTYPE=+1
                      ELSE
                           CALL INPCHK(I+1,2,IFAIL1)
                           IF(IFAIL1.EQ.0)CALL INPRDR(I+1,CMIN,0.0)
                           CALL INPCHK(I+2,2,IFAIL2)
                           IF(IFAIL2.EQ.0)CALL INPRDR(I+2,CMAX,0.0)
                           IDTYPE=0
                      ENDIF
                      IF(IDTYPE.EQ.0.AND.CMIN.EQ.CMAX)THEN
                           CALL INPMSG(I+1,
     -                          'Zero range not permitted.     ')
                           CALL INPMSG(I+2,
     -                          'See the preceding message.    ')
                      ENDIF
                      IF(INPCMP(I,'X-#RANGE').NE.0)THEN
                           DIR='X'
                      ELSEIF(INPCMP(I,'Y-#RANGE').NE.0)THEN
                           DIR='Y'
                      ENDIF
                      INEXT=I+3
                 ENDIF
*   The dielectric constant.
            ELSEIF(INPCMP(I,'EPS#ILON').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).LE.0)THEN
                      CALL INPMSG(I,'Epsilon must be specified.    ')
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL3)
                      CALL INPRDR(I+1,EPSR,-1.0)
                      INEXT=I+2
                      IF(EPSR.LE.0.0)CALL INPMSG(I,
     -                     'Epsilon must be positive.     ')
                 ENDIF
*   Anything else: not valid.
            ELSE
                 CALL INPMSG(I,'Unrecognised keyword.         ')
            ENDIF
19          CONTINUE
            CALL INPERR
*   Store the dielectricum.
            IF(DIR.EQ.' '.OR.IDTYPE.EQ.2.OR.(IDTYPE.EQ.0.AND.
     -           CMIN.EQ.CMAX).OR.EPSR.LE.0.0.OR.
     -           IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0)THEN
                 PRINT *,' !!!!!! CELINP WARNING : The DIELECTRICUM'//
     -                ' statement is either invalid or incomplete:'
                 PRINT *,'                         It is skipped,'//
     -                ' check the documentation for proper syntax.'
            ELSEIF((NXMATT.GE.MXMATT.AND.DIR.EQ.'X').OR.
     -           (NYMATT.GE.MXMATT.AND.DIR.EQ.'Y'))THEN
                 PRINT *,' !!!!!! CELINP WARNING : No room to store'//
     -                ' further dielectrica ; increase MXMATT.'
            ELSEIF(DIR.EQ.'X')THEN
                 NXMATT=NXMATT+1
                 IF(IDTYPE.EQ.+1)THEN
                      XMATT(NXMATT,1)=CMIN
                      XMATT(NXMATT,2)=0.0
                      XMATT(NXMATT,3)=0
                      XMATT(NXMATT,4)=1
                 ELSEIF(IDTYPE.EQ.0)THEN
                      XMATT(NXMATT,1)=MIN(CMIN,CMAX)
                      XMATT(NXMATT,2)=MAX(CMIN,CMAX)
                      XMATT(NXMATT,3)=0
                      XMATT(NXMATT,4)=0
                 ELSEIF(IDTYPE.EQ.-1)THEN
                      XMATT(NXMATT,1)=0.0
                      XMATT(NXMATT,2)=CMAX
                      XMATT(NXMATT,3)=1
                      XMATT(NXMATT,4)=0
                 ENDIF
                 XMATT(NXMATT,5)=EPSR
            ELSEIF(DIR.EQ.'Y')THEN
                 NYMATT=NYMATT+1
                 IF(IDTYPE.EQ.+1)THEN
                      YMATT(NYMATT,1)=CMIN
                      YMATT(NYMATT,2)=0.0
                      YMATT(NYMATT,3)=0
                      YMATT(NYMATT,4)=1
                 ELSEIF(IDTYPE.EQ.0)THEN
                      YMATT(NYMATT,1)=MIN(CMIN,CMAX)
                      YMATT(NYMATT,2)=MAX(CMIN,CMAX)
                      YMATT(NYMATT,3)=0
                      YMATT(NYMATT,4)=0
                 ELSEIF(IDTYPE.EQ.-1)THEN
                      YMATT(NYMATT,1)=0.0
                      YMATT(NYMATT,2)=CMAX
                      YMATT(NYMATT,3)=1
                      YMATT(NYMATT,4)=0
                 ENDIF
                 YMATT(NYMATT,5)=EPSR
            ENDIF
*** Read a field map.
       ELSEIF(INPCMP(1,'FIELD-MAP')+
     -      INPCMP(1,'READ-FIELD-MAP').NE.0)THEN
*   Obtain the field map for main-field use.
            CALL BOOK('INQUIRE','MAP',USER,IFAIL1)
            IF(NWORD.EQ.1)THEN
                 IFAIL1=0
            ELSEIF(USER.EQ.'OPTIMISE')THEN
                 PRINT *,' ------ CELINP MESSAGE : Deleting the'//
     -                ' background field map to make space for the'//
     -                ' main field map.'
                 CALL MAPINT
                 CALL BOOK('RELEASE','MAP','OPTIMISE',IFAIL2)
                 IF(LBGFMP)THEN
                      PRINT *,' ------ CELINP MESSAGE : Background'//
     -                     ' field deleted because of dependence on'//
     -                     ' the field map.'
                      IF(IENBGF.NE.0)CALL ALGCLR(IENBGF)
                      IENBGF=0
                 ENDIF
                 CALL BOOK('BOOK','MAP','CELL',IFAIL3)
                 IF(IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                      IFAIL1=0
                 ELSE
                      PRINT *,' !!!!!! CELINP WARNING : Change of'//
     -                     ' field map allocation failed; field'//
     -                     ' map not useable.'
                      IFAIL1=1
                 ENDIF
            ELSEIF(USER.EQ.' ')THEN
                 CALL BOOK('BOOK','MAP','CELL',IFAIL1)
            ELSE
                 IFAIL1=0
            ENDIF
            IF(IFAIL1.NE.0)PRINT *,' !!!!!! CELINP WARNING : Unable'//
     -            ' to obtain control of the field map for use as'//
     -            ' main field.'
*   Read the field map.
            IF(IFAIL1.EQ.0)THEN
                 IF(INPCMP(1,'FIELD-MAP').NE.0)THEN
                      CALL MAPREA(IFAIL1)
                 ELSE
                      CALL MAPFMF(IFAIL1)
                 ENDIF
            ENDIF
*   Check the error flag from map reading.
            IF(IFAIL1.EQ.0.AND.NWORD.GT.1)THEN
                 IF(CPOLAR)THEN
                      PRINT *,' !!!!!! CELINP WARNING : Description'//
     -                     ' started in polar coordinates; field'//
     -                     ' map ignored.'
                      GOTO 10
                 ENDIF
                 IF(.NOT.CTUBE)CCART=.TRUE.
                 IF(NWIRE.NE.0)PRINT *,' ------ CELINP MESSAGE :'//
     -                ' Deleted the wires when reading field map.'
                 IF(NX.NE.0.OR.NY.NE.2)PRINT *,' ------ CELINP'//
     -                ' MESSAGE : Deleted the planes when reading'//
     -                ' the field map.'
                 IF(NXMATT.NE.0.OR.NYMATT.NE.0)
     -                PRINT *,' ------ CELINP MESSAGE : Deleted'//
     -                ' the dielectrics when reading the field map.'
                 NWIRE=0
                 NX=0
                 NY=2
                 YNPLAN(1)=.FALSE.
                 YNPLAN(2)=.FALSE.
                 YNPLAN(3)=.FALSE.
                 YNPLAN(4)=.FALSE.
                 NXMATT=0
                 NYMATT=0
            ELSEIF(NWORD.GT.1)THEN
                 PRINT *,' !!!!!! CELINP WARNING : Reading a field'//
     -                ' map failed.'
            ENDIF
*** Write the field map in binary format.
       ELSEIF(INPCMP(1,'SAVE-F#IELD-#MAP').NE.0)THEN
            CALL MAPFMS
*** Read the cell from dataset, if GET is a keyword
       ELSEIF(INPCMP(1,'G#ET').NE.0)THEN
            CALL CELGET(IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! CELINP WARNING : New cell data must'//
     -                ' be entered.'
                 IGET=0
                 NX=0
                 NY=2
                 CCART=.FALSE.
                 CPOLAR=.FALSE.
                 CTUBE=.FALSE.
                 CALL CELINT
                 NSOLID=0
                 ICCURR=0
                 CALL MAPINT
                 IFAIL=0
            ELSE
                 IGET=1
                 IF(POLAR)THEN
                      CPOLAR=.TRUE.
                      CCART=.FALSE.
                      CTUBE=.FALSE.
                 ELSEIF(TUBE)THEN
                      CPOLAR=.FALSE.
                      CCART=.FALSE.
                      CTUBE=.TRUE.
                 ELSE
                      CPOLAR=.FALSE.
                      CCART=.TRUE.
                      CTUBE=.FALSE.
                 ENDIF
            ENDIF
*** Gravity orientation.
       ELSEIF(INPCMP(1,'GRAV#ITY').NE.0)THEN
            IF(NWORD.EQ.1)THEN
                 WRITE(LUNOUT,'(''  Gravity works along the axis ('',
     -                F6.3,'','',F6.3,'','',F6.3,'') [g].'')')
     -                (DOWN(I),I=1,3)
            ELSEIF(NWORD.NE.4)THEN
                 PRINT *,' !!!!!! CELINP WARNING : The GRAVITY'//
     -                ' command takes 3 arguments; ignored.'
            ELSE
                 CALL INPCHK(2,2,IFAIL1)
                 CALL INPCHK(3,2,IFAIL2)
                 CALL INPCHK(4,2,IFAIL3)
                 CALL INPRDR(2,DOWNXR,DOWN(1))
                 CALL INPRDR(3,DOWNYR,DOWN(2))
                 CALL INPRDR(4,DOWNZR,DOWN(3))
                 DNORM=SQRT(DOWNXR**2+DOWNYR**2+DOWNZR**2)
                 IF(DNORM.GT.0)THEN
                      DOWN(1)=DOWNXR/DNORM
                      DOWN(2)=DOWNYR/DNORM
                      DOWN(3)=DOWNZR/DNORM
                 ELSE
                      PRINT *,' !!!!!! CELINP WARNING : The gravity'//
     -                     ' vector has 0 norm ; ignored.'
                 ENDIF
            ENDIF
*** neBEM parameters.
       ELSEIF(INPCMP(1,'NEBEM-#PARAMETERS').NE.0)THEN
            CALL CELBEM
*** If OPTION is a keyword, find out what the options are,
       ELSEIF(INPCMP(1,'OPT#IONS').NE.0)THEN
            IF(NWORD.EQ.1)WRITE(LUNOUT,'(/
     -           ''  LOCAL OPTIONS CURRENTLY IN EFFECT:''//
     -           ''  Plot the layout of the cell (LAYOUT):     '',L1/
     -           ''  Print a cell summary table (CELL-PRINT):  '',L1/
     -           ''  Plot wires as markers (WIRE-MARKERS):     '',L1/
     -           ''  Layout plotted isometrically (ISOMETRIC): '',L1/
     -           ''  Include dipole terms (DIPOLE-TERMS):      '',L1/
     -           ''  Check charge calculation (CHARGE-CHECK):  '',L1)')
     -           LCELPL,LCELPR,LWRMRK,LISOCL,LDIPOL,LCHGCH
            DO 11 I=2,NWORD
*   check the plotting-of-layout option,
            IF(INPCMP(I,'NOLAY#OUT')+
     -           INPCMP(I,'NOC#ELL-PL#OT').NE.0)THEN
                 LCELPL=.FALSE.
            ELSEIF(INPCMP(I,'LAY#OUT')+
     -           INPCMP(I,'C#ELL-PL#OT').NE.0)THEN
                 LCELPL=.TRUE.
*   check the printing-of-layout option,
            ELSEIF(INPCMP(I,'NOC#ELL-PR#INT').NE.0)THEN
                 LCELPR=.FALSE.
            ELSEIF(INPCMP(I,'C#ELL-PR#INT').NE.0)THEN
                 LCELPR=.TRUE.
*   check the wire markers option,
            ELSEIF(INPCMP(I,'NOW#IRE-M#ARKERS').NE.0)THEN
                 LWRMRK=.FALSE.
            ELSEIF(INPCMP(I,'W#IRE-M#ARKERS').NE.0)THEN
                 LWRMRK=.TRUE.
*   check the isometric option,
            ELSEIF(INPCMP(I,'NOTISO#METRIC').NE.0)THEN
                 LISOCL=.FALSE.
            ELSEIF(INPCMP(I,'ISO#METRIC').NE.0)THEN
                 LISOCL=.TRUE.
*   check the dipole option,
            ELSEIF(INPCMP(I,'NODIP#OLE-#TERMS').NE.0)THEN
                 LDIPOL=.FALSE.
            ELSEIF(INPCMP(I,'DIP#OLE-#TERMS').NE.0)THEN
                 LDIPOL=.TRUE.
*   check charge calculation.
            ELSEIF(INPCMP(I,'NOCH#ARGE-#CHECK').NE.0)THEN
                 LCHGCH=.FALSE.
            ELSEIF(INPCMP(I,'CH#ARGE-#CHECK').NE.0)THEN
                 LCHGCH=.TRUE.
*   option not known.
            ELSE
                 CALL INPMSG(I,'The option is not known.      ')
            ENDIF
11          CONTINUE
            CALL INPERR
*** If PERIOD is a keyword:
       ELSEIF(INPCMP(1,'PER#IODICITY').NE.0)THEN
*   check the syntax,
            IF(NWORD.NE.3.AND.NWORD.NE.4)THEN
                 PRINT *,' !!!!!! CELINP WARNING : PERIODICITY takes'//
     -                ' 2 or 3 arguments (direction, length, type);'//
     -                ' statement is ignored.'
                 GOTO 10
            ENDIF
*   Try to read the periodicity.
            CALL INPCHK(3,2,IFAIL1)
            CALL INPRDR(3,S,-1.0)
*   Check that the periodicity direction makes sense.
            IF(INPCMP(2,'X')+INPCMP(2,'Y')+INPCMP(2,'Z')+
     -           INPCMP(2,'PHI').EQ.0)
     -           CALL INPMSG(2,'Should be X, Y, Z or PHI. ')
            IF(S.LE.0.0)CALL INPMSG(3,'Periods should be > than 0.   ')
            IPER=0
            IF(NWORD.GE.4)THEN
                 IF(INPCMP(4,'REG#ULAR').NE.0)THEN
                      IPER=0
                 ELSEIF(INPCMP(4,'MIR#ROR').NE.0)THEN
                      IPER=1
                 ELSE
                      CALL INPMSG(3,'Unknown periodicity type.     ')
                 ENDIF
            ENDIF
*   Dump error messages.
            CALL INPERR
*   No further processing in case of invalid periodicities.
            IF((INPCMP(2,'X')+INPCMP(2,'Y')+INPCMP(2,'Z')+
     -          INPCMP(2,'PHI').EQ.0).OR.IFAIL1.NE.0.OR.S.LE.0.0)THEN
                 PRINT *,' !!!!!! CELINP WARNING : PERIOD statement'//
     -                ' ignored because of syntax or argument errors.'
*   Make sure no mixed coordinates are used.
            ELSEIF((INPCMP(2,'X')+INPCMP(2,'Y').NE.0.AND.
     -           (CTUBE.OR.CPOLAR)).OR.
     -           (INPCMP(2,'PHI')+INPCMP(2,'R').NE.0.AND.CCART))THEN
                 PRINT *,' !!!!!! CELINP WARNING : Use of mixed'//
     -                ' coordinates not permitted ; PERIOD is ignored.'
*   Assign the periodicity to SX or to SY.
            ELSE
*   If it is a x or a r periodicity:
                 IF(INPCMP(2,'X')+INPCMP(2,'R').NE.0)THEN
                      IF(IPER.EQ.0)THEN
                           IF(PERX)PRINT *,' !!!!!! CELINP WARNING :'//
     -                          ' The previous x period (',SX,
     -                          ') is replaced.'
                           IF(PERMX)PRINT *,' !!!!!! CELINP WARNING :'//
     -                          ' Mirror periodicity in x cancelled.'
                           PERX=.TRUE.
                           PERMX=.FALSE.
                      ELSE
                           IF(PERMX)PRINT *,' !!!!!! CELINP WARNING :'//
     -                          ' The previous x mirror period (',SX,
     -                          ') is replaced.'
                           IF(PERX)PRINT *,' !!!!!! CELINP WARNING :'//
     -                          ' Regular periodicity in x cancelled.'
                           PERMX=.TRUE.
                           PERX=.FALSE.
                      ENDIF
                      IF(INPCMP(2,'X').NE.0)THEN
                           SX=S
                           CCART=.TRUE.
                      ELSE
                           SX=LOG(S)
                           IF(.NOT.(CTUBE.OR.CPOLAR))CPOLAR=.TRUE.
                      ENDIF
*   if it is a y or a phi periodicity:
                 ELSEIF(INPCMP(2,'Y')+INPCMP(2,'PHI').NE.0)THEN
                      IF(INPCMP(2,'PHI').NE.0.AND.
     -                     ABS(360.0-S*ANINT(360.0/S)).GT.1.0E-4)THEN
                           PRINT *,' !!!!!! CELINP WARNING : Phi'//
     -                          ' periods must divide 360 ; ignored.'
                      ELSE
                           IF(IPER.EQ.0)THEN
                                IF(PERY.AND.CCART)PRINT *,' !!!!!!'//
     -                               ' CELINP WARNING : The previous'//
     -                               ' y period (',SY,') is replaced.'
                                IF(PERMY)PRINT *,' !!!!!! CELINP'//
     -                               ' WARNING : Mirror periodicity'//
     -                               ' in y is cancelled.'
                                IF(PERY.AND.CPOLAR)PRINT *,' !!!!!!'//
     -                               ' CELINP WARNING : The previous'//
     -                               ' phi period (',SY*180.0/PI,
     -                               ') is replaced.'
                                PERY=.TRUE.
                                PERMY=.FALSE.
                           ELSE
                                IF(PERMY.AND.CCART)PRINT *,' !!!!!!'//
     -                               ' CELINP WARNING : The previous'//
     -                               ' y mirror period (',SY,
     -                               ') is replaced.'
                                IF(PERY)PRINT *,' !!!!!! CELINP'//
     -                               ' WARNING : Regular periodicity'//
     -                               ' in y is cancelled.'
                                IF(PERMY.AND.CPOLAR)PRINT *,' !!!!!!'//
     -                               ' CELINP WARNING : The previous'//
     -                               ' phi mirror period (',SY*180.0/PI,
     -                               ') is replaced.'
                                PERMY=.TRUE.
                                PERY=.FALSE.
                           ENDIF
                           IF(INPCMP(2,'Y').NE.0)THEN
                                SY=S
                                CCART=.TRUE.
                           ELSE
                                SY=PI*S/180.0
                                MTUBE=NINT(360.0/S)
                                IF(.NOT.(CTUBE.OR.CPOLAR))CPOLAR=.TRUE.
                           ENDIF
                      ENDIF
*   If it is a z periodicity:
                 ELSE
                      IF(IPER.EQ.0)THEN
                           IF(PERZ)PRINT *,' !!!!!! CELINP WARNING :'//
     -                          ' The previous z period (',SZ,
     -                          ') is replaced.'
                           IF(PERMZ)PRINT *,' !!!!!! CELINP WARNING :'//
     -                          ' Mirror periodicity in z cancelled.'
                           PERZ=.TRUE.
                           PERMZ=.FALSE.
                      ELSE
                           IF(PERMZ)PRINT *,' !!!!!! CELINP WARNING :'//
     -                          ' The previous z mirror period (',SZ,
     -                          ') is replaced.'
                           IF(PERZ)PRINT *,' !!!!!! CELINP WARNING :'//
     -                          ' Regular periodicity in z cancelled.'
                           PERMZ=.TRUE.
                           PERZ=.FALSE.
                      ENDIF
                      SZ=S
                 ENDIF
*   reset the get condition.
                 IGET=0
            ENDIF
*** Define a plane if PLANE is a keyword.
       ELSEIF(INPCMP(1,'PL#ANE').NE.0)THEN
**  Ensure that planes are not entered in a tube geometry.
            IF(CTUBE)THEN
                 PRINT *,' !!!!!! CELINP WARNING : Planes can not'//
     -                ' be used with a TUBE; plane ignored.'
                 OK=.FALSE.
                 GOTO 10
            ENDIF
**  Determine the direction of the plane.
            DIR=' '
            INEXT=2
            DO 100 I=2,NWORD
            IF(I.LT.INEXT)GOTO 100
            IF(INPCMP(I,'X')+INPCMP(I,'R').NE.0)THEN
                 IF(INPCMP(I,'X').NE.0)DIR='X'
                 IF(INPCMP(I,'R').NE.0)DIR='R'
                 INEXT=I+2
            ELSEIF(INPCMP(I,'Y')+INPCMP(I,'PHI').NE.0)THEN
                 IF(INPCMP(I,'Y').NE.0)DIR='Y'
                 IF(INPCMP(I,'PHI').NE.0)DIR='P'
                 INEXT=I+2
            ELSEIF(INPCMP(I,'V#OLTAGE')+INPCMP(I,'LAB#EL')+
     -           INPCMP(I,'GAP').NE.0)THEN
                 INEXT=I+2
            ELSEIF(INPCMP(I,'X-STRIP')+INPCMP(I,'Y-STRIP')+
     -           INPCMP(I,'R-STRIP')+INPCMP(I,'PHI-STRIP')+
     -           INPCMP(I,'Z-STRIP').NE.0)THEN
                 INEXT=I+3
            ENDIF
100         CONTINUE
**  Make sure a direction is indicated.
            IF(DIR.EQ.' ')THEN
                 PRINT *,' !!!!!! CELINP WARNING : Direction of the'//
     -                ' plane not indicated; plane ignored.'
                 OK=.FALSE.
                 GOTO 10
*   Make sure we can store this plane.
            ELSEIF((DIR.EQ.'X'.OR.DIR.EQ.'R').AND.NX.GE.2)THEN
                 PRINT *,' !!!!!! CELINP WARNING : At most 2 planes'//
     -                ' at constant x or r permitted; plane ignored.'
                 OK=.FALSE.
                 GOTO 10
            ELSEIF((DIR.EQ.'Y'.OR.DIR.EQ.'P').AND.NY.GE.4)THEN
                 PRINT *,' !!!!!! CELINP WARNING : At most 2 planes'//
     -                ' at constant y or phi permitted; plane ignored.'
                 OK=.FALSE.
                 GOTO 10
*   Make sure no mixed coordinates are used.
            ELSEIF(((DIR.EQ.'X'.OR.DIR.EQ.'Y').AND.CPOLAR).OR.
     -           ((DIR.EQ.'R'.OR.DIR.EQ.'P').AND.CCART))THEN
                 PRINT *,' !!!!!! CELINP WARNING : Use of mixed'//
     -                ' coordinates not permitted ; plane ignored.'
                 OK=.FALSE.
                 GOTO 10
            ENDIF
**  We now start modifying the cell.
            IGET=0
**  Maintain a flag to be able to delete faulty planes.
            DELETE=.FALSE.
*   Set coordinate system flags.
            IF(DIR.EQ.'X'.OR.DIR.EQ.'R')THEN
                 NX=NX+1
                 YNPLAN(NX)=.TRUE.
                 NPSTR1(NX)=0
                 NPSTR2(NX)=0
            ELSEIF(DIR.EQ.'Y'.OR.DIR.EQ.'P')THEN
                 NY=NY+1
                 YNPLAN(NY)=.TRUE.
                 NPSTR1(NY)=0
                 NPSTR2(NY)=0
            ENDIF
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ CELINP DEBUG   :'',
     -           '' Plane direction: '',A,'' NX, NY='',2I2)') DIR,
     -           NX,NY
**  Read command in detail.
            INEXT=2
            PLATPR='?'
            COOR=0.0
            VOLT=0.0
            DO 110 I=2,NWORD
            IF(I.LT.INEXT)GOTO 110
**  Plane at constant x or constant r, accept at most 2 planes.
            IF(INPCMP(I,'X')+INPCMP(I,'R').NE.0)THEN
*   Read coordinate.
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,COOR,0.0)
*   Reject syntax errors.
                 IF(IFAIL1.NE.0)THEN
                      DELETE=.TRUE.
                      OK=.FALSE.
*   Make sure a radial plane has r>0.
                 ELSEIF(COOR.LE.0.AND.DIR.EQ.'R')THEN
                      PRINT *,' !!!!!! CELINP WARNING : The radius of'//
     -                     ' constant r planes must be larger than'//
     -                     ' zero; plane ignored.'
                      DELETE=.TRUE.
                      OK=.FALSE.
                 ENDIF
*   Next word.
                 INEXT=I+2
**  Plane at constant y or constant phi, accept at most 2 planes.
            ELSEIF(INPCMP(I,'Y')+INPCMP(I,'PHI').NE.0)THEN
*   Read coordinate.
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,COOR,0.0)
*   Reject syntax errors.
                 IF(IFAIL1.NE.0)THEN
                      OK=.FALSE.
                      DELETE=.TRUE.
                 ENDIF
*   Next word.
                 INEXT=I+2
**  Voltage definition,
            ELSEIF(INPCMP(I,'V#OLTAGE').NE.0)THEN
*   Read voltage.
                 CALL INPCHK(I+1,2,IFAIL2)
                 CALL INPRDR(I+1,VOLT,0.0)
*   Reject syntax errors.
                 IF(IFAIL2.NE.0)THEN
                      OK=.FALSE.
                      DELETE=.TRUE.
                 ENDIF
*   Next word.
                 INEXT=I+2
**  Global plane label.
            ELSEIF(INPCMP(I,'LAB#EL').NE.0)THEN
*   Read label.
                 CALL INPSTR(I+1,I+1,STRING,NC)
                 PLATPR=STRING(1:1)
*   Reject syntax errors.
                 IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',PLATPR).EQ.
     -                0)THEN
                      CALL INPMSG(I+1,'The label must be a letter.')
                      OK=.FALSE.
                      DELETE=.TRUE.
                 ENDIF
*   Next word.
                 INEXT=I+2
**  Strips.
            ELSEIF(INPCMP(I,'X-STRIP')+INPCMP(I,'Y-STRIP')+
     -           INPCMP(I,'R-STRIP')+INPCMP(I,'PHI-STRIP')+
     -           INPCMP(I,'Z-STRIP').NE.0)THEN
*   Ensure there is no coordinate system conflict.
                 IF((INPCMP(I,'X-STRIP')+INPCMP(I,'Y-STRIP').NE.0.AND.
     -                (DIR.EQ.'R'.OR.DIR.EQ.'P')).OR.
     -                (INPCMP(I,'R-STRIP')+
     -                INPCMP(I,'PHI-STRIP').NE.0.AND.
     -                (DIR.EQ.'X'.OR.DIR.EQ.'Y')))THEN
                      PRINT *,' !!!!!! CELINP WARNING : Use of mixed'//
     -                     ' coordinates not permitted ; strip ignored.'
                      OK=.FALSE.
                      DELETE=.TRUE.
                 ENDIF
*   Initial values.
                 SMIN=0.0
                 SMAX=0.0
                 GAP=-1.0
                 STRTPR='?'
*   Read range.
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,SMIN,0.0)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+2,SMAX,0.0)
*   Coordinate transformations for polar coordinates.
                 IF(INPCMP(I,'PHI-STRIP').NE.0)THEN
                      SMIN=PI*SMIN/180
                      SMAX=PI*SMAX/180
                      SMIN=MOD(SMIN,2*PI)
                      IF(SMIN.GT.PI)SMIN=SMIN-2*PI
                      IF(SMIN.LT.-PI)SMIN=SMIN+2*PI
                      SMAX=MOD(SMAX,2*PI)
                      IF(SMAX.GT.PI)SMAX=SMAX-2*PI
                      IF(SMAX.LT.-PI)SMAX=SMAX+2*PI
                 ELSEIF(INPCMP(I,'R-STRIP').NE.0)THEN
                      IF(SMIN.LE.0.OR.SMAX.LE.0)THEN
                           CALL INPMSG(I+1,'Strip must be in r > 0.')
                           CALL INPMSG(I+2,'Strip must be in r > 0.')
                           OK=.FALSE.
                           DELETE=.TRUE.
                           SMIN=1
                           SMAX=2
                      ELSE
                           SMIN=LOG(SMIN)
                           SMAX=LOG(SMAX)
                      ENDIF
                 ENDIF
*   Order the coordinates if required.
                 IF(ABS(SMIN-SMAX).LT.1E-4)THEN
                      CALL INPMSG(I+1,'Zero range not permitted.')
                      CALL INPMSG(I+2,'Zero range not permitted.')
                      OK=.FALSE.
                      DELETE=.TRUE.
                      SMIN=1
                      SMAX=1
                 ELSEIF(SMIN.GT.SMAX)THEN
                      SAUX=SMIN
                      SMIN=SMAX
                      SMAX=SAUX
                 ENDIF
*   Make sure strips and plane are perpendicular.
                 IF((DIR.EQ.'X'.AND.INPCMP(I,'X-STRIP').NE.0).OR.
     -                (DIR.EQ.'R'.AND.INPCMP(I,'R-STRIP').NE.0).OR.
     -                (DIR.EQ.'Y'.AND.INPCMP(I,'Y-STRIP').NE.0).OR.
     -                (DIR.EQ.'P'.AND.INPCMP(I,'PHI-STRIP').NE.0))THEN
                      CALL INPMSG(I,'Same direction strip and plane')
                      OK=.FALSE.
                      DELETE=.TRUE.
*   Reject syntax errors.
                 ELSEIF(IFAIL1.NE.0.OR.IFAIL2.NE.0)THEN
                      OK=.FALSE.
                      DELETE=.TRUE.
                 ENDIF
*   Next word.
                 INEXT=I+3
**  Search for optional arguments, first initialise them.
                 DO 120 J=I+3,NWORD
                 IF(J.LT.INEXT)GOTO 120
*   Gap width.
                 IF(INPCMP(J,'GAP').NE.0)THEN
                      CALL INPCHK(J+1,2,IFAIL2)
                      CALL INPRDR(J+1,GAP,0.0)
                      IF(IFAIL2.NE.0)THEN
                           OK=.FALSE.
                           DELETE=.TRUE.
                           GAP=-1.0
                      ELSEIF(GAP.LE.0)THEN
                           CALL INPMSG(J+1,'Gap must be > 0')
                           OK=.FALSE.
                           DELETE=.TRUE.
                           GAP=-1.0
                      ENDIF
                      INEXT=J+2
*   Strip label.
                 ELSEIF(INPCMP(J,'LAB#EL').NE.0)THEN
                      CALL INPSTR(J+1,J+1,STRING,NC)
                      STRTPR=STRING(1:1)
                      IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',STRTPR).EQ.
     -                     0)THEN
                           CALL INPMSG(J+1,
     -                          'The label must be a letter.')
                           OK=.FALSE.
                           DELETE=.TRUE.
                           STRTPR='?'
                      ENDIF
                      INEXT=J+2
*   Otherwise, leave the loop.
                 ELSE
                      GOTO 130
                 ENDIF
120              CONTINUE
130              CONTINUE
**  Store the strip.
                 IF(INPCMP(I,'R-STRIP')+INPCMP(I,'PHI-STRIP')+
     -                INPCMP(I,'X-STRIP')+INPCMP(I,'Y-STRIP').NE.0)THEN
                      IF(DIR.EQ.'X'.OR.DIR.EQ.'R')THEN
                           IF(NPSTR1(NX).GE.MXPSTR)THEN
                                CALL INPMSG(I,'Maximum number of'//
     -                               ' strips reached.')
                                OK=.FALSE.
                                DELETE=.TRUE.
                           ELSE
                                NPSTR1(NX)=NPSTR1(NX)+1
                                PLSTR1(NX,NPSTR1(NX),1)=SMIN
                                PLSTR1(NX,NPSTR1(NX),2)=SMAX
                                PLSTR1(NX,NPSTR1(NX),3)=GAP
                                PSLAB1(NX,NPSTR1(NX))=STRTPR
                           ENDIF
                      ELSE
                           IF(NPSTR1(NY).GE.MXPSTR)THEN
                                CALL INPMSG(I,'Maximum number of'//
     -                               ' strips reached.')
                                OK=.FALSE.
                                DELETE=.TRUE.
                           ELSE
                                NPSTR1(NY)=NPSTR1(NY)+1
                                PLSTR1(NY,NPSTR1(NY),1)=SMIN
                                PLSTR1(NY,NPSTR1(NY),2)=SMAX
                                PLSTR1(NY,NPSTR1(NY),3)=GAP
                                PSLAB1(NY,NPSTR1(NY))=STRTPR
                           ENDIF
                      ENDIF
                 ELSEIF(INPCMP(I,'Z-STRIP').NE.0)THEN
                      IF(DIR.EQ.'X'.OR.DIR.EQ.'R')THEN
                           IF(NPSTR2(NX).GE.MXPSTR)THEN
                                CALL INPMSG(I,'Maximum number of'//
     -                               ' strips reached.')
                                OK=.FALSE.
                                DELETE=.TRUE.
                           ELSE
                                NPSTR2(NX)=NPSTR2(NX)+1
                                PLSTR2(NX,NPSTR2(NX),1)=SMIN
                                PLSTR2(NX,NPSTR2(NX),2)=SMAX
                                PLSTR2(NX,NPSTR2(NX),3)=GAP
                                PSLAB2(NX,NPSTR2(NX))=STRTPR
                           ENDIF
                      ELSE
                           IF(NPSTR2(NY).GE.MXPSTR)THEN
                                CALL INPMSG(I,'Maximum number of'//
     -                               ' strips reached.')
                                OK=.FALSE.
                                DELETE=.TRUE.
                           ELSE
                                NPSTR2(NY)=NPSTR2(NY)+1
                                PLSTR2(NY,NPSTR2(NY),1)=SMIN
                                PLSTR2(NY,NPSTR2(NY),2)=SMAX
                                PLSTR2(NY,NPSTR2(NY),3)=GAP
                                PSLAB2(NY,NPSTR2(NY))=STRTPR
                           ENDIF
                      ENDIF
                 ENDIF
*   Unknown field.
            ELSE
                 CALL INPMSG(I,'Not a known parameter.        ')
                 CALL INPMSG(I+1,'See the previous message.     ')
            ENDIF
110         CONTINUE
**  Print the errors generated so far.
            CALL INPERR
*   Delete in case of errors.
            IF(DELETE)THEN
                 PRINT *,' !!!!!! CELINP WARNING : Plane ignored'//
     -                ' because of syntax or value errors.'
                 IF(DIR.EQ.'X'.OR.DIR.EQ.'R')THEN
                      YNPLAN(NX)=.FALSE.
                      NPSTR1(NX)=0
                      NPSTR2(NX)=0
                      NX=NX-1
                 ELSE
                      YNPLAN(NY)=.FALSE.
                      NPSTR1(NY)=0
                      NPSTR2(NY)=0
                      NY=NY-1
                 ENDIF
*   Skip the rest.
                 GOTO 10
            ENDIF
**  Store the data.
            IF(DIR.EQ.'Y')THEN
                 CCART=.TRUE.
                 COPLAN(NY)=COOR
                 VTPLAN(NY)=VOLT
                 PLATYP(NY)=PLATPR
            ELSEIF(DIR.EQ.'P')THEN
                 CPOLAR=.TRUE.
                 COPLAN(NY)=PI*COOR/180.0
                 VTPLAN(NY)=VOLT
                 PLATYP(NY)=PLATPR
            ELSEIF(DIR.EQ.'X')THEN
                 CCART=.TRUE.
                 COPLAN(NX)=COOR
                 VTPLAN(NX)=VOLT
                 PLATYP(NX)=PLATPR
            ELSEIF(DIR.EQ.'R')THEN
                 CPOLAR=.TRUE.
                 COPLAN(NX)=LOG(COOR)
                 VTPLAN(NX)=VOLT
                 PLATYP(NX)=PLATPR
            ELSE
                 PRINT *,' ###### CELINP ERROR   : Direction not'//
     -                ' recognised; program error - please report.'
                 OK=.FALSE.
                 GOTO 10
            ENDIF
*** Provide a reset.
       ELSEIF(INPCMP(1,'RES#ET').NE.0)THEN
            DO 12 I=2,NWORD
*   Coordinate system.
            IF(INPCMP(I,'COOR#DINATES').NE.0)THEN
                 CCART=.FALSE.
                 CPOLAR=.FALSE.
                 CTUBE=.FALSE.
*   Local variables.
            ELSEIF(INPCMP(I,'DEF#INITIONS').NE.0)THEN
                 NVAR=0
*   Dielectrica.
            ELSEIF(INPCMP(I,'DIEL#ECTRICA').NE.0)THEN
                 NXMATT=0
                 NYMATT=0
*   Field map.
            ELSEIF(INPCMP(I,'F#IELD-M#AP').NE.0)THEN
                 CALL MAPINT
*   Solids.
            ELSEIF(INPCMP(I,'SOL#IDS').NE.0)THEN
                 NSOLID=0
                 ICCURR=0
*   Periodicities.
            ELSEIF(INPCMP(I,'PER#IODICITIES').NE.0)THEN
                 PERX=.FALSE.
                 PERY=.FALSE.
                 PERZ=.FALSE.
                 PERMX=.FALSE.
                 PERMY=.FALSE.
                 PERMZ=.FALSE.
                 PERAX=.FALSE.
                 PERAY=.FALSE.
                 PERAZ=.FALSE.
                 PERRX=.FALSE.
                 PERRY=.FALSE.
                 PERRZ=.FALSE.
*   Planes.
            ELSEIF(INPCMP(I,'PL#ANES').NE.0)THEN
                 NX=0
                 NY=2
                 YNPLAN(1)=.FALSE.
                 YNPLAN(2)=.FALSE.
                 YNPLAN(3)=.FALSE.
                 YNPLAN(4)=.FALSE.
*   Tube.
            ELSEIF(INPCMP(I,'TUB#E').NE.0)THEN
                 CTUBE=.FALSE.
*   Wires.
            ELSEIF(INPCMP(I,'ROW#S')+INPCMP(I,'WIR#ES').NE.0)THEN
                 NWIRE=0
*   Something unknown.
            ELSE
                 CALL INPMSG(I,'Is not known, can not be reset')
            ENDIF
12          CONTINUE
*   Everything.
            IF(NWORD.EQ.1)THEN
                 CALL CELINT
                 NX=0
                 NY=2
                 CCART=.FALSE.
                 CPOLAR=.FALSE.
                 CTUBE=.FALSE.
                 CALL MAPINT
                 NSOLID=0
                 ICCURR=0
            ENDIF
*   Dump error messages.
            CALL INPERR
*   Reset error flag.
            IFAIL=0
*   Reset data origin flag.
            IGET=0
*** If ROW is a keyword, read the next few lines as rows.
       ELSEIF(INPCMP(1,'RO#WS')+INPCMP(1,'WIR#ES').NE.0)THEN
*   First find out whether they are in a polar or in a Cartesian system.
            IF(NWORD.EQ.1)THEN
                 IF(.NOT.(CPOLAR.OR.CCART.OR.CTUBE))CCART=.TRUE.
            ELSEIF(NWORD.EQ.2)THEN
                 IF(INPCMP(2,'CART#ESIAN').NE.0)THEN
                      IF(CPOLAR.OR.CTUBE)THEN
                           PRINT *,' !!!!!! CELINP WARNING : Mixed'//
     -                          ' coordinates not permitted ;'//
     -                          ' polar coordinates assumed.'
                      ELSE
                           CCART=.TRUE.
                      ENDIF
                 ELSEIF(INPCMP(2,'POL#AR').NE.0)THEN
                      IF(CCART)THEN
                           PRINT *,' !!!!!! CELINP WARNING : Mixed'//
     -                          ' coordinates not permitted ;'//
     -                          ' Cartesian coordinates assumed.'
                      ELSE
                           CPOLAR=.TRUE.
                           CTUBE=.FALSE.
                      ENDIF
                 ELSEIF(INPCMP(2,'TUBE').NE.0)THEN
                      IF(CCART)THEN
                           PRINT *,' !!!!!! CELINP WARNING : Mixed'//
     -                          ' coordinates not permitted ;'//
     -                          ' Cartesian coordinates assumed.'
                      ELSE
                           CTUBE=.TRUE.
                           CPOLAR=.FALSE.
                      ENDIF
                 ELSE
                      CALL INPSTR(2,2,STRING,NC)
                      PRINT *,' !!!!!! CELINP WARNING : '//STRING(1:NC)
     -                     //' is not known as a coordinate system'//
     -                     ' to ROWS ; it is ignored.'
                      IF(.NOT.(CPOLAR.OR.CCART))CCART=.TRUE.
                 ENDIF
            ELSE
                 PRINT *,' !!!!!! CELINP WARNING : ROWS has at most'//
     -                ' one argument ; arguments ignored.'
                 IF(.NOT.(CPOLAR.OR.CCART.OR.CTUBE))CCART=.TRUE.
            ENDIF
*   Add the loop variable to the list.
            IF(NVAR+1.GT.MXVAR)THEN
                 PRINT *,' !!!!!! CELINP WARNING : Variable stack'//
     -                ' exhausted, no room for a loop variable.'
            ELSE
                 NVAR=NVAR+1
                 VARLIS(NVAR)='I'
                 VAR(NVAR)=0.0
            ENDIF
*   Print a prompt for interactive mode reading of cell data
            IF(STDSTR('INPUT'))PRINT *,' ====== CELINP INPUT   :'//
     -           ' Please enter the rows, terminate with a blank line.'
            CALL INPPRM('Rows','ADD-NOPRINT')
*   Initialise number of wires.
            NWIRE=0
20          CONTINUE
*   Input a line and make some preliminary checks.
            CALL INPWRD(NWORD)
            CALL INPSTR(1,1,STRING,NC)
            IF(STRING(1:1).EQ.'&')THEN
                 PRINT *,' !!!!!! CELINP WARNING : The section can'//
     -                ' not be left at this point ; line ignored.'
                 GOTO 20
            ENDIF
            IF(NWORD.GT.9)PRINT *,' !!!!!! CELINP WARNING : At most 9'//
     -           ' items expected on a wire line ; excess is ignored.'
            IF(NWORD.EQ.0)GOTO 60
*   Read wire codes, checking that they are letters,
            WIRTPR=STRING(1:1)
            IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',WIRTPR).EQ.0)THEN
                 CALL INPMSG(1,'The wire code must be a letter')
                 IFAIL1=1
            ELSE
                 IFAIL1=0
            ENDIF
*   Read n which may be symbolic but should not contain a loop-variable.
            NFUNCT=0
            IF(NWORD.GE.2.AND.INPCMP(2,'*').EQ.0)THEN
                 CALL INPSTR(2,2,STRING,NC)
                 FUNCT(1:NC)=STRING(1:NC)
                 NFUNCT=NC
            ELSE
                 FUNCT(1:3)='1.0'
                 NFUNCT=3
            ENDIF
            CALL ALGPRE(FUNCT,NFUNCT,VARLIS,NVAR,NRES,USE,IENTRY,IFAIL4)
            IF(USE(NVAR).AND.IFAIL4.EQ.0)THEN
                 CALL INPMSG(2,'Invalid use of loop variable I')
                 IFAIL4=1
                 IFAIL5=1
                 RES(1)=0
            ELSEIF(NRES.NE.1.AND.IFAIL4.EQ.0)THEN
                 CALL INPMSG(2,'Returns more than 1 result.   ')
                 IFAIL4=1
                 IFAIL5=1
                 RES(1)=0
            ELSEIF(IFAIL4.EQ.0)THEN
                 CALL ALGEXE(IENTRY,VAR,MODVAR,NVAR,RES,MODRES,1,IFAIL5)
            ELSE
                 IFAIL5=1
            ENDIF
            NR=NINT(RES(1))
            CALL ALGCLR(IENTRY)
*   Check that the number of wires in the row is positive and integer.
            IF(NR.LE.0.AND.IFAIL4.EQ.0)THEN
                 CALL INPMSG(2,'Number of wires should be > 0.')
                 IFAIL2=1
            ELSEIF(ABS(NR-RES(1)).GT.1.0E-3.AND.IFAIL4.EQ.0)THEN
                 CALL INPMSG(2,'Does not evaluate to integer. ')
                 IFAIL2=1
            ELSE
                 IFAIL2=0
            ENDIF
*   Translate d, x, y, V, W, l, s - symbolic, loop variable permitted.
            NFUNCT=0
            DO 21 I=3,9
            IF(I.EQ.9.AND.INPCMP(I,'CU-BE#RYLLIUM')+
     -           INPCMP(I,'C#OPPER-BE#RYLLIUM')+
     -           INPCMP(I,'BE#RYLLIUM-#CU')+
     -           INPCMP(I,'BE#RYLLIUM-#COPPER').NE.0)THEN
                 FUNCT(NFUNCT+1:NFUNCT+4)=',8.7'
                 NFUNCT=NFUNCT+4
            ELSEIF(I.EQ.9.AND.INPCMP(I,'W')+
     -           INPCMP(I,'TUNG#STEN').NE.0)THEN
                 FUNCT(NFUNCT+1:NFUNCT+5)=',19.3'
                 NFUNCT=NFUNCT+5
            ELSEIF(NWORD.GE.I.AND.INPCMP(I,'*').EQ.0)THEN
                 CALL INPSTR(I,I,STRING,NC)
                 FUNCT(NFUNCT+1:NFUNCT+NC+1)=','//STRING(1:NC)
                 NFUNCT=NFUNCT+NC+1
            ELSE
                 IF(I.EQ.3)THEN
                      FUNCT(NFUNCT+1:NFUNCT+5)=',0.01'
                      NFUNCT=NFUNCT+5
                 ELSEIF(I.EQ.7)THEN
                      FUNCT(NFUNCT+1:NFUNCT+5)=',50.0'
                      NFUNCT=NFUNCT+5
                 ELSEIF(I.EQ.8)THEN
                      FUNCT(NFUNCT+1:NFUNCT+6)=',100.0'
                      NFUNCT=NFUNCT+6
                 ELSEIF(I.EQ.9)THEN
                      FUNCT(NFUNCT+1:NFUNCT+5)=',19.3'
                      NFUNCT=NFUNCT+5
                 ELSE
                      FUNCT(NFUNCT+1:NFUNCT+4)=',0.0'
                      NFUNCT=NFUNCT+4
                 ENDIF
            ENDIF
21          CONTINUE
            FUNCT(1:1)=' '
            CALL ALGPRE(FUNCT,NFUNCT,VARLIS,NVAR,NRES,USE,IENTRY,IFAIL6)
*   Dump messages and skip the row if not meaningful.
            CALL INPERR
            IF(IFAIL1+IFAIL2+IFAIL4+IFAIL5+IFAIL6.NE.0)THEN
                 PRINT *,' !!!!!! CELINP WARNING : Row skipped'//
     -                ' because of syntax or value errors.'
                 CALL ALGCLR(IENTRY)
                 GOTO 20
            ENDIF
*   Add the new wires to the list, making sure that # is not > MXWIRE.
            DO 30 J=0,NR-1
            VAR(NVAR)=J
            MODVAR(NVAR)=2
            CALL ALGEXE(IENTRY,VAR,MODVAR,NVAR,RES,MODRES,7,IFAIL7)
            DR=RES(1)
            XR=RES(2)
            YR=RES(3)
            VR=RES(4)
            WR=RES(5)
            UR=RES(6)
            SR=RES(7)
            IF(IFAIL7.NE.0)THEN
                 PRINT '(''  !!!!!! CELINP WARNING : Algebra errors;'',
     -                '' wire '',I3,'' of this row is skipped.'')',J+1
                 GOTO 30
            ELSEIF(DR.LE.0.0)THEN
                 PRINT '(''  !!!!!! CELINP WARNING : Wire '',I3,'' of'',
     -                '' this row is skipped because its diameter is'',
     -                '' not positive.'')',J+1
                 GOTO 30
            ELSEIF(WR.LE.0.0)THEN
                 PRINT '(''  !!!!!! CELINP WARNING : Wire '',I3,'' of'',
     -                '' this row is skipped because its tension is'',
     -                '' not positive.'')',J+1
                 GOTO 30
            ELSEIF(UR.LE.0.0)THEN
                 PRINT '(''  !!!!!! CELINP WARNING : Wire '',I3,'' of'',
     -                '' this row is skipped because its length is'',
     -                '' not positive.'')',J+1
                 GOTO 30
            ELSEIF(SR.LE.0.0)THEN
                 PRINT '(''  !!!!!! CELINP WARNING : Wire '',I3,'' of'',
     -                '' this row is skipped because its density is'',
     -                '' not positive.'')',J+1
                 GOTO 30
            ENDIF
            NWIRE=NWIRE+1
            IF(NWIRE.GT.MXWIRE)GOTO 30
            X(NWIRE)=XR
            Y(NWIRE)=YR
            V(NWIRE)=VR
            W(NWIRE)=WR
            U(NWIRE)=UR
            DENS(NWIRE)=SR
            D(NWIRE)=DR
            WIRTYP(NWIRE)=WIRTPR
            INDSW(NWIRE)=0
*   Convert from polar to internal coordinates if the cell is polar.
            IF(CPOLAR.AND..NOT.CTUBE)THEN
                 IF(X(NWIRE).LE.D(I))THEN
                      PRINT '(''  !!!!!! CELINP WARNING : Wire '',I3,
     -                     '' of this row is too close to the origin;'',
     -                     '' the wire is skipped.'')',J+1
                      NWIRE=NWIRE-1
                 ELSE
                      D(NWIRE)=DR/X(NWIRE)
                      CALL CFMPTR(X(NWIRE),Y(NWIRE),X(NWIRE),Y(NWIRE),1,
     -                     IFAIL1)
                 ENDIF
            ENDIF
30          CONTINUE
*   Release the algebra entry point.
            CALL ALGCLR(IENTRY)
            GOTO 20
60          CONTINUE
*   Reset the prompt.
            CALL INPPRM(' ','BACK-PRINT')
*   Reset the loop variable.
            NVAR=NVAR-1
*   Warn if no wires are found.
            IF(NWIRE.EQ.0)THEN
                 PRINT *,' !!!!!! CELINP WARNING : No rows found'//
     -                ' after the instruction ROW.'
*   Warn if NWIRE > MXWIRE.
            ELSEIF(NWIRE.GT.MXWIRE)THEN
                 PRINT *,' ###### CELINP ERROR   : The number of wires'
     -                //' found in the input is larger than MXWIRE'//
     -                ' for the present compilation'
                 PRINT *,'                         a correct value may'
     -                //' be obtained by inserting the following'//
     -                ' cards in the Patchy cradle before +PAM.'
                 PRINT *,' '
                 PRINT *,'+REP,P=COMMONS,C=<look at the listing>.'
                 PRINT *,'       PARAMETER(MXWIRE=',NWIRE,
     -                ', MXSW=',MXSW,')'
                 PRINT *,' '
                 NWIRE=MXWIRE
                 IFAIL=1
            ENDIF
*   Proceed with next input line in this section (reset GET condition).
            IGET=0
*** Listing of solids.
       ELSEIF(INPCMP(1,'SOL#IDS').NE.0)THEN
            CALL CELSOL
*** TUBE statement.
       ELSEIF(INPCMP(1,'TUBE').NE.0)THEN
*   Can not be handled if the cell has started to be Cartesian.
            IF(CCART)THEN
                 PRINT *,' !!!!!! CELINP WARNING : Cell description'//
     -                ' started in Cartesian coordinates; tube ignored.'
                 GOTO 10
            ENDIF
*   Check for the presence of planes.
            IF(NY.NE.2.OR.NX.NE.0)THEN
                 PRINT *,' !!!!!! CELINP WARNING : You have already'//
     -                ' defined one or more planes; they are deleted.'
                 NX=0
                 NY=2
            ENDIF
*   Reset the origin flag.
            IGET=0
*   Check the input syntax and extract the parameters.
            RADIUS=0
            NTUBER=0
            VOLT=0.0
            PLATPR='?'
            DELETE=.FALSE.
*   Preset the tube data.
            COTUBE=1
            VTTUBE=0
            NTUBE=0
            MTUBE=0
            NPSTR1(5)=0
            NPSTR2(5)=0
            PLATYP(5)='?'
*   Read the command line.
            INEXT=2
            DO 40 I=2,NWORD
            IF(I.LT.INEXT)GOTO 40
*   Look for the radius.
            IF(INPCMP(I,'R#ADIUS').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,RADIUS,0.0)
                 IF(IFAIL1.NE.0)THEN
                      DELETE=.TRUE.
                      OK=.FALSE.
                 ELSEIF(RADIUS.LE.0.0)THEN
                      CALL INPMSG(I,'Tube radius must be > 0.')
                      CALL INPMSG(I+1,'See the previous message.')
                      DELETE=.TRUE.
                      OK=.FALSE.
                 ELSE
                      COTUBE=RADIUS
                 ENDIF
                 INEXT=I+2
*   Voltage definition,
            ELSEIF(INPCMP(I,'V#OLTAGE').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,VOLT,0.0)
                 IF(IFAIL1.NE.0)THEN
                      DELETE=.TRUE.
                      OK=.FALSE.
                 ELSE
                      VTTUBE=VOLT
                 ENDIF
                 INEXT=I+2
*   Number of edges.
            ELSEIF(INPCMP(I,'E#DGES').NE.0)THEN
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NTUBER,0)
                 IF(IFAIL1.NE.0)THEN
                      DELETE=.TRUE.
                      OK=.FALSE.
                 ELSEIF((NTUBER.NE.0.AND.NTUBER.LT.3).OR.
     -                NTUBER.GT.8)THEN
                      CALL INPMSG(I+1,'Number of edges not valid.    ')
                      DELETE=.TRUE.
                      OK=.FALSE.
                 ELSE
                      NTUBE=NTUBER
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'CIRC#LE')+
     -           INPCMP(I,'CIRC#ULAR')+
     -           INPCMP(I,'CYL#INDER')+
     -           INPCMP(I,'CYL#INDRICAL').NE.0)THEN
                 NTUBE=0
            ELSEIF(INPCMP(I,'TRI#ANGLE')+
     -           INPCMP(I,'TRI#ANGULAR').NE.0)THEN
                 NTUBE=3
            ELSEIF(INPCMP(I,'SQU#ARE').NE.0)THEN
                 NTUBE=4
            ELSEIF(INPCMP(I,'PENT#AGONAL').NE.0)THEN
                 NTUBE=5
            ELSEIF(INPCMP(I,'HEX#AGONAL').NE.0)THEN
                 NTUBE=6
            ELSEIF(INPCMP(I,'HEPT#AGONAL').NE.0)THEN
                 NTUBE=7
            ELSEIF(INPCMP(I,'OCT#AGONAL').NE.0)THEN
                 NTUBE=8
*   Label.
            ELSEIF(INPCMP(I,'LAB#EL').NE.0)THEN
                 CALL INPSTR(I+1,I+1,STRING,NC)
                 PLATPR=STRING(1:1)
                 IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',PLATPR).EQ.
     -                0)THEN
                      CALL INPMSG(I+1,'The label must be a letter.')
                      DELETE=.TRUE.
                      OK=.FALSE.
                 ELSE
                      PLATYP(5)=PLATPR
                 ENDIF
                 INEXT=I+2
**  Strips.
            ELSEIF(INPCMP(I,'PHI-STRIP')+INPCMP(I,'Z-STRIP').NE.0)THEN
*   Initial values.
                 SMIN=0.0
                 SMAX=0.0
                 GAP=-1.0
                 STRTPR='?'
*   Read range.
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,SMIN,0.0)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+2,SMAX,0.0)
*   Reject syntax errors.
                 IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)THEN
                      OK=.FALSE.
                      DELETE=.TRUE.
                 ENDIF
*   Coordinate transformations for polar coordinates.
                 IF(INPCMP(I,'PHI-STRIP').NE.0)THEN
                      SMIN=PI*SMIN/180
                      SMAX=PI*SMAX/180
                      SMIN=MOD(SMIN,2*PI)
                      IF(SMIN.GT.PI)SMIN=SMIN-2*PI
                      IF(SMIN.LT.-PI)SMIN=SMIN+2*PI
                      SMAX=MOD(SMAX,2*PI)
                      IF(SMAX.GT.PI)SMAX=SMAX-2*PI
                      IF(SMAX.LT.-PI)SMAX=SMAX+2*PI
                 ENDIF
*   Order the coordinates if required.
                 IF(ABS(SMIN-SMAX).LT.1E-4)THEN
                      CALL INPMSG(I+1,'Zero range not permitted.')
                      CALL INPMSG(I+2,'Zero range not permitted.')
                      OK=.FALSE.
                      DELETE=.TRUE.
                      SMIN=1
                      SMAX=1
                 ELSEIF(SMIN.GT.SMAX)THEN
                      SAUX=SMIN
                      SMIN=SMAX
                      SMAX=SAUX
                 ENDIF
*   Next word.
                 INEXT=I+3
**  Search for optional arguments, first initialise them.
                 DO 140 J=I+3,NWORD
                 IF(J.LT.INEXT)GOTO 140
*   Gap width.
                 IF(INPCMP(J,'GAP').NE.0)THEN
                      CALL INPCHK(J+1,2,IFAIL2)
                      CALL INPRDR(J+1,GAP,0.0)
                      IF(IFAIL2.NE.0)THEN
                           OK=.FALSE.
                           DELETE=.TRUE.
                           GAP=-1.0
                      ELSEIF(GAP.LE.0)THEN
                           CALL INPMSG(J+1,'Gap must be > 0')
                           OK=.FALSE.
                           DELETE=.TRUE.
                           GAP=-1.0
                      ENDIF
                      INEXT=J+2
*   Strip label.
                 ELSEIF(INPCMP(J,'LAB#EL').NE.0)THEN
                      CALL INPSTR(J+1,J+1,STRING,NC)
                      STRTPR=STRING(1:1)
                      IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',STRTPR).EQ.
     -                     0)THEN
                           CALL INPMSG(J+1,
     -                          'The label must be a letter.')
                           OK=.FALSE.
                           DELETE=.TRUE.
                           STRTPR='?'
                      ENDIF
                      INEXT=J+2
*   Otherwise, leave the loop.
                 ELSE
                      GOTO 150
                 ENDIF
140              CONTINUE
150              CONTINUE
**  Store the strip.
                 IF(INPCMP(I,'PHI-STRIP').NE.0)THEN
                      IF(NPSTR1(5).GE.MXPSTR)THEN
                           CALL INPMSG(I,'Maximum number of'//
     -                          ' strips reached.')
                           OK=.FALSE.
                           DELETE=.TRUE.
                      ELSE
                           NPSTR1(5)=NPSTR1(5)+1
                           PLSTR1(5,NPSTR1(5),1)=SMIN
                           PLSTR1(5,NPSTR1(5),2)=SMAX
                           PLSTR1(5,NPSTR1(5),3)=GAP
                           PSLAB1(5,NPSTR1(5))=STRTPR
                      ENDIF
                 ELSEIF(INPCMP(I,'Z-STRIP').NE.0)THEN
                      IF(NPSTR2(5).GE.MXPSTR)THEN
                           CALL INPMSG(I,'Maximum number of'//
     -                          ' strips reached.')
                           OK=.FALSE.
                           DELETE=.TRUE.
                      ELSE
                           NPSTR2(5)=NPSTR2(5)+1
                           PLSTR2(5,NPSTR2(5),1)=SMIN
                           PLSTR2(5,NPSTR2(5),2)=SMAX
                           PLSTR2(5,NPSTR2(5),3)=GAP
                           PSLAB2(5,NPSTR2(5))=STRTPR
                      ENDIF
                 ENDIF
*   Unknown field.
            ELSE
                 CALL INPMSG(I,'Not known as a valid keyword. ')
            ENDIF
40          CONTINUE
*   Print the errors generated so far, return if errors are serious.
            CALL INPERR
*   Delete tube in case of errors.
            IF(DELETE)THEN
                 PRINT *,' !!!!!! CELINP WARNING : Tube ignored'//
     -                ' because of syntax or value errors.'
                 NPSTR1(5)=0
                 NPSTR2(5)=0
                 CTUBE=.FALSE.
                 GOTO 10
            ENDIF
*   Update the coordinate system flags.
            CCART=.FALSE.
            CTUBE=.TRUE.
            CPOLAR=.FALSE.
**  Window for cutting triangles.
       ELSEIF(INPCMP(1,'WIN#DOW').NE.0)THEN
*   Check argument types.
            IF(NWORD.NE.5.AND.NWORD.NE.7)THEN
                 PRINT *,' !!!!!! CELINP WARNING : WINDOW has'//
     -                ' either 4 or 6 arguments; ignored.'
                 WINDOW=.FALSE.
*   3-dimensional window specification.
            ELSEIF(NWORD.EQ.7)THEN
                 CALL INPCHK(2,2,IFAIL1)
                 CALL INPCHK(3,2,IFAIL2)
                 CALL INPCHK(4,2,IFAIL3)
                 CALL INPCHK(5,2,IFAIL4)
                 CALL INPCHK(6,2,IFAIL5)
                 CALL INPCHK(7,2,IFAIL6)
                 CALL INPRDR(2,WXMIN,-1.0)
                 CALL INPRDR(3,WYMIN,-1.0)
                 CALL INPRDR(4,WZMIN,-1.0)
                 CALL INPRDR(5,WXMAX,+1.0)
                 CALL INPRDR(6,WYMAX,+1.0)
                 CALL INPRDR(7,WZMAX,+1.0)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.
     -                IFAIL3.EQ.0.AND.IFAIL4.EQ.0.AND.
     -                IFAIL5.EQ.0.AND.IFAIL6.EQ.0.AND.
     -                WXMIN.NE.WXMAX.AND.WYMIN.NE.WYMAX.AND.
     -                WZMIN.NE.WZMAX)THEN
                      WINDOW=.TRUE.
                 ELSE
                      PRINT *,' !!!!!! CELINP WARNING : Not a'//
     -                     ' valid window; ignored.'
                      WINDOW=.FALSE.
                 ENDIF
*   2-dimensional window specification.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 CALL INPCHK(I+4,2,IFAIL4)
                 CALL INPRDR(I+1,WXMIN,-1.0)
                 CALL INPRDR(I+2,WYMIN,-1.0)
                 CALL INPRDR(I+3,WXMAX,+1.0)
                 CALL INPRDR(I+4,WYMAX,+1.0)
                 WZMIN=-1
                 WZMAX=+1
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.
     -                IFAIL3.EQ.0.AND.IFAIL4.EQ.0.AND.
     -                WXMIN.NE.WXMAX.AND.WYMIN.NE.WYMAX)THEN
                      WINDOW=.TRUE.
                 ELSE
                      PRINT *,' !!!!!! CELINP WARNING : Not a'//
     -                     ' valid window; ignored.'
                      WINDOW=.FALSE.
                 ENDIF
                 INEXT=I+5
            ENDIF
*   Ordering of window limits.
            IF(WXMIN.GT.WXMAX)THEN
                 AUX=WXMIN
                 WXMIN=WXMAX
                 WXMAX=AUX
            ENDIF
            IF(WYMIN.GT.WYMAX)THEN
                 AUX=WYMIN
                 WYMIN=WYMAX
                 WYMAX=AUX
            ENDIF
            IF(WZMIN.GT.WZMAX)THEN
                 AUX=WZMIN
                 WZMIN=WZMAX
                 WZMAX=AUX
            ENDIF
*   Apply the cut
            CALL MAPCUT(WXMIN,WYMIN,WZMIN,WXMAX,WYMAX,WZMAX,WINDOW)
*** Call CELWRT with the name of the data set.
       ELSEIF(INPCMP(1,'WR#ITE').NE.0)THEN
            CALL CELWRT(1)
*** It is not possible to get here if a keyword is found.
       ELSE
            CALL INPSTR(1,1,STRING,NC)
            PRINT *,' !!!!!! CELINP WARNING : '//STRING(1:NC)//' is'//
     -           ' not a valid instruction ; line is skipped.'
       ENDIF
*** Finish reading loop.
       GOTO 10
50     CONTINUE
*** Set POLAR if the cell has cylindrical symmetry.
       IF(CCART)THEN
            POLAR=.FALSE.
            TUBE=.FALSE.
*   Tubes.
       ELSEIF(CTUBE)THEN
            POLAR=.FALSE.
            TUBE=.TRUE.
*   True polar cells, set phi period if neither periodic nor planes.
       ELSEIF(CPOLAR)THEN
            POLAR=.TRUE.
            TUBE=.FALSE.
            IF(.NOT.(PERY.OR.(YNPLAN(3).AND.YNPLAN(4))))THEN
                 SY=2.0*PI
                 PERY=.TRUE.
            ENDIF
       ENDIF
*** Reset a GET flag if neBEM is to be run.
       IF(BEMSET)IGET=0
*** Register the amount of CPU time used by this routine.
       CALL TIMLOG('Reading the cell definition:            ')
*** Normaly the routine should end at this point.
       END
