CDECK  ID>, MAIN.
       PROGRAM MAIN
*-----------------------------------------------------------------------
*   MAIN   - This program reads headers from the input file and calls
*            the appropriate routines to carry out the requested action.
*   VARIABLE : STRING      : serves for identifying the header.
*   (Last changed on  3/ 6/10.)
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
       DOUBLE PRECISION CLSDIS,CLSAVE
       REAL EGAS,VGAS,XGAS,YGAS,DGAS,AGAS,BGAS,HGAS,MGAS,WGAS,OGAS,SGAS,
     -      EXGAS,IOGAS,
     -      CVGAS,CXGAS,CYGAS,CDGAS,CAGAS,CBGAS,CHGAS,CMGAS,CWGAS,COGAS,
     -      CSGAS,CEXGAS,CIOGAS,
     -      VGAS2,XGAS2,YGAS2,DGAS2,AGAS2,BGAS2,HGAS2,MGAS2,WGAS2,OGAS2,
     -      SGAS2,EXGAS2,IOGAS2,
     -      AORIG,AORIG2,PENPRB,PENRMS,PENDT,ENIOG,ENEXG,
     -      BANG,BTAB,
     -      VEXTR1,VEXTR2,VEXTR3,VEXTR4,
     -      XEXTR1,XEXTR2,XEXTR3,XEXTR4,
     -      YEXTR1,YEXTR2,YEXTR3,YEXTR4,
     -      DEXTR1,DEXTR2,DEXTR3,DEXTR4,
     -      AEXTR1,AEXTR2,AEXTR3,AEXTR4,
     -      BEXTR1,BEXTR2,BEXTR3,BEXTR4,
     -      HEXTR1,HEXTR2,HEXTR3,HEXTR4,
     -      MEXTR1,MEXTR2,MEXTR3,MEXTR4,
     -      WEXTR1,WEXTR2,WEXTR3,WEXTR4,
     -      OEXTR1,OEXTR2,OEXTR3,OEXTR4,
     -      SEXTR1,SEXTR2,SEXTR3,SEXTR4,
     -      EEXTR1,EEXTR2,EEXTR3,EEXTR4,
     -      ZEXTR1,ZEXTR2,ZEXTR3,ZEXTR4,
     -      GASRNG,
     -      Z,A,RHO,CMEAN,EMPROB,EPAIR,PGAS,TGAS,GASDEN,
     -      DTION,DLION,GASFRM,ELOSCS
       LOGICAL GASOK,TAB2D,GASOPT,HEEDOK,SRIMOK,TRIMOK,GASSET
       INTEGER NGAS,NCLS,NBANG,NBTAB,NFTAB,NFCLS,
     -      IVMETH,IXMETH,IYMETH,IDMETH,IAMETH,IBMETH,IHMETH,IMMETH,
     -      IWMETH,IOMETH,ISMETH,IEMETH,IZMETH,
     -      IVEXTR,IXEXTR,IYEXTR,IDEXTR,IAEXTR,IBEXTR,IHEXTR,IMEXTR,
     -      IWEXTR,IOEXTR,ISEXTR,IEEXTR,IZEXTR,
     -      JVEXTR,JXEXTR,JYEXTR,JDEXTR,JAEXTR,JBEXTR,JHEXTR,JMEXTR,
     -      JWEXTR,JOEXTR,JSEXTR,JEEXTR,JZEXTR,
     -      IATHR,IBTHR,IHTHR,
     -      NEXGAS,NIOGAS,NCSGAS,ICSTYP
       CHARACTER*80 GASID
       CHARACTER*(MXCHAR) FCNTAB,FCNCLS
       CHARACTER*10 CLSTYP
       CHARACTER*45 DSCEXG(MXEXG),DSCIOG(MXIOG),DSCCSG(MXCSG)
       COMMON /GASDAT/ CLSDIS(MXPAIR),CLSAVE,
     -      EGAS(MXLIST),
     -      VGAS(MXLIST),XGAS(MXLIST),YGAS(MXLIST),WGAS(MXLIST),
     -      DGAS(MXLIST),OGAS(MXLIST),AGAS(MXLIST),BGAS(MXLIST),
     -      HGAS(MXLIST),MGAS(MXLIST),SGAS(MXLIST,6),
     -      EXGAS(MXLIST,MXEXG),IOGAS(MXLIST,MXIOG),
     -      CVGAS(MXLIST),CXGAS(MXLIST),CYGAS(MXLIST),CWGAS(MXLIST),
     -      CDGAS(MXLIST),COGAS(MXLIST),CAGAS(MXLIST),CBGAS(MXLIST),
     -      CHGAS(MXLIST),CMGAS(MXLIST),CSGAS(MXLIST,6),
     -      CEXGAS(MXLIST,MXEXG),CIOGAS(MXLIST,MXIOG),
     -      VGAS2(MXLIST,MXBANG,MXBTAB),WGAS2(MXLIST,MXBANG,MXBTAB),
     -      XGAS2(MXLIST,MXBANG,MXBTAB),YGAS2(MXLIST,MXBANG,MXBTAB),
     -      AGAS2(MXLIST,MXBANG,MXBTAB),BGAS2(MXLIST,MXBANG,MXBTAB),
     -      DGAS2(MXLIST,MXBANG,MXBTAB),OGAS2(MXLIST,MXBANG,MXBTAB),
     -      HGAS2(MXLIST,MXBANG,MXBTAB),MGAS2(MXLIST,MXBANG,MXBTAB),
     -      SGAS2(MXLIST,MXBANG,MXBTAB,6),
     -      EXGAS2(MXLIST,MXBANG,MXBTAB,MXEXG),
     -      IOGAS2(MXLIST,MXBANG,MXBTAB,MXIOG),
     -      AORIG(MXLIST),AORIG2(MXLIST,MXBANG,MXBTAB),
     -      PENPRB(MXEXG),PENRMS(MXEXG),PENDT(MXEXG),
     -      ENIOG(MXIOG),ENEXG(MXEXG),
     -      BANG(MXBANG),BTAB(MXBTAB),
     -      GASRNG(20,2),GASFRM(MXNBMC),ELOSCS(MXCSG),
     -      Z,A,RHO,CMEAN,EMPROB,EPAIR,PGAS,TGAS,GASDEN,
     -      DTION,DLION,
     -      VEXTR1,VEXTR2,VEXTR3,VEXTR4,
     -      XEXTR1,XEXTR2,XEXTR3,XEXTR4,
     -      YEXTR1,YEXTR2,YEXTR3,YEXTR4,
     -      DEXTR1,DEXTR2,DEXTR3,DEXTR4,
     -      AEXTR1,AEXTR2,AEXTR3,AEXTR4,
     -      BEXTR1,BEXTR2,BEXTR3,BEXTR4,
     -      HEXTR1,HEXTR2,HEXTR3,HEXTR4,
     -      MEXTR1,MEXTR2,MEXTR3,MEXTR4,
     -      WEXTR1,WEXTR2,WEXTR3,WEXTR4,
     -      OEXTR1,OEXTR2,OEXTR3,OEXTR4,
     -      SEXTR1(6),SEXTR2(6),SEXTR3(6),SEXTR4(6),
     -      EEXTR1(MXEXG),EEXTR2(MXEXG),EEXTR3(MXEXG),EEXTR4(MXEXG),
     -      ZEXTR1(MXIOG),ZEXTR2(MXIOG),ZEXTR3(MXIOG),ZEXTR4(MXIOG),
     -      IVMETH,IXMETH,IYMETH,IDMETH,IAMETH,IBMETH,IHMETH,IMMETH,
     -      IWMETH,IOMETH,ISMETH,IEMETH,IZMETH,
     -      IVEXTR,IXEXTR,IYEXTR,IDEXTR,IAEXTR,IBEXTR,IHEXTR,IMEXTR,
     -      IWEXTR,IOEXTR,ISEXTR,IEEXTR,IZEXTR,
     -      JVEXTR,JXEXTR,JYEXTR,JDEXTR,JAEXTR,JBEXTR,JHEXTR,JMEXTR,
     -      JWEXTR,JOEXTR,JSEXTR,JEEXTR,JZEXTR,
     -      NGAS,NCLS,NBANG,NBTAB,NFTAB,NFCLS,
     -      IATHR,IBTHR,IHTHR,
     -      NEXGAS,NIOGAS,NCSGAS,ICSTYP(MXCSG),
     -      GASOK(20),GASOPT(20,4),
     -      TAB2D,HEEDOK,SRIMOK,TRIMOK,GASSET
       COMMON /GASCHR/ FCNTAB,FCNCLS,CLSTYP,GASID,DSCEXG,DSCIOG,DSCCSG
       LOGICAL MAGOK
       REAL ALFA,B0X,B0Y,B0Z,SUSWIR,SUSGAS,BSCALE,BFMIN,BFMAX,
     -      BFXMIN,BFYMIN,BFZMIN,BFXMAX,BFYMAX,BFZMAX
       INTEGER MAGSRC,
     -      IBXTYP,IBYTYP,IBZTYP,
     -      IRB0X,IRB0Y,IRB0Z,IRV0X,IRV0Y,IRV0Z,
     -      IENB0X,IENB0Y,IENB0Z,IBXDIR,IBYDIR,IBZDIR,
     -      NCB0X,NCB0Y,NCB0Z
       CHARACTER*(MXCHAR) FUNB0X,FUNB0Y,FUNB0Z
       COMMON /MAGDAT/ ALFA,SUSWIR,SUSGAS,
     -      B0X,B0Y,B0Z,BSCALE,BFMIN,BFMAX,
     -      BFXMIN,BFYMIN,BFZMIN,BFXMAX,BFYMAX,BFZMAX,
     -      MAGSRC,IBXTYP,IBYTYP,IBZTYP,
     -      IRB0X,IRB0Y,IRB0Z,IRV0X,IRV0Y,IRV0Z,
     -      IENB0X,IENB0Y,IENB0Z,IBXDIR,IBYDIR,IBZDIR,
     -      NCB0X,NCB0Y,NCB0Z,
     -      MAGOK
       COMMON /MAGCHR/ FUNB0X,FUNB0Y,FUNB0Z
       LOGICAL STDSTR
       INTEGER NC,IFAIL,NWORD,INPCMP
       CHARACTER*(MXCHAR) STRING
       EXTERNAL STDSTR,INPCMP
*** Initialise variables, graphics, input and algebra.
       CALL INIT
*** Print the news.
C      PRINT *,' ------------------------------------------------------'
C      PRINT *,' News, including some old but important items.         '
C      PRINT *,' ......................................................'
C      PRINT *,' 28/09/92: Gas mixing a la G. Schultz & J. Gresser.    '
C      PRINT *,' 19/02/94: Polygons (triangle - octagon) available.    '
C      PRINT *,' 20/05/94: Magboltz 1 gas mixing interface.            '
C      PRINT *,' 04/01/97: Monte Carlo drift line integration added.   '
C      PRINT *,' 27/01/97: Heed clustering interface introduced.       '
C      PRINT *,' 21/05/97: Reading Maxwell 2D field maps.              '
C      PRINT *,' 28/10/97: Reading Maxwell 3D field maps.              '
C      PRINT *,' 30/04/99: Signals in other electrodes than wires.     '
C      PRINT *,' 21/05/99: New arrival time distribution format.       '
C      PRINT *,' 04/02/00: Magboltz 2 introduced.                      '
C      PRINT *,' 24/09/00: Heed interface corrected for cluster losses.'
C      PRINT *,' 13/06/01: Merging of gas datasets added.              '
C      PRINT *,' 28/11/01: Photons and electrons as primary for Heed.  '
C      PRINT *,' 13/05/02: Introducing ion dissociation.               '
C      PRINT *,' ......................................................'
C      PRINT *,' Garfield, Heed and Magboltz documentation is at:      '
C      PRINT *,' http://cern.ch/garfield                               '
C      PRINT *,' http://cern.ch/heed                                   '
C      PRINT *,' http://cern.ch/magboltz                               '
C      PRINT *,' ------------------------------------------------------'
C      PRINT *,' '
C      PRINT *,' '
       PRINT *,' Welcome, this is Garfield - version 7.45,'//
     -      ' updated until 11 March 2014'
       PRINT *,' '
       PRINT *,' Documentation is in http://cern.ch/garfield'
       PRINT *,' '
*** Print a message when ready to start in interactive mode.
C      IF(STDSTR('INPUT'))THEN
C           PRINT *,' ================================================'
C           PRINT *,' ==========  Ready  -  Enter a header  =========='
C           PRINT *,' ================================================'
C           PRINT *,' '
C      ENDIF
*** Start an input loop that stops at the EOF or at the STOP command.
       IFAIL=0
       CALL INPPRM('Main','NEW-PRINT')
       CALL INPWRD(NWORD)
*** Otherwise the line should start with an & symbol.
10     CONTINUE
       CALL INPNUM(NWORD)
*   Skip blank lines.
       IF(NWORD.EQ.0)THEN
            CALL INPWRD(NWORD)
            GOTO 10
       ENDIF
*   Stay in main if requested.
       IF(INPCMP(1,'&MAIN')+INPCMP(2,'MAIN').NE.0)THEN
            CALL INPWRD(NWORD)
            GOTO 10
       ENDIF
*   Make sure it starts with an ampersand.
       CALL INPSTR(1,1,STRING,NC)
       IF(INPCMP(1,'ENDDO')+INPCMP(1,'ENDIF')+INPCMP(1,'LEAVE')+
     -      INPCMP(1,'BREAK')+INPCMP(1,'ITERATE')+
     -      INPCMP(1,'CONTINUE').NE.0)THEN
            PRINT *,' !!!!!! MAIN   WARNING : "'//STRING(1:NC)//
     -           '" control statement out of context.'
            CALL INPWRD(NWORD)
            GOTO 10
       ELSEIF(STRING(1:1).NE.'&')THEN
            PRINT *,' !!!!!! MAIN   WARNING : Please enter a section'//
     -           ' header, a control statement or a global command.'
            CALL INPWRD(NWORD)
            GOTO 10
       ELSEIF(NC.EQ.1.AND.NWORD.EQ.1)THEN
            PRINT *,' !!!!!! MAIN   WARNING : A section name should'//
     -           ' be appended to the &; try again.'
            CALL INPWRD(NWORD)
            GOTO 10
       ENDIF
       IF((NWORD.GT.2.AND.NC.EQ.1).OR.(NWORD.GT.1.AND.NC.GT.1))
     -      PRINT *,' !!!!!! MAIN   WARNING : Keywords on the header'//
     -           ' line are ignored in this version of the program.'
       IF(NC.EQ.1)CALL INPSTR(2,2,STRING,NC)
*** Stop if STOP is the keyword.
       IF(INPCMP(1,'&ST#OP')+INPCMP(2,'ST#OP')+
     -      INPCMP(1,'&Q#UIT')+INPCMP(2,'Q#UIT')+
     -      INPCMP(1,'&EX#IT')+INPCMP(2,'EX#IT').NE.0)THEN
            CALL QUIT
            STOP
*** Call CELDEF if CELL is a keyword,
       ELSEIF(INPCMP(1,'&C#ELL')+INPCMP(2,'C#ELL').NE.0)THEN
*   Call cell reading routine.
            CALL CELDEF(IFAIL)
            IF(IFAIL.EQ.1)PRINT *,' !!!!!! MAIN   WARNING : The cell'//
     -           ' section failed ; various sections can not be'//
     -           ' entered.'
*** Call MAGINP if MAGNETIC is a keyword.
       ELSEIF(INPCMP(1,'&M#AGNETIC-#FIELD')+
     -      INPCMP(2,'M#AGNETIC-#FIELD').NE.0)THEN
            CALL MAGINP
            IF(GASSET)THEN
                 IF((BTAB(1)-BFMIN*BSCALE)*
     -                (BFMIN*BSCALE-BTAB(NBTAB)).LT.0.OR.
     -                (BTAB(1)-BFMAX*BSCALE)*
     -                (BFMAX*BSCALE-BTAB(NBTAB)).LT.0)THEN
                      PRINT *,' ------ MAIN   MESSAGE : Previous gas'//
     -                     ' data deleted.'
                      GASSET=.FALSE.
                 ENDIF
            ENDIF
*** Read gas data if GAS is the first keyword,
       ELSEIF(INPCMP(1,'&G#AS')+INPCMP(2,'G#AS').NE.0)THEN
*   Call the gas data reading routine.
            CALL GASDEF(IFAIL)
            IF(IFAIL.NE.0.AND.JFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! MAIN   WARNING : Gas section failed'//
     -                ' ; CO2 will be used for the time being.'
                 CALL XXXGAS(IFAIL)
                 IF(IFAIL.NE.0)PRINT *,' ###### MAIN   ERROR   : CO2'//
     -                ' data are not correct ; no gas data.'
            ELSEIF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! MAIN   WARNING : The gas section'//
     -                ' failed ; various sections can not be entered.'
            ENDIF
*** Call FLDINP if FIELD is a keyword.
       ELSEIF(INPCMP(1,'&F#IELD')+INPCMP(2,'F#IELD').NE.0)THEN
            IF(CELSET)THEN
                 CALL FLDINP
            ELSE
                 PRINT *,' !!!!!! MAIN   WARNING : No cell available'//
     -                ' to do field calculations in ; skipped.'
                 CALL SKIP
            ENDIF
*** Call OPTINP if OPTIMISE is a keyword.
       ELSEIF(INPCMP(1,'&O#PTIMISE')+INPCMP(2,'O#PTIMISE').NE.0)THEN
            IF(CELSET)THEN
                 CALL OPTINP
            ELSE
                 PRINT *,' !!!!!! MAIN   WARNING : No cell available'//
     -                ' to optimise ; the section is skipped.'
                 CALL SKIP
            ENDIF
*** Call DRFINP if DRIFT is the keyword.
       ELSEIF(INPCMP(1,'&D#RIFT')+INPCMP(2,'D#RIFT').NE.0)THEN
            IF((.NOT.GASSET).AND.JFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! MAIN   WARNING : No gas data found'//
     -                ' so far ; CO2 will be used for the time being.'
                 CALL XXXGAS(IFAIL)
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' ###### MAIN   ERROR   : The CO2 data'//
     -                     ' are not correct ; no gas data.'
                      CALL SKIP
                      GOTO 10
                 ENDIF
            ELSEIF(.NOT.GASSET)THEN
                 PRINT *,' !!!!!! MAIN   WARNING : No valid gas data'//
     -                ' found so far ; drift section not executed.'
                 CALL SKIP
                 GOTO 10
            ENDIF
            IF(CELSET)THEN
                 CALL DRFINP
            ELSE
                 PRINT *,' !!!!!! MAIN   WARNING : No valid cell data'//
     -                ' found so far ; drift section not executed.'
                 CALL SKIP
            ENDIF
*** Call SIGINP if SIGNAL is the keyword.
       ELSEIF(INPCMP(1,'&SI#GNAL')+INPCMP(2,'SI#GNAL').NE.0)THEN
            IF((.NOT.GASSET).AND.JFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! MAIN   WARNING : No gas data found'//
     -                ' so far ; CO2 will be used for the time being.'
                 CALL XXXGAS(IFAIL)
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' ###### MAIN   ERROR   : The CO2 data'//
     -                     ' are not correct ; no gas data.'
                      CALL SKIP
                      GOTO 10
                 ENDIF
            ELSEIF(.NOT.GASSET)THEN
                 PRINT *,' !!!!!! MAIN   WARNING : No valid gas data'//
     -                ' found so far ; signal section not executed.'
                 CALL SKIP
                 GOTO 10
            ENDIF
            IF(CELSET)THEN
                 CALL SIGINP
            ELSE
                 PRINT *,' !!!!!! MAIN   WARNING : No valid cell data'//
     -                ' found so far ; signal section not executed.'
                 CALL SKIP
            ENDIF
*** Header is recognised.
       ELSE
            PRINT *,' !!!!!! MAIN   WARNING : ',STRING(1:NC),' is'//
     -              ' not a valid header.'
            CALL SKIP
       ENDIF
*** Read a new header.
       CALL INPPRM('Main','NEW-PRINT')
       GOTO 10
       END
