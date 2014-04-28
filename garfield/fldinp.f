CDECK  ID>, FLDINP.
       SUBROUTINE FLDINP
*-----------------------------------------------------------------------
*   FLDINP - Routine reading and interpreting the instructions of the
*            field section.
*   Variables : NGRIDR     : NGRID as read from input file
*   (Last changed on 30/11/09.)
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
       DOUBLE PRECISION WGT,FPRMAT,
     -      FPROJ,FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX,GYBOX,GZBOX
       REAL PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM
       INTEGER NLINED,NGRIDX,NGRIDY,ITRTYP,NTRLIN,NTRSAM,INDPOS,NCTRW,
     -      NTRFLX,NINORD,
     -      NCPNAM,NCXLAB,NCYLAB,NCFPRO,IPRMAT,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,ITFSRM,NTRERR
       LOGICAL LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG,LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       COMMON /PARMS / WGT(MXLIST),FPRMAT(3,3),
     -      FPROJ(3,3),FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX(12),GYBOX(12),GZBOX(12),
     -      PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM,
     -      INDPOS(11000),IPRMAT(3),NCTRW,NCPNAM,
     -      ITRTYP,NTRLIN,NTRSAM,NTRFLX,ITFSRM,NTRERR(10),
     -      NLINED,NINORD,NGRIDX,NGRIDY,NCXLAB,NCYLAB,NCFPRO,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,
     -      LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG(10),LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       CHARACTER*80 PARTID,PXLAB,PYLAB,PROLAB
       CHARACTER*10 PNAME
       CHARACTER*5  PRVIEW
       CHARACTER*(MXCHAR) FCNTRW
       COMMON /PARCHR/ PARTID,FCNTRW,PNAME,PXLAB,PYLAB,PROLAB,PRVIEW
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
       CHARACTER*(MXCHAR) STRING
       REAL XTEST,YTEST,ZTEST,EX,EY,EZ,ETOT,BX,BY,BZ,BTOT,VOLT,CPU,RNDM
       INTEGER ILOC,NC,IFAIL1,IFAIL2,IFAIL3,IFAIL,
     -      NGRIDR,NGRDXR,NGRDYR,I,NTEST,NWORD,INPCMP
       EXTERNAL INPCMP,RNDM
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE FLDINP ///'
*** Print a header for this page.
       WRITE(*,'(''1'')')
       PRINT *,' ================================================'
       PRINT *,' ==========  Start of field section    =========='
       PRINT *,' ================================================'
       PRINT *,' '
*** Start an input loop.
       CALL INPPRM('Field','NEW-PRINT')
10     CONTINUE
       CALL INPWRD(NWORD)
       CALL INPSTR(1,1,STRING,NC)
*** Skip the line if blank.
       IF(NWORD.EQ.0)GOTO 10
*** Return to main program if '&' is the first character.
       IF(STRING(1:1).EQ.'&')THEN
            RETURN
*** Look for the AREA instruction.
       ELSEIF(INPCMP(1,'A#REA').NE.0)THEN
            CALL CELVIE(PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX)
*** Look for the keyword CHECK.
       ELSEIF(INPCMP(1,'CH#ECK').NE.0)THEN
            CALL FLDCHK
*** Look for the keyword GRID.
       ELSEIF(INPCMP(1,'G#RID').NE.0)THEN
            IF(NWORD.EQ.1)THEN
                 WRITE(LUNOUT,'(''  Current number of grid points is '',
     -                I3,'' by '',I3,''.'')') NGRIDX,NGRIDY
            ELSEIF(NWORD.EQ.2)THEN
                 CALL INPCHK(2,1,IFAIL1)
                 CALL INPRDI(2,NGRIDR,25)
                 IF(NGRIDR.LE.1.OR.NGRIDR.GT.MXGRID)
     -                CALL INPMSG(2,'GRID out of range 2 -> MXGRID.')
                 IF(IFAIL1.NE.0.OR.NGRIDR.LE.1.OR.NGRIDR.GT.MXGRID)THEN
                      PRINT *,' !!!!!! FLDINP WARNING : GRID statement',
     -                     ' ignored because of syntax or value errors.'
                 ELSE
                      NGRIDX=NGRIDR
                      NGRIDY=NGRIDR
                 ENDIF
            ELSEIF(NWORD.EQ.3)THEN
                 CALL INPCHK(2,1,IFAIL1)
                 CALL INPCHK(3,1,IFAIL2)
                 CALL INPRDI(2,NGRDXR,25)
                 CALL INPRDI(3,NGRDYR,25)
                 IF(NGRDXR.LE.1.OR.NGRDXR.GT.MXGRID)
     -                CALL INPMSG(2,'out of the range 2 -> MXGRID. ')
                 IF(NGRDYR.LE.1.OR.NGRDYR.GT.MXGRID)
     -                CALL INPMSG(2,'out of the range 2 -> MXGRID. ')
                 IF(IFAIL1.NE.0.OR.NGRDXR.LE.1.OR.NGRDXR.GT.MXGRID.OR.
     -                NGRDYR.LE.1.OR.NGRDYR.GT.MXGRID)THEN
                      PRINT *,' !!!!!! FLDINP WARNING : GRID statement',
     -                     ' ignored because of syntax or value errors.'
                 ELSE
                      NGRIDX=NGRDXR
                      NGRIDY=NGRDYR
                 ENDIF
            ELSE
                 PRINT *,' !!!!!! FLDINP WARNING : GRID requires 1'//
     -                ' or 2 arguments ; the instruction is ignored.'
            ENDIF
*** Dipole moments.
       ELSEIF(INPCMP(1,'MULT#IPOLE-#MOMENTS').NE.0)THEN
            CALL EFMWIR
*** Look for the keyword OPTION,
       ELSEIF(INPCMP(1,'OPT#IONS').NE.0)THEN
            IF(NWORD.LE.1)WRITE(LUNOUT,'(/
     -           ''  LOCAL OPTIONS CURRENTLY IN EFFECT: ''//
     -           ''  Check for multiple field map indices:         '',
     -           L1/
     -           ''  Contour all media (T) or drift medium (F):    '',
     -           L1/
     -           ''  Plot wires by markers (WIRE-MARKERS):         '',
     -           L1/
     -           ''  Check for multiple field map indices:         '',
     -           L1/)') LMAPCH,LCNTAM,LWRMRK,LMAPCH
*   Check for the various options.
            DO 11 I=2,NWORD
*   Detect multiple map indices.
            IF(INPCMP(I,'CH#ECK-MAP-#INDICES')+
     -           INPCMP(I,'CH#ECK-MAP-#INDEXING').NE.0)THEN
                 LMAPCH=.TRUE.
            ELSEIF(INPCMP(I,'NOCH#ECK-MAP-#INDICES')+
     -           INPCMP(I,'NOCH#ECK-MAP-#INDEXING').NE.0)THEN
                 LMAPCH=.FALSE.
*   Contours in other than drift media.
            ELSEIF(INPCMP(I,'CONT#OUR-ALL-#MEDIA').NE.0)THEN
                 LCNTAM=.TRUE.
            ELSEIF(INPCMP(I,'CONT#OUR-DR#IFT-#MEDIUM')+
     -           INPCMP(I,'CONT#OUR-DR#IFT-#MEDIA').NE.0)THEN
                 LCNTAM=.FALSE.
*   Wires drawn as markers.
            ELSEIF(INPCMP(I,'NOW#IRE-M#ARKERS').NE.0)THEN
                 LWRMRK=.FALSE.
            ELSEIF(INPCMP(I,'W#IRE-M#ARKERS').NE.0)THEN
                 LWRMRK=.TRUE.
            ELSE
                 CALL INPMSG(I,'The option is not known.      ')
            ENDIF
11          CONTINUE
*** Make plots if PLOT is a keyword.
       ELSEIF(INPCMP(1,'PL#OT-#FIELD').NE.0)THEN
            CALL FLDPLT
*** Look for the keyword PRINT.
       ELSEIF(INPCMP(1,'PR#INT-#FIELD').NE.0)THEN
            CALL FLDPRT
*** Test field calculation.
       ELSEIF(INPCMP(1,'S#AMPLE').NE.0)THEN
            CALL INPCHK(2,2,IFAIL1)
            CALL INPCHK(3,2,IFAIL2)
            CALL INPCHK(4,2,IFAIL3)
            CALL INPRDR(2,XTEST,0.0)
            CALL INPRDR(3,YTEST,0.0)
            CALL INPRDR(4,ZTEST,0.0)
            PRINT *,' ++++++ FLDINP DEBUG   : Sampling EFIELD + BFIELD'
            IF(.NOT.POLAR)PRINT 3020,XTEST,YTEST,ZTEST
            IF(POLAR)THEN
                 PRINT 3025,XTEST,YTEST,ZTEST
                 CALL CFMPTR(XTEST,YTEST,XTEST,YTEST,1,IFAIL)
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' !!!!!! FLDINP WARNING : Illegal polar',
     -                     ' coordinates; command not executed.'
                      CALL INPERR
                      GOTO 10
                 ENDIF
                 PRINT *,' Internal coordinates:'
                 PRINT 3020,XTEST,YTEST,ZTEST
            ENDIF
            CALL EFIELD(XTEST,YTEST,ZTEST,EX,EY,EZ,ETOT,VOLT,1,ILOC)
            PRINT *,' Location code for this point : ',ILOC
            IF(POLAR)THEN
                 EX=EX/EXP(XTEST)
                 EY=EY/EXP(XTEST)
                 ETOT=ETOT/EXP(XTEST)
            ENDIF
            IF(.NOT.POLAR)PRINT 3030,EX,EY,EZ,ETOT,VOLT
            IF(POLAR)PRINT 3035,EX,EY,EZ,ETOT,VOLT
            IF(MAGOK)THEN
                 CALL BFIELD(XTEST,YTEST,ZTEST,BX,BY,BZ,BTOT)
                 PRINT 3040,BX,BY,BZ,BTOT
            ENDIF
3020        FORMAT(' At (x,y,z) = (',F10.3,2(',',F10.3),')')
3025        FORMAT(' At (r,phi,z) = (',F10.3,2(',',F10.3),')')
3030        FORMAT(' Ex=',F15.4,' Ey=',F15.4,' Ez=',F15.4,
     -             ' Etot=',F15.4,' V=',F15.4)
3035        FORMAT(' Er=',F15.4,' Ephi=',F15.4,' Ez=',F15.4,
     -             ' Etot=',F15.4,' V=',F15.4)
3040        FORMAT(' Bx=',F15.4,' By=',F15.4,'   Bz=',F15.4,
     -             ' Btot=',F15.4)
            PRINT *,' ++++++ FLDINP DEBUG   : End of SAMPLE.'
*** Search for the SELECT instruction.
       ELSEIF(INPCMP(1,'SEL#ECT').NE.0)THEN
            CALL CELSEL(' ')
*** Perform a timing if TIME is a keyword.
       ELSEIF(INPCMP(1,'TIM#E').NE.0)THEN
            CALL INPCHK(2,1,IFAIL1)
            CALL INPRDI(2,NTEST,1000)
            IF(NTEST.LE.0)NTEST=1000
            CALL TIMED(CPU)
            DO 3050 I=1,NTEST
            XTEST=PXMIN+RNDM(I)  *(PXMAX-PXMIN)
            YTEST=PYMIN+RNDM(I+1)*(PYMAX-PYMIN)
            CALL EFIELD(XTEST,YTEST,0.0,EX,EY,EZ,ETOT,VOLT,1,ILOC)
3050        CONTINUE
            CALL TIMED(CPU)
            CALL TIMLOG('< TIME: field evaluation >              ')
            PRINT *,' ++++++ FLDINP DEBUG   : CPU time required for',
     -              NTEST,' field evaluations is ',CPU,' seconds.'
*** Look for the instruction TRACK.
       ELSEIF(INPCMP(1,'TR#ACK').NE.0)THEN
            CALL TRAREA
*** Look for the ZERO instruction
       ELSEIF(INPCMP(1,'ZERO').NE.0)THEN
            PRINT *,' !!!!! FLDINP WARNING : This instruction is',
     -           ' currently being debugged.'
            CALL ZROTST
*** It is not possible to get here if the keyword is valid.
       ELSE
            CALL INPSTR(1,1,STRING,NC)
            PRINT *,' !!!!!! FLDINP WARNING : ',STRING(1:NC),' is',
     -              ' not a valid instruction ; ignored'
       ENDIF
       CALL INPERR
       GOTO 10
       END
