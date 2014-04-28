CDECK  ID>, GASINP.
       SUBROUTINE GASINP(LGASPL,LGASPR,LGASWR,IFAIL)
*-----------------------------------------------------------------------
*   GASINP - Subroutine initialising gasdata (i.e. filling /GASDAT/).
*   VARIABLES : IFAIL        : 1 if routine failed 0 if succesful
*   (Last changed on  6/12/08.)
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
       REAL SRMDEN,ESRIM,SRMEM,SRMHD,SRMRNG,SRMDT,SRMDL,WSRIM,FSRIM,
     -      XSRIM,YSRIM,ZSRIM,ECSRIM,EKSRIM
       INTEGER NSRIM,NCSRIM,NESRIM
       COMMON /SRMDAT/
     -      ESRIM(MXLIST),SRMEM(MXLIST),
     -      SRMHD(MXLIST),SRMRNG(MXLIST),SRMDT(MXLIST),SRMDL(MXLIST),
     -      XSRIM(MXCLUS),YSRIM(MXCLUS),ZSRIM(MXCLUS),ECSRIM(MXCLUS),
     -      EKSRIM(MXCLUS),SRMDEN,WSRIM,FSRIM,
     -      NSRIM,NCSRIM,NESRIM(MXCLUS)
       CHARACTER*(MXCHAR) STRING
       CHARACTER*10 VARTAB(MXVAR),VARCLS(MXVAR),UNIT
       CHARACTER*20 STRAUX
       LOGICAL USE(MXVAR),STDSTR,LGASPL,LGASPR,LGASWR,FLAG(MXWORD+3),
     -      OVERLP,EPLOG,OK
       REAL VAR(MXVAR),RES(MXWORD),SIZ,CLSR,PGASR,TGASR,
     -      PGASRR,TGASRR,DATAR,DIONR,YMINR,YMAXR,
     -      EPMIN,EPMAX,EPMINR,EPMAXR,
     -      BANGMN,BANGMX,BAMINR,BAMAXR,
     -      BTABMN,BTABMX,BTMINR,BTMAXR
       INTEGER MODVAR(MXVAR),MODRES(MXWORD),IFAIL,INPCMP,INPTYP,I,J,K,
     -      INEXT,II,IINEXT,NWORD,IENTRY,NRES,NC,ICLS,IMETHR,IFAIL1,
     -      IFAIL2,IFAIL3,IRANGE,NCLR,IOBJ,IEXTRR,NNGAS,
     -      ITAB,IEP,IDRIFT,IVB,IVEXB,IDIFF,ITRANS,IMOBIL,ITOWN,IATT,
     -      ILOREN,IDISS,IFCN,NCUNIT,NGASR,NBANGR,NBTABR,NCAUX
       EXTERNAL STDSTR,INPCMP,INPTYP
       SAVE VARTAB,VARCLS
       DATA (VARTAB(I),I=1,7)/
     -      'EP        ','BOLTZMANN ','ECHARGE   ','ANGLE_EB  ',
     -      'B         ','T         ','P         '/
       DATA VARCLS(1)/'N         '/
*** Define some output formats.
1050   FORMAT(/'  LOCAL OPTIONS CURRENTLY IN EFFECT:'//
     -         '  Plotting graphs of the gas data (GAS-PLOT):   ',L1/
     -         '  Printing a gas summary table (GAS-PRINT):     ',L1/)
*** Identify the routine, if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE GASINP ///'
*** Preset the control variables.
       LGASWR=.FALSE.
*** Preset the gas data.
       CALL GASINT
*** Loop over the input commands, until a new heading is found.
       CALL INPPRM('Gas','NEW-PRINT')
10     CONTINUE
*** Read a new input line.
       CALL INPWRD(NWORD)
*** Skip this line if it is blank.
       CALL INPSTR(1,1,STRING,NC)
       IF(NWORD.EQ.0)GOTO 10
*** Leave if the first sign in the command is &.
       IF(STRING(1:1).EQ.'&')THEN
            IF(LDEBUG)PRINT *,' ++++++ GASINP DEBUG   : On leaving,',
     -           ' the GASOK bits are ',(GASOK(I),I=1,12)
            CALL TIMLOG('Reading the gas data:                   ')
            RETURN
*** Add an item to the gas tables.
       ELSEIF(INPCMP(1,'ADD')+INPCMP(1,'REPL#ACE').NE.0)THEN
*   Call the routine.
            CALL GASADD
*** Axes for the plots.
       ELSEIF(INPCMP(1,'AX#ES')+INPCMP(1,'PL#OT-OPT#IONS').NE.0)THEN
*   No arguments: list current settings.
            IF(NWORD.EQ.1)THEN
                 WRITE(LUNOUT,
     -                '(/''  GAS PLOT OPTIONS:''/
     -                ''  Quantity                  '',
     -                '' Log-x Log-y  Range''/)')
                 IF(.NOT.GASOPT(1,3))THEN
                      WRITE(LUNOUT,'(''  Drift velocity:           '',
     -                     '' Not plotted.'')')
                 ELSEIF(GASOPT(1,4))THEN
                      WRITE(LUNOUT,'(''  Drift velocity:           '',
     -                     2L6,2X,2E12.5)') (GASOPT(1,J),J=1,2),
     -                     (GASRNG(1,J),J=1,2)
                 ELSE
                      WRITE(LUNOUT,'(''  Drift velocity:           '',
     -                     2L6,2X,''Automatic'')') (GASOPT(1,J),J=1,2)
                 ENDIF
                 IF(.NOT.GASOPT(2,3))THEN
                      WRITE(LUNOUT,'(''  Ion mobility:             '',
     -                     '' Not plotted.'')')
                 ELSEIF(GASOPT(2,4))THEN
                      WRITE(LUNOUT,'(''  Ion mobility:             '',
     -                     2L6,2X,2E12.5)') (GASOPT(2,J),J=1,2),
     -                     (GASRNG(2,J),J=1,2)
                 ELSE
                      WRITE(LUNOUT,'(''  Ion mobility:             '',
     -                     2L6,2X,''Automatic'')') (GASOPT(2,J),J=1,2)
                 ENDIF
                 IF(.NOT.GASOPT(3,3))THEN
                      WRITE(LUNOUT,'(''  Diffusion coefficients:   '',
     -                     '' Not plotted.'')')
                 ELSEIF(GASOPT(3,4))THEN
                      WRITE(LUNOUT,'(''  Diffusion coefficients:   '',
     -                     2L6,2X,2E12.5)') (GASOPT(3,J),J=1,2),
     -                     (GASRNG(3,J),J=1,2)
                 ELSE
                      WRITE(LUNOUT,'(''  Diffusion coefficients:   '',
     -                     2L6,2X,''Automatic'')') (GASOPT(3,J),J=1,2)
                 ENDIF
                 IF(.NOT.GASOPT(4,3))THEN
                      WRITE(LUNOUT,'(''  Townsend & attachment:    '',
     -                     '' Not plotted.'')')
                 ELSEIF(GASOPT(4,4))THEN
                      WRITE(LUNOUT,'(''  Townsend & attachment:    '',
     -                     2L6,2X,2E12.5)') (GASOPT(4,J),J=1,2),
     -                     (GASRNG(4,J),J=1,2)
                 ELSE
                      WRITE(LUNOUT,'(''  Townsend & attachment:    '',
     -                     2L6,2X,''Automatic'')') (GASOPT(4,J),J=1,2)
                 ENDIF
                 IF(.NOT.GASOPT(7,3))THEN
                      WRITE(LUNOUT,'(''  Angle between v and E:    '',
     -                     '' Not plotted.'')')
                 ELSEIF(GASOPT(7,4))THEN
                      WRITE(LUNOUT,'(''  Angle between v and E:    '',
     -                     2L6,2X,2E12.5)') (GASOPT(7,J),J=1,2),
     -                     (GASRNG(7,J),J=1,2)
                 ELSE
                      WRITE(LUNOUT,'(''  Angle between v and E:    '',
     -                     2L6,2X,''Automatic'')') (GASOPT(7,J),J=1,2)
                 ENDIF
                 IF(.NOT.GASOPT(5,3))THEN
                      WRITE(LUNOUT,'(''  Cluster size distribution:'',
     -                     '' Not plotted.'')')
                 ELSE
                      WRITE(LUNOUT,'(''  Cluster size distribution:'',
     -                     2L6)') (GASOPT(5,J),J=1,2)
                 ENDIF
                 IF(.NOT.GASOPT(13,3))THEN
                      WRITE(LUNOUT,'(''  SRIM energy loss tables:  '',
     -                     '' Not plotted.'')')
                 ELSE
                      WRITE(LUNOUT,'(''  SRIM energy loss tables:  '',
     -                     '' Plotted with automatic axes.'')')
                 ENDIF
                 IF(.NOT.GASOPT(15,3))THEN
                      WRITE(LUNOUT,'(''  Ionisation & excitation:  '',
     -                     '' Not plotted.'')')
                 ELSEIF(GASOPT(15,4))THEN
                      WRITE(LUNOUT,'(''  Ionisation & excitation:  '',
     -                     2L6,2X,2E12.5)') (GASOPT(15,J),J=1,2),
     -                     (GASRNG(15,J),J=1,2)
                 ELSE
                      WRITE(LUNOUT,'(''  Ionisation & excitation:  '',
     -                     2L6,2X,''Automatic'')') (GASOPT(15,J),J=1,2)
                 ENDIF
                 IF(.NOT.GASOPT(16,3))THEN
                      WRITE(LUNOUT,'(''  TRIM energy loss tables:  '',
     -                     '' Not plotted.'')')
                 ELSE
                      WRITE(LUNOUT,'(''  TRIM energy loss tables:  '',
     -                     '' Plotted with automatic axes.'')')
                 ENDIF
                 GOTO 10
            ENDIF
*   Loop over the arguments.
            INEXT=2
            DO 700 I=2,NWORD
            IF(INEXT.GT.I)GOTO 700
*   Identify the plot.
            IOBJ=0
            IF(INPCMP(I,'DR#IFT-#VELOCITY-#PLOT').NE.0)THEN
                 IOBJ=1
                 GASOPT(1,3)=.TRUE.
            ELSEIF(INPCMP(I,'NODR#IFT-#VELOCITY-#PLOT').NE.0)THEN
                 IOBJ=0
                 GASOPT(1,3)=.FALSE.
            ELSEIF(INPCMP(I,'ION-MOB#ILITY-#PLOT')+
     -           INPCMP(I,'MOB#ILITY-#PLOT').NE.0)THEN
                 GASOPT(2,3)=.TRUE.
                 IOBJ=2
            ELSEIF(INPCMP(I,'NOION-MOB#ILITY-#PLOT')+
     -           INPCMP(I,'NOMOB#ILITY-#PLOT').NE.0)THEN
                 GASOPT(2,3)=.FALSE.
                 IOBJ=0
            ELSEIF(INPCMP(I,'DIFF#USION-#COEFFICIENTS-#PLOT')+
     -           INPCMP(I,'DIFF#USION-#PLOT').NE.0)THEN
                 IOBJ=3
                 GASOPT(3,3)=.TRUE.
            ELSEIF(INPCMP(I,'NODIFF#USION-#COEFFICIENTS-#PLOT')+
     -           INPCMP(I,'NODIFF#USION-#PLOT').NE.0)THEN
                 IOBJ=0
                 GASOPT(3,3)=.FALSE.
            ELSEIF(INPCMP(I,'TOWN#SEND-#COEFFICIENTS-#PLOT')+
     -           INPCMP(I,'TOWN#SEND-#PLOT')+
     -           INPCMP(I,'AVAL#ANCHE-#PLOT')+
     -           INPCMP(I,'ATT#ACHMENT-#COEFFICIENTS-#PLOT')+
     -           INPCMP(I,'ATT#ACHMENT-#PLOT').NE.0)THEN
                 IOBJ=4
                 GASOPT(4,3)=.TRUE.
            ELSEIF(INPCMP(I,'NOTOWN#SEND-#COEFFICIENTS-#PLOT')+
     -           INPCMP(I,'NOTOWN#SEND-#PLOT')+
     -           INPCMP(I,'NOAVAL#ANCHE-#PLOT')+
     -           INPCMP(I,'NOATT#ACHMENT-#COEFFICIENTS-#PLOT')+
     -           INPCMP(I,'NOATT#ACHMENT-#PLOT').NE.0)THEN
                 IOBJ=0
                 GASOPT(4,3)=.FALSE.
            ELSEIF(INPCMP(I,'CLUS#TER-#SIZE-#DISTRIBUTION-#PLOT')+
     -           INPCMP(I,'CLUS#TER-#SIZE-#PLOT')+
     -           INPCMP(I,'CLUS#TER-#PLOT').NE.0)THEN
                 IOBJ=5
                 GASOPT(5,3)=.TRUE.
            ELSEIF(INPCMP(I,'NOCLUS#TER-#SIZE-#DISTRIBUTION-#PLOT')+
     -           INPCMP(I,'NOCLUS#TER-#SIZE-#PLOT')+
     -           INPCMP(I,'NOCLUS#TER-#PLOT').NE.0)THEN
                 IOBJ=0
                 GASOPT(5,3)=.FALSE.
            ELSEIF(INPCMP(I,'LOR#ENTZ-#ANGLES-#PLOT').NE.0)THEN
                 IOBJ=7
                 GASOPT(7,3)=.TRUE.
            ELSEIF(INPCMP(I,'NOLOR#ENTZ-#ANGLES-#PLOT').NE.0)THEN
                 IOBJ=0
                 GASOPT(7,3)=.FALSE.
            ELSEIF(INPCMP(I,'SRIM-#PLOTS').NE.0)THEN
                 IOBJ=13
                 GASOPT(13,3)=.TRUE.
            ELSEIF(INPCMP(I,'NOSRIM-#PLOTS').NE.0)THEN
                 IOBJ=0
                 GASOPT(13,3)=.FALSE.
            ELSEIF(INPCMP(I,'EXC#ITATION-#PLOTS')+
     -           INPCMP(I,'EXC#ITATION-R#ATES-#PLOTS')+
     -           INPCMP(I,'ION#ISATION-#PLOTS')+
     -           INPCMP(I,'ION#ISATION-R#ATES-#PLOTS').NE.0)THEN
                 IOBJ=15
                 GASOPT(15,3)=.TRUE.
            ELSEIF(INPCMP(I,'NOEXC#ITATION-#PLOTS')+
     -           INPCMP(I,'NOEXC#ITATION-R#ATES-#PLOTS')+
     -           INPCMP(I,'NOION#ISATION-#PLOTS')+
     -           INPCMP(I,'NOION#ISATION-R#ATES-#PLOTS').NE.0)THEN
                 IOBJ=10
                 GASOPT(15,3)=.FALSE.
            ELSEIF(INPCMP(I,'TRIM-#PLOTS').NE.0)THEN
                 IOBJ=16
                 GASOPT(16,3)=.TRUE.
            ELSEIF(INPCMP(I,'NOTRIM-#PLOTS').NE.0)THEN
                 IOBJ=0
                 GASOPT(16,3)=.FALSE.
            ELSE
                 CALL INPMSG(I,'Not a known plot.')
                 GOTO 700
            ENDIF
*   Skip rest if plot not requested.
            IF(IOBJ.EQ.0)GOTO 700
*   Identify the axes and ranges.
            DO 730 J=I+1,NWORD
            IF(J.LT.INEXT)GOTO 730
            IF(INPCMP(J,'LIN#EAR-X').NE.0)THEN
                 GASOPT(IOBJ,1)=.FALSE.
                 INEXT=J+1
            ELSEIF(INPCMP(J,'LOG#ARITHMIC-X').NE.0)THEN
                 GASOPT(IOBJ,1)=.TRUE.
                 INEXT=J+1
            ELSEIF(INPCMP(J,'LIN#EAR-Y').NE.0)THEN
                 GASOPT(IOBJ,2)=.FALSE.
                 INEXT=J+1
            ELSEIF(INPCMP(J,'LOG#ARITHMIC-Y').NE.0)THEN
                 GASOPT(IOBJ,2)=.TRUE.
                 INEXT=J+1
            ELSEIF(INPCMP(J,'RANGE')+INPCMP(J,'SCALE').NE.0)THEN
                 IF(INPCMP(J+1,'AUTO#MATIC').NE.0)THEN
                      GASOPT(IOBJ,4)=.FALSE.
                      INEXT=J+2
                 ELSEIF(INPTYP(J+1).LE.0.OR.INPTYP(J+2).LE.0)THEN
                      CALL INPMSG(J,'Values missing')
                 ELSE
                      GASOPT(IOBJ,4)=.TRUE.
                      CALL INPCHK(J+1,2,IFAIL1)
                      CALL INPCHK(J+2,2,IFAIL2)
                      CALL INPRDR(J+1,YMINR,0.0)
                      CALL INPRDR(J+2,YMAXR,0.0)
                      IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0)THEN
                           IF(YMINR.NE.YMAXR)THEN
                                GASRNG(IOBJ,1)=MIN(YMINR,YMAXR)
                                GASRNG(IOBJ,2)=MAX(YMINR,YMAXR)
                           ELSE
                                CALL INPMSG(J+1,
     -                               'Zero range not permitted.')
                                CALL INPMSG(J+1,
     -                               'See previous message.')
                           ENDIF
                      ENDIF
                      INEXT=J+3
                 ENDIF
            ELSE
                 INEXT=J
                 GOTO 700
            ENDIF
730         CONTINUE
*   Next plot.
700         CONTINUE
*   Dump error messages.
            CALL INPERR
*** Call the routine A50E50 if ARGON-50-ETHANE-50 is the keyword.
       ELSEIF(INPCMP(1,'A#RGON-50-E#THANE-50')+
     -      INPCMP(1,'E#THANE-50-A#RGON-50').NE.0)THEN
            CALL A50E50
*** Call the routine A20E80 if ARGON-20-ETHANE-80 is the keyword.
       ELSEIF(INPCMP(1,'A#RGON-20-E#THANE-80')+
     -      INPCMP(1,'E#THANE-80-A#RGON-20').NE.0)THEN
            CALL A20E80
*** Call the routine A80E20 if ARGON-80-ETHANE-20 is the keyword.
       ELSEIF(INPCMP(1,'A#RGON-80-E#THANE-20')+
     -      INPCMP(1,'E#THANE-20-A#RGON-80').NE.0)THEN
            CALL A80E20
*** Call the routine A73M20 if ARGON-73-ETHANE-20-PROPANOL-7 is asked.
       ELSEIF(INPCMP(1,'A#RGON-73-M#ETHANE-20-#PROPANOL-#7')+
     -      INPCMP(1,'M#ETHANE-20-A#RGON-73-#PROPANOL-#7').NE.0)THEN
            CALL A73M20
*** Call the routine C80E20 if CO2-80-ETHANE-20 is the keyword.
       ELSEIF(INPCMP(1,'CO2-80-E#THANE-20')+
     -      INPCMP(1,'E#THANE-20-CO2-80').NE.0)THEN
            CALL C80E20
*** Call the routine C90E10 if CO2-90-ETHANE-10 is the keyword.
       ELSEIF(INPCMP(1,'CO2-90-E#THANE-10')+
     -      INPCMP(1,'E#THANE-10-CO2-90').NE.0)THEN
            CALL C90E10
*** Call the routine C90I10 if CO2-90-ISOBUTANE-10 is the keyword.
       ELSEIF(INPCMP(1,'CO2-90-I#SOBUTANE-10')+
     -      INPCMP(1,'I#SOBUTANE-10-CO2-90').NE.0)THEN
            CALL C90I10
*** Call the routine CO2 to transfer data if CO2 is a keyword.
       ELSEIF(INPCMP(1,'CO2').NE.0)THEN
            CALL CO2
*** Read the cluster size distribution if CLUSTER-SIZE is a keyword.
       ELSEIF(INPCMP(1,'CL#USTER-#SIZE-#DISTRIBUTION').NE.0)THEN
**  Initialise.
            NFCLS=0
            FCNCLS='?'
            NCLS=MXPAIR
            OVERLP=.FALSE.
**  Read further command line arguments.
            IINEXT=2
            DO 30 II=2,NWORD
            IF(II.LT.IINEXT)GOTO 30
*   Function following ?
            IF(INPCMP(II,'F#UNCTION').NE.0)THEN
                 CALL INPSTR(II+1,II+1,STRING,NFCLS)
                 FCNCLS=STRING(1:NFCLS)
                 IINEXT=II+2
*   Maximum number of entries for functions.
            ELSEIF(INPCMP(II,'N-#MAXIMUM')+
     -           INPCMP(II,'MAX#IMUM-#CLUSTER-#SIZE').NE.0)THEN
                 CALL INPCHK(II+1,1,IFAIL1)
                 CALL INPRDI(II+1,NCLR,0)
                 IF(NCLR.LE.0.AND.IFAIL1.EQ.0)THEN
                      CALL INPMSG(II+1,'Not a positive integer.')
                 ELSEIF(NCLR.GT.MXPAIR.OR.IFAIL1.NE.0)THEN
                      CALL INPMSG(II+1,'Should be < MXPAIR.')
                      NCLS=MXPAIR
                 ELSE
                      NCLS=NCLR
                 ENDIF
                 IINEXT=II+2
*   Overlap with table entries.
            ELSEIF(INPCMP(II,'OVERLAP-#TABLE-#AND-#FUNCTION').NE.0)THEN
                 OVERLP=.TRUE.
            ELSEIF(INPCMP(II,'NOOVERLAP-#TABLE-#AND-#FUNCTION').NE.
     -           0)THEN
                 OVERLP=.FALSE.
*   Other keywords are not known.
            ELSE
                 CALL INPMSG(II+1,'Not a known keyword.')
            ENDIF
30          CONTINUE
*   Print error messages.
            CALL INPERR
**  Check that a function was indeed specified.
            IF(NWORD.GT.1.AND.NFCLS.LE.0)PRINT *,' !!!!!! GASINP'//
     -           ' WARNING : Cluster function not found.'
            IF(NWORD.GT.1.AND.NFCLS.LE.0.AND..NOT.OVERLP)THEN
                 PRINT *,' !!!!!! GASINP WARNING : Also no OVERLAP'//
     -                ' option; CLUSTER ignored.'
                 GOTO 10
            ENDIF
**  If a function is present, process it.
            IF(NFCLS.GE.1)THEN
                 IF(INDEX(FCNCLS(1:NFCLS),'@').NE.0)THEN
                      NRES=1
                      CALL ALGEDT(VARCLS,1,IENTRY,USE,NRES)
                 ELSE
                      CALL ALGPRE(FCNCLS,NFCLS,VARCLS,1,NRES,USE,IENTRY,
     -                     IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! GASINP WARNING : Cluster'//
     -                          ' size distribution function rejected.'
                           CALL ALGCLR(IENTRY)
                           GOTO 10
                      ENDIF
                 ENDIF
                 IF(NRES.NE.1)THEN
                      PRINT *,' !!!!!! GASINP WARNING : Number of'//
     -                     ' results returned by the cluster size'//
     -                     ' distribution function is not 1.'
                      CALL ALGCLR(IENTRY)
                      GOTO 10
                 ENDIF
*   Enter the function into the CLSDIS histogram.
                 DO 200 I=1,NCLS
                 VAR(1)=I-1.0
                 MODVAR(1)=2
                 CALL ALGEXE(IENTRY,VAR,MODVAR,1,RES,MODRES,1,IFAIL1)
                 SIZ=RES(1)
                 VAR(1)=I-0.5
                 CALL ALGEXE(IENTRY,VAR,MODVAR,1,RES,MODRES,1,IFAIL2)
                 SIZ=SIZ+4.0*RES(1)
                 VAR(1)=I
                 CALL ALGEXE(IENTRY,VAR,MODVAR,1,RES,MODRES,1,IFAIL3)
                 SIZ=(SIZ+RES(1))/6.0
                 IF(SIZ.LT.0.0.OR.IFAIL1+IFAIL2+IFAIL3.NE.0)THEN
                      PRINT *,' !!!!!! GASINP WARNING : Function gave'//
     -                     ' non-positive probability or arithmetic'//
     -                     ' error for size ',I,' ; set to 0.'
                      CLSDIS(I)=0
                 ELSE
                      CLSDIS(I)=SIZ
                 ENDIF
200              CONTINUE
*   Print number of algebra errors.
                 CALL ALGERR
*   Finally accept the function and remember it was a function.
                 GASOK(5)=.TRUE.
                 CLSTYP='FUNCTION'
*   Release the instruction list.
                 CALL ALGCLR(IENTRY)
            ENDIF
**  Read a table.
            IF(NWORD.EQ.1.OR.OVERLP)THEN
                 ICLS=0
                 IFAIL=0
*   Output a prompt in interactive use.
                 IF(STDSTR('INPUT'))
     -               PRINT *,' ====== GASINP INPUT   :'//
     -               ' Please enter the cluster size distribution ;'//
     -               ' terminate with a blank line.'
                 CALL INPPRM('Cluster','ADD-NOPRINT')
*   Read the table line by line.
210              CONTINUE
                 CALL INPWRD(NWORD)
                 IF(NWORD.EQ.0)GOTO 230
                 CALL INPSTR(1,1,STRING,NC)
                 IF(STRING(1:1).EQ.'&')THEN
                      PRINT *,' !!!!!! GASINP WARNING : You can not'//
     -                        ' leave the section here ; line ignored.'
                      GOTO 210
                 ENDIF
*   And read all probabilities within each line.
                 DO 220 I=1,NWORD
                 ICLS=ICLS+1
                 IF(ICLS.GT.MXPAIR)GOTO 210
                 CALL INPCHK(I,2,IFAIL1)
                 CALL INPRDR(I,CLSR,0.0)
                 IF(CLSR.LT.0.0)THEN
                      CALL INPMSG(I,'Probabilities may not be < 0. ')
                      CLSDIS(ICLS)=0
                 ELSE
                      CLSDIS(ICLS)=CLSR
                 ENDIF
220              CONTINUE
                 CALL INPERR
                 GOTO 210
*   End of reading loop: check some correct data is present.
230              CONTINUE
*   If this was a pure table, set NCLS.
                 IF(.NOT.OVERLP)NCLS=ICLS
                 IF(NCLS.GT.MXPAIR)THEN
                      PRINT *,' !!!!!! GASINP WARNING : Too many',
     -                     ' cluster size points ; excess ignored.'
                      NCLS=MXPAIR
                      GASOK(5)=.TRUE.
                      IF(OVERLP)THEN
                           CLSTYP='OVERLAP'
                      ELSE
                           CLSTYP='TABLE'
                      ENDIF
                 ELSEIF(NCLS.EQ.0)THEN
                      PRINT *,' !!!!!! GASINP WARNING : The CLUSTER'//
     -                     ' statement is empty and is ignored.'
                 ELSE
                      GASOK(5)=.TRUE.
                      IF(OVERLP)THEN
                           CLSTYP='OVERLAP'
                      ELSE
                           CLSTYP='TABLE'
                      ENDIF
                 ENDIF
*   Reset the prompt.
                 CALL INPPRM(' ','BACK-PRINT')
            ENDIF
*** Composition.
       ELSEIF(INPCMP(1,'COMP#OSITION').NE.0)THEN
            CALL GASCMP(IFAIL1)
*** Call routine ETHANE to transfer data if ETHANE is a keyword.
       ELSEIF(INPCMP(1,'ETH#ANE').NE.0)THEN
            CALL ETHANE
*** Set the extrapolation method.
       ELSEIF(INPCMP(1,'EXT#RAPOLATIONS')+
     -      INPCMP(1,'EXT#RAPOLATE').NE.0)THEN
*   Print the current settings if entered without argument.
            IF(NWORD.EQ.1)THEN
                 WRITE(LUNOUT,'(/1X,A)') ' Currently, the'//
     -                ' extraplation methods for large E/p, are'//
     -                ' set as follows:'
*   Drift velocity for large E/p.
                 IF(IVEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)') 'v || E: constant,'
                 ELSEIF(IVEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)') 'v || E: linear,'
                 ELSEIF(IVEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)') 'v || E: exponential,'
                 ENDIF
*   Drift velocity ExB component large E/p.
                 IF(IXEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)') 'v || Btrans: constant,'
                 ELSEIF(IXEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)') 'v || Btrans: linear,'
                 ELSEIF(IXEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)') 'v || Btrans: exponential,'
                 ENDIF
*   Drift velocity B component for large E/p.
                 IF(IYEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)') 'v || ExB: constant,'
                 ELSEIF(IYEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)') 'v || ExB: linear,'
                 ELSEIF(IYEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)') 'v || ExB: exponential,'
                 ENDIF
*   Lorentz angle for large E/p.
                 IF(IWEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)') '(v,E) angle: constant,'
                 ELSEIF(IWEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)') '(v,E) angle: linear,'
                 ELSEIF(IWEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     '(v,E) angle: exponential,'
                 ENDIF
*   Mobility for large E/p.
                 IF(IMEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)') 'ion mobility: constant,'
                 ELSEIF(IMEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)') 'ion mobility: linear,'
                 ELSEIF(IMEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'ion mobility: exponential,'
                 ENDIF
*   Longitudinal diffusion for large E/p.
                 IF(IDEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'longitudinal diffusion: constant,'
                 ELSEIF(IDEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'longitudinal diffusion: linear,'
                 ELSEIF(IDEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'longitudinal diffusion: exponential,'
                 ENDIF
*   Transverse diffusion for large E/p.
                 IF(IOEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'transverse diffusion: constant,'
                 ELSEIF(IOEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'transverse diffusion: linear,'
                 ELSEIF(IOEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'transverse diffusion: exponential,'
                 ENDIF
*   Diffusion tensor for large E/p.
                 IF(ISEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'diffusion tensor: constant,'
                 ELSEIF(ISEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'diffusion tensor: linear,'
                 ELSEIF(ISEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'diffusion tensor: exponential,'
                 ENDIF
*   Townsend coefficient for large E/p.
                 IF(IAEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'Townsend coefficient: constant,'
                 ELSEIF(IAEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'Townsend coefficient: linear,'
                 ELSEIF(IAEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'Townsend coefficient: exponential,'
                 ENDIF
*   Attachment coefficient for large E/p.
                 IF(IBEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'attachment coefficient: constant,'
                 ELSEIF(IBEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'attachment coefficient: linear,'
                 ELSEIF(IBEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'attachment coefficient: exponential,'
                 ENDIF
*   Dissociation coefficient for large E/p.
                 IF(IHEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'dissociation coefficient: constant,'
                 ELSEIF(IHEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'dissociation coefficient: linear,'
                 ELSEIF(IHEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'dissociation coefficient: exponential,'
                 ENDIF
*   Excitation rates for large E/p.
                 IF(IEEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'excitation rates: constant,'
                 ELSEIF(IEEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'excitation rates: linear,'
                 ELSEIF(IEEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'excitation rates: exponential,'
                 ENDIF
*   Ionisation rates for large E/p.
                 IF(IZEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'ionisation rates: constant.'
                 ELSEIF(IZEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'ionisation rates: linear.'
                 ELSEIF(IZEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'ionisation rates: exponential.'
                 ENDIF
*   Small values.
                 WRITE(LUNOUT,'(/1X,A)') ' The extrapolations'//
     -                ' to E/p below the first table point are done'//
     -                ' as follows:'
*   Drift velocity for small E/p.
                 IF(JVEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)') 'v || E: constant,'
                 ELSEIF(JVEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)') 'v || E: linear,'
                 ELSEIF(JVEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)') 'v || E: exponential,'
                 ENDIF
                 IF(JXEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)') 'v || Btrans: constant,'
                 ELSEIF(JXEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)') 'v || Btrans: linear,'
                 ELSEIF(JXEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)') 'v || Btrans: exponential,'
                 ENDIF
                 IF(JYEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)') 'v || ExB: constant,'
                 ELSEIF(JYEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)') 'v || ExB: linear,'
                 ELSEIF(JYEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)') 'v || ExB: exponential,'
                 ENDIF
*   Lorentz angle for small E/p.
                 IF(JWEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)') '(v,E) angle: constant,'
                 ELSEIF(JWEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)') '(v,E) angle: linear,'
                 ELSEIF(JWEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     '(v,E) angle: exponential,'
                 ENDIF
*   Ion mobility for small E/p.
                 IF(JMEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)') 'ion mobility: constant,'
                 ELSEIF(JMEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)') 'ion mobility: linear,'
                 ELSEIF(JMEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'ion mobility: exponential,'
                 ENDIF
*   Longitudinal diffusion for small E/p.
                 IF(JDEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'longitudinal diffusion: constant,'
                 ELSEIF(JDEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'longitudinal diffusion: linear,'
                 ELSEIF(JDEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'longitudinal diffusion: exponential,'
                 ENDIF
*   Transverse diffusion for small E/p.
                 IF(JOEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'transverse diffusion: constant,'
                 ELSEIF(JOEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'transverse diffusion: linear,'
                 ELSEIF(JOEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'transverse diffusion: exponential,'
                 ENDIF
*   Diffusion tensor for small E/p.
                 IF(JSEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'diffusion tensor: constant,'
                 ELSEIF(JSEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'diffusion tensor: linear,'
                 ELSEIF(JSEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'diffusion tensor: exponential,'
                 ENDIF
*   Townsend coefficient for small E/p.
                 IF(JAEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'Townsend coefficient: constant,'
                 ELSEIF(JAEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'Townsend coefficient: linear,'
                 ELSEIF(JAEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'Townsend coefficient: exponential,'
                 ENDIF
*   Attachment coefficient for small E/p.
                 IF(JBEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'attachment coefficient: constant,'
                 ELSEIF(JBEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'attachment coefficient: linear,'
                 ELSEIF(JBEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'attachment coefficient: exponential,'
                 ENDIF
*   Dissociation coefficient for small E/p.
                 IF(JHEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'dissociation coefficient: constant,'
                 ELSEIF(JHEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'dissociation coefficient: linear,'
                 ELSEIF(JHEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'dissociation coefficient: exponential,'
                 ENDIF
*   Excitation rates for small E/p.
                 IF(JEEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'excitation rates: constant,'
                 ELSEIF(JEEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'excitation rates: linear,'
                 ELSEIF(JEEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'excitation rates: exponential,'
                 ENDIF
*   Ionisation rates for small E/p.
                 IF(JZEXTR.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'ionisation rates: constant.'
                 ELSEIF(JZEXTR.EQ.1)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'ionisation rates: linear.'
                 ELSEIF(JZEXTR.EQ.2)THEN
                      WRITE(LUNOUT,'(5X,A)')
     -                     'ionisation rates: exponential.'
                 ENDIF
*   Number of points used for the extrapolations.
                 WRITE(LUNOUT,'('' '')')
                 IF(IVEXTR.EQ.0.OR.
     -                IXEXTR.EQ.0.OR.IYEXTR.EQ.0.OR.
     -                IAEXTR.EQ.0.OR.IBEXTR.EQ.0.OR.
     -                IMEXTR.EQ.0.OR.IWEXTR.EQ.0.OR.
     -                IDEXTR.EQ.0.OR.IOEXTR.EQ.0.OR.
     -                ISEXTR.EQ.0.OR.IHEXTR.EQ.0.OR.
     -                JVEXTR.EQ.0.OR.JDEXTR.EQ.0.OR.
     -                JXEXTR.EQ.0.OR.JYEXTR.EQ.0.OR.
     -                JAEXTR.EQ.0.OR.JBEXTR.EQ.0.OR.
     -                JMEXTR.EQ.0.OR.JWEXTR.EQ.0.OR.
     -                JOEXTR.EQ.0.OR.JSEXTR.EQ.0.OR.
     -                JHEXTR.EQ.0.OR.JEEXTR.EQ.0.OR.
     -                JZEXTR.EQ.0)
     -                WRITE(LUNOUT,'(1X,A)')
     -                ' Constant extrapolations use the last point.'
                 IF(IVEXTR.GT.0.OR.
     -                IXEXTR.GT.0.OR.IYEXTR.GT.0.OR.
     -                IAEXTR.GT.0.OR.IBEXTR.GT.0.OR.
     -                IMEXTR.GT.0.OR.IWEXTR.GT.0.OR.
     -                IDEXTR.GT.0.OR.IOEXTR.GT.0.OR.
     -                ISEXTR.GT.0.OR.IHEXTR.EQ.0.OR.
     -                JVEXTR.GT.0.OR.JDEXTR.GT.0.OR.
     -                JXEXTR.GT.0.OR.JYEXTR.GT.0.OR.
     -                JAEXTR.GT.0.OR.JBEXTR.GT.0.OR.
     -                JMEXTR.GT.0.OR.JWEXTR.GT.0.OR.
     -                JOEXTR.GT.0.OR.JSEXTR.GT.0.OR.
     -                JHEXTR.GT.0.OR.JEEXTR.GT.0.OR.
     -                JZEXTR.GT.0)
     -                WRITE(LUNOUT,'(1X,A)')
     -                ' Linear and exponential extrapolations are'//
     -                ' based on the last 2 points.'
                 WRITE(LUNOUT,'('' '')')
            ENDIF
*   Read the string if there are arguments.
            INEXT=2
            DO 710 I=2,NWORD
            IF(I.LT.INEXT)GOTO 710
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'The method should be specified')
                 GOTO 710
            ELSEIF(INPCMP(I+1,'C#ONSTANT').NE.0)THEN
                 IEXTRR=0
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'E#XPONENTIALLY').NE.0)THEN
                 IEXTRR=2
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'L#INEARLY').NE.0)THEN
                 IEXTRR=1
                 INEXT=I+2
            ELSE
                 CALL INPMSG(I,'Valid method not specified.   ')
                 CALL INPMSG(I+1,'Unknown extrapolation method. ')
                 GOTO 710
            ENDIF
            IF(INPCMP(I,'H#IGH-DR#IFT-#VELOCITY')+
     -           INPCMP(I,'DR#IFT-#VELOCITY').NE.0)THEN
                 IVEXTR=IEXTRR
            ELSEIF(INPCMP(I,'H#IGH-DI#FFUSION-#COEFFICIENT')+
     -           INPCMP(I,'DI#FFUSION-#COEFFICIENT')+
     -           INPCMP(I,'H#IGH-LONG#ITUDINAL-'//
     -                'DI#FFUSION-#COEFFICIENT')+
     -           INPCMP(I,'LONG#ITUDINAL-'//
     -                'DI#FFUSION-#COEFFICIENT').NE.0)THEN
                 IDEXTR=IEXTRR
            ELSEIF(INPCMP(I,'H#IGH-TRANS#VERSE-'//
     -                'DI#FFUSION-#COEFFICIENT')+
     -           INPCMP(I,'TRANS#VERSE-'//
     -                'DI#FFUSION-#COEFFICIENT').NE.0)THEN
                 IOEXTR=IEXTRR
            ELSEIF(INPCMP(I,'H#IGH-DI#FFUSION-TENS#OR')+
     -           INPCMP(I,'DI#FFUSION-TENS#OR').NE.0)THEN
                 ISEXTR=IEXTRR
            ELSEIF(INPCMP(I,'H#IGH-LOR#ENTZ-#ANGLE')+
     -           INPCMP(I,'LOR#ENTZ-#ANGLE').NE.0)THEN
                 IWEXTR=IEXTRR
            ELSEIF(INPCMP(I,'H#IGH-T#OWNSEND-#COEFFICIENT')+
     -           INPCMP(I,'T#OWNSEND-#COEFFICIENT').NE.0)THEN
                 IAEXTR=IEXTRR
            ELSEIF(INPCMP(I,'H#IGH-A#TTACHMENT-#COEFFICIENT')+
     -           INPCMP(I,'A#TTACHMENT-#COEFFICIENT').NE.0)THEN
                 IBEXTR=IEXTRR
            ELSEIF(INPCMP(I,'H#IGH-DISS#OCIATION-#COEFFICIENT')+
     -           INPCMP(I,'DISS#OCIATION-#COEFFICIENT')+
     -           INPCMP(I,'H#IGH-ION-DISS#OCIATION-#COEFFICIENT')+
     -           INPCMP(I,'ION-DISS#OCIATION-#COEFFICIENT').NE.0)THEN
                 IHEXTR=IEXTRR
            ELSEIF(INPCMP(I,'H#IGH-ION-MOB#ILITY')+
     -           INPCMP(I,'ION-MOB#ILITY').NE.0)THEN
                 IMEXTR=IEXTRR
            ELSEIF(INPCMP(I,'H#IGH-EXC#ITATION-#RATES')+
     -           INPCMP(I,'EXC#ITATION-#RATES').NE.0)THEN
                 IEEXTR=IEXTRR
            ELSEIF(INPCMP(I,'H#IGH-ION#ISATION-#RATES')+
     -           INPCMP(I,'ION#ISATION-#RATES').NE.0)THEN
                 IZEXTR=IEXTRR
            ELSEIF(INPCMP(I,'L#OW-DR#IFT-#VELOCITY').NE.0)THEN
                 JVEXTR=IEXTRR
            ELSEIF(INPCMP(I,'L#OW-DI#FFUSION-#COEFFICIENT')+
     -           INPCMP(I,'L#OW-LONG#ITUDINAL-'//
     -           'DI#FFUSION-#COEFFICIENT').NE.0)THEN
                 JDEXTR=IEXTRR
            ELSEIF(INPCMP(I,'L#OW-TRANS#VERSE-'//
     -           'DI#FFUSION-#COEFFICIENT').NE.0)THEN
                 JOEXTR=IEXTRR
            ELSEIF(INPCMP(I,'L#OW-DI#FFUSION-TENS#OR').NE.0)THEN
                 JSEXTR=IEXTRR
            ELSEIF(INPCMP(I,'L#OW-LOR#ENTZ-#ANGLE').NE.0)THEN
                 JWEXTR=IEXTRR
            ELSEIF(INPCMP(I,'L#OW-T#OWNSEND-#COEFFICIENT').NE.0)THEN
                 JAEXTR=IEXTRR
            ELSEIF(INPCMP(I,'L#OW-A#TTACHMENT-#COEFFICIENT').NE.0)THEN
                 JBEXTR=IEXTRR
            ELSEIF(INPCMP(I,'LOW-ION-DISS#OCIATION-#COEFFICIENT')+
     -           INPCMP(I,'LOW-DISS#OCIATION-#COEFFICIENT').NE.0)THEN
                 JHEXTR=IEXTRR
            ELSEIF(INPCMP(I,'L#OW-ION-MOB#ILITY').NE.0)THEN
                 JMEXTR=IEXTRR
            ELSEIF(INPCMP(I,'L#OW-EXC#ITATION-#RATES').NE.0)THEN
                 JEEXTR=IEXTRR
            ELSEIF(INPCMP(I,'L#OW-ION#ISATION-#RATES').NE.0)THEN
                 JZEXTR=IEXTRR
            ELSE
                 CALL INPMSG(I,'Unknown object to extrapolate.')
            ENDIF
710         CONTINUE
            CALL INPERR
*** Set GAS-ID if this is a keyword.
       ELSEIF(INPCMP(1,'GAS-ID#ENTIFIER').NE.0)THEN
            IF(NWORD.EQ.1.AND.GASID.EQ.' ')THEN
                 WRITE(LUNOUT,'(2X/''The gas identification has'',
     -                '' not yet been set.''/)')
            ELSEIF(NWORD.EQ.1)THEN
                 WRITE(LUNOUT,'(2X/''The current gas identification'',
     -                '' is: '',A/)') GASID
            ELSE
                 CALL INPSTR(2,2,STRING,NC)
                 IF(NC.GT.80)PRINT *,' !!!!!! GASINP WARNING : The'//
     -                ' gas identifier is truncated to 80 characters.'
                 GASID=STRING(1:MIN(NC,80))
            ENDIF
*** Read the gas from dataset, if GET is a keyword.
       ELSEIF(INPCMP(1,'GET').NE.0)THEN
            CALL GASGET(IFAIL1)
            IF(IFAIL1.NE.0)CALL GASINT
*** Heed gas mixing.
       ELSEIF(INPCMP(1,'HEED').NE.0)THEN
            CALL GASHEE(IFAIL1)
*   Heed solids
       ELSEIF(INPCMP(1,'MAT#ERIAL').NE.0)THEN
            CALL GASSOL(IFAIL1)
*** SRIM tables
       ELSEIF(INPCMP(1,'SRIM').NE.0)THEN
            CALL GASSRM(IFAIL1)
*** TRIM tables  TRIMCAT Module
       ELSEIF(INPCMP(1,'TRIM').NE.0)THEN
            CALL GASTRM(IFAIL1)
*** Set the interpolation method.
       ELSEIF(INPCMP(1,'INT#ERPOLATIONS')+
     -      INPCMP(1,'INT#ERPOLATE').NE.0)THEN
*   Print the current settings if entered without argument.
            IF(NWORD.EQ.1)THEN
                 WRITE(LUNOUT,'(/1X,A)')
     -                ' Currently the interpolation methods'//
     -                ' are chosen as follows:'
*   Drift velocity.
                 IF(IVMETH.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,''v || E: Cubic splines,'')')
                 ELSE
                      WRITE(LUNOUT,'(5X,''v || E: Newton'',
     -                     '' interpolation of order'',I3,'','')')
     -                     IVMETH
                 ENDIF
                 IF(IXMETH.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,''v || Btrans: Cubic'',
     -                     '' splines,'')')
                 ELSE
                      WRITE(LUNOUT,'(5X,''v || Btrans: Newton'',
     -                     '' interpolation of order'',I3,'','')')
     -                     IXMETH
                 ENDIF
                 IF(IYMETH.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,''v || ExB: Cubic'',
     -                     '' splines,'')')
                 ELSE
                      WRITE(LUNOUT,'(5X,''v || ExB: Newton'',
     -                     '' interpolation of order'',I3,'','')')
     -                     IYMETH
                 ENDIF
*   Lorentz angle.
                 IF(IWMETH.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,''(v,E) angle: Cubic'',
     -                     '' splines,'')')
                 ELSE
                      WRITE(LUNOUT,'(5X,''(v,E) angle: Newton'',
     -                     '' interpolation of order'',I3,'','')')
     -                     IWMETH
                 ENDIF
*   Ion mobility.
                 IF(IMMETH.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,''ion mobility: Cubic'',
     -                     '' splines,'')')
                 ELSE
                      WRITE(LUNOUT,'(5X,''ion mobility: Newton'',
     -                     '' interpolation of order'',I3,'','')')
     -                     IMMETH
                 ENDIF
*   Longitudinal diffusion.
                 IF(IDMETH.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,''longitudinal diffusion:'',
     -                     '' Cubic splines,'')')
                 ELSE
                      WRITE(LUNOUT,'(5X,''longitudinal diffusion:'',
     -                     '' Newton interpolation of order'',I3,
     -                     '','')') IDMETH
                 ENDIF
*   Transverse diffusion.
                 IF(IOMETH.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,''transverse diffusion:'',
     -                     '' Cubic splines,'')')
                 ELSE
                      WRITE(LUNOUT,'(5X,''transverse diffusion:'',
     -                     '' Newton interpolation of order'',I3,
     -                     '','')') IOMETH
                 ENDIF
*   Diffusion tensor.
                 IF(ISMETH.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,''diffusion tensor:'',
     -                     '' Cubic splines,'')')
                 ELSE
                      WRITE(LUNOUT,'(5X,''diffusion tensor:'',
     -                     '' Newton interpolation of order'',I3,
     -                     '','')') ISMETH
                 ENDIF
*   Townsend coefficient.
                 IF(IAMETH.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,''Townsend coefficient:'',
     -                     '' Cubic splines,'')')
                 ELSE
                      WRITE(LUNOUT,'(5X,''Townsend coefficient:'',
     -                     '' Newton interpolation of order'',I3,
     -                     '','')') IAMETH
                 ENDIF
*   Attachment coefficient.
                 IF(IBMETH.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,''attachment coefficient:'',
     -                     '' Cubic splines.'')')
                 ELSE
                      WRITE(LUNOUT,'(5X,''attachment coefficient:'',
     -                     '' Newton interpolation of order'',I3,
     -                     ''.'')') IBMETH
                 ENDIF
*   Ion dissociation coefficient.
                 IF(IHMETH.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,''dissociation coefficient:'',
     -                     '' Cubic splines.'')')
                 ELSE
                      WRITE(LUNOUT,'(5X,''dissociation coefficient:'',
     -                     '' Newton interpolation of order'',I3,
     -                     ''.'')') IHMETH
                 ENDIF
*   Excitations.
                 IF(IEMETH.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,''excitation rates:'',
     -                     '' Cubic splines.'')')
                 ELSE
                      WRITE(LUNOUT,'(5X,''excitation rates:'',
     -                     '' Newton interpolation of order'',I3,
     -                     ''.'')') IEMETH
                 ENDIF
*   Ionisations.
                 IF(IZMETH.EQ.0)THEN
                      WRITE(LUNOUT,'(5X,''ionisation rates:'',
     -                     '' Cubic splines.'')')
                 ELSE
                      WRITE(LUNOUT,'(5X,''ionisation rates:'',
     -                     '' Newton interpolation of order'',I3,
     -                     ''.'')') IZMETH
                 ENDIF
                 WRITE(LUNOUT,'('' '')')
            ENDIF
*   Read the string if there are arguments.
            INEXT=2
            DO 720 I=2,NWORD
            IF(I.LT.INEXT)GOTO 720
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'The method should be specified')
                 GOTO 720
            ELSEIF(INPCMP(I+1,'SPL#INES').NE.0)THEN
                 IMETHR=0
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'LIN#EAR').NE.0)THEN
                 IMETHR=1
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'QUA#DRATIC').NE.0)THEN
                 IMETHR=2
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'CUB#IC').NE.0)THEN
                 IMETHR=3
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'NEW#TON-#POLYNOMIALS').NE.0)THEN
                 IF(NWORD.LT.I+2.OR.INPTYP(I+2).NE.1)THEN
                      IMETHR=2
                      INEXT=I+2
                 ELSE
                      CALL INPCHK(I+2,1,IFAIL1)
                      CALL INPRDI(I+2,IMETHR,2)
                      IF(IMETHR.LT.1)THEN
                           CALL INPMSG(I+2,
     -                          'The order must be 1 or larger.')
                           IMETHR=2
                      ENDIF
                      INEXT=I+3
                 ENDIF
            ELSE
                 CALL INPMSG(I,'Not followed by a method.     ')
                 CALL INPMSG(I+1,'Unknown interpolation method. ')
                 INEXT=I+2
                 GOTO 720
            ENDIF
            IF(INPCMP(I,'DR#IFT-#VELOCITY').NE.0)THEN
                 IVMETH=IMETHR
            ELSEIF(INPCMP(I,'LOR#ENTZ-#ANGLE').NE.0)THEN
                 IWMETH=IMETHR
            ELSEIF(INPCMP(I,'ION-MOB#ILITY').NE.0)THEN
                 IMMETH=IMETHR
            ELSEIF(INPCMP(I,'ION-DISS#OCIATION-#COEFFICIENT')+
     -           INPCMP(I,'DISS#OCIATION-#COEFFICIENT').NE.0)THEN
                 IHMETH=IMETHR
            ELSEIF(INPCMP(I,'DI#FFUSION-#COEFFICIENT')+
     -           INPCMP(I,'LONG#ITUDINAL-'//
     -           'DI#FFUSION-#COEFFICIENT').NE.0)THEN
                 IDMETH=IMETHR
            ELSEIF(INPCMP(I,'TRANS#VERSE-'//
     -           'DI#FFUSION-#COEFFICIENT').NE.0)THEN
                 IOMETH=IMETHR
            ELSEIF(INPCMP(I,'DI#FFUSION-TENS#OR').NE.0)THEN
                 ISMETH=IMETHR
            ELSEIF(INPCMP(I,'T#OWNSEND-#COEFFICIENT').NE.0)THEN
                 IAMETH=IMETHR
            ELSEIF(INPCMP(I,'A#TTACHMENT-#COEFFICIENT').NE.0)THEN
                 IBMETH=IMETHR
            ELSEIF(INPCMP(I,'EXC#ITATION-#RATES').NE.0)THEN
                 IEMETH=IMETHR
            ELSEIF(INPCMP(I,'ION#ISATION-#RATES').NE.0)THEN
                 IZMETH=IMETHR
            ELSE
                 CALL INPMSG(I,'Unknown object to interpolate.')
            ENDIF
720         CONTINUE
            CALL INPERR
*** Call routine ISOBUT to transfer data if ISOBUTANE is a keyword.
       ELSEIF(INPCMP(1,'ISO#BUTANE').NE.0)THEN
            CALL ISOBUT
*** Magboltz gas mixing.
       ELSEIF(INPCMP(1,'MAG#BOLTZ').NE.0)THEN
            CALL GASBMC(IFAIL1)
            IF(IFAIL1.NE.0)CALL GASINT
*** Gas data merging.
       ELSEIF(INPCMP(1,'MER#GE').NE.0)THEN
            CALL GASMRG(IFAIL1)
            IF(IFAIL1.NE.0)CALL GASINT
*** Call routine METHAN to transfer data if METHANE is a keyword.
       ELSEIF(INPCMP(1,'MET#HANE').NE.0)THEN
            CALL METHAN
*** Gas mixing.
       ELSEIF(INPCMP(1,'MIX').NE.0)THEN
            CALL GASMIX
*** Identify the options if OPTION is a keyword.
       ELSEIF(INPCMP(1,'OPT#IONS').NE.0)THEN
            IF(NWORD.EQ.1)PRINT 1050,LGASPL,LGASPR
            DO 400 I=2,NWORD
*   Check for gas plot options.
            IF(INPCMP(I,'NOG#AS-PL#OT').NE.0)THEN
                 LGASPL=.FALSE.
            ELSEIF(INPCMP(I,'G#AS-PL#OT').NE.0)THEN
                 LGASPL=.TRUE.
*   Check for gas print options.
            ELSEIF(INPCMP(I,'NOG#AS-PR#INT').NE.0)THEN
                 LGASPR=.FALSE.
            ELSEIF(INPCMP(I,'G#AS-PR#INT').NE.0)THEN
                 LGASPR=.TRUE.
*   Option is not known.
            ELSE
                 CALL INPMSG(I,'The option is not known.      ')
            ENDIF
400         CONTINUE
            CALL INPERR
*** Find the gas-parameter setting instructions.
       ELSEIF(INPCMP(1,'PAR#AMETERS').NE.0)THEN
            IF(NWORD.LT.3)THEN
                 WRITE(LUNOUT,'(''  CURRENT SETTINGS OF'',
     -                '' SOME GAS PARAMETERS: ''//
     -                ''  Number of protons in one molecule : '',F5.0/
     -                ''  Atomic number of the gas          : '',F5.0/
     -                ''  Density                           : '',E10.3,
     -                '' [g/cm3]''//
     -                ''  Average number of clusters per cm : '',F10.2/
     -                ''  Most probable energy loss per cm  : '',F10.2,
     -                '' [eV/cm]''/
     -                ''  Energy needed for one ion pair    : '',F10.2,
     -                '' [eV]''/)')
     -                A,Z,RHO,CMEAN,EMPROB,EPAIR
                 IF(DLION.LT.0)THEN
                      WRITE(LUNOUT,'(
     -                     ''  Longitudinal ion diffusion        :'',
     -                     '' Thermal'')')
                 ELSE
                      WRITE(LUNOUT,'(
     -                     ''  Longitudinal ion diffusion        :'',
     -                     F10.3,'' [cm for 1 cm of drift]'')') DLION
                 ENDIF
                 IF(DTION.LT.0)THEN
                      WRITE(LUNOUT,'(
     -                     ''  Transverse ion diffusion          :'',
     -                     '' Thermal'')')
                 ELSE
                      WRITE(LUNOUT,'(
     -                     ''  Transverse ion diffusion          :'',
     -                     F10.3,'' [cm for 1 cm of drift]'')') DTION
                 ENDIF
            ENDIF
            DO 500 I=2,NWORD-1,2
            IF(INPCMP(I,'A').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,A,-1.0)
                 IF(A.LE.0.0.AND.IFAIL1.EQ.0)
     -                CALL INPMSG(I+1,'The atomic number must be > 0.')
            ELSEIF(INPCMP(I,'Z').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,Z,-1.0)
                 IF(Z.LE.0.0.AND.IFAIL1.EQ.0)
     -                CALL INPMSG(I+1,'The nuclear charge is not > 0.')
            ELSEIF(INPCMP(I,'E#NERGY-M#OST-#PROBABLE')+
     -           INPCMP(I,'M#OST-PR#OBABLE-E#NERGY-#LOSS').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,EMPROB,-1.0)
                 GASOK(5)=.TRUE.
                 CLSTYP='LANDAU'
                 IF(EMPROB.LE.0.0.AND.IFAIL1.EQ.0)
     -                CALL INPMSG(I+1,'The energy loss should be > 0.')
            ELSEIF(INPCMP(I,'ME#AN').NE.0.OR.
     -             INPCMP(I,'N-#MEAN').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,CMEAN,-1.0)
                 IF(CMEAN.LE.0.0.AND.IFAIL1.EQ.0)
     -                CALL INPMSG(I+1,'The cluster spacing is not > 0')
            ELSEIF(INPCMP(I,'P#AIR-C#REATION-#ENERGY')+
     -             INPCMP(I,'E#NERGY-P#AIR').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,EPAIR,-1.0)
                 GASOK(5)=.TRUE.
                 CLSTYP='LANDAU'
                 IF(EPAIR.LE.0.0.AND.IFAIL1.EQ.0)
     -                CALL INPMSG(I+1,'The pair energy should be > 0.')
            ELSEIF(INPCMP(I,'R#HO').NE.0.OR.
     -             INPCMP(I,'D#ENSITY').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,RHO,-1.0)
                 IF(RHO.LE.0.0.AND.IFAIL1.EQ.0)THEN
                      CALL INPMSG(I+1,'The density should be > 0.    ')
                 ELSE
                      IF(SRMDEN.GT.0)
     -                     PRINT *,' ------ GASINP MESSAGE : SRIM gas'//
     -                     ' density replaced by new value.'
                      SRMDEN=RHO
                 ENDIF
            ELSEIF(INPCMP(I,'TR#ANSVERSE-ION-DIFF#USION').NE.0)THEN
                 IF(INPCMP(I+1,'TH#ERMAL').NE.0)THEN
                      DTION=-1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,DIONR,-1.0)
                      IF(DIONR.LT.0.0.AND.IFAIL1.EQ.0)THEN
                           CALL INPMSG(I+1,
     -                          'The diffusion should be > 0.  ')
                      ELSEIF(DIONR.GE.0)THEN
                           DTION=DIONR
                      ENDIF
                 ENDIF
            ELSEIF(INPCMP(I,'LONG#ITUDINAL-ION-DIFF#USION').NE.0)THEN
                 IF(INPCMP(I+1,'TH#ERMAL').NE.0)THEN
                      DLION=-1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,DIONR,-1.0)
                      IF(DIONR.LT.0.0.AND.IFAIL1.EQ.0)THEN
                           CALL INPMSG(I+1,
     -                          'The diffusion should be > 0.  ')
                      ELSEIF(DIONR.GE.0)THEN
                           DLION=DIONR
                      ENDIF
                 ENDIF
            ELSEIF(INPCMP(I,'ION-DIFF#USION').NE.0)THEN
                 IF(INPCMP(I+1,'TH#ERMAL').NE.0)THEN
                      DLION=-1
                      DTION=-1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,DIONR,-1.0)
                      IF(DIONR.LT.0.0.AND.IFAIL1.EQ.0)THEN
                           CALL INPMSG(I+1,
     -                          'The diffusion should be > 0.  ')
                      ELSEIF(DIONR.GE.0)THEN
                           DTION=DIONR
                           DLION=DIONR
                      ENDIF
                 ENDIF
            ELSE
                 CALL INPMSG(I,'The keyword is not known.     ')
                 CALL INPMSG(I+1,'See the preceding message.    ')
            ENDIF
500         CONTINUE
*   Check for an extra keyword.
            IF(NWORD.EQ.2*INT(REAL(NWORD)/2.0).AND.NWORD.GT.1)
     -           CALL INPMSG(NWORD,'Extra keyword cannot be used. ')
            CALL INPERR
*** If PRESSURE is a keyword, find the pressure.
       ELSEIF(INPCMP(1,'PR#ESSURE').NE.0)THEN
            IF(NWORD.EQ.1)THEN
                 CALL OUTFMT(PGAS,2,STRAUX,NCAUX,'LEFT')
                 WRITE(LUNOUT,'(''  The pressure of the gas is '',
     -                A,'' Torr.'')') STRAUX(1:NCAUX)
            ELSEIF(NWORD.EQ.2.OR.NWORD.EQ.3)THEN
                 IF(NWORD.EQ.3)THEN
                      CALL INPSTR(3,3,UNIT,NCUNIT)
                 ELSE
                      UNIT='TORR'
                      NCUNIT=4
                 ENDIF
                 CALL INPCHK(2,2,IFAIL1)
                 CALL INPRDR(2,PGASRR,760.0)
                 CALL UNITS(PGASRR,UNIT(1:NCUNIT),PGASR,'TORR',IFAIL2)
                 IF(IFAIL2.NE.0)THEN
                      CALL INPMSG(3,'Not a valid pressure unit.')
                 ELSEIF(PGASR.LE.0.0.AND.IFAIL1.EQ.0)THEN
                      CALL INPMSG(2,'The pressure must be positive.')
                      IFAIL1=1
                 ELSE
                      PGAS=PGASR
                 ENDIF
                 CALL INPERR
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! GASINP WARNING : The',
     -                ' PRESSURE statement is ignored.'
            ELSE
                  PRINT *,' !!!!!! GASINP WARNING : PRESSURE takes',
     -                    ' a single argument ; excess ignored.'
            ENDIF
*** The RESET instruction.
       ELSEIF(INPCMP(1,'RES#ET')+INPCMP(1,'DEL#ETE').NE.0)THEN
            DO 60 I=2,NWORD
*   Drift velocity.
            IF(INPCMP(I,'DR#IFT-#VELOCITY').NE.0)THEN
                 GASOK(1)=.FALSE.
                 GASOK(9)=.FALSE.
                 GASOK(10)=.FALSE.
                 IVEXTR=1
                 JVEXTR=0
                 IVMETH=2
                 IXEXTR=1
                 JXEXTR=0
                 IXMETH=2
                 IYEXTR=1
                 JYEXTR=0
                 IYMETH=2
*   Ion mobility.
            ELSEIF(INPCMP(I,'ION-MOB#ILITY')+
     -           INPCMP(I,'MOB#ILITY').NE.0)THEN
                 GASOK(2)=.FALSE.
                 IMEXTR=1
                 JMEXTR=0
                 IMMETH=2
*   Diffusion.
            ELSEIF(INPCMP(I,'LONG#ITUDINAL-DIFF#USION').NE.0)THEN
                 GASOK(3)=.FALSE.
                 IDEXTR=1
                 JDEXTR=0
                 IDMETH=2
            ELSEIF(INPCMP(I,'TRANS#VERSE-DIFF#USION').NE.0)THEN
                 GASOK(8)=.FALSE.
                 IOEXTR=1
                 JOEXTR=0
                 IOMETH=2
            ELSEIF(INPCMP(I,'DIFF#USION-TENS#OR').NE.0)THEN
                 GASOK(11)=.FALSE.
                 ISEXTR=1
                 JSEXTR=0
                 ISMETH=2
            ELSEIF(INPCMP(I,'DIFF#USION').NE.0)THEN
                 GASOK(8)=.FALSE.
                 IOEXTR=1
                 JOEXTR=0
                 IOMETH=2
                 GASOK(3)=.FALSE.
                 IDEXTR=1
                 JDEXTR=0
                 IDMETH=2
                 GASOK(11)=.FALSE.
                 ISEXTR=1
                 JSEXTR=0
                 ISMETH=2
*   Townsend coefficients.
            ELSEIF(INPCMP(I,'TOWN#SEND-#COEFFICIENTS').NE.0)THEN
                 GASOK(4)=.FALSE.
                 IAEXTR=1
                 JAEXTR=0
                 IAMETH=2
*   Clustering data.
            ELSEIF(INPCMP(I,'CLUST#ERING-#DATA').NE.0)THEN
                 GASOK(5)=.FALSE.
                 HEEDOK=.FALSE.
                 SRIMOK=.FALSE.
                 CALL SRMINT
                 NCLS=0
                 CLSTYP='NOT SET'
                 FCNCLS=' '
                 NFCLS=1
                 A=0
                 Z=0
                 EMPROB=0
                 EPAIR=0
                 RHO=0
                 CMEAN=0
*   Attachment coefficients.
            ELSEIF(INPCMP(I,'ATT#ACHMENT-#COEFFICIENTS').NE.0)THEN
                 GASOK(6)=.FALSE.
                 IBEXTR=1
                 JBEXTR=0
                 IBMETH=2
*   Ion dissociation coefficients.
            ELSEIF(INPCMP(I,'ION-DISS#OCIATION-#COEFFICIENTS')+
     -           INPCMP(I,'DISS#OCIATION-#COEFFICIENTS').NE.0)THEN
                 GASOK(12)=.FALSE.
                 IHEXTR=1
                 JHEXTR=0
                 IHMETH=2
*   Lorentz angle.
            ELSEIF(INPCMP(I,'LOR#ENTZ-#ANGLES').NE.0)THEN
                 GASOK(7)=.FALSE.
                 IWEXTR=1
                 JWEXTR=0
                 IWMETH=2
*   Excitation rates.
            ELSEIF(INPCMP(I,'EXC#ITATION-#RATES').NE.0)THEN
                 GASOK(15)=.FALSE.
                 IEEXTR=1
                 JEEXTR=0
                 IEMETH=2
*   Ionisation rates.
            ELSEIF(INPCMP(I,'ION#ISATION-#RATES').NE.0)THEN
                 GASOK(16)=.FALSE.
                 IZEXTR=1
                 JZEXTR=0
                 IZMETH=2
*   Gas identifier.
            ELSEIF(INPCMP(I,'GAS-ID#ENTIFIER').NE.0)THEN
                 GASID=' '
*   All tables.
            ELSEIF(INPCMP(I,'TAB#LES').NE.0)THEN
                 NGAS=0
                 TAB2D=.FALSE.
                 GASOK(1)=.FALSE.
                 GASOK(2)=.FALSE.
                 GASOK(3)=.FALSE.
                 GASOK(4)=.FALSE.
                 GASOK(6)=.FALSE.
                 GASOK(7)=.FALSE.
                 GASOK(8)=.FALSE.
                 GASOK(9)=.FALSE.
                 GASOK(10)=.FALSE.
                 GASOK(11)=.FALSE.
                 GASOK(12)=.FALSE.
                 GASOK(15)=.FALSE.
                 GASOK(16)=.FALSE.
                 FCNTAB=' '
                 NFTAB=1
                 IVEXTR=1
                 IXEXTR=1
                 IYEXTR=1
                 IDEXTR=1
                 IOEXTR=1
                 ISEXTR=1
                 IAEXTR=1
                 IBEXTR=1
                 IMEXTR=1
                 IWEXTR=1
                 IEEXTR=1
                 IZEXTR=1
                 JVEXTR=0
                 JXEXTR=0
                 JYEXTR=0
                 JDEXTR=0
                 JOEXTR=0
                 JSEXTR=0
                 JAEXTR=0
                 JBEXTR=0
                 JMEXTR=0
                 JWEXTR=0
                 JEEXTR=0
                 JZEXTR=0
                 IVMETH=2
                 IXMETH=2
                 IYMETH=2
                 IDMETH=2
                 IOMETH=2
                 ISMETH=2
                 IAMETH=2
                 IBMETH=2
                 IMMETH=2
                 IWMETH=2
                 IEMETH=2
                 IZMETH=2
*   All the rest is not known.
            ELSE
                 CALL INPMSG(I,'Is not known, can not be reset')
            ENDIF
60          CONTINUE
*   Reset everything.
            IF(NWORD.EQ.1)CALL GASINT
*   Dump error messages.
            CALL INPERR
*** Read gas table if TABLE is a keyword.
       ELSEIF(INPCMP(1,'TAB#LE').NE.0)THEN
*   Initialize the various pointers: the function data.
            NFTAB=1
            FCNTAB=' '
            IFCN=0
*   The table data.
            ITAB=0
            IDIFF=0
            ITRANS=0
            IDRIFT=0
            IVB=0
            IVEXB=0
            IEP=0
            ITOWN=0
            IATT=0
            IDISS=0
            IMOBIL=0
            ILOREN=0
*   Table type.
            TAB2D=.FALSE.
*   E range.
            IRANGE=0
            EPMIN=100.0/PGAS
            EPMAX=100000.0/PGAS
            EPLOG=.TRUE.
            NGAS=20
*   E-B angles.
            IF(MAGOK)THEN
                 BANGMN=0
                 BANGMX=PI/2
                 NBANG=4
            ELSE
                 BANGMN=PI/2
                 BANGMX=PI/2
                 NBANG=1
            ENDIF
*   B field magnitude.
            IF(MAGOK)THEN
                 IF(ABS((BFMIN-BFMAX)*BSCALE).LT.0.0001)THEN
                      BTABMN=BFMIN*BSCALE
                      BTABMX=BFMAX*BSCALE
                      NBTAB=1
                 ELSE
                      BTABMN=BFMIN*BSCALE
                      BTABMX=BFMAX*BSCALE
                      NBTAB=6
                 ENDIF
            ELSE
                 BTABMN=0
                 BTABMX=0
                 NBTAB=1
            ENDIF
*   Reset the relevant GASOK bits.
            GASOK(1)=.FALSE.
            GASOK(2)=.FALSE.
            GASOK(3)=.FALSE.
            GASOK(4)=.FALSE.
            GASOK(6)=.FALSE.
            GASOK(7)=.FALSE.
            GASOK(8)=.FALSE.
            GASOK(9)=.FALSE.
            GASOK(10)=.FALSE.
            GASOK(12)=.FALSE.
**  Flag the words.
            DO 600 I=1,NWORD+3
            IF(I.GT.NWORD)THEN
                 FLAG(I)=.TRUE.
            ELSE
                 IF(INPCMP(I,'E/P')+
     -                INPCMP(I,'A#TTACHMENT-#COEFFICIENT')+
     -                INPCMP(I,'DI#FFUSION-#COEFFICIENT')+
     -                INPCMP(I,'ION-DISS#OCIATION-#COEFFICIENT')+
     -                INPCMP(I,'DISS#OCIATION-#COEFFICIENT')+
     -                INPCMP(I,'LONG#ITUDINAL-'//
     -                     'DI#FFUSION-#COEFFICIENT')+
     -                INPCMP(I,'TRANS#VERSE-'//
     -                     'DI#FFUSION-#COEFFICIENT')+
     -                INPCMP(I,'DUM#MY')+
     -                INPCMP(I,'DR#IFT-#VELOCITY')+
     -                INPCMP(I,'E-VEL#OCITY')+
     -                INPCMP(I,'B#TRANSVERSE-VEL#OCITY')+
     -                INPCMP(I,'EXB-VEL#OCITY')+
     -                INPCMP(I,'LOR#ENTZ-#ANGLE')+
     -                INPCMP(I,'ION-MOB#ILITY')+
     -                INPCMP(I,'T#OWNSEND-#COEFFICIENT')+
     -                INPCMP(I,'N-E/P')+INPCMP(I,'E/P-R#ANGE')+
     -                INPCMP(I,'LIN#EAR-#E/P-#SCALE')+
     -                INPCMP(I,'LOG#ARITHMIC-#E/P-#SCALE')+
     -                INPCMP(I,'N-B')+INPCMP(I,'B-R#ANGE')+
     -                INPCMP(I,'B-F#IELD')+
     -                INPCMP(I,'N-ANG#LE')+INPCMP(I,'ANG#LE-R#ANGE')+
     -                INPCMP(I,'ANG#LE').NE.0)THEN
                      FLAG(I)=.TRUE.
                 ELSE
                      FLAG(I)=.FALSE.
                 ENDIF
            ENDIF
600         CONTINUE
**  Read the command string, segment by segment.
            INEXT=2
            OK=.TRUE.
            DO 610 I=2,NWORD
            IF(I.LT.INEXT)GOTO 610
*   Skip dummy fields.
            IF(INPCMP(I,'DUM#MY').NE.0)THEN
                 IF(FLAG(I+1))THEN
                      ITAB=ITAB+1
                      INEXT=I+1
                 ELSE
                      INEXT=I+2
                 ENDIF
*   Check for E/p.
            ELSEIF(INPCMP(I,'E/P').NE.0)THEN
                 IF(IEP.NE.0)THEN
                      CALL INPMSG(I,'Has already been entered.     ')
                      OK=.FALSE.
                 ELSE
                      IF(FLAG(I+1))THEN
                           ITAB=ITAB+1
                           IEP=ITAB
                           INEXT=I+1
                      ELSEIF(I.LT.NWORD)THEN
                           CALL INPMSG(I,
     -                          'E/p cannot be a function.     ')
                           OK=.FALSE.
                      ENDIF
                 ENDIF
*   Check for the attachment coefficient.
            ELSEIF(INPCMP(I,'A#TTACHMENT-#COEFFICIENT').NE.0)THEN
                 IF(IATT.NE.0)THEN
                      CALL INPMSG(I,'Has already been entered.     ')
                      OK=.FALSE.
                 ELSE
                      IF(FLAG(I+1))THEN
                           ITAB=ITAB+1
                           IATT=ITAB
                      ELSEIF(I.LT.NWORD)THEN
                           IFCN=IFCN+1
                           IATT=-IFCN
                           CALL INPSTR(I+1,I+1,STRING,NC)
                           FCNTAB(NFTAB:NFTAB+NC)=STRING(1:NC)//','
                           NFTAB=NFTAB+NC+1
                           INEXT=I+2
                      ENDIF
                      GASOK(6)=.TRUE.
                 ENDIF
*   Check for the ion dissociation coefficient.
            ELSEIF(INPCMP(I,'ION-DISS#OCIATION-#COEFFICIENT')+
     -           INPCMP(I,'DISS#OCIATION-#COEFFICIENT').NE.0)THEN
                 IF(IDISS.NE.0)THEN
                      CALL INPMSG(I,'Has already been entered.     ')
                      OK=.FALSE.
                 ELSE
                      IF(FLAG(I+1))THEN
                           ITAB=ITAB+1
                           IDISS=ITAB
                      ELSEIF(I.LT.NWORD)THEN
                           IFCN=IFCN+1
                           IDISS=-IFCN
                           CALL INPSTR(I+1,I+1,STRING,NC)
                           FCNTAB(NFTAB:NFTAB+NC)=STRING(1:NC)//','
                           NFTAB=NFTAB+NC+1
                           INEXT=I+2
                      ENDIF
                      GASOK(12)=.TRUE.
                 ENDIF
*   Check for a longitudinal diffusion coefficient.
            ELSEIF(INPCMP(I,'DI#FFUSION-#COEFFICIENT')+
     -           INPCMP(I,'LONG#ITUDINAL-'//
     -                'DI#FFUSION-#COEFFICIENT').NE.0)THEN
                 IF(IDIFF.NE.0)THEN
                      CALL INPMSG(I,'Has already been entered.     ')
                      OK=.FALSE.
                 ELSE
                      IF(FLAG(I+1))THEN
                           ITAB=ITAB+1
                           IDIFF=ITAB
                      ELSEIF(I+1.LE.NWORD)THEN
                           IFCN=IFCN+1
                           IDIFF=-IFCN
                           CALL INPSTR(I+1,I+1,STRING,NC)
                           FCNTAB(NFTAB:NFTAB+NC)=STRING(1:NC)//','
                           NFTAB=NFTAB+NC+1
                           INEXT=I+2
                      ENDIF
                      GASOK(3)=.TRUE.
                 ENDIF
*   Check for a transverse diffusion coefficient.
            ELSEIF(INPCMP(I,'TRANS#VERSE-'//
     -                'DI#FFUSION-#COEFFICIENT').NE.0)THEN
                 IF(ITRANS.NE.0)THEN
                      CALL INPMSG(I,'Has already been entered.     ')
                      OK=.FALSE.
                 ELSE
                      IF(FLAG(I+1))THEN
                           ITAB=ITAB+1
                           ITRANS=ITAB
                      ELSEIF(I+1.LE.NWORD)THEN
                           IFCN=IFCN+1
                           ITRANS=-IFCN
                           CALL INPSTR(I+1,I+1,STRING,NC)
                           FCNTAB(NFTAB:NFTAB+NC)=STRING(1:NC)//','
                           NFTAB=NFTAB+NC+1
                           INEXT=I+2
                      ENDIF
                      GASOK(8)=.TRUE.
                 ENDIF
*   Check for a drift velocity terms.
            ELSEIF(INPCMP(I,'DR#IFT-#VELOCITY')+
     -           INPCMP(I,'E-VEL#OCITY').NE.0)THEN
                 IF(IDRIFT.NE.0)THEN
                      CALL INPMSG(I,'Has already been entered.     ')
                      OK=.FALSE.
                 ELSE
                      IF(FLAG(I+1))THEN
                           ITAB=ITAB+1
                           IDRIFT=ITAB
                      ELSEIF(I.LT.NWORD)THEN
                           IFCN=IFCN+1
                           IDRIFT=-IFCN
                           CALL INPSTR(I+1,I+1,STRING,NC)
                           FCNTAB(NFTAB:NFTAB+NC)=STRING(1:NC)//','
                           NFTAB=NFTAB+NC+1
                           INEXT=I+2
                      ENDIF
                      GASOK(1)=.TRUE.
                 ENDIF
            ELSEIF(INPCMP(I,'B#TRANSVERSE-VEL#OCITY')+
     -           INPCMP(I,'B#TRANSVERSAL-VEL#OCITY').NE.0)THEN
                 IF(.NOT.MAGOK)THEN
                      CALL INPMSG(I,'There is no magnetic field.')
                      OK=.FALSE.
                 ELSEIF(IVB.NE.0)THEN
                      CALL INPMSG(I,'Has already been entered.     ')
                      OK=.FALSE.
                 ELSE
                      IF(FLAG(I+1))THEN
                           ITAB=ITAB+1
                           IVB=ITAB
                      ELSEIF(I.LT.NWORD)THEN
                           IFCN=IFCN+1
                           IVB=-IFCN
                           CALL INPSTR(I+1,I+1,STRING,NC)
                           FCNTAB(NFTAB:NFTAB+NC)=STRING(1:NC)//','
                           NFTAB=NFTAB+NC+1
                           INEXT=I+2
                      ENDIF
                      GASOK(9)=.TRUE.
                 ENDIF
            ELSEIF(INPCMP(I,'EXB-VEL#OCITY').NE.0)THEN
                 IF(.NOT.MAGOK)THEN
                      CALL INPMSG(I,'There is no magnetic field.')
                      OK=.FALSE.
                 ELSEIF(IVEXB.NE.0)THEN
                      CALL INPMSG(I,'Has already been entered.     ')
                      OK=.FALSE.
                 ELSE
                      IF(FLAG(I+1))THEN
                           ITAB=ITAB+1
                           IVEXB=ITAB
                      ELSEIF(I.LT.NWORD)THEN
                           IFCN=IFCN+1
                           IVEXB=-IFCN
                           CALL INPSTR(I+1,I+1,STRING,NC)
                           FCNTAB(NFTAB:NFTAB+NC)=STRING(1:NC)//','
                           NFTAB=NFTAB+NC+1
                           INEXT=I+2
                      ENDIF
                      GASOK(10)=.TRUE.
                 ENDIF
*   Check for the Lorentz angle.
            ELSEIF(INPCMP(I,'LOR#ENTZ-#ANGLE').NE.0)THEN
                 IF(.NOT.MAGOK)THEN
                      CALL INPMSG(I,'There is no magnetic field.')
                      OK=.FALSE.
                 ELSEIF(ILOREN.NE.0)THEN
                      CALL INPMSG(I,'Has already been entered.')
                      OK=.FALSE.
                 ELSE
                      IF(FLAG(I+1))THEN
                           ITAB=ITAB+1
                           ILOREN=ITAB
                      ELSEIF(I.LT.NWORD)THEN
                           IFCN=IFCN+1
                           ILOREN=-IFCN
                           CALL INPSTR(I+1,I+1,STRING,NC)
                           FCNTAB(NFTAB:NFTAB+NC)=STRING(1:NC)//','
                           NFTAB=NFTAB+NC+1
                           INEXT=I+2
                      ENDIF
                      GASOK(7)=.TRUE.
                 ENDIF
*   Check for the mobility.
            ELSEIF(INPCMP(I,'ION-MOB#ILITY').NE.0)THEN
                 IF(IMOBIL.NE.0)THEN
                      CALL INPMSG(I,'Has already been entered.     ')
                      OK=.FALSE.
                 ELSE
                      IF(FLAG(I+1))THEN
                           ITAB=ITAB+1
                           IMOBIL=ITAB
                      ELSEIF(I.LT.NWORD)THEN
                           IFCN=IFCN+1
                           IMOBIL=-IFCN
                           CALL INPSTR(I+1,I+1,STRING,NC)
                           FCNTAB(NFTAB:NFTAB+NC)=STRING(1:NC)//','
                           NFTAB=NFTAB+NC+1
                           INEXT=I+2
                      ENDIF
                      GASOK(2)=.TRUE.
                 ENDIF
*   Check for the Townsend coefficient.
            ELSEIF(INPCMP(I,'T#OWNSEND-#COEFFICIENT').NE.0)THEN
                 IF(ITOWN.NE.0)THEN
                      CALL INPMSG(I,'Has already been entered.     ')
                      OK=.FALSE.
                 ELSE
                      IF(FLAG(I+1))THEN
                           ITAB=ITAB+1
                           ITOWN=ITAB
                      ELSEIF(I.LT.NWORD)THEN
                           IFCN=IFCN+1
                           ITOWN=-IFCN
                           CALL INPSTR(I+1,I+1,STRING,NC)
                           FCNTAB(NFTAB:NFTAB+NC)=STRING(1:NC)//','
                           NFTAB=NFTAB+NC+1
                           INEXT=I+2
                      ENDIF
                      GASOK(4)=.TRUE.
                 ENDIF
*   Look for the E/P-RANGE parameter.
            ELSEIF(INPCMP(I,'E/P-R#ANGE').NE.0)THEN
                 IF(FLAG(I+1))THEN
                      CALL INPMSG(I,'RANGE should have 2 arguments.')
                      OK=.FALSE.
                      INEXT=I+1
                 ELSEIF(.NOT.FLAG(I+1).AND.FLAG(I+2))THEN
                      CALL INPMSG(I,'RANGE should have 2 arguments.')
                      CALL INPMSG(I+1,'See the previous message.     ')
                      INEXT=I+2
                      OK=.FALSE.
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPCHK(I+2,2,IFAIL2)
                      CALL INPRDR(I+1,EPMINR,0.1)
                      CALL INPRDR(I+2,EPMAXR,100.0)
                      IF(IFAIL1.EQ.0.AND.EPMINR.LE.0.0)THEN
                           CALL INPMSG(I+1,
     -                          'The minimum E/P should be > 0.')
                           OK=.FALSE.
                      ELSEIF(IFAIL2.EQ.0.AND.EPMAXR.LE.0.0)THEN
                           CALL INPMSG(I+1,
     -                          'The maximum E/P should be > 0.')
                           OK=.FALSE.
                      ELSE
                           IF(EPMINR.EQ.EPMAXR)THEN
                                CALL INPMSG(I+1,
     -                               'A zero range not is permitted.')
                                CALL INPMSG(I+2,
     -                               'A zero range not is permitted.')
                                OK=.FALSE.
                           ELSE
                                EPMIN=MIN(EPMINR,EPMAXR)
                                EPMAX=MAX(EPMINR,EPMAXR)
                                IRANGE=1
                           ENDIF
                      ENDIF
                      INEXT=I+3
                 ENDIF
*   Look for the N-E/P parameter.
            ELSEIF(INPCMP(I,'N-E/P').NE.0)THEN
                 IF(FLAG(I+1))THEN
                      CALL INPMSG(I,'N should have one argument.   ')
                      INEXT=I+1
                      OK=.FALSE.
                 ELSE
                      CALL INPCHK(I+1,1,IFAIL1)
                      CALL INPRDI(I+1,NGASR,20)
                      IF(IFAIL1.EQ.0.AND.NGASR.LE.1)THEN
                           CALL INPMSG(I+1,
     -                          'Number of gas points is < 2.  ')
                           OK=.FALSE.
                      ELSEIF(IFAIL1.EQ.0.AND.NGASR.GT.MXLIST)THEN
                           CALL INPMSG(I+1,
     -                          'Number of gas points > MXLIST.')
                           OK=.FALSE.
                      ELSEIF(IFAIL1.EQ.0)THEN
                           NGAS=NGASR
                           IRANGE=1
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Kind of E/p scale.
            ELSEIF(INPCMP(I,'LIN#EAR-#E/P-#SCALE').NE.0)THEN
                 EPLOG=.FALSE.
            ELSEIF(INPCMP(I,'LOG#ARITHMIC-#E/P-#SCALE').NE.0)THEN
                 EPLOG=.TRUE.
*   Look for the B-RANGE parameter.
            ELSEIF(INPCMP(I,'B-R#ANGE').NE.0)THEN
                 IF(FLAG(I+1))THEN
                      CALL INPMSG(I,'RANGE should have 2 arguments.')
                      INEXT=I+1
                      OK=.FALSE.
                 ELSEIF(.NOT.FLAG(I+1).AND.FLAG(I+2))THEN
                      CALL INPMSG(I,'RANGE should have 2 arguments.')
                      CALL INPMSG(I+1,'See the previous message.     ')
                      INEXT=I+2
                      OK=.FALSE.
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPCHK(I+2,2,IFAIL2)
                      CALL INPRDR(I+1,BTMINR,BTABMN/100)
                      CALL INPRDR(I+2,BTMAXR,BTABMX/100)
                      IF(IFAIL1.EQ.0.AND.BTMINR.LE.0.0)THEN
                           CALL INPMSG(I+1,
     -                          'The minimum B should be > 0.')
                           OK=.FALSE.
                      ELSEIF(IFAIL2.EQ.0.AND.BTMAXR.LE.0.0)THEN
                           CALL INPMSG(I+1,
     -                          'The maximum B should be > 0.')
                           OK=.FALSE.
                      ELSE
                           IF(BTMINR.EQ.BTMAXR)THEN
                                CALL INPMSG(I+1,
     -                               'A zero range not is permitted.')
                                CALL INPMSG(I+2,
     -                               'A zero range not is permitted.')
                                OK=.FALSE.
                           ELSE
                                BTABMN=100*MIN(BTMINR,BTMAXR)
                                BTABMX=100*MAX(BTMINR,BTMAXR)
                                TAB2D=.TRUE.
                           ENDIF
                      ENDIF
                      INEXT=I+3
                 ENDIF
*   Look for the N-B parameter.
            ELSEIF(INPCMP(I,'N-B').NE.0)THEN
                 IF(FLAG(I+1))THEN
                      CALL INPMSG(I,'N should have one argument.   ')
                      INEXT=I+1
                      OK=.FALSE.
                 ELSE
                      CALL INPCHK(I+1,1,IFAIL1)
                      CALL INPRDI(I+1,NBTABR,NBTAB)
                      IF(IFAIL1.EQ.0.AND.NBTABR.LE.1)THEN
                           CALL INPMSG(I+1,
     -                          'Number of B fields is < 2.  ')
                           OK=.FALSE.
                      ELSEIF(IFAIL1.EQ.0.AND.NBTABR.GT.MXBTAB)THEN
                           CALL INPMSG(I+1,
     -                          'Number of B fields > MXBTAB.')
                           OK=.FALSE.
                      ELSEIF(IFAIL1.EQ.0)THEN
                           NBTAB=NBTABR
                           TAB2D=.TRUE.
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Look for the B-field keyword.
            ELSEIF(INPCMP(I,'B-F#IELD').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,BTMINR,(BTABMN+BTABMX)/200)
                 IF(IFAIL1.EQ.0.AND.NWORD.GE.I+1)THEN
                      IF(BTMINR.LT.0)THEN
                           CALL INPMSG(I+1,'B field is not > 0.')
                           OK=.FALSE.
                      ELSE
                           BTABMN=100*BTMINR
                           BTABMX=100*BTMINR
                           NBTAB=1
                           TAB2D=.TRUE.
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Missing or invalid arguments. ')
                      OK=.FALSE.
                 ENDIF
                 INEXT=I+2
*   Look for the ANGLE-RANGE parameter.
            ELSEIF(INPCMP(I,'ANG#LE-R#ANGE').NE.0)THEN
                 IF(FLAG(I+1))THEN
                      CALL INPMSG(I,'RANGE should have 2 arguments.')
                      INEXT=I+1
                      OK=.FALSE.
                 ELSEIF(.NOT.FLAG(I+1).AND.FLAG(I+2))THEN
                      CALL INPMSG(I,'RANGE should have 2 arguments.')
                      CALL INPMSG(I+1,'See the previous message.     ')
                      INEXT=I+2
                      OK=.FALSE.
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPCHK(I+2,2,IFAIL2)
                      CALL INPRDR(I+1,BAMINR,180*BANGMN/PI)
                      CALL INPRDR(I+2,BAMAXR,180*BANGMX/PI)
                      IF(IFAIL1.EQ.0.AND.
     -                     (BAMINR.LT.0.OR.BAMINR.GT.90.0))THEN
                           CALL INPMSG(I+1,
     -                          'Min angle not in range [0,90].')
                           OK=.FALSE.
                      ELSEIF(IFAIL2.EQ.0.AND.
     -                     (BAMAXR.LT.0.OR.BAMAXR.GT.90.0))THEN
                           CALL INPMSG(I+1,
     -                          'Max angle not in range [0,90].')
                           OK=.FALSE.
                      ELSE
                           IF(BAMINR.EQ.BAMAXR)THEN
                                CALL INPMSG(I+1,
     -                               'A zero range not is permitted.')
                                CALL INPMSG(I+2,
     -                               'A zero range not is permitted.')
                                OK=.FALSE.
                           ELSE
                                BANGMN=PI*MIN(BAMINR,BAMAXR)/180
                                BANGMX=PI*MAX(BAMINR,BAMAXR)/180
                                TAB2D=.TRUE.
                           ENDIF
                      ENDIF
                      INEXT=I+3
                 ENDIF
*   Look for the N-ANGLE parameter.
            ELSEIF(INPCMP(I,'N-ANG#LE').NE.0)THEN
                 IF(FLAG(I+1))THEN
                      CALL INPMSG(I,'N should have one argument.   ')
                      INEXT=I+1
                      OK=.FALSE.
                 ELSE
                      CALL INPCHK(I+1,1,IFAIL1)
                      CALL INPRDI(I+1,NBANGR,NBANG)
                      IF(IFAIL1.EQ.0.AND.NBANGR.LE.1)THEN
                           CALL INPMSG(I+1,
     -                          'Number of angles is < 2.  ')
                           OK=.FALSE.
                      ELSEIF(IFAIL1.EQ.0.AND.NBANGR.GT.MXBANG)THEN
                           CALL INPMSG(I+1,
     -                          'Number of angles > MXBANG.')
                           OK=.FALSE.
                      ELSEIF(IFAIL1.EQ.0)THEN
                           NBANG=NBANGR
                           TAB2D=.TRUE.
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Look for the ANGLE keyword.
            ELSEIF(INPCMP(I,'ANG#LE').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,BAMINR,180*BANGMN/PI)
                 IF(IFAIL1.EQ.0.AND.NWORD.GE.I+1)THEN
                      IF(BAMINR.LT.0.OR.BAMINR.GT.90.0)THEN
                           CALL INPMSG(I+1,'Out of range [0,90].')
                           OK=.FALSE.
                      ELSE
                           BANGMN=PI*BAMINR/180
                           BANGMX=PI*BAMINR/180
                           NBANG=1
                           TAB2D=.TRUE.
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Missing or invalid arguments. ')
                      OK=.FALSE.
                 ENDIF
                 INEXT=I+2
*   Unknown entry.
            ELSE
                 CALL INPMSG(I,'Unknown table entry, ignored. ')
                 ITAB=ITAB+1
                 OK=.FALSE.
                 INEXT=I+1
            ENDIF
*   Next entry.
610         CONTINUE
**  Dump the error messages.
            CALL INPERR
*   Check for B dependence in table.
            IF(TAB2D.AND..NOT.MAGOK)THEN
                 PRINT *,' !!!!!! GASINP WARNING : The table has'//
     -                ' a B dependence, but there is no B field;'//
     -                ' dependence reset.'
                 TAB2D=.FALSE.
                 OK=.FALSE.
            ENDIF
**  Check whether we have to continue or not.
            IF(JFAIL.EQ.2.AND..NOT.OK)THEN
                 PRINT *,' ###### GASINP ERROR   : TABLE not'//
     -                ' executed because of the above errors.'
                 NGAS=0
                 GOTO 10
            ELSEIF(JFAIL.EQ.3.AND..NOT.OK)THEN
                 PRINT *,' ###### GASINP ERROR   : Program terminated'//
     -                ' because of the above errors.'
                 NGAS=0
                 CALL QUIT
                 RETURN
            ENDIF
**  Preset the OK flag again for the data processing.
            OK=.TRUE.
**  Take care of defaults: no arguments provided.
            IF(ITAB.EQ.0.AND.IFCN.EQ.0)THEN
                 IEP=1
                 IDRIFT=2
                 IVB=0
                 IVEXB=0
                 IDIFF=3
                 ITOWN=0
                 IATT=0
                 IDISS=0
                 IMOBIL=0
                 ILOREN=0
                 ITRANS=0
                 ITAB=3
                 GASOK(1)=.TRUE.
                 GASOK(2)=.FALSE.
                 GASOK(3)=.TRUE.
                 GASOK(4)=.FALSE.
                 GASOK(6)=.FALSE.
                 GASOK(7)=.FALSE.
                 GASOK(8)=.FALSE.
                 GASOK(9)=.FALSE.
                 GASOK(10)=.FALSE.
                 GASOK(12)=.FALSE.
            ENDIF
*   Table will follow: preset NGAS to 0.
            IF(ITAB.NE.0)NGAS=0
*   If there have not been any B or angle declarations, reset N.
            IF(.NOT.TAB2D)THEN
                 NBTAB=1
                 NBANG=1
            ENDIF
**  Warn if a RANGE or an N has been specified when not needed.
            IF(IRANGE.NE.0.AND.(IEP.GT.0.OR.
     -           IDRIFT.GT.0.OR.IVB.GT.0.OR.IVEXB.GT.0.OR.
     -           IDIFF.GT.0.OR.ITOWN.GT.0.OR.IATT.GT.0.OR.
     -           IMOBIL.GT.0.OR.ILOREN.GT.0.OR.ITRANS.GT.0.OR.
     -           IDISS.GT.0))THEN
                 PRINT *,' !!!!!! GASINP WARNING : RANGE and N'//
     -                ' ignored because a table is expected.'
                 OK=.FALSE.
            ENDIF
**  Generate some debugging output.
            IF(LDEBUG)THEN
                 WRITE(LUNOUT,'(''  ++++++ GASINP DEBUG   : TABLE '',
     -                ''debug output:''/26X,''Function: "'',A,''"''/26X,
     -                ''IEP='',I2,'', IDRIFT='',I2,'', IVB='',I2,
     -                '', IVEXB='',I2,'', IDIFF='',I2,
     -                '', ITOWN='',I2,'', IATT='',I2,
     -                '', IMOBIL='',I2,'', IDISS='',I2,
     -                '', ILOREN='',I2,'', ITRANS='',I2)')
     -                FCNTAB(1:MAX(1,NFTAB-2)),
     -                IEP,IDRIFT,IVB,IVEXB,IDIFF,ITOWN,IATT,
     -                IMOBIL,IDISS,ILOREN,ITRANS
                 WRITE(LUNOUT,'(26X,''EPMIN='',E10.3,'', EPMAX='',E10.3,
     -                '', NGAS='',I3)') EPMIN,EPMAX,NGAS
            ENDIF
**  Establish the B field range and angle range.
            DO 611 J=1,NBANG
            IF(NBANG.GT.1)THEN
                 BANG(J)=BANGMN+REAL(J-1)*(BANGMX-BANGMN)/REAL(NBANG-1)
            ELSE
                 BANG(J)=(BANGMN+BANGMX)/2
            ENDIF
611         CONTINUE
            DO 612 K=1,NBTAB
            IF(NBTAB.GT.1)THEN
                 BTAB(K)=BTABMN+REAL(K-1)*(BTABMX-BTABMN)/REAL(NBTAB-1)
            ELSE
                 BTAB(K)=(BTABMN+BTABMX)/2
            ENDIF
612         CONTINUE
**  Check whether a function has been specified somewhere.
            IENTRY=0
            IF(IDRIFT.LT.0.OR.IVB.LT.0.OR.IVEXB.LT.0.OR.
     -           IDIFF.LT.0.OR.ITRANS.LT.0.OR.ITOWN.LT.0.OR.
     -           IATT.LT.0.OR.IMOBIL.LT.0.OR.IDISS.LT.0.OR.
     -           ILOREN.LT.0)THEN
*   Check for the presence of a function.
                 IF(NFTAB.LE.2)THEN
                      PRINT *,' !!!!!! GASINP WARNING : The function'//
     -                     ' seems to be empty; rejected.'
                      NGAS=0
                      GOTO 10
                 ENDIF
*   Remove the comma at the end of the string.
                 FCNTAB(NFTAB-1:NFTAB-1)='  '
                 NFTAB=NFTAB-2
*   Convert the string to an instruction list (via ALGEDT if @ appears).
                 IF(INDEX(FCNTAB(1:NFTAB),'@').NE.0)THEN
                      NRES=IFCN
                      CALL ALGEDT(VARTAB,7,IENTRY,USE,NRES)
                 ELSE
                      CALL ALGPRE(FCNTAB,NFTAB,VARTAB,7,NRES,USE,IENTRY,
     -                     IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! GASINP WARNING : Entries'//
     -                          ' specified as functions are ignored:'
                           IF(IDRIFT.LT.0)PRINT *,'                  '//
     -                          '       the drift velocity || E'
                           IF(IVB.LT.0)PRINT *,'                  '//
     -                          '       the drift velocity || Btrans'
                           IF(IVEXB.LT.0)PRINT *,'                  '//
     -                          '       the drift velocity || ExB'
                           IF(ILOREN.LT.0)PRINT *,'                  '//
     -                          '       the Lorentz angle'
                           IF(IDIFF.LT.0)PRINT *,'                  '//
     -                          '       the longitudinal diffusion'
                           IF(ITRANS.LT.0)PRINT *,'                  '//
     -                          '       the transverse diffusion'
                           IF(ITOWN.LT.0)PRINT *,'                  '//
     -                          '       the Townsend coefficient'
                           IF(IATT.LT.0)PRINT *,'                  '//
     -                          '       the attachment coefficient'
                           IF(IDISS.LT.0)PRINT *,'                  '//
     -                          '       the dissociation coefficient'
                           IF(IMOBIL.LT.0)PRINT *,'                  '//
     -                          '       the ion mobility'
                           NGAS=0
                           CALL ALGCLR(IENTRY)
                           GOTO 10
                      ENDIF
                 ENDIF
                 IF(NRES.NE.IFCN)THEN
                      PRINT *,' !!!!!! GASINP WARNING : Number'//
     -                     ' of functions being returned is wrong.'
                      NGAS=0
                      CALL ALGCLR(IENTRY)
                      GOTO 10
                 ENDIF
*   Warn if the function does not depend explicitely on EP.
                 IF(.NOT.USE(1))PRINT *,' ------ GASINP MESSAGE : The'//
     -                 ' function is independent of E/p, but accepted.'
*   Ensure the function does not depend on B or angle if 1D.
                 IF((.NOT.TAB2D).AND.(USE(4).OR.USE(5)))THEN
                      PRINT *,' !!!!!! GASINP WARNING : The function'//
     -                     ' depends on B or angle(E,B) but the table'//
     -                     ' has no B part; rejected.'
                      NGAS=0
                      CALL ALGCLR(IENTRY)
                      GOTO 10
                 ENDIF
            ENDIF
**  Read the cards if at least one item has been tabulated.
            IF(IDRIFT.GT.0.OR.IVB.GT.0.OR.IVEXB.GT.0.OR.
     -           IDIFF.GT.0.OR.ITRANS.GT.0.OR.IMOBIL.GT.0.OR.
     -           ITOWN.GT.0.OR.IATT.GT.0.OR.ILOREN.GT.0.OR.
     -           IDISS.GT.0.OR.IEP.GT.0)THEN
*   Check that E/p has been specified.
                 IF(IEP.EQ.0)THEN
                      PRINT *,' !!!!!! GASINP WARNING : E/p has to be'//
     -                     ' present in the table; table rejected.'
                      NGAS=0
                      IF(IENTRY.NE.0)CALL ALGCLR(IENTRY)
                      GOTO 10
                 ENDIF
*   Prompt in interactive mode.
                 NGAS=0
                 IF(STDSTR('INPUT'))
     -                PRINT *,' ====== GASINP INPUT   :'//
     -                ' Please enter the table, enter a'//
     -                ' blank line when ready.'
                 CALL INPPRM('Table','ADD-NOPRINT')
*   And start an input loop.
620              CONTINUE
                 CALL INPWRD(NWORD)
                 IF(NWORD.EQ.0)GOTO 660
                 CALL INPSTR(1,1,STRING,NC)
*   Take appropriate action if a & is met.
                 IF(STRING(1:1).EQ.'&')THEN
                      PRINT *,' !!!!!! GASINP WARNING : You can not'//
     -                     ' leave the section here ; line is ignored.'
                      GOTO 620
                 ENDIF
*    Make sure each line contains the right number of items.
                 IF(NWORD.NE.ITAB)THEN
                      PRINT *,' !!!!!! GASINP WARNING : Gas tables'//
     -                     ' must contain the number of items listed'//
     -                     ' in the TABLE line.'
                      GOTO 620
                 ENDIF
*   Preset error flag.
                 IFAIL1=0
*   Read the items and check their syntax + validity.
                 NGAS=NGAS+1
                 DO 630 I=1,ITAB
                 CALL INPCHK(I,2,IFAIL2)
                 IF(IFAIL2.NE.0)IFAIL1=1
                 CALL INPRDR(I,DATAR,-1.0)
                 DO 631 J=1,NBANG
                 DO 632 K=1,NBTAB
                 IF(NGAS.LE.MXLIST.AND.IFAIL2.EQ.0)THEN
                      IF(I.EQ.IEP)THEN
                           EGAS(NGAS)=DATAR
                      ELSEIF(I.EQ.IDRIFT)THEN
                           VGAS(NGAS)=DATAR
                           VGAS2(NGAS,J,K)=DATAR
                      ELSEIF(I.EQ.IVB)THEN
                           XGAS(NGAS)=DATAR
                           XGAS2(NGAS,J,K)=DATAR
                      ELSEIF(I.EQ.IVEXB)THEN
                           YGAS(NGAS)=DATAR
                           YGAS2(NGAS,J,K)=DATAR
                      ELSEIF(I.EQ.ILOREN)THEN
                           IF(DATAR.LT.0.OR.DATAR.GT.90.0)THEN
                                PRINT *,' !!!!!! GASINP WARNING :'//
     -                               ' Lorentz angle outside the'//
     -                               ' range [0,90] degrees.'
                                IFAIL1=1
                                WGAS(NGAS)=0
                           ELSE
                                WGAS(NGAS)=PI*DATAR/180.0
                           ENDIF
                           WGAS2(NGAS,J,K)=WGAS(NGAS)
                      ELSEIF(I.EQ.IDIFF)THEN
                           DGAS(NGAS)=DATAR
                           DGAS2(NGAS,J,K)=DATAR
                      ELSEIF(I.EQ.ITRANS)THEN
                           OGAS(NGAS)=DATAR
                           OGAS2(NGAS,J,K)=DATAR
                      ELSEIF(I.EQ.ITOWN)THEN
                           IF(DATAR.EQ.0)THEN
                                AGAS(NGAS)=-30
                           ELSEIF(DATAR.GT.0)THEN
                                AGAS(NGAS)=MAX(-30.0,LOG(DATAR))
                           ELSE
                                PRINT *,' !!!!!! GASINP WARNING :'//
     -                               ' Townsend coefficient < 0;'//
     -                               ' data rejected.'
                                IFAIL1=1
                                AGAS(NGAS)=-30.0
                           ENDIF
                           AGAS2(NGAS,J,K)=AGAS(NGAS)
                      ELSEIF(I.EQ.IATT)THEN
                           IF(DATAR.EQ.0)THEN
                                BGAS(NGAS)=-30
                           ELSEIF(DATAR.GT.0)THEN
                                BGAS(NGAS)=MAX(-30.0,LOG(DATAR))
                           ELSE
                                PRINT *,' !!!!!! GASINP WARNING :'//
     -                               ' Attachment coefficient < 0;'//
     -                               ' data rejected.'
                                IFAIL1=1
                                BGAS(NGAS)=-30.0
                           ENDIF
                           BGAS2(NGAS,J,K)=BGAS(NGAS)
                      ELSEIF(I.EQ.IDISS)THEN
                           IF(DATAR.EQ.0)THEN
                                HGAS(NGAS)=-30
                           ELSEIF(DATAR.GT.0)THEN
                                HGAS(NGAS)=MAX(-30.0,LOG(DATAR))
                           ELSE
                                PRINT *,' !!!!!! GASINP WARNING :'//
     -                               ' Dissociation coefficient < 0;'//
     -                               ' data rejected.'
                                IFAIL1=1
                                HGAS(NGAS)=-30.0
                           ENDIF
                           HGAS2(NGAS,J,K)=HGAS(NGAS)
                      ELSEIF(I.EQ.IMOBIL)THEN
                           MGAS(NGAS)=DATAR
                           MGAS2(NGAS,J,K)=DATAR
                      ENDIF
                 ENDIF
632              CONTINUE
631              CONTINUE
630              CONTINUE
*   Evaluate the function value, if needed.
                 IF(IDRIFT.LT.0.OR.IVB.LT.0.OR.IVEXB.LT.0.OR.
     -                IDIFF.LT.0.OR.ITRANS.LT.0.OR.ILOREN.LT.0.OR.
     -                ITOWN.LT.0.OR.IATT.LT.0.OR.IMOBIL.LT.0.OR.
     -                IDISS.LT.0)THEN
                      DO 641 J=1,NBANG
                      DO 642 K=1,NBTAB
                      VAR(1)=EGAS(NGAS)
                      VAR(2)=BOLTZ
                      VAR(3)=ECHARG
                      VAR(4)=180*BANG(J)/PI
                      VAR(5)=BTAB(K)/100
                      VAR(6)=TGAS
                      VAR(7)=PGAS
                      MODVAR(1)=2
                      MODVAR(2)=2
                      MODVAR(3)=2
                      MODVAR(4)=2
                      MODVAR(5)=2
                      MODVAR(6)=2
                      MODVAR(7)=2
                      CALL ALGEXE(IENTRY,VAR,MODVAR,7,
     -                     RES,MODRES,NRES,IFAIL2)
                      IF(IFAIL2.NE.0)THEN
                           PRINT *,' !!!!!! GASINP WARNING : Error'//
     -                          ' evaluating the function.'
                           IFAIL1=1
                      ENDIF
                      DO 640 I=1,IFCN
                      IF(MODRES(I).NE.2)THEN
                           PRINT *,' !!!!!! GASINP WARNING : Function'//
     -                          ' does not return a number.'
                           IFAIL1=1
                           RES(I)=0
                      ENDIF
                      IF(I.EQ.-IDRIFT)THEN
                           VGAS(NGAS)=RES(I)
                           VGAS2(NGAS,J,K)=RES(I)
                      ELSEIF(I.EQ.-IVB)THEN
                           XGAS(NGAS)=RES(I)
                           XGAS2(NGAS,J,K)=RES(I)
                      ELSEIF(I.EQ.-IVEXB)THEN
                           YGAS(NGAS)=RES(I)
                           YGAS2(NGAS,J,K)=RES(I)
                      ELSEIF(I.EQ.-ILOREN)THEN
                           IF(RES(I).LT.0.OR.RES(I).GT.90.0)THEN
                                PRINT *,' !!!!!! GASINP WARNING :'//
     -                               ' Lorentz angle outside the'//
     -                               ' range [0,90] degrees.'
                                IFAIL1=1
                                WGAS(NGAS)=0
                           ELSE
                                WGAS(NGAS)=PI*RES(I)/180.0
                           ENDIF
                           WGAS2(NGAS,J,K)=WGAS(NGAS)
                      ELSEIF(I.EQ.-IDIFF)THEN
                           DGAS(NGAS)=RES(I)
                           DGAS2(NGAS,J,K)=RES(I)
                      ELSEIF(I.EQ.-ITRANS)THEN
                           OGAS(NGAS)=RES(I)
                           OGAS2(NGAS,J,K)=RES(I)
                      ELSEIF(I.EQ.-ITOWN)THEN
                           IF(RES(I).EQ.0)THEN
                                AGAS(NGAS)=-30.0
                           ELSEIF(RES(I).GT.0)THEN
                                AGAS(NGAS)=MAX(-30.0,LOG(RES(I)))
                           ELSE
                                AGAS(NGAS)=-30.0
                                PRINT *,' !!!!!! GASINP WARNING :'//
     -                               ' Townsend coefficient < 0;'//
     -                               ' data rejected.'
                                IFAIL1=1
                           ENDIF
                           AGAS2(NGAS,J,K)=AGAS(NGAS)
                      ELSEIF(I.EQ.-IATT)THEN
                           IF(RES(I).EQ.0)THEN
                                BGAS(NGAS)=-30.0
                           ELSEIF(RES(I).GT.0)THEN
                                BGAS(NGAS)=MAX(-30.0,LOG(RES(I)))
                           ELSE
                                BGAS(NGAS)=-30.0
                                PRINT *,' !!!!!! GASINP WARNING :'//
     -                               ' Attachment coefficient < 0;'//
     -                               ' data rejected.'
                                IFAIL1=1
                           ENDIF
                           BGAS2(NGAS,J,K)=BGAS(NGAS)
                      ELSEIF(I.EQ.-IDISS)THEN
                           IF(RES(I).EQ.0)THEN
                                HGAS(NGAS)=-30.0
                           ELSEIF(RES(I).GT.0)THEN
                                HGAS(NGAS)=MAX(-30.0,LOG(RES(I)))
                           ELSE
                                HGAS(NGAS)=-30.0
                                PRINT *,' !!!!!! GASINP WARNING :'//
     -                               ' Dissociation coefficient < 0;'//
     -                               ' data rejected.'
                                IFAIL1=1
                           ENDIF
                           HGAS2(NGAS,J,K)=HGAS(NGAS)
                      ELSEIF(I.EQ.-IMOBIL)THEN
                           MGAS(NGAS)=RES(I)
                           MGAS2(NGAS,J,K)=RES(I)
                      ENDIF
640                   CONTINUE
642                   CONTINUE
641                   CONTINUE
                 ENDIF
*   Dump error messages.
                 CALL INPERR
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! GASINP WARNING : The input'//
     -                     ' line is ignored, see preceding message.'
                      NGAS=NGAS-1
                      OK=.FALSE.
                 ENDIF
*   Proceed with the next line.
                 GOTO 620
**  End of list, carry out a few checks.
660              CONTINUE
*   Reset the prompt.
                 CALL INPPRM(' ','BACK-PRINT')
*   Warn if the table was empty.
                 IF(NGAS.LE.2)PRINT *,' !!!!!! GASINP WARNING : The'//
     -                ' gas table did not contain enough points (> 2).'
*   Warn if parts of the table were omitted for lack of storage space.
                 IF(NGAS.GT.MXLIST)THEN
                      PRINT *,' !!!!!! GASINP WARNING : ',NGAS-MXLIST,
     -                     ' data points could not be stored ; you'//
     -                     ' could increase the MXLIST parameter.'
                      NGAS=MXLIST
                      OK=.FALSE.
                 ENDIF
**  If the table is presented as a pure function:
            ELSEIF(IDRIFT.LT.0.OR.IVB.LT.0.OR.IVEXB.LT.0.OR.
     -           IDIFF.LT.0.OR.ITRANS.LT.0.OR.ILOREN.LT.0.OR.
     -           ITOWN.LT.0.OR.IATT.LT.0.OR.IMOBIL.LT.0.OR.
     -           IDISS.LT.0)THEN
*   Make the table using the function.
                 NNGAS=NGAS
                 DO 680 I=1,NNGAS
*   Preset the error flag for this E/p.
                 IFAIL1=0
*   Set E/p.
                 IF(EPLOG)THEN
                      EGAS(I)=EPMIN*(EPMAX/EPMIN)**
     -                     (REAL(I-1)/REAL(NGAS-1))
                 ELSE
                      EGAS(I)=EPMIN+(EPMAX-EPMIN)*
     -                     (REAL(I-1)/REAL(NGAS-1))
                 ENDIF
*   Loop over angles and B field.
                 DO 681 J=1,NBANG
                 IF(NBANG.GT.1)THEN
                      BANG(J)=BANGMN+REAL(J-1)*(BANGMX-BANGMN)/
     -                     REAL(NBANG-1)
                 ELSE
                      BANG(J)=(BANGMN+BANGMX)/2
                 ENDIF
                 DO 682 K=1,NBTAB
                 IF(NBTAB.GT.1)THEN
                      BTAB(K)=BTABMN+REAL(K-1)*(BTABMX-BTABMN)/
     -                     REAL(NBTAB-1)
                 ELSE
                      BTAB(K)=(BTABMN+BTABMX)/2
                 ENDIF
*   Evaluate the functions.
                 VAR(1)=EGAS(I)
                 VAR(2)=BOLTZ
                 VAR(3)=ECHARG
                 VAR(4)=180*BANG(J)/PI
                 VAR(5)=BTAB(K)/100
                 VAR(6)=TGAS
                 VAR(7)=PGAS
                 MODVAR(1)=2
                 MODVAR(2)=2
                 MODVAR(3)=2
                 MODVAR(4)=2
                 MODVAR(5)=2
                 MODVAR(6)=2
                 MODVAR(7)=2
                 CALL ALGEXE(IENTRY,VAR,MODVAR,7,RES,MODRES,NRES,IFAIL2)
                 IF(IFAIL2.NE.0)THEN
                      PRINT *,' !!!!!! GASINP WARNING : Arithmetic'//
     -                     ' error evaluating the function at E/p=',
     -                     EGAS(I)
                      IFAIL1=1
                 ENDIF
*   Assign the results.
                 DO 670 II=1,IFCN
                 IF(MODRES(II).NE.2)THEN
                      PRINT *,' !!!!!! GASINP WARNING : Function does'//
     -                     ' not return a number for E/p=',EGAS(I)
                      IFAIL1=1
                      RES(II)=0
                 ENDIF
                 IF(II.EQ.-IDRIFT)THEN
                      VGAS(I)=RES(II)
                      VGAS2(I,J,K)=RES(II)
                 ELSEIF(II.EQ.-IVB)THEN
                      XGAS(I)=RES(II)
                      XGAS2(I,J,K)=RES(II)
                 ELSEIF(II.EQ.-IVEXB)THEN
                      YGAS(I)=RES(II)
                      YGAS2(I,J,K)=RES(II)
                 ELSEIF(II.EQ.-ILOREN)THEN
                      IF(RES(II).LT.0.OR.RES(II).GT.90.0)THEN
                           PRINT *,' !!!!!! GASINP WARNING : Lorentz'//
     -                          ' angle outside the range [0,90]'//
     -                          ' degrees for E/p=',EGAS(I)
                           IFAIL1=1
                           WGAS(I)=0
                      ELSE
                           WGAS(I)=PI*RES(II)/180.0
                      ENDIF
                      WGAS2(I,J,K)=WGAS(I)
                 ELSEIF(II.EQ.-IDIFF)THEN
                      DGAS(I)=RES(II)
                      DGAS2(I,J,K)=RES(II)
                 ELSEIF(II.EQ.-ITRANS)THEN
                      OGAS(I)=RES(II)
                      OGAS2(I,J,K)=RES(II)
                 ELSEIF(II.EQ.-ITOWN)THEN
                      IF(RES(II).EQ.0)THEN
                           AGAS(I)=-30.0
                      ELSEIF(RES(II).GT.0)THEN
                           AGAS(I)=MAX(-30.0,LOG(RES(II)))
                      ELSE
                           AGAS(I)=-30.0
                           PRINT *,' !!!!!! GASINP WARNING : Townsend'//
     -                          ' coefficient < 0 for E/p=',EGAS(I)
                           IFAIL1=1
                      ENDIF
                      AGAS2(I,J,K)=AGAS(I)
                 ELSEIF(II.EQ.-IATT)THEN
                      IF(RES(II).EQ.0)THEN
                           BGAS(I)=-30.0
                      ELSEIF(RES(II).GT.0)THEN
                           BGAS(I)=MAX(-30.0,LOG(RES(II)))
                      ELSE
                           BGAS(I)=-30.0
                           PRINT *,' !!!!!! GASINP WARNING :'//
     -                          ' Attachment coefficient < 0 for'//
     -                          ' E/p=',EGAS(I)
                           IFAIL1=1
                      ENDIF
                      BGAS2(I,J,K)=BGAS(I)
                 ELSEIF(II.EQ.-IDISS)THEN
                      IF(RES(II).EQ.0)THEN
                           HGAS(I)=-30.0
                      ELSEIF(RES(II).GT.0)THEN
                           HGAS(I)=MAX(-30.0,LOG(RES(II)))
                      ELSE
                           HGAS(I)=-30.0
                           PRINT *,' !!!!!! GASINP WARNING :'//
     -                          ' Dissociation coefficient < 0 for'//
     -                          ' E/p=',EGAS(I)
                           IFAIL1=1
                      ENDIF
                      HGAS2(I,J,K)=HGAS(I)
                 ELSEIF(II.EQ.-IMOBIL)THEN
                      MGAS(I)=RES(II)
                      MGAS2(I,J,K)=RES(II)
                 ENDIF
670              CONTINUE
*   Next angle and B field.
682              CONTINUE
681              CONTINUE
*   Check the errors for this E/p.
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! GASINP WARNING : The data'//
     -                     ' for E/p=',EGAS(I),' is ignored, see'//
     -                     ' preceding message.'
                      NGAS=NGAS-1
                      OK=.FALSE.
                 ENDIF
*   Next E/p.
680              CONTINUE
            ENDIF
**  Release the algebra entry point, if used.
            IF(IENTRY.NE.0)CALL ALGCLR(IENTRY)
**  Check whether we have to continue or not.
            IF(JFAIL.EQ.2.AND..NOT.OK)THEN
                 PRINT *,' ###### GASINP ERROR   : TABLE is'//
     -                ' rejected because of the above errors.'
                 NGAS=0
                 GOTO 10
            ELSEIF(JFAIL.EQ.3.AND..NOT.OK)THEN
                 PRINT *,' ###### GASINP ERROR   : Program terminated'//
     -                ' because of the above errors.'
                 NGAS=0
                 CALL QUIT
                 RETURN
            ENDIF
*** If TEMPERATURE is a keyword, find the temperature.
       ELSEIF(INPCMP(1,'TEMP#ERATURE').NE.0)THEN
            IF(NWORD.EQ.1)THEN
                 CALL OUTFMT(TGAS,2,STRAUX,NCAUX,'LEFT')
                 WRITE(LUNOUT,'(''  The temperature of the gas is '',
     -                A,'' K.'')') STRAUX(1:NCAUX)
            ELSEIF(NWORD.EQ.2.OR.NWORD.EQ.3)THEN
                 IF(NWORD.EQ.3)THEN
                      CALL INPSTR(3,3,UNIT,NCUNIT)
                 ELSE
                      UNIT='K'
                      NCUNIT=1
                 ENDIF
                 CALL INPCHK(2,2,IFAIL1)
                 CALL INPRDR(2,TGASRR,300.0)
                 CALL UNITS(TGASRR,UNIT(1:NCUNIT),TGASR,'K',IFAIL2)
                 IF(IFAIL2.NE.0)THEN
                      CALL INPMSG(3,'Not a valid temperature unit.')
                 ELSEIF(TGASR.LE.0.0.AND.IFAIL1.EQ.0)THEN
                      CALL INPMSG(2,'The temperature is not > 0 K. ')
                      IFAIL1=1
                 ELSE
                      TGAS=TGASR
                 ENDIF
                 CALL INPERR
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! GASINP WARNING : The',
     -                ' TEMPERATURE statement is ignored.'
            ELSE
                  PRINT *,' !!!!!! GASINP WARNING : TEMPERATURE takes',
     -                    ' a single argument ; statement ignored.'
            ENDIF
*** Transfers.
       ELSEIF(INPCMP(1,'L#IST-EXC#ITATIONS-#IONISATIONS-#LEVELS')+
     -      INPCMP(1,'L#IST-EXC#ITATIONS-#LEVELS')+
     -      INPCMP(1,'L#IST-ION#ISATIONS-#EXCITATIONS-#LEVELS')+
     -      INPCMP(1,'L#IST-ION#ISATIONS-#LEVELS').NE.0)THEN
            CALL GASLPT
       ELSEIF(INPCMP(1,'PEN#NING-#TRANSFER').NE.0)THEN
            CALL GASRPT
*** If USER1 is a keyword call routine USER1 to transfer user gas data.
C      ELSEIF(INPCMP(1,'US#ER-#GAS').NE.0)THEN
C            CALL GASUSR       abbreviation point in the keyword.
*** Call GASWRT to prepare writing the gas dataset.
       ELSEIF(INPCMP(1,'WR#ITE').NE.0)THEN
            CALL GASWRT(1)
            LGASWR=.TRUE.
*** If normal intructions are used, it is not possible to get here.
       ELSE
            CALL INPSTR(1,1,STRING,NC)
            PRINT *,' !!!!!! GASINP WARNING : '//STRING(1:NC)//' is'//
     -           ' not a valid instruction ; the line ignored.'
       ENDIF
       GOTO 10
       END
