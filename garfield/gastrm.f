CDECK  ID>, GASTRM.
       SUBROUTINE GASTRM(FAIL)
*-----------------------------------------------------------------------
*   GASTRM - Reads the TRIM command line
*   TRIMCAT Module - Garfield TRIM Clustering Model
*   Contributed by James Butterworth
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL WTRIM, FTRIM, TRMLMN, TRMLMX, TRMDEN, TRMEMI,
     -      TRMTGD, TRMIOE, ECTRIM, EKTRIM, XTRIM, YTRIM, ZTRIM, NETRIM,
     -      TRMHDI, TRMY, TRMZ
       INTEGER NTRIM, NCTRIM, LTRIM, ITRIM
       COMMON /TRMDAT/
     -      NTRIM, NCTRIM, WTRIM, FTRIM, LTRIM, TRMLMN, TRMLMX, TRMDEN,
     -      TRMEMI(MXLIST), TRMHDI(MXLIST), TRMTGD(MXLIST),
     -      TRMIOE(MXLIST), TRMY(MXLIST), TRMZ(MXLIST),
     -      XTRIM(MXCLUS), YTRIM(MXCLUS), ZTRIM(MXCLUS), ECTRIM(MXCLUS),
     -      EKTRIM(MXCLUS), NETRIM(MXCLUS), ITRIM
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
*  Declarations
       CHARACTER*(MXNAME) RNGFLE, EXYFLE
       INTEGER FAIL, NWORDS, INEXT, I
       INTEGER INPCMP, IFAIL, NCRNG, NCEXY
       LOGICAL OK
       EXTERNAL INPCMP
*  Executable Section
*  Printing the TRIMCAT Message
       PRINT *,' '
       PRINT *,' ======== TRIMCAT Module ========'
       PRINT *,' Garfield TRIM Clustering Model'
       PRINT *,' Contributed by James Butterworth'
       PRINT *,' '
*  Assume it will work
       OK=.TRUE.
*  Set initial values
       CALL TRMINT
*  Count the number of words in the command line
       CALL INPNUM(NWORDS)
        RNGFLE = ' '
        NCRNG = 0
        EXYFLE = ' '
        NCEXY = 0
*  Looping over the each word in the command line
       INEXT = 2
       DO 10 I=1, NWORDS
          IF (I.LT.INEXT) GOTO 10
*	 Gets the name of file RANGE_3D
          IF (INPCMP(I, 'RANGE-#FILE').NE.0) THEN
             IF ((I+1).GT.NWORDS) THEN
                CALL INPMSG(I, 'Argument Missing')
                OK=.FALSE.
             ELSE
                CALL INPSTR(I+1,I+1,RNGFLE,NCRNG)
                INEXT=I+2
             ENDIF
*	 Gets the name of file EXYZ
          ELSEIF (INPCMP(I, 'EXYZ-#FILE').NE.0) THEN
             IF ((I+1).GT.NWORDS) THEN
                CALL INPMSG(I, 'Argument Missing')
                OK=.FALSE.
             ELSE
                CALL INPSTR(I+1,I+1,EXYFLE,NCEXY)
                INEXT=I+2
             ENDIF
*	 Gets the work function
          ELSEIF (INPCMP(I, 'WORK-#FUNCTION').NE.0) THEN
             IF ((I+1).GT.NWORDS) THEN
                CALL INPMSG(I, 'Argument Missing')
                OK=.FALSE.
             ELSE
                CALL INPCHK(I+1,2,IFAIL)
                CALL INPRDR(I+1,WTRIM,0.0)
                IF (WTRIM.LE.0.0) THEN
                   CALL INPMSG(I, 'Must be > 0.')
                   OK=.FALSE.
                ENDIF
                INEXT=I+2
             ENDIF
*	 Gets the Fano factor
          ELSEIF (INPCMP(I, 'FANO-#FACTOR').NE.0) THEN
             IF ((I+1).GT.NWORDS) THEN
                CALL INPMSG(I, 'Argument Missing')
                OK=.FALSE.
             ELSE
                CALL INPCHK(I+1,2,IFAIL)
                CALL INPRDR(I+1,FTRIM,0.174)
                IF (FTRIM.LE.0.0) THEN
                   CALL INPMSG(I, 'Must be > 0.')
                   OK=.FALSE.
                ENDIF
                INEXT=I+2
             ENDIF
*	 Gets the Layer
          ELSEIF (INPCMP(I, 'LAYER').NE.0) THEN
             IF ((I+1).GT.NWORDS) THEN
                CALL INPMSG(I, 'Argument Missing')
                OK=.FALSE.
             ELSE
                CALL INPCHK(I+1,1,IFAIL)
                CALL INPRDI(I+1,LTRIM,0)
                IF (LTRIM.LE.0) THEN
                   CALL INPMSG(I, 'Must be > 0.')
                   OK=.FALSE.
                ENDIF
                INEXT=I+2
             ENDIF
*	 Gets the Ion Number
          ELSEIF (INPCMP(I, 'ION').NE.0) THEN
             IF ((I+1).GT.NWORDS) THEN
                CALL INPMSG(I, 'Argument Missing')
                OK=.FALSE.
             ELSE
                CALL INPCHK(I+1,1,IFAIL)
                CALL INPRDI(I+1,ITRIM,0)
                IF (ITRIM.LE.0) THEN
                   CALL INPMSG(I, 'Must be > 0.')
                   OK=.FALSE.
                ENDIF
                INEXT=I+2
             ENDIF
          ENDIF
10     CONTINUE
*  Print any errors accumulated so far
       CALL INPERR
*  Check that the RANGE filename is not blank
       IF (RNGFLE.EQ.' '.OR.NCRNG.LT.1) THEN
          PRINT*, ' !!!!!! GASTRM WARNING : RANGE_3D file is'//
     -            ' not specified.'
          OK=.FALSE.
       END IF
*  Check that the EXYZ filename is not blank
       IF (EXYFLE.EQ.' '.OR.NCEXY.LT.1) THEN
          PRINT*, ' !!!!!! GASTRM WARNING : EXYZ file is'//
     -            ' not specified.'
          OK=.FALSE.
       END IF
*  Check that the work function is valid
       IF (WTRIM.LE.0.0) THEN
          PRINT*, ' !!!!!! GASTRM WARNING : Work function is'//
     -            ' not correctly specified.'
          OK=.FALSE.
       END IF
*  Check that the fano factor is valid
       IF (FTRIM.LE.0.0.AND.ABS(FTRIM+1.0).GT.1E-4) THEN
          PRINT*, ' !!!!!! GASTRM WARNING : Fano-factor is'//
     -            ' not correctly specified.'
          OK=.FALSE.
       END IF
*  Check that the layer is valid
       IF (LTRIM.LE.0) THEN
          PRINT*, ' !!!!!! GASTRM WARNING : Layer is'//
     -            ' not correctly specified.'
          OK=.FALSE.
       END IF
*  Check that the ion number is valid
       IF (ITRIM.LE.0) THEN
          PRINT*, ' !!!!!! GASTRM WARNING : Ion number is'//
     -            ' not correctly specified.'
          OK=.FALSE.
       END IF
*  If OK then proceed to read in the files
       IF (OK) THEN
          PRINT*, ' Reading in RANGE_3D file.'
          CALL TRMRER(RNGFLE, NCRNG, IFAIL)
          IF (IFAIL.GT.0) THEN
             PRINT*, ' Failed!'
             OK =.FALSE.
          ELSE
             PRINT*, ' Success!'
          END IF
          PRINT*, ' Reading in EXYZ file.'
          CALL TRMREE(EXYFLE, NCEXY, IFAIL)
          IF (IFAIL.GT.0) THEN
             PRINT*, ' Failed!'
             OK =.FALSE.
          ELSE
             PRINT*, ' Success!'
          END IF
          PRINT*, ' Calculating nuclear dE/dx.'
          CALL TRMCAL(IFAIL)
          IF (IFAIL.GT.0) THEN
             PRINT*, ' Failed!'
             OK =.FALSE.
          ELSE
             PRINT*, ' Success!'
          END IF
       END IF
*  If not OK then take appropriate action
       IF(.NOT.OK)THEN
            IF(JFAIL.EQ.1.OR.JFAIL.EQ.2)THEN
                 PRINT *,' !!!!!! GASTRM WARNING : Reading the'//
     -                ' TRIM command failed; command ignored.'
                 FAIL=1
                 PRINT *,' !!!!!! TRIMCAT Module failed.'
                 PRINT *, ' '
            PRINT *,' Continuing with Gas Section'
                 RETURN
            ELSE
                 PRINT *,' !!!!!! GASTRM WARNING : The TRIM'//
     -                ' command failed; program quit.'
                 CALL QUIT
            ENDIF
       ENDIF
*  Successful termination
       FAIL = 0
       TRIMOK =.TRUE.
       PRINT *,' ****** TRIMCAT Module terminated successfully.'
       PRINT *, ' '
       PRINT *,' Continuing with Gas Section'
       END
