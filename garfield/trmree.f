CDECK  ID>, TRMREE.
       SUBROUTINE TRMREE(EXYFLE, NCEXY, FAIL)
*-----------------------------------------------------------------------
*   TRMREE - Reads the TRIM EXYZ file.
*   TRIMCAT Module - Garfield TRIM Clustering Model
*   Contributed by James Butterworth
*   (Last changed on 27/10/11.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
*  Delcarations
       CHARACTER*(*) EXYFLE
       CHARACTER*(80) TEMP, FSTRNG
       INTEGER NCEXY, FAIL, IFAIL, NCTEMP, CION, INFION, COUNTR
       REAL IONIE, INTVAL, TDEPTH, PREVE, PREVX, PREVY, PREVZ, PREVS
       LOGICAL FSTTME, FIRST
*  Executable section
*  Assume failure
       FAIL = 1
*  Opening the file
       CALL DSNOPN(EXYFLE, NCEXY, 12, 'READ-FILE', IFAIL)
       IF (IFAIL.NE.0) THEN
          PRINT*, ' !!!!!! TRMREE WARNING : Unable to open the EXYZ'//
     -            ' file ', EXYFLE(1:NCEXY),'; not read.'
          RETURN
       ENDIF
*  Record the opening
       CALL DSNLOG(EXYFLE(1:NCEXY), 'TRIM EXYZ File', 'Sequential',
     -       'Read only')
*  Switch input stream to data file
       CALL INPSWI('UNIT12')
*  Skipping the header
       FSTTME = .TRUE.
10     CONTINUE
          CALL INPGET
          CALL INPSTR(5, 5, TEMP, NCTEMP)
          IF (TEMP(1:6).NE.'ENERGY') GOTO 10
          IF (FSTTME) THEN
             FSTTME = .FALSE.
             GOTO 10
          END IF
       CONTINUE
*  Getting the ion energy
       CALL INPGET
       CALL INPSTR(3, 3, TEMP, NCTEMP)
       IF (NCTEMP.LE.9) THEN
          FSTRNG = '(F'//CHAR((NCTEMP + 45))//')'
       ELSE
          FSTRNG = '(F1'//CHAR((MOD(NCTEMP,10) + 45))//')'
       END IF
       IF (TEMP(NCTEMP - 3:NCTEMP - 2).EQ.'K') THEN
          READ(TEMP(1:(NCTEMP - 3)),FSTRNG) IONIE
       ELSEIF (TEMP(NCTEMP - 3:NCTEMP - 2).EQ.'M') THEN
          READ(TEMP(1:(NCTEMP - 3)),FSTRNG) IONIE
       ELSEIF (TEMP(NCTEMP - 3:NCTEMP - 2).EQ.'G') THEN
          READ(TEMP(1:(NCTEMP - 3)),FSTRNG) IONIE
       ELSEIF (TEMP(NCTEMP - 3:NCTEMP - 2).EQ.'U') THEN
          READ(TEMP(1:(NCTEMP - 3)),FSTRNG) IONIE
       ELSEIF (TEMP(NCTEMP - 3:NCTEMP - 2).EQ.'N') THEN
          READ(TEMP(1:(NCTEMP - 3)),FSTRNG) IONIE
       ELSE
          READ(TEMP(1:(NCTEMP - 2)),FSTRNG) IONIE
       END IF
       IF(LDEBUG)WRITE(LUNOUT,*) ' ++++++ TRMREE DEBUG   :',
     -		   ' Initial Ion Energy (keV) : ', IONIE
       IONIE = IONIE * 1000
*  Skip heading lines
20     CONTINUE
          CALL INPGET
          CALL INPSTR(1,1,TEMP,NCTEMP)
       IF (TEMP(1:7).NE.'-------') GOTO 20
       CONTINUE
*  Reading through until the desired ion is reached
       INFION = 0
30     CONTINUE
          CALL INPGET
          CALL INPRDI(1, CION, 0)
          IF (CION.NE.INFION) THEN
             INFION = CION
             IF (CION.NE.ITRIM) THEN
                PRINT*, ' Skipping ion ', INFION
             ELSE
                PRINT*, ' Reading ion ', INFION
             END IF
          END IF
          IF (CION.EQ.0) THEN
             PRINT*, ' !!!!!! TRMREE WARNING : Unable to find the'//
     -            ' specified ion: ', ITRIM,'.'
             RETURN
          END IF
          IF (CION.NE.ITRIM) GOTO 30
       CONTINUE
*  Reading in the ion data
       COUNTR = 0
       INTVAL = (TRMLMX - TRMLMN) / 100
       FIRST = .TRUE.
40     CONTINUE
          PREVE = 0.0
          PREVX = 0.0
          PREVY = 0.0
          PREVZ = 0.0
          PREVS = 0.0
60        CONTINUE
             CALL INPRDR(2, PREVE, 0.0)
             CALL INPRDR(3, PREVX, 0.0)
             CALL INPRDR(4, PREVY, 0.0)
             CALL INPRDR(5, PREVZ, 0.0)
             CALL INPRDR(6, PREVS, 0.0)
             CALL INPGET
             CALL INPRDR(3, TDEPTH, 0.0)
             CALL INPRDI(1, CION, 0)
             IF (CION.NE.ITRIM) GOTO 50
          IF (TDEPTH.LT.(COUNTR*INTVAL)) GOTO 60
*** Next line commented out, label not used (RV 27/10/11)
*70        CONTINUE
          IF ((TDEPTH.GT.TRMLMN).AND.(TDEPTH.LE.TRMLMX)) THEN
             IF (FIRST) THEN
                NTRIM = 0
                FIRST = .FALSE.
             END IF
             NTRIM = NTRIM + 1
             IF ((COUNTR*INTVAL).EQ.TDEPTH) THEN
                CALL INPRDR(3, TRMTGD(NTRIM), 0.0)
                CALL INPRDR(2, TRMIOE(NTRIM), 0.0)
                CALL INPRDR(4, TRMY(NTRIM), 0.0)
                CALL INPRDR(5, TRMZ(NTRIM), 0.0)
                CALL INPRDR(6, TRMEMI(NTRIM), 0.0)
             ELSE
                TRMTGD(NTRIM) = COUNTR*INTVAL
                TRMIOE(NTRIM) = PREVE
                TRMY(NTRIM) = PREVY
                TRMZ(NTRIM) = PREVZ
                TRMEMI(NTRIM) = PREVS
             END IF
          END IF
          COUNTR = COUNTR + 1
       IF (CION.EQ.ITRIM) GOTO 40
50     CONTINUE
*  First value of dE/dX may be too high - residual from previous layer
       IF (TRMEMI(1).GT.(10*TRMEMI(2))) THEN
          TRMEMI(1) = TRMEMI(2)
       END IF
*  End of subroutine - revert to normal data stream
       CALL INPSWI('RESTORE')
       CLOSE(UNIT=12, ERR=2030)
       FAIL = 0
*  Print out debug message
       IF (LDEBUG) THEN
          PRINT*, ' ++++++ TRMREE DEBUG   : Values read in from file.'
          PRINT 100
100       FORMAT (1X,8X,'X',12X,'Y',11X,'Z',11X,'E',4X,'dE/dX EM')
          COUNTR = 1
110       CONTINUE
             PRINT 120, TRMTGD(COUNTR), TRMY(COUNTR), TRMZ(COUNTR),
     -                  TRMIOE(COUNTR), TRMEMI(COUNTR)
             COUNTR = COUNTR + 1
          IF (COUNTR.NE.NTRIM) GOTO 110
          CONTINUE
120       FORMAT (1X,G9.3,3X,G10.3,3X,G9.3,3X,G9.3,3X,G9.3)
       END IF
       RETURN
*  Error Handling
2030   CONTINUE
       PRINT*, ' !!!!!! TRMREE WARNING : Error closing the EXYZ'//
     -         ' file ', EXYFLE(1:NCEXY),'; no immediate problems'//
     -         ' expected.'
       END