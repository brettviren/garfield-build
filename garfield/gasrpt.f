CDECK  ID>, GASRPT.
       SUBROUTINE GASRPT
*-----------------------------------------------------------------------
*   GASRPT - Reads the Penning transfer probabilities.
*   (Last changed on  3/ 8/10.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER NWORD,I,J,K,INEXT,INPCMP,INPTYP,NCSEL,IFAIL1,NSEL,NALL
       REAL RATE,RANGE,DELAY
       CHARACTER*(MXINCH) SELECT
       LOGICAL OK,ALWAYS,MATCH
       EXTERNAL INPCMP,INPTYP
*** Identify.
       IF(LIDENT)PRINT *,' /// ROUTINE GASRPT ///'
*** Read the command line.
       CALL INPNUM(NWORD)
*   List if there are no arguments.
       IF(NWORD.LE.1)THEN
            CALL GASLPT
            RETURN
       ENDIF
*** Check that there is excitation data.
       IF(.NOT.GASSET)THEN
            PRINT *,' !!!!!! GASRPT WARNING : There is no gas data at'//
     -           ' present; PENNING command not executed.'
            RETURN
       ELSEIF(NEXGAS.LE.0.OR..NOT.GASOK(15))THEN
            PRINT *,' !!!!!! GASRPT WARNING : The present gas has no'//
     -           ' excitation rates; PENNING command not executed.'
            RETURN
       ENDIF
       IF(NIOGAS.LE.0.OR..NOT.GASOK(16))
     -      PRINT *,' ------ GASRPT MESSAGE : No ionisation rates'//
     -      ' have been entered for the present gas; all transfers'//
     -      ' will be classified as disallowed.'
*** Loop over the words.
       INEXT=2
       OK=.TRUE.
       DO 10 I=2,NWORD
       IF(I.LT.INEXT)GOTO 10
*   Get the selection flag
       CALL INPSTR(I,I,SELECT,NCSEL)
*   Move to the next word.
       INEXT=I+1
       IF(INEXT.GT.NWORD)THEN
            CALL INPMSG(I,'Rate is missing.')
            OK=.FALSE.
            GOTO 10
       ENDIF
*   Pick up the rate
       CALL INPCHK(INEXT,2,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            OK=.FALSE.
            RATE=0.0
       ELSE
            CALL INPRDR(INEXT,RATE,0.0)
       ENDIF
       IF(RATE.LT.0.OR.RATE.GT.100.0)THEN
            CALL INPMSG(INEXT,'Out of range [0,100]')
            OK=.FALSE.
       ENDIF
       INEXT=INEXT+1
*   Look for other options.
       ALWAYS=.FALSE.
       RANGE=0.0
       DELAY=0.0
       DO 50 J=INEXT,NWORD
       IF(J.LT.INEXT)GOTO 50
*   Check for selection options: do not check
       IF(INPCMP(J,'FORCE#D').NE.0)THEN
            ALWAYS=.TRUE.
            INEXT=J+1
       ELSEIF(INPCMP(J,'ALLOW#ED')+INPCMP(J,'PER#MITTED').NE.0)THEN
            ALWAYS=.FALSE.
            INEXT=J+1
       ELSEIF(INPCMP(J,'RANGE').NE.0)THEN
            CALL INPCHK(J+1,2,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 OK=.FALSE.
                 RANGE=0.0
            ELSE
                 CALL INPRDR(J+1,RANGE,0.0)
            ENDIF
            IF(RANGE.LT.0)THEN
                 CALL INPMSG(INEXT,'Range < 0')
                 OK=.FALSE.
            ENDIF
            INEXT=J+2
       ELSEIF(INPCMP(J,'DELAY').NE.0)THEN
            CALL INPCHK(J+1,2,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 OK=.FALSE.
                 DELAY=0.0
            ELSE
                 CALL INPRDR(J+1,DELAY,0.0)
            ENDIF
            IF(DELAY.LT.0)THEN
                 CALL INPMSG(INEXT,'Delay < 0')
                 OK=.FALSE.
            ENDIF
            INEXT=J+2
       ELSE
            GOTO 60
       ENDIF
50     CONTINUE
60     CONTINUE
*   Debugging.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASRPT DEBUG   : '',
     -      ''Select '',A,'', always = '',L1,'', rate = '',F10.3,
     -      '', range = '',F10.3,'', delay = '',F10.3,'', OK = '',L1)')
     -      SELECT(1:NCSEL),ALWAYS,RATE,RANGE,DELAY,OK
*   If not OK, skip the rest
       IF(.NOT.OK)GOTO 10
**  Store the rate
       NSEL=0
       NALL=0
       DO 20 J=1,NEXGAS
*   Look for matches.
       CALL WLDCRD(DSCEXG(J),SELECT(1:NCSEL),.FALSE.,MATCH)
       IF(.NOT.MATCH)GOTO 20
       NALL=NALL+1
*   Check the energy level.
       IF(.NOT.ALWAYS)THEN
            DO 30 K=1,NIOGAS
            IF(ENIOG(K).LT.ENEXG(J))GOTO 40
30          CONTINUE
            GOTO 20
40          CONTINUE
       ENDIF
       NSEL=NSEL+1
*   Update the probability vector
       PENPRB(J)=RATE/100.0
*   Set the range
       PENRMS(J)=RANGE
*   Set the time delay
       PENDT(J)=DELAY
20     CONTINUE
*   Check we found at least 1 state.
       IF(NALL.LE.0)THEN
            CALL INPMSG(I,'Does no match any excitation.')
       ELSEIF(NSEL.LE.0)THEN
            CALL INPMSG(I,'Only disallowed transfers.')
       ENDIF
10     CONTINUE
*** List error messages
       CALL INPERR
*** Update the Townsend coefficient.
       CALL GASSPT
       END
