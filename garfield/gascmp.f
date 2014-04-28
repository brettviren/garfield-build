CDECK  ID>, GASCMP.
      SUBROUTINE GASCMP(IFAIL)
*-----------------------------------------------------------------------
*   GASCMP - Records the Magboltz gas composition.
*   (Last changed on  3/ 4/08.)
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
       INTEGER IFAIL,I,INEXT,NWORD,IFAIL1,INPTYP,INPCMP
       REAL AUX,FRASUM
       LOGICAL OK
       EXTERNAL INPTYP,INPCMP
*** Get number of words.
       CALL INPNUM(NWORD)
*** List gases in case there are no arguments.
       IF(NWORD.LE.1)THEN
            WRITE(LUNOUT,'(''  Current Magboltz gas composition:'')')
            FRASUM=0
            DO 50 I=1,MXNBMC
            IF(GASFRM(I).GT.0)WRITE(LUNOUT,'(5X,''Ingredient '',I3,
     -           '', fraction: '',F10.3)') I,GASFRM(I)
            FRASUM=FRASUM+GASFRM(I)
50          CONTINUE
            WRITE(LUNOUT,'(5X,''Sum of fractions:'',9X,F10.3)') FRASUM
            RETURN
       ENDIF
*** Preset the gas mixture fractions.
       DO 10 I=1,MXNBMC
       GASFRM(I)=0.0
10     CONTINUE
*** Read the command line.
       INEXT=2
*   Control of whether to proceed.
       OK=.TRUE.
       DO 20 I=2,NWORD
       IF(I.LT.INEXT)GOTO 20
*** Fractions, CF4 (current data).
       IF(INPCMP(I,'CF4')+
     -      INPCMP(I,'FREON-#14')+
     -      INPCMP(I,'TETRAFLUOROMETHANE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(1)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Argon (isotropic).
       ELSEIF(INPCMP(I,'AR#GON-ISO#TROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(2)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Helium 4 (isotropic).
       ELSEIF(INPCMP(I,'HE#LIUM-4-ISO#TROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(3)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Helium 3 (isotropic).
       ELSEIF(INPCMP(I,'HE#LIUM-3-ISO#TROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(4)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Neon (isotropic).
       ELSEIF(INPCMP(I,'NEON-ISO#TROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(5)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Krypton (isotropic).
       ELSEIF(INPCMP(I,'KR#YPTON-ISO#TROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(6)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Xenon (isotropic).
       ELSEIF(INPCMP(I,'XE#NON-ISO#TROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(7)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Methane (only version, isotropic).
       ELSEIF(INPCMP(I,'METHANE')+
     -      INPCMP(I,'CH4').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(8)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Ethane.
       ELSEIF(INPCMP(I,'ETHANE')+
     -      INPCMP(I,'C2H6').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(9)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Propane.
       ELSEIF(INPCMP(I,'PROPA#NE')+
     -      INPCMP(I,'C3H8').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(10)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Isobutane.
       ELSEIF(INPCMP(I,'ISO#BUTANE')+
     -      INPCMP(I,'IC4H10')+
     -      INPCMP(I,'ISO-C4H10')+
     -      INPCMP(I,'C4H10').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(11)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   CO2 (isotropic).
       ELSEIF(INPCMP(I,'CO2')+
     -      INPCMP(I,'CARB#ON-DIOX#IDE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(12)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Neopentane (current data)
       ELSEIF(INPCMP(I,'NEOPENT#ANE')+
     -      INPCMP(I,'NEO-PENT#ANE')+
     -      INPCMP(I,'NEO-C5H12')+
     -      INPCMP(I,'C5H12').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(13)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Water (current data).
       ELSEIF(INPCMP(I,'WA#TER-#VAPOUR')+
     -      INPCMP(I,'H2O').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(14)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Oxygen (current data).
       ELSEIF(INPCMP(I,'OX#YGEN')+
     -      INPCMP(I,'O2').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(15)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Nitrogen (isotropic).
       ELSEIF(INPCMP(I,'NI#TROGEN-ISO#TROPIC')+
     -      INPCMP(I,'N2-ISO#TROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(16)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Nitric oxide (NO).
       ELSEIF(INPCMP(I,'NITRI#C-OX#IDE')+
     -      INPCMP(I,'NITROGEN-MONOXIDE')+
     -      INPCMP(I,'NO').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(17)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Nitrous oxide (N2O).
       ELSEIF(INPCMP(I,'NITRO#US-OX#IDE')+
     -      INPCMP(I,'DINITROGEN-MONOXIDE')+
     -      INPCMP(I,'LAUGHING-GAS')+
     -      INPCMP(I,'N2O').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(18)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Ethene (C2H4).
       ELSEIF(INPCMP(I,'ETHE#NE')+
     -      INPCMP(I,'ETHYLENE')+
     -      INPCMP(I,'C2H4').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(19)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Acetylene (C2H2).
       ELSEIF(INPCMP(I,'ACETYL#ENE')+
     -      INPCMP(I,'ETHYNE')+
     -      INPCMP(I,'C2H2').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(20)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Hydrogen.
       ELSEIF(INPCMP(I,'HYDROGEN')+
     -      INPCMP(I,'H2').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(21)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Deuterium.
       ELSEIF(INPCMP(I,'DEUTERIUM')+
     -      INPCMP(I,'D2').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(22)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Carbon monoxide (CO).
       ELSEIF(INPCMP(I,'CO')+
     -      INPCMP(I,'CARB#ON-MONOX#IDE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(23)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Methylal (dimethoxymethane, CH3-O-CH2-O-CH3, "hot" version).
       ELSEIF(INPCMP(I,'METHY#LAL-#HOT')+
     -      INPCMP(I,'DIMETHOXYMETHANE-#HOT')+
     -      INPCMP(I,'DMM-#HOT')+
     -      INPCMP(I,'C3H8O2-#HOT').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(24)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   DME.
       ELSEIF(INPCMP(I,'DME')+
     -      INPCMP(I,'DIMETHYL-ETHER')+
     -      INPCMP(I,'METHOXYMETHANE')+
     -      INPCMP(I,'METHYL-ETHER')+
     -      INPCMP(I,'WOOD-ETHER')+
     -      INPCMP(I,'C2H6O').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(25)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Reid step.
       ELSEIF(INPCMP(I,'REID-STEP').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(26)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Maxwell model.
       ELSEIF(INPCMP(I,'MAXWELL-MODEL').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(27)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Reid ramp.
       ELSEIF(INPCMP(I,'REID-RAMP').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(28)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   C2F6.
       ELSEIF(INPCMP(I,'C2F6')+
     -      INPCMP(I,'FREON-116')+
     -      INPCMP(I,'ZYRON-116-#N5')+
     -      INPCMP(I,'HEXAFLUOROETHANE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(29)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   SF6.
       ELSEIF(INPCMP(I,'SF6')+
     -      INPCMP(I,'SULPHUR-HEXAFLUORIDE')+
     -      INPCMP(I,'SULFUR-HEXAFLUORIDE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(30)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   NH3.
       ELSEIF(INPCMP(I,'AMMONIA')+
     -      INPCMP(I,'NH3').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(31)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Propene.
       ELSEIF(INPCMP(I,'PROPE#NE')+
     -      INPCMP(I,'PROPYLENE')+
     -      INPCMP(I,'C3H6').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(32)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Cyclopropane.
       ELSEIF(INPCMP(I,'C#YCLO-PROPA#NE')+
     -      INPCMP(I,'CYCLOPROPA#NE')+
     -      INPCMP(I,'C#YCLO-C3H6').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(33)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Methanol.
       ELSEIF(INPCMP(I,'METHANOL')+
     -      INPCMP(I,'METHYL-ALCOHOL')+
     -      INPCMP(I,'WOOD-ALCOHOL')+
     -      INPCMP(I,'CH3OH').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(34)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Ethanol.
       ELSEIF(INPCMP(I,'ETHANOL')+
     -      INPCMP(I,'ETHYL-ALCOHOL')+
     -      INPCMP(I,'GRAIN-ALCOHOL')+
     -      INPCMP(I,'C2H5OH').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(35)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Propanol.
       ELSEIF(INPCMP(I,'PROPANOL')+
     -      INPCMP(I,'2-PROP#ANOL')+
     -      INPCMP(I,'ISO-PROP#ANOL')+
     -      INPCMP(I,'ISOPROP#ANOL')+
     -      INPCMP(I,'ISOPROPYL-#ALCOHOL')+
     -      INPCMP(I,'C3H7OH').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(36)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Cesium / Caesium.
       ELSEIF(INPCMP(I,'CE#SIUM')+
     -      INPCMP(I,'CAE#SIUM')+
     -      INPCMP(I,'CS').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(37)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Fluorine.
       ELSEIF(INPCMP(I,'FLUOR#INE')+
     -      INPCMP(I,'F2').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(38)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   CS2.
       ELSEIF(INPCMP(I,'CS2')+
     -      INPCMP(I,'C#ARBON-DISULPH#IDE')+
     -      INPCMP(I,'C#ARBON-DISULF#IDE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(39)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   COS.
       ELSEIF(INPCMP(I,'COS')+
     -      INPCMP(I,'C#ARBONYL-SULPH#IDE')+
     -      INPCMP(I,'C#ARBONYL-SULF#IDE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(40)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Deuterated methane.
       ELSEIF(INPCMP(I,'DEUT#ERIUM-METHANE')+
     -      INPCMP(I,'DEUT#ERATED-METHANE')+
     -      INPCMP(I,'CD4').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(41)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   BF3.
       ELSEIF(INPCMP(I,'BF3')+
     -      INPCMP(I,'BORON-TRIFLUORIDE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(42)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   C2HF5 and C2H2F4.
       ELSEIF(INPCMP(I,'C2HF5')+
     -      INPCMP(I,'C2H2F4')+
     -      INPCMP(I,'C2F5H')+
     -      INPCMP(I,'C2F4H2')+
     -      INPCMP(I,'FREON-134-#A')+
     -      INPCMP(I,'FREON-125')+
     -      INPCMP(I,'ZYRON-125')+
     -      INPCMP(I,'TETRAFLUOROETHANE')+
     -      INPCMP(I,'PENTAFLUOROETHANE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(43)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Helium 3 (anisotropic).
       ELSEIF(INPCMP(I,'HE#LIUM-3-#ANISOTROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(44)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Helium 4 (anisotropic).
       ELSEIF(INPCMP(I,'HE#LIUM-#4-#ANISOTROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(45)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Neon (anisotropic).
       ELSEIF(INPCMP(I,'NEON-#ANISOTROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(46)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Argon (anisotropic).
       ELSEIF(INPCMP(I,'AR#GON-#ANISOTROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(47)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Krypton (anisotropic).
       ELSEIF(INPCMP(I,'KR#YPTON-#ANISOTROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(48)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Xenon (anisotropic).
       ELSEIF(INPCMP(I,'XE#NON-#ANISOTROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(49)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   CHF3.
       ELSEIF(INPCMP(I,'CHF3')+
     -      INPCMP(I,'FREON-23')+
     -      INPCMP(I,'TRIFLUOROMETHANE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(50)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   CF3Br.
       ELSEIF(INPCMP(I,'CF3BR')+
     -      INPCMP(I,'TRIFLUOROBROMOMETHANE')+
     -      INPCMP(I,'HALON-1301')+
     -      INPCMP(I,'FREON-13B1').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(51)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   C3F8.
       ELSEIF(INPCMP(I,'C3F8')+
     -      INPCMP(I,'OCTAFLUOROPROPANE')+
     -      INPCMP(I,'R218')+
     -      INPCMP(I,'FREON-218')+
     -      INPCMP(I,'PERFLUOROPROPANE')+
     -      INPCMP(I,'RC-218')+
     -      INPCMP(I,'PFC-218').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(52)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Ozone.
       ELSEIF(INPCMP(I,'OZONE')+
     -      INPCMP(I,'O3').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(53)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Mercury.
       ELSEIF(INPCMP(I,'MERCURY')+
     -      INPCMP(I,'HG')+
     -      INPCMP(I,'HG2').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(54)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   H2S.
       ELSEIF(INPCMP(I,'H2S')+
     -      INPCMP(I,'HYDROGEN-SULPHIDE')+
     -      INPCMP(I,'HYDROGEN-SULFIDE')+
     -      INPCMP(I,'HEPATIC-ACID')+
     -      INPCMP(I,'SEWER-GAS')+
     -      INPCMP(I,'SULFUR-HYDRIDE')+
     -      INPCMP(I,'DIHYDROGEN-MONOSULFIDE')+
     -      INPCMP(I,'DIHYDROGEN-MONOSULPHIDE')+
     -      INPCMP(I,'SULPHUR-HYDRIDE')+
     -      INPCMP(I,'STINK-DAMP')+
     -      INPCMP(I,'SULFURETED-HYDROGEN ').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(55)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   n-butane.
       ELSEIF(INPCMP(I,'N-BUTANE')+
     -      INPCMP(I,'N-C4H10').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(56)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   n-pentane.
       ELSEIF(INPCMP(I,'N-PENTANE')+
     -      INPCMP(I,'N-C5H12').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(57)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Nitrogen.
       ELSEIF(INPCMP(I,'NI#TROGEN-#ANISOTROPIC')+
     -      INPCMP(I,'N2-#ANISOTROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(58)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Germane, GeH4.
       ELSEIF(INPCMP(I,'GERM#ANE')+
     -      INPCMP(I,'GERMANIUM-HYDRIDE')+
     -      INPCMP(I,'GERMANIUM-TETRAHYDRIDE')+
     -      INPCMP(I,'GERMANOMETHANE')+
     -      INPCMP(I,'MONOGERMANE')+
     -      INPCMP(I,'GEH4').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(59)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Silane, SiH4.
       ELSEIF(INPCMP(I,'SIL#ANE')+
     -      INPCMP(I,'SILICON-HYDRIDE')+
     -      INPCMP(I,'SILICON-TETRAHYDRIDE')+
     -      INPCMP(I,'SILICANE')+
     -      INPCMP(I,'MONOSILANE')+
     -      INPCMP(I,'SIH4').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 GASFRM(60)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
       ELSE
            CALL INPMSG(I,'Not a known keyword')
            OK=.FALSE.
       ENDIF
20     CONTINUE
*** Dump error messages.
       CALL INPERR
*** Check and debugging.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASCMP DEBUG   : The gas'',
     -      '' composition follows:'')')
       FRASUM=0
       DO 30 I=1,MXNBMC
       IF(GASFRM(I).LT.0)THEN
            PRINT *,' !!!!!! GASCMP WARNING : Negative fraction for'//
     -           ' gas component ',I,'; fraction set to 0.'
            GASFRM(I)=0
            OK=.FALSE.
       ELSEIF(GASFRM(I).GT.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Ingredient '',I3,
     -           '', fraction: '',F10.3)') I,GASFRM(I)
            FRASUM=FRASUM+GASFRM(I)
       ENDIF
30     CONTINUE
       IF(FRASUM.LE.0)THEN
            PRINT *,' !!!!!! GASCMP WARNING : Sum of gas fractions'//
     -           ' less or equal to 0; no mixture set.'
            OK=.FALSE.
       ELSEIF(LDEBUG)THEN
            WRITE(LUNOUT,'(26X,''Sum of fractions:         '',
     -           F10.3)') FRASUM
       ENDIF
*** Error processing.
       IF(JFAIL.EQ.2.AND..NOT.OK)THEN
            PRINT *,' ###### GASCMP ERROR   : Magboltz mixture not'//
     -           ' set because of the above errors.'
            DO 40 I=1,MXNBMC
            GASFRM(I)=0
40          CONTINUE
       ELSEIF(JFAIL.EQ.3.AND..NOT.OK)THEN
            PRINT *,' ###### GASCMP ERROR   : Program terminated'//
     -           ' because of the above errors.'
            CALL QUIT
            RETURN
       ELSEIF(.NOT.OK)THEN
            PRINT *,' !!!!!! GASCMP WARNING : Continuing inspite'//
     -           ' of the above problems; verify gas composition.'
       ENDIF
       END
