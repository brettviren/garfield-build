CDECK  ID>, DLCIO1.
       SUBROUTINE DLCIO1(IORATE)
*-----------------------------------------------------------------------
*   DLCIO1 - Routine returning the number of ionisations for the current
*            drift line. The routine uses an adaptive Simpson style
*            integration.
*   VARIABLES : ALFA.      : Townsend coefficients (1,2 end; M middle).
*               ALFINT     : Integral of the Townsend coefficient.
*               IORATE     : Returned rates.
*   (Last changed on  1/ 3/08.)
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
       DOUBLE PRECISION XU,YU,ZU,TU,XTARG,YTARG,TMC,DMC
       REAL DXMIN,DYMIN,DZMIN,DXMAX,DYMAX,DZMAX,DTARG,EPSDFI,EPSTWI,
     -      EPSATI,RDF2,DSCMIN,DSCMAX,DTFACT,
     -      DDXMIN,DDXMAX,DDYMIN,DDYMAX,DDZMIN,DDZMAX,EPSDIF,RTRAP,
     -      STMAX,EQTTHR,EQTASP,EQTCLS,QPCHAR
       INTEGER NU,ISTAT,ITARG,MXDIFS,MXTWNS,MXATTS,MDF2,
     -      ISTAT1,ISTAT2,ISTAT3,ISTAT4,ISTAT5,ISTAT6,NMC,MCMETH,
     -      IPTYPE,IPTECH
       LOGICAL LREPSK,LKINK,LSTMAX,LEQSRT,LEQCRS,LEQMRK,LAVPRO
       COMMON /DRFDAT/ XU(MXLIST),YU(MXLIST),ZU(MXLIST),TU(MXLIST),
     -      XTARG,YTARG,TMC,DMC,DTARG,
     -      DXMIN,DYMIN,DZMIN,DXMAX,DYMAX,DZMAX,
     -      DDXMIN,DDXMAX,DDYMIN,DDYMAX,DDZMIN,DDZMAX,
     -      EQTTHR,EQTASP,EQTCLS,QPCHAR,
     -      RTRAP,STMAX,EPSDIF,EPSDFI,EPSTWI,EPSATI,RDF2,DSCMIN,DSCMAX,
     -      DTFACT,MDF2,
     -      MXDIFS,MXTWNS,MXATTS,
     -      NU,ISTAT,ITARG,
     -      ISTAT1,ISTAT2,ISTAT3,ISTAT4,ISTAT5,ISTAT6,NMC,MCMETH,IPTYPE,
     -      IPTECH,LREPSK,LKINK,LSTMAX,LEQSRT,LEQCRS,LEQMRK,LAVPRO
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL TWNVEC(MXLIST),ALFA1,ALFA2,ALFAM,EXM,EYM,EZM,ETOTM,
     -      EX,EY,EZ,ETOT,BX,BY,BZ,BTOT,VOLT,GASTWN,GASVEL,DRES,
     -      BXM,BYM,BZM,BTOTM,IOVEC(MXIOG,MXLIST),IOVECM(MXIOG),
     -      IOVEC1(MXIOG),IOVEC2(MXIOG),VEL,VELM,IORATE(MXIOG)
       DOUBLE PRECISION XAUX1,XAUX2,YAUX1,YAUX2,STEP,ALFINT,
     -      XPOS1,XPOS2,XPOSM,YPOS1,YPOS2,YPOSM,ZPOSM,ZPOS1,ZPOS2,
     -      TOTSTP,CRUDE,IONCR(MXIOG),STACK(MXSTCK,4+MXIOG),SINT
       INTEGER LOCVEC(MXLIST),ISTACK,NSTACK,ILOCRS,NFC,ILOCM,IU,ILOC,
     -      I,J,STRLEN
       EXTERNAL GASTWN,GASVEL,STRLEN
*** Identify the routine
       IF(LIDENT)PRINT *,' /// ROUTINE DLCIO1 ///'
*** Default results.
       DO 110 J=1,NIOGAS
       IONCR(J)=0
110    CONTINUE
*** Return straight away if there is only one data point.
       IF(NU.LE.1)RETURN
*   Check that this is an electron drift line.
       IF(IPTYPE.NE.1.OR.IPTECH.NE.1)THEN
            PRINT *,' !!!!!! DLCIO1 WARNING : This is not an RKF'//
     -           ' integrated electron drift line; no integration.'
            RETURN
       ENDIF
*** Obtain a very rough estimate of the result.
       CRUDE=0.0
       DO 100 IU=1,NU
*   Evaluate the fields.
       CALL EFIELD(REAL(XU(IU)),REAL(YU(IU)),REAL(ZU(IU)),
     -      EX,EY,EZ,ETOT,VOLT,0,ILOC)
       CALL BFIELD(REAL(XU(IU)),REAL(YU(IU)),REAL(ZU(IU)),
     -      BX,BY,BZ,BTOT)
*   Cheat in case the point is located inside a wire.
       IF(ILOC.GT.0.AND.ILOC.LE.MXWIRE)THEN
            DRES=D(ILOC)
            ILOCRS=ILOC
            D(ILOCRS)=0.0
            CALL EFIELD(REAL(XU(IU)),REAL(YU(IU)),REAL(ZU(IU)),
     -           EX,EY,EZ,ETOT,VOLT,0,ILOC)
            CALL BFIELD(REAL(XU(IU)),REAL(YU(IU)),REAL(ZU(IU)),
     -           BX,BY,BZ,BTOT)
            D(ILOCRS)=DRES
            IF(LDEBUG)PRINT *,' ++++++ DLCIO1 DEBUG   : Drift-line',
     -           ' data point in wire ',ILOCRS,' detected; d=0 fix.'
       ENDIF
*   In case this didn't help, just log the failure.
       LOCVEC(IU)=ILOC
*   Calculate Townsend coefficients and ionisation rates.
       IF(POLAR)THEN
            TWNVEC(IU)=GASTWN(EX/EXP(REAL(XU(IU))),EY/EXP(REAL(XU(IU))),
     -           EZ,BX,BY,BZ)
            VEL=GASVEL(EX/EXP(REAL(XU(IU))),EY/EXP(REAL(XU(IU))),
     -           EZ,BX,BY,BZ)
            CALL GASIOR(EX/EXP(REAL(XU(IU))),EY/EXP(REAL(XU(IU))),
     -           EZ,BX,BY,BZ,IOVEC(1,IU))
            IF(IU.GT.1)THEN
                 CALL CF2RTC(XU(IU),YU(IU),XAUX1,YAUX1,1)
                 CALL CF2RTC(XU(IU-1),YU(IU-1),XAUX2,YAUX2,1)
                 STEP=SQRT((XAUX2-XAUX1)**2+(YAUX2-YAUX1)**2+
     -                (ZU(IU)-ZU(IU-1))**2)
            ENDIF
       ELSE
            TWNVEC(IU)=GASTWN(EX,EY,EZ,BX,BY,BZ)
            VEL=GASVEL(EX,EY,EZ,BX,BY,BZ)
            CALL GASIOR(EX,EY,EZ,BX,BY,BZ,IOVEC(1,IU))
            IF(IU.GT.1)STEP=SQRT((XU(IU)-XU(IU-1))**2+
     -           (YU(IU)-YU(IU-1))**2+(ZU(IU)-ZU(IU-1))**2)
       ENDIF
*   Convert rates from THz to 1/cm via v in cm/microsec.
       DO 130 J=1,NIOGAS
       IOVEC(J,IU)=1.0E6*IOVEC(J,IU)/VEL
       IF(IOVEC(J,IU).LT.0)THEN
            PRINT *,' !!!!!! DLCIO1 WARNING : Met negative'//
     -           ' ionisation rate ',IOVEC(J,IU),' THz of level ',J,
     -           ' at step ',IU,'/',NU,'; set to 0.'
            IOVEC(J,IU)=0
       ENDIF
130    CONTINUE
*   Compute the gain
       IF(IU.GT.1)THEN
            IF(CRUDE.LT.40.0)THEN
                 DO 120 J=1,NIOGAS
                 CALL INTEXC(DBLE(TWNVEC(IU-1)),DBLE(TWNVEC(IU)),
     -                DBLE(IOVEC(J,IU-1)),DBLE(IOVEC(J,IU)),
     -                STEP,EXP(CRUDE),SINT)
                      IONCR(J)=IONCR(J)+SINT
120              CONTINUE
            ELSE
                 PRINT *,' !!!!!! DLCIO1 WARNING : Reduced precision'//
     -                ' in the integration of the ionisation rates.'
            ENDIF
            CRUDE=CRUDE+STEP*(TWNVEC(IU)+TWNVEC(IU-1))/2.0
       ENDIF
*   Compute the ionisations
100    CONTINUE
       NFC=NU
*** Print a heading for the debugging output.
       IF(LDEBUG)THEN
            PRINT *,' ++++++ DLCIO1 DEBUG   : Crude rates:'
            DO 220 I=1,NIOGAS
            PRINT *,' Level ',I,': ',IONCR(I),' (',
     -           DSCIOG(I)(1:STRLEN(DSCIOG(I))),')'
220         CONTINUE
            PRINT *,' ++++++ DLCIO1 DEBUG   : Crude gain: ',
     -           EXP(MIN(40.0,CRUDE))
            PRINT *,' ++++++ DLCIO1 DEBUG   : Townsend integration',
     -           ' debugging output follows:'
            PRINT *,' '
            PRINT *,'  IU  loc              XU(IU)'//
     -           '              YU(IU)'//
     -           '              ZU(IU)'//
     -           ' number of electrons'
            PRINT *,'                         [cm]'//
     -           '                [cm]'//
     -           '                [cm]'//
     -           '           [numeric]'
            PRINT *,' '
            PRINT '(2(2X,I3),3(5X,E15.8))',1,LOCVEC(1),XU(1),YU(1),ZU(1)
       ENDIF
*** Initialise the sum ALFINT
       ALFINT=0.0
       DO 210 J=1,NIOGAS
       IONCR(J)=0
210    CONTINUE
*** Loop over the whole drift-line.
       ISTACK=0
       DO 10 IU=1,NU-1
       IF(LOCVEC(IU).NE.0.OR.LOCVEC(IU+1).NE.0)GOTO 30
*   Initial values for the position.
       XPOS1=XU(IU)
       YPOS1=YU(IU)
       ZPOS1=ZU(IU)
       ALFA1=TWNVEC(IU)
       XPOS2=XU(IU+1)
       YPOS2=YU(IU+1)
       ZPOS2=ZU(IU+1)
       ALFA2=TWNVEC(IU+1)
       DO 160 J=1,NIOGAS
       IOVEC1(J)=IOVEC(J,IU)
       IOVEC2(J)=IOVEC(J,IU+1)
160    CONTINUE
*   Calculate the total steplength, in Cartesian coordinates.
       IF(POLAR)THEN
            CALL CF2RTC(XPOS1,YPOS1,XAUX1,YAUX1,1)
            CALL CF2RTC(XPOS2,YPOS2,XAUX2,YAUX2,1)
            TOTSTP=SQRT((XAUX2-XAUX1)**2+(YAUX2-YAUX1)**2+
     -           (ZPOS2-ZPOS1)**2)
       ELSE
            TOTSTP=SQRT((XPOS2-XPOS1)**2+(YPOS2-YPOS1)**2+
     -           (ZPOS2-ZPOS1)**2)
       ENDIF
*   Return at this point of further refinement is needed.
       NSTACK=0
20     CONTINUE
*   Set the new middle point, to be used for comparison.
       XPOSM=0.5*(XPOS1+XPOS2)
       YPOSM=0.5*(YPOS1+YPOS2)
       ZPOSM=0.5*(ZPOS1+ZPOS2)
*   Compute the field and the Townsend coeff. at the middle point.
       CALL EFIELD(REAL(XPOSM),REAL(YPOSM),REAL(ZPOSM),
     -      EXM,EYM,EZM,ETOTM,VOLT,0,ILOCM)
       CALL BFIELD(REAL(XPOSM),REAL(YPOSM),REAL(ZPOSM),
     -      BXM,BYM,BZM,BTOTM)
       NFC=NFC+1
*   Cheat in case the point is located inside a wire.
       IF(ILOCM.GT.0.AND.ILOCM.LE.MXWIRE)THEN
            DRES=D(ILOCM)
            ILOCRS=ILOCM
            D(ILOCRS)=0.0
            CALL EFIELD(REAL(XPOSM),REAL(YPOSM),REAL(ZPOSM),
     -           EXM,EYM,EZM,ETOTM,VOLT,0,ILOCM)
            CALL BFIELD(REAL(XPOSM),REAL(YPOSM),REAL(ZPOSM),
     -           BXM,BYM,BZM,BTOTM)
            NFC=NFC+1
            D(ILOCRS)=DRES
            IF(LDEBUG)PRINT *,' ++++++ DLCIO1 DEBUG   : Intermediate',
     -           ' point in wire ',ILOCRS,' detected; d=0 fix.'
       ENDIF
*   Skip this step in case the ILOC is not due to a wire.
       IF(ILOCM.NE.0)GOTO 30
       IF(POLAR)THEN
            ALFAM=GASTWN(EXM/EXP(REAL(XPOSM)),EYM/EXP(REAL(XPOSM)),EZM,
     -           BXM,BYM,BZM)
            VELM=GASVEL(EXM/EXP(REAL(XPOSM)),EYM/EXP(REAL(XPOSM)),EZM,
     -           BXM,BYM,BZM)
            CALL GASIOR(EXM/EXP(REAL(XPOSM)),EYM/EXP(REAL(XPOSM)),EZM,
     -           BXM,BYM,BZM,IOVECM)
       ELSE
            ALFAM=GASTWN(EXM,EYM,EZM,BXM,BYM,BZM)
            VELM=GASVEL(EXM,EYM,EZM,BXM,BYM,BZM)
            CALL GASIOR(EXM,EYM,EZM,BXM,BYM,BZM,IOVECM)
       ENDIF
*   Convert rates from THz to 1/cm via v in cm/microsec.
       DO 140 J=1,NIOGAS
       IOVECM(J)=1.0E6*IOVECM(J)/VELM
       IF(IOVECM(J).LT.0)THEN
            PRINT *,' !!!!!! DLCIO1 WARNING : Met negative'//
     -           ' ionisation rate ',IOVECM(J),' THz of level ',J,
     -           '; set to 0.'
            IOVECM(J)=0
       ENDIF
140    CONTINUE
*   Compare first and second order estimates, divide if too large.
       IF(NSTACK.LT.MIN(MXSTCK,MXTWNS).AND.EPSTWI*CRUDE.LT.
     -      TOTSTP*ABS(ALFA1-2.0*ALFAM+ALFA2)/3.0)THEN
            NSTACK=NSTACK+1
            ISTACK=MAX(ISTACK,NSTACK)
            STACK(NSTACK,1)=XPOS2
            STACK(NSTACK,2)=YPOS2
            STACK(NSTACK,3)=ZPOS2
            STACK(NSTACK,4)=ALFA2
            XPOS2=XPOSM
            YPOS2=YPOSM
            ZPOS2=ZPOSM
            ALFA2=ALFAM
            DO 170 J=1,NIOGAS
            STACK(NSTACK,4+J)=IOVEC2(J)
            IOVEC2(J)=IOVECM(J)
170         CONTINUE
            GOTO 20
*   No further subdevision is required, transform polar coordinates.
       ELSE
*   Make sure the distances are measured in cartesian coordinates.
            IF(POLAR)THEN
                 CALL CF2RTC(XPOS1,YPOS1,XAUX1,YAUX1,1)
                 CALL CF2RTC(XPOS2,YPOS2,XAUX2,YAUX2,1)
                 STEP=SQRT((XAUX2-XAUX1)**2+(YAUX2-YAUX1)**2+
     -                (ZPOS2-ZPOS1)**2)
            ELSE
                 STEP=SQRT((XPOS2-XPOS1)**2+(YPOS2-YPOS1)**2+
     -                (ZPOS2-ZPOS1)**2)
            ENDIF
*   Add the new rates
            IF(ALFINT+STEP*(ALFA1+2*ALFAM).LT.40.0)THEN
                 DO 150 J=1,NIOGAS
                 CALL INTEXC(DBLE(ALFA1),DBLE(ALFAM),
     -                DBLE(IOVEC1(J)),DBLE(IOVECM(J)),STEP/2,
     -                EXP(ALFINT),SINT)
                 IONCR(J)=IONCR(J)+SINT
                 CALL INTEXC(DBLE(ALFAM),DBLE(ALFA2),
     -                DBLE(IOVECM(J)),DBLE(IOVEC2(J)),STEP/2,
     -                EXP(ALFINT+STEP*(5.0*ALFA1+8.0*ALFAM-ALFA2)/24.0),
     -                SINT)
                 IONCR(J)=IONCR(J)+SINT
150              CONTINUE
            ELSE
                 PRINT *,' !!!!!! DLCIO1 WARNING : Reduced precision'//
     -                ' in the integration of the ionisation rates.'
            ENDIF
*   Add the new term to the integral.
            ALFINT=ALFINT+STEP*(ALFA1+4.0*ALFAM+ALFA2)/6.0
*   Continue with the next segment (if complete) or the next subsegment.
            XPOS1=XPOS2
            YPOS1=YPOS2
            ZPOS1=ZPOS2
            ALFA1=ALFA2
            DO 180 J=1,NIOGAS
            IOVEC1(J)=IOVEC2(J)
180         CONTINUE
            IF(NSTACK.GT.0)THEN
                 XPOS2=STACK(NSTACK,1)
                 YPOS2=STACK(NSTACK,2)
                 ZPOS2=STACK(NSTACK,3)
                 ALFA2=STACK(NSTACK,4)
                 DO 190 J=1,NIOGAS
                 IOVEC2(J)=STACK(NSTACK,4+J)
190              CONTINUE
                 NSTACK=NSTACK-1
                 GOTO 20
            ENDIF
       ENDIF
*   Continue with the next segment.
30     CONTINUE
*   Print some debugging output.
       IF(LDEBUG)PRINT '(2I5,4(5X,E15.8))',IU+1,LOCVEC(IU+1),XU(IU+1),
     -      YU(IU+1),ZU(IU+1),EXP(MIN(50.0D0,ALFINT))
10     CONTINUE
*** Make a single precision copy.
       IF(LDEBUG)PRINT *,' ++++++ DLCIO1 DEBUG   : EFIELD calls: ',NFC,
     -      ', deepest stack: ',ISTACK
       DO 200 I=1,NIOGAS
       IORATE(I)=REAL(IONCR(I))
       IF(LDEBUG)PRINT *,' ++++++ DLCIO1 DEBUG   : Final rate ',I,': ',
     -      IORATE(I)
200    CONTINUE
       END
