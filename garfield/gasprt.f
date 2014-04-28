CDECK  ID>, GASPRT.
       SUBROUTINE GASPRT
*-----------------------------------------------------------------------
*   GASPRT - Routine printing an overview of the gas information.
*   VARIABLES : none
*   (Last changed on 31/ 7/08.)
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
       CHARACTER*120 STRING,SYMBOL,UNIT
       CHARACTER*20 STR1,STR2,STR3,STR4,STR5,STR6,STR7,STR8
       INTEGER I,J,K,IG,NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,
     -      IGASOK,IMETH,IEXTR,JEXTR,
     -      NCSYMB,NCUNIT,ITEM,NC,STRLEN
       REAL EXTR1,EXTR2,EXTR3,EXTR4,VAL1,VALN
       EXTERNAL STRLEN
*** Identify the routine, if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE GASPRT ///'
*** Print a suitable heading for the gas tables.
       WRITE(LUNOUT,'(''1 SUMMARY OF THE GAS DATA''/
     -                ''  =======================''/)')
       IF(GASID.NE.' ')WRITE(LUNOUT,'(''  Identification: '',A)')
     -      GASID
*** Check for transport tables.
       IF(.NOT.(GASOK(1).OR.GASOK(2).OR.GASOK(3).OR.
     -      GASOK(4).OR.GASOK(6).OR.GASOK(7).OR.GASOK(8).OR.
     -      GASOK(9).OR.GASOK(10).OR.GASOK(11).OR.GASOK(12)))THEN
            WRITE(LUNOUT,'(''  Transport properties have not been'',
     -           '' entered.'')')
            GOTO 100
       ENDIF
*** 2D tables.
       IF(TAB2D)THEN
**  Electrons: Loop over angles and B fields.
            DO 10 J=1,NBANG
            DO 40 K=1,NBTAB
*   Print the header of part 1 of this combination.
            STRING(1:34)='Electron transport properties for '
            NC=34
            CALL OUTFMT(180*BANG(J)/PI,2,STR1,NC1,'LEFT')
            STRING(NC+1:NC+26+NC1)='angle(E,B) = '//STR1(1:NC1)//
     -           ' degrees and '
            NC=NC+26+NC1
            CALL OUTFMT(BTAB(K)/100,2,STR1,NC1,'LEFT')
            STRING(NC+1:NC+7+NC1)='B = '//STR1(1:NC1)//' T:'
            NC=NC+7+NC1
            WRITE(LUNOUT,'(/2X,A/)') STRING(1:NC)
*   Print the items to be shown in part 1.
            WRITE(LUNOUT,'(''         E        v || E'',
     -           ''   v || Btrans      v || ExB    angle(v,E)'',
     -           ''      Townsend    Attachment'')')
            WRITE(LUNOUT,'(''    [V/cm] [cm/microsec]'',
     -           '' [cm/microsec] [cm/microsec]     [degrees]'',
     -           ''        [1/cm]        [1/cm]''/)')
            DO 20 I=1,NGAS
            CALL OUTFMT(EGAS(I)*PGAS,2,STR1,NC1,'RIGHT')
            IF(GASOK(1))THEN
                 CALL OUTFMT(VGAS2(I,J,K),2,STR2,NC2,'RIGHT')
            ELSE
                 STR2='       Not available'
            ENDIF
            IF(GASOK(9))THEN
                 CALL OUTFMT(XGAS2(I,J,K),2,STR3,NC3,'RIGHT')
            ELSE
                 STR3='       Not available'
            ENDIF
            IF(GASOK(10))THEN
                 CALL OUTFMT(YGAS2(I,J,K),2,STR4,NC4,'RIGHT')
            ELSE
                 STR4='       Not available'
            ENDIF
            IF(GASOK(7))THEN
                 CALL OUTFMT(180*WGAS2(I,J,K)/PI,2,STR5,NC5,'RIGHT')
            ELSE
                 STR5='       Not available'
            ENDIF
            IF(GASOK(4).AND.AGAS2(I,J,K).GT.-20)THEN
                 CALL OUTFMT(EXP(AGAS2(I,J,K))*PGAS,2,STR7,NC7,'RIGHT')
            ELSEIF(GASOK(4))THEN
                 STR7='                   0'
            ELSE
                 STR7='       Not available'
            ENDIF
            IF(GASOK(6).AND.BGAS2(I,J,K).GT.-20)THEN
                 CALL OUTFMT(EXP(BGAS2(I,J,K))*PGAS,2,STR8,NC8,'RIGHT')
            ELSEIF(GASOK(6))THEN
                 STR8='                   0'
            ELSE
                 STR8='       Not available'
            ENDIF
            WRITE(LUNOUT,'(8A)') STR1(11:20),STR2(7:20),STR3(7:20),
     -           STR4(7:20),STR5(7:20),STR7(7:20),STR8(7:20)
20          CONTINUE
*   Print the items to be shown in part 2.
            WRITE(LUNOUT,'(/''         E'',
     -           ''    sigma || E  sigma || Btr  sigma || ExB'',
     -           ''    rho(E,Btr)    rho(E,ExB)  rho(Btr,ExB)'')')
            WRITE(LUNOUT,'(''    [V/cm]'',
     -           ''                         [micron for 1 cm]'',
     -           ''                                       [-]''/)')
            DO 60 I=1,NGAS
            CALL OUTFMT(EGAS(I)*PGAS,2,STR1,NC1,'RIGHT')
            IF(GASOK(11))THEN
                 CALL OUTFMT(10000*SQRT(SGAS2(I,J,K,1)/PGAS),2,
     -                STR2,NC2,'RIGHT')
                 CALL OUTFMT(10000*SQRT(SGAS2(I,J,K,2)/PGAS),2,
     -                STR3,NC3,'RIGHT')
                 CALL OUTFMT(10000*SQRT(SGAS2(I,J,K,3)/PGAS),2,
     -                STR4,NC4,'RIGHT')
                 IF(SGAS2(I,J,K,1)*SGAS2(I,J,K,2).GT.0)THEN
                      CALL OUTFMT(SGAS2(I,J,K,4)/SQRT(SGAS2(I,J,K,1)*
     -                     SGAS2(I,J,K,2)),2,STR5,NC5,'RIGHT')
                 ELSE
                      STR5='         Not defined'
                 ENDIF
                 IF(SGAS2(I,J,K,1)*SGAS2(I,J,K,3).GT.0)THEN
                      CALL OUTFMT(SGAS2(I,J,K,5)/SQRT(SGAS2(I,J,K,1)*
     -                     SGAS2(I,J,K,3)),2,STR6,NC6,'RIGHT')
                 ELSE
                      STR6='         Not defined'
                 ENDIF
                 IF(SGAS2(I,J,K,2)*SGAS2(I,J,K,3).GT.0)THEN
                      CALL OUTFMT(SGAS2(I,J,K,6)/SQRT(SGAS2(I,J,K,2)*
     -                     SGAS2(I,J,K,3)),2,STR7,NC7,'RIGHT')
                 ELSE
                      STR7='         Not defined'
                 ENDIF
            ELSE
                 IF(GASOK(3))THEN
                      CALL OUTFMT(10000*DGAS2(I,J,K)/SQRT(PGAS),2,
     -                     STR2,NC2,'RIGHT')
                 ELSE
                      STR2='       Not available'
                 ENDIF
                 IF(GASOK(8))THEN
                      CALL OUTFMT(10000*OGAS2(I,J,K)/SQRT(PGAS),2,
     -                     STR3,NC3,'RIGHT')
                 ELSE
                      STR3='       Not available'
                 ENDIF
                 STR4='       Not available'
                 STR5='       Not available'
                 STR6='       Not available'
                 STR7='       Not available'
            ENDIF
            WRITE(LUNOUT,'(8A)') STR1(11:20),STR2(7:20),STR3(7:20),
     -           STR4(7:20),STR5(7:20),STR6(7:20),STR7(7:20)
60          CONTINUE
40          CONTINUE
10          CONTINUE
**  Electrons: excitation and ionisation rates, loop over angles + B
            DO 310 J=1,NBANG
            DO 320 K=1,NBTAB
            IF(GASOK(15).AND.NEXGAS.GE.1)THEN
*   Loop over the blocks.
                 DO 330 IG=0,NEXGAS-1,6
*   Format a header.
                 STRING(1:21)='Excitation rates for '
                 NC=21
                 CALL OUTFMT(180*BANG(J)/PI,2,STR1,NC1,'LEFT')
                 STRING(NC+1:NC+26+NC1)='angle(E,B) = '//STR1(1:NC1)//
     -                ' degrees and '
                 NC=NC+26+NC1
                 CALL OUTFMT(BTAB(K)/100,2,STR1,NC1,'LEFT')
                 STRING(NC+1:NC+7+NC1)='B = '//STR1(1:NC1)//' T:'
                 NC=NC+7+NC1
                 WRITE(LUNOUT,'(/2X,A/)') STRING(1:NC)
*   Print a header.
                 STRING(1:10)='         E'
                 NC=10
                 DO 340 I=1,6
                 IF(IG+I.GT.NEXGAS)GOTO 340
                 WRITE(STR2,'(''      Rate '',I3)') IG+I
                 STRING(NC+1:NC+14)=STR2(1:14)
                 NC=NC+14
340              CONTINUE
                 WRITE(LUNOUT,'(A)') STRING(1:NC)
                 STRING(1:10)='    [V/cm]'
                 NC=10
                 DO 350 I=1,6
                 IF(IG+I.GT.NEXGAS)GOTO 350
                 STRING(NC+1:NC+14)='         [THz]'
                 NC=NC+14
350              CONTINUE
                 WRITE(LUNOUT,'(A/)') STRING(1:NC)
*   Loop over the gas lines
                 DO 360 I=1,NGAS
                 CALL OUTFMT(EGAS(I)*PGAS,2,STR1,NC1,'RIGHT')
                 IF(NEXGAS.GE.IG+1)THEN
                      CALL OUTFMT(EXGAS2(I,J,K,IG+1)*PGAS,2,STR2,NC2,
     -                     'RIGHT')
                 ELSE
                      STR2=' '
                 ENDIF
                 IF(NEXGAS.GE.IG+2)THEN
                      CALL OUTFMT(EXGAS2(I,J,K,IG+2)*PGAS,2,STR3,NC3,
     -                     'RIGHT')
                 ELSE
                      STR3=' '
                 ENDIF
                 IF(NEXGAS.GE.IG+3)THEN
                      CALL OUTFMT(EXGAS2(I,J,K,IG+3)*PGAS,2,STR4,NC4,
     -                     'RIGHT')
                 ELSE
                      STR4=' '
                 ENDIF
                 IF(NEXGAS.GE.IG+4)THEN
                      CALL OUTFMT(EXGAS2(I,J,K,IG+4)*PGAS,2,STR5,NC5,
     -                     'RIGHT')
                 ELSE
                      STR5=' '
                 ENDIF
                 IF(NEXGAS.GE.IG+5)THEN
                      CALL OUTFMT(EXGAS2(I,J,K,IG+5)*PGAS,2,STR6,NC6,
     -                     'RIGHT')
                 ELSE
                      STR6=' '
                 ENDIF
                 IF(NEXGAS.GE.IG+6)THEN
                      CALL OUTFMT(EXGAS2(I,J,K,IG+6)*PGAS,2,STR7,NC7,
     -                     'RIGHT')
                 ELSE
                      STR7=' '
                 ENDIF
                 WRITE(LUNOUT,'(8A)') STR1(11:20),STR2(7:20),
     -                STR3(7:20),STR4(7:20),STR5(7:20),STR6(7:20),
     -                STR7(7:20)
360              CONTINUE
*   Explain what these rates are
                 WRITE(LUNOUT,'('' '')')
                 DO 370 I=IG+1,MIN(IG+6,NEXGAS)
                 CALL OUTFMT(ENEXG(I),2,STR2,NC2,'LEFT')
                 WRITE(LUNOUT,'(''  Rate '',I3,'': '',A,
     -                '' (threshold: '',A,'' eV)'')')
     -                I,DSCEXG(I)(1:STRLEN(DSCEXG(I))),STR2(1:NC2)
370              CONTINUE
                 WRITE(LUNOUT,'('' '')')
330              CONTINUE
            ENDIF
            IF(GASOK(16).AND.NIOGAS.GE.1)THEN
*   Loop over the blocks.
                 DO 380 IG=0,NIOGAS-1,6
*   Format a header.
                 STRING(1:21)='Ionisation rates for '
                 NC=21
                 CALL OUTFMT(180*BANG(J)/PI,2,STR1,NC1,'LEFT')
                 STRING(NC+1:NC+26+NC1)='angle(E,B) = '//STR1(1:NC1)//
     -                ' degrees and '
                 NC=NC+26+NC1
                 CALL OUTFMT(BTAB(K)/100,2,STR1,NC1,'LEFT')
                 STRING(NC+1:NC+7+NC1)='B = '//STR1(1:NC1)//' T:'
                 NC=NC+7+NC1
                 WRITE(LUNOUT,'(/2X,A/)') STRING(1:NC)
*   Print a header.
                 STRING(1:10)='         E'
                 NC=10
                 DO 390 I=1,6
                 IF(IG+I.GT.NIOGAS)GOTO 390
                 WRITE(STR2,'(''      Rate '',I3)') IG+I
                 STRING(NC+1:NC+14)=STR2(1:14)
                 NC=NC+14
390              CONTINUE
                 WRITE(LUNOUT,'(A)') STRING(1:NC)
                 STRING(1:10)='    [V/cm]'
                 NC=10
                 DO 400 I=1,6
                 IF(IG+I.GT.NIOGAS)GOTO 400
                 STRING(NC+1:NC+14)='         [THz]'
                 NC=NC+14
400              CONTINUE
                 WRITE(LUNOUT,'(A/)') STRING(1:NC)
*   Loop over the gas lines
                 DO 410 I=1,NGAS
                 CALL OUTFMT(EGAS(I)*PGAS,2,STR1,NC1,'RIGHT')
                 IF(NIOGAS.GE.IG+1)THEN
                      CALL OUTFMT(IOGAS2(I,J,K,IG+1)*PGAS,2,STR2,NC2,
     -                     'RIGHT')
                 ELSE
                      STR2=' '
                 ENDIF
                 IF(NIOGAS.GE.IG+2)THEN
                      CALL OUTFMT(IOGAS2(I,J,K,IG+2)*PGAS,2,STR3,NC3,
     -                     'RIGHT')
                 ELSE
                      STR3=' '
                 ENDIF
                 IF(NIOGAS.GE.IG+3)THEN
                      CALL OUTFMT(IOGAS2(I,J,K,IG+3)*PGAS,2,STR4,NC4,
     -                     'RIGHT')
                 ELSE
                      STR4=' '
                 ENDIF
                 IF(NIOGAS.GE.IG+4)THEN
                      CALL OUTFMT(IOGAS2(I,J,K,IG+4)*PGAS,2,STR5,NC5,
     -                     'RIGHT')
                 ELSE
                      STR5=' '
                 ENDIF
                 IF(NIOGAS.GE.IG+5)THEN
                      CALL OUTFMT(IOGAS2(I,J,K,IG+5)*PGAS,2,STR6,NC6,
     -                     'RIGHT')
                 ELSE
                      STR6=' '
                 ENDIF
                 IF(NIOGAS.GE.IG+6)THEN
                      CALL OUTFMT(IOGAS2(I,J,K,IG+6)*PGAS,2,STR7,NC7,
     -                     'RIGHT')
                 ELSE
                      STR7=' '
                 ENDIF
                 WRITE(LUNOUT,'(8A)') STR1(11:20),STR2(7:20),
     -                STR3(7:20),STR4(7:20),STR5(7:20),STR6(7:20),
     -                STR7(7:20)
410              CONTINUE
*   Explain what these rates are.
                 WRITE(LUNOUT,'('' '')')
                 DO 420 I=IG+1,MIN(IG+6,NIOGAS)
                 CALL OUTFMT(ENIOG(I),2,STR2,NC2,'LEFT')
                 WRITE(LUNOUT,'(''  Rate '',I3,'': '',A,
     -                '' (threshold: '',A,'' eV)'')')
     -                I,DSCIOG(I)(1:STRLEN(DSCIOG(I))),STR2(1:NC2)
420              CONTINUE
                 WRITE(LUNOUT,'('' '')')
380              CONTINUE
            ENDIF
320         CONTINUE
310         CONTINUE
**  Ions: Loop over angles and B fields.
            DO 210 J=1,NBANG
            DO 240 K=1,NBTAB
*   Print the header for this combination.
            STRING(1:29)='Ion transport properties for '
            NC=29
            CALL OUTFMT(180*BANG(J)/PI,2,STR1,NC1,'LEFT')
            STRING(NC+1:NC+26+NC1)='angle(E,B) = '//STR1(1:NC1)//
     -           ' degrees and '
            NC=NC+26+NC1
            CALL OUTFMT(BTAB(K)/100,2,STR1,NC1,'LEFT')
            STRING(NC+1:NC+7+NC1)='B = '//STR1(1:NC1)//' T:'
            NC=NC+7+NC1
            WRITE(LUNOUT,'(/2X,A/)') STRING(1:NC)
*   Print the items to be shown.
            WRITE(LUNOUT,'(''         E      Mobility'',
     -           ''  Dissociation'')')
            WRITE(LUNOUT,'(''    [V/cm] [cm2/V.musec]'',
     -           ''        [1/cm]''/)')
            DO 220 I=1,NGAS
            CALL OUTFMT(EGAS(I)*PGAS,2,STR1,NC1,'RIGHT')
            IF(GASOK(2))THEN
                 CALL OUTFMT(MGAS2(I,J,K),2,STR2,NC2,'RIGHT')
            ELSE
                 STR2='       Not available'
            ENDIF
            IF(GASOK(12).AND.HGAS2(I,J,K).GT.-20)THEN
                 CALL OUTFMT(EXP(HGAS2(I,J,K))*PGAS,2,STR3,NC3,'RIGHT')
            ELSEIF(GASOK(12))THEN
                 STR3='                   0'
            ELSE
                 STR3='       Not available'
            ENDIF
            WRITE(LUNOUT,'(8A)') STR1(11:20),STR2(7:20),STR3(7:20)
220         CONTINUE
240         CONTINUE
210         CONTINUE
*** 1D tables.
       ELSE
*   Electrons: print the header for this combination.
            STRING(1:34)='Electron transport properties for '
            NC=34
            CALL OUTFMT(180*BANG(1)/PI,2,STR1,NC1,'LEFT')
            STRING(NC+1:NC+26+NC1)='angle(E,B) = '//STR1(1:NC1)//
     -           ' degrees and '
            NC=NC+26+NC1
            CALL OUTFMT(BTAB(1)/100,2,STR1,NC1,'LEFT')
            STRING(NC+1:NC+7+NC1)='B = '//STR1(1:NC1)//' T:'
            NC=NC+7+NC1
            WRITE(LUNOUT,'(/2X,A/)') STRING(1:NC)
*   Print the table.
            WRITE(LUNOUT,'(''         E        Vdrift'',
     -           ''    Diffusion  (long, trans)'',
     -           ''      Townsend    Attachment Lorentz angle'')')
            WRITE(LUNOUT,'(''    [V/cm] [cm/microsec]'',
     -           ''           [micron for 1 cm]'',
     -           ''        [1/cm]        [1/cm]     [degrees]''/)')
*   Loop over the gas
            DO 30 I=1,NGAS
            CALL OUTFMT(EGAS(I)*PGAS,2,STR1,NC1,'RIGHT')
            IF(GASOK(1))THEN
                 CALL OUTFMT(VGAS(I),2,STR2,NC2,'RIGHT')
            ELSE
                 STR2='       Not available'
            ENDIF
            IF(GASOK(3))THEN
                 CALL OUTFMT(10000*DGAS(I)/SQRT(PGAS),2,
     -                STR4,NC4,'RIGHT')
            ELSE
                 STR4='       Not available'
            ENDIF
            IF(GASOK(8))THEN
                 CALL OUTFMT(10000*OGAS(I)/SQRT(PGAS),2,
     -                STR5,NC5,'RIGHT')
            ELSE
                 STR5='       Not available'
            ENDIF
            IF(GASOK(4).AND.AGAS(I).GT.-20)THEN
                 CALL OUTFMT(EXP(AGAS(I))*PGAS,2,STR6,NC6,'RIGHT')
            ELSEIF(GASOK(4))THEN
                 STR6='                   0'
            ELSE
                 STR6='       Not available'
            ENDIF
            IF(GASOK(6).AND.BGAS(I).GT.-20)THEN
                 CALL OUTFMT(EXP(BGAS(I))*PGAS,2,STR7,NC7,'RIGHT')
            ELSEIF(GASOK(6))THEN
                 STR7='                   0'
            ELSE
                 STR7='       Not available'
            ENDIF
            IF(GASOK(7))THEN
                 CALL OUTFMT(180*WGAS(I)/PI,2,STR8,NC8,'RIGHT')
            ELSE
                 STR8='       Not available'
            ENDIF
            WRITE(LUNOUT,'(8A)') STR1(11:20),STR2(7:20),
     -           STR4(7:20),STR5(7:20),STR6(7:20),STR7(7:20),STR8(7:20)
30          CONTINUE
**  Electrons: excitation and ionisation rates.
            IF(GASOK(15).AND.NEXGAS.GE.1)THEN
*   Loop over the blocks.
                 DO 90 IG=0,NEXGAS-1,6
*   Format a header.
                 STRING(1:21)='Excitation rates for '
                 NC=21
                 CALL OUTFMT(180*BANG(1)/PI,2,STR1,NC1,'LEFT')
                 STRING(NC+1:NC+26+NC1)='angle(E,B) = '//STR1(1:NC1)//
     -                ' degrees and '
                 NC=NC+26+NC1
                 CALL OUTFMT(BTAB(1)/100,2,STR1,NC1,'LEFT')
                 STRING(NC+1:NC+7+NC1)='B = '//STR1(1:NC1)//' T:'
                 NC=NC+7+NC1
                 WRITE(LUNOUT,'(/2X,A/)') STRING(1:NC)
*   Print a header.
                 STRING(1:10)='         E'
                 NC=10
                 DO 91 I=1,6
                 IF(IG+I.GT.NEXGAS)GOTO 91
                 WRITE(STR2,'(''      Rate '',I3)') IG+I
                 STRING(NC+1:NC+14)=STR2(1:14)
                 NC=NC+14
91               CONTINUE
                 WRITE(LUNOUT,'(A)') STRING(1:NC)
                 STRING(1:10)='    [V/cm]'
                 NC=10
                 DO 92 I=1,6
                 IF(IG+I.GT.NEXGAS)GOTO 92
                 STRING(NC+1:NC+14)='         [THz]'
                 NC=NC+14
92               CONTINUE
                 WRITE(LUNOUT,'(A/)') STRING(1:NC)
*   Loop over the gas lines
                 DO 70 I=1,NGAS
                 CALL OUTFMT(EGAS(I)*PGAS,2,STR1,NC1,'RIGHT')
                 IF(NEXGAS.GE.IG+1)THEN
                      CALL OUTFMT(EXGAS(I,IG+1)*PGAS,2,STR2,NC2,'RIGHT')
                 ELSE
                      STR2=' '
                 ENDIF
                 IF(NEXGAS.GE.IG+2)THEN
                      CALL OUTFMT(EXGAS(I,IG+2)*PGAS,2,STR3,NC3,'RIGHT')
                 ELSE
                      STR3=' '
                 ENDIF
                 IF(NEXGAS.GE.IG+3)THEN
                      CALL OUTFMT(EXGAS(I,IG+3)*PGAS,2,STR4,NC4,'RIGHT')
                 ELSE
                      STR4=' '
                 ENDIF
                 IF(NEXGAS.GE.IG+4)THEN
                      CALL OUTFMT(EXGAS(I,IG+4)*PGAS,2,STR5,NC5,'RIGHT')
                 ELSE
                      STR5=' '
                 ENDIF
                 IF(NEXGAS.GE.IG+5)THEN
                      CALL OUTFMT(EXGAS(I,IG+5)*PGAS,2,STR6,NC6,'RIGHT')
                 ELSE
                      STR6=' '
                 ENDIF
                 IF(NEXGAS.GE.IG+6)THEN
                      CALL OUTFMT(EXGAS(I,IG+6)*PGAS,2,STR7,NC7,'RIGHT')
                 ELSE
                      STR7=' '
                 ENDIF
                 WRITE(LUNOUT,'(8A)') STR1(11:20),STR2(7:20),
     -                STR3(7:20),STR4(7:20),STR5(7:20),STR6(7:20),
     -                STR7(7:20)
70               CONTINUE
*   Explain what these rates are
                 WRITE(LUNOUT,'('' '')')
                 DO 80 I=IG+1,MIN(IG+6,NEXGAS)
                 CALL OUTFMT(ENEXG(I),2,STR2,NC2,'LEFT')
                 WRITE(LUNOUT,'(''  Rate '',I3,'': '',A,
     -                '' (threshold: '',A,'' eV)'')')
     -                I,DSCEXG(I)(1:STRLEN(DSCEXG(I))),STR2(1:NC2)
80               CONTINUE
                 WRITE(LUNOUT,'('' '')')
90               CONTINUE
            ENDIF
            IF(GASOK(16).AND.NIOGAS.GE.1)THEN
*   Loop over the blocks.
                 DO 93 IG=0,NIOGAS-1,6
*   Format a header.
                 STRING(1:21)='Ionisation rates for '
                 NC=21
                 CALL OUTFMT(180*BANG(1)/PI,2,STR1,NC1,'LEFT')
                 STRING(NC+1:NC+26+NC1)='angle(E,B) = '//STR1(1:NC1)//
     -                ' degrees and '
                 NC=NC+26+NC1
                 CALL OUTFMT(BTAB(1)/100,2,STR1,NC1,'LEFT')
                 STRING(NC+1:NC+7+NC1)='B = '//STR1(1:NC1)//' T:'
                 NC=NC+7+NC1
                 WRITE(LUNOUT,'(/2X,A/)') STRING(1:NC)
*   Print a header.
                 STRING(1:10)='         E'
                 NC=10
                 DO 94 I=1,6
                 IF(IG+I.GT.NIOGAS)GOTO 94
                 WRITE(STR2,'(''      Rate '',I3)') IG+I
                 STRING(NC+1:NC+14)=STR2(1:14)
                 NC=NC+14
94               CONTINUE
                 WRITE(LUNOUT,'(A)') STRING(1:NC)
                 STRING(1:10)='    [V/cm]'
                 NC=10
                 DO 95 I=1,6
                 IF(IG+I.GT.NIOGAS)GOTO 95
                 STRING(NC+1:NC+14)='         [THz]'
                 NC=NC+14
95               CONTINUE
                 WRITE(LUNOUT,'(A/)') STRING(1:NC)
*   Loop over the gas lines
                 DO 96 I=1,NGAS
                 CALL OUTFMT(EGAS(I)*PGAS,2,STR1,NC1,'RIGHT')
                 IF(NIOGAS.GE.IG+1)THEN
                      CALL OUTFMT(IOGAS(I,IG+1)*PGAS,2,STR2,NC2,'RIGHT')
                 ELSE
                      STR2=' '
                 ENDIF
                 IF(NIOGAS.GE.IG+2)THEN
                      CALL OUTFMT(IOGAS(I,IG+2)*PGAS,2,STR3,NC3,'RIGHT')
                 ELSE
                      STR3=' '
                 ENDIF
                 IF(NIOGAS.GE.IG+3)THEN
                      CALL OUTFMT(IOGAS(I,IG+3)*PGAS,2,STR4,NC4,'RIGHT')
                 ELSE
                      STR4=' '
                 ENDIF
                 IF(NIOGAS.GE.IG+4)THEN
                      CALL OUTFMT(IOGAS(I,IG+4)*PGAS,2,STR5,NC5,'RIGHT')
                 ELSE
                      STR5=' '
                 ENDIF
                 IF(NIOGAS.GE.IG+5)THEN
                      CALL OUTFMT(IOGAS(I,IG+5)*PGAS,2,STR6,NC6,'RIGHT')
                 ELSE
                      STR6=' '
                 ENDIF
                 IF(NIOGAS.GE.IG+6)THEN
                      CALL OUTFMT(IOGAS(I,IG+6)*PGAS,2,STR7,NC7,'RIGHT')
                 ELSE
                      STR7=' '
                 ENDIF
                 WRITE(LUNOUT,'(8A)') STR1(11:20),STR2(7:20),
     -                STR3(7:20),STR4(7:20),STR5(7:20),STR6(7:20),
     -                STR7(7:20)
96               CONTINUE
*   Explain what these rates are.
                 WRITE(LUNOUT,'('' '')')
                 DO 97 I=IG+1,MIN(IG+6,NIOGAS)
                 CALL OUTFMT(ENIOG(I),2,STR2,NC2,'LEFT')
                 WRITE(LUNOUT,'(''  Rate '',I3,'': '',A,
     -                '' (threshold: '',A,'' eV)'')')
     -                I,DSCIOG(I)(1:STRLEN(DSCIOG(I))),STR2(1:NC2)
97               CONTINUE
                 WRITE(LUNOUT,'('' '')')
93               CONTINUE
            ENDIF
**  Ions: Print the header for this combination.
            STRING(1:29)='Ion transport properties for '
            NC=29
            CALL OUTFMT(180*BANG(1)/PI,2,STR1,NC1,'LEFT')
            STRING(NC+1:NC+26+NC1)='angle(E,B) = '//STR1(1:NC1)//
     -           ' degrees and '
            NC=NC+26+NC1
            CALL OUTFMT(BTAB(1)/100,2,STR1,NC1,'LEFT')
            STRING(NC+1:NC+7+NC1)='B = '//STR1(1:NC1)//' T:'
            NC=NC+7+NC1
            WRITE(LUNOUT,'(/2X,A/)') STRING(1:NC)
*   Print the items to be shown.
            WRITE(LUNOUT,'(''         E      Mobility'',
     -           ''  Dissociation'')')
            WRITE(LUNOUT,'(''    [V/cm] [cm2/V.musec]'',
     -           ''        [1/cm]''/)')
            DO 230 I=1,NGAS
            CALL OUTFMT(EGAS(I)*PGAS,2,STR1,NC1,'RIGHT')
            IF(GASOK(2))THEN
                 CALL OUTFMT(MGAS(I),2,STR2,NC2,'RIGHT')
            ELSE
                 STR2='       Not available'
            ENDIF
            IF(GASOK(12).AND.HGAS(I).GT.-20)THEN
                 CALL OUTFMT(EXP(HGAS(I))*PGAS,2,STR3,NC3,'RIGHT')
            ELSEIF(GASOK(12))THEN
                 STR3='                   0'
            ELSE
                 STR3='       Not available'
            ENDIF
            WRITE(LUNOUT,'(8A)') STR1(11:20),STR2(7:20),STR3(7:20)
230         CONTINUE
       ENDIF
*** Print the extrapolation formulae.
       WRITE(LUNOUT,'(/''  Interpolations and extrapolations:''/)')
*   Loop over the items.
       DO 50 ITEM=1,18
*   Initial values, invalid in as far as possible.
       IGASOK=0
       IMETH=0
       IEXTR=-1
       JEXTR=-1
       SYMBOL='?'
       NCSYMB=1
       UNIT='?'
       NCUNIT=1
       EXTR1=0.0
       EXTR2=0.0
       EXTR3=0.0
       EXTR4=0.0
       VAL1=0.0
       VALN=0.0
*   Print a header.
       IF(ITEM.EQ.1)THEN
            WRITE(LUNOUT,'(''  Drift velocity along E:'')')
            IGASOK=1
            IMETH=IVMETH
            IEXTR=IVEXTR
            JEXTR=JVEXTR
            EXTR1=VEXTR1
            EXTR2=VEXTR2
            EXTR3=VEXTR3
            EXTR4=VEXTR4
            VAL1=VGAS(1)
            VALN=VGAS(NGAS)
            SYMBOL='v'
            NCSYMB=1
            UNIT='[cm/microsec]'
            NCUNIT=13
       ELSEIF(ITEM.EQ.2)THEN
            WRITE(LUNOUT,'(/''  Drift velocity along Btrans:'')')
            IGASOK=9
            IMETH=IXMETH
            IEXTR=IXEXTR
            JEXTR=JXEXTR
            EXTR1=XEXTR1
            EXTR2=XEXTR2
            EXTR3=XEXTR3
            EXTR4=XEXTR4
            VAL1=XGAS(1)
            VALN=XGAS(NGAS)
            SYMBOL='v'
            NCSYMB=1
            UNIT='[cm/microsec]'
            NCUNIT=13
       ELSEIF(ITEM.EQ.3)THEN
            WRITE(LUNOUT,'(/''  Drift velocity along ExB:'')')
            IGASOK=10
            IMETH=IYMETH
            IEXTR=IYEXTR
            JEXTR=JYEXTR
            EXTR1=YEXTR1
            EXTR2=YEXTR2
            EXTR3=YEXTR3
            EXTR4=YEXTR4
            VAL1=YGAS(1)
            VALN=YGAS(NGAS)
            SYMBOL='v'
            NCSYMB=1
            UNIT='[cm/microsec]'
            NCUNIT=13
       ELSEIF(ITEM.EQ.4)THEN
            WRITE(LUNOUT,'(/''  Angle between v and E:'')')
            IGASOK=7
            IMETH=IWMETH
            IEXTR=IWEXTR
            JEXTR=JWEXTR
            EXTR1=WEXTR1
            EXTR2=WEXTR2
            EXTR3=WEXTR3
            EXTR4=WEXTR4
            VAL1=WGAS(1)
            VALN=WGAS(NGAS)
            SYMBOL='angle(v,E)'
            NCSYMB=10
            UNIT='[radian]'
            NCUNIT=13
       ELSEIF(ITEM.EQ.5)THEN
            WRITE(LUNOUT,'(/''  Ion mobility:'')')
            IGASOK=2
            IMETH=IMMETH
            IEXTR=IMEXTR
            JEXTR=JMEXTR
            EXTR1=MEXTR1
            EXTR2=MEXTR2
            EXTR3=MEXTR3
            EXTR4=MEXTR4
            VAL1=MGAS(1)
            VALN=MGAS(NGAS)
            SYMBOL='mu ion'
            NCSYMB=6
            UNIT='[cm^2/(microsec.V)]'
            NCUNIT=19
       ELSEIF(ITEM.EQ.6)THEN
            WRITE(LUNOUT,'(/''  Longitudinal diffusion:'')')
            IGASOK=3
            IMETH=IDMETH
            IEXTR=IDEXTR
            JEXTR=JDEXTR
            EXTR1=DEXTR1
            EXTR2=DEXTR2
            EXTR3=DEXTR3
            EXTR4=DEXTR4
            VAL1=DGAS(1)
            VALN=DGAS(NGAS)
            SYMBOL='sigma_L.sqrt(p)'
            NCSYMB=15
            UNIT='[cm.sqrt(Torr) for 1 cm]'
            NCUNIT=24
       ELSEIF(ITEM.EQ.7)THEN
            WRITE(LUNOUT,'(/''  Transverse diffusion:'')')
            IGASOK=8
            IMETH=IOMETH
            IEXTR=IOEXTR
            JEXTR=JOEXTR
            EXTR1=OEXTR1
            EXTR2=OEXTR2
            EXTR3=OEXTR3
            EXTR4=OEXTR4
            VAL1=OGAS(1)
            VALN=OGAS(NGAS)
            SYMBOL='sigma_T.sqrt(p)'
            NCSYMB=15
            UNIT='[cm.sqrt(Torr) for 1 cm]'
            NCUNIT=24
       ELSEIF(ITEM.EQ.8)THEN
            WRITE(LUNOUT,'(/''  Townsend coefficient:'')')
            IGASOK=4
            IMETH=IAMETH
            IEXTR=IAEXTR
            JEXTR=JAEXTR
            EXTR1=AEXTR1
            EXTR2=AEXTR2
            EXTR3=AEXTR3
            EXTR4=AEXTR4
            VAL1=AGAS(1)
            VALN=AGAS(NGAS)
            SYMBOL='log(alpha/p)'
            NCSYMB=12
            UNIT='[-log(cm.Torr)]'
            NCUNIT=15
       ELSEIF(ITEM.EQ.9)THEN
            WRITE(LUNOUT,'(/''  Attachment coefficient:'')')
            IGASOK=6
            IMETH=IBMETH
            IEXTR=IBEXTR
            JEXTR=JBEXTR
            EXTR1=BEXTR1
            EXTR2=BEXTR2
            EXTR3=BEXTR3
            EXTR4=BEXTR4
            VAL1=BGAS(1)
            VALN=BGAS(NGAS)
            SYMBOL='log(eta/p)'
            NCSYMB=10
            UNIT='[-log(cm.Torr)]'
            NCUNIT=15
       ELSEIF(ITEM.EQ.16)THEN
            WRITE(LUNOUT,'(/''  Ion dissociation coefficient:'')')
            IGASOK=12
            IMETH=IHMETH
            IEXTR=IHEXTR
            JEXTR=JHEXTR
            EXTR1=HEXTR1
            EXTR2=HEXTR2
            EXTR3=HEXTR3
            EXTR4=HEXTR4
            VAL1=HGAS(1)
            VALN=HGAS(NGAS)
            SYMBOL='log(diss/p)'
            NCSYMB=11
            UNIT='[-log(cm.Torr)]'
            NCUNIT=15
       ELSEIF(ITEM.GE.10.AND.ITEM.LE.15)THEN
            WRITE(LUNOUT,'(/''  Diffusion tensor:'')')
            IGASOK=11
            IMETH=ISMETH
            IEXTR=ISEXTR
            JEXTR=JSEXTR
            EXTR1=SEXTR1(ITEM-9)
            EXTR2=SEXTR2(ITEM-9)
            EXTR3=SEXTR3(ITEM-9)
            EXTR4=SEXTR4(ITEM-9)
            VAL1=SGAS(1,ITEM-9)
            VALN=SGAS(NGAS,ITEM-9)
            IF(ITEM.EQ.10)THEN
                 SYMBOL='cov(E,E)'
                 NCSYMB=8
            ELSEIF(ITEM.EQ.11)THEN
                 SYMBOL='cov(Btr,Btr)'
                 NCSYMB=12
            ELSEIF(ITEM.EQ.12)THEN
                 SYMBOL='cov(ExB,ExB)'
                 NCSYMB=12
            ELSEIF(ITEM.EQ.13)THEN
                 SYMBOL='cov(E,Btr)'
                 NCSYMB=10
            ELSEIF(ITEM.EQ.14)THEN
                 SYMBOL='cov(E,ExB)'
                 NCSYMB=10
            ELSEIF(ITEM.EQ.15)THEN
                 SYMBOL='cov(Btr,ExB)'
                 NCSYMB=12
            ENDIF
            UNIT='[cm.Torr]'
            NCUNIT=9
       ELSEIF(ITEM.EQ.17)THEN
            WRITE(LUNOUT,'(/''  Excitation rates:'')')
            IGASOK=15
            IMETH=IEMETH
            IEXTR=IEEXTR
            JEXTR=JEEXTR
            EXTR1=0.0
            EXTR2=0.0
            EXTR3=0.0
            EXTR4=0.0
            VAL1=0.0
            VALN=0.0
            SYMBOL='excitation rate'
            NCSYMB=15
            UNIT='[THz]'
            NCUNIT=5
       ELSEIF(ITEM.EQ.18)THEN
            WRITE(LUNOUT,'(/''  Ionisation rates:'')')
            IGASOK=16
            IMETH=IZMETH
            IEXTR=IZEXTR
            JEXTR=JZEXTR
            EXTR1=0.0
            EXTR2=0.0
            EXTR3=0.0
            EXTR4=0.0
            VAL1=0.0
            VALN=0.0
            SYMBOL='ionisation rate'
            NCSYMB=15
            UNIT='[THz]'
            NCUNIT=5
       ENDIF
**  Quickly done if there is no such data.
       IF(IGASOK.LE.0)THEN
            WRITE(LUNOUT,'(''       # Incorrect data..'')')
       ELSEIF(.NOT.GASOK(IGASOK))THEN
            WRITE(LUNOUT,'(''       Not applicable.'')')
**  Data on a (E,angle,B) grid.
       ELSEIF(TAB2D)THEN
*   Interpolation method.
            IF(IMETH.EQ.1)THEN
                 WRITE(LUNOUT,'(7X,''Linear interpolation for:'')')
            ELSEIF(IMETH.EQ.2)THEN
                 WRITE(LUNOUT,'(7X,''Quadratic interpolation for:'')')
            ELSEIF(IMETH.EQ.3)THEN
                 WRITE(LUNOUT,'(7X,''Cubic interpolation for:'')')
            ELSE
                 WRITE(LUNOUT,'(7X,''# Inapplicable'',
     -                '' interpolation method for:'')')
            ENDIF
*   Range of applicability.
            CALL OUTFMT(EGAS(1)*PGAS,2,STR1,NC1,'LEFT')
            CALL OUTFMT(EGAS(NGAS)*PGAS,2,STR2,NC2,'LEFT')
            WRITE(LUNOUT,'(11X,A,'' < E < '',A,'' V/cm,'')')
     -           STR1(1:NC1),STR2(1:NC2)
            IF(NBANG.EQ.1)THEN
                 WRITE(LUNOUT,'(11X,''all angles between E and B,'')')
            ELSE
                 CALL OUTFMT(180*BANG(1)/PI,2,STR1,NC1,'LEFT')
                 CALL OUTFMT(180*BANG(NBANG)/PI,2,STR2,NC2,
     -                'LEFT')
                 WRITE(LUNOUT,'(11X,A,'' < angle(E,B) < '',
     -                A,'' degrees,'')') STR1(1:NC1),STR2(1:NC2)
            ENDIF
            IF(NBANG.EQ.1)THEN
                 WRITE(LUNOUT,'(11X,''all magnetic field strengths.'')')
            ELSE
                 CALL OUTFMT(BTAB(1)/100,2,STR1,NC1,'LEFT')
                 CALL OUTFMT(BTAB(NBTAB)/100,2,STR2,NC2,'LEFT')
                 WRITE(LUNOUT,'(11X,A,'' < B < '',A,'' T.'')')
     -                STR1(1:NC1),STR2(1:NC2)
            ENDIF
*   Special case of alpha and eta.
            IF((ITEM.EQ.8.AND.IATHR.GT.1).OR.
     -           (ITEM.EQ.9.AND.IBTHR.GT.1))THEN
                 CALL OUTFMT(EGAS(1)*PGAS,2,STR1,NC1,'LEFT')
                 IF(ITEM.EQ.8)THEN
                      CALL OUTFMT(EGAS(IATHR)*PGAS,2,STR2,NC2,'LEFT')
                 ELSE
                      CALL OUTFMT(EGAS(IBTHR)*PGAS,2,STR2,NC2,'LEFT')
                 ENDIF
                 WRITE(LUNOUT,'(7X,''For numeric stability, linear'',
     -                '' interpolation is used in the subrange:''/
     -                11X,A,'' < E < '',A,'' V/cm,'')')
     -                STR1(1:NC1),STR2(1:NC2)
            ENDIF
*   Extrapolation method.
            WRITE(LUNOUT,'(7X,''Constant extrapolation for:''/
     -           11X,''values outside this range.'')')
**  Data only having E dependence.
       ELSE
*   Range limits.
            CALL OUTFMT(EGAS(1)*PGAS,2,STR1,NC1,'LEFT')
            CALL OUTFMT(EGAS(NGAS)*PGAS,2,STR2,NC2,'LEFT')
*   Extrapolation towards lower E/p.
            IF(JEXTR.EQ.0.AND.(ITEM.EQ.17.OR.ITEM.EQ.18))THEN
                 WRITE(LUNOUT,'(''       for E < '',A,'' V/cm: '',
     -                A,'' is constant.'')')
     -                STR2(1:NC2),SYMBOL(1:NCSYMB)
            ELSEIF(JEXTR.EQ.0)THEN
                 CALL OUTFMT(VAL1,2,STR5,NC5,'LEFT')
                 WRITE(LUNOUT,'(''       for E < '',A,'' V/cm: '',
     -                A,'' = '',A,'' '',A,'','')') STR1(1:NC1),
     -                SYMBOL(1:NCSYMB),STR5(1:NC5),UNIT(1:NCUNIT)
            ELSEIF(JEXTR.EQ.1.AND.(ITEM.EQ.17.OR.ITEM.EQ.18))THEN
                 WRITE(LUNOUT,'(''       for E < '',A,'' V/cm: '',
     -                A,'' is extrapolated linearly.'')')
     -                STR2(1:NC2),SYMBOL(1:NCSYMB)
            ELSEIF(JEXTR.EQ.1)THEN
                 CALL OUTFMT(EXTR3,2,STR3,NC3,'LEFT')
                 CALL OUTFMT(ABS(EXTR4/PGAS),2,STR4,NC4,'LEFT')
                 IF(EXTR4.LT.0)THEN
                      WRITE(LUNOUT,'(''       for E < '',A,
     -                     '' V/cm: '',A,'' = '',A,'' - '',A,
     -                     '' * E '',A,'','')')
     -                     STR1(1:NC1),SYMBOL(1:NCSYMB),
     -                     STR3(1:NC3),STR4(1:NC4),UNIT(1:NCUNIT)
                 ELSE
                      WRITE(LUNOUT,'(''       for E < '',A,
     -                     '' V/cm: '',A,'' = '',A,'' + '',A,
     -                     '' * E '',A,'','')')
     -                     STR1(1:NC1),SYMBOL(1:NCSYMB),
     -                     STR3(1:NC3),STR4(1:NC4),UNIT(1:NCUNIT)
                 ENDIF
            ELSEIF(JEXTR.EQ.2.AND.(ITEM.EQ.17.OR.ITEM.EQ.18))THEN
                 WRITE(LUNOUT,'(''       for E > '',A,'' V/cm: '',
     -                A,'' is extrapolated exponentially.'')')
     -                STR2(1:NC2),SYMBOL(1:NCSYMB)
            ELSEIF(JEXTR.EQ.2)THEN
                 CALL OUTFMT(EXTR3,2,STR3,NC3,'LEFT')
                 CALL OUTFMT(ABS(EXTR4/PGAS),2,STR4,NC4,'LEFT')
                 IF(EXTR4.LT.0)THEN
                      WRITE(LUNOUT,'(''       for E < '',A,
     -                     '' V/cm: '',A,'' = exp('',A,'' - '',A,
     -                     '' * E) '',A,'','')')
     -                     STR1(1:NC1),SYMBOL(1:NCSYMB),
     -                     STR3(1:NC3),STR4(1:NC4),UNIT(1:NCUNIT)
                 ELSE
                      WRITE(LUNOUT,'(''       for E < '',A,
     -                     '' V/cm: '',A,'' = exp('',A,'' + '',A,
     -                     '' * E) '',A,'','')')
     -                     STR1(1:NC1),SYMBOL(1:NCSYMB),
     -                     STR3(1:NC3),STR4(1:NC4),UNIT(1:NCUNIT)
                 ENDIF
            ELSE
                 PRINT *,' !!!!!! GASPRT WARNING : Unknown'//
     -                ' extrapolation method seen.'
            ENDIF
*   Interpolation.
            IF(IMETH.EQ.0)THEN
                 WRITE(LUNOUT,'(''       for '',A,'' < E < '',A,
     -                '' V/cm: '',A,'' is interpolated with cubic'',
     -                '' splines,'')') STR1(1:NC1),STR2(1:NC2),
     -                SYMBOL(1:NCSYMB)
            ELSEIF(IMETH.EQ.1)THEN
                 WRITE(LUNOUT,'(''       for '',A,'' < E < '',A,
     -                '' V/cm: '',A,'' is linearly interpolated,'')')
     -                STR1(1:NC1),STR2(1:NC2),SYMBOL(1:NCSYMB)
            ELSEIF(IMETH.EQ.2)THEN
                 WRITE(LUNOUT,'(''       for '',A,'' < E < '',A,
     -                '' V/cm: '',A,'' is quadratically'',
     -                '' interpolated,'')')
     -                STR1(1:NC1),STR2(1:NC2),SYMBOL(1:NCSYMB)
            ELSEIF(IMETH.EQ.3)THEN
                 WRITE(LUNOUT,'(''       for '',A,'' < E < '',A,
     -                '' V/cm: '',A,'' is cubicly interpolated,'')')
     -                STR1(1:NC1),STR2(1:NC2),SYMBOL(1:NCSYMB)
            ELSE
                 CALL OUTFMT(REAL(IMETH),2,STR6,NC6,'LEFT')
                 WRITE(LUNOUT,'(''       for '',A,'' < E < '',A,
     -                '' V/cm: '',A,'' is interpolated with Newton'',
     -                '' polynomials of order '',A)')
     -                STR1(1:NC1),STR2(1:NC2),SYMBOL(1:NCSYMB),
     -                STR6(1:NC6)
            ENDIF
*   Special case of alpha, eta and dissociation.
            IF((ITEM.EQ.8.AND.IATHR.GT.1).OR.
     -           (ITEM.EQ.9.AND.IBTHR.GT.1).OR.
     -           (ITEM.EQ.16.AND.IHTHR.GT.1))THEN
                 CALL OUTFMT(EGAS(1)*PGAS,2,STR3,NC3,'LEFT')
                 IF(ITEM.EQ.8)THEN
                      CALL OUTFMT(EGAS(IATHR)*PGAS,2,STR4,NC4,'LEFT')
                 ELSEIF(ITEM.EQ.9)THEN
                      CALL OUTFMT(EGAS(IBTHR)*PGAS,2,STR4,NC4,'LEFT')
                 ELSEIF(ITEM.EQ.16)THEN
                      CALL OUTFMT(EGAS(IHTHR)*PGAS,2,STR4,NC4,'LEFT')
                 ELSE
                      STR4='?'
                      NC4=1
                 ENDIF
                 WRITE(LUNOUT,'(7X,''but for '',A,'' < E < '',A,
     -                '' V/cm, linear interpolation is used for'',
     -                '' better numeric stability,'')')
     -                STR3(1:NC3),STR4(1:NC4)
            ENDIF
*   Extrapolation towards higher E/p.
            IF(IEXTR.EQ.0.AND.(ITEM.EQ.17.OR.ITEM.EQ.18))THEN
                 WRITE(LUNOUT,'(''       for E > '',A,'' V/cm: '',
     -                A,'' is constant.'')')
     -                STR2(1:NC2),SYMBOL(1:NCSYMB)
            ELSEIF(IEXTR.EQ.0)THEN
                 CALL OUTFMT(VALN,2,STR5,NC5,'LEFT')
                 WRITE(LUNOUT,'(''       for E > '',A,'' V/cm: '',
     -                A,'' = '',A,'' '',A,''.'')') STR2(1:NC2),
     -                SYMBOL(1:NCSYMB),STR5(1:NC5),UNIT(1:NCUNIT)
            ELSEIF(IEXTR.EQ.1.AND.(ITEM.EQ.17.OR.ITEM.EQ.18))THEN
                 WRITE(LUNOUT,'(''       for E > '',A,'' V/cm: '',
     -                A,'' is extrapolated linearly.'')')
     -                STR2(1:NC2),SYMBOL(1:NCSYMB)
            ELSEIF(IEXTR.EQ.1)THEN
                 CALL OUTFMT(EXTR1,2,STR3,NC3,'LEFT')
                 CALL OUTFMT(ABS(EXTR2/PGAS),2,STR4,NC4,'LEFT')
                 IF(EXTR2.LT.0)THEN
                      WRITE(LUNOUT,'(''       for E > '',A,
     -                     '' V/cm: '',A,'' = '',A,'' - '',A,
     -                     '' * E '',A,''.'')')
     -                     STR2(1:NC2),SYMBOL(1:NCSYMB),
     -                     STR3(1:NC3),STR4(1:NC4),UNIT(1:NCUNIT)
                 ELSE
                      WRITE(LUNOUT,'(''       for E > '',A,
     -                     '' V/cm: '',A,'' = '',A,'' + '',A,
     -                     '' * E '',A,''.'')')
     -                     STR2(1:NC2),SYMBOL(1:NCSYMB),
     -                     STR3(1:NC3),STR4(1:NC4),UNIT(1:NCUNIT)
                 ENDIF
            ELSEIF(IEXTR.EQ.2.AND.(ITEM.EQ.17.OR.ITEM.EQ.18))THEN
                 WRITE(LUNOUT,'(''       for E > '',A,'' V/cm: '',
     -                A,'' is extrapolated exponentially.'')')
     -                STR2(1:NC2),SYMBOL(1:NCSYMB)
            ELSEIF(IEXTR.EQ.2)THEN
                 CALL OUTFMT(EXTR1,2,STR3,NC3,'LEFT')
                 CALL OUTFMT(ABS(EXTR2/PGAS),2,STR4,NC4,'LEFT')
                 IF(EXTR2.LT.0)THEN
                      WRITE(LUNOUT,'(''       for E > '',A,
     -                     '' V/cm: '',A,'' = exp('',A,'' - '',A,
     -                     '' * E) '',A,''.'')')
     -                     STR2(1:NC2),SYMBOL(1:NCSYMB),
     -                     STR3(1:NC3),STR4(1:NC4),UNIT(1:NCUNIT)
                 ELSE
                      WRITE(LUNOUT,'(''       for E > '',A,
     -                     '' V/cm: '',A,'' = exp('',A,'' + '',A,
     -                     '' * E) '',A,''.'')')
     -                     STR2(1:NC2),SYMBOL(1:NCSYMB),
     -                     STR3(1:NC3),STR4(1:NC4),UNIT(1:NCUNIT)
                 ENDIF
            ELSE
                 PRINT *,' !!!!!! GASPRT WARNING : Unknown'//
     -                ' extrapolation method seen.'
            ENDIF
       ENDIF
**  Next item.
50     CONTINUE
*** Print some information about the clustersize information.
100    CONTINUE
       CALL OUTFMT(PGAS,2,STR1,NC1,'LEFT')
       CALL OUTFMT(TGAS,2,STR2,NC2,'LEFT')
       WRITE(LUNOUT,'(//
     -      ''  Other data:''//
     -      ''  Pressure of the gas               : '',A,'' Torr''/
     -      ''  Temperature of the gas            : '',A,'' K''/)')
     -      STR1(1:NC1),STR2(1:NC2)
       IF((CLSTYP.EQ.'LANDAU'.AND.GASOK(5)).OR.SRIMOK)THEN
            CALL OUTFMT(Z,2,STR1,NC1,'LEFT')
            CALL OUTFMT(A,2,STR2,NC2,'LEFT')
            CALL OUTFMT(RHO,2,STR3,NC3,'LEFT')
            WRITE(LUNOUT,'(
     -           ''  Number of protons in one molecule : '',A/
     -           ''  Atomic number of the gas          : '',A/
     -           ''  Density                           : '',A,
     -           '' g/cm3''/)')
     -           STR1(1:NC1),STR2(1:NC2),STR3(1:NC3)
       ENDIF
       IF(CLSTYP.EQ.'LANDAU'.AND.GASOK(5))THEN
            CALL OUTFMT(EMPROB,2,STR4,NC4,'LEFT')
            CALL OUTFMT(EPAIR,2,STR5,NC5,'LEFT')
            WRITE(LUNOUT,'(
     -           ''  Most probable energy loss per cm  : '',A,
     -           '' eV/cm''/
     -           ''  Energy needed for one ion pair    : '',A,
     -           '' eV'')') STR4(1:NC4),STR5(1:NC5)
       ENDIF
       IF(GASOK(5))THEN
            CALL OUTFMT(CMEAN,2,STR1,NC1,'LEFT')
            WRITE(LUNOUT,'(
     -           ''  Average number of clusters        : '',A,
     -           '' per cm'')') STR1(1:NC1)
       ENDIF
       IF(DLION.LT.0)THEN
            WRITE(LUNOUT,'(
     -      ''  Longitudinal ion diffusion        : Thermal'')')
       ELSE
            CALL OUTFMT(10000*DLION,2,STR1,NC1,'LEFT')
            WRITE(LUNOUT,'(
     -           ''  Longitudinal ion diffusion        : '',A,
     -           '' micron for 1 cm of drift'')') STR1(1:NC1)
       ENDIF
       IF(DTION.LT.0)THEN
            WRITE(LUNOUT,'(
     -      ''  Transverse ion diffusion          : Thermal'')')
       ELSE
            CALL OUTFMT(10000*DTION,2,STR2,NC2,'LEFT')
            WRITE(LUNOUT,'(
     -           ''  Transverse ion diffusion          : '',A,
     -           '' micron for 1 cm of drift'')') STR2(1:NC2)
       ENDIF
*** Register the amount of CPU time used for printing,
       CALL TIMLOG('Printing of the gas data:               ')
       END
