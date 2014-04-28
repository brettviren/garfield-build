CDECK  ID>, GASMXB.
       SUBROUTINE GASMXB
*-----------------------------------------------------------------------
*   GASMXB - Sets the break points for the integration routines, find
*            the lowest ionisation potential and store the gas name.
*   REFERENCE : Ionisation data from Handbook of Chemistry and Physics,
*               72nd edition 1991-1992, CRC press, p 10-211 to 10-219,
*               Edited by David R. Lide.
*   (Last changed on 23/ 2/99.)
*-----------------------------------------------------------------------
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
*-----------------------------------------------------------------------
*   GMXDAT - Common block for gas mixing.
*   (Last changed on 20/ 2/97.)
*-----------------------------------------------------------------------
       REAL BREAK,FRAC,XLOSCH,EFLD,ESTEP,ECRIT
       INTEGER NBREAK
       COMMON /GMXDAT/ BREAK(MXLIST),FRAC(MXFRAC),XLOSCH,
     -      EFLD,ESTEP,ECRIT,NBREAK
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
       CHARACTER*169 AUX
*** Initial value of the ionisation level.
       ECRIT=1.0E10
*** Initial value of the break point list.
       NBREAK=1
       BREAK(1)=0.0
*** Blank the gas name string.
       AUX=' '
       GASID=' '
*** Argon (Ar).
       IF(FRAC(1).GT.0.0)THEN
*   Break points.
            BREAK(NBREAK+1)=0.3
            BREAK(NBREAK+2)=1.15
            BREAK(NBREAK+3)=11.5
            NBREAK=NBREAK+3
*   Name.
            WRITE(AUX(1:13),'(''Ar    !'',I3,''%,!'')')
     -           NINT(FRAC(1)*100.0)
*   Ionisation levels.
            ECRIT=MIN(15.759,ECRIT)
       ENDIF
*** Methane (CH4).
       IF(FRAC(2).GT.0.0)THEN
*   Break points.
            BREAK(NBREAK+1)=0.3
            BREAK(NBREAK+2)=0.36
            BREAK(NBREAK+3)=2.0
            BREAK(NBREAK+4)=8.0
            NBREAK=NBREAK+4
*   Name.
            WRITE(AUX(14:26),'(''CH4   !'',I3,''%,!'')')
     -           NINT(FRAC(2)*100.0)
*   Ionisation levels.
            ECRIT=MIN(12.6,ECRIT)
       ENDIF
*** Neon (Ne).
       IF(FRAC(3).GT.0.0)THEN
*   Break points.
            BREAK(NBREAK+1)=7
            NBREAK=NBREAK+1
*   Name.
            WRITE(AUX(27:39),'(''Ne    !'',I3,''%,!'')')
     -           NINT(FRAC(3)*100.0)
*   Ionisation levels.
            ECRIT=MIN(21.564,ECRIT)
       ENDIF
*** Isobutane (C4 H10).
       IF(FRAC(4).GT.0.0)THEN
*   Break points.
            BREAK(NBREAK+1)=0.20
            BREAK(NBREAK+2)=0.36
            BREAK(NBREAK+3)=0.60
            BREAK(NBREAK+4)=8.0
            NBREAK=NBREAK+4
*   Name.
            WRITE(AUX(40:52),'(''C4H10 !'',I3,''%,!'')')
     -           NINT(FRAC(4)*100.0)
*   Ionisation levels (n-C4H10: 10.63 eV, iso: 10.57 eV).
            ECRIT=MIN(10.6,ECRIT)
       ENDIF
*** CO2.
       IF(FRAC(5).GT.0.0)THEN
*   Break points.
            BREAK(NBREAK+1)=0.20
            BREAK(NBREAK+2)=1.32
            BREAK(NBREAK+3)=3.25
            BREAK(NBREAK+4)=4.2
            BREAK(NBREAK+5)=6.0
            BREAK(NBREAK+6)=25.0
            NBREAK=NBREAK+6
*   Name.
            WRITE(AUX(53:65),'(''CO2   !'',I3,''%,!'')')
     -           NINT(FRAC(5)*100.0)
*   Ionisation levels.
            ECRIT=MIN(13.769,ECRIT)
       ENDIF
*** Helium (He).
       IF(FRAC(6).GT.0.0)THEN
*   Break points.
            NBREAK=NBREAK
*   Name.
            WRITE(AUX(157:169),'(''He    !'',I3,''%,!'')')
     -           NINT(FRAC(6)*100.0)
*   Ionisation levels.
            ECRIT=MIN(24.587,ECRIT)
       ENDIF
*** Ethane (C2 H6).
       IF(FRAC(7).GT.0.0)THEN
*   Break points.
            BREAK(NBREAK+1)=0.025
            BREAK(NBREAK+2)=0.035
            BREAK(NBREAK+3)=0.07
            BREAK(NBREAK+4)=0.09
            BREAK(NBREAK+5)=0.2
            BREAK(NBREAK+6)=0.3
            BREAK(NBREAK+7)=0.36
            BREAK(NBREAK+8)=0.6
            BREAK(NBREAK+9)=1.0
            NBREAK=NBREAK+9
*   Name.
            WRITE(AUX(66:78),'(''C2H6  !'',I3,''%,!'')')
     -           NINT(FRAC(7)*100.0)
*   Ionisation levels.
            ECRIT=MIN(11.5,ECRIT)
       ENDIF
*** Nitrogen (N).
       IF(FRAC(8).GT.0.0)THEN
*   Break points.
            BREAK(NBREAK+1)=1.3
            BREAK(NBREAK+2)=1.4
            BREAK(NBREAK+3)=1.5
            BREAK(NBREAK+4)=1.6
            BREAK(NBREAK+5)=1.7
            BREAK(NBREAK+6)=1.8
            BREAK(NBREAK+7)=1.9
            BREAK(NBREAK+8)=2.0
            BREAK(NBREAK+9)=5.0
            NBREAK=NBREAK+9
*   Name.
            WRITE(AUX(79:91),'(''N     !'',I3,''%,!'')')
     -           NINT(FRAC(8)*100.0)
*   Ionisation levels.
            ECRIT=MIN(14.534,ECRIT)
       ENDIF
*** Xenon (Xe).
       IF(FRAC(9).GT.0.0)THEN
*   Break points.
            BREAK(NBREAK+1)=0.01
            BREAK(NBREAK+2)=0.035
            BREAK(NBREAK+3)=0.1
            BREAK(NBREAK+4)=0.18
            BREAK(NBREAK+5)=0.5
            BREAK(NBREAK+6)=0.7
            BREAK(NBREAK+7)=2.0
            BREAK(NBREAK+8)=4.1
            BREAK(NBREAK+9)=10.0
            NBREAK=NBREAK+9
*   Name.
            WRITE(AUX(92:104),'(''Xe    !'',I3,''%,!'')')
     -           NINT(FRAC(9)*100.0)
*   Ionisation levels.
            ECRIT=MIN(12.130,ECRIT)
       ENDIF
*** Methylal (C3 H8 O2).
       IF(FRAC(10).GT.0.0)THEN
*   Break points.
            BREAK(NBREAK+1)=0.36
            BREAK(NBREAK+2)=2.0
            BREAK(NBREAK+3)=4.0
            NBREAK=NBREAK+3
*   Name.
            WRITE(AUX(105:117),'(''C3H8O2!'',I3,''%,!'')')
     -           NINT(FRAC(10)*100.0)
*   Ionisation levels (n-C3H7OH: 10.1 eV, iso: 10.15 eV).
            ECRIT=MIN(10.1,ECRIT)
       ENDIF
*** Krypton.
       IF(FRAC(11).GT.0.0)THEN
*   Break points.
            BREAK(NBREAK+1)=0.01
            BREAK(NBREAK+2)=0.02
            BREAK(NBREAK+3)=0.04
            BREAK(NBREAK+4)=0.07
            BREAK(NBREAK+5)=0.1
            BREAK(NBREAK+6)=0.145
            BREAK(NBREAK+7)=0.2
            BREAK(NBREAK+8)=0.3
            BREAK(NBREAK+9)=0.4
            BREAK(NBREAK+10)=0.5
            BREAK(NBREAK+11)=0.6
            BREAK(NBREAK+12)=0.8
            BREAK(NBREAK+13)=1.0
            BREAK(NBREAK+14)=2.0
            BREAK(NBREAK+15)=3.0
            BREAK(NBREAK+16)=4.0
            BREAK(NBREAK+17)=5.0
            BREAK(NBREAK+18)=7.0
            BREAK(NBREAK+19)=10.0
            NBREAK=NBREAK+19
*   Name.
            WRITE(AUX(118:130),'(''Kr    !'',I3,''%,!'')')
     -           NINT(FRAC(11)*100.0)
*   Ionisation levels.
            ECRIT=MIN(13.999961,ECRIT)
       ENDIF
*** Ammonia.
       IF(FRAC(12).GT.0.0)THEN
*   Break points.
            BREAK(NBREAK+1)=0.01
            BREAK(NBREAK+2)=0.02
            BREAK(NBREAK+3)=0.04
            BREAK(NBREAK+4)=0.1
            BREAK(NBREAK+5)=0.2
            BREAK(NBREAK+6)=0.4
            BREAK(NBREAK+7)=1.0
            BREAK(NBREAK+8)=2.0
            BREAK(NBREAK+9)=3.0
            BREAK(NBREAK+10)=5.0
            BREAK(NBREAK+11)=7.0
            BREAK(NBREAK+12)=10.0
            NBREAK=NBREAK+12
*   Name.
            WRITE(AUX(131:143),'(''NH3   !'',I3,''%,!'')')
     -           NINT(FRAC(12)*100.0)
*   Ionisation levels.
            ECRIT=MIN(10.16,ECRIT)
       ENDIF
*** Test gas.
       IF(FRAC(13).GT.0.0)THEN
*   Break points.
            NBREAK=NBREAK
*   Name.
            WRITE(AUX(144:156),'(''Test  !'',I3,''%,!'')')
     -           NINT(FRAC(13)*100.0)
*   Ionisation levels.
            ECRIT=ECRIT
       ENDIF
*** Sort the break points upwards.
       CALL FLPSOR(BREAK,NBREAK)
*** List the break points if debugging has been requested.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMXB DEBUG   : Number of'',
     -      '' integration break points: '',I3/
     -      (26X,5(F10.3:)/))') NBREAK,(BREAK(I),I=1,NBREAK)
*** Get rid of blanks in the gas name.
       NOUT=0
       DO 10 I=1,169
       IF(AUX(I:I).NE.' ')THEN
            NOUT=NOUT+1
            IF(NOUT.LE.80.AND.AUX(I:I).EQ.'!')THEN
                 GASID(NOUT:NOUT)=' '
            ELSEIF(NOUT.LE.80)THEN
                 GASID(NOUT:NOUT)=AUX(I:I)
            ENDIF
       ENDIF
10     CONTINUE
       IF(NOUT.GT.80)THEN
            GASID(78:80)='...'
            NOUT=80
       ELSE
            GASID(NOUT-1:NOUT)='. '
            NOUT=NOUT-1
       ENDIF
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMXB DEBUG   : Name: '',
     -      A)') GASID(1:NOUT)
*** Lowest ionisation level.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMXB DEBUG   : Lowest'',
     -      '' ionisation level at '',F10.3,'' eV.'')') ECRIT
       END
