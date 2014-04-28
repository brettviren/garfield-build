CDECK  ID>, DLCMIC.
       SUBROUTINE DLCMIC(X1,Y1,Z1,OPTION,NCOPT,EF,ES,DIRX,DIRY,DIRZ,
     -      IRCS,IHF)
*-----------------------------------------------------------------------
*   DLCMIC - Microscopic MC tracking front-end
*            NSTATL(IPT): 1=elastic, 2=ionise, 3=attach, 4=excitation,
*                         5=super-elastic, 6=inelastic
*            NSTATN(I)  : individual level described in DSCRPT(I)
*   (Last changed on 25/ 5/09.)
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
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
*   Array dimensions.
       integer mxngas
       parameter(mxngas=6)
*-----------------------------------------------------------------------
*   MAGPAR - Interface parameters for gas mixing with Magboltz.
*   (Last changed on  2/ 3/08.)
*-----------------------------------------------------------------------
       INTEGER MXGNAM
       PARAMETER(MXGNAM=60)
       DOUBLE PRECISION FRAMIX
       LOGICAL LF0PLT,LCSPLT,LGKEEP,LBMCPR
       COMMON /MAGPAR/ FRAMIX(MXGNAM),LF0PLT,LCSPLT,LGKEEP,LBMCPR
*   Sometimes IPLAST is called LAST
       DOUBLE PRECISION CF,EIN,TCF,RGAS,WPL
       INTEGER IARRY,IPN,IPLAST,ISIZE
       COMMON/LARGE/CF(2048,512),EIN(512),TCF(2048),IARRY(512),
     -      RGAS(512),IPN(512),WPL(512),IPLAST,ISIZE
       CHARACTER*30 DSCRPT
       COMMON/SCRIP/DSCRPT(512)
*   Changed name of common from /NAMES/ to /MBGNAM/ for Mac OS X
       CHARACTER*15 NAMEG
       COMMON /MBGNAM/ NAMEG(mxngas)
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       REAL X1,Y1,Z1,EF,ES,DIRX,DIRY,DIRZ,DNORM
       INTEGER IFAIL,NSTATL(5*MXNGAS),NSTATN(512),I,IPT,IGAS,ITYPE,
     -      IREF,IHF,ISIZ(1),MATSLT,IRCS,ISCS,NCOPT
       LOGICAL LPLION,LPLEXC,LPLINE,LPLELA,LPLSUP,LPLATT,
     -      lheinrich
       CHARACTER*(*) OPTION
       EXTERNAL MATSLT
*** Check the energy
       IF(ES.LT.0)THEN
            PRINT *,' !!!!!! DLCMIC WARNING : Starting energy is'//
     -           ' negative; abandoned.'
            NU=0
            ISTAT=-3
            RETURN
       ELSEIF(EF.LE.0)THEN
            PRINT *,' !!!!!! DLCMIC WARNING : Final energy is'//
     -           ' non-positive; abandoned.'
            NU=0
            ISTAT=-3
            RETURN
       ENDIF
*** Initialise the gas
       CALL DLCMII(EF,NSTATL,NSTATN,IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! DLCMIC WARNING : Initialisation failed'//
     -           ' ; no calculation performed.'
            RETURN
       ENDIF
*** Decode the options: Magboltz print option.
       LBMCPR=.FALSE.
       IF(INDEX(OPTION(1:NCOPT),'NOPRINT').NE.0)THEN
            LBMCPR=.FALSE.
       ELSEIF(INDEX(OPTION(1:NCOPT),'PRINT').NE.0)THEN
            LBMCPR=.TRUE.
       ENDIF
*   Mark ionisations
       LPLION=.FALSE.
       IF(INDEX(OPTION(1:NCOPT),'NOMARK-ION').NE.0)THEN
            LPLION=.FALSE.
       ELSEIF(INDEX(OPTION(1:NCOPT),'MARK-ION').NE.0)THEN
            LPLION=.TRUE.
       ENDIF
*   Mark excitations
       LPLEXC=.FALSE.
       IF(INDEX(OPTION(1:NCOPT),'NOMARK-EXC').NE.0)THEN
            LPLEXC=.FALSE.
       ELSEIF(INDEX(OPTION(1:NCOPT),'MARK-EXC').NE.0)THEN
            LPLEXC=.TRUE.
       ENDIF
*   Mark inelastic
       LPLINE=.FALSE.
       IF(INDEX(OPTION(1:NCOPT),'NOMARK-INEL').NE.0)THEN
            LPLINE=.FALSE.
       ELSEIF(INDEX(OPTION(1:NCOPT),'MARK-INEL').NE.0)THEN
            LPLINE=.TRUE.
       ENDIF
*   Mark elastic
       LPLELA=.FALSE.
       IF(INDEX(OPTION(1:NCOPT),'NOMARK-ELAS').NE.0)THEN
            LPLELA=.FALSE.
       ELSEIF(INDEX(OPTION(1:NCOPT),'MARK-ELAS').NE.0)THEN
            LPLELA=.TRUE.
       ENDIF
*   Mark super-elastic
       LPLSUP=.FALSE.
       IF(INDEX(OPTION(1:NCOPT),'NOMARK-SUP').NE.0)THEN
            LPLSUP=.FALSE.
       ELSEIF(INDEX(OPTION(1:NCOPT),'MARK-SUP').NE.0)THEN
            LPLSUP=.TRUE.
       ENDIF
*   Mark attachment
       LPLATT=.FALSE.
       IF(INDEX(OPTION(1:NCOPT),'NOMARK-ATT').NE.0)THEN
            LPLATT=.FALSE.
       ELSEIF(INDEX(OPTION(1:NCOPT),'MARK-ATT').NE.0)THEN
            LPLATT=.TRUE.
       ENDIF
*   Heinrich options
       lheinrich=.FALSE.
       IF(INDEX(OPTION(1:NCOPT),'NOHEINRICH').NE.0)THEN
            lheinrich=.FALSE.
       ELSEIF(INDEX(OPTION(1:NCOPT),'HEINRICH').NE.0)THEN
            lheinrich=.TRUE.
       ENDIF
*** Book an histogram for the energy distribution, if needed.
       IF(IHF.LT.0)THEN
            CALL HISADM('ALLOCATE',IHF,100,0.0,EF,.FALSE.,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! DLCMIC WARNING : Unable to obtain'//
     -                ' matrix storage; energy not histogrammed.'
                 IHF=0
            ENDIF
       ENDIF
*** Normalise the initial vector.
       IF(DIRX**2+DIRY**2+DIRZ**2.LE.0)THEN
            PRINT *,' !!!!!! DLCMIC WARNING : Initial direction'//
     -           ' vector has zero norm; no calculation.'
            RETURN
       ELSE
            DNORM=SQRT(DIRX**2+DIRY**2+DIRZ**2)
            DIRX=DIRX/DNORM
            DIRY=DIRY/DNORM
            DIRZ=DIRZ/DNORM
       ENDIF
*** Step.
       CALL DLCMST(X1,Y1,Z1,EF,ES,DIRX,DIRY,DIRZ,NSTATL,NSTATN,IHF,
     -      LPLION,LPLEXC,LPLINE,LPLELA,LPLSUP,LPLATT,lheinrich)
*** Extract the statistics: first initialise the counters.
       IF(IRCS.GT.0)THEN
*   Loop over the levels.
            DO 10 I=1,IPLAST
            IPT=IARRY(I)
            IGAS=1+(IPT-1)/5
            ITYPE=IPT-5*(IGAS-1)
            IF(ITYPE.EQ.4.AND.DSCRPT(I)(1:4).NE.' EXC')ITYPE=6
            CALL GASIDO(IREF,NAMEG(IGAS)//DSCRPT(I),ITYPE,0.0,IFAIL)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMIC DEBUG   :'',
     -           '' Gas '',I3,'', cs type '',I1,'', cs "'',A,''",'',
     -           '' count '',I6,'', ref '',I3)') IGAS,ITYPE,DSCRPT(I),
     -           NSTATN(I),IREF
            IF(IFAIL.NE.0)PRINT *,' !!!!!! DLCMIC WARNING : Unable'//
     -           ' to store a rate.'
10          CONTINUE
*** Store the matrices: elastic.
            ISIZ(1)=IPLAST
            CALL MATADM('ALLOCATE',IRCS,1,ISIZ,2,IFAIL)
            ISCS=MATSLT(IRCS)
            DO 20 I=1,IPLAST
            MVEC(MORG(ISCS)+I)=NSTATN(I)
20          CONTINUE
       ENDIF
       END
       SUBROUTINE DLCMII(EF,NSTATL,NSTATN,IFAIL)
*-----------------------------------------------------------------------
*   DLCMII - Initialisation for microscopic MC tracking.
*   (Last changed on  3/ 4/08.)
*-----------------------------------------------------------------------
       implicit none
*   Array dimensions.
       integer mxngas
       parameter(mxngas=6)
*-----------------------------------------------------------------------
*   MAGPAR - Interface parameters for gas mixing with Magboltz.
*   (Last changed on  2/ 3/08.)
*-----------------------------------------------------------------------
       INTEGER MXGNAM
       PARAMETER(MXGNAM=60)
       DOUBLE PRECISION FRAMIX
       LOGICAL LF0PLT,LCSPLT,LGKEEP,LBMCPR
       COMMON /MAGPAR/ FRAMIX(MXGNAM),LF0PLT,LCSPLT,LGKEEP,LBMCPR
       INTEGER NGAS,NSTEP,IDBG
       DOUBLE PRECISION EFINAL,ESTEP,AKT,ARY,TEMPC,TORR
       PARAMETER(ARY=13.60569172)
       COMMON/INPT/NGAS,NSTEP,EFINAL,ESTEP,AKT,TEMPC,TORR,IDBG
       INTEGER I,IFAIL,NMAX,NSTATL(5*MXNGAS),NSTATN(512)
       DOUBLE PRECISION E,B,BTH,T,P
       REAL EF,PGAS,TGAS,FRASUM,GASFRM(MXGNAM)
       CHARACTER*80 GASID
*** Retrieve pressure and temperature.
       CALL GASINF(PGAS,TGAS,GASID,GASFRM)
*** Set the gas mixture.
       FRASUM=0
       DO 10 I=1,MXGNAM
       FRAMIX(I)=GASFRM(I)
       FRASUM=FRASUM+FRAMIX(I)
10     CONTINUE
       IF(FRASUM.LE.0)THEN
            PRINT *,' !!!!!! DLCMII WARNING : Sum of gas fractions'//
     -           ' less or equal to 0; no calculation.'
            IFAIL=1
            RETURN
       ENDIF
*** Set E, B and angle: dummy values
       E=1000.0
       B=0.0
       BTH=0.0
*   Set the pressure and temperature.
       T=TGAS
       P=PGAS
*   Number of collisions: dummy value
       NMAX=2
*** Establish the parameters.
       CALL SETB7(E,B,BTH,T,P,NMAX,IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! DLCMII WARNING : Setting Magboltz'//
     -           ' parameters failed.'
            IFAIL=1
            RETURN
       ENDIF
*   Overrrule the energy limit
       EFINAL=DBLE(EF)
*** Mix the gases
       CALL MIXER7(IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! DLCMII WARNING : Setting Magboltz'//
     -           ' cross section tables failed.'
            IFAIL=1
            RETURN
       ENDIF
*** Plot the cross sections
C      call gaspcs('DLCMIC')
*** Initial statistics.
       DO 20 I=1,5*MXNGAS
       NSTATL(I)=0
20     CONTINUE
       DO 30 I=1,512
       NSTATN(I)=0
30     CONTINUE
*** Has worked.
       IFAIL=0
       END
       SUBROUTINE DLCMST(X1,Y1,Z1,EF,ES,DIRX,DIRY,DIRZ,
     -      NSTATL,NSTATN,IHF,
     -      LPLION,LPLEXC,LPLINE,LPLELA,LPLSUP,LPLATT,lheinrich)
*-----------------------------------------------------------------------
*   DLCMST - Microscopic MC tracking, derived from MONTE
*   (Last changed on  2/ 8/10.)
*-----------------------------------------------------------------------
      implicit none
*   Array dimensions.
       integer mxngas
       parameter(mxngas=6)
*-----------------------------------------------------------------------
*   MAGPAR - Interface parameters for gas mixing with Magboltz.
*   (Last changed on  2/ 3/08.)
*-----------------------------------------------------------------------
       INTEGER MXGNAM
       PARAMETER(MXGNAM=60)
       DOUBLE PRECISION FRAMIX
       LOGICAL LF0PLT,LCSPLT,LGKEEP,LBMCPR
       COMMON /MAGPAR/ FRAMIX(MXGNAM),LF0PLT,LCSPLT,LGKEEP,LBMCPR
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
       INTEGER NGAS,NSTEP,IDBG
       DOUBLE PRECISION EFINAL,ESTEP,AKT,ARY,TEMPC,TORR
       PARAMETER(ARY=13.60569172)
       COMMON/INPT/NGAS,NSTEP,EFINAL,ESTEP,AKT,TEMPC,TORR,IDBG
       DOUBLE PRECISION PIR2,ECHARG,EMASS,AMU,BOLTZ,BOLTZJ,
     -      AWB,ALOSCH,ABZERO,ATMOS
       PARAMETER(PIR2=8.79735534D-17)
       PARAMETER(ECHARG=1.602176462D-19)
       PARAMETER(EMASS=9.10938188D-31)
       PARAMETER(AMU=1.66053873D-27)
       PARAMETER(BOLTZ=8.617342D-5)
       PARAMETER(BOLTZJ=1.3806503D-23)
       PARAMETER(AWB=1.758820174D10)
       PARAMETER(ALOSCH=2.6867775D19)
       PARAMETER(ABZERO=273.15D0)
       PARAMETER(ATMOS=760.0D0)
       DOUBLE PRECISION CONST1,CONST2,CONST3,CONST4,CONST5
       COMMON/CNSTS1/CONST1,CONST2,CONST3,CONST4,CONST5
       DOUBLE PRECISION TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX,RSTART,
     -      EMAG
       INTEGER NMAX
       COMMON/SETP/TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX(8),RSTART,
     -      EMAG,NMAX
*   Sometimes IPLAST is called LAST
       DOUBLE PRECISION CF,EIN,TCF,RGAS,WPL
       INTEGER IARRY,IPN,IPLAST,ISIZE
       COMMON/LARGE/CF(2048,512),EIN(512),TCF(2048),IARRY(512),
     -      RGAS(512),IPN(512),WPL(512),IPLAST,ISIZE
*   Is in effect the old ANCT common.
       DOUBLE PRECISION PSCT,ANGCT
       INTEGER INDEX,NISO
       COMMON/ANIS/PSCT(2048,512),ANGCT(2048,512),INDEX(512),NISO
       CHARACTER*30 DSCRPT
       COMMON/SCRIP/DSCRPT(512)
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       DOUBLE PRECISION X,Y,Z,ST,RDUM,E1,E,EX,EY,EZ,
     -      CONST6,CONST7,CONST9,TDASH,TLIM,T,T2,
     -      DCX1,DCY1,DCZ1,DCX2,DCY2,DCZ2,THETA0,PHI0,
     -      AP,BP,A,F3,F4,F5,F6,F8,F9,R1,R2,R3,R4,R5,R9,R31,
     -      S1,S2,EXTRA,EI,D,Q,ARG1,ARGZ,CSQD,DELTAE,U,
     -      DRAND48,EMAX,XPL(1),YPL(1),ZPL(1),
     -      xold, yold, zold, eold
       REAL X1,Y1,Z1,ES,EF,XS,YS,ZS,EXS,EYS,EZS,ETOTS,VOLTS,
     -      DIRX,DIRY,DIRZ,XNEW,YNEW,ZNEW,DELAY,ENEW
       INTEGER INTEM,J1,NCOL,NNULL,I,IE,IPT,ILOC,IGAS,ITYPE,
     -      NSTATL(5*MXNGAS),NSTATN(512),IHF
       LOGICAL LPLION,LPLEXC,LPLINE,LPLELA,LPLSUP,LPLATT,lheinrich,
     -      ADDNEW
       EXTERNAL DRAND48
      integer ioncount
*** Debugging
       IF(LDEBUG)WRITE(LUNOUT,*) '  ++++++ DLCMST DEBUG   : Start: ',
     -      X1,Y1,Z1,', energy start/max: ',ES,EF
*** Initialise the steps
       NU=1
       XU(NU)=DBLE(X1)
       YU(NU)=DBLE(Y1)
       ZU(NU)=DBLE(Z1)
       TU(NU)=0
       CALL DLCSTA(-1.0,1)
       IF(ISTAT.NE.0)RETURN
*** Heinrich options.
       if(lheinrich)then
            open(unit=38,file='heinrich.dump')
       endif
*** Set technique and particle labels
       IPTYPE=1
       QPCHAR=-1.0
       IPTECH=4
*** Starting point.
       X=X1/1.0D2
       Y=Y1/1.0D2
       Z=Z1/1.0D2
*   Starting time.
       ST=0.0D0
*   Starting energy and keeping track of energy
       E1=DBLE(ES)
       EMAX=E1
*   Various parameters.
       SMALL=1.0D-20
       RDUM=RSTART
       CONST9=CONST3*0.01D0
       INTEM=8
       NCOL=0
       NNULL=0
*   Number of collisions for de-correlation ?
       TDASH=0.0D0
*** Initial direction cosines
       DCX1=DBLE(DIRX)
       DCY1=DBLE(DIRY)
       DCZ1=DBLE(DIRZ)
*   Parameters to compute position after step
       F4=2.0D0*ACOS(-1.0D0)
       DELTAE=EFINAL/DBLE(INTEM)
*** Main loop
       DO 210 J1=1,100000000
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMST DEBUG   : Step '',
     -      I3,'', iteration '',I5,'', collision '',I5)') NU,J1,NCOL
*** Get the electric and magnetic field
       XS=REAL(X*100.0)
       YS=REAL(Y*100.0)
       ZS=REAL(Z*100.0)
       CALL EFIELD(XS,YS,ZS,EXS,EYS,EZS,ETOTS,VOLTS,0,ILOC)
       EX=-DBLE(EXS)
       EY=-DBLE(EYS)
       EZ=-DBLE(EZS)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMST DEBUG   : At ('',
     -      3E15.8,''), E = ('',3E15.8,''), loc = '',I5,'', nu = '',
     -      I5)') X,Y,Z,EX,EY,EZ,ILOC,NU
*   If this is not in the drift medium, stop now.
       IF(ILOC.NE.0.OR.
     -      XS.LT.DDXMIN.OR.XS.GT.DDXMAX.OR.
     -      YS.LT.DDYMIN.OR.YS.GT.DDYMAX.OR.
     -      ZS.LT.DDZMIN.OR.ZS.GT.DDZMAX)THEN
            IF(NU.GE.MXLIST)THEN
                 ISTAT=-2
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMST DEBUG   :'',
     -                '' Too many steps, istat = -2, e_max = '',E15.8,
     -                '' eV, nu = '',I4,'', ncol = '',I5)') EMAX,NU,NCOL
*   Heinrich options.
                 if(lheinrich)close(unit=38)
                 RETURN
            ENDIF
            IF(ILOC.EQ.-5.OR.ILOC.EQ.-6)THEN
                 CALL DLCFMP(XU(NU),YU(NU),ZU(NU),
     -                X*1.0D2,Y*1.0D2,Z*1.0D2,
     -                T*1.0D-6,ILOC,QPCHAR,IPTECH)
            ELSE
                 NU=NU+1
                 XU(NU)=X*1.0D2
                 YU(NU)=Y*1.0D2
                 ZU(NU)=Z*1.0D2
                 TU(NU)=ST*1.0D-6
                 CALL DLCSTA(-1.0,1)
            ENDIF
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMST DEBUG   :'',
     -           '' loc = '',I5,'', istat = '',I5,'', e_max = '',E15.8,
     -           '' eV, nu = '',I4,'', ncol = '',I5)')
     -           ILOC,ISTAT,EMAX,NU,NCOL
*   Heinrich options.
            if(lheinrich)close(unit=38)
            RETURN
       ENDIF
*** Determine free time
    1  R1=drand48(RDUM)
       I=INT(E1/DELTAE)+1
       I=MIN(I,INTEM)
       TLIM=TCFMAX(I)
       T=-LOG(R1)/TLIM+TDASH
       t = dtfact*t
       TDASH=T
       AP=CONST3*(DCX1*EX+DCY1*EY+DCZ1*EZ)*SQRT(E1)
       BP=(EX**2+EY**2+EZ**2)*CONST1
       E=E1+(AP+BP*T)*T
*   Keep track of the highest energy
       EMAX=MAX(E,EMAX)
       IF(E.GT.EFINAL)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMST DEBUG   :'',
     -           '' Energy '',E15.8,'' eV exceeds e_maximum '',E15.8,
     -           '' eV; abandoned at nu = '',I4,'', ncol = '',I5)')
     -           E,EFINAL,NU,NCOL
            IF(NU.GE.MXLIST)THEN
                 ISTAT=-2
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMST DEBUG   :'',
     -                '' Too many steps, istat = -2, e_max = '',E15.8,
     -                '' eV, nu = '',I4,'', ncol = '',I5)') EMAX,NU,NCOL
*   Heinrich options.
                 if(lheinrich)close(unit=38)
                 RETURN
            ENDIF
            NU=NU+1
            XU(NU)=X*1.0D2
            YU(NU)=Y*1.0D2
            ZU(NU)=Z*1.0D2
            TU(NU)=ST*1.0D-6
            ISTAT=-9
*   Heinrich options.
            if(lheinrich)close(unit=38)
            RETURN
       ENDIF
       IF(IHF.GT.0)CALL HISENT(IHF,REAL(E),1.0)
*   Check null collisions
       IE=INT(E/ESTEP)+1
       IE=MIN(IE,2048)
       IF(TCF(IE).GT.TLIM) THEN
            TDASH=TDASH+LOG(R1)/TLIM
            TCFMAX(I)=1.05D0*TCFMAX(I)
            IF(LBMCPR)WRITE(LUNOUT,996)
996         FORMAT(/,5X,' WARNING NULL COLLISION TIME INCREASED',/)
            GO TO 1
       ENDIF
*   Test for real or null collision
       R5=drand48(RDUM)
       TLIM=TCF(IE)/TLIM
       IF(R5.GT.TLIM) THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMST DEBUG   :'',
     -           '' Null collision.'')')
            NNULL=NNULL+1
            GO TO 1
       ENDIF
*** Direction cosines and positions at instant before collision
       T2=T*T
       TDASH=0.0D0
       CONST6=SQRT(E1/E)
       DCX2=DCX1*CONST6+EX*T*CONST5/SQRT(E)
       DCY2=DCY1*CONST6+EY*T*CONST5/SQRT(E)
       DCZ2=DCZ1*CONST6+EZ*T*CONST5/SQRT(E)
*   const7: velocity [m/psec], a: velocity time [m]
       CONST7=CONST9*SQRT(E1)
       A=T*CONST7
       NCOL=NCOL+1
       xold=x
       yold=y
       zold=z
       eold=e1
       X=X+DCX1*A+T2*EX*CONST2
       Y=Y+DCY1*A+T2*EY*CONST2
       Z=Z+DCZ1*A+T2*EZ*CONST2
       ST=ST+T
*** Add new point
       IF(NMC*(NCOL/NMC).EQ.NCOL)THEN
            IF(NU.GE.MXLIST)THEN
                 ISTAT=-2
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMST DEBUG   :'',
     -                '' Too many steps, istat = -2, e_max = '',E15.8,
     -                '' eV, nu = '',I4,'', ncol = '',I5)') EMAX,NU,NCOL
*   Heinrich options.
                 if(lheinrich)close(unit=38)
                 RETURN
            ENDIF
            NU=NU+1
            XU(NU)=X*1.0D2
            YU(NU)=Y*1.0D2
            ZU(NU)=Z*1.0D2
            TU(NU)=ST*1.0D-6
       ENDIF
*** Determination of real collision type
       R2=drand48(RDUM)
*   Find location within 4 units in collision array
       CALL SORT(I,R2,IE)
  140  I=I+1
       IF(CF(IE,I).LT.R2) GO TO 140
       S1=RGAS(I)
       EI=EIN(I)
*   Use flat distribution of  electron energy between E-EION and 0.0 eV
       IF(IPN(I).GT.0)THEN
            R9=drand48(RDUM)
            EXTRA=R9*(E-EI)
            EI=EXTRA+EI
       ENDIF
*** Generate scattering angles, update lab cosines, type of collision.
       IPT=IARRY(I)
       IGAS=1+(IPT-1)/5
       ITYPE=IPT-5*(IGAS-1)
*   NSTATL(IPT): 1=elastic, 2=ionise, 3=attach, 4=inelastic, 5=super
       NSTATL(IPT)=NSTATL(IPT)+1
*   NSTATN(I): individual level described in DSCRPT(I)
       NSTATN(I)=NSTATN(I)+1
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMST DEBUG   : Gas '',I3,
     -      '' type '',I1,'' level '',A)') IGAS,ITYPE,DSCRPT(I)
*   Elastic.
       XPL(1)=X*1.0D2
       YPL(1)=Y*1.0D2
       ZPL(1)=Z*1.0D2
       IF(ITYPE.EQ.1.AND.LPLELA)THEN
            CALL GRATTS('ELASTIC','POLYMARKER')
            CALL PLAGPM(1,XPL,YPL,ZPL)
*   Ionisation.
       ELSEIF(ITYPE.EQ.2.AND.LPLION)THEN
            CALL GRATTS('IONISATION','POLYMARKER')
            CALL PLAGPM(1,XPL,YPL,ZPL)
*   Heinrich update
            if(lheinrich)ioncount=ioncount+1
*   Electron ends due to attachment.
       ELSEIF(ITYPE.EQ.3)THEN
            IF(LPLATT)THEN
                 CALL GRATTS('ATTACHMENT','POLYMARKER')
                 CALL PLAGPM(1,XPL,YPL,ZPL)
            ENDIF
            IF(NU.GE.MXLIST)THEN
                 ISTAT=-2
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMST DEBUG   :'',
     -                '' Too many steps, istat = -2, e_max = '',E15.8,
     -                '' eV, nu = '',I4,'', ncol = '',I5)') EMAX,NU,NCOL
*   Heinrich options.
                if(lheinrich)close(unit=38)
                RETURN
            ENDIF
            IF(ABS(TU(NU)-ST*1.0D-6).GT.1.0D-6)THEN
                 NU=NU+1
                 XU(NU)=X*1.0D2
                 YU(NU)=Y*1.0D2
                 ZU(NU)=Z*1.0D2
                 TU(NU)=ST*1.0D-6
            ENDIF
            ISTAT=-7
*   Heinrich options.
            if(lheinrich)close(unit=38)
            RETURN
*   Inelastic and excitation.
       ELSEIF(ITYPE.EQ.4.AND.(LPLINE.OR.LPLEXC))THEN
            IF(LPLEXC.AND.DSCRPT(I)(1:4).EQ.' EXC')THEN
                 CALL GRATTS('EXCITATION','POLYMARKER')
                 CALL PLAGPM(1,XPL,YPL,ZPL)
            ELSEIF(LPLINE.AND.DSCRPT(I)(1:4).NE.' EXC')THEN
                 CALL GRATTS('INELASTIC','POLYMARKER')
                 CALL PLAGPM(1,XPL,YPL,ZPL)
            ENDIF
*   Super-elastic
       ELSEIF(ITYPE.EQ.5.AND.LPLSUP)THEN
            CALL GRATTS('SUPER-ELASTIC','POLYMARKER')
            CALL PLAGPM(1,XPL,YPL,ZPL)
       ENDIF
*   Call a user procedure to dealing with excitations.
       CALL GASEXU(ITYPE,IGAS,I,
     -      REAL(X*100),REAL(Y*100),REAL(Z*100),
     -      REAL(E),REAL(ST*1.0D-6),
     -      ADDNEW,XNEW,YNEW,ZNEW,DELAY,ENEW)
*   Write a Heinrich record
       if(lheinrich)then
            write(38,'(1x,I5,2X,i5,8(e20.13,2x),a)') itype,ioncount,
     -           x*100,y*100,z*100,e,
     -           xold*100,yold*100,zold*100,eold,dscrpt(i)
       endif
*** Fix energy loss smaller than incident energy if error occurs
       IF(E.LT.EI) THEN
            IF(LBMCPR)WRITE(LUNOUT,994) E,EI,J1
994         FORMAT(2X,' WARNING ENERGY =',F10.5,
     -           ' LESS THAN ENERGY LOSS EI=',F10.5,' AT ITER=',I12,
     -           ' DUE TO BINNING ERROR')
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMST DEBUG   : Gas '',I3,
     -           '' type '',I1,'' level '',A)') IGAS,ITYPE,DSCRPT(I)
            EI=E-0.0001D0
       ENDIF
*   Scatter
       S2=(S1*S1)/(S1-1.0D0)
*   Anisotropic scattering to obtain theta
       IF(INDEX(I).NE.0) THEN
            R31=drand48(RDUM)
            R3=drand48(RDUM)
            F3=1.0D0-R3*ANGCT(IE,I)
            IF(R31.GT.PSCT(IE,I)) F3=-F3
*   Isotropic scattering
       ELSE
            R3=drand48(RDUM)
            F3=1.0D0-2.0D0*R3
       ENDIF
       THETA0=ACOS(F3)
*   Obtain phi
       R4=drand48(RDUM)
       PHI0=F4*R4
*   Work out the new direction
       F8=SIN(PHI0)
       F9=COS(PHI0)
       ARG1=1.0D0-S1*EI/E
       ARG1=MAX(ARG1,SMALL)
       D=1.0D0-F3*SQRT(ARG1)
       E1=E*(1.0D0-EI/(S1*E)-2.0D0*D/S2)
       E1=MAX(E1,SMALL)
       Q=SQRT((E/E1)*ARG1)/S1
       Q=MIN(Q,1.0D0)
       THETA=ASIN(Q*SIN(THETA0))
       F6=COS(THETA)
       U=(S1-1.0D0)*(S1-1.0D0)/ARG1
       CSQD=F3*F3
       IF(F3.LT.0.0D0.AND.CSQD.GT.U) F6=-1.0D0*F6
       F5=SIN(THETA)
       DCZ2=MIN(DCZ2,1.0D0)
       ARGZ=SQRT(DCX2*DCX2+DCY2*DCY2)
       IF(ARGZ.EQ.0.0D0) THEN
            IF(LBMCPR)WRITE(LUNOUT,9232) J1,E1
9232        FORMAT(3X,'WARNING ARGZ= 0.0 AT J1 =',I10,' E1=',E12.3)
            DCZ1=F6
            DCX1=F9*F5
            DCY1=F8*F5
       ELSE
            DCZ1=DCZ2*F6+ARGZ*F5*F8
            DCY1=DCY2*F6+(F5/ARGZ)*(DCX2*F9-DCY2*DCZ2*F8)
            DCX1=DCX2*F6-(F5/ARGZ)*(DCY2*F9+DCX2*DCZ2*F8)
       ENDIF
 210   CONTINUE
*** End of loop.
       PRINT *,' !!!!!! DLCMST WARNING : Reached end of loop, should'//
     -      ' not happen; returning ISTAT = -2.'
       ISTAT=-2
*   Heinrich options.
       if(lheinrich)close(unit=38)
       END
       SUBROUTINE DLCMIA(X1,Y1,Z1,OPTION,EF,ES,DIRX,DIRY,DIRZ,IRCS,IHF,
     -      TOFF,NETOT,NITOT,IFAIL)
*-----------------------------------------------------------------------
*   DLCMIA - Microscopic MC tracking front-end
*            NSTATL(IPT): 1=elastic, 2=ionise, 3=attach, 4=excitation,
*                         5=super-elastic, 6=inelastic
*            NSTATN(I)  : individual level described in DSCRPT(I)
*   (Last changed on 15/12/10.)
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
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
*   Array dimensions.
       integer mxngas
       parameter(mxngas=6)
*-----------------------------------------------------------------------
*   MAGPAR - Interface parameters for gas mixing with Magboltz.
*   (Last changed on  2/ 3/08.)
*-----------------------------------------------------------------------
       INTEGER MXGNAM
       PARAMETER(MXGNAM=60)
       DOUBLE PRECISION FRAMIX
       LOGICAL LF0PLT,LCSPLT,LGKEEP,LBMCPR
       COMMON /MAGPAR/ FRAMIX(MXGNAM),LF0PLT,LCSPLT,LGKEEP,LBMCPR
*   Sometimes IPLAST is called LAST
       DOUBLE PRECISION CF,EIN,TCF,RGAS,WPL
       INTEGER IARRY,IPN,IPLAST,ISIZE
       COMMON/LARGE/CF(2048,512),EIN(512),TCF(2048),IARRY(512),
     -      RGAS(512),IPN(512),WPL(512),IPLAST,ISIZE
       CHARACTER*30 DSCRPT
       COMMON/SCRIP/DSCRPT(512)
*   Changed name of common from /NAMES/ to /MBGNAM/ for Mac OS X
       CHARACTER*15 NAMEG
       COMMON /MBGNAM/ NAMEG(mxngas)
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       LOGICAL FPERX,FPERY,LCROSS,TRASET,TRAFLG,LITAIL,LDTAIL,LRTAIL,
     -      LEPULS,LIPULS,SIGSET,RESSET
       INTEGER NPAIR,ICLUST,NFOUR,MFEXP,MXMIN,MXMAX,
     -      MYMIN,MYMAX,NTRBNK,ITRMAJ,NTIME,NORIA,
     -      NASIMP,JIORD,NISIMP,NMQUAD,NCANG,IENANG
       REAL TIMSIG,SIGNAL,TCLUST,SCLUST,ACLUST,BCLUST,FCLUST,
     -      AVALAN,TSTART,TDEV,PRSTHR,
     -      TRABNK,TRAVEC
       CHARACTER*(MXCHAR) FCNANG
       CHARACTER*12 AVATYP
       CHARACTER*3 FCELTP
       COMMON /SIGDAT/ TIMSIG(MXLIST),SIGNAL(MXLIST,MXSW,2),
     -      AVALAN(2),TRAVEC(MXLIST),
     -      TRABNK(MXLIST,9),TSTART,TDEV,PRSTHR,
     -      TCLUST,SCLUST,ACLUST,BCLUST,FCLUST,ICLUST,NPAIR,
     -      NFOUR,ITRMAJ,JIORD,NISIMP,NMQUAD,IENANG,NTIME,NORIA,
     -      MFEXP,MXMIN,MXMAX,MYMIN,MYMAX,NTRBNK,NASIMP,NCANG,
     -      TRASET,TRAFLG(9),FPERX,FPERY,LCROSS,LITAIL,LDTAIL,LRTAIL,
     -      LEPULS,LIPULS,SIGSET,RESSET
       COMMON /SIGCHR/ FCELTP,AVATYP,FCNANG
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
       REAL X1,Y1,Z1,EF,ES,DIRX,DIRY,DIRZ,DNORM,TOFF
       INTEGER IFAIL1,IFAIL,NSTATL(5*MXNGAS),NSTATN(512),I,IPT,IGAS,
     -      ITYPE,IREF,IHF,ISIZ(1),MATSLT,IRCS,ISCS,NETOT,NITOT,NEMAX
       LOGICAL LPLION,LPLEXC,LPLINE,LPLELA,LPLSUP,LPLATT,LELEPL,LIONPL,
     -      lheinrich,LSIGAD
       CHARACTER*(*) OPTION
       EXTERNAL MATSLT
*** Assume this will fail.
       IFAIL=1
*** Check the energy
       IF(ES.LT.0)THEN
            PRINT *,' !!!!!! DLCMIA WARNING : Starting energy is'//
     -           ' negative; abandoned.'
            NU=0
            ISTAT=-3
            RETURN
       ELSEIF(EF.LE.0)THEN
            PRINT *,' !!!!!! DLCMIA WARNING : Final energy is'//
     -           ' non-positive; abandoned.'
            NU=0
            ISTAT=-3
            RETURN
       ENDIF
*** Initialise the gas
       CALL DLCMII(EF,NSTATL,NSTATN,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! DLCMIA WARNING : Initialisation failed'//
     -           ' ; no calculation performed.'
            RETURN
       ENDIF
*** Decode the options: Magboltz print option.
       LBMCPR=.FALSE.
       IF(INDEX(OPTION,'NOPRINT').NE.0)THEN
            LBMCPR=.FALSE.
       ELSEIF(INDEX(OPTION,'PRINT').NE.0)THEN
            LBMCPR=.TRUE.
       ENDIF
*   Mark ionisations
       LPLION=.FALSE.
       IF(INDEX(OPTION,'NOMARK-ION').NE.0)THEN
            LPLION=.FALSE.
       ELSEIF(INDEX(OPTION,'MARK-ION').NE.0)THEN
            LPLION=.TRUE.
       ENDIF
*   Mark excitations
       LPLEXC=.FALSE.
       IF(INDEX(OPTION,'NOMARK-EXC').NE.0)THEN
            LPLEXC=.FALSE.
       ELSEIF(INDEX(OPTION,'MARK-EXC').NE.0)THEN
            LPLEXC=.TRUE.
       ENDIF
*   Mark inelastic
       LPLINE=.FALSE.
       IF(INDEX(OPTION,'NOMARK-INEL').NE.0)THEN
            LPLINE=.FALSE.
       ELSEIF(INDEX(OPTION,'MARK-INEL').NE.0)THEN
            LPLINE=.TRUE.
       ENDIF
*   Mark elastic
       LPLELA=.FALSE.
       IF(INDEX(OPTION,'NOMARK-ELAS').NE.0)THEN
            LPLELA=.FALSE.
       ELSEIF(INDEX(OPTION,'MARK-ELAS').NE.0)THEN
            LPLELA=.TRUE.
       ENDIF
*   Mark super-elastic
       LPLSUP=.FALSE.
       IF(INDEX(OPTION,'NOMARK-SUP').NE.0)THEN
            LPLSUP=.FALSE.
       ELSEIF(INDEX(OPTION,'MARK-SUP').NE.0)THEN
            LPLSUP=.TRUE.
       ENDIF
*   Mark attachment
       LPLATT=.FALSE.
       IF(INDEX(OPTION,'NOMARK-ATT').NE.0)THEN
            LPLATT=.FALSE.
       ELSEIF(INDEX(OPTION,'MARK-ATT').NE.0)THEN
            LPLATT=.TRUE.
       ENDIF
*   Heinrich options
       lheinrich=.FALSE.
       IF(INDEX(OPTION,'NOHEINRICH').NE.0)THEN
            lheinrich=.FALSE.
       ELSEIF(INDEX(OPTION,'HEINRICH').NE.0)THEN
            lheinrich=.TRUE.
       ENDIF
*   Signal
       LSIGAD=.FALSE.
       IF(INDEX(OPTION,'NOSIGNAL').NE.0)THEN
            LSIGAD=.FALSE.
       ELSEIF(INDEX(OPTION,'SIGNAL').NE.0)THEN
            LSIGAD=.TRUE.
       ENDIF
*** Electron and ion tracking, avalanche limit
       LELEPL=.FALSE.
       LIONPL=.FALSE.
       NEMAX=0
*   Decode the options.
       IF(INDEX(OPTION,'NOPLOT-ELECTRON').NE.0)THEN
            LELEPL=.FALSE.
       ELSEIF(INDEX(OPTION,'PLOT-ELECTRON').NE.0)THEN
            LELEPL=.TRUE.
       ENDIF
       IF(INDEX(OPTION,'NOPLOT-ION').NE.0)THEN
            LIONPL=.FALSE.
       ELSEIF(INDEX(OPTION,'PLOT-ION').NE.0)THEN
            LIONPL=.TRUE.
       ENDIF
       IF(INDEX(OPTION,'ABORT-100000').NE.0)THEN
            NEMAX=100000
       ELSEIF(INDEX(OPTION,'ABORT-10000').NE.0)THEN
            NEMAX=10000
       ELSEIF(INDEX(OPTION,'ABORT-1000').NE.0)THEN
            NEMAX=1000
       ELSEIF(INDEX(OPTION,'ABORT-100').NE.0)THEN
            NEMAX=100
       ELSEIF(INDEX(OPTION,'ABORT-10').NE.0)THEN
            NEMAX=10
       ENDIF
*** Book an histogram for the energy distribution, if needed.
       IF(IHF.LT.0)THEN
            CALL HISADM('ALLOCATE',IHF,100,0.0,EF,.FALSE.,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! DLCMIA WARNING : Unable to obtain'//
     -                ' matrix storage; energy not histogrammed.'
                 IHF=0
            ENDIF
       ENDIF
*** Normalise the initial vector.
       IF(DIRX**2+DIRY**2+DIRZ**2.LE.0)THEN
            PRINT *,' !!!!!! DLCMIA WARNING : Initial direction'//
     -           ' vector has zero norm; no calculation.'
            RETURN
       ELSE
            DNORM=SQRT(DIRX**2+DIRY**2+DIRZ**2)
            DIRX=DIRX/DNORM
            DIRY=DIRY/DNORM
            DIRZ=DIRZ/DNORM
       ENDIF
*** Initialise signal calculations if not yet done.
       IF(LSIGAD.AND..NOT.SIGSET)THEN
            CALL SIGINI(IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! DLCMIA WARNING : Initialisation of'//
     -                ' signal calculation failed; no avalanche.'
                 RETURN
            ENDIF
       ENDIF
*** Step.
       CALL DLCMSA(X1,Y1,Z1,EF,ES,DIRX,DIRY,DIRZ,
     -      NSTATL,NSTATN,IHF,TOFF,NETOT,NITOT,
     -      LPLION,LPLEXC,LPLINE,LPLELA,LPLSUP,LPLATT,
     -      LELEPL,LIONPL,lheinrich,LSIGAD,NEMAX,IFAIL1)
*   Check error condition.
       IF(IFAIL1.NE.0)RETURN
*** Extract the statistics: first initialise the counters.
       IF(IRCS.GT.0)THEN
*   Loop over the levels.
            DO 10 I=1,IPLAST
            IPT=IARRY(I)
            IGAS=1+(IPT-1)/5
            ITYPE=IPT-5*(IGAS-1)
            IF(ITYPE.EQ.4.AND.DSCRPT(I)(1:4).NE.' EXC')ITYPE=6
            CALL GASIDO(IREF,NAMEG(IGAS)//DSCRPT(I),ITYPE,0.0,IFAIL1)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMIA DEBUG   :'',
     -           '' Gas '',I3,'', cs type '',I1,'', cs "'',A,''",'',
     -           '' count '',I6,'', ref '',I3)') IGAS,ITYPE,DSCRPT(I),
     -           NSTATN(I),IREF
            IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCMIA WARNING : Unable'//
     -           ' to store a rate.'
10          CONTINUE
*** Store the matrices: elastic.
            ISIZ(1)=IPLAST
            CALL MATADM('ALLOCATE',IRCS,1,ISIZ,2,IFAIL1)
            ISCS=MATSLT(IRCS)
            DO 20 I=1,IPLAST
            MVEC(MORG(ISCS)+I)=NSTATN(I)
20          CONTINUE
       ENDIF
*** Success
       IFAIL=0
       END
       SUBROUTINE DLCMSA(X1,Y1,Z1,EF,ES,DIRX,DIRY,DIRZ,
     -      NSTATL,NSTATN,IHF,TOFF,NETOT,NITOT,
     -      LPLION,LPLEXC,LPLINE,LPLELA,LPLSUP,LPLATT,
     -      LELEPL,LIONPL,lheinrich,LSIGAD,NEMAX,IFAIL)
*-----------------------------------------------------------------------
*   DLCMSA - Microscopic MC tracking, with avalanche derived from MONTE.
*   (Last changed on  2/ 8/10.)
*-----------------------------------------------------------------------
      implicit none
*   Array dimensions.
       integer mxngas
       parameter(mxngas=6)
*-----------------------------------------------------------------------
*   MAGPAR - Interface parameters for gas mixing with Magboltz.
*   (Last changed on  2/ 3/08.)
*-----------------------------------------------------------------------
       INTEGER MXGNAM
       PARAMETER(MXGNAM=60)
       DOUBLE PRECISION FRAMIX
       LOGICAL LF0PLT,LCSPLT,LGKEEP,LBMCPR
       COMMON /MAGPAR/ FRAMIX(MXGNAM),LF0PLT,LCSPLT,LGKEEP,LBMCPR
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
       INTEGER NGAS,NSTEP,IDBG
       DOUBLE PRECISION EFINAL,ESTEP,AKT,ARY,TEMPC,TORR
       PARAMETER(ARY=13.60569172)
       COMMON/INPT/NGAS,NSTEP,EFINAL,ESTEP,AKT,TEMPC,TORR,IDBG
       DOUBLE PRECISION PIR2,ECHARG,EMASS,AMU,BOLTZ,BOLTZJ,
     -      AWB,ALOSCH,ABZERO,ATMOS
       PARAMETER(PIR2=8.79735534D-17)
       PARAMETER(ECHARG=1.602176462D-19)
       PARAMETER(EMASS=9.10938188D-31)
       PARAMETER(AMU=1.66053873D-27)
       PARAMETER(BOLTZ=8.617342D-5)
       PARAMETER(BOLTZJ=1.3806503D-23)
       PARAMETER(AWB=1.758820174D10)
       PARAMETER(ALOSCH=2.6867775D19)
       PARAMETER(ABZERO=273.15D0)
       PARAMETER(ATMOS=760.0D0)
       DOUBLE PRECISION CONST1,CONST2,CONST3,CONST4,CONST5
       COMMON/CNSTS1/CONST1,CONST2,CONST3,CONST4,CONST5
       DOUBLE PRECISION TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX,RSTART,
     -      EMAG
       INTEGER NMAX
       COMMON/SETP/TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX(8),RSTART,
     -      EMAG,NMAX
*   Sometimes IPLAST is called LAST
       DOUBLE PRECISION CF,EIN,TCF,RGAS,WPL
       INTEGER IARRY,IPN,IPLAST,ISIZE
       COMMON/LARGE/CF(2048,512),EIN(512),TCF(2048),IARRY(512),
     -      RGAS(512),IPN(512),WPL(512),IPLAST,ISIZE
*   Is in effect the old ANCT common.
       DOUBLE PRECISION PSCT,ANGCT
       INTEGER INDEX,NISO
       COMMON/ANIS/PSCT(2048,512),ANGCT(2048,512),INDEX(512),NISO
       CHARACTER*30 DSCRPT
       COMMON/SCRIP/DSCRPT(512)
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       REAL XLIST(MXMCA),YLIST(MXMCA),ZLIST(MXMCA),TLIST(MXMCA),
     -      ELIST(MXMCA),
     -      XELIST(MXMCA),YELIST(MXMCA),ZELIST(MXMCA),TELIST(MXMCA)
       INTEGER NLIST(MXMCA),ISLIST(MXMCA),NMCA
       COMMON /MCAMAT/ XLIST,YLIST,ZLIST,TLIST,ELIST,
     -      XELIST,YELIST,ZELIST,TELIST,NLIST,ISLIST,NMCA
       INTEGER NCOLM
       PARAMETER(NCOLM=1000000)
       DOUBLE PRECISION X,Y,Z,ST,RDUM,E1,E,EX,EY,EZ,
     -      CONST6,CONST7,CONST9,TDASH,TLIM,T,T2,
     -      DCX1,DCY1,DCZ1,DCX2,DCY2,DCZ2,THETA0,PHI0,
     -      AP,BP,A,F3,F4,F5,F6,F8,F9,R1,R2,R3,R4,R5,R9,R31,
     -      S1,S2,EXTRA,EI,D,Q,ARG1,ARGZ,CSQD,DELTAE,U,
     -      DRAND48,EMAX,XPL(1),YPL(1),ZPL(1),ESEC
       REAL X1,Y1,Z1,ES,EF,XS,YS,ZS,EXS,EYS,EZS,ETOTS,VOLTS,
     -      DIRX,DIRY,DIRZ,XNEW,YNEW,ZNEW,DELAY,ENEW,TOFF
C     -      ,exs2,eys2,ezs2,etots2
       INTEGER INTEM,J1,NCOL,NNULL,I,IE,IPT,ILOC,IGAS,ITYPE,NEMAX,
     -      NSTATL(5*MXNGAS),NSTATN(512),IHF,NETOT,NITOT,IMCA,IFAIL1,
     -      IFAIL
       LOGICAL LPLION,LPLEXC,LPLINE,LPLELA,LPLSUP,LPLATT,LELEPL,LIONPL,
     -      lheinrich,LSIGAD,ADDNEW
       EXTERNAL DRAND48
      integer ioncount
*** By default, failure.
       IFAIL=1
C      print *,' Microscopic avalanche'
C      print *,' Position: ',x1,y1,z1
C      print *,' Energy: at start ',es,' eV, maximal ',ef,' eV'
C      print *,' Velocity vector: ',dirx,diry,dirz
*** Initialise the avalanche table.
       NMCA=1
       XLIST(1)=X1/1.0D2
       YLIST(1)=Y1/1.0D2
       ZLIST(1)=Z1/1.0D2
       TLIST(1)=TOFF/1.0D-6
       ELIST(1)=ES
       EMAX=ELIST(1)
       NETOT=1
       NITOT=0
*** Heinrich options.
       if(lheinrich)then
            open(unit=38,file='heinrich.ions')
       endif
*** Loop over the table.
       IMCA=0
100    CONTINUE
       IMCA=IMCA+1
*   Check we are still in the table.
       IF(IMCA.GT.NMCA)THEN
*   Release memory.
            CALL BOOK('RELEASE','MCAMAT','MCA',IFAIL1)
*   This was a success
            IFAIL=0
*   Heinrich options.
            if(lheinrich)close(unit=38)
*   Return
            RETURN
       ENDIF
*** Starting point.
       X=XLIST(IMCA)
       Y=YLIST(IMCA)
       Z=ZLIST(IMCA)
*   Starting time.
       ST=TLIST(IMCA)
*   Starting energy.
       E1=DBLE(ELIST(IMCA))
*** Initialise the steps
       NU=1
       XU(NU)=100.0*X
       YU(NU)=100.0*Y
       ZU(NU)=100.0*Z
       TU(NU)=1.0D-6*ST
*** Set technique and particle labels
       IPTYPE=1
       QPCHAR=-1.0
       IPTECH=4
*** Restart ionisation counter
       ioncount=0
*** Get the electric and magnetic field for the starting point.
       XS=REAL(X*100.0)
       YS=REAL(Y*100.0)
       ZS=REAL(Z*100.0)
       CALL EFIELD(XS,YS,ZS,EXS,EYS,EZS,ETOTS,VOLTS,0,ILOC)
       EX=-DBLE(EXS)
       EY=-DBLE(EYS)
       EZ=-DBLE(EZS)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMSA DEBUG   : At ('',
     -      3E15.8,''), E = ('',3E15.8,''), loc = '',I5,'', nu = '',
     -      I5)') X,Y,Z,EX,EY,EZ,ILOC,NU
*   If this is not in the drift medium, stop now.
       IF(ILOC.EQ.-5.OR.ILOC.EQ.-6)THEN
            ISTAT=ILOC
            XELIST(IMCA)=REAL(XU(NU))/100.0
            YELIST(IMCA)=REAL(YU(NU))/100.0
            ZELIST(IMCA)=REAL(ZU(NU))/100.0
            TELIST(IMCA)=REAL(TU(NU))*1.0E6
            ISLIST(IMCA)=ISTAT
*   Add the signal.
            IF(LSIGAD)CALL SIGADM(0.0, .TRUE., 1.0, IFAIL)
            GOTO 100
       ENDIF
*   Start status checking.
       CALL DLCSTA(-1.0,1)
*   Check that we are inside the area, terminate if not.
       IF(ISTAT.NE.0.OR.ILOC.NE.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMSA DEBUG   :'',
     -           '' loc = '',I5,'', istat = '',I5,'', e_max = '',E15.8,
     -           '' eV, nu = '',I4,'', ncol = '',I5)')
     -           ILOC,ISTAT,EMAX,NU,NCOL
*   End of drift line processing.
            XELIST(IMCA)=REAL(XU(NU))/100.0
            YELIST(IMCA)=REAL(YU(NU))/100.0
            ZELIST(IMCA)=REAL(ZU(NU))/100.0
            TELIST(IMCA)=REAL(TU(NU))*1.0E6
            ISLIST(IMCA)=ISTAT
*   Add the signal.
            IF(LSIGAD)CALL SIGADM(0.0, .TRUE., 1.0, IFAIL)
            GOTO 100
       ENDIF
*** Various parameters.
       SMALL=1.0D-20
       RDUM=RSTART
       CONST9=CONST3*0.01D0
       NCOL=0
       NNULL=0
*   Number of collisions for de-correlation ?
       TDASH=0.0D0
*   Parameters to compute position after step
       INTEM=8
       F4=2.0D0*ACOS(-1.0D0)
       DELTAE=EFINAL/DBLE(INTEM)
*** Initial direction cosines: first electron is specified
       IF(IMCA.EQ.1)THEN
            DCX1=DBLE(DIRX)
            DCY1=DBLE(DIRY)
            DCZ1=DBLE(DIRZ)
*   The secondary electrons are isotropic
       ELSE
            THETA0=ACOS(MIN(1.0,MAX(-1.0,1.0-2.0*DRAND48(RDUM))))
            PHI0=F4*DRAND48(RDUM)
            DCX1=COS(PHI0)*SIN(THETA0)
            DCY1=SIN(PHI0)*SIN(THETA0)
            DCZ1=COS(THETA0)
       ENDIF
*** Main loop
       DO 210 J1=1,100000000
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMSA DEBUG   : Step '',
     -      I3,'', iteration '',I5,'', collision '',I5)') NU,J1,NCOL
*** Get the electric and magnetic field (check only).
C       xs=real(x*100.0)
C       ys=real(y*100.0)
C       zs=real(z*100.0)
C       call efield(xs,ys,zs,exs2,eys2,ezs2,etots2,volts,0,iloc)
C       if(abs(exs-exs2).gt.1.0e-5*(1+abs(exs)+abs(exs2)).or.
C     -    abs(eys-eys2).gt.1.0e-5*(1+abs(eys)+abs(eys2)).or.
C     -    abs(ezs-ezs2).gt.1.0e-5*(1+abs(ezs)+abs(ezs2)).or.
C     -    abs(etots-etots2).gt.1.0e-5*(1+abs(etots)+abs(etots2)))then
C            print *,' Step ',j1,' field clash:'
C            print *,'             e = ',exs2,eys2,ezs2,etots2
C            print *,'             o = ',exs,eys,ezs,etots
C       endif
*** Determine free time
    1  R1=drand48(RDUM)
       I=INT(E1/DELTAE)+1
       I=MIN(I,INTEM)
       TLIM=TCFMAX(I)
       T=-LOG(R1)/TLIM+TDASH
       t = dtfact*t
       TDASH=T
       AP=CONST3*(DCX1*EX+DCY1*EY+DCZ1*EZ)*SQRT(E1)
       BP=(EX**2+EY**2+EZ**2)*CONST1
       E=E1+(AP+BP*T)*T
*   Keep track of the highest energy
       EMAX=MAX(E,EMAX)
       IF(E.GT.EFINAL)THEN
            PRINT *,' !!!!!! DLCMSA WARNING : Instantaneous electron'//
     -           ' energy ',E,' eV exceeds E_maximum ',EFINAL,' eV;'//
     -           ' avalanche tracking stopped.'
C            print *,' E1 = ',E1,', AP = ',AP,', BP = ',BP,', T = ',T
*   Too many points.
            IF(NU.GE.MXLIST)THEN
                 ISTAT=-2
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMSA DEBUG   :'',
     -                '' Too many steps, istat = -2, e_max = '',E15.8,
     -                '' eV, nu = '',I4,'', ncol = '',I5)') EMAX,NU,NCOL
                 IF(LELEPL)CALL DLCPLT
                 XELIST(IMCA)=X
                 YELIST(IMCA)=Y
                 ZELIST(IMCA)=Z
                 TELIST(IMCA)=ST
                 ISLIST(IMCA)=ISTAT
*   Add the signal.
                 IF(LSIGAD)CALL SIGADM(0.0, .TRUE., 1.0, IFAIL)
*   Heinrich options.
                 if(lheinrich)close(unit=38)
                 RETURN
            ENDIF
*   Add the point.
            NU=NU+1
            XU(NU)=X*1.0D2
            YU(NU)=Y*1.0D2
            ZU(NU)=Z*1.0D2
            TU(NU)=ST*1.0D-6
*   Record the problem.
            ISTAT=-9
*   Plot it.
            IF(LELEPL)CALL DLCPLT
*   Record the end point.
            XELIST(IMCA)=REAL(XU(NU))/100.0
            YELIST(IMCA)=REAL(YU(NU))/100.0
            ZELIST(IMCA)=REAL(ZU(NU))/100.0
            TELIST(IMCA)=REAL(TU(NU))*1.0E6
            ISLIST(IMCA)=ISTAT
*   Add the signal.
            IF(LSIGAD)CALL SIGADM(0.0, .TRUE., 1.0, IFAIL)
*   Heinrich options.
            if(lheinrich)close(unit=38)
            RETURN
       ENDIF
       IF(IHF.GT.0)CALL HISENT(IHF,REAL(E),1.0)
*   Check null collisions
       IE=INT(E/ESTEP)+1
       IE=MIN(IE,2048)
       IF(TCF(IE).GT.TLIM) THEN
            TDASH=TDASH+LOG(R1)/TLIM
            TCFMAX(I)=1.05D0*TCFMAX(I)
            IF(LBMCPR)WRITE(LUNOUT,996)
996         FORMAT(/,5X,' WARNING NULL COLLISION TIME INCREASED',/)
            GO TO 1
       ENDIF
*   Test for real or null collision
       R5=drand48(RDUM)
       TLIM=TCF(IE)/TLIM
       IF(R5.GT.TLIM) THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMSA DEBUG   :'',
     -           '' Null collision.'')')
            NNULL=NNULL+1
            GO TO 1
       ENDIF
*** Direction cosines and positions at instant before collision
       T2=T*T
       TDASH=0.0D0
       CONST6=SQRT(E1/E)
       DCX2=DCX1*CONST6+EX*T*CONST5/SQRT(E)
       DCY2=DCY1*CONST6+EY*T*CONST5/SQRT(E)
       DCZ2=DCZ1*CONST6+EZ*T*CONST5/SQRT(E)
*   const7: velocity [m/psec], a: velocity time [m]
       CONST7=CONST9*SQRT(E1)
       A=T*CONST7
       NCOL=NCOL+1
       X=X+DCX1*A+T2*EX*CONST2
       Y=Y+DCY1*A+T2*EY*CONST2
       Z=Z+DCZ1*A+T2*EZ*CONST2
       ST=ST+T
*** Check the location: first compute the E field.
       XS=REAL(X*100.0)
       YS=REAL(Y*100.0)
       ZS=REAL(Z*100.0)
       CALL EFIELD(XS,YS,ZS,EXS,EYS,EZS,ETOTS,VOLTS,0,ILOC)
       EX=-DBLE(EXS)
       EY=-DBLE(EYS)
       EZ=-DBLE(EZS)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMSA DEBUG   : At ('',
     -      3E15.8,''), E = ('',3E15.8,''), loc = '',I5,'', nu = '',
     -      I5)') X,Y,Z,EX,EY,EZ,ILOC,NU
*   Then verify status and position wrt box.
       IF(ILOC.NE.0.OR.
     -      XS.LT.DDXMIN.OR.XS.GT.DDXMAX.OR.
     -      YS.LT.DDYMIN.OR.YS.GT.DDYMAX.OR.
     -      ZS.LT.DDZMIN.OR.ZS.GT.DDZMAX)THEN
            IF(NU.GE.MXLIST)THEN
                 ISTAT=-2
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMSA DEBUG   :'',
     -                '' Too many steps, istat = -2, e_max = '',E15.8,
     -                '' eV, nu = '',I4,'', ncol = '',I5)') EMAX,NU,NCOL
                 IF(LELEPL)CALL DLCPLT
                 XELIST(IMCA)=X
                 YELIST(IMCA)=Y
                 ZELIST(IMCA)=Z
                 TELIST(IMCA)=ST
                 ISLIST(IMCA)=ISTAT
*   Add the signal.
                 IF(LSIGAD)CALL SIGADM(0.0, .TRUE., 1.0, IFAIL)
                 GOTO 100
            ENDIF
            IF(ILOC.EQ.-5.OR.ILOC.EQ.-6)THEN
                 CALL DLCFMP(XU(NU),YU(NU),ZU(NU),
     -                X*1.0D2,Y*1.0D2,Z*1.0D2,
     -                T*1.0D-6,ILOC,QPCHAR,IPTECH)
                 ISTAT=ILOC
            ELSE
                 NU=NU+1
                 XU(NU)=X*1.0D2
                 YU(NU)=Y*1.0D2
                 ZU(NU)=Z*1.0D2
                 TU(NU)=ST*1.0D-6
                 CALL DLCSTA(-1.0,1)
            ENDIF
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMSA DEBUG   :'',
     -           '' loc = '',I5,'', istat = '',I5,'', e_max = '',E15.8,
     -           '' eV, nu = '',I4,'', ncol = '',I5)')
     -           ILOC,ISTAT,EMAX,NU,NCOL
            IF(LELEPL)CALL DLCPLT
            XELIST(IMCA)=REAL(XU(NU))/100.0
            YELIST(IMCA)=REAL(YU(NU))/100.0
            ZELIST(IMCA)=REAL(ZU(NU))/100.0
            TELIST(IMCA)=REAL(TU(NU))*1.0E6
            ISLIST(IMCA)=ISTAT
*   Add the signal.
            IF(LSIGAD)CALL SIGADM(0.0, .TRUE., 1.0, IFAIL)
            GOTO 100
       ENDIF
*** Add the new point.
       IF(NMC*(NCOL/NMC).EQ.NCOL)THEN
            IF(NU.GE.MXLIST)THEN
                 ISTAT=-2
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMSA DEBUG   :'',
     -                '' Too many steps, istat = -2, e_max = '',E15.8,
     -                '' eV, nu = '',I4,'', ncol = '',I5)') EMAX,NU,NCOL
                 IF(LELEPL)CALL DLCPLT
                 XELIST(IMCA)=X
                 YELIST(IMCA)=Y
                 ZELIST(IMCA)=Z
                 TELIST(IMCA)=ST
                 ISLIST(IMCA)=ISTAT
*   Add the signal.
                 IF(LSIGAD)CALL SIGADM(0.0, .TRUE., 1.0, IFAIL)
                 GOTO 100
            ENDIF
            NU=NU+1
            XU(NU)=X*1.0D2
            YU(NU)=Y*1.0D2
            ZU(NU)=Z*1.0D2
            TU(NU)=ST*1.0D-6
       ENDIF
*** Determination of real collision type
       R2=drand48(RDUM)
*   Find location within 4 units in collision array
       CALL SORT(I,R2,IE)
  140  I=I+1
       IF(CF(IE,I).LT.R2) GO TO 140
       S1=RGAS(I)
       EI=EIN(I)
*   Use flat distribution of  electron energy between E-EION and 0.0 eV
       IF(IPN(I).GT.0)THEN
            R9=drand48(RDUM)
            EXTRA=R9*(E-EI)
            EI=EXTRA+EI
       ENDIF
*** Generate scattering angles, update lab cosines, type of collision.
       IPT=IARRY(I)
       IGAS=1+(IPT-1)/5
       ITYPE=IPT-5*(IGAS-1)
*   NSTATL(IPT): 1=elastic, 2=ionise, 3=attach, 4=inelastic, 5=super
       NSTATL(IPT)=NSTATL(IPT)+1
*   NSTATN(I): individual level described in DSCRPT(I)
       NSTATN(I)=NSTATN(I)+1
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMSA DEBUG   : Gas '',I3,
     -      '' type '',I1,'' level '',A)') IGAS,ITYPE,DSCRPT(I)
**  Common
       XPL(1)=X*1.0D2
       YPL(1)=Y*1.0D2
       ZPL(1)=Z*1.0D2
**  Elastic.
       IF(ITYPE.EQ.1.AND.LPLELA)THEN
            CALL GRATTS('ELASTIC','POLYMARKER')
            CALL PLAGPM(1,XPL,YPL,ZPL)
**  Ionisation.
       ELSEIF(ITYPE.EQ.2)THEN
*   Plot if asked.
            IF(LPLION)THEN
                 CALL GRATTS('IONISATION','POLYMARKER')
                 CALL PLAGPM(1,XPL,YPL,ZPL)
            ENDIF
*   Use OPAL Peterson and Beaty splitting factor.
            R9=drand48(RDUM)
C           ESEC=R9*(E-EI)
            ESEC=WPL(I)*TAN(R9*ATAN((E-EI)/(2.0D0*WPL(I))))
            EI=ESEC+EI
*   Ensure the energy is not negative
            IF(ESEC.LT.0)THEN
                 PRINT *,' !!!!!! DLCMSA WARNING : Secondary'//
     -                ' electron energy ',ESEC,' eV < 0; set'//
     -                ' to "small".'
                 ESEC=SMALL
            ENDIF
*   Ensure we do not pass the maximum permitted avalanche size.
            IF(NMCA+1.GT.NEMAX.AND.NEMAX.GT.0)THEN
                 PRINT *,' !!!!!! DLCMSA WARNING : Avalanche exceeds'//
     -                ' maximum permitted size; avalanche ended.'
                 CALL BOOK('RELEASE','MCAMAT','MCA',IFAIL1)
*   Heinrich options.
                 if(lheinrich)close(unit=38)
                 RETURN
*   Ensure there is still space in the table.
            ELSEIF(NMCA+1.GT.MXMCA)THEN
                 PRINT *,' !!!!!! DLCMSA WARNING : Overflow of'//
     -                ' secondary electron table; avalanche ended.'
                 CALL BOOK('RELEASE','MCAMAT','MCA',IFAIL1)
*   Heinrich options.
                 if(lheinrich)close(unit=38)
                 RETURN
            ENDIF
*   Add the point to the table,
            NMCA=NMCA+1
*   Store position, energy and time
            XLIST(NMCA)=X
            YLIST(NMCA)=Y
            ZLIST(NMCA)=Z
            TLIST(NMCA)=ST
            ELIST(NMCA)=ESEC
*   Increment the electron counter
            NETOT=NETOT+1
            NITOT=NITOT+1
*   Write a Heinrich record
            if(lheinrich)then
                 ioncount=ioncount+1
                 write(38,'(1x,i5,2x,i5,8(2x,e12.5))')
     -                itype,ioncount,
     -                xlist(imca),ylist(imca),zlist(imca),elist(imca),
     -                xlist(nmca),ylist(nmca),zlist(nmca),elist(nmca)
            endif
**  Electron ends due to attachment.
       ELSEIF(ITYPE.EQ.3)THEN
*   Plot position if requested.
            IF(LPLATT)THEN
                 CALL GRATTS('ATTACHMENT','POLYMARKER')
                 CALL PLAGPM(1,XPL,YPL,ZPL)
            ENDIF
*   See whether we still have room.
            IF(NU.GE.MXLIST)THEN
                 ISTAT=-2
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMSA DEBUG   :'',
     -                '' Too many steps, istat = -2, e_max = '',E15.8,
     -                '' eV, nu = '',I4,'', ncol = '',I5)') EMAX,NU,NCOL
                 IF(LELEPL)CALL DLCPLT
                 XELIST(IMCA)=X
                 YELIST(IMCA)=Y
                 ZELIST(IMCA)=Z
                 TELIST(IMCA)=ST
                 ISLIST(IMCA)=ISTAT
*   Add the signal.
                 IF(LSIGAD)CALL SIGADM(0.0, .TRUE., 1.0, IFAIL)
                 GOTO 100
            ENDIF
*   Add point if we still have room.
            NU=NU+1
            XU(NU)=X*1.0D2
            YU(NU)=Y*1.0D2
            ZU(NU)=Z*1.0D2
            TU(NU)=ST*1.0D-6
            ISTAT=-7
*   Decrement the electron counter
            NETOT=NETOT-1
*   Plot and take next electron.
            IF(LELEPL)CALL DLCPLT
            XELIST(IMCA)=REAL(XU(NU))/100.0
            YELIST(IMCA)=REAL(YU(NU))/100.0
            ZELIST(IMCA)=REAL(ZU(NU))/100.0
            TELIST(IMCA)=REAL(TU(NU))*1.0E6
            ISLIST(IMCA)=ISTAT
*   Add the signal.
            IF(LSIGAD)CALL SIGADM(0.0, .TRUE., 1.0, IFAIL)
            GOTO 100
**  Inelastic and excitation.
       ELSEIF(ITYPE.EQ.4)THEN
*   Plot excitations.
            IF(LPLEXC.AND.DSCRPT(I)(1:4).EQ.' EXC')THEN
                 CALL GRATTS('EXCITATION','POLYMARKER')
                 CALL PLAGPM(1,XPL,YPL,ZPL)
*   Plot other inelastic states.
            ELSEIF(LPLINE.AND.DSCRPT(I)(1:4).NE.' EXC')THEN
                 CALL GRATTS('INELASTIC','POLYMARKER')
                 CALL PLAGPM(1,XPL,YPL,ZPL)
            ENDIF
*   Call a user procedure to dealing with excitations.
            CALL GASEXU(ITYPE,IGAS,I,
     -           REAL(X*100),REAL(Y*100),REAL(Z*100),
     -           REAL(E),REAL(ST*1.0D-6),
     -           ADDNEW,XNEW,YNEW,ZNEW,DELAY,ENEW)
            IF(ADDNEW)THEN
*   Ensure we do not pass the maximum permitted avalanche size.
                 IF(NMCA+1.GT.NEMAX.AND.NEMAX.GT.0)THEN
                      PRINT *,' !!!!!! DLCMSA WARNING : Avalanche'//
     -                     ' exceeds maximum permitted size;'//
     -                     ' avalanche ended.'
                      CALL BOOK('RELEASE','MCAMAT','MCA',IFAIL1)
                      RETURN
*   Ensure there is still space in the table.
                 ELSEIF(NMCA+1.GT.MXMCA)THEN
                      PRINT *,' !!!!!! DLCMSA WARNING : Overflow of'//
     -                     ' secondary electron table; avalanche ended.'
                      CALL BOOK('RELEASE','MCAMAT','MCA',IFAIL1)
                      RETURN
                 ENDIF
*   Add the point to the table,
                 NMCA=NMCA+1
*   Store position, energy and time
                 XLIST(NMCA)=XNEW/100.0
                 YLIST(NMCA)=YNEW/100.0
                 ZLIST(NMCA)=ZNEW/100.0
                 TLIST(NMCA)=ST+DELAY/1.0D-6
                 ELIST(NMCA)=ENEW
C       print *,' Adding an electron at ',xnew,ynew,znew,
C     -      ' with delay ',delay,' energy: ',enew
*   Increment the electron counter
                 NETOT=NETOT+1
                 NITOT=NITOT+1
            ENDIF
*   Write a Heinrich record
       if(lheinrich)then
            write(38,'(1x,I5,2X,i5,5(2x,e20.13),2x,a)') itype,ioncount,
     -           x*100,y*100,z*100,e,st*1.0d-6,dscrpt(i)
       endif
**  Super-elastic
       ELSEIF(ITYPE.EQ.5.AND.LPLSUP)THEN
            CALL GRATTS('SUPER-ELASTIC','POLYMARKER')
            CALL PLAGPM(1,XPL,YPL,ZPL)
       ENDIF
*** Fix energy loss smaller than incident energy if error occurs
       IF(E.LT.EI) THEN
            IF(LBMCPR)WRITE(LUNOUT,994) E,EI,J1
994         FORMAT(2X,' WARNING ENERGY =',F8.3,
     -           ' LESS THAN ENERGY LOSS EI=',F8.3,' AT ITER=',I12,
     -           ' DUE TO BINNING ERROR')
            EI=E-0.0001D0
       ENDIF
*   Scatter
       S2=(S1*S1)/(S1-1.0D0)
*   Anisotropic scattering to obtain theta
       IF(INDEX(I).NE.0) THEN
            R31=drand48(RDUM)
            R3=drand48(RDUM)
            F3=1.0D0-R3*ANGCT(IE,I)
            IF(R31.GT.PSCT(IE,I)) F3=-F3
*   Isotropic scattering
       ELSE
            R3=drand48(RDUM)
            F3=1.0D0-2.0D0*R3
       ENDIF
       THETA0=ACOS(F3)
*   Obtain phi
       R4=drand48(RDUM)
       PHI0=F4*R4
*   Work out the new direction
       F8=SIN(PHI0)
       F9=COS(PHI0)
       ARG1=1.0D0-S1*EI/E
       ARG1=MAX(ARG1,SMALL)
       D=1.0D0-F3*SQRT(ARG1)
       E1=E*(1.0D0-EI/(S1*E)-2.0D0*D/S2)
       E1=MAX(E1,SMALL)
       Q=SQRT((E/E1)*ARG1)/S1
       Q=MIN(Q,1.0D0)
       THETA=ASIN(Q*SIN(THETA0))
       F6=COS(THETA)
       U=(S1-1.0D0)*(S1-1.0D0)/ARG1
       CSQD=F3*F3
       IF(F3.LT.0.0D0.AND.CSQD.GT.U) F6=-1.0D0*F6
       F5=SIN(THETA)
       DCZ2=MIN(DCZ2,1.0D0)
       ARGZ=SQRT(DCX2*DCX2+DCY2*DCY2)
       IF(ARGZ.EQ.0.0D0) THEN
            IF(LBMCPR)WRITE(LUNOUT,9232) J1,E1
9232        FORMAT(3X,'WARNING ARGZ= 0.0 AT J1 =',I10,' E1=',E12.3)
            DCZ1=F6
            DCX1=F9*F5
            DCY1=F8*F5
       ELSE
            DCZ1=DCZ2*F6+ARGZ*F5*F8
            DCY1=DCY2*F6+(F5/ARGZ)*(DCX2*F9-DCY2*DCZ2*F8)
            DCX1=DCX2*F6-(F5/ARGZ)*(DCY2*F9+DCX2*DCZ2*F8)
       ENDIF
  210  CONTINUE
*** Should not reach this point.
       PRINT *,' !!!!!! DLCMSA WARNING : Should not reach end of'//
     -      ' loop - please report.'
       IFAIL=0
       END
      SUBROUTINE DLCMIR(X1,Y1,Z1,ES,EF,NSTATL,NSTATN)
*-----------------------------------------------------------------------
*   DLCMIR - Microscopic MC tracking, minimal variation on MONTE
*   (Last changed on 25/ 3/08.)
*-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
*   Array dimensions.
       integer mxngas
       parameter(mxngas=6)
       INTEGER NGAS,NSTEP,IDBG
       DOUBLE PRECISION EFINAL,ESTEP,AKT,ARY,TEMPC,TORR
       PARAMETER(ARY=13.60569172)
       COMMON/INPT/NGAS,NSTEP,EFINAL,ESTEP,AKT,TEMPC,TORR,IDBG
       DOUBLE PRECISION PIR2,ECHARG,EMASS,AMU,BOLTZ,BOLTZJ,
     -      AWB,ALOSCH,ABZERO,ATMOS
       PARAMETER(PIR2=8.79735534D-17)
       PARAMETER(ECHARG=1.602176462D-19)
       PARAMETER(EMASS=9.10938188D-31)
       PARAMETER(AMU=1.66053873D-27)
       PARAMETER(BOLTZ=8.617342D-5)
       PARAMETER(BOLTZJ=1.3806503D-23)
       PARAMETER(AWB=1.758820174D10)
       PARAMETER(ALOSCH=2.6867775D19)
       PARAMETER(ABZERO=273.15D0)
       PARAMETER(ATMOS=760.0D0)
       DOUBLE PRECISION CONST1,CONST2,CONST3,CONST4,CONST5
       COMMON/CNSTS1/CONST1,CONST2,CONST3,CONST4,CONST5
       DOUBLE PRECISION TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX,RSTART,
     -      EMAG
       INTEGER NMAX
       COMMON/SETP/TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX(8),RSTART,
     -      EMAG,NMAX
*   Sometimes IPLAST is called LAST
       DOUBLE PRECISION CF,EIN,TCF,RGAS,WPL
       INTEGER IARRY,IPN,IPLAST,ISIZE
       COMMON/LARGE/CF(2048,512),EIN(512),TCF(2048),IARRY(512),
     -      RGAS(512),IPN(512),WPL(512),IPLAST,ISIZE
*   Is in effect the old ANCT common.
       DOUBLE PRECISION PSCT,ANGCT
       INTEGER INDEX,NISO
       COMMON/ANIS/PSCT(2048,512),ANGCT(2048,512),INDEX(512),NISO
       DOUBLE PRECISION ALPHA,ATT
       COMMON /CTOWNS/ ALPHA,ATT
       DOUBLE PRECISION ALPER,ATTER
       COMMON /CTWNER/ ALPER,ATTER
       DOUBLE PRECISION DXXER,DYYER,DZZER,DYZER,DXYER,DXZER
       COMMON /DIFERB/ DXXER,DYYER,DZZER,DYZER,DXYER,DXZER
       DOUBLE PRECISION DFLER,DFTER
       COMMON /DIFERL/ DFLER,DFTER
       DOUBLE PRECISION DIFXX,DIFYY,DIFZZ,DIFYZ,DIFXY,DIFXZ
       COMMON /DIFLAB/ DIFXX,DIFYY,DIFZZ,DIFYZ,DIFXY,DIFXZ
       DOUBLE PRECISION DIFLN,DIFTR
       COMMON /DIFVEL/ DIFLN,DIFTR
       DOUBLE PRECISION WX,WY,WZ
       COMMON /VEL/ WX,WY,WZ
       DOUBLE PRECISION DWX,DWY,DWZ
       COMMON /VELERR/ DWX,DWY,DWZ
       DOUBLE PRECISION CON
       INTEGER ITHRM
       COMMON /THRM/ CON,ITHRM
*   Adjusted size of ICOLL
       DOUBLE PRECISION TIME,SPEC,TMAX1,AVE,DEN,XID,X,Y,Z,ST
       INTEGER ICOLL,NNULL,ICOLN
       COMMON/OUTPT/TIME(300),ICOLL(5*mxngas),SPEC(2048),TMAX1,
     -      AVE,DEN,XID,X,Y,Z,ST,NNULL,ICOLN(512)
*-----------------------------------------------------------------------
*   MAGPAR - Interface parameters for gas mixing with Magboltz.
*   (Last changed on  2/ 3/08.)
*-----------------------------------------------------------------------
       INTEGER MXGNAM
       PARAMETER(MXGNAM=60)
       DOUBLE PRECISION FRAMIX
       LOGICAL LF0PLT,LCSPLT,LGKEEP,LBMCPR
       COMMON /MAGPAR/ FRAMIX(MXGNAM),LF0PLT,LCSPLT,LGKEEP,LBMCPR
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
      REAL QCHARGE
      DIMENSION XST(100000),YST(100000),ZST(100000),STO(100000)
      DIMENSION WZST(10),AVEST(10)
      DIMENSION DFZZST(10),DFYYST(10),DFXXST(10)
      real X1,Y1,Z1,ES,EF
      integer ifail,NSTATL(5*MXNGAS),NSTATN(512)
*** Initialise the gas
       call dlcmii(ef,NSTATL,NSTATN,ifail)
       lbmcpr=.true.
*** Initialise the steps
       NU=1
       XU(NU)=X1
       YU(NU)=Y1
       ZU(NU)=Z1
       TU(NU)=0
*** Set technique and particle labels
       IPTYPE=1
       QPCHAR=QCHARGE
       IPTECH=4
C -------------------------------------------------------------------
C   CALCULATES COLLISION EVENTS AND UPDATES DIFFUSION AND VELOCITY.
C   USED WITH MAGNETIC FIELD B =0.0   ELECTRIC FIELD IN Z DIRECTION.
C -------------------------------------------------------------------
      WX=0.0D0
      WY=0.0D0
      DWX=0.0D0
      DWY=0.0D0
      DIFYZ=0.0D0
      DIFXY=0.0D0
      DIFXZ=0.0D0
      DYZER=0.0D0
      DXYER=0.0D0
      DXZER=0.0D0
      X=x1
      Y=y1
      Z=z1
      ST=0.0D0
      ST1=0.0D0
      ST2=0.0D0
      SUME2=0.0D0
      SUMXX=0.0D0
      SUMYY=0.0D0
      SUMZZ=0.0D0
      SUMVX=0.0D0
      SUMVY=0.0D0
      ZOLD=0.0D0
      STOLD=0.0D0
      ST1OLD=0.0D0
      ST2OLD=0.0D0
      SZZOLD=0.0D0
      SXXOLD=0.0D0
      SYYOLD=0.0D0
      SVXOLD=0.0D0
      SVYOLD=0.0D0
      SME2OLD=0.0D0
      SMALL=1.0D-20
      TMAX1=0.0D0
      RDUM=RSTART
      E1=ESTART
      CONST9=CONST3*0.01D0
      ARAT=EMASS/AMU
      INTEM=8
      ITMAX=10
      ID=0
      NCOL=0
      NNULL=0
C  NUMBER OF COLLISIONS FOR DE-CORRELATION
      NCOLM=100000
      TDASH=0.0D0
C
C     INITIAL DIRECTION COSINES
C
      DCZ1=COS(THETA)
      DCX1=SIN(THETA)*COS(PHI)
      DCY1=SIN(THETA)*SIN(PHI)
C
      BP=EMAG*EMAG*CONST1
      F1=EMAG*CONST2
      F2=EMAG*CONST3
      F4=2.0D0*ACOS(-1.0D0)
      DELTAE=EFINAL/DBLE(INTEM)
      J2M=NMAX/ITMAX
C MAIN LOOP
      DO 210 J1=1,1
      DO 133 J2=1,10000000
    1 R1=drand48(RDUM)
      I=INT(E1/DELTAE)+1
      I=MIN(I,INTEM)
      TLIM=TCFMAX(I)
      T=-LOG(R1)/TLIM+TDASH
      TDASH=T
      AP=DCZ1*F2*SQRT(E1)
      E=E1+(AP+BP*T)*T
      IE=INT(E/ESTEP)+1
      IE=MIN(IE,2048)
      IF(TCF(IE).GT.TLIM) THEN
       TDASH=TDASH+LOG(R1)/TLIM
       TCFMAX(I)=1.05D0*TCFMAX(I)
      if(lbmcpr)WRITE(lunout,996)
996   FORMAT(/,5X,' WARNING NULL COLLISION TIME INCREASED',/)
       GO TO 1
      ENDIF
C
C     TEST FOR REAL OR NULL COLLISION
C
      R5=drand48(RDUM)
      TLIM=TCF(IE)/TLIM
      IF(R5.GT.TLIM) THEN
       NNULL=NNULL+1
       GO TO 1
      ENDIF
C
C  CALCULATE DIRECTION COSINES AND POSITIONS AT INSTANT BEFORE COLLISION
C    ALSO UPDATE DIFFUSION  AND ENERGY CALCULATIONS.
      T2=T*T
      IF(T.GE.TMAX1) TMAX1=T
      TDASH=0.0D0
      CONST6=SQRT(E1/E)
      DCX2=DCX1*CONST6
      DCY2=DCY1*CONST6
      DCZ2=DCZ1*CONST6+EMAG*T*CONST5/SQRT(E)
      A=AP*T
      B=BP*T2
      SUME2=SUME2+T*(E1+A/2.0D0+B/3.0D0)
      CONST7=CONST9*SQRT(E1)
      A=T*CONST7
      NCOL=NCOL+1
      CX1=DCX1*CONST7
      CY1=DCY1*CONST7
      CZ1=DCZ1*CONST7
      X=X+DCX1*A
      Y=Y+DCY1*A
      Z=Z+DCZ1*A+T2*F1
      ST=ST+T
      IT=INT(T+1.0)
      IT=MIN(IT,300)
      TIME(IT)=TIME(IT)+1.0D0
      SPEC(IE)=SPEC(IE)+1.0D0
      WZ=Z/ST
      SUMVX=SUMVX+CX1*CX1*T2
      SUMVY=SUMVY+CY1*CY1*T2
      IF(ID.EQ.0) GO TO 121
      KDUM=0
      DO 120 JDUM=1,4
      ST2=ST2+T
      NCOLDM=NCOL+KDUM
      IF(NCOLDM.GT.NCOLM) NCOLDM=NCOLDM-NCOLM
      SDIF=ST-STO(NCOLDM)
      SUMXX=SUMXX+((X-XST(NCOLDM))**2)*T/SDIF
      SUMYY=SUMYY+((Y-YST(NCOLDM))**2)*T/SDIF
      IF(J1.LT.3) GO TO 120
      ST1=ST1+T
      SUMZZ=SUMZZ+((Z-ZST(NCOLDM)-WZ*SDIF)**2)*T/SDIF
  120 KDUM=KDUM+12500
  121 continue
C      XST(NCOL)=X
C      YST(NCOL)=Y
C      ZST(NCOL)=Z
C      STO(NCOL)=ST
C      IF(NCOL.GE.NCOLM) THEN
C       ID=ID+1
C       XID=DBLE(ID)
C       NCOL=0
C      ENDIF
*** Add new point
       IF(Z*1.0D2.GT.1)THEN
            IF(NU.GE.MXLIST)THEN
                 ISTAT=-2
                 RETURN
            ENDIF
            NU=NU+1
            XU(NU)=X*1.0D2
            YU(NU)=Y*1.0D2
            ZU(NU)=Z*1.0D2
            TU(NU)=ST*1.0D-6
            ISTAT=-1
            RETURN
       ELSEIF(NMC*(NCOL/NMC).EQ.NCOL)THEN
            IF(NU.GE.MXLIST)THEN
                 ISTAT=-2
                 RETURN
            ENDIF
            NU=NU+1
            XU(NU)=X*1.0D2
            YU(NU)=Y*1.0D2
            ZU(NU)=Z*1.0D2
            TU(NU)=ST*1.0D-6
       ENDIF
C ---------------------------------------------------------------------
C     DETERMINATION OF REAL COLLISION TYPE
C ---------------------------------------------------------------------
      R2=drand48(RDUM)
C FIND LOCATION WITHIN 4 UNITS IN COLLISION ARRAY
      CALL SORT(I,R2,IE)
  140 I=I+1
      IF(CF(IE,I).LT.R2) GO TO 140
      S1=RGAS(I)
      EI=EIN(I)
      IF(IPN(I).LE.0) GO TO 666
C  USE FLAT DISTRIBUTION OF  ELECTRON ENERGY BETWEEN E-EION AND 0.0 EV
C  SAME AS IN BOLTZMANN
      R9=drand48(RDUM)
      EXTRA=R9*(E-EI)
      EI=EXTRA+EI
C
C  GENERATE SCATTERING ANGLES AND UPDATE  LABORATORY COSINES AFTER
C   COLLISION ALSO UPDATE ENERGY OF ELECTRON.
C
  666 IPT=IARRY(I)
      ICOLL(IPT)=ICOLL(IPT)+1
      ICOLN(I)=ICOLN(I)+1
      IF(E.LT.EI) THEN
      if(lbmcpr)WRITE(lunout,994) E,EI,J2
 994  FORMAT(2X,' WARNING ENERGY =',F8.3,' LESS THAN ENERGY LOSS EI=',F8
     /.3,' AT ITER=',I12,' DUE TO BINNING ERROR')
C  FIX ENERGY LOSS SMALLER THAN INCIDENT ENERGY IF ERROR OCCURS
       EI=E-0.0001D0
      ENDIF
      S2=(S1*S1)/(S1-1.0D0)
C  ANISOTROPIC SCATTERING
      IF(INDEX(I).NE.0) THEN
       R31=drand48(RDUM)
       R3=drand48(RDUM)
       F3=1.0D0-R3*ANGCT(IE,I)
       IF(R31.GT.PSCT(IE,I)) F3=-F3
      ELSE
C ISOTROPIC  SCATTERING
       R3=drand48(RDUM)
       F3=1.0D0-2.0D0*R3
      ENDIF
      THETA0=ACOS(F3)
      R4=drand48(RDUM)
      PHI0=F4*R4
      F8=SIN(PHI0)
      F9=COS(PHI0)
      ARG1=1.0D0-S1*EI/E
      ARG1=MAX(ARG1,SMALL)
      D=1.0D0-F3*SQRT(ARG1)
      E1=E*(1.0D0-EI/(S1*E)-2.0D0*D/S2)
      E1=MAX(E1,SMALL)
      Q=SQRT((E/E1)*ARG1)/S1
      Q=MIN(Q,1.0D0)
      THETA=ASIN(Q*SIN(THETA0))
      F6=COS(THETA)
      U=(S1-1.0D0)*(S1-1.0D0)/ARG1
      CSQD=F3*F3
      IF(F3.LT.0.0D0.AND.CSQD.GT.U) F6=-1.0D0*F6
      F5=SIN(THETA)
      DCZ2=MIN(DCZ2,1.0D0)
      ARGZ=SQRT(DCX2*DCX2+DCY2*DCY2)
      IF(ARGZ.EQ.0.0D0) THEN
      if(lbmcpr)WRITE(lunout,9232) ITER,ID,E1
 9232  FORMAT(3X,'WARNING ARGZ= 0.0 AT ITER =',I10,' ID =',I10,' E1=',E1
     /2.3)
       DCZ1=F6
       DCX1=F9*F5
       DCY1=F8*F5
       GO TO 130
      ENDIF
      DCZ1=DCZ2*F6+ARGZ*F5*F8
      DCY1=DCY2*F6+(F5/ARGZ)*(DCX2*F9-DCY2*DCZ2*F8)
      DCX1=DCX2*F6-(F5/ARGZ)*(DCY2*F9+DCX2*DCZ2*F8)
  130 CONTINUE
  133 CONTINUE
C   ------------------------------------------
      IF(J1.EQ.1.and.lbmcpr) WRITE(lunout,201)
  201 FORMAT(/,'    VEL      POS        TIME      ENERGY   COUNT   DIFXX
     /     DIFYY     DIFZZ',/)
      WZ=WZ*1.0D+09
      AVE=SUME2/ST
      DIFLN=0.0D0
      IF(NISO.EQ.0) THEN
       DIFXX=5.0D+15*SUMVX/ST
       DIFYY=5.0D+15*SUMVY/ST
       DFXXST(J1)=5.0D+15*(SUMVX-SVXOLD)/(ST-STOLD)
       DFYYST(J1)=5.0D+15*(SUMVY-SVYOLD)/(ST-STOLD)
      ELSE
       IF(ST2.NE.0.0D0) THEN
        DIFYY=5.0D+15*SUMYY/ST2
        DIFXX=5.0D+15*SUMXX/ST2
        DFXXST(J1)=5.0D+15*(SUMXX-SXXOLD)/(ST2-ST2OLD)
        DFYYST(J1)=5.0D+15*(SUMYY-SYYOLD)/(ST2-ST2OLD)
       ELSE
        DFXXST(J1)=0.0D0
        DFYYST(J1)=0.0D0
       ENDIF
      ENDIF
      IF(ST1.NE.0.0D0) THEN
       DIFZZ=5.0D+15*SUMZZ/ST1
       DFZZST(J1)=5.0D+15*(SUMZZ-SZZOLD)/(ST1-ST1OLD)
      ELSE
       DFZZST(J1)=0.0D0
      ENDIF
      WZST(J1)=(Z-ZOLD)/(ST-STOLD)*1.0D+09
      AVEST(J1)=(SUME2-SME2OLD)/(ST-STOLD)
      ZOLD=Z
      STOLD=ST
      ST1OLD=ST1
      ST2OLD=ST2
      SVXOLD=SUMVX
      SVYOLD=SUMVY
      SZZOLD=SUMZZ
      SXXOLD=SUMXX
      SYYOLD=SUMYY
      SME2OLD=SUME2
      if(lbmcpr)WRITE(lunout,202) WZ,Z,ST,AVE,ID,DIFXX,DIFYY,DIFZZ
  202 FORMAT(1X,F8.2,2(1X,D10.3),F9.4,1X,I5,1X,3(2X,F8.1))
C LOOP
  210 CONTINUE
C CALCULATE ERRORS AND CHECK AVERAGES
      TWZST=0.0D0
      TAVE=0.0D0
      T2WZST=0.0D0
      T2AVE=0.0D0
      TZZST=0.0D0
      TYYST=0.0D0
      TXXST=0.0D0
      T2ZZST=0.0D0
      T2YYST=0.0D0
      T2XXST=0.0D0
      DO 768 K=1,10
      TWZST=TWZST+WZST(K)
      TAVE=TAVE+AVEST(K)
      T2WZST=T2WZST+WZST(K)*WZST(K)
      T2AVE=T2AVE+AVEST(K)*AVEST(K)
      TXXST=TXXST+DFXXST(K)
      TYYST=TYYST+DFYYST(K)
      T2YYST=T2YYST+DFYYST(K)*DFYYST(K)
      T2XXST=T2XXST+DFXXST(K)*DFXXST(K)
      IF(K.LT.3) GO TO 768
      TZZST=TZZST+DFZZST(K)
      T2ZZST=T2ZZST+DFZZST(K)*DFZZST(K)
  768 CONTINUE
      DWZ=100.0D0*SQRT((T2WZST-TWZST*TWZST/10.0D0)/9.0D0)/WZ
      DEN=100.0D0*SQRT((T2AVE-TAVE*TAVE/10.0D0)/9.0D0)/AVE
      DXXER=100.0D0*SQRT((T2XXST-TXXST*TXXST/10.0D0)/9.0D0)/DIFXX
      DYYER=100.0D0*SQRT((T2YYST-TYYST*TYYST/10.0D0)/9.0D0)/DIFYY
      DZZER=100.0D0*SQRT((T2ZZST-TZZST*TZZST/8.0D0)/7.0D0)/DIFZZ
      DIFLN=DIFZZ
      DIFTR=(DIFXX+DIFYY)/2.0D0
C  CONVERT CM/SEC
      WZ=WZ*1.0D05
      DFLER=DZZER
      DFTER=(DXXER+DYYER)/2.0D0
C CALCULATE TOWNSEND COEFICIENTS AND ERRORS
      ANCATT=0.0D0
      ANCION=0.0D0
      DO 800 I=1,NGAS
      ANCATT=ANCATT+ICOLL((5*I)-2)
  800 ANCION=ANCION+ICOLL((5*I)-3)
      ATTER=0.0D0
      IF(ANCATT.EQ.0.0D0) GO TO 810
      ATTER=100.0D0*SQRT(ANCATT)/ANCATT
  810 ATT=ANCATT/(ST*WZ)*1.0D12
      ALPER=0.0D0
      IF(ANCION.EQ.0.0D0) GO TO 820
      ALPER=100.0D0*SQRT(ANCION)/ANCION
  820 ALPHA=ANCION/(ST*WZ)*1.0D12
      END
