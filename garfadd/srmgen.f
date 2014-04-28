CDECK  ID>, SRMGEN.
       SUBROUTINE SRMGEN(IFAIL)
*-----------------------------------------------------------------------
*   SRMGEN - Generates electrons for a SRIM track
*   (Last changed on 10/12/07.)
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
       REAL SRMDEN,ESRIM,SRMEM,SRMHD,SRMRNG,SRMDT,SRMDL,WSRIM,FSRIM,
     -      XSRIM,YSRIM,ZSRIM,ECSRIM,EKSRIM
       INTEGER NSRIM,NCSRIM,NESRIM
       COMMON /SRMDAT/
     -      ESRIM(MXLIST),SRMEM(MXLIST),
     -      SRMHD(MXLIST),SRMRNG(MXLIST),SRMDT(MXLIST),SRMDL(MXLIST),
     -      XSRIM(MXCLUS),YSRIM(MXCLUS),ZSRIM(MXCLUS),ECSRIM(MXCLUS),
     -      EKSRIM(MXCLUS),SRMDEN,WSRIM,FSRIM,
     -      NSRIM,NCSRIM,NESRIM(MXCLUS)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL X,Y,Z,E,DEDXEM,DEDXHD,PRANGE,STRLON,STRLAT,SP,CP,ST,CT,
     -      DNORM,STEP,XDIR,YDIR,ZDIR,TRLEN,THETA,PHI,SIGT1,SIGT2,SIGL,
     -      ELOSS,DSUM,RANLAN,RNDUNI,RNDNOR,DIVDIF,RNDDE,DEEM,DEHD,
     -      SRMDEM,SRMDHD,STPMIN,STPMAX,EPOOL,ECL,ERND1
       INTEGER IFAIL,IFAIL1,I
       EXTERNAL DIVDIF,RANLAN,RNDUNI,RNDNOR,RNDDE,SRMDEM,SRMDHD
       LOGICAL FINISH
*** Assume this will fail
       IFAIL=1
*** Reset the cluster count
       NCSRIM=0
*** Initialise error count
       DO 30 I=1,10
       NTRERR(I)=0
30     CONTINUE
*** Initial situation: starting position
       X=XT0
       Y=YT0
       Z=ZT0
*   Initial direction
       XDIR=XT1-XT0
       YDIR=YT1-YT0
       ZDIR=ZT1-ZT0
       TRLEN=SQRT(XDIR**2+YDIR**2+ZDIR**2)
       IF(TRLEN.LE.0)THEN
            PRINT *,' !!!!!! SRMGEN WARNING : Track length = 0; no'//
     -           ' SRIM cluster generation.'
            RETURN
       ENDIF
       XDIR=XDIR/TRLEN
       YDIR=YDIR/TRLEN
       ZDIR=ZDIR/TRLEN
*** Header of debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMGEN DEBUG   : Track'',
     -      '' generation with the following parameters:''/
     -      26X,''Table size         '',I12/
     -      26X,''Track length       '',E12.5,'' cm''/
     -      26X,''Particle energy    '',E12.5,'' MeV''/
     -      26X,''Particle mass      '',E12.5,'' MeV''/
     -      26X,''Particle charge    '',F12.1/
     -      26X,''Work function      '',E12.5,'' eV''/
     -      26X,''Fano factor        '',E12.5/
     -      26X,''Long. straggling:  '',L1/
     -      26X,''Trans. straggling: '',L1/
     -      26X,''Vavilov generator: '',L1/
     -      26X,''Cluster size       '',F12.1)')
     -      NSRIM,TRLEN,TRENER,TRMASS,TRCHAR,WSRIM,FSRIM,
     -      LDLSRM,LDTSRM,LTRVVL,TRNSRM
*   Initial energy
       E=TRENER
*   Total distance covered
       DSUM=0.0
*   Pool of unused energy
       EPOOL=0.0
*** Loop over the clusters
10     CONTINUE
**  See whether the particle has (numerically) stopped.
       IF(ABS(E).LT.1E-20*TRMASS.OR.ABS(E).LT.1E-9*WSRIM)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMGEN DEBUG   :'',
     -           '' Particle energy E = '',E12.5,'' MeV such that'',
     -           '' beta2 = 0 or E << W; particle stopped.'')')  E
            ELOSS=E
            GOTO 40
       ENDIF
**  Work out what the energy loss per cm is at the start of the step.
       DEDXEM=SRMDEM(E)*SRMDEN
       DEDXHD=SRMDHD(E)*SRMDEN
       IF(E.LT.ESRIM(1))THEN
            PRANGE=SRMRNG(1)
            STRLON=SRMDL(1)
            STRLAT=SRMDT(1)
       ELSEIF(E.GT.ESRIM(NSRIM))THEN
            PRANGE=SRMRNG(NSRIM)
            STRLON=SRMDL(NSRIM)
            STRLAT=SRMDT(NSRIM)
       ELSE
            PRANGE=DIVDIF(SRMRNG,ESRIM,NSRIM,E,2)
            STRLON=DIVDIF(SRMDL,ESRIM,NSRIM,E,2)
            STRLAT=DIVDIF(SRMDT,ESRIM,NSRIM,E,2)
       ENDIF
       IF(.NOT.LDLSRM)STRLON=0
       IF(.NOT.LDTSRM)STRLAT=0
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMGEN DEBUG   : E = '',
     -      E10.3,'' MeV, dEdx em, hd = '',2E10.3,'' MeV/cm, e-/cm = '',
     -      F10.3)') E,DEDXEM,DEDXHD,1E6*DEDXEM/WSRIM
**  Find the step size for which we get approximately the # clusters.
       IF(TRNSRM.GT.0)THEN
            STEP=(TRNSRM*WSRIM)/(1E6*DEDXEM)
       ELSEIF(TRNSRM.LT.-1.5)THEN
            STEP=TRLEN
       ELSE
            STEP=TRENER/(0.5*MXCLUS*(DEDXEM+DEDXHD))
       ENDIF
*   Truncate if this step exceeds the length.
       IF(DSUM+STEP.GT.TRLEN)THEN
            STEP=TRLEN-DSUM
            FINISH=.TRUE.
       ELSE
            FINISH=.FALSE.
       ENDIF
*   Make an accurate integration of the energy loss over the step.
       CALL SRMRKS(STEP,E,DEEM,DEHD)
**  If the energy loss exceeds the particle energy, truncate step.
       IF(DEEM+DEHD.GT.E)THEN
            CALL SRMDEZ(E,STEP,STPMAX,IFAIL1)
            STEP=STPMAX
            CALL SRMRKS(STEP,E,DEEM,DEHD)
            DEEM=E*DEEM/(DEHD+DEEM)
            DEHD=E-DEEM
            FINISH=.TRUE.
       ELSE
            STPMAX=TRLEN-DSUM
       ENDIF
**  Ensure that this is larger than the minimum modelable step size.
       CALL SRMMST(E,DEEM,STEP,ITFSRM,STPMIN,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! SRMGEN WARNING : Failure computing the'//
     -           ' minimum step size; clustering abandoned.'
            RETURN
       ENDIF
*   No way to find a suitable step size: use fixed energy loss
       IF(STPMIN.GT.STPMAX)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMGEN DEBUG   :'',
     -           '' Step min > Step max; depositing all energy.'')')
            ELOSS=DEEM
            IF(ABS(E-DEHD-ELOSS).GT.1E-8*WSRIM)NTRERR(3)=NTRERR(3)+1
            IF(E-ELOSS-DEHD.LT.0)ELOSS=E-DEHD
            FINISH=.TRUE.
*   If needed enlarge the step size
       ELSEIF(STEP.LT.STPMIN)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMGEN DEBUG   :'',
     -           '' Enlarging step size.'')')
            STEP=STPMIN
            CALL SRMRKS(STEP,E,DEEM,DEHD)
            IF(DEEM+DEHD.GT.E)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMGEN DEBUG   :'',
     -                '' Excess energy loss; recomputing step_max.'')')
                 CALL SRMDEZ(E,STEP,STPMAX,IFAIL1)
                 STEP=STPMAX
                 CALL SRMRKS(STEP,E,DEEM,DEHD)
                 DEEM=E*DEEM/(DEHD+DEEM)
                 DEHD=E-DEEM
                 ELOSS=DEEM
            ELSE
                 ELOSS=RNDDE(E,DEEM,STEP,ITFSRM)
            ENDIF
*   Draw an actual energy loss for such a step.
       ELSE
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMGEN DEBUG   :'',
     -           '' Using existing step size.'')')
            ELOSS=RNDDE(E,DEEM,STEP,ITFSRM)
       ENDIF
**  Ensure we are neither below 0 nor above the total energy.
       IF(ELOSS.LT.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMGEN DEBUG   :'',
     -           '' Truncating negative energy loss.'')')
            ELOSS=0
            NTRERR(1)=NTRERR(1)+1
       ELSEIF(ELOSS.GT.E-DEHD)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMGEN DEBUG   :'',
     -           '' Excess energy loss, using mean instead.'')')
            IF(ABS(E-DEHD-ELOSS).GT.1E-8*WSRIM)NTRERR(4)=NTRERR(4)+1
            ELOSS=DEEM
            IF(E-ELOSS-DEHD.LT.0)THEN
                 ELOSS=E-DEHD
                 FINISH=.TRUE.
            ENDIF
       ENDIF
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMGEN DEBUG   : Step'',
     -      '' length = '',E10.3,'' cm, mean loss = '',E10.3,
     -      '' MeV, actual loss = '',E10.3,'' MeV.'')')
     -       STEP,DEEM,ELOSS
**  Add a cluster.
40     CONTINUE
       IF(NCSRIM.GE.MXCLUS)THEN
            PRINT *,' !!!!!! SRMGEN WARNING : Maximum number of'//
     -           ' clusters reached; clustering abandoned.'
            RETURN
       ENDIF
       NCSRIM=NCSRIM+1
       XSRIM(NCSRIM)=X
       YSRIM(NCSRIM)=Y
       ZSRIM(NCSRIM)=Z
       IF(FSRIM.LE.0)THEN
            NESRIM(NCSRIM)=INT((ELOSS+EPOOL)/(1.0E-6*WSRIM))
            ECSRIM(NCSRIM)=1.0E-6*WSRIM*NESRIM(NCSRIM)
       ELSE
            ECL=ELOSS+EPOOL
            NESRIM(NCSRIM)=0.0
            ECSRIM(NCSRIM)=0.0
50          CONTINUE
            CALL RNDHWF(1.0E-6*WSRIM,FSRIM,ERND1)
            IF(ECL-ERND1.GE.0)THEN
                 NESRIM(NCSRIM)=NESRIM(NCSRIM)+1
                 ECSRIM(NCSRIM)=ECSRIM(NCSRIM)+ERND1
                 ECL=ECL-ERND1
                 GOTO 50
            ENDIF
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''EM + pool: '',E12.5,
     -           '' eV, w: '',E12.5,'' eV, E/w: '',E12.5,
     -           '', n: '',I10)')
     -           1.0E6*(ELOSS+EPOOL),WSRIM,(ELOSS+EPOOL)/(1.0E-6*WSRIM),
     -           NESRIM(NCSRIM)
       ENDIF
       EKSRIM(NCSRIM)=E
       EPOOL=(ELOSS+EPOOL) - ECSRIM(NCSRIM)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMGEN DEBUG   : Cluster '',
     -      I5,'' at ('',E10.3,'','',E10.3,'','',E10.3,
     -      '') cm, e = '',E10.3,'' MeV, n = '',I10,
     -      '', pool = '',E10.3,'' MeV.'')')
     -      NCSRIM,XSRIM(NCSRIM),YSRIM(NCSRIM),ZSRIM(NCSRIM),
     -      ECSRIM(NCSRIM),NESRIM(NCSRIM),EPOOL
**  Keep track of the length and energy
       DSUM=DSUM+STEP
       E=E-ELOSS-DEHD
*   Stop of the flag is raised
       IF(FINISH)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMGEN DEBUG   :'',
     -           '' Finishing flag raised.'')')
            GOTO 20
*   Stop if the distance has been reached
       ELSEIF(DSUM.GT.TRLEN)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMGEN DEBUG   :'',
     -           '' Reached track length d = '',E10.3,'' cm.'')') TRLEN
            GOTO 20
*   Single cluster
       ELSEIF(TRNSRM.LT.-1.5)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMGEN DEBUG   :'',
     -           '' Single cluster requested.'')')
            GOTO 20
*   No energy left
       ELSEIF(E.LE.TRENER*1E-9)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMGEN DEBUG   :'',
     -           '' Energy exhausted.'')')
            GOTO 20
       ENDIF
**  Draw scattering distances
       SIGT1=RNDNOR(0.0,SQRT(STEP/PRANGE)*STRLAT)
       SIGT2=RNDNOR(0.0,SQRT(STEP/PRANGE)*STRLAT)
       SIGL=RNDNOR(0.0,SQRT(STEP/PRANGE)*STRLON)
*   Rotation angles to bring z-axis in line
       IF(XDIR**2+ZDIR**2.LE.0)THEN
            IF(YDIR.LT.0)THEN
                 THETA=-PI/2
            ELSEIF(YDIR.GT.0)THEN
                 THETA=+PI/2
            ELSE
                 PRINT *,' !!!!!! SRMGEN WARNING : Encountered'//
     -                ' zero step length; clustering abandoned.'
                 RETURN
            ENDIF
            PHI=0
       ELSE
            PHI=ATAN2(XDIR,ZDIR)
            THETA=ATAN2(YDIR,SQRT(XDIR**2+ZDIR**2))
       ENDIF
*   Update position
       CP=COS(PHI)
       CT=COS(THETA)
       SP=SIN(PHI)
       ST=SIN(THETA)
       X=X+STEP*XDIR+CP*SIGT1-SP*ST*SIGT2+SP*CT*SIGL
       Y=Y+STEP*YDIR+            CT*SIGT2   +ST*SIGL
       Z=Z+STEP*ZDIR-SP*SIGT1-CP*ST*SIGT2+CP*CT*SIGL
**  Update direction
       if(.false.)then
       XDIR=STEP*XDIR+CP*SIGT1-SP*ST*SIGT2+SP*CT*SIGL
       YDIR=STEP*YDIR+            CT*SIGT2   +ST*SIGL
       ZDIR=STEP*ZDIR-SP*SIGT1-CP*ST*SIGT2+CP*CT*SIGL
       DNORM=SQRT(XDIR**2+YDIR**2+ZDIR**2)
       IF(DNORM.LE.0)THEN
            PRINT *,' !!!!!! SRMGEN WARNING : Step length = 0;'//
     -           ' clustering abandoned.'
            RETURN
       ENDIF
       XDIR=XDIR/DNORM
       YDIR=YDIR/DNORM
       ZDIR=ZDIR/DNORM
       endif
       GOTO 10
*** End of loop.
20     CONTINUE
*   Print error messages
       CALL SRMERR
*   Seems to have worked.
       IFAIL=0
       END
