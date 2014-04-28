CDECK  ID>, TRMGEN.
       SUBROUTINE TRMGEN(FAIL)
*-----------------------------------------------------------------------
*   TRMGEN - Generates TRIM clusters
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
       REAL X, Y, Z, XDIR, YDIR, ZDIR, TRLEN, ELOSS, ECL, ERND1, RNDNOR,
     -      THETA, PHI, X0, Y0, Z0, ALPHA, R
       INTEGER FAIL
       EXTERNAL RNDNOR
*** Assume this will fail
       FAIL = 1
*** Reset the cluseter count
       NCTRIM = 0
*** Initial situation: starting position
       X = XT0
       Y = YT0
       Z = ZT0
*   Initial direction
       XDIR=XT1-XT0
       YDIR=YT1-YT0
       ZDIR=ZT1-ZT0
       TRLEN=SQRT(XDIR**2+YDIR**2+ZDIR**2)
       IF (TRLEN.LE.0) THEN
          PRINT*, ' !!!!!! TRMGEN WARNING: Track length = 0; no TRIM'//
     -            ' cluster generation.'
          RETURN
       ENDIF
       XDIR=XDIR/TRLEN
       YDIR=YDIR/TRLEN
       ZDIR=ZDIR/TRLEN
       PHI=ATAN2(ZDIR,XDIR)
       THETA=ATAN2(YDIR,SQRT(XDIR**2+ZDIR**2))
*** Header of debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ TRMGEN DEBUG   : Track'',
     -      '' generation with the following parameters:''/
     -      26X,''Table size         '',I12/
     -      26X,''Track length       '',E12.5,'' cm''/
     -      26X,''Particle energy    '',E12.5,'' MeV''/
     -      26X,''Particle mass      '',E12.5,'' MeV''/
     -      26X,''Particle charge    '',F12.1/
     -      26X,''Work function      '',E12.5,'' eV''/
     -      26X,''Fano factor        '',E12.5)')
     -      NTRIM,TRLEN,TRENER,TRMASS,TRCHAR,WTRIM,FTRIM
*   Loop over clusters
10     CONTINUE
**  Add a cluster
          NCTRIM = NCTRIM + 1
          XTRIM(NCTRIM) = X
          YTRIM(NCTRIM) = Y
          ZTRIM(NCTRIM) = Z
          IF (NCTRIM.GT.1) THEN
             ELOSS = (TRMEMI(NCTRIM) + TRMHDI(NCTRIM)) * (TRMTGD(NCTRIM)
     -               - TRMTGD(NCTRIM - 1))
          ELSE
             ELOSS = (TRMEMI(NCTRIM) + TRMHDI(NCTRIM)) *
     -               (TRMTGD(NCTRIM + 1) - TRMTGD(NCTRIM))
          ENDIF
          IF (FTRIM.LE.0) THEN
             NETRIM(NCTRIM)=INT(ELOSS/WTRIM)
             ECTRIM(NCTRIM)=WTRIM*NETRIM(NCTRIM)
          ELSE
             ECL = ELOSS
             NETRIM(NCTRIM) = 0.0
             ECTRIM(NCTRIM) = 0.0
30           CONTINUE
                CALL RNDHWF(WTRIM,FTRIM,ERND1)
                IF ((ECL - ERND1).GE.0) THEN
                   NETRIM(NCTRIM) = NETRIM(NCTRIM) + 1.0
                   ECTRIM(NCTRIM) = ECTRIM(NCTRIM) + ERND1
                   ECL = ECL - ERND1
                   GOTO 30
                ENDIF
             CONTINUE
          ENDIF
          IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ TRMGEN DEBUG   : '',
     -      '' Cluster generated with following values:''/
     -      26X,''Target depth       '',E12.5,'' A''/
     -      26X,''dE/dx (EM)         '',E12.5,'' eV/A''/
     -      26X,''dE/dx (HD)         '',E12.5,'' eV/A''/
     -      26X,''Ion Energy         '',E12.5,'' eV''/
     -      26X,''Energy Loss        '',E12.5,'' eV''/
     -      26X,''Number of e-s      '',F12.1/
     -      26X,''Energy of cluster  '',E12.5,'' eV''/
     -      26X,''x-coordinate       '',E12.5/
     -      26X,''y-coordinate       '',E12.5/
     -      26X,''z-coordinate       '',E12.5)')
     -      TRMTGD(NCTRIM),TRMEMI(NCTRIM),TRMHDI(NCTRIM),TRMIOE(NCTRIM),
     -      ELOSS, NETRIM(NCTRIM),ECTRIM(NCTRIM),X,Y,Z
          IF (NCTRIM.EQ.NTRIM) GOTO 20
*   Update position
       X0 = XT0 + ((TRMTGD(NCTRIM + 1)-TRMTGD(1))*COS(THETA)*COS(PHI))
     -     * 1E-8
       Y0 = YT0 + ((TRMTGD(NCTRIM + 1)-TRMTGD(1))*SIN(THETA))
     -     * 1E-8
       Z0 = ZT0 + ((TRMTGD(NCTRIM + 1)-TRMTGD(1))*COS(THETA)*SIN(PHI))
     -     * 1E-8
       ALPHA = ATAN2((TRMZ(NCTRIM) - TRMZ(1)), (TRMY(NCTRIM) - TRMY(1)))
       R = SQRT((TRMY(NCTRIM) - TRMY(1))**2 + (TRMZ(NCTRIM) -
     -     TRMZ(1))**2)
       X = X0 + (R*SIN(THETA) * 1E-8)
       Y = Y0 + (R*COS(THETA)*COS(ALPHA) * 1E-8)
       Z = Z0 + (R*COS(THETA)*SIN(ALPHA) * 1E-8)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ TRMGEN DEBUG   : '',
     -      '' Incrementing position:''/
     -      26X,''xdir               '',E12.5,/
     -      26X,''ydir               '',E12.5,/
     -      26X,''zdir               '',E12.5,/
     -      26X,''theta              '',E12.5,/
     -      26X,''phi                '',E12.5,/
     -      26X,''New x              '',E12.5,'' cm''/
     -      26X,''New y              '',E12.5,'' cm''/
     -      26X,''New z              '',E12.5,'' cm'')')
     -      XDIR, YDIR, ZDIR, theta, phi, X, Y, Z
       GOTO 10
20     CONTINUE
*   Seems to have worked
       FAIL = 0
       END
