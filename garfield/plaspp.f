CDECK  ID>, PLASPP.
       SUBROUTINE PLASPP(IVOL,IOFCOL)
*-----------------------------------------------------------------------
*   PLASPP - Plots a sphere in 3D perspective.
*   (Last changed on  7/ 7/10.)
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
       PARAMETER (MXWIRE=   300,MXSW  =   50)
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
       PARAMETER (MXMAP =  5000,MXEPS =   10)
       PARAMETER (MXWMAP=     5)
       PARAMETER (MXSOLI=  1000)
       PARAMETER (MXPLAN= 50000, MXPOIN=100000,MXEDGE=100)
       PARAMETER (MXSBUF= 20000)
       PARAMETER (MXMCA = 50000)
*   The parameter MXNBMC must equal MXGNAM (sequence MAGBPARM) !
       INTEGER MXNBMC
       PARAMETER(MXNBMC=60)
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
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
       REAL USERX0,USERX1,USERY0,USERY1,FRXMIN,FRXMAX,FRYMIN,FRYMAX,
     -      ARRANG,ARRLEN,BARFRC,DISPX0,DISPX1,DISPY0,DISPY1,
     -      GPXN,GPXN10,GPYN,GPYN10,GPXL,GPYL,GPXT
       LOGICAL LGRID,LGRALL,LOGX,LOGY,LSTAMP,LGCLRB,LGCLRA,
     -      LWAITA,LWAITB,LXCCH,LGLCLP,LGMCLP,LGACLP,LGTCLP,
     -      WKMULT(MXWKLS)
       INTEGER NWK,WKID(MXWKLS),WKCON(MXWKLS),WKFREF(MXWKLS),
     -         WKLUN(MXWKLS),WKSTAT(MXWKLS),WKSREQ(MXWKLS),
     -         NCWKNM(MXWKLS),NCSTMP,IGHIST,IGBAR,NCGKS
       CHARACTER*20 WKNAME(MXWKLS),WKATTR(MXWKLS)
       CHARACTER*80 STAMP
       CHARACTER*(MXNAME) GKSLOG
       COMMON /GRADAT/ USERX0,USERX1,USERY0,USERY1,ARRANG,ARRLEN,
     -      BARFRC,
     -      FRXMIN,FRXMAX,FRYMIN,FRYMAX,DISPX0,DISPX1,DISPY0,DISPY1,
     -      GPXN,GPXN10,GPYN,GPYN10,GPXL,GPYL,GPXT,
     -      LGRID,LGRALL,LOGX,LOGY,LSTAMP,LGCLRB,LGCLRA,LWAITA,LWAITB,
     -      LXCCH,LGLCLP,LGMCLP,LGACLP,LGTCLP,
     -      NWK,WKID,WKCON,WKFREF,WKLUN,WKSTAT,WKSREQ,NCWKNM,NCSTMP,
     -      IGHIST,IGBAR,NCGKS,WKMULT
       COMMON /GRACHR/ WKNAME,WKATTR,STAMP,GKSLOG
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER IVOL,IREF,IOFCOL,ICOL,NMAX,I,J,N,IFAIL,NPL,IRPL
       PARAMETER(NMAX=50)
       DOUBLE PRECISION R,X0,Y0,Z0,WW,PHI0,PHI1,THETA0,THETA1,
     -      XPL(4),YPL(4),ZPL(4),CI,SI
*** Locate the conductor.
       IF(IVOL.LT.1.OR.IVOL.GT.MXSOLI)THEN
            PRINT *,' !!!!!! PLASPP WARNING : Volume reference is out'//
     -           ' of range ; not plotted.'
            RETURN
       ENDIF
       IREF=ISTART(IVOL)
       IF(IREF.LT.0.OR.IREF+8.GT.MXSBUF)THEN
            PRINT *,' !!!!!! PLASPP WARNING : Volume address is out'//
     -           ' of range ; not plotted.'
            RETURN
       ENDIF
*** Locate the sphere parameters.
       R= CBUF(IREF+1)
       IF(R.LE.0)THEN
            PRINT *,' !!!!!! PLASPP WARNING : Sphere ',IVOL,' has a'//
     -           ' non-positive radius; not plotted.'
            RETURN
       ENDIF
       X0=CBUF(IREF+2)
       Y0=CBUF(IREF+3)
       Z0=CBUF(IREF+4)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLASPP DEBUG   : Drawing a'',
     -      '' sphere from address '',I4/26X,''Radius='',E10.3/
     -      26X,''Centre='',3E10.3)') IREF,R,X0,Y0,Z0
       N= MIN(MXEDGE-1,NMAX-1,NINT(CBUF(IREF+5)))
*** Determine a suitable number of points on the radii.
       IF(N.LT.1)THEN
            IF(R.LT.1E-3*MAX(ABS(FRXMAX-FRXMIN),ABS(FRYMAX-FRYMIN)))THEN
                 N=MIN(NMAX-1,MXEDGE-1,5)
            ELSEIF(R.LT.1E-2*MAX(ABS(FRXMAX-FRXMIN),
     -           ABS(FRYMAX-FRYMIN)))THEN
                 N=MIN(NMAX-1,MXEDGE-1,10)
            ELSEIF(R.LT.1E-1*MAX(ABS(FRXMAX-FRXMIN),
     -           ABS(FRYMAX-FRYMIN)))THEN
                 N=MIN(NMAX-1,MXEDGE-1,20)
            ELSE
                 N=MIN(NMAX-1,MXEDGE-1)
            ENDIF
       ENDIF
*** Loop over the sphere.
       DO 10 I=1,N
       PHI0=2.0D0*PI*DBLE(I-1)/DBLE(N)
       PHI1=2.0D0*PI*DBLE(I)/DBLE(N)
       DO 20 J=1,N
       THETA0=-PI/2+PI*DBLE(J-1)/DBLE(N)
       THETA1=-PI/2+PI*DBLE(J)/DBLE(N)
*   Corners of this parcel.
       IF(J.EQ.1)THEN
            XPL(1)=X0+R*COS(PHI0)*COS(THETA0)
            YPL(1)=Y0+R*SIN(PHI0)*COS(THETA0)
            ZPL(1)=Z0+R          *SIN(THETA0)
            XPL(2)=X0+R*COS(PHI1)*COS(THETA1)
            YPL(2)=Y0+R*SIN(PHI1)*COS(THETA1)
            ZPL(2)=Z0+R          *SIN(THETA1)
            XPL(3)=X0+R*COS(PHI0)*COS(THETA1)
            YPL(3)=Y0+R*SIN(PHI0)*COS(THETA1)
            ZPL(3)=Z0+R          *SIN(THETA1)
            NPL=3
       ELSEIF(J.EQ.N)THEN
            XPL(1)=X0+R*COS(PHI0)*COS(THETA0)
            YPL(1)=Y0+R*SIN(PHI0)*COS(THETA0)
            ZPL(1)=Z0+R          *SIN(THETA0)
            XPL(2)=X0+R*COS(PHI1)*COS(THETA0)
            YPL(2)=Y0+R*SIN(PHI1)*COS(THETA0)
            ZPL(2)=Z0+R          *SIN(THETA0)
            XPL(3)=X0+R*COS(PHI1)*COS(THETA1)
            YPL(3)=Y0+R*SIN(PHI1)*COS(THETA1)
            ZPL(3)=Z0+R          *SIN(THETA1)
            NPL=3
       ELSE
            XPL(1)=X0+R*COS(PHI0)*COS(THETA0)
            YPL(1)=Y0+R*SIN(PHI0)*COS(THETA0)
            ZPL(1)=Z0+R          *SIN(THETA0)
            XPL(2)=X0+R*COS(PHI1)*COS(THETA0)
            YPL(2)=Y0+R*SIN(PHI1)*COS(THETA0)
            ZPL(2)=Z0+R          *SIN(THETA0)
            XPL(3)=X0+R*COS(PHI1)*COS(THETA1)
            YPL(3)=Y0+R*SIN(PHI1)*COS(THETA1)
            ZPL(3)=Z0+R          *SIN(THETA1)
            XPL(4)=X0+R*COS(PHI0)*COS(THETA1)
            YPL(4)=Y0+R*SIN(PHI0)*COS(THETA1)
            ZPL(4)=Z0+R          *SIN(THETA1)
            NPL=4
       ENDIF
*   Inclination angle in theta.
       CI=COS(ATAN2(
     -      (COS(THETA0)-COS(THETA1))*SQRT((1+COS(PHI1-PHI0))/2),
     -      SIN(THETA1)-SIN(THETA0)))
       SI=SIN(ATAN2(
     -      (COS(THETA0)-COS(THETA1))*SQRT((1+COS(PHI1-PHI0))/2),
     -      SIN(THETA1)-SIN(THETA0)))
*   Compute the colour index.
       CALL COLWGT(COS((PHI0+PHI1)/2)*CI,SIN((PHI0+PHI1)/2)*CI,SI,WW)
       IF(WW.GE.0)THEN
            ICOL=IOFCOL+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
       ELSE
            ICOL=IOFCOL
       ENDIF
*   Store the panel.
       CALL PLABU1('STORE',IRPL,NPL,XPL,YPL,ZPL,
     -      COS((PHI0+PHI1)/2)*CI,SIN((PHI0+PHI1)/2)*CI,SI,
     -      ICOL,IVOL,IFAIL)
       IF(IFAIL.NE.0)PRINT *,' !!!!!! PLASPP WARNING : Unable to'//
     -      ' store a panel of a sphere.'
*   Next point.
20     CONTINUE
10     CONTINUE
*** Look for intersections with the outside box, x=xmin.
       CALL PLASPC(IVOL,GXMIN,(GYMIN+GYMAX)/2,(GZMIN+GZMAX)/2,
     -      -1.0D0,0.0D0,0.0D0,IOFCOL+1)
*   x=xmax.
       CALL PLASPC(IVOL,GXMAX,(GYMIN+GYMAX)/2,(GZMIN+GZMAX)/2,
     -      +1.0D0,0.0D0,0.0D0,IOFCOL+1)
*   y=ymin.
       CALL PLASPC(IVOL,(GXMIN+GXMAX)/2,GYMIN,(GZMIN+GZMAX)/2,
     -      0.0D0,-1.0D0,0.0D0,IOFCOL+1)
*   y=ymax.
       CALL PLASPC(IVOL,(GXMIN+GXMAX)/2,GYMAX,(GZMIN+GZMAX)/2,
     -      0.0D0,+1.0D0,0.0D0,IOFCOL+1)
*   z=zmin.
       CALL PLASPC(IVOL,(GXMIN+GXMAX)/2,(GYMIN+GYMAX)/2,GZMIN,
     -      0.0D0,0.0D0,-1.0D0,IOFCOL+1)
*   z=zmax.
       CALL PLASPC(IVOL,(GXMIN+GXMAX)/2,(GYMIN+GYMAX)/2,GZMAX,
     -      0.0D0,0.0D0,+1.0D0,IOFCOL+1)
       END
