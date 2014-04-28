CDECK  ID>, PLATBP.
       SUBROUTINE PLATBP(IVOL,IOFCOL)
*-----------------------------------------------------------------------
*   PLATBP - Surface panels of a Toblerone bar.
*   (Last changed on 11/ 3/10.)
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
       INTEGER IVOL,IREF,IOFCOL,ICOL,IFAIL,IRPL
       DOUBLE PRECISION XL,YL,XH,ZH,X0,Y0,Z0,A,B,C,CT,ST,CP,SP,
     -      U1,V1,W1,WW,FNORM,XPL(4),YPL(4),ZPL(4),XROOF,ZROOF
*** Locate the conductor.
       IF(IVOL.LT.1.OR.IVOL.GT.MXSOLI)THEN
            PRINT *,' !!!!!! PLATBP WARNING : Volume reference is out'//
     -           ' of range ; not plotted.'
            RETURN
       ENDIF
       IREF=ISTART(IVOL)
       IF(IREF.LT.0.OR.IREF+8.GT.MXSBUF)THEN
            PRINT *,' !!!!!! PLATBP WARNING : Volume address is out'//
     -           ' of range ; not plotted.'
            RETURN
       ENDIF
*** Locate the cube parameters.
       XL=ABS(CBUF(IREF+1))
       YL=ABS(CBUF(IREF+2))
       ZH=ABS(CBUF(IREF+3))
       X0=CBUF(IREF+4)
       Y0=CBUF(IREF+5)
       Z0=CBUF(IREF+6)
       XH=CBUF(IREF+14)
       FNORM=SQRT(CBUF(IREF+7)**2+CBUF(IREF+8)**2+CBUF(IREF+9)**2)
       IF(FNORM.LE.0)THEN
            PRINT *,' !!!!!! PLATBP WARNING : Ridge ',IVOL,' has'//
     -           ' a zero norm direction vector; not plotted.'
            RETURN
       ENDIF
       A= CBUF(IREF+7)/FNORM
       B= CBUF(IREF+8)/FNORM
       C= CBUF(IREF+9)/FNORM
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLATBP DEBUG   : Drawing a'',
     -      '' ridge from address '',I4/
     -      26X,''Centre=      '',3E10.3/
     -      26X,''Half-lengths='',2E10.3/
     -      26X,''Ridge       ='',2E10.3/
     -      26X,''Direction=   '',3E10.3)')
     -      IREF,X0,Y0,Z0,XL,YL,XH,ZH,A,B,C
*   Shorthand for the rotations.
       CT=CBUF(IREF+10)
       ST=CBUF(IREF+11)
       CP=CBUF(IREF+12)
       SP=CBUF(IREF+13)
*** Draw the 5 sides of the ridge, start with the floor
       U1=-XL
       V1=-YL
       W1=0
       XPL(1)=X0+CP*CT*U1-SP*V1+CP*ST*W1
       YPL(1)=Y0+SP*CT*U1+CP*V1+SP*ST*W1
       ZPL(1)=Z0   -ST*U1         +CT*W1
       U1=-XL
       V1=+YL
       W1=0
       XPL(2)=X0+CP*CT*U1-SP*V1+CP*ST*W1
       YPL(2)=Y0+SP*CT*U1+CP*V1+SP*ST*W1
       ZPL(2)=Z0   -ST*U1         +CT*W1
       U1=+XL
       V1=+YL
       W1=0
       XPL(3)=X0+CP*CT*U1-SP*V1+CP*ST*W1
       YPL(3)=Y0+SP*CT*U1+CP*V1+SP*ST*W1
       ZPL(3)=Z0   -ST*U1         +CT*W1
       U1=+XL
       V1=-YL
       W1=0
       XPL(4)=X0+CP*CT*U1-SP*V1+CP*ST*W1
       YPL(4)=Y0+SP*CT*U1+CP*V1+SP*ST*W1
       ZPL(4)=Z0   -ST*U1         +CT*W1
       CALL COLWGT(-CP*ST,-SP*ST,-CT,WW)
       IF(WW.GE.0)THEN
            ICOL=IOFCOL+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
       ELSE
            ICOL=IOFCOL
       ENDIF
       CALL PLABU1('STORE',IRPL,4,XPL,YPL,ZPL,-CP*ST,-SP*ST,-CT,
     -      ICOL,IVOL,IFAIL)
       IF(IFAIL.NE.0)PRINT *,' !!!!!! PLATBP WARNING : Unable to'//
     -      ' store a panel of a ridge.'
*  Side triangle at y=ymin
       U1=-XL
       V1=-YL
       W1=0
       XPL(1)=X0+CP*CT*U1-SP*V1+CP*ST*W1
       YPL(1)=Y0+SP*CT*U1+CP*V1+SP*ST*W1
       ZPL(1)=Z0   -ST*U1         +CT*W1
       U1=+XL
       V1=-YL
       W1=0
       XPL(2)=X0+CP*CT*U1-SP*V1+CP*ST*W1
       YPL(2)=Y0+SP*CT*U1+CP*V1+SP*ST*W1
       ZPL(2)=Z0   -ST*U1         +CT*W1
       U1=XH
       V1=-YL
       W1=ZH
       XPL(3)=X0+CP*CT*U1-SP*V1+CP*ST*W1
       YPL(3)=Y0+SP*CT*U1+CP*V1+SP*ST*W1
       ZPL(3)=Z0   -ST*U1         +CT*W1
       CALL COLWGT(SP,-CP,0.0D0,WW)
       IF(WW.GE.0)THEN
            ICOL=IOFCOL+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
       ELSE
            ICOL=IOFCOL
       ENDIF
       CALL PLABU1('STORE',IRPL,3,XPL,YPL,ZPL,SP,-CP,0.0D0,
     -      ICOL,IVOL,IFAIL)
       IF(IFAIL.NE.0)PRINT *,' !!!!!! PLATBP WARNING : Unable to'//
     -      ' store a panel of a ridge.'
*  Side triangle at y=ymax
       U1=-XL
       V1=+YL
       W1=0
       XPL(1)=X0+CP*CT*U1-SP*V1+CP*ST*W1
       YPL(1)=Y0+SP*CT*U1+CP*V1+SP*ST*W1
       ZPL(1)=Z0   -ST*U1         +CT*W1
       U1=+XL
       V1=+YL
       W1=0
       XPL(2)=X0+CP*CT*U1-SP*V1+CP*ST*W1
       YPL(2)=Y0+SP*CT*U1+CP*V1+SP*ST*W1
       ZPL(2)=Z0   -ST*U1         +CT*W1
       U1=XH
       V1=+YL
       W1=ZH
       XPL(3)=X0+CP*CT*U1-SP*V1+CP*ST*W1
       YPL(3)=Y0+SP*CT*U1+CP*V1+SP*ST*W1
       ZPL(3)=Z0   -ST*U1         +CT*W1
       CALL COLWGT(-SP,CP,0.0D0,WW)
       IF(WW.GE.0)THEN
            ICOL=IOFCOL+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
       ELSE
            ICOL=IOFCOL
       ENDIF
       CALL PLABU1('STORE',IRPL,3,XPL,YPL,ZPL,-SP,CP,0.0D0,
     -      ICOL,IVOL,IFAIL)
       IF(IFAIL.NE.0)PRINT *,' !!!!!! PLATBP WARNING : Unable to'//
     -      ' store a panel of a ridge.'
*   The roof, part at +x
       U1=+XL
       V1=-YL
       W1=0
       XPL(1)=X0+CP*CT*U1-SP*V1+CP*ST*W1
       YPL(1)=Y0+SP*CT*U1+CP*V1+SP*ST*W1
       ZPL(1)=Z0   -ST*U1         +CT*W1
       U1=+XL
       V1=+YL
       W1=0
       XPL(2)=X0+CP*CT*U1-SP*V1+CP*ST*W1
       YPL(2)=Y0+SP*CT*U1+CP*V1+SP*ST*W1
       ZPL(2)=Z0   -ST*U1         +CT*W1
       U1=XH
       V1=+YL
       W1=ZH
       XPL(3)=X0+CP*CT*U1-SP*V1+CP*ST*W1
       YPL(3)=Y0+SP*CT*U1+CP*V1+SP*ST*W1
       ZPL(3)=Z0   -ST*U1         +CT*W1
       U1=XH
       V1=-YL
       W1=ZH
       XPL(4)=X0+CP*CT*U1-SP*V1+CP*ST*W1
       YPL(4)=Y0+SP*CT*U1+CP*V1+SP*ST*W1
       ZPL(4)=Z0   -ST*U1         +CT*W1
       XROOF=    ZH /SQRT(ZH**2+(XL-XH)**2)
       ZROOF=(XL-XH)/SQRT(ZH**2+(XL-XH)**2)
       CALL COLWGT(CP*CT*XROOF   +CP*ST*ZROOF,
     -             SP*CT*XROOF   +SP*ST*ZROOF,
     -               -ST*XROOF      +CT*ZROOF,WW)
       IF(WW.GE.0)THEN
            ICOL=IOFCOL+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
       ELSE
            ICOL=IOFCOL
       ENDIF
       CALL PLABU1('STORE',IRPL,4,XPL,YPL,ZPL,
     -      CP*CT*XROOF   +CP*ST*ZROOF,
     -      SP*CT*XROOF   +SP*ST*ZROOF,
     -        -ST*XROOF      +CT*ZROOF,
     -      ICOL,IVOL,IFAIL)
       IF(IFAIL.NE.0)PRINT *,' !!!!!! PLATBP WARNING : Unable to'//
     -      ' store a panel of a ridge.'
*   The roof, part at -x
       U1=-XL
       V1=-YL
       W1=0
       XPL(1)=X0+CP*CT*U1-SP*V1+CP*ST*W1
       YPL(1)=Y0+SP*CT*U1+CP*V1+SP*ST*W1
       ZPL(1)=Z0   -ST*U1         +CT*W1
       U1=-XL
       V1=+YL
       W1=0
       XPL(2)=X0+CP*CT*U1-SP*V1+CP*ST*W1
       YPL(2)=Y0+SP*CT*U1+CP*V1+SP*ST*W1
       ZPL(2)=Z0   -ST*U1         +CT*W1
       U1=XH
       V1=+YL
       W1=ZH
       XPL(3)=X0+CP*CT*U1-SP*V1+CP*ST*W1
       YPL(3)=Y0+SP*CT*U1+CP*V1+SP*ST*W1
       ZPL(3)=Z0   -ST*U1         +CT*W1
       U1=XH
       V1=-YL
       W1=ZH
       XPL(4)=X0+CP*CT*U1-SP*V1+CP*ST*W1
       YPL(4)=Y0+SP*CT*U1+CP*V1+SP*ST*W1
       ZPL(4)=Z0   -ST*U1         +CT*W1
       XROOF=   -ZH /SQRT(ZH**2+(XL+XH)**2)
       ZROOF=(XL+XH)/SQRT(ZH**2+(XL+XH)**2)
       CALL COLWGT(CP*CT*XROOF   +CP*ST*ZROOF,
     -             SP*CT*XROOF   +SP*ST*ZROOF,
     -               -ST*XROOF      +CT*ZROOF,WW)
       IF(WW.GE.0)THEN
            ICOL=IOFCOL+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
       ELSE
            ICOL=IOFCOL
       ENDIF
       CALL PLABU1('STORE',IRPL,4,XPL,YPL,ZPL,
     -      CP*CT*XROOF   +CP*ST*ZROOF,
     -      SP*CT*XROOF   +SP*ST*ZROOF,
     -        -ST*XROOF      +CT*ZROOF,
     -      ICOL,IVOL,IFAIL)
       IF(IFAIL.NE.0)PRINT *,' !!!!!! PLATBP WARNING : Unable to'//
     -      ' store a panel of a ridge.'
*** Look for intersections with the outside ridge, x=xmin.
       CALL PLATBC(IVOL,GXMIN,(GYMIN+GYMAX)/2,(GZMIN+GZMAX)/2,
     -      -1.0D0,0.0D0,0.0D0,IOFCOL+1)
*   x=xmax.
       CALL PLATBC(IVOL,GXMAX,(GYMIN+GYMAX)/2,(GZMIN+GZMAX)/2,
     -      +1.0D0,0.0D0,0.0D0,IOFCOL+1)
*   y=ymin.
       CALL PLATBC(IVOL,(GXMIN+GXMAX)/2,GYMIN,(GZMIN+GZMAX)/2,
     -      0.0D0,-1.0D0,0.0D0,IOFCOL+1)
*   y=ymax.
       CALL PLATBC(IVOL,(GXMIN+GXMAX)/2,GYMAX,(GZMIN+GZMAX)/2,
     -      0.0D0,+1.0D0,0.0D0,IOFCOL+1)
*   z=zmin.
       CALL PLATBC(IVOL,(GXMIN+GXMAX)/2,(GYMIN+GYMAX)/2,GZMIN,
     -      0.0D0,0.0D0,-1.0D0,IOFCOL+1)
*   z=zmax.
       CALL PLATBC(IVOL,(GXMIN+GXMAX)/2,(GYMIN+GYMAX)/2,GZMAX,
     -      0.0D0,0.0D0,+1.0D0,IOFCOL+1)
       END
