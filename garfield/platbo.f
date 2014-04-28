CDECK  ID>, PLATBO.
       SUBROUTINE PLATBO(IVOL)
*-----------------------------------------------------------------------
*   PLATBO - Plots the outlines of a ridge.
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER IREF,IVOL
       DOUBLE PRECISION X0,Y0,Z0,XL,YL,XH,ZH,CT,ST,CP,SP,U,V,W,
     -      XPL(5),YPL(5),ZPL(5)
*** Locate the conductor.
       IF(IVOL.LT.1.OR.IVOL.GT.MXSOLI)THEN
            PRINT *,' !!!!!! PLATBO WARNING : Volume reference is out'//
     -           ' of range ; not plotted.'
            RETURN
       ENDIF
       IREF=ISTART(IVOL)
       IF(IREF.LT.0.OR.IREF+13.GT.MXSBUF)THEN
            PRINT *,' !!!!!! PLATBO WARNING : Volume address is out'//
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
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLATBO DEBUG   : Outlining'',
     -      '' a ridge from address '',I4/
     -      26X,''Centre=      '',3E10.3/
     -      26X,''Half-lengths='',2E10.3/
     -      26X,''Ridge       ='',2E10.3)') IREF,X0,Y0,Z0,XL,YL,XH,ZH
*   Shorthand for the rotations.
       CT=CBUF(IREF+10)
       ST=CBUF(IREF+11)
       CP=CBUF(IREF+12)
       SP=CBUF(IREF+13)
*** The z=zmin face.
       U=-XL
       V=-YL
       W=0
       XPL(1)=X0+CP*CT*U-SP*V+CP*ST*W
       YPL(1)=Y0+SP*CT*U+CP*V+SP*ST*W
       ZPL(1)=Z0   -ST*U        +CT*W
       U=-XL
       V=+YL
       W=0
       XPL(2)=X0+CP*CT*U-SP*V+CP*ST*W
       YPL(2)=Y0+SP*CT*U+CP*V+SP*ST*W
       ZPL(2)=Z0   -ST*U        +CT*W
       U=+XL
       V=+YL
       W=0
       XPL(3)=X0+CP*CT*U-SP*V+CP*ST*W
       YPL(3)=Y0+SP*CT*U+CP*V+SP*ST*W
       ZPL(3)=Z0   -ST*U        +CT*W
       U=+XL
       V=-YL
       W=0
       XPL(4)=X0+CP*CT*U-SP*V+CP*ST*W
       YPL(4)=Y0+SP*CT*U+CP*V+SP*ST*W
       ZPL(4)=Z0   -ST*U        +CT*W
       XPL(5)=XPL(1)
       YPL(5)=YPL(1)
       ZPL(5)=ZPL(1)
       CALL PLAGPL(5,XPL,YPL,ZPL)
*** The triangle at y=ymin
       U=-XL
       V=-YL
       W=0
       XPL(1)=X0+CP*CT*U-SP*V+CP*ST*W
       YPL(1)=Y0+SP*CT*U+CP*V+SP*ST*W
       ZPL(1)=Z0   -ST*U        +CT*W
       U=XH
       V=-YL
       W=ZH
       XPL(2)=X0+CP*CT*U-SP*V+CP*ST*W
       YPL(2)=Y0+SP*CT*U+CP*V+SP*ST*W
       ZPL(2)=Z0   -ST*U        +CT*W
       U=+XL
       V=-YL
       W=0
       XPL(3)=X0+CP*CT*U-SP*V+CP*ST*W
       YPL(3)=Y0+SP*CT*U+CP*V+SP*ST*W
       ZPL(3)=Z0   -ST*U        +CT*W
       CALL PLAGPL(3,XPL,YPL,ZPL)
*** The triangle at y=ymax
       U=-XL
       V=+YL
       W=0
       XPL(1)=X0+CP*CT*U-SP*V+CP*ST*W
       YPL(1)=Y0+SP*CT*U+CP*V+SP*ST*W
       ZPL(1)=Z0   -ST*U        +CT*W
       U=XH
       V=+YL
       W=ZH
       XPL(2)=X0+CP*CT*U-SP*V+CP*ST*W
       YPL(2)=Y0+SP*CT*U+CP*V+SP*ST*W
       ZPL(2)=Z0   -ST*U        +CT*W
       U=+XL
       V=+YL
       W=0
       XPL(3)=X0+CP*CT*U-SP*V+CP*ST*W
       YPL(3)=Y0+SP*CT*U+CP*V+SP*ST*W
       ZPL(3)=Z0   -ST*U        +CT*W
       CALL PLAGPL(3,XPL,YPL,ZPL)
*** The roof ridge
       U=XH
       V=-YL
       W=ZH
       XPL(1)=X0+CP*CT*U-SP*V+CP*ST*W
       YPL(1)=Y0+SP*CT*U+CP*V+SP*ST*W
       ZPL(1)=Z0   -ST*U        +CT*W
       U=XH
       V=+YL
       W=ZH
       XPL(2)=X0+CP*CT*U-SP*V+CP*ST*W
       YPL(2)=Y0+SP*CT*U+CP*V+SP*ST*W
       ZPL(2)=Z0   -ST*U        +CT*W
       CALL PLAGPL(2,XPL,YPL,ZPL)
       END
