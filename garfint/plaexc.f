CDECK  ID>, PLAEXC.
       SUBROUTINE PLAEXC(IVOL,X0PL,Y0PL,Z0PL,APL,BPL,CPL,ICOL)
*-----------------------------------------------------------------------
*   PLAEXC - Cuts extrusion IVOL with a plane.
*   (Last changed on  8/10/11.)
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
       INTEGER IREF,N,IVOL,IFAIL,I,NPL,ICOL,IRPL
       DOUBLE PRECISION ZL,X0,Y0,Z0,A,B,C,CT,ST,CP,SP,FNORM,
     -      U,V,W,X1,Y1,Z1,X2,Y2,Z2,XPL(MXEDGE),YPL(MXEDGE),ZPL(MXEDGE),
     -      X0PL,Y0PL,Z0PL,APL,BPL,CPL,XCUT,YCUT,ZCUT
*** Locate the conductor.
       IF(IVOL.LT.1.OR.IVOL.GT.MXSOLI)THEN
            PRINT *,' !!!!!! PLAEXC WARNING : Volume reference is out'//
     -           ' of range ; not plotted.'
            RETURN
       ENDIF
       IREF=ISTART(IVOL)
       IF(IREF.LT.0.OR.IREF+9.GT.MXSBUF)THEN
            PRINT *,' !!!!!! PLAEXC WARNING : Volume address is out'//
     -           ' of range ; not plotted.'
            RETURN
       ENDIF
*** Locate the extrusion parameters, first the half length in z.
       ZL=ABS(CBUF(IREF+2))
*   Centre.
       X0=CBUF(IREF+3)
       Y0=CBUF(IREF+4)
       Z0=CBUF(IREF+5)
*   Direction vector.
       FNORM=SQRT(CBUF(IREF+6)**2+CBUF(IREF+7)**2+CBUF(IREF+8)**2)
       IF(FNORM.LE.0)THEN
            PRINT *,' !!!!!! PLAEXC WARNING : Extrusion ',IVOL,' has'//
     -           ' a zero norm direction vector; not plotted.'
            RETURN
       ENDIF
       A= CBUF(IREF+6)/FNORM
       B= CBUF(IREF+7)/FNORM
       C= CBUF(IREF+8)/FNORM
*   Number of points
       N = NINT(CBUF(IREF+9))
       IF(N.GT.MXEDGE.OR.IREF+23+2*N.GT.MXSBUF)THEN
            PRINT *,' !!!!!! PLAEXC WARNING : Volume address is out'//
     -           ' of range ; not checked.'
            RETURN
       ENDIF
*   Shorthand for the rotations.
       CT=CBUF(IREF+10)
       ST=CBUF(IREF+11)
       CP=CBUF(IREF+12)
       SP=CBUF(IREF+13)
*   Debugging.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLAEXC DEBUG   : Drawing'',
     -      '' an extrusion of volume '',I4/26X,
     -      '', N='',I5,'', Half-length='',E10.3/26X,''Centre='',3E10.3/
     -      26X,''Direction='',3E10.3)') IVOL,N,ZL,X0,Y0,Z0,A,B,C
*** Initialise the number of points.
       NPL=0
*** Go through the lines of the top lid, first point.
       U=CBUF(IREF+23+2*N-1)
       V=CBUF(IREF+23+2*N)
       W=ZL
       X1=X0+CP*CT*U-SP*V+CP*ST*W
       Y1=Y0+SP*CT*U+CP*V+SP*ST*W
       Z1=Z0   -ST*U        +CT*W
*   Loop over the points.
       DO 10 I=1,N
*   Local coordinates,
       U=CBUF(IREF+23+2*I-1)
       V=CBUF(IREF+23+2*I)
       W=ZL
*   Rotate into place.
       X2=X0+CP*CT*U-SP*V+CP*ST*W
       Y2=Y0+SP*CT*U+CP*V+SP*ST*W
       Z2=Z0   -ST*U        +CT*W
*   Cut with the plane.
       CALL PLALIN(X1,Y1,Z1,X2,Y2,Z2,X0PL,Y0PL,Z0PL,APL,BPL,CPL,
     -      XCUT,YCUT,ZCUT,IFAIL)
*   Store the result if there is one.
       IF(IFAIL.EQ.0.AND.NPL.LE.MXEDGE)THEN
            NPL=NPL+1
            XPL(NPL)=XCUT
            YPL(NPL)=YCUT
            ZPL(NPL)=ZCUT
       ELSEIF(NPL.GE.MXEDGE)THEN
            PRINT *,' !!!!!! PLAEXC WARNING : Too many intersects'//
     -           ' between extrusion and plane; stopped.'
            RETURN
       ENDIF
*   Shift the coordinates.
       X1=X2
       Y1=Y2
       Z1=Z2
10     CONTINUE
*** Go through the lines of the bottom lid, first point.
       IF(ZL.GT.0)THEN
            U=CBUF(IREF+23+2*N-1)
            V=CBUF(IREF+23+2*N)
            W=-ZL
            X1=X0+CP*CT*U-SP*V+CP*ST*W
            Y1=Y0+SP*CT*U+CP*V+SP*ST*W
            Z1=Z0   -ST*U        +CT*W
*   Loop over the points.
            DO 20 I=1,N
*   Local coordinates,
            U=CBUF(IREF+23+2*I-1)
            V=CBUF(IREF+23+2*I)
            W=-ZL
*   Rotate into place.
            X2=X0+CP*CT*U-SP*V+CP*ST*W
            Y2=Y0+SP*CT*U+CP*V+SP*ST*W
            Z2=Z0   -ST*U        +CT*W
*   Cut with the plane.
            CALL PLALIN(X1,Y1,Z1,X2,Y2,Z2,X0PL,Y0PL,Z0PL,APL,BPL,CPL,
     -           XCUT,YCUT,ZCUT,IFAIL)
*   Store the result if there is one.
            IF(IFAIL.EQ.0.AND.NPL.LE.MXEDGE)THEN
                 NPL=NPL+1
                 XPL(NPL)=XCUT
                 YPL(NPL)=YCUT
                 ZPL(NPL)=ZCUT
            ELSEIF(NPL.GE.MXEDGE)THEN
                 PRINT *,' !!!!!! PLAEXC WARNING : Too many'//
     -                ' intersects between extrusion and plane;'//
     -                ' stopped.'
                 RETURN
            ENDIF
*   Shift the coordinates.
            X1=X2
            Y1=Y2
            Z1=Z2
20          CONTINUE
       ENDIF
*** Go through the ribs.
       DO 30 I=1,N
*   Bottom and top of the line along the axis of the extrusion.
       U=CBUF(IREF+23+2*N-1)
       V=CBUF(IREF+23+2*N)
       W=ZL
*   Rotated into place.
       X1=X0+CP*CT*U-SP*V-CP*ST*W
       Y1=Y0+SP*CT*U+CP*V-SP*ST*W
       Z1=Z0   -ST*U        -CT*W
       X2=X0+CP*CT*U-SP*V+CP*ST*W
       Y2=Y0+SP*CT*U+CP*V+SP*ST*W
       Z2=Z0   -ST*U        +CT*W
*   Cut with the plane.
       CALL PLALIN(X1,Y1,Z1,X2,Y2,Z2,X0PL,Y0PL,Z0PL,APL,BPL,CPL,
     -      XCUT,YCUT,ZCUT,IFAIL)
*   Store the result if there is one.
       IF(IFAIL.EQ.0.AND.NPL.LE.MXEDGE)THEN
            NPL=NPL+1
            XPL(NPL)=XCUT
            YPL(NPL)=YCUT
            ZPL(NPL)=ZCUT
       ELSEIF(NPL.GE.MXEDGE)THEN
            PRINT *,' !!!!!! PLAEXC WARNING : Too many intersects'//
     -           ' between extrusion and plane; stopped.'
            RETURN
       ENDIF
30     CONTINUE
*** Get rid of butterflies.
       CALL BUTFLD(NPL,XPL,YPL,ZPL)
*** Store the plane.
       IF(NPL.GE.3)THEN
            CALL PLABU1('STORE',IRPL,NPL,XPL,YPL,ZPL,
     -           APL,BPL,CPL,ICOL,IVOL,IFAIL)
            IF(IFAIL.NE.0)PRINT *,' !!!!!! PLAEXC WARNING : Failed to'//
     -           ' store a side cut of an extrusion.'
       ENDIF
       END
