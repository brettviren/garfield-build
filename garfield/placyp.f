CDECK  ID>, PLACYP.
       SUBROUTINE PLACYP(IVOL,IOFCOL)
*-----------------------------------------------------------------------
*   PLACYP - Generates a table of polygons for a cylinder.
*   (Last changed on  9/10/11.)
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
       INTEGER IREF,IOFCOL,NMAX,N,IVOL,ICOL,IFAIL,I,IRPL
       PARAMETER(NMAX=MXEDGE/4-1)
       DOUBLE PRECISION R,ZL,X0,Y0,Z0,A,B,C,CT,ST,CP,SP,FNORM,WW,
     -      U,V,W,XPL(MXEDGE),YPL(MXEDGE),ZPL(MXEDGE),AROT, ALPHA
*** Locate the conductor.
       IF(IVOL.LT.1.OR.IVOL.GT.MXSOLI)THEN
            PRINT *,' !!!!!! PLACYP WARNING : Volume reference is out'//
     -           ' of range ; not plotted.'
            RETURN
       ENDIF
       IREF=ISTART(IVOL)
       IF(IREF.LT.0.OR.IREF+23.GT.MXSBUF)THEN
            PRINT *,' !!!!!! PLACYP WARNING : Volume address is out'//
     -           ' of range ; not plotted.'
            RETURN
       ENDIF
*** Locate the cylinder parameters, first the radius.
       R= CBUF(IREF+1)
       IF(R.LE.0)THEN
            PRINT *,' !!!!!! PLACYP WARNING : Cylinder ',IVOL,' has a'//
     -           ' non-positive radius; not plotted.'
            RETURN
       ENDIF
*   Half length in z.
       ZL=ABS(CBUF(IREF+2))
*   Centre.
       X0=CBUF(IREF+3)
       Y0=CBUF(IREF+4)
       Z0=CBUF(IREF+5)
*   Direction vector.
       FNORM=SQRT(CBUF(IREF+6)**2+CBUF(IREF+7)**2+CBUF(IREF+8)**2)
       IF(FNORM.LE.0)THEN
            PRINT *,' !!!!!! PLACYP WARNING : Cylinder ',IVOL,' has a'//
     -           ' zero norm direction vector; not plotted.'
            RETURN
       ENDIF
       A= CBUF(IREF+6)/FNORM
       B= CBUF(IREF+7)/FNORM
       C= CBUF(IREF+8)/FNORM
*   Shorthand for the rotations.
       CT=CBUF(IREF+10)
       ST=CBUF(IREF+11)
       CP=CBUF(IREF+12)
       SP=CBUF(IREF+13)
*   Axial rotation.
       AROT=CBUF(IREF+14)
*   Determine a suitable number of points on the radii.
       N=MIN(NMAX-1,NINT(CBUF(IREF+9)))
       IF(N.LT.1)THEN
            IF(R.LT.1E-2*MAX(ABS(GXMAX-GXMIN),ABS(GYMAX-GYMIN),
     -           ABS(GZMAX-GZMIN)))THEN
                 N=MIN(MXEDGE-3,NMAX-1,2)
            ELSEIF(R.LT.1E-1*MAX(ABS(GXMAX-GXMIN),ABS(GYMAX-GYMIN),
     -           ABS(GZMAX-GZMIN)))THEN
                 N=MIN(MXEDGE-3,NMAX-1,3)
            ELSE
                 N=MIN(MXEDGE-3,NMAX-1,4)
            ENDIF
       ENDIF
*   Set the mean radius if desired.
       IF(CBUF(IREF+24).GT.0.5)THEN
            ALPHA = PI/(4.0D0*DBLE(N-1))
            R = 2*R/(1.0D0 + ASINH(TAN(ALPHA))*COS(ALPHA)/TAN(ALPHA))
       ENDIF
*   Debugging.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLACYP DEBUG   : Drawing a'',
     -      '' cylinder of volume '',I4/26X,''Radius='',E10.3,
     -      '', N='',I5,'', Half-length='',E10.3/26X,''Centre='',3E10.3/
     -      26X,''Direction='',3E10.3)') IVOL,R,N,ZL,X0,Y0,Z0,A,B,C
*** Create the top lid.
       IF(CBUF(IREF+22).GT.0.5)THEN
            DO 10 I=2,4*N-3
*   Local coordinates,
            U=R*COS(AROT+(PI/2)*DBLE(I-1)/DBLE(N-1))
            V=R*SIN(AROT+(PI/2)*DBLE(I-1)/DBLE(N-1))
            W=ZL
*   Rotate into place.
            XPL(I-1)=X0+CP*CT*U-SP*V+CP*ST*W
            YPL(I-1)=Y0+SP*CT*U+CP*V+SP*ST*W
            ZPL(I-1)=Z0   -ST*U        +CT*W
10          CONTINUE
*   Compute colour index.
            CALL COLWGT(A,B,C,WW)
            IF(WW.GT.0)THEN
                 ICOL=IOFCOL+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
            ELSE
                 ICOL=IOFCOL
            ENDIF
*   Store the plane.
            CALL PLABU1('STORE',IRPL,4*N-4,XPL,YPL,ZPL,A,B,C,ICOL,IVOL,
     -           IFAIL)
            IF(IFAIL.NE.0)PRINT *,' !!!!!! PLACYP WARNING : Unable to'//
     -           ' store the top lid of a cylinder.'
       ENDIF
*** Create the bottom lid.
       IF(CBUF(IREF+23).GT.0.5)THEN
            DO 20 I=2,4*N-3
*   Local coordinates,
            U=R*COS(AROT+(PI/2)*DBLE(I-1)/DBLE(N-1))
            V=R*SIN(AROT+(PI/2)*DBLE(I-1)/DBLE(N-1))
            W=-ZL
*   Rotate into place.
            XPL(I-1)=X0+CP*CT*U-SP*V+CP*ST*W
            YPL(I-1)=Y0+SP*CT*U+CP*V+SP*ST*W
            ZPL(I-1)=Z0   -ST*U        +CT*W
20          CONTINUE
*   Compute colour index.
            CALL COLWGT(-A,-B,-C,WW)
            IF(WW.GT.0)THEN
                 ICOL=IOFCOL+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
            ELSE
                 ICOL=IOFCOL
            ENDIF
*   Store the plane.
            CALL PLABU1('STORE',IRPL,4*N-4,XPL,YPL,ZPL,-A,-B,-C,
     -           ICOL,IVOL,IFAIL)
            IF(IFAIL.NE.0)PRINT *,' !!!!!! PLACYP WARNING : Unable to'//
     -           ' store the bottom lid of a cylinder.'
       ENDIF
*** Create the side panels.
       U=R*COS(AROT+(PI/2)*DBLE(0)/DBLE(N-1))
       V=R*SIN(AROT+(PI/2)*DBLE(0)/DBLE(N-1))
       W=ZL
*   Rotate into place.
       XPL(1)=X0+CP*CT*U-SP*V+CP*ST*W
       YPL(1)=Y0+SP*CT*U+CP*V+SP*ST*W
       ZPL(1)=Z0   -ST*U        +CT*W
       XPL(2)=X0+CP*CT*U-SP*V-CP*ST*W
       YPL(2)=Y0+SP*CT*U+CP*V-SP*ST*W
       ZPL(2)=Z0   -ST*U        -CT*W
**  Go around the cylinder.
       DO 30 I=2,4*N-3
*   Bottom and top of the line along the axis of the cylinder.
       U=R*COS(AROT+(PI/2)*DBLE(I-1)/DBLE(N-1))
       V=R*SIN(AROT+(PI/2)*DBLE(I-1)/DBLE(N-1))
       W=ZL
*   Rotated into place.
       XPL(3)=X0+CP*CT*U-SP*V-CP*ST*W
       YPL(3)=Y0+SP*CT*U+CP*V-SP*ST*W
       ZPL(3)=Z0   -ST*U        -CT*W
       XPL(4)=X0+CP*CT*U-SP*V+CP*ST*W
       YPL(4)=Y0+SP*CT*U+CP*V+SP*ST*W
       ZPL(4)=Z0   -ST*U        +CT*W
*   Compute the colour index for this segment.
       CALL COLWGT(CP*CT*COS(AROT+(PI/2)*DBLE(I-1.5)/DBLE(N-1))-
     -      SP*          SIN(AROT+(PI/2)*DBLE(I-1.5)/DBLE(N-1)),
     -      SP*CT*       COS(AROT+(PI/2)*DBLE(I-1.5)/DBLE(N-1))+
     -      CP*          SIN(AROT+(PI/2)*DBLE(I-1.5)/DBLE(N-1)),
     -     -ST*          COS(AROT+(PI/2)*DBLE(I-1.5)/DBLE(N-1)),WW)
       IF(WW.GT.0)THEN
            ICOL=IOFCOL+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
       ELSE
            ICOL=IOFCOL
       ENDIF
*   Store the plane.
       CALL PLABU1('STORE',IRPL,4,XPL,YPL,ZPL,
     -      CP*CT*COS(AROT+(PI/2)*DBLE(I-1.5)/DBLE(N-1))-
     -      SP*   SIN(AROT+(PI/2)*DBLE(I-1.5)/DBLE(N-1)),
     -      SP*CT*COS(AROT+(PI/2)*DBLE(I-1.5)/DBLE(N-1))+
     -      CP*   SIN(AROT+(PI/2)*DBLE(I-1.5)/DBLE(N-1)),
     -     -ST*   COS(AROT+(PI/2)*DBLE(I-1.5)/DBLE(N-1)),
     -      ICOL,IVOL,IFAIL)
       IF(IFAIL.NE.0)PRINT *,' !!!!!! PLACYP WARNING : Unable to'//
     -      ' store a panel of a cylinder.'
*   Shift the points.
       XPL(1)=XPL(4)
       YPL(1)=YPL(4)
       ZPL(1)=ZPL(4)
       XPL(2)=XPL(3)
       YPL(2)=YPL(3)
       ZPL(2)=ZPL(3)
30     CONTINUE
*** Look for intersections with the outside box, x=xmin.
       CALL PLACYC(IVOL,GXMIN,(GYMIN+GYMAX)/2,(GZMIN+GZMAX)/2,
     -      -1.0D0,0.0D0,0.0D0,IOFCOL+1)
*   x=xmax.
       CALL PLACYC(IVOL,GXMAX,(GYMIN+GYMAX)/2,(GZMIN+GZMAX)/2,
     -      +1.0D0,0.0D0,0.0D0,IOFCOL+1)
*   y=ymin.
       CALL PLACYC(IVOL,(GXMIN+GXMAX)/2,GYMIN,(GZMIN+GZMAX)/2,
     -      0.0D0,-1.0D0,0.0D0,IOFCOL+1)
*   y=ymax.
       CALL PLACYC(IVOL,(GXMIN+GXMAX)/2,GYMAX,(GZMIN+GZMAX)/2,
     -      0.0D0,+1.0D0,0.0D0,IOFCOL+1)
*   z=zmin.
       CALL PLACYC(IVOL,(GXMIN+GXMAX)/2,(GYMIN+GYMAX)/2,GZMIN,
     -      0.0D0,0.0D0,-1.0D0,IOFCOL+1)
*   z=zmax.
       CALL PLACYC(IVOL,(GXMIN+GXMAX)/2,(GYMIN+GYMAX)/2,GZMAX,
     -      0.0D0,0.0D0,+1.0D0,IOFCOL+1)
       END
