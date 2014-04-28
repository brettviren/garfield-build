CDECK  ID>, ONLINE.
       LOGICAL FUNCTION ONLINE(X1S,Y1S,X2S,Y2S,US,VS)
*-----------------------------------------------------------------------
*   ONLINE - Determines whether a point (U,V) lies on the straight lines
*            (X1,Y1) to (X2,Y2).
*   (Last changed on 22/ 9/98.)
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
       DOUBLE PRECISION X1,X2,Y1,Y2,U,V,XC,YC,XL,EPSX,EPSY
       REAL X1S,Y1S,X2S,Y2S,US,VS
*** Convert input (single precision) variables to double precision.
       X1=DBLE(X1S)
       X2=DBLE(X2S)
       Y1=DBLE(Y1S)
       Y2=DBLE(Y2S)
       U=DBLE(US)
       V=DBLE(VS)
*** Set tolerances.
       IF(LEPSG)THEN
            EPSX=EPSGX
            EPSY=EPSGY
C      print *,' Using set tolerances: ',epsx,epsy
       ELSE
            EPSX=1.0D-5*MAX(ABS(X1),ABS(X2),ABS(U))
            EPSY=1.0D-5*MAX(ABS(Y1),ABS(Y2),ABS(V))
            IF(EPSX.LE.0)EPSX=1.0D-5
            IF(EPSY.LE.0)EPSY=1.0D-5
C      print *,' Setting tolerances: ',epsx,epsy
       ENDIF
*   Verify the tolerances.
       IF(EPSX.LE.0.OR.EPSY.LE.0)THEN
            PRINT *,' !!!!!! ONLINE WARNING : Tolerances not'//
     -           ' > 0; returning False.'
            ONLINE=.FALSE.
            RETURN
       ENDIF
*** Point to be examined coincides with start or end,
       IF((ABS(X1-U).LE.EPSX.AND.ABS(Y1-V).LE.EPSY).OR.
     -      (ABS(X2-U).LE.EPSX.AND.ABS(Y2-V).LE.EPSY))THEN
            ONLINE=.TRUE.
            RETURN
*** The line (X1,Y1) to (X2,Y2) is in fact a point.
       ELSEIF(ABS(X1-X2).LE.EPSX.AND.ABS(Y1-Y2).LE.EPSY)THEN
            ONLINE=.FALSE.
            RETURN
*** (U,V) is nearer to (X1,Y1).
       ELSEIF(ABS(U-X1)+ABS(V-Y1).LT.ABS(U-X2)+ABS(V-Y2))THEN
C      print *,' Nearer to point 1'
            XL=((U-X1)*(X2-X1)+(V-Y1)*(Y2-Y1))/((X2-X1)**2+(Y2-Y1)**2)
            IF(XL.LT.0.0D0)THEN
                 XC=X1
                 YC=Y1
            ELSEIF(XL.GT.1.0D0)THEN
                 XC=X2
                 YC=Y2
            ELSE
                 XC=X1+XL*(X2-X1)
                 YC=Y1+XL*(Y2-Y1)
            ENDIF
*** (U,V) is nearer to (X2,Y2).
       ELSE
C      print *,' Nearer to point 2'
            XL=((U-X2)*(X1-X2)+(V-Y2)*(Y1-Y2))/((X2-X1)**2+(Y2-Y1)**2)
            IF(XL.LT.0.0D0)THEN
                 XC=X2
                 YC=Y2
            ELSEIF(XL.GT.1.0D0)THEN
                 XC=X1
                 YC=Y1
            ELSE
                 XC=X2+XL*(X1-X2)
                 YC=Y2+XL*(Y1-Y2)
            ENDIF
       ENDIF
C      print *,' Nearest point: ',xc,yc
*** See whether the point is on the line.
       IF(ABS(U-XC).LT.EPSX.AND.ABS(V-YC).LT.EPSY)THEN
            ONLINE=.TRUE.
       ELSE
            ONLINE=.FALSE.
       ENDIF
       END
