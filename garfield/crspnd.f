CDECK  ID>, CRSPND.
       SUBROUTINE CRSPND(X1,Y1,X2,Y2,U1,V1,U2,V2,XC,YC,CROSS)
*-----------------------------------------------------------------------
*   CRSPND - Determines whether the 2 straight lines (X1,Y1) to (X2,Y2)
*            and (U1,U2) to (V1,V2) cross at an intermediate point for
*            both lines. The variables have been introduced to make this
*            already elementary routine more understandable.
*   VARIABLES : A           : Matrix storing direction vectors.
*               DET         : Determinant of A.
*               EPS         : Minimum value for DET to be non-zero.
*   (Last changed on  3/ 9/98.)
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
       DOUBLE PRECISION X1,X2,Y1,Y2,U1,U2,V1,V2,A(2,2),DET,EPSX,EPSY,
     -      AUX,XC,YC
       LOGICAL ONLIND,CROSS
       EXTERNAL ONLIND
*** Matrix to compute the crossing point.
       A(1,1)=Y2-Y1
       A(2,1)=V2-V1
       A(1,2)=X1-X2
       A(2,2)=U1-U2
       DET=A(1,1)*A(2,2)-A(1,2)*A(2,1)
*** Initial values.
       XC=0
       YC=0
*** Set tolerances.
       IF(LEPSG)THEN
            EPSX=EPSGX
            EPSY=EPSGY
       ELSE
            EPSX=1.0D-10*MAX(ABS(X1),ABS(X2),ABS(U1),ABS(U2))
            EPSY=1.0D-10*MAX(ABS(Y1),ABS(Y2),ABS(V1),ABS(V2))
            IF(EPSX.LE.0)EPSX=1.0D-10
            IF(EPSY.LE.0)EPSY=1.0D-10
       ENDIF
*   Verify the tolerances.
       IF(EPSX.LE.0.OR.EPSY.LE.0)THEN
            PRINT *,' !!!!!! CRSPND WARNING : Tolerances not'//
     -           ' > 0; returning False.'
            CROSS=.FALSE.
            RETURN
       ENDIF
*** Check for a point of one line located on the other line.
       IF(ONLIND(X1,Y1,X2,Y2,U1,V1))THEN
C      print *,' Point on other line'
            XC=U1
            YC=V1
            CROSS=.TRUE.
       ELSEIF(ONLIND(X1,Y1,X2,Y2,U2,V2))THEN
C      print *,' Point on other line'
            XC=U2
            YC=V2
            CROSS=.TRUE.
       ELSEIF(ONLIND(U1,V1,U2,V2,X1,Y1))THEN
C      print *,' Point on other line'
            XC=X1
            YC=Y1
            CROSS=.TRUE.
       ELSEIF(ONLIND(U1,V1,U2,V2,X2,Y2))THEN
C      print *,' Point on other line'
            XC=X2
            YC=Y2
            CROSS=.TRUE.
*** Otherwise parallel lines do not cross.
       ELSEIF(ABS(DET).LT.EPSX*EPSY)THEN
C      print *,' Parallel, non-touching'
            CROSS=.FALSE.
       ELSE
*** Crossing, non-trivial lines: solve crossing equations.
            AUX=A(2,2)
            A(2,2)=A(1,1)/DET
            A(1,1)=AUX/DET
            A(1,2)=-A(1,2)/DET
            A(2,1)=-A(2,1)/DET
*   Compute crossing point.
            XC=A(1,1)*(X1*Y2-X2*Y1)+A(1,2)*(U1*V2-U2*V1)
            YC=A(2,1)*(X1*Y2-X2*Y1)+A(2,2)*(U1*V2-U2*V1)
*   See whether the crossing point is on both lines.
            IF(  ONLIND(X1,Y1,X2,Y2,XC,YC).AND.
     -           ONLIND(U1,V1,U2,V2,XC,YC))THEN
C      print *,' Intersecting lines at ',xc,yc
                 CROSS=.TRUE.
            ELSE
C      print *,' Crossing point not on both lines ',xc,yc
                 CROSS=.FALSE.
            ENDIF
       ENDIF
       END
