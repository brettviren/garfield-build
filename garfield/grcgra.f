CDECK  ID>, GRCGRA.
       SUBROUTINE GRCGRA(F,XX,YY,DFDX,DFDY,IOPT1,IOPT2,IFLAG)
*-----------------------------------------------------------------------
*   GRCGRA - Calculates the (normalised) gradient of F at (XX,YY).
*   VARIABLES : IOPT1       : If 0, the normal gradient is returned,
*                             if 1, the orthognal gradient.
*               IOPT2       : If 0, no normalisation, if 1 normalisation
*                             on one grid length along the gradient.
*   (Last changed on 22/ 6/98.)
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
       LOGICAL XDONE(0:MXGRID,0:MXGRID),YDONE(0:MXGRID,0:MXGRID),
     -      TRANS,CLAB
       REAL GRID(0:MXGRID,0:MXGRID),EPSTRA,EPSGRA,CXMIN,CXMAX,CYMIN,
     -      CYMAX,STINIT,DNTHR,DXGRA,DYGRA
       INTEGER ILOCGR(0:MXGRID,0:MXGRID),NBITER,NNITER,NFC,NGCMAX
       COMMON /CONDAT/ GRID,XDONE,YDONE,ILOCGR,
     -      NBITER,NNITER,EPSTRA,EPSGRA,DXGRA,DYGRA,
     -      STINIT,DNTHR,CXMIN,CXMAX,CYMIN,CYMAX,NFC,NGCMAX,TRANS,CLAB
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
       REAL XX,YY,DFDX,DFDY,AUX,DFNORM,FXP,FXM,FYP,FYM,FM
       INTEGER IOPT1,IOPT2,ILOCXM,ILOCXP,ILOCYP,ILOCYM,ILOCM,IFLAG
       EXTERNAL F
*** Preset flag to 0: free point, change to 1 if needed.
       IFLAG=0
*** Function evaluation for the symmetric gradient.
       CALL F(XX+DXGRA,YY,FXP,ILOCXP)
       CALL F(XX-DXGRA,YY,FXM,ILOCXM)
       CALL F(XX,YY+DYGRA,FYP,ILOCYP)
       CALL F(XX,YY-DYGRA,FYM,ILOCYM)
       NFC=NFC+4
*   If one or more points are special, try asymmetric gradients.
       IF((ILOCXP.EQ.0.AND.ILOCXM.NE.0).OR.
     -      (ILOCXP.NE.0.AND.ILOCXM.EQ.0).OR.
     -      (ILOCYP.EQ.0.AND.ILOCYM.NE.0).OR.
     -      (ILOCYP.NE.0.AND.ILOCYM.EQ.0))THEN
            CALL F(XX,YY,FM,ILOCM)
            NFC=NFC+1
       ELSE
            FM=0
            ILOCM=-1
       ENDIF
*** Compute the symmetric x-gradient if this is possible.
       IF(ILOCXP.EQ.0.AND.ILOCXM.EQ.0)THEN
            DFDX=(FXP-FXM)/(2*DXGRA)
*   Abandon if there is no hope.
       ELSEIF(ILOCM.NE.0)THEN
            DFDX=0
            IFLAG=1
*   Take the +assymetric gradient.
       ELSEIF(ILOCXP.EQ.0)THEN
            DFDX=(FXP-FM)/DXGRA
*   Take the -assymetric gradient.
       ELSEIF(ILOCXM.EQ.0)THEN
            DFDX=(FM-FXM)/DXGRA
       ELSE
            WRITE(10,'(''  !!!!!! GRCGRA WARNING : Unexpected case'',
     -           '' computing an x-gradient.'')')
            IFLAG=1
       ENDIF
*** Compute the symmetric y-gradient if this is possible.
       IF(ILOCYP.EQ.0.AND.ILOCYM.EQ.0)THEN
            DFDY=(FYP-FYM)/(2*DYGRA)
*   Abandon if there is no hope.
       ELSEIF(ILOCM.NE.0)THEN
            DFDY=0
            IFLAG=1
*   Take the +assymetric gradient.
       ELSEIF(ILOCYP.EQ.0)THEN
            DFDY=(FYP-FM)/DYGRA
*   Take the -assymetric gradient.
       ELSEIF(ILOCYM.EQ.0)THEN
            DFDY=(FM-FYM)/DYGRA
       ELSE
            WRITE(10,'(''  !!!!!! GRCGRA WARNING : Unexpected case'',
     -           '' computing a y-gradient.'')')
            IFLAG=1
       ENDIF
*** Check the flag.
       IF(IFLAG.NE.0)THEN
            DFDX=0
            DFDY=0
            RETURN
       ENDIF
*** Check for a zero gradient for other reasons.
       IF(DFDX**2+DFDY**2.EQ.0)RETURN
*** Reverse the gradient in case of IOPT1=1.
       IF(IOPT1.EQ.1)THEN
            AUX=DFDX
            DFDX=-DFDY
            DFDY=AUX
       ENDIF
*** Normalise the gradient to one grid unit if IOPT2=1.
       IF(IOPT2.EQ.1)THEN
            DFNORM=SQRT(((DFDX*REAL(NGRIDX))/(CXMAX-CXMIN))**2+
     -           ((DFDY*REAL(NGRIDY))/(CYMAX-CYMIN))**2)
            DFDX=DFDX/DFNORM
            DFDY=DFDY/DFNORM
       ENDIF
       END
