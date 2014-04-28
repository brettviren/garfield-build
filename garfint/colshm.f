CDECK  ID>, COLSHM.
       SUBROUTINE COLSHM
*-----------------------------------------------------------------------
*   COLSHM - Plots a colour map for the shadowing effects.
*   (Last changed on 12/11/02.)
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
       REAL XPL(5),YPL(5),XMIN,YMIN,XMAX,YMAX
       INTEGER I,J,NC,NTAB
       CHARACTER*20 STR
*** Switch to graphics mode.
       CALL GRGRAF(.TRUE.)
*   Switch to normalised device coordinates.
       CALL GSELNT(0)
*** Attributes, start with the solid interior style.
       CALL GSFAIS(1)
*   Set reasonable character attributes.
       CALL GSTXFP(0,2)
       CALL GSCHXP(1.0)
       CALL GSCHSP(0.0)
       CALL GSCHH(0.012)
       CALL GSTXAL(2,3)
       CALL GSCHUP(0.0,1.0)
       CALL GSTXCI(1)
*   Set reasonable polyline attributes.
       CALL GSPLCI(1)
       CALL GSLN(1)
       CALL GSLWSC(1.0)
*** Loop over colour tables.
       NTAB=10
       DO 10 I=1,NTAB
*   Make sure this table exists.
       IF(  (I.EQ. 1.AND.ICOLBX.LE.0).OR.
     -      (I.EQ. 2.AND.ICOLPL.LE.0).OR.
     -      (I.EQ. 3.AND.ICOLW1.LE.0).OR.
     -      (I.EQ. 4.AND.ICOLW2.LE.0).OR.
     -      (I.EQ. 5.AND.ICOLW3.LE.0).OR.
     -      (I.EQ. 6.AND.ICOLD1.LE.0).OR.
     -      (I.EQ. 7.AND.ICOLD2.LE.0).OR.
     -      (I.EQ. 8.AND.ICOLD3.LE.0).OR.
     -      (I.EQ. 9.AND.ICOLST.LE.0).OR.
     -      (I.EQ.10.AND.ICOLRB.LE.0))GOTO 10
*   Set the horizontal extent covered by this table.
       XMIN=0.05+REAL(I-1)*0.91/REAL(NTAB)
       XMAX=0.05+REAL(I  )*0.91/REAL(NTAB)-0.01
*   Label the tables.
       IF(I.EQ.1)THEN
            CALL GTX(0.5*(XMIN+XMAX),0.95,'Box')
       ELSEIF(I.EQ.2)THEN
            CALL GTX(0.5*(XMIN+XMAX),0.95,'Planes')
       ELSEIF(I.EQ.3)THEN
            CALL GTX(0.5*(XMIN+XMAX),0.95,'Conductor 1')
       ELSEIF(I.EQ.4)THEN
            CALL GTX(0.5*(XMIN+XMAX),0.95,'Conductor 2')
       ELSEIF(I.EQ.5)THEN
            CALL GTX(0.5*(XMIN+XMAX),0.95,'Conductor 3')
       ELSEIF(I.EQ.6)THEN
            CALL GTX(0.5*(XMIN+XMAX),0.95,'Dielectric 1')
       ELSEIF(I.EQ.7)THEN
            CALL GTX(0.5*(XMIN+XMAX),0.95,'Dielectric 2')
       ELSEIF(I.EQ.8)THEN
            CALL GTX(0.5*(XMIN+XMAX),0.95,'Dielectric 3')
       ELSEIF(I.EQ.9)THEN
            CALL GTX(0.5*(XMIN+XMAX),0.95,'Strips')
       ELSEIF(I.EQ.10)THEN
            CALL GTX(0.5*(XMIN+XMAX),0.95,'Rainbow')
       ELSE
            CALL GTX(0.5*(XMIN+XMAX),0.95,'Unknown')
       ENDIF
*** Loop over the colours.
       DO 20 J=1,NPRCOL
       YMIN=0.1+REAL(J-1)*0.8/REAL(NPRCOL)
       YMAX=0.1+REAL(J  )*0.8/REAL(NPRCOL)
*   On first pass, label the colours.
       IF(I.EQ.1)THEN
            CALL OUTFMT(REAL(J),2,STR,NC,'LEFT')
            CALL GTX(0.025,0.5*(YMIN+YMAX),STR(1:NC))
       ENDIF
*   Plot a rectangle with the colour.
       XPL(1)=XMIN
       YPL(1)=YMIN
       XPL(2)=XMIN
       YPL(2)=YMAX
       XPL(3)=XMAX
       YPL(3)=YMAX
       XPL(4)=XMAX
       YPL(4)=YMIN
       XPL(5)=XMIN
       YPL(5)=YMIN
       IF(I.EQ.1)THEN
            CALL GSFACI(ICOLBX+J-1)
       ELSEIF(I.EQ.2)THEN
            CALL GSFACI(ICOLPL+J-1)
       ELSEIF(I.EQ.3)THEN
            CALL GSFACI(ICOLW1+J-1)
       ELSEIF(I.EQ.4)THEN
            CALL GSFACI(ICOLW2+J-1)
       ELSEIF(I.EQ.5)THEN
            CALL GSFACI(ICOLW3+J-1)
       ELSEIF(I.EQ.6)THEN
            CALL GSFACI(ICOLD1+J-1)
       ELSEIF(I.EQ.7)THEN
            CALL GSFACI(ICOLD2+J-1)
       ELSEIF(I.EQ.8)THEN
            CALL GSFACI(ICOLD3+J-1)
       ELSEIF(I.EQ.9)THEN
            CALL GSFACI(ICOLST+J-1)
       ELSEIF(I.EQ.10)THEN
            CALL GSFACI(ICOLRB+J-1)
       ELSE
            PRINT *,' !!!!!! COLSHM WARNING : Unknown index.'
            CALL GSFACI(0)
       ENDIF
       CALL GFA(5,XPL,YPL)
*   Next shade.
20     CONTINUE
*   Draw an overall box around this table.
       XPL(1)=XMIN
       YPL(1)=0.1
       XPL(2)=XMIN
       YPL(2)=0.9
       XPL(3)=XMAX
       YPL(3)=0.9
       XPL(4)=XMAX
       YPL(4)=0.1
       XPL(5)=XMIN
       YPL(5)=0.1
       CALL GPL(5,XPL,YPL)
*   Next colour table.
10     CONTINUE
*** Next page.
       CALL GRALOG('Colour shading map:')
       CALL GRNEXT
*** Keep track of CPU time consumption.
       CALL TIMLOG('Producing a colour shading map:         ')
       END
