CDECK  ID>, GRCONT.
       SUBROUTINE GRCONT(F,FMIN,FMAX,QXMIN,QYMIN,QXMAX,QYMAX,
     -      NF,AUTO,TRANSF,LABEL)
*-----------------------------------------------------------------------
*   GRCONT - Routine plotting contours of the function F in the window
*            (XNIN,YMIN) to (XMAX,YMAX) using a grid of NGRIDX+1 by
*            NGRIDY+1 points.
*   VARIABLES : AUTO       : If .TRUE. the scale will be determined
*                            automatically.
*   (Last changed on 28/ 5/98.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL FMIN,FMAX,GRMIN,GRMAX,STEP,X0,Y0,QXMIN,QYMIN,QXMAX,QYMAX,
     -      XPL,YPL,FC
       INTEGER NF,INIT,IX,IY,IF,IFAIL
       LOGICAL AUTO,LOOP,TRANSF,LABEL
       EXTERNAL F
*** Check the dimensions.
       IF(NGRIDX.LE.0.OR.NGRIDX.GT.MXGRID.OR.
     -      NGRIDY.LE.0.OR.NGRIDY.GT.MXGRID)THEN
            WRITE(10,'(1X,A)') ' !!!!!! GRCONT WARNING : Grid'//
     -           ' dimensions out of range ; contours not plotted.'
            RETURN
       ENDIF
       IF(NF.LT.1)THEN
            WRITE(10,'(1X,A)') ' !!!!!! GRCONT WARNING : Number of'//
     -           ' contours is smaller than 1 ; no contours plotted.'
            RETURN
       ENDIF
*** Copy the area etc to the local variables.
       CXMIN=QXMIN
       CXMAX=QXMAX
       CYMIN=QYMIN
       CYMAX=QYMAX
       TRANS=TRANSF
       CLAB =LABEL
*** Set gradient step size.
       DXGRA=EPSGRA*ABS(CXMAX-CXMIN)
       DYGRA=EPSGRA*ABS(CYMAX-CYMIN)
       IF(DXGRA.LE.0.OR.DYGRA.LE.0)THEN
            WRITE(LUNOUT,'(''  !!!!!! GRCONT WARNING : Gradient step'',
     -           '' size is 0 ; check AREA and !CONTOUR-PARAMETERS.'')')
            RETURN
       ENDIF
*** Fill the grid.
       INIT=0
       DO 10 IX=0,NGRIDX
       DO 20 IY=0,NGRIDY
       CALL F(CXMIN+REAL(IX)*(CXMAX-CXMIN)/REAL(NGRIDX),
     -      CYMIN+REAL(IY)*(CYMAX-CYMIN)/REAL(NGRIDY),
     -      GRID(IX,IY),ILOCGR(IX,IY))
       IF(INIT.EQ.0)THEN
            GRMAX=GRID(IX,IY)
            GRMIN=GRID(IX,IY)
            INIT=1
       ELSE
            IF(GRMIN.GT.GRID(IX,IY))GRMIN=GRID(IX,IY)
            IF(GRMAX.LT.GRID(IX,IY))GRMAX=GRID(IX,IY)
       ENDIF
20     CONTINUE
10     CONTINUE
       NFC=(NGRIDX+1)*(NGRIDY+1)
*   Verify that a grid range has been set.
       IF(INIT.EQ.0)THEN
            WRITE(10,'(''  !!!!!! GRCONT WARNING : No range found,'',
     -           '' no contours plotted.'')')
            RETURN
*   Check the range makes sense if fixed.
       ELSEIF((.NOT.AUTO).AND.
     -      (MAX(FMIN,FMAX).LT.MIN(GRMIN,GRMAX).OR.
     -       MIN(FMIN,FMAX).GT.MAX(GRMIN,GRMAX)))THEN
            WRITE(10,'(''  !!!!!! GRCONT WARNING : Specified range ('',
     -           2E12.5,'') does not overlap''/26X,
     -           ''with effective range ('',2E12.5,'').''/
     -           26X,''No contours will be drawn.'')')
     -           FMIN,FMAX,GRMIN,GRMAX
            RETURN
*   Optionally fix the scale.
       ELSEIF(AUTO)THEN
            FMIN=GRMIN
            FMAX=GRMAX
            IF(GRMIN.EQ.GRMAX)THEN
                 STEP=0.0
                 NF=0
            ELSE
                 CALL ROUND(FMIN,FMAX,NF,'SMALLER',STEP)
                 NF=NINT((FMAX-FMIN)/STEP)
            ENDIF
       ELSEIF(NF.NE.0)THEN
            STEP=(FMAX-FMIN)/REAL(NF)
       ELSE
            WRITE(10,'(''  !!!!!! GRCONT WARNING : Unable to find'',
     -           '' a contour range ; no contours drawn.'')')
            RETURN
       ENDIF
       IF(LDEBUG)WRITE(10,'(1X,A,2E15.8/26X,A,2E15.8/26X,A,I3/
     -      26X,A,E15.8)')
     -      ' ++++++ GRCONT DEBUG   : Grid function range:  ',
     -      GRMIN,GRMAX,'Contour height range: ',FMIN,FMAX,
     -      'Number of contours:   ',NF,
     -      'Step size :           ',STEP
*** Set the attributes for contours.
       CALL GRATTS('CONTOUR-NORMAL','POLYLINE')
*** Loop over the contour heights.
       DO 100 IF=0,NF
       FC=FMIN+REAL(IF)*STEP
       IF(FC.GT.FMAX)GOTO 100
       IF(LDEBUG)WRITE(10,'(1X,A,E15.8)') ' ++++++ GRCONT DEBUG   :'//
     -      ' Contour height = ',FC
*** Clear the buffers that remember whether a contour was done.
       DO 110 IX=0,NGRIDX
       DO 120 IY=0,NGRIDY
       XDONE(IX,IY)=.FALSE.
       YDONE(IX,IY)=.FALSE.
120    CONTINUE
110    CONTINUE
*** Check point by point whether there is a contour crossing.
       DO 130 IX=0,NGRIDX
       DO 140 IY=0,NGRIDY
**  Avoid addressing problems.
       IF(IX.GE.NGRIDX)GOTO 150
**  Check in x.
       IF((.NOT.XDONE(IX,IY)).AND.
     -      (ILOCGR(IX,IY).EQ.0.OR.ILOCGR(IX+1,IY).EQ.0).AND.
     -      (GRID(IX,IY)-FC)*(GRID(IX+1,IY)-FC).LT.0)THEN
            IF(LDEBUG)THEN
                 CALL GSMK(4)
                 XPL=CXMIN+REAL(IX)*(CXMAX-CXMIN)/REAL(NGRIDX)
                 YPL=CYMIN+REAL(IY)*(CYMAX-CYMIN)/REAL(NGRIDY)
                 IF(TRANS)CALL CFMRTC(XPL,YPL,XPL,YPL,1)
                 CALL GPM(1,XPL,YPL)
                 WRITE(10,'(1X,A,2E15.8)') ' ++++++ GRCONT DEBUG   :'//
     -                ' Start from an x-segment at ',XPL,YPL
            ENDIF
            CALL GRCBIS(F,FC,X0,Y0,
     -           CXMIN+REAL(IX)  *(CXMAX-CXMIN)/REAL(NGRIDX),
     -           CYMIN+REAL(IY)  *(CYMAX-CYMIN)/REAL(NGRIDY),
     -           GRID(IX,IY),ILOCGR(IX,IY),
     -           CXMIN+REAL(IX+1)*(CXMAX-CXMIN)/REAL(NGRIDX),
     -           CYMIN+REAL(IY)  *(CYMAX-CYMIN)/REAL(NGRIDY),
     -           GRID(IX+1,IY),ILOCGR(IX+1,IY),
     -           IFAIL)
            IF(IFAIL.EQ.0)THEN
                 XDONE(IX,IY)=.TRUE.
                 CALL GRCTRA(F,FC,X0,Y0,-1.0,LOOP)
                 IF(.NOT.LOOP)CALL GRCTRA(F,FC,X0,Y0,+1.0,LOOP)
            ENDIF
       ENDIF
**  Avoid addressing problems.
150    CONTINUE
       IF(IY.GE.NGRIDY)GOTO 140
**  And similarly in y.
       IF((.NOT.YDONE(IX,IY)).AND.
     -      (ILOCGR(IX,IY).EQ.0.OR.ILOCGR(IX,IY+1).EQ.0).AND.
     -      (GRID(IX,IY)-FC)*(GRID(IX,IY+1)-FC).LT.0)THEN
            IF(LDEBUG)THEN
                 CALL GSMK(5)
                 XPL=CXMIN+REAL(IX)*(CXMAX-CXMIN)/REAL(NGRIDX)
                 YPL=CYMIN+REAL(IY)*(CYMAX-CYMIN)/REAL(NGRIDY)
                 IF(TRANS)CALL CFMRTC(XPL,YPL,XPL,YPL,1)
                 CALL GPM(1,XPL,YPL)
                 WRITE(10,'(1X,A,2E15.8)') ' ++++++ GRCONT DEBUG   :'//
     -                ' Start from a y-segment at ',XPL,YPL
            ENDIF
            CALL GRCBIS(F,FC,X0,Y0,
     -           CXMIN+REAL(IX)  *(CXMAX-CXMIN)/REAL(NGRIDX),
     -           CYMIN+REAL(IY)  *(CYMAX-CYMIN)/REAL(NGRIDY),
     -           GRID(IX,IY),ILOCGR(IX,IY),
     -           CXMIN+REAL(IX)  *(CXMAX-CXMIN)/REAL(NGRIDX),
     -           CYMIN+REAL(IY+1)*(CYMAX-CYMIN)/REAL(NGRIDY),
     -           GRID(IX,IY+1),ILOCGR(IX,IY+1),
     -           IFAIL)
            IF(IFAIL.EQ.0)THEN
                 YDONE(IX,IY)=.TRUE.
                 CALL GRCTRA(F,FC,X0,Y0,-1.0,LOOP)
                 IF(.NOT.LOOP)CALL GRCTRA(F,FC,X0,Y0,+1.0,LOOP)
            ENDIF
       ENDIF
140    CONTINUE
130    CONTINUE
*** Next contour height.
100    CONTINUE
       END
