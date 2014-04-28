CDECK  ID>, GRCUPD.
       SUBROUTINE GRCUPD(F,X1,Y1,FC,STATUS,IFLAG)
*-----------------------------------------------------------------------
*   GRCUPD - Updates the grid for the contour segment (XPL,YPL).
*-----------------------------------------------------------------------
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
       LOGICAL CROSS
       CHARACTER*(*) STATUS
       EXTERNAL CROSS,F
       INTEGER INIT
       SAVE INIT,X0,Y0
       DATA INIT/0/
*** Check and set of the initialisation flag, first the start.
       IF(INDEX(STATUS,'START').NE.0)THEN
            X0=X1
            Y0=Y1
            IFLAG=0
            INIT=1
            RETURN
*   Last step on the contour: lock but do this one.
       ELSEIF(INDEX(STATUS,'END').NE.0)THEN
            INIT=0
*   For other operations, INIT must be set properly.
       ELSEIF(INIT.EQ.0)THEN
            WRITE(10,'(''  !!!!!! GRCUPD WARNING : This routine has'',
     -           '' not been initialsed properly; program bug.'')')
            IFLAG=1
            RETURN
       ENDIF
*** In case the contour left the area, update the boundary.
       IF(INDEX(STATUS,'EDGE').NE.0)THEN
*   Update of the lower x border.
            IF(1+2*INT(0.001+IFLAG/2).EQ.IFLAG)THEN
                 IUPD=INT((Y1-CYMIN)*REAL(NGRIDY)/(CYMAX-CYMIN))
                 IF(IUPD.GE.0.AND.IUPD.LE.NGRIDY)YDONE(0,IUPD)=.TRUE.
            ENDIF
*   Update of the higher x border.
            IF(1+2*INT(0.001+IFLAG/4).EQ.IFLAG/2)THEN
                 IUPD=INT((Y1-CYMIN)*REAL(NGRIDY)/(CYMAX-CYMIN))
                 IF(IUPD.GE.0.AND.IUPD.LE.NGRIDY)
     -                YDONE(NGRIDX,IUPD)=.TRUE.
            ENDIF
*   Update of the lower y border.
            IF(1+2*INT(0.001+IFLAG/8).EQ.IFLAG/4)THEN
                 IUPD=INT((X1-CXMIN)*REAL(NGRIDX)/(CXMAX-CXMIN))
                 IF(IUPD.GE.0.AND.IUPD.LE.NGRIDX)XDONE(IUPD,0)=.TRUE.
            ENDIF
*   Update of the higher y border.
            IF(1+2*INT(0.001+IFLAG/16).EQ.IFLAG/8)THEN
                 IUPD=INT((X1-CXMIN)*REAL(NGRIDX)/(CXMAX-CXMIN))
                 IF(IUPD.GE.0.AND.IUPD.LE.NGRIDX)
     -                XDONE(IUPD,NGRIDY)=.TRUE.
            ENDIF
       ENDIF
*** IFLAG has now been used, assume the routine will work.
       IFLAG=0
*** Determine other grid lines the contour may have crossed.
       IXMIN=MIN(INT((X0-CXMIN)*REAL(NGRIDX)/(CXMAX-CXMIN)),
     -      INT((X1-CXMIN)*REAL(NGRIDX)/(CXMAX-CXMIN)))
       IXMAX=MAX(INT((X0-CXMIN)*REAL(NGRIDX)/(CXMAX-CXMIN)),
     -      INT((X1-CXMIN)*REAL(NGRIDX)/(CXMAX-CXMIN)))
       IYMIN=MIN(INT((Y0-CYMIN)*REAL(NGRIDY)/(CYMAX-CYMIN)),
     -      INT((Y1-CYMIN)*REAL(NGRIDY)/(CYMAX-CYMIN)))
       IYMAX=MAX(INT((Y0-CYMIN)*REAL(NGRIDY)/(CYMAX-CYMIN)),
     -      INT((Y1-CYMIN)*REAL(NGRIDY)/(CYMAX-CYMIN)))
       IXMIN=MIN(MXGRID,NGRIDX,MAX(0,IXMIN))
       IXMAX=MIN(MXGRID,NGRIDX,MAX(0,IXMAX))
       IYMIN=MIN(MXGRID,NGRIDY,MAX(0,IYMIN))
       IYMAX=MIN(MXGRID,NGRIDY,MAX(0,IYMAX))
**  Skip the case no line was crossed.
       IF(IXMIN.EQ.IXMAX.AND.IYMIN.EQ.IYMAX)THEN
            X0=X1
            Y0=Y1
            RETURN
       ENDIF
      if(ldebug)write(10,'('' x-range: '',2I3,'' y-range: '',2I3)')
     -     ixmin,ixmax,iymin,iymax
**  Loop over the subgrid.
       DO 20 IX=IXMIN,IXMAX
       DO 30 IY=IYMIN,IYMAX
**  x-update, skipped if the grid point is on the boundary.
       IF((.NOT.XDONE(IX,IY)).AND.IX.LT.NGRIDX.AND.CROSS(
     -      CXMIN+REAL(IX)*(CXMAX-CXMIN)/REAL(NGRIDX),
     -      CYMIN+REAL(IY)*(CYMAX-CYMIN)/REAL(NGRIDY),
     -      CXMIN+REAL(IX+1)*(CXMAX-CXMIN)/REAL(NGRIDX),
     -      CYMIN+REAL(IY)*(CYMAX-CYMIN)/REAL(NGRIDY),
     -      X0,Y0,X1,Y1))THEN
*   Assume no update occurs.
            IDONE=0
*   Crossing point within bounds, update always if FC within bounds.
            IF((GRID(IX,IY)-FC)*(FC-GRID(IX+1,IY)).GE.0)THEN
                 XDONE(IX,IY)=.TRUE.
                 IDONE=1
            ENDIF
*   Check whether the contour sneaked before the grid point.
            IF(IX.GT.0.AND.IDONE.EQ.0)THEN
                 CALL GRCMIN(IX,IY,X0,Y0,X1,Y1,DNCR,ITYP)
                 IF((GRID(IX-1,IY)-FC)*(FC-GRID(IX,IY)).GE.0.AND.
     -                DNCR.LT.DNTHR)THEN
                      XDONE(IX-1,IY)=.TRUE.
                      IF(LDEBUG)WRITE(10,'(''  ++++++ GRCUPD'',
     -                     '' DEBUG   : Low-x update, d='',E15.8,
     -                     '' at '',2I3,''.'')') DNCR,IX-1,IY
                      IDONE=1
                 ENDIF
            ENDIF
*   Check whether the contour sneaked past the grid segment.
            IF(IX.LT.NGRIDX-1.AND.IDONE.EQ.0)THEN
                 CALL GRCMIN(IX+1,IY,X0,Y0,X1,Y1,DNCR,ITYP)
                 IF((GRID(IX+1,IY)-FC)*(FC-GRID(IX+2,IY)).GE.0.AND.
     -                DNCR.LT.DNTHR)THEN
                      XDONE(IX+1,IY)=.TRUE.
                      IF(LDEBUG)WRITE(10,'(''  ++++++ GRCUPD'',
     -                     '' DEBUG   : High-x update, d='',E15.8,
     -                     '' at '',2I3,''.'')') DNCR,IX+1,IY
                      IDONE=1
                 ENDIF
            ENDIF
*   Make sure an update is found.
            IF(IDONE.EQ.0)THEN
                 WRITE(10,'(''  !!!!!! GRCUPD WARNING : No x-update'',
     -                '' performed inspite of a segment crossing.'')')
C                CALL F(X0,Y0,F0,ILOC0)
C                CALL F(X1,Y1,F1,ILOC1)
C                NFC=NFC+2
C                WRITE(10,'(26X,''Grid='',4E12.5/
C    -                26X,''Step='',4E12.5/26X,''F Grid='',3E12.5/
C    -                26X,''F step='',3E12.5/
C    -                26X,''Loc   ='',12X,2I12)')
C    -                CXMIN+IX*(CXMAX-CXMIN)/REAL(NGRIDX),
C    -                CYMIN+IY*(CYMAX-CYMIN)/REAL(NGRIDY),
C    -                CXMIN+(IX+1)*(CXMAX-CXMIN)/REAL(NGRIDX),
C    -                CYMIN+IY*(CYMAX-CYMIN)/REAL(NGRIDY),
C    -                X0,Y0,X1,Y1,
C    -                GRID(IX-1,IY),GRID(IX,IY),GRID(IX+1,IY),
C    -                FC,F0,F1,ILOC0,ILOC1
                 XDONE(IX,IY)=.TRUE.
            ENDIF
       ENDIF
**  y-update, skipped if the grid point is on the boundary.
       IF((.NOT.YDONE(IX,IY)).AND.IY.LT.NGRIDY.AND.CROSS(
     -      CXMIN+REAL(IX)*(CXMAX-CXMIN)/REAL(NGRIDX),
     -      CYMIN+REAL(IY)*(CYMAX-CYMIN)/REAL(NGRIDY),
     -      CXMIN+REAL(IX)*(CXMAX-CXMIN)/REAL(NGRIDX),
     -      CYMIN+REAL(IY+1)*(CYMAX-CYMIN)/REAL(NGRIDY),
     -      X0,Y0,X1,Y1))THEN
*   Assume no update occurs.
            IDONE=0
*   Crossing point within bounds, update always if FC within bounds.
            IF((GRID(IX,IY)-FC)*(FC-GRID(IX,IY+1)).GE.0)THEN
                 YDONE(IX,IY)=.TRUE.
                 IDONE=1
            ENDIF
*   Check whether the contour sneaked before the grid point.
            IF(IY.GT.0.AND.IDONE.EQ.0)THEN
                 CALL GRCMIN(IX,IY,X0,Y0,X1,Y1,DNCR,ITYP)
                 IF((GRID(IX,IY-1)-FC)*(FC-GRID(IX,IY)).GE.0.AND.
     -                DNCR.LT.DNTHR)THEN
                      YDONE(IX,IY-1)=.TRUE.
                      IF(LDEBUG)WRITE(10,'(''  ++++++ GRCUPD'',
     -                     '' DEBUG   : Low-y update, d='',E15.8,
     -                     '' at '',2I3,''.'')') DNCR,IX,IY-1
                      IDONE=1
                 ENDIF
            ENDIF
*   Check whether the contour sneaked past the grid segment.
            IF(IY.LT.NGRIDY-1.AND.IDONE.EQ.0)THEN
                 CALL GRCMIN(IX,IY+1,X0,Y0,X1,Y1,DNCR,ITYP)
                 IF((GRID(IX,IY+1)-FC)*(FC-GRID(IX,IY+2)).GE.0.AND.
     -                DNCR.LT.DNTHR)THEN
                      YDONE(IX,IY+1)=.TRUE.
                      IF(LDEBUG)WRITE(10,'(''  ++++++ GRCUPD'',
     -                     '' DEBUG   : High y-update, d='',E15.8,
     -                     '' at '',2I3,''.'')') DNCR,IX,IY+1
                      IDONE=1
                 ENDIF
            ENDIF
*   Make sure an update is found.
            IF(IDONE.EQ.0)THEN
                 WRITE(10,'(''  !!!!!! GRCUPD WARNING : No y-update'',
     -                '' performed inspite of a segment crossing.'')')
C                WRITE(10,'(26X,''IX,IY='',2I3/26X,''F='',3E15.8)')
C    -                IX,IY,GRID(IX,IY),FC,GRID(IX,IY+1)
                 YDONE(IX,IY)=.TRUE.
            ENDIF
       ENDIF
30     CONTINUE
20     CONTINUE
*** Shift the positions.
       X0=X1
       Y0=Y1
       END
