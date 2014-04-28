CDECK  ID>, GRAREA.
       SUBROUTINE GRAREA(NIN,XIN,YIN)
*-----------------------------------------------------------------------
*   GRAREA - Draws an area in either log or linear coordinates.
*   VARIABLES: NU          : Number of points
*              (XU,YU)     : Vertices of the area
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
       REAL XIN(*),YIN(*),XU(MXLIST),YU(MXLIST),XPL(MXLIST),YPL(MXLIST),
     -      XOUT(MXLIST),YOUT(MXLIST),XCUR,YCUR,XLAST,YLAST,X0,Y0,X1,Y1,
     -      QOUT,QIN,QFIRST,XFIRST,YFIRST,EPSX,EPSY
       INTEGER NIN,NU,NPL,NOUT,I,J,II,IFAIL,IMAX,NTOTP,NINP,NTOTM,NINM,
     -      IOUT,IIN,IFIRST,ISTART
       LOGICAL CROSS,ONLINE,CURIN,LASTIN,ALLIN,ADD(4),IN1,IN2,IN3,IN4,
     -      EDGE1,EDGE2,EDGE3,EDGE4,RESET
       EXTERNAL CROSS,ONLINE
*** Verify array length.
       IF(NIN.GT.MXLIST)THEN
            PRINT *,' !!!!!! GRAREA WARNING : Input array too long;'//
     -           ' not plotted.'
            RETURN
       ELSEIF(NIN.LE.2)THEN
            RETURN
       ENDIF
*** Set precisions.
       IF(LEPSG)THEN
            EPSX=REAL(EPSGX)
            EPSY=REAL(EPSGY)
            RESET=.FALSE.
       ELSE
            EPSX=1E-5*(FRXMAX-FRXMIN)
            EPSY=1E-5*(FRYMAX-FRYMIN)
            IF(EPSX.LE.0)EPSX=1.0E-5
            IF(EPSY.LE.0)EPSY=1.0E-5
            CALL EPSSET('SET',DBLE(EPSX),DBLE(EPSY),0.0D0)
            RESET=.TRUE.
       ENDIF
*** Convert input array to log scales if desired, find starting point.
       ISTART=0
       ALLIN=.TRUE.
       DO 10 I=1,NIN
*   Transform x-coordinate if requested.
       IF(LOGX)THEN
            IF(XIN(I).LE.0.0)THEN
                 XCUR=FRXMIN-2*ABS(FRXMAX-FRXMIN)
            ELSE
                 XCUR=LOG10(XIN(I))
            ENDIF
       ELSE
            XCUR=XIN(I)
       ENDIF
*   Transform y-coordinate if requested.
       IF(LOGY)THEN
            IF(YIN(I).LE.0.0)THEN
                 YCUR=FRYMIN-2*ABS(FRYMAX-FRYMIN)
            ELSE
                 YCUR=LOG10(YIN(I))
            ENDIF
       ELSE
            YCUR=YIN(I)
       ENDIF
*   See whether all points are in the box.
       IF(XCUR.LT.FRXMIN.OR.XCUR.GT.FRXMAX.OR.
     -      YCUR.LT.FRYMIN.OR.YCUR.GT.FRYMAX)ALLIN=.FALSE.
*   Internal points are good starting points.
       IF(ISTART.EQ.0.AND.
     -      XCUR.GE.FRXMIN+EPSX.AND.XCUR.LE.FRXMAX-EPSX.AND.
     -      YCUR.GE.FRYMIN+EPSY.AND.YCUR.LE.FRYMAX-EPSY)ISTART=I
*   Crossings can also be used.
       IF(I.GT.1)THEN
            X0=XLAST
            Y0=YLAST
            X1=XCUR
            Y1=YCUR
            CALL CLIP(X0,Y0,X1,Y1,FRXMIN,FRYMIN,FRXMAX,FRYMAX,IFAIL)
            IF(ISTART.EQ.0.AND.IFAIL.EQ.0.AND.
     -           0.5*(X0+X1).GT.FRXMIN+EPSX.AND.
     -           0.5*(X0+X1).LT.FRXMAX-EPSX.AND.
     -           0.5*(Y0+Y1).GT.FRYMIN+EPSY.AND.
     -           0.5*(Y0+Y1).LT.FRYMAX-EPSY)ISTART=I
       ENDIF
*   Store the data.
       XU(I)=XCUR
       YU(I)=YCUR
*   Shift "current" to "last".
       XLAST=XCUR
       YLAST=YCUR
10     CONTINUE
*   Store number of points again for convenience.
       NU=NIN
*** If all points are within the area, simply plot.
       IF(ALLIN)THEN
            CALL GFA(NU,XU,YU)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GRAREA DEBUG   : All'',
     -           '' points in the area ; plotted without clipping.'')')
            GOTO 3030
*** No starting point found, check whether box is entirely enclosed.
       ELSEIF(ISTART.EQ.0)THEN
            CALL INTERN(NU,XU,YU,FRXMIN,FRYMIN,IN1,EDGE1)
            CALL INTERN(NU,XU,YU,FRXMAX,FRYMIN,IN2,EDGE2)
            CALL INTERN(NU,XU,YU,FRXMAX,FRYMAX,IN3,EDGE3)
            CALL INTERN(NU,XU,YU,FRXMIN,FRYMAX,IN4,EDGE4)
            IF(IN1.OR.IN2.OR.IN3.OR.IN4.OR.
     -           (EDGE1.AND.EDGE2.AND.EDGE3.AND.EDGE4))THEN
                 XPL(1)=FRXMIN
                 YPL(1)=FRYMIN
                 XPL(2)=FRXMAX
                 YPL(2)=FRYMIN
                 XPL(3)=FRXMAX
                 YPL(3)=FRYMAX
                 XPL(4)=FRXMIN
                 YPL(4)=FRYMAX
                 XPL(5)=FRXMIN
                 YPL(5)=FRYMIN
                 CALL GFA(5,XPL,YPL)
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GRAREA DEBUG   :'',
     -                '' Plot frame entirely within area.'')')
            ENDIF
            GOTO 3030
       ENDIF
*** Non-trivial cases: loop over the points.
       NOUT=0
       NPL=0
       IFIRST=0
       DO 100 II=ISTART-1,ISTART+NU-1
*   Reduce II.
       I=1+MOD(II+NU-1,NU)
*   Store point.
       XCUR=XU(I)
       YCUR=YU(I)
*   See whether this point is in the area.
       IF(XCUR.GE.FRXMIN.AND.XCUR.LE.FRXMAX.AND.
     -      YCUR.GE.FRYMIN.AND.YCUR.LE.FRYMAX)THEN
            CURIN=.TRUE.
       ELSE
            CURIN=.FALSE.
       ENDIF
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GRAREA DEBUG   : At point '',
     -      I3,'' ('',I3,''), (x,y)= '',2E12.5,'', in='',L1)')
     -      I,II,XCUR,YCUR,CURIN
*   For the first point, skip all the rest.
       IF(II.LT.ISTART)GOTO 110
**  Clip this section to the size of the box.
       X0=XLAST
       Y0=YLAST
       X1=XCUR
       Y1=YCUR
       CALL CLIP(X0,Y0,X1,Y1,FRXMIN,FRYMIN,FRXMAX,FRYMAX,IFAIL)
*   If this is the first point, at least part should be inside.
       IF(II.EQ.ISTART.AND.IFAIL.NE.0)THEN
            PRINT *,' !!!!!! GRAREA WARNING : No crossing found'//
     -           ' while expecting one; polygon not drawn.'
            IF(LGSTOP)THEN
                 OPEN(UNIT=12,FILE='grarea.dat',STATUS='UNKNOWN')
                 WRITE(12,*) EPSGX,EPSGY,EPSGZ,LEPSG
                 WRITE(12,*) FRXMIN,FRYMIN,FRXMAX,FRYMAX
                 WRITE(12,*) NIN
                 DO 200 J=1,NIN
                 WRITE(12,*) XIN(J),YIN(J)
200              CONTINUE
                 CLOSE(12)
                 CALL QUIT
            ENDIF
            GOTO 3030
*   Store the first point.
       ELSEIF(II.EQ.ISTART)THEN
            NPL=1
            XPL(NPL)=X0
            YPL(NPL)=Y0
            IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -           ''Started "plot" buffer with (x,y)='',2E12.5)')
     -           X0,Y0
       ENDIF
**  Skip processing if the points coincide.
       IF(ABS(XCUR-XLAST).LE.EPSX.AND.ABS(YCUR-YLAST).LE.EPSY.AND.
     -      II.GT.ISTART)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Coincides with previous'',
     -           '' point, skipped.'')')
            GOTO 110
       ENDIF
**  If fully outside the box, add to "out" buffer.
       IF(IFAIL.NE.0)THEN
*   Buffer not yet started if the previous point was on the edge.
            IF(NOUT.LE.0)THEN
                 IF(ONLINE(FRXMIN,FRYMIN,FRXMAX,FRYMIN,
     -                XLAST,YLAST))THEN
                      IOUT=1
                      QOUT=XLAST-FRXMIN
                 ELSEIF(ONLINE(FRXMAX,FRYMIN,FRXMAX,FRYMAX,
     -                XLAST,YLAST))THEN
                      IOUT=2
                      QOUT=YLAST-FRYMIN
                 ELSEIF(ONLINE(FRXMAX,FRYMAX,FRXMIN,FRYMAX,
     -                XLAST,YLAST))THEN
                      IOUT=3
                      QOUT=FRXMAX-XLAST
                 ELSEIF(ONLINE(FRXMIN,FRYMAX,FRXMIN,FRYMIN,
     -                XLAST,YLAST))THEN
                      IOUT=4
                      QOUT=FRYMAX-YLAST
                 ELSE
                      PRINT *,' !!!!!! GRAREA WARNING : No leaving'//
     -                     ' edge found ; polygon not drawn.'
                      GOTO 3030
                 ENDIF
                 NOUT=1
                 XOUT(NOUT)=XLAST
                 YOUT(NOUT)=YLAST
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                ''Box is left via edge '',I1,'', Q='',E12.5,
     -                '', "out" list started.'')') IOUT,QOUT
            ENDIF
*   Add the 2nd point to the "out" buffer.
            IF(NOUT.GE.MXLIST)GOTO 3010
            NOUT=NOUT+1
            XOUT(NOUT)=XCUR
            YOUT(NOUT)=YCUR
            IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -           ''Segment fully outside, added as '',I3,
     -           '' to "out".'')') NOUT
            GOTO 110
**  If fully inside the box, add to "plot" buffer.
       ELSEIF(LASTIN.AND.CURIN)THEN
            IF(NPL.GE.MXLIST)GOTO 3020
            NPL=NPL+1
            XPL(NPL)=X1
            YPL(NPL)=Y1
            IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -           ''Segment fully inside,  added as '',I3,
     -           '' to "plot".'')') NPL
            GOTO 110
       ENDIF
**  We re-enter the box.
       IF(.NOT.LASTIN)THEN
*   Determine the re-entrance side and coordinate.
            IF(ONLINE(FRXMIN,FRYMIN,FRXMAX,FRYMIN,X0,Y0))THEN
                 IIN=1
                 QIN=X0-FRXMIN
            ELSEIF(ONLINE(FRXMAX,FRYMIN,FRXMAX,FRYMAX,X0,Y0))THEN
                 IIN=2
                 QIN=Y0-FRYMIN
            ELSEIF(ONLINE(FRXMAX,FRYMAX,FRXMIN,FRYMAX,X0,Y0))THEN
                 IIN=3
                 QIN=FRXMAX-X0
            ELSEIF(ONLINE(FRXMIN,FRYMAX,FRXMIN,FRYMIN,X0,Y0))THEN
                 IIN=4
                 QIN=FRYMAX-Y0
            ELSE
                 PRINT *,' !!!!!! GRAREA WARNING : No re-entrance'//
     -                ' edge found ; polygon not drawn.'
                 GOTO 3030
            ENDIF
*   Debugging output.
            IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -           ''Box entered via edge '',I1,'', Q='',E12.5)')
     -           IIN,QIN
*   If this is the first segment, simply record it.
            IF(II.EQ.ISTART)THEN
                 IFIRST=IIN
                 QFIRST=QIN
                 XFIRST=X0
                 YFIRST=Y0
*   Skip in case we re-enter at the point where we left.
            ELSEIF(IIN.NE.IOUT.OR.
     -           ((IIN.EQ.1.OR.IIN.EQ.3).AND.ABS(QIN-QOUT).GT.EPSX).OR.
     -           ((IIN.EQ.2.OR.IIN.EQ.4).AND.ABS(QIN-QOUT).GT.EPSY))THEN
*   Add the re-entry point and complete the loop with the leaving point.
                 IF(NOUT.GE.MXLIST)GOTO 3010
                 NOUT=NOUT+1
                 XOUT(NOUT)=X0
                 YOUT(NOUT)=Y0
                 IF(NOUT.GE.MXLIST)GOTO 3010
                 NOUT=NOUT+1
                 XOUT(NOUT)=XOUT(1)
                 YOUT(NOUT)=YOUT(1)
*   Reduce the list of "out" points.
                 CALL GRARED(NOUT,XOUT,YOUT)
                 IF(NOUT.LE.2)THEN
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,''Number of "out"'',
     -                     '' points reduced to '',I3,'' - not adding'',
     -                     '' corners.'')') NOUT
                      IF(NOUT.GE.1)THEN
                           IF(NPL.GE.MXLIST)GOTO 3020
                           NPL=NPL+1
                           XPL(NPL)=XOUT(1)
                           YPL(NPL)=YOUT(1)
                      ENDIF
                      IF(NOUT.GE.2)THEN
                           IF(NPL.GE.MXLIST)GOTO 3020
                           NPL=NPL+1
                           XPL(NPL)=XOUT(2)
                           YPL(NPL)=YOUT(2)
                      ENDIF
                      GOTO 310
                 ENDIF
*   Find the corners that are located inside the curve.
                 CALL INTERN(NOUT,XOUT,YOUT,FRXMIN,FRYMIN,IN1,EDGE1)
                 CALL INTERN(NOUT,XOUT,YOUT,FRXMAX,FRYMIN,IN2,EDGE2)
                 CALL INTERN(NOUT,XOUT,YOUT,FRXMAX,FRYMAX,IN3,EDGE3)
                 CALL INTERN(NOUT,XOUT,YOUT,FRXMIN,FRYMAX,IN4,EDGE4)
                 ADD(1)=IN1.OR.EDGE1
                 ADD(2)=IN2.OR.EDGE2
                 ADD(3)=IN3.OR.EDGE3
                 ADD(4)=IN4.OR.EDGE4
*   Count corners in the positive direction.
                 IF(IOUT.LT.IIN.OR.(IOUT.EQ.IIN.AND.QOUT.LE.QIN))THEN
                      IMAX=IIN
                 ELSE
                      IMAX=IIN+4
                 ENDIF
                 NTOTP=0
                 NINP=0
                 DO 120 J=IOUT+1,IMAX
                 NTOTP=NTOTP+1
                 IF(ADD(1+MOD(J-1,4)))NINP=NINP+1
120              CONTINUE
*   Count corners in the negative direction.
                 IF(IOUT.GT.IIN.OR.(IOUT.EQ.IIN.AND.QOUT.GT.QIN))THEN
                      IMAX=IOUT
                 ELSE
                      IMAX=IOUT+4
                 ENDIF
                 NTOTM=0
                 NINM=0
                 DO 130 J=IMAX,IIN+1,-1
                 NTOTM=NTOTM+1
                 IF(ADD(1+MOD(J-1,4)))NINM=NINM+1
130              CONTINUE
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                ''"out" Buffer contains '',I3,'' points''/
     -                26X,''In/Edge flags: 1: '',2L1,'', 2: '',2L1,
     -                '', 3: '',2L1,'', 4: '',2L1/
     -                26X,''Corner counts: +: '',I3,''/'',I3,
     -                '', -: '',I3,''/'',I3)')
     -                NOUT,IN1,EDGE1,IN2,EDGE2,IN3,EDGE3,IN4,EDGE4,
     -                NINP,NTOTP,NINM,NTOTM
*   Add the corners that are located inside the curve.
                 IF(NTOTP+NTOTM.NE.4)THEN
                      PRINT *,' !!!!!! GRAREA WARNING : Error'//
     -                     ' counting corners ; polygon not drawn.'
                      IF(LGSTOP)THEN
                           OPEN(UNIT=12,FILE='grarea.dat',
     -                          STATUS='UNKNOWN')
                           WRITE(12,*) EPSGX,EPSGY,EPSGZ,LEPSG
                           WRITE(12,*) FRXMIN,FRYMIN,FRXMAX,FRYMAX
                           WRITE(12,*) NIN
                           DO 210 J=1,NIN
                           WRITE(12,*) XIN(J),YIN(J)
210                        CONTINUE
                           CLOSE(12)
                           CALL QUIT
                      ENDIF
                      GOTO 3030
                 ELSEIF(NINP.GT.0.AND.NINP.EQ.NTOTP.AND.NINM.EQ.0)THEN
                      IF(IOUT.LT.IIN.OR.(IOUT.EQ.IIN.AND.
     -                     QOUT.LE.QIN))THEN
                           IMAX=IIN
                      ELSE
                           IMAX=IIN+4
                      ENDIF
                      DO 140 J=IOUT+1,IMAX
                      IF(1+MOD(J-1,4).EQ.1)THEN
                           IF(NPL.GE.MXLIST)GOTO 3020
                           NPL=NPL+1
                           XPL(NPL)=FRXMIN
                           YPL(NPL)=FRYMIN
                           IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                          ''Added corner 1 in + sense.'')')
                      ELSEIF(1+MOD(J-1,4).EQ.2)THEN
                           IF(NPL.GE.MXLIST)GOTO 3020
                           NPL=NPL+1
                           XPL(NPL)=FRXMAX
                           YPL(NPL)=FRYMIN
                           IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                          ''Added corner 2 in + sense.'')')
                      ELSEIF(1+MOD(J-1,4).EQ.3)THEN
                           IF(NPL.GE.MXLIST)GOTO 3020
                           NPL=NPL+1
                           XPL(NPL)=FRXMAX
                           YPL(NPL)=FRYMAX
                           IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                          ''Added corner 3 in + sense.'')')
                      ELSE
                           IF(NPL.GE.MXLIST)GOTO 3020
                           NPL=NPL+1
                           XPL(NPL)=FRXMIN
                           YPL(NPL)=FRYMAX
                           IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                          ''Added corner 4 in + sense.'')')
                      ENDIF
140                   CONTINUE
                 ELSEIF(NINM.GT.0.AND.NINM.EQ.NTOTM.AND.NINP.EQ.0)THEN
                      IF(IOUT.GT.IIN.OR.(IOUT.EQ.IIN.AND.
     -                     QOUT.GT.QIN))THEN
                           IMAX=IOUT
                      ELSE
                           IMAX=IOUT+4
                      ENDIF
                      DO 150 J=IMAX,IIN+1,-1
                      IF(1+MOD(J-1,4).EQ.1)THEN
                           IF(NPL.GE.MXLIST)GOTO 3020
                           NPL=NPL+1
                           XPL(NPL)=FRXMIN
                           YPL(NPL)=FRYMIN
                           IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                          ''Added corner 1 in - sense.'')')
                      ELSEIF(1+MOD(J-1,4).EQ.2)THEN
                           IF(NPL.GE.MXLIST)GOTO 3020
                           NPL=NPL+1
                           XPL(NPL)=FRXMAX
                           YPL(NPL)=FRYMIN
                           IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                          ''Added corner 2 in - sense.'')')
                      ELSEIF(1+MOD(J-1,4).EQ.3)THEN
                           IF(NPL.GE.MXLIST)GOTO 3020
                           NPL=NPL+1
                           XPL(NPL)=FRXMAX
                           YPL(NPL)=FRYMAX
                           IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                          ''Added corner 3 in - sense.'')')
                      ELSE
                           IF(NPL.GE.MXLIST)GOTO 3020
                           NPL=NPL+1
                           XPL(NPL)=FRXMIN
                           YPL(NPL)=FRYMAX
                           IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                          ''Added corner 4 in - sense.'')')
                      ENDIF
150                   CONTINUE
                 ELSEIF(NINM.NE.0.OR.NINP.NE.0)THEN
                      PRINT *,' !!!!!! GRAREA WARNING : Error'//
     -                     ' deciding direction ; polygon not drawn.'
                      IF(LGSTOP)THEN
                           OPEN(UNIT=12,FILE='grarea.dat',
     -                          STATUS='UNKNOWN')
                           WRITE(12,*) EPSGX,EPSGY,EPSGZ,LEPSG
                           WRITE(12,*) FRXMIN,FRYMIN,FRXMAX,FRYMAX
                           WRITE(12,*) NIN
                           DO 220 J=1,NIN
                           WRITE(12,*) XIN(J),YIN(J)
220                        CONTINUE
                           CLOSE(12)
                           CALL QUIT
                      ENDIF
                      GOTO 3030
                 ENDIF
*   Resume here if there was no real loop outside.
310              CONTINUE
*   Reset the out buffer.
                 NOUT=0
*   In other cases, still reset the buffer.
            ELSE
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''Corner search skipped'',
     -                '', "in" and "out" coincide.'')')
                 NOUT=0
            ENDIF
*   Add the re-entrance point to the "plot" buffer.
            IF(NPL.GE.MXLIST)GOTO 3020
            NPL=NPL+1
            XPL(NPL)=X0
            YPL(NPL)=Y0
            IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -           ''Added first point to "plot" buffer as '',I3)') NPL
       ENDIF
**  Add the end point of the segment to the plot buffer.
       IF(NPL.GE.MXLIST)GOTO 3020
       NPL=NPL+1
       XPL(NPL)=X1
       YPL(NPL)=Y1
       IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -      ''Added last point to "plot" buffer as '',I3)') NPL
**  We leave the box.
       IF(.NOT.CURIN)THEN
*   Determine the leaving side and coordinate.
            IF(ONLINE(FRXMIN,FRYMIN,FRXMAX,FRYMIN,X1,Y1))THEN
                 IOUT=1
                 QOUT=X1-FRXMIN
            ELSEIF(ONLINE(FRXMAX,FRYMIN,FRXMAX,FRYMAX,X1,Y1))THEN
                 IOUT=2
                 QOUT=Y1-FRYMIN
            ELSEIF(ONLINE(FRXMAX,FRYMAX,FRXMIN,FRYMAX,X1,Y1))THEN
                 IOUT=3
                 QOUT=FRXMAX-X1
            ELSEIF(ONLINE(FRXMIN,FRYMAX,FRXMIN,FRYMIN,X1,Y1))THEN
                 IOUT=4
                 QOUT=FRYMAX-Y1
            ELSE
                 PRINT *,' !!!!!! GRAREA WARNING : No leaving'//
     -                ' edge found ; polygon not drawn.'
                 GOTO 3030
            ENDIF
*   Start a list of "out" points.
            NOUT=1
            XOUT(NOUT)=X1
            YOUT(NOUT)=Y1
*   Also add the point located outside.
            IF(NOUT.GE.MXLIST)GOTO 3010
            NOUT=NOUT+1
            XOUT(NOUT)=XCUR
            YOUT(NOUT)=YCUR
*   Debugging output.
            IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -           ''Box is left via edge '',I1,'', Q='',E12.5,
     -           '', "out" list started, point added as 2.'')')
     -           IOUT,QOUT
       ENDIF
**  Shift "current" to "last".
110    CONTINUE
       XLAST=XCUR
       YLAST=YCUR
       LASTIN=CURIN
100    CONTINUE
*** End of the list of points, check whether the first point was "out".
       IF(IFIRST.NE.0.AND.
     -      (IFIRST.NE.IOUT.OR.
     -      ((IFIRST.EQ.1.OR.IFIRST.EQ.3).AND.
     -      ABS(QFIRST-QOUT).GT.EPSX).OR.
     -      ((IFIRST.EQ.2.OR.IFIRST.EQ.4).AND.
     -      ABS(QFIRST-QOUT).GT.EPSY)))THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Finishing loop, first'',
     -           '' segment entered over edge '',I2,'' at Q='',E12.5)')
     -           IFIRST,QFIRST
*   Make sure there is an "out" buffer already.
            IF(NOUT.EQ.0)THEN
                 PRINT *,' !!!!!! GRAREA WARNING : "out" Buffer'//
     -                ' unexpectedly found empty ; not plotted.'
                 GOTO 3030
            ENDIF
*   Add the first point to the "out" buffer.
            IF(NOUT.GE.MXLIST)GOTO 3010
            NOUT=NOUT+1
            XOUT(NOUT)=XFIRST
            YOUT(NOUT)=YFIRST
*   Restore the entrance edge and offset.
            IIN=IFIRST
            QIN=QFIRST
*   Close the loop with the first point.
            IF(NOUT.GE.MXLIST)GOTO 3010
            NOUT=NOUT+1
            XOUT(NOUT)=XOUT(1)
            YOUT(NOUT)=YOUT(1)
*   Reduce the list of "out" points.
            CALL GRARED(NOUT,XOUT,YOUT)
            IF(NOUT.LE.2)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''Number of "out"'',
     -                '' points reduced to '',I3,'' - not adding'',
     -                '' corners.'')') NOUT
                 IF(NOUT.GE.1)THEN
                      IF(NPL.GE.MXLIST)GOTO 3020
                      NPL=NPL+1
                      XPL(NPL)=XOUT(1)
                      YPL(NPL)=YOUT(1)
                 ENDIF
                 IF(NOUT.GE.2)THEN
                      IF(NPL.GE.MXLIST)GOTO 3020
                      NPL=NPL+1
                      XPL(NPL)=XOUT(2)
                      YPL(NPL)=YOUT(2)
                 ENDIF
                 GOTO 300
            ENDIF
*   Find the corners that are located inside the curve.
            CALL INTERN(NOUT,XOUT,YOUT,FRXMIN,FRYMIN,IN1,EDGE1)
            CALL INTERN(NOUT,XOUT,YOUT,FRXMAX,FRYMIN,IN2,EDGE2)
            CALL INTERN(NOUT,XOUT,YOUT,FRXMAX,FRYMAX,IN3,EDGE3)
            CALL INTERN(NOUT,XOUT,YOUT,FRXMIN,FRYMAX,IN4,EDGE4)
            ADD(1)=IN1.OR.EDGE1
            ADD(2)=IN2.OR.EDGE2
            ADD(3)=IN3.OR.EDGE3
            ADD(4)=IN4.OR.EDGE4
*   Count corners in the positive direction.
            IF(IOUT.LT.IIN.OR.(IOUT.EQ.IIN.AND.QOUT.LE.QIN))THEN
                 IMAX=IIN
            ELSE
                 IMAX=IIN+4
            ENDIF
            NTOTP=0
            NINP=0
            DO 160 J=IOUT+1,IMAX
            NTOTP=NTOTP+1
            IF(ADD(1+MOD(J-1,4)))NINP=NINP+1
160         CONTINUE
*   Count corners in the negative direction.
            IF(IOUT.GT.IIN.OR.(IOUT.EQ.IIN.AND.QOUT.GT.QIN))THEN
                 IMAX=IOUT
            ELSE
                 IMAX=IOUT+4
            ENDIF
            NTOTM=0
            NINM=0
            DO 170 J=IMAX,IIN+1,-1
            NTOTM=NTOTM+1
            IF(ADD(1+MOD(J-1,4)))NINM=NINM+1
170         CONTINUE
            IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -           ''"out" Buffer contains '',I3,'' points''/
     -           26X,''In/Edge flags: 1: '',2L1,'', 2: '',2L1,
     -           '', 3: '',2L1,'', 4: '',2L1/
     -           26X,''Corner counts: +: '',I3,''/'',I3,
     -           '', -: '',I3,''/'',I3)')
     -           NOUT,IN1,EDGE1,IN2,EDGE2,IN3,EDGE3,IN4,EDGE4,
     -           NINP,NTOTP,NINM,NTOTM
*   Add the corners that are located inside the curve.
            IF(NTOTP+NTOTM.NE.4)THEN
                 PRINT *,' !!!!!! GRAREA WARNING : Error'//
     -                ' counting corners ; polygon not drawn.'
                 GOTO 3030
            ELSEIF(NINP.GT.0.AND.NINP.EQ.NTOTP.AND.NINM.EQ.0)THEN
                 IF(IOUT.LT.IIN.OR.(IOUT.EQ.IIN.AND.
     -                QOUT.LE.QIN))THEN
                      IMAX=IIN
                 ELSE
                      IMAX=IIN+4
                 ENDIF
                 DO 180 J=IOUT+1,IMAX
                 IF(1+MOD(J-1,4).EQ.1)THEN
                      IF(NPL.GE.MXLIST)GOTO 3020
                      NPL=NPL+1
                      XPL(NPL)=FRXMIN
                      YPL(NPL)=FRYMIN
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                     ''Added corner 1 in + sense.'')')
                 ELSEIF(1+MOD(J-1,4).EQ.2)THEN
                      IF(NPL.GE.MXLIST)GOTO 3020
                      NPL=NPL+1
                      XPL(NPL)=FRXMAX
                      YPL(NPL)=FRYMIN
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                     ''Added corner 2 in + sense.'')')
                 ELSEIF(1+MOD(J-1,4).EQ.3)THEN
                      IF(NPL.GE.MXLIST)GOTO 3020
                      NPL=NPL+1
                      XPL(NPL)=FRXMAX
                      YPL(NPL)=FRYMAX
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                     ''Added corner 3 in + sense.'')')
                 ELSE
                      IF(NPL.GE.MXLIST)GOTO 3020
                      NPL=NPL+1
                      XPL(NPL)=FRXMIN
                      YPL(NPL)=FRYMAX
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                     ''Added corner 4 in + sense.'')')
                 ENDIF
180              CONTINUE
            ELSEIF(NINM.GT.0.AND.NINM.EQ.NTOTM.AND.NINP.EQ.0)THEN
                 IF(IOUT.GT.IIN.OR.(IOUT.EQ.IIN.AND.
     -                QOUT.GT.QIN))THEN
                      IMAX=IOUT
                 ELSE
                      IMAX=IOUT+4
                 ENDIF
                 DO 190 J=IMAX,IIN+1,-1
                 IF(1+MOD(J-1,4).EQ.1)THEN
                      IF(NPL.GE.MXLIST)GOTO 3020
                      NPL=NPL+1
                      XPL(NPL)=FRXMIN
                      YPL(NPL)=FRYMIN
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                     ''Added corner 1 in - sense.'')')
                 ELSEIF(1+MOD(J-1,4).EQ.2)THEN
                      IF(NPL.GE.MXLIST)GOTO 3020
                      NPL=NPL+1
                      XPL(NPL)=FRXMAX
                      YPL(NPL)=FRYMIN
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                     ''Added corner 2 in - sense.'')')
                 ELSEIF(1+MOD(J-1,4).EQ.3)THEN
                      IF(NPL.GE.MXLIST)GOTO 3020
                      NPL=NPL+1
                      XPL(NPL)=FRXMAX
                      YPL(NPL)=FRYMAX
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                     ''Added corner 3 in - sense.'')')
                 ELSE
                      IF(NPL.GE.MXLIST)GOTO 3020
                      NPL=NPL+1
                      XPL(NPL)=FRXMIN
                      YPL(NPL)=FRYMAX
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                     ''Added corner 4 in - sense.'')')
                 ENDIF
190              CONTINUE
            ELSEIF(NINM.NE.0.OR.NINP.NE.0)THEN
                 PRINT *,' !!!!!! GRAREA WARNING : Error'//
     -                ' deciding direction ; polygon not drawn.'
                 IF(LGSTOP)THEN
                      OPEN(UNIT=12,FILE='grarea.dat',STATUS='UNKNOWN')
                      WRITE(12,*) EPSGX,EPSGY,EPSGZ,LEPSG
                      WRITE(12,*) FRXMIN,FRYMIN,FRXMAX,FRYMAX
                      WRITE(12,*) NIN
                      DO 230 J=1,NIN
                      WRITE(12,*) XIN(J),YIN(J)
230                   CONTINUE
                      CLOSE(12)
                      CALL QUIT
                 ENDIF
                 GOTO 3030
            ENDIF
       ENDIF
*** And plot the buffer.
300    CONTINUE
       IF(NPL.GT.2)CALL GFA(NPL,XPL,YPL)
       GOTO 3030
*** Buffer overflows.
3010   CONTINUE
       PRINT *,' !!!!!! GRAREA WARNING : Overflow of "out" buffer;'//
     -      ' polygon not plotted.'
       GOTO 3030
3020   CONTINUE
       PRINT *,' !!!!!! GRAREA WARNING : Overflow of "plot" buffer;'//
     -      ' polygon not plotted.'
       GOTO 3030
*** Termination.
3030   CONTINUE
       IF(RESET)CALL EPSSET('RESET',0.0D0,0.0D0,0.0D0)
       END
