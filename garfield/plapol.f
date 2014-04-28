CDECK  ID>, PLAPOL.
       SUBROUTINE PLAPOL(XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,XIN,YIN,ZIN,NIN,
     -      A,B,C,XPL,YPL,ZPL,NPL)
*-----------------------------------------------------------------------
*   PLAPOL - Cuts a box with a polygon.
*   (Last changed on 30/ 9/98.)
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
       INTEGER NIN,NPL,IFAIL,NBOX,I,J
       DOUBLE PRECISION XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,
     -      XIN(NIN),YIN(NIN),ZIN(NIN),
     -      A,B,C,EPSX,EPSY,EPSZ,ZAUX1,ZAUX2,
     -      XPOL(MXEDGE),YPOL(MXEDGE),ZPOL(MXEDGE),XAUX,YAUX,ZAUX,
     -      XBOX(12),YBOX(12),ZBOX(12),
     -      XPL(MXEDGE),YPL(MXEDGE),ZPL(MXEDGE),
     -      X0,Y0,Z0,X1,Y1,Z1
       LOGICAL ONLIND,ADD,SKIP,INSIDE,EDGE
       EXTERNAL ONLIND
C      print *,' PLAPOL - Number of points: ',NIN
C      do i=1,nin
C      print '(3f12.5)',xin(i),yin(i),zin(i)
C      enddo
C      print *,' PLAPOL - Plane: ',a,b,c
*** Make sure there is at least 1 input point.
       IF(NIN.LT.0)THEN
            NPL=0
            RETURN
*   Check that there is enough storage space.
       ELSEIF(NIN.GT.MXEDGE)THEN
            PRINT *,' !!!!!! PLAPOL WARNING : Array dimensions are'//
     -           ' not sufficient ; no plot vector returned.'
            NPL=0
            RETURN
       ENDIF
*** Compute the, at most, 6 distinct crossings between plane and box.
       NBOX=0
       CALL PLALIN(XMIN,YMIN,ZMIN,XMAX,YMIN,ZMIN,XIN(1),YIN(1),ZIN(1),
     -      A,B,C,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NBOX=NBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,XBOX(NBOX),YBOX(NBOX),ZBOX(NBOX))
       ENDIF
       CALL PLALIN(XMIN,YMIN,ZMIN,XMIN,YMAX,ZMIN,XIN(1),YIN(1),ZIN(1),
     -      A,B,C,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NBOX=NBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,XBOX(NBOX),YBOX(NBOX),ZBOX(NBOX))
       ENDIF
       CALL PLALIN(XMIN,YMIN,ZMIN,XMIN,YMIN,ZMAX,XIN(1),YIN(1),ZIN(1),
     -      A,B,C,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NBOX=NBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,XBOX(NBOX),YBOX(NBOX),ZBOX(NBOX))
       ENDIF
       CALL PLALIN(XMAX,YMAX,ZMIN,XMAX,YMIN,ZMIN,XIN(1),YIN(1),ZIN(1),
     -      A,B,C,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NBOX=NBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,XBOX(NBOX),YBOX(NBOX),ZBOX(NBOX))
       ENDIF
       CALL PLALIN(XMAX,YMAX,ZMIN,XMIN,YMAX,ZMIN,XIN(1),YIN(1),ZIN(1),
     -      A,B,C,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NBOX=NBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,XBOX(NBOX),YBOX(NBOX),ZBOX(NBOX))
       ENDIF
       CALL PLALIN(XMAX,YMAX,ZMIN,XMAX,YMAX,ZMAX,XIN(1),YIN(1),ZIN(1),
     -      A,B,C,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NBOX=NBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,XBOX(NBOX),YBOX(NBOX),ZBOX(NBOX))
       ENDIF
       CALL PLALIN(XMAX,YMIN,ZMAX,XMAX,YMIN,ZMIN,XIN(1),YIN(1),ZIN(1),
     -      A,B,C,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NBOX=NBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,XBOX(NBOX),YBOX(NBOX),ZBOX(NBOX))
       ENDIF
       CALL PLALIN(XMAX,YMIN,ZMAX,XMAX,YMAX,ZMAX,XIN(1),YIN(1),ZIN(1),
     -      A,B,C,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NBOX=NBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,XBOX(NBOX),YBOX(NBOX),ZBOX(NBOX))
       ENDIF
       CALL PLALIN(XMAX,YMIN,ZMAX,XMIN,YMIN,ZMAX,XIN(1),YIN(1),ZIN(1),
     -      A,B,C,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NBOX=NBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,XBOX(NBOX),YBOX(NBOX),ZBOX(NBOX))
       ENDIF
       CALL PLALIN(XMIN,YMAX,ZMAX,XMIN,YMAX,ZMIN,XIN(1),YIN(1),ZIN(1),
     -      A,B,C,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NBOX=NBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,XBOX(NBOX),YBOX(NBOX),ZBOX(NBOX))
       ENDIF
       CALL PLALIN(XMIN,YMAX,ZMAX,XMIN,YMIN,ZMAX,XIN(1),YIN(1),ZIN(1),
     -      A,B,C,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NBOX=NBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,XBOX(NBOX),YBOX(NBOX),ZBOX(NBOX))
       ENDIF
       CALL PLALIN(XMIN,YMAX,ZMAX,XMAX,YMAX,ZMAX,XIN(1),YIN(1),ZIN(1),
     -      A,B,C,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NBOX=NBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,XBOX(NBOX),YBOX(NBOX),ZBOX(NBOX))
       ENDIF
*** If there are no box points, there can't be an intersect.
       IF(NBOX.LE.0)THEN
C      print *,' Polygon plane does not cross the box'
            NPL=0
            RETURN
       ENDIF
*** Ensure there is no butterfly.
C      print *,' Box before butterfly: '
C      do i=1,nbox
C      print '(3e12.5)',xbox(i),ybox(i),zbox(i)
C      enddo
       CALL BUTFLD(NBOX,XBOX,YBOX,ZBOX)
C      call gsln(2)
C      call gpl2(nbox,xbox,ybox)
C      call guwk(0,1)
C      print *,' Number of box points: ',nbox
*** Set tolerances.
       IF(LEPSG)THEN
            EPSX=EPSGX
            EPSY=EPSGY
            EPSZ=EPSGZ
       ELSE
            EPSX=1.0D-8*ABS(XMAX-XMIN)
            EPSY=1.0D-8*ABS(YMAX-YMIN)
            EPSZ=1.0D-8*ABS(ZMAX-ZMIN)
            IF(EPSX.LE.0)EPSX=1.0E-8
            IF(EPSY.LE.0)EPSY=1.0E-8
            IF(EPSZ.LE.0)EPSZ=1.0E-8
       ENDIF
*** Compute projections of the input points.
       DO 20 I=1,NIN
       CALL PLACO3(XIN(I),YIN(I),ZIN(I),XPOL(I),YPOL(I),ZPOL(I))
20     CONTINUE
C      call gpl2(nin,xpol,ypol)
C      call guwk(0,1)
*** Next find the intersections between the two sets.
       NPL=0
       DO 40 J=1,NIN
C      print *,' Polygon corner ',J,' : ',xpol(j),ypol(j)
*   Set flag to see whether we search for mid-line intersects.
       SKIP=.FALSE.
*   Scan the box.
       DO 30 I=1,NBOX
*   See whether the polygon start is on any of the box edges.
       IF(ONLIND(XBOX(1+MOD(I-1,NBOX)),YBOX(1+MOD(I-1,NBOX)),
     -      XBOX(1+MOD(I,NBOX)),YBOX(1+MOD(I,NBOX)),
     -      XPOL(J),YPOL(J)).AND.
     -      XIN(J).GE.XMIN-EPSX.AND.XIN(J).LE.XMAX+EPSX.AND.
     -      YIN(J).GE.YMIN-EPSY.AND.YIN(J).LE.YMAX+EPSY.AND.
     -      ZIN(J).GE.ZMIN-EPSZ.AND.ZIN(J).LE.ZMAX+EPSZ)THEN
            NPL=NPL+1
            XPL(NPL)=XPOL(J)
            YPL(NPL)=YPOL(J)
            ZPL(NPL)=ZPOL(J)
C      print *,' Polygon corner on box line: ',xpl(npl),ypl(npl),
C     -     zpl(npl)
            SKIP=.TRUE.
       ENDIF
*   See whether a box corner is on this polygon segment.
       IF(ONLIND(XPOL(1+MOD(J-1,NIN)),YPOL(1+MOD(J-1,NIN)),
     -      XPOL(1+MOD(J,NIN)),YPOL(1+MOD(J,NIN)),
     -      XBOX(I),YBOX(I)))THEN
            NPL=NPL+1
            XPL(NPL)=XBOX(I)
            YPL(NPL)=YBOX(I)
            ZPL(NPL)=ZBOX(I)
C      print *,' Box corner on polygon line: ',xpl(npl),ypl(npl),
C     -     zpl(npl)
            SKIP=.TRUE.
       ENDIF
30     CONTINUE
*   Make sure that the polygon segment at least crosses the box.
       X0=XIN(1+MOD(J-1,NIN))
       Y0=YIN(1+MOD(J-1,NIN))
       Z0=ZIN(1+MOD(J-1,NIN))
       X1=XIN(1+MOD(J  ,NIN))
       Y1=YIN(1+MOD(J  ,NIN))
       Z1=ZIN(1+MOD(J  ,NIN))
       IF(.NOT.(((ABS(X0-XMIN).LT.EPSX.AND.ABS(X1-XMIN).LT.EPSX).OR.
     -           (ABS(X0-XMAX).LT.EPSX.AND.ABS(X1-XMAX).LT.EPSX)).AND.
     -      ((YMIN-Y0)*(Y0-YMAX).GE.0.OR.(YMIN-Y1)*(Y1-YMAX).GE.0).AND.
     -      ((ZMIN-Z0)*(Z0-ZMAX).GE.0.OR.(ZMIN-Z1)*(Z1-ZMAX).GE.0).OR.
     -          ((ABS(Y0-YMIN).LT.EPSY.AND.ABS(Y1-YMIN).LT.EPSY).OR.
     -           (ABS(Y0-YMAX).LT.EPSY.AND.ABS(Y1-YMAX).LT.EPSY)).AND.
     -      ((XMIN-X0)*(X0-XMAX).GE.0.OR.(XMIN-X1)*(X1-XMAX).GE.0).AND.
     -      ((ZMIN-Z0)*(Z0-ZMAX).GE.0.OR.(ZMIN-Z1)*(Z1-ZMAX).GE.0).OR.
     -          ((ABS(Z0-ZMIN).LT.EPSZ.AND.ABS(Z1-ZMIN).LT.EPSZ).OR.
     -           (ABS(Z0-ZMAX).LT.EPSZ.AND.ABS(Z1-ZMAX).LT.EPSZ)).AND.
     -      ((XMIN-X0)*(X0-XMAX).GE.0.OR.(XMIN-X1)*(X1-XMAX).GE.0).AND.
     -      ((YMIN-Y0)*(Y0-YMAX).GE.0.OR.(YMIN-Y1)*(Y1-YMAX).GE.0)))THEN
            CALL CLIP3D(X0,Y0,Z0,X1,Y1,Z1,XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,
     -           IFAIL)
C      if(ifail.ne.0)print *,' Segment not through volume.'
C      if(ifail.ne.0)print '(2x,3f12.5)',x0,y0,z0
C      if(ifail.ne.0)print '(2x,3f12.5)',x1,y1,z1
            IF(IFAIL.NE.0)SKIP=.TRUE.
C      else
C      print *,' Line segment on outer box.'
       ENDIF
*   If neither of this happened, look for mid-line intersects.
       IF(.NOT.SKIP)THEN
            DO 100 I=1,NBOX
            CALL CRSPND(XBOX(1+MOD(I-1,NBOX)),YBOX(1+MOD(I-1,NBOX)),
     -           XBOX(1+MOD(I  ,NBOX)),YBOX(1+MOD(I  ,NBOX)),
     -           XPOL(1+MOD(J-1,NIN )),YPOL(1+MOD(J-1,NIN )),
     -           XPOL(1+MOD(J  ,NIN )),YPOL(1+MOD(J  ,NIN )),
     -           XAUX,YAUX,ADD)
            IF(ADD)THEN
                 NPL=NPL+1
                 XPL(NPL)=XAUX
                 YPL(NPL)=YAUX
                 IF(XBOX(1+MOD(I,NBOX)).EQ.XBOX(1+MOD(I-1,NBOX)).AND.
     -                YBOX(1+MOD(I,NBOX)).EQ.YBOX(1+MOD(I-1,NBOX)))THEN
                      PRINT *,' !!!!!! PLAPOL WARNING : Unable to'//
     -                     ' compute intersect offset ; skipped.'
                      NPL=NPL-1
                      GOTO 100
                 ELSEIF(ABS(XBOX(1+MOD(I,NBOX))-
     -                XBOX(1+MOD(I-1,NBOX))).GT.
     -                ABS(YBOX(1+MOD(I,NBOX))-
     -                YBOX(1+MOD(I-1,NBOX))))THEN
                      ZAUX1=ZBOX(1+MOD(I-1,NBOX))+
     -                     (XAUX-XBOX(1+MOD(I-1,NBOX)))*
     -                     (ZBOX(1+MOD(I,NBOX))-ZBOX(1+MOD(I-1,NBOX)))/
     -                     (XBOX(1+MOD(I,NBOX))-XBOX(1+MOD(I-1,NBOX)))
                 ELSE
                      ZAUX1=ZBOX(1+MOD(I-1,NBOX))+
     -                     (YAUX-YBOX(1+MOD(I-1,NBOX)))*
     -                     (ZBOX(1+MOD(I,NBOX))-ZBOX(1+MOD(I-1,NBOX)))/
     -                     (YBOX(1+MOD(I,NBOX))-YBOX(1+MOD(I-1,NBOX)))
                 ENDIF
                 IF(XPOL(1+MOD(J,NIN)).EQ.XPOL(1+MOD(J-1,NIN)).AND.
     -                YPOL(1+MOD(J,NIN)).EQ.YPOL(1+MOD(J-1,NIN)))THEN
                      PRINT *,' !!!!!! PLAPOL WARNING : Unable to'//
     -                     ' compute intersect offset ; skipped.'
                      NPL=NPL-1
                      GOTO 100
                 ELSEIF(ABS(XPOL(1+MOD(J,NIN))-XPOL(1+MOD(J-1,NIN))).GT.
     -                ABS(YPOL(1+MOD(J,NIN))-YPOL(1+MOD(J-1,NIN))))THEN
                      ZAUX2=ZPOL(1+MOD(J-1,NIN))+
     -                     (XAUX-XPOL(1+MOD(J-1,NIN)))*
     -                     (ZPOL(1+MOD(J,NIN))-ZPOL(1+MOD(J-1,NIN)))/
     -                     (XPOL(1+MOD(J,NIN))-XPOL(1+MOD(J-1,NIN)))
                 ELSE
                      ZAUX2=ZPOL(1+MOD(J-1,NIN))+
     -                     (YAUX-YPOL(1+MOD(J-1,NIN)))*
     -                     (ZPOL(1+MOD(J,NIN))-ZPOL(1+MOD(J-1,NIN)))/
     -                     (YPOL(1+MOD(J,NIN))-YPOL(1+MOD(J-1,NIN)))
                 ENDIF
                 ZPL(NPL)=0.5*(ZAUX1+ZAUX2)
C      print *,' Offsets:       ',zaux1,zaux2,zpl(npl)
C      print *,' Line crossing: ',xpl(npl),ypl(npl),zpl(npl)
            ENDIF
100         CONTINUE
       ENDIF
40     CONTINUE
*** Find the vertices of the box internal to the polygon.
       DO 50 I=1,NBOX
C      print *,' Box ',i,':',xbox(i),ybox(i),zbox(i)
       CALL INTERD(NIN,XPOL,YPOL,XBOX(I),YBOX(I),INSIDE,EDGE)
*   Skip box corners on the polygon.
       IF(EDGE)GOTO 50
*   Add internal points.
       IF(INSIDE)THEN
            NPL=NPL+1
            XPL(NPL)=XBOX(I)
            YPL(NPL)=YBOX(I)
            ZPL(NPL)=ZBOX(I)
C      print *,' box in polygon: ',xpl(npl),ypl(npl),zpl(npl)
       ENDIF
50     CONTINUE
*** Find the vertices of the polygon internal to the box.
       DO 70 I=1,NIN
C      print *,' Pol ',i,':',xpol(i),ypol(i),zpol(i)
*   Skip points which were not inside the box.
       IF(XIN(I).LT.XMIN-EPSX.OR.XIN(I).GT.XMAX+EPSX.OR.
     -      YIN(I).LT.YMIN-EPSY.OR.YIN(I).GT.YMAX+EPSY.OR.
     -      ZIN(I).LT.ZMIN-EPSZ.OR.ZIN(I).GT.ZMAX+EPSZ)GOTO 70
*   Check whether the point is internal.
       CALL INTERD(NBOX,XBOX,YBOX,XPOL(I),YPOL(I),INSIDE,EDGE)
*   Skip polygon corners on the box.
       IF(EDGE)GOTO 70
*   Add internal points.
       IF(INSIDE)THEN
            NPL=NPL+1
            XPL(NPL)=XPOL(I)
            YPL(NPL)=YPOL(I)
            ZPL(NPL)=ZPOL(I)
C      print *,' polygon in box: ',xpl(npl),ypl(npl),zpl(npl)
       ENDIF
70     CONTINUE
*** Ensure there is no butterfly.
C      print *,' Checking for butterfly'
       CALL BUTFLD(NPL,XPL,YPL,ZPL)
C      print *,' Continue ? Enter an integer.'
C      read *,j
C      call gsln(1)
C      call gpl2(npl,xpl,ypl)
C      call guwk(0,1)
C      print *,' PLAPOL - Final result, NPL=',npl
C      do i=1,npl
C      print '(3f12.5)',xpl(i),ypl(i),zpl(i)
C      enddo
       END
