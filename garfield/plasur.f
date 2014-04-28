CDECK  ID>, PLASUR.
       SUBROUTINE PLASUR(SURF,IDIM,XVEC,YVEC,NX,NY,PHI,THETA,
     -      XTXT,YTXT,ZTXT,TITLE,OPTION)
*-----------------------------------------------------------------------
*   PLASUR - Plots a surface
*   (Last changed on 26/10/07.)
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
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER NX,NY,IX,IY,I,J,ICOL,IREF,IVOL,IFAIL,IFAIL1,IFAIL2,NU,
     -      IF,IT,IDIM,IPRSAV(3)
       REAL SURF(IDIM,*),XVEC(NX),YVEC(NY),PHI,THETA,
     -      QXMIN,QYMIN,QZMIN,QXMAX,QYMAX,QZMAX,DX,DY,DZ
       DOUBLE PRECISION A,B,C,DET,XPL(4),YPL(4),ZPL(4),
     -      VXMIN,VYMIN,VXMAX,VYMAX,XU(12),YU(12),ZU(12),FMIN,FMAX,
     -      XT(3),YT(3),ZT(3),FNORM,D12,D23,D13,
     -      FPMSAV(3,3),FPRSAV(3,3),FSAVA,FSAVB,FSAVC,FSAVD,FSAVN,
     -      PALSAV,PBLSAV,PCLSAV,SCX,SCY,SCZ
       CHARACTER*(*) XTXT,YTXT,ZTXT,TITLE,OPTION
       LOGICAL LXGR,LYGR,LZGR
*** Decode options.
       LXGR=.FALSE.
       LYGR=.FALSE.
       LZGR=.FALSE.
       IF(INDEX(OPTION,'NOX-GRID').NE.0)THEN
            LXGR=.FALSE.
       ELSEIF(INDEX(OPTION,'X-GRID').NE.0)THEN
            LXGR=.TRUE.
       ENDIF
       IF(INDEX(OPTION,'NOY-GRID').NE.0)THEN
            LYGR=.FALSE.
       ELSEIF(INDEX(OPTION,'Y-GRID').NE.0)THEN
            LYGR=.TRUE.
       ENDIF
       IF(INDEX(OPTION,'NOZ-GRID').NE.0)THEN
            LZGR=.FALSE.
       ELSEIF(INDEX(OPTION,'Z-GRID').NE.0)THEN
            LZGR=.TRUE.
       ENDIF
*** Start progress printing.
       CALL PROINT('SURFACE',1,6)
*** Determine limits of the box.
       CALL PROFLD(1,'Determining ranges',-1.0)
       QXMIN=XVEC(1)
       QXMAX=XVEC(1)
       DO 100 I=2,NX
       IF(XVEC(I).LT.QXMIN)QXMIN=XVEC(I)
       IF(XVEC(I).GT.QXMAX)QXMAX=XVEC(I)
100    CONTINUE
       QYMIN=YVEC(1)
       QYMAX=YVEC(1)
       DO 110 I=2,NY
       IF(YVEC(I).LT.QYMIN)QYMIN=YVEC(I)
       IF(YVEC(I).GT.QYMAX)QYMAX=YVEC(I)
110    CONTINUE
       QZMIN=SURF(1,1)
       QZMAX=SURF(1,1)
       DO 120 I=1,NX
       DO 130 J=1,NY
       IF(SURF(I,J).LT.QZMIN)QZMIN=SURF(I,J)
       IF(SURF(I,J).GT.QZMAX)QZMAX=SURF(I,J)
130    CONTINUE
120    CONTINUE
*   Ensure every range is non-zero.
       IF(QXMIN.GE.QXMAX.OR.QYMIN.GE.QYMAX.OR.QZMIN.GE.QZMAX)THEN
            PRINT *,' !!!!!! PLASUR WARNING : Zero range in one'//
     -           ' or more dimensions; no plot.'
            CALL PROEND
            GOTO 1000
       ENDIF
*   Drawing margins.
       DX=0.005*(QXMAX-QXMIN)
       DY=0.005*(QYMAX-QYMIN)
       DZ=0.005*(QZMAX-QZMIN)
*   Enlarge the vertical range for better plotting.
       QZMIN=QZMIN-2*DZ
       QZMAX=QZMAX+2*DZ
       CALL GRASET(QXMIN,QYMIN,QZMIN,QXMAX,QYMAX,QZMAX)
*   Set geometrical tolerances.
       CALL EPSSET('SET',1D-7*(QXMAX-QXMIN),1D-7*(QYMAX-QYMIN),
     -      1D-7*(QZMAX-QZMIN))
*   Update margins according options.
       IF(.NOT.LXGR)DX=0
       IF(.NOT.LYGR)DY=0
       IF(.NOT.LZGR)DZ=0
*** Establish the projection matrix.
       CALL PROFLD(1,'Computing projection matrix',-1.0)
*   Save old values.
       DO 200 I=1,3
       IPRSAV(I)=IPRMAT(I)
       DO 210 J=1,3
       FPMSAV(I,J)=FPRMAT(I,J)
       FPRSAV(I,J)=FPROJ(I,J)
210    CONTINUE
200    CONTINUE
       FSAVA=FPROJA
       FSAVB=FPROJB
       FSAVC=FPROJC
       FSAVD=FPROJD
       FSAVN=FPROJN
       PALSAV=PRAL
       PBLSAV=PRBL
       PCLSAV=PRCL
*   Compute the stretching factors.
       SCX=1
       SCY=(QYMAX-QYMIN)/(QXMAX-QXMIN)
       SCZ=(QZMAX-QZMIN)/(QXMAX-QXMIN)
*   Rotation matrix for the given angles.
       FPROJ(1,1)=SCX* COS(PHI)
       FPROJ(1,2)=-SCY*SIN(PHI)
       FPROJ(1,3)=SCZ* 0
       FPROJ(2,1)=SCX* SIN(PHI)*COS(THETA)
       FPROJ(2,2)=SCY* COS(PHI)*COS(THETA)
       FPROJ(2,3)=SCZ* SIN(THETA)
       FPROJ(3,1)=-SCX*SIN(PHI)*SIN(THETA)
       FPROJ(3,2)=-SCY*COS(PHI)*SIN(THETA)
       FPROJ(3,3)=SCZ* COS(THETA)
*   Store the plane parameters.
       FPROJA=FPROJ(3,1)
       FPROJB=FPROJ(3,2)
       FPROJC=FPROJ(3,3)
       FPROJN=1
       FPROJD=1
*   Prepare the projection matrix.
       FPRMAT(1,1)=FPROJ(1,1)
       FPRMAT(2,1)=FPROJ(1,2)
       FPRMAT(3,1)=FPROJ(1,3)
       FPRMAT(1,2)=FPROJ(2,1)
       FPRMAT(2,2)=FPROJ(2,2)
       FPRMAT(3,2)=FPROJ(2,3)
       FPRMAT(1,3)=FPROJA
       FPRMAT(2,3)=FPROJB
       FPRMAT(3,3)=FPROJC
*   Solve the matrix.
       CALL DFACT(3,FPRMAT,3,IPRMAT,IFAIL1,DET,IFAIL2)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLASUR DEBUG   :'',
     -      '' Determinant of projection: '',E15.8)') DET
       IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)THEN
            PRINT *,' !!!!!! PLASUR WARNING  : Unable to solve'//
     -           ' the projection matrix; reset to default.'
            CALL PROEND
            GOTO 1000
       ENDIF
*   Establish light direction.
       PRAL=+COS(PRPHIL)*COS(PRTHL)*FPROJA-SIN(PRPHIL)*FPROJB+
     -       COS(PRPHIL)*SIN(PRTHL)*FPROJC
       PRBL=+SIN(PRPHIL)*COS(PRTHL)*FPROJA+COS(PRPHIL)*FPROJB+
     -       SIN(PRPHIL)*SIN(PRTHL)*FPROJC
       PRCL=            -SIN(PRTHL)*FPROJA+
     -                   COS(PRTHL)*FPROJC
       FNORM=SQRT(PRAL**2+PRBL**2+PRCL**2)
       IF(FNORM.GT.0)THEN
            PRAL=PRAL/FNORM
            PRBL=PRBL/FNORM
            PRCL=PRCL/FNORM
       ENDIF
*** Initialise the panel buffer.
       CALL PROFLD(1,'Initialising buffer',-1.0)
       CALL PLABU1('RESET',IREF,0,XPL,YPL,ZPL,
     -      0.0D0,0.0D0,0.0D0,0,0,IFAIL)
*** Set a volume indicator.
       IVOL=1
*** Generate a rainbow colour table.
       IF(ICOLRB.EQ.0)THEN
            CALL PROFLD(1,'Making colour table',-1.0)
            ICOLRB=ICOL0
            CALL COLRBW(ICOLRB)
            ICOL0=ICOL0+NPRCOL
       ENDIF
*** Loop over the matrix.
       CALL PROFLD(1,'Projecting surface',REAL(NX-1))
       DO 10 IX=1,NX-1
       CALL PROSTA(1,REAL(IX))
       DO 20 IY=1,NY-1
*   Store the outline of the entire panel.
       XPL(1)=XVEC(IX)+DX
       YPL(1)=YVEC(IY)+DY
       ZPL(1)=SURF(IX,IY)
       XPL(2)=XVEC(IX+1)-DX
       YPL(2)=YVEC(IY)+DY
       ZPL(2)=SURF(IX+1,IY)
       XPL(3)=XVEC(IX+1)-DX
       YPL(3)=YVEC(IY+1)-DY
       ZPL(3)=SURF(IX+1,IY+1)
       XPL(4)=XVEC(IX)+DX
       YPL(4)=YVEC(IY+1)-DY
       ZPL(4)=SURF(IX,IY+1)
*   Loop over the function values.
       DO 30 IF=1,NPRCOL-1
       FMIN=QZMIN+DBLE(IF-1)*(QZMAX-QZMIN)/DBLE(NPRCOL-1)+DZ/2
       FMAX=QZMIN+DBLE(IF  )*(QZMAX-QZMIN)/DBLE(NPRCOL-1)-DZ/2
       IF(  FMIN.GT.MAX(SURF(IX,IY),SURF(IX+1,IY),
     -                  SURF(IX+1,IY+1),SURF(IX,IY+1)).OR.
     -      FMAX.LT.MIN(SURF(IX,IY),SURF(IX+1,IY),
     -                  SURF(IX+1,IY+1),SURF(IX,IY+1)))GOTO 30
*   Find the section of the panel for this function value range.
       CALL PLATRI(4,XPL,YPL,ZPL,NU,XU,YU,ZU,FMIN,FMAX)
       IF(NU.LT.3)GOTO 30
*   Set the colour index.
       ICOL=ICOLRB+IF-1
*   Break the resulting curve into triangles.
       XT(1)=XU(1)
       YT(1)=YU(1)
       ZT(1)=ZU(1)
       DO 40 IT=3,NU
       XT(2)=XU(IT-1)
       YT(2)=YU(IT-1)
       ZT(2)=ZU(IT-1)
       XT(3)=XU(IT)
       YT(3)=YU(IT)
       ZT(3)=ZU(IT)
*   Compute a normal vector of the 1st panel.
       DET=-XT(3)*YT(2)*ZT(1)+XT(2)*YT(3)*ZT(1)+
     -      XT(3)*YT(1)*ZT(2)-XT(1)*YT(3)*ZT(2)-
     -      XT(2)*YT(1)*ZT(3)+XT(1)*YT(2)*ZT(3)
       IF(DET.NE.0)THEN
            A=( YT(2)*ZT(1)-YT(3)*ZT(1)-YT(1)*ZT(2)+
     -           YT(3)*ZT(2)+YT(1)*ZT(3)-YT(2)*ZT(3))/DET
            B=(-XT(2)*ZT(1)+XT(3)*ZT(1)+XT(1)*ZT(2)-
     -           XT(3)*ZT(2)-XT(1)*ZT(3)+XT(2)*ZT(3))/DET
            C=( XT(2)*YT(1)-XT(3)*YT(1)-XT(1)*YT(2)+
     -           XT(3)*YT(2)+XT(1)*YT(3)-XT(2)*YT(3))/DET
       ELSE
            D12= (YT(1)*ZT(2)-ZT(1)*YT(2))**2+
     -           (ZT(1)*XT(2)-XT(1)*ZT(2))**2+
     -           (XT(1)*YT(2)-YT(1)*XT(2))**2
            D13= (YT(1)*ZT(3)-ZT(1)*YT(3))**2+
     -           (ZT(1)*XT(3)-XT(1)*ZT(3))**2+
     -           (XT(1)*YT(3)-YT(1)*XT(3))**2
            D23= (YT(2)*ZT(3)-ZT(2)*YT(3))**2+
     -           (ZT(2)*XT(3)-XT(2)*ZT(3))**2+
     -           (XT(2)*YT(3)-YT(2)*XT(3))**2
            IF(D12.GE.D13.AND.D12.GE.D23)THEN
                 A=YT(1)*ZT(2)-ZT(1)*YT(2)
                 B=ZT(1)*XT(2)-XT(1)*ZT(2)
                 C=XT(1)*YT(2)-YT(1)*XT(2)
            ELSEIF(D13.GE.D12.AND.D13.GE.D23)THEN
                 A=YT(1)*ZT(3)-ZT(1)*YT(3)
                 B=ZT(1)*XT(3)-XT(1)*ZT(3)
                 C=XT(1)*YT(3)-YT(1)*XT(3)
            ELSE
                 A=YT(2)*ZT(3)-ZT(2)*YT(3)
                 B=ZT(2)*XT(3)-XT(2)*ZT(3)
                 C=XT(2)*YT(3)-YT(2)*XT(3)
            ENDIF
       ENDIF
*   Normalise.
       FNORM=SQRT(A**2+B**2+C**2)
       IF(FNORM.LE.0)THEN
            PRINT *,' !!!!!! PLASUR WARNING : Panel with zero-norm'//
     -           ' normal vector; panel skipped.'
            GOTO 40
       ELSE
            A=A/FNORM
            B=B/FNORM
            C=C/FNORM
       ENDIF
*   Store the 1st panel in the buffer.
       IF(A*FPROJA+B*FPROJB+C*FPROJC.LE.0)THEN
            CALL PLABU1('STORE',IREF,3,XT,YT,ZT,-A,-B,-C,
     -           ICOL,IVOL,IFAIL)
       ELSE
            CALL PLABU1('STORE',IREF,3,XT,YT,ZT,A,B,C,
     -           ICOL,IVOL,IFAIL)
       ENDIF
       IF(IFAIL.NE.0)PRINT *,' !!!!!! PLASUR WARNING : Unable to'//
     -      ' store a panel of the surface.'
40     CONTINUE
30     CONTINUE
20     CONTINUE
10     CONTINUE
*** Plot the frame.
       CALL PROFLD(1,'Plotting frame',-1.0)
       CALL GRAXI3(VXMIN,VYMIN,VXMAX,VYMAX,XTXT,YTXT,ZTXT,TITLE,
     -      'PLOT,NOCELL')
*** Project the panels.
       CALL PROFLD(1,'Projecting panels',-1.0)
       CALL PLASRP
       CALL PROEND
*** Plot the panels.
       CALL PLAPLT
*** Next plot.
       CALL GRNEXT
*** Continue here in case of errors.
1000   CONTINUE
*** Reset tolerances.
       CALL EPSSET('RESET',0.0D0,0.0D0,0.0D0)
*** Restore projection matrices.
       DO 220 I=1,3
       IPRMAT(I)=IPRSAV(I)
       DO 230 J=1,3
       FPRMAT(I,J)=FPMSAV(I,J)
       FPROJ(I,J)=FPRSAV(I,J)
230    CONTINUE
220    CONTINUE
       FPROJA=FSAVA
       FPROJB=FSAVB
       FPROJC=FSAVC
       FPROJD=FSAVD
       FPROJN=FSAVN
       PRAL=PALSAV
       PRBL=PBLSAV
       PRCL=PCLSAV
       END
