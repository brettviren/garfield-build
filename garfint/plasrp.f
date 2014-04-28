CDECK  ID>, PLASRP.
       SUBROUTINE PLASRP
*-----------------------------------------------------------------------
*   PLASRP - Cuts the current set of planes to avoid overlaps and sorts
*            them for plotting, version for 3D impressions.
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
       INTEGER NPL1,NPL2,IVOL1,ICOL1,ICOL2,IFAIL1,IFAIL2,
     -      I,J,K,L,NREF,NFIRST,NLAST,NNLAST,
     -      IREF,IREFL(2*MXPLAN),IREFO(MXPLAN),NPLAN1,NPLAN2
       DOUBLE PRECISION XPL1(MXEDGE),YPL1(MXEDGE),ZPL1(MXEDGE),
     -      XPL2(MXEDGE),YPL2(MXEDGE),ZPL2(MXEDGE),
     -      APL1,BPL1,CPL1,DPL1,APL2,BPL2,CPL2,DPL2,
     -      XCUT,YCUT,ZCUT,XEMIN,XEMAX,YEMIN,YEMAX,ZEMIN,ZEMAX
C       DOUBLE PRECISION VEC(3),FNORM
       LOGICAL PLAGT,KEEP,MARK(2*MXPLAN)
       EXTERNAL PLAGT
*** Identification output.
       IF(LIDENT)PRINT *,' /// ROUTINE PLASRP ///'
*** Set the tolerances.
       CALL PLACO3(GXMIN,GYMIN,GZMIN,XCUT,YCUT,ZCUT)
       XEMIN=XCUT
       YEMIN=YCUT
       ZEMIN=ZCUT
       XEMAX=XCUT
       YEMAX=YCUT
       ZEMAX=ZCUT
       CALL PLACO3(GXMIN,GYMIN,GZMAX,XCUT,YCUT,ZCUT)
       IF(XCUT.LT.XEMIN)XEMIN=XCUT
       IF(XCUT.GT.XEMAX)XEMAX=XCUT
       IF(YCUT.LT.YEMIN)YEMIN=YCUT
       IF(YCUT.GT.YEMAX)YEMAX=YCUT
       IF(ZCUT.LT.ZEMIN)ZEMIN=ZCUT
       IF(ZCUT.GT.ZEMAX)ZEMAX=ZCUT
       CALL PLACO3(GXMIN,GYMAX,GZMIN,XCUT,YCUT,ZCUT)
       IF(XCUT.LT.XEMIN)XEMIN=XCUT
       IF(XCUT.GT.XEMAX)XEMAX=XCUT
       IF(YCUT.LT.YEMIN)YEMIN=YCUT
       IF(YCUT.GT.YEMAX)YEMAX=YCUT
       IF(ZCUT.LT.ZEMIN)ZEMIN=ZCUT
       IF(ZCUT.GT.ZEMAX)ZEMAX=ZCUT
       CALL PLACO3(GXMIN,GYMAX,GZMAX,XCUT,YCUT,ZCUT)
       IF(XCUT.LT.XEMIN)XEMIN=XCUT
       IF(XCUT.GT.XEMAX)XEMAX=XCUT
       IF(YCUT.LT.YEMIN)YEMIN=YCUT
       IF(YCUT.GT.YEMAX)YEMAX=YCUT
       IF(ZCUT.LT.ZEMIN)ZEMIN=ZCUT
       IF(ZCUT.GT.ZEMAX)ZEMAX=ZCUT
       CALL PLACO3(GXMAX,GYMIN,GZMIN,XCUT,YCUT,ZCUT)
       IF(XCUT.LT.XEMIN)XEMIN=XCUT
       IF(XCUT.GT.XEMAX)XEMAX=XCUT
       IF(YCUT.LT.YEMIN)YEMIN=YCUT
       IF(YCUT.GT.YEMAX)YEMAX=YCUT
       IF(ZCUT.LT.ZEMIN)ZEMIN=ZCUT
       IF(ZCUT.GT.ZEMAX)ZEMAX=ZCUT
       CALL PLACO3(GXMAX,GYMIN,GZMAX,XCUT,YCUT,ZCUT)
       IF(XCUT.LT.XEMIN)XEMIN=XCUT
       IF(XCUT.GT.XEMAX)XEMAX=XCUT
       IF(YCUT.LT.YEMIN)YEMIN=YCUT
       IF(YCUT.GT.YEMAX)YEMAX=YCUT
       IF(ZCUT.LT.ZEMIN)ZEMIN=ZCUT
       IF(ZCUT.GT.ZEMAX)ZEMAX=ZCUT
       CALL PLACO3(GXMAX,GYMAX,GZMIN,XCUT,YCUT,ZCUT)
       IF(XCUT.LT.XEMIN)XEMIN=XCUT
       IF(XCUT.GT.XEMAX)XEMAX=XCUT
       IF(YCUT.LT.YEMIN)YEMIN=YCUT
       IF(YCUT.GT.YEMAX)YEMAX=YCUT
       IF(ZCUT.LT.ZEMIN)ZEMIN=ZCUT
       IF(ZCUT.GT.ZEMAX)ZEMAX=ZCUT
       CALL PLACO3(GXMAX,GYMAX,GZMAX,XCUT,YCUT,ZCUT)
       IF(XCUT.LT.XEMIN)XEMIN=XCUT
       IF(XCUT.GT.XEMAX)XEMAX=XCUT
       IF(YCUT.LT.YEMIN)YEMIN=YCUT
       IF(YCUT.GT.YEMAX)YEMAX=YCUT
       IF(ZCUT.LT.ZEMIN)ZEMIN=ZCUT
       IF(ZCUT.GT.ZEMAX)ZEMAX=ZCUT
       CALL EPSSET('SET',1D-7*(XEMAX-XEMIN),1D-7*(YEMAX-YEMIN),
     -      1D-7*(ZEMAX-ZEMIN))
*** Progress printing.
       CALL PROFLD(1,'Counting planes',-1.0)
       CALL PROSTA(1,0.0)
*** Find out how many planes are in store.
       CALL PLABU1('QUERY',NPLAN1,NPL1,XPL1,YPL1,ZPL1,APL1,BPL1,CPL1,
     -      ICOL1,IVOL1,IFAIL1)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLASRP DEBUG   : Found '',
     -      I5,'' geometric panels.'')') NPLAN1
*** Reset the plot-plane buffer.
       CALL PROFLD(1,'Projecting planes',REAL(NPLAN1))
       CALL PLABU2('RESET',IREF,NPL1,XPL2,YPL2,ZPL2,
     -      APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL2)
*** Project the planes.
       NPLAN2=0
       DO 10 I=1,NPLAN1
       CALL PROSTA(1,REAL(I))
*   Read plane.
       CALL PLABU1('READ',I,NPL1,XPL1,YPL1,ZPL1,APL1,BPL1,CPL1,ICOL1,
     -      IVOL1,IFAIL1)
*   Skip empty and deleted planes.
       IF(IFAIL1.NE.0.OR.NPL1.LT.3)GOTO 10
*   Ensure that the plane is visible.
       IF(APL1*FPROJA+BPL1*FPROJB+CPL1*FPROJC.LT.
     -      1D-6*SQRT((APL1**2+BPL1**2+CPL1**2)*
     -      (FPROJA**2+FPROJB**2+FPROJC**2)))THEN
            GOTO 10
       ENDIF
*   Project points, adjusting to box dimensions, also compute offset.
C      IF(IVOL1.GT.0)THEN
            CALL PLAPOL(GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -           XPL1,YPL1,ZPL1,NPL1,APL1,BPL1,CPL1,XPL2,YPL2,ZPL2,NPL2)
C      ELSE
C           DO 30 J=1,NPL1
C           CALL PLACO3(XPL1(J),YPL1(J),ZPL1(J),XPL2(J),YPL2(J),ZPL2(J))
C30         CONTINUE
C           NPL2=NPL1
C      ENDIF
*   Verify the resulting plane.
       CALL PLACHK(NPL2,XPL2,YPL2,ZPL2,IFAIL2)
       IF(IFAIL2.NE.0)GOTO 10
*   Compute the norm vector of the projected plane and re-check.
C       VEC(1)=APL1
C       VEC(2)=BPL1
C       VEC(3)=CPL1
C       CALL DFEQN(3,FPRMAT,3,IPRMAT,1,VEC)
C       FNORM=SQRT(VEC(1)**2+VEC(2)**2+VEC(3)**2)
C       IF(FNORM.LE.0.OR.NPL2.LE.2)THEN
C            PRINT *,' !!!!!! PLASRP WARNING : Unable to project a'//
C     -           ' panel; panel skipped.'
C            GOTO 10
C       ENDIF
C       APL2=VEC(1)/FNORM
C       BPL2=VEC(2)/FNORM
C       CPL2=VEC(3)/FNORM
C       DPL2=0
C       DO 20 J=1,NPL2
C       DPL2=DPL2+APL2*XPL2(J)+BPL2*YPL2(J)+CPL2*ZPL2(J)
C20     CONTINUE
C       DPL2=DPL2/NPL2
       CALL PLANOR(NPL2,XPL2,YPL2,ZPL2,APL2,BPL2,CPL2,DPL2,IFAIL2)
       IF(IFAIL2.NE.0)THEN
            PRINT *,' !!!!!! PLASRP WARNING : Unable to project a'//
     -           ' panel; panel skipped.'
            GOTO 10
       ENDIF
*   Skip planes perpendicular to the view.
       IF(ABS(CPL2).LT.1.0E-2*SQRT(APL2**2+BPL2**2))GOTO 10
*   Store the projected plane.
       CALL PLABU2('STORE',IREF,NPL2,XPL2,YPL2,ZPL2,
     -      APL2,BPL2,CPL2,DPL2,ICOL1,IFAIL2)
       IF(IFAIL2.NE.0)THEN
            PRINT *,' !!!!!! PLASRP WARNING : Storage error for a'//
     -           ' projected plane ; plot likely to be incomplete.'
       ELSE
            IF(NPLAN2.GE.2*MXPLAN)GOTO 3010
            NPLAN2=NPLAN2+1
            IREFL(NPLAN2)=IREF
       ENDIF
10     CONTINUE
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLASRP DEBUG   : Created '',
     -      I5,'' projected planes.'')') NPLAN2
*** Split planes that have hide each other in part.
       IF(LSPLIT)THEN
            CALL PROFLD(1,'Cutting overlaps',REAL(NPLAN2))
**  Loop over plane I, which is the one being cut.
            NFIRST=NPLAN2+1
            DO 100 I=1,NPLAN2
*   Progress printing.
            CALL PROSTA(1,REAL(I))
*   Set the initial mark value.
            MARK(I)=.FALSE.
*   Copy its reference to the end.
            IREFL(NFIRST)=IREFL(I)
*   Initialise the counter of planes generated sofar.
            NLAST=NFIRST
**  Loop over plane J, which is the one that cuts.
            DO 110 J=1,NPLAN2
            IF(I.EQ.J)GOTO 110
**  Cut plane I with all other planes.
            NNLAST=NLAST
            DO 120 K=NFIRST,NNLAST
            IF(IREFL(K).EQ.0)GOTO 120
*   Perform the actual split.
            LGSIG=.FALSE.
            CALL PLASPL(IREFL(K),IREFL(J),NREF,IREFO,KEEP,IFAIL1)
*   Debugging output and quit when stop flag is set.
            IF(LGSTOP.AND.LGSIG)THEN
                 PRINT *,' !!!!!! PLASRP WARNING : Separation error'//
     -                ' detected ; generating dump and quitting.'
                 CALL PLABU2('READ',IREFL(K),NPL1,XPL1,YPL1,ZPL1,
     -                APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
                 CALL PLABU2('READ',IREFL(J),NPL2,XPL2,YPL2,ZPL2,
     -                APL2,BPL2,CPL2,DPL2,ICOL2,IFAIL2)
                 OPEN(UNIT=12,FILE='plaspl.dat',STATUS='UNKNOWN')
                 WRITE(12,*) EPSGX,EPSGY,EPSGZ,LEPSG
                 WRITE(12,*) APL1,BPL1,CPL1,DPL1,ICOL1
                 WRITE(12,*) NPL1
                 DO 200 L=1,NPL1
                 WRITE(12,*) XPL1(L),YPL1(L),ZPL1(L)
200              CONTINUE
                 WRITE(12,*) APL2,BPL2,CPL2,DPL2,ICOL2
                 WRITE(12,*) NPL2
                 DO 210 L=1,NPL2
                 WRITE(12,*) XPL2(L),YPL2(L),ZPL2(L)
210              CONTINUE
                 CLOSE(12)
                 CALL QUIT
            ENDIF
*   Store the result, delete the original.
            IF(IFAIL1.EQ.0.AND..NOT.KEEP)THEN
                 IF(IREFL(K).NE.IREFL(I))THEN
                      CALL PLABU2('DELETE',IREFL(K),
     -                     NPL1,XPL1,YPL1,ZPL1,APL1,BPL1,CPL1,DPL1,
     -                     ICOL1,IFAIL1)
                 ELSE
                      MARK(I)=.TRUE.
                 ENDIF
                 IREFL(K)=0
                 IF(NREF.EQ.1.AND.IREFO(1).NE.0)THEN
                      IREFL(K)=IREFO(1)
                 ELSE
                      DO 130 L=1,NREF
                      IF(IREFO(L).NE.0)THEN
                           IF(NLAST.GE.2*MXPLAN)GOTO 3010
                           NLAST=NLAST+1
                           IREFL(NLAST)=IREFO(L)
                      ENDIF
130                   CONTINUE
                 ENDIF
            ELSEIF(.NOT.KEEP)THEN
                 PRINT *,' !!!!!! PLASRP WARNING : Unable to remove;'//
     -                ' invisible parts ; keeping original.'
            ENDIF
120         CONTINUE
**  Compress the list.
            NNLAST=NLAST
            NLAST=NFIRST-1
            DO 140 K=NFIRST,NNLAST
            IF(IREFL(K).EQ.0)GOTO 140
            NLAST=NLAST+1
            IREFL(NLAST)=IREFL(K)
140         CONTINUE
*   If there is not a single plane left, stop cutting.
            IF(NLAST.LT.NFIRST)GOTO 100
**  Next plane that cuts.
110         CONTINUE
**  Next plane being cut, update the start of list marker.
            IF(NLAST.GE.2*MXPLAN)GOTO 3010
            NFIRST=NLAST+1
100         CONTINUE
**  Remove the original planes.
            DO 150 I=1,NPLAN2
            IF(MARK(I))CALL PLABU2('DELETE',IREFL(I),
     -           NPL1,XPL1,YPL1,ZPL1,APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
150         CONTINUE
       ENDIF
*** Sort the planes so that the backmost plane is plotted first.
       NQ=0
       CALL PROFLD(1,'Counting planes',-1.0)
       CALL PROSTA(1,0.0)
       DO 300 I=1,MXPLAN
*   Read the plane.
       CALL PLABU2('READ',I,NPL1,XPL1,YPL1,ZPL1,APL1,BPL1,CPL1,DPL1,
     -      ICOL1,IFAIL1)
*   Skip if deleted or empty.
       IF(IFAIL1.NE.0.OR.NPL1.LE.2)GOTO 300
*   Compute largest offset.
       NQ=NQ+1
*   Store reference.
       IQ(NQ)=I
300    CONTINUE
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLASRP DEBUG   : Created '',
     -      I5,'' visible planes.'')') NQ
*   Sort the planes.
       IF(LSORT)THEN
            CALL PROFLD(1,'Sorting planes',-1.0)
            CALL PROSTA(1,0.0)
            CALL BSORT(IQ,NQ,PLAGT)
       ENDIF
       RETURN
*** Error processing.
3010   CONTINUE
       PRINT *,' !!!!!! PLASRP WARNING : Removing invisible parts'//
     -      ' generated too many sub-panels ; aborted.'
       END
