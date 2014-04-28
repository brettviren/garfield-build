CDECK  ID>, SIGADD.
       SUBROUTINE SIGADD(ISW,CROSS,NSIG,TIME,SIG,Q,TMIN,TSHIFT,IFAIL)
*-----------------------------------------------------------------------
*   SIGADD - Adds a signal to the current signal banks.
*   (Last changed on 27/ 7/08.)
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
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL,
     -      BEMSET
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL,BEMSET
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       LOGICAL FPERX,FPERY,LCROSS,TRASET,TRAFLG,LITAIL,LDTAIL,LRTAIL,
     -      LEPULS,LIPULS,SIGSET,RESSET
       INTEGER NPAIR,ICLUST,NFOUR,MFEXP,MXMIN,MXMAX,
     -      MYMIN,MYMAX,NTRBNK,ITRMAJ,NTIME,NORIA,
     -      NASIMP,JIORD,NISIMP,NMQUAD,NCANG,IENANG
       REAL TIMSIG,SIGNAL,TCLUST,SCLUST,ACLUST,BCLUST,FCLUST,
     -      AVALAN,TSTART,TDEV,PRSTHR,
     -      TRABNK,TRAVEC
       CHARACTER*(MXCHAR) FCNANG
       CHARACTER*12 AVATYP
       CHARACTER*3 FCELTP
       COMMON /SIGDAT/ TIMSIG(MXLIST),SIGNAL(MXLIST,MXSW,2),
     -      AVALAN(2),TRAVEC(MXLIST),
     -      TRABNK(MXLIST,9),TSTART,TDEV,PRSTHR,
     -      TCLUST,SCLUST,ACLUST,BCLUST,FCLUST,ICLUST,NPAIR,
     -      NFOUR,ITRMAJ,JIORD,NISIMP,NMQUAD,IENANG,NTIME,NORIA,
     -      MFEXP,MXMIN,MXMAX,MYMIN,MYMAX,NTRBNK,NASIMP,NCANG,
     -      TRASET,TRAFLG(9),FPERX,FPERY,LCROSS,LITAIL,LDTAIL,LRTAIL,
     -      LEPULS,LIPULS,SIGSET,RESSET
       COMMON /SIGCHR/ FCELTP,AVATYP,FCNANG
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER ISW,NSIG,IFAIL,I,J,MSIG,KIORD
       REAL Q,TMIN,TSHIFT
       LOGICAL CROSS
       DOUBLE PRECISION TIME(*),SIG(*),TIMIN,TIMAX,TINT,SUM,CHECK
       DOUBLE PRECISION DIVDF2,TSIMP
       EXTERNAL DIVDF2
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE SIGADD (CERNLIB) ///'
*** Don't do anything if there are no points on the signal.
       IF(NSIG.LT.2)THEN
            IFAIL=0
            RETURN
       ENDIF
*** Assume that the routine will fail.
       IFAIL=1
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGADD DEBUG   : Adding a '',
     -      I4,''-vector to sense wire '',I4,'', terms='',I3,'',''/
     -      26X,''order='',I3,'', charge='',F10.3,'', tmin='',F10.3,
     -      '',''/26X,''shift='',F10.3)')
     -      NSIG,ISW,NISIMP,JIORD,Q,TMIN,TSHIFT
*** Ensure that the sense wire number is in range.
       IF(ISW.LE.0.OR.ISW.GT.NSW)THEN
            PRINT *,' !!!!!! SIGADD WARNING : Sense wire number out'//
     -           ' of range; signals not added.'
            RETURN
       ENDIF
*** Debugging output.
C       IF(LDEBUG)THEN
C            WRITE(LUNOUT,'(''  ++++++ SIGADD DEBUG   : Raw signal''/
C     -           ''   Slot  Time [microsec]  Signal [microA]''/)')
C            DO 40 I=1,NSIG
C            WRITE(LUNOUT,'(2X,I5,2X,E15.8,2X,E15.8)')
C     -           I,TIME(I),SIG(I)
C40          CONTINUE
C       ENDIF
*** Verify that the signal has no 2 equal times in succession.
       DO 100 I=2,NSIG
       IF(TIME(I).LE.TIME(I-1))THEN
            MSIG=I-1
            IF(MSIG.LT.NSIG-1)PRINT *,' !!!!!! SIGADD WARNING :'//
     -           ' Cutting signal at step ',MSIG,' out of ',NSIG,
     -           ' (equal time).'
            GOTO 110
       ENDIF
100    CONTINUE
       MSIG=NSIG
110    CONTINUE
*** Store the interpolation order.
       KIORD=MIN(JIORD,MSIG-1)
*** Debugging header.
C       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGADD DEBUG   :'',
C     -      '' Signal added to signal bank:''/
C     -      ''   Slot  Time [microsec]  Signal [microA]''/)')
*** Add the signal to the signal bank.
       CHECK=0
       DO 10 I=1,NTIME
       TINT=TIMSIG(I)-TSHIFT
**  Averageing mode: establish integration time window.
       IF(NISIMP.GT.0)THEN
*   Truncate the time window to overlap with the computed signal.
            TIMIN=MAX(TIME(1),TINT-DBLE(TDEV/2),DBLE(TMIN))
            TIMAX=MIN(TIME(NSIG),TINT+DBLE(TDEV/2))
*   Skip this point if there is no overlap.
            IF(TIMAX.LE.TIMIN)GOTO 10
**  Summing mode
       ELSEIF(NISIMP.EQ.-1)THEN
            TIMIN=TINT-DBLE(TDEV/2)
            TIMAX=TINT+DBLE(TDEV/2)
**  Sampling mode: just check the point is in the computed signal.
       ELSE
            IF(TINT.LT.TIME(1).OR.TINT.GT.TIME(NSIG))GOTO 10
       ENDIF
*   Newton-Raphson integration over this bin.
       IF(NISIMP.EQ.-1)THEN
            SUM=0
            DO 40 J=1,MSIG
            IF(TIME(J).LE.TIMAX.AND.TIME(J+1).GE.TIMIN)SUM=SUM+
     -           SIG(J)*(MIN(TIME(J+1),TIMAX)-MAX(TIME(J),TIMIN))/
     -           TDEV
40          CONTINUE
       ELSEIF(NISIMP.LE.0)THEN
            SUM=DIVDF2(SIG,TIME,MSIG,TINT,JIORD)
       ELSE
            DO 20 J=-NISIMP,NISIMP
            TSIMP=TIMIN+DBLE(J+NISIMP)*(TIMAX-TIMIN)/DBLE(2*NISIMP)
            IF(J.EQ.-NISIMP)THEN
                 SUM=DIVDF2(SIG,TIME,MSIG,TSIMP,KIORD)
            ELSEIF(J.EQ.NISIMP)THEN
                 SUM=SUM+DIVDF2(SIG,TIME,MSIG,TSIMP,KIORD)
            ELSEIF(J+NISIMP.EQ.2*((J+NISIMP)/2))THEN
                 SUM=SUM+2*DIVDF2(SIG,TIME,MSIG,TSIMP,KIORD)
            ELSE
                 SUM=SUM+4*DIVDF2(SIG,TIME,MSIG,TSIMP,KIORD)
            ENDIF
20          CONTINUE
       ENDIF
*   Normalise the integral if Simpson-Raphson was used.
       IF(NISIMP.GT.0)SUM=SUM*(TIMAX-TIMIN)/(6*NISIMP*TDEV)
*   Add the result to the signal.
       IF(CROSS)THEN
            SIGNAL(I,ISW,2)=SIGNAL(I,ISW,2)-ECHARG*1E12*Q*SUM
       ELSE
            SIGNAL(I,ISW,1)=SIGNAL(I,ISW,1)-ECHARG*1E12*Q*SUM
       ENDIF
*   Debugging.
C       IF(LDEBUG)WRITE(LUNOUT,'(2X,I5,2X,E15.8,2X,E15.8)')
C     -      I,TIMSIG(I),ECHARG*1E12*Q*SUM
       CHECK=CHECK+Q*SUM
10     CONTINUE
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGADD DEBUG   : Total'',
     -      '' added: '',E15.8)') CHECK
*** Seems to have worked since we got here.
       IFAIL=0
       END
