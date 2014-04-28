CDECK  ID>, SIGIPR.
       SUBROUTINE SIGIPR(IFAIL)
*-----------------------------------------------------------------------
*   SIGIPR - Prepares the ion tail calculation by filling the signal
*            matrices (ie non-periodic capacitance matrices),
*            Fourier transforming them if necessary, inverting them and
*            Fourier transforming them back. Because of the large number
*            of terms involved, a (scratch) external file on unit 13 is
*            used to store the intermediate and final results. This file
*            is handled by the routines IONBGN and IONIO.
*   VARIABLES : FFTMAT      : Matrix used for Fourier transforms.
*   (Last changed on  4/10/06.)
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
       COMPLEX SIGMAT
       REAL QPLANE,EWXCOR,EWYCOR
       INTEGER IWORK,DUMMY
       COMMON /MATRIX/ SIGMAT(MXWIRE,MXWIRE),QPLANE(5,MXWIRE),
     -      IWORK(MXWIRE),DUMMY(2*MXWIRE+6)
       COMMON /SPLDAT/ EWXCOR(5),EWYCOR(5)
       COMPLEX FFTMAT(MXFOUR,MXWIRE)
       INTEGER IFAIL,MX,MY,I,J,II,JJ,M,IOS
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE SIGIPR ///'
*** Set some parameters.
       IFAIL=0
*** Book the signal matrices.
       CALL BOOK('BOOK','MATRIX','SIGNAL',IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! SIGIPR WARNING : Unable to obtain'//
     -           ' signal matrix storage; no induced currents.'
            RETURN
       ENDIF
*** Open unit 13 for writing of matrices if Fourier transf. are needed.
       IF(FPERX.OR.FPERY)THEN
            CALL IONBGN(IFAIL)
            IF(IFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! SIGIPR WARNING : No storage'//
     -                ' available for the signal matrices; no'//
     -                ' induced currents.'
                 RETURN
            ENDIF
       ENDIF
*** Have the matrix/matrices filled (and stored).
       DO 10 MX=MXMIN,MXMAX
       DO 20 MY=MYMIN,MYMAX
*   Select layer to be produced.
       IF(FCELTP.EQ.'A  ')THEN
            CALL IPRA00(MX,MY)
       ELSEIF(FCELTP.EQ.'B2X')THEN
            CALL IPRB2X(MY)
       ELSEIF(FCELTP.EQ.'B2Y')THEN
            CALL IPRB2Y(MX)
       ELSEIF(FCELTP.EQ.'C2X')THEN
            CALL IPRC2X
       ELSEIF(FCELTP.EQ.'C2Y')THEN
            CALL IPRC2Y
       ELSEIF(FCELTP.EQ.'C3 ')THEN
            CALL IPRC30
       ELSEIF(FCELTP.EQ.'D1 ')THEN
            CALL IPRD10
       ELSEIF(FCELTP.EQ.'D3 ')THEN
            CALL IPRD30
       ELSE
            PRINT *,' !!!!!! SIGIPR WARNING : Unknown signal cell'//
     -           ' type ',FCELTP,' see; abandoned.'
            RETURN
       ENDIF
       IF(LDEBUG)PRINT *,' ++++++ SIGIPR DEBUG   : Signal matrix MX=',
     -      MX,' MY=',MY,' has been calculated.'
*   Store the matrix.
       IF(FPERX.OR.FPERY)CALL IONIO(MX,MY,1,0,IFAIL)
*   Quit if storing failed.
       IF(IFAIL.NE.0)GOTO 2010
*   Dump the signal matrix before inversion, if DEBUG is requested.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(/''  ++++++ SIGIPR DEBUG   : Dump of'',
     -           '' signal matrix ('',I2,'','',I2,'') before'',
     -           '' inversion follows:''/)') MX,MY
            DO 710 I=0,NWIRE-1,10
            DO 720 J=0,NWIRE-1,10
            WRITE(LUNOUT,'(''  Re-Block '',I2,''.'',I2/)') I/10,J/10
            DO 730 II=1,10
            IF(I+II.GT.NWIRE)GOTO 730
            WRITE(LUNOUT,'(2X,10(E12.5,1X:))')
     -           (REAL(SIGMAT(I+II,J+JJ)),JJ=1,MIN(NWIRE-J,10))
730         CONTINUE
            WRITE(LUNOUT,'(''  Im-Block '',I2,''.'',I2/)') I/10,J/10
            DO 740 II=1,10
            IF(I+II.GT.NWIRE)GOTO 740
            WRITE(LUNOUT,'(2X,10(E12.5,1X:))')
     -           (AIMAG(SIGMAT(I+II,J+JJ)),JJ=1,MIN(NWIRE-J,10))
740         CONTINUE
720         CONTINUE
710         CONTINUE
            WRITE(LUNOUT,'(/''  ++++++ SIGIPR DEBUG   : End of the'',
     -           '' uninverted capacitance matrix dump.''/)')
       ENDIF
*   Next layer.
20     CONTINUE
10     CONTINUE
*** Have them fourier transformed (singly periodic case).
       IF((FPERX.AND..NOT.FPERY).OR.(FPERY.AND..NOT.FPERX))THEN
            DO 30 I=1,NWIRE
            DO 40 M=-NFOUR/2+1,NFOUR/2
            CALL IONIO(M,M,2,I,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
            DO 50 J=1,NWIRE
            FFTMAT(M+NFOUR/2,J)=SIGMAT(I,J)
50          CONTINUE
40          CONTINUE
            DO 60 J=1,NWIRE
            CALL CFFT(FFTMAT(1,J),MFEXP)
60          CONTINUE
            DO 70 M=-NFOUR/2+1,NFOUR/2
            CALL IONIO(M,M,2,I,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
            DO 80 J=1,NWIRE
            SIGMAT(I,J)=FFTMAT(M+NFOUR/2,J)
80          CONTINUE
            CALL IONIO(M,M,1,I,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
70          CONTINUE
30          CONTINUE
       ENDIF
*   have them fourier transformed (doubly periodic case).
       IF(FPERX.AND.FPERY)THEN
            DO 100 I=1,NWIRE
            DO 110 MX=MXMIN,MXMAX
            DO 120 MY=MYMIN,MYMAX
            CALL IONIO(MX,MY,2,I,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
            DO 130 J=1,NWIRE
            FFTMAT(MY+NFOUR/2,J)=SIGMAT(I,J)
130         CONTINUE
120         CONTINUE
            DO 140 J=1,NWIRE
            CALL CFFT(FFTMAT(1,J),MFEXP)
140         CONTINUE
            DO 150 MY=MYMIN,MYMAX
            CALL IONIO(MX,MY,2,I,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
            DO 160 J=1,NWIRE
            SIGMAT(I,J)=FFTMAT(MY+NFOUR/2,J)
160         CONTINUE
            CALL IONIO(MX,MY,1,I,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
150         CONTINUE
110         CONTINUE
            DO 170 MY=MYMIN,MYMAX
            DO 180 MX=MXMIN,MXMAX
            CALL IONIO(MX,MY,2,I,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
            DO 190 J=1,NWIRE
            FFTMAT(MX+NFOUR/2,J)=SIGMAT(I,J)
190         CONTINUE
180         CONTINUE
            DO 200 J=1,NWIRE
            CALL CFFT(FFTMAT(1,J),MFEXP)
200         CONTINUE
            DO 210 MX=MXMIN,MXMAX
            CALL IONIO(MX,MY,2,I,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
            DO 220 J=1,NWIRE
            SIGMAT(I,J)=FFTMAT(MX+NFOUR/2,J)
220         CONTINUE
            CALL IONIO(MX,MY,1,I,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
210         CONTINUE
170         CONTINUE
100         CONTINUE
       ENDIF
*** Invert the matrices.
       DO 300 MX=MXMIN,MXMAX
       DO 310 MY=MYMIN,MYMAX
*   Retrieve the layer.
       IF(FPERX.OR.FPERY)THEN
            CALL IONIO(MX,MY,2,0,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
       ENDIF
*   Invert.
       IF(NWIRE.GE.1)CALL CINV(NWIRE,SIGMAT,MXWIRE,IWORK,IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! SIGIPR WARNING : Inversion of signal',
     -           ' matrix (',MX,',',MY,') failed; no reliable',
     -           ' results; ion tail preparation is abandoned.'
            IFAIL=1
            RETURN
       ENDIF
*   Store the matrix back.
       IF(FPERX.OR.FPERY)THEN
            CALL IONIO(MX,MY,1,0,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
       ENDIF
*   Next layer.
310    CONTINUE
300    CONTINUE
*** And transform the matrices back to the original domain.
       IF((FPERX.AND..NOT.FPERY).OR.(FPERY.AND..NOT.FPERX))THEN
            DO 410 I=1,NWIRE
            DO 420 M=-NFOUR/2+1,NFOUR/2
            CALL IONIO(M,M,2,I,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
            DO 430 J=1,NWIRE
            FFTMAT(M+NFOUR/2,J)=SIGMAT(I,J)
430         CONTINUE
420         CONTINUE
            DO 440 J=1,NWIRE
            CALL CFFT(FFTMAT(1,J),-MFEXP)
440         CONTINUE
            DO 450 M=-NFOUR/2+1,NFOUR/2
            CALL IONIO(M,M,2,I,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
            DO 460 J=1,NWIRE
            SIGMAT(I,J)=FFTMAT(M+NFOUR/2,J)/NFOUR
460         CONTINUE
            CALL IONIO(M,M,1,I,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
450         CONTINUE
410         CONTINUE
       ENDIF
*   have them transformed to the original domain (doubly periodic).
       IF(FPERX.AND.FPERY)THEN
            DO 500 I=1,NWIRE
            DO 510 MX=MXMIN,MXMAX
            DO 520 MY=MYMIN,MYMAX
            CALL IONIO(MX,MY,2,I,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
            DO 530 J=1,NWIRE
            FFTMAT(MY+NFOUR/2,J)=SIGMAT(I,J)
530         CONTINUE
520         CONTINUE
            DO 540 J=1,NWIRE
            CALL CFFT(FFTMAT(1,J),-MFEXP)
540         CONTINUE
            DO 550 MY=MYMIN,MYMAX
            CALL IONIO(MX,MY,2,I,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
            DO 560 J=1,NWIRE
            SIGMAT(I,J)=FFTMAT(MY+NFOUR/2,J)/NFOUR
560         CONTINUE
            CALL IONIO(MX,MY,1,I,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
550         CONTINUE
510         CONTINUE
            DO 570 MY=MYMIN,MYMAX
            DO 580 MX=MXMIN,MXMAX
            CALL IONIO(MX,MY,2,I,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
            DO 590 J=1,NWIRE
            FFTMAT(MX+NFOUR/2,J)=SIGMAT(I,J)
590         CONTINUE
580         CONTINUE
            DO 600 J=1,NWIRE
            CALL CFFT(FFTMAT(1,J),-MFEXP)
600         CONTINUE
            DO 610 MX=MXMIN,MXMAX
            CALL IONIO(MX,MY,2,I,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
            DO 620 J=1,NWIRE
            SIGMAT(I,J)=FFTMAT(MX+NFOUR/2,J)/NFOUR
620         CONTINUE
            CALL IONIO(MX,MY,1,I,IFAIL)
            IF(IFAIL.NE.0)GOTO 2010
610         CONTINUE
570         CONTINUE
500         CONTINUE
       ENDIF
*** Dump the signal matrix after inversion, if DEBUG is requested.
       IF(LDEBUG)THEN
            DO 750 MX=MXMIN,MXMAX
            DO 760 MY=MYMIN,MYMAX
            WRITE(LUNOUT,'(/''  ++++++ SIGIPR DEBUG   : Dump of'',
     -           '' signal matrix ('',I2,'','',I2,'') after'',
     -           '' inversion follows:''/)') MX,MY
            DO 770 I=0,NWIRE-1,10
            DO 780 J=0,NWIRE-1,10
            WRITE(LUNOUT,'(''  Re-Block '',I2,''.'',I2/)') I/10,J/10
            DO 790 II=1,10
            IF(I+II.GT.NWIRE)GOTO 790
            WRITE(LUNOUT,'(2X,10(E12.5,1X:))')
     -           (REAL(SIGMAT(I+II,J+JJ)),JJ=1,MIN(NWIRE-J,10))
790         CONTINUE
            WRITE(LUNOUT,'(''  Im-Block '',I2,''.'',I2/)') I/10,J/10
            DO 800 II=1,10
            IF(I+II.GT.NWIRE)GOTO 800
            WRITE(LUNOUT,'(2X,10(E12.5,1X:))')
     -           (AIMAG(SIGMAT(I+II,J+JJ)),JJ=1,MIN(NWIRE-J,10))
800         CONTINUE
780         CONTINUE
770         CONTINUE
            WRITE(LUNOUT,'(/''  ++++++ SIGIPR DEBUG   : End of the'',
     -           '' inverted capacitance matrix dump.''/)')
760         CONTINUE
750         CONTINUE
       ENDIF
*** Register the amount of CPU time used for these manipulations.
       CALL TIMLOG('Preparing the ion tail calculation:     ')
       RETURN
*** Handle error conditions.
2010   CONTINUE
       PRINT *,' !!!!!! SIGIPR WARNING : Ion tail preparation stopped'//
     -      ' because of an I/O error; resubmit or set'//
     -      ' fourier to 1 (see writeup)'
       CALL INPIOS(IOS)
       CLOSE(UNIT=13,IOSTAT=IOS,ERR=2030)
       IFAIL=1
       RETURN
2030   CONTINUE
       PRINT *,' ###### SIGIPR ERROR   : Problems closing scratch'//
     -      ' data set on unit 13 (used for intermediate'//
     -      ' results)'
       PRINT *,'                         CLOSE was attempted because'//
     -      ' of a previous error condition'
       CALL INPIOS(IOS)
       IFAIL=1
       END
