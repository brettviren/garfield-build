CDECK  ID>, IONBGN.
       SUBROUTINE IONBGN(IFAIL)
*-----------------------------------------------------------------------
*   IONBGN - Routine initialising the data set for the signal matrices.
*   VARIABLES : NPEREC       : Number of columns per wire record.
*               NRECMT       : Number of records per wire matrix.
*               IRECP0       : First plane record.
*               NRECS        : Total number of records on unit 13.
*               OPEN         : Used for checking the status of unit 13.
*   (Last changed on  9/11/98.)
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
       COMPLEX SIGMAT
       REAL QPLANE,EWXCOR,EWYCOR
       INTEGER IWORK,DUMMY
       COMMON /MATRIX/ SIGMAT(MXWIRE,MXWIRE),QPLANE(5,MXWIRE),
     -      IWORK(MXWIRE),DUMMY(2*MXWIRE+6)
       COMMON /SPLDAT/ EWXCOR(5),EWYCOR(5)
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
       INTEGER IFAIL,NPEREC,NRECMT,NRECS,IOS,IRECP0
       LOGICAL OPEN
       CHARACTER*(MXNAME) FILE
       COMMON /FILE13/ NPEREC,NRECMT,IRECP0,NRECS
       IFAIL=0
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE IONBGN ///'
*** Return if no file is needed.
       IF(.NOT.(FPERX.OR.FPERY))RETURN
       IFAIL=1
*** Determine the number of wire columns fitting in a record.
       NPEREC=INT((MXRECL-4)/(8*NWIRE))
*   Stop in case MXRECL is unacceptably small.
       IF(NPEREC.LE.0)THEN
            PRINT *,' ###### IONBGN ERROR   : Unable to allocate',
     -           ' storage space for the wire matrices, a MXRECL',
     -           ' of at least ',8*NWIRE+4,' is needed;'
            PRINT *,'                         Increase MXRECL if',
     -           ' possible or specify FOURIER 1.'
            RETURN
       ENDIF
*   Set NPEREC to NWIRE if the wire matrix fits in single record.
       IF(NPEREC.GT.NWIRE)NPEREC=NWIRE
*** Ensure that the plane matrix fits in a single record.
       IF(20*NWIRE+4.GT.MXRECL)THEN
            PRINT *,' ###### IONBGN ERROR   : Unable to allocate'//
     -           ' storage space for the plane matrices, a MXRECL'//
     -           ' of at least ',20*NWIRE+4,' is needed;'
            PRINT *,'                         Increase MXRECL if'//
     -           ' possible or specify FOURIER 1.'
            RETURN
       ENDIF
*** Determine number of records, first records per wire matrix.
       NRECMT=NWIRE/NPEREC
       IF(NPEREC*NRECMT.LT.NWIRE)NRECMT=NRECMT+1
*   Multiply by the number of Fourier copies of the matrix.
       IF(FPERX.AND.FPERY)THEN
            NRECS=NFOUR**2*NRECMT
       ELSEIF(FPERX.OR.FPERY)THEN
            NRECS=NFOUR*NRECMT
       ELSE
            NRECS=1
       ENDIF
*   Record the start of the plane records.
       IRECP0=NRECS+1
*   Add the plane records, each Fourier copy takes 1 record.
       IF(FPERX.AND.FPERY)THEN
            NRECS=NRECS+NFOUR**2
       ELSEIF(FPERX.OR.FPERY)THEN
            NRECS=NRECS+NFOUR
       ELSE
            NRECS=NRECS+1
       ENDIF
*   Check that it does not exceed 1000.
       IF(NRECS.GT.1000)THEN
            PRINT *,' ###### IONBGN ERROR   : Unable to allocate'//
     -           ' storage space for the plane matrices, maximum'//
     -           ' number of records in a direct access'
            PRINT *,'                         file would be exceeded;'//
     -           ' decrease the value of FOURIER or increase'//
     -           ' MXRECL if the disks allow.'
            RETURN
       ENDIF
*** Open the dataset, if it is not yet open.
       INQUIRE(UNIT=13,OPENED=OPEN)
       IF(OPEN)THEN
            PRINT *,' !!!!!! IONBGN WARNING : Unit 13 was still open'//
     -           ' and is now being closed (program bug)'
            CLOSE(UNIT=13,IOSTAT=IOS,ERR=2030)
       ENDIF
       OPEN(UNIT=13,STATUS='SCRATCH',FORM='UNFORMATTED',
     -       ACCESS='DIRECT',RECL=NPEREC*8*NWIRE+4,IOSTAT=IOS,ERR=2020)
       FILE='<Signal matrices>'
       CALL DSNLOG(FILE,'Scratch   ','Direct    ','Read/Write')
*   and set IFAIL to 0, since it apparently worked.
       IFAIL=0
       IF(LDEBUG)PRINT *,' ++++++ IONBGN DEBUG   : Unit 13 opened',
     -       ' with columns/rec=',NPEREC,',  rec/matrix=',NRECMT,
     -       ',  recl=',NPEREC*8*NWIRE+4,' byte,   records=',NRECS
       RETURN
*** Handle the error conditions.
2020   CONTINUE
       PRINT *,' ###### IONBGN ERROR   : Unable to open scratch'//
     -      ' data set on unit 13 (used for signal matrices);'//
     -      ' ion tails cannot be calculated.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=13,IOSTAT=IOS,ERR=2030)
       RETURN
2030   CONTINUE
       PRINT *,' ###### IONBGN ERROR   : Unable to close scratch'//
     -      ' data set on unit 13 (attempted because of previous'//
     -      ' error condition).'
       CALL INPIOS(IOS)
       END
