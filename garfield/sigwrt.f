CDECK  ID>, SIGWRT.
       SUBROUTINE SIGWRT
*-----------------------------------------------------------------------
*   SIGWRT - A routine that writes the signals to a file
*   VARIABLES : VALID      : Valid dataset available, if set to .TRUE.
*               FILE etc   : Data on the file to be written.
*   (Last changed on 19/ 1/11.)
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
       CHARACTER*(MXCHAR) STRING
       CHARACTER*(MXNAME) FILE,AUX
       CHARACTER*80 FCNWRC,UNIT
       CHARACTER*29 REMARK
       CHARACTER*10 VARLIS(MXVAR)
       CHARACTER*8 TIME,DATE,MEMBER,FORMAT
       LOGICAL FLAG(MXWORD+3),VALID,IFWRT(MXLIST),USE(MXVAR)
C       LOGICAL EXMEMB
       INTEGER INPCMP,IENTRY,NCWRC,I,NWORD,INEXT,NCFILE,NCMEMB,NCREM,
     -      ISW,IOS,NWRITE,J,MODVAR(MXVAR),IFAIL,MODRES(1),NRES,NOUT,
     -      NCUNIT,NCAUX
       REAL RES(1),VAR(MXVAR),SCALET,SCALEI
       EXTERNAL INPCMP
       SAVE VALID,FILE,NCFILE,MEMBER,NCMEMB,REMARK,NCREM,FORMAT,
     -      IENTRY,NCWRC,SCALET,SCALEI
*** Initialise the various parameters.
       DATA VALID /.FALSE./
       DATA FILE   /' '/
       DATA MEMBER /'< none >'/
       DATA REMARK /'none'/
       DATA FORMAT /'SPICE   '/
       DATA NCFILE,NCMEMB,NCREM /1,8,4/
       DATA IENTRY /0/
       DATA SCALET /1.0/, SCALEI /1.0/
       FCNWRC=' '
       NCWRC=0
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE SIGWRT ///'
*** Get the number of words, return if there is only one.
       CALL INPNUM(NWORD)
       IF(NWORD.LE.1)THEN
            PRINT *,' !!!!!! SIGWRT WARNING : WRITE takes at least 1'//
     -           ' argument (a dataset name); data will not be written.'
            RETURN
       ENDIF
**  Mark keywords.
       DO 10 I=1,NWORD+3
       FLAG(I)=.TRUE.
       IF(I.GT.NWORD)GOTO 10
       IF(INPCMP(I,'D#ATASET')+INPCMP(I,'FILE-#NAME')+
     -      INPCMP(I,'R#EMARK')+
     -      INPCMP(I,'WR#ITE-IF')+INPCMP(I,'F#ORMAT')+
     -      INPCMP(I,'U#NITS').EQ.0)FLAG(I)=.FALSE.
10     CONTINUE
**  Loop over the words.
       INEXT=2
       DO 20 I=2,NWORD
       IF(I.LT.INEXT)GOTO 20
*   Look for a DATASET.
       IF(INPCMP(I,'D#ATASET')+INPCMP(I,'FILE-#NAME').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'The dataset name is missing.  ')
                 INEXT=I+1
            ELSE
                 CALL INPSTR(I+1,I+1,STRING,NCFILE)
                 FILE=STRING
                 INEXT=I+2
                 IF(.NOT.FLAG(I+2))THEN
                      CALL INPSTR(I+2,I+2,STRING,NCMEMB)
                      MEMBER=STRING
                      INEXT=I+3
                 ENDIF
                 VALID=.TRUE.
            ENDIF
*   FORMAT specification, either SPICE or SCEPTRE.
       ELSEIF(INPCMP(I,'F#ORMAT').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'No format specification found.')
                 INEXT=I+1
            ELSE
                 IF(INPCMP(I+1,'SC#EPTRE').NE.0)THEN
                      FORMAT='SCEPTRE '
                 ELSEIF(INPCMP(I+1,'SP#ICE').NE.0)THEN
                      FORMAT='SPICE   '
                 ELSEIF(INPCMP(I+1,'SOR#IN').NE.0)THEN
                      FORMAT='SORIN    '
                 ELSE
                      CALL INPMSG(I+1,'Not a known dataset format.   ')
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Remark.
       ELSEIF(INPCMP(I,'REM#ARK').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'The remark is missing.        ')
                 INEXT=I+1
            ELSE
                 CALL INPSTR(I+1,I+1,STRING,NCREM)
                 REMARK=STRING
                 INEXT=I+2
            ENDIF
*   Look for a write condition.
       ELSEIF(INPCMP(I,'WR#ITE-IF').NE.0)THEN
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'The function is not specified.')
            ELSE
                 CALL INPSTR(I+1,I+1,FCNWRC,NCWRC)
                 IF(NCWRC.GT.0.AND.IENTRY.GT.0)THEN
                      CALL ALGCLR(IENTRY)
                      IENTRY=0
                 ENDIF
            ENDIF
            INEXT=I+2
*   Units.
       ELSEIF(INPCMP(I,'U#NITS').NE.0)THEN
            DO 30 J=I+1,NWORD
            IF(FLAG(J))THEN
                 INEXT=J
                 GOTO 20
            ELSEIF(INPCMP(J,'SEC#ONDS').NE.0)THEN
                 SCALET=1E-6
            ELSEIF(INPCMP(J,'MIL#LI-SEC#ONDS')+
     -           INPCMP(J,'MILLISEC#ONDS')+
     -           INPCMP(J,'MSEC#ONDS').NE.0)THEN
                 SCALET=1E-3
            ELSEIF(INPCMP(J,'MIC#RO-SEC#ONDS')+
     -           INPCMP(J,'MICROSEC#ONDS')+
     -           INPCMP(J,'MUSEC#ONDS').NE.0)THEN
                 SCALET=1
            ELSEIF(INPCMP(J,'N#ANO-SEC#ONDS')+
     -           INPCMP(J,'NANOSEC#ONDS')+
     -           INPCMP(J,'NSEC#ONDS').NE.0)THEN
                 SCALET=1E+3
            ELSEIF(INPCMP(J,'P#ICO-SEC#ONDS')+
     -           INPCMP(J,'PICOSEC#ONDS')+
     -           INPCMP(J,'PSEC#ONDS').NE.0)THEN
                 SCALET=1E+6
            ELSEIF(INPCMP(J,'F#EMTO-SEC#ONDS')+
     -           INPCMP(J,'F#EMTOSEC#ONDS')+
     -           INPCMP(J,'FSEC#ONDS').NE.0)THEN
                 SCALET=1E+9
            ELSEIF(INPCMP(J,'A#TTO-SEC#ONDS')+
     -           INPCMP(J,'ATTOSEC#ONDS')+
     -           INPCMP(J,'ASEC#ONDS').NE.0)THEN
                 SCALET=1E+12
            ELSEIF(INPCMP(J,'KI#LO-A#MPERES')+
     -           INPCMP(J,'KILOA#MPERES')+
     -           INPCMP(J,'KA#MPERES').NE.0)THEN
                 SCALEI=1E-9
            ELSEIF(INPCMP(J,'A#MPERES').NE.0)THEN
                 SCALEI=1E-6
            ELSEIF(INPCMP(J,'MIL#LI-A#MPERES')+
     -           INPCMP(J,'MILLIA#MPERES')+
     -           INPCMP(J,'MA#MPERES').NE.0)THEN
                 SCALEI=1E-3
            ELSEIF(INPCMP(J,'MIC#RO-A#MPERES')+
     -           INPCMP(J,'MICROA#MPERES')+
     -           INPCMP(J,'MUA#MPERES').NE.0)THEN
                 SCALEI=1
            ELSEIF(INPCMP(J,'N#ANO-A#MPERES')+
     -           INPCMP(J,'NANOA#MPERES')+
     -           INPCMP(J,'NA#MPERES').NE.0)THEN
                 SCALEI=1E+3
            ELSEIF(INPCMP(J,'P#ICO-A#MPERES')+
     -           INPCMP(J,'PICOA#MPERES')+
     -           INPCMP(J,'PA#MPERES').NE.0)THEN
                 SCALEI=1E+6
            ELSEIF(INPCMP(J,'F#EMTO-A#MPERES')+
     -           INPCMP(J,'F#EMTOA#MPERES')+
     -           INPCMP(J,'FA#MPERES').NE.0)THEN
                 SCALEI=1E+9
            ELSEIF(INPCMP(J,'A#TTO-A#MPERES')+
     -           INPCMP(J,'ATTOA#MPERES')+
     -           INPCMP(J,'AA#MPERES').NE.0)THEN
                 SCALEI=1E+12
            ELSE
                 CALL INPMSG(J,'Not a known unit.')
            ENDIF
30          CONTINUE
            INEXT=NWORD+1
*   Invalid keyword.
       ELSE
            CALL INPMSG(I,'Invalid as a keyword.          ')
       ENDIF
20     CONTINUE
**  Print error messages.
       CALL INPERR
**  Check the dataset name length, if such a name will be needed.
       IF(VALID)THEN
            IF(NCFILE.GT.MXNAME)PRINT *,' !!!!!! SIGWRT WARNING : File',
     -           ' name truncated to MXNAME (=',MXNAME,') characters.'
            IF(NCMEMB.GT.8)PRINT *,' !!!!!! SIGWRT WARNING : Member',
     -           ' name shortened to ',MEMBER,', first 8 characters.'
            IF(NCREM.GT.29)PRINT *,' !!!!!! SIGWRT WARNING : Remark',
     -           ' shortened to ',REMARK,', first 29 characters.'
            NCFILE=MIN(NCFILE,MXNAME)
            NCMEMB=MIN(NCMEMB,8)
            NCREM=MIN(NCREM,29)
       ELSE
            PRINT *,' !!!!!! SIGWRT WARNING : No dataset name found;'//
     -           ' signals not written.'
            RETURN
       ENDIF
*   Check whether the member already exists.
C       CALL DSNREM(FILE(1:NCFILE),MEMBER(1:NCMEMB),'SIGNAL',EXMEMB)
C       IF(JEXMEM.EQ.2.AND.EXMEMB)THEN
C            PRINT *,' ------ SIGWRT MESSAGE : A copy of the member'//
C     -           ' exists; new member will be appended.'
C       ELSEIF(JEXMEM.EQ.3.AND.EXMEMB)THEN
C            PRINT *,' !!!!!! SIGWRT WARNING : A copy of the member'//
C     -           ' exists already; member will not be written.'
C            RETURN
C       ENDIF
*   Print some debugging output if requested.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGWRT DEBUG   : File= '',A,
     -      '', member='',A/26X,''remark='',A,'', format='',A)')
     -      FILE(1:NCFILE),MEMBER(1:NCMEMB),REMARK(1:NCREM),FORMAT
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''Write condition: '',A)')
     -      FCNWRC(1:MAX(1,NCWRC))
*** Translate the write condition, if there is no entry point yet.
       IF(IENTRY.LE.0.AND.NCWRC.GT.0)THEN
            VARLIS(1)='TIME      '
            VARLIS(2)='SIGNAL    '
            VARLIS(3)='SAMPLE    '
            CALL ALGPRE(FCNWRC(1:NCWRC),NCWRC,VARLIS,3,
     -           NRES,USE,IENTRY,IFAIL)
*   Verify that the translation worked correctly.
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! SIGWRT WARNING : Write condition'//
     -                ' could not be translated ; set to True.'
                 CALL ALGCLR(IENTRY)
                 IENTRY=0
                 NCWRC=0
                 RETURN
*   Make sure that there is only one result coming back.
            ELSEIF(NRES.NE.1)THEN
                 PRINT *,' !!!!!! SIGWRT WARNING : The write'//
     -                ' condition does not return 1 result ;'//
     -                ' set to True.'
                 CALL ALGCLR(IENTRY)
                 IENTRY=0
                 NCWRC=0
                 RETURN
            ENDIF
       ENDIF
*** Format the description of the units.
       UNIT='time in '
       NCUNIT=8
       IF(NINT(LOG10(SCALET)).EQ.12)THEN
            UNIT(NCUNIT+1:NCUNIT+11)='atto second'
            NCUNIT=NCUNIT+11
       ELSEIF(NINT(LOG10(SCALET)).EQ.9)THEN
            UNIT(NCUNIT+1:NCUNIT+12)='femto second'
            NCUNIT=NCUNIT+12
       ELSEIF(NINT(LOG10(SCALET)).EQ.6)THEN
            UNIT(NCUNIT+1:NCUNIT+11)='pico second'
            NCUNIT=NCUNIT+11
       ELSEIF(NINT(LOG10(SCALET)).EQ.3)THEN
            UNIT(NCUNIT+1:NCUNIT+11)='nano second'
            NCUNIT=NCUNIT+11
       ELSEIF(NINT(LOG10(SCALET)).EQ.0)THEN
            UNIT(NCUNIT+1:NCUNIT+12)='micro second'
            NCUNIT=NCUNIT+12
       ELSEIF(NINT(LOG10(SCALET)).EQ.-3)THEN
            UNIT(NCUNIT+1:NCUNIT+12)='milli second'
            NCUNIT=NCUNIT+12
       ELSEIF(NINT(LOG10(SCALET)).EQ.-6)THEN
            UNIT(NCUNIT+1:NCUNIT+6)='second'
            NCUNIT=NCUNIT+6
       ELSE
            UNIT(NCUNIT+1:NCUNIT+16)='<unknown> second'
            NCUNIT=NCUNIT+16
       ENDIF
       UNIT(NCUNIT+1:NCUNIT+13)=', current in '
       NCUNIT=NCUNIT+13
       IF(NINT(LOG10(SCALEI)).EQ.12)THEN
            UNIT(NCUNIT+1:NCUNIT+11)='atto Ampere'
            NCUNIT=NCUNIT+11
       ELSEIF(NINT(LOG10(SCALEI)).EQ.9)THEN
            UNIT(NCUNIT+1:NCUNIT+12)='femto Ampere'
            NCUNIT=NCUNIT+12
       ELSEIF(NINT(LOG10(SCALEI)).EQ.6)THEN
            UNIT(NCUNIT+1:NCUNIT+11)='pico Ampere'
            NCUNIT=NCUNIT+11
       ELSEIF(NINT(LOG10(SCALEI)).EQ.3)THEN
            UNIT(NCUNIT+1:NCUNIT+11)='nano Ampere'
            NCUNIT=NCUNIT+11
       ELSEIF(NINT(LOG10(SCALEI)).EQ.0)THEN
            UNIT(NCUNIT+1:NCUNIT+12)='micro Ampere'
            NCUNIT=NCUNIT+12
       ELSEIF(NINT(LOG10(SCALEI)).EQ.-3)THEN
            UNIT(NCUNIT+1:NCUNIT+12)='milli Ampere'
            NCUNIT=NCUNIT+12
       ELSEIF(NINT(LOG10(SCALEI)).EQ.-6)THEN
            UNIT(NCUNIT+1:NCUNIT+6)='Ampere'
            NCUNIT=NCUNIT+6
       ELSEIF(NINT(LOG10(SCALEI)).EQ.-9)THEN
            UNIT(NCUNIT+1:NCUNIT+11)='kilo Ampere'
            NCUNIT=NCUNIT+11
       ELSE
            UNIT(NCUNIT+1:NCUNIT+16)='<unknown> Ampere'
            NCUNIT=NCUNIT+16
       ENDIF
*** Write the information to the dataset, start opening it.
       IF(FORMAT.NE.'SORIN   ')THEN
            CALL DSNOPN(FILE,NCFILE,12,'WRITE-LIBRARY',IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! SIGWRT WARNING : Opening ',
     -                FILE(1:NCFILE),' failed ; no signal data written.'
                 RETURN
            ENDIF
            CALL DSNLOG(FILE,'Signals   ','Sequential','Write     ')
       ENDIF
*** Loop over all sense wires.
       DO 240 ISW=1,NSW
*   For single signal files, open now.
       IF(FORMAT.EQ.'SORIN   ')THEN
            CALL NUMSAV(REAL(ISW),'GROUP',IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! SIGWRT WARNING : Storing read-out ',
     -                ' group variable "GROUP" failed;',
     -                ' no signals written.'
                 RETURN
            ENDIF
            AUX=FILE
            NCAUX=NCFILE
            CALL INPSUB(AUX,NCAUX,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! SIGWRT WARNING : Global variable ',
     -                ' substitution in "',AUX(1:NCAUX),'" failed;',
     -                ' no signals written.'
                 RETURN
            ENDIF
            CALL DSNOPN(AUX,NCAUX,12,'WRITE-FILE',IFAIL)
            CALL DSNLOG(AUX,'Signals   ','Sequential','Write     ')
*   Otherwise write a heading record to the already open file,
       ELSE
            CALL DATTIM(DATE,TIME)
            WRITE(STRING,'(''% Created '',A8,'' At '',A8,
     -           '' < none > SIGNAL   "Direct signal, group '',I3,
     -           ''     "'')') DATE,TIME,ISW
            IF(REMARK.NE.'none')STRING(51:79)=REMARK
            IF(MEMBER.NE.'< none >')STRING(32:39)=MEMBER
            WRITE(12,'(A80)',IOSTAT=IOS,ERR=2010) STRING
*   Inform the user about the conductors in this group.
            CALL CELPRC(12,ISW)
       ENDIF
*   Evaluate the function.
       NWRITE=0
       IF(IENTRY.NE.0)THEN
            DO 205 J=1,NTIME
            VAR(1)=TIMSIG(J)
            MODVAR(1)=2
            VAR(2)=SIGNAL(J,ISW,1)
            MODVAR(2)=2
            VAR(3)=REAL(J)
            MODVAR(3)=2
            CALL ALGEXE(IENTRY,VAR,MODVAR,3,RES,MODRES,1,IFAIL)
            IF(IFAIL.EQ.0.AND.MODRES(1).EQ.3)THEN
                 IF(ABS(RES(1)).LT.1E-3)THEN
                      IFWRT(J)=.FALSE.
                 ELSE
                      IFWRT(J)=.TRUE.
                      NWRITE=NWRITE+1
                 ENDIF
            ELSE
                 PRINT *,' !!!!!! SIGWRT WARNING : WRITE-IF does not'//
     -                ' evaluate to a valid logical; set to True.'
                 IFWRT(J)=.TRUE.
                 NWRITE=NWRITE+1
            ENDIF
205         CONTINUE
       ELSE
            DO 206 J=1,NTIME
            IFWRT(J)=.TRUE.
            NWRITE=NWRITE+1
206         CONTINUE
       ENDIF
*   Check there is something to be written.
       IF(NWRITE.LE.0)THEN
            WRITE(12,'('' No signal data selected by WRITE-IF'')')
       ELSEIF(FORMAT.NE.'SORIN   ')THEN
            WRITE(12,'('' Number of signal records: '',I5)') NWRITE
       ENDIF
*   Write the name of the units.
       IF(FORMAT.NE.'SORIN')
     -      WRITE(12,'('' Units used: '',A,''.'')') UNIT(1:NCUNIT)
*   Write the data at the end of the file.
       NOUT=0
       DO 210 J=1,NTIME
       IF(.NOT.IFWRT(J))GOTO 210
       NOUT=NOUT+1
       IF(FORMAT.EQ.'SPICE   ')THEN
            IF(NOUT.EQ.NWRITE.AND.NOUT.EQ.1)THEN
                 WRITE(12,'('' .STIMULUS signal PWL''/
     -                '' + TIME_SCALE_FACTOR = '',E10.3/
     -                '' + VALUE_SCALE_FACTOR = '',E10.3/
     -                '' + ( '',E15.8,2X,E15.8,'' )'')',
     -                IOSTAT=IOS,ERR=2010) 1.0E-6*SCALET,1.0E-6*SCALEI,
     -                TIMSIG(J)*SCALET,SIGNAL(J,ISW,1)*SCALEI
            ELSEIF(NOUT.EQ.1)THEN
                 WRITE(12,'('' .STIMULUS signal PWL''/
     -                '' + TIME_SCALE_FACTOR = '',E10.3/
     -                '' + VALUE_SCALE_FACTOR = '',E10.3/
     -                '' + ( '',E15.8,2X,E15.8)',IOSTAT=IOS,
     -                ERR=2010) 1.0E-6*SCALET,1.0E-6*SCALEI,
     -                TIMSIG(J)*SCALET,SIGNAL(J,ISW,1)*SCALEI
            ELSEIF(NOUT.GT.1.AND.NOUT.LT.NWRITE)THEN
                 WRITE(12,'('' +'',4X,E15.8,2X,E15.8)',IOSTAT=IOS,
     -                ERR=2010)
     -                TIMSIG(J)*SCALET,SIGNAL(J,ISW,1)*SCALEI
            ELSEIF(NOUT.EQ.NWRITE)THEN
                 WRITE(12,'('' +'',4X,E15.8,2X,E15.8,'' )'')',
     -                IOSTAT=IOS,ERR=2010)
     -                TIMSIG(J)*SCALET,SIGNAL(J,ISW,1)*SCALEI
            ENDIF
       ELSEIF(FORMAT.EQ.'SCEPTRE ')THEN
            WRITE(12,'(2X,E15.8,'' , '',E15.8)',IOSTAT=IOS,ERR=2010)
     -           TIMSIG(J)*SCALET,SIGNAL(J,ISW,1)*SCALEI
       ELSEIF(FORMAT.EQ.'SORIN   ')THEN
            WRITE(12,'(2X,E15.8,''  '',E15.8)',IOSTAT=IOS,ERR=2010)
     -           TIMSIG(J)*SCALET,
     -           (SIGNAL(J,ISW,1)+SIGNAL(J,ISW,2))*SCALEI
       ELSE
            PRINT *,' ###### SIGWRT ERROR   : Signal dataset'//
     -           ' format not known ('//FORMAT//'); respecify.'
            RETURN
       ENDIF
210    CONTINUE
**  Same procedure for the cross induced signals.
       IF(LCROSS.AND.FORMAT.NE.'SORIN   ')THEN
            WRITE(STRING,'(''% Created '',A8,'' At '',A8,
     -          '' < none > SIGNAL   "Cross-talk, group '',I3,
     -          ''        "'')') DATE,TIME,ISW
            IF(REMARK.NE.'none')STRING(51:79)=REMARK
            IF(MEMBER.NE.'< none >')STRING(32:39)=MEMBER
            WRITE(12,'(A80)',IOSTAT=IOS,ERR=2010) STRING
*   Inform the user about the wires in this group.
            CALL CELPRC(12,ISW)
*   Evaluate the function.
            NWRITE=0
            IF(IENTRY.NE.0)THEN
                 DO 270 J=1,NTIME
                 VAR(1)=TIMSIG(J)
                 MODVAR(1)=2
                 VAR(2)=SIGNAL(J,ISW,2)
                 MODVAR(2)=2
                 VAR(3)=REAL(J)
                 MODVAR(3)=2
                 CALL ALGEXE(IENTRY,VAR,MODVAR,3,RES,MODRES,1,IFAIL)
                 IF(IFAIL.EQ.0.AND.MODRES(1).EQ.3)THEN
                      IF(ABS(RES(1)).LT.1E-3)THEN
                           IFWRT(J)=.FALSE.
                      ELSE
                           IFWRT(J)=.TRUE.
                           NWRITE=NWRITE+1
                      ENDIF
                 ELSE
                      PRINT *,' !!!!!! SIGWRT WARNING : WRITE-IF does'//
     -                     ' not evaluate to a valid logical;'//
     -                     ' set to True.'
                      IFWRT(J)=.TRUE.
                      NWRITE=NWRITE+1
                 ENDIF
270              CONTINUE
            ELSE
                 DO 280 J=1,NTIME
                 IFWRT(J)=.TRUE.
                 NWRITE=NWRITE+1
280              CONTINUE
            ENDIF
*   Check there is something to be written.
            IF(NWRITE.LE.0)THEN
                 WRITE(12,'('' No signal data selected by WRITE-IF'')')
            ELSE
                 WRITE(12,'('' Number of signal records: '',I5)') NWRITE
            ENDIF
*   Write the name of the units.
            WRITE(12,'('' Units used: '',A,''.'')') UNIT(1:NCUNIT)
*   Write the data at the end of the file.
            NOUT=0
            DO 220 J=1,NTIME
            IF(.NOT.IFWRT(J))GOTO 220
            NOUT=NOUT+1
            IF(FORMAT.EQ.'SPICE   ')THEN
                 IF(NOUT.EQ.NWRITE.AND.NOUT.EQ.1)THEN
                      WRITE(12,'('' .STIMULUS signal PWL''/
     -                     '' + TIME_SCALE_FACTOR = '',E10.3/
     -                     '' + VALUE_SCALE_FACTOR = '',E10.3/
     -                     '' + ( '',E15.8,2X,E15.8,'' )'')',
     -                     IOSTAT=IOS,ERR=2010)
     -                     1.0E-6*SCALET,1.0E-6*SCALEI,
     -                     TIMSIG(J)*SCALET,SIGNAL(J,ISW,2)*SCALEI
                 ELSEIF(NOUT.EQ.1)THEN
                      WRITE(12,'('' .STIMULUS signal PWL''/
     -                     '' + TIME_SCALE_FACTOR = '',E10.3/
     -                     '' + VALUE_SCALE_FACTOR = '',E10.3/
     -                     '' + ( '',E15.8,2X,E15.8)',IOSTAT=IOS,
     -                     ERR=2010) 1.0E-6*SCALET,1.0E-6*SCALEI,
     -                     TIMSIG(J)*SCALET,SIGNAL(J,ISW,2)*SCALEI
                 ELSEIF(NOUT.GT.1.AND.NOUT.LT.NWRITE)THEN
                      WRITE(12,'('' +'',4X,E15.8,2X,E15.8)',IOSTAT=IOS,
     -                     ERR=2010)
     -                     TIMSIG(J)*SCALET,SIGNAL(J,ISW,2)*SCALEI
                 ELSEIF(NOUT.EQ.NWRITE)THEN
                      WRITE(12,'('' +'',4X,E15.8,2X,E15.8,'' )'')',
     -                     IOSTAT=IOS,ERR=2010)
     -                     TIMSIG(J)*SCALET,SIGNAL(J,ISW,2)*SCALEI
                 ENDIF
            ELSEIF(FORMAT.EQ.'SCEPTRE ')THEN
                 WRITE(12,'(2X,E15.8,'' , '',E15.8)',
     -                IOSTAT=IOS,ERR=2010)
     -                TIMSIG(J)*SCALET,SIGNAL(J,ISW,2)*SCALEI
            ELSE
                 PRINT *,' ###### SIGWRT ERROR   : Signal dataset'//
     -                ' format not known ('//FORMAT//'); respecify.'
                 RETURN
            ENDIF
220         CONTINUE
       ENDIF
*** Signal group files.
       IF(FORMAT.EQ.'SORIN   ')
     -      CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
240    CONTINUE
*** Normal end of the routine, return after closing the file.
       IF(FORMAT.NE.'SORIN   ')
     -      CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
*** Handle error conditions.
2010   CONTINUE
       PRINT *,' ###### SIGWRT ERROR   : Error while writing'//
     -         ' to the file ',FILE(1:NCFILE),' on unit 12.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2030   CONTINUE
       PRINT *,' ###### SIGWRT ERROR   : '//FILE(1:NCFILE)//
     -      ' could not be closed properly ; results not predictable.'
       CALL INPIOS(IOS)
       END
