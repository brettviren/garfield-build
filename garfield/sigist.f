CDECK  ID>, SIGIST.
       SUBROUTINE SIGIST(ACTION,NSIG,TIME,SIG,ISW,IW,IA,IQ,IFAIL)
*-----------------------------------------------------------------------
*   SIGIST - Routine keeping the various ion signals in a scratch file.
*   (Last changed on 24/ 2/97.)
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
       INTEGER LUNIST,MXIREC
       PARAMETER(MXIREC=1000,LUNIST=14)
       DOUBLE PRECISION SIG(*),TIME(*)
       INTEGER IADREF(MXIREC),NREF,ISTATE,IFAIL,NSIG,NVEC(MXIREC),
     -    ISW,IW,IA,IQ,NUSED(MXIREC),ILEAST,IREC,I,
     -    ISTORE,IAGE(MXIREC),IADDR,IOS
       LOGICAL OPEN
       CHARACTER*(*) ACTION
       SAVE IADREF,NREF,ISTATE,NVEC,NUSED,ISTORE,IAGE
       DATA NREF/0/, ISTATE/0/, ISTORE/0/
*** Identify the routine if required.
       IF(LIDENT)PRINT *,' /// ROUTINE SIGIST (File) ///'
*** Assume the operation will fail.
       IFAIL=1
*** Open the scratch file.
       IF(ACTION.EQ.'OPEN')THEN
*   Check that the file is not already open.
            INQUIRE(UNIT=LUNIST,OPENED=OPEN)
*   Close if it is.
            IF(OPEN)THEN
                 PRINT *,' !!!!!! SIGIST WARNING : Ion signal unit'//
     -                ' unexpectedly open; closed.'
                 CLOSE(UNIT=LUNIST,ERR=2030,IOSTAT=IOS)
            ENDIF
*   Open the file.
            OPEN(UNIT=LUNIST,STATUS='SCRATCH',ACCESS='DIRECT',
     -           FORM='UNFORMATTED',ERR=2020,RECL=16*MXLIST,
     -           IOSTAT=IOS)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGIST DEBUG   :'',
     -           '' Signal file opened on unit '',I6)') LUNIST
*   Keep track of this.
            ISTATE=1
            NREF=0
*   Reset the number of stores.
            ISTORE=0
*   Seems to have worked.
            IFAIL=0
*** Reset the file.
       ELSEIF(ACTION.EQ.'RESET')THEN
            IF(ISTATE.EQ.0)THEN
                 PRINT *,' !!!!!! SIGIST WARNING : No signal file'//
     -                ' currently active; not reset.'
            ELSE
                 NREF=0
                 IFAIL=0
                 ISTORE=0
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGIST DEBUG   :'',
     -                '' Signal file reset on unit '',I6)') LUNIST
            ENDIF
*** Store a record.
       ELSEIF(ACTION.EQ.'STORE')THEN
*   Check the state of the file.
            IF(ISTATE.NE.1)THEN
                 PRINT *,' !!!!!! SIGIST WARNING : Request to store'//
     -                ' but signal file not open; not stored.'
                 RETURN
            ENDIF
*   Check address range validity.
            IF((IA.LT.1.OR.IA.GT.NORIA).OR.
     -           (ISW.LT.1.OR.ISW.GT.MXSW).OR.
     -           (IW.LT.1.OR.IW.GT.MXWIRE).OR.
     -           ABS(IQ).NE.1)THEN
                 PRINT *,' !!!!!! SIGIST WARNING : Signal address'//
     -                ' out of range; not stored.'
                 RETURN
            ENDIF
*   Compute reference address.
            IADDR=IQ*(IA+MXORIA*(ISW-1+MXSW*(IW-1)))
*   Locate the reference in the tables and also the least used record.
            ILEAST=1
            DO 10 I=1,NREF
            IF(NUSED(I).LT.NUSED(ILEAST).OR.
     -           (NUSED(I).EQ.NUSED(ILEAST).AND.
     -           IAGE(I).LT.IAGE(ILEAST)))ILEAST=I
            IF(IADREF(I).EQ.IADDR)THEN
                 IREC=I
                 GOTO 20
            ENDIF
10          CONTINUE
*   New record, allocate space.
            IF(NREF.LT.MXIREC)THEN
                 NREF=NREF+1
                 IADREF(NREF)=IADDR
                 IREC=NREF
*   Or reuse the least used record sofar.
            ELSE
                 IREC=ILEAST
                 IADREF(IREC)=IADDR
            ENDIF
*   In either case set the usage counter to 0.
            NUSED(IREC)=0
*   Write the record.
20          CONTINUE
            NVEC(IREC)=NSIG
            WRITE(UNIT=LUNIST,REC=IREC,ERR=2010,IOSTAT=IOS)
     -           (TIME(I),I=1,NSIG),(SIG(I),I=1,NSIG)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGIST DEBUG   :'',
     -           '' Stored record '',I6,'' for reference '',I6)')
     -           IREC,IADDR
*   And keep track of the age.
            ISTORE=ISTORE+1
            IAGE(IREC)=ISTORE
*   Seems to have worked.
            IFAIL=0
*** Retrieve a record.
       ELSEIF(ACTION.EQ.'READ')THEN
*   Check the state of the file.
            IF(ISTATE.NE.1)THEN
                 PRINT *,' !!!!!! SIGIST WARNING : Request to read'//
     -                ' but signal file not open; not read.'
                 RETURN
            ENDIF
*   Check address range validity.
            IF((IA.LT.1.OR.IA.GT.NORIA).OR.
     -           (ISW.LT.1.OR.ISW.GT.MXSW).OR.
     -           (IW.LT.1.OR.IW.GT.MXWIRE).OR.
     -           ABS(IQ).NE.1)THEN
                 PRINT *,' !!!!!! SIGIST WARNING : Signal address'//
     -                ' out of range; not read.'
                 RETURN
            ENDIF
*   Compute reference address.
            IADDR=IQ*(IA+MXORIA*(ISW-1+MXSW*(IW-1)))
*   Locate the reference in the tables.
            DO 30 I=1,NREF
            IF(IADREF(I).EQ.IADDR)THEN
                 IREC=I
                 GOTO 40
            ENDIF
30          CONTINUE
*   Unknown record, signal this via NSIG.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGIST DEBUG   :'',
     -           '' Record '',4I4,'' not known.'')') ISW,IW,IA,IQ
            NSIG=-1
            IFAIL=0
            RETURN
*   Read the record.
40          CONTINUE
            NSIG=NVEC(IREC)
            READ(UNIT=LUNIST,REC=IREC,ERR=2010,IOSTAT=IOS)
     -           (TIME(I),I=1,NSIG),(SIG(I),I=1,NSIG)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGIST DEBUG   :'',
     -           '' Read record '',I6,'' for reference '',I6)')
     -           IREC,IADDR
*   Increment the usage counter.
            NUSED(IREC)=NUSED(IREC)+1
*   Seems to have worked.
            IFAIL=0
*** List currently known records.
       ELSEIF(ACTION.EQ.'LIST')THEN
*   Print a header.
            WRITE(LUNOUT,'(''  ++++++ SIGIST DEBUG   : Overview of'',
     -           '' currently known records:''//''  Record   Angle'',
     -           ''    Wire   Sense  Charge   Usage   Birth''/)')
*   Loop over the records.
            DO 50 I=1,NREF
            IADDR=IADREF(I)
            IQ=SIGN(1,IADDR)
            IADDR=ABS(IADDR)
            IA=MOD(IADDR,MXORIA)
            IF(IA.EQ.0)IA=MXORIA
            IADDR=(IADDR-IA)/MXORIA
            ISW=MOD(IADDR,MXSW)+1
            IF(ISW.EQ.0)ISW=MXSW
            IW=(IADDR-ISW+1)/MXSW+1
            WRITE(LUNOUT,'(5(2X,I6))') I,IA,IW,ISW,IQ,NUSED(I),IAGE(I)
50          CONTINUE
*   Overview.
            WRITE(LUNOUT,'(/''  Total of '',I6,'' records.'')') NREF
*   This can not fail.
            IFAIL=0
*** Close the file.
       ELSEIF(ACTION.EQ.'CLOSE')THEN
*   Check that the file is indeed open.
            INQUIRE(UNIT=LUNIST,OPENED=OPEN)
*   Close if open.
            IF(.NOT.OPEN)THEN
                 PRINT *,' !!!!!! SIGIST WARNING : Ion signal unit'//
     -                ' is already closed; not closed again.'
            ELSE
                 CLOSE(UNIT=LUNIST,ERR=2030,IOSTAT=IOS)
            ENDIF
*   Keep track of the state.
            ISTATE=0
            NREF=0
*   Seems to have worked.
            IFAIL=0
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGIST DEBUG   :'',
     -           '' Closed signal unit '',I6)') LUNIST
*** Other actions are not known.
       ELSE
            PRINT *,' !!!!!! SIGIST WARNING : Action not known;'//
     -           ' nothing done.'
       ENDIF
*** I/O error handling.
       RETURN
2010   CONTINUE
       PRINT *,' !!!!!! SIGIST WARNING : Read/write error to'//
     -      ' signal file ; action not completed.'
       CALL INPIOS(IOS)
       RETURN
2020   CONTINUE
       PRINT *,' !!!!!! SIGIST WARNING : Open error on'//
     -      ' signal file ; action not completed.'
       CALL INPIOS(IOS)
       RETURN
2030   CONTINUE
       PRINT *,' !!!!!! SIGIST WARNING : Close error on'//
     -      ' signal file ; action not completed.'
       CALL INPIOS(IOS)
       END
