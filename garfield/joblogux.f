CDECK  ID>, JOBLOGUX.
       SUBROUTINE JOBLOG(TEXT)
*-----------------------------------------------------------------------
*   JOBLOG - This routine writes a log file entry (userid, date & time)
*            in /afs/cern.ch/user/r/rjd/Garfield/Log/garfield.log.
*   (Last changed on 27/10/11.)
*-----------------------------------------------------------------------
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*(*) TEXT
       CHARACTER*32 HOST
       CHARACTER*8 DATE,TIME,NAME
       LOGICAL EXIST
*** Check total length of the string.
       IF(LEN(TEXT)+34.GT.132)THEN
            PRINT *,' !!!!!! JOBLOG WARNING : Job log information'//
     -           ' string too long; no entry written.'
            RETURN
       ENDIF
*** Find out about current date and time + the user name.
       CALL DATTIM(DATE,TIME)
       call getlog(name)
       call hostnm(host)
*** Find the length of the strings.
       DO 10 I=LEN(NAME),1,-1
       IF(NAME(I:I).NE.' ')THEN
            NCNAME=I
            GOTO 20
       ENDIF
10     CONTINUE
       NCNAME=1
20     CONTINUE
       DO 30 I=LEN(HOST),1,-1
       IF(HOST(I:I).NE.' ')THEN
            NCHOST=I
            GOTO 40
       ENDIF
30     CONTINUE
       NCHOST=1
40     CONTINUE
*** Open the log file.
       INQUIRE(EXIST=EXIST,
     -      FILE='/afs/cern.ch/user/r/rjd/Garfield/Log/garfield.log')
*** Open and skip to the end of the file.
       OPEN(UNIT=12,STATUS='UNKNOWN',ACCESS='APPEND',
     -      FILE='/afs/cern.ch/user/r/rjd/Garfield/Log/garfield.log',
     -      IOSTAT=IOS,ERR=2020)
*** Open a file and write the entry in it.
       WRITE(12,'(A,'' on '',A8,'' at '',A8,2X,A)',ERR=2010)
     -      NAME(1:NCNAME)//'@'//HOST(1:NCHOST),DATE,TIME,TEXT
       CLOSE(UNIT=12,ERR=2030)
*** Log its usage so the user can in principle know what happened.
       CALL DSNLOG('garfield.log','Log file  ','Sequential',
     -      'Append    ')
*** Normal end of this routine.
       RETURN
*** I/O error handling.
2010   CONTINUE
       IF(LDEBUG)WRITE(LUNOUT,'(1X,A)') ' ++++++ JOBLOG DEBUG   :'//
     -      ' I/O error occurred while reading or writing the log file.'
       IF(LDEBUG)CALL INPIOS(IOS)
       CLOSE(UNIT=12,ERR=2030,IOSTAT=IOS)
       RETURN
2020   CONTINUE
       IF(LDEBUG)WRITE(LUNOUT,'(1X,A)') ' ++++++ JOBLOG DEBUG   :'//
     -      ' An error occurred while opening the log file.'
       IF(LDEBUG)CALL INPIOS(IOS)
       RETURN
2030   CONTINUE
       IF(LDEBUG)WRITE(LUNOUT,'(1X,A)') ' ++++++ JOBLOG DEBUG   :'//
     -      ' An error occurred while closing the log file.'
       IF(LDEBUG)CALL INPIOS(IOS)
       RETURN
       END
