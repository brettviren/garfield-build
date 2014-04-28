CDECK  ID>, HLPPACOT.
       SUBROUTINE HLPPAC(IFAIL)
*-----------------------------------------------------------------------
*   HLPPAC - Packs the help file into a direct access dataset.
*   (Last changed on 19/ 7/00.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       LOGICAL EXIST
       CHARACTER*500 IN
       CHARACTER*20 TOPIC
       CHARACTER*8 DATE,TIME
       CHARACTER*(MXHLRL) OUT
       INTEGER PATH(0:MXHLEV),SUBREC(0:MXSUBT),INPCMP,I,NWORD,IFAIL,IOS,
     -      NTOTAL,NOUT,NIN,IOUT,NREC,LEVEL,LAST,LENIN,NSUB,IFIRST,J,
     -      ILAST,N,NEWLEV,NNREC,ISTART,IEND,IADD
       EXTERNAL INPCMP
*** Determine whether character translation is to be performed.
       CALL INPNUM(NWORD)
       DO 40 I=2,NWORD
       IF(INPCMP(I,'TR#ANSLATE').NE.0)THEN
            CALL INPMSG(I,'Option only meaningful on IBM.')
       ELSEIF(INPCMP(I,'NOTR#ANSLATE').NE.0)THEN
            CALL INPMSG(I,'Option only meaningful on IBM.')
       ELSE
            CALL INPMSG(I,'Not a valid option.           ')
       ENDIF
40     CONTINUE
*** Check the existence of both raw and processed help files.
       INQUIRE(FILE='garfield.rawhelp',EXIST=EXIST)
       IF(.NOT.EXIST)THEN
            PRINT *,' !!!!!! HLPPAC WARNING : Raw help dataset not'//
     -           ' found ; direct access dataset not prepared.'
            IFAIL=1
            RETURN
       ENDIF
       INQUIRE(FILE='garfield.packhelp',EXIST=EXIST)
       IF(EXIST)THEN
            PRINT *,' !!!!!! HLPPAC WARNING : Packed help file'//
     -           ' exists already ; no new copy prepared.'
            IFAIL=1
            RETURN
       ENDIF
*** Have the number of records counted.
       CALL HLPCNT(NTOTAL,IFAIL)
*** Open the raw and the direct access help file.
       OPEN(UNIT=12,FILE='garfield.rawhelp',STATUS='OLD',IOSTAT=IOS,
     -      ERR=2020)
       OPEN(UNIT=17,FILE='garfield.packhelp',STATUS='NEW',
     -      ACCESS='DIRECT',RECL=MXHLRL,FORM='UNFORMATTED',
     -      IOSTAT=IOS,ERR=2020)
       CALL DSNLOG('garfield.rawhelp','Raw help  ','Sequential',
     -      'Read      ')
       CALL DSNLOG('garfield.packhelp','HELP file ','Direct    ',
     -      'Created   ')
*** Initialise various global variables.
       NOUT=0
       NIN=0
       IOUT=1
       NREC=0
       OUT=' '
       LEVEL=0
       PATH(0)=1
       LAST=1
**  Write the initial pointer record.
       CALL DATTIM(DATE,TIME)
       TOPIC='Root'//DATE//TIME
       NOUT=NOUT+1
       WRITE(UNIT=17,REC=NOUT,IOSTAT=IOS,ERR=2015) TOPIC,0,0
**  Read a line from the file, skipping comment lines.
10     CONTINUE
       READ(12,'(A)',IOSTAT=IOS,ERR=2010,END=20) IN
       LENIN=LEN(IN)
       NIN=NIN+1
       IF(IN(1:1).EQ.'!')GOTO 10
**  New heading level.
       IF(IN(1:2).NE.'  ')THEN
*   Empty the buffer.
            IF(IOUT.GT.1)THEN
                 NOUT=NOUT+1
                 OUT(IOUT-1:IOUT-1)=CHAR(11)
                 WRITE(UNIT=17,REC=NOUT,IOSTAT=IOS,ERR=2015) OUT
                 NREC=NREC+1
            ELSE
                 NOUT=NOUT+1
                 WRITE(UNIT=17,REC=NOUT,IOSTAT=IOS,ERR=2015) CHAR(11)
                 NREC=NREC+1
            ENDIF
            IOUT=1
            OUT=' '
*   Read the new heading level.
            CALL INPRIC(IN(1:2),NEWLEV,0,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! HLPPAC WARNING : Invalid level'//
     -                ' string "'//IN(1:2)//'" encountered at line',
     -                NIN,' ; packed help file deleted.'
                 CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
                 CLOSE(UNIT=17,STATUS='DELETE',IOSTAT=IOS,ERR=2030)
                 RETURN
            ENDIF
            IF(NEWLEV.GT.LEVEL+1.OR.NEWLEV.LE.0)THEN
                 PRINT *,' !!!!!! HLPPAC WARNING : Incorrect heading'//
     -                ' level (',NEWLEV,') encountered at line ',NIN,'.'
                 PRINT *,'                         Packed help-file'//
     -                ' deleted.'
                 CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
                 CLOSE(UNIT=17,STATUS='DELETE',IOSTAT=IOS,ERR=2030)
                 IFAIL=1
                 RETURN
            ENDIF
            IF(NEWLEV.GT.MXHLEV)THEN
                 PRINT *,' !!!!!! HLPPAC WARNING : Heading level'//
     -                ' exceeds compilation limit (',NEWLEV,' vs ',
     -                MXHLEV,') at line ',NIN,'.'
                 PRINT *,'                         Packed help-file'//
     -                ' deleted.'
                 CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
                 CLOSE(UNIT=17,STATUS='DELETE',IOSTAT=IOS,ERR=2030)
                 IFAIL=1
                 RETURN
            ENDIF
            LEVEL=NEWLEV
*   Write an almost empty header for this topic, updated later on.
            NOUT=NOUT+1
            WRITE(UNIT=17,REC=NOUT,IOSTAT=IOS,ERR=2015) IN(3:22),0,0
C      print *,' Heading at level ',LEVEL,': ',IN(3:22)
*   Update the link record for the next higher level.
            READ(UNIT=17,REC=PATH(LEVEL-1),IOSTAT=IOS,ERR=2015)
     -           TOPIC,NNREC,NSUB,(SUBREC(I),I=1,NSUB)
            NSUB=NSUB+1
            IF(NSUB.GT.MXSUBT)THEN
                 PRINT *,' ###### HLPPAC ERROR   : The help file'//
     -                ' cannot be packed because MXSUBT is too small.'
                 CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
                 CLOSE(UNIT=17,STATUS='DELETE',IOSTAT=IOS,ERR=2030)
                 IFAIL=1
                 RETURN
            ENDIF
            SUBREC(NSUB)=NOUT
            WRITE(UNIT=17,REC=PATH(LEVEL-1),IOSTAT=IOS,ERR=2015)
     -           TOPIC,NNREC,NSUB,(SUBREC(I),I=1,NSUB)
*   Update the path pointer for this level.
            PATH(LEVEL)=NOUT
*   Update the number of records the previous item had.
            READ(UNIT=17,REC=LAST,IOSTAT=IOS,ERR=2015)
     -           TOPIC,NNREC,NSUB,(SUBREC(I),I=1,NSUB)
            WRITE(UNIT=17,REC=LAST,IOSTAT=IOS,ERR=2015)
     -           TOPIC,NREC,NSUB,(SUBREC(I),I=1,NSUB)
*   Update the pointer to the new last item (this one).
            LAST=NOUT
            NREC=0
**  Ordinary line, simply written to the file.
       ELSE
*   Determine the length of the line.
            DO 100 I=LENIN,3,-1
            IF(IN(I:I).NE.' ')THEN
                 N=I
                 GOTO 110
            ENDIF
100         CONTINUE
            N=3
110         CONTINUE
*   Compress lines which contain an HTML reference.
30          CONTINUE
            IF(INDEX(IN(1:N),'"->').NE.0)THEN
                 ISTART=INDEX(IN(1:N),'"->')
                 IEND=ISTART+INDEX(IN(ISTART+1:N),'"')
                 IF(IEND.LE.ISTART)THEN
                      PRINT *,' !!!!!! HLPPAC WARNING : Reference'//
     -                     ' string at line ',NIN,' not closed;'//
     -                     ' line not compressed.'
                      GOTO 50
                 ENDIF
                 IADD=ISTART
                 DO 70 I=IEND-1,ISTART,-1
                 IF(IN(I:I).EQ.' ')THEN
                      DO 80 J=I+1,IEND-1
                      IN(IADD:IADD)=IN(J:J)
                      IADD=IADD+1
80                    CONTINUE
                      GOTO 90
                 ENDIF
70               CONTINUE
90               CONTINUE
                 DO 60 I=IEND+1,N
                 IN(IADD:IADD)=IN(I:I)
                 IADD=IADD+1
60               CONTINUE
                 IN(IADD:)=' '
                 N=IADD-1
                 GOTO 30
            ENDIF
50          CONTINUE
*   Add the present line to the buffer, separating by a LF (ASCII 10).
            IFIRST=3
120         CONTINUE
            ILAST=MIN(N+1,IFIRST+MXHLRL-1)
            IF(IOUT+ILAST-IFIRST.GT.MXHLRL)ILAST=MXHLRL-IOUT+IFIRST
            IF(ILAST.EQ.N+1)THEN
                 IF(ILAST.GT.IFIRST)OUT(IOUT:IOUT+ILAST-IFIRST)=
     -                IN(IFIRST:ILAST-1)//CHAR(10)
                 IF(ILAST.EQ.IFIRST)OUT(IOUT:IOUT)=CHAR(10)
            ELSE
                 OUT(IOUT:IOUT+ILAST-IFIRST)=IN(IFIRST:ILAST)
            ENDIF
            IF(IOUT+ILAST-IFIRST.EQ.MXHLRL)THEN
                 NOUT=NOUT+1
                 WRITE(UNIT=17,REC=NOUT,IOSTAT=IOS,ERR=2015) OUT
                 IOUT=1
                 OUT=' '
                 NREC=NREC+1
            ELSE
                 IOUT=IOUT+ILAST-IFIRST+1
            ENDIF
            IFIRST=ILAST+1
            IF(IFIRST.LE.N+1)GOTO 120
       ENDIF
       GOTO 10
*** Jump to this point at EOF on the raw help file.
20     CONTINUE
*   Write the current record to the file, if not empty.
       IF(IOUT.GT.1)THEN
            NOUT=NOUT+1
            OUT(IOUT-1:IOUT-1)=CHAR(11)
            WRITE(UNIT=17,REC=NOUT,IOSTAT=IOS,ERR=2015) OUT
            NREC=NREC+1
       ELSE
            NOUT=NOUT+1
            WRITE(UNIT=17,REC=NOUT,IOSTAT=IOS,ERR=2015) CHAR(11)
            NREC=NREC+1
       ENDIF
*   Update the number of records the final item had.
       READ(UNIT=17,REC=LAST,IOSTAT=IOS,ERR=2015)
     -      TOPIC,NNREC,NSUB,(SUBREC(I),I=1,NSUB)
       WRITE(UNIT=17,REC=LAST,IOSTAT=IOS,ERR=2015)
     -      TOPIC,NREC,NSUB,(SUBREC(I),I=1,NSUB)
*   Close the files.
       CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
       CLOSE(UNIT=17,IOSTAT=IOS,ERR=2030)
*   Signal to the calling routine that everything worked well.
       IFAIL=0
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ HLPPAC DEBUG   : Reference'',
     -      '' count of the number of records:'',I5)') NOUT
*** Keep track of CPU time consumption.
       CALL TIMLOG('Packing the help file')
       RETURN
*** Handle I/O errors.
2010   CONTINUE
       PRINT *,' ###### HLPPAC ERROR   : I/O error reading the raw'//
     -      ' help file at line ',NIN,' ; packed help file deleted.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
       CLOSE(UNIT=17,STATUS='DELETE',IOSTAT=IOS,ERR=2030)
       IFAIL=1
       RETURN
2015   CONTINUE
       PRINT *,' ###### HLPPAC ERROR   : I/O error on the direct'//
     -      ' access help file ; dataset not prepared.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
       CLOSE(UNIT=17,STATUS='DELETE',IOSTAT=IOS,ERR=2030)
       IFAIL=1
       RETURN
2020   CONTINUE
       PRINT *,' ###### HLPPAC ERROR   : Unable to open a help'//
     -      ' file ; direct access dataset not prepared.'
       CALL INPIOS(IOS)
       IFAIL=1
       RETURN
2030   CONTINUE
       PRINT *,' !!!!!! HLPPAC WARNING : Unable to close the raw or'//
     -      ' the packed help file ; direct access file probably OK.'
       CALL INPIOS(IOS)
       RETURN
       END
