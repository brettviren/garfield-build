CDECK  ID>, MATWRT.
       SUBROUTINE MATWRT(IREF,FILE,MEMB,REM,IFAIL)
*-----------------------------------------------------------------------
*   MATWRT - This routine writes a matrix to a dataset.
*   VARIABLES :
*   (Last changed on 30/ 8/97.)
*-----------------------------------------------------------------------
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
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*132 STRING
       CHARACTER*(*) FILE,MEMB,REM
       CHARACTER*29 REMARK
       CHARACTER*8 TIME,DATE,MEMBER
       LOGICAL EXMEMB
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE MATWRT ///'
*** Preset IFAIL to 1: failure.
       IFAIL=1
*** Check whether the member already exists.
       CALL DSNREM(FILE,MEMB,'MATRIX',EXMEMB)
       IF(JEXMEM.EQ.2.AND.EXMEMB)THEN
            PRINT *,' ------ MATWRT MESSAGE : A copy of the member'//
     -           ' exists; new member will be appended.'
       ELSEIF(JEXMEM.EQ.3.AND.EXMEMB)THEN
            PRINT *,' !!!!!! MATWRT WARNING : A copy of the member'//
     -           ' exists already; member will not be written.'
            RETURN
       ENDIF
*** Transfer variables.
       REMARK=REM
       MEMBER=MEMB
*** Print some debugging output if requested.
       IF(LDEBUG)PRINT *,' ++++++ MATWRT DEBUG   : Ref=',IREF,
     -      ', File=',FILE,', member=',MEMBER,', Remark=',REMARK,'.'
*** Find the slot where the matrix is stored.
       DO 10 I=1,MXMAT
       IF(MREF(I).EQ.IREF)THEN
            ISLOT=I
            GOTO 20
       ENDIF
10     CONTINUE
       PRINT *,' !!!!!! MATWRT WARNING : Matrix to be written has'//
     -      ' not been found.'
       RETURN
20     CONTINUE
*** Open the dataset for sequential write and inform DSNLOG.
       CALL DSNOPN(FILE,LEN(FILE),12,'WRITE-LIBRARY',IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! MATWRT WARNING : Opening ',FILE,
     -           ' failed ; matrix will not be written.'
            IFAIL=1
            RETURN
       ENDIF
       CALL DSNLOG(FILE,'Matrix    ','Sequential','Write     ')
       IF(LDEBUG)PRINT *,' ++++++ MATWRT DEBUG   : Dataset ',
     -      FILE,' opened on unit 12 for seq write.'
*   Now write a heading record to the file.
       CALL DATTIM(DATE,TIME)
       WRITE(STRING,'(''% Created '',A8,'' At '',A8,1X,A8,'' MATRIX  '',
     -      1X,''"'',A29,''"'')') DATE,TIME,MEMBER,REMARK
       WRITE(12,'(A80)',IOSTAT=IOS,ERR=2010) STRING
*   Write the matrix.
       WRITE(12,'('' MATRIX INFORMATION:''/'' Dimension: '',I10/
     -      '' Mode:      '',I10/
     -      '' Sizes:     '',12I10:(/12X,12I10))',IOSTAT=IOS,ERR=2010)
     -      MDIM(ISLOT),MMOD(ISLOT),(MSIZ(ISLOT,I),I=1,MDIM(ISLOT))
       WRITE(12,'('' CONTENTS''/(2X,8E15.8))',IOSTAT=IOS,ERR=2010)
     -      (MVEC(MORG(ISLOT)+I),I=1,MLEN(ISLOT))
*   Close the file after the operation.
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       CALL TIMLOG('Writing a matrix to a dataset:       ')
*** Things worked, reset error flag.
       IFAIL=0
       RETURN
*** Handle the error conditions.
2010   CONTINUE
       PRINT *,' ###### MATWRT ERROR   : Error while writing'//
     -      ' to ',FILE,' via unit 12 ; matrix not written.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2030   CONTINUE
       PRINT *,' ###### MATWRT ERROR   : Dataset ',FILE,
     -      ' unit 12 cannot be closed ; results not predictable'
       CALL INPIOS(IOS)
       END
