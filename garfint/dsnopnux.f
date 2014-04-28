CDECK  ID>, DSNOPNUX.
       SUBROUTINE DSNOPN(DSNAME,NCDSN,LUNDSN,ACCESS,IFAIL)
*-----------------------------------------------------------------------
*   DSNOPN - Opens a file.
*   VARIABLES : FILE/DSNAME : The name of the file to be opened.
*               NC/NCDSN    : Number of characters in FILE.
*               LUNDSN      : The logical file number to open the file.
*               ACCESS      : The type of access to the file.
*   (Last changed on  6/ 3/08.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*(*) DSNAME,ACCESS
       LOGICAL EXBACK
       LOGICAL OPEN,EXIS
       CHARACTER*(MXNAME) FILE
       INTEGER NC,NCDSN,LUNDSN,IFAIL,IOS
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE DSNOPN (Unix+Cygwin) ///'
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DSNOPN DEBUG   : Request'',
     -      '' to open '',A/26X,''on unit '',I2,'' with access '',A)')
     -      DSNAME(1:NCDSN),LUNDSN,ACCESS
*** Initialise IFAIL to 1.
       IFAIL=1
*** Check that the unit is closed.
       INQUIRE(UNIT=LUNDSN,OPENED=OPEN)
       IF(OPEN)THEN
            PRINT *,' !!!!!! DSNOPN WARNING : Unit ',LUNDSN,' is'//
     -           ' found to be open ; attempt to close it.'
            CLOSE(UNIT=LUNDSN,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       ENDIF
*** Perform subsitutions of environment variables.
       CALL DSNFMT(DSNAME,NCDSN,FILE,NC,'ANY',IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! DSNOPN WARNING : The file is not opened'//
     -           ' because of the above error.'
            RETURN
       ENDIF
*** Store the file existence flag.
       INQUIRE(FILE=FILE(1:NC),EXIST=EXIS)
       IF((.NOT.EXIS).AND.
     -      (ACCESS(1:4).EQ.'READ'.OR.ACCESS(1:2).EQ.'RW'))THEN
            PRINT *,' !!!!!! DSNOPN WARNING : The file '//FILE(1:NC)//
     -           ' has not been found; not opened.'
            IFAIL=1
            RETURN
       ENDIF
*** Check that the file is not open.
       INQUIRE(FILE=FILE(1:NC),OPENED=OPEN)
       IF(OPEN)THEN
            PRINT *,' !!!!!! DSNOPN WARNING : The file '//FILE(1:NC)//
     -           ' is already open; no access given.'
            IFAIL=1
            RETURN
       ENDIF
*** Open the dataset.
       IF(INDEX(ACCESS,'WRITE').NE.0)THEN
*   If an output file, shift previous copies.
            IF(INDEX(ACCESS,'FILE').NE.0.AND.EXIS)THEN
                 INQUIRE(FILE=FILE(1:NC)//'.bak',EXIST=EXBACK)
                 IF(EXBACK)CALL unlink(FILE(1:NC)//'.bak')
                 CALL rename(FILE(1:NC),FILE(1:NC)//'.bak')
                 OPEN(UNIT=LUNDSN,FILE=FILE(1:NC),STATUS='NEW',
     -                ACCESS='SEQUENTIAL',FORM='FORMATTED',
     -                IOSTAT=IOS,ERR=2020)
                 EXIS=.FALSE.
*   If a binary output file, shift previous copies.
            ELSEIF(INDEX(ACCESS,'BINARY').NE.0.AND.EXIS)THEN
                 INQUIRE(FILE=FILE(1:NC)//'.bak',EXIST=EXBACK)
                 IF(EXBACK)CALL unlink(FILE(1:NC)//'.bak')
                 CALL rename(FILE(1:NC),FILE(1:NC)//'.bak')
                 OPEN(UNIT=LUNDSN,FILE=FILE(1:NC),STATUS='NEW',
     -                ACCESS='SEQUENTIAL',FORM='UNFORMATTED',
     -                IOSTAT=IOS,ERR=2020)
                 EXIS=.FALSE.
*   Otherwise skip to the end of the file if it exist.
            ELSEIF(EXIS)THEN
                 OPEN(UNIT=LUNDSN,FILE=FILE(1:NC),STATUS='OLD',
     -                ACCESS='APPEND',IOSTAT=IOS,ERR=2020)
*   Or open a new file if it didn't yet exist.
            ELSEIF(INDEX(ACCESS,'BINARY').NE.0)THEN
                 OPEN(UNIT=LUNDSN,FILE=FILE(1:NC),STATUS='NEW',
     -                ACCESS='SEQUENTIAL',FORM='UNFORMATTED',
     -                IOSTAT=IOS,ERR=2020)
            ELSE
                 OPEN(UNIT=LUNDSN,FILE=FILE(1:NC),STATUS='NEW',
     -                ACCESS='SEQUENTIAL',FORM='FORMATTED',
     -                IOSTAT=IOS,ERR=2020)
            ENDIF
*   Open for non-binary read or read/write access.
       ELSEIF(INDEX(ACCESS,'BINARY').EQ.0)THEN
            OPEN(UNIT=LUNDSN,FILE=FILE(1:NC),STATUS='UNKNOWN',
     -           ACCESS='SEQUENTIAL',FORM='FORMATTED',
     -           IOSTAT=IOS,ERR=2020)
*   Open for binary read or read/write access.
       ELSE
            OPEN(UNIT=LUNDSN,FILE=FILE(1:NC),STATUS='UNKNOWN',
     -           ACCESS='SEQUENTIAL',FORM='UNFORMATTED',
     -           IOSTAT=IOS,ERR=2020)
       ENDIF
*** Write a first record on the dataset if it is new.
       IF((.NOT.EXIS).AND.
     -      INDEX(ACCESS,'BINARY').EQ.0.AND.
     -      INDEX(ACCESS,'FILE').EQ.0.AND.
     -      INDEX(ACCESS,'WRITE')+INDEX(ACCESS,'RW').NE.0)
     -      WRITE(LUNDSN,'(''*----.----1----.----2----.----3'',
     -      ''----.----4----.----5----.----6----.----7----.----8----.'',
     -      ''----9----.---10----.---11----.---12----.---13--'')',
     -      IOSTAT=IOS,ERR=2015)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DSNOPN DEBUG   : Dataset '',
     -      A,'' opened on unit '',I2,''.'')') FILE(1:NC),LUNDSN
*** Everything looks all right, set IFAIL to 0 (OK) and return.
       IFAIL=0
       RETURN
*** Handle I/O problems.
2010   CONTINUE
       PRINT *,' ###### DSNOPN ERROR   : Error while skipping to'//
     -      ' the end of the file '//FILE(1:NC)//'.'
       CALL INPIOS(IOS)
       IFAIL=1
       RETURN
2015   CONTINUE
       PRINT *,' ###### DSNOPN ERROR   : Failure to write a heading'//
     -      ' record to the new file '//FILE(1:NC)//'.'
       CALL INPIOS(IOS)
       IFAIL=1
       RETURN
2020   CONTINUE
       PRINT *,' ###### DSNOPN ERROR   : Failure to open '//FILE(1:NC)//
     -      ' on unit ',LUNDSN
       CALL INPIOS(IOS)
       IFAIL=1
       RETURN
2030   CONTINUE
       PRINT *,' !!!!!! DSNOPN WARNING : I/O problem when closing'//
     -      ' an unknown file on unit ',LUNDSN
       CALL INPIOS(IOS)
       IFAIL=1
       RETURN
2040   CONTINUE
       PRINT *,' ###### DSNOPN ERROR   : Backspace at the end of the'//
     -      ' file '//FILE(1:NC)//' failed.'
       CALL INPIOS(IOS)
       IFAIL=1
       END
