CDECK  ID>, HLPCNT.
       SUBROUTINE HLPCNT(NOUT,IFAIL)
*-----------------------------------------------------------------------
*   HLPCNT - Counts the number of records the packed dataset will have.
*   (Last changed on 21/11/90.)
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
       CHARACTER*80 IN
       CHARACTER*(MXHLRL) OUT
*** Check the existence of both raw and processed help files.
       INQUIRE(FILE='garfield.rawhelp',EXIST=EXIST)
       IF(.NOT.EXIST)THEN
            PRINT *,' !!!!!! HLPCNT WARNING : Raw help dataset not'//
     -           ' found ; no record count.'
            IFAIL=1
            RETURN
       ENDIF
*** Open the raw help file.
       OPEN(UNIT=12,FILE='garfield.rawhelp',STATUS='OLD',IOSTAT=IOS,
     -      ERR=2020)
*** Initialise various global variables.
       NOUT=1
       NIN=0
       IOUT=1
       OUT=' '
**  Read a line from the file, skipping comment lines.
10     CONTINUE
       READ(12,'(A80)',IOSTAT=IOS,ERR=2010,END=20) IN
       LENIN=80
       NIN=NIN+1
       IF(IN(1:1).EQ.'!')GOTO 10
**  New heading level.
       IF(IN(1:2).NE.'  ')THEN
            NOUT=NOUT+2
            IOUT=1
            OUT=' '
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
*   Add the present line to the buffer.
            IFIRST=3
120         CONTINUE
            ILAST=MIN(N+1,IFIRST+MXHLRL-1)
            IF(IOUT+ILAST-IFIRST.GT.MXHLRL)ILAST=MXHLRL-IOUT+IFIRST
            IF(IOUT+ILAST-IFIRST.EQ.MXHLRL)THEN
                 NOUT=NOUT+1
                 IOUT=1
                 OUT=' '
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
       NOUT=NOUT+1
*   Close the files.
       CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
*   Signal to the calling routine that everything worked well.
       IFAIL=0
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ HLPCNT DEBUG   : Expected'',
     -      '' count of the number of output records:'',I5/26X,
     -      ''The input file contains'',I5,'' records.'')') NOUT,NIN
       RETURN
*** Handle I/O errors.
2010   CONTINUE
       PRINT *,' ###### HLPCNT ERROR   : I/O error reading the raw'//
     -      ' help file at record ',NIN,'; no record count.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
       IFAIL=1
       RETURN
2020   CONTINUE
       PRINT *,' ###### HLPCNT ERROR   : Unable to open the raw help'//
     -      ' file ; no record count.'
       CALL INPIOS(IOS)
       IFAIL=1
       RETURN
2030   CONTINUE
       PRINT *,' !!!!!! HLPCNT WARNING : Unable to close the raw'//
     -      ' help file ; record count probably OK.'
       CALL INPIOS(IOS)
       RETURN
       END
