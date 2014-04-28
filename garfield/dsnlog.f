CDECK  ID>, DSNLOG.
       SUBROUTINE DSNLOG(DSNAME,TYPNAM,ACCESS,OPER)
*-----------------------------------------------------------------------
*   DSNLOG - Routine accumulating data on dataset use (eg sceptre data-
*            sets) with an entry to print the data (DSNPRT).
*   VARIABLES : NAME       : Line with information on the dataset.
*               LIST       : List of the above descriptions.
*               ICOUNT     : Counts the number of names entered.
*               ACCESS     : Type of access, set by calling routine.
*               TYPNAM     : Type of data, set by calling routine.
*               OPER       : Type of operation carried out.
*   (Last changed on 26/ 9/08.)
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
       CHARACTER*40 DSN
       CHARACTER*76 LIST(100)
       CHARACTER*(*) DSNAME
       CHARACTER*10 ACCESS,OPER,TYPNAM
       INTEGER ICOUNT,J
       SAVE LIST,ICOUNT
*** Initialise ICOUNT to 0.
       DATA ICOUNT/0/
*** Store the information, if there is still room for them.
       IF(ICOUNT.LT.100)THEN
            DSN=' '
            DSN=DSNAME
            ICOUNT=ICOUNT+1
            LIST(ICOUNT)=DSN//'  '//TYPNAM//'  '//ACCESS//'  '//OPER
       ENDIF
*** Issue a warning if 100 datasets have been accessed
       IF(ICOUNT.EQ.100)THEN
            ICOUNT=101
            PRINT *,' ------ DSNLOG MESSAGE : 100 Datasets have been'//
     -           ' used ; further dataset information not stored.'
       ENDIF
       RETURN
*** Print the list.
       ENTRY DSNPRT
       WRITE(*,'(''1'')')
       IF(ICOUNT.EQ.0)THEN
            PRINT *,' No data sets have been accessed.'
            RETURN
       ENDIF
       PRINT *,' The following datasets have been accessed:'
       PRINT *,' =========================================='
       PRINT *,' '
       PRINT *,' Dataset name                              Type       ',
     -         ' Access      Operation '
       PRINT *,' '
       DO 10 J=1,MIN(ICOUNT,100)
       PRINT *,' ',LIST(J)
10     CONTINUE
       PRINT *,' '
       PRINT *,' '
       END
