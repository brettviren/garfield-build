CDECK  ID>, HLPDEB.
       SUBROUTINE HLPDEB
*-----------------------------------------------------------------------
*   HLPDEB - Debugging routine that dumps the entire HELP file.
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
       INTEGER PATH(MXSUBT)
       CHARACTER*20 TOPIC
       LOGICAL EXIST
*** Open the help file.
       INQUIRE(FILE='garfield.packhelp',EXIST=EXIST)
       IF(.NOT.EXIST)THEN
            PRINT *,' !!!!!! HLPDEB WARNING : The HELP library can''t'//
     -           ' be found; no help is offered.'
            CALL INPPRM(' ','BACK')
            RETURN
       ENDIF
       OPEN(UNIT=17,FILE='garfield.packhelp',ACCESS='DIRECT',
     -      STATUS='OLD',RECL=MXHLRL,IOSTAT=IOS,ERR=2020)
*** Search the entire tree, start at the root.
       NPATH=1
       PATH(1)=1
10     CONTINUE
       CALL HLPINQ(PATH,NPATH,EXIST,NSUB,TOPIC,IREC,IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! HLPDEB WARNING : Inquiry for the'//
     -           ' existence of a topic failed; help ended.'
            RETURN
       ENDIF
       IF(EXIST)THEN
            CALL HLPPRT(IREC,2*NPATH,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! HLPDEB WARNING : Unable to print'//
     -                ' the subtopics; help ended.'
                 RETURN
            ENDIF
            CALL HLPSUB(IREC,2*NPATH,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! HLPDEB WARNING : Unable to list'//
     -                ' the subtopics; help ended.'
                 RETURN
            ENDIF
            NPATH=NPATH+1
            PATH(NPATH)=1
       ELSE
            NPATH=NPATH-1
            IF(NPATH.LE.0)THEN
                 PRINT *,' End of listing.'
                 CLOSE(UNIT=17,STATUS='KEEP',ERR=2030)
                 RETURN
            ENDIF
            PATH(NPATH)=PATH(NPATH)+1
       ENDIF
       GOTO 10
*** Handle I/O errors during opening of the file.
2020   CONTINUE
       PRINT *,' ###### HLPDEB WARNING : Unable to open the help file.'
       RETURN
2030   CONTINUE
       PRINT *,' ###### HLPDEB WARNING : Unable to close the help file.'
       END
