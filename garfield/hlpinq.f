CDECK  ID>, HLPINQ.
       SUBROUTINE HLPINQ(PATH,NPATH,EXIST,NSUB,TOPIC,IREC,IFAIL)
*-----------------------------------------------------------------------
*   HLPINQ - This routine determines whether some branch exists or not
*            and it returns the number of subbranches (NSUB) and the
*            topic string (TOPIC) if it does.
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
       INTEGER PATH(NPATH),SUBREC(MXSUBT)
       LOGICAL EXIST
       CHARACTER*20 TOPIC
*** Start lifting the root reference list.
       NXTREC=1
*** And next trace down the path.
       DO 10 I=1,NPATH
       READ(UNIT=17,REC=NXTREC,IOSTAT=IOS,ERR=2010)
     -      TOPIC,NREC,NSUB,(SUBREC(J),J=1,MIN(MXSUBT,NSUB))
       IF(NSUB.GT.MXSUBT)THEN
            PRINT *,' ###### HLPINQ ERROR   : Number of subrecords'//
     -           ' exceeds MXSUBT; recompile with at least ',NSUB
            IFAIL=1
            RETURN
       ENDIF
*   Make sure the next branch really exists, flag if not.
       IF(NSUB.LT.PATH(I).OR.0.GE.PATH(I))THEN
            EXIST=.FALSE.
            IFAIL=0
            RETURN
       ENDIF
*   Set the next reference record.
       NXTREC=SUBREC(PATH(I))
10     CONTINUE
*** Passing here means the record exists.
       READ(UNIT=17,REC=NXTREC,IOSTAT=IOS,ERR=2010)
     -      TOPIC,NREC,NSUB,(SUBREC(I),I=1,MIN(MXSUBT,NSUB))
       IF(NSUB.GT.MXSUBT)THEN
            PRINT *,' ###### HLPINQ ERROR   : Number of subrecords'//
     -           ' exceeds MXSUBT; recompile with at least ',NSUB
            IFAIL=1
            RETURN
       ENDIF
       IREC=NXTREC
       EXIST=.TRUE.
       IFAIL=0
       RETURN
*** Take care of I/O problems.
2010   CONTINUE
       PRINT *,' ###### HLPINQ ERROR   : I/O error on the HELP file.'
       CALL INPIOS(IOS)
       IFAIL=1
       END
