CDECK  ID>, HLPSUB.
       SUBROUTINE HLPSUB(IREC,INDENT,IFAIL)
*-----------------------------------------------------------------------
*   HLPSUB - List the subtopics for the item starting at record IREC.
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
       INTEGER SUBREC(MXSUBT)
       CHARACTER*20 TOPIC
       CHARACTER*80 OUT
       CHARACTER*(MXSUBT) BLANK
*** Set BLANK to blank.
       BLANK=' '
*** Read the heading record and loop over all records of the item.
       READ(UNIT=17,REC=IREC,IOSTAT=IOS,ERR=2010)
     -      TOPIC,NREC,NSUB,(SUBREC(J),J=1,MIN(MXSUBT,NSUB))
       IF(NSUB.GT.MXSUBT)THEN
            PRINT *,' ###### HLPSUB ERROR   : Number of subrecords'//
     -           ' exceeds MXSUBT; recompile with at least ',NSUB
            IFAIL=1
            RETURN
       ENDIF
*** Last record done, print the candidate subtopics.
       IF(NSUB.GT.0)THEN
*   Print a heading.
            WRITE(LUNOUT,'(/,1X,A,/)')
     -           BLANK(1:INDENT)//'Additional information available:'
            OUT=' '
            IOUT=1
*   Pick up the topics one by one.
            DO 100 I=1,NSUB
            READ(UNIT=17,REC=SUBREC(I),IOSTAT=IOS,ERR=2010) TOPIC
*   Figure out how long the topic is.
            DO 110 J=20,1,-1
            IF(TOPIC(J:J).NE.' ')THEN
                 NTOPIC=J
                 GOTO 120
            ENDIF
110         CONTINUE
*   Substitute a string if empty.
            TOPIC='< not named >'
            NTOPIC=13
120         CONTINUE
*   Output the string if the new topic won't fit anymore.
            IF(INDENT+IOUT+NTOPIC-1.GE.80)THEN
                 WRITE(LUNOUT,'(1X,A)') BLANK(1:INDENT)//OUT(1:IOUT)
                 IOUT=1
                 OUT=' '
            ENDIF
*   Store the subtopic names in an output string, properly tabbed.
            OUT(IOUT:IOUT+NTOPIC-1)=TOPIC(1:NTOPIC)
            DO 130 J=1,61,15
            IF(OUT(MAX(1,J-2):).EQ.' ')THEN
                 IOUT=J
                 GOTO 100
            ENDIF
130         CONTINUE
*   Output the string if the new topic won't fit anymore.
            WRITE(LUNOUT,'(1X,A)') BLANK(1:INDENT)//OUT(1:IOUT+NTOPIC-1)
            IOUT=1
            OUT=' '
100         CONTINUE
*   Don't forget to output the last piece of string.
            WRITE(LUNOUT,'(1X,A)') BLANK(1:INDENT)//OUT(1:IOUT)
       ELSE
            WRITE(LUNOUT,'(/,''  No subtopics.'',/)')
       ENDIF
*** Things worked it seems.
       IFAIL=0
       RETURN
*** Handle I/O errors.
2010   CONTINUE
       PRINT *,' ###### HLPSUB ERROR   : I/O error on the HELP file.'
       CALL INPIOS(IOS)
       IFAIL=1
       END
