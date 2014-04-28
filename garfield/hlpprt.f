CDECK  ID>, HLPPRT.
       SUBROUTINE HLPPRT(IREC,INDENT,IFAIL)
*-----------------------------------------------------------------------
*   HLPPRT - Prints the item starting at record IREC
*   (Last changed on 20/ 7/00.)
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
       INTEGER SUBREC(MXSUBT),NREC,NSUB,J,IOUT,NSTR,I0,I1,IREC,INDENT,
     -      IFAIL,IOS
       CHARACTER*20 TOPIC
       CHARACTER*132 OUT
       CHARACTER*(MXSUBT) BLANK
       CHARACTER*(MXHLRL) STRING
*** Set BLANK to blank.
       BLANK=' '
*** Read the heading record and loop over all records of the item.
       READ(UNIT=17,REC=IREC,IOSTAT=IOS,ERR=2010)
     -      TOPIC,NREC,NSUB,(SUBREC(J),J=1,MIN(MXSUBT,NSUB))
       IF(NSUB.GT.MXSUBT)THEN
            PRINT *,' ###### HLPPRT ERROR   : Number of subrecords'//
     -           ' exceeds MXSUBT; recompile with at least ',NSUB
            IFAIL=1
            RETURN
       ENDIF
*   Print the TOPIC as a heading.
       WRITE(LUNOUT,'(1X,A,/)') BLANK(1:INDENT)//TOPIC
*   Record loop.
       OUT=' '
       IOUT=1
       DO 10 J=1,NREC
       READ(UNIT=17,REC=IREC+J,IOSTAT=IOS,ERR=2010) STRING
*   Determine the length of the string.
       NSTR=INDEX(STRING,CHAR(11))-1
       IF(NSTR.EQ.-1)THEN
            NSTR=MXHLRL
       ELSEIF(NSTR.EQ.0)THEN
            GOTO 10
       ENDIF
*   Figure out where the line-breaks are.
       I0=1
20     CONTINUE
       I1=I0+INDEX(STRING(I0:NSTR),CHAR(10))-2
*   Take the end of the line in case there is no LF left.
       IF(I1.EQ.I0-2)I1=NSTR
*   Print or skip a line if I1 < I0.
       IF(I1.LT.I0)THEN
            IF(IOUT.GT.1)THEN
                 WRITE(LUNOUT,'(1X,A)') BLANK(1:INDENT)//OUT(1:IOUT-1)
            ELSE
                 WRITE(LUNOUT,'(1X)')
            ENDIF
            OUT=' '
            IOUT=1
            I0=I1+2
            IF(I0.LE.NSTR)GOTO 20
*   Restrict when the total record would be too long.
       ELSEIF(IOUT+I1-I0.GT.LEN(OUT))THEN
            PRINT *,' ###### HLPPRT ERROR   : Record longer'//
     -           ' than ',LEN(OUT),' characters encountered.'
            I1=LEN(OUT)+I0-IOUT
            OUT(IOUT:IOUT+I1-I0)=STRING(I0:I1)
            IOUT=IOUT+I1-I0+1
            WRITE(LUNOUT,'(1X,A)')
     -           BLANK(1:INDENT)//OUT(1:IOUT-1)
            OUT=' '
            IOUT=1
            I0=I1+1
            IF(I0.LE.NSTR)GOTO 20
*   Buffer when no line-break is present at the end of record.
       ELSEIF(I1.EQ.NSTR.AND.STRING(NSTR:NSTR).NE.CHAR(10))THEN
            OUT(IOUT:IOUT+I1-I0)=STRING(I0:I1)
            IOUT=IOUT+I1-I0+1
            I0=I1+1
            IF(I0.LE.NSTR)GOTO 20
*   Output when the line-break is seen.
       ELSE
            OUT(IOUT:IOUT+I1-I0)=STRING(I0:I1)
            WRITE(LUNOUT,'(1X,A)') BLANK(1:INDENT)//OUT(1:IOUT+I1-I0)
            OUT=' '
            IOUT=1
            I0=I1+2
            IF(I0.LE.NSTR)GOTO 20
       ENDIF
*   Next record.
10     CONTINUE
*** Print the remainder of the last record.
       IF(IOUT.GT.1)WRITE(LUNOUT,'(1X,A)')
     -      BLANK(1:INDENT)//OUT(1:IOUT-1)
       WRITE(LUNOUT,'('' '')')
*** Things worked it seems.
       IFAIL=0
       RETURN
*** Handle I/O errors.
2010   CONTINUE
       PRINT *,' ###### HLPPRT ERROR   : I/O error on the HELP file.'
       CALL INPIOS(IOS)
       IFAIL=1
       END
