CDECK  ID>, STRBUF.
       SUBROUTINE STRBUF(COMM,IREF,STRING,NC,IFAIL)
*-----------------------------------------------------------------------
*   STRBUF - General purpose dynamical string store.
*   (Last changed on  6/11/99.)
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
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       INTEGER MXSTRL,MXNSTR
       PARAMETER (MXSTRL=50000,MXNSTR=1000)
       CHARACTER*(*) STRING,COMM
       CHARACTER*(MXSTRL) BUFFER
       CHARACTER*10 NAME
       INTEGER IREF,IREFL,NC,REF(3,MXNSTR),NBUF,ISTART,I,J,IFAIL,NOLD
       LOGICAL ACTIVE(MXNSTR)
       SAVE BUFFER,REF,NBUF,ISTART,IREFL,ACTIVE
       DATA ISTART,NBUF,IREFL /1,0,1/
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE STRBUF ///'
*** If requested, store the string.
       IF(COMM.EQ.'STORE')THEN
*   Garbage collection if there is no more space.
            IF(ISTART+NC-1.GT.MXSTRL.OR.NBUF+1.GT.MXNSTR)THEN
*   Inform in case debugging is requested.
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ STRBUF DEBUG   :'',
     -                '' Garbage collection to make room for a new'',
     -                '' string.''/26X,''Free storage: '',I5,
     -                '', Needed: '',I5/26X,''Strings in store: '',I5,
     -                '', Available: '',I5)')
     -                MXSTRL-ISTART+1,NC,NBUF,MXNSTR
*   Reset the start pointer, string number pointer etc.
                 ISTART=1
                 NOLD=NBUF
                 NBUF=0
*   Loop over the strings in store, skipping those that are dropped.
                 DO 10 I=1,NOLD
                 IF(.NOT.ACTIVE(I))GOTO 10
                 NBUF=NBUF+1
                 IF(REF(2,I).GT.0)THEN
                      DO 30 J=1,REF(2,I)
                      BUFFER(ISTART+J-1:ISTART+J-1)=
     -                     BUFFER(REF(1,I)+J-1:REF(1,I)+J-1)
30                    CONTINUE
                 ENDIF
                 REF(1,NBUF)=ISTART
                 REF(2,NBUF)=REF(2,I)
                 REF(3,NBUF)=REF(3,I)
                 ISTART=ISTART+REF(2,NBUF)
                 ACTIVE(NBUF)=.TRUE.
10               CONTINUE
*   Check the amount of free storage again.
                 IF(LDEBUG)WRITE(LUNOUT,'('' ++++++ STRBUF DEBUG   :'',
     -                '' Free storage after garbage collect: '',I5/
     -                26X,''Number of strings in use: '',I5)')
     -                MXSTRL-ISTART+1,NBUF
                 IF(ISTART+NC-1.GT.MXSTRL.OR.NBUF+1.GT.MXNSTR)THEN
                      PRINT *,' ###### STRBUF WARNING : No room to'//
     -                     ' store your string; delete some strings'//
     -                     ' or increase MXSTRL, MXNSTR and recompile.'
                      IFAIL=1
                      RETURN
                 ENDIF
            ENDIF
*   Store the new string.
            NBUF=NBUF+1
            IF(NC.GT.0)BUFFER(ISTART:ISTART+NC-1)=STRING(1:NC)
            IREF=IREFL
            IREFL=IREFL+1
            REF(1,NBUF)=ISTART
            REF(2,NBUF)=NC
            REF(3,NBUF)=IREF
            ACTIVE(NBUF)=.TRUE.
            ISTART=ISTART+NC
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ STRBUF DEBUG   :'',
     -           '' Stored "'',A,''"''/26X,''Reference='',I5,
     -           '', Start='',I5,'', Record='',I5)')
     -           STRING(1:NC),IREF,REF(1,NBUF),NBUF
            IFAIL=0
*** Read an existing string.
       ELSEIF(COMM.EQ.'READ')THEN
            DO 100 I=1,NBUF
            IF(REF(3,I).NE.IREF)GOTO 100
            IF(.NOT.ACTIVE(I))PRINT *,' !!!!!! STRBUF WARNING :'//
     -           ' The string has been deleted but is still in store.'
            IF(REF(2,I).GT.LEN(STRING))PRINT *,' !!!!!! STRBUF'//
     -           ' WARNING : String longer than receiving string'//
     -           ' length; truncated.'
            IF(REF(2,I).GT.0)THEN
                 STRING=BUFFER(REF(1,I):REF(1,I)+REF(2,I)-1)
            ELSE
                 STRING=' '
            ENDIF
            NC=MIN(REF(2,I),LEN(STRING))
            IFAIL=0
            RETURN
100         CONTINUE
            PRINT *,' !!!!!! STRBUF WARNING : The string you ask for'//
     -           ' is not in store.'
            NC=20
            STRING='< string not found >'
            IFAIL=1
*** Delete the string.
       ELSEIF(COMM.EQ.'DELETE')THEN
            DO 200 I=1,NBUF
            IF(REF(3,I).NE.IREF)GOTO 200
            ACTIVE(I)=.FALSE.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ STRBUF DEBUG   :'',
     -           '' Deleted string with reference '',I5)') IREF
            IFAIL=0
            RETURN
200         CONTINUE
            PRINT *,' !!!!!! STRBUF WARNING : The string you ask for'//
     -           ' is not in store.'
            IFAIL=1
*** Dump the entire contents.
       ELSEIF(COMM.EQ.'DUMP')THEN
            WRITE(LUNOUT,'(/''  CURRENTLY KNOWN STRINGS:''//
     -           ''    No Start    NC   Ref Global     String'')')
            DO 300 I=1,NBUF
            NAME='< none >'
            DO 710 J=1,NGLB
            IF(GLBMOD(J).EQ.1.AND.NINT(GLBVAL(J)).EQ.REF(3,I))
     -           NAME=GLBVAR(J)
710         CONTINUE
            IF(.NOT.ACTIVE(I))THEN
                 WRITE(LUNOUT,'(4(1X,I5),1X,A10,1X,A)') I,REF(1,I),
     -                REF(2,I),REF(3,I),NAME,'(deleted)'
            ELSE
                 WRITE(LUNOUT,'(4(1X,I5),1X,A10,1X,A)') I,REF(1,I),
     -                REF(2,I),REF(3,I),NAME,
     -                BUFFER(REF(1,I):REF(1,I)+REF(2,I)-1)
            ENDIF
300         CONTINUE
            IFAIL=0
            WRITE(LUNOUT,'(/''  Total of '',I3,'' strings.''/)') NBUF
*** Anything else is not valid.
       ELSE
            PRINT *,' ###### STRBUF ERROR   : Unknown command ',COMM,
     -           ' received.'
            IFAIL=1
       ENDIF
       END
