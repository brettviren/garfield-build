CDECK  ID>, BOOK.
       SUBROUTINE BOOK(ACTION,REFER,MYNAME,IFAIL)
*-----------------------------------------------------------------------
*   BOOK   - Book keeping of various items.
*   (Last changed on 12/10/00.)
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
       INTEGER MXBOOK
       PARAMETER(MXBOOK=50)
       CHARACTER*(*) ACTION,REFER,MYNAME
       CHARACTER*10 NAME(MXBOOK),USER(MXBOOK)
       INTEGER STATE(MXBOOK),IFAIL,INPCMX,NBOOK,IREF,I
       EXTERNAL INPCMX
       SAVE NAME,STATE,NBOOK,USER
       DATA NBOOK/0/
*** Allocate a new class.
       IF(INPCMX(ACTION,'INIT#IALISE').NE.0)THEN
*   Check there is space left.
            IF(NBOOK.GE.MXBOOK)THEN
                 PRINT *,' ###### BOOK   ERROR   : No room to for'//
     -                ' the new object ',REFER,'.'
                 IFAIL=1
                 RETURN
            ENDIF
*   Add the item to the list.
            NBOOK=NBOOK+1
            NAME(NBOOK)=REFER
            STATE(NBOOK)=0
            USER(NBOOK)=' '
*   Debugging output.
            IF(LDEBUG)PRINT *,' ++++++ BOOK   DEBUG   : New object ',
     -           REFER,' declared as item ',NBOOK,'.'
*   Successful completion.
            IFAIL=0
*** Book an object.
       ELSEIF(INPCMX(ACTION,'BOOK').NE.0)THEN
*   Locate the object.
            IREF=0
            DO 10 I=1,NBOOK
            IF(INPCMX(REFER,NAME(I)).NE.0)IREF=I
10          CONTINUE
*   Object not known.
            IF(IREF.EQ.0)THEN
                 PRINT *,' !!!!!! BOOK   WARNING : The object ',
     -                REFER,' is not known ; not booked.'
                 IFAIL=1
                 RETURN
            ENDIF
*   First check the object has not yet been booked.
            IF(STATE(IREF).EQ.1.AND.USER(IREF).EQ.MYNAME)THEN
                 PRINT *,' ------ BOOK   MESSAGE : Object ',
     -                REFER,' is already booked by same user;'//
     -                ' not booked again.'
                 IFAIL=0
                 RETURN
            ELSEIF(STATE(IREF).EQ.1)THEN
                 PRINT *,' !!!!!! BOOK   WARNING : Object ',
     -                REFER,' is already booked by user '//
     -                USER(IREF)//'; not booked again.'
                 IFAIL=1
                 RETURN
            ENDIF
*   Book the object.
            STATE(IREF)=1
            USER(IREF)=MYNAME
*   Debugging output.
            IF(LDEBUG)PRINT *,' ++++++ BOOK   DEBUG   : Object ',
     -           REFER,' booked by ',MYNAME,'.'
*   Successful completion.
            IFAIL=0
*** Release an object.
       ELSEIF(INPCMX(ACTION,'REL#EASE').NE.0)THEN
*   Locate the object.
            IREF=0
            DO 20 I=1,NBOOK
            IF(INPCMX(REFER,NAME(I)).NE.0)IREF=I
20          CONTINUE
*   Object not known.
            IF(IREF.EQ.0)THEN
                 PRINT *,' !!!!!! BOOK   WARNING : The object ',
     -                REFER,' is not known ; not released.'
                 IFAIL=1
                 RETURN
            ENDIF
*   Don't release an object booked by someone else.
            IF(STATE(IREF).EQ.1.AND.USER(IREF).NE.MYNAME)THEN
                 PRINT *,' !!!!!! BOOK   WARNING : The object ',
     -                REFER,' was booked by ',USER(IREF)
                 PRINT *,'                         Permission'//
     -                ' to release denied ; not released.'
                 IFAIL=1
                 RETURN
            ENDIF
*   Debugging output.
            IF(LDEBUG)PRINT *,' ++++++ BOOK   DEBUG   : Object ',
     -           REFER,' released, previous state ',STATE(IREF),
     -           ', previous user ',USER(IREF)
*   Release the object.
C            IF(INPCMX(ACTION,'CL#EAR').NE.0)THEN
                 STATE(IREF)=0
                 USER(IREF)=' '
C            ELSE
C                 STATE(IREF)=2
C            ENDIF
*   Successful completion.
            IFAIL=0
*** Inquiry.
       ELSEIF(INPCMX(ACTION,'INQ#UIRE').NE.0)THEN
*   Locate the object.
            IREF=0
            DO 30 I=1,NBOOK
            IF(INPCMX(REFER,NAME(I)).NE.0)IREF=I
30          CONTINUE
*   Object not known.
            IF(IREF.EQ.0)THEN
                 PRINT *,' !!!!!! BOOK   WARNING : The object ',
     -                REFER,' is not known ; no information.'
                 IFAIL=1
                 RETURN
            ENDIF
*   Return the user name.
            IF(STATE(IREF).EQ.0)THEN
                 MYNAME=' '
            ELSE
                 MYNAME=USER(IREF)
            ENDIF
*   Successful completion.
            IFAIL=0
*** List of states.
       ELSEIF(INPCMX(ACTION,'L#IST').NE.0)THEN
*   Header, depending on the number of objects.
            IF(NBOOK.EQ.0)THEN
                 WRITE(LUNOUT,'(/''  No objects defined sofar.''/)')
                 IFAIL=0
                 RETURN
            ELSE
                 WRITE(LUNOUT,'(/''  CURRENTLY KNOWN OBJECTS:''//
     -                ''  Name      '',5X,'' Status'')')
            ENDIF
*   List of objects.
            DO 40 I=1,NBOOK
            IF(STATE(I).EQ.0)THEN
                 WRITE(LUNOUT,'(2X,A10,5X,'' Declared, not in use'')')
     -                NAME(I)
            ELSEIF(STATE(I).EQ.1)THEN
                 WRITE(LUNOUT,'(2X,A10,5X,'' Booked by '',A10)')
     -                NAME(I),USER(I)
            ELSEIF(STATE(I).EQ.2)THEN
                 WRITE(LUNOUT,'(2X,A10,5X,'' Free, last used by '',
     -                A10)') NAME(I),USER(I)
            ELSE
                 WRITE(LUNOUT,'(2X,A10,5X,'' Declared, state code '',
     -                I5,'', user '',A10)') STATE(I),NAME(I),USER(I)
            ENDIF
40          CONTINUE
            WRITE(LUNOUT,'('' '')')
*   Always successful.
            IFAIL=0
*** Unknown action.
       ELSE
            PRINT *,' !!!!!! BOOK   WARNING : Unknown request ',ACTION,
     -           ' received; nothing done.'
            IFAIL=1
       ENDIF
       END
