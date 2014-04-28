CDECK  ID>, STDSTRO.
       LOGICAL FUNCTION STDSTR(STREAM)
*-----------------------------------------------------------------------
*   STDSTR - Checks whether the data stream STREAM is connected to
*            standard input or output.
*   (Last changed on 19/12/10.)
*-----------------------------------------------------------------------
       implicit none
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*(*) STREAM
       INTEGER LENARG,IARG,NARGS,INIT,INPCMX,DUMMY
       CHARACTER*128 ARGS
       LOGICAL LSTATE,INTRAC
       EXTERNAL INPCMX,INTRAC
       SAVE LSTATE,INIT
*** For input.
       IF(STREAM.EQ.'INPUT')THEN
*   On first call, determine the state.
            DATA INIT/0/
            IF(INIT.EQ.0)THEN
*   Default is obtained from INTRAC.
                 LSTATE=INTRAC(DUMMY)
*   Loop over the command line arguments.
                 NARGS=iargc()
                 DO 10 IARG=1,NARGS
*   Fetch the option.
                 CALL ARGGET(IARG,ARGS,LENARG)
*   If -interactive, then force interactive mode.
                 IF(INPCMX(args(1:LENARG),'-interact#ive').NE.0)THEN
                      LSTATE=.TRUE.
*   If -batch, then force batch mode.
                 ELSEIF(INPCMX(ARGS(1:LENARG),'-batch').NE.0)THEN
                      LSTATE=.FALSE.
                 ENDIF
10               CONTINUE
                 INIT=1
            ENDIF
*   On subsequent calls, retrieve old state.
            STDSTR=LSTATE
*** Output.
       ELSEIF(STREAM.EQ.'OUTPUT')THEN
            STDSTR=LUNOUT.EQ.6
*** Other streams not known.
       ELSE
            PRINT *,' !!!!!! STDSTR WARNING : Received an unknown'//
     -           ' stream name "',STREAM,'"; returning "True".'
            STDSTR=.TRUE.
       ENDIF
       END
