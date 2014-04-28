CDECK  ID>, SGFLAG.
       SUBROUTINE SGFLAG
*-----------------------------------------------------------------------
*   SGFLAG - Sets GFLAG in HIGZ according to the workstations active.
*   (Last changed on  8/ 1/08.)
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
C*KEEP,HIFLAG.
C*CMZ :  1.21/05 16/06/94  14.37.23  by  O.Couet
*-- Author :
       LOGICAL GFLAG,GLFLAG,ZFLAG,PFLAG,MFLAG,TFLAG,
     -      ASFLAG,GRFLAG,AXFLAG,CFLAG
       COMMON /HIFLAG/ GFLAG,GLFLAG,ZFLAG,PFLAG,MFLAG,TFLAG,
     -      ASFLAG,GRFLAG,AXFLAG,CFLAG
       INTEGER IOPSTA,IERR,NACT,IWK,I,IDUM,ICONID,IWKTYP,ICAT
*** Disable temporarily.
C       return
*** Initial setting.
       GFLAG=.FALSE.
*** Determine Operating State value.
       CALL GQOPS(IOPSTA)
*** For states less than 'workstation active' flag is off.
       IF(IOPSTA.LT.3)THEN
            GFLAG=.FALSE.
*** If a workstation is active, see whether there is an interactive one.
       ELSE
            GFLAG=.FALSE.
            CALL GQACWK(0,IERR,NACT,IWK)
            DO 10 I=1,NACT
            CALL GQACWK(I,IERR,IDUM,IWK)
            CALL GQWKC(IWK,IERR,ICONID,IWKTYP)
            CALL GQWKCA(IWKTYP,IERR,ICAT)
            IF(ICAT.EQ.2)GFLAG=.TRUE.
10          CONTINUE
       ENDIF
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SGFLAG DEBUG   :'',
     -      '' Setting GFLAG to '',L1,''.'')') GFLAG
       END
