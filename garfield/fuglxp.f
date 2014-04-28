CDECK  ID>, FUGLXP.
       SUBROUTINE FUGLXP (FUNC,XFCUM,X2LOW,X2HIGH,IFAIL)
*-----------------------------------------------------------------------
*   FUGLXP - Prepares the user function FUNC for FUGLUX.
*            Inspired by and mostly copied from FUNPRE and FUNRAN
*            except that
*            1. FUNLUX uses RANLUX underneath,
*            2. FUNLXP expands the first and last bins to cater for
*               functions with long tails on left and/or right,
*            3. FUNLXP calls FUNPCT to do the actual finding of
*               percentiles.
*            4. both FUNLXP and FUNPCT use RADAPT for Gaussian
*               integration.
*   Origin: V152, Fred James, Sept 1994
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
       EXTERNAL FUNC
       INTEGER IFAIL,IERR
       REAL XFCUM(200),X2LOW,X2HIGH,XLOW,XHIGH,XRANGE,X2,X3,RTEPS,TFTOT,
     -      TFTOT1,TFTOT2,UNCERT,FUNC
       PARAMETER (RTEPS=0.0002)
*** Find range where function is non-zero.
       CALL FUGLZ(FUNC,X2LOW,X2HIGH,XLOW,XHIGH)
       XRANGE = XHIGH-XLOW
       IF(XRANGE .LE. 0)THEN
            PRINT *,' ###### FUGLXP ERROR   : Non-zero range of the'//
     -           ' function has non-positive length; function not'//
     -           ' prepared for random number generation.'
            IFAIL=1
            RETURN
       ENDIF
*** Integrate the function.
       CALL RADAPT(FUNC,XLOW,XHIGH,1,RTEPS,0.,TFTOT ,UNCERT)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ FUGLXP DEBUG   : Integral'',
     -      '' from '',E12.5,'' to '',E12.5,'' is '',E12.5)')
     -      XLOW,XHIGH,TFTOT
*** Compute percentiles.
       CALL FUGPCT(FUNC,XLOW,XHIGH,XFCUM,1,99,TFTOT,IERR)
       IF (IERR .GT. 0)  GOTO 900
       X2 = XFCUM(3)
       CALL RADAPT(FUNC,XLOW,X2,1,RTEPS,0.,TFTOT1 ,UNCERT)
       CALL FUGPCT(FUNC,XLOW,X2 ,XFCUM,101,49,TFTOT1,IERR)
       IF (IERR .GT. 0)  GOTO 900
       X3 = XFCUM(98)
       CALL RADAPT(FUNC,X3,XHIGH,1,RTEPS,0.,TFTOT2 ,UNCERT)
       CALL FUGPCT(FUNC,X3,XHIGH,XFCUM,151,49,TFTOT2,IERR)
       IF (IERR .GT. 0)  GOTO 900
*** Seems to have worked.
       IFAIL=0
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ FUGLXP DEBUG   : Function'',
     -      '' successfully prepared.'')')
       RETURN
*** Error processing.
  900  CONTINUE
       IFAIL=1
       PRINT *,' ###### FUGLXP ERROR   : Error while computing the'//
     -     ' percentiles ; can not generate random numbers.'
       END
