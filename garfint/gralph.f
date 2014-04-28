CDECK  ID>, GRALPH.
       SUBROUTINE GRALPH
*-----------------------------------------------------------------------
*   GRALPH - Switches the screen from graphics to alpha mode. Largely
*            copied from GKSPACK (J551) written by Ian McLaren.
*   (Last changed on 13/12/07.)
*-----------------------------------------------------------------------
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
*** Check there is at least one workstation active.
       CALL GQOPS(IOPSTA)
       IF(IOPSTA.LT.3)THEN
            IF(LDEBUG)WRITE(10,'(''  ++++++ GRALPH DEBUG   :'',
     -           '' No active workstations.'')')
            RETURN
       ENDIF
*** Check that there is at least one workstation with input.
       CALL GQACWK(0,IERR,NACT,IWK)
       IWKREQ=-1
       DO 20 I=1,NACT
       CALL GQACWK(I,IERR,IDUM,IWK)
*   Locate one that has input facilities.
       CALL GQWKC(IWK,IERR1,ICONID,IWKTYP)
       CALL GQWKCA(IWKTYP,IERR2,IWKCAT)
       IF(IWKCAT.EQ.1.OR.IWKCAT.EQ.2)IWKREQ=IWK
20     CONTINUE
*   Return if not found.
       IF(IWKREQ.EQ.-1)THEN
            IF(LDEBUG)WRITE(10,'(''  ++++++ GRALPH DEBUG   :'',
     -           '' No active workstation with input.'')')
            RETURN
       ENDIF
*** Switch back to alpha mode (HIGZ version).
       CALL IGSA(IWKREQ)
       END
