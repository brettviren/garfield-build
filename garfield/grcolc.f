CDECK  ID>, GRCOLC.
       SUBROUTINE GRCOLC(IWKID,IWKTYP,IFLAG)
*-----------------------------------------------------------------------
*   GRCOLC - Routine figures out whether a wk has got colours or not.
*   (Last changed on  5/ 9/99.)
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
       INTEGER IWKID,IWKTYP,IFLAG,IERR,ISTATE,ICONID,IWKCAT,
     -      NCOLS,ICOLS,NPRE
*** Initial value: 1 meaning no colours.
       IFLAG=1
*** Make sure the wk is active.
       CALL GQWKS(IWKID,IERR,ISTATE)
       IF(IERR.NE.0.OR.ISTATE.NE.1)THEN
            PRINT *,' !!!!!! GRCOLC WARNING : The workstation on'//
     -           ' which the colours are to be set is not active.'
            RETURN
       ENDIF
*** Determine wk type and category.
       CALL GQWKC(IWKID,IERR,ICONID,IWKTYP)
       IF(IERR.NE.0)THEN
            PRINT *,' !!!!!! GRCOLC WARNING : Unable to determine the'//
     -           ' workstation type ; no colours set.'
            RETURN
       ENDIF
       CALL GQWKCA(IWKTYP,IERR,IWKCAT)
       IF(IERR.NE.0)THEN
            PRINT *,' !!!!!! GRCOLC WARNING : Unable to determine the'//
     -           ' workstation category ; no colours set.'
            RETURN
       ENDIF
*   For WISS and MO, no way to see whether there are colours.
       IF(IWKCAT.EQ.3.OR.IWKCAT.EQ.4)THEN
            IF(LDEBUG)PRINT *,' ++++++ GRCOLC DEBUG   : Workstation'//
     -           ' category WISS or MO; no further checks.'
            IFLAG=-1
            RETURN
       ENDIF
*** Ask the number of colours.
       CALL GQCF(IWKTYP,IERR,NCOLS,ICOLS,NPRE)
       IF(LDEBUG)WRITE(LUNOUT,*)
     -      ' ++++++ GRCOLC DEBUG   : Colour data'//
     -      ' for workstation ',IWKID,' of type ',IWKTYP,':'
       IF(LDEBUG)WRITE(LUNOUT,*)
     -      '                         Colours y/n',
     -      ICOLS,', number of colours: ',NCOLS,', predefined: ',NPRE
       IF(IERR.NE.0)THEN
            PRINT *,' !!!!!! GRCOLC WARNING : Unable to determine'//
     -           ' whether the workstation has colours ; nothing done.'
            RETURN
       ELSEIF(ICOLS.EQ.0.OR.NCOLS.EQ.2)THEN
            PRINT *,' !!!!!! GRCOLC WARNING : The workstation has'//
     -           ' no colour facilities ; nothing done.'
            RETURN
       ENDIF
*** OK, set flag to 0.
       IFLAG=0
       END
