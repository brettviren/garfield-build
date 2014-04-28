CDECK  ID>, GRQUIT.
       SUBROUTINE GRQUIT
*-----------------------------------------------------------------------
*   GRQUIT - This routines calls some routines that print information
*            collected during the run and closes in batch mode the
*            display file.
*-----------------------------------------------------------------------
       LOGICAL OPEN,INTRAC
       EXTERNAL INTRAC
       CALL GRGRAF(.TRUE.)
       CALL GDAWK(1)
       CALL GCLWK(1)
       CALL GCLKS
       INQUIRE(UNIT=10,OPENED=OPEN)
       IF(OPEN)CLOSE(UNIT=10,STATUS='KEEP')
       INQUIRE(UNIT=11,OPENED=OPEN)
       CLOSE(UNIT=11,STATUS='KEEP')
       END
