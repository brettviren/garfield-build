CDECK  ID>, TIMLOG.
       SUBROUTINE TIMLOG(NAME)
*-----------------------------------------------------------------------
*   TIMLOG - Routine accumulating data on CPU-time usage and printing
*            its data when called with an empty name.
*   VARIABLES : CPU        : CPU time used since previous timing.
*               TIME       : Vector containing the cpu times used.
*               NAME       : Description of the step just completed.
*               LIST       : List of the above descriptions.
*               ICOUNT     : Counts the number of names entered.
*   (Last changed on  7/10/08.)
*-----------------------------------------------------------------------
       implicit none
       CHARACTER*(*) NAME
       CHARACTER*40 LIST(100)
       INTEGER ICOUNT,J
       REAL CPU
       INTEGER TIME(100)
       SAVE LIST,TIME,ICOUNT
*** Initialise ICOUNT.
       DATA ICOUNT/0/
*** If the input is all blank, print the LIST and TIME vectors.
       IF(NAME(1:1).EQ.' ')THEN
            WRITE(*,'(''1'')')
            IF(ICOUNT.EQ.0)THEN
                 PRINT *,' No steps have been executed.'
                 RETURN
            ENDIF
            PRINT *,' CPU time usage for some selected steps:'
            PRINT *,' ======================================='
            PRINT *,' '
            PRINT *,' Description of the step                '//
     -           '  CPU time used'
            PRINT *,' '
            DO 10 J=1,MIN(ICOUNT,100)
            PRINT '(2X,A40,I14)',LIST(J),TIME(J)
10          CONTINUE
*** Otherwise store the information obtained.
       ELSEIF(ICOUNT.LT.100)THEN
            ICOUNT=ICOUNT+1
            LIST(ICOUNT)=NAME
            CALL TIMED(CPU)
            TIME(ICOUNT)=INT(1000.0*CPU)
*** Print a warning if 100 items have been stored.
       ELSEIF(ICOUNT.EQ.100)THEN
            ICOUNT=101
            PRINT *,' ------ TIMLOG MESSAGE : 100 Items have been'//
     -           ' stored ; no further CPU time registration.'
       ENDIF
       END
