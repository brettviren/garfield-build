CDECK  ID>, GRALOG.
       SUBROUTINE GRALOG(NAME)
*-----------------------------------------------------------------------
*   GRALOG - Routine accumulating data on the plots being produced.
*   GRAPRT   and printing its data when called with an empty name.
*   VARIABLES : NAME       : Description of the plot just completed
*               LIST       : List of the above descriptions
*               ICOUNT     : Counts the number of names entered
*   (Last changed on 26/ 9/08.)
*-----------------------------------------------------------------------
       implicit none
       CHARACTER*40 LIST(100)
       CHARACTER*(*) NAME
       INTEGER ICOUNT,J
       SAVE LIST,ICOUNT
*** Initialise ICOUNT to 0.
       DATA ICOUNT/0/
*** Store the information in LIST.
       IF(ICOUNT.LT.100)THEN
            ICOUNT=ICOUNT+1
            LIST(ICOUNT)=NAME
            RETURN
       ENDIF
*   Issue a warning if 100 plots have been made.
       IF(ICOUNT.EQ.100)THEN
            ICOUNT=101
            PRINT *,' ------ GRALOG MESSAGE : 100 Plots have been'//
     -           ' made ; information on other plots will not be stored'
       ENDIF
       RETURN
*** Print the data stored during the run.
       ENTRY GRAPRT
       WRITE(*,'(''1'')')
       IF(ICOUNT.EQ.0)THEN
            PRINT *,' No plots have been made.'
            RETURN
       ENDIF
       PRINT *,' List of the plots and their frame numbers:'
       PRINT *,' =========================================='
       PRINT *,' '
       PRINT *,' Description of the plot                 Frame number'
       PRINT *,' '
       DO 10 J=1,MIN(100,ICOUNT)
       PRINT '(2X,A40,I12)',LIST(J),J-1
10     CONTINUE
       PRINT *,' '
       PRINT *,' '
       END
