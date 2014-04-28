CDECK  ID>, ROUND.
       SUBROUTINE ROUND(XMIN,XMAX,N,DIR,STEP)
*-----------------------------------------------------------------------
*   ROUND  - Rounds the input range (XMIN.XMAX) to the nearest decent
*            interval.
*   VARIABLES : DIR         : The new interval may be larger if .TRUE.
*               N           : The number of intermediate points.
*               STEP        : Contains the step size.
*   (Last changed on 20/ 5/99.)
*-----------------------------------------------------------------------
       implicit none
       REAL XMAX,XMIN,STEP,STNEW,XMINC,XMAXC
       INTEGER N,K
       CHARACTER*(*) DIR
*** Check the validity of the input.
       IF(XMAX.EQ.XMIN)THEN
            STEP=0.0
            RETURN
       ELSEIF(XMAX.LE.XMIN)THEN
            PRINT *,' !!!!!! ROUND  WARNING : Illegal range: ',XMIN,XMAX
            RETURN
       ELSEIF(N.LE.0)THEN
            PRINT *,' !!!!!! ROUND  WARNING : Illegal number of points.'
            RETURN
       ENDIF
*** Set the rough interval.
       STEP=(XMAX-XMIN)/REAL(N)
*   Compute order of magnitude.
       K=NINT(LOG10(STEP))
*   Very large range: abandon.
       IF(K.GT.30)THEN
            RETURN
*   Normal range larger than 1: eliminate order of magnitude.
       ELSEIF(K.GE.0)THEN
            STEP=STEP/10.0**K
*   Very small range: abandon.
       ELSEIF(K.LT.-30)THEN
            RETURN
*   Normal range smaller than 1: eliminate order of magnitude.
       ELSE
            STEP=STEP*10.0**(-K)
       ENDIF
*   Make more bins.
       IF(INDEX(DIR,'COARSER').NE.0)THEN
            IF(STEP.GE.0.1.AND.STEP.LT.0.2)THEN
                 STNEW=0.2
            ELSEIF(STEP.GE.0.2.AND.STEP.LT.0.5)THEN
                 STNEW=0.5
            ELSEIF(STEP.GE.0.5.AND.STEP.LT.1.0)THEN
                 STNEW=1.0
            ELSEIF(STEP.GE.1.0.AND.STEP.LT.2.0)THEN
                 STNEW=2.0
            ELSEIF(STEP.GE.2.0.AND.STEP.LT.5.0)THEN
                 STNEW=5.0
            ELSEIF(STEP.GE.5.0.AND.STEP.LT.10.0)THEN
                 STNEW=10.0
            ELSE
                 PRINT *,' ###### ROUND  ERROR : Unable to find a',
     -                ' new interval for STEP=',STEP,' program bug.'
                 RETURN
            ENDIF
*   Or make fewer bins.
       ELSE
            IF(STEP.GE.0.1.AND.STEP.LT.0.2)THEN
                 STNEW=0.1
            ELSEIF(STEP.GE.0.2.AND.STEP.LT.0.5)THEN
                 STNEW=0.2
            ELSEIF(STEP.GE.0.5.AND.STEP.LT.1.0)THEN
                 STNEW=0.5
            ELSEIF(STEP.GE.1.0.AND.STEP.LT.2.0)THEN
                 STNEW=1.0
            ELSEIF(STEP.GE.2.0.AND.STEP.LT.5.0)THEN
                 STNEW=2.0
            ELSEIF(STEP.GE.5.0.AND.STEP.LT.10.0)THEN
                 STNEW=5.0
            ELSE
                 PRINT *,' ###### ROUND  ERROR : Unable to find a',
     -                ' new interval for STEP=',STEP,' program bug.'
                 RETURN
            ENDIF
       ENDIF
*   Add order of magnitude again.
       IF(K.GE.0)THEN
            STEP=STNEW*10.0**K
       ELSE
            STEP=STNEW/10.0**(-K)
       ENDIF
*   Check whether the bins need to be integer.
       IF(INDEX(DIR,'INTEGER').NE.0.AND.STEP.LT.1)STEP=1
*** Set the new XMIN and XMAX.
       XMINC=STEP*ANINT(XMIN/STEP)
       XMAXC=STEP*ANINT(XMAX/STEP)
       IF(INDEX(DIR,'LARGER').NE.0)THEN
            IF(XMINC.LE.XMIN+STEP/10.0)XMIN=XMINC
            IF(XMINC.GT.XMIN+STEP/10.0)XMIN=XMINC-STEP
            IF(XMAXC.LT.XMAX-STEP/10.0)XMAX=XMAXC+STEP
            IF(XMAXC.GE.XMAX-STEP/10.0)XMAX=XMAXC
       ELSE
            IF(XMINC.LT.XMIN-STEP/10.0)XMIN=XMINC+STEP
            IF(XMINC.GE.XMIN-STEP/10.0)XMIN=XMINC
            IF(XMAXC.LE.XMAX+STEP/10.0)XMAX=XMAXC
            IF(XMAXC.GT.XMAX+STEP/10.0)XMAX=XMAXC-STEP
       ENDIF
       END
