CDECK  ID>, RNDSPH.
      SUBROUTINE RNDSPH(X,Y,Z)
*-----------------------------------------------------------------------
*   RNDSPG - Generates vectors uniformly over a unit sphere.
*   (Last changed on  5/ 9/13.)
*-----------------------------------------------------------------------
      implicit none
      REAL X,Y,Z,RVEC(3),R
 10   CONTINUE
      CALL RNORML(RVEC,3)
      R = SQRT(RVEC(1)**2+RVEC(2)**2+RVEC(3)**2)
      IF(R.LE.0)GOTO 10
      X = RVEC(1)/R
      Y = RVEC(2)/R
      Z = RVEC(3)/R
      END
