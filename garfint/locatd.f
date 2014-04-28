CDECK  ID>, LOCATD.
       INTEGER FUNCTION LOCATD(ARRAY,LENGTH,OBJECT)
*-----------------------------------------------------------------------
*   LOCATD - binary search thru ARRAY to find OBJECT. ARRAY is assumed
*            to be sorted prior to call. If a match is found, function
*            returns position of element. If no match is found, function
*            gives negative of nearest element smaller than object.
*   Author:  F. James, double precision version.
*   (Last changed on 17/10/95.)
*-----------------------------------------------------------------------
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       DOUBLE PRECISION ARRAY(*)
       INTEGER LENGTH,NABOVE,NBELOW,MIDDLE
       NABOVE = LENGTH + 1
       NBELOW = 0
   10  IF (NABOVE-NBELOW .LE. 1)  GO TO 200
       MIDDLE = (NABOVE+NBELOW) / 2
       IF (OBJECT - ARRAY(MIDDLE))  100, 180, 140
  100  NABOVE = MIDDLE
       GO TO 10
  140  NBELOW = MIDDLE
       GO TO 10
  180  LOCATD = MIDDLE
       GO TO 300
  200  LOCATD = -NBELOW
  300  RETURN
       END
