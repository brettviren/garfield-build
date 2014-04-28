CDECK  ID>, ANGCUT.
      SUBROUTINE ANGCUT(PSCT1,ANGC,PSCT2)
*-----------------------------------------------------------------------
*   ANGCUT - Set angle cuts on angular distribution and renormalise
*            forward scattering probability
*   (Last changed on 22/ 5/05.)
*-----------------------------------------------------------------------
       implicit none
       DOUBLE PRECISION ANGC,PSCT1,PSCT2,API,RADS,CNS,THETAC,FAC
       ANGC=1.0D0
       PSCT2=PSCT1
       IF(PSCT1.LE.1.0D0) RETURN
       API=ACOS(-1.0D0)
       RADS=2.0D0/API
       CNS=PSCT1-0.5D0
       THETAC=ASIN(2.0D0*SQRT(CNS-CNS*CNS))
       FAC=(1.0D0-COS(THETAC))/(SIN(THETAC)*SIN(THETAC))
       PSCT2=(CNS*FAC)+0.5D0
       ANGC=THETAC*RADS
       END
