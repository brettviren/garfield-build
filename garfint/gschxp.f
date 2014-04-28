CDECK  ID>, GSCHXP.
       SUBROUTINE GSCHXP(SZSF)
*-----------------------------------------------------------------------
*   GSCHXP - Set character expansion factor, imitated in HIGZ.
*   (Last changed on 30/ 6/95.)
*-----------------------------------------------------------------------
       REAL SZSF
       COMMON /CHXP/CHXP0
       IF(SZSF.LT.0.)SZSF=1.0
       CHXP0=SZSF
       CALL IGQ('CHHE',HEIT)
       CHH = HEIT*SZSF
       CALL ISCHH(CHH)
       END
