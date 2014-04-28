CDECK  ID>, GSTXP.
       SUBROUTINE GSTXP(IRL)
*-----------------------------------------------------------------------
*   GSTXP  - Set the text path, limited availability in HIGZ.
*   (Last changed on 30/ 6/95.)
*-----------------------------------------------------------------------
       IF(IRL.LT.0 .OR. IRL.GT.3)IRL=0
       IF(IRL.EQ.0)CALL ISTXAL(0,0)
       IF(IRL.EQ.1)CALL ISTXAL(3,0)
       IF(IRL.EQ.2)CALL ISTXAL(0,1)
       IF(IRL.EQ.3)CALL ISTXAL(0,3)
       END
