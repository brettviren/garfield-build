CDECK  ID>, DATTIMO.
       SUBROUTINE DATTIM(DAT,TIM)
*-----------------------------------------------------------------------
*   DATTIM - Interface to DATIMH for non-Vax computers.
*   (Last changed on 30/ 8/98.)
*-----------------------------------------------------------------------
       implicit none
       CHARACTER*8 DAT,TIM
       CALL DATIMH(DAT,TIM)
       END
