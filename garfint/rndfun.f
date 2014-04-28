CDECK  ID>, RNDFUN.
       REAL FUNCTION RNDFUN(ARG)
*-----------------------------------------------------------------------
*   RNDFUN - Generates random numbers according to a function, uses the
*            V152 routines.
*   (Last changed on 30/ 8/99.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER IENTRY
       REAL ARG,CUMRNF(200),XRAN(1)
       LOGICAL FUNSET
       COMMON /RNDFCM/ IENTRY,FUNSET,CUMRNF
*** Verify that the function has been prepared.
       IF(.NOT.FUNSET)THEN
            PRINT *,' !!!!!! RNDFUN WARNING : Before using'//
     -           ' RND_FUNCTION, you must call PREPARE_RND_FUNCTION;'//
     -           ' no random number'
            RNDFUN=0
            RETURN
       ENDIF
*** Generate a random number.
       CALL FUGLUX(CUMRNF,XRAN,1)
       RNDFUN=XRAN(1)
       END
