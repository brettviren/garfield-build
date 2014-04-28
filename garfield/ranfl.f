CDECK  ID>, RANFL.
       real function ranfl()
*-----------------------------------------------------------------------
*   RANFL  - Random number generator. Originally based on CERNLIB
*            routine RANF G900, modified to rely on
*   (Last changed on 15/ 6/06.)
*-----------------------------------------------------------------------
       implicit none
C      real ranfl,ranf,x
       INTEGER LEN
       PARAMETER(LEN=1)
       REAL XRAN(LEN)
        real*8 iranfl

        integer sseed              ! Flag to start first event
                                   ! from seed point of random number generator.
        real*8 rseed               ! Place for seed.
        integer seed(2)            ! Form for writting and inputting
                                   ! without modification during
                                   ! binary to demical transformation.
        equivalence (rseed,seed(1))

        common / comran /
     +  iranfl,
     +  rseed, sseed

        save / comran /
	
*** Counter of number of calls.
C       iranfl=iranfl+3
*** Several preliminary calls to avoid correlations. (RV 15/6/2006)
C       x=ranf()
C       x=ranf()
C       ranfl=ranf()
*** Call RANLUX instead of RANF
       IRANFL=IRANFL+1
       CALL RANLUX(XRAN,LEN)
       RANFL=XRAN(1)
       end
