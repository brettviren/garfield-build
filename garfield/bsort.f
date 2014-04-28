CDECK  ID>, BSORT.
       SUBROUTINE BSORT(A,NR,COMPGT)
*-----------------------------------------------------------------------
*   BSORT  - Bubble sort using function COMPGT for comparisons.
*   Variables:
*   (Last changed on 20/ 1/97.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER NR,A(NR),AUX,I,J,INEW,NSWAP,NCOMP
       LOGICAL COMPGT
       EXTERNAL COMPGT
*** Counters.
       NCOMP=0
       NSWAP=0
C      print *,' Initial ',(A(I),I=1,NR)
C      do i=1,nr
C      print *,(compgt(a(i),a(j)),j=1,nr)
C      enddo
*** Loop over element to be put into place.
       DO 10 I=NR-1,1,-1
       CALL PROSTA(1,REAL(NR-I))
*** Find its proper place.
       INEW=I
       DO 20 J=I+1,NR
       IF(COMPGT(A(I),A(J)))INEW=J
       NCOMP=NCOMP+1
20     CONTINUE
*** Move it into that place.
       IF(INEW.NE.I)THEN
*   Check the sort.
            DO 50 J=I+1,INEW
            IF(COMPGT(A(J),A(I)))THEN
                  PRINT *,' !!!!!! BSORT  WARNING : Data not sortable'//
     -                 ' use the SPLIT-INTERSECTING-PLANES option.'
                  RETURN
            ENDIF
50          CONTINUE
*   Exchange.
C      print *,' Exchanging ',A(I),' and ',A(INEW)
            AUX=A(I)
            DO 30 J=I+1,INEW
            A(J-1)=A(J)
30          CONTINUE
            A(INEW)=AUX
            NSWAP=NSWAP+1
       ENDIF
10     CONTINUE
C      print *,' Final  ',(A(I),I=1,NR)
*** Statistics.
C      print *,' Comparisons: ',ncomp,', Swaps: ',nswap
       END
