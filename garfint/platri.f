CDECK  ID>, PLATRI.
       SUBROUTINE PLATRI(NPL,XPL,YPL,ZPL,NU,XU,YU,ZU,FMIN,FMAX)
*-----------------------------------------------------------------------
*   PLATRI - Selects the part of a rectangle inside function values.
*   (Last changed on 12/11/02)
*-----------------------------------------------------------------------
       implicit none
       INTEGER NPL,NU,I,J
       DOUBLE PRECISION XPL(*),YPL(*),ZPL(*),FMIN,FMAX,XU(*),YU(*),ZU(*)
*** Start a new triangle.
       NU=0
*** See whether we leave the range over the line 1-2.
       DO 10 I=1,4
       J=I+1
       IF(J.GT.4)J=J-4
       IF((ZPL(I)-FMIN)*(FMIN-ZPL(J)).GE.0.AND.ZPL(J).NE.ZPL(I))THEN
            NU=NU+1
            XU(NU)=XPL(I)+(XPL(J)-XPL(I))*(FMIN-ZPL(I))/(ZPL(J)-ZPL(I))
            YU(NU)=YPL(I)+(YPL(J)-YPL(I))*(FMIN-ZPL(I))/(ZPL(J)-ZPL(I))
            ZU(NU)=FMIN
            IF(((I.EQ.1.OR.I.EQ.3).AND.
     -           (ABS(XPL(I)-XU(NU)).LT.1E-3*ABS(XPL(I)-XPL(J)).OR.
     -            ABS(XPL(J)-XU(NU)).LT.1E-3*ABS(XPL(I)-XPL(J)))).OR.
     -         ((I.EQ.2.OR.I.EQ.4).AND.
     -           (ABS(YPL(I)-YU(NU)).LT.1E-3*ABS(YPL(I)-YPL(J)).OR.
     -            ABS(YPL(J)-YU(NU)).LT.1E-3*ABS(YPL(I)-YPL(J)))))THEN
                 NU=NU-1
            ENDIF
       ENDIF
       IF((ZPL(I)-FMAX)*(FMAX-ZPL(J)).GE.0.AND.ZPL(J).NE.ZPL(I))THEN
            NU=NU+1
            XU(NU)=XPL(I)+(XPL(J)-XPL(I))*(FMAX-ZPL(I))/(ZPL(J)-ZPL(I))
            YU(NU)=YPL(I)+(YPL(J)-YPL(I))*(FMAX-ZPL(I))/(ZPL(J)-ZPL(I))
            ZU(NU)=FMAX
            IF(((I.EQ.1.OR.I.EQ.3).AND.
     -           (ABS(XPL(I)-XU(NU)).LT.1E-3*ABS(XPL(I)-XPL(J)).OR.
     -            ABS(XPL(J)-XU(NU)).LT.1E-3*ABS(XPL(I)-XPL(J)))).OR.
     -         ((I.EQ.2.OR.I.EQ.4).AND.
     -           (ABS(YPL(I)-YU(NU)).LT.1E-3*ABS(YPL(I)-YPL(J)).OR.
     -            ABS(YPL(J)-YU(NU)).LT.1E-3*ABS(YPL(I)-YPL(J)))))THEN
                 NU=NU-1
            ENDIF
       ENDIF
       IF((FMIN-ZPL(I))*(ZPL(I)-FMAX).GE.0)THEN
            NU=NU+1
            XU(NU)=XPL(I)
            YU(NU)=YPL(I)
            ZU(NU)=ZPL(I)
       ENDIF
10     CONTINUE
*** Eliminate butterflies.
       CALL BUTFLD(NU,XU,YU,ZU)
       END
