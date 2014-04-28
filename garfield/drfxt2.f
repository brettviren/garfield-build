CDECK  ID>, DRFXT2.
       SUBROUTINE DRFXT2(U1,V1,U2,V2,U3,V3,UMIN,VMIN,IFAIL,IFLAG)
*-----------------------------------------------------------------------
*   DRFXT2 - Determines the minimum of a parabola, the expressions have
*            been calculated using the Macsyma system.
*   VARIABLES : IFAIL       : 1 if the parabola is degenerate.
*               IFLAG       : -1 maximum, 0 failure, +1 minimum.
*-----------------------------------------------------------------------
       DOUBLE PRECISION X1,Y1,X2,Y2,X3,Y3,DIV1,DIV2,XMIN,YMIN
       IFAIL=1
       IFLAG=0
       UMIN=0
       VMIN=0
*** Make a double precision copy
       X1=U1
       Y1=V1
       X2=U2
       Y2=V2
       X3=U3
       Y3=V3
*** Prevent divisions by zero.
       DIV1=2*(X1*(Y3-Y2)+X2*(Y1-Y3)+X3*(Y2-Y1))
       DIV2=X1**2*(X2-X3)+X2**2*(X3-X1)+X3**2*(X1-X2)
       IF(DIV1.EQ.0.OR.DIV2.EQ.0)RETURN
       XMIN=(X1**2*(Y3-Y2)+X2**2*(Y1-Y3)+X3**2*(Y2-Y1))/DIV1
       YMIN=(X1**2*(X2*Y3-X3*Y2)+X2**2*(X3*Y1-X1*Y3)+
     -       X3**2*(X1*Y2-X2*Y1)-DIV1*XMIN**2/2)/DIV2
       IFAIL=0
*** See whether it is a maximum or a minimum.
       IF(DIV1/DIV2.GT.0)THEN
            IFLAG=+1
       ELSE
            IFLAG=-1
       ENDIF
*** Make a single precision copy.
       UMIN=XMIN
       VMIN=YMIN
       END
