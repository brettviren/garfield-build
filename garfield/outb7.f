CDECK  ID>, OUTB7.
       SUBROUTINE OUTB7(VBOL,XBOL,YBOL,WBOL,DBOL,OBOL,ABOL,BBOL,SBOL)
*-----------------------------------------------------------------------
*   OUTB7  - Extracts the results from Magboltz.
*   (Last changed on 22/ 9/05.)
*-----------------------------------------------------------------------
       implicit none
       DOUBLE PRECISION ALPHA,ATT
       COMMON /CTOWNS/ ALPHA,ATT
       DOUBLE PRECISION ALPER,ATTER
       COMMON /CTWNER/ ALPER,ATTER
       DOUBLE PRECISION DXXER,DYYER,DZZER,DYZER,DXYER,DXZER
       COMMON /DIFERB/ DXXER,DYYER,DZZER,DYZER,DXYER,DXZER
       DOUBLE PRECISION DFLER,DFTER
       COMMON /DIFERL/ DFLER,DFTER
       DOUBLE PRECISION DIFXX,DIFYY,DIFZZ,DIFYZ,DIFXY,DIFXZ
       COMMON /DIFLAB/ DIFXX,DIFYY,DIFZZ,DIFYZ,DIFXY,DIFXZ
       DOUBLE PRECISION DIFLN,DIFTR
       COMMON /DIFVEL/ DIFLN,DIFTR
       DOUBLE PRECISION WX,WY,WZ
       COMMON /VEL/ WX,WY,WZ
       DOUBLE PRECISION DWX,DWY,DWZ
       COMMON /VELERR/ DWX,DWY,DWZ
       INTEGER NGAS,NSTEP,IDBG
       DOUBLE PRECISION EFINAL,ESTEP,AKT,ARY,TEMPC,TORR
       PARAMETER(ARY=13.60569172)
       COMMON/INPT/NGAS,NSTEP,EFINAL,ESTEP,AKT,TEMPC,TORR,IDBG
       REAL VBOL,XBOL,YBOL,WBOL,DBOL,OBOL,ABOL,BBOL,SBOL(6)
*** Velocity.
       VBOL=MAX(0.0,REAL(WZ*1D-6))
       XBOL=REAL(WX*1D-6)
       YBOL=REAL(WY*1D-6)
*** Lorentz angle.
       WBOL=REAL(ATAN2(SQRT(WX**2+WY**2),WZ))
*** Diffusion: longitudinal,
       DBOL=SQRT(2D-6*DIFZZ*TORR/VBOL)
*   transverse,
       OBOL=SQRT(2D-6*0.5*(DIFXX+DIFYY)*TORR/VBOL)
*   tensor.
       SBOL(1)=2D-6*DIFZZ*TORR/VBOL
       SBOL(2)=2D-6*DIFXX*TORR/VBOL
       SBOL(3)=2D-6*DIFYY*TORR/VBOL
       SBOL(4)=2D-6*DIFXZ*TORR/VBOL
       SBOL(5)=2D-6*DIFYZ*TORR/VBOL
       SBOL(6)=2D-6*DIFXY*TORR/VBOL
*** Townsend coefficient.
       IF(ALPHA.GT.0)THEN
            ABOL=REAL(LOG(ALPHA/TORR))
       ELSE
            ABOL=-30
       ENDIF
*** Attachment coefficient.
       IF(ATT.GT.0)THEN
            BBOL=REAL(LOG(ATT/TORR))
       ELSE
            BBOL=-30
       ENDIF
       END