CDECK  ID>, RAIN.
       SUBROUTINE RAIN(WL,R,G,B)
*-----------------------------------------------------------------------
*   RAIN   - RGB values for visible wavelengths
*            The spectrum is generated using approximate RGB values for
*            visible wavelengths between 380 nm and 780 nm. The red,
*            green and blue values (RGB) are assumed to vary linearly
*            with wavelength (for GAMMA=1).
*   Author:  Dan Bruton (astro@tamu.edu)
*   Source:  http://www.physics.sfasu.edu/astro/color/spectra.html
*            Original last updated on February 20, 1996,
*   (Last changed on 11/11/02)
*-----------------------------------------------------------------------
       implicit none
       REAL WL,R,G,B,GAMMA,SSS
*** Gamma setting.
       GAMMA=0.40
*** Parametrisations for various wave lengths.
       IF(WL.GE.380.AND.WL.LE.440)THEN
            R = (440-WL)/60
            G = 0
            B = 1
       ELSEIF(WL.GE.440.AND.WL.LE.490)THEN
            R = 0
            G = (WL-440)/50
            B = 1
       ELSEIF(WL.GE.490.AND.WL.LE.510)THEN
            R = 0
            G = 1
            B = (510-WL)/20
       ELSEIF(WL.GE.510.AND.WL.LE.580)THEN
            R = (WL-510)/70
            G = 1
            B = 0
       ELSEIF(WL.GE.580.AND.WL.LE.645)THEN
            R = 1
            G = (645-WL)/65
            B = 0
       ELSEIF(WL.GE.645.AND.WL.LE.780)THEN
            R = 1
            G = 0
            B = 0
       ELSEIF(WL.LT.380.OR.WL.GT.780)THEN
            PRINT *,' !!!!!! RAIN   WARNING : Wave length outside the'//
     -           ' visible range; set to (0,0,0).'
            R = 0
            G = 0
            B = 0
            RETURN
       ENDIF
*** Let the intensity sss fall off near the vision limits.
       IF(WL.GT.700)THEN
            SSS=0.3+0.7*(780-WL)/80
       ELSEIF(WL.LT.420)THEN
            SSS=0.3+0.7*(WL-380)/40
       ELSE
            SSS=1
       ENDIF
*** Smoothen.
       SSS=SQRT(SSS)
*** Gamma adjust.
       R=(SSS*R)**GAMMA
       G=(SSS*G)**GAMMA
       B=(SSS*B)**GAMMA
       END
