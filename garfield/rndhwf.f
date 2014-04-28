CDECK  ID>, RNDHWF.
       SUBROUTINE RNDHWF(W,F,RND)
*-----------------------------------------------------------------------
*   RNDHWF - Generates random energies needed to create a single e- in
*            a gas with asymptotic work function W and Fano factor F,
*            according to Igor Smirnov's phenomenological model.
*   (Last changed on 27/ 6/07.)
*-----------------------------------------------------------------------
       implicit none
       REAL W,F,RND,RNDUNI,WREF,FREF,X,E
       PARAMETER(WREF=30.0, FREF=0.174)
       EXTERNAL RNDUNI
*** Check parameters
       IF(W.LE.0.OR.F.LT.0)THEN
            PRINT *,' !!!!!! RNDHWF WARNING : Work and/or Fano'//
     -           ' parameter out of range; returning 0.'
            RND=0
            RETURN
*   Special case of F=0
       ELSEIF(F.EQ.0)THEN
            RND=W
            RETURN
       ENDIF
*** First generate a standardised (W=30, F=0.174) random energy
       X=RNDUNI(1.0)*WREF*0.82174
*   E = 0 to w/2:     p = 0,       integral = 0
       IF(X.LT.0)THEN
            PRINT *,' !!!!!! RNDHWF WARNING : Random number is below'//
     -           ' the applicable range - program error; returning w/2.'
            E=WREF/2
*   E = w/2 to w:     p = 1,       integral = E-w/2
       ELSEIF(X.LT.WREF/2)THEN
            E=WREF/2+X
*   E = w to 3.064 w: p = (w/E)^4, integral = w^4/3 (1/E^3 - 1/w^3)
       ELSEIF(X.LT.WREF*0.82174)THEN
            E=(2*WREF**4/(5*WREF-6*X))**(1.0/3.0)
*   E > 3.064 w:      p = 0,       integral = 0
       ELSE
            PRINT *,' !!!!!! RNDHWF WARNING : Random number is above'//
     -           ' applicable range - program error; returning 3.064 w.'
            E=3.064*WREF
       ENDIF
*** Scale.
       RND=(W/WREF)*SQRT(F/FREF)*E+W*(1-SQRT(F/FREF))
       END
