CDECK  ID>, DIPFIT.
       SUBROUTINE DIPFIT(ANGLE,VOLT,N,AMPDIP,PHIDIP,IFAIL)
*-----------------------------------------------------------------------
*   DIPFIT - Determines the dipole moment of a wire.
*   (Last changed on 26/10/07.)
*-----------------------------------------------------------------------
       implicit none
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,MEV2KG,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      MEV2KG = 1.782661845E-30,
     -      BOLTZ=1.380658E-23)
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER IFAIL,N,NTRY,ITRY,I,J,NITMAX
       PARAMETER(NTRY=100)
       REAL ANGLE(N),VOLT(N),AMPDIP,PHIDIP,X1,X2,X3,F1,F2,F3,
     -      XPARA,FPARA,PHITRY(NTRY),SUMTRY(NTRY),PHIMAX,SUMMAX,
     -      EPS,EPSF,EPSX,DET
*** Initial values.
       PHIDIP=0.0
       AMPDIP=0.0
       IFAIL=1
*** Initial search for a maximum
       SUMMAX=0
       DO 10 ITRY=1,NTRY
*   Make the internal product with a shifted cosine
       PHITRY(ITRY)=REAL(ITRY-1)*2.0*PI/REAL(NTRY)
       SUMTRY(ITRY)=0
       DO 20 J=1,N
       SUMTRY(ITRY)=SUMTRY(ITRY)+VOLT(J)*COS(PHITRY(ITRY)-ANGLE(J))
20     CONTINUE
       SUMTRY(ITRY)=SUMTRY(ITRY)*2.0/REAL(N)
*   See whether this one beats earlier
       IF(SUMTRY(ITRY).GT.SUMMAX)THEN
            PHIMAX=PHITRY(ITRY)
            SUMMAX=SUMTRY(ITRY)
       ENDIF
10     CONTINUE
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ DIPFIT DEBUG   :'',
     -           '' Maximum of scan at phi = '',E12.5,
     -           '', product = '',E12.5)') PHIMAX,SUMMAX
            CALL GRGRPH(PHITRY,SUMTRY,NTRY,'Angle [radians]',
     -           'Cos projection','Search of maximum')
            CALL GRNEXT
       ENDIF
       PHIDIP=PHIMAX
       AMPDIP=SUMMAX
*** Scan in the neighbourbood
       EPS=0.1
       X1=PHIMAX-EPS
       X2=PHIMAX
       X3=PHIMAX+EPS
       F1=0
       F2=SUMMAX
       F3=0
       DO 30 J=1,N
       F1=F1+VOLT(J)*COS(X1-ANGLE(J))
       F3=F3+VOLT(J)*COS(X3-ANGLE(J))
30     CONTINUE
       F1=F1*2.0/REAL(N)
       F3=F3*2.0/REAL(N)
*** Refine the estimate by parabolic extremum search.
       NITMAX=10
       EPSF=1E-3*SUMMAX
       EPSX=1E-3*2*PI
       DO 40 I=1,NITMAX
*   Estimate parabolic extremum.
       DET=(F1-F2)*X3 + (F3-F1)*X2 + (F2-F3)*X1
       IF(ABS(DET).LE.0)THEN
            PRINT *,' ++++++ DIPFIT WARNING : Determinant = 0;'//
     -           ' parabolic search stopped.'
            PHIDIP=X2
            AMPDIP=F2
            IFAIL=1
            RETURN
       ENDIF
       XPARA=((F1-F2)*X3**2+(F3-F1)*X2**2+(F2-F3)*X1**2)/(2*DET)
       FPARA=0
       DO 50 J=1,N
       FPARA=FPARA+VOLT(J)*COS(XPARA-ANGLE(J))
50     CONTINUE
       FPARA=FPARA*2.0/REAL(N)
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DIPFIT DEBUG   :'',
     -      '' Start of iteration '',I3//
     -      26X,''Point 1:  x='',E15.8,'' f='',E15.8/
     -      26X,''Point 2:  x='',E15.8,'' f='',E15.8/
     -      26X,''Point 3:  x='',E15.8,'' f='',E15.8//
     -      26X,''Parabola: x='',E15.8,'' f='',E15.8)')
     -      I,X1,F1,X2,F2,X3,F3,XPARA,FPARA
*   Check that the new estimate doesn't coincide with an old point.
       IF(ABS(XPARA-X1).LT.EPSX*(EPSX+ABS(XPARA)).OR.
     -      ABS(XPARA-X2).LT.EPSX*(EPSX+ABS(XPARA)).OR.
     -      ABS(XPARA-X3).LT.EPSX*(EPSX+ABS(XPARA)))THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Location convergence'',
     -           '' criterion satisfied.''/)')
            PHIDIP=XPARA
            AMPDIP=FPARA
            IFAIL=0
            RETURN
       ENDIF
*   Check convergence.
       IF(ABS(FPARA-F1).LT.EPSF*(ABS(FPARA)+ABS(F1)+EPSF))THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Function value convergence'',
     -           '' criterion satisfied.''/)')
            PHIDIP=XPARA
            AMPDIP=FPARA
            IFAIL=0
            RETURN
       ENDIF
*   Store the value in the table.
       IF(FPARA.GT.F1)THEN
            F3=F2
            X3=X2
            F2=F1
            X2=X1
            F1=FPARA
            X1=XPARA
       ELSEIF(FPARA.GT.F2)THEN
            F3=F2
            X3=X2
            F2=FPARA
            X2=XPARA
       ELSEIF(FPARA.GT.F3)THEN
            F3=FPARA
            X3=XPARA
       ELSE
            PRINT *,' !!!!!! DIPFIT WARNING : Parabolic extremum'//
     -           ' is worse than current optimum; search stopped.'
            WRITE(*,'(
     -      26X,''Point 1:  x='',E15.8,'' f='',E15.8/
     -      26X,''Point 2:  x='',E15.8,'' f='',E15.8/
     -      26X,''Point 3:  x='',E15.8,'' f='',E15.8//
     -      26X,''Parabola: x='',E15.8,'' f='',E15.8)')
     -      X1,F1,X2,F2,X3,F3,XPARA,FPARA
            PHIDIP=X2
            AMPDIP=F2
            IFAIL=1
            RETURN
       ENDIF
40     CONTINUE
*** No convergence.
       PRINT *,' !!!!!! DIPFIT WARNING : No convergence after maximum'//
     -      ' number of steps.'
       PRINT *,'                         Current extremum f=',F2
       PRINT *,'                         Found for        x=',X2
       PHIDIP=X2
       AMPDIP=F2
       IFAIL=1
       END
