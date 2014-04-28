CDECK  ID>, INTEXC.
       SUBROUTINE INTEXC(A1,A2,S1,S2,STEP,N1,SINT)
*-----------------------------------------------------------------------
*   INTEXC - Integrates excitation rates over a step over an
*            underlying avalanche. Uses a set of approximations
*            to evaluate the analytic expressions that assume a
*            linear dependence of alpha and rate.
*            All expressions computed with Mathematica.
*   (Last changed on 24/ 8/09.)
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
       DOUBLE PRECISION A1,A2,DA,S1,S2,DS,N1,STEP,ERFIR,ERF,SINT
       EXTERNAL ERFIR
       INTRINSIC ERF,ERFC
*** Protect against incorrect arguments and overflow
       IF(STEP.EQ.0)THEN
            SINT=0
            RETURN
       ELSEIF(A1.LT.0.OR.A2.LT.0.OR.S1.LT.0.OR.S2.LT.0.OR.STEP.LT.0)THEN
            PRINT *,' !!!!!! INTEXC WARNING : Incorrect arguments'//
     -           ' received; no evaluation.'
            SINT=0
            RETURN
       ELSEIF(STEP*MAX(A1,A2).GT.40.0)THEN
            PRINT *,' !!!!!! INTEXC WARNING : Can not integrate'//
     -           ' without overflow; no evaluation.'
            SINT=0
            RETURN
       ENDIF
*** Compute steps.
       DA=(A2-A1)/STEP
       DS=(S2-S1)/STEP
*** Various formulae: increasing alpha using reduced Erfi formulae.
       IF(DA.GT.0)THEN
C            print *,' da > 0'
            SINT=N1*(EXP(A1*STEP+DA*STEP**2/2.0)-1.0)*DS/DA +
     -           N1*SQRT(PI/2.0)*(A1*DS-S1*DA)*(
     -           ERFIR(A1/SQRT(2.0*DA))-
     -           EXP(A1*STEP+DA*STEP**2/2.0)*
     -           ERFIR((A1+DA*STEP)/SQRT(2.0*DA)))/SQRT(DA)**3
*   Constant alpha without gain.
       ELSEIF(A1.EQ.0.AND.DA.EQ.0)THEN
C            print *,' a = 0, da = 0'
            SINT=N1*STEP*(S1+DS*STEP/2.0)
       ELSEIF(DA.EQ.0)THEN
*   Constant alpha and gain.
C            print *,' a > 0, da = 0'
            SINT=N1*(DS-A1*S1+EXP(A1*STEP)*(A1*S1+DS*(A1*STEP-1.0)))/
     -           A1**2
*   Tiny alpha decrease, using reduced Erfc asymptotic approximation.
       ELSEIF(A1/SQRT(-2.0*DA).GT.8.OR.
     -      (A1+DA*STEP)/SQRT(-2.0*DA).GT.8)THEN
C            print *,' very small da < 0: ',a1/sqrt(-2.0*da),
C     -           (a1+da*step)/sqrt(-2.0*da)
            SINT=N1*(EXP(A1*STEP+DA*STEP**2/2.0)-1.0)*DS/DA +
     -           N1*SQRT(2.0)*(A1*DS-S1*DA)*
     -           (EXP(A1*STEP+DA*STEP**2/2.0)/
     -              ((A1+DA*STEP)/SQRT(-2.0*DA)+
     -              SQRT((A1+DA*STEP)**2/(-2.0*DA)+1.6))-
     -           1.0/(A1/SQRT(-2.0*DA)+SQRT(A1**2/(-2.0*DA)+1.6)))/
     -           SQRT(-DA)**3
*   Small alpha decrease, using Erfc regular expression.
       ELSEIF(A1/SQRT(-2.0*DA).GT.3.OR.
     -      (A1+DA*STEP)/SQRT(-2.0*DA).GT.3)THEN
C            print *,' small da < 0: ',a1/sqrt(-2.0*da),
C     -           (a1+da*step)/sqrt(-2.0*da)
            SINT=N1*(EXP(A1*STEP+DA*STEP**2/2.0)-1.0)*DS/DA +
     -           N1*SQRT(PI/2.0)*(A1*DS-S1*DA)*EXP(-A1**2/(2*DA))*
     -           (ERFC((A1+DA*STEP)/SQRT(-2.0*DA))-
     -           ERFC(A1/SQRT(-2.0*DA)))/SQRT(-DA)**3
*   Other alpha decrease, using Erf expressions.
       ELSE
C            print *,' da < 0'
            SINT=N1*(EXP(A1*STEP+DA*STEP**2/2.0)-1.0)*DS/DA +
     -           N1*SQRT(PI/2.0)*(A1*DS-S1*DA)*EXP(-A1**2/(2*DA))*
     -           (ERF(A1/SQRT(-2.0*DA))-
     -           ERF((A1+DA*STEP)/SQRT(-2.0*DA)))/SQRT(-DA)**3
       ENDIF
C      print *,' alpha: ',A1,A2,', exc/v: ',S1,S2,', step: ',STEP,
C     -     ', n: ',N1,', sint: ',sint
       END
