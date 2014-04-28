CDECK  ID>, LSTREL1.
      SUBROUTINE lstREL1(EEL,CHARGE,nmat,DEDX)
C.
	implicit none

C.    ******************************************************************
C.    *                                                                *
C.    *       Compute ion losses for electron/positron                 *
C.    *                                                                *
C.    *    ==>Called by : GDRELA                                       *
C.    *       Author    G.Patrick *********                            *
C.    *                                                                *
C.    ******************************************************************
C.
	real EEL	! kinetic energy
	real CHARGE	! +/-1.
c	integer JMA	! =LQ(JMATE-I)  I-number of medium
	integer nmat	! number of matter	
	real DEDX	! loss

c 	include 'ener.inc'
c	Energy mesh

      integer pqener,qener	! Max. quantity and quantity of bins.
				! Quantity must not be more than pqener - 1.
      PARAMETER (pqener=501)
      real ener,enerc		! The left edges and the centers
				! of the energy intervals.
				! ener(qener+1) is the right edge
				! of the last interval.
C
      COMMON / coEner /
     +       qener, ener(pqener), enerc(pqener)
	save / coEner /
c 	include 'atoms.inc'


	integer pQAt		! Max. quantity of atoms.
	parameter (pQAt=19)
	integer KeyTeor		! Key to use only theor. photo-absorbtion
				! cross section with thresholds and
				! weights from the subroutine shteor.
				! If 0 then they are used only for
				! the atoms which are absent
				! in the subroutine readPas and
				! in the subroutine shellfi.
	integer Zat		! Atomic number (charge of atomic nucleus).
	real Aat		! Atomic weight.
	integer pQShellAt	! Max. quantity of atomic shells.
	parameter (pQShellAt=17)
	integer QShellAt	! Quantity of atomic shells.
	real cphoAt		! Integral of photo-absorbtion
				! cross secton for one atom.
	real	ThresholdAt	! Threshold and
	real 	WeightShAt 	! Weight of atomic shells for the
				! photo-absorbtion cross secton
				! relatively cphoAt.
	real PWeightShAt	! Initial integral of
				! photo-absorbtion cross secton.
	real  PhotAt		! Photo-absorbtion cross secton.
	real  PhotIonAt		! Photo-ionization cross secton.
c	The physical definition of two previous arrays of values:
c	mean values of cross sections for each energy interval.
	real  RLenAt		! Radiation lengt*density for dens=1
	real  RuthAt		! Const for Rutherford cross cection
				! (dimensionless).
c	integer num_at_mol	! Number for atoms in several special
c				! molecules, now obsolete.
	real ISPhotBAt		! Shell integral of cs before normalization
	real IAPhotBAt 		! Atomic integral of cs before normalization
	real ISPhotAt		! Shell integral of cs
	real IAPhotAt		! Atomic integral of cs
	real ISPhotIonAt	! Shell integral of cs
	real IAPhotIonAt	! Atomic integral of cs
	real MinThresholdAt	! Minimal ionization potential of atom.
	integer NshMinThresholdAt ! Number of shell with minimal energy,
			! it must be the last shell ( see AbsGam.f)
	integer Min_ind_E_At, Max_ind_E_At ! Indexes of energy intervals
			! where program adds excitation to cs
			! They placed in common only to print and check.
	integer nseqAt	! Sequensed pointer in order of increasing Zat
			! atom number nsAt(1) is least charged.
	integer QseqAt	! Quantity of initialized atoms

	common / catoms /
     +		KeyTeor,
     +		Zat(pQAt), Aat(pQAt),	
     +		QShellAt(pQAt), cphoAt(pQAt),
     +		ThresholdAt(pQShellAt,pQAt), WeightShAt(pQShellAt,pQAt),
     +		PWeightShAt(pQShellAt,pQAt),
     +		PhotAt(pqener,pQShellAt,pQAt),
     +		PhotIonAt(pqener,pQShellAt,pQAt),
     +		ISPhotBAt(pQShellAt,pQAt),
     +		IAPhotBAt(pQAt),
     +		ISPhotAt(pQShellAt,pQAt),
     +		IAPhotAt(pQAt),
     +		ISPhotIonAt(pQShellAt,pQAt),
     +		IAPhotIonAt(pQAt),
     +		MinThresholdAt(pQAt),
     +		NshMinThresholdAt(pQAt),
     +		Min_ind_E_At(pQAt), Max_ind_E_At(pQAt),
     +		RLenAt(pQAt),
     +		RuthAt(pQAt),
     +		nseqAt(pQAt),
     +		QseqAt
	save / catoms /
c 	include 'matters.inc'
	integer pQMat		! Max. quantity of matters.
	parameter (pQMat=10)
	integer QAtMat		! Quantity of atoms in matter.
	integer AtMAt		! Number of atom in matter
				! (the pointer to atoms.inc).
	real WeightAtMat	! Weight of atom in matter.
	real A_Mean		! Average A.
	real Z_Mean		! Average Z.
	real DensMat		! Density (g/cm3).
	real DensMatDL		! Density (g/cm3) for energy loss of deltaelect.
	real DensMatDS		! Density (g/cm3) for mult. scat. of deltaelect.
	real ElDensMat		! Electron density(MeV3).
	real XElDensMat		! Longitud. Electron Dens. for x=1cm(MeV2/cm)
	real wplaMat		! Plasm frequancy.
	real RLenMat		! Radiation Lengt.
	real RuthMat		! Const for Rutherford cross section (1/cm3).
	real PhotMat		! Photoabsirbtion cross section per one atom.
	real PhotIonMat		! Photoionization cross section per one atom.
	real epsip		! plasm dielectric constant.
	real epsi1		! real part of dielectric constant.
	real epsi2		! imaginary part of dielectric constant.
	real min_ioniz_pot	! Minimum ionization potential,
				! it is using only for switching off
				! the Cherenkov radiation below it.
	real Atm_Pressure	! Standart atmosferic pressure.
	parameter (Atm_Pressure=760.0)
	real Cur_Pressure	! Current pressure for initialized medium.
				! During gas initialization
				! the subroutine gasdens uses it for
				! calculating of density.
	real Pressure		! Pressure for given medium.	
	real Atm_Temper		! Standart atmosferic temperature.
	parameter (Atm_Temper=293.0)
	real Cur_Temper		! Current temperature for initialized medium.
				! During gas initialization
				! the subroutine gasdens uses it for
				! calculating of density.
	real Temper		! Temperature for given medium.	
	real WWW		! The mean work per pair production.
	real FFF		! Fano parameter.
	common / cmatte /
     +		QAtMat(pQMat),
     +		AtMat(pQAt,pQMat),
     +		WeightAtMat(pQAt,pQMat),
     +		A_Mean(pQMat),Z_Mean(pQMat),
     +		DensMat(pQMat),ElDensMat(pQMat),XElDensMat(pQMat),
     +		DensMatDL(pQMat),DensMatDS(pQMat),
     +		wplaMat(pQMat),
     +		RLenMat(pQMat),
     +		RuthMat(pQMat),
     +		PhotMat(pqener,pQMat),
     +		PhotIonMat(pqener,pQMat),
     +		epsip(pqener,pQMat),
     +		epsi1(pqener,pQMat),
     +		epsi2(pqener,pQMat),
     +		min_ioniz_pot(pQMat),
     +		Cur_Pressure,Pressure(pQMat),
     +		Cur_Temper,Temper(pQMat),
     +		WWW(pQMat),FFF(pQMat)
	save / cmatte /
	
	integer nat
	
	real*8 PI,TWOPI, PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,
     +		EMMU,PMASS,AVO
      PARAMETER (PI=3.14159265358979324)
      PARAMETER (TWOPI=6.28318530717958648)
      PARAMETER (PIBY2=1.57079632679489662)
      PARAMETER (DEGRAD=0.0174532925199432958)
      PARAMETER (RADDEG=57.2957795130823209)
      PARAMETER (CLIGHT=29979245800.)
      PARAMETER (BIG=10000000000.)
      PARAMETER (EMASS=0.0005109990615)
      PARAMETER (EMMU=0.105658387)
      PARAMETER (PMASS=0.9382723128)
      PARAMETER (AVO=0.60221367)

c      PARAMETER (KWBANK=69000,KWWORK=5200)
c      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
c     +             ,LMAIN,LR1,WS(KWBANK)
c      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
c      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
c      EQUIVALENCE (JCG,JGSTAT)

c      COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
c     +             ,DCUTE,DCUTM ,PPCUTM,TOFMAX,GCUTS(5)

	real DCUTE
c+SEQ,GCBANK
c+SEQ,GCCUTS
C
	real DENS
	real GAM, GAM2, T, TCME, BET2, Y, D, D2, D3, D4, F
	real POTI, POTL, FAC, C, X0, X1, AA, X, DEL, XA
	real S1,S2
	real CON2,CON3,CON4,CON5,CON6
	real AJ,ZJ, WJ,	WJJ(pQAt)
	integer IP
	real CONS
      DATA CONS/0.153536E-3/
C.
C.    ------------------------------------------------------------------
C.
	DCUTE=1.0e5
	DENS=DensMatDL(nmat)	
c      JPROB=LQ(JMA-4)
C
      GAM=EEL/EMASS + 1.
      GAM2=GAM*GAM
      T=GAM-1.
      DEDX=0.
      IF(T.LE.0.)GO TO 99
      TCME=DCUTE/EMASS
      BET2=1.-1./GAM2
C     ------------------------------
      IF(CHARGE.GT.0.) THEN
         Y=1./(1.+GAM)
         D=TCME
         IF(T.LT.TCME) D=T
         D2=D*D/2.
         D3=2.*D2*D/3.
         D4=D2*D2
         F=LOG(T*D)-BET2*(T+2.*D-Y*(3.*D2
     *    +Y*(D-D3+Y*(D2-T*D3+D4))))/T
C
      ELSE
        D=TCME
         IF(T.LT.2.*TCME) D=0.5*T
         F=-1.-BET2+LOG((T-D)*D)+T/(T-D)
     *    +(0.5*D*D+(1.+2.*T)*LOG(1.-D/T))/GAM2
      ENDIF
C
	if(QAtMat(nmat).eq.1)then
		POTI=16.E-9*ZAt(AtMat(1,nmat))**0.9
		S1=Zat(AtMat(1,nmat))/Aat(AtMat(1,nmat))
	else
		S1=0.0
		S2=0.0
		do nat=1,QAtMat(nmat)
			AJ=Aat(AtMat(nat,nmat))
			WJJ(nat)=WeightAtMat(nat,nmat)*AJ
			S1=S1+WJJ(nat)
		enddo
		do nat=1,QAtMat(nmat)
			WJJ(nat)=WJJ(nat)/S1
		enddo
		S1=0.0
		do nat=1,QAtMat(nmat)
			ZJ=Zat(AtMat(nat,nmat))
			AJ=Aat(AtMat(nat,nmat))
			WJ=WJJ(nat)	
		        S1=S1+WJ*ZJ/AJ
		        S2=S2+WJ*ZJ*LOG(ZJ)/AJ
		enddo
	        POTI=16.E-9*EXP(0.9*S2/S1)
	endif
				
      POTL=LOG(POTI/EMASS)
      CON2=DENS*S1
      FAC=DENS*S1
      CON3=1.+2.*LOG(POTI/(28.8E-9*SQRT(CON2)))
	C= CON3
C
C             Condensed material ?
C             (at present that means: DENS.GT.0.05 g/cm**3)
C
      IF(DENS.GT.0.05)THEN
         IF(POTI.LT.1.E-7)THEN
            IF(CON3.LT.3.681)THEN
               CON4=0.2
            ELSE
               CON4=0.326*CON3-1.
            ENDIF
            CON5=2.
         ELSE
            IF(CON3.LT.5.215)THEN
               CON4=0.2
            ELSE
               CON4=0.326*CON3-1.5
            ENDIF
            CON5=3.
         ENDIF
      ELSE
C
C             Gas (T=0 C, P= 1 ATM)
C             if T.NE. 0 C and/or P.NE. 1 ATM
C             you have to modify the variable X
C             X=>X+0.5*LOG((273+T C)/(273*P ATM))
C             in the function GDRELE
C             ------------------------
C
         IF(CON3.LE.12.25)THEN
            IP=INT((CON3-10.)/0.5)+1
            IF(IP.LT.0) IP=0
            IF(IP.GT.4) IP=4
            CON4=1.6+0.1*FLOAT(IP)
            CON5=4.
         ELSE
            IF(CON3.LE.13.804)THEN
               CON4=2.
               CON5=5.
            ELSE
               CON4=0.326*CON3-2.5
               CON5=5.
            ENDIF
         ENDIF
      ENDIF
C
      XA=CON3/4.606
      CON6=4.606*(XA-CON4)/(CON5-CON4)**3.

      X0=CON4
      X1=CON5
      AA=CON6
C
      X=LOG(GAM2-1.)/4.606
      DEL=0.
      IF(X.GT.X0)THEN
         DEL=4.606*X+C
         IF(X.LE.X1)DEL=DEL+AA*(X1-X)**3.
      ENDIF
C
      DEDX=CONS*FAC*(LOG(2.*T+4.)-2.*POTL+F-DEL)/BET2
      IF(DEDX.LT.0.)DEDX=0.
C
  99  RETURN
      END
