CDECK  ID>, GASSOL.
       SUBROUTINE GASSOL(IFAIL)
*-----------------------------------------------------------------------
*   GASSOL - Sets a solid for HEED
*   (Last changed on 26/10/07.)
*-----------------------------------------------------------------------
      implicit none
       INTEGER MXWIRE,MXSW,MXLIST,MXCHA,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR,
     -         MXPAIR,MXPART,MXFOUR,MXCLUS,
     -         MXLINE,MXEQUT,
     -         MXRECL,MXINCH,MXWORD,MXCHAR,MXNAME,MXLUN,
     -         MXINS,MXREG,MXARG,MXCONS,MXVAR,MXALGE,
     -         MXZERO,MXSTCK,MXFPNT,MXFPAR,MXWKLS,
     -         MXHLEV,MXHLRL,MXSUBT,
     -         MXDLVL,MXILVL,MXDLIN,
     -         MXHIST,MXFRAC,MXBANG,MXBTAB,
     -         MXEXG,MXIOG,MXCSG,
     -         MXORIA,
     -         MXMAT,MXEMAT,MXMDIM,
     -         MXSHOT,MXZPAR,
     -         MXMAP,MXEPS,MXWMAP,MXSOLI,MXSBUF,
     -         MXPLAN,MXPOIN,MXEDGE,
     -         MXMCA
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXHIST=   200, MXCHA = MXLIST/2)
       PARAMETER (MXGRID=    50)
       PARAMETER (MXNAME=   200, MXLUN =    30)
       PARAMETER (MXCLUS=   500, MXPAIR=  2000, MXPART= 10000)
       PARAMETER (MXLINE=   150, MXEQUT=    50)
       PARAMETER (MXFOUR=    16)
       PARAMETER (MXRECL= 10000)
       PARAMETER (MXINCH=  2000, MXWORD=   200, MXCHAR=MXINCH)
       PARAMETER (MXINS =  1000, MXREG =   500, MXCONS=  -500,
     -            MXVAR =   500, MXALGE=   500, MXARG =   100)
       PARAMETER (MXMAT =   500, MXEMAT=200000, MXMDIM=   10)
       PARAMETER (MXZERO=MXWIRE)
       PARAMETER (MXSTCK=     5)
       PARAMETER (MXFPNT= 20000, MXFPAR=    10)
       PARAMETER (MXWKLS=    10)
       PARAMETER (MXHLEV=     9, MXSUBT=   200, MXHLRL=  860)
       PARAMETER (MXDLVL=    10, MXILVL=    20, MXDLIN= 2500)
       PARAMETER (MXFRAC=    13)
       PARAMETER (MXBANG=    20, MXBTAB=    25)
       PARAMETER (MXEXG =    50, MXIOG =    10, MXCSG =  200)
       PARAMETER (MXORIA=  1000)
       PARAMETER (MXSHOT=    10, MXZPAR=4*MXSHOT+2)
       PARAMETER (MXMAP =350000,MXEPS =   10)
       PARAMETER (MXWMAP=     5)
       PARAMETER (MXSOLI=  1000)
       PARAMETER (MXPLAN= 50000, MXPOIN=100000,MXEDGE=100)
       PARAMETER (MXSBUF= 20000)
       PARAMETER (MXMCA = 50000)
*   The parameter MXNBMC must equal MXGNAM (sequence MAGBPARM) !
       INTEGER MXNBMC
       PARAMETER(MXNBMC=60)
       DOUBLE PRECISION CLSDIS,CLSAVE
       REAL EGAS,VGAS,XGAS,YGAS,DGAS,AGAS,BGAS,HGAS,MGAS,WGAS,OGAS,SGAS,
     -      EXGAS,IOGAS,
     -      CVGAS,CXGAS,CYGAS,CDGAS,CAGAS,CBGAS,CHGAS,CMGAS,CWGAS,COGAS,
     -      CSGAS,CEXGAS,CIOGAS,
     -      VGAS2,XGAS2,YGAS2,DGAS2,AGAS2,BGAS2,HGAS2,MGAS2,WGAS2,OGAS2,
     -      SGAS2,EXGAS2,IOGAS2,
     -      AORIG,AORIG2,PENPRB,PENRMS,PENDT,ENIOG,ENEXG,
     -      BANG,BTAB,
     -      VEXTR1,VEXTR2,VEXTR3,VEXTR4,
     -      XEXTR1,XEXTR2,XEXTR3,XEXTR4,
     -      YEXTR1,YEXTR2,YEXTR3,YEXTR4,
     -      DEXTR1,DEXTR2,DEXTR3,DEXTR4,
     -      AEXTR1,AEXTR2,AEXTR3,AEXTR4,
     -      BEXTR1,BEXTR2,BEXTR3,BEXTR4,
     -      HEXTR1,HEXTR2,HEXTR3,HEXTR4,
     -      MEXTR1,MEXTR2,MEXTR3,MEXTR4,
     -      WEXTR1,WEXTR2,WEXTR3,WEXTR4,
     -      OEXTR1,OEXTR2,OEXTR3,OEXTR4,
     -      SEXTR1,SEXTR2,SEXTR3,SEXTR4,
     -      EEXTR1,EEXTR2,EEXTR3,EEXTR4,
     -      ZEXTR1,ZEXTR2,ZEXTR3,ZEXTR4,
     -      GASRNG,
     -      Z,A,RHO,CMEAN,EMPROB,EPAIR,PGAS,TGAS,GASDEN,
     -      DTION,DLION,GASFRM,ELOSCS
       LOGICAL GASOK,TAB2D,GASOPT,HEEDOK,SRIMOK,TRIMOK,GASSET
       INTEGER NGAS,NCLS,NBANG,NBTAB,NFTAB,NFCLS,
     -      IVMETH,IXMETH,IYMETH,IDMETH,IAMETH,IBMETH,IHMETH,IMMETH,
     -      IWMETH,IOMETH,ISMETH,IEMETH,IZMETH,
     -      IVEXTR,IXEXTR,IYEXTR,IDEXTR,IAEXTR,IBEXTR,IHEXTR,IMEXTR,
     -      IWEXTR,IOEXTR,ISEXTR,IEEXTR,IZEXTR,
     -      JVEXTR,JXEXTR,JYEXTR,JDEXTR,JAEXTR,JBEXTR,JHEXTR,JMEXTR,
     -      JWEXTR,JOEXTR,JSEXTR,JEEXTR,JZEXTR,
     -      IATHR,IBTHR,IHTHR,
     -      NEXGAS,NIOGAS,NCSGAS,ICSTYP
       CHARACTER*80 GASID
       CHARACTER*(MXCHAR) FCNTAB,FCNCLS
       CHARACTER*10 CLSTYP
       CHARACTER*45 DSCEXG(MXEXG),DSCIOG(MXIOG),DSCCSG(MXCSG)
       COMMON /GASDAT/ CLSDIS(MXPAIR),CLSAVE,
     -      EGAS(MXLIST),
     -      VGAS(MXLIST),XGAS(MXLIST),YGAS(MXLIST),WGAS(MXLIST),
     -      DGAS(MXLIST),OGAS(MXLIST),AGAS(MXLIST),BGAS(MXLIST),
     -      HGAS(MXLIST),MGAS(MXLIST),SGAS(MXLIST,6),
     -      EXGAS(MXLIST,MXEXG),IOGAS(MXLIST,MXIOG),
     -      CVGAS(MXLIST),CXGAS(MXLIST),CYGAS(MXLIST),CWGAS(MXLIST),
     -      CDGAS(MXLIST),COGAS(MXLIST),CAGAS(MXLIST),CBGAS(MXLIST),
     -      CHGAS(MXLIST),CMGAS(MXLIST),CSGAS(MXLIST,6),
     -      CEXGAS(MXLIST,MXEXG),CIOGAS(MXLIST,MXIOG),
     -      VGAS2(MXLIST,MXBANG,MXBTAB),WGAS2(MXLIST,MXBANG,MXBTAB),
     -      XGAS2(MXLIST,MXBANG,MXBTAB),YGAS2(MXLIST,MXBANG,MXBTAB),
     -      AGAS2(MXLIST,MXBANG,MXBTAB),BGAS2(MXLIST,MXBANG,MXBTAB),
     -      DGAS2(MXLIST,MXBANG,MXBTAB),OGAS2(MXLIST,MXBANG,MXBTAB),
     -      HGAS2(MXLIST,MXBANG,MXBTAB),MGAS2(MXLIST,MXBANG,MXBTAB),
     -      SGAS2(MXLIST,MXBANG,MXBTAB,6),
     -      EXGAS2(MXLIST,MXBANG,MXBTAB,MXEXG),
     -      IOGAS2(MXLIST,MXBANG,MXBTAB,MXIOG),
     -      AORIG(MXLIST),AORIG2(MXLIST,MXBANG,MXBTAB),
     -      PENPRB(MXEXG),PENRMS(MXEXG),PENDT(MXEXG),
     -      ENIOG(MXIOG),ENEXG(MXEXG),
     -      BANG(MXBANG),BTAB(MXBTAB),
     -      GASRNG(20,2),GASFRM(MXNBMC),ELOSCS(MXCSG),
     -      Z,A,RHO,CMEAN,EMPROB,EPAIR,PGAS,TGAS,GASDEN,
     -      DTION,DLION,
     -      VEXTR1,VEXTR2,VEXTR3,VEXTR4,
     -      XEXTR1,XEXTR2,XEXTR3,XEXTR4,
     -      YEXTR1,YEXTR2,YEXTR3,YEXTR4,
     -      DEXTR1,DEXTR2,DEXTR3,DEXTR4,
     -      AEXTR1,AEXTR2,AEXTR3,AEXTR4,
     -      BEXTR1,BEXTR2,BEXTR3,BEXTR4,
     -      HEXTR1,HEXTR2,HEXTR3,HEXTR4,
     -      MEXTR1,MEXTR2,MEXTR3,MEXTR4,
     -      WEXTR1,WEXTR2,WEXTR3,WEXTR4,
     -      OEXTR1,OEXTR2,OEXTR3,OEXTR4,
     -      SEXTR1(6),SEXTR2(6),SEXTR3(6),SEXTR4(6),
     -      EEXTR1(MXEXG),EEXTR2(MXEXG),EEXTR3(MXEXG),EEXTR4(MXEXG),
     -      ZEXTR1(MXIOG),ZEXTR2(MXIOG),ZEXTR3(MXIOG),ZEXTR4(MXIOG),
     -      IVMETH,IXMETH,IYMETH,IDMETH,IAMETH,IBMETH,IHMETH,IMMETH,
     -      IWMETH,IOMETH,ISMETH,IEMETH,IZMETH,
     -      IVEXTR,IXEXTR,IYEXTR,IDEXTR,IAEXTR,IBEXTR,IHEXTR,IMEXTR,
     -      IWEXTR,IOEXTR,ISEXTR,IEEXTR,IZEXTR,
     -      JVEXTR,JXEXTR,JYEXTR,JDEXTR,JAEXTR,JBEXTR,JHEXTR,JMEXTR,
     -      JWEXTR,JOEXTR,JSEXTR,JEEXTR,JZEXTR,
     -      NGAS,NCLS,NBANG,NBTAB,NFTAB,NFCLS,
     -      IATHR,IBTHR,IHTHR,
     -      NEXGAS,NIOGAS,NCSGAS,ICSTYP(MXCSG),
     -      GASOK(20),GASOPT(20,4),
     -      TAB2D,HEEDOK,SRIMOK,TRIMOK,GASSET
       COMMON /GASCHR/ FCNTAB,FCNCLS,CLSTYP,GASID,DSCEXG,DSCIOG,DSCCSG
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
c      Main control variables


	integer soo		   ! Flag, allowing to print
				   !  to stream 'oo'
				   ! If it is 0, no print will be at all,
				   ! except the case of serious problems.
	integer oo		   ! The output stream logical number.
	integer qevt		   ! Quantity of events to produce.
	integer nevt		   ! Current number of the event.
	integer ninfo		   ! Quantity of the first events
				   ! to print debug info.
	integer ssimioni	   ! Flag to simulate ionization loss,
				   ! 0 - no ionization,
				   ! 1 - to simulate ionization.
				   !
				   !
				   !
	integer srandoff	   ! Flag to swich off the randomization
				   ! in function treatdel.
				   ! It is for debug and without guarantee.
	parameter (srandoff=0)	   ! Normal regim with randommization.

	integer pqup		   ! dimensions of arrays of auxiliary
				   ! parameters in abs.inc, rga.inc,
				   ! del.inc
	parameter (pqup=1)


	integer sret_err	! Sign to return the control from current
		! subroutine to which is called it if error is occured.
		! 1 - to return, 0 - to stop.
		! It is intended for handling with subroutine SHEED.
		! In the case of error it can return the control instead of
		! stop. But not for every possible errors return is done.
		! Some of the most original errors could lead to stop.
		! When working with HEED program, sret_err must be zero.
	integer s_err	! Sign of error.
		! 1 - error, 0 - no error

	character*9  TaskName	   ! Name of task, using for generating
				   ! file names.
	character*40 OutputFile	   ! Name of file with output listing.
				   ! Using only in IniHeed.
*** Common split in character and non-character parts (RV 26/10/2007).
	common / cGoEve /
     +	soo, oo,
     +	qevt,nevt,ninfo,
     +	ssimioni,
     +	sret_err, s_err
        common / cGoEveCHR /
     +	TaskName,
     +	OutputFile

	save / cGoEve /
	save / cGoEveCHR /
*** End of change

c	Numbers(pointers) of atoms in atom.inc.

c	Since for some of them a special treatment is provided
c	in subroutine Iniatom and this subroutine recognize them by number,
c	the user must not initialize another atoms on these places,
c	even if subroutine AtomsByDefault is not called.
c	Another atoms can be initialized on free places.

	integer num_H
	integer num_H3
	integer num_H4
	integer num_He
	integer num_Li
	integer num_C
	integer num_C1
	integer num_C2
	integer num_C3
c	integer num_C4
	integer num_N
	integer num_O
	integer num_F
	integer num_Ne
	integer num_Al
	integer num_Si
	integer num_Ar
	integer num_Kr
	integer num_Xe
	parameter (num_H  = 1  )
	parameter (num_H3 = 2  )
	parameter (num_H4 = 3  )
	parameter (num_He = 4  )
	parameter (num_Li = 5  )
	parameter (num_C  = 6  )
	parameter (num_N  = 7  )
	parameter (num_O  = 8  )
	parameter (num_F  = 9  )
	parameter (num_Ne  =10 )
	parameter (num_Al = 11 )
	parameter (num_Si = 12 )
	parameter (num_Ar = 13 )
	parameter (num_Kr = 14 )
	parameter (num_Xe = 15 )
	parameter (num_C1  = 16 )	! C in CO2
	parameter (num_C2  = 17 )	! C in CF4
	parameter (num_C3  = 18 )	! C in CH4
*** Additions (RV 20/9/99).
        integer num_S
        parameter (num_S = 19)
*** End of additions.
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
	
       REAL watom(pqat),AUX,DENSOL,FANO,WORK,FRTOT
       INTEGER qatom,aatom(pqat),IFAIL,INPTYP,INPCMP,IFAIL1,
     -      INEXT,NWORD,I,nmat
       LOGICAL USED(pqat)
       EXTERNAL INPTYP,INPCMP
       SAVE qatom,aatom,watom,fano,densol,work
*** Identify.
       IF(LIDENT)PRINT *,' /// ROUTINE GASSOL ///'
       PRINT *,' ------ GASSOL MESSAGE : Heed version 1.01,'//
     -      ' interface last changed on 24/11/03.'
*** Assume the routine will fail.
       IFAIL=1
*** Initialise the gas mix.
       DO 20 I=1,pqat
       USED(I)=.FALSE.
20     CONTINUE
       qatom=0
       FANO=-1
       DENSOL=-1
       WORK=-1
*** Determine number of words.
       CALL INPNUM(NWORD)
*** Loop over the input.
       INEXT=2
       DO 10 I=2,NWORD
       IF(I.LT.INEXT)GOTO 10
*** Fractions, first Hydrogen.
       IF(INPCMP(I,'H#YDROGEN').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(num_H))THEN
                 CALL INPMSG(I,'Element already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qatom=qatom+1
                 aatom(qatom)=num_H
                 watom(qatom)=AUX
                 USED(num_H)=.TRUE.
            ENDIF
            INEXT=I+2
*   Helium
       ELSEIF(INPCMP(I,'HE#LIUM').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(num_He))THEN
                 CALL INPMSG(I,'Element already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qatom=qatom+1
                 aatom(qatom)=num_He
                 watom(qatom)=AUX
                 USED(num_He)=.TRUE.
            ENDIF
            INEXT=I+2
*   Lithium
       ELSEIF(INPCMP(I,'LI#THIUM').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(num_Li))THEN
                 CALL INPMSG(I,'Element already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qatom=qatom+1
                 aatom(qatom)=num_Li
                 watom(qatom)=AUX
                 USED(num_Li)=.TRUE.
            ENDIF
            INEXT=I+2
*   Carbon
       ELSEIF(INPCMP(I,'C#ARBON').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(num_C))THEN
                 CALL INPMSG(I,'Element already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qatom=qatom+1
                 aatom(qatom)=num_C
                 watom(qatom)=AUX
                 USED(num_C)=.TRUE.
            ENDIF
            INEXT=I+2
*   Nitrogen
       ELSEIF(INPCMP(I,'N#ITROGEN').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(num_N))THEN
                 CALL INPMSG(I,'Element already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qatom=qatom+1
                 aatom(qatom)=num_N
                 watom(qatom)=AUX
                 USED(num_N)=.TRUE.
            ENDIF
            INEXT=I+2
*   Oxygen
       ELSEIF(INPCMP(I,'O#XYGEN').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(num_O))THEN
                 CALL INPMSG(I,'Element already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qatom=qatom+1
                 aatom(qatom)=num_O
                 watom(qatom)=AUX
                 USED(num_O)=.TRUE.
            ENDIF
            INEXT=I+2
*   Fluorine
       ELSEIF(INPCMP(I,'F#LUORINE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(num_F))THEN
                 CALL INPMSG(I,'Element already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qatom=qatom+1
                 aatom(qatom)=num_F
                 watom(qatom)=AUX
                 USED(num_F)=.TRUE.
            ENDIF
            INEXT=I+2
*   Neon
       ELSEIF(INPCMP(I,'NE#ON').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(num_Ne))THEN
                 CALL INPMSG(I,'Element already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qatom=qatom+1
                 aatom(qatom)=num_Ne
                 watom(qatom)=AUX
                 USED(num_Ne)=.TRUE.
            ENDIF
            INEXT=I+2
*   Aluminium
       ELSEIF(INPCMP(I,'AL#UMINIUM').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(num_Al))THEN
                 CALL INPMSG(I,'Element already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qatom=qatom+1
                 aatom(qatom)=num_Al
                 watom(qatom)=AUX
                 USED(num_Al)=.TRUE.
            ENDIF
            INEXT=I+2
*   Silicon
       ELSEIF(INPCMP(I,'SI#LICON').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(num_Si))THEN
                 CALL INPMSG(I,'Element already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qatom=qatom+1
                 aatom(qatom)=num_Si
                 watom(qatom)=AUX
                 USED(num_Si)=.TRUE.
            ENDIF
            INEXT=I+2
*   Argon
       ELSEIF(INPCMP(I,'AR#GON').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(num_Ar))THEN
                 CALL INPMSG(I,'Element already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qatom=qatom+1
                 aatom(qatom)=num_Ar
                 watom(qatom)=AUX
                 USED(num_Ar)=.TRUE.
            ENDIF
            INEXT=I+2
*   Krypton
       ELSEIF(INPCMP(I,'KR#YPTON').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(num_Kr))THEN
                 CALL INPMSG(I,'Element already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qatom=qatom+1
                 aatom(qatom)=num_Kr
                 watom(qatom)=AUX
                 USED(num_Kr)=.TRUE.
            ENDIF
            INEXT=I+2
*   Xenon
       ELSEIF(INPCMP(I,'XE#NON').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(num_Xe))THEN
                 CALL INPMSG(I,'Element already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qatom=qatom+1
                 aatom(qatom)=num_Xe
                 watom(qatom)=AUX
                 USED(num_Xe)=.TRUE.
            ENDIF
            INEXT=I+2
*   Sulphur
       ELSEIF(INPCMP(I,'S#ULPHUR').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(num_S))THEN
                 CALL INPMSG(I,'Element already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qatom=qatom+1
                 aatom(qatom)=num_S
                 watom(qatom)=AUX
                 USED(num_S)=.TRUE.
            ENDIF
            INEXT=I+2
*   The density
       ELSEIF(INPCMP(I,'DENS#ITY').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 IF(AUX.LE.0)THEN
                      CALL INPMSG(I,'Density not > 0.')
                 ELSE
                      DENSOL=AUX
                 ENDIF
            ENDIF
            INEXT=I+2
*   The Fano factor
       ELSEIF(INPCMP(I,'FANO').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 IF(AUX.LE.0.OR. AUX.GT.1)THEN
                      CALL INPMSG(I,'Fano factor not > 0 and < 1.')
                 ELSE
                      FANO=AUX
                 ENDIF
            ENDIF
            INEXT=I+2
*   Work for a pair
       ELSEIF(INPCMP(I,'WORK').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 IF(AUX.LE.0)THEN
                      CALL INPMSG(I,'Work not > 0 eV.')
                 ELSE
                      WORK=AUX
                 ENDIF
            ENDIF
            INEXT=I+2
*   All the rest is not known.
       ELSE
            CALL INPMSG(I,'Not a known keyword.')
       ENDIF
10     CONTINUE
*** Print the error messages accumulated sofar.
       CALL INPERR
*** Check that there is at least 1 atom in the mixture.
       FRTOT=0.0
       DO 120 I=1,qatom
       IF(watom(I).LT.0)watom(I)=0.0
       FRTOT=FRTOT+watom(I)
120    CONTINUE
       IF(FRTOT.LE.0.0)THEN
            PRINT *,' !!!!!! GASSOL WARNING : Please have at least'//
     -           ' one atom in your mixture; nothing done.'
            IFAIL=1
            RETURN
       ENDIF
*   Check that the density has been specified.
       IF(DENSOL.LE.0)THEN
            PRINT *,' !!!!!! GASSOL WARNING : Please specify the'//
     -           ' density of your mixture; nothing done.'
            IFAIL=1
            RETURN
       ENDIF
*   Check that the work has been specified.
       IF(WORK.LE.0)THEN
            PRINT *,' !!!!!! GASSOL WARNING : Please specify the'//
     -           ' work of your mixture; nothing done.'
            IFAIL=1
            RETURN
       ENDIF
*   Check that the Fano factor has been specified.
       IF(FANO.LE.0)THEN
            PRINT *,' ------ GASSOL MESSAGE : Using a default Fano'//
     -           ' factor of 0.19 since you did not specify one.'
            FANO=0.19
       ENDIF
*** Debugging information.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ GASSOL DEBUG   : Mixture'',
     -           '' composed as follows:'')')
            DO 30 I=1,qatom
            IF(aatom(i).eq.num_H)THEN
                 WRITE(LUNOUT,'(26X,''Hydrogen    '',F10.3)') watom(I)
            ELSEIF(aatom(i).eq.num_He)THEN
                 WRITE(LUNOUT,'(26X,''Helium      '',F10.3)') watom(I)
            ELSEIF(aatom(i).eq.num_Li)THEN
                 WRITE(LUNOUT,'(26X,''Lithium     '',F10.3)') watom(I)
            ELSEIF(aatom(i).eq.num_C)THEN
                 WRITE(LUNOUT,'(26X,''Carbon      '',F10.3)') watom(I)
            ELSEIF(aatom(i).eq.num_N)THEN
                 WRITE(LUNOUT,'(26X,''Nitrogen    '',F10.3)') watom(I)
            ELSEIF(aatom(i).eq.num_O)THEN
                 WRITE(LUNOUT,'(26X,''Oxygen      '',F10.3)') watom(I)
            ELSEIF(aatom(i).eq.num_F)THEN
                 WRITE(LUNOUT,'(26X,''Fluorine    '',F10.3)') watom(I)
            ELSEIF(aatom(i).eq.num_Ne)THEN
                 WRITE(LUNOUT,'(26X,''Neon        '',F10.3)') watom(I)
            ELSEIF(aatom(i).eq.num_Al)THEN
                 WRITE(LUNOUT,'(26X,''Aluminium   '',F10.3)') watom(I)
            ELSEIF(aatom(i).eq.num_Si)THEN
                 WRITE(LUNOUT,'(26X,''Silicon     '',F10.3)') watom(I)
            ELSEIF(aatom(i).eq.num_Ar)THEN
                 WRITE(LUNOUT,'(26X,''Argon       '',F10.3)') watom(I)
            ELSEIF(aatom(i).eq.num_Kr)THEN
                 WRITE(LUNOUT,'(26X,''Krypton     '',F10.3)') watom(I)
            ELSEIF(aatom(i).eq.num_Xe)THEN
                 WRITE(LUNOUT,'(26X,''Xenon       '',F10.3)') watom(I)
            ELSEIF(aatom(i).eq.num_S)THEN
                 WRITE(LUNOUT,'(26X,''Sulphur     '',F10.3)') watom(I)
            ELSE
                 WRITE(LUNOUT,'(26X,''# Unknown # '',F10.3)') watom(I)
            ENDIF
30          CONTINUE
            WRITE(LUNOUT,'(
     -           26X,''Density:     '',F10.3,'' g/cm3''/
     -           26X,''Work:        '',F10.3,'' eV''/
     -           26X,''Fano factor: '',F10.3)') DENSOL,WORK,FANO
       ENDIF
*** Set HEED printing and error monitoring flags.
       IF(LDEBUG)THEN
            soo=1
       ELSE
            soo=0
       ENDIF
       oo=LUNOUT
       s_err=0
*** Restore after previous run.
       do i=1,pQAt
       Zat(i)=0
       enddo
       nmat=1
       QAtMat(nmat)=0
*** Initialise random number genarator
       sseed=0
       seed(1)=1121517854	! this is example
       seed(2)=612958528
*** Number of events to generate
       qevt=1       ! Quantity of events to generate
*** Parameters
       ssimioni=1              ! Simulate ionization loss
       ninfo=0                 ! Number of first events with output listing
*** Call the HEED material routine.
       call Inishl			! Cascade from excited atom
       call IniEner(150,3e-6,0.2)	! Energy mesh
       call AtomsByDefault		! Library of atoms
       Cur_Pressure=PGAS
       Cur_Temper=TGAS
       call IniMatter(1,aatom,watom,qatom,densol,work/1e6,fano)
*** Remember that this worked.
       HEEDOK=.TRUE.
       END
