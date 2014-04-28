CDECK  ID>, GASHEE.
       SUBROUTINE GASHEE(IFAIL)
*-----------------------------------------------------------------------
*   GASHEE - Sets the gas composition for HEED
*   (Last changed on 14/12/07.)
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
        integer    pqMol           ! Quantity of sorts of molecules.
        parameter (pqMol=27)

	integer 	 numm_He
	parameter 	(numm_He= 1)	

	integer 	 numm_Ne
	parameter 	(numm_Ne= 2)	

	integer 	 numm_Ar
	parameter 	(numm_Ar= 3)	

	integer 	 numm_Kr
	parameter 	(numm_Kr= 4)	

	integer 	 numm_Xe
	parameter 	(numm_Xe= 5)	

	integer 	 numm_H2
	parameter 	(numm_H2= 6)	

	integer 	 numm_N2
	parameter 	(numm_N2= 7)	

	integer 	 numm_O2
	parameter 	(numm_O2= 8)	

	integer 	 numm_NH3
	parameter 	(numm_NH3= 9)

	integer 	 numm_N2O
	parameter 	(numm_N2O= 10)

	integer 	 numm_CO2
	parameter 	(numm_CO2= 11)	

	integer 	 numm_CF4
	parameter 	(numm_CF4= 12)	

	integer 	 numm_CH4
	parameter 	(numm_CH4= 13)

	integer 	 numm_C2H2
	parameter 	(numm_C2H2= 14)

	integer 	 numm_C2H4
	parameter 	(numm_C2H4= 15)

	integer 	 numm_C2H6
	parameter 	(numm_C2H6= 16)

	integer 	 numm_C3H8
	parameter 	(numm_C3H8= 17)

	integer 	 numm_iC4H10
	parameter 	(numm_iC4H10= 18)

	integer 	 numm_C			! for debug
	parameter 	(numm_C = 19)
*** Additions (RV 4/9/98).
        integer          numm_DME
        parameter       (numm_DME= 20)

        integer          numm_H2O
        parameter       (numm_H2O= 21)
*** Additions (RV 20/9/99).
        integer          numm_SF6
        parameter       (numm_SF6= 22)

        integer          numm_C2F4H2
        parameter       (numm_C2F4H2= 23)

*** Addition (RV 14/1/00).
        integer          numm_C5H12
        parameter       (numm_C5H12= 24)
*** Addition (RV 25/2/00).
        integer          numm_C2F5H
        parameter       (numm_C2F5H= 25)
*** Addition (RV 4/9/01).
        integer          numm_C3F8
        parameter       (numm_C3F8= 26)
*** Additions (RV 14/12/07).
        integer          numm_CS2
        parameter       (numm_CS2= 27)
*** End of additions.

c	integer 	 numm_CClF3
c	parameter 	(numm_CClF3= 19)

c	integer 	 numm_CClF2
c	parameter 	(numm_CClF2= 20)

c	integer 	 numm_CBrF3
c	parameter 	(numm_CBrF3= 21)

c	integer 	 numm_SF6
c	parameter 	(numm_SF6= 22)
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

       REAL pwmol(pqmol),FRTOT,AUX
       INTEGER qmol,nmol(pqmol),IFAIL,INPTYP,INPCMP,IFAIL1,IERROR,
     -      INEXT,NWORD,I,IOS
       LOGICAL USED(pqmol)
       EXTERNAL INPTYP,INPCMP
       SAVE qmol,nmol,pwmol
*** Identify.
       IF(LIDENT)PRINT *,' /// ROUTINE GASHEE ///'
       PRINT *,' ------ GASHEE MESSAGE : Heed version 1.01,'//
     -      ' interface last changed on 14/12/07.'
*** Assume the routine will fail.
       IFAIL=1
*** Initialise the gas mix.
       DO 20 I=1,pqmol
       USED(I)=.FALSE.
20     CONTINUE
       qmol=0
*** Determine number of words.
       CALL INPNUM(NWORD)
*** Loop over the input.
       INEXT=2
       DO 10 I=2,NWORD
       IF(I.LT.INEXT)GOTO 10
*** Fractions, first Argon.
       IF(INPCMP(I,'AR#GON').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_Ar))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_Ar
                 pwmol(qmol)=AUX
                 USED(numm_Ar)=.TRUE.
            ENDIF
            INEXT=I+2
*   Methane.
       ELSEIF(INPCMP(I,'METHA#NE')+INPCMP(I,'CH4').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_CH4))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_CH4
                 pwmol(qmol)=AUX
                 USED(numm_CH4)=.TRUE.
            ENDIF
            INEXT=I+2
*   Nitrogen.
       ELSEIF(INPCMP(I,'NI#TROGEN')+INPCMP(I,'N2').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_N2))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_N2
                 pwmol(qmol)=AUX
                 USED(numm_N2)=.TRUE.
            ENDIF
            INEXT=I+2
*   CO2.
       ELSEIF(INPCMP(I,'CO2')+
     -      INPCMP(I,'CARB#ON-DIOX#IDE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_CO2))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_CO2
                 pwmol(qmol)=AUX
                 USED(numm_CO2)=.TRUE.
            ENDIF
            INEXT=I+2
*   Helium 4.
       ELSEIF(INPCMP(I,'HE#LIUM-#4').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_He))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_He
                 pwmol(qmol)=AUX
                 USED(numm_He)=.TRUE.
            ENDIF
            INEXT=I+2
*   Helium 3.
       ELSEIF(INPCMP(I,'HE#LIUM-3').NE.0)THEN
            CALL INPMSG(I,'Not yet in HEED.')
            INEXT=I+2
*   Neon.
       ELSEIF(INPCMP(I,'NEON').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_Ne))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_Ne
                 pwmol(qmol)=AUX
                 USED(numm_Ne)=.TRUE.
            ENDIF
            INEXT=I+2
*   Ethane.
       ELSEIF(INPCMP(I,'ETHA#NE')+INPCMP(I,'C2H6').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_C2H6))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_C2H6
                 pwmol(qmol)=AUX
                 USED(numm_C2H6)=.TRUE.
            ENDIF
            INEXT=I+2
*   Propane.
       ELSEIF(INPCMP(I,'PROPA#NE')+INPCMP(I,'C3H8').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_C3H8))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_C3H8
                 pwmol(qmol)=AUX
                 USED(numm_C3H8)=.TRUE.
            ENDIF
            INEXT=I+2
*   Isobutane.
       ELSEIF(INPCMP(I,'ISO#BUTANE')+INPCMP(I,'C4H10').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_iC4H10))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_iC4H10
                 pwmol(qmol)=AUX
                 USED(numm_iC4H10)=.TRUE.
            ENDIF
            INEXT=I+2
*   Pentane.
       ELSEIF(INPCMP(I,'PENT#ANE')+INPCMP(I,'C5H12')+
     -      INPCMP(I,'N#EO-PENT#ANE')+INPCMP(I,'N#EO-C5H12').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_C5H12))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_C5H12
                 pwmol(qmol)=AUX
                 USED(numm_C5H12)=.TRUE.
            ENDIF
            INEXT=I+2
*   Methylal.
       ELSEIF(INPCMP(I,'METHY#LAL')+INPCMP(I,'C3H8O2').NE.0)THEN
            CALL INPMSG(I,'Not yet in HEED.')
            INEXT=I+2
*   Xenon.
       ELSEIF(INPCMP(I,'XE#NON').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_Xe))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_Xe
                 pwmol(qmol)=AUX
                 USED(numm_Xe)=.TRUE.
            ENDIF
            INEXT=I+2
*   Krypton.
       ELSEIF(INPCMP(I,'KR#YPTON').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_Kr))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_Kr
                 pwmol(qmol)=AUX
                 USED(numm_Kr)=.TRUE.
            ENDIF
            INEXT=I+2
*   CF4.
       ELSEIF(INPCMP(I,'CF4').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_CF4))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_CF4
                 pwmol(qmol)=AUX
                 USED(numm_CF4)=.TRUE.
            ENDIF
            INEXT=I+2
*   Oxygen.
       ELSEIF(INPCMP(I,'OX#YGEN')+INPCMP(I,'O2').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_O2))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_O2
                 pwmol(qmol)=AUX
                 USED(numm_O2)=.TRUE.
            ENDIF
            INEXT=I+2
*   DME.
       ELSEIF(INPCMP(I,'DME').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_DME))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_DME
                 pwmol(qmol)=AUX
                 USED(numm_DME)=.TRUE.
            ENDIF
            INEXT=I+2
*   Ethene.
       ELSEIF(INPCMP(I,'ETHE#NE')+INPCMP(I,'C2H4').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_C2H4))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_C2H4
                 pwmol(qmol)=AUX
                 USED(numm_C2H4)=.TRUE.
            ENDIF
            INEXT=I+2
*   Acetylene.
       ELSEIF(INPCMP(I,'ACETYL#ENE')+INPCMP(I,'C2H2').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_C2H2))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_C2H2
                 pwmol(qmol)=AUX
                 USED(numm_C2H2)=.TRUE.
            ENDIF
            INEXT=I+2
*   Nitric oxide (NO).
       ELSEIF(INPCMP(I,'NITRI#C-OX#IDE')+INPCMP(I,'NO').NE.0)THEN
            CALL INPMSG(I,'Not yet in HEED.')
            INEXT=I+2
*   Nitrous oxide (N2O).
       ELSEIF(INPCMP(I,'NITRO#US-OX#IDE')+INPCMP(I,'N2O').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_N2O))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_N2O
                 pwmol(qmol)=AUX
                 USED(numm_N2O)=.TRUE.
            ENDIF
            INEXT=I+2
*   Hydrogen gas.
       ELSEIF(INPCMP(I,'HYDR#OGEN')+INPCMP(I,'H2').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_H2))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_H2
                 pwmol(qmol)=AUX
                 USED(numm_H2)=.TRUE.
            ENDIF
            INEXT=I+2
*   Ammonia gas.
       ELSEIF(INPCMP(I,'AMMO#NIA')+INPCMP(I,'NH3').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_NH3))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_NH3
                 pwmol(qmol)=AUX
                 USED(numm_NH3)=.TRUE.
            ENDIF
            INEXT=I+2
*   Water vapour.
       ELSEIF(INPCMP(I,'H2O')+INPCMP(I,'WAT#ER').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_H2O))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_H2O
                 pwmol(qmol)=AUX
                 USED(numm_H2O)=.TRUE.
            ENDIF
            INEXT=I+2
*   SF6.
       ELSEIF(INPCMP(I,'SF6').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_SF6))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_SF6
                 pwmol(qmol)=AUX
                 USED(numm_SF6)=.TRUE.
            ENDIF
            INEXT=I+2
*   CS2.
       ELSEIF(INPCMP(I,'CS2').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_CS2))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_CS2
                 pwmol(qmol)=AUX
                 USED(numm_CS2)=.TRUE.
            ENDIF
            INEXT=I+2
*   C2F4H2 (1,1,1,2 tetrafluoroethane, HFC-134a).
       ELSEIF(INPCMP(I,'C2F4H2')+INPCMP(I,'C2H2F4')+
     -      INPCMP(I,'CH2FCF3')+
     -      INPCMP(I,'HFC-134A').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_C2F4H2))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_C2F4H2
                 pwmol(qmol)=AUX
                 USED(numm_C2F4H2)=.TRUE.
            ENDIF
            INEXT=I+2
*   C2F5H (?).
       ELSEIF(INPCMP(I,'C2F5H')+INPCMP(I,'C2HF5').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_C2F5H))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_C2F5H
                 pwmol(qmol)=AUX
                 USED(numm_C2F5H)=.TRUE.
            ENDIF
            INEXT=I+2
*   C3F8.
       ELSEIF(INPCMP(I,'C3F8').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSEIF(USED(numm_C3F8))THEN
                 CALL INPMSG(I,'Gas already referenced.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 qmol=qmol+1
                 nmol(qmol)=numm_C3F8
                 pwmol(qmol)=AUX
                 USED(numm_C3F8)=.TRUE.
            ENDIF
            INEXT=I+2
*   All the rest is not known.
       ELSE
            CALL INPMSG(I,'Not a known keyword.')
       ENDIF
10     CONTINUE
*** Print the error messages accumulated sofar.
       CALL INPERR
*** Renormalise the fractions.
       FRTOT=0.0
       DO 120 I=1,qmol
       IF(pwmol(I).LT.0)pwmol(I)=0.0
       FRTOT=FRTOT+pwmol(I)
120    CONTINUE
       IF(FRTOT.LE.0.0)THEN
            PRINT *,' !!!!!! GASHEE WARNING : Please have at least'//
     -           ' one gas in your mixture; nothing done.'
            IFAIL=1
            RETURN
       ELSE
            DO 130 I=1,qmol
            pwmol(I)=pwmol(I)/FRTOT
130         CONTINUE
       ENDIF
*** Debugging information.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ GASHEE DEBUG   : Gas mix'',
     -           '' composed as follows:'')')
            DO 30 I=1,qmol
            IF(nmol(i).eq.numm_He)THEN
                 WRITE(LUNOUT,'(26X,''Helium       '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_Ne)THEN
                 WRITE(LUNOUT,'(26X,''Neon         '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_Ar)THEN
                 WRITE(LUNOUT,'(26X,''Argon        '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_Kr)THEN
                 WRITE(LUNOUT,'(26X,''Krypton      '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_Xe)THEN
                 WRITE(LUNOUT,'(26X,''Xenon        '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_H2)THEN
                 WRITE(LUNOUT,'(26X,''H2           '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_N2)THEN
                 WRITE(LUNOUT,'(26X,''N2           '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_O2)THEN
                 WRITE(LUNOUT,'(26X,''O2           '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_NH3)THEN
                 WRITE(LUNOUT,'(26X,''NH3          '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_N2O)THEN
                 WRITE(LUNOUT,'(26X,''N2O          '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_CO2)THEN
                 WRITE(LUNOUT,'(26X,''CO2          '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_CF4)THEN
                 WRITE(LUNOUT,'(26X,''CF4          '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_CH4)THEN
                 WRITE(LUNOUT,'(26X,''CH4          '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_C2H2)THEN
                 WRITE(LUNOUT,'(26X,''C2H2         '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_C2H4)THEN
                 WRITE(LUNOUT,'(26X,''C2H4         '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_C2H6)THEN
                 WRITE(LUNOUT,'(26X,''C2H6         '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_C3H8)THEN
                 WRITE(LUNOUT,'(26X,''C3H8         '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_iC4H10)THEN
                 WRITE(LUNOUT,'(26X,''iC4H10       '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_C5H12)THEN
                 WRITE(LUNOUT,'(26X,''C5H12        '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_DME)THEN
                 WRITE(LUNOUT,'(26X,''DME          '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_H2O)THEN
                 WRITE(LUNOUT,'(26X,''H2O          '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_SF6)THEN
                 WRITE(LUNOUT,'(26X,''SF6          '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_CS2)THEN
                 WRITE(LUNOUT,'(26X,''CS2          '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_C2F4H2)THEN
                 WRITE(LUNOUT,'(26X,''C2F4H2       '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSEIF(nmol(i).eq.numm_C2F5H)THEN
                 WRITE(LUNOUT,'(26X,''C2F5H        '',F10.3,'' %'')')
     -                100*pwmol(I)
            ELSE
                 WRITE(LUNOUT,'(26X,''# Unknown #  '',F10.3,'' %'')')
     -                100*pwmol(I)
            ENDIF
30          CONTINUE
            WRITE(LUNOUT,'(26X,''Pressure:    '',F10.3,'' torr''/
     -           26X,''Temperature: '',F10.3,'' K'')') PGAS,TGAS
       ENDIF
*** Set HEED printing and error monitoring flags.
       IF(LDEBUG)THEN
            soo=1
       ELSE
            soo=0
       ENDIF
       oo=LUNOUT
       s_err=0
*** Call the HEED gas routine.
       ierror=0
       CALL imheed(
     -      qmol,       ! Different gas components
     -      nmol,       ! Names of gasses present in mixture
     -      pwmol,      ! Gas fractions
     -      PGAS,       ! Pressure [torr]
     -      TGAS,       ! Temperature [K]
     -      1,          ! 0 or 1: Do/don't  generate output
     -      6,          ! Output logical unit
     -      1,          ! 1/2 Short/Medium listing
     -      GASDEN,     ! (Output) computed density
     -      ierror)     ! Error indicator.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASHEE DEBUG   :'',
     -      '' HEED density: '',F10.3,'' g/l, error code: '',I3)')
     -      1000*GASDEN,ierror
*** Return error code.
       IF(ierror.NE.0)THEN
            PRINT *,' !!!!!! GASHEE WARNING : Gas preparation by'//
     -           ' HEED failed ; tracks can not be generated.'
            IFAIL=1
            HEEDOK=.FALSE.
       ELSE
            IFAIL=0
            HEEDOK=.TRUE.
       ENDIF
       RETURN
*** Write the tables.
       ENTRY GASHWR(IFAIL)
*   Assume for the moment that writing will work.
       IFAIL=0
*   See whether iniialisation has been performed.
       WRITE(12,'(''  Heed initialisation done: '',L1)',ERR=2010,
     -      IOSTAT=IOS) HEEDOK
       IF(HEEDOK)THEN
*   Write the composition.
            WRITE(12,'(''  Gas components: '',I5)',ERR=2010,
     -           IOSTAT=IOS) qmol
            DO 200 I=1,qmol
            WRITE(12,'(2X,I10,E15.8)',ERR=2010,IOSTAT=IOS)
     -           nmol(I),pwmol(I)
200         CONTINUE
       ENDIF
       RETURN
*   Errors during I/O.
2010   CONTINUE
       PRINT *,' !!!!!! GASHWR WARNING : I/O error occurred while'//
     -      ' writing Heed initialisation data.'
       CALL INPIOS(IOS)
       IFAIL=1
       RETURN
*** Retrieve initialisation data.
       ENTRY GASHGT(IFAIL)
*   Assume for the moment that reading will work.
       IFAIL=0
*   See whether initialisation should be performed.
       READ(12,'(28X,L1)',ERR=2015,IOSTAT=IOS) HEEDOK
       IF(HEEDOK)THEN
*   Read the composition.
            READ(12,'(18X,I5)',ERR=2015,IOSTAT=IOS) qmol
            IF(qmol.LT.0.OR.qmol.GT.pqmol)THEN
                 PRINT *,' !!!!!! GASHGT WARNING : Number of gas'//
     -                ' components < 0 or > current maximum; Heed'//
     -                ' initialisation not performed.'
                 RETURN
            ENDIF
            DO 210 I=1,qmol
            READ(12,'(2X,I10,E15.8)',ERR=2015,IOSTAT=IOS)
     -           nmol(I),pwmol(I)
210         CONTINUE
*   Perform the initialisation.
            ierror=0
            CALL imheed(
     -           qmol,       ! Different gas components
     -           nmol,       ! Names of gasses present in mixture
     -           pwmol,      ! Gas fractions
     -           PGAS,       ! Pressure [torr]
     -           TGAS,       ! Temperature [K]
     -           1,          ! 0 or 1: Do/don't  generate output
     -           6,          ! Output logical unit
     -           1,          ! 1/2 Short/Medium listing
     -           GASDEN,     ! (Output) computed density
     -           ierror)     ! Error indicator.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASHEE DEBUG   :'',
     -           '' HEED density: '',F10.3,'' g/l, error code: '',I3)')
     -           1000*GASDEN,ierror
*   Return error code.
            IF(ierror.NE.0)THEN
                 PRINT *,' !!!!!! GASHGT WARNING : Gas preparation by'//
     -                ' HEED failed ; tracks can not be generated.'
                 IFAIL=1
                 HEEDOK=.FALSE.
            ELSE
                 IFAIL=0
                 HEEDOK=.TRUE.
            ENDIF
       ENDIF
       RETURN
*   Errors during I/O.
2015   CONTINUE
       PRINT *,' !!!!!! GASHGT WARNING : I/O error occurred while'//
     -      ' retrieving Heed initialisation data.'
       CALL INPIOS(IOS)
       IFAIL=1
       END
