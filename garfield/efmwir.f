CDECK  ID>, EFMWIR.
       SUBROUTINE EFMWIR
*-----------------------------------------------------------------------
*   EFMWIR - Computes the dipole moment of a given wire.
*   (Last changed on 11/ 6/96.)
*-----------------------------------------------------------------------
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
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL,
     -      BEMSET
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL,BEMSET
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
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
       LOGICAL MAGOK
       REAL ALFA,B0X,B0Y,B0Z,SUSWIR,SUSGAS,BSCALE,BFMIN,BFMAX,
     -      BFXMIN,BFYMIN,BFZMIN,BFXMAX,BFYMAX,BFZMAX
       INTEGER MAGSRC,
     -      IBXTYP,IBYTYP,IBZTYP,
     -      IRB0X,IRB0Y,IRB0Z,IRV0X,IRV0Y,IRV0Z,
     -      IENB0X,IENB0Y,IENB0Z,IBXDIR,IBYDIR,IBZDIR,
     -      NCB0X,NCB0Y,NCB0Z
       CHARACTER*(MXCHAR) FUNB0X,FUNB0Y,FUNB0Z
       COMMON /MAGDAT/ ALFA,SUSWIR,SUSGAS,
     -      B0X,B0Y,B0Z,BSCALE,BFMIN,BFMAX,
     -      BFXMIN,BFYMIN,BFZMIN,BFXMAX,BFYMAX,BFZMAX,
     -      MAGSRC,IBXTYP,IBYTYP,IBZTYP,
     -      IRB0X,IRB0Y,IRB0Z,IRV0X,IRV0Y,IRV0Z,
     -      IENB0X,IENB0Y,IENB0Z,IBXDIR,IBYDIR,IBZDIR,
     -      NCB0X,NCB0Y,NCB0Z,
     -      MAGOK
       COMMON /MAGCHR/ FUNB0X,FUNB0Y,FUNB0Z
       INTEGER NPOLE,NPOLER,NPOLES,NITMAX,I,IFAIL,INEXT,IW,NCFUN,
     -      IWR
       COMMON /EFMDAT/ NPOLE
       PARAMETER(N=MXFPNT)
       CHARACTER*(MXCHAR) FUN
       CHARACTER*20 AUX
       CHARACTER*10 VARLIS(MXVAR)
       REAL PHI0(MXPOLE),POLE(MXPOLE),XPL(MXLIST),YPL(MXLIST),
     -      VAR(MXVAR),RES(1),DRES,VLTMIN,VLTMAX,VLTAVE,RMULT,
     -      RMULTR,EPSR
       DOUBLE PRECISION PAR(1+2*MXPOLE),ANGLE(N),VOLT(N),WEIGHT(N),
     -      DIST,CHI2,EPS,DAUX,PARRES(1+2*MXPOLE),EPAR(1+2*MXPOLE)
       LOGICAL LFITPR,LFITPL,USE(MXVAR)
       INTEGER MODVAR(MXVAR),MODRES(1)
       EXTERNAL EFMFUN
       SAVE IW,RMULT,NPOLES,NITMAX,EPS,LFITPR,LFITPL,VARLIS
       DATA IW/0/, RMULT/1.0/, NPOLES/4/, NITMAX/20/, EPS/1.0E-4/,
     -      LFITPR/.FALSE./, LFITPL/.FALSE./
       DATA (VARLIS(I),I=1,9) /
     -      'ANGLE     ','EX        ','EY        ','E         ',
     -      'V         ','BX        ','BY        ','BZ        ',
     -      'B         '/
*** Assume the routine fails.
       IFAIL=1
*** Special default handling for NPOLE which is in common.
       NPOLE=NPOLES
*** Default function.
       FUN='V'
       NCFUN=1
*** Decode the argument string, get the number of words.
       CALL INPNUM(NWORD)
       INEXT=2
*   Loop over the string.
       DO 100 I=2,NWORD
       IF(I.LT.INEXT)GOTO 100
*   Epsilon for fitting purposes.
       IF(INPCMP(I,'EPS#ILON').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Should have an argument.      ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL)
                 CALL INPRDR(I+1,EPSR,0.0)
                 IF(EPSR.LE.0.0.AND.IFAIL.EQ.0)THEN
                      CALL INPMSG(I,'Epsilon must be positive.     ')
                 ELSEIF(IFAIL.EQ.0)THEN
                      EPS=EPSR
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Function to be treated.
       ELSEIF(INPCMP(I,'F#UNCTION').NE.0)THEN
            IF(NWORD.LT.I+1)THEN
                 CALL INPMSG(I,'Should have an argument.      ')
            ELSE
                 CALL INPSTR(I+1,I+1,FUN,NCFUN)
                 INEXT=I+2
            ENDIF
*   Maximum number of iterations.
       ELSEIF(INPCMP(I,'I#TERATE-#MAXIMUM').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Should have an argument.      ')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL)
                 CALL INPRDI(I+1,NITMAR,0)
                 IF(NITMAR.LT.0.AND.IFAIL.EQ.0)THEN
                      CALL INPMSG(I,'Number of iterations < 0.     ')
                 ELSEIF(IFAIL.EQ.0)THEN
                      NITMAX=NITMAR
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Highest multipole order.
       ELSEIF(INPCMP(I,'O#RDER').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Should have an argument.      ')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL)
                 CALL INPRDI(I+1,NPOLER,0)
                 IF((NPOLER.LE.0.OR.NPOLER.GT.MXPOLE).AND.
     -                IFAIL.EQ.0)THEN
                      CALL INPMSG(I,'Multipole order out of range. ')
                 ELSEIF(IFAIL.EQ.0)THEN
                      NPOLE=NPOLER
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Number of radii.
       ELSEIF(INPCMP(I,'R#ADIUS').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Should have an argument.      ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL)
                 CALL INPRDR(I+1,RMULTR,0.0)
                 IF(RMULTR.LE.0.0.AND.IFAIL.EQ.0)THEN
                      CALL INPMSG(I,'Wire number out of range.     ')
                 ELSEIF(IFAIL.EQ.0)THEN
                      RMULT=RMULTR
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Print/Plot options.
       ELSEIF(INPCMP(I,'PL#OT').NE.0)THEN
            LFITPL=.TRUE.
       ELSEIF(INPCMP(I,'NOPL#OT').NE.0)THEN
            LFITPL=.FALSE.
       ELSEIF(INPCMP(I,'PR#INT').NE.0)THEN
            LFITPR=.TRUE.
       ELSEIF(INPCMP(I,'NOPR#INT').NE.0)THEN
            LFITPR=.FALSE.
*   Wire number.
       ELSEIF(INPCMP(I,'W#IRE').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Should have an argument.      ')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL)
                 CALL INPRDI(I+1,IWR,0)
                 IF((IWR.LE.0.OR.IWR.GT.NWIRE).AND.IFAIL.EQ.0)THEN
                      CALL INPMSG(I,'Wire number out of range.     ')
                 ELSEIF(IFAIL.EQ.0)THEN
                      IW=IWR
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Anything else.
       ELSE
            CALL INPMSG(I,'Not a known keyword; ignored. ')
       ENDIF
100    CONTINUE
       CALL INPERR
*** Keep track of the default value for NPOLE.
       NPOLES=NPOLE
*** Check the wire number again (cell change).
       IF(IW.LE.0.OR.IW.GT.NWIRE)THEN
            PRINT *,' !!!!!! EFMWIR WARNING : The wire number is not'//
     -           ' within range (0 -> number of wires).'
            RETURN
       ENDIF
*** Print the parameter settings.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ EFMWIR DEBUG   : Parameter'',
     -           '' settings:'',//
     -           5X,''Fit will be done for wire:     '',I3,/,
     -           5X,''Function to be fitted:         '',A,/,
     -           5X,''Highest multipole term fitted: '',I3,/,
     -           5X,''Radius multiplication factor:  '',E15.8,/,
     -           5X,''Maximum number of iterations:  '',I3,/,
     -           5X,''Epsilon for fitting purposes:  '',E15.8,/,
     -           5X,''Plotting: '',L1,'', Printing:  '',L1,/)')
     -           IW,FUN(1:NCFUN),NPOLE,RMULT,NITMAX,EPS,LFITPL,LFITPR
*** Set the radius of the wire to 0.
       DRES=D(IW)
       D(IW)=0.0
*** Translate the function.
       IF(INDEX(FUN(1:NCFUN),'@').NE.0)THEN
            NRES=0
            CALL ALGEDT(VARLIS,9,IENTRY,USE,NRES)
            FUN='Edited function'
            NCFUN=15
            IF(NRES.NE.1)THEN
                 PRINT *,' !!!!!! EFMWIR WARNING : The edited'//
     -                ' instruction list does not return 1 result;'//
     -                ' no fit.'
                 CALL ALGCLR(IENTRY)
                 RETURN
            ENDIF
       ELSE
            CALL ALGPRE(FUN,NCFUN,VARLIS,9,NRES,USE,IENTRY,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! EFMWIR WARNING : The function '//
     -                FUN(1:NCFUN)//' is not fitted because of'//
     -                ' syntax error(s).'
                 CALL ALGCLR(IENTRY)
                 RETURN
            ELSEIF(NRES.NE.1)THEN
                 PRINT *,' !!!!!! EFMWIR WARNING : The function'//
     -                ' does not return 1 result; no fit performed.'
                 CALL ALGCLR(IENTRY)
                 RETURN
            ENDIF
       ENDIF
*   Check use of variables.
       IF((.NOT.MAGOK).AND.(USE(6).OR.USE(7).OR.USE(8).OR.USE(9)))THEN
            PRINT *,' !!!!!! EFMWIR WARNING : The function relies on'//
     -           ' a magnetic field, which is not defined.'
            CALL ALGCLR(IENTRY)
            RETURN
       ENDIF
*** Loop around the wire.
       VLTMIN=0.0
       VLTMAX=0.0
       VLTAVE=0.0
       DO 10 I=1,N
*   Set angle around wire.
       ANGLE(I)=2*PI*REAL(I)/REAL(N)
*   Set up variable list.
       VAR(1)=ANGLE(I)
*   Compute E field, make sure the point is in a free region.
       IF(USE(2).OR.USE(3).OR.USE(4).OR.USE(5))THEN
            CALL EFIELD(REAL(X(IW)+RMULT*DRES*COS(ANGLE(I))/2),
     -           REAL(Y(IW)+RMULT*DRES*SIN(ANGLE(I))/2),0.0,
     -           VAR(2),VAR(3),EZ,VAR(4),VAR(5),1,ILOC)
            IF(ILOC.NE.0)THEN
                 PRINT *,' !!!!!! EFMWIR WARNING : Unexpected'//
     -                ' location code received from EFIELD ;'//
     -                ' computation stopped.'
                 GOTO 3000
            ENDIF
       ENDIF
*   Compute B field.
       IF(USE(6).OR.USE(7).OR.USE(8).OR.USE(9))
     -      CALL BFIELD(REAL(X(IW)+RMULT*DRES*COS(ANGLE(I))/2),
     -      REAL(Y(IW)+RMULT*DRES*SIN(ANGLE(I))/2),0.0,
     -      VAR(6),VAR(7),VAR(8),VAR(9))
*   Assign the variable modes.
       DO 120 J=1,9
       MODVAR(J)=2
120    CONTINUE
*   Evaluate the function.
       CALL ALGEXE(IENTRY,VAR,MODVAR,9,RES,MODRES,1,IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! EFMWIR WARNING : Algebra error'//
     -           ' evaluating the function at the angle',ANGLE(I)
            GOTO 3000
       ELSEIF(MODVAR(1).NE.2)THEN
            PRINT *,' !!!!!! EFMWIR WARNING : The result of the'//
     -           ' function is not a number at the angle',ANGLE(I)
            GOTO 3000
       ENDIF
*   Assign the result to the fitting array.
       VOLT(I)=RES(1)
*   Set weighting function to 1.
       WEIGHT(I)=1
*   Keep track of the maximum, minimum and average.
       IF(I.EQ.1)THEN
            VLTMAX=VOLT(I)
            VLTMIN=VOLT(I)
       ELSE
            IF(VLTMAX.LT.VOLT(I))VLTMAX=VOLT(I)
            IF(VLTMIN.GT.VOLT(I))VLTMIN=VOLT(I)
       ENDIF
       VLTAVE=VLTAVE+VOLT(I)
10     CONTINUE
*   Subtract the wire potential to put centre the data more or less.
       VLTAVE=VLTAVE/REAL(N)
       DO 50 I=1,N
       VOLT(I)=VOLT(I)-VLTAVE
50     CONTINUE
       VLTMAX=VLTMAX-VLTAVE
       VLTMIN=VLTMIN-VLTAVE
*** Perform the fit.
       CHI2=1E-6*N*(ABS(VLTMIN)+ABS(VLTMAX))**2/4
       DIST=1E-3*(2.0+ABS(VLTMIN)+ABS(VLTMAX))/2
       PAR(1)=(VLTMAX+VLTMIN)/2
       DO 30 I=1,NPOLE
       PAR(2*I)=(VLTMAX-VLTMIN)/2
       PAR(2*I+1)=0.0
30     CONTINUE
       CALL LSQFIT(EFMFUN,PAR,EPAR,2*NPOLE+1,ANGLE,VOLT,WEIGHT,N,
     -      NITMAX,DIST,CHI2,EPS,LFITPR,IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! EFMWIR WARNING : The procedure fitting'//
     -           ' the multipole failed ; computation stopped.'
            GOTO 3000
       ENDIF
*** Plot the result of the fit.
       IF(LFITPL)THEN
*   Frame with data curve.
            CALL GRGRP2(ANGLE,VOLT,N,
     -           'Angle around the wire [rad]',
     -           FUN(1:NCFUN)//' - average',
     -           'MULTIPOLE FIT FOR A WIRE')
            IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
            CALL OUTFMT(REAL(IW),2,AUX,NC,'LEFT')
            CALL GRCOMM(3,'Wire '//AUX(1:NC)//', type '//
     -           WIRTYP(IW))
            CALL OUTFMT(RMULT,2,AUX,NC,'LEFT')
            CALL GRCOMM(4,'Distance: '//AUX(1:NC)//' radii')
*   Sum of contributions.
            DO 20 I=1,MXLIST
            XPL(I)=2*PI*REAL(I)/REAL(MXLIST)
            CALL EFMFUN(DBLE(XPL(I)),PAR,DAUX)
            YPL(I)=REAL(DAUX)
20          CONTINUE
            CALL GRATTS('FUNCTION-2','POLYLINE')
            CALL GRLINE(MXLIST,XPL,YPL)
*   Individual contributions.
            CALL GRATTS('FUNCTION-3','POLYLINE')
            DO 70 I=1,2*NPOLE+1
            PARRES(I)=PAR(I)
            IF(2*(I/2).EQ.I)PARRES(I)=0
70          CONTINUE
            DO 80 J=1,NPOLE
            PARRES(2*J)=PAR(2*J)
            DO 90 I=1,MXLIST
            CALL EFMFUN(DBLE(XPL(I)),PARRES,DAUX)
            YPL(I)=REAL(DAUX)
90          CONTINUE
            PARRES(2*J)=0
            CALL GRLINE(MXLIST,XPL,YPL)
80          CONTINUE
            CALL GRNEXT
            CALL GRALOG('Multipole fit around a wire:            ')
       ENDIF
*** Remove radial terms from the multipole moments.
       DO 40 I=1,NPOLE
       POLE(I)=(RMULT*DRES/2)**I*PAR(2*I)
       PHI0(I)=180*MOD(REAL(PAR(2*I+1)),PI)/PI
40     CONTINUE
*** Print the results.
       WRITE(LUNOUT,'(''  Multipole moments for wire '',I3,'':''//
     -      ''  Moment            Value            Angle''/
     -      ''       -                -         [degree]''/)') IW
       WRITE(LUNOUT,'(2X,I6,2X,E15.8,8X,''Arbitrary'')') 0,VLTAVE
       DO 60 I=1,NPOLE
       WRITE(LUNOUT,'(2X,I6,2X,E15.8,2X,E15.8)') I,POLE(I),PHI0(I)
60     CONTINUE
       WRITE(LUNOUT,'('' '')')
*** Restore the wire diameter.
3000   CONTINUE
       D(IW)=DRES
       CALL ALGERR
       CALL ALGCLR(IENTRY)
       END
