CDECK  ID>, FLDCHK.
       SUBROUTINE FLDCHK
*-----------------------------------------------------------------------
*   FLDCHK - Subroutine printing the field and the potential at the
*            wire surface and at the plane surface. It checks also that
*            the Maxwell equations are satisfied.
*   VARIABLES : ERADS,E2RADS: Sum of fieldstrength at (twice) the radius
*               CHNUM       : Charge calculated from the E-field
*               VRADS,V2RADS: Sum of potential at (twice) the radius
*               TABLE       : Is used for extrapolating to the r=d/2
*               ..HIST      : Histogram's for the 'Maxwell' option
*               DVDX ETC    : Derivatives, self explanatory
*               LPLCHK      : Checking potentials at the plane surface
*               LMWCHK      : Checking that the Maxwell equations are
*                             satisfied
*               LSWCHK      : Checking the field at the s-wire surface
*               LTUCHK      : Check the field at the tube surface.
*   (Last changed on 13/12/07.)
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
       DOUBLE PRECISION WGT,FPRMAT,
     -      FPROJ,FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX,GYBOX,GZBOX
       REAL PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM
       INTEGER NLINED,NGRIDX,NGRIDY,ITRTYP,NTRLIN,NTRSAM,INDPOS,NCTRW,
     -      NTRFLX,NINORD,
     -      NCPNAM,NCXLAB,NCYLAB,NCFPRO,IPRMAT,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,ITFSRM,NTRERR
       LOGICAL LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG,LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       COMMON /PARMS / WGT(MXLIST),FPRMAT(3,3),
     -      FPROJ(3,3),FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX(12),GYBOX(12),GZBOX(12),
     -      PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM,
     -      INDPOS(11000),IPRMAT(3),NCTRW,NCPNAM,
     -      ITRTYP,NTRLIN,NTRSAM,NTRFLX,ITFSRM,NTRERR(10),
     -      NLINED,NINORD,NGRIDX,NGRIDY,NCXLAB,NCYLAB,NCFPRO,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,
     -      LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG(10),LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       CHARACTER*80 PARTID,PXLAB,PYLAB,PROLAB
       CHARACTER*10 PNAME
       CHARACTER*5  PRVIEW
       CHARACTER*(MXCHAR) FCNTRW
       COMMON /PARCHR/ PARTID,FCNTRW,PNAME,PXLAB,PYLAB,PROLAB,PRVIEW
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL DVDX(10),DVDY(10),DVDZ(10),DIVE(10),DIVB(10),PHVECT(20),
     -      ETVECT(20),
     -      VTVECT(20),EXVECT(20),EYVECT(20),EZVECT(20),TABLE(5,5),
     -      XPL(MXLIST),YPL(MXLIST),EXPL1(MXLIST),EXPL2(MXLIST),
     -      EYPL1(MXLIST),EYPL2(MXLIST),VPL1(MXLIST),VPL2(MXLIST),
     -      EX,EY,EZ,BX,BY,BZ,EXX1,EXX2,EXY1,EXY2,EXZ1,EXZ2,
     -      EYX1,EYX2,EYY1,EYY2,EYZ1,EYZ2,EZX1,EZX2,EZY1,EZY2,
     -      EZZ1,EZZ2,BXX1,BXX2,BXY1,BXY2,BXZ1,BXZ2,
     -      BYX1,BYX2,BYY1,BYY2,BYZ1,BYZ2,BZX1,BZX2,BZY1,BZY2,
     -      BZZ1,BZZ2,DX,DY,DZ,EPSWIR,EPSMXW,EPSR,STEPA,STEPB,XPRT,YPRT,
     -      ERADS,E2RADS,VRADS,V2RADS,CHNUM,CHERR,SURFTS,SURFTH,ANG,
     -      EX1,EX2,EY1,EY2,EZ1,EZ2,V1,V2,ETOT,ETOT1,ETOT2,ETOT3,ETOT4,
     -      ETOT5,ETOT6,ERSUM,EPSUM,V1SUM,PHI,BTOT,VOLT,
     -      V2SUM,E2SUM,RLOC,PHIPRO,ER,EPHI,ERCHK,DR,VX1,VX2,VY1,VY2,
     -      VZ1,VZ2,ESUM,XRNDM,YRNDM,ZRNDM,RRNDM,XPOS,YPOS,ZPOS,
     -      VT,AUX,RNDM
       INTEGER ISIZ(1),IDIM(1),NCHA,I,II,J,JJ,NWORD,INEXT,NDATA,
     -      ITAB,JTAB,NC,
     -      KTAB,NRNDM,IHISEX,IHISEY,IHISEZ,IHISDE,IHISDB,NENTEX,
     -      NENTEY,NENTEZ,NENTDE,NENTDB,IMAX,JMAX,NCHAR,
     -      ILOC,ILOC1,ILOC2,ILOC3,ILOC4,ILOC5,ILOC6,ILOC7,
     -      INPTYP,INPCMP,IFAIL,IFAIL1,IFAIL2,IFAIL3,IFAIL4,IFAIL5,
     -      IANG
       CHARACTER*133 INFILE
       LOGICAL LPLCHK,LMWCHK,LSWCHK,LMTCHK,LTUCHK,LCHCHK,LKEEP,
     -      FLAGEX(10),FLAGEY(10),FLAGEZ(10),FLAGDE(10),FLAGDB(10),
     -      LMWPRT,LMWPLT
       EXTERNAL INPCMP,INPTYP,RNDM
       SAVE NCHA,EPSWIR,EPSMXW,LMWPRT,LMWPLT
       DATA NCHA/100/,EPSWIR/1.0E-5/,EPSMXW/1.0E-3/,
     -      LMWPRT/.TRUE./,LMWPLT/.TRUE./
*** Define some output formats.
1010   FORMAT('1 Table of the field at the surface of wire ',I3/
     -        '  ============================================='//
     -        '  Wire type                    : ',A1/
     -        '  The wire is located at       : (',F9.2,',',F9.2,')'/
     -        '  The wire potential is        : ',F10.2,' [V]'/
     -        '  SETUP calculated a charge of : ',F10.2//
     -        '      Angle  Surface field Field at 2*rad',
     -        '   Surface pot.  Pot. at 2*rad  Surface angle'/
     -        '   [degree]         [V/cm]         [V/cm]',
     -        '            [V]            [V]       [degree]'/)
1040   FORMAT('1 Table of the field at the surface of plane ',I3,/,
     -        '  ==============================================',//,
     -        '  ',A1,'-coordinate                 : ',F10.2/
     -        '  Potential as specified       : ',F10.2,' [V]',/)
1050   FORMAT('1 Table of the field at the surface of the tube ',/,
     -        '  ==============================================',//,
     -        '  Radius                       : ',F10.2/
     -        '  Potential as specified       : ',F10.2,' [V]',/)
1060   FORMAT('1 FIELD-CHECK',109X,'part ',I1,'.',I1/122X,
     -        '========'//' y        x:',10(F10.2,2X:))
1100   FORMAT('1 Numerical check of the Maxwell relations for the',
     -        ' fields used by the program'/
     -        '  ================================================',
     -        '==========================='//
     -        '  The data will be printed in blocks of 10 by 10',
     -        ' points, in the format '//
     -        '  dV/dx + Ex                                 [V/cm]'/
     -        '  dV/dy + Ey                                 [V/cm]'/
     -        '  dV/dz + Ez                                 [V/cm]'/
     -        '  Divergence of the electric field           [V/cm]')
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE FLDCHK ///'
*** Find out which options have been selected.
       LSWCHK=.FALSE.
       LPLCHK=.FALSE.
       LMWCHK=.FALSE.
       LMTCHK=.FALSE.
       LTUCHK=.FALSE.
       LCHCHK=.FALSE.
       LKEEP=.FALSE.
       CALL INPNUM(NWORD)
       INEXT=2
       DO 70 I=2,NWORD
       IF(I.LT.INEXT)GOTO 70
       IF(INPCMP(I,'D#IELECTRICA').NE.0)THEN
            IF(NXMATT.GE.0.OR.NYMATT.GE.0)THEN
                 LMTCHK=.TRUE.
            ELSE
                 CALL INPMSG(I,'no dielectrica in the cell.   ')
            ENDIF
       ELSEIF(INPCMP(I,'NOD#IELECTRICA').NE.0)THEN
            LMTCHK=.FALSE.
       ELSEIF(INPCMP(I,'P#LANES').NE.0)THEN
            IF(YNPLAN(1).OR.YNPLAN(2).OR.YNPLAN(3).OR.YNPLAN(4))THEN
                 LPLCHK=.TRUE.
            ELSE
                 CALL INPMSG(I,'the cell does not have planes.')
            ENDIF
       ELSEIF(INPCMP(I,'NOP#LANES').NE.0)THEN
            LPLCHK=.FALSE.
       ELSEIF(INPCMP(I,'T#UBE').NE.0)THEN
            IF(TUBE)THEN
                 LTUCHK=.TRUE.
            ELSE
                 CALL INPMSG(I,'the cell does not have a tube.')
            ENDIF
       ELSEIF(INPCMP(I,'NOT#UBE').NE.0)THEN
            LTUCHK=.FALSE.
       ELSEIF(INPCMP(I,'W#IRES').NE.0)THEN
            LSWCHK=.TRUE.
       ELSEIF(INPCMP(I,'NOW#IRES').NE.0)THEN
            LSWCHK=.FALSE.
       ELSEIF(INPCMP(I,'CH#ARGES').NE.0)THEN
            LCHCHK=.TRUE.
       ELSEIF(INPCMP(I,'NOCH#ARGES').NE.0)THEN
            LCHCHK=.FALSE.
       ELSEIF(INPCMP(I,'M#AXWELL').NE.0)THEN
            LMWCHK=.TRUE.
       ELSEIF(INPCMP(I,'NOM#AXWELL').NE.0)THEN
            LMWCHK=.FALSE.
       ELSEIF(INPCMP(I,'F#ULL')+INPCMP(I,'A#LL').NE.0)THEN
            LPLCHK=YNPLAX.OR.YNPLAY
            LSWCHK=NSW.GT.0
            LMWCHK=.TRUE.
            LMTCHK=.TRUE.
            LTUCHK=TUBE
            LCHCHK=N3D.GT.0
*   The BINS keyword.
       ELSEIF(INPCMP(I,'B#INS').NE.0)THEN
            IF(I+1.GT.NWORD.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'Has one integer as argument.  ')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL)
                 CALL INPRDI(I+1,NCHAR,MXCHA)
                 IF(NCHAR.LE.1.OR.NCHAR.GT.MXCHA)THEN
                      CALL INPMSG(I+1,'Inacceptable number of bins.  ')
                 ELSE
                      NCHA=NCHAR
                 ENDIF
            ENDIF
            INEXT=I+2
*   The differentation epsilon for the wires.
       ELSEIF(INPCMP(I,'EPS#ILON-W#IRES').NE.0)THEN
            IF(I+1.GT.NWORD.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Has one real as argument.     ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL)
                 CALL INPRDR(I+1,EPSR,EPSWIR)
                 IF(EPSR.LE.0.0)THEN
                      CALL INPMSG(I+1,'Epsilon must be larger than 0.')
                 ELSE
                      EPSWIR=EPSR
                 ENDIF
            ENDIF
            INEXT=I+2
*   The differentation epsilon for Maxwell.
       ELSEIF(INPCMP(I,'EPS#ILON-M#AXWELL').NE.0)THEN
            IF(I+1.GT.NWORD.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Has one real as argument.     ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL)
                 CALL INPRDR(I+1,EPSR,EPSMXW)
                 IF(EPSR.LE.0.0)THEN
                      CALL INPMSG(I+1,'Epsilon must be larger than 0.')
                 ELSE
                      EPSMXW=EPSR
                 ENDIF
            ENDIF
            INEXT=I+2
*   Print and plot results.
       ELSEIF(INPCMP(I,'PR#INT').NE.0)THEN
            LMWPRT=.TRUE.
       ELSEIF(INPCMP(I,'NOPR#INT').NE.0)THEN
            LMWPRT=.FALSE.
       ELSEIF(INPCMP(I,'PL#OT').NE.0)THEN
            LMWPLT=.TRUE.
       ELSEIF(INPCMP(I,'NOPL#OT').NE.0)THEN
            LMWPLT=.FALSE.
*   Keep results or not.
       ELSEIF(INPCMP(I,'KEEP-#RESULTS').NE.0)THEN
            LKEEP=.TRUE.
       ELSEIF(INPCMP(I,'NOKEEP-#RESULTS').NE.0)THEN
            LKEEP=.FALSE.
*   Invalid keyword.
       ELSE
            CALL INPMSG(I,'the option is not known.      ')
       ENDIF
70     CONTINUE
       CALL INPERR
*** Check that at least one of the options is on.
       IF(.NOT.(LPLCHK.OR.LSWCHK.OR.LMWCHK.OR.LMTCHK.OR.
     -      LTUCHK.OR.LCHCHK))THEN
            PRINT *,' !!!!!! FLDCHK WARNING : To obtain output from'//
     -           ' CHECK, select at least one of the'
            PRINT *,'                         options (CHARGES,'//
     -           ' DIELECTRICA, MAXWELL, PLANES, TUBE, WIRES or FULL).'
            RETURN
       ENDIF
*** Handle the 'WIRE' option.
       IF(LSWCHK)THEN
            DO 60 I=1,NWIRE
*   Skip non sense wires.
            IF(INDSW(I).EQ.0)GOTO 60
*   Prepare the extrapolation stepsizes for this wire.
            STEPB=0.25*(1.0/(0.5+EPSWIR*MAX(ABS(X(I)),ABS(Y(I)),
     -           D(I)/2.0)/D(I))-1.0)
            STEPA=1.0+5.0*STEPB
            IF(LDEBUG)PRINT *,' ++++++ FLDCHK DEBUG   : The table'//
     -           ' points are at D/(',STEPA,' - I * ',STEPB,')'
*   Extrapolation is impossible if STEPB .LE. 0 (numerically unstable).
            IF(STEPB.LE.0)THEN
                 PRINT *,' !!!!!! FLDCHK WARNING : The field near the'//
     -                ' surface of wire ',I,' can not be calculated'
                 PRINT *,'                         to a reasonable'//
     -                ' accuracy with single precision arithmetic.'
                 GOTO 60
            ENDIF
*   Print a suitable heading.
            XPRT=X(I)
            YPRT=Y(I)
            IF(POLAR)CALL CFMRTP(XPRT,YPRT,XPRT,YPRT,1)
            WRITE(LUNOUT,1010) I,WIRTYP(I),XPRT,YPRT,V(I),E(I)
*   Make a table of the field at the wire surface.
            ERADS =0.0
            E2RADS=0.0
            VRADS =0.0
            V2RADS=0.0
            CHNUM =0.0
            SURFTS=0.0
            NDATA =0
*   Loop over the angle around the wire.
            DO 50 IANG=1,10
            ANG=REAL(IANG-1)*1.9*PI/9.0
*   Set up a table for the extrapolation.
            DO 10 ITAB=1,5
            TABLE(1,ITAB)=D(I)/(STEPA-REAL(ITAB)*STEPB)
            XPOS=X(I)+COS(ANG)*TABLE(1,ITAB)
            YPOS=Y(I)+SIN(ANG)*TABLE(1,ITAB)
            CALL EFIELD(XPOS,YPOS,0.0,TABLE(2,ITAB),TABLE(3,ITAB),EZ,
     -           TABLE(4,ITAB),TABLE(5,ITAB),1,ILOC)
            IF(ILOC.NE.0)THEN
                 IF(ILOC.GT.0)WRITE(LUNOUT,'(1X,F10.1,'' Leaving'',
     -                '' the wire at this angle, you enter wire '',
     -                I3,'' very soon; no data printed.'')')
     -                180*ANG/PI,ILOC
                 IF(ILOC.LT.0)WRITE(LUNOUT,'(1X,F10.1,'' Leaving'',
     -                '' the wire at this angle, you get outside'',
     -                '' a plane very soon; no data printed.'')')
     -                180*ANG/PI
                 GOTO 50
            ENDIF
            IF(POLAR)THEN
                 TABLE(2,ITAB)=TABLE(2,ITAB)/EXP(XPOS)
                 TABLE(3,ITAB)=TABLE(3,ITAB)/EXP(XPOS)
                 TABLE(4,ITAB)=TABLE(4,ITAB)/EXP(XPOS)
            ENDIF
            IF(TABLE(4,ITAB).EQ.0)PRINT *,' !!!!!! FLDCHK WARNING :'//
     -           ' Field zero at ITAB=',ITAB,' (program bug) ;'//
     -           ' extrapolation probably incorrect'
10          CONTINUE
*   Loop over the quantities to be extrapolated.
            DO 40 KTAB=2,5
*   Extrapolate using Neville polynomial extrapolation.
            DO 30 ITAB=1,5
            DO 20 JTAB=ITAB-1,1,-1
            TABLE(KTAB,JTAB)=TABLE(KTAB,JTAB+1)+
     -           (TABLE(KTAB,JTAB+1)-TABLE(KTAB,JTAB))*
     -           (0.5*D(I)-TABLE(1,ITAB))/(TABLE(1,ITAB)-TABLE(1,JTAB))
20          CONTINUE
30          CONTINUE
40          CONTINUE
*   Add new values at r and at 2 r to the sum.
            NDATA=NDATA+1
            ERADS=ERADS+TABLE(4,1)
            VRADS=VRADS+TABLE(5,1)
            E2RADS=E2RADS+TABLE(4,5)
            V2RADS=V2RADS+TABLE(5,5)
*   Store the results for a save later.
            PHVECT(NDATA)=ANG
            EXVECT(NDATA)=TABLE(2,1)
            EYVECT(NDATA)=TABLE(3,1)
            ETVECT(NDATA)=TABLE(4,1)
            VTVECT(NDATA)=TABLE(5,1)
*   Compute radial component of the electric field.
            CHNUM=CHNUM+TABLE(2,5)*COS(ANG)+TABLE(3,5)*SIN(ANG)
*   Compute the angle at the surface of the wire.
            SURFTH=TABLE(2,1)*COS(ANG)+TABLE(3,1)*SIN(ANG)
            IF(TABLE(4,1).EQ.0.0)SURFTH=1.0
            IF(TABLE(4,1).NE.0.0)SURFTH=SURFTH/TABLE(4,1)
            IF(ABS(SURFTH).GT.1.0)SURFTH=1.0
            SURFTH=90.0+(180.0/PI)*ACOS(SURFTH)
            SURFTS=SURFTS+SURFTH
*   Print values for this angle.
            WRITE(LUNOUT,'(1X,F10.1,5F15.4)')
     -           180*ANG/PI,TABLE(4,1),TABLE(4,5),
     -           TABLE(5,1),TABLE(5,5),SURFTH
50          CONTINUE
*   Check data has been collected.
            IF(NDATA.EQ.0)THEN
                 WRITE(LUNOUT,'(/''  No data collected for this'',
     -                '' wire; no averages or check sums.''/)')
                 GOTO 60
            ENDIF
*   Print averages.
            WRITE(LUNOUT,'(''0  Averages'',5F15.4,/)')
     -           ERADS/NDATA,E2RADS/NDATA,
     -           VRADS/NDATA,V2RADS/NDATA,SURFTS/NDATA
*   Print check-charge.
            CHNUM=CHNUM*D(I)/NDATA
            IF(POLAR)CHNUM=CHNUM*EXP(X(I))
            IF(E(I).EQ.0.OR.CHNUM.EQ.0)THEN
                 CHERR=0.0
            ELSE
                 CHERR=100.0*ABS((CHNUM-E(I))/E(I))
            ENDIF
            WRITE(LUNOUT,'(/''  Charge calculated using the electric'',
     -           '' field '',E10.3,'' (relative error '',E10.3,
     -           ''%)''/)') CHNUM,CHERR
*   Save the data if required, format the wire number.
            IF(LKEEP)THEN
                 CALL OUTFMT(REAL(I),2,INFILE,NC,'LEFT')
                 ISIZ(1)=NDATA
                 IDIM(1)=20
                 CALL MATSAV(EXVECT,1,IDIM,ISIZ,
     -                'EX_'//INFILE(1:NC),IFAIL1)
                 CALL MATSAV(EYVECT,1,IDIM,ISIZ,
     -                'EY_'//INFILE(1:NC),IFAIL2)
                 CALL MATSAV(ETVECT,1,IDIM,ISIZ,
     -                'E_'//INFILE(1:NC),IFAIL3)
                 CALL MATSAV(VTVECT,1,IDIM,ISIZ,
     -                'V_'//INFILE(1:NC),IFAIL4)
                 CALL MATSAV(PHVECT,1,IDIM,ISIZ,
     -                'PHI_'//INFILE(1:NC),IFAIL5)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0.AND.
     -                IFAIL4.EQ.0.AND.IFAIL5.EQ.0)THEN
                      PRINT *,' ------ FLDCHK MESSAGE : Saving'//
     -                     ' EX_'//INFILE(1:NC)//', EY_'//INFILE(1:NC)//
     -                     ', E_'//INFILE(1:NC)//', V_'//INFILE(1:NC)//
     -                     ' and PHI_'//INFILE(1:NC)//' as surface'//
     -                     ' field on wire '//INFILE(1:NC)//'.'
                 ELSE
                      PRINT *,' !!!!!! FLDCHK WARNING : Unable to'//
     -                     ' save the surface field of wire '//
     -                     INFILE(1:NC)//'.'
                 ENDIF
            ENDIF
*   Next wire,
60          CONTINUE
*   End of this step, register amount of CPU time.
            CALL TIMLOG('Check: field on the wire-surface:       ')
       ENDIF
*** Check of the planes.
       IF(LPLCHK)THEN
            DO 120 I=1,4
            IF(.NOT.YNPLAN(I))GOTO 120
*   Print a suitable heading.
            IF(.NOT.POLAR)THEN
                 IF(I.LE.2)WRITE(LUNOUT,1040) I,'X',COPLAN(I),VTPLAN(I)
                 IF(I.GE.3)WRITE(LUNOUT,1040) I,'Y',COPLAN(I),VTPLAN(I)
            ELSE
                 IF(I.LE.2)WRITE(LUNOUT,1040)
     -                I,'R',EXP(COPLAN(I)),VTPLAN(I)
                 IF(I.GE.3)WRITE(LUNOUT,1040)
     -                I,'P',180.0*COPLAN(I)/PI,VTPLAN(I)
            ENDIF
            IF(I.LE.2)THEN
                 IF(POLAR)THEN
                      WRITE(LUNOUT,'('' phi-coord.       V inside'',
     -                     ''      V outside      Er inside     E'',
     -                     ''r outside''/)')
                 ELSE
                      WRITE(LUNOUT,'(''   y-coord.         V left'',
     -                     ''        V right        Ex left      '',
     -                     '' Ex right''/)')
                 ENDIF
                 DO 100 J=0,10
                 CALL EFIELD(COPLAN(I)-(XMAX-XMIN)/1000.0,
     -                YMIN+J*(YMAX-YMIN)/10.0,0.0,
     -                EX1,EY1,EZ1,ETOT1,V1,1,ILOC)
                 CALL EFIELD(COPLAN(I)+(XMAX-XMIN)/1000.0,
     -                YMIN+J*(YMAX-YMIN)/10.0,0.0,
     -                EX2,EY2,EZ2,ETOT2,V2,1,ILOC)
                 IF(POLAR)THEN
                      EX1=EX1/EXP(COPLAN(I)-(XMAX-XMIN)/1000.0)
                      EX2=EX2/EXP(COPLAN(I)+(XMAX-XMIN)/1000.0)
                 ENDIF
                 IF(POLAR)WRITE(LUNOUT,'(1X,F10.1,4F15.4)')
     -                180.0*(YMIN+J*(YMAX-YMIN)/10.0)/PI,V1,V2,EX1,EX2
                 IF(.NOT.POLAR)WRITE(LUNOUT,'(1X,F10.1,4F15.4)')
     -                YMIN+J*(YMAX-YMIN)/10.0,V1,V2,EX1,EX2
100              CONTINUE
            ELSE
                 IF(POLAR)THEN
                      WRITE(LUNOUT,'(''   r-coord.        V above'',
     -                     ''        V under     Ephi above    '',
     -                     '' Ephi under''/)')
                 ELSE
                      WRITE(LUNOUT,'(''   x coord.        V above'',
     -                     ''        V under       Ey above      '',
     -                     '' Ey under''/)')
                 ENDIF
                 DO 110 J=0,10
                 IF(POLAR)XPRT=LOG(EXP(XMIN)+
     -                J*(EXP(XMAX)-EXP(XMIN))/10.0)
                 IF(.NOT.POLAR)XPRT=XMIN+J*(XMAX-XMIN)/10.0
                 CALL EFIELD(XPRT,COPLAN(I)+(YMAX-YMIN)/1000.0,0.0,
     -                EX1,EY1,EZ1,ETOT1,V1,1,ILOC)
                 CALL EFIELD(XPRT,COPLAN(I)-(YMAX-YMIN)/1000.0,0.0,
     -                EX2,EY2,EZ2,ETOT2,V2,1,ILOC)
                 IF(POLAR)THEN
                      EY1=EY1/EXP(XPRT)
                      EY2=EY2/EXP(XPRT)
                 ENDIF
                 IF(POLAR)WRITE(LUNOUT,'(1X,F10.1,4F15.4)')
     -                EXP(XPRT),V1,V2,EY1,EY2
                 IF(.NOT.POLAR)WRITE(LUNOUT,'(1X,F10.1,4F15.4)')
     -                XPRT,V1,V2,EY1,EY2
110              CONTINUE
            ENDIF
120         CONTINUE
*   Register the amount of CPU time spent on this operation.
            CALL TIMLOG('Check: field on the planes:             ')
       ENDIF
*** Check of the tube.
       IF(LTUCHK.AND..NOT.TUBE)THEN
            PRINT *,' !!!!!! FLDCHK WARNING : Tube checking requested'//
     -           ' but the cell has no tube.'
       ELSEIF(LTUCHK)THEN
*   Print a heading.
            WRITE(LUNOUT,1050) COTUBE,VTTUBE
            WRITE(LUNOUT,'(''        phi     V inside    V outside'',
     -           ''    Er inside  Ephi inside    E outside''/
     -                     ''  [degrees]          [V]          [V]'',
     -           ''       [V/cm]       [V/cm]       [V/cm]''/)')
*   Summing variables.
            ERSUM=0.0
            EPSUM=0.0
            V1SUM=0.0
            V2SUM=0.0
            E2SUM=0.0
*   Angular loop.
            DO 130 J=1,25
            IF(NTUBE.GT.2)THEN
                 PHI=REAL(J-1)*2*PI/REAL(25*NTUBE)
                 IF(COS(PI/REAL(NTUBE)-PHI).EQ.0)GOTO 130
                 RLOC=COTUBE*COS(PI/REAL(NTUBE))/COS(PI/REAL(NTUBE)-PHI)
                 PHIPRO=PI/REAL(NTUBE)
            ELSE
                 PHI=REAL(J-1)*2*PI/REAL(25)
                 RLOC=COTUBE
                 PHIPRO=PHI
            ENDIF
            CALL EFIELD(0.999*RLOC*COS(PHI),0.999*RLOC*SIN(PHI),
     -           0.0,EX1,EY1,EZ1,ETOT1,V1,1,ILOC)
            CALL EFIELD(1.001*RLOC*COS(PHI),1.001*RLOC*SIN(PHI),
     -           0.0,EX2,EY2,EZ2,ETOT2,V2,1,ILOC)
            ER  = COS(PHIPRO)*EX1+SIN(PHIPRO)*EY1
            EPHI=-SIN(PHIPRO)*EX1+COS(PHIPRO)*EY1
            ERSUM=ERSUM+ER
            EPSUM=EPSUM+EPHI
            V1SUM=V1SUM+V1
            V2SUM=V2SUM+V2
            E2SUM=E2SUM+ETOT2
            WRITE(LUNOUT,'(1X,F10.1,5(1X,F12.5))')
     -           180*PHI/PI,V1,V2,ER,EPHI,ETOT2
130         CONTINUE
*   Print averages.
            WRITE(LUNOUT,'(/2X,''Average: '',5(1X,F12.5))')
     -           V1SUM/25.0,V2SUM/25.0,ERSUM/25.0,
     -           EPSUM/25.0,E2SUM/25.0
*   Print summary.
            IF(NTUBE.GT.2)THEN
                 ERSUM=ERSUM*0.999*SQRT(2*(1-COS(2*PI/REAL(NTUBE))))*
     -                COTUBE*NTUBE/25.0
            ELSE
                 ERSUM=ERSUM*0.999*COTUBE/25.0
            ENDIF
            ERCHK=0.0
            DO 140 J=1,NWIRE
            IF(MTUBE.EQ.0.OR.X(J)**2+Y(J)**2.LT.D(J)**2/4)THEN
                 ERCHK=ERCHK+E(J)
            ELSE
                 ERCHK=ERCHK+E(J)*MTUBE
            ENDIF
140         CONTINUE
            WRITE(LUNOUT,'(/''  Charge check: Tube      : '',E12.5/16X,
     -           ''Wires     : '',E12.5)') ERSUM,ERCHK
*   Register the amount of CPU time spent on this operation.
            CALL TIMLOG('Check: field on the tube surface:       ')
       ENDIF
*** Check that the charges match the electric field around them.
       IF(LCHCHK.AND.N3D.EQ.0)THEN
            PRINT *,' !!!!!! FLDCHK WARNING : Charge checking has been',
     -           ' requested but there are no charges.'
       ELSEIF(LCHCHK)THEN
*   Print a header.
            WRITE(LUNOUT,'(''1 Check of the three dimensional charges''/
     -           ''  ======================================''//
     -           ''      No    Charge given    Charge found'')')
*   Loop over the charges.
            DO 410 I=1,N3D
*   Determine for each of the charges a radius.
            DR=1E-4*(1+ABS(X3D(I))+ABS(Y3D(I))+ABS(Z3D(I)))
*   Check that there are no other charges nearby.
            DO 420 J=1,N3D
            IF(I.EQ.J)GOTO 420
            IF((X3D(I)-X3D(J))**2+(Y3D(I)-Y3D(J))**2+
     -           (Z3D(I)-Z3D(J))**2.LT.DR**2)THEN
                 PRINT *,' !!!!!! FLDCHK WARNING : Charge ',J,' is',
     -                ' located too close to charge ',I,' to be able',
     -                ' to verify the charges.'
                 GOTO 410
            ENDIF
420         CONTINUE
*   Check that there are no wires nearby.
            DO 430 J=1,NWIRE
            IF((X3D(I)-X(J))**2+(Y3D(I)-Y(J))**2.LT.DR**2)THEN
                 PRINT *,' !!!!!! FLDCHK WARNING : Wire ',J,' is',
     -                ' located too close to charge ',I,' to be able',
     -                ' to verify the charges.'
                 GOTO 410
            ENDIF
430         CONTINUE
*   Determine the flow out of the sphere by MC integration.
            ESUM=0.0
            NRNDM=0
            DO 440 J=1,1000
*   Generate a random point on the unit circle.
            XRNDM=-1+2*RNDM(1*J)
            YRNDM=-1+2*RNDM(2*J)
            ZRNDM=-1+2*RNDM(3*J)
            RRNDM=SQRT(XRNDM**2+YRNDM**2+ZRNDM**2)
            IF(RRNDM.EQ.0)GOTO 440
            XRNDM=DR*XRNDM/RRNDM
            YRNDM=DR*YRNDM/RRNDM
            ZRNDM=DR*ZRNDM/RRNDM
*   Evaluate the field at that point.
            CALL EFIELD(X3D(I)+XRNDM,Y3D(I)+YRNDM,Z3D(I)+ZRNDM,
     -           EX,EY,EZ,ETOT,VOLT,0,ILOC)
            IF(ILOC.NE.0)GOTO 440
            NRNDM=NRNDM+1
*   Project the field onto the out-bound vector.
            ESUM=ESUM+(EX*XRNDM+EY*YRNDM+EZ*ZRNDM)/DR
*   Next MC cycle.
440         CONTINUE
*   Print results for this charge.
            IF(NRNDM.EQ.0)THEN
                 WRITE(LUNOUT,'(2X,I6,1X,''No data collected.'')') I
            ELSE
                 WRITE(LUNOUT,'(2X,I6,1X,E15.8,1X,E15.8)')
     -                I,E3D(I),ESUM*DR**2/NRNDM
            ENDIF
*   Next charge.
410         CONTINUE
       ENDIF
*** Check that E and V are consistent ('MAXWELL' option).
       IF(LMWCHK)THEN
*   Print a suitable heading.
            IF(LMWPRT)THEN
                 WRITE(LUNOUT,1100)
                 IF(MAGOK)THEN
                      WRITE(LUNOUT,'(''  Divergence of the'',
     -                     '' magnetic field [V microsec/cm2]'')')
                 ELSE
                      WRITE(LUNOUT,'(''  Potential        '',
     -                     ''                             [V]'')')
                 ENDIF
                 IF(POLAR)WRITE(LUNOUT,
     -                '(''  WARNING: The coordinates are internal.'')')
            ENDIF
*   Allocate histograms.
            CALL HISADM('ALLOCATE',IHISEX,NCHA,0.0,0.0,.TRUE.,IFAIL1)
            CALL HISADM('ALLOCATE',IHISEY,NCHA,0.0,0.0,.TRUE.,IFAIL2)
            CALL HISADM('ALLOCATE',IHISEZ,NCHA,0.0,0.0,.TRUE.,IFAIL3)
            CALL HISADM('ALLOCATE',IHISDE,NCHA,0.0,0.0,.TRUE.,IFAIL4)
            CALL HISADM('ALLOCATE',IHISDB,NCHA,0.0,0.0,.TRUE.,IFAIL5)
            IF(IFAIL1+IFAIL2+IFAIL3+IFAIL4+IFAIL5.NE.0)THEN
                 PRINT *,' !!!!!! FLDCHK WARNING : Unable to allocate'//
     -                ' all required histograms.'
            ENDIF
            NENTEX=0
            NENTEY=0
            NENTEZ=0
            NENTDE=0
            NENTDB=0
*   Loop over the whole area.
            ZPOS=0.0
            DO 240 JJ=0,10*INT(REAL(NGRIDY-1)/10.0),10
            JMAX=MIN(NGRIDY-JJ,10)
            DO 230 II=0,10*INT(REAL(NGRIDX-1)/10.0),10
            IMAX=MIN(NGRIDX-II,10)
            IF(LMWPRT)WRITE(LUNOUT,1060) 1+II/10,1+JJ/10,
     -           (PXMIN+(PXMAX-PXMIN)*REAL(II+I-1)/REAL(NGRIDX-1),
     -           I=1,IMAX)
            IF(LMWPRT)WRITE(LUNOUT,'('' '')')
            DO 220 J=1,JMAX
            YPOS=PYMIN+(PYMAX-PYMIN)*REAL(JJ+J-1)/REAL(NGRIDY-1)
            DO 210 I=1,IMAX
            XPOS=PXMIN+(PXMAX-PXMIN)*REAL(II+I-1)/REAL(NGRIDX-1)
*   Preset the flags.
            FLAGEX(I)=.TRUE.
            FLAGEY(I)=.TRUE.
            FLAGEZ(I)=.TRUE.
            FLAGDE(I)=.TRUE.
            FLAGDB(I)=.TRUE.
*   Choose step sizes.
            DX=EPSMXW*(1.0+ABS(XPOS))
            DY=EPSMXW*(1.0+ABS(YPOS))
            DZ=EPSMXW*(1.0+ABS(ZPOS))
*   Calculate the field.
            CALL EFIELD(XPOS   ,YPOS   ,ZPOS   ,
     -           EX  ,EY  ,EZ  ,ETOT ,VT ,1,ILOC1)
            CALL EFIELD(XPOS+DX,YPOS   ,ZPOS   ,
     -           EXX1,EYX1,EZX1,ETOT1,VX1,1,ILOC2)
            CALL EFIELD(XPOS-DX,YPOS   ,ZPOS   ,
     -           EXX2,EYX2,EZX2,ETOT2,VX2,1,ILOC3)
            CALL EFIELD(XPOS   ,YPOS+DY,ZPOS   ,
     -           EXY1,EYY1,EZY1,ETOT3,VY1,1,ILOC4)
            CALL EFIELD(XPOS   ,YPOS-DY,ZPOS   ,
     -           EXY2,EYY2,EZY2,ETOT4,VY2,1,ILOC5)
            CALL EFIELD(XPOS   ,YPOS   ,ZPOS+DZ,
     -           EXZ1,EYZ1,EZZ1,ETOT5,VZ1,1,ILOC6)
            CALL EFIELD(XPOS   ,YPOS   ,ZPOS-DZ,
     -           EXZ2,EYZ2,EZZ2,ETOT6,VZ2,1,ILOC7)
            EXVECT(I)=EX
            EYVECT(I)=EY
            EZVECT(I)=EZ
            VTVECT(I)=VT
            IF(MAGOK)THEN
                 CALL BFIELD(XPOS   ,YPOS   ,ZPOS   ,
     -                BX  ,BY  ,BZ  ,BTOT)
                 CALL BFIELD(XPOS+DX,YPOS   ,ZPOS   ,
     -                BXX1,BYX1,BZX1,BTOT)
                 CALL BFIELD(XPOS-DX,YPOS   ,ZPOS   ,
     -                BXX2,BYX2,BZX2,BTOT)
                 CALL BFIELD(XPOS   ,YPOS+DY,ZPOS   ,
     -                BXY1,BYY1,BZY1,BTOT)
                 CALL BFIELD(XPOS   ,YPOS-DY,ZPOS   ,
     -                BXY2,BYY2,BZY2,BTOT)
                 CALL BFIELD(XPOS   ,YPOS   ,ZPOS+DZ,
     -                BXZ1,BYZ1,BZZ1,BTOT)
                 CALL BFIELD(XPOS   ,YPOS   ,ZPOS-DZ,
     -                BXZ2,BYZ2,BZZ2,BTOT)
            ENDIF
*   Skip histogramming if (XPOS,YPOS) lies within or near a wire.
            IF(ILOC1.NE.0.OR.ILOC2.NE.0.OR.ILOC3.NE.0.OR.ILOC4.NE.0.OR.
     -           ILOC5.NE.0.OR.ILOC6.NE.0.OR.ILOC7.NE.0)THEN
                 DVDX(I)=-EX
                 DVDY(I)=-EY
                 DVDZ(I)=-EZ
                 DIVE(I)=0.0
                 DIVB(I)=0.0
                 FLAGEX(I)=.FALSE.
                 FLAGEY(I)=.FALSE.
                 FLAGEZ(I)=.FALSE.
                 FLAGDE(I)=.FALSE.
                 FLAGDB(I)=.FALSE.
                 GOTO 210
            ENDIF
*   Calculate derivatives and divergence.
            IF((VX1-VT)*(VT-VX2).LT.0)FLAGEX(I)=.FALSE.
            DVDX(I)=(VX1-VX2)/(2*DX)
            IF((VY1-VT)*(VT-VY2).LT.0)FLAGEY(I)=.FALSE.
            DVDY(I)=(VY1-VY2)/(2*DY)
            IF((VZ1-VT)*(VT-VZ2).LT.0)FLAGEZ(I)=.FALSE.
            DVDZ(I)=(VZ1-VZ2)/(2*DZ)
            IF((EXX1-EX)*(EX-EXX2).LT.0.OR.
     -           (EYY1-EY)*(EY-EYY2).LT.0.OR.
     -           (EZZ1-EZ)*(EZ-EZZ2).LT.0)FLAGDE(I)=.FALSE.
            DIVE(I)=(EXX1-EXX2)/(2*DX)+(EYY1-EYY2)/(2*DY)+
     -           (EZZ1-EZZ2)/(2*DZ)
            IF(MAGOK)THEN
                 IF((BXX1-BX)*(BX-BXX2).LT.0.OR.
     -                (BYY1-BY)*(BY-BYY2).LT.0.OR.
     -                (BZZ1-BZ)*(BZ-BZZ2).LT.0)FLAGDB(I)=.FALSE.
                 DIVB(I)=(BXX1-BXX2)/(2*DX)+(BYY1-BYY2)/(2*DY)+
     -                (BZZ1-BZZ2)/(2*DZ)
            ENDIF
*   Fill histograms.
            IF(ABS(DVDX(I))+ABS(EX).NE.0.AND.FLAGEX(I))THEN
                 CALL HISENT(IHISEX,(DVDX(I)+EX)/(ABS(DVDX(I))+ABS(EX)),
     -                1.0)
                 NENTEX=NENTEX+1
            ENDIF
            IF(ABS(DVDY(I))+ABS(EY).NE.0.AND.FLAGEY(I))THEN
                 CALL HISENT(IHISEY,(DVDY(I)+EY)/(ABS(DVDY(I))+ABS(EY)),
     -                1.0)
                 NENTEY=NENTEY+1
            ENDIF
            IF(ABS(DVDZ(I))+ABS(EZ).NE.0.AND.FLAGEZ(I))THEN
                 CALL HISENT(IHISEZ,(DVDZ(I)+EZ)/(ABS(DVDZ(I))+ABS(EZ)),
     -                1.0)
                 NENTEZ=NENTEZ+1
            ENDIF
            IF(FLAGDE(I))THEN
                 CALL HISENT(IHISDE,DIVE(I),1.0)
                 NENTDE=NENTDE+1
            ENDIF
            IF(MAGOK.AND.FLAGDB(I))THEN
                 CALL HISENT(IHISDB,DIVB(I),1.0)
                 NENTDB=NENTDB+1
            ENDIF
210         CONTINUE
*   Print the quantities obtained.
            WRITE(INFILE,'(1X,F8.2,3X,10(F10.3,2X:))')
     -           YPOS,(DVDX(I)+EXVECT(I),I=1,IMAX)
            DO 250 I=1,IMAX
            IF(.NOT.FLAGEX(I))INFILE(1+I*12:10+I*12)=' (invalid)'
250         CONTINUE
            IF(LMWPRT)WRITE(LUNOUT,'(A)') INFILE
            WRITE(INFILE,'(12X,10(F10.3,2X:))')
     -           (DVDY(I)+EYVECT(I),I=1,IMAX)
            DO 260 I=1,IMAX
            IF(.NOT.FLAGEY(I))INFILE(1+I*12:10+I*12)=' (invalid)'
260         CONTINUE
            IF(LMWPRT)WRITE(LUNOUT,'(A)') INFILE
            WRITE(INFILE,'(12X,10(F10.3,2X:))')
     -           (DVDZ(I)+EZVECT(I),I=1,IMAX)
            DO 265 I=1,IMAX
            IF(.NOT.FLAGEZ(I))INFILE(1+I*12:10+I*12)=' (invalid)'
265         CONTINUE
            IF(LMWPRT)WRITE(LUNOUT,'(A)') INFILE
            WRITE(INFILE,'(12X,10(F10.3,2X:))') (DIVE(I),I=1,IMAX)
            DO 270 I=1,IMAX
            IF(.NOT.FLAGDE(I))INFILE(1+I*12:10+I*12)=' (invalid)'
270         CONTINUE
            IF(LMWPRT)WRITE(LUNOUT,'(A)') INFILE
            IF(MAGOK)THEN
                 WRITE(INFILE,'(12X,10(F10.3,2X:))') (DIVB(I),I=1,IMAX)
                 DO 280 I=1,IMAX
                 IF(.NOT.FLAGDB(I))INFILE(1+I*12:10+I*12)=' (invalid)'
280              CONTINUE
                 IF(LMWPRT)WRITE(LUNOUT,'(A)') INFILE
            ELSE
                 IF(LMWPRT)WRITE(LUNOUT,'(12X,10(F10.3,2X:))')
     -                (VTVECT(I),I=1,IMAX)
            ENDIF
            IF(LMWPRT)WRITE(LUNOUT,'('' '')')
220         CONTINUE
230         CONTINUE
240         CONTINUE
*   Plot the histograms.
            IF(NENTEX.GT.0.AND.LMWPLT)THEN
                 CALL HISPLT(IHISEX,
     -                '(dV/dx + Ex) / (|dV/dx| + |Ex|)',
     -                'Relative error in Ex',.TRUE.)
                 IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
                 CALL GRNEXT
                 CALL GRALOG('Relative error in Ex')
            ELSEIF(LMWPLT)THEN
                 PRINT *,' !!!!!! FLDCHK WARNING : No useable data'//
     -                ' collected for dV/dx + Ex; check epsilon.'
            ENDIF
            IF(NENTEY.GT.0.AND.LMWPLT)THEN
                 CALL HISPLT(IHISEY,
     -                '(dV/dy + Ey) / (|dV/dy| + |Ey|)',
     -                'Relative error in Ey',.TRUE.)
                 IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
                 CALL GRNEXT
                 CALL GRALOG('Relative error in Ey')
            ELSEIF(LMWPLT)THEN
                 PRINT *,' !!!!!! FLDCHK WARNING : No useable data'//
     -                ' collected for dV/dy + Ey; check epsilon.'
            ENDIF
            IF(NENTEZ.GT.0.AND.LMWPLT)THEN
                 CALL HISPLT(IHISEZ,
     -                '(dV/dz + Ez) / (|dV/dz| + |Ez|)',
     -                'Relative error in Ez',.TRUE.)
                 IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
                 CALL GRNEXT
                 CALL GRALOG('Relative error in Ez')
            ELSEIF(LMWPLT)THEN
                 PRINT *,' !!!!!! FLDCHK WARNING : No useable data'//
     -                ' collected for dV/dz + Ez; check epsilon.'
            ENDIF
            IF(NENTDE.GT.0.AND.LMWPLT)THEN
                 CALL HISPLT(IHISDE,
     -                'dEx/dx+dEy/dy+dEz/dz [V/cm2]',
     -                'Divergence of the electric field',.TRUE.)
                 IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
                 CALL GRNEXT
                 CALL GRALOG('Divergence of the E field')
            ELSEIF(LMWPLT)THEN
                 PRINT *,' !!!!!! FLDCHK WARNING : No useable data'//
     -                ' collected for div E; check epsilon.'
            ENDIF
            IF(MAGOK.AND.NENTDB.GT.0.AND.LMWPLT)THEN
                 CALL HISPLT(IHISDB,
     -                'dBz/dz + dBz/dz',
     -                'Divergence of the magnetic field',.TRUE.)
                 IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
                 CALL GRNEXT
                 CALL GRALOG('Divergence of the B field               ')
            ELSEIF(MAGOK.AND.LMWPLT)THEN
                 PRINT *,' !!!!!! FLDCHK WARNING : No useable data'//
     -                ' collected for div B; check epsilon.'
            ENDIF
*   Delete histograms.
            IF(LKEEP)THEN
                 CALL HISSAV(IHISEX,'EX_ERROR',IFAIL1)
                 CALL HISSAV(IHISEY,'EY_ERROR',IFAIL2)
                 CALL HISSAV(IHISEZ,'EZ_ERROR',IFAIL3)
                 CALL HISSAV(IHISDE,'DIV_E',IFAIL4)
                 CALL HISSAV(IHISDB,'DIV_B',IFAIL5)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0.AND.
     -                IFAIL4.EQ.0.AND.IFAIL5.EQ.0)THEN
                      PRINT *,' ------ FLDCHK MESSAGE : Maxwell'//
     -                     ' histograms saved as EX_ERROR, EY_ERROR'//
     -                     ' EZ_ERROR, DIV_E (DIV_B).'
                 ELSE
                      PRINT *,' !!!!!! FLDCHK WARNING : Error saving'//
     -                     ' the Maxwell histograms.'
                 ENDIF
            ELSE
                 CALL HISADM('DELETE',IHISEX,0,0.0,0.0,.FALSE.,IFAIL1)
                 CALL HISADM('DELETE',IHISEY,0,0.0,0.0,.FALSE.,IFAIL2)
                 CALL HISADM('DELETE',IHISEZ,0,0.0,0.0,.FALSE.,IFAIL3)
                 CALL HISADM('DELETE',IHISDE,0,0.0,0.0,.FALSE.,IFAIL4)
                 CALL HISADM('DELETE',IHISDB,0,0.0,0.0,.FALSE.,IFAIL5)
            ENDIF
*   Register the amount of cpu time spent on this operation.
            CALL TIMLOG('Check: consistency of E and V:          ')
       ENDIF
*** Look for 'DIELECTRICA' option.
       IF(LMTCHK)THEN
            IF(YNMATX)THEN
*   Prepare a comment label.
                 INFILE='Dielectric constant: '
                 CALL OUTFMT(XMATT(1,5),2,INFILE(22:),NC,'LEFT')
*   Walk along the boundary.
                 DO 300 I=1,MXLIST
                 XPL(I)=YMIN+REAL(I-1)*(YMAX-YMIN)/REAL(MXLIST-1)
                 CALL EFIELD(COMATX-1.0E-3*(1+ABS(COMATX)),XPL(I),0.0,
     -                EXPL1(I),EYPL1(I),EZ,ETOT,VPL1(I),1,ILOC1)
                 CALL EFIELD(COMATX+1.0E-3*(1+ABS(COMATX)),XPL(I),0.0,
     -                EXPL2(I),EYPL2(I),EZ,ETOT,VPL2(I),1,ILOC2)
300              CONTINUE
*   Plot the Ex ratio.
                 DO 310 I=1,MXLIST
                 IF(EXPL1(I).EQ.0.OR.EXPL2(I).EQ.0)THEN
                      YPL(I)=0.0
                 ELSE
                      YPL(I)=EXPL1(I)/EXPL2(I)
                 ENDIF
310              CONTINUE
                 CALL GRGRPH(XPL,YPL,MXLIST,'y-axis [cm]',
     -                'Ex right / Ex left','CHECKING EX')
                 AUX=XPL(2)
                 XPL(2)=XPL(MXLIST)
                 IF(XMATT(1,3).NE.0)THEN
                      YPL(1)=XMATT(1,5)
                 ELSE
                      YPL(1)=1/XMATT(1,5)
                 ENDIF
                 YPL(2)=YPL(1)
                 CALL GRATTS('COMMENT','POLYLINE')
                 CALL GPL(2,XPL,YPL)
                 XPL(2)=AUX
                 IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
                 CALL GRCOMM(3,INFILE(1:21+NC))
                 CALL GRNEXT
                 CALL GRALOG('Check of Ex on a dielectric x-boundary. ')
*   Plot the Ey ratio.
                 DO 320 I=1,MXLIST
                 IF(EYPL1(I).EQ.0.OR.EYPL2(I).EQ.0)THEN
                      YPL(I)=1.0
                 ELSE
                      YPL(I)=EYPL1(I)/EYPL2(I)
                 ENDIF
320              CONTINUE
                 CALL GRGRPH(XPL,YPL,MXLIST,'y-axis [cm]',
     -                'Ey right / Ey left','CHECKING EY')
                 AUX=XPL(2)
                 XPL(2)=XPL(MXLIST)
                 YPL(1)=1.0
                 YPL(2)=1.0
                 CALL GRATTS('COMMENT','POLYLINE')
                 CALL GPL(2,XPL,YPL)
                 XPL(2)=AUX
                 IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
                 CALL GRCOMM(3,INFILE(1:21+NC))
                 CALL GRNEXT
                 CALL GRALOG('Check of Ey on a dielectric x-boundary. ')
*   Plot the V ratio.
                 DO 330 I=1,MXLIST
                 IF(VPL1(I).EQ.0.OR.VPL2(I).EQ.0)THEN
                      YPL(I)=1.0
                 ELSE
                      YPL(I)=VPL1(I)/VPL2(I)
                 ENDIF
330              CONTINUE
                 CALL GRGRPH(XPL,YPL,MXLIST,'y-axis [cm]',
     -                'V right / V left','CHECKING V')
                 AUX=XPL(2)
                 XPL(2)=XPL(MXLIST)
                 YPL(1)=1.0
                 YPL(2)=1.0
                 CALL GRATTS('COMMENT','POLYLINE')
                 CALL GPL(2,XPL,YPL)
                 XPL(2)=AUX
                 IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
                 CALL GRCOMM(3,INFILE(1:21+NC))
                 CALL GRNEXT
                 CALL GRALOG('Check of V on a dielectric x-boundary.  ')
            ENDIF
*   Register the amount of CPU time spent on this operation.
            CALL TIMLOG('Check: dielectrica:                     ')
       ENDIF
       END
