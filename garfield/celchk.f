CDECK  ID>, CELCHK.
       SUBROUTINE CELCHK(IFAIL)
*-----------------------------------------------------------------------
*   CELCHK - Subroutine checking the wire positions, The equipotential
*            planes and the periodicity. Two planes having different
*            voltages are not allowed to have a common line, wires are
*            not allowed to be at the same position etc.
*            This routine determines also the cell-dimensions.
*   VARIABLE  : WRONG(I)   : .TRUE. if wire I will be removed
*               IPLAN.     : Number of wires with coord > than plane .
*   (Last changed on  3/ 4/10.)
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
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
       REAL EXMAP,EYMAP,EZMAP,VMAP,EWXMAP,EWYMAP,EWZMAP,VWMAP,
     -      BXMAP,BYMAP,BZMAP,
     -      XMAP,YMAP,ZMAP,XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,
     -      VMMIN,VMMAX,EPSMAT,EPSSUR,XFMOFF,YFMOFF,ZFMOFF
       INTEGER MATMAP,NMAP,NEPS,MAPORD,MAPTYP,IDRMAT,INDEWS,
     -      NWMAP
       LOGICAL MAPFLG,LMAPPL,SETAX,SETAY,SETAZ,ELMDGN,LSFDER
       CHARACTER EWSTYP
       CHARACTER*10 MATSRC
       COMMON /FLDMAP/ VMAP(MXMAP,10),VWMAP(MXMAP,10,MXWMAP),
     -      EXMAP(MXMAP,10),EYMAP(MXMAP,10),EZMAP(MXMAP,10),
     -      EWXMAP(MXMAP,10,MXWMAP),EWYMAP(MXMAP,10,MXWMAP),
     -      EWZMAP(MXMAP,10,MXWMAP),
     -      BXMAP(MXMAP,10),BYMAP(MXMAP,10),BZMAP(MXMAP,10),
     -      XMAP(MXMAP,10),YMAP(MXMAP,10),ZMAP(MXMAP,10),
     -      XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,VMMIN,VMMAX,
     -      XFMOFF,YFMOFF,ZFMOFF,
     -      EPSMAT(MXEPS),EPSSUR(MXEPS),MATMAP(MXMAP),
     -      NMAP,NEPS,MAPORD,MAPTYP,IDRMAT,INDEWS(MXWMAP),NWMAP,
     -      MAPFLG(10+4*MXWMAP),ELMDGN(MXMAP),
     -      LMAPPL,SETAX,SETAY,SETAZ,LSFDER
       COMMON /FLDCHR/ EWSTYP(MXWMAP),MATSRC
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       LOGICAL WRONG(MXWIRE),WRMATX(MXMATT),WRMATY(MXMATT),OK,
     -      SETX,SETY,SETZ,SETV,SETQ
       REAL CONEW1,CONEW2,CONEW3,CONEW4,COHLP,VTHLP,XNEW,YNEW,
     -      XPRT,YPRT,XPRTI,YPRTI,XPRTJ,YPRTJ,XSEPAR,YSEPAR,
     -      XAUX1,YAUX1,XAUX2,YAUX2,SMIN,SMAX,GAP
       DOUBLE PRECISION EPS,VOLT,CHARGE
       INTEGER IFAIL,I,J,IPLAN1,IPLAN2,IPLAN3,IPLAN4,IWIRE,NXOLD,NYOLD,
     -      IOUT,NC1,NC2,NC3,NC4,IFAIL1,NELEM,NHLP,ISOL,ISHAPE,IMAT,
     -      IBOUND
       CHARACTER*10 USER
       CHARACTER*20 STR1,STR2,STR3,STR4
       CHARACTER LABHLP
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE CELCHK ///'
       IFAIL=1
       OK=.TRUE.
*** See whether this is a field map cell.
       CALL BOOK('INQUIRE','MAP',USER,IFAIL1)
*   Unable to tell: reset the field map.
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! CELCHK WARNING : Unable to obtain'//
     -           ' field map allocation information ; assumed to'//
     -           ' be a non-field map cell.'
            OK=.FALSE.
            CALL MAPINT
*   Field map chamber: ensure that there are no other elements.
       ELSEIF(USER.EQ.'CELL'.AND.
     -      (MAPFLG(2).OR.MAPFLG(3).OR.MAPFLG(4).OR.MAPFLG(5)))THEN
            IF(TUBE)THEN
                 TUBE=.FALSE.
                 PRINT *,' !!!!!! CELCHK WARNING : Field map cell'//
     -                ' found to have a tube; tube deleted.'
                 OK=.FALSE.
            ENDIF
            IF(POLAR)THEN
                 POLAR=.FALSE.
                 PRINT *,' !!!!!! CELCHK WARNING : Field map cell'//
     -                ' found to be polar; set to Cartesian.'
                 OK=.FALSE.
            ENDIF
            IF(NWIRE.NE.0)THEN
                 NWIRE=0
                 PRINT *,' !!!!!! CELCHK WARNING : Wires found in'//
     -                ' field map cell; wires deleted.'
                 OK=.FALSE.
            ENDIF
            IF(YNPLAN(1).OR.YNPLAN(2).OR.YNPLAN(3).OR.YNPLAN(4))THEN
                 YNPLAN(1)=.FALSE.
                 YNPLAN(2)=.FALSE.
                 YNPLAN(3)=.FALSE.
                 YNPLAN(4)=.FALSE.
                 PRINT *,' !!!!!! CELCHK WARNING : Plane found in'//
     -                ' field map cell; planes deleted.'
                 OK=.FALSE.
            ENDIF
            IF(NXMATT.NE.0.OR.NYMATT.NE.0)THEN
                 NXMATT=0
                 NYMATT=0
                 PRINT *,' !!!!!! CELCHK WARNING : Dielectric slab'//
     -                ' found in a field map cell; dielectrica deleted.'
                 OK=.FALSE.
            ENDIF
            IF(NMAP.LE.0.OR..NOT.MAPFLG(1))THEN
                 PRINT *,' ###### CELCHK ERROR   : The field map has'//
     -                ' no elements or no mesh; cell rejected.'
                 RETURN
            ENDIF
            GOTO 3000
       ENDIF
*** See whether this is a solids-only cell.
       IF(BEMSET.AND.NSOLID.GT.0.AND.NWIRE.EQ.0.AND.
     -      .NOT.(YNPLAN(1).OR.YNPLAN(2).OR.YNPLAN(3).OR.YNPLAN(4)).AND.
     -      .NOT.TUBE)THEN
*   Ensure that there are some potentials or charges.
            VMIN=0.0
            VMAX=0.0
            SETV=.FALSE.
            SETQ=.FALSE.
            DO 400 ISOL=1,NSOLID
            CALL BEMVOL(ISOL,ISHAPE,IMAT,EPS,VOLT,CHARGE,IBOUND,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! CELCHK WARNING : Inquire for solid ',
     -                ISOL,' failed; solid ignored for ranges.'
                 OK=.FALSE.
                 GOTO 400
            ENDIF
            IF(IBOUND.EQ.1)THEN
                 IF(SETV)THEN
                      VMIN=MIN(VMIN,VOLT)
                      VMAX=MAX(VMAX,VOLT)
                 ELSE
                      VMIN=VOLT
                      VMAX=VOLT
                      SETV=.TRUE.
                 ENDIF
            ELSEIF(IBOUND.EQ.2.OR.IBOUND.EQ.5)THEN
                 SETQ=.TRUE.
            ENDIF
400         CONTINUE
*   Check that at least some different voltages are present.
            IF((.NOT.SETQ).AND.(VMIN.EQ.VMAX.OR..NOT.SETV))THEN
                 PRINT *,' ###### CELCHK ERROR   : All potentials'//
     -                ' of solids are identical and no charges;'//
     -                ' cell rejected.'
                 OK=.FALSE.
                 RETURN
            ENDIF
*   Skip wire processing.
            GOTO 3000
       ENDIF
*** See whether this is a cell-planes
       IF(PERZ.OR.
     -      PERMX.OR.PERMY.OR.PERMZ.OR.
     -      PERAX.OR.PERAY.OR.PERAZ.OR.
     -      PERRX.OR.PERRY.OR.PERRZ)THEN
            PRINT *,' !!!!!! CELCHK WARNING : Inapplicable'//
     -           ' periodicity found in a wire/plane cell.'
            PERZ=.FALSE.
            PERMX=.FALSE.
            PERMY=.FALSE.
            PERMZ=.FALSE.
            PERAX=.FALSE.
            PERAY=.FALSE.
            PERAZ=.FALSE.
            PERRX=.FALSE.
            PERRY=.FALSE.
            PERRZ=.FALSE.
            OK=.FALSE.
       ENDIF
*** Checks on the planes, first move the x planes to the basic cell.
       IF(PERX)THEN
            CONEW1=COPLAN(1)-SX*ANINT(COPLAN(1)/SX)
            CONEW2=COPLAN(2)-SX*ANINT(COPLAN(2)/SX)
*   Check that they are not one on top of the other.
            IF(YNPLAN(1).AND.YNPLAN(2).AND.CONEW1.EQ.CONEW2)THEN
                 IF(CONEW1.GT.0.0)THEN
                      CONEW1=CONEW1-SX
                 ELSE
                      CONEW2=CONEW2+SX
                 ENDIF
            ENDIF
*   Print some warnings if the planes have been moved.
            IF((CONEW1.NE.COPLAN(1).AND.YNPLAN(1)).OR.
     -           (CONEW2.NE.COPLAN(2).AND.YNPLAN(2)))
     -           PRINT *,' ------ CELCHK MESSAGE : The planes in x or'//
     -                ' r are moved to the basic period; this should'//
     -                ' not affect the results.'
            COPLAN(1)=CONEW1
            COPLAN(2)=CONEW2
*   Two planes should now be separated by SX, cancel PERX if not.
            IF(YNPLAN(1).AND.YNPLAN(2).AND.
     -           ABS(COPLAN(2)-COPLAN(1)).NE.SX)THEN
                 PRINT *,' !!!!!! CELCHK WARNING : The separation of'//
     -                ' the x or r planes does not match the period;'//
     -                ' the periodicity is cancelled.'
                 PERX=.FALSE.
                 OK=.FALSE.
            ENDIF
*   If there are two planes left, they should have identical V's.
            IF(YNPLAN(1).AND.YNPLAN(2).AND.VTPLAN(1).NE.VTPLAN(2))THEN
                 PRINT *,' !!!!!! CELCHK WARNING : The voltages of'//
     -                ' the two x (or r) planes differ;'//
     -                ' the periodicity is cancelled.'
                 PERX=.FALSE.
                 OK=.FALSE.
            ENDIF
       ENDIF
**  Idem for the y or r planes: move them to the basic period.
       IF(PERY)THEN
            CONEW3=COPLAN(3)-SY*ANINT(COPLAN(3)/SY)
            CONEW4=COPLAN(4)-SY*ANINT(COPLAN(4)/SY)
*   Check that they are not one on top of the other.
            IF(YNPLAN(3).AND.YNPLAN(4).AND.CONEW3.EQ.CONEW4)THEN
                 IF(CONEW3.GT.0.0)THEN
                      CONEW3=CONEW3-SY
                 ELSE
                      CONEW4=CONEW4+SY
                 ENDIF
            ENDIF
*   Print some warnings if the planes have been moved.
            IF((CONEW3.NE.COPLAN(3).AND.YNPLAN(3)).OR.
     -           (CONEW4.NE.COPLAN(4).AND.YNPLAN(4)))
     -           PRINT *,' ------ CELCHK MESSAGE : The planes in y'//
     -                ' are moved to the basic period; this should'//
     -                ' not affect the results.'
            COPLAN(3)=CONEW3
            COPLAN(4)=CONEW4
*   Two planes should now be separated by SY, cancel PERY if not.
            IF(YNPLAN(3).AND.YNPLAN(4).AND.
     -           ABS(COPLAN(4)-COPLAN(3)).NE.SY)THEN
                 PRINT *,' !!!!!! CELCHK WARNING : The separation of'//
     -                ' the two y planes does not match the period;'//
     -                ' the periodicity is cancelled.'
                 PERY=.FALSE.
                 OK=.FALSE.
            ENDIF
*   If there are two planes left, they should have identical V's.
            IF(YNPLAN(3).AND.YNPLAN(4).AND.VTPLAN(3).NE.VTPLAN(4))THEN
                 PRINT *,' !!!!!! CELCHK WARNING : The voltages of'//
     -                ' the two y planes differ;'//
     -                ' the periodicity is cancelled.'
                 PERY=.FALSE.
                 OK=.FALSE.
            ENDIF
       ENDIF
**  Check that there is no voltage conflict of crossing planes.
       DO 20 I=1,2
       DO 10 J=3,4
       IF(YNPLAN(I).AND.YNPLAN(J).AND.VTPLAN(I).NE.VTPLAN(J))THEN
            PRINT *,' !!!!!! CELCHK WARNING : Conflicting potential of',
     -           ' 2 crossing planes; one y (or phi) plane is removed.'
            YNPLAN(J)=.FALSE.
            OK=.FALSE.
       ENDIF
10     CONTINUE
20     CONTINUE
**  Make sure the the coordinates of the planes are properly ordered.
       DO 30 I=1,3,2
       IF(YNPLAN(I).AND.YNPLAN(I+1))THEN
            IF(COPLAN(I).EQ.COPLAN(I+1))THEN
                 PRINT *,' !!!!!! CELCHK WARNING : Two planes are on'//
     -                ' top of each other; one of them is removed.'
                 YNPLAN(I+1)=.FALSE.
                 OK=.FALSE.
            ENDIF
            IF(COPLAN(I).GT.COPLAN(I+1))THEN
                 IF(LDEBUG)PRINT *,' ++++++ CELCHK DEBUG   : Planes ',I,
     -                ' and ',I+1,' are interchanged.'
                 COHLP=COPLAN(I)
                 COPLAN(I)=COPLAN(I+1)
                 COPLAN(I+1)=COHLP
                 VTHLP=VTPLAN(I)
                 VTPLAN(I)=VTPLAN(I+1)
                 VTPLAN(I+1)=VTHLP
                 LABHLP=PLATYP(I)
                 PLATYP(I)=PLATYP(I+1)
                 PLATYP(I+1)=LABHLP
                 DO 300 J=1,MXPSTR
                 SMIN=PLSTR1(I,J,1)
                 SMAX=PLSTR1(I,J,2)
                 GAP= PLSTR1(I,J,3)
                 LABHLP=PSLAB1(I,J)
                 PLSTR1(I,J,1)=PLSTR1(I+1,J,1)
                 PLSTR1(I,J,2)=PLSTR1(I+1,J,2)
                 PLSTR1(I,J,3)=PLSTR1(I+1,J,3)
                 PSLAB1(I,J)=PSLAB1(I+1,J)
                 PLSTR1(I+1,J,1)=SMIN
                 PLSTR1(I+1,J,2)=SMAX
                 PLSTR1(I+1,J,3)=GAP
                 PSLAB1(I+1,J)=LABHLP
                 SMIN=PLSTR2(I,J,1)
                 SMAX=PLSTR2(I,J,2)
                 GAP= PLSTR2(I,J,3)
                 LABHLP=PSLAB2(I,J)
                 PLSTR2(I,J,1)=PLSTR2(I+1,J,1)
                 PLSTR2(I,J,2)=PLSTR2(I+1,J,2)
                 PLSTR2(I,J,3)=PLSTR2(I+1,J,3)
                 PSLAB2(I,J)=PSLAB2(I+1,J)
                 PLSTR2(I+1,J,1)=SMIN
                 PLSTR2(I+1,J,2)=SMAX
                 PLSTR2(I+1,J,3)=GAP
                 PSLAB2(I+1,J)=LABHLP
300              CONTINUE
                 NHLP=NPSTR1(I)
                 NPSTR1(I)=NPSTR1(I+1)
                 NPSTR1(I+1)=NHLP
                 NHLP=NPSTR2(I)
                 NPSTR2(I)=NPSTR2(I+1)
                 NPSTR2(I+1)=NHLP
            ENDIF
       ENDIF
30     CONTINUE
*** Checks on the wires, start moving them to the basic x period.
       IF(PERX)THEN
            DO 40 I=1,NWIRE
            XNEW=X(I)-SX*ANINT(X(I)/SX)
            IF(ANINT(X(I)/SX).NE.0)THEN
                 XPRT=X(I)
                 YPRT=Y(I)
                 IF(POLAR)CALL CFMRTP(XPRT,YPRT,XPRT,YPRT,1)
                 CALL OUTFMT(XPRT,2,STR1,NC1,'LEFT')
                 CALL OUTFMT(YPRT,2,STR2,NC2,'LEFT')
                 PRINT *,' ------ CELCHK MESSAGE : The '//WIRTYP(I)//
     -                '-wire at ('//STR1(1:NC1)//','//STR2(1:NC2)//
     -                ') is moved to the basic x'
                 PRINT *,'                         (or r) period;'//
     -                ' this should not affect the results.'
            ENDIF
            X(I)=XNEW
40          CONTINUE
       ENDIF
**  In case of y-periodicity, all wires should be in the first y-period.
       IF(TUBE.AND.PERY)THEN
            DO 55 I=1,NWIRE
            XNEW=X(I)
            YNEW=Y(I)
            CALL CFMCTP(XNEW,YNEW,XNEW,YNEW,1)
            IF(ANINT((PI*YNEW)/(SY*180.0)).NE.0)THEN
                 CALL OUTFMT(X(I),2,STR1,NC1,'LEFT')
                 CALL OUTFMT(Y(I),2,STR2,NC2,'LEFT')
                 PRINT *,' ------ CELCHK MESSAGE : The '//WIRTYP(I)//
     -                '-wire at ('//STR1(1:NC1)//','//STR2(1:NC2)//
     -                ') is moved to the basic phi period;'
                 PRINT *,'                         this should not',
     -                ' affect the results.'
                 YNEW=YNEW-180*SY*ANINT((PI*YNEW)/(SY*180.0))/PI
                 CALL CFMPTC(XNEW,YNEW,X(I),Y(I),1)
            ENDIF
55          CONTINUE
       ELSEIF(PERY)THEN
            DO 50 I=1,NWIRE
            YNEW=Y(I)-SY*ANINT(Y(I)/SY)
            IF(ANINT(Y(I)/SY).NE.0)THEN
                 XPRT=X(I)
                 YPRT=Y(I)
                 IF(POLAR)CALL CFMRTP(XPRT,YPRT,XPRT,YPRT,1)
                 CALL OUTFMT(XPRT,2,STR1,NC1,'LEFT')
                 CALL OUTFMT(YPRT,2,STR2,NC2,'LEFT')
                 PRINT *,' ------ CELCHK MESSAGE : The '//WIRTYP(I)//
     -                '-wire at ('//STR1(1:NC1)//','//STR2(1:NC2)//
     -                ') is moved to the basic y period;'
                 PRINT *,'                         this should not',
     -                ' affect the results.'
            ENDIF
            Y(I)=YNEW
50          CONTINUE
       ENDIF
*** Make sure the plane numbering is standard: P1 wires P2, P3 wires P4.
       IPLAN1=0
       IPLAN2=0
       IPLAN3=0
       IPLAN4=0
       DO 60 I=1,NWIRE
       IF(YNPLAN(1).AND.X(I).LE.COPLAN(1))IPLAN1=IPLAN1+1
       IF(YNPLAN(2).AND.X(I).LE.COPLAN(2))IPLAN2=IPLAN2+1
       IF(YNPLAN(3).AND.Y(I).LE.COPLAN(3))IPLAN3=IPLAN3+1
       IF(YNPLAN(4).AND.Y(I).LE.COPLAN(4))IPLAN4=IPLAN4+1
60     CONTINUE
*   find out whether smaller (-1) or larger (+1) coord. are to be kept.
       IF(YNPLAN(1).AND.YNPLAN(2))THEN
            IF(IPLAN1.GT.NWIRE/2)THEN
                 YNPLAN(2)=.FALSE.
                 IPLAN1=-1
            ELSE
                 IPLAN1=+1
            ENDIF
            IF(IPLAN2.LT.NWIRE/2)THEN
                 YNPLAN(1)=.FALSE.
                 IPLAN2=+1
            ELSE
                 IPLAN2=-1
            ENDIF
       ENDIF
       IF(YNPLAN(1).AND..NOT.YNPLAN(2))THEN
            IF(IPLAN1.GT.NWIRE/2)THEN
                 IPLAN1=-1
            ELSE
                 IPLAN1=+1
            ENDIF
       ENDIF
       IF(YNPLAN(2).AND..NOT.YNPLAN(1))THEN
            IF(IPLAN2.LT.NWIRE/2)THEN
                 IPLAN2=+1
            ELSE
                 IPLAN2=-1
            ENDIF
       ENDIF
       IF(YNPLAN(3).AND.YNPLAN(4))THEN
            IF(IPLAN3.GT.NWIRE/2)THEN
                 YNPLAN(4)=.FALSE.
                 IPLAN3=-1
            ELSE
                 IPLAN3=+1
            ENDIF
            IF(IPLAN4.LT.NWIRE/2)THEN
                 YNPLAN(3)=.FALSE.
                 IPLAN4=+1
            ELSE
                 IPLAN4=-1
            ENDIF
       ENDIF
       IF(YNPLAN(3).AND..NOT.YNPLAN(4))THEN
            IF(IPLAN3.GT.NWIRE/2)THEN
                 IPLAN3=-1
            ELSE
                 IPLAN3=+1
            ENDIF
       ENDIF
       IF(YNPLAN(4).AND..NOT.YNPLAN(3))THEN
            IF(IPLAN4.LT.NWIRE/2)THEN
                 IPLAN4=+1
            ELSE
                 IPLAN4=-1
            ENDIF
       ENDIF
*   Adapt the numbering of the planes if necessary.
       IF(IPLAN1.EQ.-1)THEN
            YNPLAN(1)=.FALSE.
            YNPLAN(2)=.TRUE.
            COPLAN(2)=COPLAN(1)
            VTPLAN(2)=VTPLAN(1)
            PLATYP(2)=PLATYP(1)
            DO 310 J=1,MXPSTR
            PLSTR1(2,J,1)=PLSTR1(1,J,1)
            PLSTR1(2,J,2)=PLSTR1(1,J,2)
            PLSTR1(2,J,3)=PLSTR1(1,J,3)
            PSLAB1(2,J)=  PSLAB1(1,J)
            PLSTR2(2,J,1)=PLSTR2(1,J,1)
            PLSTR2(2,J,2)=PLSTR2(1,J,2)
            PLSTR2(2,J,3)=PLSTR2(1,J,3)
            PSLAB2(2,J)=  PSLAB2(1,J)
310         CONTINUE
            NPSTR1(2)=    NPSTR1(1)
            NPSTR2(2)=    NPSTR2(1)
            NPSTR1(1)=    0
            NPSTR2(1)=    0
       ENDIF
       IF(IPLAN2.EQ.+1)THEN
            YNPLAN(2)=.FALSE.
            YNPLAN(1)=.TRUE.
            COPLAN(1)=COPLAN(2)
            VTPLAN(1)=VTPLAN(2)
            PLATYP(1)=PLATYP(2)
            DO 320 J=1,MXPSTR
            PLSTR1(1,J,1)=PLSTR1(2,J,1)
            PLSTR1(1,J,2)=PLSTR1(2,J,2)
            PLSTR1(1,J,3)=PLSTR1(2,J,3)
            PSLAB1(1,J)=  PSLAB1(2,J)
            PLSTR2(1,J,1)=PLSTR2(2,J,1)
            PLSTR2(1,J,2)=PLSTR2(2,J,2)
            PLSTR2(1,J,3)=PLSTR2(2,J,3)
            PSLAB2(1,J)=  PSLAB2(2,J)
320         CONTINUE
            NPSTR1(1)=    NPSTR1(2)
            NPSTR2(1)=    NPSTR2(2)
            NPSTR1(2)=    0
            NPSTR2(2)=    0
       ENDIF
       IF(IPLAN3.EQ.-1)THEN
            YNPLAN(3)=.FALSE.
            YNPLAN(4)=.TRUE.
            COPLAN(4)=COPLAN(3)
            VTPLAN(4)=VTPLAN(3)
            PLATYP(4)=PLATYP(3)
            DO 330 J=1,MXPSTR
            PLSTR1(4,J,1)=PLSTR1(3,J,1)
            PLSTR1(4,J,2)=PLSTR1(3,J,2)
            PLSTR1(4,J,3)=PLSTR1(3,J,3)
            PSLAB1(4,J)=  PSLAB1(3,J)
            PLSTR2(4,J,1)=PLSTR2(3,J,1)
            PLSTR2(4,J,2)=PLSTR2(3,J,2)
            PLSTR2(4,J,3)=PLSTR2(3,J,3)
            PSLAB2(4,J)=  PSLAB2(3,J)
330         CONTINUE
            NPSTR1(4)=    NPSTR1(3)
            NPSTR2(4)=    NPSTR2(3)
            NPSTR1(3)=    0
            NPSTR2(3)=    0
       ENDIF
       IF(IPLAN4.EQ.+1)THEN
            YNPLAN(4)=.FALSE.
            YNPLAN(3)=.TRUE.
            COPLAN(3)=COPLAN(4)
            VTPLAN(3)=VTPLAN(4)
            PLATYP(3)=PLATYP(4)
            DO 340 J=1,MXPSTR
            PLSTR1(3,J,1)=PLSTR1(4,J,1)
            PLSTR1(3,J,2)=PLSTR1(4,J,2)
            PLSTR1(3,J,3)=PLSTR1(4,J,3)
            PSLAB1(3,J)=  PSLAB1(4,J)
            PLSTR2(3,J,1)=PLSTR2(4,J,1)
            PLSTR2(3,J,2)=PLSTR2(4,J,2)
            PLSTR2(3,J,3)=PLSTR2(4,J,3)
            PSLAB2(3,J)=  PSLAB2(4,J)
340         CONTINUE
            NPSTR1(3)=    NPSTR1(4)
            NPSTR2(3)=    NPSTR2(4)
            NPSTR1(4)=    0
            NPSTR2(4)=    0
       ENDIF
*** Second pass for the wires, check position relative to the planes.
       DO 70 I=1,NWIRE
       WRONG(I)=.FALSE.
       IF(YNPLAN(1).AND.X(I)-.5*D(I).LE.COPLAN(1))WRONG(I)=.TRUE.
       IF(YNPLAN(2).AND.X(I)+.5*D(I).GE.COPLAN(2))WRONG(I)=.TRUE.
       IF(YNPLAN(3).AND.Y(I)-.5*D(I).LE.COPLAN(3))WRONG(I)=.TRUE.
       IF(YNPLAN(4).AND.Y(I)+.5*D(I).GE.COPLAN(4))WRONG(I)=.TRUE.
       IF(TUBE)THEN
            CALL INTUBE(X(I),Y(I),COTUBE,NTUBE,IOUT)
            IF(IOUT.NE.0)THEN
                 CALL OUTFMT(X(I),2,STR1,NC1,'LEFT')
                 CALL OUTFMT(Y(I),2,STR2,NC2,'LEFT')
                 PRINT *,' !!!!!! CELCHK WARNING : The '//WIRTYP(I)//
     -                '-wire at ('//STR1(1:NC1)//','//STR2(1:NC2)//
     -                ') is located outside the tube; removed.'
                 WRONG(I)=.TRUE.
                 OK=.FALSE.
            ENDIF
       ELSEIF(WRONG(I))THEN
            XPRT=X(I)
            YPRT=Y(I)
            IF(POLAR)CALL CFMRTP(XPRT,YPRT,XPRT,YPRT,1)
            CALL OUTFMT(XPRT,2,STR1,NC1,'LEFT')
            CALL OUTFMT(YPRT,2,STR2,NC2,'LEFT')
            PRINT *,' !!!!!! CELCHK WARNING : The '//WIRTYP(I)//
     -           '-wire at ('//STR1(1:NC1)//','//STR2(1:NC2)//
     -           ') is located outside the planes; it is removed.'
            OK=.FALSE.
       ELSEIF((PERX.AND.D(I).GE.SX).OR.(PERY.AND.D(I).GE.SY))THEN
            XPRT=X(I)
            YPRT=Y(I)
            IF(POLAR)CALL CFMRTP(XPRT,YPRT,XPRT,YPRT,1)
            CALL OUTFMT(XPRT,2,STR1,NC1,'LEFT')
            CALL OUTFMT(YPRT,2,STR2,NC2,'LEFT')
            PRINT *,' !!!!!! CELCHK WARNING : The diameter of the '//
     -           WIRTYP(I)//'-wire at ('//STR1(1:NC1)//','//
     -           STR2(1:NC2)//') exceeds 1 period; it is removed.'
            WRONG(I)=.TRUE.
            OK=.FALSE.
       ENDIF
70     CONTINUE
**  Check the wire spacing.
       DO 90 I=1,NWIRE
       IF(WRONG(I))GOTO 90
       DO 80 J=I+1,NWIRE
       IF(WRONG(J))GOTO 80
       IF(TUBE)THEN
            IF(PERY)THEN
                 CALL CFMCTP(X(I),Y(I),XAUX1,YAUX1,1)
                 CALL CFMCTP(X(J),Y(J),XAUX2,YAUX2,1)
                 YAUX1=YAUX1-SY*ANINT(YAUX1/SY)
                 YAUX2=YAUX2-SY*ANINT(YAUX2/SY)
                 CALL CFMPTC(XAUX1,YAUX1,XAUX1,YAUX1,1)
                 CALL CFMPTC(XAUX2,YAUX2,XAUX2,YAUX2,1)
                 XSEPAR=XAUX1-XAUX2
                 YSEPAR=YAUX1-YAUX2
            ELSE
                 XSEPAR=X(I)-X(J)
                 YSEPAR=Y(I)-Y(J)
            ENDIF
       ELSE
            XSEPAR=ABS(X(I)-X(J))
            IF(PERX)XSEPAR=XSEPAR-SX*ANINT(XSEPAR/SX)
            YSEPAR=ABS(Y(I)-Y(J))
            IF(PERY)YSEPAR=YSEPAR-SY*ANINT(YSEPAR/SY)
       ENDIF
       IF(XSEPAR**2+YSEPAR**2.LT.0.25*(D(I)+D(J))**2)THEN
            XPRTI=X(I)
            YPRTI=Y(I)
            XPRTJ=X(J)
            YPRTJ=Y(J)
            IF(POLAR)CALL CFMRTP(XPRTI,YPRTI,XPRTI,YPRTI,1)
            IF(POLAR)CALL CFMRTP(XPRTJ,YPRTJ,XPRTJ,YPRTJ,1)
            CALL OUTFMT(XPRTI,2,STR1,NC1,'LEFT')
            CALL OUTFMT(YPRTI,2,STR2,NC2,'LEFT')
            CALL OUTFMT(XPRTJ,2,STR3,NC3,'LEFT')
            CALL OUTFMT(YPRTJ,2,STR4,NC4,'LEFT')
            PRINT *,' !!!!!! CELCHK WARNING : The '//WIRTYP(I)//
     -           '-wire at ('//STR1(1:NC1)//','//STR2(1:NC2)//') and'//
     -           ' the '//WIRTYP(J)//'-wire at ('//STR3(1:NC3)//','//
     -           STR4(1:NC4)//')'
            PRINT *,'                         overlap at least',
     -           ' partially; the latter is removed.'
            WRONG(J)=.TRUE.
            OK=.FALSE.
       ENDIF
80     CONTINUE
90     CONTINUE
**  Remove the wires which are not acceptable for one reason or another.
       IWIRE=NWIRE
       NWIRE=0
       DO 100 I=1,IWIRE
       IF(.NOT.WRONG(I))THEN
            NWIRE=NWIRE+1
            X(NWIRE)=X(I)
            Y(NWIRE)=Y(I)
            D(NWIRE)=D(I)
            V(NWIRE)=V(I)
            WIRTYP(NWIRE)=WIRTYP(I)
       ENDIF
100    CONTINUE
*** Ensure that some elements are left.
       NELEM=NWIRE
       IF(YNPLAN(1))NELEM=NELEM+1
       IF(YNPLAN(2))NELEM=NELEM+1
       IF(YNPLAN(3))NELEM=NELEM+1
       IF(YNPLAN(4))NELEM=NELEM+1
       IF(TUBE)NELEM=NELEM+1
       IF(NELEM.LT.2)THEN
            PRINT *,' ###### CELCHK ERROR   : Neither a field map,'//
     -           ' nor wires and planes, nor solids with boundary'//
     -           ' conditions; cell rejected.'
            OK=.FALSE.
            RETURN
       ENDIF
*** Check dielectrica, initialise the remove flag for the slabs.
       DO 150 I=1,MXMATT
       WRMATX(I)=.FALSE.
       WRMATY(I)=.FALSE.
150    CONTINUE
*   Check overlapping x-slabs and kill slabs outside the planes.
       DO 160 I=1,NXMATT
       IF(WRMATX(I))GOTO 160
       DO 170 J=I+1,NXMATT
       IF(WRMATX(J))GOTO 170
       IF(XMATT(I,3).NE.0.AND.XMATT(J,3).NE.0)THEN
            PRINT *,' !!!!!! CELCHK WARNING : Two dielectric slabs'//
     -           ' extend to -infinity in x ; one is removed.'
            WRMATX(J)=.TRUE.
            OK=.FALSE.
       ELSEIF(XMATT(I,4).NE.0.AND.XMATT(J,4).NE.0)THEN
            PRINT *,' !!!!!! CELCHK WARNING : Two dielectric slabs'//
     -           ' extend to +infinity in x ; one is removed.'
            WRMATX(J)=.TRUE.
            OK=.FALSE.
       ELSEIF((XMATT(I,3).NE.0.AND.XMATT(I,2).GT.XMATT(J,1)).OR.
     -      (XMATT(I,4).NE.0.AND.XMATT(I,1).LT.XMATT(J,2)).OR.
     -      (XMATT(J,3).NE.0.AND.XMATT(J,2).GT.XMATT(I,1)).OR.
     -      (XMATT(J,4).NE.0.AND.XMATT(J,1).LT.XMATT(I,2)))THEN
            PRINT *,' !!!!!! CELCHK WARNING : A dielectric'//
     -           ' semi-infinite x-slab overlaps partially'
            PRINT *,'                         with another x-slab.'//
     -           ' One of the slabs is removed.'
            WRMATX(J)=.TRUE.
            OK=.FALSE.
       ELSEIF(XMATT(I,3).EQ.0.AND.XMATT(I,4).EQ.0.AND.
     -      XMATT(J,3).EQ.0.AND.XMATT(J,4).EQ.0.AND.
     -      ((XMATT(I,1)-XMATT(J,1))*(XMATT(J,1)-XMATT(I,2)).GT.0.OR.
     -      (XMATT(I,1)-XMATT(J,2))*(XMATT(J,2)-XMATT(I,2)).GT.0))THEN
            PRINT *,' !!!!!! CELCHK WARNING : Two finite dielectric'//
     -           ' x-slabs overlap (in part) ; one is removed.'
            WRMATX(J)=.TRUE.
            OK=.FALSE.
       ENDIF
170    CONTINUE
       IF(WRMATX(I))GOTO 160
       IF((YNPLAN(1).AND.
     -      (XMATT(I,3).NE.0.OR.COPLAN(1).GT.XMATT(I,1))).OR.
     -      (YNPLAN(2).AND.
     -      (XMATT(I,4).NE.0.OR.COPLAN(2).LT.XMATT(I,2))))THEN
            PRINT *,' !!!!!! CELCHK WARNING : A dielectric x-slab'//
     -           ' covers a plane ; it is removed.'
            WRMATX(I)=.TRUE.
            OK=.FALSE.
       ENDIF
       IF(WRMATX(I))GOTO 160
       IF(PERX.AND.(XMATT(I,3).NE.0.OR.XMATT(I,4).NE.0.OR.
     -      ABS(XMATT(I,1)-XMATT(I,2)).GT.SX))THEN
            PRINT *,' !!!!!! CELCHK WARNING : The dielectric x-slab'//
     -           ' from (',XMATT(I,1),' to ',XMATT(I,2),')'
            PRINT *,'                         covers more than one'//
     -           ' x-period ; it is removed.'
            WRMATX(I)=.TRUE.
            OK=.FALSE.
       ENDIF
160    CONTINUE
*   Check overlapping y-slabs and kill slabs outside the planes.
       DO 180 I=1,NYMATT
       IF(WRMATY(I))GOTO 180
       DO 190 J=I+1,NYMATT
       IF(WRMATY(J))GOTO 190
       IF(YMATT(I,3).NE.0.AND.YMATT(J,3).NE.0)THEN
            PRINT *,' !!!!!! CELCHK WARNING : Two dielectric slabs'//
     -           ' extend to -infinity in y ; one is removed.'
            WRMATY(J)=.TRUE.
            OK=.FALSE.
       ELSEIF(YMATT(I,4).NE.0.AND.YMATT(J,4).NE.0)THEN
            PRINT *,' !!!!!! CELCHK WARNING : Two dielectric slabs'//
     -           ' extend to +infinity in y ; one is removed.'
            WRMATY(J)=.TRUE.
            OK=.FALSE.
       ELSEIF((YMATT(I,3).NE.0.AND.YMATT(I,2).GT.YMATT(J,1)).OR.
     -      (YMATT(I,4).NE.0.AND.YMATT(I,1).LT.YMATT(J,2)).OR.
     -      (YMATT(J,3).NE.0.AND.YMATT(J,2).GT.YMATT(I,1)).OR.
     -      (YMATT(J,4).NE.0.AND.YMATT(J,1).LT.YMATT(I,2)))THEN
            PRINT *,' !!!!!! CELCHK WARNING : A dielectric'//
     -           ' semi-infinite y-slab overlaps partially'
            PRINT *,'                         with another y-slab.'//
     -           ' One of the slabs is removed.'
            WRMATY(J)=.TRUE.
            OK=.FALSE.
       ELSEIF(YMATT(I,3).EQ.0.AND.YMATT(I,4).EQ.0.AND.
     -      YMATT(J,3).EQ.0.AND.YMATT(J,4).EQ.0.AND.
     -      ((YMATT(I,1)-YMATT(J,1))*(YMATT(J,1)-YMATT(I,2)).GT.0.OR.
     -      (YMATT(I,1)-YMATT(J,2))*(YMATT(J,2)-YMATT(I,2)).GT.0))THEN
            PRINT *,' !!!!!! CELCHK WARNING : Two finite dielectric'//
     -           ' y-slabs overlap (in part) ; one is removed.'
            WRMATY(J)=.TRUE.
            OK=.FALSE.
       ENDIF
190    CONTINUE
       IF(WRMATY(I))GOTO 180
       IF((YNPLAN(3).AND.
     -      (YMATT(I,3).NE.0.OR.COPLAN(3).GT.YMATT(I,1))).OR.
     -      (YNPLAN(4).AND.
     -      (YMATT(I,4).NE.0.OR.COPLAN(4).LT.YMATT(I,2))))THEN
            PRINT *,' !!!!!! CELCHK WARNING : A dielectric y-slab'//
     -           ' covers a plane ; it is removed.'
            WRMATY(I)=.TRUE.
            OK=.FALSE.
       ENDIF
       IF(WRMATY(I))GOTO 180
       IF(PERX.AND.(YMATT(I,3).NE.0.OR.YMATT(I,4).NE.0.OR.
     -      ABS(YMATT(I,1)-YMATT(I,2)).GT.SX))THEN
            PRINT *,' !!!!!! CELCHK WARNING : The dielectric y-slab'//
     -           ' from (',YMATT(I,1),' to ',YMATT(I,2),')'
            PRINT *,'                         covers more than one'//
     -           ' x-period ; it is removed.'
            WRMATY(I)=.TRUE.
            OK=.FALSE.
       ENDIF
180    CONTINUE
*   And finally crossing slabs with different epsilons.
       DO 200 I=1,NXMATT
       IF(WRMATX(I))GOTO 200
       DO 210 J=1,NYMATT
       IF(WRMATY(J))GOTO 210
       IF(ABS(XMATT(I,5)-YMATT(J,5)).GT.1.0E-5*(1.0+ABS(XMATT(I,5))+
     -      ABS(YMATT(J,5))))THEN
            PRINT *,' !!!!!! CELCHK WARNING : A dielectric x-slab'//
     -           ' crosses a y-slab but has a'
            PRINT *,'                         different dielectric'//
     -           ' constant; the x-slab is removed.'
            WRMATX(I)=.TRUE.
            OK=.FALSE.
       ENDIF
210    CONTINUE
200    CONTINUE
*   Remove slabs, first x, than y.
       NXOLD=NXMATT
       NXMATT=0
       DO 220 I=1,NXOLD
       IF(WRMATX(I))GOTO 220
       NXMATT=NXMATT+1
       DO 230 J=1,5
       XMATT(NXMATT,J)=XMATT(I,J)
230    CONTINUE
220    CONTINUE
       NYOLD=NYMATT
       NYMATT=0
       DO 240 I=1,NYOLD
       IF(WRMATY(I))GOTO 240
       NYMATT=NYMATT+1
       DO 250 J=1,5
       YMATT(NYMATT,J)=YMATT(I,J)
250    CONTINUE
240    CONTINUE
*** Determine maximum and minimum coordinates and potentials.
       SETX=.FALSE.
       SETY=.FALSE.
       SETZ=.FALSE.
       SETV=.FALSE.
       XMIN=0
       XMAX=0
       YMIN=0
       YMAX=0
       ZMIN=0
       ZMAX=0
       VMIN=0
       VMAX=0
*   Loop over the wires.
       DO 120 I=1,NWIRE
       IF(SETX)THEN
            XMIN=MIN(XMIN,X(I)-D(I)/2)
            XMAX=MAX(XMAX,X(I)+D(I)/2)
       ELSE
            XMIN=X(I)-D(I)/2
            XMAX=X(I)+D(I)/2
            SETX=.TRUE.
       ENDIF
       IF(SETY)THEN
            YMIN=MIN(YMIN,Y(I)-D(I)/2)
            YMAX=MAX(YMAX,Y(I)+D(I)/2)
       ELSE
            YMIN=Y(I)-D(I)/2
            YMAX=Y(I)+D(I)/2
            SETY=.TRUE.
       ENDIF
       IF(SETZ)THEN
            ZMIN=MIN(ZMIN,-U(I)/2)
            ZMAX=MAX(ZMAX,+U(I)/2)
       ELSE
            ZMIN=-U(I)/2
            ZMAX=+U(I)/2
            SETZ=.TRUE.
       ENDIF
       IF(SETV)THEN
            VMIN=MIN(VMIN,V(I))
            VMAX=MAX(VMAX,V(I))
       ELSE
            VMIN=V(I)
            VMAX=V(I)
            SETV=.TRUE.
       ENDIF
120    CONTINUE
*   Consider the planes.
       DO 130 I=1,4
       IF(YNPLAN(I))THEN
            IF(I.LE.2)THEN
                 IF(SETX)THEN
                      XMIN=MIN(XMIN,COPLAN(I))
                      XMAX=MAX(XMAX,COPLAN(I))
                 ELSE
                      XMIN=COPLAN(I)
                      XMAX=COPLAN(I)
                      SETX=.TRUE.
                 ENDIF
            ELSE
                 IF(SETY)THEN
                      YMIN=MIN(YMIN,COPLAN(I))
                      YMAX=MAX(YMAX,COPLAN(I))
                 ELSE
                      YMIN=COPLAN(I)
                      YMAX=COPLAN(I)
                      SETY=.TRUE.
                 ENDIF
            ENDIF
            IF(SETV)THEN
                 VMIN=MIN(VMIN,VTPLAN(I))
                 VMAX=MAX(VMAX,VTPLAN(I))
            ELSE
                 VMIN=VTPLAN(I)
                 VMAX=VTPLAN(I)
                 SETV=.TRUE.
            ENDIF
       ENDIF
130    CONTINUE
*   Consider the dielectrica.
       DO 260 I=1,NXMATT
       IF(XMATT(I,3).EQ.0)THEN
            IF(SETX)THEN
                 XMIN=MIN(XMIN,XMATT(I,1))
                 XMAX=MAX(XMAX,XMATT(I,1))
            ELSE
                 XMIN=XMATT(I,1)
                 XMAX=XMATT(I,1)
                 SETX=.TRUE.
            ENDIF
       ENDIF
       IF(XMATT(I,4).EQ.0)THEN
            IF(SETX)THEN
                 XMIN=MIN(XMIN,XMATT(I,2))
                 XMAX=MAX(XMAX,XMATT(I,2))
            ELSE
                 XMIN=XMATT(I,2)
                 XMAX=XMATT(I,2)
                 SETX=.TRUE.
            ENDIF
       ENDIF
260    CONTINUE
       DO 270 I=1,NYMATT
       IF(YMATT(I,3).EQ.0)THEN
            IF(SETY)THEN
                 YMIN=MIN(YMIN,YMATT(I,1))
                 YMAX=MAX(YMAX,YMATT(I,1))
            ELSE
                 YMIN=YMATT(I,1)
                 YMAX=YMATT(I,1)
                 SETY=.TRUE.
            ENDIF
       ENDIF
       IF(YMATT(I,4).EQ.0)THEN
            IF(SETY)THEN
                 YMIN=MIN(YMIN,YMATT(I,2))
                 YMAX=MAX(YMAX,YMATT(I,2))
            ELSE
                 YMIN=YMATT(I,2)
                 YMAX=YMATT(I,2)
                 SETY=.TRUE.
            ENDIF
       ENDIF
270    CONTINUE
*   Consider the tube.
       IF(TUBE)THEN
            XMIN=-1.1*COTUBE
            XMAX=+1.1*COTUBE
            SETX=.TRUE.
            YMIN=-1.1*COTUBE
            YMAX=+1.1*COTUBE
            SETY=.TRUE.
            VMIN=MIN(VMIN,VTTUBE)
            VMAX=MAX(VMAX,VTTUBE)
            SETV=.TRUE.
       ENDIF
**  In case of x-periodicity, XMAX-XMIN should be SX,
       IF(PERX.AND.SX.GT.(XMAX-XMIN))THEN
            XMIN=-SX/2.0
            XMAX=SX/2.0
            SETX=.TRUE.
       ENDIF
*   in case of y-periodicity, YMAX-YMIN should be SY,
       IF(PERY.AND.SY.GT.(YMAX-YMIN))THEN
            YMIN=-SY/2.0
            YMAX=SY/2.0
            SETY=.TRUE.
       ENDIF
*   in case the cell is polar, the y range should be < 2 pi.
       IF(POLAR.AND.YMAX-YMIN.GE.2.0*PI)THEN
            YMIN=-PI
            YMAX=+PI
            SETY=.TRUE.
       ENDIF
**  Fill in missing dimensions.
       IF(SETX.AND.XMIN.NE.XMAX.AND.(YMIN.EQ.YMAX.OR..NOT.SETY))THEN
            YMIN=YMIN-ABS(XMAX-XMIN)/2
            YMAX=YMAX+ABS(XMAX-XMIN)/2
            SETY=.TRUE.
       ENDIF
       IF(SETY.AND.YMIN.NE.YMAX.AND.(XMIN.EQ.XMAX.OR..NOT.SETX))THEN
            XMIN=XMIN-ABS(YMAX-YMIN)/2
            XMAX=XMAX+ABS(YMAX-YMIN)/2
            SETX=.TRUE.
       ENDIF
       IF(.NOT.SETZ)THEN
            ZMIN=-(ABS(XMAX-XMIN)+ABS(YMAX-YMIN))/4
            ZMAX=+(ABS(XMAX-XMIN)+ABS(YMAX-YMIN))/4
            SETZ=.TRUE.
       ENDIF
*   Ensure that all dimensions are now set.
       IF(.NOT.(SETX.AND.SETY.AND.SETZ))THEN
            PRINT *,' !!!!!! CELCHK WARNING : Unable to establish'//
     -           ' default dimensions in all directions; use AREA.'
            OK=.FALSE.
       ENDIF
*** Check that at least some different voltages are present.
       IF(VMIN.EQ.VMAX.OR..NOT.SETV)THEN
            PRINT *,' ###### CELCHK ERROR   : All potentials in the'//
     -           ' cell are the same; there is no point in going on.'
            OK=.FALSE.
            RETURN
       ENDIF
*** Resume here for maps.
3000   CONTINUE
*** Take action on the warnings if requested.
       IF(JFAIL.EQ.2.AND..NOT.OK)THEN
            PRINT *,' ###### CELCHK ERROR   : Cell declared to be'//
     -           ' unuseable because of the above warnings.'
            RETURN
       ELSEIF(JFAIL.EQ.3.AND..NOT.OK)THEN
            PRINT *,' ###### CELCHK ERROR   : Program terminated'//
     -           ' because of the above warnings.'
            CALL QUIT
            RETURN
       ENDIF
*** Cell seems to be alright since it passed all critical tests.
       IFAIL=0
*** Print the amount of CPU time used.
       CALL TIMLOG('Checking that the cell makes sense:     ')
       END
