CDECK  ID>, BEMINT.
       SUBROUTINE BEMINT(IFAIL)
*-----------------------------------------------------------------------
*   BEMINT - Calls the initialisation of neBEM
*   (Last changed on 15/ 4/12.)
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
       INTEGER NBEM,IREFB1(MXPLAN),NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,
     -      BEMNEW,BEMINV,BEMSLV
       DOUBLE PRECISION BEMQTH,BEMSTH,BEMSSC,BEMTGT,BEMEPA,BEMEPD
       LOGICAL LBDUMP
       COMMON /BEMDAT/ BEMQTH,BEMSSC,BEMSTH,BEMTGT,BEMEPA,BEMEPD,
     -      IREFB1,NBEM,NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,BEMNEW,
     -      BEMINV,BEMSLV,LBDUMP
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER IFAIL,IVOL,IREF,NREF,ICOL,IPL,NPL,I,IMAX,IBEMST,
     -      ISHAPE1,IMAT1,IBOUND1,IFAIL1,ISHAPE2,IMAT2,IBOUND2,IFAIL2,
     -      IDEBUG
       DOUBLE PRECISION XPL(MXEDGE),YPL(MXEDGE),ZPL(MXEDGE),APL,BPL,CPL,
     -      BXMIN,BXMAX,BYMIN,BYMAX,BZMIN,BZMAX,DIST,
     -      EPS1,VOLT1,CHARGE1,EPS2,VOLT2,CHARGE2
       LOGICAL SET
*** Assume this will fail.
       IFAIL=1
*** Check that some solids exist.
       IF(NSOLID.LE.0)THEN
            PRINT *,' !!!!!! BEMINT WARNING : No solids have been'//
     -           ' defined sofar; no point in running neBEM.'
            RETURN
       ENDIF
*** Check the neBEM status.
       CALL BEMQST(IBEMST)
       IF(IBEMST.LT.1)THEN
            PRINT *,' !!!!!! BEMINT WARNING : neBEM initialisation'//
     -           ' not done yet; trying to do this now.'
            CALL BEMINI(IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! BEMINT WARNING : Failed to'//
     -                ' initialise neBEM; no preparation done.'
                 RETURN
            ENDIF
       ELSEIF(IBEMST.GT.1)THEN
            PRINT *,' ------ BEMINT MESSAGE : neBEM already used;'//
     -           ' re-initialising.'
            CALL BEMEND
            CALL BEMINI(IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! BEMINT WARNING : Failed to'//
     -                ' initialise neBEM; no preparation done.'
                 RETURN
            ENDIF
       ENDIF
*** Progress printing.
       CALL PROINT('neBEM',1,6)
*** Be sure we won't have intersections with the bounding box.
       CALL PROFLD(1,'Finding envelope',-1.0)
       CALL PROSTA(1,0.0)
       DO 40 IVOL=1,NSOLID
       ICOL=0
       CALL PROSTA(1,REAL(IVOL))
*   cylinders ...
       IF(ISOLTP(IVOL).EQ.1)THEN
            CALL PLACYE(IVOL,BXMIN,BYMIN,BZMIN,BXMAX,BYMAX,BZMAX)
*   cylindrical holes ...
       ELSEIF(ISOLTP(IVOL).EQ.2)THEN
            CALL PLACHE(IVOL,BXMIN,BYMIN,BZMIN,BXMAX,BYMAX,BZMAX)
*   boxes ...
       ELSEIF(ISOLTP(IVOL).EQ.3)THEN
            CALL PLABXE(IVOL,BXMIN,BYMIN,BZMIN,BXMAX,BYMAX,BZMAX)
*   spheres ...
       ELSEIF(ISOLTP(IVOL).EQ.4)THEN
            CALL PLASPE(IVOL,BXMIN,BYMIN,BZMIN,BXMAX,BYMAX,BZMAX)
*   Toblerone ...
       ELSEIF(ISOLTP(IVOL).EQ.5)THEN
            CALL PLATBE(IVOL,BXMIN,BYMIN,BZMIN,BXMAX,BYMAX,BZMAX)
*   extrusion ...
       ELSEIF(ISOLTP(IVOL).EQ.6)THEN
            CALL PLAEXE(IVOL,BXMIN,BYMIN,BZMIN,BXMAX,BYMAX,BZMAX)
*   other things not known.
       ELSE
            PRINT *,' !!!!!! BEMINT WARNING : Solid of unknown type ',
     -           ISOLTP(IVOL),'; not considered for envelope.'
            GOTO 40
       ENDIF
       IF(IVOL.EQ.1)THEN
            GXMIN=BXMIN
            GXMAX=BXMAX
            GYMIN=BYMIN
            GYMAX=BYMAX
            GZMIN=BZMIN
            GZMAX=BZMAX
       ELSE
            GXMIN=MIN(BXMIN,GXMIN)
            GXMAX=MAX(BXMAX,GXMAX)
            GYMIN=MIN(BYMIN,GYMIN)
            GYMAX=MAX(BYMAX,GYMAX)
            GZMIN=MIN(BZMIN,GZMIN)
            GZMAX=MAX(BZMAX,GZMAX)
       ENDIF
40     CONTINUE
*   Enlarge a bit.
       DIST=0.01*ABS(GXMAX-GXMIN)
       GXMIN=GXMIN-DIST
       GXMAX=GXMAX+DIST
       DIST=0.01*ABS(GYMAX-GYMIN)
       GYMIN=GYMIN-DIST
       GYMAX=GYMAX+DIST
       DIST=0.01*ABS(GZMAX-GZMIN)
       GZMIN=GZMIN-DIST
       GZMAX=GZMAX+DIST
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ BEMINT DEBUG   : Envelope''/
     -      26X,E12.5,'' < x < '',E12.5/26X,E12.5,'' < y < '',E12.5/
     -      26X,E12.5,'' < z < '',E12.5)')
     -      GXMIN,GXMAX,GYMIN,GYMAX,GZMIN,GZMAX
*** Calculate the absolute size threshold accordingly
       BEMSTH=BEMSSC*MAX(GXMAX-GXMIN,GYMAX-GYMIN,GZMAX-GZMIN)**2
*** Pass the option parameters to neBEM.
       IF(LDEBUG)THEN
            IDEBUG=10
       ELSE
            IDEBUG=0
       ENDIF
       CALL BEMPAR(NBEMMN,NBEMMX,BEMTGT,BEMNEW,BEMINV,IDEBUG,BEMSLV)
*** Reset the buffer of the panels.
       CALL PLABU1('RESET',IREF,0,XPL,YPL,ZPL,
     -      0.0D0,0.0D0,0.0D0,0,0,IFAIL1)
       CALL BEMBU1('RESET',IREF,0,XPL,YPL,ZPL,
     -      0.0D0,0.0D0,0.0D0,0,0,IFAIL1)
*** Generate the list of surface pannels.
       CALL PROFLD(1,'Generating volumes',REAL(NSOLID))
       DO 10 IVOL=1,NSOLID
       ICOL=0
       CALL PROSTA(1,REAL(IVOL))
*   cylinders and wires ...
       IF(ISOLTP(IVOL).EQ.1)THEN
            IF(CBUF(ISTART(IVOL)+9).LT.-0.5)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ BEMINT DEBUG   :'',
     -                '' Skipping wire volume '',I5)') IVOL
            ELSE
                 CALL PLACYP(IVOL,ICOL)
            ENDIF
*   cylindrical holes ...
       ELSEIF(ISOLTP(IVOL).EQ.2)THEN
            CALL PLACHP(IVOL,ICOL)
*   boxes ...
       ELSEIF(ISOLTP(IVOL).EQ.3)THEN
            CALL PLABXP(IVOL,ICOL)
*   spheres ...
       ELSEIF(ISOLTP(IVOL).EQ.4)THEN
            CALL PLASPP(IVOL,ICOL)
*   Toblerone ...
       ELSEIF(ISOLTP(IVOL).EQ.5)THEN
            CALL PLATBP(IVOL,ICOL)
*   extrusion ...
       ELSEIF(ISOLTP(IVOL).EQ.6)THEN
            CALL PLAEXP(IVOL,ICOL)
*   other things not known.
       ELSE
            PRINT *,' !!!!!! BEMINT WARNING : Asked to plot a'//
     -           ' solid of unknown type ',ISOLTP(IVOL),
     -           '; not plotted.'
       ENDIF
10     CONTINUE
*** Apply cuts.
       CALL CELSCT('APPLY')
*** Find a volume which encloses all solids.
       CALL PROFLD(1,'Finding bounding box',-1.0)
       CALL PROSTA(1,0.0)
*   Count panels.
       CALL PLABU1('QUERY',NREF,NPL,XPL,YPL,ZPL,APL,BPL,CPL,
     -      ICOL,IVOL,IFAIL1)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ BEMINT DEBUG   : After'',
     -      '' generating, '',I5,'' panels.'')') NREF
*   Make sure that the buffer is OK.
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! BEMINT WARNING : Unable to count the'//
     -           ' panels for the solid.'
            CALL PROEND
            RETURN
       ENDIF
*   Loop over all panels.
       SET=.FALSE.
       DO 20 IREF=1,NREF
       CALL PLABU1('READ',IREF,NPL,XPL,YPL,ZPL,APL,BPL,CPL,
     -      ICOL,IVOL,IFAIL1)
       IF(IFAIL1.NE.0)GOTO 20
       DO 30 IPL=1,NPL
       IF(SET)THEN
            XMIN=MIN(XMIN,XPL(IPL))
            XMAX=MAX(XMAX,XPL(IPL))
            YMIN=MIN(YMIN,YPL(IPL))
            YMAX=MAX(YMAX,YPL(IPL))
            ZMIN=MIN(ZMIN,ZPL(IPL))
            ZMAX=MAX(ZMAX,ZPL(IPL))
       ELSE
            XMIN=XPL(IPL)
            XMAX=XPL(IPL)
            YMIN=YPL(IPL)
            YMAX=YPL(IPL)
            ZMIN=ZPL(IPL)
            ZMAX=ZPL(IPL)
            SET=.TRUE.
       ENDIF
30     CONTINUE
20     CONTINUE
*** Reduce to basic periodic copy.
       CALL BEMBAS
*** Find contact panels.
       CALL PROFLD(1,'Finding contacts',-1.0)
       CALL PROSTA(1,0.0)
       CALL PLABEM(IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! BEMINT WARNING : Finding the'//
     -           ' contact panels failed; neBEM not called.'
            CALL PROEND
            RETURN
       ENDIF
*** Count the number of effectively existing panels.
       CALL PROFLD(1,'Counting surfaces',-1.0)
       CALL PROSTA(1,0.0)
       CALL PLABU1('QUERY',IMAX,NPL,XPL,YPL,ZPL,
     -      APL,BPL,CPL,ICOL,IVOL,IFAIL1)
       NBEM=0
       DO 50 I=1,IMAX
*   Retrieve a panel.
       CALL PLABU1('READ',I,NPL,XPL,YPL,ZPL,
     -      APL,BPL,CPL,ICOL,IVOL,IFAIL1)
       IF(IFAIL1.NE.0)GOTO 50
*   See whether this is trivial.
       CALL BEMVOL(IVOL,ISHAPE1,IMAT1,EPS1,VOLT1,CHARGE1,IBOUND1,IFAIL1)
       CALL BEMVOL(ICOL,ISHAPE2,IMAT2,EPS2,VOLT2,CHARGE2,IBOUND2,IFAIL2)
       IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)THEN
            PRINT *,' !!!!!! BEMINT WARNING : Failure when retrieving'//
     -           ' volume properties; primitive kept.'
       ELSEIF(IBOUND1.EQ.1.AND.IBOUND2.EQ.1.AND.
     -      ABS(VOLT1-VOLT2).LT.1.0D-6*(1+ABS(VOLT1)+ABS(VOLT2)))THEN
            PRINT *,' ++++++ BEMINT DEBUG   : Same voltage, skipped.'
            GOTO 50
       ELSEIF(IBOUND1.EQ.4.AND.IBOUND2.EQ.4.AND.
     -      ABS(EPS1-EPS2).LT.1.0D-6*(1+ABS(EPS1)+ABS(EPS2)))THEN
            PRINT *,' ++++++ BEMINT DEBUG   : Same epsilon, skipped.'
            GOTO 50
       ELSEIF(IBOUND1.EQ.0.AND.IBOUND2.EQ.4.AND.
     -      ABS(EPS2-1.0).LT.1.0D-6)THEN
            PRINT *,' ++++++ BEMINT DEBUG   : Gas to Eps=1, skipped.'
            GOTO 50
       ELSEIF(IBOUND2.EQ.0.AND.IBOUND1.EQ.4.AND.
     -      ABS(EPS1-1.0).LT.1.0D-6)THEN
            PRINT *,' ++++++ BEMINT DEBUG   : Eps=1 to gas, skipped.'
            GOTO 50
       ENDIF
*   Store.
       CALL BEMBU1('STORE',IREF,NPL,XPL,YPL,ZPL,
     -      APL,BPL,CPL,ICOL,IVOL,IFAIL1)
       NBEM=NBEM+1
       IREFB1(NBEM)=IREF
50     CONTINUE
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ BEMINT DEBUG   : Highest'',
     -      '' panel: '',I5,'', existing panels: '',I5)')
     -      IMAX,NBEM
*** Transfer the geometry.
       CALL PROFLD(1,'Reading geometry',-1.0)
       CALL PROSTA(1,0.0)
       CALL BEMGEO(IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! BEMINT WARNING : neBEM'//
     -           ' reading of the solid structure failed.'
            CALL PROEND
            RETURN
       ENDIF
*** Discretise.
       CALL PROFLD(1,'Generating elements',-1.0)
       CALL PROSTA(1,0.0)
       CALL BEMDIS(IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! BEMINT WARNING : neBEM'//
     -           ' element generation failed.'
            CALL PROEND
            RETURN
       ENDIF
*** Establish boundary conditions.
       CALL PROFLD(1,'Boundary conditions',-1.0)
       CALL PROSTA(1,0.0)
       CALL BEMBND(IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! BEMINT WARNING : neBEM'//
     -           ' setting of boundary conditions failed.'
            CALL PROEND
            RETURN
       ENDIF
*** Solve (0: find matrix, 1: use existing matrix, not yet respected)
       CALL PROFLD(1,'Solving matrix',-1.0)
       CALL PROSTA(1,0.0)
       CALL BEMSOL(IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! BEMINT WARNING : neBEM solving or'//
     -           ' retrieving of influence matrix failed.'
            CALL PROEND
            RETURN
       ENDIF
*** End of progress printing.
       CALL PROEND
*** Seems to have worked.
       BEMSET=.TRUE.
       IFAIL=0
       END
