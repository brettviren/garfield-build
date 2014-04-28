CDECK  ID>, DRFTR2.
       SUBROUTINE DRFTR2(PLTVEC,POSVEC,ISTVEC,NVEC,MARKER,TEXT,TITLE)
*-----------------------------------------------------------------------
*   DRFTR2 - Auxiliary routine to DRFTRA, it plots the various graphs
*            such as the mean velocity.
*   VARIABLES : PLTVEC      : The vector to be plotted.
*               ISTVEC      : Vector of status codes.
*               other args  : Texts to be plotted along the axes.
*   (Last changed on 31/07/08.)
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
       REAL PLTVEC(MXLIST),XPL(MXLIST),YPL(MXLIST),POSVEC(MXLIST),
     -      PMIN,PMAX,PDEL,XEMIN,XEMAX,XDEL,AUX,XTXT,YTXT
       INTEGER ISTVEC(MXLIST),IND(MXLIST),IAUX,I,J,NVEC,NPL,NC,NCAUX
       CHARACTER*(*) TEXT,TITLE
       CHARACTER*20 AUXSTR
       LOGICAL MARKER,PSET
       CHARACTER*40 COMSTR
*** Make sure NVEC is > 0.
       IF(NVEC.LE.1)THEN
            PRINT *,' !!!!!! DRFTR2 WARNING : Insufficient number'//
     -           ' of points in the plot vector; no graph.'
            RETURN
       ENDIF
*** Sort the coordinate vector and make the plot vectors follow.
       DO 20 I=1,NVEC
       IND(I)=I
20     CONTINUE
C       CALL SORTZV(POSVEC,IND,NVEC,1,0,0)
       CALL SORTTF(POSVEC,IND,NVEC)
       DO 100 I=1,NVEC
*   Rearrange positions.
       AUX=POSVEC(I)
       POSVEC(I)=POSVEC(IND(I))
       POSVEC(IND(I))=AUX
*   Rearrange plot vector.
       AUX=PLTVEC(I)
       PLTVEC(I)=PLTVEC(IND(I))
       PLTVEC(IND(I))=AUX
*   Rearrange status codes.
       IAUX=ISTVEC(I)
       ISTVEC(I)=ISTVEC(IND(I))
       ISTVEC(IND(I))=IAUX
*   Update sort vector.
       DO 110 J=I,NVEC
       IF(IND(J).EQ.I)IND(J)=IND(I)
110    CONTINUE
100    CONTINUE
*** Determine the range of the plotted vector, excluding abnormal ends.
       PSET=.FALSE.
       DO 10 I=1,NVEC
       IF(ISTVEC(I).EQ.-2.OR.ISTVEC(I).EQ.-3)GOTO 10
       IF(PSET)THEN
            PMIN=MIN(PMIN,PLTVEC(I))
            PMAX=MAX(PMAX,PLTVEC(I))
       ELSE
            PMIN=PLTVEC(I)
            PMAX=PLTVEC(I)
            PSET=.TRUE.
       ENDIF
10     CONTINUE
*   Ensure that a range has been found.
       IF(.NOT.PSET)THEN
            PRINT *,' !!!!!! DRFTR2 WARNING : No complete drift lines'//
     -           ' have been seen ; no useful plots can be made.'
            RETURN
       ENDIF
*   Slightly increase the range to get a reasonable plot.
       PDEL=ABS(PMAX-PMIN)
       IF(PMIN.LT.0)THEN
            PMIN=PMIN-0.1*PDEL
       ELSE
            PMIN=MAX(0.0,PMIN-0.1*PDEL)
       ENDIF
       PMAX=PMAX+0.1*PDEL
       XDEL=ABS(POSVEC(NVEC)-POSVEC(1))
       XEMIN=POSVEC(1)-0.1*XDEL
       XEMAX=POSVEC(NVEC)+0.1*XDEL
*** Open a frame following the coordinate along the track.
       CALL GRCART(XEMIN,PMIN,XEMAX,PMAX,
     -      'Distance from track start [cm]',TEXT,TITLE)
*   Add some comments to the plot.
       IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
       IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
*** Plot the curve, start by initialising the plot vector.
       NPL=1
       XPL(NPL)=POSVEC(1)
       YPL(NPL)=PLTVEC(1)
*   Loop over the points.
       DO 40 I=2,NVEC
**  Change of status or end of line.
       IF(I.EQ.NVEC.OR.ISTVEC(I).NE.ISTVEC(I-1))THEN
*   End of line, but no change of status: add.
            IF(ISTVEC(I).EQ.ISTVEC(I-1))THEN
                 IF(NPL.GE.MXLIST)THEN
                      PRINT *,' ###### DRFTR2 ERROR   : Plot buffer'//
     -                     ' overflow ; plot closed.'
                      CALL GRNEXT
                      RETURN
                 ENDIF
                 NPL=NPL+1
                 XPL(NPL)=POSVEC(I)
                 YPL(NPL)=PLTVEC(I)
            ENDIF
*   Unless abandoned or out of steps: draw the graph.
            IF(ISTVEC(I-1).NE.-2.AND.ISTVEC(I-1).NE.-3)THEN
                 IF(NPL.EQ.1.OR.(MARKER.AND.NPL.GE.1))THEN
                      CALL GRATTS('FUNCTION-1','POLYMARKER')
                      CALL GPM(NPL,XPL,YPL)
                 ELSEIF(NPL.GT.1)THEN
                      CALL GRATTS('FUNCTION-1','POLYLINE')
                      CALL GPL(NPL,XPL,YPL)
                 ENDIF
            ENDIF
*   A string explaining where the particles ended.
            IF(ISTVEC(I-1).EQ.-9)THEN
                 COMSTR='Energy too large'
                 NC=16
            ELSEIF(ISTVEC(I-1).EQ.-8)THEN
                 COMSTR='Bend too sharp'
                 NC=14
            ELSEIF(ISTVEC(I-1).EQ.-7)THEN
                 COMSTR='Attached'
                 NC=8
            ELSEIF(ISTVEC(I-1).EQ.-6)THEN
                 COMSTR='Left mesh'
                 NC=9
            ELSEIF(ISTVEC(I-1).EQ.-5)THEN
                 COMSTR='Left drift medium'
                 NC=17
            ELSEIF(ISTVEC(I-1).EQ.-4)THEN
                 COMSTR='Plane'
                 NC=5
            ELSEIF(ISTVEC(I-1).EQ.-3)THEN
                 COMSTR='Abnormal'
                 NC=8
            ELSEIF(ISTVEC(I-1).EQ.-2)THEN
                 COMSTR='Too many steps'
                 NC=14
            ELSEIF(ISTVEC(I-1).EQ.-1)THEN
                 COMSTR='Left the area'
                 NC=13
            ELSEIF(ISTVEC(I-1).GT.0.AND.
     -           ISTVEC(I-1).LE.MXWIRE)THEN
                 CALL OUTFMT(REAL(ISTVEC(I-1)),2,AUXSTR,NCAUX,'LEFT')
                 COMSTR='Wire '//AUXSTR(1:NCAUX)
                 NC=5+NCAUX
            ELSEIF(ISTVEC(I-1).GT.MXWIRE.AND.
     -           ISTVEC(I-1).LE.2*MXWIRE)THEN
                 CALL OUTFMT(REAL(ISTVEC(I-1)-MXWIRE),2,
     -                AUXSTR,NCAUX,'LEFT')
                 COMSTR='Replica '//AUXSTR(1:NCAUX)
                 NC=8+NCAUX
            ELSEIF(ISTVEC(I-1).GT.2*MXWIRE.AND.
     -           ISTVEC(I-1).LE.2*MXWIRE+MXSOLI)THEN
                 CALL OUTFMT(REAL(ISTVEC(I-1)-2*MXWIRE),2,
     -                AUXSTR,NCAUX,'LEFT')
                 COMSTR='Solid '//AUXSTR(1:NCAUX)
                 NC=6+NCAUX
            ELSE
                 COMSTR='Unknown'
                 NC=7
            ENDIF
            XTXT=(XPL(1)+XPL(NPL))/2
            YTXT=PMIN+0.02*(PMAX-PMIN)
            CALL GRATTS('COMMENT','TEXT')
            CALL GSTXAL(2,5)
            CALL GRTEXT(XTXT,YTXT,COMSTR(1:NC))
            CALL GSTXAL(0,0)
*   Change of status: plot a vertical bar indicating the separation,
            IF(ISTVEC(I).NE.ISTVEC(I-1))THEN
                 XPL(1)=(POSVEC(I)+POSVEC(I-1))/2
                 XPL(2)=(POSVEC(I)+POSVEC(I-1))/2
                 YPL(1)=PMIN
                 YPL(2)=PMAX
                 CALL GRATTS('COMMENT','POLYLINE')
                 CALL GPL(2,XPL,YPL)
            ENDIF
*   Start a new list or reset the list.
            IF(ISTVEC(I).NE.ISTVEC(I-1))THEN
                 NPL=1
                 XPL(NPL)=POSVEC(I)
                 YPL(NPL)=PLTVEC(I)
            ELSE
                 NPL=0
            ENDIF
**  No change in status: add to buffer.
       ELSE
            IF(NPL.GE.MXLIST)THEN
                 PRINT *,' ###### DRFTR2 ERROR   : Plot buffer'//
     -                ' overflow ; plot closed.'
                 CALL GRNEXT
                 RETURN
            ENDIF
            NPL=NPL+1
            XPL(NPL)=POSVEC(I)
            YPL(NPL)=PLTVEC(I)
       ENDIF
40     CONTINUE
*** Plot any data not yet plotted, if not abandoned or out of steps.
       IF(ISTVEC(NVEC).NE.-2.AND.ISTVEC(NVEC).NE.-3)THEN
            IF(NPL.EQ.1.OR.(MARKER.AND.NPL.GE.1))THEN
                 CALL GRATTS('FUNCTION-1','POLYMARKER')
                 CALL GPM(NPL,XPL,YPL)
            ELSEIF(NPL.GT.1)THEN
                 CALL GRATTS('FUNCTION-1','POLYLINE')
                 CALL GPL(NPL,XPL,YPL)
            ENDIF
       ENDIF
*   A string explaining where the particles ended.
       IF(NPL.GE.1)THEN
            IF(ISTVEC(NVEC).EQ.-9)THEN
                 COMSTR='Energy too large'
                 NC=16
            ELSEIF(ISTVEC(NVEC).EQ.-8)THEN
                 COMSTR='Bend too sharp'
                 NC=14
            ELSEIF(ISTVEC(NVEC).EQ.-7)THEN
                 COMSTR='Attached'
                 NC=8
            ELSEIF(ISTVEC(NVEC).EQ.-6)THEN
                 COMSTR='Left mesh'
                 NC=9
            ELSEIF(ISTVEC(NVEC).EQ.-5)THEN
                 COMSTR='Left drift medium'
                 NC=17
            ELSEIF(ISTVEC(NVEC).EQ.-4)THEN
                 COMSTR='Plane'
                 NC=5
            ELSEIF(ISTVEC(NVEC).EQ.-3)THEN
                 COMSTR='Abnormal'
                 NC=8
            ELSEIF(ISTVEC(NVEC).EQ.-2)THEN
                 COMSTR='Too many steps'
                 NC=14
            ELSEIF(ISTVEC(NVEC).EQ.-1)THEN
                 COMSTR='Left the area'
                 NC=13
            ELSEIF(ISTVEC(NVEC).GT.0.AND.
     -           ISTVEC(NVEC).LE.MXWIRE)THEN
                 CALL OUTFMT(REAL(ISTVEC(NVEC)),2,AUXSTR,NCAUX,'LEFT')
                 COMSTR='Wire '//AUXSTR(1:NCAUX)
                 NC=5+NCAUX
            ELSEIF(ISTVEC(NVEC).GT.MXWIRE.AND.
     -           ISTVEC(NVEC).LE.2*MXWIRE)THEN
                 CALL OUTFMT(REAL(ISTVEC(NVEC)-MXWIRE),2,
     -                AUXSTR,NCAUX,'LEFT')
                 COMSTR='Replica '//AUXSTR(1:NCAUX)
                 NC=8+NCAUX
            ELSEIF(ISTVEC(NVEC).GT.2*MXWIRE.AND.
     -           ISTVEC(NVEC).LE.2*MXWIRE+MXSOLI)THEN
                 CALL OUTFMT(REAL(ISTVEC(NVEC)-2*MXWIRE),2,
     -                AUXSTR,NCAUX,'LEFT')
                 COMSTR='Solid '//AUXSTR(1:NCAUX)
                 NC=6+NCAUX
            ELSE
                 COMSTR='Unknown'
                 NC=7
            ENDIF
            XTXT=(XPL(1)+XPL(NPL))/2
            YTXT=PMIN+0.02*(PMAX-PMIN)
            CALL GRATTS('COMMENT','TEXT')
            CALL GSTXAL(2,5)
            CALL GRTEXT(XTXT,YTXT,COMSTR(1:NC))
            CALL GSTXAL(0,0)
       ENDIF
*** Close this frame etc.
       CALL GRNEXT
       END
