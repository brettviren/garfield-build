CDECK  ID>, DRFEQT.
       SUBROUTINE DRFEQT(TSTEP,ISTEQT)
*-----------------------------------------------------------------------
*   DRFEQT - The main routine (DRFEQT) accumulates equal drift time data
*   DRFEQP   which is plotted as a set of contours in the entry DRFEQP,
*   DRFEQR   DRFEQR resets the (error) counters used in the rest and
*   DRFEQE   finally DRFEQE prints the error messages.
*   VARIABLES : NSTORE      : Number of drift lines currently stored
*               NFAIL       : Registers the number of failures.
*               XPL,YPL,ZPL : Used for sorting + plotting.
*               IXYPL       : Drift line which gave this point.
*               XYT         : Stores all equal time contours.
*               BREAK       : .TRUE. if the segment is interrupted by a
*                             drift line and if it is too long.
*               FRSTBR      : The BREAK flag for the first segment.
*   (Last changed on 19/ 6/00.)
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
       DOUBLE PRECISION XU,YU,ZU,TU,XTARG,YTARG,TMC,DMC
       REAL DXMIN,DYMIN,DZMIN,DXMAX,DYMAX,DZMAX,DTARG,EPSDFI,EPSTWI,
     -      EPSATI,RDF2,DSCMIN,DSCMAX,DTFACT,
     -      DDXMIN,DDXMAX,DDYMIN,DDYMAX,DDZMIN,DDZMAX,EPSDIF,RTRAP,
     -      STMAX,EQTTHR,EQTASP,EQTCLS,QPCHAR
       INTEGER NU,ISTAT,ITARG,MXDIFS,MXTWNS,MXATTS,MDF2,
     -      ISTAT1,ISTAT2,ISTAT3,ISTAT4,ISTAT5,ISTAT6,NMC,MCMETH,
     -      IPTYPE,IPTECH
       LOGICAL LREPSK,LKINK,LSTMAX,LEQSRT,LEQCRS,LEQMRK,LAVPRO
       COMMON /DRFDAT/ XU(MXLIST),YU(MXLIST),ZU(MXLIST),TU(MXLIST),
     -      XTARG,YTARG,TMC,DMC,DTARG,
     -      DXMIN,DYMIN,DZMIN,DXMAX,DYMAX,DZMAX,
     -      DDXMIN,DDXMAX,DDYMIN,DDYMAX,DDZMIN,DDZMAX,
     -      EQTTHR,EQTASP,EQTCLS,QPCHAR,
     -      RTRAP,STMAX,EPSDIF,EPSDFI,EPSTWI,EPSATI,RDF2,DSCMIN,DSCMAX,
     -      DTFACT,MDF2,
     -      MXDIFS,MXTWNS,MXATTS,
     -      NU,ISTAT,ITARG,
     -      ISTAT1,ISTAT2,ISTAT3,ISTAT4,ISTAT5,ISTAT6,NMC,MCMETH,IPTYPE,
     -      IPTECH,LREPSK,LKINK,LSTMAX,LEQSRT,LEQCRS,LEQMRK,LAVPRO
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
       REAL USERX0,USERX1,USERY0,USERY1,FRXMIN,FRXMAX,FRYMIN,FRYMAX,
     -      ARRANG,ARRLEN,BARFRC,DISPX0,DISPX1,DISPY0,DISPY1,
     -      GPXN,GPXN10,GPYN,GPYN10,GPXL,GPYL,GPXT
       LOGICAL LGRID,LGRALL,LOGX,LOGY,LSTAMP,LGCLRB,LGCLRA,
     -      LWAITA,LWAITB,LXCCH,LGLCLP,LGMCLP,LGACLP,LGTCLP,
     -      WKMULT(MXWKLS)
       INTEGER NWK,WKID(MXWKLS),WKCON(MXWKLS),WKFREF(MXWKLS),
     -         WKLUN(MXWKLS),WKSTAT(MXWKLS),WKSREQ(MXWKLS),
     -         NCWKNM(MXWKLS),NCSTMP,IGHIST,IGBAR,NCGKS
       CHARACTER*20 WKNAME(MXWKLS),WKATTR(MXWKLS)
       CHARACTER*80 STAMP
       CHARACTER*(MXNAME) GKSLOG
       COMMON /GRADAT/ USERX0,USERX1,USERY0,USERY1,ARRANG,ARRLEN,
     -      BARFRC,
     -      FRXMIN,FRXMAX,FRYMIN,FRYMAX,DISPX0,DISPX1,DISPY0,DISPY1,
     -      GPXN,GPXN10,GPYN,GPYN10,GPXL,GPYL,GPXT,
     -      LGRID,LGRALL,LOGX,LOGY,LSTAMP,LGCLRB,LGCLRA,LWAITA,LWAITB,
     -      LXCCH,LGLCLP,LGMCLP,LGACLP,LGTCLP,
     -      NWK,WKID,WKCON,WKFREF,WKLUN,WKSTAT,WKSREQ,NCWKNM,NCSTMP,
     -      IGHIST,IGBAR,NCGKS,WKMULT
       COMMON /GRACHR/ WKNAME,WKATTR,STAMP,GKSLOG
       REAL TSTEP,TSTREF
       INTEGER IXYT(MXLINE),NXYT(MXLINE),NFAIL(4),IXYPL(MXLINE+1),IOS,
     -      NWORD,INPCMP,NCMEMB,NCREM,INEXT,NWRT,IWRT,IFAIL,IWIRE,IEQT,
     -      IPL,IBEGIN,ISTORE,JSTORE,NCFILE,JEQ,J1,J2,I,NPL,ISTEP,
     -      NSTORE,NSTEP,JSTART,IAUX,IMAX,ISTEQT,NCSTAT
       DOUBLE PRECISION DIVDF2,XYT(MXLINE,0:MXEQUT+1,3),XPL(MXLINE+1),
     -      YPL(MXLINE+1),ZPL(MXLINE+1),APL(MXLINE+1),XCOG,YCOG,
     -      XAUX,YAUX,ZAUX,AAUX,SXX,SXY,SYY,CT,ST,DISTOT,DISMAX,
     -      EPSX,EPSY,EPSZ
       LOGICAL BREAK,FRSTBR,CROSSD,EXMEMB,CIRCLE,DONE(MXLINE)
       CHARACTER*(MXINCH) STRING
       CHARACTER*(MXNAME) FILE
       CHARACTER*80 STATUS
       CHARACTER*29 REMARK
       CHARACTER*8 TIME,DATE,MEMBER
       EXTERNAL CROSSD,DIVDF2,INPCMP
       SAVE XYT,IXYT,NXYT,NSTORE,NFAIL,TSTREF
       DATA NSTORE/0/,NFAIL/0,0,0,0/,TSTREF/-1.0/
*** Main routine, identify if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE DRFEQT ///'
*   Check that the drift line has enough steps.
       IF(NU.LT.3)RETURN
*   Increment the number of stored lines if there is still space.
       IF(NSTORE.GE.MXLINE)THEN
            NFAIL(4)=NFAIL(4)+1
            RETURN
       ENDIF
       NSTORE=NSTORE+1
*   Store the step size.
       TSTREF=TSTEP
*   Find the number of points to be stored, limited by MXEQUT.
       NSTEP=MIN(INT(TU(NU)/TSTEP),MXEQUT)
*   Interpolate (time,position) at start, end and regular t intervals.
       CALL PLACO3(XU(1),YU(1),ZU(1),
     -      XYT(NSTORE,0,1),XYT(NSTORE,0,2),XYT(NSTORE,0,3))
       DO 10 ISTEP=1,NSTEP
       CALL PLACO3(
     -      DIVDF2(XU,TU,NU,DBLE(ISTEP*TSTEP),1),
     -      DIVDF2(YU,TU,NU,DBLE(ISTEP*TSTEP),1),
     -      DIVDF2(ZU,TU,NU,DBLE(ISTEP*TSTEP),1),
     -      XYT(NSTORE,ISTEP,1),XYT(NSTORE,ISTEP,2),XYT(NSTORE,ISTEP,3))
10     CONTINUE
       CALL PLACO3(XU(NU),YU(NU),ZU(NU),XYT(NSTORE,NSTEP+1,1),
     -      XYT(NSTORE,NSTEP+1,2),XYT(NSTORE,NSTEP+1,3))
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DRFEQT DEBUG   : Found '',
     -      I4,'' points for drift line '',I4)') NSTEP,NSTORE
*   Store the number of points on this spline and the d.l. return code.
       NXYT(NSTORE)=NSTEP
       IXYT(NSTORE)=ISTEQT
*   Keep track of the largest (unconstrained by MXEQUT) # of contours.
       NFAIL(3)=MAX(NFAIL(3),INT(TU(NU)/TSTEP))
       RETURN
*** Now plot the data: entry DRFEQP.
       ENTRY DRFEQP
       IF(LIDENT)PRINT *,' /// ENTRY DRFEQP ///'
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DRFEQP DEBUG   : Drawing '',
     -      I4,'' contours, '',I4,'' drift lines.''/
     -      26X,''Connection threshold:   '',F10.3/
     -      26X,''Aspect ratio threshold: '',F10.3/
     -      26X,''Loop closing threshold: '',F10.3/
     -      26X,''Sort contours:          '',L10/
     -      26X,''Check for crossings:    '',L10/
     -      26X,''Mark isochron points:   '',L10)')
     -      NFAIL(3),NSTORE,EQTTHR,EQTASP,EQTCLS,LEQSRT,LEQCRS,LEQMRK
**  Switch to plotting mode for equal time contours.
       CALL GRATTS('ISOCHRON','POLYLINE')
       CALL GRATTS('ISOCHRON','POLYMARKER')
*   Set tolerances.
       EPSX=1D-6*ABS(USERX1-USERX0)
       EPSY=1D-6*ABS(USERY1-USERY0)
       IF(EPSX.LT.1D-6)EPSX=1D-6
       IF(EPSY.LT.1D-6)EPSY=1D-6
       EPSZ=1D-6
       CALL EPSSET('SET',EPSX,EPSY,EPSZ)
**  Loop over the equal time contours.
       DO 1000 IEQT=1,NFAIL(3)
*   Loop over the wires and over the solids.
       DO 1010 IWIRE=-20,2*MXWIRE+NSOLID
       IF((IWIRE.GT.-20.AND.IWIRE.LT.-15).OR.
     -      (IWIRE.GT.-11.AND.IWIRE.LT.1).OR.
     -      (IWIRE.GT.NWIRE.AND.IWIRE.LE.2*MXWIRE))GOTO 1010
*   Initial number of stored points.
       NPL=0
*   Loop over the drift lines, picking up the points when OK.
       DO 1020 ISTORE=1,NSTORE
*   Reject any undesirable combinations.
       IF(IXYT(ISTORE).NE.IWIRE.OR.IEQT.GT.NXYT(ISTORE))GOTO 1020
*   Copy the data of this contour and this wire into the plot vector.
       NPL=NPL+1
       XPL(NPL)=XYT(ISTORE,IEQT,1)
       YPL(NPL)=XYT(ISTORE,IEQT,2)
       ZPL(NPL)=XYT(ISTORE,IEQT,3)
       IXYPL(NPL)=ISTORE
1020   CONTINUE
**  Plot the contour, skip if there are no points.
       IF(NPL.EQ.0)GOTO 1010
*   Skip sorting if not requested, if using markers, if only 1 point.
       IF((.NOT.LEQSRT).OR.LEQMRK.OR.NPL.EQ.1)THEN
            CIRCLE=.FALSE.
            GOTO 1340
       ENDIF
*   Sort contours on angle, first compute centre of gravity.
       XCOG=0
       YCOG=0
       DO 1210 J1=1,NPL
       XCOG=XCOG+XPL(J1)
       YCOG=YCOG+YPL(J1)
1210   CONTINUE
       XCOG=XCOG/REAL(NPL)
       YCOG=YCOG/REAL(NPL)
*   Compute angles wrt to the centre of gravity and principal axes.
       SXX=0
       SXY=0
       SYY=0
       DO 1220 J1=1,NPL
       SXX=SXX+(XPL(J1)-XCOG)**2
       SXY=SXY+(XPL(J1)-XCOG)*(YPL(J1)-YCOG)
       SYY=SYY+(YPL(J1)-YCOG)**2
1220   CONTINUE
       CT=COS(0.5*ATAN2(2*SXY,SXX-SYY))
       ST=SIN(0.5*ATAN2(2*SXY,SXX-SYY))
*   Evaluate dispersions around the principal axes.
       SXX=0
       SYY=0
       DO 1230 J1=1,NPL
       SXX=SXX+ABS(+CT*(XPL(J1)-XCOG)+ST*(YPL(J1)-YCOG))
       SYY=SYY+ABS(-ST*(XPL(J1)-XCOG)+CT*(YPL(J1)-YCOG))
1230   CONTINUE
*   Decide whether this is more linear or more circular.
       IF(  ABS(SXX).GT.EQTASP*ABS(SYY).OR.
     -      ABS(SYY).GT.EQTASP*ABS(SXX))THEN
            CIRCLE=.FALSE.
       ELSE
            CIRCLE=.TRUE.
       ENDIF
*   Set a sorting coordinate accordingly.
       DO 1240 J1=1,NPL
       IF(CIRCLE)THEN
            APL(J1)=ATAN2(YPL(J1)-YCOG,XPL(J1)-XCOG)
       ELSE
            APL(J1)=CT*(XPL(J1)-XCOG)+ST*(YPL(J1)-YCOG)
       ENDIF
1240   CONTINUE
*   Sort the points (bubble sort).
       DO 1250 J1=1,NPL
       DO 1260 J2=J1+1,NPL
       IF(APL(J2).LT.APL(J1))THEN
            IAUX=IXYPL(J1)
            XAUX=XPL(J1)
            YAUX=YPL(J1)
            ZAUX=ZPL(J1)
            AAUX=APL(J1)
            IXYPL(J1)=IXYPL(J2)
            XPL(J1)=XPL(J2)
            YPL(J1)=YPL(J2)
            ZPL(J1)=ZPL(J2)
            APL(J1)=APL(J2)
            IXYPL(J2)=IAUX
            XPL(J2)=XAUX
            YPL(J2)=YAUX
            ZPL(J2)=ZAUX
            APL(J2)=AAUX
       ENDIF
1260   CONTINUE
1250   CONTINUE
*   For circles, pperhaps add the first point to the end of the list.
       IF(CIRCLE)THEN
*   Compute breakpoint, total distance and maximum distance.
            DISTOT=0
            DISMAX=SQRT((XPL(1)-XPL(NPL))**2+(YPL(1)-YPL(NPL))**2)
            IMAX=1
            DO 1270 J1=2,NPL
            DISTOT=DISTOT+SQRT((XPL(J1)-XPL(J1-1))**2+
     -           (YPL(J1)-YPL(J1-1))**2)
            IF(DISMAX.LT.SQRT((XPL(J1)-XPL(J1-1))**2+
     -           (YPL(J1)-YPL(J1-1))**2))THEN
                 DISMAX=SQRT((XPL(J1)-XPL(J1-1))**2+
     -                (YPL(J1)-YPL(J1-1))**2)
                 IMAX=J1
            ENDIF
1270        CONTINUE
*   If a true loop, close it.
            IF(DISMAX.LT.EQTCLS*DISTOT)THEN
                 NPL=NPL+1
                 XPL(NPL)=XPL(1)
                 YPL(NPL)=YPL(1)
                 ZPL(NPL)=ZPL(1)
                 IXYPL(NPL)=IXYPL(1)
*   Otherwise shift the points to make a line.
            ELSEIF(IMAX.GT.1)THEN
                 DO 1280 J1=1,NPL
                 DONE(J1)=.FALSE.
1280             CONTINUE
1290             CONTINUE
                 DO 1300 J1=1,NPL
                 IF(.NOT.DONE(J1))THEN
                      JSTART=J1
                      GOTO 1310
                 ENDIF
1300             CONTINUE
                 GOTO 1330
1310             CONTINUE
                 J2=JSTART
                 J1=1+MOD(J2+IMAX-2,NPL)
                 XAUX=XPL(J2)
                 YAUX=YPL(J2)
                 ZAUX=ZPL(J2)
                 IAUX=IXYPL(J2)
                 DO 1320 I=1,NPL
                 XPL(J2)=XPL(J1)
                 YPL(J2)=YPL(J1)
                 ZPL(J2)=ZPL(J1)
                 IXYPL(J2)=IXYPL(J1)
                 DONE(J2)=.TRUE.
                 IF(J1.EQ.JSTART)THEN
                      XPL(J2)=XAUX
                      YPL(J2)=YAUX
                      ZPL(J2)=ZAUX
                      IXYPL(J2)=IAUX
                      DONE(J2)=.TRUE.
                      GOTO 1290
                 ENDIF
                 J2=J1
                 J1=1+MOD(J2+IMAX-2,NPL)
1320             CONTINUE
1330             CONTINUE
                 CIRCLE=.FALSE.
            ELSE
                 CIRCLE=.FALSE.
            ENDIF
       ENDIF
**  Plot this contour.
1340   CONTINUE
*   Simply mark the contours if this was requested.
       IF(LEQMRK)THEN
            DO 1350 I=1,NPL
            XAUX=FPROJ(1,1)*XPL(I)+FPROJ(2,1)*YPL(I)+
     -           ZPL(I)*FPROJA/FPROJN
            YAUX=FPROJ(1,2)*XPL(I)+FPROJ(2,2)*YPL(I)+
     -           ZPL(I)*FPROJB/FPROJN
            ZAUX=FPROJ(1,3)*XPL(I)+FPROJ(2,3)*YPL(I)+
     -           ZPL(I)*FPROJC/FPROJN
            XPL(I)=XAUX
            YPL(I)=YAUX
            ZPL(I)=ZAUX
1350        CONTINUE
            CALL PLAGPM(NPL,XPL,YPL,ZPL)
            GOTO 1010
       ENDIF
**  Regular plotting.
       IBEGIN=1
       FRSTBR=.FALSE.
       DO 1070 IPL=1,NPL-1
       BREAK=.FALSE.
*   Reject contour segments which are long compared with AREA.
       IF(  ABS(XPL(IPL+1)-XPL(IPL)).GT.(USERX1-USERX0)*EQTTHR.OR.
     -      ABS(YPL(IPL+1)-YPL(IPL)).GT.(USERY1-USERY0)*EQTTHR)
     -      BREAK=.TRUE.
*   Set the BREAK flag if it crosses some stored drift line segment.
       IF(LEQCRS.AND..NOT.BREAK)THEN
            DO 1080 JSTORE=1,NSTORE
            DO 1090 JEQ=0,MXEQUT
            IF(JEQ.GT.NXYT(JSTORE))GOTO 1090
            IF((IXYPL(IPL).EQ.JSTORE.OR.IXYPL(IPL+1).EQ.JSTORE).AND.
     -           (JEQ.EQ.IEQT.OR.JEQ+1.EQ.IEQT))GOTO 1090
            BREAK=CROSSD(
     -           XYT(JSTORE,JEQ  ,1),XYT(JSTORE,JEQ  ,2),
     -           XYT(JSTORE,JEQ+1,1),XYT(JSTORE,JEQ+1,2),
     -           XPL(       IPL    ),YPL(       IPL    ),
     -           XPL(       IPL+1  ),YPL(       IPL+1  ))
            IF(BREAK)GOTO 1100
1090        CONTINUE
1080        CONTINUE
1100        CONTINUE
       ENDIF
*   If there has been a break, plot what we have already.
       IF(BREAK)THEN
            DO 1110 I=IBEGIN,IPL
            XAUX=FPROJ(1,1)*XPL(I)+FPROJ(2,1)*YPL(I)+
     -           ZPL(I)*FPROJA/FPROJN
            YAUX=FPROJ(1,2)*XPL(I)+FPROJ(2,2)*YPL(I)+
     -           ZPL(I)*FPROJB/FPROJN
            ZAUX=FPROJ(1,3)*XPL(I)+FPROJ(2,3)*YPL(I)+
     -           ZPL(I)*FPROJC/FPROJN
            XPL(I)=XAUX
            YPL(I)=YAUX
            ZPL(I)=ZAUX
1110        CONTINUE
            IF(IPL-IBEGIN.GE.1)THEN
                 CALL PLAGPL(IPL-IBEGIN+1,XPL(IBEGIN),YPL(IBEGIN),
     -                ZPL(IBEGIN))
            ELSEIF(IBEGIN.NE.1.OR..NOT.CIRCLE)THEN
                 CALL PLAGPM(1,XPL(IBEGIN),YPL(IBEGIN),ZPL(IBEGIN))
            ELSEIF(IBEGIN.EQ.1)THEN
                 FRSTBR=.TRUE.
            ENDIF
            IBEGIN=IPL+1
       ENDIF
1070   CONTINUE
*   Plot the remainder; if there is a break, put a * if FRSTBR is on.
       DO 1120 I=IBEGIN,NPL
       XAUX=FPROJ(1,1)*XPL(I)+FPROJ(2,1)*YPL(I)+ZPL(I)*FPROJA/FPROJN
       YAUX=FPROJ(1,2)*XPL(I)+FPROJ(2,2)*YPL(I)+ZPL(I)*FPROJB/FPROJN
       ZAUX=FPROJ(1,3)*XPL(I)+FPROJ(2,3)*YPL(I)+ZPL(I)*FPROJC/FPROJN
       XPL(I)=XAUX
       YPL(I)=YAUX
       ZPL(I)=ZAUX
1120   CONTINUE
       IF(.NOT.BREAK.AND.NPL-IBEGIN.GT.0)THEN
            CALL PLAGPL(NPL-IBEGIN+1,XPL(IBEGIN),YPL(IBEGIN),
     -           ZPL(IBEGIN))
       ELSEIF((FRSTBR.OR..NOT.CIRCLE).AND.IBEGIN.EQ.NPL)THEN
            CALL PLAGPM(1,XPL(IBEGIN),YPL(IBEGIN),ZPL(IBEGIN))
       ENDIF
*   Continue with the next combination of wire number and time.
1010   CONTINUE
1000   CONTINUE
*   Reset tolerances.
       CALL EPSSET('RESET',EPSX,EPSY,EPSZ)
**  Log this plot.
       CALL TIMLOG('Plotting equal time contours:           ')
       RETURN
*** Write out the data: entry DRFEQW.
       ENTRY DRFEQW
*   Identify the entry.
       IF(LIDENT)PRINT *,' /// ENTRY DRFEQW ///'
*   Check contour data is present.
       IF(NSTORE.LE.0.OR.TSTREF.LE.0.0)THEN
            PRINT *,' !!!!!! DRFEQW WARNING : No equal time data in'//
     -           ' store; no dataset written.'
            RETURN
       ENDIF
*   Warn if the error codes are non-zero.
       IF(NFAIL(1).GT.0.OR.NFAIL(2).GT.0.OR.
     -      NFAIL(3).GT.0.OR.NFAIL(4).GT.0)THEN
            PRINT *,' ------ DRFEQW MESSAGE : Error messages have'//
     -           ' been issued for the contours to be written out.'
       ENDIF
*   Initial dataset description.
       FILE=' '
       NCFILE=1
       MEMBER='< none >'
       NCMEMB=8
       REMARK='none'
       NCREM=4
*   Make sure there is at least one argument.
       CALL INPNUM(NWORD)
       IF(NWORD.EQ.1)THEN
            PRINT *,' !!!!!! DRFEQW WARNING : WRITE takes at least one',
     -           ' argument (a dataset name); data will not be written.'
            RETURN
*   Check whether keywords have been used.
       ELSEIF(INPCMP(2,'D#ATASET')+INPCMP(2,'R#EMARK').NE.0)THEN
            INEXT=2
            DO 1560 I=2,NWORD
            IF(I.LT.INEXT)GOTO 1560
            IF(INPCMP(I,'D#ATASET').NE.0)THEN
                 IF(INPCMP(I+1,'R#EMARK').NE.0.OR.I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'The dataset name is missing.  ')
                      INEXT=I+1
                 ELSE
                      CALL INPSTR(I+1,I+1,STRING,NCFILE)
                      FILE=STRING
                      INEXT=I+2
                      IF(INPCMP(I+2,'R#EMARK').EQ.0.AND.
     -                     I+2.LE.NWORD)THEN
                           CALL INPSTR(I+2,I+2,STRING,NCMEMB)
                           MEMBER=STRING
                           INEXT=I+3
                      ENDIF
                 ENDIF
            ELSEIF(INPCMP(I,'R#EMARK').NE.0)THEN
                 IF(INPCMP(I+1,'D#ATASET').NE.0.OR.I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'The remark is missing.        ')
                      INEXT=I+1
                 ELSE
                      CALL INPSTR(I+1,I+1,STRING,NCREM)
                      REMARK=STRING
                      INEXT=I+2
                 ENDIF
            ELSE
                 CALL INPMSG(I,'The parameter is not known.   ')
            ENDIF
1560        CONTINUE
*   Otherwise the string is interpreted as a file name (+ member name).
       ELSE
            CALL INPSTR(2,2,STRING,NCFILE)
            FILE=STRING
            IF(NWORD.GE.3)THEN
                 CALL INPSTR(3,3,STRING,NCMEMB)
                 MEMBER=STRING
            ENDIF
            IF(NWORD.GE.4)THEN
                 CALL INPSTR(4,NWORD,STRING,NCREM)
                 REMARK=STRING
            ENDIF
       ENDIF
*   Print error messages.
       CALL INPERR
       IF(NCFILE.GT.MXNAME)PRINT *,' !!!!!! DRFEQW WARNING : The file',
     -      ' name is truncated to MXNAME (=',MXNAME,') characters.'
       IF(NCMEMB.GT.8)PRINT *,' !!!!!! DRFEQW WARNING : The member',
     -      ' name is shortened to ',MEMBER,', first 8 characters.'
       IF(NCREM.GT.29)PRINT *,' !!!!!! DRFEQW WARNING : The remark',
     -      ' shortened to ',REMARK,', first 29 characters.'
       NCFILE=MIN(NCFILE,MXNAME)
       NCMEMB=MIN(NCMEMB,8)
       NCREM=MIN(NCREM,29)
*   Check whether the member already exists.
       CALL DSNREM(FILE(1:NCFILE),MEMBER(1:NCMEMB),'ISOCHRON',EXMEMB)
       IF(JEXMEM.EQ.2.AND.EXMEMB)THEN
            PRINT *,' ------ DRFEQW MESSAGE : A copy of the member'//
     -           ' exists; new member will be appended.'
       ELSEIF(JEXMEM.EQ.3.AND.EXMEMB)THEN
            PRINT *,' !!!!!! DRFEQW WARNING : A copy of the member'//
     -           ' exists already; member will not be written.'
            RETURN
       ENDIF
*   Print some debugging output if requested.
       IF(LDEBUG)THEN
            PRINT *,' ++++++ DRFEQW DEBUG   : File= '//FILE(1:NCFILE)//
     -           ', member= '//MEMBER(1:NCMEMB)
            PRINT *,'                         Remark= '//REMARK(1:NCREM)
       ENDIF
*** Open the dataset for sequential write and inform DSNLOG.
       CALL DSNOPN(FILE,NCFILE,12,'WRITE-LIBRARY',IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! DRFEQW WARNING : Opening '//FILE(1:NCFILE),
     -           ' failed ; the isochrons will not be written.'
            RETURN
       ENDIF
       CALL DSNLOG(FILE,'Isochrons','Sequential','Write     ')
       IF(LDEBUG)PRINT *,' ++++++ DRFEQW DEBUG   : Dataset '//
     -      FILE(1:NCFILE)//' opened on unit 12 for sequential write.'
*   Now write a heading record to the file.
       CALL DATTIM(DATE,TIME)
       WRITE(STRING,'(''% Created '',A8,'' At '',A8,1X,A8,'' ISOCHRON'',
     -      1X,''"'',A29,''"'')') DATE,TIME,MEMBER,REMARK
       WRITE(12,'(A80)',IOSTAT=IOS,ERR=2010) STRING
       WRITE(12,'(2X,''Note: The coordinates listed below'',
     -      '' have been re-converted''/2X,''from the internal'',
     -      '' representation in which they were stored.''/)',
     -      IOSTAT=IOS,ERR=2010)
*   Loop over the wires and the drift lines.
       DO 1500 IEQT=1,NFAIL(3)
       DO 1510 IWIRE=-20,2*MXWIRE+NSOLID
       IF((IWIRE.GT.-20.AND.IWIRE.LT.-15).OR.
     -      (IWIRE.GT.-11.AND.IWIRE.LT.1).OR.
     -      (IWIRE.GT.NWIRE.AND.IWIRE.LE.2*MXWIRE))GOTO 1510
*   Initial number of stored points.
       NWRT=0
*   Loop over the drift lines, picking up the points when OK.
       DO 1520 ISTORE=1,NSTORE
*   Reject any undesirable combinations.
       IF(IXYT(ISTORE).NE.IWIRE.OR.IEQT.GT.NXYT(ISTORE))GOTO 1520
*   Copy the data of this contour and this wire into the output vector.
       NWRT=NWRT+1
       XPL(NWRT)=XYT(ISTORE,IEQT,1)
       YPL(NWRT)=XYT(ISTORE,IEQT,2)
       ZPL(NWRT)=XYT(ISTORE,IEQT,3)
*   Transform back to the original coordinate system.
       XAUX=FPROJ(1,1)*XPL(NWRT)+FPROJ(2,1)*YPL(NWRT)+
     -      ZPL(NWRT)*FPROJA/FPROJN
       YAUX=FPROJ(1,2)*XPL(NWRT)+FPROJ(2,2)*YPL(NWRT)+
     -      ZPL(NWRT)*FPROJB/FPROJN
       ZAUX=FPROJ(1,3)*XPL(NWRT)+FPROJ(2,3)*YPL(NWRT)+
     -      ZPL(NWRT)*FPROJC/FPROJN
       XPL(NWRT)=XAUX
       YPL(NWRT)=YAUX
       ZPL(NWRT)=ZAUX
1520   CONTINUE
*   Header for this combination.
       CALL DLCSTF(IWIRE,STATUS,NCSTAT)
       WRITE(12,'(''  Drift line status: '',A/
     -      ''  Drift time:        '',E12.5,'' [microsec].''/
     -      ''  Data points:       '',I12/)',IOSTAT=IOS,ERR=2010)
     -      STATUS(1:NCSTAT),TSTREF*IEQT,NWRT
       IF(NWRT.GT.0)THEN
*   Write out the list of points.
            IF(POLAR)THEN
                 WRITE(12,'(11X,''r [cm]'',4X,''phi [degrees]'',11X,
     -                ''z [cm]''/)',IOSTAT=IOS,ERR=2010)
            ELSE
                 WRITE(12,'(11X,''x [cm]'',11X,''y [cm]'',11X,
     -                ''z [cm]''/)',IOSTAT=IOS,ERR=2010)
            ENDIF
            WRITE(12,'((3(2X,E15.8)))',IOSTAT=IOS,ERR=2010)
     -           (XPL(IWRT),YPL(IWRT),ZPL(IWRT),IWRT=1,NWRT)
            WRITE(12,'('' '')',IOSTAT=IOS,ERR=2010)
       ENDIF
*   Continue with the next combination of wire number and time.
1510   CONTINUE
1500   CONTINUE
*   Close the file.
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
*   Log writing.
       CALL TIMLOG('Writing out equal time contours:        ')
       RETURN
*** Print error messages, entry DRFEQE.
       ENTRY DRFEQE
       IF(LIDENT)PRINT *,' /// ENTRY DRFEQE ///'
       IF(NFAIL(1).NE.0)PRINT *,' ###### DRFEQT ERROR   : Preparing'//
     -      ' an equal time interpolation failed ',NFAIL(1),' times.'
       IF(NFAIL(2).NE.0)PRINT *,' ###### DRFEQT ERROR   : Obtaining'//
     -      ' an equal time interpolation failed ',NFAIL(2),' times.'
*   Print some error message in case of memory overflow.
       IF(NFAIL(3).GT.0)THEN
            PRINT *,' !!!!!! DRFEQT WARNING : With the time interval'//
     -           ' you specified, ',NFAIL(3),' contours are generated.'
            PRINT *,'                         Increase MXEQUT by this'//
     -           ' value and recompile, to have them all plotted.'
       ENDIF
       IF(NFAIL(4).GT.0)THEN
            PRINT *,' !!!!!! DRFEQT WARNING : MXLINE is smaller than'//
     -           ' the number of drift lines (',NFAIL(4),') to be'
            PRINT *,'                         stored for eqaul time'//
     -           ' contour plotting, increase MXLINE by this value.'
       ENDIF
       RETURN
*** Reset the drift lines: entry DRFEQR.
       ENTRY DRFEQR
       IF(LIDENT)PRINT *,' /// ENTRY DRFEQR ///'
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DRFEQR DEBUG   : Reset of'',
     -      '' isochron buffer.'')')
       NSTORE=0
       DO 1900 I=1,4
       NFAIL(I)=0
1900   CONTINUE
       RETURN
*** Handle the error conditions.
2010   CONTINUE
       PRINT *,' ###### DRFEQW ERROR   : Error while writing'//
     -      ' to ',FILE(1:NCFILE),' via unit 12 ; no contours written.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2030   CONTINUE
       PRINT *,' ###### DRFEQW ERROR   : Dataset '//FILE(1:NCFILE)//
     -      ' unit 12 cannot be closed ; results not predictable'
       CALL INPIOS(IOS)
       END
