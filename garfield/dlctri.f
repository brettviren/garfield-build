CDECK  ID>, DLCTRI.
       SUBROUTINE DLCTRI(XCL,YCL,ZCL,TCL,ICL,SCL,ACL,BCL,FCL,
     -      LDIFF,LAVAL,LATTA,IFAIL)
*-----------------------------------------------------------------------
*   DLCTRI - Interpolates on a track  prepared by DLCTRP. The main
*            objective of this method is to gain lots of speed.
*   VARIABLES : (XCL,YCL,ZCL): Position of the cluster.
*               TCL          : Interpolated drift-time.
*               ICL          : ISTAT code.
*               SCL,ACL,BCL  : Diffusion, avalanche and loss
*               FCL          : Incidence angle on the wire
*   (Last changed on 20/ 5/99.)
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
       LOGICAL FPERX,FPERY,LCROSS,TRASET,TRAFLG,LITAIL,LDTAIL,LRTAIL,
     -      LEPULS,LIPULS,SIGSET,RESSET
       INTEGER NPAIR,ICLUST,NFOUR,MFEXP,MXMIN,MXMAX,
     -      MYMIN,MYMAX,NTRBNK,ITRMAJ,NTIME,NORIA,
     -      NASIMP,JIORD,NISIMP,NMQUAD,NCANG,IENANG
       REAL TIMSIG,SIGNAL,TCLUST,SCLUST,ACLUST,BCLUST,FCLUST,
     -      AVALAN,TSTART,TDEV,PRSTHR,
     -      TRABNK,TRAVEC
       CHARACTER*(MXCHAR) FCNANG
       CHARACTER*12 AVATYP
       CHARACTER*3 FCELTP
       COMMON /SIGDAT/ TIMSIG(MXLIST),SIGNAL(MXLIST,MXSW,2),
     -      AVALAN(2),TRAVEC(MXLIST),
     -      TRABNK(MXLIST,9),TSTART,TDEV,PRSTHR,
     -      TCLUST,SCLUST,ACLUST,BCLUST,FCLUST,ICLUST,NPAIR,
     -      NFOUR,ITRMAJ,JIORD,NISIMP,NMQUAD,IENANG,NTIME,NORIA,
     -      MFEXP,MXMIN,MXMAX,MYMIN,MYMAX,NTRBNK,NASIMP,NCANG,
     -      TRASET,TRAFLG(9),FPERX,FPERY,LCROSS,LITAIL,LDTAIL,LRTAIL,
     -      LEPULS,LIPULS,SIGSET,RESSET
       COMMON /SIGCHR/ FCELTP,AVATYP,FCNANG
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
C       logical ldebug,lident
C       integer lunout
C       parameter(ldebug=.true.,lident=.false.,lunout=6)
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
       REAL XCL,YCL,ZCL,ACL,BCL,SCL,TCL,FCL,DCLUST,VV,TT,VT,DIVDIF,
     -      XSTART,YSTART,ZSTART
       INTEGER ICL,IFAIL,I,ISTART,ISTPRV,IFOUND,NVEC
       LOGICAL LDIFF,LAVAL,LATTA
       EXTERNAL DIVDIF
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE DLCTRI ///'
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCTRI DEBUG   : (x,y,z) '',
     -      3E15.8)') XCL,YCL,ZCL
*** Initialise the IFAIL flag on 1, i.e. fail, set the output to zero.
       IFAIL=1
       TCL=0.0
       SCL=0.0
       ACL=0.0
       BCL=1.0
       FCL=0.0
       ICL=0
*** Return if the track has not been properly prepared.
       IF(.NOT.TRASET.OR.NTRBNK.LE.1)THEN
            PRINT *,' ###### DLCTRI ERROR   : Interpolation cannot be'//
     -           ' performed because the track has not been prepared.'
            RETURN
       ENDIF
*** Check whether the cluster is roughly on the stored track.
       IF(ABS(XCL-TRABNK(1,1))+ABS(YCL-TRABNK(1,2))+
     -      ABS(ZCL-TRABNK(1,3)).LT.ABS(XCL-TRABNK(NTRBNK,1))+
     -      ABS(YCL-TRABNK(NTRBNK,2))+ABS(ZCL-TRABNK(NTRBNK,3)))THEN
            VT=(XCL-TRABNK(1,1))*(TRABNK(NTRBNK,1)-TRABNK(1,1))+
     -           (YCL-TRABNK(1,2))*(TRABNK(NTRBNK,2)-TRABNK(1,2))+
     -           (ZCL-TRABNK(1,3))*(TRABNK(NTRBNK,3)-TRABNK(1,3))
            VV=(XCL-TRABNK(1,1))**2+
     -           (YCL-TRABNK(1,2))**2+
     -           (ZCL-TRABNK(1,3))**2
            TT=(TRABNK(NTRBNK,1)-TRABNK(1,1))**2+
     -           (TRABNK(NTRBNK,2)-TRABNK(1,2))**2+
     -           (TRABNK(NTRBNK,3)-TRABNK(1,3))**2
       ELSE
            VT=(XCL-TRABNK(NTRBNK,1))*(TRABNK(NTRBNK,1)-TRABNK(1,1))+
     -           (YCL-TRABNK(NTRBNK,2))*(TRABNK(NTRBNK,2)-TRABNK(1,2))+
     -           (ZCL-TRABNK(NTRBNK,3))*(TRABNK(NTRBNK,3)-TRABNK(1,3))
            VV=(XCL-TRABNK(NTRBNK,1))**2+
     -           (YCL-TRABNK(NTRBNK,2))**2+
     -           (ZCL-TRABNK(NTRBNK,3))**2
            TT=(TRABNK(NTRBNK,1)-TRABNK(1,1))**2+
     -           (TRABNK(NTRBNK,2)-TRABNK(1,2))**2+
     -           (TRABNK(NTRBNK,3)-TRABNK(1,3))**2
       ENDIF
*** If it isn't, then compute the drift line explicitely.
       IF(VV*TT-VT**2.GT.(1E-2*TT)**2.OR.
     -      (XCL.LT.MIN(TRABNK(3,1),TRABNK(NTRBNK-2,1)).OR.
     -       XCL.GT.MAX(TRABNK(3,1),TRABNK(NTRBNK-2,1))).OR.
     -      (YCL.LT.MIN(TRABNK(3,2),TRABNK(NTRBNK-2,2)).OR.
     -       YCL.GT.MAX(TRABNK(3,2),TRABNK(NTRBNK-2,2))).OR.
     -      (ZCL.LT.MIN(TRABNK(3,3),TRABNK(NTRBNK-2,3)).OR.
     -       ZCL.GT.MAX(TRABNK(3,3),TRABNK(NTRBNK-2,3))))THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCTRI ERROR   :'',
     -           '' Cluster at '',3E15.8/26X,'' is not located'',
     -           '' on the track.'')') XCL,YCL,ZCL
            GOTO 1010
*** Maybe the point is very close to the begin or end point ?
       ELSEIF(SQRT((XCL-TRABNK(3,1))**2+(YCL-TRABNK(3,2))**2+
     -      (ZCL-TRABNK(3,3))**2).LT.
     -      1.0E-4*(1+SQRT(XCL**2+YCL**2+ZCL**2)))THEN
            TCL=TRABNK(3,4)
            SCL=TRABNK(3,5)
            ACL=TRABNK(3,6)
            BCL=TRABNK(3,7)
            ICL=NINT(TRABNK(3,8))
            FCL=TRABNK(3,9)
            IFAIL=0
            IF(LDEBUG)PRINT *,' ++++++ DLCTRI DEBUG   : Cluster'//
     -           ' coincides with track starting point.'
            RETURN
       ELSEIF(SQRT((XCL-TRABNK(NTRBNK-2,1))**2+
     -      (YCL-TRABNK(NTRBNK-2,2))**2+
     -      (ZCL-TRABNK(NTRBNK-2,3))**2).LT.
     -      1.0E-4*(1+SQRT(XCL**2+YCL**2+ZCL**2)))THEN
            TCL=TRABNK(NTRBNK-2,4)
            SCL=TRABNK(NTRBNK-2,5)
            ACL=TRABNK(NTRBNK-2,6)
            BCL=TRABNK(NTRBNK-2,7)
            FCL=TRABNK(NTRBNK-2,9)
            ICL=NINT(TRABNK(NTRBNK-2,8))
            IFAIL=0
            IF(LDEBUG)PRINT *,' ++++++ DLCTRI DEBUG   : Cluster'//
     -           ' coincides with track end point.'
            RETURN
*** Could also be that the cluster is in the end zones.
       ELSEIF((TRABNK(1,1)-XCL)*(XCL-TRABNK(3,1)).GE.0.AND.
     -      (TRABNK(1,2)-YCL)*(YCL-TRABNK(3,2)).GE.0.AND.
     -      (TRABNK(1,3)-ZCL)*(ZCL-TRABNK(3,3)).GE.0)THEN
            IF(LDEBUG)PRINT *,' ++++++ DLCTRI DEBUG  : Cluster in'//
     -           ' start zone.'
            GOTO 1010
       ELSEIF((TRABNK(NTRBNK-2,1)-XCL)*(XCL-TRABNK(NTRBNK,1)).GE.0.AND.
     -      (TRABNK(NTRBNK-2,2)-YCL)*(YCL-TRABNK(NTRBNK,2)).GE.0.AND.
     -      (TRABNK(NTRBNK-2,3)-ZCL)*(ZCL-TRABNK(NTRBNK,3)).GE.0)THEN
            IF(LDEBUG)PRINT *,' ++++++ DLCTRI DEBUG  : Cluster in'//
     -           ' end zone.'
            GOTO 1010
       ENDIF
*** Only cases left of points on the track.
       ISTPRV=NINT(TRABNK(1,8))
       IFOUND=0
       ISTART=3
       DO 10 I=4,NTRBNK-1
*   Check whether this step covers the cluster position.
       IF(ISTPRV.EQ.NINT(TRABNK(I,8)).AND.
     -      ((ITRMAJ.EQ.1.AND.
     -        (TRABNK(I-1,1)-XCL)*(TRABNK(I,1)-XCL).LE.0).OR.
     -       (ITRMAJ.EQ.2.AND.
     -        (TRABNK(I-1,2)-YCL)*(TRABNK(I,2)-YCL).LE.0).OR.
     -       (ITRMAJ.EQ.3.AND.
     -        (TRABNK(I-1,3)-ZCL)*(TRABNK(I,3)-ZCL).LE.0)))IFOUND=I
*   Change of ISTAT, check whether the cluster has been covered.
       IF(ISTPRV.EQ.NINT(TRABNK(I,8)).AND.I.NE.NTRBNK-1)GOTO 10
*   Interpolate if that is the case.
       IF(IFOUND.NE.0)THEN
*   Fix the number of points in the interpolation vector.
            NVEC=I-ISTART
            IF(I.EQ.NTRBNK-1.AND.ISTPRV.EQ.NINT(TRABNK(I,8)))NVEC=NVEC+1
            IF(ISTART+NVEC.GT.NTRBNK-1)NVEC=NTRBNK-ISTART-1
*   Interpolation is not meaningful on a single point, return abend.
            IF(NVEC.LT.NINORD)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCTRI DEBUG   :'',
     -                '' Too few points: '',I5)') NVEC
                 GOTO 1010
*   Interpolate normally with 2 or more points, then return on IFAIL=0.
            ELSE
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCTRI DEBUG   :'',
     -                '' Interpolation from I='',I3,'' to '',I3)')
     -                ISTART,ISTART+NVEC-1
                 DCLUST=ABS(XCL-TRABNK(1,1))+ABS(YCL-TRABNK(1,2))+
     -                ABS(ZCL-TRABNK(1,3))
                 ICL=NINT(TRABNK(ISTART,8))
                 IF(TRAFLG(4))TCL=DIVDIF(TRABNK(ISTART,4),
     -                TRAVEC(ISTART),NVEC,DCLUST,MIN(NINORD,NVEC))
                 IF(TRAFLG(5).AND.LDIFF)SCL=DIVDIF(TRABNK(ISTART,5),
     -                TRAVEC(ISTART),NVEC,DCLUST,MIN(NINORD,NVEC))
                 IF(TRAFLG(6).AND.LAVAL)ACL=DIVDIF(TRABNK(ISTART,6),
     -                TRAVEC(ISTART),NVEC,DCLUST,MIN(NINORD,NVEC))
                 IF(TRAFLG(7).AND.LATTA)BCL=DIVDIF(TRABNK(ISTART,7),
     -                TRAVEC(ISTART),NVEC,DCLUST,MIN(NINORD,NVEC))
                 IF(TRAFLG(9))FCL=DIVDIF(TRABNK(ISTART,9),
     -                TRAVEC(ISTART),NVEC,DCLUST,MIN(NINORD,NVEC))
            ENDIF
            IFAIL=0
            RETURN
*   Reset the current interpolation vector if not.
       ELSE
            ISTPRV=NINT(TRABNK(I,8))
            ISTART=I
       ENDIF
10     CONTINUE
*** Interpolation failed because the cluster is outside the track.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCTRI DEBUG   : Unable to'',
     -      '' interpolate an in-range, colinear cluster.'')')
*** If something fails, compute an explicit drift line.
1010   CONTINUE
       IF(LINCAL)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCTRI DEBUG   :'',
     -           '' Computing a drift line from '',3E12.5)') XCL,YCL,ZCL
       ELSE
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCTRI DEBUG   :'',
     -           '' Drift line from '',3E12.5,'' abandoned.'')')
     -            XCL,YCL,ZCL
            IFAIL=0
            ICL=-3
            RETURN
       ENDIF
*   Set the starting point.
       XSTART=XCL
       YSTART=YCL
       ZSTART=ZCL
       IF(POLAR)CALL CFMCTR(XSTART,YSTART,XSTART,YSTART,1)
       CALL DLCALC(XSTART,YSTART,ZSTART,-1.0,1)
       IF(POLAR)CALL CFMRTC(XSTART,YSTART,XSTART,YSTART,1)
*   Store drift time, diffusion, Townsend, attachment and status.
       TCL=REAL(TU(NU))
       IF(TRAFLG(5).AND.LDIFF)CALL DLCDIF(SCL)
       IF(TRAFLG(6).AND.LAVAL)CALL DLCTWN(ACL)
       IF(TRAFLG(7).AND.LATTA)CALL DLCATT(BCL)
       ICL=ISTAT
       CALL DLCPHI(FCL)
*   End of this calculation.
       IFAIL=0
       END
