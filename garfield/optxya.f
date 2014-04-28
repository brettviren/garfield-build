CDECK  ID>, OPTXYA.
       SUBROUTINE OPTXYA(XFIT,YFIT,AFIT,WEIGHT,IFAIL)
*-----------------------------------------------------------------------
*   OPTXYA - Routine fixing the X, Y and A vectors for the fit.
*   (Last changed on 20/10/99.)
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
       CHARACTER*(MXCHAR) FUNFLD,FUNPOS,FUNWGT
       CHARACTER*10 VALTYP,PNTTYP
       REAL VST(MXWIRE),VPLST(5)
       LOGICAL EVALT,EVALD,EVALA
       INTEGER NPOINT,NSWIRE,IOPT,NFLD,NPOS,NWGT,IENFLD,IENPOS,IENWGT
       COMMON /OPTDAT/ VST,VPLST,NPOINT,NSWIRE,IOPT,NFLD,NPOS,NWGT,
     -      IENFLD,IENPOS,IENWGT,EVALT,EVALD,EVALA
       COMMON /OPTCHR/ FUNFLD,FUNPOS,FUNWGT,VALTYP,PNTTYP
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
       REAL RES(1),VAR(MXVAR),VSUM
       DOUBLE PRECISION XFIT(MXFPNT),YFIT(MXFPNT),WEIGHT(MXFPNT),
     -      AFIT(MXFPAR)
       INTEGER MODVAR(MXVAR),MODRES(1),IFAIL,I,J,ISWIRE,ISW,IW,IP,NSUM
*** Check the size of the problem.
       IF((PNTTYP.EQ.'TRACK'.AND.NPOINT.GT.MXFPNT).OR.
     -      (PNTTYP.EQ.'WIRE'.AND.NSWIRE.GT.MXFPNT).OR.
     -      (PNTTYP.EQ.'GRID'.AND.NGRIDX*NGRIDY.GT.MXFPNT))THEN
            PRINT *,' !!!!!! OPTXYA WARNING : The number of points'//
     -           ' in the fit is too large ; decrease GRID or POINTS'//
     -           ' as appropriate.'
            IFAIL=1
            RETURN
       ENDIF
       IF((PNTTYP.EQ.'TRACK'.AND.NPOINT.LT.1).OR.
     -      (PNTTYP.EQ.'WIRE'.AND.NSWIRE.LT.1).OR.
     -      (PNTTYP.EQ.'GRID'.AND.NGRIDX*NGRIDY.LT.1))THEN
            PRINT *,' !!!!!! OPTXYA WARNING : The number of points'//
     -           ' in the fit is too small ; increase GRID or POINTS'//
     -           ' as appropriate.'
            IFAIL=1
            RETURN
       ENDIF
*** Loop over the track or ...
       IF(PNTTYP.EQ.'TRACK')THEN
            DO 10 I=1,NPOINT
*   Internal coordinate.
            XFIT(I)=I
*   Position variables.
            VAR(1)=XT0+REAL(I-1)*(XT1-XT0)/REAL(NPOINT-1)
            VAR(2)=YT0+REAL(I-1)*(YT1-YT0)/REAL(NPOINT-1)
            IF(POLAR)CALL CFMCTP(VAR(1),VAR(2),VAR(1),VAR(2),1)
            MODVAR(1)=2
            MODVAR(2)=2
*   Position dependent target function.
            CALL ALGEXE(IENPOS,VAR,MODVAR,2,RES,MODRES,1,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! OPTXYA WARNING : Arithmetic error'//
     -                ' evaluating the position function.'
                 RETURN
            ENDIF
            YFIT(I)=RES(1)
*   Position dependent weighting function.
            CALL ALGEXE(IENWGT,VAR,MODVAR,2,RES,MODRES,1,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! OPTXYA WARNING : Arithmetic error'//
     -                ' evaluating the weighting function.'
                 RETURN
            ELSEIF(RES(1).LE.0.0)THEN
                 PRINT *,' !!!!!! OPTXYA WARNING : The weighting'//
     -                ' function is not >0 at (',VAR(1),',',VAR(2),').'
                 RETURN
            ENDIF
            WEIGHT(I)=RES(1)
10          CONTINUE
*** over the grid or ...
       ELSEIF(PNTTYP.EQ.'GRID')THEN
            DO 30 I=1,NGRIDX
            DO 20 J=1,NGRIDY
*   Internal coordinate.
            XFIT(I+NGRIDX*(J-1))=I+NGRIDX*(J-1)
*   Grid position.
            IF(.NOT.POLAR)THEN
                 VAR(1)=PXMIN+REAL(I-1)*(PXMAX-PXMIN)/REAL(NGRIDX-1)
            ELSE
                 VAR(1)=LOG(EXP(PXMIN)+REAL(I-1)*
     -                (EXP(PXMAX)-EXP(PXMIN))/REAL(NGRIDX-1))
            ENDIF
            VAR(2)=PYMIN+REAL(J-1)*(PYMAX-PYMIN)/REAL(NGRIDY-1)
            IF(POLAR)CALL CFMRTP(VAR(1),VAR(2),VAR(1),VAR(2),1)
            MODVAR(1)=2
            MODVAR(2)=2
*   Position dependent target function.
            CALL ALGEXE(IENPOS,VAR,MODVAR,2,RES,MODRES,1,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! OPTXYA WARNING : Arithmetic error'//
     -                ' evaluating the position function.'
                 RETURN
            ENDIF
            YFIT(I+NGRIDX*(J-1))=RES(1)
*   Position dependent weighting function.
            CALL ALGEXE(IENWGT,VAR,MODVAR,2,RES,MODRES,1,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! OPTXYA WARNING : Arithmetic error'//
     -                ' evaluating the weighting function.'
                 RETURN
            ELSEIF(RES(1).EQ.0.0)THEN
                 PRINT *,' !!!!!! OPTXYA WARNING : The weighting'//
     -                ' function is zero at (',VAR(1),',',VAR(2),').'
                 RETURN
            ENDIF
            WEIGHT(I+NGRIDX*(J-1))=RES(1)
20          CONTINUE
30          CONTINUE
*** over the wire surface or ...
       ELSEIF(PNTTYP.EQ.'WIRE')THEN
            ISWIRE=0
            DO 50 I=1,NWIRE
*   Wire selection.
            IF(WIRTYP(I).NE.'S')GOTO 50
            ISWIRE=ISWIRE+1
*   Internal coordinate.
            XFIT(ISWIRE)=ISWIRE
*   Position.
            VAR(1)=X(I)
            VAR(2)=Y(I)
            IF(POLAR)CALL CFMRTP(VAR(1),VAR(2),VAR(1),VAR(2),1)
            MODVAR(1)=2
            MODVAR(2)=2
*   Position dependent target function.
            CALL ALGEXE(IENPOS,VAR,MODVAR,2,RES,MODRES,1,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! OPTXYA WARNING : Arithmetic error'//
     -                ' evaluating the position function.'
                 RETURN
            ENDIF
            YFIT(ISWIRE)=RES(1)
*   Position dependent weighting function.
            CALL ALGEXE(IENWGT,VAR,MODVAR,2,RES,MODRES,1,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! OPTXYA WARNING : Arithmetic error'//
     -                ' evaluating the weighting function.'
                 RETURN
            ELSEIF(RES(1).EQ.0.0)THEN
                 PRINT *,' !!!!!! OPTXYA WARNING : The weighting'//
     -                ' function is zero for wire ',I,'.'
                 RETURN
            ENDIF
            WEIGHT(ISWIRE)=RES(1)
50          CONTINUE
*** over something unknown.
       ELSE
            PRINT *,' ###### OPTXYA ERROR   : Unknown averaging type ',
     -           PNTTYP,' received; program bug - please report.'
            IFAIL=1
            RETURN
       ENDIF
*** Next set the parameters to be fitted, check size first.
       IF(NSW.GT.MXFPAR)THEN
            PRINT *,' !!!!!! OPTXYA WARNING : The number of'//
     -           ' electrode groups is too large ; decrease to ',
     -           MXFPAR,'.'
            IFAIL=1
            RETURN
       ENDIF
       IF(NSW.LT.1)THEN
            PRINT *,' !!!!!! OPTXYA WARNING : There are no'//
     -           ' electrode groups ; use SELECT to get some.'
            IFAIL=1
            RETURN
       ENDIF
*   Loop over the electrode groups.
       DO 120 ISW=1,NSW
*   Sum the current potential of the fitting parameters.
       VSUM=0.0
       NSUM=0
       DO 130 IW=1,NWIRE
       IF(INDSW(IW).EQ.ISW)THEN
            VSUM=VSUM+V(IW)
            NSUM=NSUM+1
       ENDIF
130    CONTINUE
       DO 140 IP=1,4
       IF(YNPLAN(IP).AND.INDPLA(IP).EQ.ISW)THEN
            VSUM=VSUM+VTPLAN(IP)
            NSUM=NSUM+1
       ENDIF
140    CONTINUE
       IF(TUBE.AND.INDPLA(5).EQ.ISW)THEN
            VSUM=VSUM+VTTUBE
            NSUM=NSUM+1
       ENDIF
*   Take the average.
       IF(NSUM.EQ.0)THEN
            PRINT *,' !!!!!! OPTXYA WARNING : Group ',ISW,' is'//
     -           ' empty; SET not executed.'
            IFAIL=1
            RETURN
       ENDIF
       AFIT(ISW)=VSUM/NSUM
120    CONTINUE
*** Subtract from the original settings and store as starting values.
       DO 150 IW=1,NWIRE
       IF(INDSW(IW).GT.0)THEN
            VST(IW)=V(IW)-AFIT(INDSW(IW))
       ELSE
            VST(IW)=V(IW)
       ENDIF
150    CONTINUE
       DO 160 IP=1,4
       IF(YNPLAN(IP).AND.INDPLA(IP).GT.0)THEN
            VPLST(IP)=VTPLAN(IP)-AFIT(INDPLA(IP))
       ELSE
            VPLST(IP)=VTPLAN(IP)
       ENDIF
160    CONTINUE
       IF(TUBE.AND.INDPLA(5).GT.0)THEN
            VPLST(5)=VTTUBE-AFIT(INDPLA(5))
       ELSE
            VPLST(5)=VTTUBE
       ENDIF
*** Things seem to be OK, set IFAIL to 0 and return.
       IFAIL=0
       END
