CDECK  ID>, CELVIE.
       SUBROUTINE CELVIE(QXMIN,QYMIN,QZMIN,QXMAX,QYMAX,QZMAX)
*-----------------------------------------------------------------------
*   CELVIE - Establishes viewing angles for the chamber.
*   (Last changed on 11/10/11.)
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
       CHARACTER*(MXCHAR) STRING,STRAUX
       CHARACTER*13 AUX1,AUX2,AUX3,AUX4,AUX5,AUX6
       CHARACTER*10 VARLIS(MXVAR)
       INTEGER MODVAR(MXVAR),MODRES(1),NCAUX,NRES,IENTRY,I,J,K,NWORD,NC,
     -      IFAIL,IFAIL1,IFAIL2,IFAIL3,IFAIL4,IFAIL5,IFAIL6,NCOLR,
     -      INEXT,INPCMP,IEQ,IREF,IVOL,IERR,ICOL,NC1,NC2,NC3,NC4,NC5,NC6
       REAL VAR(MXVAR),RES(1),FRES(3,3,3),FXR,FYR,FZR,FNORM,AUXU(3),
     -      AUXV(3),REFR,ABSR,
     -      QXMIN,QYMIN,QZMIN,QXMAX,QYMAX,QZMAX,
     -      QXMIND,QYMIND,QZMIND,QXMAXD,QYMAXD,QZMAXD,
     -      QXMINR,QYMINR,QZMINR,QXMAXR,QYMAXR,QZMAXR
       DOUBLE PRECISION XPL(MXEDGE),YPL(MXEDGE),ZPL(MXEDGE),DET,
     -      XAUX,YAUX,ZAUX
       LOGICAL USE(MXVAR),OK,FLAG(MXWORD+6),VIEW,PROJEC
       EXTERNAL INPCMP
*** Find current number of arguments.
       CALL INPNUM(NWORD)
*** Set default AREA parameters, in a format useful for printing.
       QXMIND=QXMIN
       QYMIND=QYMIN
       QZMIND=QZMIN
       QXMAXD=QXMAX
       QYMAXD=QYMAX
       QZMAXD=QZMAX
       IF(POLAR)CALL CFMRTP(QXMIND,QYMIND,QXMIND,QYMIND,1)
       IF(POLAR)CALL CFMRTP(QXMAXD,QYMAXD,QXMAXD,QYMAXD,1)
*** Show current matrix if no arguments are given.
       IF(NWORD.EQ.1)THEN
            IF(LDEBUG)THEN
                 WRITE(LUNOUT,'(''  In-plane vector of current view: '',
     -                3E12.5)') (FPROJ(3,I),I=1,3)
                 WRITE(LUNOUT,'(''  u-vector in current view plane:  '',
     -                3E12.5)') (FPROJ(1,I),I=1,3)
                 WRITE(LUNOUT,'(''  v-vector in current view plane:  '',
     -                3E12.5)') (FPROJ(2,I),I=1,3)
            ENDIF
            CALL OUTFMT(QXMIND,2,AUX1,NC1,'RIGHT')
            CALL OUTFMT(QXMAXD,2,AUX2,NC2,'LEFT')
            CALL OUTFMT(QYMIND,2,AUX3,NC3,'RIGHT')
            CALL OUTFMT(QYMAXD,2,AUX4,NC4,'LEFT')
            CALL OUTFMT(QZMIND,2,AUX5,NC5,'RIGHT')
            CALL OUTFMT(QZMAXD,2,AUX6,NC6,'LEFT')
            IF(POLAR)THEN
                 WRITE(LUNOUT,'(''  The current area is '',
     -                A13,'' <  r  < '',A13/22X,
     -                A13,'' < phi < '',A13/
     -                ''  [in cm and degrees] '',
     -                A13,'' <  z  < '',A13)')
     -                AUX1,AUX2,AUX3,AUX4,AUX5,AUX6
            ELSE
                 WRITE(LUNOUT,'(''  The current area is '',
     -                A13,'' < x < '',A13/22X,
     -                A13,'' < y < '',A13/
     -                ''  [in cm]             '',
     -                A13,'' < z < '',A13)')
     -                AUX1,AUX2,AUX3,AUX4,AUX5,AUX6
            ENDIF
            CALL OUTFMT(PROROT*180/PI,2,AUX1,NC1,'LEFT')
            WRITE(LUNOUT,'(/''  Current view plane: '',A,'' rotated '',
     -           A,'' degrees''/''  Coordinate axes: u = '',A,'', '',
     -           ''v = '',A)') PROLAB(1:NCFPRO),AUX1(1:NC1),
     -           PXLAB(1:MAX(1,NCXLAB-10)),PYLAB(1:MAX(1,NCYLAB-10))
            IF(PRVIEW.EQ.'X-Y')THEN
                 WRITE(LUNOUT,'(''  Plots show the x-y plane.'')')
            ELSEIF(PRVIEW.EQ.'X-Z')THEN
                 WRITE(LUNOUT,'(''  Plots show the x-z plane.'')')
            ELSEIF(PRVIEW.EQ.'Y-Z')THEN
                 WRITE(LUNOUT,'(''  Plots show the y-z plane.'')')
            ELSEIF(PRVIEW.EQ.'R-PHI')THEN
                 WRITE(LUNOUT,'(''  Plots show the r-phi plane.'')')
            ELSEIF(PRVIEW.EQ.'CUT')THEN
                 WRITE(LUNOUT,'(
     -                ''  Plots show a cut at the above plane.'')')
            ELSEIF(PRVIEW.EQ.'NEBEM')THEN
                 WRITE(LUNOUT,'(''  Plots show neBEM panels.'')')
            ELSEIF(PRVIEW.EQ.'3D')THEN
                 WRITE(LUNOUT,'(''  Plots show a 3D impression.'')')
                 IF(LOUTL)THEN
                      WRITE(LUNOUT,'(''  Outlines of solids are'',
     -                   '' shown.'')')
                 ELSE
                      WRITE(LUNOUT,'(''  Outlines of solids are'',
     -                   '' not shown.'')')
                 ENDIF
            ELSE
                 WRITE(LUNOUT,'(''  ##### Unknown projection '',
     -                A,''.'')') PRVIEW
            ENDIF
            CALL OUTFMT(PRPHIL*180/PI,2,AUX1,NC1,'LEFT')
            CALL OUTFMT(PRTHL*180/PI,2,AUX2,NC2,'LEFT')
            WRITE(LUNOUT,'(/''  Light source placed at phi = '',A,
     -           '', theta = '',A,'' degrees,'')')
     -           AUX1(1:NC1),AUX2(1:NC2)
            CALL OUTFMT((1-PRFABS)*PRFREF*100,2,AUX1,NC1,'LEFT')
            CALL OUTFMT((1-PRFABS)*(1-PRFREF)*100,2,AUX2,NC2,'LEFT')
            CALL OUTFMT(PRFABS*100,2,AUX3,NC3,'LEFT')
            WRITE(LUNOUT,'(''  Of the light, '',A,'' % is absorbed, '',
     -           A,'' % reflected and '',A,'' % diffused.'')')
     -           AUX3(1:NC3),AUX1(1:NC1),AUX2(1:NC2)
            RETURN
       ENDIF
*** First flag the keywords.
       DO 50 I=2,NWORD+6
       IF(INPCMP(I,'ROT#ATE')+INPCMP(I,'ROT#ATION-#ANGLE')+
     -      INPCMP(I,'X-Y')+INPCMP(I,'X-Z')+INPCMP(I,'Y-Z')+
     -      INPCMP(I,'R-PHI')+INPCMP(I,'CUT')+INPCMP(I,'3D')+
     -      INPCMP(I,'NEBEM')+
     -      INPCMP(I,'V#IEW')+INPCMP(I,'PL#ANE')+
     -      INPCMP(I,'LIGHT-#ORIGIN')+
     -      INPCMP(I,'REFL#ECTED-#FRACTION')+
     -      INPCMP(I,'ABS#ORBED-#FRACTION')+
     -      INPCMP(I,'COL#OURS')+
     -      INPCMP(I,'FULL-B#OX-#TICKMARKS')+
     -      INPCMP(I,'PART#IAL-B#OX-#TICKMARKS')+
     -      INPCMP(I,'FULL-T#UBE')+INPCMP(I,'PART#IAL-T#UBE')+
     -      INPCMP(I,'FULL-P#LANES')+INPCMP(I,'PART#IAL-P#LANES')+
     -      INPCMP(I,'SPL#IT-#INTERSECTING-#PLANES')+
     -      INPCMP(I,'NOSPL#IT-#INTERSECTING-#PLANES')+
     -      INPCMP(I,'SORT-#PLANES')+INPCMP(I,'NOSORT-#PLANES')+
     -      INPCMP(I,'OUT#LINE')+INPCMP(I,'NOOUT#LINE')+
     -      INPCMP(I,'PL#OT-MAP')+INPCMP(I,'NOPL#OT-MAP').NE.0)THEN
            FLAG(I)=.TRUE.
       ELSEIF(I.EQ.1.OR.I.GT.NWORD)THEN
            FLAG(I)=.TRUE.
       ELSE
            FLAG(I)=.FALSE.
       ENDIF
50     CONTINUE
*** Get the area component, if specified.
       IF(NWORD.GE.7.AND..NOT.(FLAG(2).OR.FLAG(3).OR.FLAG(4).OR.
     -      FLAG(5).OR.FLAG(6).OR.FLAG(7)))THEN
            CALL INPCHK(2,2,IFAIL1)
            CALL INPCHK(3,2,IFAIL2)
            CALL INPCHK(4,2,IFAIL3)
            CALL INPCHK(5,2,IFAIL4)
            CALL INPCHK(6,2,IFAIL5)
            CALL INPCHK(7,2,IFAIL6)
            CALL INPRDR(2,QXMINR,QXMIND)
            CALL INPRDR(3,QYMINR,QYMIND)
            CALL INPRDR(4,QZMINR,QZMIND)
            CALL INPRDR(5,QXMAXR,QXMAXD)
            CALL INPRDR(6,QYMAXR,QYMAXD)
            CALL INPRDR(7,QZMAXR,QZMAXD)
            INEXT=8
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0.OR.
     -           IFAIL4.NE.0.OR.IFAIL5.NE.0.OR.IFAIL6.NE.0)THEN
                 PRINT *,' !!!!!! CELVIE WARNING : AREA part',
     -                ' ignored because of syntax errors.'
                 GOTO 40
            ENDIF
       ELSEIF(NWORD.GE.5.AND..NOT.(FLAG(2).OR.FLAG(3).OR.FLAG(4).OR.
     -      FLAG(5)))THEN
            CALL INPCHK(2,2,IFAIL1)
            CALL INPCHK(3,2,IFAIL2)
            CALL INPCHK(4,2,IFAIL3)
            CALL INPCHK(5,2,IFAIL4)
            CALL INPRDR(2,QXMINR,QXMIND)
            CALL INPRDR(3,QYMINR,QYMIND)
            CALL INPRDR(4,QXMAXR,QXMAXD)
            CALL INPRDR(5,QYMAXR,QYMAXD)
            QZMINR=QZMIND
            QZMAXR=QZMAXD
            INEXT=6
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.
     -           IFAIL3.NE.0.OR.IFAIL4.NE.0)THEN
                 PRINT *,' !!!!!! CELVIE WARNING : AREA part'//
     -                ' ignored because of syntax errors.'
                 GOTO 40
            ENDIF
       ELSE
            INEXT=2
            GOTO 40
       ENDIF
*   Convert polar boundaries to internal coordinates.
       IFAIL1=0
       IFAIL2=0
       IF(POLAR)THEN
            CALL CFMPTR(QXMINR,QYMINR,QXMINR,QYMINR,1,IFAIL1)
            CALL CFMPTR(QXMAXR,QYMAXR,QXMAXR,QYMAXR,1,IFAIL2)
       ENDIF
*   Perform some elementary checks on these bounds.
       IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)THEN
            PRINT *,' !!!!!! CELVIE WARNING : Incorrect area in'//
     -           ' polar coordinates specified ; AREA is ignored.'
       ELSE
            IF(QXMINR.EQ.QXMAXR)THEN
                 PRINT *,' !!!!!! CELVIE WARNING : Zero range'//
     -                ' not permitted; x or r-part ignored.'
            ELSE
                 QXMIN=MIN(QXMINR,QXMAXR)
                 QXMAX=MAX(QXMINR,QXMAXR)
            ENDIF
            IF(QYMINR.EQ.QYMAXR)THEN
                 PRINT *,' !!!!!! CELVIE WARNING : Zero range'//
     -                ' not permitted; y or phi-part ignored.'
            ELSE
                 QYMIN=MIN(QYMINR,QYMAXR)
                 QYMAX=MAX(QYMINR,QYMAXR)
            ENDIF
       ENDIF
       IF(QZMINR.EQ.QZMAXR)THEN
            PRINT *,' !!!!!! CELVIE WARNING : Zero range'//
     -           ' not permitted; z-part ignored.'
       ELSE
            QZMIN=MIN(QZMINR,QZMAXR)
            QZMAX=MAX(QZMINR,QZMAXR)
       ENDIF
*   Assign them to the graphics area.
       GXMIN=QXMIN
       GYMIN=QYMIN
       GZMIN=QZMIN
       GXMAX=QXMAX
       GYMAX=QYMAX
       GZMAX=QZMAX
*** Get the other options.
40     CONTINUE
*** Default options.
       VIEW=.FALSE.
       PROJEC=.FALSE.
*** Search for further arguments.
       DO 10 I=2,NWORD
       IF(I.LT.INEXT)GOTO 10
**  Viewing plane.
       IF(INPCMP(I,'V#IEW')+INPCMP(I,'PL#ANE').NE.0)THEN
*   Ensure a definition is present.
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'Plane is missing.')
                 GOTO 10
            ENDIF
*   Check the format.
            CALL INPSTR(I+1,I+1,STRING,NC)
            IF(INDEX(STRING(1:NC),'=').EQ.0.AND.I+2.GT.NWORD)THEN
                 CALL INPMSG(I+1,'Incomplete formula.')
                 VIEW=.FALSE.
            ELSEIF(INDEX(STRING(1:NC),'=').EQ.0)THEN
                 CALL INPSTR(I+1,I+2,STRING,NC)
                 INEXT=I+3
                 VIEW=.TRUE.
            ELSE
                 INEXT=I+2
                 VIEW=.TRUE.
            ENDIF
            IEQ=INDEX(STRING(1:NC),'=')
            IF(IEQ.EQ.0.OR.IEQ.GE.LEN(STRING).OR.IEQ.GE.NC)THEN
                 CALL INPMSG(I+1,'= sign missing or misplaced.')
                 VIEW=.FALSE.
            ENDIF
*   Replace the "=" sign.
            IF(VIEW)THEN
                 STRAUX=STRING(IEQ+1:)
                 NCAUX=NC-IEQ
                 STRING(IEQ:)='-('//STRAUX(1:NCAUX)//')'
                 NC=IEQ+NCAUX+2
            ENDIF
*   Reset the rotation.
            PROROT=0
**  Rotation of the local coordinate frame.
       ELSEIF(INPCMP(I,'ROT#ATE')+
     -      INPCMP(I,'ROT#ATION-#ANGLE').NE.0)THEN
            IF(NWORD.LT.I+1)THEN
                 CALL INPMSG(I,'Argument missing.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL)
                 IF(IFAIL.EQ.0)THEN
                      CALL INPRDR(I+1,PROROT,0.0)
                      PROROT=PI*PROROT/180
                 ENDIF
                 INEXT=I+2
            ENDIF
**  Traditional x-y, x-z, y-z and r-phi plots.
       ELSEIF(INPCMP(I,'X-Y').NE.0)THEN
            IF(POLAR)THEN
                 CALL INPMSG(I,'The cell is polar.')
            ELSE
                 PRVIEW='X-Y'
                 PROJEC=.TRUE.
                 FPROJ(1,1)=1
                 FPROJ(1,2)=0
                 FPROJ(1,3)=0
                 FPROJ(2,1)=0
                 FPROJ(2,2)=1
                 FPROJ(2,3)=0
                 FPROJ(3,1)=0
                 FPROJ(3,2)=0
                 FPROJ(3,3)=0
                 PXLAB='x-Axis [cm]'
                 NCXLAB=11
                 PYLAB='y-Axis [cm]'
                 NCYLAB=11
                 PROLAB='x-y'
                 NCFPRO=3
                 FPROJA=0
                 FPROJB=0
                 FPROJC=1
                 FPROJD=0
                 FPROJN=1
                 PROROT=0
                 IF(VIEW)THEN
                      PRINT *,' !!!!!! CELVIE WARNING : Specifying'//
     -                     ' an x-y view cancels the VIEW plane.'
                      VIEW=.FALSE.
                 ENDIF
            ENDIF
       ELSEIF(INPCMP(I,'X-Z').NE.0)THEN
            IF(POLAR)THEN
                 CALL INPMSG(I,'The cell is polar.')
            ELSE
                 PRVIEW='X-Z'
                 PROJEC=.TRUE.
                 FPROJ(1,1)=1
                 FPROJ(1,2)=0
                 FPROJ(1,3)=0
                 FPROJ(2,1)=0
                 FPROJ(2,2)=0
                 FPROJ(2,3)=1
                 FPROJ(3,1)=0
                 FPROJ(3,2)=0
                 FPROJ(3,3)=0
                 PXLAB='x-Axis [cm]'
                 NCXLAB=11
                 PYLAB='z-Axis [cm]'
                 NCYLAB=11
                 PROLAB='x-z'
                 NCFPRO=3
                 FPROJA=0
                 FPROJB=1
                 FPROJC=0
                 FPROJD=0
                 FPROJN=1
                 PROROT=0
                 IF(VIEW)THEN
                      PRINT *,' !!!!!! CELVIE WARNING : Specifying'//
     -                     ' an x-z view cancels the VIEW plane.'
                      VIEW=.FALSE.
                 ENDIF
            ENDIF
       ELSEIF(INPCMP(I,'Y-Z').NE.0)THEN
            IF(POLAR)THEN
                 CALL INPMSG(I,'The cell is polar.')
            ELSE
                 PRVIEW='Y-Z'
                 PROJEC=.TRUE.
                 FPROJ(1,1)=0
                 FPROJ(1,2)=1
                 FPROJ(1,3)=0
                 FPROJ(2,1)=0
                 FPROJ(2,2)=0
                 FPROJ(2,3)=1
                 FPROJ(3,1)=0
                 FPROJ(3,2)=0
                 FPROJ(3,3)=0
                 PXLAB='y-Axis [cm]'
                 NCXLAB=11
                 PYLAB='z-Axis [cm]'
                 NCYLAB=11
                 PROLAB='y-z'
                 NCFPRO=3
                 FPROJA=1
                 FPROJB=0
                 FPROJC=0
                 FPROJD=0
                 FPROJN=1
                 PROROT=0
                 IF(VIEW)THEN
                      PRINT *,' !!!!!! CELVIE WARNING : Specifying'//
     -                     ' an y-z view cancels the VIEW plane.'
                      VIEW=.FALSE.
                 ENDIF
            ENDIF
       ELSEIF(INPCMP(I,'R-PHI').NE.0)THEN
            IF(POLAR)THEN
                 PRVIEW='R-PHI'
                 PROJEC=.TRUE.
                 FPROJ(1,1)=1
                 FPROJ(1,2)=0
                 FPROJ(1,3)=0
                 FPROJ(2,1)=0
                 FPROJ(2,2)=1
                 FPROJ(2,3)=0
                 FPROJ(3,1)=0
                 FPROJ(3,2)=0
                 FPROJ(3,3)=0
                 PXLAB='r-Axis [cm]'
                 NCXLAB=11
                 PYLAB='phi-Axis [degrees]'
                 NCYLAB=18
                 PROLAB='r-phi'
                 NCFPRO=5
                 FPROJA=0
                 FPROJB=0
                 FPROJC=1
                 FPROJD=0
                 FPROJN=1
                 PROROT=0
                 IF(VIEW)THEN
                      PRINT *,' !!!!!! CELVIE WARNING : Specifying'//
     -                     ' an r-phi view cancels the VIEW plane.'
                      VIEW=.FALSE.
                 ENDIF
            ELSE
                 CALL INPMSG(I,'The cell is not polar')
            ENDIF
**  Plot as a cut.
       ELSEIF(INPCMP(I,'CUT').NE.0)THEN
            IF(POLAR)THEN
                 CALL INPMSG(I,'The cell is polar.')
            ELSE
                 PRVIEW='CUT'
                 PROJEC=.TRUE.
            ENDIF
**  Plot in 3D.
       ELSEIF(INPCMP(I,'3D').NE.0)THEN
            IF(POLAR)THEN
                 CALL INPMSG(I,'The cell is polar.')
            ELSE
                 PRVIEW='3D'
                 PROJEC=.TRUE.
            ENDIF
**  Plot in 3D using neBEM panels.
       ELSEIF(INPCMP(I,'NEBEM').NE.0)THEN
            IF(POLAR)THEN
                 CALL INPMSG(I,'The cell is polar.')
            ELSE
                 PRVIEW='NEBEM'
                 PROJEC=.TRUE.
            ENDIF
**  Set the light origin relative to the normal vector.
       ELSEIF(INPCMP(I,'LIGHT-#ORIGIN').NE.0)THEN
            IF(NWORD.LT.I+2)THEN
                 CALL INPMSG(I,'Arguments missing.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,PRPHIL,0.0)
                 CALL INPRDR(I+2,PRTHL,30.0)
                 PRPHIL=PRPHIL*PI/180.0
                 PRTHL=PRTHL*PI/180.0
                 INEXT=I+3
            ENDIF
**  Set the reflection component.
       ELSEIF(INPCMP(I,'REFL#ECTED-#FRACTION').NE.0)THEN
            IF(NWORD.LT.I+1)THEN
                 CALL INPMSG(I,'Arguments missing.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,REFR,PRFREF*100)
                 IF(REFR.LT.0.OR.REFR.GT.100)THEN
                      CALL INPMSG(I+1,'Fraction out of range [0,100].')
                 ELSE
                      PRFREF=REFR/100
                 ENDIF
                 INEXT=I+2
            ENDIF
**  Set the absorbed component.
       ELSEIF(INPCMP(I,'ABS#ORBED-#FRACTION').NE.0)THEN
            IF(NWORD.LT.I+1)THEN
                 CALL INPMSG(I,'Arguments missing.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,ABSR,PRFABS*100)
                 IF(ABSR.LT.0.OR.ABSR.GT.100)THEN
                      CALL INPMSG(I+1,'Fraction out of range [0,100].')
                 ELSE
                      PRFABS=ABSR/100
                 ENDIF
                 INEXT=I+2
            ENDIF
**  Set the number of colours in the shading tables.
       ELSEIF(INPCMP(I,'COL#OURS').NE.0)THEN
            IF(NWORD.LT.I+1)THEN
                 CALL INPMSG(I,'Arguments missing.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NCOLR,NPRCOL)
                 IF(NCOLR.LE.1)THEN
                      CALL INPMSG(I+1,'Number of colours < 2.')
                 ELSE
                      NPRCOL=NCOLR
                 ENDIF
                 INEXT=I+2
            ENDIF
**  Draw full boxes, tube and planes or not.
       ELSEIF(INPCMP(I,'FULL-B#OX-#TICKMARKS').NE.0)THEN
            LFULLB=.TRUE.
       ELSEIF(INPCMP(I,'PART#IAL-B#OX-#TICKMARKS').NE.0)THEN
            LFULLB=.FALSE.
       ELSEIF(INPCMP(I,'FULL-T#UBE').NE.0)THEN
            LFULLT=.TRUE.
       ELSEIF(INPCMP(I,'PART#IAL-T#UBE').NE.0)THEN
            LFULLT=.FALSE.
       ELSEIF(INPCMP(I,'FULL-P#LANES').NE.0)THEN
            LFULLP=.TRUE.
       ELSEIF(INPCMP(I,'PART#IAL-P#LANES').NE.0)THEN
            LFULLP=.FALSE.
       ELSEIF(INPCMP(I,'SPL#IT-#INTERSECTING-#PLANES').NE.0)THEN
            LSPLIT=.TRUE.
       ELSEIF(INPCMP(I,'NOSPL#IT-#INTERSECTING-#PLANES').NE.0)THEN
            LSPLIT=.FALSE.
       ELSEIF(INPCMP(I,'SORT-#PLANES').NE.0)THEN
            LSORT=.TRUE.
       ELSEIF(INPCMP(I,'NOSORT-#PLANES').NE.0)THEN
            LSORT=.FALSE.
       ELSEIF(INPCMP(I,'OUT#LINE').NE.0)THEN
            LOUTL=.TRUE.
       ELSEIF(INPCMP(I,'NOOUT#LINE').NE.0)THEN
            LOUTL=.FALSE.
       ELSEIF(INPCMP(I,'PL#OT-MAP').NE.0)THEN
            LMAPPL=.TRUE.
       ELSEIF(INPCMP(I,'NOPL#OT-MAP').NE.0)THEN
            LMAPPL=.FALSE.
       ELSEIF(INPCMP(I,'STEP').NE.0)THEN
            LGSTEP=.TRUE.
       ELSEIF(INPCMP(I,'NOSTEP').NE.0)THEN
            LGSTEP=.FALSE.
*   Other keywords not known.
       ELSE
            CALL INPMSG(I,'Not a known option.')
       ENDIF
10     CONTINUE
*** R-PHI projection always and only in polar cells.
       IF(POLAR.AND.PRVIEW.NE.'R-PHI')THEN
            PRINT *,' !!!!!! CELVIE WARNING : Only the r-phi view is'//
     -           ' available in polar cells; VIEW ignored.'
            RETURN
       ELSEIF(.NOT.POLAR.AND.PRVIEW.EQ.'R-PHI')THEN
            PRINT *,' !!!!!! CELVIE WARNING : The r-phi view is'//
     -           ' only available in polar cells; VIEW ignored.'
            RETURN
       ENDIF
*** Verify that there is no rotation in x-y, y-z and x-z projections.
       IF((PRVIEW.EQ.'X-Y'.OR.PRVIEW.EQ.'X-Z'.OR.
     -      PRVIEW.EQ.'Y-Z'.OR.PRVIEW.EQ.'R-PHI').AND.PROROT.NE.0)THEN
            PRINT *,' !!!!!! CELVIE WARNING : No rotation angle may'//
     -           ' be specified for an ',PRVIEW,' projection; set to 0.'
            PROROT=0
       ENDIF
*** Set a projection if not explicitely done.
       IF(VIEW.AND.(.NOT.PROJEC).AND.(PRVIEW.NE.'3D'.AND.
     -      PRVIEW.NE.'NEBEM'))PRVIEW='CUT'
*** Progress printing.
       IF(PRVIEW.EQ.'3D'.OR.PRVIEW.EQ.'CUT'.OR.PRVIEW.EQ.'NEBEM')
     -      CALL PROINT('AREA',1,6)
*** If no formula was given, skip most of the rest.
       IF(.NOT.VIEW)GOTO 1000
       IF(PRVIEW.EQ.'3D'.OR.PRVIEW.EQ.'CUT'.OR.PRVIEW.EQ.'NEBEM')THEN
            CALL PROFLD(1,'Processing formula',-1.0)
            CALL PROSTA(1,0.0)
       ENDIF
*** Translate the formula.
       VARLIS(1)='X'
       VARLIS(2)='Y'
       VARLIS(3)='Z'
       CALL ALGPRE(STRING(1:NC),NC,VARLIS,3,NRES,USE,IENTRY,IFAIL)
*   Check the results.
       IF(IFAIL.NE.0)THEN
            CALL INPMSG(2,'Formula not translatable.')
            CALL ALGCLR(IENTRY)
            IF(PRVIEW.EQ.'3D'.OR.PRVIEW.EQ.'CUT'.OR.
     -           PRVIEW.EQ.'NEBEM')CALL PROEND
            RETURN
       ELSEIF(NRES.NE.1)THEN
            CALL INPMSG(2,'Does not return 1 result.')
            CALL ALGCLR(IENTRY)
            IF(PRVIEW.EQ.'3D'.OR.PRVIEW.EQ.'CUT'.OR.
     -           PRVIEW.EQ.'NEBEM')CALL PROEND
            RETURN
       ELSEIF(.NOT.(USE(1).OR.USE(2).OR.USE(3)))THEN
            CALL INPMSG(2,'Does not depend on x, y or z.')
            CALL ALGCLR(IENTRY)
            IF(PRVIEW.EQ.'3D'.OR.PRVIEW.EQ.'CUT'.OR.
     -           PRVIEW.EQ.'NEBEM')CALL PROEND
            RETURN
       ENDIF
*** Compute function values at a (3x3) set of points.
       OK=.TRUE.
       MODVAR(1)=2
       MODVAR(2)=2
       MODVAR(3)=2
       DO 80 I=-1,1
       DO 90 J=-1,1
       DO 100 K=-1,1
       VAR(1)=0.5*(XMIN+XMAX)+I*(1+ABS(XMIN)+ABS(XMAX))
       VAR(2)=0.5*(YMIN+YMAX)+J*(1+ABS(YMIN)+ABS(YMAX))
       VAR(3)=0.5*(ZMIN+ZMAX)+K*(1+ABS(ZMIN)+ABS(ZMAX))
       CALL ALGEXE(IENTRY,VAR,MODVAR,3,RES,MODRES,1,IFAIL)
       IF(IFAIL.NE.0.OR.MODRES(1).NE.2)OK=.FALSE.
       FRES(2+I,2+J,2+K)=RES(1)
100    CONTINUE
90     CONTINUE
80     CONTINUE
*   Ensure that all function evaluations worked.
       IF(.NOT.OK)THEN
            CALL INPMSG(2,'Formula can not be evaluated.')
            CALL ALGCLR(IENTRY)
            IF(PRVIEW.EQ.'3D'.OR.PRVIEW.EQ.'CUT'.OR.
     -           PRVIEW.EQ.'NEBEM')CALL PROEND
            RETURN
       ENDIF
*** Extract parameters.
       FXR=((FRES(3,1,1)-FRES(1,1,1))+
     -      (FRES(3,1,2)-FRES(1,1,2))+
     -      (FRES(3,2,1)-FRES(1,2,1))+
     -      (FRES(3,2,2)-FRES(1,2,2)))/(8*(1+ABS(XMIN)+ABS(XMAX)))
       FYR=((FRES(1,3,1)-FRES(1,1,1))+
     -      (FRES(1,3,2)-FRES(1,1,2))+
     -      (FRES(2,3,1)-FRES(2,1,1))+
     -      (FRES(2,3,2)-FRES(2,1,2)))/(8*(1+ABS(YMIN)+ABS(YMAX)))
       FZR=((FRES(1,1,3)-FRES(1,1,1))+
     -      (FRES(1,2,3)-FRES(1,2,1))+
     -      (FRES(2,1,3)-FRES(2,1,1))+
     -      (FRES(2,2,3)-FRES(2,2,1)))/(8*(1+ABS(ZMIN)+ABS(ZMAX)))
*   Check the linearity and extract parameters.
       IF(ABS(FXR-0.5*(FRES(3,1,1)-FRES(1,1,1))/
     -      (1+ABS(XMIN)+ABS(XMAX))).GT.1E-4*(1+ABS(FXR)).OR.
     -      ABS(FXR-0.5*(FRES(3,1,2)-FRES(1,1,2))/
     -      (1+ABS(XMIN)+ABS(XMAX))).GT.1E-4*(1+ABS(FXR)).OR.
     -      ABS(FXR-0.5*(FRES(3,2,1)-FRES(1,2,1))/
     -      (1+ABS(XMIN)+ABS(XMAX))).GT.1E-4*(1+ABS(FXR)).OR.
     -      ABS(FXR-0.5*(FRES(3,2,2)-FRES(1,2,2))/
     -      (1+ABS(XMIN)+ABS(XMAX))).GT.1E-4*(1+ABS(FXR)).OR.
     -      ABS(FYR-0.5*(FRES(1,3,1)-FRES(1,1,1))/
     -      (1+ABS(YMIN)+ABS(YMAX))).GT.1E-4*(1+ABS(FYR)).OR.
     -      ABS(FYR-0.5*(FRES(1,3,2)-FRES(1,1,2))/
     -      (1+ABS(YMIN)+ABS(YMAX))).GT.1E-4*(1+ABS(FYR)).OR.
     -      ABS(FYR-0.5*(FRES(2,3,1)-FRES(2,1,1))/
     -      (1+ABS(YMIN)+ABS(YMAX))).GT.1E-4*(1+ABS(FYR)).OR.
     -      ABS(FYR-0.5*(FRES(2,3,2)-FRES(2,1,2))/
     -      (1+ABS(YMIN)+ABS(YMAX))).GT.1E-4*(1+ABS(FYR)).OR.
     -      ABS(FZR-0.5*(FRES(1,1,3)-FRES(1,1,1))/
     -      (1+ABS(ZMIN)+ABS(ZMAX))).GT.1E-4*(1+ABS(FZR)).OR.
     -      ABS(FZR-0.5*(FRES(1,2,3)-FRES(1,2,1))/
     -      (1+ABS(ZMIN)+ABS(ZMAX))).GT.1E-4*(1+ABS(FZR)).OR.
     -      ABS(FZR-0.5*(FRES(2,1,3)-FRES(2,1,1))/
     -      (1+ABS(ZMIN)+ABS(ZMAX))).GT.1E-4*(1+ABS(FZR)).OR.
     -      ABS(FZR-0.5*(FRES(2,2,3)-FRES(2,2,1))/
     -      (1+ABS(ZMIN)+ABS(ZMAX))).GT.1E-4*(1+ABS(FZR)))THEN
            CALL INPMSG(2,'Formula is not linear.')
            CALL ALGCLR(IENTRY)
            IF(PRVIEW.EQ.'3D'.OR.PRVIEW.EQ.'CUT'.OR.
     -           PRVIEW.EQ.'NEBEM')CALL PROEND
            RETURN
       ENDIF
*** Establish perpendicular vectors.
       FNORM=SQRT(FXR**2+FYR**2+FZR**2)
       IF(FXR**2+FZR**2.GT.0)THEN
            FPROJ(1,1)= FZR/SQRT(FXR**2+FZR**2)
            FPROJ(1,2)= 0
            FPROJ(1,3)=-FXR/SQRT(FXR**2+FZR**2)
            FPROJ(2,1)=-FXR*FYR/(SQRT(FXR**2+FZR**2)*FNORM)
            FPROJ(2,2)= (FXR**2+FZR**2)/(SQRT(FXR**2+FZR**2)*FNORM)
            FPROJ(2,3)=-FYR*FZR/(SQRT(FXR**2+FZR**2)*FNORM)
            FPROJ(3,1)= FXR
            FPROJ(3,2)= FYR
            FPROJ(3,3)= FZR
       ELSEIF(FYR**2+FZR**2.GT.0)THEN
            FPROJ(1,1)= (FYR**2+FZR**2)/(SQRT(FYR**2+FZR**2)*FNORM)
            FPROJ(1,2)=-FXR*FZR/(SQRT(FYR**2+FZR**2)*FNORM)
            FPROJ(1,3)=-FYR*FZR/(SQRT(FYR**2+FZR**2)*FNORM)
            FPROJ(2,1)= 0
            FPROJ(2,2)= FZR/SQRT(FYR**2+FZR**2)
            FPROJ(2,3)=-FYR/SQRT(FYR**2+FZR**2)
            FPROJ(3,1)= FXR
            FPROJ(3,2)= FYR
            FPROJ(3,3)= FZR
       ELSE
            CALL INPMSG(2,'Does not describe a plane.')
            CALL INPMSG(3,'See previous message.')
            CALL INPMSG(4,'See previous message.')
            CALL ALGCLR(IENTRY)
            IF(PRVIEW.EQ.'3D'.OR.PRVIEW.EQ.'CUT'.OR.
     -           PRVIEW.EQ.'NEBEM')CALL PROEND
            CALL PLAINT
            IF(POLAR)THEN
                 PRVIEW='R-PHI'
            ELSE
                 PRVIEW='X-Y'
            ENDIF
            RETURN
       ENDIF
*** Rotate the vectors.
       DO 20 I=1,3
       AUXU(I)=COS(PROROT)*FPROJ(1,I)-SIN(PROROT)*FPROJ(2,I)
       AUXV(I)=SIN(PROROT)*FPROJ(1,I)+COS(PROROT)*FPROJ(2,I)
20     CONTINUE
       DO 30 I=1,3
       FPROJ(1,I)=AUXU(I)
       FPROJ(2,I)=AUXV(I)
30     CONTINUE
*** Normalise the in-plane vector.
       VAR(1)=0
       VAR(2)=0
       VAR(3)=0
       CALL ALGEXE(IENTRY,VAR,MODVAR,3,RES,MODRES,1,IFAIL)
       IF(IFAIL.NE.0.OR.MODRES(1).NE.2)THEN
            CALL INPMSG(2,'Unable to compute the norm.')
            CALL ALGCLR(IENTRY)
            IF(PRVIEW.EQ.'3D'.OR.PRVIEW.EQ.'CUT'.OR.
     -           PRVIEW.EQ.'NEBEM')CALL PROEND
            CALL PLAINT
            IF(POLAR)THEN
                 PRVIEW='R-PHI'
            ELSE
                 PRVIEW='X-Y'
            ENDIF
            RETURN
       ENDIF
       FPROJ(3,1)=-RES(1)*FPROJ(3,1)/FNORM**2
       FPROJ(3,2)=-RES(1)*FPROJ(3,2)/FNORM**2
       FPROJ(3,3)=-RES(1)*FPROJ(3,3)/FNORM**2
*** Store the plane parameters.
       FPROJA=FXR
       FPROJB=FYR
       FPROJC=FZR
       FPROJN=FNORM
       FPROJD=-RES(1)
*** Delete the entry point.
       CALL ALGCLR(IENTRY)
*** Format the x-axis label.
       IF(PRVIEW.EQ.'3D'.OR.PRVIEW.EQ.'CUT'.OR.
     -           PRVIEW.EQ.'NEBEM')THEN
            CALL PROFLD(1,'Formatting labels',-1.0)
            CALL PROSTA(1,0.0)
       ENDIF
       NCXLAB=0
       PXLAB=' '
       IF(ABS(FPROJ(1,1)-1).LE.1E-4)THEN
            PXLAB(NCXLAB+1:NCXLAB+1)='x'
            NCXLAB=NCXLAB+1
       ELSEIF(ABS(FPROJ(1,1)+1).LE.1E-4)THEN
            PXLAB(NCXLAB+1:NCXLAB+2)='-x'
            NCXLAB=NCXLAB+2
       ELSEIF(FPROJ(1,1).GT.1E-4)THEN
            CALL OUTFMT(REAL(FPROJ(1,1)),2,STRAUX,NCAUX,'LEFT')
            PXLAB(NCXLAB+1:NCXLAB+2+NCAUX)=STRAUX(1:NCAUX)//'*x'
            NCXLAB=NCXLAB+2+NCAUX
       ELSEIF(FPROJ(1,1).LT.-1E-4)THEN
            CALL OUTFMT(REAL(-FPROJ(1,1)),2,STRAUX,NCAUX,'LEFT')
            PXLAB(NCXLAB+1:NCXLAB+3+NCAUX)='-'//STRAUX(1:NCAUX)//'*x'
            NCXLAB=NCXLAB+3+NCAUX
       ENDIF
       IF(ABS(FPROJ(1,2)-1).LE.1E-4)THEN
            IF(NCXLAB.EQ.0)THEN
                 PXLAB(NCXLAB+1:NCXLAB+1)='y'
                 NCXLAB=NCXLAB+1
            ELSE
                 PXLAB(NCXLAB+1:NCXLAB+2)='+y'
                 NCXLAB=NCXLAB+2
            ENDIF
       ELSEIF(ABS(FPROJ(1,2)+1).LE.1E-4)THEN
            PXLAB(NCXLAB+1:NCXLAB+2)='-y'
            NCXLAB=NCXLAB+2
       ELSEIF(FPROJ(1,2).GT.1E-4)THEN
            CALL OUTFMT(REAL(FPROJ(1,2)),2,STRAUX,NCAUX,'LEFT')
            IF(NCXLAB.EQ.0)THEN
                 PXLAB(NCXLAB+1:NCXLAB+2+NCAUX)=STRAUX(1:NCAUX)//'*y'
                 NCXLAB=NCXLAB+2+NCAUX
            ELSE
                 PXLAB(NCXLAB+1:NCXLAB+3+NCAUX)='+'//
     -                STRAUX(1:NCAUX)//'*y'
                 NCXLAB=NCXLAB+3+NCAUX
            ENDIF
       ELSEIF(FPROJ(1,2).LT.-1E-4)THEN
            CALL OUTFMT(REAL(-FPROJ(1,2)),2,STRAUX,NCAUX,'LEFT')
            PXLAB(NCXLAB+1:NCXLAB+3+NCAUX)='-'//STRAUX(1:NCAUX)//'*y'
            NCXLAB=NCXLAB+3+NCAUX
       ENDIF
       IF(ABS(FPROJ(1,3)-1).LE.1E-4)THEN
            IF(NCXLAB.EQ.0)THEN
                 PXLAB(NCXLAB+1:NCXLAB+1)='z'
                 NCXLAB=NCXLAB+1
            ELSE
                 PXLAB(NCXLAB+1:NCXLAB+2)='+z'
                 NCXLAB=NCXLAB+2
            ENDIF
       ELSEIF(ABS(FPROJ(1,3)+1).LE.1E-4)THEN
            PXLAB(NCXLAB+1:NCXLAB+2)='-z'
            NCXLAB=NCXLAB+2
       ELSEIF(FPROJ(1,3).GT.1E-4)THEN
            CALL OUTFMT(REAL(FPROJ(1,3)),2,STRAUX,NCAUX,'LEFT')
            IF(NCXLAB.EQ.0)THEN
                 PXLAB(NCXLAB+1:NCXLAB+2+NCAUX)=STRAUX(1:NCAUX)//'*z'
                 NCXLAB=NCXLAB+2+NCAUX
            ELSE
                 PXLAB(NCXLAB+1:NCXLAB+3+NCAUX)='+'//
     -                STRAUX(1:NCAUX)//'*z'
                 NCXLAB=NCXLAB+3+NCAUX
            ENDIF
       ELSEIF(FPROJ(1,3).LT.-1E-4)THEN
            CALL OUTFMT(REAL(-FPROJ(1,3)),2,STRAUX,NCAUX,'LEFT')
            PXLAB(NCXLAB+1:NCXLAB+3+NCAUX)='-'//STRAUX(1:NCAUX)//'*z'
            NCXLAB=NCXLAB+3+NCAUX
       ENDIF
       PXLAB(NCXLAB+1:NCXLAB+10)=' Axis [cm]'
       IF(NCXLAB.EQ.1)PXLAB(2:2)='-'
       NCXLAB=NCXLAB+10
*   Format the y-axis label.
       NCYLAB=0
       PYLAB=' '
       IF(ABS(FPROJ(2,1)-1).LE.1E-4)THEN
            PYLAB(NCYLAB+1:NCYLAB+1)='x'
            NCYLAB=NCYLAB+1
       ELSEIF(ABS(FPROJ(2,1)+1).LE.1E-4)THEN
            PYLAB(NCYLAB+1:NCYLAB+2)='-x'
            NCYLAB=NCYLAB+2
       ELSEIF(FPROJ(2,1).GT.1E-4)THEN
            CALL OUTFMT(REAL(FPROJ(2,1)),2,STRAUX,NCAUX,'LEFT')
            PYLAB(NCYLAB+1:NCYLAB+2+NCAUX)=STRAUX(1:NCAUX)//'*x'
            NCYLAB=NCYLAB+2+NCAUX
       ELSEIF(FPROJ(2,1).LT.-1E-4)THEN
            CALL OUTFMT(REAL(-FPROJ(2,1)),2,STRAUX,NCAUX,'LEFT')
            PYLAB(NCYLAB+1:NCYLAB+3+NCAUX)='-'//STRAUX(1:NCAUX)//'*x'
            NCYLAB=NCYLAB+3+NCAUX
       ENDIF
       IF(ABS(FPROJ(2,2)-1).LE.1E-4)THEN
            IF(NCYLAB.EQ.0)THEN
                 PYLAB(NCYLAB+1:NCYLAB+1)='y'
                 NCYLAB=NCYLAB+1
            ELSE
                 PYLAB(NCYLAB+1:NCYLAB+2)='+y'
                 NCYLAB=NCYLAB+2
            ENDIF
       ELSEIF(ABS(FPROJ(2,2)+1).LE.1E-4)THEN
            PYLAB(NCYLAB+1:NCYLAB+2)='-y'
            NCYLAB=NCYLAB+2
       ELSEIF(FPROJ(2,2).GT.1E-4)THEN
            CALL OUTFMT(REAL(FPROJ(2,2)),2,STRAUX,NCAUX,'LEFT')
            IF(NCYLAB.EQ.0)THEN
                 PYLAB(NCYLAB+1:NCYLAB+2+NCAUX)=STRAUX(1:NCAUX)//'*y'
                 NCYLAB=NCYLAB+2+NCAUX
            ELSE
                 PYLAB(NCYLAB+1:NCYLAB+3+NCAUX)='+'//
     -                STRAUX(1:NCAUX)//'*y'
                 NCYLAB=NCYLAB+3+NCAUX
            ENDIF
       ELSEIF(FPROJ(2,2).LT.-1E-4)THEN
            CALL OUTFMT(REAL(-FPROJ(2,2)),2,STRAUX,NCAUX,'LEFT')
            PYLAB(NCYLAB+1:NCYLAB+3+NCAUX)='-'//STRAUX(1:NCAUX)//'*y'
            NCYLAB=NCYLAB+3+NCAUX
       ENDIF
       IF(ABS(FPROJ(2,3)-1).LE.1E-4)THEN
            IF(NCYLAB.EQ.0)THEN
                 PYLAB(NCYLAB+1:NCYLAB+1)='z'
                 NCYLAB=NCYLAB+1
            ELSE
                 PYLAB(NCYLAB+1:NCYLAB+2)='+z'
                 NCYLAB=NCYLAB+2
            ENDIF
       ELSEIF(ABS(FPROJ(2,3)+1).LE.1E-4)THEN
            PYLAB(NCYLAB+1:NCYLAB+2)='-z'
            NCYLAB=NCYLAB+2
       ELSEIF(FPROJ(2,3).GT.1E-4)THEN
            CALL OUTFMT(REAL(FPROJ(2,3)),2,STRAUX,NCAUX,'LEFT')
            IF(NCYLAB.EQ.0)THEN
                 PYLAB(NCYLAB+1:NCYLAB+2+NCAUX)=STRAUX(1:NCAUX)//'*z'
                 NCYLAB=NCYLAB+2+NCAUX
            ELSE
                 PYLAB(NCYLAB+1:NCYLAB+3+NCAUX)='+'//
     -                STRAUX(1:NCAUX)//'*z'
                 NCYLAB=NCYLAB+3+NCAUX
            ENDIF
       ELSEIF(FPROJ(2,3).LT.-1E-4)THEN
            CALL OUTFMT(REAL(-FPROJ(2,3)),2,STRAUX,NCAUX,'LEFT')
            PYLAB(NCYLAB+1:NCYLAB+3+NCAUX)='-'//STRAUX(1:NCAUX)//'*z'
            NCYLAB=NCYLAB+3+NCAUX
       ENDIF
       PYLAB(NCYLAB+1:NCYLAB+10)=' Axis [cm]'
       IF(NCYLAB.EQ.1)PYLAB(2:2)='-'
       NCYLAB=NCYLAB+10
*   Format the plane description.
       NCFPRO=0
       PROLAB=' '
       IF(ABS(FPROJA-1).LE.1E-4)THEN
            PROLAB(NCFPRO+1:NCFPRO+1)='x'
            NCFPRO=NCFPRO+1
       ELSEIF(ABS(FPROJA+1).LE.1E-4)THEN
            PROLAB(NCFPRO+1:NCFPRO+2)='-x'
            NCFPRO=NCFPRO+2
       ELSEIF(FPROJA.GT.1E-4)THEN
            CALL OUTFMT(REAL(FPROJA),2,STRAUX,NCAUX,'LEFT')
            PROLAB(NCFPRO+1:NCFPRO+2+NCAUX)=STRAUX(1:NCAUX)//'*x'
            NCFPRO=NCFPRO+2+NCAUX
       ELSEIF(FPROJA.LT.-1E-4)THEN
            CALL OUTFMT(REAL(-FPROJA),2,STRAUX,NCAUX,'LEFT')
            PROLAB(NCFPRO+1:NCFPRO+3+NCAUX)='-'//STRAUX(1:NCAUX)//'*x'
            NCFPRO=NCFPRO+3+NCAUX
       ENDIF
       IF(ABS(FPROJB-1).LE.1E-4)THEN
            IF(NCFPRO.EQ.0)THEN
                 PROLAB(NCFPRO+1:NCFPRO+1)='y'
                 NCFPRO=NCFPRO+1
            ELSE
                 PROLAB(NCFPRO+1:NCFPRO+2)='+y'
                 NCFPRO=NCFPRO+2
            ENDIF
       ELSEIF(ABS(FPROJB+1).LE.1E-4)THEN
            PROLAB(NCFPRO+1:NCFPRO+2)='-y'
            NCFPRO=NCFPRO+2
       ELSEIF(FPROJB.GT.1E-4)THEN
            CALL OUTFMT(REAL(FPROJB),2,STRAUX,NCAUX,'LEFT')
            IF(NCFPRO.EQ.0)THEN
                 PROLAB(NCFPRO+1:NCFPRO+2+NCAUX)=STRAUX(1:NCAUX)//'*y'
                 NCFPRO=NCFPRO+2+NCAUX
            ELSE
                 PROLAB(NCFPRO+1:NCFPRO+3+NCAUX)='+'//
     -                STRAUX(1:NCAUX)//'*y'
                 NCFPRO=NCFPRO+3+NCAUX
            ENDIF
       ELSEIF(FPROJB.LT.-1E-4)THEN
            CALL OUTFMT(REAL(-FPROJB),2,STRAUX,NCAUX,'LEFT')
            PROLAB(NCFPRO+1:NCFPRO+3+NCAUX)='-'//STRAUX(1:NCAUX)//'*y'
            NCFPRO=NCFPRO+3+NCAUX
       ENDIF
       IF(ABS(FPROJC-1).LE.1E-4)THEN
            IF(NCFPRO.EQ.0)THEN
                 PROLAB(NCFPRO+1:NCFPRO+1)='z'
                 NCFPRO=NCFPRO+1
            ELSE
                 PROLAB(NCFPRO+1:NCFPRO+2)='+z'
                 NCFPRO=NCFPRO+2
            ENDIF
       ELSEIF(ABS(FPROJC+1).LE.1E-4)THEN
            PROLAB(NCFPRO+1:NCFPRO+2)='-z'
            NCFPRO=NCFPRO+2
       ELSEIF(FPROJC.GT.1E-4)THEN
            CALL OUTFMT(REAL(FPROJC),2,STRAUX,NCAUX,'LEFT')
            IF(NCFPRO.EQ.0)THEN
                 PROLAB(NCFPRO+1:NCFPRO+2+NCAUX)=STRAUX(1:NCAUX)//'*z'
                 NCFPRO=NCFPRO+2+NCAUX
            ELSE
                 PROLAB(NCFPRO+1:NCFPRO+3+NCAUX)='+'//
     -                STRAUX(1:NCAUX)//'*z'
                 NCFPRO=NCFPRO+3+NCAUX
            ENDIF
       ELSEIF(FPROJC.LT.-1E-4)THEN
            CALL OUTFMT(REAL(-FPROJC),2,STRAUX,NCAUX,'LEFT')
            PROLAB(NCFPRO+1:NCFPRO+3+NCAUX)='-'//STRAUX(1:NCAUX)//'*z'
            NCFPRO=NCFPRO+3+NCAUX
       ENDIF
       PROLAB(NCFPRO+1:NCFPRO+1)='='
       NCFPRO=NCFPRO+1
       CALL OUTFMT(REAL(FPROJD),2,STRAUX,NCAUX,'LEFT')
       PROLAB(NCFPRO+1:NCFPRO+NCAUX)=STRAUX(1:NCAUX)
       NCFPRO=NCFPRO+NCAUX
*** Next generate the tables.
1000   CONTINUE
*** Prepare the projection matrix.
       FPRMAT(1,1)=FPROJ(1,1)
       FPRMAT(2,1)=FPROJ(1,2)
       FPRMAT(3,1)=FPROJ(1,3)
       FPRMAT(1,2)=FPROJ(2,1)
       FPRMAT(2,2)=FPROJ(2,2)
       FPRMAT(3,2)=FPROJ(2,3)
       FNORM=SQRT(FPROJA**2+FPROJB**2+FPROJC**2)
       IF(FNORM.LE.0)THEN
            PRINT *,' !!!!!! CELVIE WARNING : Zero norm vector'//
     -           ' of viewing plane; reset to default.'
            IF(PRVIEW.EQ.'3D'.OR.PRVIEW.EQ.'CUT'.OR.
     -           PRVIEW.EQ.'NEBEM')CALL PROEND
            CALL PLAINT
            IF(POLAR)THEN
                 PRVIEW='R-PHI'
            ELSE
                 PRVIEW='X-Y'
            ENDIF
            RETURN
       ENDIF
       FPRMAT(1,3)=FPROJA/FNORM
       FPRMAT(2,3)=FPROJB/FNORM
       FPRMAT(3,3)=FPROJC/FNORM
*   Solve the matrix.
       CALL DFACT(3,FPRMAT,3,IPRMAT,IFAIL1,DET,IFAIL2)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ CELVIE DEBUG   :'',
     -      '' Determinant of projection: '',E15.8)') DET
       IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)THEN
            PRINT *,' !!!!!! CELVIE WARNING  : Unable to solve'//
     -           ' the projection matrix; reset to default.'
            IF(PRVIEW.EQ.'3D'.OR.PRVIEW.EQ.'CUT'.OR.
     -           PRVIEW.EQ.'NEBEM')CALL PROEND
            CALL PLAINT
            IF(POLAR)THEN
                 PRVIEW='R-PHI'
            ELSE
                 PRVIEW='X-Y'
            ENDIF
            RETURN
       ENDIF
*   Compute the, at most, 6 distinct crossings between plane and box.
       NGBOX=0
       CALL PLALIN(GXMIN,GYMIN,GZMIN, GXMAX,GYMIN,GZMIN,
     -      FPROJ(3,1),FPROJ(3,2),FPROJ(3,3),
     -      FPROJA,FPROJB,FPROJC,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NGBOX=NGBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,
     -           GXBOX(NGBOX),GYBOX(NGBOX),GZBOX(NGBOX))
       ENDIF
       CALL PLALIN(GXMIN,GYMIN,GZMIN, GXMIN,GYMAX,GZMIN,
     -      FPROJ(3,1),FPROJ(3,2),FPROJ(3,3),
     -      FPROJA,FPROJB,FPROJC,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NGBOX=NGBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,
     -           GXBOX(NGBOX),GYBOX(NGBOX),GZBOX(NGBOX))
       ENDIF
       CALL PLALIN(GXMIN,GYMIN,GZMIN, GXMIN,GYMIN,GZMAX,
     -      FPROJ(3,1),FPROJ(3,2),FPROJ(3,3),
     -      FPROJA,FPROJB,FPROJC,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NGBOX=NGBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,
     -           GXBOX(NGBOX),GYBOX(NGBOX),GZBOX(NGBOX))
       ENDIF
       CALL PLALIN(GXMAX,GYMAX,GZMIN, GXMIN,GYMAX,GZMIN,
     -      FPROJ(3,1),FPROJ(3,2),FPROJ(3,3),
     -      FPROJA,FPROJB,FPROJC,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NGBOX=NGBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,
     -           GXBOX(NGBOX),GYBOX(NGBOX),GZBOX(NGBOX))
       ENDIF
       CALL PLALIN(GXMAX,GYMAX,GZMIN, GXMAX,GYMIN,GZMIN,
     -      FPROJ(3,1),FPROJ(3,2),FPROJ(3,3),
     -      FPROJA,FPROJB,FPROJC,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NGBOX=NGBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,
     -           GXBOX(NGBOX),GYBOX(NGBOX),GZBOX(NGBOX))
       ENDIF
       CALL PLALIN(GXMAX,GYMAX,GZMIN, GXMAX,GYMAX,GZMAX,
     -      FPROJ(3,1),FPROJ(3,2),FPROJ(3,3),
     -      FPROJA,FPROJB,FPROJC,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NGBOX=NGBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,
     -           GXBOX(NGBOX),GYBOX(NGBOX),GZBOX(NGBOX))
       ENDIF
       CALL PLALIN(GXMAX,GYMIN,GZMAX, GXMIN,GYMIN,GZMAX,
     -      FPROJ(3,1),FPROJ(3,2),FPROJ(3,3),
     -      FPROJA,FPROJB,FPROJC,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NGBOX=NGBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,
     -           GXBOX(NGBOX),GYBOX(NGBOX),GZBOX(NGBOX))
       ENDIF
       CALL PLALIN(GXMAX,GYMIN,GZMAX, GXMAX,GYMAX,GZMAX,
     -      FPROJ(3,1),FPROJ(3,2),FPROJ(3,3),
     -      FPROJA,FPROJB,FPROJC,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NGBOX=NGBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,
     -           GXBOX(NGBOX),GYBOX(NGBOX),GZBOX(NGBOX))
       ENDIF
       CALL PLALIN(GXMAX,GYMIN,GZMAX, GXMAX,GYMIN,GZMIN,
     -      FPROJ(3,1),FPROJ(3,2),FPROJ(3,3),
     -      FPROJA,FPROJB,FPROJC,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NGBOX=NGBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,
     -           GXBOX(NGBOX),GYBOX(NGBOX),GZBOX(NGBOX))
       ENDIF
       CALL PLALIN(GXMIN,GYMAX,GZMAX, GXMAX,GYMAX,GZMAX,
     -      FPROJ(3,1),FPROJ(3,2),FPROJ(3,3),
     -      FPROJA,FPROJB,FPROJC,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NGBOX=NGBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,
     -           GXBOX(NGBOX),GYBOX(NGBOX),GZBOX(NGBOX))
       ENDIF
       CALL PLALIN(GXMIN,GYMAX,GZMAX, GXMIN,GYMIN,GZMAX,
     -      FPROJ(3,1),FPROJ(3,2),FPROJ(3,3),
     -      FPROJA,FPROJB,FPROJC,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NGBOX=NGBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,
     -           GXBOX(NGBOX),GYBOX(NGBOX),GZBOX(NGBOX))
       ENDIF
       CALL PLALIN(GXMIN,GYMAX,GZMAX, GXMIN,GYMAX,GZMIN,
     -      FPROJ(3,1),FPROJ(3,2),FPROJ(3,3),
     -      FPROJA,FPROJB,FPROJC,XAUX,YAUX,ZAUX,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NGBOX=NGBOX+1
            CALL PLACO3(XAUX,YAUX,ZAUX,
     -           GXBOX(NGBOX),GYBOX(NGBOX),GZBOX(NGBOX))
       ENDIF
*   Ensure there is no butterfly.
       CALL BUTFLD(NGBOX,GXBOX,GYBOX,GZBOX)
*   Establish light direction.
       PRAL=+COS(PRPHIL)*COS(PRTHL)*FPROJA-SIN(PRPHIL)*FPROJB+
     -       COS(PRPHIL)*SIN(PRTHL)*FPROJC
       PRBL=+SIN(PRPHIL)*COS(PRTHL)*FPROJA+COS(PRPHIL)*FPROJB+
     -       SIN(PRPHIL)*SIN(PRTHL)*FPROJC
       PRCL=            -SIN(PRTHL)*FPROJA+
     -                   COS(PRTHL)*FPROJC
       FNORM=SQRT(PRAL**2+PRBL**2+PRCL**2)
       IF(FNORM.GT.0)THEN
            PRAL=PRAL/FNORM
            PRBL=PRBL/FNORM
            PRCL=PRCL/FNORM
       ENDIF
*   Reset the buffer of the panels.
       IF(PRVIEW.NE.'NEBEM')CALL PLABU1('RESET',IREF,0,XPL,YPL,ZPL,
     -      0.0D0,0.0D0,0.0D0,0,0,IFAIL)
**  Copy the wires to the solids.
       IF(NWIRE.NE.0.AND.NSOLID.EQ.0.AND.
     -      (PRVIEW.EQ.'3D'.OR.PRVIEW.EQ.'CUT'.OR.
     -      PRVIEW.EQ.'NEBEM'))
     -      CALL CELCNW(QXMIN,QYMIN,QXMAX,QYMAX)
**  Generate the plot panels for each solid, first for 3D views.
       IF(PRVIEW.EQ.'3D')THEN
*   Prepare colour tables.
            ICOL0=30
            ICOLBX=0
            ICOLPL=0
            ICOLST=0
            ICOLW1=0
            ICOLW2=0
            ICOLW3=0
            ICOLD1=0
            ICOLD2=0
            ICOLD3=0
            ICOLRB=0
*   Loop over the volumes.
            CALL PROFLD(1,'Generating volumes',REAL(NSOLID))
            DO 1010 IVOL=1,NSOLID
            CALL PROSTA(1,REAL(IVOL))
*   Assign the colour index, generating colour tables as required.
            IF(ISOLMT(IVOL).EQ.1)THEN
                 IF(ICOLW1.EQ.0)THEN
                      CALL GRATTS('CONDUCTORS-1','AREA')
                      ICOLW1=ICOL0
                      CALL COLSHD(ICOLW1)
                      ICOL0=ICOL0+NPRCOL
                 ENDIF
                 ICOL=ICOLW1
            ELSEIF(ISOLMT(IVOL).EQ.2)THEN
                 IF(ICOLW2.EQ.0)THEN
                      CALL GRATTS('CONDUCTORS-2','AREA')
                      ICOLW2=ICOL0
                      CALL COLSHD(ICOLW2)
                      ICOL0=ICOL0+NPRCOL
                 ENDIF
                 ICOL=ICOLW2
            ELSEIF(ISOLMT(IVOL).EQ.3)THEN
                 IF(ICOLW3.EQ.0)THEN
                      CALL GRATTS('CONDUCTORS-3','AREA')
                      ICOLW3=ICOL0
                      CALL COLSHD(ICOLW3)
                      ICOL0=ICOL0+NPRCOL
                 ENDIF
                 ICOL=ICOLW3
            ELSEIF(ISOLMT(IVOL).EQ.11)THEN
                 IF(ICOLD1.EQ.0)THEN
                      CALL GRATTS('DIELECTRIC-1','AREA')
                      ICOLD1=ICOL0
                      CALL COLSHD(ICOLD1)
                      ICOL0=ICOL0+NPRCOL
                 ENDIF
                 ICOL=ICOLD1
            ELSEIF(ISOLMT(IVOL).EQ.12)THEN
                 IF(ICOLD2.EQ.0)THEN
                      CALL GRATTS('DIELECTRIC-2','AREA')
                      ICOLD2=ICOL0
                      CALL COLSHD(ICOLD2)
                      ICOL0=ICOL0+NPRCOL
                 ENDIF
                 ICOL=ICOLD2
            ELSEIF(ISOLMT(IVOL).EQ.13)THEN
                 IF(ICOLD3.EQ.0)THEN
                      CALL GRATTS('DIELECTRIC-3','AREA')
                      ICOLD3=ICOL0
                      CALL COLSHD(ICOLD3)
                      ICOL0=ICOL0+NPRCOL
                 ENDIF
                 ICOL=ICOLD3
            ELSE
                 ICOL=0
            ENDIF
*   cylinders ...
            IF(ISOLTP(IVOL).EQ.1)THEN
                 CALL PLACYP(IVOL,ICOL)
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
*   Extrusion ...
            ELSEIF(ISOLTP(IVOL).EQ.6)THEN
                 CALL PLAEXP(IVOL,ICOL)
*   other things not known.
            ELSE
                 PRINT *,' !!!!!! CELVIE WARNING : Asked to plot a'//
     -                ' solid of unknown type ',ISOLTP(IVOL),
     -                '; not plotted.'
            ENDIF
1010        CONTINUE
*   Apply cuts.
            CALL CELSCT('APPLY')
*   And sort them for plotting.
            CALL PLASRP
**  Using the neBEM panels.
       ELSEIF(PRVIEW.EQ.'NEBEM')THEN
            CALL BEMVIE(IFAIL)
**  Same thing for cut views.
       ELSEIF(PRVIEW.EQ.'CUT')THEN
*   Create the colour entries.
            CALL PROFLD(1,'Making colour table',-1.0)
            CALL PROSTA(1,0.0)
            CALL GRATTS('CONDUCTORS-1','AREA')
            CALL GQFACI(IERR,ICOLW1)
            IF(IERR.NE.0)ICOLW1=1
            CALL GRATTS('CONDUCTORS-2','AREA')
            CALL GQFACI(IERR,ICOLW2)
            IF(IERR.NE.0)ICOLW2=1
            CALL GRATTS('CONDUCTORS-3','AREA')
            CALL GQFACI(IERR,ICOLW3)
            IF(IERR.NE.0)ICOLW3=1
            CALL GRATTS('DIELECTRIC-1','AREA')
            CALL GQFACI(IERR,ICOLD1)
            IF(IERR.NE.0)ICOLD1=1
            CALL GRATTS('DIELECTRIC-2','AREA')
            CALL GQFACI(IERR,ICOLD2)
            IF(IERR.NE.0)ICOLD2=1
            CALL GRATTS('DIELECTRIC-3','AREA')
            CALL GQFACI(IERR,ICOLD3)
            IF(IERR.NE.0)ICOLD3=1
*   Loop over the volumes.
            CALL PROFLD(1,'Generating volumes',REAL(NSOLID))
            DO 1020 IVOL=1,NSOLID
            CALL PROSTA(1,REAL(IVOL))
*   Assign the colour index.
            IF(ISOLMT(IVOL).EQ.1)THEN
                 ICOL=ICOLW1
            ELSEIF(ISOLMT(IVOL).EQ.2)THEN
                 ICOL=ICOLW2
            ELSEIF(ISOLMT(IVOL).EQ.3)THEN
                 ICOL=ICOLW3
            ELSEIF(ISOLMT(IVOL).EQ.11)THEN
                 ICOL=ICOLD1
            ELSEIF(ISOLMT(IVOL).EQ.12)THEN
                 ICOL=ICOLD2
            ELSEIF(ISOLMT(IVOL).EQ.13)THEN
                 ICOL=ICOLD3
            ELSE
                 ICOL=0
            ENDIF
*   cylinders ...
            IF(ISOLTP(IVOL).EQ.1)THEN
                 CALL PLACYC(IVOL,FPROJ(3,1),FPROJ(3,2),FPROJ(3,3),
     -                FPROJA,FPROJB,FPROJC,ICOL)
*   cylindrical holes.
            ELSEIF(ISOLTP(IVOL).EQ.2)THEN
                 CALL PLACHC(IVOL,FPROJ(3,1),FPROJ(3,2),FPROJ(3,3),
     -                FPROJA,FPROJB,FPROJC,ICOL)
*   boxes ...
            ELSEIF(ISOLTP(IVOL).EQ.3)THEN
                 CALL PLABXC(IVOL,FPROJ(3,1),FPROJ(3,2),FPROJ(3,3),
     -                FPROJA,FPROJB,FPROJC,ICOL)
*   spheres ...
            ELSEIF(ISOLTP(IVOL).EQ.4)THEN
                 CALL PLASPC(IVOL,FPROJ(3,1),FPROJ(3,2),FPROJ(3,3),
     -                FPROJA,FPROJB,FPROJC,ICOL)
*   Toblerone ...
            ELSEIF(ISOLTP(IVOL).EQ.5)THEN
                 CALL PLATBC(IVOL,FPROJ(3,1),FPROJ(3,2),FPROJ(3,3),
     -                FPROJA,FPROJB,FPROJC,ICOL)
*   Extrusion ...
            ELSEIF(ISOLTP(IVOL).EQ.6)THEN
                 CALL PLAEXC(IVOL,FPROJ(3,1),FPROJ(3,2),FPROJ(3,3),
     -                FPROJA,FPROJB,FPROJC,ICOL)
*   other things not known.
            ELSE
                 PRINT *,' !!!!!! CELVIE WARNING : Asked to plot a'//
     -                ' solid of unknown type ',ISOLTP(IVOL),
     -                '; not plotted.'
            ENDIF
1020        CONTINUE
*   And sort them for plotting.
            CALL PLASRC
       ENDIF
*** End of progress printing.
       IF(PRVIEW.EQ.'3D'.OR.PRVIEW.EQ.'CUT'.OR.
     -      PRVIEW.EQ.'NEBEM')CALL PROEND
*** Reset tolerances.
       CALL EPSSET('RESET',0.0D0,0.0D0,0.0D0)
       END
