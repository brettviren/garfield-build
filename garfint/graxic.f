CDECK  ID>, GRAXIC.
       SUBROUTINE GRAXIC(VXMIN,VYMIN,VXMAX,VYMAX,TITLE,OPTION)
*-----------------------------------------------------------------------
*   GRAXIC - Draws axis for the cell, using any kind of axis,
*            respecting the viewing plane labels.
*   Variables : VXMIN etc   : Viewing area limits.
*               TITLE       : Global title.
*               OPTION      : VIEW (compute view) or PLOT (plot frame)
*   (Last changed on 26/10/07.)
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
       PARAMETER (MXWIRE=   300,MXSW  =   50)
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
       PARAMETER (MXMAP =  5000,MXEPS =   10)
       PARAMETER (MXWMAP=     5)
       PARAMETER (MXSOLI=  1000)
       PARAMETER (MXPLAN= 50000, MXPOIN=100000,MXEDGE=100)
       PARAMETER (MXSBUF= 20000)
       PARAMETER (MXMCA = 50000)
*   The parameter MXNBMC must equal MXGNAM (sequence MAGBPARM) !
       INTEGER MXNBMC
       PARAMETER(MXNBMC=60)
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
       CHARACTER*(*) TITLE,OPTION
       DOUBLE PRECISION XX(8),YY(8),ZZ(8),
     -      XPL(40),YPL(40),ZPL(40),XCUT,YCUT,VXMIN,VXMAX,VYMIN,VYMAX,
     -      PHIARR,U4,U1,U2,U3,V4,V1,V2,V3,UUMIN,UUMAX,VVMIN,VVMAX
       INTEGER I,J,NPL,II,JJ,IADJAC(8,3),IMARK,ITYPE
       LOGICAL IN(8),CUT,PLOTX,PLOTY
       SAVE IADJAC
*** Adjacency tables.
       DATA (IADJAC(I,1),I=1,8) /2, 1, 1, 2, 1, 2, 3, 4/
       DATA (IADJAC(I,2),I=1,8) /3, 4, 4, 3, 6, 5, 5, 6/
       DATA (IADJAC(I,3),I=1,8) /5, 6, 7, 8, 7, 8, 8, 7/
*** Initialise the list of corners.
       DO 10 I=1,8
       IN(I)=.FALSE.
       IF(2*(I/2).EQ.I)THEN
            XX(I)=GXMAX
       ELSE
            XX(I)=GXMIN
       ENDIF
       II=(I+1)/2
       IF(2*(II/2).EQ.II)THEN
            YY(I)=GYMAX
       ELSE
            YY(I)=GYMIN
       ENDIF
       II=(II+1)/2
       IF(2*(II/2).EQ.II)THEN
            ZZ(I)=GZMAX
       ELSE
            ZZ(I)=GZMIN
       ENDIF
10     CONTINUE
*** Add the corners of the box that are in the viewing plane.
       NPL=0
       DO 20 I=1,8
       IF(ABS(FPROJA*XX(I)+FPROJB*YY(I)+FPROJC*ZZ(I)-FPROJD).LT.
     -      1.0D-6*MAX(ABS(XX(I)),ABS(YY(I)),ABS(ZZ(I)),
     -      ABS(FPROJA),ABS(FPROJB),ABS(FPROJC),ABS(FPROJD)))THEN
            IN(I)=.TRUE.
            CALL PLACOO(XX(I),YY(I),ZZ(I),XCUT,YCUT)
            NPL=NPL+1
            XPL(NPL)=XCUT
            YPL(NPL)=YCUT
       ENDIF
20     CONTINUE
*** Cut the 12 edges with the viewing plane.
       DO 30 I=1,8
       DO 40 JJ=1,3
       J=IADJAC(I,JJ)
       IF(J.LT.I)GOTO 40
       IF(.NOT.(IN(I).OR.IN(J)))THEN
            CALL PLACUT(XX(I),YY(I),ZZ(I),XX(J),YY(J),ZZ(J),
     -           XCUT,YCUT,CUT)
            IF(CUT)THEN
                 NPL=NPL+1
                 XPL(NPL)=XCUT
                 YPL(NPL)=YCUT
            ENDIF
       ENDIF
40     CONTINUE
30     CONTINUE
*** Ensure there is no butterfly.
       DO 70 I=1,NPL
       ZPL(I)=0
70     CONTINUE
       CALL BUTFLD(NPL,XPL,YPL,ZPL)
*** Determine the minimum Cartesian frame that fits around this.
       IF(NPL.EQ.0)THEN
            PRINT *,' !!!!!! GRAXIC WARNING : AREA has no point in'//
     -           ' common with the viewing plane; unit frame.'
            VXMIN=-1
            VXMAX=+1
            VYMIN=-1
            VYMAX=+1
            IMARK=0
            ITYPE=0
       ELSEIF(NPL.EQ.1)THEN
            PRINT *,' !!!!!! GRAXIC WARNING : AREA has only a point'//
     -           ' in the viewing plane; unit sized frame.'
            VXMIN=XPL(1)-1
            VXMAX=XPL(1)+1
            VYMIN=YPL(1)-1
            VYMAX=YPL(1)+1
            IMARK=0
            ITYPE=0
       ELSEIF(NPL.EQ.2)THEN
            PRINT *,' !!!!!! GRAXIC WARNING : AREA has only a line'//
     -           ' in the viewing plane; frame enlarged.'
            VXMIN=MIN(XPL(1),XPL(2))-1
            VXMAX=MAX(XPL(1),XPL(2))+1
            VYMIN=MIN(YPL(1),YPL(2))-1
            VYMAX=MAX(YPL(1),YPL(2))+1
            IMARK=0
            ITYPE=0
       ELSE
            IMARK=0
            ITYPE=0
            VXMIN=XPL(1)+ABS(XPL(1))+1
            VXMAX=XPL(1)-ABS(XPL(1))-1
            VYMIN=YPL(1)+ABS(YPL(1))+1
            VYMAX=YPL(1)-ABS(YPL(1))-1
            DO 50 I=1,NPL
            IF(VXMIN.GT.XPL(I))THEN
                 VXMIN=XPL(I)
                 IMARK=I
                 ITYPE=2
            ENDIF
            IF(VXMAX.LT.XPL(I))THEN
                 VXMAX=XPL(I)
                 IMARK=I
                 ITYPE=4
            ENDIF
            IF(VYMIN.GT.YPL(I))THEN
                 VYMIN=YPL(I)
                 IMARK=I
                 ITYPE=1
            ENDIF
            IF(VYMAX.LT.YPL(I))THEN
                 VYMAX=YPL(I)
                 IMARK=I
                 ITYPE=3
            ENDIF
50          CONTINUE
       ENDIF
*** Return here unless OPTION has been set to PLOT.
       IF(OPTION.NE.'PLOT')RETURN
*** Plot a coordinate frame.
       CALL GRCART(REAL(VXMIN),REAL(VYMIN),REAL(VXMAX),REAL(VYMAX),
     -      PXLAB(1:NCXLAB),PYLAB(1:NCYLAB),TITLE)
       IF(PROLAB(1:NCFPRO).NE.'z=0')CALL GRCOMM(5,'Viewing plane: '//
     -      PROLAB(1:NCFPRO))
*** Plot the outline that corresponds to the AREA.
       IF(NPL.GT.2.AND.NPL+IMARK+5.LT.40.AND.ITYPE.NE.0.AND.
     -      IMARK.NE.0)THEN
*   Mark the area outsize the AREA.
            DO 60 I=1,NPL
            IF(I.GT.NPL-IMARK+1)THEN
                 XPL(I+IMARK-1)=XPL(I+IMARK-1-NPL)
                 YPL(I+IMARK-1)=YPL(I+IMARK-1-NPL)
            ENDIF
60          CONTINUE
            XPL(NPL+IMARK)=XPL(IMARK)
            YPL(NPL+IMARK)=YPL(IMARK)
            IF(ITYPE.EQ.1)THEN
                 XPL(NPL+IMARK+1)=VXMIN
                 YPL(NPL+IMARK+1)=VYMIN
                 XPL(NPL+IMARK+2)=VXMIN
                 YPL(NPL+IMARK+2)=VYMAX
                 XPL(NPL+IMARK+3)=VXMAX
                 YPL(NPL+IMARK+3)=VYMAX
                 XPL(NPL+IMARK+4)=VXMAX
                 YPL(NPL+IMARK+4)=VYMIN
                 XPL(NPL+IMARK+5)=VXMIN
                 YPL(NPL+IMARK+5)=VYMIN
            ELSEIF(ITYPE.EQ.2)THEN
                 XPL(NPL+IMARK+1)=VXMIN
                 YPL(NPL+IMARK+1)=VYMAX
                 XPL(NPL+IMARK+2)=VXMAX
                 YPL(NPL+IMARK+2)=VYMAX
                 XPL(NPL+IMARK+3)=VXMAX
                 YPL(NPL+IMARK+3)=VYMIN
                 XPL(NPL+IMARK+4)=VXMIN
                 YPL(NPL+IMARK+4)=VYMIN
                 XPL(NPL+IMARK+5)=VXMIN
                 YPL(NPL+IMARK+5)=VYMAX
            ELSEIF(ITYPE.EQ.3)THEN
                 XPL(NPL+IMARK+1)=VXMAX
                 YPL(NPL+IMARK+1)=VYMAX
                 XPL(NPL+IMARK+2)=VXMAX
                 YPL(NPL+IMARK+2)=VYMIN
                 XPL(NPL+IMARK+3)=VXMIN
                 YPL(NPL+IMARK+3)=VYMIN
                 XPL(NPL+IMARK+4)=VXMIN
                 YPL(NPL+IMARK+4)=VYMAX
                 XPL(NPL+IMARK+5)=VXMAX
                 YPL(NPL+IMARK+5)=VYMAX
            ELSEIF(ITYPE.EQ.4)THEN
                 XPL(NPL+IMARK+1)=VXMAX
                 YPL(NPL+IMARK+1)=VYMIN
                 XPL(NPL+IMARK+2)=VXMIN
                 YPL(NPL+IMARK+2)=VYMIN
                 XPL(NPL+IMARK+3)=VXMIN
                 YPL(NPL+IMARK+3)=VYMAX
                 XPL(NPL+IMARK+4)=VXMAX
                 YPL(NPL+IMARK+4)=VYMAX
                 XPL(NPL+IMARK+5)=VXMAX
                 YPL(NPL+IMARK+5)=VYMIN
            ENDIF
            XPL(NPL+IMARK+6)=XPL(IMARK)
            YPL(NPL+IMARK+6)=YPL(IMARK)
*   Fill the excluded area.
            CALL GRATTS('OUTSIDE-AREA','AREA')
            CALL GRARE2(NPL+7,XPL(IMARK),YPL(IMARK))
*   Outline.
            CALL GRATTS('BOX-TICKMARKS','POLYLINE')
            CALL GRLIN2(NPL+6,XPL(IMARK),YPL(IMARK))
       ENDIF
*** Display the coordinate axes, first compute locations.
       IF(PROLAB(1:NCFPRO).NE.'z=0'.OR.PROROT.NE.0)THEN
            CALL PLACOO(0.0D0,0.0D0,0.0D0,U4,V4)
            CALL PLACOO(1.0D0,0.0D0,0.0D0,U1,V1)
            CALL PLACOO(0.0D0,1.0D0,0.0D0,U2,V2)
            CALL PLACOO(0.0D0,0.0D0,1.0D0,U3,V3)
            UUMIN=MIN(U4,U1,U2,U3)
            UUMAX=MAX(U4,U1,U2,U3)
            VVMIN=MIN(V4,V1,V2,V3)
            VVMAX=MAX(V4,V1,V2,V3)
       ENDIF
*   Proceed only if this worked and if the frame is not degenerate.
       IF(MAX(UUMAX-UUMIN,VVMAX-VVMIN).GT.0.AND.
     -      (PROLAB(1:NCFPRO).NE.'z=0'.OR.PROROT.NE.0))THEN
            U4=0.02+0.06*(U4-UUMIN)/MAX(UUMAX-UUMIN,VVMAX-VVMIN)
            U1=0.02+0.06*(U1-UUMIN)/MAX(UUMAX-UUMIN,VVMAX-VVMIN)
            U2=0.02+0.06*(U2-UUMIN)/MAX(UUMAX-UUMIN,VVMAX-VVMIN)
            U3=0.02+0.06*(U3-UUMIN)/MAX(UUMAX-UUMIN,VVMAX-VVMIN)
            V4=0.02+0.06*(V4-VVMIN)/MAX(UUMAX-UUMIN,VVMAX-VVMIN)
            V1=0.02+0.06*(V1-VVMIN)/MAX(UUMAX-UUMIN,VVMAX-VVMIN)
            V2=0.02+0.06*(V2-VVMIN)/MAX(UUMAX-UUMIN,VVMAX-VVMIN)
            V3=0.02+0.06*(V3-VVMIN)/MAX(UUMAX-UUMIN,VVMAX-VVMIN)
*   Set representations.
            CALL GRATTS('BOX-TICKMARKS','POLYLINE')
            CALL GRATTS('NUMBERS','TEXT')
            CALL GSTXAL(2,3)
            CALL GSCHUP(0.0,1.0)
*   Switch to normalisation transformation 0.
            CALL GSELNT(0)
*   Plot the x-axis.
            IF(ABS(U1-U4).GT.0.001.OR.ABS(V1-V4).GT.0.001)THEN
                 XPL(1)=DISPX0+U4
                 XPL(2)=DISPX0+U1
                 YPL(1)=DISPY0+V4
                 YPL(2)=DISPY0+V1
                 CALL GPL2(2,XPL,YPL)
                 PHIARR=ATAN2(YPL(2)-YPL(1),XPL(2)-XPL(1))
                 XPL(1)=DISPX0+U1-SQRT((U1-U4)**2+(V1-V4)**2)*
     -                0.2*COS(PHIARR+ARRANG)
                 YPL(1)=DISPY0+V1-SQRT((U1-U4)**2+(V1-V4)**2)*
     -                0.2*SIN(PHIARR+ARRANG)
                 XPL(2)=DISPX0+U1
                 YPL(2)=DISPY0+V1
                 XPL(3)=DISPX0+U1-SQRT((U1-U4)**2+(V1-V4)**2)*
     -                0.2*COS(PHIARR-ARRANG)
                 YPL(3)=DISPY0+V1-SQRT((U1-U4)**2+(V1-V4)**2)*
     -                0.2*SIN(PHIARR-ARRANG)
                 CALL GPL2(3,XPL,YPL)
                 CALL GTX(DISPX0+REAL(U4+1.2*(U1-U4)),
     -                DISPY0+REAL(V4+1.2*(V1-V4)),'x')
                 PLOTX=.TRUE.
            ELSE
                 PLOTX=.FALSE.
            ENDIF
*   Plot the y-axis, if different from the x-axis.
            IF((ABS(U2-U4).GT.0.001.OR.ABS(V2-V4).GT.0.001).AND.
     -           (ABS(U2-U1).GT.0.001.OR.ABS(V2-V1).GT.0.001.OR.
     -           .NOT.PLOTX))THEN
                 XPL(1)=DISPX0+U4
                 XPL(2)=DISPX0+U2
                 YPL(1)=DISPY0+V4
                 YPL(2)=DISPY0+V2
                 CALL GPL2(2,XPL,YPL)
                 PHIARR=ATAN2(YPL(2)-YPL(1),XPL(2)-XPL(1))
                 XPL(1)=DISPX0+U2-SQRT((U2-U4)**2+(V2-V4)**2)*
     -                0.2*COS(PHIARR+ARRANG)
                 YPL(1)=DISPY0+V2-SQRT((U2-U4)**2+(V2-V4)**2)*
     -                0.2*SIN(PHIARR+ARRANG)
                 XPL(2)=DISPX0+U2
                 YPL(2)=DISPY0+V2
                 XPL(3)=DISPX0+U2-SQRT((U2-U4)**2+(V2-V4)**2)*
     -                0.2*COS(PHIARR-ARRANG)
                 YPL(3)=DISPY0+V2-SQRT((U2-U4)**2+(V2-V4)**2)*
     -                0.2*SIN(PHIARR-ARRANG)
                 CALL GPL2(3,XPL,YPL)
                 CALL GTX(DISPX0+REAL(U4+1.2*(U2-U4)),
     -                DISPY0+REAL(V4+1.2*(V2-V4)),'y')
                 PLOTY=.TRUE.
            ELSE
                 PLOTY=.FALSE.
            ENDIF
*   Plot the z-axis, if different from the x- and y-axes.
            IF((ABS(U3-U4).GT.0.001.OR.ABS(V3-V4).GT.0.001).AND.
     -           (ABS(U3-U1).GT.0.001.OR.ABS(V3-V1).GT.0.001.OR.
     -           .NOT.PLOTX).AND.
     -           (ABS(U3-U2).GT.0.001.OR.ABS(V3-V2).GT.0.001.OR.
     -           .NOT.PLOTY))THEN
                 XPL(1)=DISPX0+U4
                 XPL(2)=DISPX0+U3
                 YPL(1)=DISPY0+V4
                 YPL(2)=DISPY0+V3
                 CALL GPL2(2,XPL,YPL)
                 PHIARR=ATAN2(YPL(2)-YPL(1),XPL(2)-XPL(1))
                 XPL(1)=DISPX0+U3-SQRT((U3-U4)**2+(V3-V4)**2)*
     -                0.2*COS(PHIARR+ARRANG)
                 YPL(1)=DISPY0+V3-SQRT((U3-U4)**2+(V3-V4)**2)*
     -                0.2*SIN(PHIARR+ARRANG)
                 XPL(2)=DISPX0+U3
                 YPL(2)=DISPY0+V3
                 XPL(3)=DISPX0+U3-SQRT((U3-U4)**2+(V3-V4)**2)*
     -                0.2*COS(PHIARR-ARRANG)
                 YPL(3)=DISPY0+V3-SQRT((U3-U4)**2+(V3-V4)**2)*
     -                0.2*SIN(PHIARR-ARRANG)
                 CALL GPL2(3,XPL,YPL)
                 CALL GTX(DISPX0+REAL(U4+1.2*(U3-U4)),
     -                DISPY0+REAL(V4+1.2*(V3-V4)),'z')
            ENDIF
*   Switch back to normalisation transformation 1.
            CALL GSELNT(1)
       ENDIF
*** Get the viewport input priorities right.
       CALL GSVPIP(1,0,0)
*** Reset the bar chart and histogram counters.
       IGBAR=0
       IGHIST=0
       END
