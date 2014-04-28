CDECK  ID>, EFIELD.
       SUBROUTINE EFIELD(XIN,YIN,ZIN,EX,EY,EZ,ETOT,VOLT,IOPT,ILOC)
*-----------------------------------------------------------------------
*   EFIELD - Subroutine calculating the electric field and the potential
*            at a given place. It makes use of the routines POT...,
*            depending on the type of the cell.
*   VARIABLES : XPOS       : x-coordinate of the place where the field
*                            is to be calculated.
*               YPOS, ZPOS : y- and z-coordinates
*               EX, EY, EZ : x-, y-, z-component of the electric field.
*               VOLT       : potential at (XPOS,YPOS).
*               IOPT       : 1 if both E and V are required, 0 if only E
*                            is to be computed.
*               ILOC       : Tells where the point is located (0: normal
*                            I > 0: in wire I, -1: outside a plane,
*                            -5: in a material, -6: outside the mesh,
*                            -10: unknown potential).
*   (Last changed on 19/ 3/09.)
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
       REAL XIN,YIN,ZIN,EX,EY,EZ,ETOT,VOLT,XPOS,YPOS,ZPOS,DXWIR,DYWIR,
     -      AROT,EX3D,EY3D,EZ3D,V3D,EXBGF,EYBGF,EZBGF,VBGF,XAUX,YAUX,
     -      EXD,EYD,VOLTD
       INTEGER IOUT,ILOC,IOPT,I
*** Initialise the field for returns without actual calculations.
       EX=0.0
       EY=0.0
       EZ=0.0
       ETOT=0.0
       VOLT=0.0
       ILOC=0
*** For boundary element methods, just transfer.
       IF(ICTYPE.EQ.-1)THEN
            XPOS=XIN
            YPOS=YIN
            ZPOS=ZIN
*** For finite element programs, just transfer.
       ELSEIF(ICTYPE.EQ.0)THEN
            XPOS=XIN
            YPOS=YIN
            ZPOS=ZIN
*** In case of periodicity, move the point into the basic cell.
       ELSE
            IF(PERX)THEN
                 XPOS=XIN-SX*ANINT(XIN/SX)
            ELSE
                 XPOS=XIN
            ENDIF
            IF(PERY.AND.TUBE)THEN
                 CALL CFMCTP(XIN,YIN,XPOS,YPOS,1)
                 AROT=180*SY*ANINT((PI*YPOS)/(SY*180.0))/PI
                 YPOS=YPOS-AROT
                 CALL CFMPTC(XPOS,YPOS,XPOS,YPOS,1)
            ELSEIF(PERY)THEN
                 YPOS=YIN-SY*ANINT(YIN/SY)
            ELSE
                 YPOS=YIN
            ENDIF
*   Move the point to the correct side of the plane.
            IF(PERX.AND.YNPLAN(1).AND.XPOS.LE.COPLAN(1))XPOS=XPOS+SX
            IF(PERX.AND.YNPLAN(2).AND.XPOS.GE.COPLAN(2))XPOS=XPOS-SX
            IF(PERY.AND.YNPLAN(3).AND.YPOS.LE.COPLAN(3))YPOS=YPOS+SY
            IF(PERY.AND.YNPLAN(4).AND.YPOS.GE.COPLAN(4))YPOS=YPOS-SY
*   In case (XPOS,YPOS) is located behind a plane there is no field.
            IOUT=0
            IF(TUBE)THEN
                 CALL INTUBE(XPOS,YPOS,COTUBE,NTUBE,IOUT)
                 IF(IOUT.NE.0)VOLT=VTTUBE
            ELSE
                 IF(YNPLAN(1).AND.XPOS.LT.COPLAN(1))IOUT=1
                 IF(YNPLAN(2).AND.XPOS.GT.COPLAN(2))IOUT=2
                 IF(YNPLAN(3).AND.YPOS.LT.COPLAN(3))IOUT=3
                 IF(YNPLAN(4).AND.YPOS.GT.COPLAN(4))IOUT=4
                 IF(IOUT.EQ.1)VOLT=VTPLAN(1)
                 IF(IOUT.EQ.2)VOLT=VTPLAN(2)
                 IF(IOUT.EQ.3)VOLT=VTPLAN(3)
                 IF(IOUT.EQ.4)VOLT=VTPLAN(4)
            ENDIF
            IF(IOUT.NE.0)THEN
                 ILOC=-4
                 RETURN
            ENDIF
**  If (XPOS,YPOS) is within a wire, there is no field either.
            DO 10 I=1,NWIRE
*   Correct for x-periodicity.
            IF(PERX)THEN
                 DXWIR=(XPOS-X(I))-SX*ANINT((XPOS-X(I))/SX)
            ELSE
                 DXWIR=XPOS-X(I)
            ENDIF
*   Correct for y-periodicity.
            IF(PERY.AND..NOT.TUBE)THEN
                 DYWIR=(YPOS-Y(I))-SY*ANINT((YPOS-Y(I))/SY)
            ELSE
                 DYWIR=YPOS-Y(I)
            ENDIF
*   Check the actual position.
            IF(DXWIR**2+DYWIR**2.LT.0.25*D(I)**2)THEN
                 VOLT=V(I)
                 ILOC=I
                 RETURN
            ENDIF
*   Next wire.
10          CONTINUE
       ENDIF
*** Call the appropriate potential calculation function.
       IF(ICTYPE.EQ.-1)THEN
            CALL EFCBEM(XPOS,YPOS,ZPOS,EX,EY,EZ,VOLT,IOPT,ILOC)
       ELSEIF(ICTYPE.EQ.0)THEN
            CALL EFCFMP(XPOS,YPOS,ZPOS,EX,EY,EZ,VOLT,IOPT,ILOC)
            IF(ILOC.NE.0.AND.ILOC.NE.-5)RETURN
       ELSEIF(ICTYPE.EQ.1.AND.NXMATT.EQ.0.AND.NYMATT.EQ.0)THEN
            CALL EFCA00(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.1)THEN
            CALL EFDA00(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.2)THEN
            CALL EFCB1X(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.3)THEN
            CALL EFCB1Y(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.4)THEN
            CALL EFCB2X(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.5)THEN
            CALL EFCB2Y(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.6)THEN
            CALL EFCC10(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.7)THEN
            CALL EFCC2X(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.8)THEN
            CALL EFCC2Y(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.9)THEN
            CALL EFCC30(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.10)THEN
            CALL EFCD10(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.11)THEN
            CALL EFCD20(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSEIF(ICTYPE.EQ.12)THEN
            CALL EFCD30(XPOS,YPOS,EX,EY,VOLT,IOPT)
C       ELSEIF(ICTYPE.EQ.13)THEN
C           CALL EFCD40(XPOS,YPOS,EX,EY,VOLT,IOPT)
       ELSE
            ILOC=-10
            RETURN
       ENDIF
*** Add dipole terms if requested
       IF(LDIPOL)THEN
            IF(ICTYPE.EQ.1)THEN
                 CALL EMCA00(XPOS,YPOS,EXD,EYD,VOLTD,IOPT)
            ELSEIF(ICTYPE.EQ.2)THEN
                 CALL EMCB1X(XPOS,YPOS,EXD,EYD,VOLTD,IOPT)
            ELSEIF(ICTYPE.EQ.3)THEN
                 CALL EMCB1Y(XPOS,YPOS,EXD,EYD,VOLTD,IOPT)
            ELSEIF(ICTYPE.EQ.4)THEN
                 CALL EMCB2X(XPOS,YPOS,EXD,EYD,VOLTD,IOPT)
            ELSEIF(ICTYPE.EQ.5)THEN
                 CALL EMCB2Y(XPOS,YPOS,EXD,EYD,VOLTD,IOPT)
            ELSEIF(ICTYPE.EQ.6)THEN
                 CALL EMCC10(XPOS,YPOS,EXD,EYD,VOLTD,IOPT)
            ELSE
                 EXD=0
                 EYD=0
                 VOLTD=0
                 CALL EMCA00(XPOS,YPOS,EXD,EYD,VOLTD,IOPT)
            ENDIF
            EX=EX+EXD
            EY=EY+EYD
            VOLT=VOLT+VOLTD
       ENDIF
*** Rotate the field in some special cases.
       IF(ICTYPE.GT.0)THEN
            IF(PERY.AND.TUBE)THEN
                 CALL CFMCTP(EX,EY,XAUX,YAUX,1)
                 YAUX=YAUX+AROT
                 CALL CFMPTC(XAUX,YAUX,EX,EY,1)
            ENDIF
*** Correct for the equipotential planes.
            EX=EX-CORVTA
            EY=EY-CORVTB
            VOLT=VOLT+CORVTA*XPOS+CORVTB*YPOS+CORVTC
       ENDIF
*** Add three dimensional point charges.
       IF(N3D.GT.0)THEN
            IF(ICTYPE.EQ.1.OR.ICTYPE.EQ.2.OR.ICTYPE.EQ.3)THEN
                 CALL E3DA00(XIN,YIN,ZIN,EX3D,EY3D,EZ3D,V3D)
            ELSEIF(ICTYPE.EQ.4)THEN
                 CALL E3DB2X(XIN,YIN,ZIN,EX3D,EY3D,EZ3D,V3D)
            ELSEIF(ICTYPE.EQ.5)THEN
                 CALL E3DB2Y(XIN,YIN,ZIN,EX3D,EY3D,EZ3D,V3D)
            ELSEIF(ICTYPE.EQ.10)THEN
                 CALL E3DD10(XIN,YIN,ZIN,EX3D,EY3D,EZ3D,V3D)
            ELSE
                 EX3D=0.0
                 EY3D=0.0
                 EZ3D=0.0
                 V3D=0.0
                 CALL E3DA00(XIN,YIN,ZIN,EX3D,EY3D,EZ3D,V3D)
            ENDIF
            EX=EX+EX3D
            EY=EY+EY3D
            EZ=EZ+EZ3D
            VOLT=VOLT+V3D
       ENDIF
*** Add a background field if present.
       IF(IENBGF.GT.0)THEN
            CALL EFCBGF(XIN,YIN,ZIN,EXBGF,EYBGF,EZBGF,VBGF)
            EX=EX+EXBGF
            EY=EY+EYBGF
            EZ=EZ+EZBGF
            VOLT=VOLT+VBGF
       ENDIF
*** Finally calculate the value of ETOT (magnitude of the E-field).
       ETOT=SQRT(EX**2+EY**2+EZ**2)
       END
