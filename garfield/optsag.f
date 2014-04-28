CDECK  ID>, OPTSAG.
       SUBROUTINE OPTSAG(IWIRE,START,CSAG,XSAG,YSAG,NSAG,IFAIL)
*-----------------------------------------------------------------------
*   OPTSAG - Computes the wire sag due to eletrostatic and gravitational
*            forces, using a Runge-Kutta-Nystrom multiple shoot method,
*            where the intermediate conditions are imposed through a
*            Broyden rank-1 zero search.
*   (Last changed on 13/ 4/99.)
*-----------------------------------------------------------------------
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       DOUBLE PRECISION FX(MXGRID,MXGRID),FY(MXGRID,MXGRID),
     -      XSCAN(MXGRID),YSCAN(MXGRID),EPS,EPSX,EPSF,STEP
       REAL XORIG(MXWIRE),YORIG(MXWIRE),XOFF(MXWIRE),YOFF(MXWIRE)
       INTEGER NITMAX,NSHOT,NSTEP,IW,NSCANX,NSCANY,JSORD,NFITER
       LOGICAL LFGRAV,LFELEC,LFEXTR,LFWARN,LZROPR,LFITER
       COMMON /SHPDAT/ FX,FY,XSCAN,YSCAN,EPS,EPSX,EPSF,STEP,
     -      XORIG,YORIG,XOFF,YOFF,
     -      NITMAX,NSHOT,NSTEP,IW,NSCANX,NSCANY,JSORD,NFITER,
     -      LFGRAV,LFELEC,LFEXTR,LFWARN,LZROPR,LFITER
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
       INTEGER IFAIL,IFAIL1,NSAG,I
       CHARACTER*(*) START
       REAL CSAG(0:*),XSAG(0:*),YSAG(0:*),RNDM
       DOUBLE PRECISION COOR,XST(2),DXST(2),WORK(12),XX(4*MXSHOT+2),
     -      FXMEAN,FYMEAN,SAGX0,SAGY0,FORCE(2)
       EXTERNAL OPTSHT,OPTSTP,RNDM
*** Assume the routine will fail.
       IFAIL=1
*** Check the values of the parameters.
       IF(IWIRE.LE.0.OR.IWIRE.GT.NWIRE)THEN
            PRINT *,' !!!!!! OPTSAG WARNING : Wire number out of'//
     -           ' range; sag not computed.'
            RETURN
       ELSEIF(NSAG.LT.NSTEP*(NSHOT+1))THEN
            PRINT *,' !!!!!! OPTSAG WARNING : Output arrays are'//
     -           ' too small; sag not computed.'
            RETURN
       ENDIF
*** Copy the wire number to the common block.
       IW=IWIRE
*** Temporarily set the number of output values to 0.
       NSAG=0
*** Compute the step width based on the number of steps.
       STEP=DBLE(U(IW))/DBLE(NSTEP*(NSHOT+1))
*** Compute expected maximum sag, constant-force approximation.
       XST(1)=0
       XST(2)=0
       DXST(1)=0
       DXST(2)=0
       FXMEAN=0
       FYMEAN=0
*   Check whether there is extrapolation.
       LFWARN=.FALSE.
*   Loop over the whole wire.
       DO 40 I=0,NSTEP*(NSHOT+1)
       COOR=I*STEP
       CALL OPTSTP(COOR,XST,DXST,FORCE)
       FXMEAN=FXMEAN+FORCE(1)
       FYMEAN=FYMEAN+FORCE(2)
40     CONTINUE
*   Check the extrapolation warning flag.
       IF(LFWARN)THEN
            PRINT *,' !!!!!! OPTSAG WARNING : Wire at nominal'//
     -           ' position outside scanning area; no sag calculated.'
            RETURN
       ENDIF
*   Compute expected sag.
       SAGX0=-FXMEAN*DBLE(U(IW))**2/DBLE(8*(1+NSTEP*(NSHOT+1)))
       SAGY0=-FYMEAN*DBLE(U(IW))**2/DBLE(8*(1+NSTEP*(NSHOT+1)))
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ OPTSAG DEBUG   :'',
     -      '' Parabolic sag dx='',E12.5,'', dy='',E12.5,'' [cm]'')')
     -      SAGX0,SAGY0
*** Starting position: parabolic sag.
       IF(START.EQ.'PARABOLIC')THEN
*   Derivative first point.
            XX(1)=4*SAGX0/U(IW)
            XX(2)=4*SAGY0/U(IW)
*   Intermediate points, both position and derivative.
            DO 10 I=1,NSHOT
*   Position.
            COOR=I*NSTEP*STEP-U(IW)/2
*   Deflection.
            XX(4*I-1)=SAGX0*(1-4*COOR**2/U(IW)**2)
            XX(4*I)=SAGY0*(1-4*COOR**2/U(IW)**2)
*   Derivative.
            XX(4*I+1)=-8*SAGX0*COOR/U(IW)**2
            XX(4*I+2)=-8*SAGY0*COOR/U(IW)**2
10          CONTINUE
*** Starting position: random position.
       ELSEIF(START.EQ.'RANDOM')THEN
            DO 15 I=1,4*NSHOT+2
*   Derivatives.
            IF(I-1.EQ.4*((I-1)/4).OR.I-2.EQ.4*((I-2)/4))THEN
                 XX(I)=RNDM(I)-0.5
*   Positions.
            ELSE
                 XX(I)=0.1*(RNDM(I)-0.5)*U(IW)
            ENDIF
15          CONTINUE
*** Unknown starting position.
       ELSE
            PRINT *,' !!!!!! OPTSAG WARNING : Unknown starting'//
     -           ' choice received ; no sag calculated.'
            RETURN
       ENDIF
*** Search for solution.
       CALL OPTZRO(OPTSHT,XX,4*NSHOT+2,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! OPTSAG WARNING : Failed to solve'//
     -           ' the differential equation for the sag; no'//
     -           ' sag returned.'
            RETURN
       ENDIF
*** And return the detailed solution, first the starting point.
       CSAG(0)=-U(IW)/2
       XSAG(0)=0
       YSAG(0)=0
       COOR=-U(IW)/2
       DO 30 I=0,NSHOT
*   Set the starting value and starting derivative.
       IF(I.EQ.0)THEN
            XST(1)=0
            XST(2)=0
            DXST(1)=XX(1)
            DXST(2)=XX(2)
       ELSE
            XST(1)=XX(4*I-1)
            XST(2)=XX(4*I)
            DXST(1)=XX(4*I+1)
            DXST(2)=XX(4*I+2)
       ENDIF
*   Store the intermediate values.
       DO 20 J=1,NSTEP
       CALL DRKNYS(2,STEP,COOR,XST,DXST,OPTSTP,WORK)
       CSAG(I*NSTEP+J)=COOR
       XSAG(I*NSTEP+J)=XST(1)
       YSAG(I*NSTEP+J)=XST(2)
20     CONTINUE
30     CONTINUE
*** Seems to have worked.
       NSAG=NSTEP*(NSHOT+1)
       IFAIL=0
       END
