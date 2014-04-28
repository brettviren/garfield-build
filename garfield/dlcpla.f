CDECK  ID>, DLCPLA.
       SUBROUTINE DLCPLA(IPLANE,Q,ITYPE)
*-----------------------------------------------------------------------
*   DLCPLA - Terminates drift line calculation by making a last linear
*            step to the boundary identified by IPLANE. Version for use
*            with RKF integration.
*   VARIABLES : F3          : Drift-velocity at the one but last point,
*                             assumed to be constant over the step.
*               SPEED       : Magitude of F3.
*   (Last changed on  5/12/01.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER ILOC,IFAIL,ITYPE,IPLANE
       REAL Q
       DOUBLE PRECISION F3(3),SPEED
*** Identify this routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE DLCPLA ///'
*** Calculate the drift velocity at the current last point.
       CALL DLCVEL(XU(NU),YU(NU),ZU(NU),F3,Q,ITYPE,ILOC)
       SPEED=SQRT(F3(1)**2+F3(2)**2)
       IF(SPEED.EQ.0.0D0.OR.ILOC.NE.0)THEN
            PRINT *,' !!!!!! DLCPLA WARNING : Drift line not properly'//
     -           ' terminated because of zero drift field.'
            ISTAT=-3
            IF(ILOC.NE.0)ISTAT=ILOC
            RETURN
       ENDIF
*** Check we may still add points.
       IF(NU.GE.MXLIST)THEN
            ISTAT=-2
            IF(LDEBUG)PRINT *,' ++++++ DLCPLA DEBUG   : Last point'//
     -           ' not added because MXLIST is reached.'
            NU=MXLIST
            RETURN
       ENDIF
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCPLA DEBUG   : Entered'',
     -      '' at NU='',I3,'' for IPLANE='',I2,''.'')') NU,IPLANE
*** Check the components.
       IF((IPLANE.EQ.1.AND.F3(1).GE.0.0D0).OR.
     -      (IPLANE.EQ.2.AND.F3(1).LE.0.0D0).OR.
     -      (IPLANE.EQ.3.AND.F3(2).GE.0.0D0).OR.
     -      (IPLANE.EQ.4.AND.F3(2).LE.0.0D0))THEN
            PRINT *,' !!!!!! DLCPLA WARNING : The particle moves away'//
     -           ' from the boundary it is supposed to hit ; abandoned.'
            ISTAT=-3
            RETURN
       ENDIF
*** Add the last step towards the plane.
       NU=NU+1
       IF(IPLANE.EQ.1)THEN
            XU(NU)=DBLE(DDXMIN)
            YU(NU)=YU(NU-1)+(F3(2)/F3(1))*(XU(NU)-XU(NU-1))
            ZU(NU)=ZU(NU-1)+(F3(3)/F3(1))*(XU(NU)-XU(NU-1))
            ISTAT=ISTAT1
       ELSEIF(IPLANE.EQ.2)THEN
            XU(NU)=DBLE(DDXMAX)
            YU(NU)=YU(NU-1)+(F3(2)/F3(1))*(XU(NU)-XU(NU-1))
            ZU(NU)=ZU(NU-1)+(F3(3)/F3(1))*(XU(NU)-XU(NU-1))
            ISTAT=ISTAT2
       ELSEIF(IPLANE.EQ.3)THEN
            YU(NU)=DBLE(DDYMIN)
            XU(NU)=XU(NU-1)+(F3(1)/F3(2))*(YU(NU)-YU(NU-1))
            ZU(NU)=ZU(NU-1)+(F3(3)/F3(2))*(YU(NU)-YU(NU-1))
            ISTAT=ISTAT3
       ELSEIF(IPLANE.EQ.4)THEN
            YU(NU)=DBLE(DDYMAX)
            XU(NU)=XU(NU-1)+(F3(1)/F3(2))*(YU(NU)-YU(NU-1))
            ZU(NU)=ZU(NU-1)+(F3(3)/F3(2))*(YU(NU)-YU(NU-1))
            ISTAT=ISTAT4
       ELSE
            PRINT *,' ###### DLCPLA ERROR   : Unrecognised IPLANE=',
     -           IPLANE,' received (program bug - please report).'
            ISTAT=-3
       ENDIF
*** Clip the step to the full set of boundaries.
       CALL CLIP2D(XU(NU-1),YU(NU-1),XU(NU),YU(NU),
     -      DBLE(DDXMIN),DBLE(DDYMIN),DBLE(DDXMAX),DBLE(DDYMAX),IFAIL)
*** And fill in the time for the last step.
       TU(NU)=TU(NU-1)+SQRT((XU(NU)-XU(NU-1))**2+
     -      (YU(NU)-YU(NU-1))**2)/SPEED
       END
