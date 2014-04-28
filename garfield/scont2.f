CDECK  ID>, SCONT2.
       SUBROUTINE SCONT2(X0,Y0,Z0,RES,MODRES,NRES,ILOC)
*-----------------------------------------------------------------------
*   SCONT2 - Performs formula evaluations for the signal field plots.
*   (Last changed on 26/ 2/09.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER NRES,MODRES(NRES),MODVAR(MXVAR),ILOC,ILOC1,
     -      IENTRY,ITYPE,I,IFAIL,JSW
       REAL RES(NRES),VAR(MXVAR),QPLT,X0,Y0,Z0,VOLT,EX,EY,EZ
       DOUBLE PRECISION F0(3),QTMIN,QTMAX,QQTMIN,QQTMAX
       LOGICAL EVALW,EVALB,EVALV,EVALE,EVALI,EVALQ,EVALP,LMCDR
       COMMON /CN3DAT/ QTMIN,QTMAX,IENTRY,EVALW,EVALB,EVALV,EVALE,EVALI,
     -      EVALQ,EVALP,QPLT,ITYPE,JSW,LMCDR
*** Verify that we are in the drift area.
       IF(X0.LT.DXMIN.OR.X0.GT.DXMAX.OR.
     -      Y0.LT.DYMIN.OR.Y0.GT.DYMAX.OR.
     -      Z0.LT.DZMIN.OR.Z0.GT.DZMAX)THEN
            ILOC=-1
            RETURN
       ENDIF
*** Store the coordinates.
       VAR(1)= X0
       VAR(2)= Y0
       VAR(23)=Z0
*** Always calculate the E field for verification purposes.
       CALL EFIELD(VAR(1),VAR(2),VAR(23),VAR(3),VAR(4),VAR(5),VAR(6),
     -      VOLT,0,ILOC)
*   Location code -5 (in a material) can be acceptable.
       IF(ILOC.EQ.-5.AND.LCNTAM)ILOC=0
*   Also inside a solid is acceptable for contours.
       IF(ILOC.GT.2*MXWIRE.AND.ILOC.LE.2*MXWIRE+MXSOLI.AND.
     -      LCNTAM)ILOC=0
*   For other non-zero locations return.
       IF(ILOC.NE.0)THEN
            DO 10 I=1,NRES
            RES(I)=0
            MODRES(I)=0
10          CONTINUE
            RETURN
       ENDIF
*** Calculate the B field.
       IF(EVALB)CALL BFIELD(VAR(1),VAR(2),VAR(23),
     -      VAR(7),VAR(8),VAR(9),VAR(10))
*** Compute the local drift velocity.
       IF(EVALV)THEN
            CALL DLCVEL(DBLE(VAR(1)),DBLE(VAR(2)),DBLE(VAR(23)),
     -           F0,QPLT,ITYPE,ILOC1)
            VAR(11)=REAL(F0(1))
            VAR(12)=REAL(F0(2))
            VAR(13)=REAL(F0(3))
            VAR(14)=REAL(SQRT(F0(1)**2+F0(2)**2+F0(3)**2))
       ENDIF
*** Calculate the Ew field.
       IF(EVALW)THEN
            CALL SIGFLS(VAR(1),VAR(2),VAR(23),EX,EY,EZ,JSW)
*   Assign the results.
            VAR(17)=EX
            VAR(18)=EY
            VAR(19)=EZ
            VAR(20)=SQRT(EX**2+EY**2+EZ**2)
       ENDIF
*** Electron drift line related quantities.
       IF(EVALE)THEN
*   Set electron parameters.
            QPLT=-1.0
            ITYPE=1
*   Compute the drift line.
            IF(LMCDR)THEN
                 CALL DLCMC(VAR(1),VAR(2),VAR(23),QPLT,ITYPE)
            ELSE
                 CALL DLCALC(VAR(1),VAR(2),VAR(23),QPLT,ITYPE)
            ENDIF
*   Time and status.
            VAR(15)=TU(NU)
            VAR(24)=ISTAT
*   Induced charge.
            IF(EVALQ)THEN
                 IF(QTMIN.LT.0)THEN
                      QQTMIN=TU(1)
                 ELSE
                      QQTMIN=QTMIN
                 ENDIF
                 IF(QTMAX.LT.0)THEN
                      QQTMAX=TU(NU)
                 ELSE
                      QQTMAX=QTMAX
                 ENDIF
                 CALL SIGQIN(VAR(21),JSW,QQTMIN,QQTMAX)
            ENDIF
       ENDIF
*** Ion drift line related quantities.
       IF(EVALI)THEN
*   Set ion parameters.
            QPLT=+1.0
            ITYPE=2
*   Compute the drift line.
            CALL DLCALC(VAR(1),VAR(2),VAR(23),QPLT,ITYPE)
*   Time and status.
            VAR(16)=TU(NU)
            VAR(25)=ISTAT
*   Induced charge.
            IF(EVALP)CALL SIGQIN(VAR(22),JSW,TU(1),TU(NU))
       ENDIF
*** Transform vectors and covectors to polar coordinates if needed.
       IF(POLAR)THEN
            CALL CFMRTP(VAR(1),VAR(2),VAR(1),VAR(2),1)
            VAR(3)=VAR(3)/VAR(1)
            VAR(4)=VAR(4)/VAR(1)
            VAR(6)=SQRT(VAR(3)**2+VAR(4)**2+VAR(5)**2)
            VAR(17)=VAR(17)/VAR(1)
            VAR(18)=VAR(18)/VAR(1)
            VAR(20)=SQRT(VAR(17)**2+VAR(18)**2+VAR(19)**2)
            VAR(11)=VAR(11)*VAR(1)
            VAR(12)=VAR(12)*VAR(1)
            VAR(14)=SQRT(VAR(11)**2+VAR(12)**2+VAR(13)**2)
       ENDIF
*** Assign modes.
       DO 100 I=1,25
       MODVAR(I)=2
100    CONTINUE
*** Evaluate the function
       CALL ALGEXE(IENTRY,VAR,MODVAR,25,RES,MODRES,NRES,IFAIL)
       END
