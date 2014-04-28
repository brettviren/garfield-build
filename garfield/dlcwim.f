CDECK  ID>, DLCWIM.
       SUBROUTINE DLCWIM
*-----------------------------------------------------------------------
*   DLCWIM - Terminates drift line calculation by truncating the last
*            step towards the surface of the wire inside which it has
*            landed. Version for microscopic tracking (not using the
*            drift velocity vector).
*   (Last changed on 29/ 7/08.)
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
C       logical ldebug,lident
C       integer lunout
C       parameter(ldebug=.true.,lident=.true.,lunout=6)
       REAL EX,EY,EZ,ETOT,VOLT,DIST,STEP,XDIST,YDIST
       INTEGER ILOC
       LOGICAL SHIFT
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE DLCWIM ///'
*** Check NU and print debugging output.
       IF(NU.LT.1)THEN
            PRINT *,' !!!!!! DLCWIM WARNING : Number of steps is not'//
     -           ' > 0; terminating on ISTAT=-3.'
            ISTAT=-3
            RETURN
       ENDIF
*** Ensure the last position is inside a wire.
       CALL EFIELD(REAL(XU(NU)),REAL(YU(NU)),REAL(ZU(NU)),
     -      EX,EY,EZ,ETOT,VOLT,0,ILOC)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCWIM DEBUG   : NU = '',
     -      I5,'', pos = '',3E15.8,'', loc = '',I5,'', E = '',E15.8)')
     -      NU,XU(NU),YU(NU),ZU(NU),ILOC,ETOT
*   Quit if not ...
       IF(ILOC.LE.0.OR.ILOC.GT.NWIRE)THEN
            IF(NU.EQ.1.AND.RTRAP.GT.1)THEN
                 PRINT *,' !!!!!! DLCDIW WARNING : Premature end of'//
     -                ' a drift-line due to a TRAP-RADIUS > 1.'
            ELSE
                 PRINT *,' !!!!!! DLCWIM WARNING : Final location is'//
     -                ' not in a wire; abandoning the drift line.'
            ENDIF
            ISTAT=-3
            RETURN
*   Otherwise establish the target wire, which can be a copy.
       ELSE
            SHIFT=.FALSE.
            IF(PERX)THEN
                 XDIST=XU(NU)-DBLE(X(ILOC))
                 IF(ABS(XDIST).GT.SX/2)SHIFT=.TRUE.
                 XTARG=X(ILOC)+SX*ANINT(XDIST/SX)
            ELSE
                 XTARG=X(ILOC)
            ENDIF
            IF(PERY)THEN
                 YDIST=YU(NU)-DBLE(Y(ILOC))
                 IF(ABS(YDIST).GT.SY/2)SHIFT=.TRUE.
                 YTARG=Y(ILOC)+SY*ANINT(YDIST/SY)
            ELSE
                 YTARG=Y(ILOC)
            ENDIF
            DTARG=D(ILOC)
            IF(SHIFT)THEN
                 ITARG=ILOC+MXWIRE
            ELSE
                 ITARG=ILOC
            ENDIF
       ENDIF
*   Debugging.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCWIM DEBUG   : ITARG='',
     -      I5,'' (x,y)=('',E15.8,'' , '',E15.8,''), d='',E15.8/26X,
     -      ''Last location: '',3E15.8)') ITARG,XTARG,YTARG,DTARG,
     -      XU(NU),YU(NU),ZU(NU)
*** And be sure the one-but last position is outside the wire.
10     CONTINUE
       IF(NU.LT.2)THEN
            PRINT *,' !!!!!! DLCWIM WARNING : Unable to find a',
     -           ' point on the drift-line outside the wire.'
            ISTAT=-3
            RETURN
       ENDIF
       CALL EFIELD(REAL(XU(NU-1)),REAL(YU(NU-1)),REAL(ZU(NU-1)),
     -      EX,EY,EZ,ETOT,VOLT,0,ILOC)
       IF(ILOC.NE.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCWIM DEBUG   : '',
     -           '' Going back one step to NU = '',I5,'', pos = '',
     -           3E15.8,'', loc = '',I5,'', E = '',E15.8)')
     -           NU,XU(NU),YU(NU),ZU(NU),ILOC,ETOT
            NU=NU-1
            GOTO 10
       ENDIF
*** Truncate the step
       STEP=SQRT((XU(NU)-XU(NU-1))**2+(YU(NU)-YU(NU-1))**2)
       DIST=SQRT((XTARG -XU(NU-1))**2+(YTARG -YU(NU-1))**2)
       IF(STEP.LE.0.OR.DIST.LE.0)THEN
            PRINT *,' !!!!!! DLCWIM WARNING : Zero length step or'//
     -           ' distance; drift line abandoned.'
            ISTAT=-3
            RETURN
       ENDIF
       XU(NU)=XTARG+DTARG*(XU(NU-1)-XTARG)/(2.0001*DIST)
       YU(NU)=YTARG+DTARG*(YU(NU-1)-YTARG)/(2.0001*DIST)
       ZU(NU)=ZU(NU-1)+(ZU(NU)-ZU(NU-1))*SQRT((XU(NU)-XU(NU-1))**2+
     -      (YU(NU)-YU(NU-1))**2)/STEP
       TU(NU)=TU(NU-1)+(TU(NU)-TU(NU-1))*SQRT((XU(NU)-XU(NU-1))**2+
     -      (YU(NU)-YU(NU-1))**2)/STEP
       ISTAT=ITARG
       END
