CDECK  ID>, SIGQIN.
       SUBROUTINE SIGQIN(QTOT,ISW,TMIN,TMAX)
*-----------------------------------------------------------------------
*   SIGQIN - Integrates the induced charge over a drift line.
*   (Last changed on 20/ 3/03.)
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
       DOUBLE PRECISION F1(3),F2(3),F3(3),AUX(1),DGMLT1,
     -      DMEAN,XMID,YMID,ZMID,TABTIM,TABLAM,TMIN,TMAX,TIMIN,TIMAX
       REAL QTOT,DRES,EX1,EY1,EZ1,EX2,EY2,EZ2,EX3,EY3,EZ3
       INTEGER ILOC1,ILOC2,ILOC3,IU,IWIRE,NTAB,ISW,JSW,I
       PARAMETER(NTAB=10)
       EXTERNAL DGMLT1,FSCONT
       COMMON /SQIDAT/ TABTIM(NTAB),TABLAM(NTAB),IU,JSW
*** Initial value for the induced charge.
       QTOT=0
*** Avoid single-step lines (risk of being in the wire centre).
       IF(NU.LE.1)RETURN
*** Copy the sense wire number to the common.
       JSW=ISW
*** Set the wire diameter to 0, if a wire was hit.
       IF(ISTAT.GE.1.AND.ISTAT.LE.NWIRE)THEN
            IWIRE=ISTAT
       ELSEIF(ISTAT.GT.MXWIRE.AND.ISTAT.LE.2*MXWIRE)THEN
            IWIRE=ISTAT-MXWIRE
       ELSE
            IWIRE=0
       ENDIF
       IF(IWIRE.GT.0)THEN
            DRES=D(IWIRE)
            D(IWIRE)=D(IWIRE)/2
       ENDIF
*** Initial step.
       CALL DLCVEL(XU(1),YU(1),ZU(1),F1,QPCHAR,IPTYPE,ILOC1)
       CALL SIGFLS(REAL(XU(1)),REAL(YU(1)),REAL(ZU(1)),EX1,EY1,EZ1,JSW)
       IF(ILOC1.NE.0)THEN
            PRINT *,' !!!!!! SIGQIN WARNING : Ran into non-free'//
     -           ' zone at first step ; Q=0.'
            QTOT=0
            IF(IWIRE.GT.0)D(IWIRE)=DRES
            RETURN
       ENDIF
*** Loop over the drift line.
       DO 10 IU=2,NU
*   Evaluate end-point.
       CALL DLCVEL(XU(IU),YU(IU),ZU(IU),F3,QPCHAR,IPTYPE,ILOC3)
*   Abandon if this fails.
       IF(ILOC3.NE.0)THEN
            PRINT *,' !!!!!! SIGQIN WARNING : Ran into non-free'//
     -           ' zone at end of regular step ; Q=0.'
            QTOT=0
            IF(IWIRE.GT.0)D(IWIRE)=DRES
            RETURN
       ENDIF
       CALL SIGFLS(REAL(XU(IU)),REAL(YU(IU)),REAL(ZU(IU)),
     -      EX3,EY3,EZ3,JSW)
*   Avoid integration outside the time limits.
       IF(TMIN.GT.TU(IU).OR.TMAX.LT.TU(IU-1))GOTO 30
*   Try a parabolic weighted mean of the average position.
       XMID=XU(IU-1)+(TU(IU)-TU(IU-1))*(3*F1(1)+F3(1))/8
       YMID=YU(IU-1)+(TU(IU)-TU(IU-1))*(3*F1(2)+F3(2))/8
       ZMID=ZU(IU-1)+(TU(IU)-TU(IU-1))*(3*F1(3)+F3(3))/8
       CALL DLCVEL(XMID,YMID,ZMID,F2,QPCHAR,IPTYPE,ILOC2)
*   If this fails, try a straight mean.
       IF(ILOC2.NE.0)THEN
            XMID=(XU(IU)+XU(IU-1))/2
            YMID=(YU(IU)+YU(IU-1))/2
            ZMID=(ZU(IU)+ZU(IU-1))/2
            CALL DLCVEL(XMID,YMID,ZMID,F2,QPCHAR,IPTYPE,ILOC2)
*   If this too fails, abandon.
            IF(ILOC2.NE.0)THEN
                 PRINT *,' !!!!!! SIGQIN WARNING : Ran into non-free'//
     -                ' zone in middle of regular step ; Q=0.'
                 QTOT=0
                 IF(IWIRE.GT.0)D(IWIRE)=DRES
                 RETURN
            ENDIF
       ENDIF
       CALL SIGFLS(REAL(XMID),REAL(YMID),REAL(ZMID),EX2,EY2,EZ2,JSW)
*   Compare 1st and 2nd order.
       IF((TU(IU)-TMIN)*(TMIN-TU(IU-1)).LE.0.AND.
     -      (TU(IU)-TMAX)*(TMAX-TU(IU-1)).LE.0.AND.
     -      ABS((TU(IU)-TU(IU-1))*(
     -        (EX1*F1(1)+EY1*F1(2)+EZ1*F1(3))-
     -      2*(EX2*F2(1)+EY2*F2(2)+EZ2*F2(3))+
     -        (EX3*F3(1)+EY3*F3(2)+EZ3*F3(3))))/3.LT.1D-6)THEN
*   If they agree, use Simpsons formula.
            QTOT=QTOT+QPCHAR*(TU(IU)-TU(IU-1))*(
     -             (EX1*F1(1)+EY1*F1(2)+EZ1*F1(3))+
     -           4*(EX2*F2(1)+EY2*F2(2)+EZ2*F2(3))+
     -             (EX3*F3(1)+EY3*F3(2)+EZ3*F3(3)))/6
*   Otherwise use 6-point Gaussian integration.
       ELSE
*   Prepare an interpolation table for the time-lambda relation.
            TABTIM(1)=0
            TABLAM(1)=0
            DMEAN=SQRT((XU(IU)-XU(IU-1))**2+(YU(IU)-YU(IU-1))**2+
     -           (ZU(IU)-ZU(IU-1))**2)/DBLE(NTAB-1)
            DO 20 I=2,NTAB
            CALL DLCVEL(
     -           XU(IU-1)+(I-2.0D0)*(XU(IU)-XU(IU-1))/DBLE(NTAB-1),
     -           YU(IU-1)+(I-2.0D0)*(YU(IU)-YU(IU-1))/DBLE(NTAB-1),
     -           ZU(IU-1)+(I-2.0D0)*(ZU(IU)-ZU(IU-1))/DBLE(NTAB-1),
     -           F1,QPCHAR,IPTYPE,ILOC1)
            CALL DLCVEL(
     -           XU(IU-1)+(I-1.5D0)*(XU(IU)-XU(IU-1))/DBLE(NTAB-1),
     -           YU(IU-1)+(I-1.5D0)*(YU(IU)-YU(IU-1))/DBLE(NTAB-1),
     -           ZU(IU-1)+(I-1.5D0)*(ZU(IU)-ZU(IU-1))/DBLE(NTAB-1),
     -           F2,QPCHAR,IPTYPE,ILOC2)
            CALL DLCVEL(
     -           XU(IU-1)+(I-1.0D0)*(XU(IU)-XU(IU-1))/DBLE(NTAB-1),
     -           YU(IU-1)+(I-1.0D0)*(YU(IU)-YU(IU-1))/DBLE(NTAB-1),
     -           ZU(IU-1)+(I-1.0D0)*(ZU(IU)-ZU(IU-1))/DBLE(NTAB-1),
     -           F3,QPCHAR,IPTYPE,ILOC3)
            IF(SQRT(F1(1)**2+F1(2)**2+F1(3)**2).LE.0.OR.
     -           SQRT(F2(1)**2+F2(2)**2+F2(3)**2).LE.0.OR.
     -           SQRT(F3(1)**2+F3(2)**2+F3(3)**2).LE.0.OR.
     -           ILOC1.NE.0.OR.ILOC2.NE.0.OR.ILOC3.NE.0)THEN
                 PRINT *,' !!!!!! SIGQIN WARNING : Ran into non'//
     -                ' free area in a Gauss step ; Qe set to 0.'
                 QTOT=0
                 IF(IWIRE.GT.0)D(IWIRE)=DRES
                 RETURN
            ENDIF
            TABTIM(I)=TABTIM(I-1)+DMEAN*(
     -           1/SQRT(F1(1)**2+F1(2)**2+F1(3)**2)+
     -           4/SQRT(F2(1)**2+F2(2)**2+F2(3)**2)+
     -           1/SQRT(F3(1)**2+F3(2)**2+F3(3)**2))/6
            TABLAM(I)=DBLE(I-1)/DBLE(NTAB-1)
20          CONTINUE
*   Set integration limits.
            TIMIN=MAX(TMIN-TU(IU-1),TABTIM(1))
            TIMAX=MIN(TMAX-TU(IU-1),TABTIM(NTAB))
*   Add the contribution.
            QTOT=QTOT+QPCHAR*DGMLT1(FSCONT,TIMIN,TIMAX,1,8,AUX)
       ENDIF
*   Shift the field for the end-point to the starting point.
30     CONTINUE
       EX1=EX3
       EY1=EY3
       EZ1=EZ3
       F1(1)=F3(1)
       F1(2)=F3(2)
       F1(3)=F3(3)
10     CONTINUE
*** Restore the wire diameter.
       IF(IWIRE.GT.0)D(IWIRE)=DRES
*** Invert sign of the induced charge.
       QTOT=-QTOT
       END
