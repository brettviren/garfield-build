CDECK  ID>, DLCSOL.
       SUBROUTINE DLCSOL(XX0,YY0,ZZ0,XX1,YY1,ZZ1,DT,ILOC,Q,ITYPE)
*-----------------------------------------------------------------------
*   DLCSOL - Terminates drift line calculation by making a last step
*            to the boundary of a solid.
*   VARIABLES : (XX0,YY0,ZZ0): Last point in drift medium.
*               (XX1,YY1,ZZ1): Estimated step, outside drift medium.
*               (X0,Y0,Z0)  : Final point just inside medium
*               (X1,Y1,Z1)  : Final point just outside medium
*               FF0         : Drift velocity at (XX0,YY0,ZZ0)
*               F0          : Drift velocity at (X0,Y0,Z0)
*               DELTAT      : Time step
*   (Last changed on  8/ 5/10.)
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
       INTEGER NBISEC
       PARAMETER(NBISEC=20)
       INTEGER ILOC,ITYPE,ILOC0,ILOC1,ILOCM,ILOCS,I,ILOCVF,IFAIL,IVOL
       REAL Q,EOVERM,X0,Y0,Z0,X1,Y1,Z1,XM,YM,ZM
       DOUBLE PRECISION XX0,YY0,ZZ0,XX1,YY1,ZZ1,POS(3),
     -      FF0(3),F0(3),ACC(3),SPEED,ACCEL,STEP,
     -      XOLD,YOLD,ZOLD,DT
       COMMON /VFUCOM/ EOVERM,ILOCVF
*** Identify this routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE DLCSOL ///'
*** Ensure there is a previous stored step.
       IF(NU.LE.0)THEN
            PRINT *,' ###### DLCSOL ERROR   : Called at first step;'//
     -           ' program bug, please report.'
            ISTAT=-3
            RETURN
       ENDIF
*** Check we may still add points.
       IF(NU.GE.MXLIST)THEN
            ISTAT=-2
            IF(LDEBUG)PRINT *,' ++++++ DLCSOL DEBUG   : Last point'//
     -           ' not added because MXLIST is reached.'
            RETURN
       ENDIF
*** Ensure we got an appropriate location code.
       IF(ILOC.LE.2*MXWIRE)THEN
            PRINT *,' ###### DLCSOL ERROR   : Called for location'//
     -           ' code ',ILOC,'; program bug, please report.'
            ISTAT=-3
            RETURN
       ENDIF
*** Initialise the bisection loop.
       X0=REAL(XX0)
       Y0=REAL(YY0)
       Z0=REAL(ZZ0)
       X1=REAL(XX1)
       Y1=REAL(YY1)
       Z1=REAL(ZZ1)
       CALL CELVPT(DBLE(X0),DBLE(Y0),DBLE(Z0),ILOC0)
       CALL CELVPT(DBLE(X1),DBLE(Y1),DBLE(Z1),ILOC1)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCSOL DEBUG   : Starting'',
     -      '' from (x,y,z)='',3E15.8,'' loc='',I5/34X,
     -      '' to   (x,y,z)='',3E15.8,'' loc='',I5)') X0,Y0,Z0,ILOC0,
     -      X1,Y1,Z1,ILOC1
       IF(ILOC0.NE.0.OR.ILOC1.EQ.0)THEN
            IF(LDEBUG)PRINT *,' ++++++ DLCSOL DEBUG   : Called but'//
     -           ' ILOC=',ILOC0,ILOC1,' returning ISTAT=-3.'
            ISTAT=-3
            RETURN
       ENDIF
*** Perform some bisections.
       ILOCS=ILOC1
       DO 10 I=1,NBISEC
*   Quit bisection when interval becomes too small.
       IF(ABS(X1-X0).LE.1D-6*(ABS(X0)+ABS(X1)).AND.
     -      ABS(Y1-Y0).LE.1D-6*(ABS(Y0)+ABS(Y1)).AND.
     -      ABS(Z1-Z0).LE.1D-6*(ABS(Z0)+ABS(Z1)))THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCSOL DEBUG   :'',
     -           '' Bisection ended at loop '',I5,'' (interval'',
     -           '' too small).'')') I
            GOTO 20
       ENDIF
*   Middle point.
       XM=0.5*(X0+X1)
       YM=0.5*(Y0+Y1)
       ZM=0.5*(Z0+Z1)
*   Evaluate field.
       CALL CELVPT(DBLE(XM),DBLE(YM),DBLE(ZM),ILOCM)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCSOL DEBUG   : Bisection'',
     -      '' at  (x,y,z)='',3E15.8,'' loc='',I5)') XM,YM,ZM,ILOCM
*   Shift limits of the bisection.
       IF(ILOCM.EQ.0)THEN
            X0=XM
            Y0=YM
            Z0=ZM
       ELSE
            X1=XM
            Y1=YM
            Z1=ZM
            ILOCS=ILOCM
       ENDIF
10     CONTINUE
*   Maximum number of iterations reached.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCSOL DEBUG   :'',
     -      '' Bisection ended at loop '',I5,'' (maximum number'',
     -      '' of iterations).'')') NBISEC
*** Calculate the drift velocity over the last step.
20     CONTINUE
**  Runge-Kutta-Fehlberg or Monte-Carlo drifting.
       IF(ITYPE.EQ.1.OR.ITYPE.EQ.2)THEN
*   Compute drift velocity at begin and end of the step.
            CALL DLCVEL(XX0,YY0,ZZ0,FF0,Q,ITYPE,ILOC0)
            CALL DLCVEL(DBLE(X0),DBLE(Y0),DBLE(Z0),F0,Q,ITYPE,ILOC1)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCSOL DEBUG   :'',
     -           '' Loc start: '',I5,'' Loc end: '',I5)') ILOC0,ILOC1
*   Average if both are in a free area.
            IF(ILOC0.EQ.0.AND.ILOC1.EQ.0)THEN
                 SPEED=SQRT((FF0(1)+F0(1))**2+(FF0(2)+F0(2))**2+
     -                (FF0(3)+F0(3))**2)/2
*   Or approximate with the last step only.
            ELSEIF(ILOC0.EQ.0)THEN
                 SPEED=SQRT(FF0(1)**2+FF0(2)**2+FF0(3)**2)
                 PRINT *,' ------ DLCSOL MESSAGE : Unable to compute'//
     -                ' mean drift speed at last step; approximated.'
*   At least one should be OK.
            ELSE
                 PRINT *,' !!!!!! DLCSOL WARNING : Unable to compute'//
     -                ' drift speed at last step; aborted.'
                 ISTAT=-3
                 RETURN
            ENDIF
**  Vacuum drift.
       ELSEIF(ITYPE.EQ.3)THEN
*   If there are already steps, estimate speed from the last step.
            IF(NU.GT.1)THEN
                 IF(TU(NU)-TU(NU-1).GT.0)THEN
                      FF0(1)=(XU(NU)-XU(NU-1))/(TU(NU)-TU(NU-1))
                      FF0(2)=(YU(NU)-YU(NU-1))/(TU(NU)-TU(NU-1))
                      FF0(3)=(ZU(NU)-ZU(NU-1))/(TU(NU)-TU(NU-1))
                 ELSE
                      PRINT *,' !!!!!! DLCSOL WARNING : Drift speed'//
     -                     ' over previous step is 0; aborted.'
                      ISTAT=-3
                      RETURN
                 ENDIF
*   Otherwise set speed to 0.
            ELSE
                 FF0(1)=0
                 FF0(2)=0
                 FF0(3)=0
            ENDIF
*   Use speed and location to compute the acceleration.
            POS(1)=XX0
            POS(2)=YY0
            POS(3)=ZZ0
            ILOCVF=0
            CALL DLCVFU(0.0D0,POS,FF0,ACC)
            IF(ILOCVF.NE.0)THEN
                 PRINT *,' !!!!!! DLCSOL WARNING : Unable to compute'//
     -                ' acceleration over last step; aborted.'
                 ISTAT=-3
                 RETURN
            ENDIF
*   Estimate from these what the average speed for the last step is.
            SPEED=SQRT(FF0(1)**2+FF0(2)**2+FF0(3)**2)
            ACCEL=SQRT(ACC(1)**2+ACC(2)**2+ACC(3)**2)
            STEP=SQRT((X0-XX0)**2+(Y0-YY0)**2+(Z0-ZZ0)**2)
            SPEED=SPEED/2+SQRT(SPEED**2+2*ACCEL*STEP)/2
**  Microscopic tracking
       ELSEIF(ITYPE.EQ.4)THEN
            IF(DT.LE.0)THEN
                 PRINT *,' !!!!!! DLCSOL WARNING : Time delay over'//
     -                ' the proposed step is not > 0; aborted.'
                 ISTAT=-3
                 RETURN
            ELSE
                 SPEED=SQRT((XX0-XX1)**2+(YY0-YY1)**2+(ZZ0-ZZ1)**2)/DT
            ENDIF
**  Anything else
       ELSE
            PRINT *,' ###### DLCSOL ERROR   : Unknown tracking method'//
     -           ' received: ',ITYPE,'; abandoning the drift line.'
            ISTAT=-3
            RETURN
       ENDIF
**  Check velocity.
       IF(SPEED.LE.0)THEN
            PRINT *,' !!!!!! DLCSOL WARNING : Drift line not properly'//
     -           ' terminated because of zero velocity.'
            ISTAT=-3
            RETURN
       ENDIF
*** Add the last step to the boundary.
       NU=NU+1
       XU(NU)=DBLE(X0)
       YU(NU)=DBLE(Y0)
       ZU(NU)=DBLE(Z0)
*** And fill in the time for the last step.
       TU(NU)=TU(NU-1)+SQRT((XU(NU)-XU(NU-1))**2+
     -      (YU(NU)-YU(NU-1))**2+(ZU(NU)-ZU(NU-1))**2)/SPEED
*** Assign the status code.
       CALL CELVPT(DBLE(X0),DBLE(Y0),DBLE(Z0),IVOL)
       IF(IVOL.LE.0)THEN
            ISTAT=2*MXWIRE+ILOCS
       ELSE
            ISTAT=2*MXWIRE+IVOL
       ENDIF
*** Check that the particle is still inside the drift area, clip if not.
       IF(XU(NU).LT.DDXMIN)ISTAT=ISTAT1
       IF(XU(NU).GT.DDXMAX)ISTAT=ISTAT2
       IF(YU(NU).LT.DDYMIN)ISTAT=ISTAT3
       IF(YU(NU).GT.DDYMAX)ISTAT=ISTAT4
       IF(ZU(NU).LT.DDZMIN)ISTAT=ISTAT5
       IF(ZU(NU).GT.DDZMAX)ISTAT=ISTAT6
       IF(ISTAT.NE.ILOCS)THEN
            XOLD=XU(NU)
            YOLD=YU(NU)
            ZOLD=ZU(NU)
            CALL CLIP3D(XU(NU-1),    YU(NU-1),    ZU(NU-1),
     -                  XU(NU),      YU(NU),      ZU(NU),
     -                  DBLE(DDXMIN),DBLE(DDYMIN),DBLE(DDZMIN),
     -                  DBLE(DDXMAX),DBLE(DDYMAX),DBLE(DDZMAX),IFAIL)
            IF(IFAIL.NE.0.OR.(XOLD.EQ.XU(NU-1).AND.
     -           YOLD.EQ.YU(NU-1).AND.ZOLD.EQ.ZU(NU-1)))THEN
                 NU=NU-1
            ELSE
                 TU(NU)=TU(NU-1)+(TU(NU)-TU(NU-1))*SQRT(
     -                ((XU(NU)-XU(NU-1))**2+(YU(NU)-YU(NU-1))**2+
     -                 (ZU(NU)-ZU(NU-1))**2)/
     -                ((XOLD-XU(NU-1))**2+(YOLD-YU(NU-1))**2+
     -                 (ZOLD-ZU(NU-1))**2))
            ENDIF
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCSOL DEBUG   : Area'',
     -           '' left or solid entered, ISTAT='',I5)') ISTAT
       ENDIF
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCSOL DEBUG   : NU='',I5/
     -      5X,''Old step: '',3E12.5,'' location: '',I10/
     -      5X,''End     : '',3E12.5,'' location: '',I10/
     -      5X,''New step: '',3E12.5,'' status:   '',I10/
     -      5X,''Speed   : '',E12.5)')
     -      NU,XX0,YY0,ZZ0,ILOC0,XX1,YY1,ZZ1,ILOC1,X0,Y0,Z0,ISTAT,SPEED
       END
