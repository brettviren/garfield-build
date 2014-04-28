CDECK  ID>, DLCWIR.
       SUBROUTINE DLCWIR(ISKIP,Q,ITYPE)
*-----------------------------------------------------------------------
*   DLCWIR - Terminates drift line calculation by making some last steps
*            towards the surface of the wire on which it is supposed to
*            end. The precision is controlled in order to obtain a
*            good estimate of the total remaining drift-time.
*   VARIABLES : (X1,Y1)     : First point of an integration segment.
*               (XM,YM)     : Middle point of an integration segment.
*               (X2,Y2)     : Last point of an integration segment.
*               F1, FM, F2  : Velocities at (X1,Y1), (XM,YM), (X2,Y2).
*               ONWIRE      : .TRUE. if the last point is on the wire.
*               ISKIP       : Skip searching for the nearest wire and
*                             use (XTARG,YTARG) instead.
*   (Last changed on 28/ 7/08.)
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
       INTEGER MXSPLT
       PARAMETER (MXSPLT=10)
       DOUBLE PRECISION F1(3),FM(3),F2(3),X1,XM,X2,Y1,YM,Y2,
     -      Z1,ZM,Z2,T1,T2,DIST2,TCRUDE,XDIST,YDIST,DISMIN
       REAL Q,EX,EY,EZ,ETOT,VOLT,XWAUX,YWAUX
       INTEGER ITYPE,IFLAG,ILOC,ILOC1,ILOC2,ILOCM,I,ISKIP,ISPLIT,IWEND
       LOGICAL ONWIRE,SHIFT
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE DLCWIR ///'
*** Call dedicated routine for microscopic tracking.
       IF(IPTECH.EQ.4)THEN
            CALL DLCWIM
            RETURN
       ENDIF
*** Debugging.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCWIR DEBUG   : ITARG='',
     -      I5,'' (x,y)=('',E15.8,'' , '',E15.8,''), d='',E15.8)')
     -      ITARG,XTARG,YTARG,DTARG
*** Step backwards until we have a point where the field is non-zero.
10     CONTINUE
       CALL EFIELD(REAL(XU(NU)),REAL(YU(NU)),REAL(ZU(NU)),
     -      EX,EY,EZ,ETOT,VOLT,0,ILOC)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCWIR DEBUG   : NU = '',
     -      I5,'', pos = '',3E15.8,'', loc = '',I5,'', E = '',E15.8)')
     -      NU,XU(NU),YU(NU),ZU(NU),ILOC,ETOT
       IF(ILOC.NE.0.OR.ETOT.EQ.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCWIR DEBUG   :'',
     -           '' Last point at NU='',I5,'' is at zero field ;'',
     -           '' NU lowered by 1.'')') NU
            IF(NU.GT.1)THEN
                 NU=NU-1
                 GOTO 10
            ELSE
                 PRINT *,' !!!!!! DLCWIR WARNING : Unable to find a',
     -                ' point on the drift-line where E is not zero.'
                 ISTAT=-3
            ENDIF
       ENDIF
*** Make sure space is left for the steps to come.
       IF(NU.GE.MXLIST)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCWIR DEBUG   : No'',
     -           '' storage left ; stepping to the wire not done.'')')
            ISTAT=-2
            NU=MXLIST
            RETURN
       ENDIF
*** Skip finding the wire if ISKIP=1.
       IF(ISKIP.EQ.1)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCWIR DEBUG   :'',
     -           '' Wire search skipped due to ISKIP='',I5)') ISKIP
            IF(ITARG.GT.MXWIRE)THEN
                 IWEND=ITARG-MXWIRE
            ELSE
                 IWEND=ITARG
            ENDIF
       ELSE
*** Find out whether the diagnosis about the target wire is correct.
            ITARG=0
            DISMIN=0
            IWEND=0
            DO 20 I=1,NWIRE
*   First find the wire closest to where we are now.
            XWAUX=X(I)
            YWAUX=Y(I)
            SHIFT=.FALSE.
            IF(PERX)THEN
                 XDIST=XU(NU)-DBLE(X(I))
                 IF(ABS(XDIST).GT.SX/2)SHIFT=.TRUE.
                 XWAUX=X(I)+SX*ANINT(XDIST/SX)
            ENDIF
            IF(PERY)THEN
                 YDIST=YU(NU)-DBLE(Y(I))
                 IF(ABS(YDIST).GT.SY/2)SHIFT=.TRUE.
                 YWAUX=Y(I)+SY*ANINT(YDIST/SY)
            ENDIF
            DIST2=(XU(NU)-XWAUX)**2+(YU(NU)-YWAUX)**2
*   Keep track of which one is closest.
            IF(ITARG.EQ.0.OR.DIST2.LT.DISMIN)THEN
                 DISMIN=DIST2
                 XTARG=XWAUX
                 YTARG=YWAUX
                 DTARG=D(I)
                 IWEND=I
                 IF(SHIFT)THEN
                      ITARG=I+MXWIRE
                 ELSE
                      ITARG=I
                 ENDIF
            ENDIF
20          CONTINUE
            IF(IWEND.EQ.0)THEN
                 PRINT *,' ###### DLCWIR ERROR   : No target wire'//
     -                ' found ; program bug - please report.'
                 ISTAT=-3
            ENDIF
       ENDIF
*** Cheat with the target wire to avoid getting into it.
       D(IWEND)=DTARG/2
**  Final stepping towards the wire starts.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCWIR DEBUG   : Stepping'',
     -      '' towards wire '',I5,'' started at NU= '',I5)') IWEND,NU
       X1=XU(NU)
       Y1=YU(NU)
       Z1=ZU(NU)
       T1=TU(NU)
**  Make an estimate for a full step towards the wire.
       CALL DLCVEL(X1,Y1,Z1,F1,Q,ITYPE,ILOC1)
       IF(SQRT(F1(1)**2+F1(2)**2).LE.0.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCWIR DEBUG   :'',
     -      '' Initial drift-velocity zero; quit on ISTAT=-3.'')')
            ISTAT=-3
            D(IWEND)=DTARG
            RETURN
       ENDIF
*   Estimate the time needed to reach the wire.
       TCRUDE=(SQRT((X1-XTARG)**2+(Y1-YTARG)**2)-DTARG/2.0)/
     -      SQRT(F1(1)**2+F1(2)**2)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCWIR DEBUG   : Estimated'',
     -      '' time needed to reach the wire: '',E12.5)') TCRUDE
*   Special handling for small TCRUDE.
       IF(TCRUDE.LT.1.0E-6*TU(NU))THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCWIR DEBUG   :'',
     -           '' Small TCRUDE exception; no further processing.'')')
            ISTAT=ITARG
            D(IWEND)=DTARG
            RETURN
       ENDIF
*** Iteration starts here: set the number of integration divisions to 0.
       ISPLIT=0
100    CONTINUE
*   Estimate where the drift-line will end up after this time.
       X2=X1+F1(1)*TCRUDE
       Y2=Y1+F1(2)*TCRUDE
       Z2=Z1+F1(3)*TCRUDE
*   Set the flag for being in the wire to .FALSE.
       ONWIRE=.FALSE.
**  Take action depending on where we end up, first moving away.
       CALL DLCMIN(XTARG,YTARG,X1,Y1,X2,Y2,DIST2,IFLAG)
       IF(IFLAG.EQ.-1)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCWIR DEBUG   :'',
     -           '' Particle moves away from the wire ; quit on'',
     -           '' ISTAT=-3.'')')
            ISTAT=-3
            D(IWEND)=DTARG
            RETURN
*   Next the case the wire has been crossed.
       ELSEIF(IFLAG.EQ.0.OR.DIST2.LE.(DTARG/2)**2)THEN
            X2=XTARG-0.5*DTARG*(XTARG-X1)/
     -           SQRT((X1-XTARG)**2+(Y1-YTARG)**2)
            Y2=YTARG-0.5*DTARG*(YTARG-Y1)/
     -           SQRT((X1-XTARG)**2+(Y1-YTARG)**2)
            TCRUDE=SQRT(((X2-X1)**2+(Y2-Y1)**2)/(F1(1)**2+F1(2)**2))
            Z2=Z1+TCRUDE*F1(3)
            ONWIRE=.TRUE.
       ENDIF
**  Calculate the drift-velocity at the end point.
       CALL DLCVEL(X2,Y2,Z2,F2,Q,ITYPE,ILOC2)
**  Set a point halfway between 1 and 2 for an accuracy check.
       XM=0.5*(X1+X2)
       YM=0.5*(Y1+Y2)
       ZM=0.5*(Z1+Z2)
       CALL DLCVEL(XM,YM,ZM,FM,Q,ITYPE,ILOCM)
**  Check the location codes.
       IF(ILOC1.NE.0.OR.ILOCM.NE.0.OR.ILOC2.NE.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCWIR DEBUG   :'',
     -           '' ILOC position codes cause ISTAT=-3 quit: '',
     -           3I5)') ILOC1,ILOCM,ILOC2
            ISTAT=-3
            D(IWEND)=DTARG
            RETURN
       ENDIF
*   Check the non-zeroness of the velocities.
       IF(SQRT(F1(1)**2+F1(2)**2).LE.0.0.OR.
     -      SQRT(FM(1)**2+FM(2)**2).LE.0.0.OR.
     -      SQRT(F2(1)**2+F2(2)**2).LE.0.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCWIR DEBUG   :'',
     -           '' Intermediate drift-velocity zero; ISTAT=-3.'')')
            ISTAT=-3
            D(IWEND)=DTARG
            RETURN
       ENDIF
**  Compare first and second order estimates.
       IF(ISPLIT.GE.MXSPLT.OR.SQRT((X2-X1)**2+(Y2-Y1)**2)*
     -      ABS(1.0/SQRT(F1(1)**2+F1(2)**2)-2.0/SQRT(FM(1)**2+FM(2)**2)+
     -      1.0/SQRT(F2(1)**2+F2(2)**2))/3.0.LT.1.0D-4*(1+ABS(T1)))THEN
*   Accurate enough: integrate the drift-time over this segment.
            T2=T1+SQRT((X2-X1)**2+(Y2-Y1)**2)*
     -           (1.0/SQRT(F1(1)**2+F1(2)**2)+
     -            4.0/SQRT(FM(1)**2+FM(2)**2)+
     -            1.0/SQRT(F2(1)**2+F2(2)**2))/6.0
*   Add to the drift-line if there is space left.
            IF(NU.GE.MXLIST)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCWIR DEBUG   :'',
     -                '' No space left for ; ISTAT=-2 return.'')')
                 ISTAT=-2
                 NU=MXLIST
                 D(IWEND)=DTARG
                 RETURN
            ELSE
                 NU=NU+1
                 XU(NU)=X2
                 YU(NU)=Y2
                 ZU(NU)=Z2
                 TU(NU)=T2
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCWIR DEBUG   :'',
     -                '' Adding point '',I5,'' at '',4E15.8)')
     -                NU,XU(NU),YU(NU),ZU(NU),TU(NU)
                 IF(ONWIRE)THEN
                      ISTAT=ITARG
                      D(IWEND)=DTARG
                      IF(LDEBUG)
     -                     WRITE(LUNOUT,'(''  ++++++ DLCWIR DEBUG   :'',
     -                     '' This was the last step.'')')
                      RETURN
                 ENDIF
                 IF(LDEBUG.AND.ISPLIT.GT.0)
     -                WRITE(LUNOUT,'(''  ++++++ DLCWIR DEBUG   :'',
     -                '' Adding at ISPLIT='',I5)') ISPLIT
            ENDIF
*   Proceed with the next step.
            X1=X2
            Y1=Y2
            Z1=Z2
            T1=T2
            ILOC1=ILOC2
            F1(1)=F2(1)
            F1(2)=F2(2)
            F1(3)=F2(3)
            GOTO 100
**  Halve the step-size if the accuracy is insufficient.
       ELSE
            TCRUDE=TCRUDE/2
            ISPLIT=ISPLIT+1
            GOTO 100
       ENDIF
       END
