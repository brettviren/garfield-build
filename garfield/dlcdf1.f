CDECK  ID>, DLCDF1.
       SUBROUTINE DLCDF1(SIGMA)
*-----------------------------------------------------------------------
*   DLCDF1 - Routine returning the integrated diffusion coefficient of
*            the current drift line. The routine uses an adaptive
*            Simpson integration.
*   VARIABLES : SIGMA.     : Diffusion coefficients (1,2 end; M middle).
*               V.         : Drift velocity (1,2: end points; M middle).
*               CRUDE      : Crude estimate of SIGMA.
*   (Last changed on  4/ 2/00.)
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
       REAL SIGVEC(MXLIST),VELVEC(MXLIST),SIGMA,SIGMA1,SIGMA2,
     -      SIGMAM,V1,V2,VM,EX,EY,EZ,ETOT,EXM,EYM,EZM,ETOTM,VOLT,
     -      BX,BY,BZ,BTOT,BXM,BYM,BZM,BTOTM,DRES,GASDFL
       DOUBLE PRECISION XAUX1,XAUX2,YAUX1,YAUX2,STEP,SUM,XPOS1,XPOS2,
     -      XPOSM,YPOS1,YPOS2,YPOSM,ZPOS1,ZPOS2,ZPOSM,TOTSTP,CRUDE,
     -      STACK(MXSTCK,5),F1(3)
       INTEGER LOCVEC(MXLIST),ILOC,ILOC2,ILOCM,IU,ILOCRS,NSTACK,ISTACK,
     -      NFC
       EXTERNAL GASDFL
*** Identify the routine
       IF(LIDENT)PRINT *,' /// ROUTINE DLCDF1 ///'
*** Return straight away if there is only one data point.
       IF(NU.LE.1)THEN
            SIGMA=0.0
            RETURN
       ENDIF
*** Obtain a rough estimate of the result.
       CRUDE=0.0
       DO 100 IU=1,NU
       CALL EFIELD(REAL(XU(IU)),REAL(YU(IU)),REAL(ZU(IU)),
     -      EX,EY,EZ,ETOT,VOLT,0,ILOC)
       CALL BFIELD(REAL(XU(IU)),REAL(YU(IU)),REAL(ZU(IU)),
     -      BX,BY,BZ,BTOT)
       CALL DLCVEL(XU(IU),YU(IU),ZU(IU),F1,-1.0,1,ILOC2)
*   Cheat in case the point is located inside a wire.
       IF(ILOC.GT.0.AND.ILOC.LE.MXWIRE)THEN
            DRES=D(ILOC)
            ILOCRS=ILOC
            D(ILOCRS)=0.0
            CALL EFIELD(REAL(XU(IU)),REAL(YU(IU)),REAL(ZU(IU)),
     -           EX,EY,EZ,ETOT,VOLT,0,ILOC)
            CALL BFIELD(REAL(XU(IU)),REAL(YU(IU)),REAL(ZU(IU)),
     -           BX,BY,BZ,BTOT)
            CALL DLCVEL(XU(IU),YU(IU),ZU(IU),F1,-1.0,1,ILOC2)
            D(ILOCRS)=DRES
            IF(LDEBUG)PRINT *,' ++++++ DLCDF1 DEBUG   : Drift-line',
     -           ' data point in wire ',ILOCRS,' detected; d=0 fix.'
       ENDIF
*   Store the information for this point of the drift line.
       LOCVEC(IU)=ILOC
       IF(POLAR)THEN
            VELVEC(IU)=SQRT(F1(1)**2+F1(2)**2+F1(3)**2)*EXP(XU(IU))
            SIGVEC(IU)=GASDFL(EX/EXP(REAL(XU(IU))),
     -           EY/EXP(REAL(XU(IU))),EZ,BX,BY,BZ)
            IF(IU.GT.1)THEN
                 CALL CF2RTC(XU(IU),YU(IU),XAUX1,YAUX1,1)
                 CALL CF2RTC(XU(IU-1),YU(IU-1),XAUX2,YAUX2,1)
                 STEP=SQRT((XAUX2-XAUX1)**2+(YAUX2-YAUX1)**2+
     -                (ZU(IU)-ZU(IU-1))**2)
            ENDIF
       ELSE
            VELVEC(IU)=SQRT(F1(1)**2+F1(2)**2+F1(3)**2)
            SIGVEC(IU)=GASDFL(EX,EY,EZ,BX,BY,BZ)
            IF(IU.GT.1)STEP=SQRT((XU(IU)-XU(IU-1))**2+
     -           (YU(IU)-YU(IU-1))**2+(ZU(IU)-ZU(IU-1))**2)
       ENDIF
       IF(IU.GT.1)THEN
            IF(VELVEC(IU)*VELVEC(IU-1).GT.0)
     -           CRUDE=CRUDE+STEP*((SIGVEC(IU)/VELVEC(IU))**2+
     -           (SIGVEC(IU-1)/VELVEC(IU-1))**2)/2.0
       ENDIF
100    CONTINUE
       NFC=NU
       CRUDE=SQRT(CRUDE)
*** Initialise the double precision copy of SIGMA: SUM.
       SUM=0.0
*** Loop over the whole drift-line.
       ISTACK=0
       DO 10 IU=1,NU-1
       IF(LOCVEC(IU).NE.0.OR.LOCVEC(IU+1).NE.0)GOTO 10
*   Initial values for the position.
       XPOS1=XU(IU)
       YPOS1=YU(IU)
       ZPOS1=ZU(IU)
       V1=VELVEC(IU)
       SIGMA1=SIGVEC(IU)
       XPOS2=XU(IU+1)
       YPOS2=YU(IU+1)
       ZPOS2=ZU(IU+1)
       V2=VELVEC(IU+1)
       SIGMA2=SIGVEC(IU+1)
*   Calculate the total steplength, in Cartesian coordinates.
       IF(POLAR)THEN
            CALL CF2RTC(XPOS1,YPOS1,XAUX1,YAUX1,1)
            CALL CF2RTC(XPOS2,YPOS2,XAUX2,YAUX2,1)
            TOTSTP=SQRT((XAUX2-XAUX1)**2+(YAUX2-YAUX1)**2+
     -           (ZPOS2-ZPOS1)**2)
       ELSE
            TOTSTP=SQRT((XPOS2-XPOS1)**2+(YPOS2-YPOS1)**2+
     -           (ZPOS2-ZPOS1)**2)
       ENDIF
**  Return at this point of further refinement is needed.
       NSTACK=0
20     CONTINUE
*   Set the new middle point, to be used for comparison.
       XPOSM=0.5*(XPOS1+XPOS2)
       YPOSM=0.5*(YPOS1+YPOS2)
       ZPOSM=0.5*(ZPOS1+ZPOS2)
*   Compute field, diffusion and velocity at the middle point.
       CALL EFIELD(REAL(XPOSM),REAL(YPOSM),REAL(ZPOSM),
     -      EXM,EYM,EZM,ETOTM,VOLT,0,ILOCM)
       CALL BFIELD(REAL(XPOSM),REAL(YPOSM),REAL(ZPOSM),
     -      BXM,BYM,BZM,BTOTM)
       CALL DLCVEL(XPOSM,YPOSM,ZPOSM,F1,-1.0,1,ILOC2)
       NFC=NFC+1
*   Cheat in case the point is located inside a wire.
       IF(ILOCM.GT.0.AND.ILOCM.LE.MXWIRE)THEN
            DRES=D(ILOCM)
            ILOCRS=ILOCM
            D(ILOCRS)=0.0
            CALL EFIELD(REAL(XPOSM),REAL(YPOSM),REAL(ZPOSM),
     -           EXM,EYM,EZM,ETOTM,VOLT,0,ILOCM)
            CALL BFIELD(REAL(XPOSM),REAL(YPOSM),REAL(ZPOSM),
     -           BXM,BYM,BZM,BTOTM)
            CALL DLCVEL(XPOSM,YPOSM,ZPOSM,F1,-1.0,1,ILOC2)
            NFC=NFC+1
            D(ILOCRS)=DRES
            IF(LDEBUG)PRINT *,' ++++++ DLCDF1 DEBUG   : Intermediate',
     -           ' point in wire ',ILOCRS,' detected; d=0 fix.'
       ENDIF
*   In case this still didn't help, skip this step.
       IF(ILOCM.NE.0)GOTO 10
*   Otherwise compute drift speed and diffusion at intermediate point.
       IF(POLAR)THEN
            VM=SQRT(F1(1)**2+F1(2)**2+F1(3)**2)*EXP(XPOSM)
            SIGMAM=GASDFL(EXM/EXP(REAL(XPOSM)),EYM/EXP(REAL(XPOSM)),EZM,
     -           BXM,BYM,BZM)
       ELSE
            VM=SQRT(F1(1)**2+F1(2)**2+F1(3)**2)
            SIGMAM=GASDFL(EXM,EYM,EZM,BXM,BYM,BZM)
       ENDIF
*   Prevent division by zero in the strange case the speed is 0.
       IF(V1*V2*VM.EQ.0.0.OR.SIGMA1*SIGMAM*SIGMA2.EQ.0.0)THEN
            PRINT *,' !!!!!! DLCDF1 WARNING : Drift velocity or',
     -           ' diffusion = 0 detected; some points skipped.'
            GOTO 10
       ENDIF
*** Compare first and second order estimates, divide if too large.
       IF(NSTACK.LT.MIN(MXSTCK,MXDIFS).AND.EPSDFI*CRUDE.LT.
     -      ABS((SIGMA1/V1)**2-2.0*(SIGMAM/VM)**2+(SIGMA2/V2)**2)*
     -      SQRT(TOTSTP*2.0/((SIGMA1/V1)**2+(SIGMA2/V2)**2))/6.0)THEN
            NSTACK=NSTACK+1
            ISTACK=MAX(ISTACK,NSTACK)
            STACK(NSTACK,1)=XPOS2
            STACK(NSTACK,2)=YPOS2
            STACK(NSTACK,5)=ZPOS2
            STACK(NSTACK,3)=V2
            STACK(NSTACK,4)=SIGMA2
            XPOS2=XPOSM
            YPOS2=YPOSM
            ZPOS2=ZPOSM
            V2=VM
            SIGMA2=SIGMAM
            GOTO 20
**  No further subdevision is required, transform polar coordinates.
       ELSE
*   Make sure the distances are measured in cartesian coordinates.
            IF(POLAR)THEN
                 CALL CF2RTC(XPOS1,YPOS1,XAUX1,YAUX1,1)
                 CALL CF2RTC(XPOS2,YPOS2,XAUX2,YAUX2,1)
                 STEP=SQRT((XAUX2-XAUX1)**2+(YAUX2-YAUX1)**2+
     -                (ZPOS2-ZPOS1)**2)
            ELSE
                 STEP=SQRT((XPOS2-XPOS1)**2+(YPOS2-YPOS1)**2+
     -                (ZPOS2-ZPOS1)**2)
            ENDIF
*   Add the new term to the integral.
            SUM=SUM+STEP*
     -           ((SIGMA1/V1)**2+4.0*(SIGMAM/VM)**2+(SIGMA2/V2)**2)/6.0
*   Continue with the next segment (if complete) or the next subsegment.
            XPOS1=XPOS2
            YPOS1=YPOS2
            ZPOS1=ZPOS2
            V1=V2
            SIGMA1=SIGMA2
            IF(NSTACK.GT.0)THEN
                 XPOS2=STACK(NSTACK,1)
                 YPOS2=STACK(NSTACK,2)
                 ZPOS2=STACK(NSTACK,5)
                 V2=STACK(NSTACK,3)
                 SIGMA2=STACK(NSTACK,4)
                 NSTACK=NSTACK-1
                 GOTO 20
            ENDIF
       ENDIF
*   Continue with the next segment.
10     CONTINUE
*** Remember: we calculated the square of the diffusion coefficient.
       SIGMA=REAL(SQRT(SUM))
       IF(LDEBUG)THEN
            PRINT *,' ++++++ DLCDF1 DEBUG   : EFIELD calls: ',NFC,
     -           ', deepest stack: ',ISTACK
            PRINT *,'                         Final estimate: ',SIGMA,
     -           ' (crude estimate: ',CRUDE,').'
       ENDIF
       END
