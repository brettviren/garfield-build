CDECK  ID>, DLCTW2.
       SUBROUTINE DLCTW2(FACTOR)
*-----------------------------------------------------------------------
*   DLCTW2 - Routine returning the multiplication factor for the current
*            drift line projected over the locally mean path. The
*            routine uses an adaptive Simpson style integration.
*   VARIABLES : ALFA.      : Townsend coefficients (1,2 end; M middle).
*               ALFINT     : Integral of the Townsend coefficient.
*               FACTOR     : The multiplication factor.
*   (Last changed on  5/ 2/00.)
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
       REAL TWNVEC(MXLIST),ALFA1,ALFA2,ALFAM,FACTOR,EXM,EYM,EZM,ETOTM,
     -      EX,EY,EZ,ETOT,VOLT,GASTWN,DRES,SCALE,BX,BY,BZ,BTOT,
     -      BXM,BYM,BZM,BTOTM
       DOUBLE PRECISION XAUX1,XAUX2,YAUX1,YAUX2,STEP,ALFINT,
     -      XPOS1,XPOS2,XPOSM,YPOS1,YPOS2,YPOSM,ZPOSM,ZPOS1,ZPOS2,
     -      TOTSTP,CRUDE,STACK(MXSTCK,4),VD(3)
       INTEGER LOCVEC(MXLIST),ISTACK,NSTACK,ILOCRS,NFC,ILOCM,IU,ILOC
       EXTERNAL GASTWN
*** Identify the routine
       IF(LIDENT)PRINT *,' /// ROUTINE DLCTW2 ///'
*** Return straight away if there is only one data point.
       IF(NU.LE.1)THEN
            FACTOR=1.0
            RETURN
       ENDIF
*** Obtain a very rough estimate of the result.
       CRUDE=0.0
       SCALE=1
       DO 100 IU=1,NU
       CALL EFIELD(REAL(XU(IU)),REAL(YU(IU)),REAL(ZU(IU)),
     -      EX,EY,EZ,ETOT,VOLT,0,ILOC)
       CALL BFIELD(REAL(XU(IU)),REAL(YU(IU)),REAL(ZU(IU)),
     -      BX,BY,BZ,BTOT)
*   Cheat in case the point is located inside a wire.
       IF(ILOC.GT.0.AND.ILOC.LE.MXWIRE)THEN
            DRES=D(ILOC)
            ILOCRS=ILOC
            D(ILOCRS)=0.0
            CALL EFIELD(REAL(XU(IU)),REAL(YU(IU)),REAL(ZU(IU)),
     -           EX,EY,EZ,ETOT,VOLT,0,ILOC)
            CALL BFIELD(REAL(XU(IU)),REAL(YU(IU)),REAL(ZU(IU)),
     -           BX,BY,BZ,BTOT)
            D(ILOCRS)=DRES
            IF(LDEBUG)PRINT *,' ++++++ DLCTW2 DEBUG   : Drift-line',
     -           ' data point in wire ',ILOCRS,' detected; d=0 fix.'
       ENDIF
*   In case this didn't help, just log the failure.
       LOCVEC(IU)=ILOC
*   Compute projection of the path.
       IF(IU.GT.1)THEN
            CALL DLCVEL((XU(IU-1)+XU(IU))/2,(YU(IU-1)+YU(IU))/2,
     -           (ZU(IU-1)+ZU(IU))/2,VD,QPCHAR,IPTYPE,ILOC)
            IF(((XU(IU)-XU(IU-1))**2+(YU(IU)-YU(IU-1))**2+
     -           (ZU(IU)-ZU(IU-1))**2)*
     -           (VD(1)**2+VD(2)**2+VD(3)**2).LE.0)THEN
                 SCALE=0
            ELSE
                 SCALE=((XU(IU)-XU(IU-1))*VD(1)+
     -                (YU(IU)-YU(IU-1))*VD(2)+(ZU(IU)-ZU(IU-1))*VD(3))/
     -                SQRT(((XU(IU)-XU(IU-1))**2+(YU(IU)-YU(IU-1))**2+
     -                (ZU(IU)-ZU(IU-1))**2)*
     -                (VD(1)**2+VD(2)**2+VD(3)**2))
            ENDIF
C      print *,' Scale = ',scale
C      print *,' x: ',xu(iu-1),xu(iu),vd(1)
C      print *,' y: ',yu(iu-1),yu(iu),vd(2)
C      print *,' z: ',zu(iu-1),zu(iu),vd(3)
       ENDIF
*   Compute Townsend coefficients and step length.
       IF(POLAR)THEN
            TWNVEC(IU)=GASTWN(EX/EXP(REAL(XU(IU))),EY/EXP(REAL(XU(IU))),
     -           EZ,BX,BY,BZ)
            IF(IU.GT.1)THEN
                 CALL CF2RTC(XU(IU),YU(IU),XAUX1,YAUX1,1)
                 CALL CF2RTC(XU(IU-1),YU(IU-1),XAUX2,YAUX2,1)
                 STEP=SQRT((XAUX2-XAUX1)**2+(YAUX2-YAUX1)**2+
     -                (ZU(IU)-ZU(IU-1))**2)
            ENDIF
       ELSE
            TWNVEC(IU)=GASTWN(EX,EY,EZ,BX,BY,BZ)
            IF(IU.GT.1)STEP=SQRT((XU(IU)-XU(IU-1))**2+
     -           (YU(IU)-YU(IU-1))**2+(ZU(IU)-ZU(IU-1))**2)
       ENDIF
       IF(IU.GT.1)CRUDE=CRUDE+STEP*SCALE*(TWNVEC(IU)+TWNVEC(IU-1))/2.0
100    CONTINUE
       NFC=NU
*** Ensure that the crude sum is positive.
       IF(CRUDE.LT.0)THEN
            PRINT *,' !!!!!! DLCTW2 WARNING : Negative Townsend sum'//
     -           ' in 1st order ; multiplication set to 1.'
            FACTOR=1
            RETURN
       ELSEIF(CRUDE.EQ.0)THEN
            FACTOR=1
            RETURN
       ENDIF
*** Print a heading for the debugging output.
       IF(LDEBUG)THEN
            PRINT *,' ++++++ DLCTW2 DEBUG   : Townsend integration',
     -           ' debugging output follows:'
            PRINT *,' '
            PRINT *,'  IU  loc              XU(IU)'//
     -           '              YU(IU)'//
     -           '              ZU(IU)'//
     -           ' number of electrons'
            PRINT *,'                         [cm]'//
     -           '                [cm]'//
     -           '                [cm]'//
     -           '           [numeric]'
            PRINT *,' '
            PRINT '(2(2X,I3),3(5X,E15.8))',1,LOCVEC(1),XU(1),YU(1),ZU(1)
       ENDIF
*** Initialise the sum ALFINT
       ALFINT=0.0
*** Loop over the whole drift-line.
       ISTACK=0
       DO 10 IU=1,NU-1
       IF(LOCVEC(IU).NE.0.OR.LOCVEC(IU+1).NE.0)GOTO 30
*   Initial values for the position.
       XPOS1=XU(IU)
       YPOS1=YU(IU)
       ZPOS1=ZU(IU)
       ALFA1=TWNVEC(IU)
       XPOS2=XU(IU+1)
       YPOS2=YU(IU+1)
       ZPOS2=ZU(IU+1)
       ALFA2=TWNVEC(IU+1)
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
*   Compute projection of the path.
       CALL DLCVEL((XU(IU+1)+XU(IU))/2,(YU(IU+1)+YU(IU))/2,
     -      (ZU(IU+1)+ZU(IU))/2,VD,QPCHAR,IPTYPE,ILOC)
       IF(((XU(IU+1)-XU(IU))**2+(YU(IU+1)-YU(IU))**2+
     -      (ZU(IU+1)-ZU(IU))**2)*
     -      (VD(1)**2+VD(2)**2+VD(3)**2).LE.0)THEN
            SCALE=0
       ELSE
            SCALE=((XU(IU+1)-XU(IU))*VD(1)+
     -           (YU(IU+1)-YU(IU))*VD(2)+(ZU(IU+1)-ZU(IU))*VD(3))/
     -           SQRT(((XU(IU+1)-XU(IU))**2+(YU(IU+1)-YU(IU))**2+
     -           (ZU(IU+1)-ZU(IU))**2)*
     -           (VD(1)**2+VD(2)**2+VD(3)**2))
        ENDIF
C      print *,' Scale = ',scale
*   Return at this point of further refinement is needed.
       NSTACK=0
20     CONTINUE
*   Set the new middle point, to be used for comparison.
       XPOSM=0.5*(XPOS1+XPOS2)
       YPOSM=0.5*(YPOS1+YPOS2)
       ZPOSM=0.5*(ZPOS1+ZPOS2)
*   Compute the field and the Townsend coeff. at the middle point.
       CALL EFIELD(REAL(XPOSM),REAL(YPOSM),REAL(ZPOSM),
     -      EXM,EYM,EZM,ETOTM,VOLT,0,ILOCM)
       CALL BFIELD(REAL(XPOSM),REAL(YPOSM),REAL(ZPOSM),
     -      BXM,BYM,BZM,BTOTM)
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
            NFC=NFC+1
            D(ILOCRS)=DRES
            IF(LDEBUG)PRINT *,' ++++++ DLCTW2 DEBUG   : Intermediate',
     -           ' point in wire ',ILOCRS,' detected; d=0 fix.'
       ENDIF
*   Skip this step in case the ILOC is not due to a wire.
       IF(ILOCM.NE.0)GOTO 30
       IF(POLAR)THEN
            ALFAM=GASTWN(EXM/EXP(REAL(XPOSM)),EYM/EXP(REAL(XPOSM)),EZM,
     -           BXM,BYM,BZM)
       ELSE
            ALFAM=GASTWN(EXM,EYM,EZM,BXM,BYM,BZM)
       ENDIF
*   Compare first and second order estimates, divide if too large.
       IF(NSTACK.LT.MIN(MXSTCK,MXTWNS).AND.EPSTWI*CRUDE.LT.
     -      TOTSTP*ABS(ALFA1-2.0*ALFAM+ALFA2)/3.0)THEN
            NSTACK=NSTACK+1
            ISTACK=MAX(ISTACK,NSTACK)
            STACK(NSTACK,1)=XPOS2
            STACK(NSTACK,2)=YPOS2
            STACK(NSTACK,3)=ZPOS2
            STACK(NSTACK,4)=ALFA2
            XPOS2=XPOSM
            YPOS2=YPOSM
            ZPOS2=ZPOSM
            ALFA2=ALFAM
            GOTO 20
*   No further subdevision is required, transform polar coordinates.
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
            ALFINT=ALFINT+STEP*SCALE*(ALFA1+4.0*ALFAM+ALFA2)/6.0
*   Continue with the next segment (if complete) or the next subsegment.
            XPOS1=XPOS2
            YPOS1=YPOS2
            ZPOS1=ZPOS2
            ALFA1=ALFA2
            IF(NSTACK.GT.0)THEN
                 XPOS2=STACK(NSTACK,1)
                 YPOS2=STACK(NSTACK,2)
                 ZPOS2=STACK(NSTACK,3)
                 ALFA2=STACK(NSTACK,4)
                 NSTACK=NSTACK-1
                 GOTO 20
            ENDIF
       ENDIF
*   Continue with the next segment.
30     CONTINUE
*   Print some debugging output.
       IF(LDEBUG)PRINT '(2I5,4(5X,E15.8))',IU+1,LOCVEC(IU+1),XU(IU+1),
     -      YU(IU+1),ZU(IU+1),EXP(MIN(50.0D0,ALFINT))
10     CONTINUE
*** Finally take the exponential.
       IF(ALFINT.LT.0.0)THEN
            FACTOR=1.0
       ELSEIF(ALFINT.LT.46.0)THEN
            FACTOR=EXP(ALFINT)
       ELSE
            PRINT *,' !!!!!! DLCTW2 WARNING : The Townsend coefficient',
     -           ' can not be integrated without overflow; set to 1E20.'
            FACTOR=1.0E20
       ENDIF
       IF(LDEBUG)THEN
            PRINT *,' ++++++ DLCTW2 DEBUG   : EFIELD calls: ',NFC,
     -           ', deepest stack: ',ISTACK
            PRINT *,'                         Final log estimate: ',
     -           ALFINT,' (crude estimate: ',CRUDE,').'
       ENDIF
       END
