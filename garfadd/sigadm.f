CDECK  ID>, SIGADM.
       SUBROUTINE SIGADM(TSHIFT,CROSS,WEIGHT,IFAIL)
*-----------------------------------------------------------------------
*   SIGADM - Adds the signals induced by the current microscopic
*            tracking drift line.
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
       LOGICAL FPERX,FPERY,LCROSS,TRASET,TRAFLG,LITAIL,LDTAIL,LRTAIL,
     -      LEPULS,LIPULS,SIGSET,RESSET
       INTEGER NPAIR,ICLUST,NFOUR,MFEXP,MXMIN,MXMAX,
     -      MYMIN,MYMAX,NTRBNK,ITRMAJ,NTIME,NORIA,
     -      NASIMP,JIORD,NISIMP,NMQUAD,NCANG,IENANG
       REAL TIMSIG,SIGNAL,TCLUST,SCLUST,ACLUST,BCLUST,FCLUST,
     -      AVALAN,TSTART,TDEV,PRSTHR,
     -      TRABNK,TRAVEC
       CHARACTER*(MXCHAR) FCNANG
       CHARACTER*12 AVATYP
       CHARACTER*3 FCELTP
       COMMON /SIGDAT/ TIMSIG(MXLIST),SIGNAL(MXLIST,MXSW,2),
     -      AVALAN(2),TRAVEC(MXLIST),
     -      TRABNK(MXLIST,9),TSTART,TDEV,PRSTHR,
     -      TCLUST,SCLUST,ACLUST,BCLUST,FCLUST,ICLUST,NPAIR,
     -      NFOUR,ITRMAJ,JIORD,NISIMP,NMQUAD,IENANG,NTIME,NORIA,
     -      MFEXP,MXMIN,MXMAX,MYMIN,MYMAX,NTRBNK,NASIMP,NCANG,
     -      TRASET,TRAFLG(9),FPERX,FPERY,LCROSS,LITAIL,LDTAIL,LRTAIL,
     -      LEPULS,LIPULS,SIGSET,RESSET
       COMMON /SIGCHR/ FCELTP,AVATYP,FCNANG
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
       LOGICAL CROSS
       INTEGER ISW,JSW,IU,ILOCRS,IFAIL,IFAIL1,I,J
       REAL EX,EY,EZ,DRES,TSHIFT,WEIGHT,EXT,EYT,EZT
       DOUBLE PRECISION VDRIFT(3),TIME(MXLIST),SIG(MXLIST),SUM,
     -      WG6(6),TG6(6)
       SAVE TG6,WG6
*** Locations and weights for 6-point Gaussian integration.
       DATA (TG6(I),WG6(I),I=1,6) /
     -      -0.93246 95142 03152 028D0, 0.17132 44923 79170 345D0,
     -      -0.66120 93864 66264 514D0, 0.36076 15730 48138 608D0,
     -      -0.23861 91860 83196 909D0, 0.46791 39345 72691 047D0,
     -       0.23861 91860 83196 909D0, 0.46791 39345 72691 047D0,
     -       0.66120 93864 66264 514D0, 0.36076 15730 48138 608D0,
     -       0.93246 95142 03152 028D0, 0.17132 44923 79170 345D0/
*** Identification.
       IF(LIDENT)PRINT *,' /// ROUTINE SIGADM ///'
*** Assume the procedure will fail.
       IFAIL=1
*** Ensure there is a drift line.
       IF(ISTAT.EQ.0)THEN
            PRINT *,' !!!!!! SIGADM WARNING : The current drift'//
     -           ' line has not yet ended; no signals computed.'
            RETURN
       ELSEIF(IPTYPE.NE.1)THEN
            PRINT *,' !!!!!! SIGADM WARNING : Current drift line is'//
     -           ' not for an e-; no signals computed.'
            RETURN
       ELSEIF(IPTECH.NE.4)THEN
            PRINT *,' !!!!!! SIGADM WARNING : Current drift line is'//
     -           ' not microscopically tracked; no signals computed.'
            RETURN
       ELSEIF(ABS(QPCHAR).LT.0.1)THEN
            PRINT *,' !!!!!! SIGADM WARNING : Current drift line is'//
     -           ' for an uncharged particle; no signals computed.'
            RETURN
       ENDIF
*** Make sure the time resolution has been set.
       IF(.NOT.RESSET)THEN
            PRINT *,' !!!!!! SIGADM WARNING : The time resolution has'//
     -           ' not yet been set; no signals computed.'
            RETURN
       ENDIF
*** Obtain the sense wire number.
       CALL DLCISW(ISTAT,ISW)
*   Cheat in case the point is located inside a wire.
       IF(ISTAT.GT.0)THEN
            ILOCRS=MOD(ISTAT,MXWIRE)
            DRES=D(ILOCRS)
       ELSE
            ILOCRS=0
            DRES=0
       ENDIF
       IF(ILOCRS.GT.0)D(ILOCRS)=DRES/2
**  Cross induction: loop over all sense wires.
       IF(CROSS)THEN
*   Loop over the read-out electrodes.
            DO 10 JSW=1,NSW
*   Compute contribution of the current drift line to the signal.
            SUM=0
            DO 20 IU=1,NU-1
            IF(NMQUAD.EQ.0)THEN
                 CALL SIGFLS(REAL(XU(IU)+XU(IU+1))/2.0,
     -                REAL(YU(IU)+YU(IU+1))/2.0,
     -                REAL(ZU(IU)+ZU(IU+1))/2.0,
     -                EX,EY,EZ,JSW)
            ELSEIF(NMQUAD.EQ.1)THEN
                 EX=0
                 EY=0
                 EZ=0
                 DO 40 J=1,6
                 CALL SIGFLS(
     -                REAL(XU(IU)+(1+TG6(J))/2*(XU(IU+1)-XU(IU))),
     -                REAL(YU(IU)+(1+TG6(J))/2*(YU(IU+1)-YU(IU))),
     -                REAL(ZU(IU)+(1+TG6(J))/2*(ZU(IU+1)-ZU(IU))),
     -                EXT,EYT,EZT,JSW)
                 EX=EX+EXT*WG6(J)/2
                 EY=EY+EYT*WG6(J)/2
                 EZ=EZ+EZT*WG6(J)/2
40               CONTINUE
            ELSE
                 PRINT *,' !!!!!! SIGADM WARNING : Unknown quadrature'//
     -                ' type ',NMQUAD,' requested; Ew set to 0.'
                 EX=0
                 EY=0
                 EZ=0
            ENDIF
            IF(TU(IU+1).GT.TU(IU))THEN
                 VDRIFT(1)=(XU(IU+1)-XU(IU))/(TU(IU+1)-TU(IU))
                 VDRIFT(2)=(YU(IU+1)-YU(IU))/(TU(IU+1)-TU(IU))
                 VDRIFT(3)=(ZU(IU+1)-ZU(IU))/(TU(IU+1)-TU(IU))
                 SIG(IU)=VDRIFT(1)*EX+VDRIFT(2)*EY+VDRIFT(3)*EZ
            ELSE
                 SIG(IU)=0
C                 PRINT *,' !!!!!! SIGADM WARNING : Zero interval'//
C     -                ' at step ',IU,'/',NU,'; skipped.'
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGADM WARNING :'',
     -                '' IU/NU = '',I5,''/'',I5,'', dt = '',E12.5,
     -                '', Ew = '',3E12.5)') IU,NU,TU(IU+1)-TU(IU),
     -                EX,EY,EZ
            ENDIF
            TIME(IU)=TU(IU)
            SUM=SUM+SIG(IU)*(TU(IU+1)-TU(IU))
20          CONTINUE
            TIME(NU)=TU(NU)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGADM DEBUG   :'',
     -           '' Group '',I5,'' sum = '',E15.8)') JSW,SUM
*   Add this current to the total.
            CALL SIGADD(JSW,ISW.NE.JSW,NU-1,TIME,SIG,
     -           QPCHAR*WEIGHT,0.0,TSHIFT,IFAIL1)
            IF(IFAIL1.NE.0)PRINT *,' !!!!!! SIGADM WARNING : Adding'//
     -           ' the computed signal failed; signal incomplete.'
*   Finish loop over the sense wires,
10          CONTINUE
*   Make sure we will know cross induced signals have been computed.
            LCROSS=.TRUE.
**  Otherwise do not do the loop.
       ELSEIF(ISW.NE.0)THEN
*   Compute contribution of the current drift line to the signal
            SUM=0
            DO 30 IU=1,NU-1
            IF(NMQUAD.EQ.0)THEN
                 CALL SIGFLS(REAL(XU(IU)+XU(IU+1))/2.0,
     -                REAL(YU(IU)+YU(IU+1))/2.0,
     -                REAL(ZU(IU)+ZU(IU+1))/2.0,
     -                EX,EY,EZ,ISW)
            ELSEIF(NMQUAD.EQ.1)THEN
                 EX=0
                 EY=0
                 EZ=0
                 DO 50 J=1,6
                 CALL SIGFLS(
     -                REAL(XU(IU)+(1+TG6(J))/2*(XU(IU+1)-XU(IU))),
     -                REAL(YU(IU)+(1+TG6(J))/2*(YU(IU+1)-YU(IU))),
     -                REAL(ZU(IU)+(1+TG6(J))/2*(ZU(IU+1)-ZU(IU))),
     -                EXT,EYT,EZT,ISW)
                 EX=EX+EXT*WG6(J)/2
                 EY=EY+EYT*WG6(J)/2
                 EZ=EZ+EZT*WG6(J)/2
50               CONTINUE
            ELSE
                 PRINT *,' !!!!!! SIGADM WARNING : Unknown quadrature'//
     -                ' type ',NMQUAD,' requested; Ew set to 0.'
                 EX=0
                 EY=0
                 EZ=0
            ENDIF
            IF(TU(IU+1).GT.TU(IU))THEN
                 VDRIFT(1)=(XU(IU+1)-XU(IU))/(TU(IU+1)-TU(IU))
                 VDRIFT(2)=(YU(IU+1)-YU(IU))/(TU(IU+1)-TU(IU))
                 VDRIFT(3)=(ZU(IU+1)-ZU(IU))/(TU(IU+1)-TU(IU))
                 SIG(IU)=VDRIFT(1)*EX+VDRIFT(2)*EY+VDRIFT(3)*EZ
            ELSE
                 SIG(IU)=0
C                 PRINT *,' !!!!!! SIGADM WARNING : Zero interval'//
C     -                ' at step ',IU,'/',NU,'; skipped.'
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGADM WARNING :'',
     -                '' IU/NU = '',I5,''/'',I5,'', dt = '',E12.5,
     -                '', Ew = '',3E12.5)') IU,NU,TU(IU+1)-TU(IU),
     -                EX,EY,EZ
            ENDIF
            TIME(IU)=TU(IU)
            SUM=SUM+SIG(IU)*(TU(IU+1)-TU(IU))
30          CONTINUE
            TIME(NU)=TU(NU)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGADM DEBUG   '',
     -           '' Group '',I5,'' sum = '',E12.5)') ISW,SUM
*   Add this current to the total.
            CALL SIGADD(ISW,.FALSE.,NU-1,TIME,SIG,
     -           QPCHAR*WEIGHT,0.0,TSHIFT,IFAIL1)
            IF(IFAIL1.NE.0)PRINT *,' !!!!!! SIGADM WARNING : Adding'//
     -           ' the computed signal failed; signal incomplete.'
*   Make sure we will know cross induced signals have not been computed.
            LCROSS=.FALSE.
       ENDIF
*** Restore the wire diameter.
       IF(ILOCRS.GT.0)D(ILOCRS)=DRES
*** Things seem to have worked.
       IFAIL=0
       END
