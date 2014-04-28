CDECK  ID>, DLCDF2.
       SUBROUTINE DLCDF2(DIFF)
*-----------------------------------------------------------------------
*   DLCDF2 - Integrates both transverse and longitudinal diffusion over
*            the current drift line.
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       DOUBLE PRECISION COV(2,2),SUM(2,2),STEP,C,S,F1(3),F2(3),F3(3),
     -      F4(3),SCL,SCT,EPS,TEMP,DL,DT,RHO2,SIZE,SLAST,SNOW,VLAST,
     -      VNOW,SIGMA
C       double precision fl(3),templ
       REAL GASDFL,GASDFT,XWIRE,YWIRE,DWIRE,EX1,EY1,EZ1,E1,
     -      BX1,BY1,BZ1,BTOT1,BX,BY,BZ,BTOT,DIFF,
     -      EX2,EY2,EZ2,E2,BX2,BY2,BZ2,B2,EX,EY,EZ,ETOT,VOLT
       INTEGER ILOC,ILOC1,ILOC2,IWIRE,ILAST,I,IFAIL
       EXTERNAL GASDFL,GASDFT
*** Identify the routine
       IF(LIDENT)PRINT *,' /// ROUTINE DLCDF2 ///'
       IF(LDEBUG)PRINT *,' ++++++ DLCDF2 DEBUG   : Starting to sum'//
     -      ' L&T diffusion, NU=',NU,' ISTAT=',ISTAT
*** Assume the routine will fail.
       IFAIL=1
*** Initialise some variables.
       DIFF=0
       TEMP=0
       SIZE=0
       ILAST=1
       F2(1)=0
       F2(2)=0
       F2(3)=0
*** Verify that there are some steps.
       IF(NU.LT.2)THEN
            IF(LDEBUG)PRINT *,' ++++++ DLCDF2 DEBUG   :'//
     -           ' The drift line has no steps ; diffusion=0.'
            RETURN
       ENDIF
*** Initialise the covariance matrix.
       SUM(1,1)=0
       SUM(1,2)=0
       SUM(2,1)=0
       SUM(2,2)=0
*** Initialise the various quantities that are shifted through.
       CALL EFIELD(REAL(XU(1)),REAL(YU(1)),REAL(ZU(1)),
     -      EX1,EY1,EZ1,E1,VOLT,0,ILOC1)
       CALL BFIELD(REAL(XU(1)),REAL(YU(1)),REAL(ZU(1)),
     -      BX1,BY1,BZ1,BTOT1)
       CALL DLCVEL(XU(1),YU(1),ZU(1),F1,-1.0,1,ILOC2)
       IF(ILOC1.NE.0.OR.ILOC2.NE.0)THEN
            PRINT *,' !!!!!! DLCDF2 WARNING : Initial point on drift'//
     -           ' line has unusual location codes ',ILOC1,ILOC2
            RETURN
       ENDIF
*** Set the radius to zero temporarily for a drift line going to a wire.
       IF(ISTAT.GE.1.AND.ISTAT.LE.MXWIRE+NWIRE)THEN
*   Obtain the wire number.
            IF(ISTAT.GT.MXWIRE)THEN
                 IWIRE=ISTAT-MXWIRE
            ELSE
                 IWIRE=ISTAT
            ENDIF
*   Store the wire diameter and set temporarily to zero.
            DWIRE=D(IWIRE)
            D(IWIRE)=0.0
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCDF2 DEBUG   :'',
     -           '' Temporarily setting the diameter of wire '',I4,
     -           '' to 0.'')') IWIRE
*   Locate the nearest replica of the wire. at the end point.
            XWIRE=X(IWIRE)
            IF(PERX)XWIRE=XWIRE-SX*ANINT((XWIRE-XU(NU))/SX)
            YWIRE=Y(IWIRE)
            IF(PERY)YWIRE=YWIRE-SY*ANINT((YWIRE-YU(NU))/SY)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCDF2 DEBUG   :'',
     -           '' Wire replica nearest to end point: ('',E15.8,'','',
     -           E15.8,'').'')') XWIRE,YWIRE
       ELSE
            IWIRE=0
            DWIRE=0
            XWIRE=0
            YWIRE=0
       ENDIF
*** Loop over the steps
C       templ=0.0
C       call efield(real(xu(1)),real(yu(1)),real(zu(1)),
C     -      ex,ey,ez,etot,volt,0,iloc)
C       call bfield(real(xu(1)),real(yu(1)),real(zu(1)),
C     -      bx,by,bz,btot)
C       call dlcvel(xu(1),yu(1),zu(1),fl,-1.0,1,iloc2)
C       vlast=sqrt(fl(1)**2+fl(2)**2+fl(3)**2)
C       slast=gasdfl(ex,ey,ez,bx,by,bz)
       DO 10 I=1,NU-1
*   Get pure longitudinal diffusion.
C       call efield(real(xu(i+1)),real(yu(i+1)),real(zu(i+1)),
C     -      ex,ey,ez,etot,volt,0,iloc)
C       call bfield(real(xu(i+1)),real(yu(i+1)),real(zu(i+1)),
C     -      bx,by,bz,btot)
C       call dlcvel(xu(i+1),yu(i+1),zu(i+1),fl,-1.0,1,iloc2)
C       vnow=sqrt(fl(1)**2+fl(2)**2+fl(3)**2)
C       snow=gasdfl(ex,ey,ez,bx,by,bz)
C       step=sqrt((xu(i+1)-xu(i))**2+(yu(i+1)-yu(i))**2+
C     -      (zu(i+1)-zu(i))**2)
C       templ=templ+step*((snow/vnow)**2+(slast/vlast)**2)/2
C       vlast=vnow
C       slast=snow
*   Stop this integration if the cloud is less than n radii from a wire.
       IF(IWIRE.GT.0.AND.MDF2.NE.0.AND.RDF2*SIZE.GT.MAX(0.0D0,
     -      SQRT((XU(I+1)-XWIRE)**2+(YU(I+1)-YWIRE)**2)-DWIRE/2))THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCDF2 DEBUG   :'',
     -           '' n * Size > Distance at IU='',I3,''/'',I3/25X,
     -           '' Size =     '',E15.8,'' [cm],''/25X,
     -           '' Distance = '',E15.8,'' [cm].'')')
     -           I+1,NU,SIZE,SQRT((XU(I+1)-XWIRE)**2+(YU(I+1)-YWIRE)**2)
            GOTO 20
       ENDIF
*   Length and orientation of the step.
       STEP=SQRT((XU(I+1)-XU(I))**2+(YU(I+1)-YU(I))**2+
     -      (ZU(I+1)-ZU(I))**2)
       IF(STEP.LE.0.0.OR.STEP.LE.1.0E-6*DWIRE)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCDF2 DEBUG   :'',
     -           '' Skipping step '',I3,'' of length '',E15.8)')
     -           I,STEP
            GOTO 10
       ENDIF
       C=(XU(I+1)-XU(I))/STEP
       S=(YU(I+1)-YU(I))/STEP
*   Transverse diffusion scaling factor.
       EPS=1.0E-3*(1+ABS(XU(I))+ABS(YU(I)))
       CALL DLCVEL(XU(I)-S*EPS,YU(I)+C*EPS,ZU(I),F3,-1.0,1,ILOC1)
       CALL DLCVEL(XU(I)+S*EPS,YU(I)-C*EPS,ZU(I),F4,-1.0,1,ILOC2)
       IF(ILOC1.NE.0.OR.ILOC2.NE.0)THEN
            PRINT *,' !!!!!! DLCDF2 WARNING : Unable to compute'//
     -           ' transverse scaling factor; set to 1.'
            SCT=1.0
       ELSE
            SCT=1+(TU(I+1)-TU(I))*(-S*F3(1)+C*F3(2)+S*F4(1)-C*F4(2))/
     -           (2*EPS)
       ENDIF
       IF(SCT.LE.0)THEN
            PRINT *,' !!!!!! DLCDF2 WARNING : Transverse scaling'//
     -           ' factor < 0 detected; set to 1.'
            SCT=1.0
       ENDIF
*   Longitudinal scaling factor.
       CALL DLCVEL(XU(I+1),YU(I+1),ZU(I+1),F2,-1.0,1,ILOC2)
       IF(ILOC2.NE.0)THEN
            PRINT *,' !!!!!! DLCDF2 WARNING : Final point has unusual'//
     -           ' location code; summing terminated.'
            GOTO 20
       ELSEIF(SQRT(F1(1)**2+F1(2)**2+F1(3)**2).EQ.0.OR.
     -      SQRT(F2(1)**2+F2(2)**2+F2(3)**2).EQ.0)THEN
            PRINT *,' !!!!!! DLCDF2 WARNING : Longitudinal velocity'//
     -           ' of 0 detected.'
            SCL=1.0
       ELSE
            SCL=SQRT(F2(1)**2+F2(2)**2+F2(3)**2)/
     -           SQRT(F1(1)**2+F1(2)**2+F1(3)**2)
       ENDIF
*   Compute the field at the end point in view of getting diffusions.
       CALL EFIELD(REAL(XU(I+1)),REAL(YU(I+1)),REAL(ZU(I+1)),
     -      EX2,EY2,EZ2,E2,VOLT,0,ILOC)
       CALL BFIELD(REAL(XU(I+1)),REAL(YU(I+1)),REAL(ZU(I+1)),
     -      BX2,BY2,BZ2,B2)
       IF(ILOC.NE.0)THEN
            IF(I.EQ.NU-1)THEN
                 GOTO 20
            ELSE
                 PRINT *,' !!!!!! DLCDF2 WARNING : Intermediate point'//
     -                ' has unusual location code ',ILOC
                 GOTO 20
            ENDIF
       ENDIF
*   Obtain longitudinal and transverse diffusion at this step.
       DL=SQRT(0.5*(GASDFL(EX1,EY1,EZ1,BX1,BY1,BZ1)**2+
     -      (GASDFL(EX2,EY2,EZ2,BX2,BY2,BZ2)/SCL)**2))
       DT=(GASDFT(EX1,EY1,EZ1,BX1,BY1,BZ1)+
     -      GASDFT(EX2,EY2,EZ2,BX2,BY2,BZ2))/2
*   Compensate diffusion for step length.
       DL=DL*SQRT(STEP)
       DT=DT*SQRT(STEP)
*   Add this step to the sum.
       SUM(1,1)=SUM(1,1)+C**2*DL**2+S**2*DT**2
       SUM(1,2)=SUM(1,2)+C*S*(DT**2-DL**2)
       SUM(2,1)=SUM(2,1)+C*S*(DT**2-DL**2)
       SUM(2,2)=SUM(2,2)+C**2*DT**2+S**2*DL**2
*   Align with the drift line, rotating inverted matrix.
       COV(1,1)=C**2*SUM(1,1)-C*S*SUM(1,2)-C*S*SUM(2,1)+S**2*SUM(2,2)
       COV(1,2)=C**2*SUM(2,1)-C*S*SUM(2,2)+C*S*SUM(1,1)-S**2*SUM(1,2)
       COV(2,1)=C**2*SUM(1,2)-C*S*SUM(2,2)+C*S*SUM(1,1)-S**2*SUM(2,1)
       COV(2,2)=C**2*SUM(2,2)+C*S*SUM(1,2)+C*S*SUM(2,1)+S**2*SUM(1,1)
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCDF2 DEBUG   :'',
     -      '' Transverse scaling:   '',E15.8/
     -      26X,''Longitudinal scaling: '',E15.8)') SCT,SCL
*   Update the covariance matrix.
       COV(1,1)=COV(1,1)*SCL**2
       COV(1,2)=COV(1,2)*SCL*SCT
       COV(2,1)=COV(2,1)*SCT*SCL
       COV(2,2)=COV(2,2)*SCT**2
*   Evaluate the correlation coefficient.
       IF(COV(1,1)*COV(2,2).LT.COV(1,2)*COV(2,1))THEN
            RHO2=1.0
            PRINT *,' !!!!!! DLCDF2 WARNING : Correlation > 1 ; set'//
     -           ' to 1.'
       ELSEIF(COV(1,1)*COV(2,2).EQ.0)THEN
            RHO2=0.0
       ELSE
            RHO2=(COV(1,2)*COV(2,1))/(COV(1,1)*COV(2,2))
       ENDIF
*   Keep continuously track of longitudinal component.
       TEMP=COV(1,1)*(1-RHO2)/(F2(1)**2+F2(2)**2+F2(3)**2)
*   Realign the matrix with the coordinate system.
       SUM(1,1)=C**2*COV(1,1)+C*S*COV(1,2)+C*S*COV(2,1)+S**2*COV(2,2)
       SUM(1,2)=C**2*COV(2,1)+C*S*COV(2,2)-C*S*COV(1,1)-S**2*COV(1,2)
       SUM(2,1)=C**2*COV(1,2)+C*S*COV(2,2)-C*S*COV(1,1)-S**2*COV(2,1)
       SUM(2,2)=C**2*COV(2,2)-C*S*COV(1,2)-C*S*COV(2,1)+S**2*COV(1,1)
*   And monitor the size of the cloud.
       SIZE=SQRT(MAX(0.0D0,COV(1,1)*(1-RHO2),COV(2,2)*(1-RHO2)))
*   Debugging output:
C       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCDF2 DEBUG   : Matrix'',
C     -      '' at (x,y,z)='',3(E12.5,1X),'' step '',I3,'':''/
C     -      26X,2E15.8/26X,2E15.8/
C     -      26X,''Longitudinal size = '',E15.8,'' [cm]''/
C     -      26X,''Transverse size   = '',E15.8,'' [cm]''/
C     -      26X,''Correlation       = '',E15.8/
C     -      26X,''Start speed       = '',E15.8,'' [cm/microsec]''/
C     -      26X,''End speed         = '',E15.8,'' [cm/microsec]''/
C     -      26X,''Diffusion L + T   = '',E15.8,'' [microsec]''/
C     -      26X,''Diffusion L only  = '',E15.8,'' [microsec]'')')
C     -      XU(I),YU(I),ZU(I),I,SUM(1,1),SUM(1,2),SUM(2,1),SUM(2,2),
C     -      SQRT(MAX(0.0D0,COV(1,1)*(1-RHO2))),
C     -      SQRT(MAX(0.0D0,COV(2,2)*(1-RHO2))),
C     -      SQRT(RHO2),SQRT(F1(1)**2+F1(2)**2+F1(3)**2),
C     -      SQRT(F2(1)**2+F2(2)**2+F2(3)**2),SQRT(TEMP),SQRT(TEMPL)
*   Shift some parameters for next iteration.
       EX1=EX2
       EY1=EY2
       EZ1=EZ2
       F1(1)=F2(1)
       F1(2)=F2(2)
       F1(3)=F2(3)
*   Remember that we carried this step out.
       ILAST=I+1
10     CONTINUE
*** Continue here in case of aborted integration.
20     CONTINUE
*** Drift line hits the wire, first no treatment (longitudinal).
       IF(IWIRE.GT.0.AND.(MDF2.EQ.0.OR.MDF2.EQ.3))THEN
*   Restore wire diameter.
            D(IWIRE)=DWIRE
**  Integration over the cloud, either full or with constant velocity.
       ELSEIF(IWIRE.GT.0.AND.(MDF2.EQ.1.OR.MDF2.EQ.2))THEN
*   Output estimate sofar.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCDF2 DEBUG   :'',
     -           '' Diffusion estimates before wire stepping''/25X,
     -           '' Longitudinal component only: '',E15.8,
     -           '' [microsec]'')') SQRT(MAX(0.0D0,TEMP))
*   Estimate the spread in distances from the cloud.
            CALL DLCDIW(SUM,XU(ILAST),YU(ILAST),ZU(ILAST),
     -           XWIRE,YWIRE,DWIRE,SIGMA,IFAIL)
            TEMP=SIGMA**2
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCDF2 DEBUG   :'',
     -           '' Diffusion estimates during wire stepping''/25X,
     -           '' Standard deviation cloud size: '',E15.8,
     -           '' [microsec]'')') SQRT(MAX(0.0D0,TEMP))
*   Add the purely longitudinal term for the last step.
            CALL EFIELD(REAL(XU(ILAST)),REAL(YU(ILAST)),REAL(ZU(ILAST)),
     -           EX,EY,EZ,ETOT,VOLT,0,ILOC)
            CALL BFIELD(REAL(XU(ILAST)),REAL(YU(ILAST)),REAL(ZU(ILAST)),
     -           BX,BY,BZ,BTOT)
            CALL DLCVEL(XU(ILAST),YU(ILAST),ZU(ILAST),F1,-1.0,1,ILOC1)
            VLAST=SQRT(MAX(0.0D0,F1(1)**2+F1(2)**2+F1(3)**2))
            SLAST=GASDFL(EX,EY,EZ,BX,BY,BZ)
            DO 30 I=ILAST,NU-1
            CALL EFIELD(REAL(XU(I+1)),REAL(YU(I+1)),REAL(ZU(I+1)),
     -           EX,EY,EZ,ETOT,VOLT,0,ILOC)
            CALL BFIELD(REAL(XU(I+1)),REAL(YU(I+1)),REAL(ZU(I+1)),
     -           BX,BY,BZ,BTOT)
            CALL DLCVEL(XU(I+1),YU(I+1),ZU(I+1),F2,-1.0,1,ILOC2)
            VNOW=SQRT(MAX(0.0D0,F2(1)**2+F2(2)**2+F2(3)**2))
            SNOW=GASDFL(EX,EY,EZ,BX,BY,BZ)
            STEP=SQRT((XU(I+1)-XU(I))**2+(YU(I+1)-YU(I))**2+
     -           (ZU(I+1)-ZU(I))**2)
            IF(VNOW.GT.0.AND.VLAST.GT.0)
     -           TEMP=TEMP+STEP*((SNOW/VNOW)**2+(SLAST/VLAST)**2)/2
            VLAST=VNOW
            SLAST=SNOW
30          CONTINUE
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCDF2 DEBUG   :'',
     -           '' Diffusion estimates after wire stepping''/25X,
     -           '' Including long diff last step: '',E15.8,
     -           '' [microsec]'')') SQRT(MAX(0.0D0,TEMP))
*   Restore the wire diameter.
            D(IWIRE)=DWIRE
**  Take the largest axis (useful if there is a B field).
       ELSEIF(IWIRE.GT.0.AND.MDF2.EQ.4)THEN
*   Output estimate sofar.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCDF2 DEBUG   :'',
     -           '' Diffusion estimate from longitudinal''/25X,
     -           '' component only: '',E15.8,
     -           '' [microsec]'')') SQRT(MAX(0.0D0,TEMP))
*   Compute largest dimension of the cloud, first align the cloud.
            IF((SUM(2,2)-SUM(1,1))**2+(SUM(1,2)+SUM(2,1))**2.GT.0)THEN
                 C=SQRT(0.5*(1+(SUM(2,2)-SUM(1,1))/
     -                SQRT((SUM(2,2)-SUM(1,1))**2+
     -                (SUM(1,2)+SUM(2,1))**2)))
                 S=SIGN(SQRT(1-C**2),SUM(1,2)+SUM(2,1))
            ELSE
                 C=1
                 S=0
            ENDIF
*   Determine maximum cloud cross section.
            SIZE=MAX(SQRT(MAX(0.0D0,C**2*SUM(1,1)-C*S*SUM(1,2)-
     -           C*S*SUM(2,1)+S**2*SUM(2,2))),
     -           SQRT(MAX(0.0D0,C**2*SUM(2,2)+C*S*SUM(1,2)+
     -           C*S*SUM(2,1)+S**2*SUM(1,1))))
*   Compute the drift velocity at the last point.
            CALL EFIELD(REAL(XU(ILAST)),REAL(YU(ILAST)),REAL(ZU(ILAST)),
     -           EX,EY,EZ,ETOT,VOLT,0,ILOC)
            CALL BFIELD(REAL(XU(ILAST)),REAL(YU(ILAST)),REAL(ZU(ILAST)),
     -           BX,BY,BZ,BTOT)
            CALL DLCVEL(XU(ILAST),YU(ILAST),ZU(ILAST),F1,-1.0,1,ILOC1)
            VLAST=SQRT(MAX(0.0D0,F1(1)**2+F1(2)**2+F1(3)**2))
            SLAST=GASDFL(EX,EY,EZ,BX,BY,BZ)
*   Compensate size for speed.
            IF(VLAST.LE.0.0)THEN
                 PRINT *,' !!!!!! DLCDF2 WARNING : End point speed'//
     -                ' before wire stepping zero; diffusion=0.'
                 TEMP=0
            ELSE
                 TEMP=(SIZE/VLAST)**2
            ENDIF
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCDF2 DEBUG   :'',
     -           '' Diffusion estimates from largest cloud''/25X,
     -           '' dimension: '',E15.8,'' [microsec]'')')
     -           SQRT(MAX(0.0D0,TEMP))
*   Add the purely longitudinal term for the last step.
            DO 40 I=ILAST,NU-1
            CALL EFIELD(REAL(XU(I+1)),REAL(YU(I+1)),REAL(ZU(I+1)),
     -           EX,EY,EZ,ETOT,VOLT,0,ILOC)
            CALL BFIELD(REAL(XU(I+1)),REAL(YU(I+1)),REAL(ZU(I+1)),
     -           BX,BY,BZ,BTOT)
            CALL DLCVEL(XU(I+1),YU(I+1),ZU(I+1),F2,-1.0,1,ILOC2)
            VNOW=SQRT(MAX(0.0D0,F2(1)**2+F2(2)**2+F2(3)**2))
            SNOW=GASDFL(EX,EY,EZ,BX,BY,BZ)
            STEP=SQRT((XU(I+1)-XU(I))**2+(YU(I+1)-YU(I))**2+
     -           (ZU(I+1)-ZU(I))**2)
            IF(VNOW.GT.0.AND.VLAST.GT.0)
     -           TEMP=TEMP+STEP*((SNOW/VNOW)**2+(SLAST/VLAST)**2)/2
            VLAST=VNOW
            SLAST=SNOW
40          CONTINUE
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCDF2 DEBUG   :'',
     -           '' Diffusion estimates after wire stepping''/25X,
     -           '' Including long diff last step: '',E15.8,
     -           '' [microsec]'')') SQRT(MAX(0.0D0,TEMP))
*   Restore the wire diameter.
            D(IWIRE)=DWIRE
**  Other termination codes, not valid.
       ELSEIF(IWIRE.GT.0)THEN
*   Issue warning.
            PRINT *,' !!!!!! DLCDF2 WARNING : Unknown integration'//
     -           ' code (',MDF2,') received; program bug.'
*   Restore wire diameter.
            D(IWIRE)=DWIRE
       ENDIF
*** Integration done, retrieve the result we accumulated.
       IF(TEMP.LT.0.0)THEN
            PRINT *,' !!!!!! DLCDF2 WARNING : Final longitudinal'//
     -           ' component < 0 ; diffusion=0.'
            RETURN
       ENDIF
       DIFF=SQRT(TEMP)
*** Things seem to have worked.
       IFAIL=0
       END
