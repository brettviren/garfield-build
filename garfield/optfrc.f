CDECK  ID>, OPTFRC.
       SUBROUTINE OPTFRC
*-----------------------------------------------------------------------
*   OPTFRC - Studies the electrostatic forces on a wire.
*   (Last changed on 21/10/00.)
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
       DOUBLE PRECISION FX(MXGRID,MXGRID),FY(MXGRID,MXGRID),
     -      XSCAN(MXGRID),YSCAN(MXGRID),EPS,EPSX,EPSF,STEP
       REAL XORIG(MXWIRE),YORIG(MXWIRE),XOFF(MXWIRE),YOFF(MXWIRE)
       INTEGER NITMAX,NSHOT,NSTEP,IW,NSCANX,NSCANY,JSORD,NFITER
       LOGICAL LFGRAV,LFELEC,LFEXTR,LFWARN,LZROPR,LFITER
       COMMON /SHPDAT/ FX,FY,XSCAN,YSCAN,EPS,EPSX,EPSF,STEP,
     -      XORIG,YORIG,XOFF,YOFF,
     -      NITMAX,NSHOT,NSTEP,IW,NSCANX,NSCANY,JSORD,NFITER,
     -      LFGRAV,LFELEC,LFEXTR,LFWARN,LZROPR,LFITER
       INTEGER INPCMP,INPTYP,IX,IY,INEXT,IFAIL1,ISIZ(2),IDIM(2),IIW,
     -      IFAIL2,IFAIL3,IFAIL4,I,J,II,JJ,NXR,NYR,NC,NWORD,IFAIL,JW,
     -      NSAG,NSHOTR,NSTEPR,NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,NC9,NC10,
     -      NC11,NC12,JSORDR,IWR,ITER,NFITRR,IOS,IMAX,JMAX
       REAL XPL(MXGRID),YPL(MXGRID),XSAG(0:MXLIST),YSAG(0:MXLIST),
     -      CSAG(0:MXLIST),XNEAR,YNEAR,SHIFTX,SHIFTY,CORR,
     -      FX0,FY0,XSAGMX,YSAGMX,XSAGAV,YSAGAV,XSAGMI,YSAGMI,
     -      BXMIN,BYMIN,BXMAX,BYMAX,SXMIN,SYMIN,SXMAX,SYMAX,EX,EY,
     -      FXMIN,FXMAX,FYMIN,FYMAX,FSXMIN,FSYMIN,FSXMAX,FSYMAX,
     -      SXMINR,SXMAXR,SYMINR,SYMAXR,SFACT,SFACTR,XOFFR,YOFFR,TOLER,
     -      TOLERR,CORMAX
       DOUBLE PRECISION WLENG,SS
       CHARACTER*20 AUXSTR,AUX1,AUX2,AUX3,AUX4,AUX5,AUX6,AUX7,AUX8,AUX9,
     -      AUX10,AUX11,AUX12
       LOGICAL LFAST,LSAGPR,LSAGPL,LSAGKP,LFRCPR,LFRCPL,LFRCKP,INAREA,
     -      OK,SFORCE,SLARGE,CONVIT,LSTAB
       EXTERNAL INPCMP,INPTYP
       SAVE LFAST,LSAGPR,LSAGPL,LSAGKP,LFRCPR,LFRCPL,LFRCKP,LSTAB
       DATA LFAST  /.FALSE./, LSTAB  /.FALSE./,
     -      LSAGPR /.TRUE./ , LSAGPL /.FALSE./, LSAGKP /.FALSE./,
     -      LFRCPR /.FALSE./, LFRCPL /.FALSE./, LFRCKP /.FALSE./
*** Routine identification.
       IF(LIDENT)PRINT *,' /// ROUTINE OPTFRC ///'
*** Check for polar cells.
       IF(POLAR)THEN
            PRINT *,' !!!!!! OPTFRC WARNING : This instruction is not'//
     -           ' able to handle polar cells.'
            RETURN
       ENDIF
*** General purpose parameters.
       SFORCE=.FALSE.
       SLARGE=.FALSE.
       SFACT=2.0
*** Number of shots and number of intermediate steps.
       NSHOT=2
       NSTEP=20
*** Differentiation parameter, iterations, convergence criteria.
       EPS=1.0E-4
       NITMAX=100
       EPSX=1E-4
       EPSF=1E-4
       JSORD=2
       NSCANX=MIN(11,MXGRID)
       NSCANY=MIN(11,MXGRID)
*** Terms to be included.
       LFELEC=.TRUE.
       LFGRAV=.TRUE.
*** Permit extrapolation or not.
       LFEXTR=.FALSE.
*** Print flag for debugging purposes.
       LZROPR=.FALSE.
*** Iterate over all wires or not, update for such iterations.
       LFITER=NSW.GT.1
       NFITER=5
       TOLER=0.0010
*** Store nominal wire position and preset wire offset.
       DO 5 I=1,NWIRE
       XORIG(I)=X(I)
       YORIG(I)=Y(I)
       XOFF(I)=0
       YOFF(I)=0
5      CONTINUE
*** Decode the argument list.
       CALL INPNUM(NWORD)
       INEXT=2
       DO 10 I=2,NWORD
       IF(I.LT.INEXT)GOTO 10
*   Printing.
       IF(INPCMP(I,'PR#INT-S#AG').NE.0)THEN
            LSAGPR=.TRUE.
       ELSEIF(INPCMP(I,'NOPR#INT-S#AG').NE.0)THEN
            LSAGPR=.FALSE.
       ELSEIF(INPCMP(I,'PR#INT-F#ORCES').NE.0)THEN
            LFRCPR=.TRUE.
       ELSEIF(INPCMP(I,'NOPR#INT-F#ORCES').NE.0)THEN
            LFRCPR=.FALSE.
       ELSEIF(INPCMP(I,'PR#INT-Z#ERO-#SEARCH').NE.0)THEN
            LZROPR=.TRUE.
       ELSEIF(INPCMP(I,'NOPR#INT-Z#ERO-#SEARCH').NE.0)THEN
            LZROPR=.FALSE.
*   Plotting.
       ELSEIF(INPCMP(I,'PL#OT-S#AG').NE.0)THEN
            LSAGPL=.TRUE.
       ELSEIF(INPCMP(I,'NOPL#OT-S#AG').NE.0)THEN
            LSAGPL=.FALSE.
       ELSEIF(INPCMP(I,'PL#OT-F#ORCES').NE.0)THEN
            LFRCPL=.TRUE.
       ELSEIF(INPCMP(I,'NOPL#OT-F#ORCES').NE.0)THEN
            LFRCPL=.FALSE.
*   Option to keep the results.
       ELSEIF(INPCMP(I,'KEEP-S#AG').NE.0)THEN
            LSAGKP=.TRUE.
       ELSEIF(INPCMP(I,'NOKEEP-S#AG').NE.0)THEN
            LSAGKP=.FALSE.
       ELSEIF(INPCMP(I,'KEEP-F#ORCES').NE.0)THEN
            LFRCKP=.TRUE.
       ELSEIF(INPCMP(I,'NOKEEP-F#ORCES').NE.0)THEN
            LFRCKP=.FALSE.
       ELSEIF(INPCMP(I,'KEEP-R#ESULTS').NE.0)THEN
            LSAGKP=.TRUE.
            LFRCKP=.TRUE.
       ELSEIF(INPCMP(I,'NOKEEP-R#ESULTS').NE.0)THEN
            LSAGKP=.FALSE.
            LFRCKP=.FALSE.
*   Inclusion or not of gravity and electrostatics.
       ELSEIF(INPCMP(I,'GRAV#ITY').NE.0)THEN
            LFGRAV=.TRUE.
       ELSEIF(INPCMP(I,'NOGRAV#ITY').NE.0)THEN
            LFGRAV=.FALSE.
       ELSEIF(INPCMP(I,'ELEC#TROSTATICS').NE.0)THEN
            LFELEC=.TRUE.
       ELSEIF(INPCMP(I,'NOELEC#TROSTATICS').NE.0)THEN
            LFELEC=.FALSE.
*   Detailed or fast calculation.
       ELSEIF(INPCMP(I,'DET#AILED').NE.0)THEN
            LFAST=.FALSE.
       ELSEIF(INPCMP(I,'FAST').NE.0)THEN
            LFAST=.TRUE.
*   Check for wire stability or not.
       ELSEIF(INPCMP(I,'CH#ECK-STAB#ILITY')+
     -      INPCMP(I,'STAB#ILITY-#CHECK').NE.0)THEN
            LSTAB=.TRUE.
       ELSEIF(INPCMP(I,'NOCH#ECK-STAB#ILITY')+
     -      INPCMP(I,'NOSTAB#ILITY-#CHECK').NE.0)THEN
            LSTAB=.FALSE.
*   Iterate or not.
       ELSEIF(INPCMP(I,'ITER#ATE').NE.0)THEN
            LFITER=.TRUE.
            IF(INPTYP(I+1).EQ.1)THEN
                  CALL INPCHK(I+1,1,IFAIL1)
                  CALL INPRDI(I+1,NFITRR,5)
                  IF(NFITRR.GE.1)THEN
                       NFITER=NFITRR
                  ELSE
                       CALL INPMSG(I+1,'Should be > 0.')
                  ENDIF
                  INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'NOITER#ATE').NE.0)THEN
            LFITER=.FALSE.
            NFITER=0
*   Extrapolate or not beyond scanning area.
       ELSEIF(INPCMP(I,'EXTR#APOLATE').NE.0)THEN
            LFEXTR=.TRUE.
       ELSEIF(INPCMP(I,'NOEXTR#APOLATE').NE.0)THEN
            LFEXTR=.FALSE.
*   Scanning size.
       ELSEIF(INPCMP(I,'SCAN#NING-GR#ID').NE.0)THEN
            IF(INPTYP(I+1).EQ.4)THEN
                 INEXT=I+2
                 IFAIL1=0
            ELSEIF(INPTYP(I+1).EQ.1)THEN
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NXR,NSCANX)
                 IF(IFAIL1.EQ.0.AND.NXR.GT.1.AND.NXR.LE.MXGRID)THEN
                      NSCANX=NXR
                      IF(INPTYP(I+2).NE.1.AND.
     -                     INPTYP(I+2).NE.4)NSCANY=NXR
                 ELSE
                      IFAIL1=1
                      CALL INPMSG(I+1,'Should be 1 < n <= MXGRID')
                 ENDIF
                 INEXT=I+2
            ELSE
                 IFAIL1=1
            ENDIF
            IF(IFAIL1.EQ.0.AND.INPTYP(I+2).EQ.4)THEN
                 INEXT=I+3
            ELSEIF(IFAIL1.EQ.0.AND.INPTYP(I+2).EQ.1)THEN
                 CALL INPCHK(I+2,1,IFAIL1)
                 CALL INPRDI(I+2,NYR,NSCANY)
                 IF(IFAIL1.EQ.0.AND.NYR.GT.1.AND.NYR.LE.MXGRID)THEN
                      NSCANY=NYR
                 ELSE
                      CALL INPMSG(I+2,'Should be 1 < n <= MXGRID')
                 ENDIF
                 INEXT=I+3
            ENDIF
*   Scanning area.
       ELSEIF(INPCMP(I,'SCAN#NING-A#REA').NE.0)THEN
            IF(INPCMP(I+1,'MAX#IMAL')+INPCMP(I+1,'MAX#IMUM')+
     -           INPCMP(I+1,'LARG#EST').NE.0)THEN
                 SLARGE=.TRUE.
                 SFORCE=.FALSE.
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'F#IRST-ORD#ER-#ENLARGED-#BY')+
     -           INPCMP(I+1,'ENL#ARGED-#BY').NE.0)THEN
                 IF(NWORD.GE.I+2.AND.
     -                (INPTYP(I+2).EQ.1.OR.INPTYP(I+2).EQ.2))THEN
                      CALL INPCHK(I+2,2,IFAIL1)
                      CALL INPRDR(I+2,SFACTR,2.0)
                      IF(SFACTR.LE.0)THEN
                           CALL INPMSG(I+1,'Should be > 0.')
                      ELSE
                           SFACT=SFACTR
                      ENDIF
                      INEXT=I+3
                 ELSEIF(NWORD.GE.I+2.AND.INPTYP(I+2).EQ.4)THEN
                      SFACT=2.0
                      INEXT=I+3
                 ELSE
                      SFACT=2.0
                      INEXT=I+2
                 ENDIF
                 SFORCE=.FALSE.
                 SLARGE=.FALSE.
            ELSEIF((INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2).OR.
     -           (INPTYP(I+2).NE.1.AND.INPTYP(I+2).NE.2).OR.
     -           (INPTYP(I+3).NE.1.AND.INPTYP(I+3).NE.2).OR.
     -           (INPTYP(I+4).NE.1.AND.INPTYP(I+4).NE.2))THEN
                 CALL INPMSG(I,'Incorrect set of arguments.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 CALL INPCHK(I+4,2,IFAIL4)
                 CALL INPRDR(I+1,SXMINR,0.0)
                 CALL INPRDR(I+2,SYMINR,0.0)
                 CALL INPRDR(I+3,SXMAXR,0.0)
                 CALL INPRDR(I+4,SYMAXR,0.0)
                 SFORCE=.TRUE.
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.
     -                SXMINR.EQ.SXMAXR)THEN
                      CALL INPMSG(I+1,'Zero range not permitted.')
                      CALL INPMSG(I+2,'See previous message.')
                      SFORCE=.FALSE.
                 ELSE
                      FSXMIN=MIN(SXMINR,SXMAXR)
                      FSXMAX=MAX(SXMINR,SXMAXR)
                 ENDIF
                 IF(IFAIL3.EQ.0.AND.IFAIL4.EQ.0.AND.
     -                SYMINR.EQ.SYMAXR)THEN
                      CALL INPMSG(I+3,'Zero range not permitted.')
                      CALL INPMSG(I+4,'See previous message.')
                      SFORCE=.FALSE.
                 ELSE
                      FSYMIN=MIN(SYMINR,SYMAXR)
                      FSYMAX=MAX(SYMINR,SYMAXR)
                 ENDIF
                 SLARGE=.FALSE.
                 INEXT=I+5
            ENDIF
*   Initial wire offsets.
       ELSEIF(INPCMP(I,'OFF#SET').NE.0)THEN
            IF(INPTYP(I+1).NE.1.OR.
     -           (INPTYP(I+2).NE.1.AND.INPTYP(I+2).NE.2).OR.
     -           (INPTYP(I+3).NE.1.AND.INPTYP(I+3).NE.2))THEN
                 CALL INPMSG(I,'Incorrect set of arguments.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 CALL INPRDI(I+1,IWR,0)
                 CALL INPRDR(I+2,XOFFR,0.0)
                 CALL INPRDR(I+3,YOFFR,0.0)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0.AND.
     -                (IWR.LE.0.OR.IWR.GT.NWIRE))THEN
                      CALL INPMSG(I+1,'Wire number out of range.')
                 ELSEIF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                      XOFF(IWR)=XOFFR
                      YOFF(IWR)=YOFFR
                 ENDIF
                 INEXT=I+4
            ENDIF
*   Shots and steps per shot.
       ELSEIF(INPCMP(I,'SHOT#S').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'The argument is missing')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NSHOTR,NSHOT)
                 IF(NSHOTR.GE.0)THEN
                      NSHOT=NSHOTR
                 ELSE
                      CALL INPMSG(I+1,'Must be at least 0.')
                 ENDIF
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'STEP#S-#PER-#SHOT').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'The argument is missing')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NSTEPR,NSTEP)
                 IF(NSTEPR.GE.1)THEN
                      NSTEP=NSTEPR
                 ELSE
                      CALL INPMSG(I+1,'Must be at least 1.')
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Interpolation order.
       ELSEIF(INPCMP(I,'INT#ERPOLATION-ORD#ER').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'The argument is missing')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,JSORDR,JSORD)
                 IF(JSORDR.GE.1.AND.JSORDR.LE.10)THEN
                      JSORD=JSORDR
                 ELSE
                      CALL INPMSG(I+1,'Must be at in the range [1,10]')
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Wire shift tolerance.
       ELSEIF(INPCMP(I,'TOL#ERANCE').NE.0)THEN
            IF(NWORD.LT.I+1.OR.(
     -           INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2))THEN
                 CALL INPMSG(I,'The argument is missing')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,TOLERR,0.0010)
                 IF(TOLERR.GT.0)THEN
                      TOLER=TOLERR
                 ELSE
                      CALL INPMSG(I+1,'Must be > 0')
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Unrecognised keywords.
       ELSE
            CALL INPMSG(I,'Not a known keyword.')
       ENDIF
*   Next keyword.
10     CONTINUE
*** Dump the error messages.
       CALL INPERR
*** Check interpolation order compared with grid size.
       IF(JSORD.GT.NSCANX-1.OR.JSORD.GT.NSCANY-1.OR.JSORD.LT.1)THEN
            JSORD=MIN(NSCANX-1,NSCANY-1,JSORD)
            IF(JSORD.LT.1)JSORD=1
            PRINT *,' !!!!!! OPTFRC WARNING : Interpolation order'//
     -           ' larger than scanning grid size; reduced to ',JSORD
       ENDIF
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ OPTFRC DEBUG   : Settings'',
     -      '' of options:''//
     -      26X,''0th Order only:         '',L1/
     -      26X,''Plot forces:            '',L1/
     -      26X,''Print force table:      '',L1/
     -      26X,''Store forces:           '',L1/
     -      26X,''Plot wire sag:          '',L1/
     -      26X,''Print wire sag:         '',L1/
     -      26X,''Store wire sag:         '',L1)')
     -      LFAST,LFRCPL,LFRCPR,LFRCKP,LSAGPL,LSAGPR,LSAGKP
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ OPTFRC DEBUG   : Settings'',
     -      '' of parameters:''//
     -      26X,''Number of shots:         '',I5/
     -      26X,''Steps per shot:          '',I5/
     -      26X,''Epsilon differentials:   '',E10.3/
     -      26X,''Position convergence:    '',E10.3/
     -      26X,''Function convergence:    '',E10.3/
     -      26X,''Zero search iterations:  '',I5/
     -      26X,''Zero search printing:    '',L5/
     -      26X,''Permit extrapolation:    '',L5/
     -      26X,''Do all-wire iterations:  '',L5/
     -      26X,''# all-wire iterations:   '',I5/
     -      26X,''Maximum scanning area:   '',L5/
     -      26X,''Scanning area enlarging: '',E10.3/
     -      26X,''Forced scanning area:    '',L5/
     -      26X,''User scanning area:      '',4E10.3/
     -      26X,''Scanning grid density:   '',2I5)')
     -      NSHOT,NSTEP,EPS,EPSX,EPSF,NITMAX,LZROPR,LFEXTR,LFITER,
     -      NFITER,SLARGE,SFACT,SFORCE,FSXMIN,FSYMIN,FSXMAX,FSYMAX,
     -      NSCANX,NSCANY
*** Return here for a further loop.
       CALL LOGSAV(.TRUE.,'OK',IFAIL1)
       ITER=0
       CONVIT=.NOT.LFITER
1000   CONTINUE
*   Increment iteration counter.
       ITER=ITER+1
*   Reset larges wire shift.
       CORMAX=0
*** Establish the initial configuration.
       DO 15 J=1,NWIRE
       X(J)=XORIG(J)+XOFF(J)
       Y(J)=YORIG(J)+YOFF(J)
15     CONTINUE
*** Loop over wires.
       DO 20 IIW=1,NWIRE
*   Reject all that were not SELECT'ed.
       IF(INDSW(IIW).EQ.0)GOTO 20
*   Place the current wire at its nominal position.
       X(IIW)=XORIG(IIW)
       Y(IIW)=YORIG(IIW)
*** First order approximation, also used if detail is required.
       CALL SETUP(IFAIL)
*   Print a warning if this failed.
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! OPTFRC WARNING : Charge'//
     -           ' calculation failed at central position.'
            CALL LOGSAV(.FALSE.,'OK',IFAIL1)
            FX0=0.0
            FY0=0.0
*   Otherwise compute the forces.
       ELSE
            CALL FFIELD(IIW,EX,EY)
            FX0=0
            FY0=0
            IF(LFELEC)THEN
                 FX0=FX0-EX*E(IIW)*2*PI*EPS0*100
                 FY0=FY0-EY*E(IIW)*2*PI*EPS0*100
            ENDIF
            IF(LFGRAV)THEN
                 FX0=FX0-DOWN(1)*GRAV*DENS(IIW)*PI*D(IIW)**2/4000
                 FY0=FY0-DOWN(2)*GRAV*DENS(IIW)*PI*D(IIW)**2/4000
            ENDIF
       ENDIF
*   And compute the shift from this.
       SHIFTX=-125*FX0*U(IIW)**2/(GRAV*W(IIW))
       SHIFTY=-125*FY0*U(IIW)**2/(GRAV*W(IIW))
*   Get the elongation from this.
       SS=4*SQRT(DBLE(SHIFTX)**2+DBLE(SHIFTY)**2)/U(IIW)
       IF(SS.LE.0)THEN
            WLENG=U(IIW)
       ELSE
            WLENG=(SQRT(1+SS**2)+LOG(SS+SQRT(1+SS**2))/SS)*U(IIW)/2
       ENDIF
*** If requested, print results.
       IF(LSAGPR.AND.CONVIT)THEN
            CALL OUTFMT(REAL(IIW),2,AUX1,NC1,'LEFT')
            CALL OUTFMT(XORIG(IIW),2,AUX2,NC2,'LEFT')
            CALL OUTFMT(YORIG(IIW),2,AUX3,NC3,'LEFT')
            CALL OUTFMT(V(IIW),2,AUX4,NC4,'LEFT')
            CALL OUTFMT(U(IIW),2,AUX5,NC5,'LEFT')
            CALL OUTFMT(W(IIW),2,AUX6,NC6,'LEFT')
            CALL OUTFMT(FX0,2,AUX7,NC7,'LEFT')
            CALL OUTFMT(FY0,2,AUX8,NC8,'LEFT')
            CALL OUTFMT(SHIFTX,2,AUX9,NC9,'LEFT')
            CALL OUTFMT(SHIFTY,2,AUX10,NC10,'LEFT')
            CALL OUTFMT(REAL(WLENG-U(IIW))/U(IIW),2,AUX11,NC11,'LEFT')
            WRITE(LUNOUT,'(''  FORCES AND DISPLACEMENT IN 0th ORDER''//
     -      ''  Wire information: number   = '',A/
     -      ''                    type     = '',A1/
     -      ''                    location = ('',A,'', '',A,'') cm''/
     -      ''                    voltage  = '',A,'' V''/
     -      ''                    length   = '',A,'' cm''/
     -      ''                    tension  = '',A,'' g''//
     -      ''  In this position: Fx       = '',A,'' N/cm''/
     -      ''                    Fy       = '',A,'' N/cm''/
     -      ''                    x-shift  = '',A,'' cm''/
     -      ''                    y-shift  = '',A,'' cm''/
     -      ''                    stretch  = '',A,'' fraction'')')
     -      AUX1(1:NC1),WIRTYP(IIW),AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4),
     -      AUX5(1:NC5),AUX6(1:NC6),AUX7(1:NC7),AUX8(1:NC8),AUX9(1:NC9),
     -      AUX10(1:NC10),AUX11(1:NC11)
       ENDIF
*** Save the forces if requested and if the rest is skipped.
       IF(LFAST.AND.LFRCKP.AND.CONVIT)THEN
*   Format the wire number.
            CALL OUTFMT(REAL(IIW),2,AUXSTR,NC,'LEFT')
*   Assign the results to globals.
            CALL NUMSAV(FX0,'FORCE_X_'//AUXSTR(1:NC),IFAIL1)
            CALL NUMSAV(FY0,'FORCE_Y_'//AUXSTR(1:NC),IFAIL2)
*   Check the error condition.
            IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0)THEN
                 PRINT *,' ------ OUTFRC MESSAGE : The forces'//
     -                ' acting on wire '//AUXSTR(1:NC)//' are'
                 PRINT *,'                         saved as FORCE_X_'//
     -                AUXSTR(1:NC)//' and FORCE_Y_'//AUXSTR(1:NC)//'.'
            ELSE
                 PRINT *,' !!!!!! OPTFRC WARNING : Saving the forces'//
     -                ' failed.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
            ENDIF
       ENDIF
*** Save the sag if requested and if the rest is skipped.
       IF(LFAST.AND.LSAGKP.AND.CONVIT)THEN
*   Format the wire number.
            CALL OUTFMT(REAL(IIW),2,AUXSTR,NC,'LEFT')
*   Assign the results to globals.
            CALL NUMSAV(SHIFTX,'SHIFT_X_'//AUXSTR(1:NC),IFAIL1)
            CALL NUMSAV(SHIFTY,'SHIFT_Y_'//AUXSTR(1:NC),IFAIL2)
            CALL NUMSAV(REAL((WLENG-U(IIW))/U(IIW)),
     -           'STRETCH_'//AUXSTR(1:NC),IFAIL3)
*   Check the error condition.
            IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                 PRINT *,' ------ OUTFRC MESSAGE : Shift and'//
     -                ' elongation of wire '//AUXSTR(1:NC)
                 PRINT *,'                         saved as SHIFT_X_'//
     -                AUXSTR(1:NC)//', SHIFT_Y_'//AUXSTR(1:NC)//
     -                ' and STRETCH_'//AUXSTR(1:NC)//'.'
            ELSE
                 PRINT *,' !!!!!! OPTFRC WARNING : Saving the sag'//
     -                ' failed.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
            ENDIF
       ENDIF
*** And skip the rest if fast calculation was requested.
       IF(LFAST)THEN
            CORMAX=MAX(CORMAX,ABS(2.0*SHIFTX/3.0-XOFF(IIW)),
     -           ABS(2.0*SHIFTY/3.0-YOFF(IIW)))
            X(IIW)=XORIG(IIW)+XOFF(IIW)
            Y(IIW)=YORIG(IIW)+YOFF(IIW)
            XOFF(IIW)=2.0*SHIFTX/3.0
            YOFF(IIW)=2.0*SHIFTY/3.0
            GOTO 20
       ENDIF
*** Detailed calculation: compute a 'safe box' around the wire.
       IF(PERX)THEN
            BXMIN=X(IIW)-SX/2
            BXMAX=X(IIW)+SX/2
       ELSE
            BXMIN=2*XMIN-XMAX
            BXMAX=2*XMAX-XMIN
       ENDIF
       IF(PERY)THEN
            BYMIN=Y(IIW)-SY/2
            BYMAX=Y(IIW)+SY/2
       ELSE
            BYMIN=2*YMIN-YMAX
            BYMAX=2*YMAX-YMIN
       ENDIF
*   If the initial area is almost zero in 1 direction, make it square.
       IF(ABS(BXMAX-BXMIN).LT.0.1*ABS(BYMAX-BYMIN))THEN
            BXMIN=X(IIW)-ABS(BYMAX-BYMIN)/2
            BXMAX=X(IIW)+ABS(BYMAX-BYMIN)/2
       ELSEIF(ABS(BYMAX-BYMIN).LT.0.1*ABS(BXMAX-BXMIN))THEN
            BYMIN=Y(IIW)-ABS(BXMAX-BXMIN)/2
            BYMAX=Y(IIW)+ABS(BXMAX-BXMIN)/2
       ENDIF
*   Scan the other wires.
       DO 100 JW=1,NWIRE
       IF(JW.EQ.IIW)GOTO 100
       IF(PERX)THEN
            XNEAR=X(JW)-ANINT((X(JW)-X(IIW))/SX)*SX
       ELSE
            XNEAR=X(JW)
       ENDIF
       IF(PERY)THEN
            YNEAR=Y(JW)-ANINT((Y(JW)-Y(IIW))/SY)*SY
       ELSE
            YNEAR=Y(JW)
       ENDIF
       IF(ABS(XNEAR-X(IIW)).GT.ABS(YNEAR-Y(IIW)))THEN
            IF(XNEAR.LT.X(IIW))THEN
                 BXMIN=MAX(BXMIN,XNEAR+D(JW)+D(IIW))
                 IF(PERX)BXMAX=MIN(BXMAX,XNEAR+SX-D(JW)-D(IIW))
            ELSE
                 BXMAX=MIN(BXMAX,XNEAR-D(JW)-D(IIW))
                 IF(PERX)BXMIN=MAX(BXMIN,XNEAR-SX+D(JW)+D(IIW))
            ENDIF
       ELSE
            IF(YNEAR.LT.Y(IIW))THEN
                 BYMIN=MAX(BYMIN,YNEAR-D(JW)-D(IIW),YNEAR+D(JW)+D(IIW))
                 IF(PERY)BYMAX=MIN(BYMAX,YNEAR+SY-D(JW)-D(IIW))
            ELSE
                 BYMAX=MIN(BYMAX,YNEAR-D(JW)-D(IIW),YNEAR+D(JW)+D(IIW))
                 IF(PERY)BYMIN=MAX(BYMIN,YNEAR-SY+D(JW)+D(IIW))
            ENDIF
       ENDIF
100    CONTINUE
*   Scan the planes.
       IF(YNPLAN(1))BXMIN=MAX(BXMIN,COPLAN(1)+D(IIW))
       IF(YNPLAN(2))BXMAX=MIN(BXMAX,COPLAN(2)-D(IIW))
       IF(YNPLAN(3))BYMIN=MAX(BYMIN,COPLAN(3)+D(IIW))
       IF(YNPLAN(4))BYMAX=MIN(BYMAX,COPLAN(4)-D(IIW))
*   If there is a tube, check all corners.
       IF(TUBE.AND.COTUBE**2-D(IIW)**2.GT.0)THEN
            CORR=SQRT((BXMIN**2+BYMIN**2)/(COTUBE**2-D(IIW)**2))
            IF(CORR.GT.1)THEN
                 BXMIN=BXMIN/CORR
                 BYMIN=BYMIN/CORR
            ENDIF
            CORR=SQRT((BXMIN**2+BYMIN**2)/(COTUBE**2-D(IIW)**2))
            IF(CORR.GT.1)THEN
                 BXMIN=BXMIN/CORR
                 BYMAX=BYMAX/CORR
            ENDIF
            CORR=SQRT((BXMIN**2+BYMIN**2)/(COTUBE**2-D(IIW)**2))
            IF(CORR.GT.1)THEN
                 BXMAX=BXMAX/CORR
                 BYMIN=BYMIN/CORR
            ENDIF
            CORR=SQRT((BXMIN**2+BYMIN**2)/(COTUBE**2-D(IIW)**2))
            IF(CORR.GT.1)THEN
                 BXMAX=BXMAX/CORR
                 BYMAX=BYMAX/CORR
            ENDIF
       ELSEIF(TUBE)THEN
            PRINT *,' !!!!!! OPTFRC WARNING : Wire diameter too'//
     -           ' large compared to tube; wire ',IIW,' skipped.'
            CALL LOGSAV(.FALSE.,'OK',IFAIL1)
            GOTO 20
       ENDIF
*   Make sure we found a reasonable 'safe area'.
       IF((BXMIN-X(IIW))*(X(IIW)-BXMAX).LE.0.OR.
     -      (BYMIN-Y(IIW))*(Y(IIW)-BYMAX).LE.0)THEN
            PRINT *,' !!!!!! OPTFRC WARNING : Unable to find'//
     -           ' an area free of elements around wire ',IIW
            CALL LOGSAV(.FALSE.,'OK',IFAIL1)
            GOTO 20
       ENDIF
*** Now set a reasonable scanning range: if user specified range:
       IF(SFORCE)THEN
            SXMIN=X(IIW)+FSXMIN
            SYMIN=Y(IIW)+FSYMIN
            SXMAX=X(IIW)+FSXMAX
            SYMAX=Y(IIW)+FSYMAX
*   if maximum area:
       ELSEIF(SLARGE)THEN
            SXMIN=BXMIN
            SXMAX=BXMAX
            SYMIN=BYMIN
            SYMAX=BYMAX
*   if 0th order estimate of shift is not small:
       ELSEIF(ABS(SHIFTX).GT.D(IIW)/20.OR.ABS(SHIFTY).GT.D(IIW)/20)THEN
            SXMIN=MAX(BXMIN,MIN(X(IIW)+SFACT*SHIFTX,
     -           X(IIW)-SHIFTX/SFACT))
            SYMIN=MAX(BYMIN,MIN(Y(IIW)+SFACT*SHIFTY,
     -           Y(IIW)-SHIFTY/SFACT))
            SXMAX=MIN(BXMAX,MAX(X(IIW)+SFACT*SHIFTX,
     -           X(IIW)-SHIFTX/SFACT))
            SYMAX=MIN(BYMAX,MAX(Y(IIW)+SFACT*SHIFTY,
     -           Y(IIW)-SHIFTY/SFACT))
*   If one is very small, make the area square within bounds.
            IF(ABS(SXMAX-SXMIN).LT.0.1*ABS(SYMAX-SYMIN))THEN
                 SXMIN=MAX(BXMIN,X(IIW)-0.5*ABS(SYMAX-SYMIN))
                 SXMAX=MIN(BXMAX,X(IIW)+0.5*ABS(SYMAX-SYMIN))
            ELSEIF(ABS(SYMAX-SYMIN).LT.0.1*ABS(SXMAX-SXMIN))THEN
                 SYMIN=MAX(BYMIN,Y(IIW)-0.5*ABS(SXMAX-SXMIN))
                 SYMAX=MIN(BYMAX,Y(IIW)+0.5*ABS(SXMAX-SXMIN))
            ENDIF
*   Otherwise, take full acceptable range.
       ELSE
            SXMIN=BXMIN
            SYMIN=BYMIN
            SXMAX=BXMAX
            SYMAX=BYMAX
       ENDIF
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ OPTFRC DEBUG   : '',
     -      ''Free area '',E12.5,'' < x < '',E12.5/26X,
     -      ''          '',E12.5,'' < y < '',E12.5/26X,
     -      ''Scan area '',E12.5,'' < x < '',E12.5/26X,
     -      ''          '',E12.5,'' < y < '',E12.5)')
     -      BXMIN,BXMAX,BYMIN,BYMAX,SXMIN,SXMAX,SYMIN,SYMAX
*** Prepare an interpolation table.
       OK=.TRUE.
       DO 30 IX=1,NSCANX
       XSCAN(IX)=SXMIN+REAL(IX-1)*(SXMAX-SXMIN)/REAL(NSCANX-1)
       DO 40 IY=1,NSCANY
       YSCAN(IY)=SYMIN+REAL(IY-1)*(SYMAX-SYMIN)/REAL(NSCANY-1)
*   Get the wire position for this shift.
       X(IIW)=REAL(XSCAN(IX))
       Y(IIW)=REAL(YSCAN(IY))
*   Verify the current situation.
       CALL CELWCH(IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! OPTFRC WARNING : Scan involves a'//
     -           ' disallowed wire position; wire ',IIW,' skipped.'
            CALL LOGSAV(.FALSE.,'OK',IFAIL1)
            FX(IX,IY)=0.0
            FY(IX,IY)=0.0
            OK=.FALSE.
            GOTO 40
       ENDIF
*   Recompute the charges for this configuration.
       CALL SETUP(IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! OPTFRC WARNING : Failed to compute'//
     -           ' charges at a scan point; wire ',IIW,' skipped.'
            CALL LOGSAV(.FALSE.,'OK',IFAIL1)
            FX(IX,IY)=0.0
            FY(IX,IY)=0.0
            OK=.FALSE.
            GOTO 40
       ENDIF
*   Compute the forces.
       CALL FFIELD(IIW,EX,EY)
       FX(IX,IY)=-EX*E(IIW)*2*PI*EPS0*100
       FY(IX,IY)=-EY*E(IIW)*2*PI*EPS0*100
*   And keep track of the range of the forces.
       IF(IX.EQ.1.AND.IY.EQ.1)THEN
            FXMIN=REAL(FX(IX,IY))
            FXMAX=REAL(FX(IX,IY))
            FYMIN=REAL(FY(IX,IY))
            FYMAX=REAL(FY(IX,IY))
       ELSE
            FXMIN=MIN(FXMIN,REAL(FX(IX,IY)))
            FXMAX=MAX(FXMAX,REAL(FX(IX,IY)))
            FYMIN=MIN(FYMIN,REAL(FY(IX,IY)))
            FYMAX=MAX(FYMAX,REAL(FY(IX,IY)))
       ENDIF
*   Next point.
40     CONTINUE
30     CONTINUE
*** Place the wire back in its shifted position.
       X(IIW)=XORIG(IIW)+XOFF(IIW)
       Y(IIW)=YORIG(IIW)+YOFF(IIW)
*** Skip the rest in case of failure.
       IF(.NOT.OK)GOTO 20
*** Plot the force field if requested.
       IF(LFRCPL.AND.CONVIT)THEN
*   Open a frame for the x-grid lines.
            CALL GRCART(SXMIN,MIN(FXMIN,FYMIN)-0.1*
     -           (MAX(FXMAX,FYMAX)-MIN(FXMIN,FYMIN)),
     -           SXMAX,MAX(FXMAX,FYMAX)+0.1*
     -           (MAX(FXMAX,FYMAX)-MIN(FXMIN,FYMIN)),
     -           'Wire x position [cm]','Force [N/cm]',
     -           'Forces as function of wire shift')
*   Add comments.
            IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
            CALL OUTFMT(REAL(IIW),2,AUXSTR,NC,'LEFT')
            CALL GRCOMM(2,'Wire: '//AUXSTR(1:NC)//' ('//
     -           WIRTYP(IIW)//')')
*   Plot the forces.
            DO 140 IY=1,NSCANY
            CALL GRATTS('FUNCTION-1','POLYLINE')
            DO 150 IX=1,NSCANX
            XPL(IX)=REAL(XSCAN(IX))
            YPL(IX)=REAL(FX(IX,IY))
150         CONTINUE
            CALL GRLINE(NSCANX,XPL,YPL)
            CALL GRATTS('FUNCTION-2','POLYLINE')
            DO 160 IX=1,NSCANX
            XPL(IX)=REAL(XSCAN(IX))
            YPL(IX)=REAL(FY(IX,IY))
160         CONTINUE
            CALL GRLINE(NSCANX,XPL,YPL)
140         CONTINUE
*   Register the plot and close this frame.
            CALL GRALOG('Forces on wire '//AUXSTR(1:NC))
            CALL GRNEXT
*   Open a frame for the y-grid lines.
            CALL GRCART(SYMIN,MIN(FXMIN,FYMIN)-0.1*
     -           (MAX(FXMAX,FYMAX)-MIN(FXMIN,FYMIN)),
     -           SYMAX,MAX(FXMAX,FYMAX)+0.1*
     -           (MAX(FXMAX,FYMAX)-MIN(FXMIN,FYMIN)),
     -           'Wire y position [cm]','Force [N/cm]',
     -           'Forces as function of wire shift')
*   Add comments.
            IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
            CALL GRCOMM(2,'Wire: '//AUXSTR(1:NC)//' ('//
     -           WIRTYP(IIW)//')')
*   Plot the forces.
            DO 110 IX=1,NSCANX
            CALL GRATTS('FUNCTION-1','POLYLINE')
            DO 120 IY=1,NSCANY
            XPL(IY)=REAL(YSCAN(IY))
            YPL(IY)=REAL(FX(IX,IY))
120         CONTINUE
            CALL GRLINE(NSCANY,XPL,YPL)
            CALL GRATTS('FUNCTION-2','POLYLINE')
            DO 130 IY=1,NSCANY
            XPL(IY)=REAL(YSCAN(IY))
            YPL(IY)=REAL(FY(IX,IY))
130         CONTINUE
            CALL GRLINE(NSCANY,XPL,YPL)
110         CONTINUE
*   Register the plot and close this frame.
            CALL GRALOG('Forces on wire '//AUXSTR(1:NC))
            CALL GRNEXT
       ENDIF
*** Print the table of the forces.
       IF(LFRCPR.AND.CONVIT)THEN
*   Print a header.
            WRITE(LUNOUT,'(''  FORCES ACTING ON WIRE '',I4//
     -           ''  Fx  [N/cm]''/''  Fy  [N/cm]''/''  |F| [N/cm]'')')
     -           IIW
*   Print them block by block.
            DO 170 JJ=0,10*INT((NSCANY-1)/10.0),10
            JMAX=MIN(NSCANY-JJ,10)
            DO 180 II=0,10*INT((NSCANX-1)/10.0),10
            IMAX=MIN(NSCANX-II,10)
            WRITE(LUNOUT,'(''1 Force-print'',109X,
     -           ''Part '',I1,''.'',I1)',
     -           ERR=2010,IOSTAT=IOS) 1+II/10,1+JJ/10
            WRITE(LUNOUT,'(''  ==========='',109X,''========''/)',
     -           IOSTAT=IOS,ERR=2010)
            WRITE(LUNOUT,'('' y        x:'',10(E11.4,1X:)/)',
     -           IOSTAT=IOS,ERR=2010) (XSCAN(II+I),I=1,IMAX)
            DO 190 J=1,JMAX
            WRITE(LUNOUT,'(1X,E10.3)',IOSTAT=IOS,ERR=2010)
     -           YSCAN(JJ+J)
            WRITE(LUNOUT,'(12X,10(E11.4,1X:))',IOSTAT=IOS,ERR=2010)
     -           (FX(II+I,JJ+J),I=1,IMAX)
            WRITE(LUNOUT,'(12X,10(E11.4,1X:))',IOSTAT=IOS,ERR=2010)
     -           (FY(II+I,JJ+J),I=1,IMAX)
            WRITE(LUNOUT,'(12X,10(E11.4,1X:))',IOSTAT=IOS,ERR=2010)
     -           (SQRT(FX(II+I,JJ+J)**2+FY(II+I,JJ+J)**2),I=1,IMAX)
190         CONTINUE
180         CONTINUE
170         CONTINUE
       ENDIF
*** Save the force table if requested.
       IF(LFRCKP.AND.CONVIT)THEN
*   Format the wire number.
            CALL OUTFMT(REAL(IIW),2,AUXSTR,NC,'LEFT')
*   Assign the results to globals.
            ISIZ(1)=NSCANX
            ISIZ(2)=NSCANY
            IDIM(1)=MXGRID
            IDIM(2)=MXGRID
            CALL MT2SAV(FX,2,IDIM,ISIZ,'FX_'//AUXSTR(1:NC),IFAIL1)
            CALL MT2SAV(FY,2,IDIM,ISIZ,'FY_'//AUXSTR(1:NC),IFAIL2)
            ISIZ(1)=NSCANX
            IDIM(1)=MXGRID
            CALL MT2SAV(XSCAN,1,IDIM,ISIZ,
     -           'X_F_'//AUXSTR(1:NC),IFAIL3)
            ISIZ(1)=NSCANY
            IDIM(1)=MXGRID
            CALL MT2SAV(YSCAN,1,IDIM,ISIZ,
     -           'Y_F_'//AUXSTR(1:NC),IFAIL4)
*   Check the error condition.
            IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.
     -           IFAIL3.EQ.0.AND.IFAIL4.EQ.0)THEN
                 PRINT *,' ------ OUTFRC MESSAGE : Force table'//
     -                ' of wire '//AUXSTR(1:NC)//' saved as'
                 PRINT *,'                         FX_'//AUXSTR(1:NC)//
     -                ', FY_'//AUXSTR(1:NC)//', X_F_'//AUXSTR(1:NC)//
     -                ' and Y_F_'//AUXSTR(1:NC)//'.'
            ELSE
                 PRINT *,' !!!!!! OPTFRC WARNING : Saving the force'//
     -                ' table failed.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
            ENDIF
       ENDIF
*** Compute the detailed wire shift.
       NSAG=MXLIST
       CALL OPTSAG(IIW,'PARABOLIC',CSAG,XSAG,YSAG,NSAG,IFAIL1)
C       CALL OPTSAG(IIW,'RANDOM',CSAG,XSAG,YSAG,NSAG,IFAIL1)
*   Check error status.
       IF(IFAIL1.NE.0.OR.NSAG.LE.0)THEN
            PRINT *,' !!!!!! OPTFRC WARNING : Computation of the'//
     -           ' wire sag failed; wire ',IIW,' skipped.'
            CALL LOGSAV(.FALSE.,'OK',IFAIL1)
            GOTO 20
       ENDIF
*   And compute mean and maximum sag, verify that the wire is in range.
       XSAGMI=XSAG(0)
       YSAGMI=YSAG(0)
       XSAGMX=XSAG(0)
       YSAGMX=YSAG(0)
       XSAGAV=0
       YSAGAV=0
       INAREA=.TRUE.
       DO 210 I=0,NSAG
       IF(I.EQ.0)THEN
            WLENG=0
       ELSE
            WLENG=WLENG+SQRT((XSAG(I)-XSAG(I-1))**2+
     -           (YSAG(I)-YSAG(I-1))**2+(CSAG(I)-CSAG(I-1))**2)
       ENDIF
       IF(XORIG(IIW)+XSAG(I).LT.SXMIN.OR.
     -      XORIG(IIW)+XSAG(I).GT.SXMAX.OR.
     -      YORIG(IIW)+YSAG(I).LT.SYMIN.OR.
     -      YORIG(IIW)+YSAG(I).GT.SYMAX)INAREA=.FALSE.
       XSAGMI=MIN(XSAGMI,XSAG(I))
       YSAGMI=MIN(YSAGMI,YSAG(I))
       XSAGMX=MAX(XSAGMX,XSAG(I))
       YSAGMX=MAX(YSAGMX,YSAG(I))
       XSAGAV=XSAGAV+XSAG(I)
       YSAGAV=YSAGAV+YSAG(I)
210    CONTINUE
       XSAGAV=XSAGAV/REAL(NSAG+1)
       YSAGAV=YSAGAV/REAL(NSAG+1)
*   Update the wire offset vector.
       CORMAX=MAX(CORMAX,ABS(XSAGAV-XOFF(IIW)),ABS(YSAGAV-YOFF(IIW)))
       XOFF(IIW)=XSAGAV
       YOFF(IIW)=YSAGAV
*   Warn if a point outside the scanning area was found.
       IF(.NOT.INAREA)THEN
            PRINT *,' !!!!!! OPTFRC WARNING : The wire profile is'//
     -           ' located partially outside the scanning area.'
       ENDIF
*** If required, print the wire sag.
       IF(LSAGPR.AND.CONVIT)THEN
            CALL OUTFMT(REAL(IIW),2,AUX1,NC1,'LEFT')
            CALL OUTFMT(V(IIW),2,AUX2,NC2,'LEFT')
            CALL OUTFMT(U(IIW),2,AUX3,NC3,'LEFT')
            CALL OUTFMT(W(IIW),2,AUX4,NC4,'LEFT')
            CALL OUTFMT(DENS(IIW),2,AUX5,NC5,'LEFT')
            CALL OUTFMT(XORIG(IIW),2,AUX6,NC6,'LEFT')
            CALL OUTFMT(YORIG(IIW),2,AUX7,NC7,'LEFT')
            CALL OUTFMT(XSAGAV,2,AUX8,NC8,'LEFT')
            CALL OUTFMT(YSAGAV,2,AUX9,NC9,'LEFT')
            CALL OUTFMT(MAX(ABS(XSAGMX),ABS(XSAGMI)),2,
     -           AUX10,NC10,'LEFT')
            CALL OUTFMT(MAX(ABS(YSAGMX),ABS(YSAGMI)),2,
     -           AUX11,NC11,'LEFT')
            CALL OUTFMT(100*REAL(WLENG-U(IIW))/U(IIW),2,
     -           AUX12,NC12,'LEFT')
            WRITE(LUNOUT,'(''  SAG PROFILE FOR WIRE '',A,'' (TYPE '',A1,
     -           '')''//
     -           ''  Wire voltage:           '',A,'' V''/
     -           ''  Wire length:            '',A,'' cm''/
     -           ''  Wire stretching weight: '',A,'' g''/
     -           ''  Wire density:           '',A,'' g/cm3''/
     -           ''  Nominal wire position: ('',A,'','',A,'') cm''//
     -           ''  Average sag in x and y: '',A,'' and '',A,'' cm''/
     -           ''  Maximum sag in x and y: '',A,'' and '',A,'' cm''/
     -           ''  Elongation:             '',A,'' %''//
     -           ''  Point        z [cm]    x-sag [cm]'',
     -           ''    y-sag [cm]''/)')
     -           AUX1(1:NC1),WIRTYP(IIW),AUX2(1:NC2),AUX3(1:NC3),
     -           AUX4(1:NC4),AUX5(1:NC5),AUX6(1:NC6),AUX7(1:NC7),
     -           AUX8(1:NC8),AUX9(1:NC9),AUX10(1:NC10),AUX11(1:NC11),
     -           AUX12(1:NC12)
            DO 200 I=0,NSAG
            WRITE(LUNOUT,'(I7,3(2X,E12.5))') I,CSAG(I),XSAG(I),YSAG(I)
200         CONTINUE
       ENDIF
*** Plot the wire profile, if requested.
       IF(LSAGPL.AND.CONVIT)THEN
*   Open a frame.
            CALL GRCART(CSAG(0),MIN(XSAGMI,YSAGMI)-
     -           0.1*(MAX(XSAGMX,YSAGMX)-MIN(XSAGMI,YSAGMI)),
     -           CSAG(NSAG),MAX(XSAGMX,YSAGMX)+
     -           0.1*(MAX(XSAGMX,YSAGMX)-MIN(XSAGMI,YSAGMI)),
     -           'z [cm]','Sag [cm]','Wire profile')
*   Add some comments to the plot.
            CALL OUTFMT(REAL(IIW),2,AUXSTR,NC,'LEFT')
            IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
            CALL GRCOMM(2,'Wire: '//AUXSTR(1:NC)//' ('//
     -           WIRTYP(IIW)//')')
*   Plot the curves.
            CALL GRATTS('FUNCTION-1','POLYLINE')
            CALL GRLINE(NSAG+1,CSAG(0),XSAG(0))
            CALL GRATTS('FUNCTION-2','POLYLINE')
            CALL GRLINE(NSAG+1,CSAG(0),YSAG(0))
*   Register the plot.
            CALL GRALOG('Sag profile of wire '//AUXSTR(1:NC)//':')
*   Close the frame.
            CALL GRNEXT
       ENDIF
*** Save the results if requested.
       IF(LSAGKP.AND.CONVIT)THEN
*   Format the wire number.
            CALL OUTFMT(REAL(IIW),2,AUXSTR,NC,'LEFT')
*   Assign the results to globals.
            ISIZ(1)=NSAG+1
            IDIM(1)=MXLIST+1
            CALL MATSAV(CSAG(0),1,IDIM,ISIZ,
     -           'Z_'//AUXSTR(1:NC),IFAIL1)
            CALL MATSAV(XSAG(0),1,IDIM,ISIZ,
     -           'SAG_X_'//AUXSTR(1:NC),IFAIL2)
            CALL MATSAV(YSAG(0),1,IDIM,ISIZ,
     -           'SAG_Y_'//AUXSTR(1:NC),IFAIL3)
            CALL NUMSAV(REAL((WLENG-U(IIW))/U(IIW)),
     -           'STRETCH_'//AUXSTR(1:NC),IFAIL4)
*   Check the error condition.
            IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.
     -           IFAIL3.EQ.0.AND.IFAIL4.EQ.0)THEN
                 PRINT *,' ------ OUTFRC MESSAGE : Sag profile'//
     -                ' of wire '//AUXSTR(1:NC)//' saved as'
                 PRINT *,'                         Z_'//AUXSTR(1:NC)//
     -                ', SAG_X_'//AUXSTR(1:NC)//', SAG_Y_'//
     -                AUXSTR(1:NC)//' and STRETCH_'//AUXSTR(1:NC)//'.'
            ELSE
                 PRINT *,' !!!!!! OPTFRC WARNING : Saving the results'//
     -                ' failed.'
            ENDIF
       ENDIF
*** Check for wire stability.
       IF(LSTAB)THEN
            CALL OPTENM
       ENDIF
*** Next wire.
20     CONTINUE
*** If iteration over all wires was requested ...
       IF(LFITER.AND..NOT.CONVIT)THEN
*   Print current status.
            WRITE(LUNOUT,'(''  Iteration '',I3/)') ITER
            DO 1050 I=1,NWIRE
            IF(INDSW(I).EQ.0)GOTO 1050
            WRITE(LUNOUT,'(''  Wire '',I3,'' moves on average by ('',
     -           E12.5,'','',E12.5,'') cm'')') I,XOFF(I),YOFF(I)
1050        CONTINUE
            WRITE(LUNOUT,'(/''  Largest average shift: '',E12.5,
     -           '' cm.'')') CORMAX
*   Check convergence, send for a last round if needed.
            IF(CORMAX.LE.TOLER)THEN
                 WRITE(LUNOUT,'(''  Convergence achieved.'')')
                 CONVIT=.TRUE.
                 GOTO 1000
            ELSEIF(ITER.LT.NFITER)THEN
                 GOTO 1000
            ELSE
                 WRITE(LUNOUT,'(''  Maximum number if iterations'',
     -                '' reached - iteration stopped.'')')
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
            ENDIF
       ENDIF
*** Restore the initial situation.
       DO 1100 I=1,NWIRE
       X(I)=XORIG(I)
       Y(I)=YORIG(I)
1100   CONTINUE
       CALL SETUP(IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' ###### OPTFRC ERROR   : Unable to'//
     -           ' restore the initial configuration.'
            PRINT *,'                         Setting the'//
     -           ' number of wires to 0.'
            CALL LOGSAV(.FALSE.,'OK',IFAIL1)
            NWIRE=0
       ENDIF
*** Register the amount of CPU time used with TIMLOG.
       CALL TIMLOG('Computing forces on the wires:          ')
*** Normal end of the routine.
       RETURN
*** Handle I/O errors.
2010   CONTINUE
       PRINT *,' ###### OPTFRC ERROR   : Error writing the force'//
     -      ' table on unit ',LUNOUT,' ; output terminated.'
       CALL LOGSAV(.FALSE.,'OK',IFAIL1)
       CALL INPIOS(IOS)
       END
