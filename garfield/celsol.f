CDECK  ID>, CELSOL.
       SUBROUTINE CELSOL
*-----------------------------------------------------------------------
*   CELSOL - Reads a list of solids.
*   VARIABLES : NXMIN,NXMAX: Numbers of resp first and last x-period.
*               NYMIN,NYMAX:    "    "   "     "    "   "   y   "
*               Boundary types as used in neBEM
*               1: conductor at specified potential
*               2: conductor with a specified charge
*               3: floating conductor (zero charge, perpendicular E)
*               4: dielectric without "manual" charge (plastic-plastic)
*               5: dielectric with surface charge (plastic-gas)
*               6: symmetry boundary, E parallel
*               7: symmetry boundary, E perpendicular (any use ?)
*   (Last changed on 12/10/11.)
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
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
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
       DOUBLE PRECISION WGT,FPRMAT,
     -      FPROJ,FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX,GYBOX,GZBOX
       REAL PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM
       INTEGER NLINED,NGRIDX,NGRIDY,ITRTYP,NTRLIN,NTRSAM,INDPOS,NCTRW,
     -      NTRFLX,NINORD,
     -      NCPNAM,NCXLAB,NCYLAB,NCFPRO,IPRMAT,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,ITFSRM,NTRERR
       LOGICAL LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG,LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       COMMON /PARMS / WGT(MXLIST),FPRMAT(3,3),
     -      FPROJ(3,3),FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX(12),GYBOX(12),GZBOX(12),
     -      PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM,
     -      INDPOS(11000),IPRMAT(3),NCTRW,NCPNAM,
     -      ITRTYP,NTRLIN,NTRSAM,NTRFLX,ITFSRM,NTRERR(10),
     -      NLINED,NINORD,NGRIDX,NGRIDY,NCXLAB,NCYLAB,NCFPRO,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,
     -      LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG(10),LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       CHARACTER*80 PARTID,PXLAB,PYLAB,PROLAB
       CHARACTER*10 PNAME
       CHARACTER*5  PRVIEW
       CHARACTER*(MXCHAR) FCNTRW
       COMMON /PARCHR/ PARTID,FCNTRW,PNAME,PXLAB,PYLAB,PROLAB,PRVIEW
       INTEGER NBEM,IREFB1(MXPLAN),NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,
     -      BEMNEW,BEMINV,BEMSLV
       DOUBLE PRECISION BEMQTH,BEMSTH,BEMSSC,BEMTGT,BEMEPA,BEMEPD
       LOGICAL LBDUMP
       COMMON /BEMDAT/ BEMQTH,BEMSSC,BEMSTH,BEMTGT,BEMEPA,BEMEPD,
     -      IREFB1,NBEM,NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,BEMNEW,
     -      BEMINV,BEMSLV,LBDUMP
       CHARACTER*(MXCHAR) STRING
       CHARACTER AUXTYP
       INTEGER I,INEXT,NWORD,IFAIL1,IFAIL2,IFAIL3,INPCMP,INPTYP,
     -      NC,IMAT,N,NR,NCAUX,IBOUND,NPROF,IORI,IPRMIN,J
       DOUBLE PRECISION XPROF(MXEDGE),YPROF(MXEDGE),ZPROF(MXEDGE),DET,
     -      XPRMIN
       REAL XDIR,YDIR,ZDIR,XPOS,YPOS,ZPOS,XSIZ,YSIZ,ZSIZ,R,R1,R2,
     -      AUX1,AUX2,AUX3,THETA,PHI,AROT,VOLT,CHARGE,EPS,XNOTCH,ZNOTCH,
     -      DIS1,DIS2,DIS3,DIS4,DIS5,DIS6,DIS7,DISAUX,DISDEF
       LOGICAL LRAD,LRAD1,LRAD2,LPOS,LSIZ,STDSTR,LVOLT,LEPS,LCHA,
     -      LNOTCH, LTLID,LBLID,LRMEAN,LEPROF
       EXTERNAL INPCMP,INPTYP,STDSTR
*** Read the number of words.
       CALL INPNUM(NWORD)
*** Warn if there are options.
       IF(NWORD.NE.1)PRINT *,' !!!!!! CELSOL WARNING : No arguments'//
     -      ' for SOLIDS known; ignored.'
*** Initialise the conductor table.
       NSOLID=0
       ICCURR=0
*** Let's assume for now that we can actually call neBEM if needed.
       BEMSET=.TRUE.
*** Default discretisation: current element target size.
       DISDEF=REAL(BEMTGT)
*** Set the prompt.
       CALL INPPRM('Solids','ADD-NOPRINT')
       IF(STDSTR('INPUT'))PRINT *,' ====== CELSOL INPUT   : Please'//
     -      ' enter the solids, terminate with a blank line.'
*** Read a line.
10     CONTINUE
       CALL INPWRD(NWORD)
       CALL INPSTR(1,1,STRING,NC)
*** If empty, leave the routine.
       IF(NWORD.EQ.0)THEN
            IF(.NOT.BEMSET)PRINT *,' ------ CELSOL MESSAGE : Not'//
     -           ' enough data has been entered for neBEM.'
            CALL INPPRM(' ','BACK-PRINT')
            RETURN
*** Could be an attempt to leave the section
       ELSEIF(STRING(1:1).EQ.'&')THEN
            PRINT *,' !!!!!! CELSOL WARNING : You can not'//
     -           ' leave the section here ; line is ignored.'
            GOTO 10
*** Could be a cylinder.
       ELSEIF(INPCMP(1,'CYL#INDER')+INPCMP(1,'WIRE').NE.0)THEN
*   Default parameters.
            XDIR=0
            YDIR=0
            ZDIR=1
            AROT=-PI/4
            IMAT=0
            AUXTYP='?'
            IF(INPCMP(1,'WIRE').NE.0)THEN
                 N=-1
            ELSE
                 N=0
            ENDIF
            VOLT=0
            CHARGE=0
            EPS=0
            IBOUND=0
            LVOLT=.FALSE.
            LCHA=.FALSE.
            LEPS=.FALSE.
            DIS1=DISDEF
            DIS2=DISDEF
            DIS3=DISDEF
            LTLID=.TRUE.
            LBLID=.TRUE.
            LRMEAN=.FALSE.
*   Required parameters.
            LRAD=.FALSE.
            LPOS=.FALSE.
            LSIZ=.FALSE.
*   Read the parameters.
            INEXT=2
            DO 20 I=2,NWORD
            IF(I.LT.INEXT)GOTO 20
*   Centre.
            IF(INPCMP(I,'CEN#TRE')+INPCMP(I,'CEN#TER').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                      CALL INPRDR(I+1,XPOS,0.0)
                      CALL INPRDR(I+2,YPOS,0.0)
                      CALL INPRDR(I+3,ZPOS,0.0)
                      LPOS=.TRUE.
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+4
*   Direction.
            ELSEIF(INPCMP(I,'DIR#ECTION').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                      CALL INPRDR(I+1,XDIR,0.0)
                      CALL INPRDR(I+2,YDIR,0.0)
                      CALL INPRDR(I+3,ZDIR,0.0)
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+4
*   Radius.
            ELSEIF(INPCMP(I,'R#ADIUS').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,AUX1,-1.0)
                      IF(AUX1.GT.0)THEN
                           R=AUX1
                           LRAD=.TRUE.
                      ELSE
                           CALL INPMSG(I+1,'Radius not positive.')
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
*   Half-length.
            ELSEIF(INPCMP(I,'HALF-#LENGTH').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,AUX1,-1.0)
                      IF(AUX1.GT.0)THEN
                           ZSIZ=AUX1
                           LSIZ=.TRUE.
                      ELSE
                           CALL INPMSG(I+1,'Half-length not positive.')
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
*   Rotation.
            ELSEIF(INPCMP(1,'CYL#INDER').NE.0.AND.
     -           INPCMP(I,'ROT#ATE').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,AUX1,-1.0)
                      AROT=AUX1*PI/180
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
*   Number of points: -1 = thin-wire, 0 = automatic, 2 = square
            ELSEIF(INPCMP(1,'CYL#INDER').NE.0.AND.
     -           INPCMP(I,'WIRE').NE.0)THEN
                 N=-1
            ELSEIF(INPCMP(1,'CYL#INDER').NE.0.AND.
     -           INPCMP(I,'N').NE.0)THEN
                 CALL INPCHK(I+1,1,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDI(I+1,NR,-1)
                      IF(NR.LE.1)THEN
                           CALL INPMSG(I+1,'Should be > 1.')
                      ELSE
                           N=NR
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
*   Lids and other options.
            ELSEIF(INPCMP(I,'TOP-LID').NE.0)THEN
                 LTLID=.TRUE.
            ELSEIF(INPCMP(I,'NOTOP-LID')+
     -           INPCMP(I,'NO-TOP-LID').NE.0)THEN
                 LTLID=.FALSE.
            ELSEIF(INPCMP(I,'BOT#TOM-LID').NE.0)THEN
                 LBLID=.TRUE.
            ELSEIF(INPCMP(I,'NOBOT#TOM-LID')+
     -           INPCMP(I,'NO-BOT#TOM-LID').NE.0)THEN
                 LBLID=.FALSE.
            ELSEIF(INPCMP(I,'LID#S').NE.0)THEN
                 LTLID=.TRUE.
                 LBLID=.TRUE.
            ELSEIF(INPCMP(I,'NOLID#S')+INPCMP(I,'NO-LID#S').NE.0)THEN
                 LTLID=.FALSE.
                 LBLID=.FALSE.
            ELSEIF(INPCMP(I,'MEAN-R#ADIUS').NE.0)THEN
                 LRMEAN=.TRUE.
            ELSEIF(INPCMP(I,'OUT#ER-R#ADIUS').NE.0)THEN
                 LRMEAN=.FALSE.
*   Material.
            ELSEIF(INPCMP(I,'CON#DUCTOR')+
     -           INPCMP(I,'CON#DUCTOR-1').NE.0)THEN
                 IMAT=1
            ELSEIF(INPCMP(I,'CON#DUCTOR-2').NE.0)THEN
                 IMAT=2
            ELSEIF(INPCMP(I,'CON#DUCTOR-3').NE.0)THEN
                 IMAT=3
            ELSEIF(INPCMP(1,'CYL#INDER').NE.0.AND.
     -           INPCMP(I,'DIEL#ECTRICUM')+
     -           INPCMP(I,'DIEL#ECTRICUM-1').NE.0)THEN
                 IMAT=11
            ELSEIF(INPCMP(1,'CYL#INDER').NE.0.AND.
     -           INPCMP(I,'DIEL#ECTRICUM-2').NE.0)THEN
                 IMAT=12
            ELSEIF(INPCMP(1,'CYL#INDER').NE.0.AND.
     -           INPCMP(I,'DIEL#ECTRICUM-3').NE.0)THEN
                 IMAT=13
*   Potential, dielectric constant, other boundary conditions.
            ELSEIF(INPCMP(I,'V#OLTAGE').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,VOLT,0.0)
                      LVOLT=.TRUE.
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(1,'CYL#INDER').NE.0.AND.
     -           INPCMP(I,'CH#ARGE')+INPCMP(I,'Q').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,CHARGE,0.0)
                      LCHA=.TRUE.
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(1,'CYL#INDER').NE.0.AND.
     -           INPCMP(I,'EPS#ILON').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,AUX1,-1.0)
                      IF(AUX1.GT.0)THEN
                           EPS=AUX1
                           LEPS=.TRUE.
                      ELSE
                           CALL INPMSG(I+1,'Epsilon is not positive.')
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'PARA#LLEL-#FIELD').NE.0)THEN
                 IBOUND=6
            ELSEIF(INPCMP(I,'PERP#ENDICULAR-#FIELD').NE.0)THEN
                 IBOUND=7
            ELSEIF(INPCMP(I,'FLOAT#ING-#CONDUCTOR').NE.0)THEN
                 IBOUND=3
*   Label.
            ELSEIF(INPCMP(I,'LAB#EL')+INPCMP(I,'TYPE').NE.0)THEN
                 CALL INPSTR(I+1,I+1,AUXTYP,NCAUX)
                 IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',AUXTYP).EQ.
     -                0)THEN
                      CALL INPMSG(1,'The label must be a letter.')
                      AUXTYP='?'
                 ENDIF
                 INEXT=I+2
*   Discretisation
            ELSEIF(INPCMP(I,'DIS#CRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS1=-1.0
                      DIS2=-1.0
                      DIS3=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS1=DISAUX
                                DIS2=DISAUX
                                DIS3=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(1,'CYL#INDER').NE.0.AND.
     -           INPCMP(I,'DIS#CRETISATION-LID-TOP')+
     -           INPCMP(I,'DIS#CRETISATION-TOP-#LID')+
     -           INPCMP(I,'TOP-#DISCRETISATION')+
     -           INPCMP(I,'TOP-#LID-#DISCRETISATION')+
     -           INPCMP(I,'LID-TOP-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS1=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS1=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(1,'CYL#INDER').NE.0.AND.
     -           INPCMP(I,'DIS#CRETISATION-LID-BOT#TOM')+
     -           INPCMP(I,'DIS#CRETISATION-BOT#TOM-#LID')+
     -           INPCMP(I,'BOT#TOM-#DISCRETISATION')+
     -           INPCMP(I,'BOT#TOM-#LID-#DISCRETISATION')+
     -           INPCMP(I,'LID-BOT#TOM-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS2=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS2=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(1,'CYL#INDER').NE.0.AND.
     -           INPCMP(I,'DIS#CRETISATION-BODY')+
     -           INPCMP(I,'BODY-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS3=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS3=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
*   Other things are not known.
            ELSE
                 CALL INPMSG(I,'Not a known keyword.')
            ENDIF
20          CONTINUE
*   Print error messages.
            CALL INPERR
*   Check compatibility of the options.
            IF(.NOT.(LVOLT.OR.LCHA.OR.LEPS.OR.IBOUND.GT.0))THEN
                 BEMSET=.FALSE.
            ELSEIF(LVOLT.AND.
     -           (LCHA.OR.LEPS.OR.IBOUND.EQ.6.OR.IBOUND.EQ.3.OR.
     -           (IMAT.GE.11.AND.IMAT.LE.20)))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Voltage boundary'//
     -                ' with incompatible attributes; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(LCHA.AND.(IBOUND.EQ.6.OR.IBOUND.EQ.7.OR.
     -           IBOUND.EQ.3))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Charge surface'//
     -                ' with incompatible attributes; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(LEPS.AND.(IBOUND.EQ.6.OR.IBOUND.EQ.7.OR.
     -           IBOUND.EQ.3.OR.(IMAT.GE.1.AND.IMAT.LE.10)))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Dielectric medium'//
     -                ' with incompatible attributes; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(IBOUND.EQ.6.AND.IMAT.GT.0)THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Parallel field'//
     -                ' symmetry on a volume; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(IBOUND.EQ.7.AND.(IMAT.GE.11.AND.IMAT.LE.20))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Perpendicular'//
     -                ' field on a dielectric; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF((IMAT.GE.1.AND.IMAT.LE.10).AND..NOT.
     -           (LVOLT.OR.LCHA.OR.IBOUND.EQ.3))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Conductor without'//
     -                ' suitable boundary conditions; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF((IMAT.GE.11.AND.IMAT.LE.20).AND..NOT.LEPS)THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Dielectric without'//
     -                ' epsilon; BEM disabled.'
                 BEMSET=.FALSE.
*   Otherwise complete the boundary conditions.
            ELSEIF(IBOUND.EQ.0.AND.LVOLT)THEN
                 IBOUND=1
            ELSEIF(IBOUND.EQ.0.AND.LCHA.AND.
     -           (LEPS.OR.(IMAT.GE.11.AND.IMAT.LE.20)))THEN
                 IBOUND=5
            ELSEIF(IBOUND.EQ.0.AND.LCHA)THEN
                 IBOUND=2
            ELSEIF(IBOUND.EQ.0.AND.
     -           (LEPS.OR.(IMAT.GE.11.AND.IMAT.LE.20)))THEN
                 IBOUND=4
            ELSEIF(IBOUND.EQ.0)THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Unexpected'//
     -                ' solid classification.'
            ENDIF
*   Complete materials if appropriate.
            IF(IMAT.EQ.0.AND.(LVOLT.OR.
     -           IBOUND.EQ.3.OR.IBOUND.EQ.7))IMAT=1
            IF(IMAT.EQ.0.AND.(LEPS.OR.LCHA.OR.IBOUND.EQ.5))IMAT=11
*   Enter in the conductor table.
            IF(LPOS.AND.LRAD.AND.LSIZ.AND.
     -           (NSOLID+1.GT.MXSOLI.OR.ICCURR+24.GT.MXSBUF))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Solids table'//
     -                ' is full; cylinder not stored.'
            ELSEIF(LPOS.AND.LRAD.AND.LSIZ)THEN
                 NSOLID=NSOLID+1
                 ISTART(NSOLID)=ICCURR
                 ISOLTP(NSOLID)=1
                 ISOLMT(NSOLID)=IMAT
                 SOLTYP(NSOLID)=AUXTYP
                 CBUF(ICCURR+1)=R
                 CBUF(ICCURR+2)=ZSIZ
                 CBUF(ICCURR+3)=XPOS
                 CBUF(ICCURR+4)=YPOS
                 CBUF(ICCURR+5)=ZPOS
                 CBUF(ICCURR+6)=XDIR
                 CBUF(ICCURR+7)=YDIR
                 CBUF(ICCURR+8)=ZDIR
                 CBUF(ICCURR+9)=DBLE(N)
*   Compute rotation angles.
                 IF(XDIR**2+YDIR**2.LE.0)THEN
                      PHI=0
                      IF(ZDIR.GT.0)THEN
                           THETA=0
                      ELSE
                           THETA=PI
                      ENDIF
                 ELSE
                      PHI=ATAN2(YDIR,XDIR)
                      THETA=ATAN2(SQRT(XDIR**2+YDIR**2),ZDIR)
                 ENDIF
                 CBUF(ICCURR+10)=COS(THETA)
                 CBUF(ICCURR+11)=SIN(THETA)
                 CBUF(ICCURR+12)=COS(PHI)
                 CBUF(ICCURR+13)=SIN(PHI)
*   Rotation angle of the object.
                 CBUF(ICCURR+14)=AROT
*   Boundary conditions
                 CBUF(ICCURR+15)=VOLT
                 CBUF(ICCURR+16)=EPS
                 CBUF(ICCURR+17)=DBLE(IBOUND)
                 CBUF(ICCURR+18)=CHARGE
*   Discretisation.
                 CBUF(ICCURR+19)=DIS1
                 CBUF(ICCURR+20)=DIS2
                 CBUF(ICCURR+21)=DIS3
*   Lids.
                 IF(LTLID)THEN
                      CBUF(ICCURR+22)=1
                 ELSE
                      CBUF(ICCURR+22)=0
                 ENDIF
                 IF(LBLID)THEN
                      CBUF(ICCURR+23)=1
                 ELSE
                      CBUF(ICCURR+23)=0
                 ENDIF
*   Mean radius option.
                 IF(LRMEAN)THEN
                      CBUF(ICCURR+24)=1
                 ELSE
                      CBUF(ICCURR+24)=0
                 ENDIF
*   Store size.
                 ICCURR=ICCURR+24
*   Or warn that some element is missing.
            ELSE
                 PRINT *,' !!!!!! CELSOL WARNING : Cylinder not'//
     -                ' entered because the position, the radius'//
     -                ' or the length has not been given.'
            ENDIF
*** Cylindrical hole.
       ELSEIF(INPCMP(1,'HOLE').NE.0)THEN
*   Default parameters.
            XDIR=0
            YDIR=0
            ZDIR=1
            IMAT=0
            N=0
            AUXTYP='?'
            VOLT=0
            CHARGE=0
            EPS=0
            IBOUND=0
            LVOLT=.FALSE.
            LCHA=.FALSE.
            LEPS=.FALSE.
            DIS1=DISDEF
            DIS2=DISDEF
            DIS3=DISDEF
            DIS4=DISDEF
            DIS5=DISDEF
            DIS6=DISDEF
            DIS7=DISDEF
            LRMEAN=.FALSE.
*   Required parameters.
            LRAD1=.FALSE.
            LRAD2=.FALSE.
            LPOS=.FALSE.
            LSIZ=.FALSE.
*   Read the parameters.
            INEXT=2
            DO 60 I=2,NWORD
            IF(I.LT.INEXT)GOTO 60
*   Centre.
            IF(INPCMP(I,'CEN#TRE')+INPCMP(I,'CEN#TER').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                      CALL INPRDR(I+1,XPOS,0.0)
                      CALL INPRDR(I+2,YPOS,0.0)
                      CALL INPRDR(I+3,ZPOS,0.0)
                      LPOS=.TRUE.
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+4
*   Direction.
            ELSEIF(INPCMP(I,'DIR#ECTION').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                      CALL INPRDR(I+1,XDIR,0.0)
                      CALL INPRDR(I+2,YDIR,0.0)
                      CALL INPRDR(I+3,ZDIR,0.0)
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+4
*   Number of points.
            ELSEIF(INPCMP(I,'N').NE.0)THEN
                 CALL INPCHK(I+1,1,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDI(I+1,NR,-1)
                      IF(NR.LE.1)THEN
                           CALL INPMSG(I+1,'Should be > 1.')
                      ELSE
                           N=NR
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
*   Radius or radii.
            ELSEIF(INPCMP(I,'R#ADIUS')+
     -           INPCMP(I,'R#ADII').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,AUX1,-1.0)
                      IF(AUX1.GT.0)THEN
                           R1=AUX1
                           R2=AUX1
                           LRAD1=.TRUE.
                           LRAD2=.TRUE.
                      ELSE
                           CALL INPMSG(I+1,'Radius not positive.')
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'UP#PER-R#ADIUS').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,AUX1,-1.0)
                      IF(AUX1.GT.0)THEN
                           R2=AUX1
                           LRAD2=.TRUE.
                      ELSE
                           CALL INPMSG(I+1,'Radius not positive.')
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'LOW#ER-R#ADIUS').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,AUX1,-1.0)
                      IF(AUX1.GT.0)THEN
                           R1=AUX1
                           LRAD1=.TRUE.
                      ELSE
                           CALL INPMSG(I+1,'Radius not positive.')
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
*   Half-lengths.
            ELSEIF(INPCMP(I,'HALF-#LENGTHS')+
     -           INPCMP(I,'HALF-#SIZES').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                      CALL INPRDR(I+1,AUX1,0.0)
                      CALL INPRDR(I+2,AUX2,0.0)
                      CALL INPRDR(I+3,AUX3,0.0)
                      IF(AUX1.GT.0.AND.AUX2.GT.0.AND.AUX3.GT.0)THEN
                           XSIZ=AUX1
                           YSIZ=AUX2
                           ZSIZ=AUX3
                           LSIZ=.TRUE.
                      ELSE
                           IF(AUX1.LE.0)CALL INPMSG(I+1,'Is not > 0.')
                           IF(AUX2.LE.0)CALL INPMSG(I+2,'Is not > 0.')
                           IF(AUX3.LE.0)CALL INPMSG(I+3,'Is not > 0.')
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+4
*   Options.
            ELSEIF(INPCMP(I,'MEAN-R#ADIUS')+
     -           INPCMP(I,'MEAN-R#ADII').NE.0)THEN
                 LRMEAN=.TRUE.
            ELSEIF(INPCMP(I,'OUT#ER-R#ADIUS')+
     -           INPCMP(I,'OUT#ER-R#ADII').NE.0)THEN
                 LRMEAN=.FALSE.
*   Material.
            ELSEIF(INPCMP(I,'CON#DUCTOR')+
     -           INPCMP(I,'CON#DUCTOR-1').NE.0)THEN
                 IMAT=1
            ELSEIF(INPCMP(I,'CON#DUCTOR-2').NE.0)THEN
                 IMAT=2
            ELSEIF(INPCMP(I,'CON#DUCTOR-3').NE.0)THEN
                 IMAT=3
            ELSEIF(INPCMP(I,'DIEL#ECTRICUM')+
     -           INPCMP(I,'DIEL#ECTRICUM-1').NE.0)THEN
                 IMAT=11
            ELSEIF(INPCMP(I,'DIEL#ECTRICUM-2').NE.0)THEN
                 IMAT=12
            ELSEIF(INPCMP(I,'DIEL#ECTRICUM-3').NE.0)THEN
                 IMAT=13
*   Potential, dielectric constant, other boundary conditions.
            ELSEIF(INPCMP(I,'V#OLTAGE').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,VOLT,0.0)
                      LVOLT=.TRUE.
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'CH#ARGE')+INPCMP(I,'Q').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,CHARGE,0.0)
                      LCHA=.TRUE.
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'EPS#ILON').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,AUX1,-1.0)
                      IF(AUX1.GT.0)THEN
                           EPS=AUX1
                           LEPS=.TRUE.
                      ELSE
                           CALL INPMSG(I+1,'Epsilon is not positive.')
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'PARA#LLEL-#FIELD').NE.0)THEN
                 IBOUND=6
            ELSEIF(INPCMP(I,'PERP#ENDICULAR-#FIELD').NE.0)THEN
                 IBOUND=7
            ELSEIF(INPCMP(I,'FLOAT#ING-#CONDUCTOR').NE.0)THEN
                 IBOUND=3
*   Label.
            ELSEIF(INPCMP(I,'LAB#EL')+INPCMP(I,'TYPE').NE.0)THEN
                 CALL INPSTR(I+1,I+1,AUXTYP,NCAUX)
                 IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',AUXTYP).EQ.
     -                0)THEN
                      CALL INPMSG(1,'The label must be a letter.')
                      AUXTYP='?'
                 ENDIF
                 INEXT=I+2
*   Discretisation
            ELSEIF(INPCMP(I,'DIS#CRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS1=-1.0
                      DIS2=-1.0
                      DIS3=-1.0
                      DIS4=-1.0
                      DIS5=-1.0
                      DIS6=-1.0
                      DIS7=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS1=DISAUX
                                DIS2=DISAUX
                                DIS3=DISAUX
                                DIS4=DISAUX
                                DIS5=DISAUX
                                DIS6=DISAUX
                                DIS7=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-FRONT')+
     -           INPCMP(I,'FRONT-#DISCRETISATION')+
     -           INPCMP(I,'DIS#CRETISATION-X-MAX#IMUM')+
     -           INPCMP(I,'X-MAX#IMUM-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS1=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS1=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-BACK')+
     -           INPCMP(I,'BACK-#DISCRETISATION')+
     -           INPCMP(I,'DIS#CRETISATION-X-MIN#IMUM')+
     -           INPCMP(I,'X-MIN#IMUM-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS2=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS2=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-RIGHT')+
     -           INPCMP(I,'RIGHT-#DISCRETISATION')+
     -           INPCMP(I,'DIS#CRETISATION-Y-MAX#IMUM')+
     -           INPCMP(I,'Y-MAX#IMUM-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS3=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS3=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-LEFT')+
     -           INPCMP(I,'LEFT-#DISCRETISATION')+
     -           INPCMP(I,'DIS#CRETISATION-Y-MIN#IMUM')+
     -           INPCMP(I,'Y-MIN#IMUM-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS4=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS4=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-TOP')+
     -           INPCMP(I,'TOP-#DISCRETISATION')+
     -           INPCMP(I,'DIS#CRETISATION-Z-MAX#IMUM')+
     -           INPCMP(I,'Z-MAX#IMUM-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS5=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS5=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-BOTTOM')+
     -           INPCMP(I,'BOTTOM-#DISCRETISATION')+
     -           INPCMP(I,'DIS#CRETISATION-Z-MIN#IMUM')+
     -           INPCMP(I,'Z-MIN#IMUM-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS6=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS6=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-CYL#INDER')+
     -           INPCMP(I,'CYL#INDER-#DISCRETISATION')+
     -           INPCMP(I,'DIS#CRETISATION-HOLE')+
     -           INPCMP(I,'HOLE-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS7=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS7=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
*   Other things are not known.
            ELSE
                 CALL INPMSG(I,'Not a known keyword.')
            ENDIF
60          CONTINUE
*   Print error messages.
            CALL INPERR
*   Check compatibility of the options.
            IF(.NOT.(LVOLT.OR.LCHA.OR.LEPS.OR.IBOUND.GT.0))THEN
                 BEMSET=.FALSE.
            ELSEIF(LVOLT.AND.
     -           (LCHA.OR.LEPS.OR.IBOUND.EQ.6.OR.IBOUND.EQ.3.OR.
     -           (IMAT.GE.11.AND.IMAT.LE.20)))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Voltage boundary'//
     -                ' with incompatible attributes; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(LCHA.AND.(IBOUND.EQ.6.OR.IBOUND.EQ.7.OR.
     -           IBOUND.EQ.3))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Charge surface'//
     -                ' with incompatible attributes; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(LEPS.AND.(IBOUND.EQ.6.OR.IBOUND.EQ.7.OR.
     -           IBOUND.EQ.3.OR.(IMAT.GE.1.AND.IMAT.LE.10)))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Dielectric medium'//
     -                ' with incompatible attributes; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(IBOUND.EQ.6.AND.IMAT.GT.0)THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Parallel field'//
     -                ' symmetry on a volume; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(IBOUND.EQ.7.AND.(IMAT.GE.11.AND.IMAT.LE.20))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Perpendicular'//
     -                ' field on a dielectric; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF((IMAT.GE.1.AND.IMAT.LE.10).AND..NOT.
     -           (LVOLT.OR.LCHA.OR.IBOUND.EQ.3))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Conductor without'//
     -                ' suitable boundary conditions; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF((IMAT.GE.11.AND.IMAT.LE.20).AND..NOT.LEPS)THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Dielectric without'//
     -                ' epsilon; BEM disabled.'
                 BEMSET=.FALSE.
*   Otherwise complete the boundary conditions.
            ELSEIF(IBOUND.EQ.0.AND.LVOLT)THEN
                 IBOUND=1
            ELSEIF(IBOUND.EQ.0.AND.LCHA.AND.
     -           (LEPS.OR.(IMAT.GE.11.AND.IMAT.LE.20)))THEN
                 IBOUND=5
            ELSEIF(IBOUND.EQ.0.AND.LCHA)THEN
                 IBOUND=2
            ELSEIF(IBOUND.EQ.0.AND.
     -           (LEPS.OR.(IMAT.GE.11.AND.IMAT.LE.20)))THEN
                 IBOUND=4
            ELSEIF(IBOUND.EQ.0)THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Unexpected'//
     -                ' solid classification.'
            ENDIF
*   Complete materials if appropriate.
            IF(IMAT.EQ.0.AND.(LVOLT.OR.
     -           IBOUND.EQ.3.OR.IBOUND.EQ.7))IMAT=1
            IF(IMAT.EQ.0.AND.(LEPS.OR.LCHA.OR.IBOUND.EQ.5))IMAT=11
*   Enter in the conductor table.
            IF(LPOS.AND.LRAD1.AND.LRAD2.AND.LSIZ.AND.
     -           (NSOLID+1.GT.MXSOLI.OR.ICCURR+28.GT.MXSBUF))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Solids table'//
     -                ' is full; hole not stored.'
            ELSEIF(LPOS.AND.LRAD1.AND.LRAD2.AND.LSIZ)THEN
                 NSOLID=NSOLID+1
                 ISTART(NSOLID)=ICCURR
                 ISOLTP(NSOLID)=2
                 ISOLMT(NSOLID)=IMAT
                 SOLTYP(NSOLID)=AUXTYP
                 CBUF(ICCURR+1)=R1
                 CBUF(ICCURR+2)=R2
                 CBUF(ICCURR+3)=XSIZ
                 CBUF(ICCURR+4)=YSIZ
                 CBUF(ICCURR+5)=ZSIZ
                 CBUF(ICCURR+6)=XPOS
                 CBUF(ICCURR+7)=YPOS
                 CBUF(ICCURR+8)=ZPOS
                 CBUF(ICCURR+9)=XDIR
                 CBUF(ICCURR+10)=YDIR
                 CBUF(ICCURR+11)=ZDIR
                 CBUF(ICCURR+12)=DBLE(N)
*   Compute rotation angles.
                 IF(XDIR**2+YDIR**2.LE.0)THEN
                      PHI=0
                      IF(ZDIR.GT.0)THEN
                           THETA=0
                      ELSE
                           THETA=PI
                      ENDIF
                 ELSE
                      PHI=ATAN2(YDIR,XDIR)
                      THETA=ATAN2(SQRT(XDIR**2+YDIR**2),ZDIR)
                 ENDIF
                 CBUF(ICCURR+13)=COS(THETA)
                 CBUF(ICCURR+14)=SIN(THETA)
                 CBUF(ICCURR+15)=COS(PHI)
                 CBUF(ICCURR+16)=SIN(PHI)
*   Boundary conditions
                 CBUF(ICCURR+17)=VOLT
                 CBUF(ICCURR+18)=EPS
                 CBUF(ICCURR+19)=DBLE(IBOUND)
                 CBUF(ICCURR+20)=CHARGE
*   Discretisation.
                 CBUF(ICCURR+21)=DIS1
                 CBUF(ICCURR+22)=DIS2
                 CBUF(ICCURR+23)=DIS3
                 CBUF(ICCURR+24)=DIS4
                 CBUF(ICCURR+25)=DIS5
                 CBUF(ICCURR+26)=DIS6
                 CBUF(ICCURR+27)=DIS7
*   Mean radius option.
                 IF(LRMEAN)THEN
                      CBUF(ICCURR+28)=1
                 ELSE
                      CBUF(ICCURR+28)=0
                 ENDIF
*   Store size.
                 ICCURR=ICCURR+28
*   Or warn that some element is missing.
            ELSE
                 PRINT *,' !!!!!! CELSOL WARNING : Hole not'//
     -                ' entered because the position, the radii'//
     -                ' or the box size has not been given.'
            ENDIF
*** Could be a box.
       ELSEIF(INPCMP(1,'BOX').NE.0)THEN
*   Default parameters.
            XDIR=0
            YDIR=0
            ZDIR=1
            IMAT=0
            AUXTYP='?'
            VOLT=0
            CHARGE=0
            EPS=0
            IBOUND=0
            LVOLT=.FALSE.
            LCHA=.FALSE.
            LEPS=.FALSE.
            DIS1=DISDEF
            DIS2=DISDEF
            DIS3=DISDEF
            DIS4=DISDEF
            DIS5=DISDEF
            DIS6=DISDEF
*   Required parameters.
            LSIZ=.FALSE.
            LPOS=.FALSE.
*   Read the parameters.
            INEXT=2
            DO 40 I=2,NWORD
            IF(I.LT.INEXT)GOTO 40
*   Centre.
            IF(INPCMP(I,'CEN#TRE')+INPCMP(I,'CEN#TER').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                      CALL INPRDR(I+1,XPOS,0.0)
                      CALL INPRDR(I+2,YPOS,0.0)
                      CALL INPRDR(I+3,ZPOS,0.0)
                      LPOS=.TRUE.
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+4
*   Direction.
            ELSEIF(INPCMP(I,'DIR#ECTION').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                      CALL INPRDR(I+1,XDIR,0.0)
                      CALL INPRDR(I+2,YDIR,0.0)
                      CALL INPRDR(I+3,ZDIR,0.0)
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+4
*   Half-lengths.
            ELSEIF(INPCMP(I,'HALF-#LENGTHS')+
     -           INPCMP(I,'HALF-#SIZES').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                      CALL INPRDR(I+1,AUX1,0.0)
                      CALL INPRDR(I+2,AUX2,0.0)
                      CALL INPRDR(I+3,AUX3,0.0)
                      IF((AUX1.EQ.0.AND.AUX2.EQ.0).OR.
     -                     (AUX1.EQ.0.AND.AUX3.EQ.0).OR.
     -                     (AUX2.EQ.0.AND.AUX3.EQ.0))THEN
                           CALL INPMSG(I+1,'Only one may be = 0.')
                           CALL INPMSG(I+2,'Only one may be = 0.')
                           CALL INPMSG(I+3,'Only one may be = 0.')
                      ELSEIF(AUX1.GE.0.AND.AUX2.GE.0.AND.AUX3.GE.0)THEN
                           XSIZ=AUX1
                           YSIZ=AUX2
                           ZSIZ=AUX3
                           LSIZ=.TRUE.
                      ELSE
                           IF(AUX1.LE.0)CALL INPMSG(I+1,'Is not >= 0.')
                           IF(AUX2.LE.0)CALL INPMSG(I+2,'Is not >= 0.')
                           IF(AUX3.LE.0)CALL INPMSG(I+3,'Is not >= 0.')
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+4
*   Material.
            ELSEIF(INPCMP(I,'CON#DUCTOR')+
     -           INPCMP(I,'CON#DUCTOR-1').NE.0)THEN
                 IMAT=1
            ELSEIF(INPCMP(I,'CON#DUCTOR-2').NE.0)THEN
                 IMAT=2
            ELSEIF(INPCMP(I,'CON#DUCTOR-3').NE.0)THEN
                 IMAT=3
            ELSEIF(INPCMP(I,'DIEL#ECTRICUM')+
     -           INPCMP(I,'DIEL#ECTRICUM-1').NE.0)THEN
                 IMAT=11
            ELSEIF(INPCMP(I,'DIEL#ECTRICUM-2').NE.0)THEN
                 IMAT=12
            ELSEIF(INPCMP(I,'DIEL#ECTRICUM-3').NE.0)THEN
                 IMAT=13
*   Potential, dielectric constant, other boundary conditions.
            ELSEIF(INPCMP(I,'V#OLTAGE').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,VOLT,0.0)
                      LVOLT=.TRUE.
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'CH#ARGE')+INPCMP(I,'Q').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,CHARGE,0.0)
                      LCHA=.TRUE.
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'EPS#ILON').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,AUX1,-1.0)
                      IF(AUX1.GT.0)THEN
                           EPS=AUX1
                           LEPS=.TRUE.
                      ELSE
                           CALL INPMSG(I+1,'Epsilon is not positive.')
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'PARA#LLEL-#FIELD').NE.0)THEN
                 IBOUND=6
            ELSEIF(INPCMP(I,'PERP#ENDICULAR-#FIELD').NE.0)THEN
                 IBOUND=7
            ELSEIF(INPCMP(I,'FLOAT#ING-#CONDUCTOR').NE.0)THEN
                 IBOUND=3
*   Label.
            ELSEIF(INPCMP(I,'LAB#EL')+INPCMP(I,'TYPE').NE.0)THEN
                 CALL INPSTR(I+1,I+1,AUXTYP,NCAUX)
                 IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',AUXTYP).EQ.
     -                0)THEN
                      CALL INPMSG(1,'The label must be a letter.')
                      AUXTYP='?'
                 ENDIF
                 INEXT=I+2
*   Discretisation
            ELSEIF(INPCMP(I,'DIS#CRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS1=-1.0
                      DIS2=-1.0
                      DIS3=-1.0
                      DIS4=-1.0
                      DIS5=-1.0
                      DIS6=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS1=DISAUX
                                DIS2=DISAUX
                                DIS3=DISAUX
                                DIS4=DISAUX
                                DIS5=DISAUX
                                DIS6=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-FRONT')+
     -           INPCMP(I,'FRONT-#DISCRETISATION')+
     -           INPCMP(I,'DIS#CRETISATION-X-MAX#IMUM')+
     -           INPCMP(I,'X-MAX#IMUM-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS1=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS1=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-BACK')+
     -           INPCMP(I,'BACK-#DISCRETISATION')+
     -           INPCMP(I,'DIS#CRETISATION-X-MIN#IMUM')+
     -           INPCMP(I,'X-MIN#IMUM-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS2=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS2=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-RIGHT')+
     -           INPCMP(I,'RIGHT-#DISCRETISATION')+
     -           INPCMP(I,'DIS#CRETISATION-Y-MAX#IMUM')+
     -           INPCMP(I,'Y-MAX#IMUM-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS3=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS3=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-LEFT')+
     -           INPCMP(I,'LEFT-#DISCRETISATION')+
     -           INPCMP(I,'DIS#CRETISATION-Y-MIN#IMUM')+
     -           INPCMP(I,'Y-MIN#IMUM-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS4=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS4=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-TOP')+
     -           INPCMP(I,'TOP-#DISCRETISATION')+
     -           INPCMP(I,'DIS#CRETISATION-Z-MAX#IMUM')+
     -           INPCMP(I,'Z-MAX#IMUM-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS5=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS5=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-BOTTOM')+
     -           INPCMP(I,'BOTTOM-#DISCRETISATION')+
     -           INPCMP(I,'DIS#CRETISATION-Z-MIN#IMUM')+
     -           INPCMP(I,'Z-MIN#IMUM-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS6=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS6=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
*   Other things are not known.
            ELSE
                 CALL INPMSG(I,'Not a known keyword.')
            ENDIF
40          CONTINUE
*   Print error messages.
            CALL INPERR
*   Check compatibility of the options.
            IF(.NOT.(LVOLT.OR.LCHA.OR.LEPS.OR.IBOUND.GT.0))THEN
                 BEMSET=.FALSE.
            ELSEIF(LVOLT.AND.
     -           (LCHA.OR.LEPS.OR.IBOUND.EQ.6.OR.IBOUND.EQ.3.OR.
     -           (IMAT.GE.11.AND.IMAT.LE.20)))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Voltage boundary'//
     -                ' with incompatible attributes; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(LCHA.AND.(IBOUND.EQ.6.OR.IBOUND.EQ.7.OR.
     -           IBOUND.EQ.3))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Charge surface'//
     -                ' with incompatible attributes; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(LEPS.AND.(IBOUND.EQ.6.OR.IBOUND.EQ.7.OR.
     -           IBOUND.EQ.3.OR.(IMAT.GE.1.AND.IMAT.LE.10)))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Dielectric medium'//
     -                ' with incompatible attributes; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(IBOUND.EQ.6.AND.IMAT.GT.0)THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Parallel field'//
     -                ' symmetry on a volume; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(IBOUND.EQ.7.AND.(IMAT.GE.11.AND.IMAT.LE.20))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Perpendicular'//
     -                ' field on a dielectric; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF((IMAT.GE.1.AND.IMAT.LE.10).AND..NOT.
     -           (LVOLT.OR.LCHA.OR.IBOUND.EQ.3))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Conductor without'//
     -                ' suitable boundary conditions; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF((IMAT.GE.11.AND.IMAT.LE.20).AND..NOT.LEPS)THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Dielectric without'//
     -                ' epsilon; BEM disabled.'
                 BEMSET=.FALSE.
*   Otherwise complete the boundary conditions.
            ELSEIF(IBOUND.EQ.0.AND.LVOLT)THEN
                 IBOUND=1
            ELSEIF(IBOUND.EQ.0.AND.LCHA.AND.
     -           (LEPS.OR.(IMAT.GE.11.AND.IMAT.LE.20)))THEN
                 IBOUND=5
            ELSEIF(IBOUND.EQ.0.AND.LCHA)THEN
                 IBOUND=2
            ELSEIF(IBOUND.EQ.0.AND.
     -           (LEPS.OR.(IMAT.GE.11.AND.IMAT.LE.20)))THEN
                 IBOUND=4
            ELSEIF(IBOUND.EQ.0)THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Unexpected'//
     -                ' solid classification.'
            ENDIF
*   Complete materials if appropriate.
            IF(IMAT.EQ.0.AND.(LVOLT.OR.
     -           IBOUND.EQ.3.OR.IBOUND.EQ.7))IMAT=1
            IF(IMAT.EQ.0.AND.(LEPS.OR.LCHA.OR.IBOUND.EQ.5))IMAT=11
*   Enter in the conductor table.
            IF(LPOS.AND.LSIZ.AND.
     -           (NSOLID+1.GT.MXSOLI.OR.ICCURR+23.GT.MXSBUF))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Solids table'//
     -                ' is full; box not stored.'
            ELSEIF(LPOS.AND.LSIZ)THEN
                 NSOLID=NSOLID+1
                 ISTART(NSOLID)=ICCURR
                 ISOLTP(NSOLID)=3
                 ISOLMT(NSOLID)=IMAT
                 SOLTYP(NSOLID)=AUXTYP
                 CBUF(ICCURR+1)=XSIZ
                 CBUF(ICCURR+2)=YSIZ
                 CBUF(ICCURR+3)=ZSIZ
                 CBUF(ICCURR+4)=XPOS
                 CBUF(ICCURR+5)=YPOS
                 CBUF(ICCURR+6)=ZPOS
                 CBUF(ICCURR+7)=XDIR
                 CBUF(ICCURR+8)=YDIR
                 CBUF(ICCURR+9)=ZDIR
*   Compute rotation angles.
                 IF(XDIR**2+YDIR**2.LE.0)THEN
                      PHI=0
                      IF(ZDIR.GT.0)THEN
                           THETA=0
                      ELSE
                           THETA=PI
                      ENDIF
                 ELSE
                      PHI=ATAN2(YDIR,XDIR)
                      THETA=ATAN2(SQRT(XDIR**2+YDIR**2),ZDIR)
                 ENDIF
                 CBUF(ICCURR+10)=COS(THETA)
                 CBUF(ICCURR+11)=SIN(THETA)
                 CBUF(ICCURR+12)=COS(PHI)
                 CBUF(ICCURR+13)=SIN(PHI)
*   Boundary conditions
                 CBUF(ICCURR+14)=VOLT
                 CBUF(ICCURR+15)=EPS
                 CBUF(ICCURR+16)=DBLE(IBOUND)
                 CBUF(ICCURR+17)=CHARGE
*   Discretisation.
                 CBUF(ICCURR+18)=DIS1
                 CBUF(ICCURR+19)=DIS2
                 CBUF(ICCURR+20)=DIS3
                 CBUF(ICCURR+21)=DIS4
                 CBUF(ICCURR+22)=DIS5
                 CBUF(ICCURR+23)=DIS6
*   Store size
                 ICCURR=ICCURR+23
*   Or warn that some element is missing.
            ELSE
                 PRINT *,' !!!!!! CELSOL WARNING : Box not'//
     -                ' entered because the position or the size'//
     -                ' has not been given.'
            ENDIF
*** Could also be sphere.
       ELSEIF(INPCMP(1,'SPHERE').NE.0)THEN
*   Required parameters.
            LRAD=.FALSE.
            LPOS=.FALSE.
            N=0
            IMAT=0
            AUXTYP='?'
            VOLT=0
            CHARGE=0
            EPS=0
            IBOUND=0
            LVOLT=.FALSE.
            LCHA=.FALSE.
            LEPS=.FALSE.
            DIS1=DISDEF
*   Read the parameters.
            INEXT=2
            DO 50 I=2,NWORD
            IF(I.LT.INEXT)GOTO 50
*   Centre.
            IF(INPCMP(I,'CEN#TRE')+INPCMP(I,'CEN#TER').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                      CALL INPRDR(I+1,XPOS,0.0)
                      CALL INPRDR(I+2,YPOS,0.0)
                      CALL INPRDR(I+3,ZPOS,0.0)
                      LPOS=.TRUE.
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+4
*   Radius.
            ELSEIF(INPCMP(I,'R#ADIUS').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,AUX1,-1.0)
                      IF(AUX1.GT.0)THEN
                           R=AUX1
                           LRAD=.TRUE.
                      ELSE
                           CALL INPMSG(I+1,'Radius not positive.')
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
*   Material.
            ELSEIF(INPCMP(I,'CON#DUCTOR')+
     -           INPCMP(I,'CON#DUCTOR-1').NE.0)THEN
                 IMAT=1
            ELSEIF(INPCMP(I,'CON#DUCTOR-2').NE.0)THEN
                 IMAT=2
            ELSEIF(INPCMP(I,'CON#DUCTOR-3').NE.0)THEN
                 IMAT=3
            ELSEIF(INPCMP(I,'DIEL#ECTRICUM')+
     -           INPCMP(I,'DIEL#ECTRICUM-1').NE.0)THEN
                 IMAT=11
            ELSEIF(INPCMP(I,'DIEL#ECTRICUM-2').NE.0)THEN
                 IMAT=12
            ELSEIF(INPCMP(I,'DIEL#ECTRICUM-3').NE.0)THEN
                 IMAT=13
*   Number of points.
            ELSEIF(INPCMP(I,'N').NE.0)THEN
                 CALL INPCHK(I+1,1,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDI(I+1,NR,-1)
                      IF(NR.LE.2)THEN
                           CALL INPMSG(I+1,'Should be > 2.')
                      ELSE
                           N=NR
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
*   Potential, dielectric constant, other boundary conditions.
            ELSEIF(INPCMP(I,'V#OLTAGE').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,VOLT,0.0)
                     LVOLT=.TRUE.
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'CH#ARGE').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,CHARGE,0.0)
                      LCHA=.TRUE.
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'EPS#ILON').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,AUX1,-1.0)
                      IF(AUX1.GT.0)THEN
                           EPS=AUX1
                           LEPS=.TRUE.
                      ELSE
                           CALL INPMSG(I+1,'Epsilon is not positive.')
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'PARA#LLEL-#FIELD').NE.0)THEN
                 IBOUND=6
            ELSEIF(INPCMP(I,'PERP#ENDICULAR-#FIELD').NE.0)THEN
                 IBOUND=7
            ELSEIF(INPCMP(I,'FLOAT#ING-#CONDUCTOR').NE.0)THEN
                 IBOUND=3
*   Label.
            ELSEIF(INPCMP(I,'LAB#EL')+INPCMP(I,'TYPE').NE.0)THEN
                 CALL INPSTR(I+1,I+1,AUXTYP,NCAUX)
                 IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',AUXTYP).EQ.
     -                0)THEN
                      CALL INPMSG(1,'The label must be a letter.')
                      AUXTYP='?'
                 ENDIF
                 INEXT=I+2
*   Discretisation
            ELSEIF(INPCMP(I,'DIS#CRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS1=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS1=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
*   Other things are not known.
            ELSE
                 CALL INPMSG(I,'Not a known keyword.')
            ENDIF
50          CONTINUE
*   Print error messages.
            CALL INPERR
*   Check compatibility of the options.
            IF(.NOT.(LVOLT.OR.LCHA.OR.LEPS.OR.IBOUND.GT.0))THEN
                 BEMSET=.FALSE.
            ELSEIF(LVOLT.AND.
     -           (LCHA.OR.LEPS.OR.IBOUND.EQ.6.OR.IBOUND.EQ.3.OR.
     -           (IMAT.GE.11.AND.IMAT.LE.20)))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Voltage boundary'//
     -                ' with incompatible attributes; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(LCHA.AND.(IBOUND.EQ.6.OR.IBOUND.EQ.7.OR.
     -           IBOUND.EQ.3))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Charge surface'//
     -                ' with incompatible attributes; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(LEPS.AND.(IBOUND.EQ.6.OR.IBOUND.EQ.7.OR.
     -           IBOUND.EQ.3.OR.(IMAT.GE.1.AND.IMAT.LE.10)))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Dielectric medium'//
     -                ' with incompatible attributes; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(IBOUND.EQ.6.AND.IMAT.GT.0)THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Parallel field'//
     -                ' symmetry on a volume; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(IBOUND.EQ.7.AND.(IMAT.GE.11.AND.IMAT.LE.20))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Perpendicular'//
     -                ' field on a dielectric; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF((IMAT.GE.1.AND.IMAT.LE.10).AND..NOT.
     -           (LVOLT.OR.LCHA.OR.IBOUND.EQ.3))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Conductor without'//
     -                ' suitable boundary conditions; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF((IMAT.GE.11.AND.IMAT.LE.20).AND..NOT.LEPS)THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Dielectric without'//
     -                ' epsilon; BEM disabled.'
                 BEMSET=.FALSE.
*   Otherwise complete the boundary conditions.
            ELSEIF(IBOUND.EQ.0.AND.LVOLT)THEN
                 IBOUND=1
            ELSEIF(IBOUND.EQ.0.AND.LCHA.AND.
     -           (LEPS.OR.(IMAT.GE.11.AND.IMAT.LE.20)))THEN
                 IBOUND=5
            ELSEIF(IBOUND.EQ.0.AND.LCHA)THEN
                 IBOUND=2
            ELSEIF(IBOUND.EQ.0.AND.
     -           (LEPS.OR.(IMAT.GE.11.AND.IMAT.LE.20)))THEN
                 IBOUND=4
            ELSEIF(IBOUND.EQ.0)THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Unexpected'//
     -                ' solid classification.'
            ENDIF
*   Complete materials if appropriate.
            IF(IMAT.EQ.0.AND.(LVOLT.OR.
     -           IBOUND.EQ.3.OR.IBOUND.EQ.7))IMAT=1
            IF(IMAT.EQ.0.AND.(LEPS.OR.LCHA.OR.IBOUND.EQ.5))IMAT=11
*   Enter in the conductor table.
            IF(LPOS.AND.LRAD.AND.
     -           (NSOLID+1.GT.MXSOLI.OR.ICCURR+10.GT.MXSBUF))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Solids table'//
     -                ' is full; sphere not stored.'
            ELSEIF(LPOS.AND.LRAD)THEN
                 NSOLID=NSOLID+1
                 ISTART(NSOLID)=ICCURR
                 ISOLTP(NSOLID)=4
                 ISOLMT(NSOLID)=IMAT
                 SOLTYP(NSOLID)=AUXTYP
                 CBUF(ICCURR+1)=R
                 CBUF(ICCURR+2)=XPOS
                 CBUF(ICCURR+3)=YPOS
                 CBUF(ICCURR+4)=ZPOS
                 CBUF(ICCURR+5)=DBLE(N)
*   Boundary conditions
                 CBUF(ICCURR+6)=VOLT
                 CBUF(ICCURR+7)=EPS
                 CBUF(ICCURR+8)=DBLE(IBOUND)
                 CBUF(ICCURR+9)=CHARGE
*   Discretisation.
                 CBUF(ICCURR+10)=DIS1
*   Store size
                 ICCURR=ICCURR+10
*   Or warn that some element is missing.
            ELSE
                 PRINT *,' !!!!!! CELSOL WARNING : Sphere not'//
     -                ' entered because the position or the radius'//
     -                ' has not been given.'
            ENDIF
*** Could be a Toblerone.
       ELSEIF(INPCMP(1,'TOBLER#ONE')+INPCMP(1,'RIDGE')+
     -      INPCMP(1,'ROOF').NE.0)THEN
*   Default parameters.
            XDIR=0
            YDIR=0
            ZDIR=1
            IMAT=0
            AUXTYP='?'
            VOLT=0
            CHARGE=0
            EPS=0
            IBOUND=0
            LVOLT=.FALSE.
            LCHA=.FALSE.
            LEPS=.FALSE.
            DIS1=DISDEF
            DIS2=DISDEF
            DIS3=DISDEF
            DIS4=DISDEF
            DIS5=DISDEF
*   Required parameters.
            LNOTCH=.FALSE.
            LPOS=.FALSE.
            LSIZ=.FALSE.
*   Read the parameters.
            INEXT=2
            DO 30 I=2,NWORD
            IF(I.LT.INEXT)GOTO 30
*   Centre.
            IF(INPCMP(I,'CEN#TRE')+INPCMP(I,'CEN#TER').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                      CALL INPRDR(I+1,XPOS,0.0)
                      CALL INPRDR(I+2,YPOS,0.0)
                      CALL INPRDR(I+3,ZPOS,0.0)
                      LPOS=.TRUE.
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+4
*   Direction.
            ELSEIF(INPCMP(I,'DIR#ECTION').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                      CALL INPRDR(I+1,XDIR,0.0)
                      CALL INPRDR(I+2,YDIR,0.0)
                      CALL INPRDR(I+3,ZDIR,0.0)
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+4
*   Notch.
            ELSEIF(INPCMP(I,'NOTCH').NE.0.OR.
     -           INPCMP(I,'RIDGE').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0)THEN
                      CALL INPRDR(I+1,XNOTCH,0.0)
                      CALL INPRDR(I+2,ZNOTCH,0.0)
                      IF(ZNOTCH.GT.0)THEN
                           LNOTCH=.TRUE.
                      ELSE
                           CALL INPMSG(I,'Ridge height not > 0.')
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+3
*   Half-lengths.
            ELSEIF(INPCMP(I,'HALF-#LENGTHS')+
     -           INPCMP(I,'HALF-#SIZES').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0)THEN
                      CALL INPRDR(I+1,AUX1,0.0)
                      CALL INPRDR(I+2,AUX2,0.0)
                      IF(AUX1.GT.0.AND.AUX2.GT.0)THEN
                           XSIZ=AUX1
                           YSIZ=AUX2
                           LSIZ=.TRUE.
                      ELSE
                           IF(AUX1.LE.0)CALL INPMSG(I+1,'Is not > 0.')
                           IF(AUX2.LE.0)CALL INPMSG(I+2,'Is not > 0.')
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+3
*   Material.
            ELSEIF(INPCMP(I,'CON#DUCTOR')+
     -           INPCMP(I,'CON#DUCTOR-1').NE.0)THEN
                 IMAT=1
            ELSEIF(INPCMP(I,'CON#DUCTOR-2').NE.0)THEN
                 IMAT=2
            ELSEIF(INPCMP(I,'CON#DUCTOR-3').NE.0)THEN
                 IMAT=3
            ELSEIF(INPCMP(I,'DIEL#ECTRICUM')+
     -           INPCMP(I,'DIEL#ECTRICUM-1').NE.0)THEN
                 IMAT=11
            ELSEIF(INPCMP(I,'DIEL#ECTRICUM-2').NE.0)THEN
                 IMAT=12
            ELSEIF(INPCMP(I,'DIEL#ECTRICUM-3').NE.0)THEN
                 IMAT=13
*   Potential, dielectric constant, other boundary conditions.
            ELSEIF(INPCMP(I,'V#OLTAGE').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,VOLT,0.0)
                      LVOLT=.TRUE.
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'CH#ARGE')+INPCMP(I,'Q').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,CHARGE,0.0)
                      LCHA=.TRUE.
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'EPS#ILON').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,AUX1,-1.0)
                      IF(AUX1.GT.0)THEN
                           EPS=AUX1
                           LEPS=.TRUE.
                      ELSE
                           CALL INPMSG(I+1,'Epsilon is not positive.')
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'PARA#LLEL-#FIELD').NE.0)THEN
                 IBOUND=6
            ELSEIF(INPCMP(I,'PERP#ENDICULAR-#FIELD').NE.0)THEN
                 IBOUND=7
            ELSEIF(INPCMP(I,'FLOAT#ING-#CONDUCTOR').NE.0)THEN
                 IBOUND=3
*   Label.
            ELSEIF(INPCMP(I,'LAB#EL')+INPCMP(I,'TYPE').NE.0)THEN
                 CALL INPSTR(I+1,I+1,AUXTYP,NCAUX)
                 IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',AUXTYP).EQ.
     -                0)THEN
                      CALL INPMSG(1,'The label must be a letter.')
                      AUXTYP='?'
                 ENDIF
                 INEXT=I+2
*   Discretisation
            ELSEIF(INPCMP(I,'DIS#CRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS1=-1.0
                      DIS2=-1.0
                      DIS3=-1.0
                      DIS4=-1.0
                      DIS5=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS1=DISAUX
                                DIS2=DISAUX
                                DIS3=DISAUX
                                DIS4=DISAUX
                                DIS5=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-FRONT')+
     -           INPCMP(I,'FRONT-#DISCRETISATION')+
     -           INPCMP(I,'DIS#CRETISATION-X-MAX#IMUM')+
     -           INPCMP(I,'X-MAX#IMUM-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS1=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS1=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-BACK')+
     -           INPCMP(I,'BACK-#DISCRETISATION')+
     -           INPCMP(I,'DIS#CRETISATION-X-MIN#IMUM')+
     -           INPCMP(I,'X-MIN#IMUM-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS2=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS2=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-RIGHT')+
     -           INPCMP(I,'RIGHT-#DISCRETISATION')+
     -           INPCMP(I,'DIS#CRETISATION-Y-MAX#IMUM')+
     -           INPCMP(I,'Y-MAX#IMUM-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS3=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS3=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-LEFT')+
     -           INPCMP(I,'LEFT-#DISCRETISATION')+
     -           INPCMP(I,'DIS#CRETISATION-Y-MIN#IMUM')+
     -           INPCMP(I,'Y-MIN#IMUM-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS4=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS4=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-FLOOR')+
     -           INPCMP(I,'FLOOR-#DISCRETISATION')+
     -           INPCMP(I,'DIS#CRETISATION-Z-MIN#IMUM')+
     -           INPCMP(I,'Z-MIN#IMUM-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS5=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS5=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
*   Other things are not known.
            ELSE
                 CALL INPMSG(I,'Not a known keyword.')
            ENDIF
30          CONTINUE
*   Print error messages.
            CALL INPERR
*   Check compatibility of the options.
            IF(.NOT.(LVOLT.OR.LCHA.OR.LEPS.OR.IBOUND.GT.0))THEN
                 BEMSET=.FALSE.
            ELSEIF(LVOLT.AND.
     -           (LCHA.OR.LEPS.OR.IBOUND.EQ.6.OR.IBOUND.EQ.3.OR.
     -           (IMAT.GE.11.AND.IMAT.LE.20)))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Voltage boundary'//
     -                ' with incompatible attributes; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(LCHA.AND.(IBOUND.EQ.6.OR.IBOUND.EQ.7.OR.
     -           IBOUND.EQ.3))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Charge surface'//
     -                ' with incompatible attributes; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(LEPS.AND.(IBOUND.EQ.6.OR.IBOUND.EQ.7.OR.
     -           IBOUND.EQ.3.OR.(IMAT.GE.1.AND.IMAT.LE.10)))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Dielectric medium'//
     -                ' with incompatible attributes; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(IBOUND.EQ.6.AND.IMAT.GT.0)THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Parallel field'//
     -                ' symmetry on a volume; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(IBOUND.EQ.7.AND.(IMAT.GE.11.AND.IMAT.LE.20))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Perpendicular'//
     -                ' field on a dielectric; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF((IMAT.GE.1.AND.IMAT.LE.10).AND..NOT.
     -           (LVOLT.OR.LCHA.OR.IBOUND.EQ.3))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Conductor without'//
     -                ' suitable boundary conditions; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF((IMAT.GE.11.AND.IMAT.LE.20).AND..NOT.LEPS)THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Dielectric without'//
     -                ' epsilon; BEM disabled.'
                 BEMSET=.FALSE.
*   Otherwise complete the boundary conditions.
            ELSEIF(IBOUND.EQ.0.AND.LVOLT)THEN
                 IBOUND=1
            ELSEIF(IBOUND.EQ.0.AND.LCHA.AND.
     -           (LEPS.OR.(IMAT.GE.11.AND.IMAT.LE.20)))THEN
                 IBOUND=5
            ELSEIF(IBOUND.EQ.0.AND.LCHA)THEN
                 IBOUND=2
            ELSEIF(IBOUND.EQ.0.AND.
     -           (LEPS.OR.(IMAT.GE.11.AND.IMAT.LE.20)))THEN
                 IBOUND=4
            ELSEIF(IBOUND.EQ.0)THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Unexpected'//
     -                ' solid classification.'
            ENDIF
*   Complete materials if appropriate.
            IF(IMAT.EQ.0.AND.(LVOLT.OR.
     -           IBOUND.EQ.3.OR.IBOUND.EQ.7))IMAT=1
            IF(IMAT.EQ.0.AND.(LEPS.OR.LCHA.OR.IBOUND.EQ.5))IMAT=11
*   Enter in the conductor table.
            IF(LPOS.AND.LSIZ.AND.
     -           (NSOLID+1.GT.MXSOLI.OR.ICCURR+23.GT.MXSBUF))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Solids table'//
     -                ' is full; ridge not stored.'
            ELSEIF(LPOS.AND.LSIZ.AND.LNOTCH)THEN
                 NSOLID=NSOLID+1
                 ISTART(NSOLID)=ICCURR
                 ISOLTP(NSOLID)=5
                 ISOLMT(NSOLID)=IMAT
                 SOLTYP(NSOLID)=AUXTYP
                 CBUF(ICCURR+1)=XSIZ
                 CBUF(ICCURR+2)=YSIZ
                 CBUF(ICCURR+3)=ZNOTCH
                 CBUF(ICCURR+4)=XPOS
                 CBUF(ICCURR+5)=YPOS
                 CBUF(ICCURR+6)=ZPOS
                 CBUF(ICCURR+7)=XDIR
                 CBUF(ICCURR+8)=YDIR
                 CBUF(ICCURR+9)=ZDIR
*   Compute rotation angles.
                 IF(XDIR**2+YDIR**2.LE.0)THEN
                      PHI=0
                      IF(ZDIR.GT.0)THEN
                           THETA=0
                      ELSE
                           THETA=PI
                      ENDIF
                 ELSE
                      PHI=ATAN2(YDIR,XDIR)
                      THETA=ATAN2(SQRT(XDIR**2+YDIR**2),ZDIR)
                 ENDIF
                 CBUF(ICCURR+10)=COS(THETA)
                 CBUF(ICCURR+11)=SIN(THETA)
                 CBUF(ICCURR+12)=COS(PHI)
                 CBUF(ICCURR+13)=SIN(PHI)
*   x-location of the notch.
                 CBUF(ICCURR+14)=XNOTCH
*   Boundary conditions
                 CBUF(ICCURR+15)=VOLT
                 CBUF(ICCURR+16)=EPS
                 CBUF(ICCURR+17)=DBLE(IBOUND)
                 CBUF(ICCURR+18)=CHARGE
*   Discretisation.
                 CBUF(ICCURR+19)=DIS1
                 CBUF(ICCURR+20)=DIS2
                 CBUF(ICCURR+21)=DIS3
                 CBUF(ICCURR+22)=DIS4
                 CBUF(ICCURR+23)=DIS5
*   Store size.
                 ICCURR=ICCURR+23
*   Or warn that some element is missing.
            ELSE
                 PRINT *,' !!!!!! CELSOL WARNING : Ridge not'//
     -                ' entered because the position, the radius'//
     -                ' or the length has not been given.'
            ENDIF
*** Could be a extrusion.
       ELSEIF(INPCMP(1,'EXT#RUSION').NE.0)THEN
*   Default parameters.
            XDIR=0
            YDIR=0
            ZDIR=1
            IMAT=0
            AUXTYP='?'
            NPROF=0
            VOLT=0
            CHARGE=0
            EPS=0
            IBOUND=0
            LVOLT=.FALSE.
            LCHA=.FALSE.
            LEPS=.FALSE.
            DIS1=DISDEF
            DIS2=DISDEF
            DIS3=DISDEF
            LTLID=.TRUE.
            LBLID=.TRUE.
            LRMEAN=.FALSE.
            IORI=0
*   Required parameters.
            LPOS=.FALSE.
            LSIZ=.FALSE.
            LEPROF=.FALSE.
*   Read the parameters.
            INEXT=2
            DO 70 I=2,NWORD
            IF(I.LT.INEXT)GOTO 70
*   Centre.
            IF(INPCMP(I,'CEN#TRE')+INPCMP(I,'CEN#TER').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                      CALL INPRDR(I+1,XPOS,0.0)
                      CALL INPRDR(I+2,YPOS,0.0)
                      CALL INPRDR(I+3,ZPOS,0.0)
                      LPOS=.TRUE.
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+4
*   Direction.
            ELSEIF(INPCMP(I,'DIR#ECTION').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                      CALL INPRDR(I+1,XDIR,0.0)
                      CALL INPRDR(I+2,YDIR,0.0)
                      CALL INPRDR(I+3,ZDIR,0.0)
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+4
*   Half-length.
            ELSEIF(INPCMP(I,'HALF-#LENGTH').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,AUX1,-1.0)
                      IF(AUX1.GT.0)THEN
                           ZSIZ=AUX1
                           LSIZ=.TRUE.
                      ELSE
                           CALL INPMSG(I+1,'Half-length not positive.')
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
*   Profile.
            ELSEIF(INPCMP(I,'PRO#FILE').NE.0)THEN
                 NPROF=0
                 XPRMIN=0
                 IPRMIN=0
                 DO 80 J=I+1,NWORD,2
                 IF(  (INPTYP(J)  .EQ.1.OR.INPTYP(J  ).EQ.2).AND.
     -                (INPTYP(J+1).EQ.1.OR.INPTYP(J+1).EQ.2))THEN
                      IF(NPROF+1.GT.MXEDGE)THEN
                           PRINT *,' !!!!!! CELSOL WARNING : Profile'//
     -                          ' has more points than can be stored.'
                           LEPROF=.FALSE.
                           GOTO 90
                      ENDIF
                      CALL INPCHK(J,  2,IFAIL1)
                      CALL INPCHK(J+1,2,IFAIL2)
                      NPROF=NPROF+1
                      CALL INPRDR(J,  AUX1,0.0)
                      CALL INPRDR(J+1,AUX2,0.0)
                      XPROF(NPROF)=AUX1
                      YPROF(NPROF)=AUX2
                      ZPROF(NPROF)=0
                      INEXT=J+2
                      IF((NPROF.EQ.1).OR.
     -                     (NPROF.GT.1.AND.XPRMIN.LT.XPROF(NPROF)))THEN
                           IPRMIN=NPROF
                           XPRMIN=XPROF(NPROF)
                      ENDIF
                 ELSEIF(INPTYP(J).EQ.1.OR.INPTYP(J).EQ.2)THEN
                      CALL INPMSG(I,'Number outside profile.')
                 ENDIF
 80              CONTINUE
                 IF(NPROF.GE.3.AND.IPRMIN.GT.0)THEN
                      CALL PLACHK(NPROF,XPROF,YPROF,ZPROF,IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! CELSOL WARNING : Profile'//
     -                          ' is not a proper curve; rejected.'
                           LEPROF=.FALSE.
                      ELSE
                           LEPROF=.TRUE.
                      ENDIF
                 ELSE
                      PRINT *,' !!!!!! CELSOL WARNING : Profile'//
     -                     ' has too few points; rejected.'
                      LEPROF=.FALSE.
                 ENDIF
 90              CONTINUE
                 IF(LEPROF)THEN
                      DET=(XPROF(1+MOD(IPRMIN,NPROF))-XPROF(IPRMIN))*
     -                    (YPROF(1+MOD(IPRMIN+1,NPROF))-YPROF(IPRMIN))-
     -                    (XPROF(1+MOD(IPRMIN+1,NPROF))-XPROF(IPRMIN))*
     -                    (YPROF(1+MOD(IPRMIN,NPROF))-YPROF(IPRMIN))
                      IF(DET.LT.0)THEN
                         IORI=-1
                      ELSEIF(DET.GT.0)THEN
                         IORI=+1
                      ELSE
                         PRINT *,' !!!!!! CELSOL WARNING : Unable to'//
     -                        ' determine profile orientation;'//
     -                        ' assuming it is clockwise.'
                         IORI=-1
                      ENDIF
                 ENDIF
*   Lids and other options.
            ELSEIF(INPCMP(I,'TOP-LID').NE.0)THEN
                 LTLID=.TRUE.
            ELSEIF(INPCMP(I,'NOTOP-LID')+
     -           INPCMP(I,'NO-TOP-LID').NE.0)THEN
                 LTLID=.FALSE.
            ELSEIF(INPCMP(I,'BOT#TOM-LID').NE.0)THEN
                 LBLID=.TRUE.
            ELSEIF(INPCMP(I,'NOBOT#TOM-LID')+
     -           INPCMP(I,'NO-BOT#TOM-LID').NE.0)THEN
                 LBLID=.FALSE.
            ELSEIF(INPCMP(I,'LID#S').NE.0)THEN
                 LTLID=.TRUE.
                 LBLID=.TRUE.
            ELSEIF(INPCMP(I,'NOLID#S')+INPCMP(I,'NO-LID#S').NE.0)THEN
                 LTLID=.FALSE.
                 LBLID=.FALSE.
            ELSEIF(INPCMP(I,'MEAN-R#ADIUS').NE.0)THEN
                 LRMEAN=.TRUE.
            ELSEIF(INPCMP(I,'OUT#ER-R#ADIUS').NE.0)THEN
                 LRMEAN=.FALSE.
*   Material.
            ELSEIF(INPCMP(I,'CON#DUCTOR')+
     -           INPCMP(I,'CON#DUCTOR-1').NE.0)THEN
                 IMAT=1
            ELSEIF(INPCMP(I,'CON#DUCTOR-2').NE.0)THEN
                 IMAT=2
            ELSEIF(INPCMP(I,'CON#DUCTOR-3').NE.0)THEN
                 IMAT=3
            ELSEIF(INPCMP(I,'DIEL#ECTRICUM')+
     -           INPCMP(I,'DIEL#ECTRICUM-1').NE.0)THEN
                 IMAT=11
            ELSEIF(INPCMP(I,'DIEL#ECTRICUM-2').NE.0)THEN
                 IMAT=12
            ELSEIF(INPCMP(I,'DIEL#ECTRICUM-3').NE.0)THEN
                 IMAT=13
*   Potential, dielectric constant, other boundary conditions.
            ELSEIF(INPCMP(I,'V#OLTAGE').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,VOLT,0.0)
                      LVOLT=.TRUE.
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'CH#ARGE')+INPCMP(I,'Q').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,CHARGE,0.0)
                      LCHA=.TRUE.
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'EPS#ILON').NE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(I+1,AUX1,-1.0)
                      IF(AUX1.GT.0)THEN
                           EPS=AUX1
                           LEPS=.TRUE.
                      ELSE
                           CALL INPMSG(I+1,'Epsilon is not positive.')
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Arguments not valid.')
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'PARA#LLEL-#FIELD').NE.0)THEN
                 IBOUND=6
            ELSEIF(INPCMP(I,'PERP#ENDICULAR-#FIELD').NE.0)THEN
                 IBOUND=7
            ELSEIF(INPCMP(I,'FLOAT#ING-#CONDUCTOR').NE.0)THEN
                 IBOUND=3
*   Label.
            ELSEIF(INPCMP(I,'LAB#EL')+INPCMP(I,'TYPE').NE.0)THEN
                 CALL INPSTR(I+1,I+1,AUXTYP,NCAUX)
                 IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',AUXTYP).EQ.
     -                0)THEN
                      CALL INPMSG(1,'The label must be a letter.')
                      AUXTYP='?'
                 ENDIF
                 INEXT=I+2
*   Discretisation
            ELSEIF(INPCMP(I,'DIS#CRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS1=-1.0
                      DIS2=-1.0
                      DIS3=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS1=DISAUX
                                DIS2=DISAUX
                                DIS3=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-LID-TOP')+
     -           INPCMP(I,'DIS#CRETISATION-TOP-#LID')+
     -           INPCMP(I,'TOP-#DISCRETISATION')+
     -           INPCMP(I,'TOP-#LID-#DISCRETISATION')+
     -           INPCMP(I,'LID-TOP-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS1=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS1=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-LID-BOT#TOM')+
     -           INPCMP(I,'DIS#CRETISATION-BOT#TOM-#LID')+
     -           INPCMP(I,'BOT#TOM-#DISCRETISATION')+
     -           INPCMP(I,'BOT#TOM-#LID-#DISCRETISATION')+
     -           INPCMP(I,'LID-BOT#TOM-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS2=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS2=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I,'DIS#CRETISATION-BODY')+
     -           INPCMP(I,'BODY-#DISCRETISATION')+
     -           INPCMP(I,'DIS#CRETISATION-PRO#FILE')+
     -           INPCMP(I,'PRO#FILE-#DISCRETISATION').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Argument missing.')
                 ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                      DIS3=-1.0
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      IF(IFAIL1.EQ.0)THEN
                           CALL INPRDR(I+1,DISAUX,DISDEF)
                           IF(DISAUX.GT.0)THEN
                                DIS3=DISAUX
                           ELSE
                                CALL INPMSG(I+1,
     -                               'Discretisation not > 0')
                           ENDIF
                      ELSE
                           CALL INPMSG(I,'Arguments not valid.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
*   Other things are not known.
            ELSE
                 CALL INPMSG(I,'Not a known keyword.')
            ENDIF
70          CONTINUE
*   Print error messages.
            CALL INPERR
*   Check compatibility of the options.
            IF(.NOT.(LVOLT.OR.LCHA.OR.LEPS.OR.IBOUND.GT.0))THEN
                 BEMSET=.FALSE.
            ELSEIF(LVOLT.AND.
     -           (LCHA.OR.LEPS.OR.IBOUND.EQ.6.OR.IBOUND.EQ.3.OR.
     -           (IMAT.GE.11.AND.IMAT.LE.20)))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Voltage boundary'//
     -                ' with incompatible attributes; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(LCHA.AND.(IBOUND.EQ.6.OR.IBOUND.EQ.7.OR.
     -           IBOUND.EQ.3))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Charge surface'//
     -                ' with incompatible attributes; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(LEPS.AND.(IBOUND.EQ.6.OR.IBOUND.EQ.7.OR.
     -           IBOUND.EQ.3.OR.(IMAT.GE.1.AND.IMAT.LE.10)))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Dielectric medium'//
     -                ' with incompatible attributes; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(IBOUND.EQ.6.AND.IMAT.GT.0)THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Parallel field'//
     -                ' symmetry on a volume; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF(IBOUND.EQ.7.AND.(IMAT.GE.11.AND.IMAT.LE.20))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Perpendicular'//
     -                ' field on a dielectric; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF((IMAT.GE.1.AND.IMAT.LE.10).AND..NOT.
     -           (LVOLT.OR.LCHA.OR.IBOUND.EQ.3))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Conductor without'//
     -                ' suitable boundary conditions; BEM disabled.'
                 BEMSET=.FALSE.
            ELSEIF((IMAT.GE.11.AND.IMAT.LE.20).AND..NOT.LEPS)THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Dielectric without'//
     -                ' epsilon; BEM disabled.'
                 BEMSET=.FALSE.
*   Otherwise complete the boundary conditions.
            ELSEIF(IBOUND.EQ.0.AND.LVOLT)THEN
                 IBOUND=1
            ELSEIF(IBOUND.EQ.0.AND.LCHA.AND.
     -           (LEPS.OR.(IMAT.GE.11.AND.IMAT.LE.20)))THEN
                 IBOUND=5
            ELSEIF(IBOUND.EQ.0.AND.LCHA)THEN
                 IBOUND=2
            ELSEIF(IBOUND.EQ.0.AND.
     -           (LEPS.OR.(IMAT.GE.11.AND.IMAT.LE.20)))THEN
                 IBOUND=4
            ELSEIF(IBOUND.EQ.0)THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Unexpected'//
     -                ' solid classification.'
            ENDIF
*   Complete materials if appropriate.
            IF(IMAT.EQ.0.AND.(LVOLT.OR.
     -           IBOUND.EQ.3.OR.IBOUND.EQ.7))IMAT=1
            IF(IMAT.EQ.0.AND.(LEPS.OR.LCHA.OR.IBOUND.EQ.5))IMAT=11
*   Enter in the conductor table.
            IF(LPOS.AND.LEPROF.AND.LSIZ.AND.
     -           (NSOLID+1.GT.MXSOLI.OR.ICCURR+24.GT.MXSBUF))THEN
                 PRINT *,' !!!!!! CELSOL WARNING : Solids table'//
     -                ' is full; extrusion not stored.'
            ELSEIF(LPOS.AND.LEPROF.AND.LSIZ)THEN
                 NSOLID=NSOLID+1
                 ISTART(NSOLID)=ICCURR
                 ISOLTP(NSOLID)=6
                 ISOLMT(NSOLID)=IMAT
                 SOLTYP(NSOLID)=AUXTYP
                 CBUF(ICCURR+1)=DBLE(IORI)
                 CBUF(ICCURR+2)=ZSIZ
                 CBUF(ICCURR+3)=XPOS
                 CBUF(ICCURR+4)=YPOS
                 CBUF(ICCURR+5)=ZPOS
                 CBUF(ICCURR+6)=XDIR
                 CBUF(ICCURR+7)=YDIR
                 CBUF(ICCURR+8)=ZDIR
                 CBUF(ICCURR+9)=DBLE(NPROF)
*   Compute rotation angles.
                 IF(XDIR**2+YDIR**2.LE.0)THEN
                      PHI=0
                      IF(ZDIR.GT.0)THEN
                           THETA=0
                      ELSE
                           THETA=PI
                      ENDIF
                 ELSE
                      PHI=ATAN2(YDIR,XDIR)
                      THETA=ATAN2(SQRT(XDIR**2+YDIR**2),ZDIR)
                 ENDIF
                 CBUF(ICCURR+10)=COS(THETA)
                 CBUF(ICCURR+11)=SIN(THETA)
                 CBUF(ICCURR+12)=COS(PHI)
                 CBUF(ICCURR+13)=SIN(PHI)
*   Boundary conditions
                 CBUF(ICCURR+15)=VOLT
                 CBUF(ICCURR+16)=EPS
                 CBUF(ICCURR+17)=DBLE(IBOUND)
                 CBUF(ICCURR+18)=CHARGE
*   Discretisation.
                 CBUF(ICCURR+19)=DIS1
                 CBUF(ICCURR+20)=DIS2
                 CBUF(ICCURR+21)=DIS3
*   Lids.
                 IF(LTLID)THEN
                      CBUF(ICCURR+22)=1
                 ELSE
                      CBUF(ICCURR+22)=0
                 ENDIF
                 IF(LBLID)THEN
                      CBUF(ICCURR+23)=1
                 ELSE
                      CBUF(ICCURR+23)=0
                 ENDIF
*   Profile.
                 DO 100 I=1,NPROF
                 CBUF(ICCURR+23+2*I-1)=XPROF(I)
                 CBUF(ICCURR+23+2*I)  =YPROF(I)
 100             CONTINUE
*   Store size.
                 ICCURR=ICCURR+23+2*NPROF
*   Or warn that some element is missing.
            ELSE
                 PRINT *,' !!!!!! CELSOL WARNING : Extrusion not'//
     -                ' entered because the position, the profile'//
     -                ' or the length has not been given.'
            ENDIF
*** Other things are not known.
       ELSE
            CALL INPSTR(1,1,STRING,NC)
            PRINT *,' !!!!!! CELSOL WARNING : Shape '//STRING(1:NC)//
     -           ' is not known; ignored.'
       ENDIF
*** Read the next line.
       GOTO 10
       END
