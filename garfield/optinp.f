CDECK  ID>, OPTINP.
       SUBROUTINE OPTINP
*-----------------------------------------------------------------------
*   OPTINP - Routine reading cell optimisation instructions.
*   VARIABLES :
*   (Last changed on 27/10/11.)
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
       LOGICAL MAGOK
       REAL ALFA,B0X,B0Y,B0Z,SUSWIR,SUSGAS,BSCALE,BFMIN,BFMAX,
     -      BFXMIN,BFYMIN,BFZMIN,BFXMAX,BFYMAX,BFZMAX
       INTEGER MAGSRC,
     -      IBXTYP,IBYTYP,IBZTYP,
     -      IRB0X,IRB0Y,IRB0Z,IRV0X,IRV0Y,IRV0Z,
     -      IENB0X,IENB0Y,IENB0Z,IBXDIR,IBYDIR,IBZDIR,
     -      NCB0X,NCB0Y,NCB0Z
       CHARACTER*(MXCHAR) FUNB0X,FUNB0Y,FUNB0Z
       COMMON /MAGDAT/ ALFA,SUSWIR,SUSGAS,
     -      B0X,B0Y,B0Z,BSCALE,BFMIN,BFMAX,
     -      BFXMIN,BFYMIN,BFZMIN,BFXMAX,BFYMAX,BFZMAX,
     -      MAGSRC,IBXTYP,IBYTYP,IBZTYP,
     -      IRB0X,IRB0Y,IRB0Z,IRV0X,IRV0Y,IRV0Z,
     -      IENB0X,IENB0Y,IENB0Z,IBXDIR,IBYDIR,IBZDIR,
     -      NCB0X,NCB0Y,NCB0Z,
     -      MAGOK
       COMMON /MAGCHR/ FUNB0X,FUNB0Y,FUNB0Z
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
       CHARACTER*(MXCHAR) FUNFLD,FUNPOS,FUNWGT
       CHARACTER*10 VALTYP,PNTTYP
       REAL VST(MXWIRE),VPLST(5)
       LOGICAL EVALT,EVALD,EVALA
       INTEGER NPOINT,NSWIRE,IOPT,NFLD,NPOS,NWGT,IENFLD,IENPOS,IENWGT
       COMMON /OPTDAT/ VST,VPLST,NPOINT,NSWIRE,IOPT,NFLD,NPOS,NWGT,
     -      IENFLD,IENPOS,IENWGT,EVALT,EVALD,EVALA
       COMMON /OPTCHR/ FUNFLD,FUNPOS,FUNWGT,VALTYP,PNTTYP
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
       CHARACTER*(MXCHAR) STRING
       CHARACTER*10 USER
       REAL XPOS,YPOS
       INTEGER IDUMMY,NWORD,NC,IFAIL,IFAIL1,IFAIL2,IFAIL3,IFAIL4,
     -      I,IW,ISW,NPOINR,NGRDXR,NGRDYR,NGRIDR,INPCMP,IREFNO
       LOGICAL STDSTR,CHANGE
       EXTERNAL STDSTR,INPCMP
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE OPTINP ///'
*** Print a header for this page.
       WRITE(*,'(''1'')')
       PRINT *,' ================================================'
       PRINT *,' ========== Start optimisation section =========='
       PRINT *,' ================================================'
       PRINT *,' '
*** Open a dataset and save the initial setting.
       IDUMMY=0
       CALL OPTDSN('OPEN',IDUMMY)
       CALL OPTDSN('SAVE',IREFNO)
*** Start an input loop.
       CALL INPPRM('Optimise','NEW-PRINT')
10     CONTINUE
       CALL INPWRD(NWORD)
       CALL INPSTR(1,1,STRING,NC)
*** Skip the line if blank.
       IF(NWORD.EQ.0)GOTO 10
*** Return to main program if '&' is the first character.
       IF(STRING(1:1).EQ.'&')THEN
*   Close the auxilliary file.
            CALL OPTDSN('CLOSE',IDUMMY)
            RETURN
*** Look for the ADD instruction.
       ELSEIF(INPCMP(1,'ADD').NE.0)THEN
            CALL OPTADD(CHANGE)
            IF(CHANGE)CALL CELRES(IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! OPTINP WARNING : The new cell'//
     -                ' is not acceptable ; leaving &OPTIMISE.'
                 RETURN
            ENDIF
*** Look for the AREA instruction.
       ELSEIF(INPCMP(1,'AR#EA').NE.0)THEN
            CALL CELVIE(PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX)
            CALL INPERR
*** Background field.
       ELSEIF(INPCMP(1,'BACKGR#OUND-#FIELD').NE.0)THEN
            CALL OPTBGF
       ELSEIF(INPCMP(1,'DEL#ETE-BACKGR#OUND-#FIELD').NE.0)THEN
            IF(IENBGF.NE.0)CALL ALGCLR(IENBGF)
            IENBGF=0
*** Look for 3-dimensional charges.
       ELSEIF(INPCMP(1,'CHARGE#S').NE.0)THEN
*   Print a prompt for interactive mode reading of charges.
            IF(STDSTR('INPUT'))PRINT *,' ====== OPTINP INPUT   :'//
     -           ' Please enter the charges, terminate with a'//
     -           ' blank line.'
            CALL INPPRM('Charges','ADD-NOPRINT')
*   Initialise number of charges.
            N3D=0
20          CONTINUE
*   Input a line and check the basics.
            CALL INPWRD(NWORD)
            IF(N3D.GE.MX3D)THEN
                 PRINT *,' !!!!!! OPTINP WARNING : Unable to store'//
     -                ' further charges ; increase MX3D.'
            ELSEIF(NWORD.EQ.3.OR.NWORD.EQ.4)THEN
                 N3D=N3D+1
                 CALL INPCHK(1,2,IFAIL1)
                 CALL INPCHK(2,2,IFAIL2)
                 CALL INPCHK(3,2,IFAIL3)
                 CALL INPRDR(1,X3D(N3D),0.0)
                 CALL INPRDR(2,Y3D(N3D),0.0)
                 CALL INPRDR(3,Z3D(N3D),0.0)
                 IF(NWORD.EQ.4)THEN
                      CALL INPCHK(4,2,IFAIL4)
                      CALL INPRDR(4,E3D(N3D),1.0)
                 ELSE
                      IFAIL4=0
                      E3D(N3D)=1.0
                 ENDIF
                 CALL INPERR
            ELSEIF(NWORD.GT.0)THEN
                 PRINT *,' !!!!!! OPTINP WARNING : Incorrect number'//
     -                ' of keywords ; ignoring this charge.'
            ENDIF
            IF(NWORD.NE.0)GOTO 20
            CALL INPPRM(' ','BACK-PRINT')
*** Look for the DELETE-CHARGES instruction.
       ELSEIF(INPCMP(1,'DEL#ETE-CHA#RGES').NE.0)THEN
            N3D=0
*** Look for the LIST-CHARGES instruction.
       ELSEIF(INPCMP(1,'L#IST-CHA#RGES').NE.0)THEN
            IF(N3D.EQ.0)THEN
                 WRITE(LUNOUT,'(''  No three dimensional charges'',
     -                '' are present at the moment.'')')
            ELSE
                 WRITE(LUNOUT,'(''  LIST OF 3-DIMENSIONAL CHARGES''//
     -                ''    x-charge [cm]   y-charge [cm]'',
     -                ''   z-charge [cm]   Q [4 pi eps0]''//)')
                 DO 40 I=1,N3D
                 WRITE(LUNOUT,'(1X,4(1X,E15.8))')
     -                X3D(I),Y3D(I),Z3D(I),E3D(I)
40               CONTINUE
            ENDIF
*** Look for the DRIFT-AREA instruction.
       ELSEIF(INPCMP(1,'DR#IFT-AREA').NE.0)THEN
            CALL CELVIE(DXMIN,DYMIN,DZMIN,DXMAX,DYMAX,DZMAX)
            CALL INPERR
*** Look for the CHANGE-VOLTAGES instruction.
       ELSEIF(INPCMP(1,'CHAN#GE-#VOLTAGES').NE.0)THEN
            CALL OPTCHV
*** Look for the DELETE instruction.
       ELSEIF(INPCMP(1,'DE#LETE').NE.0)THEN
            CALL OPTDEL(CHANGE)
            IF(CHANGE)CALL CELRES(IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! OPTINP WARNING : The new cell'//
     -                ' is not acceptable ; leaving &OPTIMISE.'
                 RETURN
            ENDIF
*** Display the potential settings.
       ELSEIF(INPCMP(1,'DI#SPLAY').NE.0)THEN
            WRITE(LUNOUT,'(''  CURRENT POTENTIAL SETTINGS:'',//,
     -           ''  You have selected '',I3,'' groups of wires to be'',
     -           '' varied collectively:'')') NSW
            DO 110 ISW=1,NSW
            WRITE(LUNOUT,'(/''  Group '',I3)') ISW
            DO 100 IW=1,NWIRE
            IF(INDSW(IW).NE.ISW)GOTO 100
            XPOS=X(IW)
            YPOS=Y(IW)
            IF(POLAR)CALL CFMRTP(XPOS,YPOS,XPOS,YPOS,1)
            WRITE(LUNOUT,'(5X,''Wire '',I3,'', code '',A1,'', V='',
     -           E15.8,'', at: ('',E15.8,'','',E15.8,'').'')')
     -           IW,WIRTYP(IW),V(IW),XPOS,YPOS
100         CONTINUE
110         CONTINUE
            WRITE(LUNOUT,'('' '')')
*** Search for the FACTOR instruction.
       ELSEIF(INPCMP(1,'FA#CTOR').NE.0)THEN
            CALL OPTFAC
*** Read a field map.
       ELSEIF(INPCMP(1,'FIELD-MAP')+
     -      INPCMP(1,'READ-FIELD-MAP').NE.0)THEN
*   Obtain the field map for background field use.
            CALL BOOK('INQUIRE','MAP',USER,IFAIL)
            IF(USER.EQ.'CELL')THEN
                 PRINT *,' !!!!!! OPTINP WARNING : Field map is'//
     -                ' currently used for the main field; field'//
     -                ' map not read as background field.'
                 IFAIL=1
            ELSEIF(USER.EQ.' ')THEN
                 CALL BOOK('BOOK','MAP','OPTIMISE',IFAIL)
                 IF(IFAIL.NE.0)PRINT *,' !!!!!! OPTINP WARNING :'//
     -                ' Unable to obtain control of the field map'//
     -                ' for use as background field.'
            ELSEIF(USER.EQ.'OPTIMISE')THEN
                 IFAIL=0
            ELSE
                 PRINT *,' !!!!!! OPTINP WARNING : Field map is in'//
     -                ' use by '//USER//' not reallocated.'
                 IFAIL=1
            ENDIF
*   Read the field map.
            IF(IFAIL.EQ.0)THEN
                 IF(INPCMP(1,'FIELD-MAP').NE.0)THEN
                      CALL MAPREA(IFAIL)
                 ELSE
                      CALL MAPFMF(IFAIL)
                 ENDIF
            ENDIF
*   Check the error flag from mapo reading.
            IF(IFAIL.NE.0)PRINT *,' !!!!!! OPTINP WARNING : Reading'//
     -           ' a field map failed.'
*** Delete a field map.
       ELSEIF(INPCMP(1,'DEL#ETE-F#IELD-MAP')+
     -      INPCMP(1,'DEL#ETE-MAP').NE.0)THEN
*   Delete the field map itself.
            CALL MAPINT
            CALL BOOK('RELEASE','MAP','OPTIMISE',IFAIL)
*   Check whether the background field is to be kept.
            IF(LBGFMP)THEN
                 PRINT *,' ------ OPTINP MESSAGE : Background field'//
     -                ' deleted because of dependence on the field map.'
                 IF(IENBGF.NE.0)CALL ALGCLR(IENBGF)
                 IENBGF=0
            ENDIF
*** Plot of the forces acting on a wire.
       ELSEIF(INPCMP(1,'FO#RCES').NE.0)THEN
            CALL OPTFRC
*** Look for the keyword GRID.
       ELSEIF(INPCMP(1,'G#RID').NE.0)THEN
            IF(NWORD.EQ.1)THEN
                 PRINT *,'  The number of grid points is ',NGRIDX,
     -                ' by ',NGRIDY,'.'
            ELSEIF(NWORD.EQ.2)THEN
                 CALL INPCHK(2,1,IFAIL1)
                 CALL INPRDI(2,NGRIDR,25)
                 IF(NGRIDR.LE.1.OR.NGRIDR.GT.MXGRID)
     -                CALL INPMSG(2,'GRID out of range 2 -> MXGRID.')
                 CALL INPERR
                 IF(IFAIL1.NE.0.OR.NGRIDR.LE.1.OR.NGRIDR.GT.MXGRID)THEN
                      PRINT *,' !!!!!! OPTINP WARNING : GRID statement',
     -                     ' ignored because of syntax or value errors.'
                 ELSE
                      NGRIDX=NGRIDR
                      NGRIDY=NGRIDR
                 ENDIF
            ELSEIF(NWORD.EQ.3)THEN
                 CALL INPCHK(2,1,IFAIL1)
                 CALL INPCHK(3,1,IFAIL2)
                 CALL INPRDI(2,NGRDXR,25)
                 CALL INPRDI(3,NGRDYR,25)
                 IF(NGRDXR.LE.1.OR.NGRDXR.GT.MXGRID)
     -                CALL INPMSG(2,'out of the range 2 -> MXGRID. ')
                 IF(NGRDYR.LE.1.OR.NGRDYR.GT.MXGRID)
     -                CALL INPMSG(2,'out of the range 2 -> MXGRID. ')
                 CALL INPERR
                 IF(IFAIL1.NE.0.OR.NGRDXR.LE.1.OR.NGRDXR.GT.MXGRID.OR.
     -                NGRDYR.LE.1.OR.NGRDYR.GT.MXGRID)THEN
                      PRINT *,' !!!!!! OPTINP WARNING : GRID statement',
     -                     ' ignored because of syntax or value errors.'
                 ELSE
                      NGRIDX=NGRDXR
                      NGRIDY=NGRDYR
                 ENDIF
            ELSE
                 PRINT *,' !!!!!! OPTINP WARNING : GRID requires 1'//
     -                ' or 2 arguments ; the instruction is ignored.'
            ENDIF
*** List excitation levels.
       ELSEIF(INPCMP(1,'L#IST-EXC#ITATIONS-#IONISATIONS-#LEVELS')+
     -      INPCMP(1,'L#IST-EXC#ITATIONS-#LEVELS')+
     -      INPCMP(1,'L#IST-ION#ISATIONS-#EXCITATIONS-#LEVELS')+
     -      INPCMP(1,'L#IST-ION#ISATIONS-#LEVELS').NE.0)THEN
            CALL GASLPT
       ELSEIF(INPCMP(1,'PEN#NING-#TRANSFER').NE.0)THEN
            CALL GASRPT
       ELSEIF(INPCMP(1,'PL#OT-GAS').NE.0)THEN
            CALL GASPLT
       ELSEIF(INPCMP(1,'PR#INT-GAS').NE.0)THEN
            CALL GASPRT
*** Look for the keyword OPTION,
       ELSEIF(INPCMP(1,'O#PTIONS').NE.0)THEN
*   No valid options here.
            DO 30 I=2,NWORD
            CALL INPMSG(I,'The option is not known.      ')
30          CONTINUE
            CALL INPERR
*** Look for the instruction POINT.
       ELSEIF(INPCMP(1,'P#OINTS').NE.0)THEN
            IF(NWORD.EQ.1)THEN
                 WRITE(LUNOUT,'(''  Current number of points on'',
     -                '' the track: '',I3,''.'')') NPOINT
            ELSEIF(NWORD.NE.2)THEN
                 PRINT *,' !!!!!! OPTINP WARNING : POINTS requires 1'//
     -                ' argument ; the instruction is ignored.'
            ELSE
                 CALL INPCHK(2,1,IFAIL1)
                 CALL INPRDI(2,NPOINR,20)
                 IF(NPOINR.LE.1)
     -                CALL INPMSG(2,'POINT should be larger than 1.')
                 CALL INPERR
                 IF(IFAIL1.NE.0.OR.NPOINR.LE.1)THEN
                      PRINT *,' !!!!!! OPTINP WARNING : POINTS is'//
     -                     ' ignored because of syntax or value errors.'
                 ELSE
                      NPOINT=NPOINR
                 ENDIF
            ENDIF
*** Print the cell.
       ELSEIF(INPCMP(1,'PR#INT-#CELL')+INPCMP(1,'C#ELL-PR#INT')
     -      .NE.0)THEN
            CALL CELPRT
*** Retrieve a record.
       ELSEIF(INPCMP(1,'R#ESTORE').NE.0)THEN
            IREFNO=1
            IFAIL=0
            IF(NWORD.GE.2)THEN
                 CALL INPCHK(2,1,IFAIL)
                 CALL INPRDI(2,IREFNO,1)
            ENDIF
            IF(NWORD.GT.2)PRINT *,' !!!!!! OPTINP WARNING : RETRIEVE'//
     -           ' takes a single arguments ; the rest is ignored.'
            IF(IFAIL.EQ.0)THEN
                 CALL OPTDSN('RESTORE',IREFNO)
            ELSE
                 PRINT *,' !!!!!! OPTINP WARNING : RETRIEVE is'//
     -                ' ignored because of errors.'
            ENDIF
*** Save a record.
       ELSEIF(INPCMP(1,'SA#VE').NE.0)THEN
            CALL OPTDSN('SAVE',IREFNO)
            IF(NWORD.GT.1)PRINT *,' !!!!!! OPTINP WARNING : SAVE'//
     -           ' takes no arguments ; they are ignored.'
            IF(IREFNO.EQ.0)THEN
                 PRINT *,' !!!!!! OPTINP WARNING : The voltages have'//
     -                ' not been saved.'
            ELSE
                 WRITE(LUNOUT,'(''  ------ OPTINP MESSAGE : Reference'',
     -                '' number for this set of potentials: '',I3)')
     -                IREFNO
            ENDIF
*** Write the field map in binary format.
       ELSEIF(INPCMP(1,'SAVE-F#IELD-#MAP').NE.0)THEN
            CALL MAPFMS
*** Search for the SELECT instruction.
       ELSEIF(INPCMP(1,'SEL#ECT').NE.0)THEN
            CALL CELSEL(' ')
*** Look for the SET instruction.
       ELSEIF(INPCMP(1,'SET').NE.0)THEN
            CALL OPTSET
*** Look for the instruction TRACK.
       ELSEIF(INPCMP(1,'TR#ACK').NE.0)THEN
            CALL TRAREA
*** It is not possible to get here if the keyword is valid.
       ELSE
            CALL INPSTR(1,1,STRING,NC)
            PRINT *,' !!!!!! OPTINP WARNING : '//STRING(1:NC)//' is'//
     -           ' not a valid instruction ; ignored.'
       ENDIF
*** Go on with the next input line.
       GOTO 10
       END
