CDECK  ID>, DRFGRA.
       SUBROUTINE DRFGRA
*-----------------------------------------------------------------------
*   DRFGRA - Subroutine that uses interactive graphics to do some
*            drift-line calculations.
*   (Last changed on 24/ 4/92.)
*-----------------------------------------------------------------------
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
       CHARACTER*80 AUXSTR
       CHARACTER*200 CHSTR
       REAL XPL(2),YPL(2),XPOS,YPOS,Q,XSING,YSING,ANGLE
       INTEGER ITYPE
       LOGICAL STDSTR
       EXTERNAL STDSTR,INPCMP,INPTYP
       SAVE NRETRY
       SAVE ICPET,ILPET1,ILPET2,IVPET,IPPET
       SAVE IWKLC,IWKCH,IWKVL,IWKPK,IWK
       SAVE IDEVLC,IDEVCH,IDEVVL,IDEVPK
*** Initial parameter values, number of retries.
       DATA NRETRY/2/
*   Promp/echo types.
       DATA ICPET/1/, ILPET1/1/, ILPET2/4/, IVPET/1/, IPPET/1/
*   Device.
       DATA IDEVLC/1/, IDEVCH/1/, IDEVVL/1/, IDEVPK/1/
*** Check we are in interactive mode.
       IF(.NOT.STDSTR('INPUT'))THEN
            PRINT *,' !!!!!! DRFGRA WARNING : This instruction can'//
     -           ' only be carried out in interactive mode.'
            RETURN
       ENDIF
*** Make sure the level of GKS is sufficient.
       CALL GQLVKS(IERR,ILEV)
       IF(ILEV.LT.4)THEN
            PRINT *,' !!!!!! DRFGRA WARNING : The program has been'//
     -           ' linked with a GKS of too low a level.'
            RETURN
       ENDIF
*** Find an in/out workstation, first check operating state.
       CALL GQOPS(IOPSTA)
*   No active workstations.
       IF(IOPSTA.LT.3)THEN
            PRINT *,' !!!!!! DRFGRA WARNING : No active workstations'//
     -           ' ; not executed.'
            RETURN
       ENDIF
*   Determine number of active workstations.
       CALL GQACWK(0,IERR,NACT,IWK)
       IWKREQ=-1
       DO 2 I=1,NACT
       CALL GQACWK(I,IERR,IDUM,IWK)
*   Locate one that has input facilities.
       CALL GQWKC(IWK,IERR1,ICONID,IWKTYP)
       CALL GQWKCA(IWKTYP,IERR2,IWKCAT)
       IF(IWKCAT.EQ.2)IWKREQ=IWK
2      CONTINUE
*   Issue an string request to an input workstation.
       IF(IWKREQ.EQ.-1)THEN
            PRINT *,' !!!!!! DRFGRA WARNING : No active workstations'//
     -           ' with in/out facilities ; not executed.'
            RETURN
       ENDIF
*   Set default parameters.
       IWKLC=IWKREQ
       IWKCH=IWKREQ
       IWKVL=IWKREQ
       IWKPK=IWKREQ
       IWK=IWKREQ
*   Debugging output.
       IF(LDEBUG)PRINT *,' ++++++ DRFGRA DEBUG   : Default ws for'//
     -      ' this command is ',IWKREQ
*** Initial parameters.
       Q=-1.0
       ITYPE=1
*** Decode the argument string, if present.
       CALL INPNUM(NWORD)
       INEXT=2
       DO 10 I=2,NWORD
       IF(INEXT.GT.I)GOTO 10
*   Prompt echo type for choice input.
       IF(INPCMP(I,'CH#OICE-PET').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'No prompt/echo type specified.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,ICPET,ICPET)
                 INEXT=I+2
            ENDIF
*   Prompt echo type for locator input.
       ELSEIF(INPCMP(I,'LOC#ATOR-PET').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).NE.1.OR.
     -           INPTYP(I+2).NE.1)THEN
                 CALL INPMSG(I,'Two prompt/echo types needed. ')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,ILPET1,ILPET1)
                 CALL INPCHK(I+2,1,IFAIL2)
                 CALL INPRDI(I+2,ILPET2,ILPET2)
                 INEXT=I+3
            ENDIF
*   Prompt echo type for valuator input.
       ELSEIF(INPCMP(I,'VAL#UATOR-PET').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'No prompt/echo type specified.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,IVPET,IVPET)
                 INEXT=I+2
            ENDIF
*   Prompt echo type for pick input.
       ELSEIF(INPCMP(I,'PICK-PET').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'No prompt/echo type specified.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,IPPET,IPPET)
                 INEXT=I+2
            ENDIF
*   Workstation.
       ELSEIF(INPCMP(I,'W#ORK-ST#ATION').NE.0)THEN
            IF(NWORD.LT.I+1)THEN
                 CALL INPMSG(I,'No workstation id specified.  ')
            ELSE
                 CALL INPSTR(I+1,I+1,AUXSTR,NCAUX)
                 CALL GRQIWK(AUXSTR(1:NCAUX),IWK,IFAIL)
                 IF(IFAIL.NE.0)THEN
                      CALL INPMSG(I+1,'Not a valid workstation name. ')
                 ELSE
                      IWKCH=IWK
                      IWKLC=IWK
                      IWKVL=IWK
                      IWKPK=IWK
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Choice device.
       ELSEIF(INPCMP(I,'CH#OICE-DEV#ICE').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'No device has been specified. ')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,IDEVCH,1)
                 INEXT=I+2
            ENDIF
*   Locator device.
       ELSEIF(INPCMP(I,'LOC#ATOR-DEV#ICE').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'No device has been specified. ')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,IDEVLC,1)
                 INEXT=I+2
            ENDIF
*   Pick device.
       ELSEIF(INPCMP(I,'PICK-DEV#ICE').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'No device has been specified. ')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,IDEVPK,1)
                 INEXT=I+2
            ENDIF
*   Valuator device.
       ELSEIF(INPCMP(I,'VAL#UATOR-DEV#ICE').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'No device has been specified. ')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,IDEVVL,1)
                 INEXT=I+2
            ENDIF
*   Number of retries.
       ELSEIF(INPCMP(I,'RETR#IES').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'Number of retries absent.     ')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NRETRY,5)
                 INEXT=I+2
            ENDIF
*   Unknown argument.
       ELSE
            CALL INPMSG(I,'Not a known keyword.          ')
       ENDIF
10     CONTINUE
       CALL INPERR
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DRFGRA DEBUG   : Flags'',
     -      '' currently in effect:''//
     -      ''  Choice  : PET='',I2,''   , device='',I2,'', ws='',I2/
     -      ''  Locator : PET='',I2,1X,I2,'', device='',I2,'', ws='',I2/
     -      ''  Pick    : PET='',I2,''   , device='',I2,'', ws='',I2/
     -      ''  Valuator: PET='',I2,''   , device='',I2,'', ws='',I2//
     -      ''  Number of retries: '',I2/)')
     -      ICPET,IDEVCH,IWKCH,ILPET1,ILPET2,IDEVLC,IWKLC,
     -      IPPET,IDEVPK,IWKPK,IVPET,IDEVVL,IWKVL,NRETRY
*** Check the workstation and obtain some information about it.
       CALL GQWKS(IWK,IERR,ISTATE)
       IF(IERR.NE.0.OR.ISTATE.NE.1)THEN
            PRINT *,' !!!!!! DRFGRA WARNING : The workstation over'//
     -           ' which this command is run is not active.'
            RETURN
       ENDIF
       CALL GQWKC(IWK,IERR,ICONID,IWKTYP)
       IF(IERR.NE.0)THEN
            PRINT *,' !!!!!! DRFGRA WARNING : Unable to determine the'//
     -           ' workstation type ; command not executed.'
            RETURN
       ENDIF
       CALL GQWKCA(IWKTYP,IERR,IWKCAT)
       IF(IERR.NE.0.OR.IWKCAT.NE.2)THEN
            PRINT *,' !!!!!! DRFGRA WARNING : The workstation over'//
     -           ' which this command is run'
            PRINT *,'                         doesn''t have both'//
     -           ' output and input facilities.'
            IF(LDEBUG)PRINT *,' ++++++ DRFGRA DEBUG   : Category'//
     -           ' of WS ',IWK,' is ',IWKCAT,' type is ',IWKTYP,'.'
            RETURN
       ENDIF
       CALL GQDSP(IWKTYP,ISTAT,IUNIT,RX,RY,LX,LY)
       IF(ISTAT.NE.0)THEN
            PRINT *,' !!!!!! DRFGRA WARNING : Unable to determine the'//
     -           ' workstation window size ; command not executed.'
            RETURN
       ENDIF
*** And use them to set the various display areas.
       IF(RX.LT.1.4*RY)RY=RX/1.4
*   Locator.
       XLMIN=0.01*RX
       XLMAX=0.99*RY
       YLMIN=0.01*RY
       YLMAX=0.99*RY
*   Choice.
       XCMIN=0.91*RY
       XCMAX=0.99*RX
       YCMIN=0.10*RY
       YCMAX=0.90*RY
*   Valuator.
       XVMIN=1.05*RY
       XVMAX=0.99*RX
       YVMIN=0.05*RY
       YVMAX=0.15*RY
*   Pick.
       XPMIN=0.01*RX
       XPMAX=0.99*RY
       YPMIN=0.01*RY
       YPMAX=0.99*RY
*** Plot the frame.
       CALL GRAXIS(DXMIN,DYMIN,DXMAX,DYMAX,
     -      '                                        ')
       CALL CELLAY(DXMIN,DYMIN,DXMAX,DYMAX)
       XSING=0.5*(DXMIN+DXMAX)
       YSING=0.5*(DYMIN+DYMAX)
*** Ask what the user wants via a menu.
100    CONTINUE
       ICHOIC=2
       CALL GRMENU('Quit$Smaller AREA$Larger AREA$Set a new track$'//
     -      'Single drift-line$Drift from a wire$Drift from track$'//
     -      'Clean screen$Parameter menu','$',XCMIN,YCMIN,XCMAX,YCMAX,
     -      IWKCH,IDEVCH,ICPET,ICHOIC,IFAIL)
*   Check the outcome of the menu.
       IF(IFAIL.NE.0)THEN
            CALL GRALOG('< graphics input screen >               ')
            CALL GRNEXT
            PRINT *,' !!!!!! DRFGRA WARNING : Unable to extract a'//
     -           ' value from the menu.'
            CALL TIMLOG('Drift section with graphics input.      ')
            RETURN
       ENDIF
*** Act accordingly, first the quit.
       IF(ICHOIC.EQ.1)THEN
            CALL GMSG(IWK,' ')
            CALL GRALOG('< graphics input screen >               ')
            CALL GRNEXT
            CALL TIMLOG('Drift section with graphics input.      ')
            RETURN
**  Next the smaller AREA.
       ELSEIF(ICHOIC.EQ.2)THEN
*   Prompt the user for one edge point.
            CALL GMSG(IWK,'Please point to one edge.')
*   Initialise the LOCATOR to get the point.
            LSTR=0
            PX=DXMIN
            PY=DYMIN
            IF(POLAR)CALL CFMRTC(PX,PY,PX,PY,1)
            CALL GINLC(IWKLC,IDEVLC,1,PX,PY,ILPET1,
     -           XLMIN,XLMAX,YLMIN,YLMAX,LSTR,AUXSTR)
*   Get the point.
            IRETRY=0
210         CONTINUE
            IRETRY=IRETRY+1
            CALL GRQLC(IWKLC,IDEVLC,IERR,NT,PX,PY)
            DXMINN=PX
            DYMINN=PY
            IF(POLAR)CALL CFMCTR(DXMINN,DYMINN,DXMINN,DYMINN,1)
            IF(NT.NE.1.OR.IERR.NE.1.OR.DXMINN.LT.DXMIN.OR.
     -           DXMINN.GT.DXMAX.OR.DYMINN.LT.DYMIN.OR.
     -           DYMINN.GT.DYMAX)THEN
                 CALL GMSG(IWK,'Please point in the current AREA.')
                 IF(IRETRY.LE.NRETRY)GOTO 210
                 GOTO 100
            ENDIF
*   Prompt the user for the other edge point.
            CALL GMSG(IWK,'Please point to the opposite edge.')
*   Initialise the LOCATOR to get the point.
            LSTR=0
            CALL GINLC(IWKLC,IDEVLC,1,PX,PY,ILPET2,
     -           XLMIN,XLMAX,YLMIN,YLMAX,LSTR,AUXSTR)
*   Get the point.
            IRETRY=0
220         CONTINUE
            IRETRY=IRETRY+1
            CALL GRQLC(IWKLC,IDEVLC,IERR,NT,PX,PY)
            DXMAXN=PX
            DYMAXN=PY
            IF(POLAR)CALL CFMCTR(DXMAXN,DYMAXN,DXMAXN,DYMAXN,1)
            IF(NT.NE.1.OR.IERR.NE.1.OR.DXMAXN.LT.DXMIN.OR.
     -           DXMAXN.GT.DXMAX.OR.DYMAXN.LT.DYMIN.OR.
     -           DYMAXN.GT.DYMAX)THEN
                 CALL GMSG(IWK,'Please point in the current AREA.')
                 IF(IRETRY.LE.NRETRY)GOTO 220
                 GOTO 100
            ENDIF
*   Determine the new AREA.
            IF(DXMINN.EQ.DXMAXN.OR.DYMINN.EQ.DYMAXN)THEN
                 CALL GMSG(IWK,'The new AREA is not valid.')
            ELSE
                 DXMIN=MIN(DXMINN,DXMAXN)
                 DXMAX=MAX(DXMINN,DXMAXN)
                 DYMIN=MIN(DYMINN,DYMAXN)
                 DYMAX=MAX(DYMINN,DYMAXN)
                 CALL GMSG(IWK,'Redrawing the axes')
                 CALL GRALOG('< graphics input screen >               ')
                 CALL GRNEXT
                 CALL GRAXIS(DXMIN,DYMIN,DXMAX,DYMAX,
     -                '                                        ')
                 CALL CELLAY(DXMIN,DYMIN,DXMAX,DYMAX)
                 XSING=0.5*(DXMIN+DXMAX)
                 YSING=0.5*(DYMIN+DYMAX)
            ENDIF
**  Next the bigger AREA.
       ELSEIF(ICHOIC.EQ.3)THEN
*   Prompt the user for the zoom factor.
            CALL GMSG(IWK,'Please enter the magnification factor.')
*   Initialise the VALUATOR.
            LSTR=0
            CALL GINVL(IWKVL,IDEVVL,5.0,IVPET,XVMIN,XVMAX,YVMIN,YVMAX,
     -            0.01,100.0,LSTR,AUXSTR)
*   Obtain the zoom factor.
            IRETRY=0
270         CONTINUE
            IRETRY=IRETRY+1
            CALL GRQVL(IWKVL,IDEVVL,IERR,ZOOM)
            IF(IERR.NE.1.OR.ZOOM.LE.0.0)THEN
                 CALL GMSG(IWK,
     -                'Not a valid magnification, please try again.')
                 IF(IRETRY.LE.NRETRY)GOTO 270
                 GOTO 100
            ELSEIF(ABS(ZOOM-1.0).GE.1.0E-3)THEN
                 AUX1=DXMIN
                 AUX2=DXMAX
                 DXMIN=AUX1-ABS(AUX2-AUX1)*(ZOOM-1.0)/2.0
                 DXMAX=AUX2+ABS(AUX2-AUX1)*(ZOOM-1.0)/2.0
                 AUX1=DYMIN
                 AUX2=DYMAX
                 DYMIN=AUX1-ABS(AUX2-AUX1)*(ZOOM-1.0)/2.0
                 DYMAX=AUX2+ABS(AUX2-AUX1)*(ZOOM-1.0)/2.0
                 CALL GMSG(IWK,'Redrawing the axes')
                 CALL GRALOG('< graphics input screen >               ')
                 CALL GRNEXT
                 CALL GRAXIS(DXMIN,DYMIN,DXMAX,DYMAX,
     -                '                                        ')
                 CALL CELLAY(DXMIN,DYMIN,DXMAX,DYMAX)
                 XSING=0.5*(DXMIN+DXMAX)
                 YSING=0.5*(DYMIN+DYMAX)
            ENDIF
**  New track.
       ELSEIF(ICHOIC.EQ.4)THEN
*   Plot the current track in a segment.
            IF(TRFLAG(1))THEN
                 XPL(1)=XT0
                 YPL(1)=YT0
                 XPL(2)=XT1
                 YPL(2)=YT1
                 CALL GCRSG(2)
                 CALL GRATTS('TRACK','POLYLINE')
                 CALL GPL(2,XPL,YPL)
                 CALL GCLSG
            ENDIF
*   Prompt the user for one end point.
            CALL GMSG(IWK,'Please point to one end point.')
*   Initialise the LOCATOR to get the point.
            LSTR=0
            CALL GINLC(IWKLC,IDEVLC,1,XT0,YT0,ILPET1,
     -           XLMIN,XLMAX,YLMIN,YLMAX,LSTR,AUXSTR)
*   Get the point.
            IRETRY=0
230         CONTINUE
            IRETRY=IRETRY+1
            CALL GRQLC(IWKLC,IDEVLC,IERR,NT,PX,PY)
            IF(NT.NE.1.OR.IERR.NE.1)THEN
                 CALL GMSG(IWK,'Please point in the current AREA.')
                 IF(IRETRY.LE.NRETRY)GOTO 230
                 GOTO 100
            ENDIF
            XT0N=PX
            YT0N=PY
*   Prompt the user for the other edge point.
            CALL GMSG(IWK,'Please point to the other end.')
*   Initialise the LOCATOR to get the point.
            LSTR=0
            CALL GINLC(IWKLC,IDEVLC,1,PX,PY,ILPET2,
     -           XLMIN,XLMAX,YLMIN,YLMAX,LSTR,AUXSTR)
*   Get the point.
            IRETRY=0
240         CONTINUE
            IRETRY=IRETRY+1
            CALL GRQLC(IWKLC,IDEVLC,IERR,NT,PX,PY)
            IF(NT.NE.1.OR.IERR.NE.1)THEN
                 CALL GMSG(IWK,'Please point in the current AREA.')
                 IF(IRETRY.LE.NRETRY)GOTO 240
                 GOTO 100
            ENDIF
*   Update the track.
            XT0=XT0N
            YT0=YT0N
            XT1=PX
            YT1=PY
*   Drop the segment storing the old track.
            IF(TRFLAG(1))CALL GDSG(2)
            TRFLAG(1)=.TRUE.
**  Single drift-line.
       ELSEIF(ICHOIC.EQ.5)THEN
*   Prompt the user for the starting point.
            CALL GMSG(IWK,'Please point to the starting point.')
*   Initialise the LOCATOR to get the point.
            LSTR=0
            XPOS=XSING
            YPOS=YSING
            IF(POLAR)CALL CFMRTC(XPOS,YPOS,XPOS,YPOS,1)
            CALL GINLC(IWKLC,IDEVLC,1,XPOS,YPOS,ILPET1,
     -           XLMIN,XLMAX,YLMIN,YLMAX,LSTR,AUXSTR)
*   Get the point.
            IRETRY=0
250         CONTINUE
            IRETRY=IRETRY+1
            CALL GRQLC(IWKLC,IDEVLC,IERR,NT,PX,PY)
            XPOS=PX
            YPOS=PY
            IF(POLAR)CALL CFMCTR(XPOS,YPOS,XPOS,YPOS,1)
            IF(NT.NE.1.OR.IERR.NE.1.OR.XPOS.LT.DXMIN.OR.XPOS.GT.DXMAX
     -           .OR.YPOS.LT.DYMIN.OR.YPOS.GT.DYMAX)THEN
                 CALL GMSG(IWK,'Please point in the current AREA.')
                 IF(IRETRY.LE.NRETRY)GOTO 250
                 GOTO 100
            ENDIF
            XSING=XPOS
            YSING=YPOS
            CALL DLCALC(XPOS,YPOS,0.0,Q,ITYPE)
            IF(POLAR)CALL CF2RTC(XU,YU,XU,YU,NU)
            IF(ITYPE.EQ.2)THEN
                 CALL GRATTS('ION-DRIFT-LINE','POLYLINE')
            ELSE
                 CALL GRATTS('E-DRIFT-LINE','POLYLINE')
            ENDIF
            IF(NU.GE.2)CALL GPL2(NU,XU,YU)
**  Drift-lines from a wire.
       ELSEIF(ICHOIC.EQ.6)THEN
*   Invite the user to point to one of the wires.
            CALL GMSG(IWK,'Please select a wire.')
*   Initialise the PICK device.
            LSTR=0
            CALL GINPK(IWKPK,IDEVPK,1,1,1,IPPET,
     -           XPMIN,XPMAX,YPMIN,YPMAX,LSTR,AUXSTR)
*   Get the wire number.
            IRETRY=0
260         CONTINUE
            IRETRY=IRETRY+1
            CALL GRQPK(IWKPK,IDEVPK,IERR,ISGNA,IPCID)
*   Check the choice is valid.
            IF(IERR.NE.1.OR.IPCID.LE.0.OR.IPCID.GT.NWIRE)THEN
                 CALL GMSG(IWK,'Invalid choice, please try again.')
                 IF(IRETRY.LE.NRETRY)GOTO 260
                 GOTO 100
            ELSEIF(-Q*E(IPCID).LT.0.AND.LREPSK)THEN
                 CALL GMSG(IWK,
     -                'This wire attracts the particles, try again.')
                 IF(IRETRY.LE.NRETRY)GOTO 260
                 GOTO 100
            ENDIF
*   Get a reasonable distance from the wire.
            IF(-Q*E(IPCID).LT.0)THEN
                 RDIST=0.51*RTRAP*D(IPCID)
            ELSE
                 RDIST=0.51*D(IPCID)
            ENDIF
*   Figure out how many periods are covered by the present AREA.
            NXMIN=0
            NXMAX=0
            NYMIN=0
            NYMAX=0
            IF(PERX)THEN
                 NXMIN=INT(DXMIN/SX)-1
                 NXMAX=INT(DXMAX/SX)+1
            ENDIF
            IF(PERY)THEN
                 NYMIN=INT(DYMIN/SY)-1
                 NYMAX=INT(DYMAX/SY)+1
            ENDIF
*   Loop over the periods.
            DO 330 NX=NXMIN,NXMAX
            XPOS=X(IPCID)+NX*SX
            IF(XPOS.LE.DXMIN.OR.XPOS.GE.DXMAX)GOTO 330
            DO 320 NY=NYMIN,NYMAX
            YPOS=Y(IPCID)+NY*SY
            IF(YPOS.LE.DYMIN.OR.YPOS.GE.DYMAX)GOTO 320
*   Loop over the angles.
            DO 340 IANG=1,NLINED
            ANGLE=REAL(IANG)*2*PI/REAL(NLINED)
            CALL DLCALC(XPOS+RDIST*COS(ANGLE),YPOS+RDIST*SIN(ANGLE),
     -           0.0,-Q,ITYPE)
            IF(POLAR)CALL CF2RTC(XU,YU,XU,YU,NU)
            IF(ITYPE.EQ.2)THEN
                 CALL GRATTS('ION-DRIFT-LINE','POLYLINE')
            ELSE
                 CALL GRATTS('E-DRIFT-LINE','POLYLINE')
            ENDIF
            IF(NU.GE.2)CALL GPL2(NU,XU,YU)
340         CONTINUE
320         CONTINUE
330         CONTINUE
**  Drift from the track.
       ELSEIF(ICHOIC.EQ.7)THEN
            IF(.NOT.TRFLAG(1))THEN
                 CALL GMSG(IWK,'No track defined sofar.')
                 GOTO 100
            ENDIF
*   Plot the track.
            XPL(1)=XT0
            YPL(1)=YT0
            XPL(2)=XT1
            YPL(2)=YT1
            CALL GRATTS('TRACK','POLYLINE')
            CALL GPL(2,XPL,YPL)
*   And plot drift-lines.
            DO 300 I=1,NLINED
            XPOS=XT0+REAL(I-1)*(XT1-XT0)/REAL(NLINED-1)
            YPOS=YT0+REAL(I-1)*(YT1-YT0)/REAL(NLINED-1)
            IF(POLAR)CALL CFMCTR(XPOS,YPOS,XPOS,YPOS,1)
            CALL DLCALC(XPOS,YPOS,0.0,Q,ITYPE)
            IF(POLAR)CALL CF2RTC(XU,YU,XU,YU,NU)
            IF(ITYPE.EQ.2)THEN
                 CALL GRATTS('ION-DRIFT-LINE','POLYLINE')
            ELSE
                 CALL GRATTS('E-DRIFT-LINE','POLYLINE')
            ENDIF
            IF(NU.GE.2)CALL GPL2(NU,XU,YU)
300         CONTINUE
**  Clear the page.
       ELSEIF(ICHOIC.EQ.8)THEN
            CALL GMSG(IWK,'Redrawing the axes.')
            CALL GRALOG('< graphics input screen >               ')
            CALL GRNEXT
            CALL GRAXIS(DXMIN,DYMIN,DXMAX,DYMAX,
     -           '                                        ')
            CALL CELLAY(DXMIN,DYMIN,DXMAX,DYMAX)
            XSING=0.5*(DXMIN+DXMAX)
            YSING=0.5*(DYMIN+DYMAX)
**  The parameter menu.
       ELSEIF(ICHOIC.EQ.9)THEN
*   Initialise the menu.
400         CONTINUE
            NC=1
*   Back to main menu.
            CHSTR(NC:NC+4)='Quit$'
            NC=NC+5
*   Switch particle type.
            IF(ITYPE.EQ.1)THEN
                 CHSTR(NC:NC+10)='Drift ions$'
                 NC=NC+11
            ELSE
                 CHSTR(NC:NC+15)='Drift electrons$'
                 NC=NC+16
            ENDIF
*   Switch particle charge.
            IF(Q.LE.0)THEN
                 CHSTR(NC:NC+16)='Set charge to +1$'
                 NC=NC+17
            ELSE
                 CHSTR(NC:NC+16)='Set charge to -1$'
                 NC=NC+17
            ENDIF
*   Number of drift-lines.
            CALL OUTFMT(REAL(NLINED),2,AUXSTR,NCAUX,'LEFT')
            CHSTR(NC:NC+18+NCAUX)=
     -           'Number of lines ['//AUXSTR(1:NCAUX)//']$'
            NC=NC+19+NCAUX
*   Trap radius.
            CALL OUTFMT(RTRAP,2,AUXSTR,NCAUX,'LEFT')
            CHSTR(NC:NC+14+NCAUX)='Trap radius ['//AUXSTR(1:NCAUX)//']$'
            NC=NC+15+NCAUX
*   Epsilon.
            CALL OUTFMT(EPSDIF,2,AUXSTR,NCAUX,'LEFT')
            CHSTR(NC:NC+11+NCAUX)='Accuracy ['//AUXSTR(1:NCAUX)//']$'
            NC=NC+12+NCAUX
*   Checking options.
            IF(LREPSK)THEN
                 CHSTR(NC:NC+20)='Skip repelling wires$'
                 NC=NC+21
            ELSE
                 CHSTR(NC:NC+15)='Check all wires$'
                 NC=NC+16
            ENDIF
*   Read the user request from the menu.
            ICHOIC=2
            CALL GRMENU(CHSTR(1:NC-1),'$',XCMIN,YCMIN,XCMAX,YCMAX,
     -           IWKCH,IDEVCH,ICPET,ICHOIC,IFAIL)
*   Check the return code.
            IF(IFAIL.NE.0)THEN
                 CALL GRALOG('< graphics input screen >               ')
                 CALL GRNEXT
                 PRINT *,' !!!!!! DRFGRA WARNING : Unable to read a'//
     -                ' choice with secondary menu.'
                 CALL TIMLOG('Drift section with graphics input.      ')
                 RETURN
            ENDIF
*   Act according to the choice, first back to the main menu.
            IF(ICHOIC.EQ.1)THEN
                 GOTO 100
*   Particle type.
            ELSEIF(ICHOIC.EQ.2)THEN
                 ITYPE=3-ITYPE
*   Charge.
            ELSEIF(ICHOIC.EQ.3)THEN
                 Q=-Q
*   Number of drift-lines.
            ELSEIF(ICHOIC.EQ.4)THEN
                 CALL GMSG(IWK,
     -                'Please enter the new number of drift-lines.')
*   Initialise the VALUATOR.
                 LSTR=0
                 CALL GINVL(IWKVL,IDEVVL,REAL(NLINED),IVPET,
     -                XVMIN,XVMAX,YVMIN,YVMAX,2.0,100.0,LSTR,AUXSTR)
*   Obtain the NLINED number.
                 CALL GRQVL(IWKVL,IDEVVL,IERR,AUX)
                 IF(IERR.NE.1.OR.AUX.LE.2.0.OR.AUX.GE.100.0)THEN
                      CALL GMSG(IWK,
     -                     'Not a valid number of drift-lines.')
                 ELSE
                      NLINED=INT(AUX)
                 ENDIF
*   Trap radius.
            ELSEIF(ICHOIC.EQ.5)THEN
                 CALL GMSG(IWK,
     -                'Please enter the new trapping radius.')
*   Initialise the VALUATOR.
                 LSTR=0
                 CALL GINVL(IWKVL,IDEVVL,RTRAP,IVPET,
     -                XVMIN,XVMAX,YVMIN,YVMAX,1.0,100.0,LSTR,AUXSTR)
*   Obtain the trap radius.
                 CALL GRQVL(IWKVL,IDEVVL,IERR,AUX)
                 IF(IERR.NE.1.OR.AUX.LE.1.0.OR.AUX.GE.100.0)THEN
                      CALL GMSG(IWK,'Not a valid trapping radius.')
                 ELSE
                      RTRAP=AUX
                 ENDIF
*   Epsilon.
            ELSEIF(ICHOIC.EQ.6)THEN
                 CALL GMSG(IWK,
     -                'Please enter the new accuracy.')
*   Initialise the VALUATOR.
                 LSTR=0
                 CALL GINVL(IWKVL,IDEVVL,EPSDIF,IVPET,
     -                XVMIN,XVMAX,YVMIN,YVMAX,1.0E-10,1.0,LSTR,AUXSTR)
*   Obtain the accuracy parameter.
                 CALL GRQVL(IWKVL,IDEVVL,IERR,AUX)
                 IF(IERR.NE.1.OR.AUX.LE.0.0)THEN
                      CALL GMSG(IWK,'Not a valid accuracy.')
                 ELSE
                      EPSDIF=AUX
                 ENDIF
*   Skip/check of repelling wires.
            ELSEIF(ICHOIC.EQ.7)THEN
                 LREPSK=.NOT.LREPSK
*   Any other choice.
            ELSE
                 CALL GMSG(IWK,'Invalid choice, please try again.')
            ENDIF
            GOTO 400
**  Something unknown.
       ELSE
            CALL GMSG(IWK,'Invalid choice, try again.')
       ENDIF
*** Return for a new cycle.
       GOTO 100
       END
