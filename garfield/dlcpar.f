CDECK  ID>, DLCPAR.
       SUBROUTINE DLCPAR
*-----------------------------------------------------------------------
*   DLCPAR - Routine taking care of drift line integration parameters.
*   VARIABLES :
*   (Last changed on 13/ 3/14.)
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
       CHARACTER*40 STRDF2,STRMC
       CHARACTER*20 AUX1,AUX2,AUX3,AUX4,AUX5,AUX6,AUX7
       INTEGER INPCMP,NMCR,INEXT,MXSTR,I,INPTYP,NWORD,IFAIL1,IFAIL2,
     -      NINORR,NC1,NC2,NC3,NC4,NC5,NC6,NC7
       REAL TMCR,DMCR,EPSR,RDF2R,RTRAPR,STMAXR,EQTTRR,EQTASR,EQTCLR,
     -      DSCMIR,DSCMAR,DTFACR
       EXTERNAL INPCMP
*** Get the number of words on the line.
       CALL INPNUM(NWORD)
*** If there is only one argument.
       IF(NWORD.EQ.1)THEN
            CALL OUTFMT(EPSDIF,2,AUX1,NC1,'LEFT')
            WRITE(LUNOUT,'(/
     -           ''  RUNGE-KUTTA DRIFT LINE INTEGRATION PARAMETERS:''/
     -           ''  Absolute accuracy for drift line'',
     -           '' integration:            '',A,'' cm'')') AUX1(1:NC1)
            IF(LSTMAX)THEN
                 CALL OUTFMT(STMAX,2,AUX1,NC1,'LEFT')
                 WRITE(LUNOUT,'(
     -                ''  Maximum length of an integration step:'',
     -                ''                   '',A,'' cm'')') AUX1(1:NC1)
            ELSE
                 WRITE(LUNOUT,'(
     -                ''  Maximum length of an integration step:'',
     -                ''                   Unlimited'')')
            ENDIF
            IF(MCMETH.EQ.0)THEN
                 STRMC='Take constant time steps.'
            ELSEIF(MCMETH.EQ.1)THEN
                 STRMC='Take constant distance steps.'
            ELSEIF(MCMETH.EQ.2)THEN
                 STRMC='Simulate collisions.'
            ELSE
                 STRMC='# Method not known #'
            ENDIF
            WRITE(LUNOUT,'(/
     -           ''  MONTE CARLO DRIFT LINE INTEGRATION PARAMETERS:''/
     -           ''  Monte Carlo integration method:      '',
     -           ''                    '',A)') STRMC
            CALL OUTFMT(REAL(TMC),2,AUX1,NC1,'LEFT')
            CALL OUTFMT(REAL(DMC),2,AUX2,NC2,'LEFT')
            CALL OUTFMT(REAL(NMC),2,AUX3,NC3,'LEFT')
            CALL OUTFMT(DSCMIN,2,AUX4,NC4,'LEFT')
            CALL OUTFMT(DSCMAX,2,AUX5,NC5,'LEFT')
            WRITE(LUNOUT,'(
     -           ''  Time interval between steps for'',
     -           '' MC integration:          '',A,'' microsec''/
     -           ''  Space interval between steps for'',
     -           '' MC integration:         '',A,'' cm''/
     -           ''  Number of collisions to be averaged'',
     -           '' over:                '',A/
     -           ''  Range of permitted diffusion scaling'',
     -           '' factors:            '',A,'' to '',A)')
     -           AUX1(1:NC1),AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4),
     -           AUX5(1:NC5)
            CALL OUTFMT(DTFACT,2,AUX1,NC1,'LEFT')
            WRITE(LUNOUT,'(
     -           ''  Time step scaling factor for microscopic'',
     -           '' tracking:       '',A)') AUX1(1:NC1)
            CALL OUTFMT(RTRAP,2,AUX1,NC1,'LEFT')
            WRITE(LUNOUT,'(/
     -           ''  DRIFT LINE TERMINATION PARAMETERS:''/
     -           ''  Distance at which particles are caught'',
     -           '' (TRAP-RADIUS):    '',A,'' wire radii''/
     -           ''  Skip the capture check for'',
     -           '' repelling wires:              '',L1/
     -           ''  Abandon drift line at sharp kinks'',
     -           '' (REJECT-KINKS):        '',L1)')
     -           AUX1(1:NC1),LREPSK,LKINK
            CALL OUTFMT(REAL(NINORD),2,AUX3,NC3,'LEFT')
            WRITE(LUNOUT,'(/
     -           ''  DRIFT LINE INTERPOLATION PARAMETERS:''/
     -           ''  Interpolation order:             '',
     -           ''                        '',A/
     -           ''  Compute (T) or abandon (F) if interpolation'',
     -           '' fails:       '',L1)') AUX1(1:NC1),LINCAL
            IF(MDF2.EQ.0)THEN
                 STRDF2='No special treatment'
            ELSEIF(MDF2.EQ.1)THEN
                 STRDF2='Integrate distance/velocity'
            ELSEIF(MDF2.EQ.2)THEN
                 STRDF2='Integrate distance/central velocity'
            ELSEIF(MDF2.EQ.3)THEN
                 STRDF2='Take longitudinal size.'
            ELSEIF(MDF2.EQ.4)THEN
                 STRDF2='Take largest cloud axis.'
            ELSE
                 STRDF2='# Method not known #'
            ENDIF
            CALL OUTFMT(RDF2,2,AUX1,NC1,'LEFT')
            CALL OUTFMT(REAL(MXDIFS),2,AUX2,NC2,'LEFT')
            CALL OUTFMT(REAL(MXTWNS),2,AUX3,NC3,'LEFT')
            CALL OUTFMT(REAL(MXATTS),2,AUX4,NC4,'LEFT')
            CALL OUTFMT(EPSDFI,2,AUX5,NC5,'LEFT')
            CALL OUTFMT(EPSTWI,2,AUX6,NC6,'LEFT')
            CALL OUTFMT(EPSATI,2,AUX7,NC7,'LEFT')
            WRITE(LUNOUT,'(/
     -           ''  DIFFUSION, AVALANCHE AND ATTACHMENT SUMMING'',
     -           '' PARAMETERS:''/
     -           ''  Cloud projection method for electrons'',
     -           '' hitting a wire:    '',A/
     -           ''  Switch L+T diffusion integration method:'',
     -           ''                 '',A,'' wire radii''/
     -           ''  Maximum stack depth for the diffusion'',
     -           '' integration:       '',A/
     -           ''  Maximum stack depth for the Townsend'',
     -           '' integration:        '',A/
     -           ''  Maximum stack depth for the attachment'',
     -           '' integration:      '',A/
     -           ''  Relative accuracy tolerance diffusion'',
     -           '' integration:       '',A/
     -           ''  Relative accuracy tolerance Townsend'',
     -           '' integration:        '',A/
     -           ''  Relative accuracy tolerance attachment'',
     -           '' integration:      '',A/
     -           ''  Compute multiplication over projected'',
     -           '' drift line:        '',L1)')
     -           STRDF2,AUX1(1:NC1),AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4),
     -           AUX5(1:NC5),AUX6(1:NC6),AUX7(1:NC7),LAVPRO
            CALL OUTFMT(EQTTHR,2,AUX1,NC1,'LEFT')
            CALL OUTFMT(EQTASP,2,AUX2,NC2,'LEFT')
            CALL OUTFMT(EQTCLS,2,AUX3,NC3,'LEFT')
            WRITE(LUNOUT,'(/
     -           ''  ISOCHRON PARAMETERS:''/
     -           ''  Maximum relative distance to connect'',
     -           '' isochron parts:     '',A/
     -           ''  Curves considered circular up to an'',
     -           '' aspect ratio of:     '',A/
     -           ''  Circular curves closed up to a relative'',
     -           '' distance of:     '',A/
     -           ''  Sort points on isochrons:              '',
     -           ''                  '',L1/
     -           ''  Avoid crossings between isochrons and'',
     -           '' drift lines:       '',L1/
     -           ''  Mark (T) or Draw (F) isochrons:        '',
     -           ''                  '',L1)')
     -           AUX1(1:NC1),AUX2(1:NC2),AUX3(1:NC3),
     -           LEQSRT,LEQCRS,LEQMRK
       ELSE
            INEXT=2
            DO 10 I=2,NWORD
            IF(I.LT.INEXT)GOTO 10
*   Diffusion stack size.
            IF(INPCMP(I,'DIFF#USION-ST#ACK-#DEPTH').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).NE.1)THEN
                      CALL INPMSG(I,'Should have an integer as arg.')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,1,IFAIL1)
                      CALL INPRDI(I+1,MXSTR,MXSTCK)
                      IF(MXSTR.GE.1.AND.MXSTR.LE.MXSTCK)THEN
                           MXDIFS=MXSTR
                      ELSE
                           CALL INPMSG(I+1,
     -                          'Value not in range 1 -> MXSTCK')
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Townsend stack size.
            ELSEIF(INPCMP(I,'TOWN#SEND-ST#ACK-#DEPTH').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).NE.1)THEN
                      CALL INPMSG(I,'Should have an integer as arg.')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,1,IFAIL1)
                      CALL INPRDI(I+1,MXSTR,MXSTCK)
                      IF(MXSTR.GE.1.AND.MXSTR.LE.MXSTCK)THEN
                           MXTWNS=MXSTR
                      ELSE
                           CALL INPMSG(I+1,
     -                          'Value not in range 1 -> MXSTCK')
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Attachment stack size.
            ELSEIF(INPCMP(I,'ATT#ACHMENT-ST#ACK-#DEPTH').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).NE.1)THEN
                      CALL INPMSG(I,'Should have an integer as arg.')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,1,IFAIL1)
                      CALL INPRDI(I+1,MXSTR,MXSTCK)
                      IF(MXSTR.GE.1.AND.MXSTR.LE.MXSTCK)THEN
                           MXATTS=MXSTR
                      ELSE
                           CALL INPMSG(I+1,
     -                          'Value not in range 1 -> MXSTCK')
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Diffusion accuracy.
            ELSEIF(INPCMP(I,'DIFF#USION-ACC#URACY').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).LE.0)THEN
                      CALL INPMSG(I,'Should have a numeric argument')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,EPSR,1.0E-3)
                      IF(EPSR.GT.0.0.AND.IFAIL1.EQ.0)THEN
                           EPSDFI=EPSR
                      ELSE
                           CALL INPMSG(I+1,
     -                          'This value must be positive.  ')
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Townsend accuracy.
            ELSEIF(INPCMP(I,'TOWN#SEND-ACC#URACY').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).LE.0)THEN
                      CALL INPMSG(I,'Should have a numeric argument')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,EPSR,1.0E-3)
                      IF(EPSR.GT.0.0.AND.IFAIL1.EQ.0)THEN
                           EPSTWI=EPSR
                      ELSE
                           CALL INPMSG(I+1,
     -                          'This value must be positive.  ')
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Attachment accuracy.
            ELSEIF(INPCMP(I,'ATT#ACHMENT-ACC#URACY').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).LE.0)THEN
                      CALL INPMSG(I,'Should have a numeric argument')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,EPSR,1.0E-3)
                      IF(EPSR.GT.0.AND.IFAIL1.EQ.0)THEN
                           EPSATI=EPSR
                      ELSE
                           CALL INPMSG(I+1,
     -                          'This value must be positive.')
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Projected or true drift line.
            ELSEIF(INPCMP(I,'PROJ#ECTED-#PATH-#INTEGRATION').NE.0)THEN
                 LAVPRO=.TRUE.
            ELSEIF(INPCMP(I,'TRUE-PATH-#INTEGRATION').NE.0)THEN
                 LAVPRO=.FALSE.
*   Integration accuracy.
            ELSEIF(INPCMP(I,'INT#EGRATION-ACC#URACY')+
     -           INPCMP(I,'EPS#ILON').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).LE.0)THEN
                      CALL INPMSG(I,'Should have a numeric argument')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,EPSR,1.0E-6)
                      IF(IFAIL1.EQ.0.AND.EPSR.LE.0.0)THEN
                           CALL INPMSG(I+1,
     -                          'Integration accuracy not > 0. ')
                      ELSEIF(IFAIL1.EQ.0)THEN
                           EPSDIF=EPSR
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   MC integration time interval.
            ELSEIF(INPCMP(I,'M#ONTE-C#ARLO-T#IME-#INTERVAL')+
     -           INPCMP(I,'MC-T#IME-#INTERVAL').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).LE.0)THEN
                      CALL INPMSG(I,'Should have a numeric argument')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,TMCR,0.001)
                      IF(IFAIL1.EQ.0.AND.TMCR.LE.0.0)THEN
                           CALL INPMSG(I+1,
     -                          'Integration interval not > 0. ')
                      ELSEIF(IFAIL1.EQ.0)THEN
                           TMC=TMCR
                      ENDIF
                      MCMETH=0
                      INEXT=I+2
                 ENDIF
*   MC integration distance interval.
            ELSEIF(INPCMP(I,'M#ONTE-C#ARLO-D#ISTANCE-#INTERVAL')+
     -           INPCMP(I,'M#ONTE-C#ARLO-SP#ACE-#INTERVAL')+
     -           INPCMP(I,'M#ONTE-C#ARLO-SP#ATIAL-#INTERVAL')+
     -           INPCMP(I,'MC-D#ISTANCE-#INTERVAL')+
     -           INPCMP(I,'MC-SP#ACE-#INTERVAL')+
     -           INPCMP(I,'MC-SP#ATIAL-#INTERVAL').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).LE.0)THEN
                      CALL INPMSG(I,'Should have a numeric argument')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,DMCR,0.1)
                      IF(IFAIL1.EQ.0.AND.DMCR.LE.0.0)THEN
                           CALL INPMSG(I+1,
     -                          'Integration interval not > 0. ')
                      ELSEIF(IFAIL1.EQ.0)THEN
                           DMC=DMCR
                      ENDIF
                      MCMETH=1
                      INEXT=I+2
                 ENDIF
*   MC integration step averaging.
            ELSEIF(INPCMP(I,'M#ONTE-C#ARLO-ST#EPS')+
     -           INPCMP(I,'M#ONTE-C#ARLO-C#OLLISIONS')+
     -           INPCMP(I,'MC-ST#EPS')+
     -           INPCMP(I,'MC-C#OLLISIONS').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).LE.0)THEN
                      CALL INPMSG(I,'Should have an integer arg.')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,1,IFAIL1)
                      CALL INPRDI(I+1,NMCR,100)
                      IF(IFAIL1.EQ.0.AND.NMCR.LE.0)THEN
                           CALL INPMSG(I+1,
     -                          'Number of collisions not > 0.')
                      ELSEIF(IFAIL1.EQ.0)THEN
                           NMC=NMCR
                      ENDIF
                      MCMETH=2
                      INEXT=I+2
                 ENDIF
*   Diffusion scaling range.
            ELSEIF(INPCMP(I,'DIFF#USION-SC#ALING-#RANGE').NE.0)THEN
                 IF(I+2.GT.NWORD.OR.INPTYP(I+1).LE.0.OR.
     -                INPTYP(I+2).LE.0)THEN
                      CALL INPMSG(I,'Should have 2 numeric args')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,DSCMIR,DSCMIN)
                      IF(IFAIL1.EQ.0.AND.DSCMIR.LT.0.0)THEN
                           CALL INPMSG(I+1,
     -                          'Minimum must not be < 0. ')
                      ELSEIF(IFAIL1.EQ.0)THEN
                           DSCMIN=DSCMIR
                      ENDIF
                      CALL INPCHK(I+2,2,IFAIL2)
                      CALL INPRDR(I+2,DSCMAR,DSCMAX)
                      IF(IFAIL2.EQ.0.AND.DSCMAR.LE.1.0)THEN
                           CALL INPMSG(I+1,
     -                          'Minimum must be > 1. ')
                      ELSEIF(IFAIL2.EQ.0)THEN
                           DSCMAX=DSCMAR
                      ENDIF
                      INEXT=I+3
                 ENDIF
*   Microscopic step scaling.
            ELSEIF(INPCMP(I,'MIC#ROSCOPIC-TR#ACKING-SC#ALING').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).LE.0)THEN
                      CALL INPMSG(I,'Should have a numeric argument')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,DTFACR,0.0)
                      IF(IFAIL1.EQ.0.AND.DTFACR.LE.0.0)THEN
                           CALL INPMSG(I+1,
     -                          'Scaling is not > 0. ')
                      ELSEIF(IFAIL1.EQ.0)THEN
                           DTFACT=DTFACR
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Trap radius.
            ELSEIF(INPCMP(I,'TRAP-#RADIUS').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).LE.0)THEN
                      CALL INPMSG(I,'Should have a numeric argument')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,RTRAPR,0.0)
                      IF(IFAIL1.EQ.0.AND.RTRAPR.LE.0.0)THEN
                           CALL INPMSG(I+1,
     -                          'Number of wire radii not > 0. ')
                      ELSEIF(IFAIL1.EQ.0)THEN
                           RTRAP=RTRAPR
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Maximum step length.
            ELSEIF(INPCMP(I,'MAX#IMUM-ST#EP-#LENGTH').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).LE.0)THEN
                      CALL INPMSG(I,'Should have a numeric argument')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,STMAXR,0.0)
                      IF(IFAIL1.EQ.0.AND.STMAXR.LE.0.0)THEN
                           CALL INPMSG(I+1,
     -                          'Step length must be > 0.')
                      ELSEIF(IFAIL1.EQ.0)THEN
                           STMAX=STMAXR
                           LSTMAX=.TRUE.
                      ENDIF
                      INEXT=I+2
                 ENDIF
            ELSEIF(INPCMP(I,'NOMAX#IMUM-ST#EP-#LENGTH').NE.0)THEN
                 LSTMAX=.FALSE.
*   Check of repelling wires.
            ELSEIF(INPCMP(I,'CH#ECK-ALL-#WIRES').NE.0)THEN
                 LREPSK=.FALSE.
            ELSEIF(INPCMP(I,'CH#ECK-ATT#RACTING-#WIRES').NE.0)THEN
                 LREPSK=.TRUE.
*   Check for kinks.
            ELSEIF(INPCMP(I,'CH#ECK-K#INKS')+
     -           INPCMP(I,'K#INKS-CH#ECK')+
     -           INPCMP(I,'REJ#ECT-K#INKS')+
     -           INPCMP(I,'K#INKS-REJ#ECT').NE.0)THEN
                 LKINK=.TRUE.
            ELSEIF(INPCMP(I,'NOCH#ECK-K#INKS')+
     -           INPCMP(I,'NOK#INKS-CH#ECK')+
     -           INPCMP(I,'NOREJ#ECT-K#INKS')+
     -           INPCMP(I,'NOK#INKS-REJ#ECT').NE.0)THEN
                 LKINK=.FALSE.
*   Interpolation order.
            ELSEIF(INPCMP(I,'INT#ERPOLATION-ORD#ER').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Should have an argument')
                 ELSEIF(INPCMP(I+1,'LIN#EAR').NE.0)THEN
                      NINORD=1
                      INEXT=I+2
                 ELSEIF(INPCMP(I+1,'QUAD#RATIC')+
     -                INPCMP(I+1,'PARA#BOLIC').NE.0)THEN
                      NINORD=2
                      INEXT=I+2
                 ELSEIF(INPCMP(I+1,'CUB#IC').NE.0)THEN
                      NINORD=3
                      INEXT=I+2
                 ELSEIF(INPTYP(I+1).NE.1)THEN
                      CALL INPMSG(I,'Should have 1 integer argument')
                 ELSE
                      CALL INPCHK(I+1,1,IFAIL1)
                      CALL INPRDI(I+1,NINORR,NINORD)
                      IF(IFAIL1.EQ.0.AND.(NINORR.LT.1.OR.
     -                     NINORR.GT.10))THEN
                           CALL INPMSG(I+1,'Not in the range [1,10].')
                      ELSEIF(IFAIL1.EQ.0)THEN
                           NINORD=NINORR
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Compute or abandon drift lines which can't be interpolated.
            ELSEIF(INPCMP(I,'ABANDON-#IF-#INTERPOLATION-#FAILS').NE.
     -           0)THEN
                 LINCAL=.FALSE.
            ELSEIF(INPCMP(I,'COMP#UTE-#IF-#INTERPOLATION-#FAILS').NE.
     -           0)THEN
                 LINCAL=.TRUE.
*   Switch integration method.
            ELSEIF(INPCMP(I,'CL#OUD-PROJ#ECTION-DIST#ANCE').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).LE.0)THEN
                      CALL INPMSG(I,'Should have a numeric argument')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,RDF2R,2.0)
                      IF(IFAIL1.EQ.0.AND.RDF2R.LT.0.0)THEN
                           CALL INPMSG(I+1,
     -                          'Number of wire radii not > 0. ')
                      ELSEIF(IFAIL1.EQ.0)THEN
                           RDF2=RDF2R
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Cloud projection method.
            ELSEIF(INPCMP(I,'CL#OUD-PROJ#ECTION-METH#OD').NE.0)THEN
                 INEXT=I+2
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).LE.0)THEN
                      CALL INPMSG(I,'Should have a numeric argument.')
                      INEXT=I+1
                 ELSEIF(INPCMP(I+1,'NO-#PROJECTION').NE.0)THEN
                      MDF2=0
                 ELSEIF(INPCMP(I+1,'INT#EGRATION').NE.0)THEN
                      MDF2=1
                 ELSEIF(INPCMP(I+1,'CENT#RAL-#VELOCITY-#INTEGRATION')
     -                .NE.0)THEN
                      MDF2=2
                 ELSEIF(INPCMP(I+1,'LONG#ITUDINAL-#DIMENSION').NE.0)THEN
                      MDF2=3
                 ELSEIF(INPCMP(I+1,'LARG#EST-#DIMENSION').NE.0)THEN
                      MDF2=4
                 ELSE
                      CALL INPMSG(I+1,'Not a known method.')
                 ENDIF
*   Isochron connection threshold.
            ELSEIF(INPCMP(I,'ISO#CHRONE-CONN#ECTION-#THRESHOLD').NE.
     -           0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Should have a numeric argument.')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,EQTTRR,EQTTHR)
                      IF(EQTTRR.LE.0.0.OR.EQTTRR.GT.1)THEN
                           CALL INPMSG(I+1,
     -                          'Threshold not between 0 and 1.')
                      ELSEIF(IFAIL1.EQ.0)THEN
                           EQTTHR=EQTTRR
                      ENDIF
                      INEXT=I+2
                 ENDIF
            ELSEIF(INPCMP(I,'NOISO#CHRONE-CONN#ECTION-#THRESHOLD').NE.
     -           0)THEN
                 EQTTHR=1.0
*   Isochron aspect ratio switch.
            ELSEIF(INPCMP(I,'ISO#CHRONE-ASP#ECT-#RATIO-#SWITCH').NE.
     -           0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Should have a numeric argument.')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,EQTASR,EQTASP)
                      IF(EQTASR.LE.0.0)THEN
                           CALL INPMSG(I+1,
     -                          'Ratio should be larger than 0.')
                      ELSEIF(IFAIL1.EQ.0)THEN
                           EQTASP=EQTASR
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Isochron loop closing threshold.
            ELSEIF(INPCMP(I,'ISO#CHRONE-LOOP-#THRESHOLD').NE.0)THEN
                 IF(I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'Should have a numeric argument.')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,EQTCLR,EQTCLS)
                      IF(EQTCLR.LE.0.0.OR.EQTCLR.GT.1)THEN
                           CALL INPMSG(I+1,
     -                          'Threshold not between 0 and 1.')
                      ELSEIF(IFAIL1.EQ.0)THEN
                           EQTCLS=EQTCLR
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Sort isochrons or not.
            ELSEIF(INPCMP(I,'SORT-ISO#CHRONES')+
     -           INPCMP(I,'SORT-ISO#CHRONS').NE.0)THEN
                 LEQSRT=.TRUE.
            ELSEIF(INPCMP(I,'NOSORT-ISO#CHRONES')+
     -           INPCMP(I,'NOSORT-ISO#CHRONS').NE.0)THEN
                 LEQSRT=.FALSE.
*   Check crossings between isochrons and drift lines.
            ELSEIF(INPCMP(I,'CH#ECK-ISO#CHRONE-#CROSSINGS').NE.0)THEN
                 LEQCRS=.TRUE.
            ELSEIF(INPCMP(I,'NOCH#ECK-ISO#CHRONE-#CROSSINGS').NE.0)THEN
                 LEQCRS=.FALSE.
*   Mark isochrons.
            ELSEIF(INPCMP(I,'MARK-ISO#CHRONES')+
     -           INPCMP(I,'MARK-ISO#CHRONS').NE.0)THEN
                 LEQMRK=.TRUE.
            ELSEIF(INPCMP(I,'DRAW-ISO#CHRONES')+
     -           INPCMP(I,'DRAW-ISO#CHRONS').NE.0)THEN
                 LEQMRK=.FALSE.
*   Anything else.
            ELSE
                 CALL INPMSG(I,'Not a valid keyword; ignored. ')
            ENDIF
10          CONTINUE
       ENDIF
       CALL INPERR
       END
