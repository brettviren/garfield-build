CDECK  ID>, GRAINP.
       SUBROUTINE GRAINP
*-----------------------------------------------------------------------
*   GRAINP - Serves as a subsection reading graphics command lines.
*   (Last changed on  2/ 8/09.)
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
       REAL USERX0,USERX1,USERY0,USERY1,FRXMIN,FRXMAX,FRYMIN,FRYMAX,
     -      ARRANG,ARRLEN,BARFRC,DISPX0,DISPX1,DISPY0,DISPY1,
     -      GPXN,GPXN10,GPYN,GPYN10,GPXL,GPYL,GPXT
       LOGICAL LGRID,LGRALL,LOGX,LOGY,LSTAMP,LGCLRB,LGCLRA,
     -      LWAITA,LWAITB,LXCCH,LGLCLP,LGMCLP,LGACLP,LGTCLP,
     -      WKMULT(MXWKLS)
       INTEGER NWK,WKID(MXWKLS),WKCON(MXWKLS),WKFREF(MXWKLS),
     -         WKLUN(MXWKLS),WKSTAT(MXWKLS),WKSREQ(MXWKLS),
     -         NCWKNM(MXWKLS),NCSTMP,IGHIST,IGBAR,NCGKS
       CHARACTER*20 WKNAME(MXWKLS),WKATTR(MXWKLS)
       CHARACTER*80 STAMP
       CHARACTER*(MXNAME) GKSLOG
       COMMON /GRADAT/ USERX0,USERX1,USERY0,USERY1,ARRANG,ARRLEN,
     -      BARFRC,
     -      FRXMIN,FRXMAX,FRYMIN,FRYMAX,DISPX0,DISPX1,DISPY0,DISPY1,
     -      GPXN,GPXN10,GPYN,GPYN10,GPXL,GPYL,GPXT,
     -      LGRID,LGRALL,LOGX,LOGY,LSTAMP,LGCLRB,LGCLRA,LWAITA,LWAITB,
     -      LXCCH,LGLCLP,LGMCLP,LGACLP,LGTCLP,
     -      NWK,WKID,WKCON,WKFREF,WKLUN,WKSTAT,WKSREQ,NCWKNM,NCSTMP,
     -      IGHIST,IGBAR,NCGKS,WKMULT
       COMMON /GRACHR/ WKNAME,WKATTR,STAMP,GKSLOG
       LOGICAL XDONE(0:MXGRID,0:MXGRID),YDONE(0:MXGRID,0:MXGRID),
     -      TRANS,CLAB
       REAL GRID(0:MXGRID,0:MXGRID),EPSTRA,EPSGRA,CXMIN,CXMAX,CYMIN,
     -      CYMAX,STINIT,DNTHR,DXGRA,DYGRA
       INTEGER ILOCGR(0:MXGRID,0:MXGRID),NBITER,NNITER,NFC,NGCMAX
       COMMON /CONDAT/ GRID,XDONE,YDONE,ILOCGR,
     -      NBITER,NNITER,EPSTRA,EPSGRA,DXGRA,DYGRA,
     -      STINIT,DNTHR,CXMIN,CXMAX,CYMIN,CYMAX,NFC,NGCMAX,TRANS,CLAB
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       LOGICAL LOOP,LREL
       INTEGER INPCMP,INPTYP,NWORD,INEXT,I,NC,MXOPWK,MXACWK,MXWKAS,INIT,
     -      IERR,IKEY,IFAIL,IFAIL1,IFAIL2,IFAIL3,IFAIL4,
     -      NITERR,NSTEPR,IDEFM,IREGM,IEMPTY,IFRAME,
     -      LEVEL,ISTA,IDEFD,IUPDD,IDEF,IUPD,IWK,IDUM1,IDUM2,IDUM,NACT,
     -      NC1,NC2,NC3,NC4,NCOUT
       REAL EPSR,DNR,AUX,BARFRR,ARRLER,DISX0R,DISX1R,DISY0R,DISY1R
       CHARACTER*(MXCHAR) STRING,OUT
       CHARACTER*20 AUX1,AUX2,AUX3,AUX4
       EXTERNAL INPCMP,INPTYP
       SAVE INIT,MXOPWK,MXACWK,MXWKAS
*** Identify the subroutine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE GRAINP ///'
*** First call, figure out how many workstations there are.
       DATA INIT/0/
       IF(INIT.EQ.0)THEN
            CALL GQWKM(IERR,MXOPWK,MXACWK,MXWKAS)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GRAINP DEBUG   : '',
     -           '' MXOPWK='',I3,'', MXACWK='',I3,'', MXWKAS='',I3)')
     -           MXOPWK,MXACWK,MXWKAS
            INIT=1
       ENDIF
*** First pick up the number of words and the first word.
       CALL INPNUM(NWORD)
       CALL INPSTR(1,1,STRING,NC)
*** Check it is a graphics command.
       IF(STRING(1:1).NE.'!')RETURN
*** Determine whether it is a single command or not.
       IF(NWORD.EQ.1.AND.NC.EQ.1)THEN
            LOOP=.TRUE.
            PRINT *,' '
            PRINT *,' ------------------------------------------------'
            PRINT *,' ----------    Graphics subsection     ----------'
            PRINT *,' ------------------------------------------------'
            PRINT *,' '
            CALL INPPRM('Graphics','ADD-PRINT')
       ELSE
            LOOP=.FALSE.
       ENDIF
*** Return here if LOOP is .TRUE.
10     CONTINUE
       IF(LOOP)THEN
            CALL INPGET
            CALL INPNUM(NWORD)
       ENDIF
       CALL INPSTR(1,1,STRING,NC)
*** Skip blank lines and warn for section headers.
       IF(STRING(1:1).EQ.'&')THEN
            PRINT *,' !!!!!! GRAINP WARNING : The section cannot be'//
     -           ' left at this point; first type EXIT.'
            GOTO 1010
       ELSEIF(INDEX('$%?><@',STRING(1:1)).NE.0)THEN
            PRINT *,' !!!!!! GRAINP WARNING : This command cannot be'//
     -           ' executed at the present level; first type EXIT.'
            GOTO 1010
       ELSEIF(STRING(1:1).EQ.'*')THEN
            GOTO 1010
       ENDIF
       IF(LOOP.AND.(NWORD.EQ.0.OR.(NWORD.EQ.1.AND.NC.EQ.1.AND.
     -      STRING(1:1).EQ.'!')))GOTO 1010
       IF(.NOT.LOOP.AND.NC.EQ.1.AND.NWORD.EQ.1)RETURN
*** Set the position of the command.
       IF(NC.EQ.1.AND.STRING(1:1).EQ.'!')THEN
            IKEY=2
       ELSE
            IKEY=1
       ENDIF
*** The ACTIVATE-WORKSTATION command.
       IF(INPCMP(IKEY,'!ACT#IVATE-#WORKSTATION')+
     -      INPCMP(IKEY,'ACT#IVATE-#WORKSTATION').NE.0)THEN
            IF(NWORD.LE.IKEY)THEN
                 PRINT *,' !!!!!! GRAINP WARNING : You must specify'//
     -                ' a workstation name with this command.'
*   Arguments present ?
            ELSE
*   Have the workstation(s) activated.
                 DO 30 I=IKEY+1,NWORD
                 CALL INPSTR(I,I,STRING,NC)
                 CALL GRACWK(STRING(1:NC),'DELAY')
30               CONTINUE
            ENDIF
*** Add a workstation.
       ELSEIF(INPCMP(IKEY,'ADD-#WORKSTATION')+
     -      INPCMP(IKEY,'!ADD-#WORKSTATION').NE.0)THEN
            CALL GRADWK
*** Arrow tip angle.
       ELSEIF(INPCMP(IKEY,'ARR#OW-TOP-ANG#LE')+
     -      INPCMP(IKEY,'!ARR#OW-TOP-ANG#LE')+
     -      INPCMP(IKEY,'ARR#OW-TIP-ANG#LE')+
     -      INPCMP(IKEY,'!ARR#OW-TIP-ANG#LE')+
     -      INPCMP(IKEY,'ARR#OW-ANG#LE')+
     -      INPCMP(IKEY,'!ARR#OW-ANG#LE').NE.0)THEN
            IF(NWORD.EQ.IKEY)THEN
                 WRITE(LUNOUT,'(''  Current tip angle: '',F10.3,
     -                '' degrees.'')') ARRANG*180/PI
            ELSE
                 CALL INPCHK(IKEY+1,2,IFAIL1)
                 CALL INPRDR(IKEY+1,ARRANG,ARRANG*180/PI)
                 ARRANG=ARRANG*PI/180
                 CALL INPERR
            ENDIF
*** Arrow tip length.
       ELSEIF(INPCMP(IKEY,'ARR#OW-TIP-LEN#GTH')+
     -      INPCMP(IKEY,'!ARR#OW-TIP-LEN#GTH')+
     -      INPCMP(IKEY,'ARR#OW-LEN#GTH')+
     -      INPCMP(IKEY,'!ARR#OW-LEN#GTH').NE.0)THEN
            IF(NWORD.EQ.IKEY)THEN
                 IF(ARRLEN.LT.0)THEN
                      WRITE(LUNOUT,'(''  Current tip length: '',F10.3,
     -                     '' in NDC coordinates.'')') ABS(ARRLEN)
                 ELSE
                      WRITE(LUNOUT,'(''  Current tip length: '',F10.3,
     -                     '' times total length.'')') ARRLEN
                 ENDIF
            ELSE
                 LREL=.TRUE.
                 IF(IKEY+1.EQ.NWORD)THEN
                      LREL=.TRUE.
                 ELSEIF(INPCMP(IKEY+2,'REL#ATIVE').NE.0)THEN
                      LREL=.TRUE.
                 ELSEIF(INPCMP(IKEY+2,'ABS#OLUTE').NE.0)THEN
                      LREL=.FALSE.
                 ELSE
                      CALL INPMSG(IKEY+3,'Unknown option')
                      LREL=.TRUE.
                 ENDIF
                 IF(IKEY+2.LT.NWORD)
     -                CALL INPMSG(IKEY+3,'Spurious keywords')
                 CALL INPCHK(IKEY+1,2,IFAIL1)
                 CALL INPRDR(IKEY+1,ARRLER,ARRLEN)
                 IF(ARRLER.LE.0.OR.ARRLER.GT.1)THEN
                      CALL INPMSG(IKEY+1,'Not in range <0,1].')
                 ELSE
                      ARRLEN=ARRLER
                 ENDIF
                 IF(.NOT.LREL)ARRLEN=-ABS(ARRLEN)
                 CALL INPERR
            ENDIF
*** Bar chart width.
       ELSEIF(INPCMP(IKEY,'BAR#CHART-W#IDTH')+
     -      INPCMP(IKEY,'!BAR#CHART-W#IDTH')+
     -      INPCMP(IKEY,'BAR-CH#ART-W#IDTH')+
     -      INPCMP(IKEY,'!BAR-CH#ART-W#IDTH').NE.0)THEN
            IF(NWORD.EQ.IKEY)THEN
                 WRITE(LUNOUT,'(''  Current bar chart width: '',F10.3,
     -                '' times maximum width.'')') BARFRC
            ELSE
                 CALL INPCHK(IKEY+1,2,IFAIL1)
                 CALL INPRDR(IKEY+1,BARFRR,BARFRC)
                 IF(BARFRR.LE.0.OR.BARFRR.GT.1)THEN
                      CALL INPMSG(IKEY+1,'Not in range <0,1].')
                 ELSE
                      BARFRC=BARFRR
                 ENDIF
                 CALL INPERR
            ENDIF
*** Clear screen.
       ELSEIF(INPCMP(IKEY,'!CLE#AR-#SCREEN')+
     -      INPCMP(IKEY,'CLE#AR-#SCREEN').NE.0)THEN
                 CALL GQACWK(0,IERR,NACT,IWK)
                 IF(IERR.NE.0)THEN
                      PRINT *,' !!!!!! GRAINP WARNING : Unable to'//
     -                     ' determine number of active workstations.'
                      NACT=0
                 ENDIF
                 DO 20 I=1,NACT
                 CALL GQACWK(I,IERR,IDUM,IWK)
                 CALL GCLRWK(IWK,1)
                 IF(LDEBUG)WRITE(10,'(''  ++++++ GRAINP DEBUG   :'',
     -                '' Clear sent to WS '',I3,''.'')') IWK
20               CONTINUE
*** Close a workstation.
       ELSEIF(INPCMP(IKEY,'CLO#SE-#WORKSTATION')+
     -      INPCMP(IKEY,'!CLO#SE-#WORKSTATION').NE.0)THEN
*   Argument(s) present ?
            IF(NWORD.NE.IKEY+1)THEN
                 PRINT *,' !!!!!! GRAINP WARNING : You must specify'//
     -                ' a workstation name with this command.'
            ELSE
*   Have the workstation closed.
                 DO 80 I=IKEY+1,NWORD
                 CALL INPSTR(I,I,STRING,NC)
                 CALL GRCLWK(STRING(1:NC),'DELAY')
80               CONTINUE
            ENDIF
*** Colour definition.
       ELSEIF(INPCMP(IKEY,'!COL#OUR')+INPCMP(IKEY,'COL#OUR').NE.0)THEN
            CALL GRCOLR(IKEY,IFAIL)
            IF(IFAIL.NE.0)PRINT *,' !!!!!! GRAINP WARNING : Colour'//
     -           ' inquiry or update failed.'
*** Contour parameters.
       ELSEIF(INPCMP(IKEY,'!CONT#OUR-#PARAMETERS')+
     -      INPCMP(IKEY,'CONT#OUR-#PARAMETERS').NE.0)THEN
*   Print settings of arguments are missing.
            IF(NWORD.EQ.IKEY)THEN
                 WRITE(LUNOUT,'(''  Current contour parameters:''//
     -                ''  Bisection iterations:    '',I10/
     -                ''  Newton iterations:       '',I10/
     -                ''  Epsilon for tracing:     '',E10.3/
     -                ''  Epsilon for gradients:   '',E10.3/
     -                ''  Initial step size:       '',E10.3/
     -                ''  Relative grid tolerance: '',E10.3/
     -                ''  Maximum number of steps: '',I10)')
     -                NBITER,NNITER,EPSTRA,EPSGRA,STINIT,DNTHR,NGCMAX
*   Otherwise decode argument list.
            ELSE
                 INEXT=IKEY+1
                 DO 120 I=IKEY+1,NWORD
                 IF(I.LT.INEXT)GOTO 120
                 IF(INPCMP(I,'BIS#ECTION-#ITER#ATIONS').NE.0)THEN
                      CALL INPCHK(I+1,1,IFAIL1)
                      CALL INPRDI(I+1,NITERR,NBITER)
                      IF(NITERR.GT.0)THEN
                           NBITER=NITERR
                      ELSE
                           CALL INPMSG(I+1,'Should be > 0.')
                      ENDIF
                      INEXT=I+2
                 ELSEIF(INPCMP(I,'NEWT#ON-ITER#ATIONS').NE.0)THEN
                      CALL INPCHK(I+1,1,IFAIL1)
                      CALL INPRDI(I+1,NITERR,NNITER)
                      IF(NITERR.GT.0)THEN
                           NNITER=NITERR
                      ELSE
                           CALL INPMSG(I+1,'Should be > 0.')
                      ENDIF
                      INEXT=I+2
                 ELSEIF(INPCMP(I,'ST#EP-MAX#IMUM').NE.0)THEN
                      CALL INPCHK(I+1,1,IFAIL1)
                      CALL INPRDI(I+1,NSTEPR,NGCMAX)
                      IF(NSTEPR.GT.0)THEN
                           NGCMAX=NSTEPR
                      ELSE
                           CALL INPMSG(I+1,'Should be > 0.')
                      ENDIF
                      INEXT=I+2
                 ELSEIF(INPCMP(I,'EPS#ILON-GRA#DIENT').NE.0)THEN
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,EPSR,EPSGRA)
                      IF(EPSR.GT.0)THEN
                           EPSGRA=EPSR
                      ELSE
                           CALL INPMSG(I+1,'Should be > 0.')
                      ENDIF
                      INEXT=I+2
                 ELSEIF(INPCMP(I,'EPS#ILON-TRA#CING').NE.0)THEN
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,EPSR,EPSTRA)
                      IF(EPSR.GT.0)THEN
                           EPSTRA=EPSR
                      ELSE
                           CALL INPMSG(I+1,'Should be > 0.')
                      ENDIF
                      INEXT=I+2
                 ELSEIF(INPCMP(I,'GR#ID-TOL#ERANCE').NE.0)THEN
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,DNR,DNTHR)
                      IF(DNR.GT.0)THEN
                           DNTHR=DNR
                      ELSE
                           CALL INPMSG(I+1,'Should be > 0.')
                      ENDIF
                      INEXT=I+2
                 ELSE
                      CALL INPMSG(I,'Not a known keyword.')
                 ENDIF
120              CONTINUE
*   Show error messages.
                 CALL INPERR
            ENDIF
*** The DEACTIVATE-WORKSTATION command.
       ELSEIF(INPCMP(IKEY,'!DEACT#IVATE-#WORKSTATION')+
     -      INPCMP(IKEY,'DEACT#IVATE-#WORKSTATION').NE.0)THEN
*   Arguments present ?
            IF(NWORD.LE.IKEY)THEN
                 PRINT *,' !!!!!! GRAINP WARNING : You must specify'//
     -                ' a workstation name with this command.'
*   Have the workstation deactivated.
            ELSE
                 DO 40 I=IKEY+1,NWORD
                 CALL INPSTR(I,I,STRING,NC)
                 CALL GRDAWK(STRING(1:NC),'DELAY')
40               CONTINUE
            ENDIF
*** Delete a workstation.
       ELSEIF(INPCMP(IKEY,'DEL#ETE-#WORKSTATION')+
     -      INPCMP(IKEY,'!DEL#ETE-#WORKSTATION').NE.0)THEN
            CALL GRDLWK
*** Check for the EXIT command.
       ELSEIF(INPCMP(IKEY,'EX#IT')+INPCMP(IKEY,'!EX#IT').NE.0)THEN
            PRINT *,' '
            PRINT *,' ------------------------------------------------'
            PRINT *,' ----------  Graphics subsection end   ----------'
            PRINT *,' ------------------------------------------------'
            PRINT *,' '
            CALL INPPRM(' ','BACK-PRINT')
            RETURN
*** Representation reading from dataset.
       ELSEIF(INPCMP(IKEY,'GET-COL#OURS')+
     -      INPCMP(IKEY,'!GET-COL#OURS').NE.0)THEN
            CALL GRCOLG(IKEY,IFAIL)
            IF(IFAIL.NE.0)PRINT *,' !!!!!! GRAINP WARNING : Reading'//
     -           ' a colour table failed.'
*** Representation reading from dataset.
       ELSEIF(INPCMP(IKEY,'GET-REP#RESENTATIONS')+
     -      INPCMP(IKEY,'!GET-REP#RESENTATIONS').NE.0)THEN
            CALL GRATTG(IKEY,IFAIL)
            IF(IFAIL.NE.0)PRINT *,' !!!!!! GRAINP WARNING : Reading'//
     -           ' a graphics representation member failed.'
*** Various inquire functions.
       ELSEIF(INPCMP(IKEY,'!INQ#UIRE-DEF#ERRAL-#UPDATE-#STATE')+
     -      INPCMP(IKEY,'INQ#UIRE-DEF#ERRAL-#UPDATE-#STATE').NE.0)THEN
            CALL INPSTR(IKEY+1,IKEY+1,STRING,NC)
            IF(NWK.LE.0)WRITE(LUNOUT,'(/''  There are currently no'',
     -           '' workstations defined.''/)')
            DO 90 I=1,NWK
            IF(WKNAME(I)(1:NCWKNM(I)).EQ.STRING(1:NC).OR.
     -           STRING.EQ.'*'.OR.IKEY.EQ.NWORD)THEN
                 CALL GQWKDU(I,IERR,IDEFM,IREGM,IEMPTY,IFRAME)
                 WRITE(LUNOUT,'(''  Workstation '',A,'':'')')
     -                WKNAME(I)(1:NCWKNM(I))
                 IF(IDEFM.EQ.0)WRITE(LUNOUT,'(7X,''Deferral state:   '',
     -                '' As soon as possible;'')')
                 IF(IDEFM.EQ.1)WRITE(LUNOUT,'(7X,''Deferral state:   '',
     -                '' Before next global interaction;'')')
                 IF(IDEFM.EQ.2)WRITE(LUNOUT,'(7X,''Deferral state:   '',
     -                '' Before next local interaction;'')')
                 IF(IDEFM.EQ.3)WRITE(LUNOUT,'(7X,''Deferral state:   '',
     -                '' At some time;'')')
                 IF(IDEFM.LT.0.OR.IDEFM.GT.3)WRITE(LUNOUT,'(7X,
     -                ''Deferral state:    *** NOT KNOWN ***'')')
                 IF(IREGM.EQ.0)WRITE(LUNOUT,'(7X,''Regeneration:     '',
     -                '' Suppressed;'')')
                 IF(IREGM.EQ.1)WRITE(LUNOUT,'(7X,''Regeneration:     '',
     -                '' Allowed;'')')
                 IF(IREGM.LT.0.OR.IREGM.GT.1)WRITE(LUNOUT,'(7X,
     -                ''Regeneration:      *** NOT KNOWN ***'')')
                 IF(IEMPTY.EQ.0)WRITE(LUNOUT,'(7X,''Display surface: '',
     -                ''  Not empty anymore;'')')
                 IF(IEMPTY.EQ.1)WRITE(LUNOUT,'(7X,''Display surface: '',
     -                ''  Currently empty;'')')
                 IF(IEMPTY.LT.0.OR.IEMPTY.GT.1)WRITE(LUNOUT,'(7X,
     -                ''Display surface:   *** NOT KNOWN ***'')')
                 IF(IFRAME.EQ.0)WRITE(LUNOUT,'(7X,''For an update:   '',
     -                ''  No new frame needed;'')')
                 IF(IFRAME.EQ.1)WRITE(LUNOUT,'(7X,''For an update:   '',
     -                ''  New frame needed;'')')
                 IF(IFRAME.LT.0.OR.IFRAME.GT.1)WRITE(LUNOUT,'(7X,
     -                ''For an update:     *** NOT KNOWN ***'')')
                 IF(IERR.NE.0)WRITE(LUNOUT,'(7X,''GKS inquiry error '',
     -                I4,'' occurred.'')') IERR
                 WRITE(LUNOUT,'('' '')')
            ENDIF
90          CONTINUE
*** GKS level inquiry.
       ELSEIF(INPCMP(IKEY,'!INQ#UIRE-LEV#EL-#GKS')+
     -      INPCMP(IKEY,'INQ#UIRE-LEV#EL-#GKS').NE.0)THEN
            CALL GQLVKS(IERR,LEVEL)
            IF(IERR.NE.0)GOTO 3000
            IF(LEVEL.EQ.-3)THEN
                 WRITE(LUNOUT,'(/''  Running with a level mA GKS.''/)')
            ELSEIF(LEVEL.EQ.-2)THEN
                 WRITE(LUNOUT,'(/''  Running with a level mB GKS.''/)')
            ELSEIF(LEVEL.EQ.-1)THEN
                 WRITE(LUNOUT,'(/''  Running with a level mC GKS.''/)')
            ELSEIF(LEVEL.EQ. 0)THEN
                 WRITE(LUNOUT,'(/''  Running with a level 0A GKS.''/)')
            ELSEIF(LEVEL.EQ.+1)THEN
                 WRITE(LUNOUT,'(/''  Running with a level 0B GKS.''/)')
            ELSEIF(LEVEL.EQ.+2)THEN
                 WRITE(LUNOUT,'(/''  Running with a level 0C GKS.''/)')
            ELSEIF(LEVEL.EQ.+3)THEN
                 WRITE(LUNOUT,'(/''  Running with a level 1A GKS.''/)')
            ELSEIF(LEVEL.EQ.+4)THEN
                 WRITE(LUNOUT,'(/''  Running with a level 1B GKS.''/)')
            ELSEIF(LEVEL.EQ.+5)THEN
                 WRITE(LUNOUT,'(/''  Running with a level 1C GKS.''/)')
            ELSEIF(LEVEL.EQ.+6)THEN
                 WRITE(LUNOUT,'(/''  Running with a level 2A GKS.''/)')
            ELSEIF(LEVEL.EQ.+7)THEN
                 WRITE(LUNOUT,'(/''  Running with a level 2B GKS.''/)')
            ELSEIF(LEVEL.EQ.+8)THEN
                 WRITE(LUNOUT,'(/''  Running with a level 2C GKS.''/)')
            ELSE
                 WRITE(LUNOUT,'(/''  GKS level code is '',I2,'' which'',
     -                '' is not a standard code.'')') LEVEL
            ENDIF
*** Operating state.
       ELSEIF(INPCMP(IKEY,'!INQ#UIRE-OP#ERATING-#STATE')+
     -      INPCMP(IKEY,'INQ#UIRE-OP#ERATING-#STATE').NE.0)THEN
            CALL GQOPS(ISTA)
            IF(ISTA.EQ.0)THEN
                 WRITE(LUNOUT,'(/''  GKS is closed at the moment.''/)')
            ELSEIF(ISTA.EQ.1)THEN
                 WRITE(LUNOUT,'(/''  GKS is open at the moment.''/)')
            ELSEIF(ISTA.EQ.2)THEN
                 WRITE(LUNOUT,'(/''  A workstation is open.''/)')
            ELSEIF(ISTA.EQ.3)THEN
                 WRITE(LUNOUT,'(/''  A workstation is active.''/)')
            ELSEIF(ISTA.EQ.4)THEN
                 WRITE(LUNOUT,'(/''  A segment is open.''/)')
            ELSE
                 WRITE(LUNOUT,'(/''  GKS state code is'',I3,'', which'',
     -                '' is not standard.''/)') ISTA
            ENDIF
*** List workstations.
       ELSEIF(INPCMP(IKEY,'!INQ#UIRE-W#ORKSTATIONS')+
     -      INPCMP(IKEY,'INQ#UIRE-W#ORKSTATIONS').NE.0)THEN
            IF(NWK.EQ.0)THEN
                 WRITE(LUNOUT,'(/''  Not a single workstation'',
     -                '' known at present.'')')
            ELSE
*   Header.
                 WRITE(LUNOUT,'(/''  LIST OF CURRENTLY KNOWN'',
     -                '' WORKSTATIONS: ''/)')
                 DO 70 I=1,NWK
*   Name of the workstation.
                 OUT(1:NCWKNM(I)+2)=WKNAME(I)(1:NCWKNM(I))//': '
                 NCOUT=NCWKNM(I)+2
*   Current state.
                 IF(WKSTAT(I).LT.2)THEN
                      OUT(NCOUT+1:NCOUT+7)='defined'
                      NCOUT=NCOUT+7
                 ELSEIF(WKSTAT(I).EQ.2)THEN
                      OUT(NCOUT+1:NCOUT+4)='open'
                      NCOUT=NCOUT+4
                 ELSEIF(WKSTAT(I).EQ.3)THEN
                      OUT(NCOUT+1:NCOUT+6)='active'
                      NCOUT=NCOUT+6
                 ENDIF
*   Requested state.
                 IF(WKSREQ(I).GT.WKSTAT(I))THEN
                      IF(WKSREQ(I).LT.2)THEN
                           OUT(NCOUT+1:NCOUT+16)=' (to be defined)'
                           NCOUT=NCOUT+16
                      ELSEIF(WKSREQ(I).EQ.2)THEN
                           OUT(NCOUT+1:NCOUT+15)=' (to be opened)'
                           NCOUT=NCOUT+15
                      ELSEIF(WKSREQ(I).EQ.3)THEN
                           OUT(NCOUT+1:NCOUT+18)=' (to be activated)'
                           NCOUT=NCOUT+18
                      ENDIF
                 ENDIF
*   GKS identifier
                 CALL OUTFMT(REAL(WKID(I)),2,AUX1,NC1,'LEFT')
                 OUT(NCOUT+1:NCOUT+7+NC1)=', type '//AUX1(1:NC1)
                 NCOUT=NCOUT+7+NC1
*   File information.
                 IF(WKFREF(I).GT.0)THEN
                      CALL STRBUF('READ',WKFREF(I),STRING,NC,IFAIL1)
                      IF(WKSTAT(I).GE.2)THEN
                           CALL OUTFMT(REAL(WKLUN(I)),2,AUX2,NC2,'LEFT')
                      ELSE
                           AUX2='not yet defined'
                           NC2=15
                      ENDIF
                      CALL OUTFMT(REAL(WKCON(I)),2,AUX3,NC3,'LEFT')
                      OUT(NCOUT+1:NCOUT+8+NC+8+NC2+9+NC3)=
     -                     ', file "'//STRING(1:NC)//
     -                     '", unit '//AUX2(1:NC2)//
     -                     ', offset '//AUX3(1:NC3)
                      NCOUT=NCOUT+8+NC+8+NC2+9+NC3
                      IF(WKMULT(I))THEN
                           OUT(NCOUT+1:NCOUT+16)=', multiple frame'
                           NCOUT=NCOUT+16
                      ELSE
                           OUT(NCOUT+1:NCOUT+14)=', single frame'
                           NCOUT=NCOUT+14
                      ENDIF
*   Connection identifier.
                 ELSE
                      CALL OUTFMT(REAL(WKCON(I)),2,AUX3,NC3,'LEFT')
                      OUT(NCOUT+1:NCOUT+24+NC3)=
     -                     ', connection identifier '//AUX3(1:NC3)
                      NCOUT=NCOUT+24+NC3
                 ENDIF
*   Print the string.
                 WRITE(LUNOUT,'(2X,A)') OUT(1:NCOUT)
70               CONTINUE
                 WRITE(LUNOUT,'('' '')')
            ENDIF
*** Layout of Cartesian plots.
       ELSEIF(INPCMP(IKEY,'LAY#OUT')+INPCMP(IKEY,'!LAY#OUT').NE.0)THEN
            IF(NWORD.EQ.IKEY)THEN
                 WRITE(LUNOUT,'(''  Current Cartesian layout:''//
     -                ''  Decades to x-axis:     '',F10.3/
     -                ''  Decades to y-axis:     '',F10.3/
     -                ''  Numbers to x-axis:     '',F10.3/
     -                ''  Numbers to y-axis:     '',F10.3/
     -                ''  x-Label to border:     '',F10.3/
     -                ''  y-Label to border:     '',F10.3/
     -                ''  Title to border:       '',F10.3)')
     -                GPXN10,GPYN10,GPXN,GPYN,GPXL,GPYL,GPXT
            ELSE
                 INEXT=IKEY+1
                 DO 130 I=IKEY+1,NWORD
                 IF(I.LT.INEXT)GOTO 130
                 IF(INPCMP(I,'DEC#ADE-X-#DISTANCE')+
     -                INPCMP(I,'X-DEC#ADE-#DISTANCE').NE.0)THEN
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,AUX,GPXN10)
                      IF(IFAIL1.EQ.0.AND.AUX.GE.0.AND.AUX.LE.0.1)THEN
                           GPXN10=AUX
                      ELSEIF(IFAIL1.EQ.0)THEN
                           CALL INPMSG(I+1,'Not in range [0 , 0.1]')
                      ENDIF
                      INEXT=I+2
                 ELSEIF(INPCMP(I,'DEC#ADE-Y-#DISTANCE')+
     -                INPCMP(I,'Y-DEC#ADE-#DISTANCE').NE.0)THEN
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,AUX,GPYN10)
                      IF(IFAIL1.EQ.0.AND.AUX.GE.0.AND.AUX.LE.0.1)THEN
                           GPYN10=AUX
                      ELSEIF(IFAIL1.EQ.0)THEN
                           CALL INPMSG(I+1,'Not in range [0 , 0.1]')
                      ENDIF
                      INEXT=I+2
                 ELSEIF(INPCMP(I,'N#UMBER-X-#DISTANCE')+
     -                INPCMP(I,'X-N#UMBER-#DISTANCE').NE.0)THEN
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,AUX,GPXN)
                      IF(IFAIL1.EQ.0.AND.AUX.GE.0.AND.AUX.LE.0.1)THEN
                           GPXN=AUX
                      ELSEIF(IFAIL1.EQ.0)THEN
                           CALL INPMSG(I+1,'Not in range [0 , 0.1]')
                      ENDIF
                      INEXT=I+2
                 ELSEIF(INPCMP(I,'N#UMBER-Y-#DISTANCE')+
     -                INPCMP(I,'Y-N#UMBER-#DISTANCE').NE.0)THEN
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,AUX,GPYN)
                      IF(IFAIL1.EQ.0.AND.AUX.GE.0.AND.AUX.LE.0.1)THEN
                           GPYN=AUX
                      ELSEIF(IFAIL1.EQ.0)THEN
                           CALL INPMSG(I+1,'Not in range [0 , 0.1]')
                      ENDIF
                      INEXT=I+2
                 ELSEIF(INPCMP(I,'LAB#EL-X-#DISTANCE')+
     -                INPCMP(I,'X-LAB#EL-#DISTANCE').NE.0)THEN
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,AUX,GPXL)
                      IF(IFAIL1.EQ.0.AND.AUX.GE.0.AND.AUX.LE.0.1)THEN
                           GPXL=AUX
                      ELSEIF(IFAIL1.EQ.0)THEN
                           CALL INPMSG(I+1,'Not in range [0 , 0.1]')
                      ENDIF
                      INEXT=I+2
                 ELSEIF(INPCMP(I,'LAB#EL-Y-#DISTANCE')+
     -                INPCMP(I,'Y-LAB#EL-#DISTANCE').NE.0)THEN
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,AUX,GPYL)
                      IF(IFAIL1.EQ.0.AND.AUX.GE.0.AND.AUX.LE.0.1)THEN
                           GPYL=AUX
                      ELSEIF(IFAIL1.EQ.0)THEN
                           CALL INPMSG(I+1,'Not in range [0 , 0.1]')
                      ENDIF
                      INEXT=I+2
                 ELSEIF(INPCMP(I,'TIT#LE-#X-#DISTANCE')+
     -                INPCMP(I,'X-TIT#LE-#DISTANCE').NE.0)THEN
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,AUX,GPXT)
                      IF(IFAIL1.EQ.0.AND.AUX.GE.0.AND.AUX.LE.0.1)THEN
                           GPXT=AUX
                      ELSEIF(IFAIL1.EQ.0)THEN
                           CALL INPMSG(I+1,'Not in range [0 , 0.1]')
                      ENDIF
                      INEXT=I+2
                 ELSE
                      CALL INPMSG(I,'Not a known keyword.')
                 ENDIF
130              CONTINUE
                 CALL INPERR
            ENDIF
*** Produce a colour map.
       ELSEIF(INPCMP(IKEY,'MAP-#COLOURS')+
     -      INPCMP(IKEY,'!MAP-#COLOURS').NE.0)THEN
            CALL GRCOLM
*** Open a workstation.
       ELSEIF(INPCMP(IKEY,'OPEN-#WORKSTATION')+
     -      INPCMP(IKEY,'!OPEN-#WORKSTATION').NE.0)THEN
*   Argument(s) present ?
            IF(NWORD.NE.IKEY+1)THEN
                 PRINT *,' !!!!!! GRAINP WARNING : You must specify'//
     -                ' a workstation name with this command.'
            ELSE
*   Have the workstation opened.
                 DO 50 I=IKEY+1,NWORD
                 CALL INPSTR(I,I,STRING,NC)
                 CALL GROPWK(STRING(1:NC),'DELAY')
50               CONTINUE
            ENDIF
*** Graphics options.
       ELSEIF(INPCMP(IKEY,'OPT#IONS')+
     -      INPCMP(IKEY,'!OPT#IONS').NE.0)THEN
            IF(NWORD.GT.IKEY)THEN
                 DO 60 I=IKEY+1,NWORD
                 IF(INPCMP(I,'LIN#EAR-X').NE.0)THEN
                      LOGX=.FALSE.
                 ELSEIF(INPCMP(I,'LOG#ARITHMIC-X').NE.0)THEN
                      LOGX=.TRUE.
                 ELSEIF(INPCMP(I,'LIN#EAR-Y').NE.0)THEN
                      LOGY=.FALSE.
                 ELSEIF(INPCMP(I,'LOG#ARITHMIC-Y').NE.0)THEN
                      LOGY=.TRUE.
                 ELSEIF(INPCMP(I,'LOG#ARITHMIC-Y').NE.0)THEN
                      LOGY=.FALSE.
                 ELSEIF(INPCMP(I,'GR#ID-DEC#ADES-#ONLY')+
     -                INPCMP(I,'DEC#ADES-#ONLY-#GRID').NE.0)THEN
                      LGRALL=.FALSE.
                 ELSEIF(INPCMP(I,'COMP#LETE-GR#ID')+
     -                INPCMP(I,'GR#ID-COMP#LETE').NE.0)THEN
                      LGRALL=.TRUE.
                 ELSEIF(INPCMP(I,'GR#ID-#PLOT').NE.0)THEN
                      LGRID=.TRUE.
                 ELSEIF(INPCMP(I,'NOGR#ID-#PLOT').NE.0)THEN
                      LGRID=.FALSE.
                 ELSEIF(INPCMP(I,'T#IME-S#TAMP').NE.0)THEN
                      LSTAMP=.TRUE.
                 ELSEIF(INPCMP(I,'NOT#IME-S#TAMP').NE.0)THEN
                      LSTAMP=.FALSE.
                 ELSEIF(INPCMP(I,'CL#EAR-BEF#ORE-#PLOT').NE.0)THEN
                      LGCLRB=.TRUE.
                 ELSEIF(INPCMP(I,'NOCL#EAR-BEF#ORE-#PLOT').NE.0)THEN
                      LGCLRB=.FALSE.
                 ELSEIF(INPCMP(I,'CL#EAR-AFT#ER-#PLOT').NE.0)THEN
                      LGCLRA=.TRUE.
                 ELSEIF(INPCMP(I,'NOCL#EAR-AFT#ER-#PLOT').NE.0)THEN
                      LGCLRA=.FALSE.
                 ELSEIF(INPCMP(I,'WAIT-AFT#ER-#PLOT').NE.0)THEN
                      LWAITA=.TRUE.
                 ELSEIF(INPCMP(I,'NOWAIT-AFT#ER-#PLOT').NE.0)THEN
                      LWAITA=.FALSE.
                 ELSEIF(INPCMP(I,'WAIT-BEF#ORE-#PLOT').NE.0)THEN
                      LWAITB=.TRUE.
                 ELSEIF(INPCMP(I,'NOWAIT-BEF#ORE-#PLOT').NE.0)THEN
                      LWAITB=.FALSE.
                 ELSEIF(INPCMP(I,'CLIP-L#INES').NE.0)THEN
                      LGLCLP=.TRUE.
                 ELSEIF(INPCMP(I,'NOCLIP-L#INES').NE.0)THEN
                      LGLCLP=.FALSE.
                 ELSEIF(INPCMP(I,'CLIP-M#ARKERS').NE.0)THEN
                      LGMCLP=.TRUE.
                 ELSEIF(INPCMP(I,'NOCLIP-M#ARKERS').NE.0)THEN
                      LGMCLP=.FALSE.
                 ELSEIF(INPCMP(I,'CLIP-A#REAS').NE.0)THEN
                      LGACLP=.TRUE.
                 ELSEIF(INPCMP(I,'NOCLIP-A#REAS').NE.0)THEN
                      LGACLP=.FALSE.
                 ELSEIF(INPCMP(I,'CLIP-T#EXT').NE.0)THEN
                      LGTCLP=.TRUE.
                 ELSEIF(INPCMP(I,'NOCLIP-T#EXT').NE.0)THEN
                      LGTCLP=.FALSE.
                 ELSEIF(INPCMP(I,'EX#ECUTE-CONTR#OL-#CHARACTERS').NE.
     -                0)THEN
                      LXCCH=.TRUE.
                 ELSEIF(INPCMP(I,'DISP#LAY-CONTR#OL-#CHARACTERS').NE.
     -                0)THEN
                      LXCCH=.FALSE.
                 ELSE
                      CALL INPMSG(I,'Not a valid option.')
                 ENDIF
60               CONTINUE
                 CALL INPERR
            ELSE
                 WRITE(LUNOUT,
     -                '(/''  CURRENT GRAPHICS OPTION SETTINGS:''//
     -                   ''  Plot a coordinate grid:     '',L1/
     -                   ''  Complete or decades-only:   '',L1/
     -                   ''  Time stamp on metafile:     '',L1/
     -                   ''  Logarithmic scale x-axis:   '',L1/
     -                   ''  Logarithmic scale y-axis:   '',L1/
     -                   ''  Clear screen before plot:   '',L1/
     -                   ''  Clear screen after plot:    '',L1/
     -                   ''  Wait before plot:           '',L1/
     -                   ''  Wait after plot:            '',L1/
     -                   ''  Execute control characters: '',L1/
     -                   ''  Clip lines outside plot:    '',L1/
     -                   ''  Clip markers outside plot:  '',L1/
     -                   ''  Clip areas outside plot:    '',L1/
     -                   ''  Clip text outside plot:     '',L1/)')
     -                LGRID,LGRALL,LSTAMP,LOGX,LOGY,LGCLRB,LGCLRA,
     -                LWAITB,LWAITA,LXCCH,LGLCLP,LGMCLP,LGACLP,LGTCLP
            ENDIF
*** Set deferral state.
       ELSEIF(INPCMP(IKEY,'SET-DEF#ERRAL-#STATE')+
     -      INPCMP(IKEY,'!SET-DEF#ERRAL-#STATE').NE.0)THEN
            IF(NWORD.NE.IKEY+3)THEN
                 PRINT *,' !!!!!! GRAINP WARNING : Incorrect number'//
     -                ' arguments; ignored.'
            ELSE
*   Locate the workstation.
                 CALL INPSTR(IKEY+1,IKEY+1,STRING,NC)
                 DO 100 I=1,NWK
                 IF(WKNAME(I)(1:NCWKNM(I)).EQ.STRING(1:NC))THEN
                      IWK=I
                      GOTO 110
                 ENDIF
100              CONTINUE
                 CALL INPMSG(IKEY+1,'Not a known workstation.')
                 IWK=-1
110              CONTINUE
*   Find old values.
                 IF(IWK.GE.1)THEN
                      CALL GQWKDU(IWK,IERR,IDEFD,IUPDD,IDUM1,IDUM2)
                      IF(IERR.NE.0)IDEFD=-1
                      IF(IERR.NE.0)IUPDD=-1
                 ELSE
                      IDEFD=-1
                      IUPDD=-1
                 ENDIF
*   Find the deferral and update states.
                 IDEF=-1
                 IUPD=-1
                 IF(INPCMP(IKEY+2,'AS-#SOON-#AS-#POSSIBLE')+
     -                INPCMP(IKEY+2,'ASAP').NE.0)THEN
                      IDEF=0
                 ELSEIF(INPCMP(IKEY+2,'BEF#ORE-N#EXT-I#NTERACTION-'//
     -                'GL#OBALLY')+INPCMP(IKEY+2,'BNIG').NE.0)THEN
                      IDEF=1
                 ELSEIF(INPCMP(IKEY+2,'BEF#ORE-N#EXT-I#NTERACTION-'//
     -                'LOC#ALLY')+INPCMP(IKEY+2,'BNIL').NE.0)THEN
                      IDEF=2
                 ELSEIF(INPCMP(IKEY+2,'AT-#SOME-#TIME')+
     -                INPCMP(IKEY+2,'AST').NE.0)THEN
                      IDEF=3
                 ELSEIF(INPCMP(IKEY+2,'*').NE.0.AND.IDEFD.GE.0)THEN
                      IDEF=IDEFD
                 ELSE
                      CALL INPMSG(IKEY+2,'Not a valid deferral mode.')
                 ENDIF
                 IF(INPCMP(IKEY+3,'SUP#PRESSED').NE.0)THEN
                      IUPD=0
                 ELSEIF(INPCMP(IKEY+3,'ALL#OWED').NE.0)THEN
                      IUPD=1
                 ELSEIF(INPCMP(IKEY+3,'*').NE.0.AND.IUPDD.GE.0)THEN
                      IUPD=IUPDD
                 ELSE
                      CALL INPMSG(IKEY+3,'Not a valid update mode.')
                 ENDIF
*   Set the new state.
                 IF(IDEF.GE.0.AND.IUPD.GE.0.AND.IWK.GE.0)
     -                CALL GSDS(IWK,IDEF,IUPD)
*   Show error messages.
                 CALL INPERR
            ENDIF
*** Show a shading map.
       ELSEIF(INPCMP(IKEY,'SH#ADING-#MAP')+
     -      INPCMP(IKEY,'SH#ADES-#MAP')+
     -      INPCMP(IKEY,'!SH#ADING-#MAP')+
     -      INPCMP(IKEY,'!SH#ADES-#MAP').NE.0)THEN
            CALL COLSHM
*** Stamp string.
       ELSEIF(INPCMP(IKEY,'STAMP')+
     -      INPCMP(IKEY,'!STAMP').NE.0)THEN
            IF(NWORD.EQ.IKEY)THEN
                 WRITE(LUNOUT,'(''  Current stamp string: "'',A,
     -                ''".'')') STAMP(1:NCSTMP)
            ELSE
                 CALL INPSTR(IKEY+1,IKEY+1,STAMP,NCSTMP)
            ENDIF
*** Representation setting and inquiry.
       ELSEIF(INPCMP(IKEY,'REP#RESENTATION')+
     -      INPCMP(IKEY,'!REP#RESENTATION').NE.0)THEN
            CALL GRATTR(IKEY,IFAIL)
            IF(IFAIL.NE.0)PRINT *,' !!!!!! GRAINP WARNING : Change or'//
     -           ' inquiry of the representation failed.'
*** Reset the colour table.
       ELSEIF(INPCMP(IKEY,'RESET-#COLOURS')+
     -      INPCMP(IKEY,'!RESET-#COLOURS').NE.0)THEN
            CALL GRCOLS
*** Set the viewport.
       ELSEIF(INPCMP(IKEY,'VIEW#PORT')+
     -      INPCMP(IKEY,'!VIEW#PORT').NE.0)THEN
*   Display if there are no arguments.
            IF(IKEY.EQ.NWORD)THEN
                 CALL OUTFMT(DISPX0,2,AUX1,NC1,'LEFT')
                 CALL OUTFMT(DISPX1,2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(DISPY0,2,AUX3,NC3,'LEFT')
                 CALL OUTFMT(DISPY1,2,AUX4,NC4,'LEFT')
                 WRITE(LUNOUT,'(''  Current viewport:''/
     -                5X,A,'' < x < '',A/5X,A,'' < y < '',A)')
     -                AUX1(1:NC1),AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
*   Otherwise check there are 4 numbers as arguments.
            ELSEIF((INPTYP(IKEY+1).NE.1.AND.INPTYP(IKEY+1).NE.2).OR.
     -           (INPTYP(IKEY+2).NE.1.AND.INPTYP(IKEY+2).NE.2).OR.
     -           (INPTYP(IKEY+3).NE.1.AND.INPTYP(IKEY+3).NE.2).OR.
     -           (INPTYP(IKEY+4).NE.1.AND.INPTYP(IKEY+4).NE.2).OR.
     -           NWORD.NE.IKEY+4)THEN
                 PRINT *,' !!!!!! GRAINP WARNING : VIEWPORT has 4'//
     -                ' numeric arguments; viewport not changed.'
*   Try to interpret the viewport.
            ELSE
                 CALL INPCHK(IKEY+1,2,IFAIL1)
                 CALL INPCHK(IKEY+2,2,IFAIL2)
                 CALL INPCHK(IKEY+3,2,IFAIL3)
                 CALL INPCHK(IKEY+4,2,IFAIL4)
                 IF(IFAIL1+IFAIL2+IFAIL3+IFAIL4.EQ.0)THEN
                      CALL INPRDR(IKEY+1,DISX0R,DISPX0)
                      CALL INPRDR(IKEY+2,DISY0R,DISPY0)
                      CALL INPRDR(IKEY+3,DISX1R,DISPX1)
                      CALL INPRDR(IKEY+4,DISY1R,DISPY1)
                      IF(DISX0R.GE.0.AND.DISX0R.LE.1.AND.
     -                     DISX1R.GE.0.AND.DISX1R.LE.1.AND.
     -                     DISY0R.GE.0.AND.DISY0R.LE.1.AND.
     -                     DISY1R.GE.0.AND.DISY1R.LE.1.AND.
     -                     ABS(DISX0R-DISX1R).GT.0.2.AND.
     -                     ABS(DISY0R-DISY1R).GT.0.2)THEN
                           DISPX0=MIN(DISX0R,DISX1R)
                           DISPX1=MAX(DISX0R,DISX1R)
                           DISPY0=MIN(DISY0R,DISY1R)
                           DISPY1=MAX(DISY0R,DISY1R)
                      ELSE
                           PRINT *,' !!!!!! GRAINP WARNING : Viewport'//
     -                          ' not entirely within (0,0) - (1,1)'//
     -                          ' or too small; viewport not changed.'
                      ENDIF
                 ELSE
                      CALL INPERR
                      PRINT *,' !!!!!! GRAINP WARNING : Viewport not'//
     -                     ' changed because of the above errors.'
                 ENDIF
            ENDIF
*** Colour writing to dataset.
       ELSEIF(INPCMP(IKEY,'WR#ITE-COL#OURS')+
     -      INPCMP(IKEY,'!WR#ITE-COL#OURS').NE.0)THEN
            CALL GRCOLW(IKEY,IFAIL)
            IF(IFAIL.NE.0)PRINT *,' !!!!!! GRAINP WARNING : Writing'//
     -           ' a list of colours to a dataset failed.'
*** Representation writing to dataset.
       ELSEIF(INPCMP(IKEY,'WR#ITE-REP#RESENTATIONS')+
     -      INPCMP(IKEY,'!WR#ITE-REP#RESENTATIONS').NE.0)THEN
            CALL GRATTW(IKEY,IFAIL)
            IF(IFAIL.NE.0)PRINT *,' !!!!!! GRAINP WARNING : Writing'//
     -           ' a graphics representation member failed.'
*** Invalid option.
       ELSE
            CALL INPSTR(IKEY,IKEY,STRING,NC)
            PRINT *,' !!!!!! GRAINP WARNING : '//STRING(1:NC)//' is'//
     -           ' not a valid command; it is ignored.'
       ENDIF
*** Either read a new input line or return to the calling section.
1010   CONTINUE
*** Next command, if in a sub-section.
       IF(LOOP)GOTO 10
       RETURN
*** Inquiry failed.
3000   CONTINUE
       PRINT *,' !!!!!! GRAINP WARNING : GKS inquiry function failed;'//
     -      ' no output returned.'
       END
