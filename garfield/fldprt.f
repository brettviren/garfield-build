CDECK  ID>, FLDPRT.
       SUBROUTINE FLDPRT
*-----------------------------------------------------------------------
*   FLDPRT - Subroutine printing any function of the electric field, the
*            the potential and the magnetic field on a grid of GRID **2
*            points in the area (PXMIN,PYMIN) (PXMAX,PYMAX). This
*            routine will not work for mappings other than polar.
*   VARIABLES : VECTOR       : vector to be printed.
*               FUNCT, NF    : string (and length) of the function.
*               USE          : .TRUE. if the corresponding var is used.
*               EVALE, EVALB : .TRUE. if E resp B is used.
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       CHARACTER*10 VARLIS(MXVAR)
       CHARACTER*(MXCHAR) FUNCT,STRING
       REAL VECTOR(4,10),VAR(MXVAR)
       INTEGER MODVAR(MXVAR),MODRES(4)
       DOUBLE PRECISION SUMFLD(4)
       LOGICAL USE(MXVAR),EVALE,EVALB
       SAVE VARLIS
       DATA (VARLIS(I),I=5,10)/'E         ','V         ','BX        ',
     -            'BY        ','BZ        ','B         '/
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE FLDPRT ///'
*** Make sure the variables have appropriate names.
       IF(POLAR)THEN
            VARLIS(1)='R         '
            VARLIS(2)='PHI       '
            VARLIS(3)='ER        '
            VARLIS(4)='EPHI      '
       ELSE
            VARLIS(1)='X         '
            VARLIS(2)='Y         '
            VARLIS(3)='EX        '
            VARLIS(4)='EY        '
       ENDIF
*** Get the number of words in the input string
       CALL INPNUM(NWORD)
*   Warn if the function is absent.
       IF(NWORD.EQ.1)THEN
            PRINT *,' !!!!!! FLDPRT WARNING : To obtain a table, a'//
     -           ' (list of) function(s) should be given as argument.'
            RETURN
       ENDIF
*   Loop over the input string
       DO 70 IW=1,NWORD-1,4
*   Extract the function.
       NF=1
       FUNCT=' '
       DO 10 JW=1,4
       IF(IW+JW.GT.NWORD)GOTO 10
       CALL INPSTR(IW+JW,IW+JW,STRING,NC)
       FUNCT(NF:NF+NC)=STRING(1:NC)//','
       NF=NF+NC+1
10     CONTINUE
       FUNCT(NF-1:NF-1)=' '
       NF=NF-2
       IF(NF.EQ.0)GOTO 70
*   Convert into an instruction list.
       IF(INDEX(FUNCT,'@').NE.0)THEN
            NRES=0
            CALL ALGEDT(VARLIS,10,IENTRY,USE,NRES)
            FUNCT='Edited function'
            NF=15
            IF(NRES.LE.0)THEN
                 PRINT *,' !!!!!! FLDPRT WARNING : The edited'//
     -                ' instruction list does not return results;'//
     -                ' no printout.'
                 GOTO 70
            ENDIF
       ELSE
            CALL ALGPRE(FUNCT,NF,VARLIS,10,NRES,USE,IENTRY,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! FLDPRT WARNING : Table of '//
     -                FUNCT(1:NF)//' not printed because of'//
     -                ' syntax error(s) in one of the functions.'
                 GOTO 70
            ENDIF
       ENDIF
*    Determine which quantities are going to be used.
       EVALE=.FALSE.
       EVALB=.FALSE.
       IOPT=0
       IF(USE(3).OR.USE(4).OR.USE(5).OR.USE(6))EVALE=.TRUE.
       IF(USE(7).OR.USE(8).OR.USE(9).OR.USE(10))EVALB=.TRUE.
       IF(USE(6))IOPT=1
*   Make sure the function does not use B if MAGOK is .FALSE..
       IF(EVALB.AND..NOT.MAGOK)THEN
            PRINT *,' !!!!!! FLDPRT WARNING : A magnetic field'//
     -           ' quantity is used in '//FUNCT(1:NF)
            PRINT *,'                         no such field has'//
     -           ' been defined however ; table not printed.'
            GOTO 70
       ENDIF
*** Print a header for the table.
       WRITE(LUNOUT,'(''1 Printed table of the field''/
     -      ''  ==========================''//)',IOSTAT=IOS,ERR=2010)
       WRITE(LUNOUT,'(A)',IOSTAT=IOS,ERR=2010)
     -      '  Function being printed: '//FUNCT(1:NF)
       WRITE(LUNOUT,'(A55)',IOSTAT=IOS,ERR=2010)
     -      '  where the symbolic variables stand for the following:'
       IF(USE(1).AND..NOT.POLAR)
     -      WRITE(LUNOUT,'(1X,A56)',IOSTAT=IOS,ERR=2010)
     -      ' X    = x-coordinate                                [cm]'
       IF(USE(1).AND.POLAR)
     -      WRITE(LUNOUT,'(1X,A56)',IOSTAT=IOS,ERR=2010)
     -      ' R    = radial coordinate                           [cm]'
       IF(USE(2).AND..NOT.POLAR)
     -      WRITE(LUNOUT,'(1X,A56)',IOSTAT=IOS,ERR=2010)
     -      ' Y    = y-coordinate                                [cm]'
       IF(USE(2).AND.POLAR)
     -      WRITE(LUNOUT,'(1X,A56)',IOSTAT=IOS,ERR=2010)
     -      ' PHI  = angular coordinate                      [degree]'
       IF(USE(3).AND..NOT.POLAR)
     -      WRITE(LUNOUT,'(1X,A56)',IOSTAT=IOS,ERR=2010)
     -      ' EX   = x-component of the electric field         [V/cm]'
       IF(USE(3).AND.POLAR)
     -      WRITE(LUNOUT,'(1X,A56)',IOSTAT=IOS,ERR=2010)
     -      ' ER   = radial component of the electric field    [V/cm]'
       IF(USE(4).AND..NOT.POLAR)
     -      WRITE(LUNOUT,'(1X,A56)',IOSTAT=IOS,ERR=2010)
     -      ' EY   = y-component of the electric field         [V/cm]'
       IF(USE(4).AND.POLAR)WRITE(LUNOUT,'(1X,A56)',IOSTAT=IOS,ERR=2010)
     -      ' EPHI = angular component of the electric field   [V/cm]'
       IF(USE(5))WRITE(LUNOUT,'(1X,A56)',IOSTAT=IOS,ERR=2010)
     -      ' E    = magnitude of the electric field           [V/cm]'
       IF(USE(6))WRITE(LUNOUT,'(1X,A56)',IOSTAT=IOS,ERR=2010)
     -      ' V    = electrostatic potential                      [V]'
       IF(USE(7))WRITE(LUNOUT,'(1X,A56)',IOSTAT=IOS,ERR=2010)
     -      ' BX   = x-component of the magnetic field  [V sec/cm2]'
       IF(USE(8))WRITE(LUNOUT,'(1X,A56)',IOSTAT=IOS,ERR=2010)
     -      ' BY   = y-component of the magnetic field  [V sec/cm2]'
       IF(USE(9))WRITE(LUNOUT,'(1X,A56)',IOSTAT=IOS,ERR=2010)
     -      ' BZ   = z-component of the magnetic field  [V sec/cm2]'
       IF(USE(10))WRITE(LUNOUT,'(1X,A56)',IOSTAT=IOS,ERR=2010)
     -      ' B    = magnitude of the magnetic field    [V sec/cm2]'
       WRITE(LUNOUT,'(/''  The data apply to a rectangular grid of '',
     -      I2,'' points in the area delimited by:'')')
       PXMIND=PXMIN
       PXMAXD=PXMAX
       PYMIND=PYMIN
       PYMAXD=PYMAX
       IF(POLAR)CALL CFMRTP(PXMIND,PYMIND,PXMIND,PYMIND,1)
       IF(POLAR)CALL CFMRTP(PXMAXD,PYMAXD,PXMAXD,PYMAXD,1)
       IF(POLAR)THEN
            WRITE(LUNOUT,'(''     '',F10.3,'' < r   < '',F10.3/
     -                     ''     '',F10.3,'' < phi < '',F10.3)',
     -      ERR=2010,IOSTAT=IOS) PXMIND,PXMAXD,PYMIND,PYMAXD
       ELSE
            WRITE(LUNOUT,'(''     '',F10.3,'' < x < '',F10.3/
     -                     ''     '',F10.3,'' < y < '',F10.3)',
     -      ERR=2010,IOSTAT=IOS) PXMIND,PXMAXD,PYMIND,PYMAXD
       ENDIF
*** Set the averaging variables to 0.
       DO 15 ISUM=1,NRES
       SUMFLD(ISUM)=0
15     CONTINUE
       NSUM=0
*** Loop over the area, printing the field at the same time,
       DO 60 JJ=0,10*INT((NGRIDY-1)/10.0),10
       JMAX=MIN(NGRIDY-JJ,10)
       DO 50 II=0,10*INT((NGRIDX-1)/10.0),10
       IMAX=MIN(NGRIDX-II,10)
       WRITE(LUNOUT,'(''1 FIELD-PRINT'',109X,''PART '',I1,''.'',I1)',
     -      ERR=2010,IOSTAT=IOS) 1+II/10,1+JJ/10
       WRITE(LUNOUT,'(''  ==========='',109X,''========''/)',
     -      IOSTAT=IOS,ERR=2010)
       IF(.NOT.POLAR)THEN
            WRITE(LUNOUT,'('' y        x:'',10(E11.4,1X:)/)',
     -           IOSTAT=IOS,ERR=2010)
     -           (PXMIN+(PXMAX-PXMIN)*(II+I-1)/REAL(NGRIDX-1),I=1,IMAX)
       ELSE
            WRITE(LUNOUT,'('' phi      r:'',10(E11.4,1X:)/)',
     -           IOSTAT=IOS,ERR=2010)
     -           (EXP(PXMIN)+(EXP(PXMAX)-EXP(PXMIN))*REAL(II+I-1)/
     -           REAL(NGRIDX-1),I=1,IMAX)
       ENDIF
       DO 40 J=1,JMAX
       YPOS=PYMIN+(PYMAX-PYMIN)*REAL(JJ+J-1)/REAL(NGRIDY-1)
       DO 20 I=1,IMAX
       IF(POLAR)THEN
            XPOS=LOG(EXP(PXMIN)+(EXP(PXMAX)-EXP(PXMIN))*
     -           REAL(II+I-1)/REAL(NGRIDX-1))
       ELSE
            XPOS=PXMIN+(PXMAX-PXMIN)*REAL(II+I-1)/REAL(NGRIDX-1)
       ENDIF
*   evaluate the field,
       IF(EVALE)CALL EFIELD(XPOS,YPOS,0.0,
     -      VAR(3),VAR(4),EZ,VAR(5),VAR(6),
     -      IOPT,ILOC)
       IF(EVALB)CALL BFIELD(XPOS,YPOS,0.0,
     -      VAR(7),VAR(8),VAR(9),VAR(10))
*   convert to polar coordinates if the cell is polar,
       IF(EVALE.AND.POLAR)THEN
            VAR(3)=VAR(3)/EXP(XPOS)
            VAR(4)=VAR(4)/EXP(XPOS)
            VAR(5)=VAR(5)/EXP(XPOS)
       ENDIF
       IF(POLAR)THEN
            VAR(1)=EXP(XPOS)
            VAR(2)=180.0*YPOS/PI
       ELSE
            VAR(1)=XPOS
            VAR(2)=YPOS
       ENDIF
*   Assign modes.
       DO 80 K=1,10
       MODVAR(K)=2
80     CONTINUE
*   Evaluate the field functions and store the results in VECTOR,
       CALL ALGEXE(IENTRY,VAR,MODVAR,10,VECTOR(1,I),MODRES,4,IFAIL)
*   And add the new values to the sum.
       DO 16 ISUM=1,NRES
       SUMFLD(ISUM)=SUMFLD(ISUM)+VECTOR(ISUM,I)
16     CONTINUE
       NSUM=NSUM+1
20     CONTINUE
*   Print VECTOR,
       WRITE(LUNOUT,'(1X,E10.3)',IOSTAT=IOS,ERR=2010) VAR(2)
       DO 30 K=1,NRES
       WRITE(LUNOUT,'(12X,10(E11.4,1X:))',IOSTAT=IOS,ERR=2010)
     -      (VECTOR(K,I),I=1,IMAX)
30     CONTINUE
40     CONTINUE
50     CONTINUE
60     CONTINUE
*   Finally print the averages as well.
       WRITE(LUNOUT,'(''1 Number of sampling points on the grid: '',I5
     -      /''  Averaging over this grid yields:'')',ERR=2010,
     -      IOSTAT=IOS) NSUM
       DO 65 ISUM=1,NRES
       WRITE(LUNOUT,'(/''  Function '',I1,'': '',E15.8)',ERR=2010,
     -      IOSTAT=IOS) ISUM,SUMFLD(ISUM)/NSUM
65     CONTINUE
*** Proceed with the next group of functions.
70     CONTINUE
*   Release the algebra entry point.
       CALL ALGCLR(IENTRY)
*** Register the amount of CPU time used in the step.
       CALL TIMLOG('Printing a table of the field:          ')
       RETURN
*** Handle I/O errors.
2010   CONTINUE
       PRINT *,' ###### FLDPRT ERROR   : Error writing the field'//
     -      ' table on unit ',LUNOUT,' ; output terminated.'
       CALL INPIOS(IOS)
       END
