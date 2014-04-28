CDECK  ID>, BEMVIE.
       SUBROUTINE BEMVIE(IFAIL)
*-----------------------------------------------------------------------
*   BEMVIE - 3D viewing using neBEM generated panels.
*   (Last changed on  5/ 4/10.)
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
       INTEGER NBEM,IREFB1(MXPLAN),NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,
     -      BEMNEW,BEMINV,BEMSLV
       DOUBLE PRECISION BEMQTH,BEMSTH,BEMSSC,BEMTGT,BEMEPA,BEMEPD
       LOGICAL LBDUMP
       COMMON /BEMDAT/ BEMQTH,BEMSSC,BEMSTH,BEMTGT,BEMEPA,BEMEPD,
     -      IREFB1,NBEM,NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,BEMNEW,
     -      BEMINV,BEMSLV,LBDUMP
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
       INTEGER IFAIL,IFAIL1,IVOL1,IVOL2,ICOL,IREF,NPL,IOFCOL,
     -      NCOUNT,IBEM
       DOUBLE PRECISION XPL(MXEDGE),YPL(MXEDGE),ZPL(MXEDGE),A,B,C,WW
*** Identification.
       IF(LIDENT)PRINT *,' /// ROUTINE BEMVIE ///'
*** Assume this will fail.
       IFAIL=1
*** No point if BEMSET is not true.
       IF(.NOT.BEMSET)THEN
            PRINT *,' !!!!!! BEMVIE WARNING : BEMSET is not set;'//
     -           ' not preparing for viewing.'
            RETURN
       ENDIF
*** Reset the plot buffer.
       CALL PLABU1('RESET',IREF,0,XPL,YPL,ZPL,
     -      0.0D0,0.0D0,0.0D0,0,0,IFAIL1)
*** Reinitialise colour tables.
       ICOL0=30
       ICOLBX=0
       ICOLPL=0
       ICOLST=0
       ICOLW1=0
       ICOLW2=0
       ICOLW3=0
       ICOLD1=0
       ICOLD2=0
       ICOLD3=0
       ICOLRB=0
*** Loop over the panels.
       CALL PROFLD(1,'Counting surfaces',-1.0)
       CALL PROSTA(1,0.0)
       NCOUNT=0
       DO 10 IBEM=1,NBEM
*   Retrieve a panel.
       CALL BEMBU1('READ',IREFB1(IBEM),NPL,XPL,YPL,ZPL,
     -      A,B,C,IVOL2,IVOL1,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            GOTO 10
       ELSE IF(IVOL1.LT.1.OR.IVOL1.GT.NSOLID)THEN
            PRINT *,' !!!!!! BEMVIE WARNING : Volume1 number ',IVOL1,
     -           ' of panel ',IREF,' out of range; panel skipped.'
            GOTO 10
       ENDIF
*   Count
       NCOUNT=NCOUNT+1
*   Create a colour table according to the volume type.
       IF(ISOLMT(IVOL1).EQ.1)THEN
            IF(ICOLW1.EQ.0)THEN
                 CALL GRATTS('CONDUCTORS-1','AREA')
                 ICOLW1=ICOL0
                 CALL COLSHD(ICOLW1)
                 ICOL0=ICOL0+NPRCOL
            ENDIF
            IOFCOL=ICOLW1
       ELSEIF(ISOLMT(IVOL1).EQ.2)THEN
            IF(ICOLW2.EQ.0)THEN
                 CALL GRATTS('CONDUCTORS-2','AREA')
                 ICOLW2=ICOL0
                 CALL COLSHD(ICOLW2)
                 ICOL0=ICOL0+NPRCOL
            ENDIF
            IOFCOL=ICOLW2
       ELSEIF(ISOLMT(IVOL1).EQ.3)THEN
            IF(ICOLW3.EQ.0)THEN
                 CALL GRATTS('CONDUCTORS-3','AREA')
                 ICOLW3=ICOL0
                 CALL COLSHD(ICOLW3)
                 ICOL0=ICOL0+NPRCOL
            ENDIF
            IOFCOL=ICOLW3
       ELSEIF(ISOLMT(IVOL1).EQ.11)THEN
            IF(ICOLD1.EQ.0)THEN
                 CALL GRATTS('DIELECTRIC-1','AREA')
                 ICOLD1=ICOL0
                 CALL COLSHD(ICOLD1)
                 ICOL0=ICOL0+NPRCOL
            ENDIF
            IOFCOL=ICOLD1
       ELSEIF(ISOLMT(IVOL1).EQ.12)THEN
            IF(ICOLD2.EQ.0)THEN
                 CALL GRATTS('DIELECTRIC-2','AREA')
                 ICOLD2=ICOL0
                 CALL COLSHD(ICOLD2)
                 ICOL0=ICOL0+NPRCOL
            ENDIF
            IOFCOL=ICOLD2
       ELSEIF(ISOLMT(IVOL1).EQ.13)THEN
            IF(ICOLD3.EQ.0)THEN
                 CALL GRATTS('DIELECTRIC-3','AREA')
                 ICOLD3=ICOL0
                 CALL COLSHD(ICOLD3)
                 ICOL0=ICOL0+NPRCOL
            ENDIF
            IOFCOL=ICOLD3
       ELSE
            IOFCOL=0
       ENDIF
*   Assign the appropriate weight.
       CALL COLWGT(A,B,C,WW)
*   And find out which colour we should use.
       IF(WW.GT.0)THEN
            ICOL=IOFCOL+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
       ELSE
            ICOL=IOFCOL
       ENDIF
*   Store with the appropriate colour.
       CALL PLABU1('STORE',IREF,NPL,XPL,YPL,ZPL,
     -      A,B,C,ICOL,IVOL1,IFAIL1)
       IF(IFAIL1.NE.0)PRINT *,' !!!!!! BEMVIE WARNING : Panel ',IREF,
     -      ' volume 1 storage failed; panel skipped.'
**  Check for volume 2.
       IF(IVOL2.EQ.0)THEN
            IVOL2=IVOL1
            ICOL=8
            GOTO 20
       ELSE IF(IVOL2.LT.1.OR.IVOL2.GT.NSOLID)THEN
            PRINT *,' !!!!!! BEMVIE WARNING : Volume2 number ',IVOL2,
     -           ' of panel ',IREF,' out of range; panel skipped.'
            GOTO 10
       ENDIF
*   Count
       NCOUNT=NCOUNT+1
*   Create a colour table according to the volume type.
       IF(ISOLMT(IVOL2).EQ.1)THEN
            IF(ICOLW1.EQ.0)THEN
                 CALL GRATTS('CONDUCTORS-1','AREA')
                 ICOLW1=ICOL0
                 CALL COLSHD(ICOLW1)
                 ICOL0=ICOL0+NPRCOL
            ENDIF
            IOFCOL=ICOLW1
       ELSEIF(ISOLMT(IVOL2).EQ.2)THEN
            IF(ICOLW2.EQ.0)THEN
                 CALL GRATTS('CONDUCTORS-2','AREA')
                 ICOLW2=ICOL0
                 CALL COLSHD(ICOLW2)
                 ICOL0=ICOL0+NPRCOL
            ENDIF
            IOFCOL=ICOLW2
       ELSEIF(ISOLMT(IVOL2).EQ.3)THEN
            IF(ICOLW3.EQ.0)THEN
                 CALL GRATTS('CONDUCTORS-3','AREA')
                 ICOLW3=ICOL0
                 CALL COLSHD(ICOLW3)
                 ICOL0=ICOL0+NPRCOL
            ENDIF
            IOFCOL=ICOLW3
       ELSEIF(ISOLMT(IVOL2).EQ.11)THEN
            IF(ICOLD1.EQ.0)THEN
                 CALL GRATTS('DIELECTRIC-1','AREA')
                 ICOLD1=ICOL0
                 CALL COLSHD(ICOLD1)
                 ICOL0=ICOL0+NPRCOL
            ENDIF
            IOFCOL=ICOLD1
       ELSEIF(ISOLMT(IVOL2).EQ.12)THEN
            IF(ICOLD2.EQ.0)THEN
                 CALL GRATTS('DIELECTRIC-2','AREA')
                 ICOLD2=ICOL0
                 CALL COLSHD(ICOLD2)
                 ICOL0=ICOL0+NPRCOL
            ENDIF
            IOFCOL=ICOLD2
       ELSEIF(ISOLMT(IVOL2).EQ.13)THEN
            IF(ICOLD3.EQ.0)THEN
                 CALL GRATTS('DIELECTRIC-3','AREA')
                 ICOLD3=ICOL0
                 CALL COLSHD(ICOLD3)
                 ICOL0=ICOL0+NPRCOL
            ENDIF
            IOFCOL=ICOLD3
       ELSE
            IOFCOL=0
       ENDIF
*   Assign the appropriate weight.
       CALL COLWGT(-A,-B,-C,WW)
*   And find out which colour we should use.
       IF(WW.GT.0)THEN
            ICOL=IOFCOL+NINT(MIN(1.0D0,WW)*(NPRCOL-1))
       ELSE
            ICOL=IOFCOL
       ENDIF
*   Store with the appropriate colour.
20     CONTINUE
       CALL PLABU1('STORE',IREF,NPL,XPL,YPL,ZPL,
     -      -A,-B,-C,ICOL,IVOL2,IFAIL1)
       IF(IFAIL1.NE.0)PRINT *,' !!!!!! BEMVIE WARNING : Panel ',IREF,
     -      ' volume 2 storage failed; panel skipped.'
*   Next panel.
10     CONTINUE
*** And sort them for plotting.
       CALL PLASRP
       END
