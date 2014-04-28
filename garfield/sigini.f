CDECK  ID>, SIGINI.
       SUBROUTINE SIGINI(IFAIL)
*-----------------------------------------------------------------------
*   SIGINI - Initialises signal calculations.
*   (Last changed on  3/ 4/10.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER IFAIL,IFAIL1
       DOUBLE PRECISION DUMMY(1)
*** Assume that the routine will fail.
       IFAIL=1
*** If using natural periodicity, copy the cell type.
       IF(NFOUR.EQ.0)THEN
            FCELTP=TYPE
*   Otherwise, eliminate true periodicities.
       ELSEIF(TYPE.EQ.'A  '.OR.TYPE.EQ.'B1X'.OR.
     -      TYPE.EQ.'B1Y'.OR.TYPE.EQ.'C1 ')THEN
            FCELTP='A  '
       ELSEIF(TYPE.EQ.'B2X'.OR.TYPE.EQ.'C2X')THEN
            FCELTP='B2X'
       ELSEIF(TYPE.EQ.'B2Y'.OR.TYPE.EQ.'C2Y')THEN
            FCELTP='B2Y'
       ELSEIF(TYPE.EQ.'C3 ')THEN
            FCELTP='C3 '
       ELSEIF(TYPE.EQ.'D1 ')THEN
            FCELTP='D1 '
       ELSEIF(TYPE.EQ.'D3 ')THEN
            FCELTP='D3 '
       ELSEIF(TYPE.EQ.'MAP')THEN
            FCELTP='MAP'
       ELSEIF(TYPE.EQ.'BEM')THEN
            FCELTP='BEM'
*   Other cases.
       ELSE
            PRINT *,' !!!!!! SIGINI WARNING : No potentials available'//
     -           ' to handle cell type '//TYPE//'; no signals.'
            RETURN
       ENDIF
*** Establish the directions in which convolutions occur.
       FPERX=.FALSE.
       FPERY=.FALSE.
       IF(NFOUR.EQ.0)THEN
            MFEXP=0
       ELSE
            IF(TYPE.EQ.'B1X'.OR.TYPE.EQ.'C1 '.OR.TYPE.EQ.'C2Y')
     -           FPERX=.TRUE.
            IF(TYPE.EQ.'B1Y'.OR.TYPE.EQ.'C1 '.OR.TYPE.EQ.'C2X')
     -           FPERY=.TRUE.
            MFEXP=INT(0.1+LOG(1.0*NFOUR)/LOG(2.0))
            IF(MFEXP.EQ.0)FPERX=.FALSE.
            IF(MFEXP.EQ.0)FPERY=.FALSE.
       ENDIF
*** Set maximum and minimum Fourier terms.
       MXMIN=0
       MYMIN=0
       MXMAX=0
       MYMAX=0
       IF(FPERX)MXMIN=MIN(0,-NFOUR/2+1)
       IF(FPERX)MXMAX=+NFOUR/2
       IF(FPERY)MYMIN=MIN(0,-NFOUR/2+1)
       IF(FPERY)MYMAX=+NFOUR/2
*** Print some debugging output if requested.
       IF(LDEBUG)WRITE(LUNOUT,'(
     -        ''  ++++++ SIGINI DEBUG   : Cell type           = '',A3/
     -        26X,''Fourier cell type   = '',A3/
     -        26X,''x convolutions      = '',L1/
     -        26X,''y convolutions      = '',L1/
     -        26X,''No of Fourier terms = '',I3,'' (= 2**'',I3,'')'')')
     -        TYPE,FCELTP,FPERX,FPERY,NFOUR,MFEXP
*** Prepare the signal matrices.
       IF(TYPE.NE.'MAP'.AND.TYPE.NE.'BEM')THEN
            CALL SIGIPR(IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! SIGINI WARNING : Preparing'//
     -                ' wire signal capacitance matrices failed;'//
     -                ' no signals.'
                 RETURN
            ENDIF
            CALL SIGPLP(IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! SIGINI WARNING : Preparing'//
     -                ' plane charges failed; no signals.'
                 RETURN
            ENDIF
       ENDIF
*** And open the signal file.
       CALL SIGIST('OPEN',0,DUMMY,DUMMY,0,0,0,0,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! SIGINI WARNING : Unable to'//
     -           ' open a signal storage file; no signals.'
            RETURN
       ENDIF
*** Set the angular spread entry point to 0 (i.e. absent).
       IENANG=0
*** Seems to have worked.
       IFAIL=0
       SIGSET=.TRUE.
       END
