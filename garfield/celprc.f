CDECK  ID>, CELPRC.
       SUBROUTINE CELPRC(LUNPRT,ISW)
*-----------------------------------------------------------------------
*   CELPRC - Prints the current selection to unit LUNPRT
*   (Last changed on 11/10/11.)
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
       REAL EXMAP,EYMAP,EZMAP,VMAP,EWXMAP,EWYMAP,EWZMAP,VWMAP,
     -      BXMAP,BYMAP,BZMAP,
     -      XMAP,YMAP,ZMAP,XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,
     -      VMMIN,VMMAX,EPSMAT,EPSSUR,XFMOFF,YFMOFF,ZFMOFF
       INTEGER MATMAP,NMAP,NEPS,MAPORD,MAPTYP,IDRMAT,INDEWS,
     -      NWMAP
       LOGICAL MAPFLG,LMAPPL,SETAX,SETAY,SETAZ,ELMDGN,LSFDER
       CHARACTER EWSTYP
       CHARACTER*10 MATSRC
       COMMON /FLDMAP/ VMAP(MXMAP,10),VWMAP(MXMAP,10,MXWMAP),
     -      EXMAP(MXMAP,10),EYMAP(MXMAP,10),EZMAP(MXMAP,10),
     -      EWXMAP(MXMAP,10,MXWMAP),EWYMAP(MXMAP,10,MXWMAP),
     -      EWZMAP(MXMAP,10,MXWMAP),
     -      BXMAP(MXMAP,10),BYMAP(MXMAP,10),BZMAP(MXMAP,10),
     -      XMAP(MXMAP,10),YMAP(MXMAP,10),ZMAP(MXMAP,10),
     -      XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,VMMIN,VMMAX,
     -      XFMOFF,YFMOFF,ZFMOFF,
     -      EPSMAT(MXEPS),EPSSUR(MXEPS),MATMAP(MXMAP),
     -      NMAP,NEPS,MAPORD,MAPTYP,IDRMAT,INDEWS(MXWMAP),NWMAP,
     -      MAPFLG(10+4*MXWMAP),ELMDGN(MXMAP),
     -      LMAPPL,SETAX,SETAY,SETAZ,LSFDER
       COMMON /FLDCHR/ EWSTYP(MXWMAP),MATSRC
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
       REAL XPRT,YPRT
       INTEGER LUNPRT,NC1,NC2,NC3,NC4,NC5,I,J,K,IOS,ISW,NCOUNT
       CHARACTER*20 AUX1,AUX2,AUX3,AUX4,AUX5
*** Print a header.
       IF(ISW.EQ.0)THEN
            IF(NSW.EQ.0)THEN
                 WRITE(LUNPRT,'(/''  No electrode is currently'',
     -                '' selected for read-out.'')',
     -                ERR=2010,IOSTAT=IOS)
            ELSEIF(NSW.EQ.1)THEN
                 WRITE(LUNPRT,'(/''  A single group of electrodes'',
     -                '' is currently selected for read-out:'')',
     -                ERR=2010,IOSTAT=IOS)
            ELSE
                 CALL OUTFMT(REAL(NSW),2,AUX1,NC1,'LEFT')
                 WRITE(LUNPRT,'(/''  At present, '',A,'' groups of'',
     -                '' electrodes are selected for read-out:'')',
     -                ERR=2010,IOSTAT=IOS)
     -                AUX1(1:NC1)
            ENDIF
       ENDIF
*** Loop over the electrodes.
       DO 210 I=1,NSW
       IF(ISW.NE.0.AND.I.NE.ISW)GOTO 210
*   Print a header for this group.
       CALL OUTFMT(REAL(I),2,AUX1,NC1,'LEFT')
       WRITE(LUNPRT,'(/''  Group '',A,'' consists of:'')',
     -      ERR=2010,IOSTAT=IOS) AUX1(1:NC1)
*** Loop over the wires.
       DO 220 J=1,NWIRE
*   Pick out those with a matching id.
       IF(INDSW(J).NE.I)GOTO 220
*   Format position and potential.
       XPRT=X(J)
       YPRT=Y(J)
       CALL OUTFMT(REAL(J),2,AUX2,NC2,'LEFT')
       CALL OUTFMT(V(J)   ,2,AUX5,NC5,'LEFT')
       IF(POLAR)THEN
            CALL CFMRTP(XPRT,YPRT,XPRT,YPRT,1)
            CALL OUTFMT(XPRT   ,2,AUX3,NC3,'LEFT')
            CALL OUTFMT(YPRT   ,2,AUX4,NC4,'LEFT')
            WRITE(LUNPRT,'(5X,''Wire '',A,'' with label '',A,
     -           '' at (r,phi)=('',A,'','',A,'') and at '',A,
     -           '' V'')',ERR=2010,IOSTAT=IOS) AUX2(1:NC2),WIRTYP(J),
     -           AUX3(1:NC3),AUX4(1:NC4),AUX5(1:NC5)
       ELSE
            CALL OUTFMT(XPRT   ,2,AUX3,NC3,'LEFT')
            CALL OUTFMT(YPRT   ,2,AUX4,NC4,'LEFT')
            WRITE(LUNPRT,'(5X,''Wire '',A,'' with label '',A,
     -           '' at (x,y)=('',A,'','',A,'') and at '',A,
     -           '' V'')',ERR=2010,IOSTAT=IOS) AUX2(1:NC2),WIRTYP(J),
     -           AUX3(1:NC3),AUX4(1:NC4),AUX5(1:NC5)
       ENDIF
220    CONTINUE
*** Loop over the x-planes.
       DO 230 J=1,2
*   Pick out those with a matching id.
       IF(INDPLA(J).EQ.I)THEN
*   Format position and potential.
            IF(POLAR)THEN
                 CALL OUTFMT(EXP(COPLAN(J)),2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(VTPLAN(J)     ,2,AUX3,NC3,'LEFT')
                 WRITE(LUNPRT,'(5X,''The plane with label '',A,
     -                '' at r='',A,'' cm and at '',A,'' V'')',
     -                ERR=2010,IOSTAT=IOS) PLATYP(J),AUX2(1:NC2),
     -                AUX3(1:NC3)
            ELSE
                 CALL OUTFMT(COPLAN(J)     ,2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(VTPLAN(J)     ,2,AUX3,NC3,'LEFT')
                 WRITE(LUNPRT,'(5X,''The plane with label '',A,
     -                '' at x='',A,'' cm and at '',A,'' V'')',
     -                ERR=2010,IOSTAT=IOS) PLATYP(J),AUX2(1:NC2),
     -                AUX3(1:NC3)
            ENDIF
       ENDIF
*   See whether there are selected strips in the plane.
       DO 260 K=1,NPSTR1(J)
       IF(INDST1(J,K).EQ.I)THEN
            IF(POLAR)THEN
                 CALL OUTFMT(EXP(COPLAN(J)),2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(180*PLSTR1(J,K,1)/PI,2,AUX4,NC4,'LEFT')
                 CALL OUTFMT(180*PLSTR1(J,K,2)/PI,2,AUX5,NC5,'LEFT')
                 WRITE(LUNPRT,'(5X,''The strip '',A,'' < phi < '',
     -                A,'' degrees, labeled '',A,'', of the plane'',
     -                '' at r='',A,'' cm'')',ERR=2010,IOSTAT=IOS)
     -                AUX4(1:NC4),AUX5(1:NC5),PSLAB1(J,K),AUX2(1:NC2)
            ELSE
                 CALL OUTFMT(COPLAN(J)    ,2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(PLSTR1(J,K,1),2,AUX4,NC4,'LEFT')
                 CALL OUTFMT(PLSTR1(J,K,2),2,AUX5,NC5,'LEFT')
                 WRITE(LUNPRT,'(5X,''The strip '',A,'' < y < '',
     -                A,'' cm, labeled '',A,'', of the plane'',
     -                '' at x='',A,'' cm'')',ERR=2010,IOSTAT=IOS)
     -                AUX4(1:NC4),AUX5(1:NC5),PSLAB1(J,K),AUX2(1:NC2)
            ENDIF
       ENDIF
260    CONTINUE
       DO 270 K=1,NPSTR2(J)
       IF(INDST2(J,K).EQ.I)THEN
            IF(POLAR)THEN
                 CALL OUTFMT(EXP(COPLAN(J)),2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(PLSTR2(J,K,1) ,2,AUX4,NC4,'LEFT')
                 CALL OUTFMT(PLSTR2(J,K,2) ,2,AUX5,NC5,'LEFT')
                 WRITE(LUNPRT,'(5X,''The strip '',A,'' < z < '',
     -                A,'' cm, labeled '',A,'', of the plane'',
     -                '' at r='',A,'' cm'')',ERR=2010,IOSTAT=IOS)
     -                AUX4(1:NC4),AUX5(1:NC5),PSLAB2(J,K),AUX2(1:NC2)
            ELSE
                 CALL OUTFMT(COPLAN(J)    ,2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(PLSTR2(J,K,1),2,AUX4,NC4,'LEFT')
                 CALL OUTFMT(PLSTR2(J,K,2),2,AUX5,NC5,'LEFT')
                 WRITE(LUNPRT,'(5X,''The strip '',A,'' < z < '',
     -                A,'' cm, labeled '',A,'', of the plane'',
     -                '' at x='',A,'' cm'')',ERR=2010,IOSTAT=IOS)
     -                AUX4(1:NC4),AUX5(1:NC5),PSLAB2(J,K),AUX2(1:NC2)
            ENDIF
       ENDIF
270    CONTINUE
230    CONTINUE
*** Loop over the y-planes.
       DO 240 J=3,4
*   Pick out those with a matching id.
       IF(INDPLA(J).EQ.I)THEN
*   Format position and potential.
            IF(POLAR)THEN
                 CALL OUTFMT(180*COPLAN(J)/PI,2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(VTPLAN(J)       ,2,AUX3,NC3,'LEFT')
                 WRITE(LUNPRT,'(5X,''The plane with label '',A,
     -                '' at phi='',A,'' degrees and at '',A,
     -                '' V'')',ERR=2010,IOSTAT=IOS) PLATYP(J),
     -                AUX2(1:NC2),AUX3(1:NC3)
            ELSE
                 CALL OUTFMT(COPLAN(J)       ,2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(VTPLAN(J)       ,2,AUX3,NC3,'LEFT')
                 WRITE(LUNPRT,'(5X,''The plane with label '',A,
     -                '' at y='',A,'' cm and at '',A,'' V'')',
     -                ERR=2010,IOSTAT=IOS) PLATYP(J),AUX2(1:NC2),
     -                AUX3(1:NC3)
            ENDIF
       ENDIF
*   See whether there are selected strips in the plane.
       DO 280 K=1,NPSTR1(J)
       IF(INDST1(J,K).EQ.I)THEN
            IF(POLAR)THEN
                 CALL OUTFMT(180*COPLAN(J)/PI,2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(EXP(PLSTR1(J,K,1)),2,AUX4,NC4,'LEFT')
                 CALL OUTFMT(EXP(PLSTR1(J,K,2)),2,AUX5,NC5,'LEFT')
                 WRITE(LUNPRT,'(5X,''The strip '',A,'' < r < '',
     -                A,'' cm, labeled '',A,'', of the planen at'',
     -                '' phi='',A,'' degrees'')',ERR=2010,IOSTAT=IOS)
     -                AUX4(1:NC4),AUX5(1:NC5),PSLAB1(J,K),AUX2(1:NC2)
            ELSE
                 CALL OUTFMT(COPLAN(J)    ,2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(PLSTR1(J,K,1),2,AUX4,NC4,'LEFT')
                 CALL OUTFMT(PLSTR1(J,K,2),2,AUX5,NC5,'LEFT')
                 WRITE(LUNPRT,'(5X,''The strip '',A,'' < x < '',
     -                A,'' cm, labeled '',A,'', of the plane at'',
     -                '' y='',A,'' cm'')',ERR=2010,IOSTAT=IOS)
     -                AUX4(1:NC4),AUX5(1:NC5),PSLAB1(J,K),AUX2(1:NC2)
            ENDIF
       ENDIF
280    CONTINUE
       DO 290 K=1,NPSTR2(J)
       IF(INDST2(J,K).EQ.I)THEN
            IF(POLAR)THEN
                 CALL OUTFMT(180*COPLAN(J)/PI,2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(PLSTR2(J,K,1),2,AUX4,NC4,'LEFT')
                 CALL OUTFMT(PLSTR2(J,K,2),2,AUX5,NC5,'LEFT')
                 WRITE(LUNPRT,'(5X,''The strip '',A,'' < z < '',
     -                A,'' cm, labeled '',A,'', of the plane at'',
     -                '' phi='',A,'' degrees'')',ERR=2010,IOSTAT=IOS)
     -                AUX4(1:NC4),AUX5(1:NC5),PSLAB2(J,K),AUX2(1:NC2)
            ELSE
                 CALL OUTFMT(COPLAN(J)    ,2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(PLSTR2(J,K,1),2,AUX4,NC4,'LEFT')
                 CALL OUTFMT(PLSTR2(J,K,2),2,AUX5,NC5,'LEFT')
                 WRITE(LUNPRT,'(5X,''The strip '',A,'' < z < '',
     -                A,'' cm, labeled '',A,'', of the plane at'',
     -                '' y='',A,'' cm'')',ERR=2010,IOSTAT=IOS)
     -                AUX4(1:NC4),AUX5(1:NC5),PSLAB2(J,K),AUX2(1:NC2)
            ENDIF
       ENDIF
290    CONTINUE
240    CONTINUE
*** Check whether the tube has been selected.
       IF(INDPLA(5).EQ.I)THEN
            CALL OUTFMT(COTUBE,2,AUX2,NC2,'LEFT')
            CALL OUTFMT(VTTUBE,2,AUX3,NC3,'LEFT')
            WRITE(LUNPRT,'(5X,''The tube with label '',A,
     -           '', radius='',A,'' cm and potential '',A,
     -           '' V'')',ERR=2010,IOSTAT=IOS)
     -           PLATYP(5),AUX2(1:NC2),AUX3(1:NC3)
       ENDIF
*   See whether there are selected strips in the tube.
       DO 300 K=1,NPSTR1(5)
       IF(INDST1(5,K).EQ.I)THEN
            CALL OUTFMT(180*PLSTR1(5,K,1)/PI,2,AUX4,NC4,'LEFT')
            CALL OUTFMT(180*PLSTR1(5,K,2)/PI,2,AUX5,NC5,'LEFT')
            WRITE(LUNPRT,'(5X,''The sector '',A,'' < phi < '',
     -           A,'' degrees, labeled '',A,'', of the tube'')',
     -           ERR=2010,IOSTAT=IOS)
     -           AUX4(1:NC4),AUX5(1:NC5),PSLAB1(5,K)
       ENDIF
300    CONTINUE
       DO 310 K=1,NPSTR2(5)
       IF(INDST2(5,K).EQ.I)THEN
            CALL OUTFMT(PLSTR2(5,K,1),2,AUX4,NC4,'LEFT')
            CALL OUTFMT(PLSTR2(5,K,2),2,AUX5,NC5,'LEFT')
            WRITE(LUNPRT,'(5X,''The ring '',A,'' < z < '',
     -           A,'' cm, labeled '',A,'', of the tube'')',
     -           ERR=2010,IOSTAT=IOS)
     -           AUX4(1:NC4),AUX5(1:NC5),PSLAB2(5,K)
       ENDIF
310    CONTINUE
*** Loop over the weighting field maps.
       DO 250 K=1,NWMAP
*   Pick out if matching.
       IF(INDEWS(K).NE.I)GOTO 250
*   Header.
       CALL OUTFMT(REAL(K),2,AUX1,NC1,'LEFT')
       WRITE(LUNPRT,'(5X,''Finite element weighting field map '',A,
     -      '' with label '',A,'' representing:'')',
     -      ERR=2010,IOSTAT=IOS) AUX1(1:NC1),EWSTYP(K)
*   Check for matching solids.
       NCOUNT=0
       DO 10 J=1,NSOLID
       IF(INDSOL(J).NE.I)GOTO 10
       CALL OUTFMT(REAL(J),2,AUX5,NC5,'LEFT')
*   Cylinders.
       IF(ISOLTP(J).EQ.1)THEN
            CALL OUTFMT(REAL(CBUF(ISTART(J)+3)),2,AUX2,NC2,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(J)+4)),2,AUX3,NC3,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(J)+5)),2,AUX4,NC4,'LEFT')
            WRITE(LUNPRT,'(8X,''Cylinder '',A,'' with label '',A,
     -           '' centered at ('', A,'','',A,'','',A,'')'')',
     -           ERR=2010,IOSTAT=IOS) AUX5(1:NC5),SOLTYP(J),
     -           AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
            NCOUNT=NCOUNT+1
*   Holes.
       ELSEIF(ISOLTP(J).EQ.2)THEN
            CALL OUTFMT(REAL(CBUF(ISTART(J)+6)),2,AUX2,NC2,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(J)+7)),2,AUX3,NC3,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(J)+8)),2,AUX4,NC4,'LEFT')
            WRITE(LUNPRT,'(8X,''Hole '',A,'' with label '',A,
     -           '' centered at ('', A,'','',A,'','',A,'')'')',
     -           ERR=2010,IOSTAT=IOS) AUX5(1:NC5),SOLTYP(J),
     -           AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
            NCOUNT=NCOUNT+1
*   Boxes.
       ELSEIF(ISOLTP(J).EQ.3)THEN
            CALL OUTFMT(REAL(CBUF(ISTART(J)+4)),2,AUX2,NC2,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(J)+5)),2,AUX3,NC3,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(J)+6)),2,AUX4,NC4,'LEFT')
            WRITE(LUNPRT,'(8X,''Box '',A,'' with label '',A,
     -           '' centered at ('', A,'','',A,'','',A,'')'')',
     -           ERR=2010,IOSTAT=IOS) AUX5(1:NC5),SOLTYP(J),
     -           AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
            NCOUNT=NCOUNT+1
*   Sphere.
       ELSEIF(ISOLTP(J).EQ.4)THEN
            CALL OUTFMT(REAL(CBUF(ISTART(J)+2)),2,AUX2,NC2,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(J)+3)),2,AUX3,NC3,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(J)+4)),2,AUX4,NC4,'LEFT')
            WRITE(LUNPRT,'(8X,''Sphere '',A,'' with label '',A,
     -           '' centered at ('', A,'','',A,'','',A,'')'')',
     -           ERR=2010,IOSTAT=IOS) AUX5(1:NC5),SOLTYP(J),
     -           AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
            NCOUNT=NCOUNT+1
*   Ridge.
       ELSEIF(ISOLTP(J).EQ.5)THEN
            CALL OUTFMT(REAL(CBUF(ISTART(J)+4)),2,AUX2,NC2,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(J)+5)),2,AUX3,NC3,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(J)+6)),2,AUX4,NC4,'LEFT')
            WRITE(LUNPRT,'(8X,''Ridge '',A,'' with label '',A,
     -           '' centered at ('', A,'','',A,'','',A,'')'')',
     -           ERR=2010,IOSTAT=IOS) AUX5(1:NC5),SOLTYP(J),
     -           AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
            NCOUNT=NCOUNT+1
*   Extrusion.
       ELSEIF(ISOLTP(J).EQ.6)THEN
            CALL OUTFMT(REAL(CBUF(ISTART(J)+3)),2,AUX2,NC2,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(J)+4)),2,AUX3,NC3,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(J)+5)),2,AUX4,NC4,'LEFT')
            WRITE(LUNPRT,'(8X,''Extrusion '',A,'' with label '',A,
     -           '' centered at ('', A,'','',A,'','',A,'')'')',
     -           ERR=2010,IOSTAT=IOS) AUX5(1:NC5),SOLTYP(J),
     -           AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
            NCOUNT=NCOUNT+1
*   Other
       ELSE
            PRINT *,' !!!!!! CELPRC WARNING : Found a solid'//
     -           ' of unknown type; not printed.'
       ENDIF
10     CONTINUE
       IF(NCOUNT.EQ.0)WRITE(LUNPRT,'(8X,''No matching solid'')',
     -      ERR=2010,IOSTAT=IOS)
250    CONTINUE
*** Solids with neBEM fields.
       IF(BEMSET)THEN
            DO 40 J=1,NSOLID
            IF(INDSOL(J).NE.I)GOTO 40
            CALL OUTFMT(REAL(J),2,AUX5,NC5,'LEFT')
*   Cylinders.
            IF(ISOLTP(J).EQ.1)THEN
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+3)),2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+4)),2,AUX3,NC3,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+5)),2,AUX4,NC4,'LEFT')
                 WRITE(LUNPRT,'(8X,''Cylinder '',A,'' with label '',A,
     -                '' centered at ('', A,'','',A,'','',A,'')'')',
     -                ERR=2010,IOSTAT=IOS) AUX5(1:NC5),SOLTYP(J),
     -                AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
*   Holes.
            ELSEIF(ISOLTP(J).EQ.2)THEN
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+6)),2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+7)),2,AUX3,NC3,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+8)),2,AUX4,NC4,'LEFT')
                 WRITE(LUNPRT,'(8X,''Hole '',A,'' with label '',A,
     -                '' centered at ('', A,'','',A,'','',A,'')'')',
     -                ERR=2010,IOSTAT=IOS) AUX5(1:NC5),SOLTYP(J),
     -                AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
*   Boxes.
            ELSEIF(ISOLTP(J).EQ.3)THEN
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+4)),2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+5)),2,AUX3,NC3,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+6)),2,AUX4,NC4,'LEFT')
                 WRITE(LUNPRT,'(8X,''Box '',A,'' with label '',A,
     -                '' centered at ('', A,'','',A,'','',A,'')'')',
     -                ERR=2010,IOSTAT=IOS) AUX5(1:NC5),SOLTYP(J),
     -                AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
*   Sphere.
            ELSEIF(ISOLTP(J).EQ.4)THEN
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+2)),2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+3)),2,AUX3,NC3,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+4)),2,AUX4,NC4,'LEFT')
                 WRITE(LUNPRT,'(8X,''Sphere '',A,'' with label '',A,
     -                '' centered at ('', A,'','',A,'','',A,'')'')',
     -                ERR=2010,IOSTAT=IOS) AUX5(1:NC5),SOLTYP(J),
     -                AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
*   Ridge.
            ELSEIF(ISOLTP(J).EQ.5)THEN
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+4)),2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+5)),2,AUX3,NC3,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+6)),2,AUX4,NC4,'LEFT')
                 WRITE(LUNPRT,'(8X,''Ridge '',A,'' with label '',A,
     -                '' centered at ('', A,'','',A,'','',A,'')'')',
     -                ERR=2010,IOSTAT=IOS) AUX5(1:NC5),SOLTYP(J),
     -                AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
*   Extrusion.
            ELSEIF(ISOLTP(J).EQ.6)THEN
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+3)),2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+4)),2,AUX3,NC3,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+5)),2,AUX4,NC4,'LEFT')
                 WRITE(LUNPRT,'(8X,''Extrusion '',A,'' with label '',A,
     -                '' centered at ('', A,'','',A,'','',A,'')'')',
     -                ERR=2010,IOSTAT=IOS) AUX5(1:NC5),SOLTYP(J),
     -                AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
*   Other
            ELSE
                 PRINT *,' !!!!!! CELPRC WARNING : Found a solid'//
     -                ' of unknown type; not printed.'
            ENDIF
40          CONTINUE
       ENDIF
*** Next electrode identifier.
210    CONTINUE
*** Now check for selected solids not assigned to a readout group.
       IF(ISW.EQ.0)THEN
*   See whether there are any.
            NCOUNT=0
            DO 30 J=1,NSOLID
            IF(INDSOL(J).EQ.-1)NCOUNT=NCOUNT+1
30          CONTINUE
*   Header.
            IF(NCOUNT.EQ.0)THEN
                 WRITE(LUNPRT,'(/''  No solid is currently selected'',
     -                '' outside read-out.'')',ERR=2010,IOSTAT=IOS)
            ELSE
                 WRITE(LUNPRT,'(/''  Solids which are selected but'',
     -                '' not read out:'')',ERR=2010,IOSTAT=IOS)
            ENDIF
*   Check for matching solids.
            NCOUNT=0
            DO 20 J=1,NSOLID
            IF(INDSOL(J).NE.-1)GOTO 20
            CALL OUTFMT(REAL(J),2,AUX5,NC5,'LEFT')
*   Cylinders.
            IF(ISOLTP(J).EQ.1)THEN
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+3)),2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+4)),2,AUX3,NC3,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+5)),2,AUX4,NC4,'LEFT')
                 WRITE(LUNPRT,'(5X,''Cylinder '',A,'' with label '',A,
     -                '' centered at ('', A,'','',A,'','',A,'')'')',
     -                ERR=2010,IOSTAT=IOS) AUX5(1:NC5),SOLTYP(J),
     -                AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
                 NCOUNT=NCOUNT+1
*   Holes.
            ELSEIF(ISOLTP(J).EQ.2)THEN
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+6)),2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+7)),2,AUX3,NC3,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+8)),2,AUX4,NC4,'LEFT')
                 WRITE(LUNPRT,'(5X,''Hole '',A,'' with label '',A,
     -                '' centered at ('', A,'','',A,'','',A,'')'')',
     -                ERR=2010,IOSTAT=IOS) AUX5(1:NC5),SOLTYP(J),
     -                AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
                 NCOUNT=NCOUNT+1
*   Boxes.
            ELSEIF(ISOLTP(J).EQ.3)THEN
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+4)),2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+5)),2,AUX3,NC3,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+6)),2,AUX4,NC4,'LEFT')
                 WRITE(LUNPRT,'(5X,''Box '',A,'' with label '',A,
     -                '' centered at ('', A,'','',A,'','',A,'')'')',
     -                ERR=2010,IOSTAT=IOS) AUX5(1:NC5),SOLTYP(J),
     -                AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
                 NCOUNT=NCOUNT+1
*   Sphere.
            ELSEIF(ISOLTP(J).EQ.4)THEN
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+2)),2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+3)),2,AUX3,NC3,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+4)),2,AUX4,NC4,'LEFT')
                 WRITE(LUNPRT,'(5X,''Sphere '',A,'' with label '',A,
     -                '' centered at ('', A,'','',A,'','',A,'')'')',
     -                ERR=2010,IOSTAT=IOS) AUX5(1:NC5),SOLTYP(J),
     -                AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
                 NCOUNT=NCOUNT+1
*   Ridge.
            ELSEIF(ISOLTP(J).EQ.5)THEN
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+4)),2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+5)),2,AUX3,NC3,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+6)),2,AUX4,NC4,'LEFT')
                 WRITE(LUNPRT,'(8X,''Ridge '',A,'' with label '',A,
     -                '' centered at ('', A,'','',A,'','',A,'')'')',
     -                ERR=2010,IOSTAT=IOS) AUX5(1:NC5),SOLTYP(J),
     -                AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
*   Extrusion.
            ELSEIF(ISOLTP(J).EQ.6)THEN
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+3)),2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+4)),2,AUX3,NC3,'LEFT')
                 CALL OUTFMT(REAL(CBUF(ISTART(J)+5)),2,AUX4,NC4,'LEFT')
                 WRITE(LUNPRT,'(8X,''Extrusion '',A,'' with label '',A,
     -                '' centered at ('', A,'','',A,'','',A,'')'')',
     -                ERR=2010,IOSTAT=IOS) AUX5(1:NC5),SOLTYP(J),
     -                AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
*   Other
            ELSE
                 PRINT *,' !!!!!! CELPRC WARNING : Found a solid'//
     -                ' of unknown type; not printed.'
            ENDIF
20          CONTINUE
       ENDIF
       RETURN
*** I/O errors.
2010   CONTINUE
       PRINT *,' !!!!!! CELPRC WARNING : Error writing out the group'//
     -      ' composition of the electrodes to unit ',LUNPRT
       CALL INPIOS(IOS)
       END
