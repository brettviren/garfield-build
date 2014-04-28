CDECK  ID>, CELPRT.
       SUBROUTINE CELPRT
*-----------------------------------------------------------------------
*   CELPRT - Subroutine printing all available information on the cell.
*   VARIABLES : Only trivial local variables.
*   (Last changed on 22/02/09.)
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
       CHARACTER*30 AUX1,AUX2,AUX3,AUX4,AUX5,AUX6,AUX7,AUX8,AUX9
       CHARACTER*120 OUTSTR
       INTEGER I,J,NCAUX,NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,NC9,NCOUT,
     -      LMIN,RMAX
       REAL XPRT,YPRT,DPRT,SUMCH,XMINP,YMINP,XMAXP,YMAXP
*** Identify the procedure.
       IF(LIDENT)PRINT *,' /// ROUTINE CELPRT ///'
*** Write a heading for the summary, including the cell id
       WRITE(LUNOUT,'(''1 SUMMARY OF THE CELL DATA''/
     -      ''  ========================'')')
       IF(CELLID.NE.' ')WRITE(LUNOUT,'(/''  Cell identification : '',
     -      A)') CELLID
*** Print positions of wires, applied voltages and resulting charges.
       IF(POLAR.AND.NWIRE.GE.1)THEN
            WRITE(LUNOUT,'(/''  TABLE OF THE WIRES''//
     -        ''    Nr Diameter        r      phi  Voltage'',
     -        ''      Charge  Tension   Length  Density Label''/
     -        ''       [micron]     [cm]    [deg]   [Volt]'',
     -        ''     [pC/cm]      [g]     [cm]  [g/cm3]''/)')
       ELSEIF(NWIRE.GE.1)THEN
            WRITE(LUNOUT,'(/''  TABLE OF THE WIRES''//
     -        ''    Nr Diameter        x        y  Voltage'',
     -        ''      Charge  Tension   Length  Density Label''/
     -        ''       [micron]     [cm]     [cm]   [Volt]'',
     -        ''     [pC/cm]      [g]     [cm]  [g/cm3]''/)')
       ENDIF
       DO 10 I=1,NWIRE
       XPRT=X(I)
       YPRT=Y(I)
       DPRT=D(I)
       IF(POLAR)THEN
            CALL CFMRTP(XPRT,YPRT,XPRT,YPRT,1)
            DPRT=D(I)*XPRT
       ENDIF
       CALL OUTFMT(REAL(I),2,AUX9,NC9,'RIGHT')
       CALL OUTFMT(DPRT*10000.0,2,AUX1,NC1,'RIGHT')
       CALL OUTFMT(XPRT,2,AUX2,NC2,'RIGHT')
       CALL OUTFMT(YPRT,2,AUX3,NC3,'RIGHT')
       CALL OUTFMT(V(I),2,AUX4,NC4,'RIGHT')
       CALL OUTFMT(2.0E12*PI*EPS0*E(I),2,AUX5,NC5,'RIGHT')
       CALL OUTFMT(W(I),2,AUX6,NC6,'RIGHT')
       CALL OUTFMT(U(I),2,AUX7,NC7,'RIGHT')
       CALL OUTFMT(DENS(I),2,AUX8,NC8,'RIGHT')
       WRITE(LUNOUT,'(2X,A4,A9,A9,A9,A9,A12,A9,A9,A9,5X,A1)')
     -      AUX9(27:),AUX1(22:),AUX2(22:),AUX3(22:),AUX4(22:),
     -      AUX5(19:),AUX6(22:),AUX7(22:),AUX8(22:),WIRTYP(I)
10     CONTINUE
*** Field map perhaps ?
       IF(NMAP.GE.1)THEN
            WRITE(LUNOUT,'(/''  FIELD MAP'')')
            CALL MAPPRT
       ENDIF
       IF(NSOLID.GE.1)CALL CELSPR
*** Print information on the tube if present.
       IF(TUBE)THEN
            CALL OUTFMT(VTTUBE,2,AUX1,NC1,'LEFT')
            CALL OUTFMT(COTUBE,2,AUX2,NC2,'LEFT')
            IF(NTUBE.EQ.0)THEN
                 AUX3='Circular'
                 NC3=8
            ELSEIF(NTUBE.EQ.3)THEN
                 AUX3='Triangular'
                 NC3=10
            ELSEIF(NTUBE.EQ.4)THEN
                 AUX3='Square'
                 NC3=6
            ELSEIF(NTUBE.EQ.5)THEN
                 AUX3='Pentagonal'
                 NC3=10
            ELSEIF(NTUBE.EQ.6)THEN
                 AUX3='Hexagonal'
                 NC3=9
            ELSEIF(NTUBE.EQ.7)THEN
                 AUX3='Heptagonal'
                 NC3=10
            ELSEIF(NTUBE.EQ.8)THEN
                 AUX3='Octagonal'
                 NC3=9
            ELSE
                 CALL OUTFMT(REAL(NTUBE),2,AUX5,NC5,'LEFT')
                 AUX3='polygonal with '//AUX5(1:NC5)//' corners'
                 NC3=23+NC5
            ENDIF
            IF(PLATYP(5).EQ.'?')THEN
                 AUX4='Not labeled'
                 NC4=11
            ELSE
                 AUX4=PLATYP(5)
                 NC4=1
            ENDIF
            WRITE(LUNOUT,'(/''  ENCLOSING TUBE''//
     -           ''  Potential:  '',A,'' V''/
     -           ''  Radius:     '',A,'' cm''/
     -           ''  Shape:      '',A/
     -           ''  Label:      '',A)')
     -           AUX1(1:NC1),AUX2(1:NC2),AUX3(1:NC3),AUX4(1:NC4)
            IF(NPSTR1(5).GT.0)THEN
                 WRITE(LUNOUT,'(''  phi-Strips:'')')
                 DO 110 I=1,NPSTR1(5)
                 CALL OUTFMT(180*PLSTR1(5,I,1)/PI,2,AUX1,NC1,'LEFT')
                 CALL OUTFMT(180*PLSTR1(5,I,2)/PI,2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(PLSTR1(5,I,3),2,AUX3,NC3,'LEFT')
                 IF(PSLAB1(5,I).EQ.'?')THEN
                      AUX4=' not labeled'
                      NC4=12
                 ELSE
                      AUX4=' label = '//PSLAB1(5,I)
                      NC4=10
                 ENDIF
                 WRITE(LUNOUT,'(14X,A)') AUX1(1:NC1)//' < phi < '//
     -                AUX2(1:NC2)//' degrees, gap = '//AUX3(1:NC3)//
     -                ' cm,'//AUX4(1:NC4)
110              CONTINUE
            ELSE
                 WRITE(LUNOUT,'(''  phi-Strips: None'')')
            ENDIF
            IF(NPSTR2(5).GT.0)THEN
                 WRITE(LUNOUT,'(''  z-Strips:'')')
                 DO 120 I=1,NPSTR2(5)
                 CALL OUTFMT(PLSTR2(5,I,1),2,AUX1,NC1,'LEFT')
                 CALL OUTFMT(PLSTR2(5,I,2),2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(PLSTR2(5,I,3),2,AUX3,NC3,'LEFT')
                 IF(PSLAB2(5,I).EQ.'?')THEN
                      AUX4=' not labeled'
                      NC4=12
                 ELSE
                      AUX4=' label = '//PSLAB2(5,I)
                      NC4=10
                 ENDIF
                 WRITE(LUNOUT,'(14X,A)') AUX1(1:NC1)//' < z < '//
     -                AUX2(1:NC2)//' cm, gap = '//AUX3(1:NC3)//
     -                ' cm,'//AUX4(1:NC4)
120              CONTINUE
            ELSE
                 WRITE(LUNOUT,'(''  z-Strips:   None'')')
            ENDIF
*** Print data on the equipotential planes, first those at const x or r
       ELSEIF(YNPLAN(1).OR.YNPLAN(2).OR.YNPLAN(3).OR.YNPLAN(4))THEN
            WRITE(LUNOUT,'(/''  EQUIPOTENTIAL PLANES'')')
            IF(YNPLAN(1).AND.YNPLAN(2).AND..NOT.POLAR)WRITE(LUNOUT,
     -           '(/''  There are two planes at constant x:'')')
            IF(YNPLAN(1).AND.YNPLAN(2).AND.POLAR)WRITE(LUNOUT,
     -           '(/''  There are two planes at constant r:'')')
            IF(((YNPLAN(1).AND..NOT.YNPLAN(2)).OR.(YNPLAN(2).AND..NOT.
     -           YNPLAN(1))).AND..NOT.POLAR)WRITE(LUNOUT,
     -           '(/''  There is one plane at constant x:'')')
            IF(((YNPLAN(1).AND..NOT.YNPLAN(2)).OR.(YNPLAN(2).AND..NOT.
     -           YNPLAN(1))).AND.POLAR)WRITE(LUNOUT,
     -           '(/''  There is one plane at constant r:'')')
            DO 20 I=1,2
            IF(.NOT.YNPLAN(I))GOTO 20
            NCOUT=0
            IF(POLAR)THEN
                 CALL OUTFMT(EXP(COPLAN(I)),2,AUX1,NC1,'LEFT')
                 OUTSTR(NCOUT+1:NCOUT+NC1+13)=
     -                '     r = '//AUX1(1:NC1)//' cm,'
                 NCOUT=NCOUT+NC1+13
            ELSE
                 CALL OUTFMT(COPLAN(I),2,AUX1,NC1,'LEFT')
                 OUTSTR(NCOUT+1:NCOUT+NC1+13)=
     -                '     x = '//AUX1(1:NC1)//' cm,'
                 NCOUT=NCOUT+NC1+13
            ENDIF
            IF(ABS(VTPLAN(I)).GT.1E-4)THEN
                 CALL OUTFMT(VTPLAN(I),2,AUX1,NC1,'LEFT')
                 OUTSTR(NCOUT+1:NCOUT+NC1+16)=
     -                ' potential = '//AUX1(1:NC1)//' V,'
                 NCOUT=NCOUT+NC1+16
            ELSE
                 OUTSTR(NCOUT+1:NCOUT+9)=' earthed,'
                 NCOUT=NCOUT+9
            ENDIF
            IF(PLATYP(I).NE.'?')THEN
                 OUTSTR(NCOUT+1:NCOUT+11)=' label = '//PLATYP(I)//','
                 NCOUT=NCOUT+11
            ELSE
                 OUTSTR(NCOUT+1:NCOUT+13)=' not labeled,'
                 NCOUT=NCOUT+13
            ENDIF
            IF(NPSTR1(I).EQ.0.AND.NPSTR2(I).EQ.0)THEN
                 OUTSTR(NCOUT+1:NCOUT+11)=' no strips.'
                 NCOUT=NCOUT+11
            ELSE
                 OUTSTR(NCOUT+1:NCOUT+21)=' divided into strips:'
                 NCOUT=NCOUT+21
            ENDIF
            WRITE(LUNOUT,'(A)') OUTSTR(1:NCOUT)
            DO 70 J=1,NPSTR1(I)
            CALL OUTFMT(PLSTR1(I,J,3),2,AUX4,NC4,'LEFT')
            IF(PSLAB1(I,J).EQ.'?')THEN
                 AUX5=' not labeled'
                 NC5=12
            ELSE
                 AUX5=' label = '//PSLAB1(I,J)
                 NC5=10
            ENDIF
            IF(POLAR)THEN
                 CALL OUTFMT(180*PLSTR1(I,J,1)/PI,2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(180*PLSTR1(I,J,2)/PI,2,AUX3,NC3,'LEFT')
                 WRITE(LUNOUT,'(8X,A)') AUX2(1:NC2)//' < phi < '//
     -                AUX3(1:NC3)//' degrees, gap = '//AUX4(1:NC4)//
     -                ' cm,'//AUX5(1:NC5)
            ELSE
                 CALL OUTFMT(PLSTR1(I,J,1),2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(PLSTR1(I,J,2),2,AUX3,NC3,'LEFT')
                 WRITE(LUNOUT,'(8X,A)') AUX2(1:NC2)//' < y < '//
     -                AUX3(1:NC3)//' cm, gap = '//AUX4(1:NC4)//' cm,'//
     -                AUX5(1:NC5)
            ENDIF
70          CONTINUE
            DO 80 J=1,NPSTR2(I)
            CALL OUTFMT(PLSTR2(I,J,1),2,AUX2,NC2,'LEFT')
            CALL OUTFMT(PLSTR2(I,J,2),2,AUX3,NC3,'LEFT')
            CALL OUTFMT(PLSTR2(I,J,3),2,AUX4,NC4,'LEFT')
            IF(PSLAB2(I,J).EQ.'?')THEN
                 AUX5=' not labeled'
                 NC5=12
            ELSE
                 AUX5=' label = '//PSLAB2(I,J)
                 NC5=10
            ENDIF
            WRITE(LUNOUT,'(8X,A)') AUX2(1:NC2)//' < z < '//
     -           AUX3(1:NC3)//' cm, gap = '//AUX4(1:NC4)//' cm,'//
     -           AUX5(1:NC5)
80          CONTINUE
20          CONTINUE
*  Next the planes at constant y or phi
            IF(YNPLAN(3).AND.YNPLAN(4).AND..NOT.POLAR)WRITE(LUNOUT,
     -           '(/''  There are two planes at constant y:'')')
            IF(YNPLAN(3).AND.YNPLAN(4).AND.POLAR)WRITE(LUNOUT,
     -           '(/''  There are two planes at constant phi:'')')
            IF(((YNPLAN(3).AND..NOT.YNPLAN(4)).OR.(YNPLAN(4).AND..NOT.
     -           YNPLAN(3))).AND..NOT.POLAR)WRITE(LUNOUT,
     -           '(/''  There is one plane at constant y:'')')
            IF(((YNPLAN(3).AND..NOT.YNPLAN(4)).OR.(YNPLAN(4).AND..NOT.
     -           YNPLAN(3))).AND.POLAR)WRITE(LUNOUT,
     -           '(/''  There is one plane at constant phi:'')')
            DO 30 I=3,4
            IF(.NOT.YNPLAN(I))GOTO 30
            NCOUT=0
            IF(POLAR)THEN
                 CALL OUTFMT(180*COPLAN(I)/PI,2,AUX1,NC1,'LEFT')
                 OUTSTR(NCOUT+1:NCOUT+NC1+20)=
     -                '     phi = '//AUX1(1:NC1)//' degrees,'
                 NCOUT=NCOUT+NC1+20
            ELSE
                 CALL OUTFMT(COPLAN(I),2,AUX1,NC1,'LEFT')
                 OUTSTR(NCOUT+1:NCOUT+NC1+13)=
     -                '     y = '//AUX1(1:NC1)//' cm,'
                 NCOUT=NCOUT+NC1+13
            ENDIF
            IF(ABS(VTPLAN(I)).GT.1E-4)THEN
                 CALL OUTFMT(VTPLAN(I),2,AUX1,NC1,'LEFT')
                 OUTSTR(NCOUT+1:NCOUT+NC1+16)=
     -                ' potential = '//AUX1(1:NC1)//' V,'
                 NCOUT=NCOUT+NC1+16
            ELSE
                 OUTSTR(NCOUT+1:NCOUT+9)=' earthed,'
                 NCOUT=NCOUT+9
            ENDIF
            IF(PLATYP(I).NE.'?')THEN
                 OUTSTR(NCOUT+1:NCOUT+11)=' label = '//PLATYP(I)//','
                 NCOUT=NCOUT+11
            ELSE
                 OUTSTR(NCOUT+1:NCOUT+13)=' not labeled,'
                 NCOUT=NCOUT+13
            ENDIF
            IF(NPSTR1(I).EQ.0.AND.NPSTR2(I).EQ.0)THEN
                 OUTSTR(NCOUT+1:NCOUT+11)=' no strips.'
                 NCOUT=NCOUT+11
            ELSE
                 OUTSTR(NCOUT+1:NCOUT+21)=' divided into strips:'
                 NCOUT=NCOUT+21
            ENDIF
            WRITE(LUNOUT,'(A)') OUTSTR(1:NCOUT)
            DO 90 J=1,NPSTR1(I)
            CALL OUTFMT(PLSTR1(I,J,3),2,AUX4,NC4,'LEFT')
            IF(PSLAB1(I,J).EQ.'?')THEN
                 AUX5=' not labeled'
                 NC5=12
            ELSE
                 AUX5=' label = '//PSLAB1(I,J)
                 NC5=10
            ENDIF
            IF(POLAR)THEN
                 CALL OUTFMT(EXP(PLSTR1(I,J,1)),2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(EXP(PLSTR1(I,J,2)),2,AUX3,NC3,'LEFT')
                 WRITE(LUNOUT,'(8X,A)') AUX2(1:NC2)//' < r < '//
     -                AUX3(1:NC3)//' cm, gap = '//AUX4(1:NC4)//
     -                ' cm,'//AUX5(1:NC5)
            ELSE
                 CALL OUTFMT(PLSTR1(I,J,1),2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(PLSTR1(I,J,2),2,AUX3,NC3,'LEFT')
                 WRITE(LUNOUT,'(8X,A)') AUX2(1:NC2)//' < x < '//
     -                AUX3(1:NC3)//' cm, gap = '//AUX4(1:NC4)//' cm,'//
     -                AUX5(1:NC5)
            ENDIF
90          CONTINUE
            DO 100 J=1,NPSTR2(I)
            CALL OUTFMT(PLSTR2(I,J,1),2,AUX2,NC2,'LEFT')
            CALL OUTFMT(PLSTR2(I,J,2),2,AUX3,NC3,'LEFT')
            CALL OUTFMT(PLSTR2(I,J,3),2,AUX4,NC4,'LEFT')
            IF(PSLAB2(I,J).EQ.'?')THEN
                 AUX5=' not labeled'
                 NC5=12
            ELSE
                 AUX5=' label = '//PSLAB2(I,J)
                 NC5=10
            ENDIF
            WRITE(LUNOUT,'(8X,A)') AUX2(1:NC2)//' < z < '//
     -           AUX3(1:NC3)//' cm, gap = '//AUX4(1:NC4)//' cm,'//
     -           AUX5(1:NC5)
100         CONTINUE
30          CONTINUE
       ENDIF
*** Print the type of periodicity, first header and x direction.
       IF(NMAP.LT.1)THEN
            WRITE(LUNOUT,'(/''  PERIODICITY'')')
            IF(PERX.AND.POLAR)THEN
                 CALL OUTFMT(EXP(SX),2,AUX1,NC1,'LEFT')
                 WRITE(LUNOUT,'(/''  The cell is repeated every '',A,
     -                '' cm in r.'')') AUX1(1:NC1)
            ELSEIF(PERMX.AND.POLAR)THEN
                 CALL OUTFMT(EXP(SX),2,AUX1,NC1,'LEFT')
                 WRITE(LUNOUT,'(/''  The cell has mirror periodicity'',
     -                '' in r with a length of '',A,'' cm.'')')
     -                AUX1(1:NC1)
            ELSEIF(PERX)THEN
                 CALL OUTFMT(SX,2,AUX1,NC1,'LEFT')
                 WRITE(LUNOUT,'(/''  The cell is repeated every '',A,
     -                '' cm in x.'')') AUX1(1:NC1)
            ELSEIF(PERMX)THEN
                 CALL OUTFMT(SX,2,AUX1,NC1,'LEFT')
                 WRITE(LUNOUT,'(/''  The cell has mirror periodicity'',
     -                '' in x with a length of '',A,'' cm.'')')
     -                AUX1(1:NC1)
            ELSEIF(POLAR)THEN
                 WRITE(LUNOUT,'(/''  The cell is not periodic in r.'')')
            ELSE
                 WRITE(LUNOUT,'(/''  The cell has no translation'',
     -                '' periodicity in x.'')')
            ENDIF
            IF(PERAX)THEN
                 CALL OUTFMT((XAMAX-XAMIN)*180/PI,2,AUX1,NC1,'LEFT')
                 WRITE(LUNOUT,'(''  The cell has axial periodicity'',
     -                '' around x with a length of '',A,
     -                '' degrees.'')') AUX1(1:NC1)
            ELSEIF(PERRX)THEN
                 WRITE(LUNOUT,'(''  The cell is rotationally'',
     -                '' symmetric around the x-axis.'')')
            ELSE
                 WRITE(LUNOUT,'(''  The cell has no axial'',
     -                '' periodicity around the x axis.'')')
            ENDIF
*   In y.
            IF(PERY.AND.(POLAR.OR.TUBE))THEN
                 CALL OUTFMT(180*SY/PI,2,AUX1,NC1,'LEFT')
                 WRITE(LUNOUT,'(''  The cell is repeated in phi'',
     -                '' every '',A,'' degrees.'')') AUX1(1:NC1)
            ELSEIF(PERMY.AND.(POLAR.OR.TUBE))THEN
                 CALL OUTFMT(180*SY/PI,2,AUX1,NC1,'LEFT')
                 WRITE(LUNOUT,'(''  The cell has mirror periodicity'',
     -                '' in phi with a length of '',A,'' degrees.'')')
     -                AUX1(1:NC1)
            ELSEIF(PERY)THEN
                 CALL OUTFMT(SY,2,AUX1,NC1,'LEFT')
                 WRITE(LUNOUT,'(''  The cell is repeated every '',A,
     -                '' cm in y.'')') AUX1(1:NC1)
            ELSEIF(PERMY)THEN
                 CALL OUTFMT(SY,2,AUX1,NC1,'LEFT')
                 WRITE(LUNOUT,'(''  The cell has mirror periodicity'',
     -                '' in y with a length of '',A,'' cm.'')')
     -                AUX1(1:NC1)
            ELSEIF(POLAR)THEN
                 WRITE(LUNOUT,'(''  The cell is not periodic in'',
     -                '' phi.'')')
            ELSE
                 WRITE(LUNOUT,'(''  The cell has no translation'',
     -                '' periodicity in y.'')')
            ENDIF
            IF(PERAY)THEN
                 CALL OUTFMT((YAMAX-YAMIN)*180/PI,2,AUX1,NC1,'LEFT')
                 WRITE(LUNOUT,'(''  The cell has axial periodicity'',
     -                '' around y with a length of '',A,'' degrees.'')')
     -                AUX1(1:NC1)
            ELSEIF(PERRY)THEN
                 WRITE(LUNOUT,'(''  The cell is rotationally'',
     -                '' symmetric around the y-axis.'')')
            ELSE
                 WRITE(LUNOUT,'(''  The cell has no axial'',
     -                '' periodicity around the y axis.'')')
            ENDIF
*   In z.
            IF(PERZ)THEN
                 CALL OUTFMT(SZ,2,AUX1,NC1,'LEFT')
                 WRITE(LUNOUT,'(''  The cell is repeated every '',A,
     -                '' cm in z.'')') AUX1(1:NC1)
            ELSEIF(PERMZ)THEN
                 CALL OUTFMT(SZ,2,AUX1,NC1,'LEFT')
                 WRITE(LUNOUT,'(''  The cell has mirror periodicity'',
     -                '' in z with a length of '',A,'' cm.'')')
     -                AUX1(1:NC1)
            ELSE
                 WRITE(LUNOUT,'(''  The cell has no translation'',
     -                '' periodicity in z.'')')
            ENDIF
            IF(PERAZ)THEN
                 CALL OUTFMT((ZAMAX-ZAMIN)*180/PI,2,AUX1,NC1,'LEFT')
                 WRITE(LUNOUT,'(''  The cell has axial periodicity'',
     -                '' around z with a length of '',A,'' degrees.'')')
     -                AUX1(1:NC1)
            ELSEIF(PERRZ)THEN
                 WRITE(LUNOUT,'(''  The cell is rotationally'',
     -                '' symmetric around the z-axis.'')')
            ELSE
                 WRITE(LUNOUT,'(''  The cell has no axial'',
     -                '' periodicity around the z axis.'')')
            ENDIF
       ENDIF
*** List the dielectrica.
       IF(NXMATT.NE.0.OR.NYMATT.NE.0)THEN
            WRITE(LUNOUT,'(/''  LIST OF DIELECTRICA''//
     -        ''  Direction         From            To       Epsilon''/
     -        ''                    [cm]          [cm]    [relative]''/
     -        )')
            DO 50 I=1,NXMATT
            IF(XMATT(I,3).NE.0)THEN
                 AUX1='    -infinity'
            ELSE
                 CALL OUTFMT(XMATT(I,1),2,AUX1,NCAUX,'RIGHT')
            ENDIF
            IF(XMATT(I,4).NE.0)THEN
                 AUX2='    +infinity'
            ELSE
                 CALL OUTFMT(XMATT(I,2),2,AUX2,NCAUX,'RIGHT')
            ENDIF
            CALL OUTFMT(XMATT(I,5),2,AUX3,NCAUX,'RIGHT')
            WRITE(LUNOUT,'(10X,A1,A13,1X,A13,1X,A13)')
     -           'x',AUX1,AUX2,AUX3
50          CONTINUE
            DO 60 I=1,NYMATT
            IF(YMATT(I,3).NE.0)THEN
                 AUX1='    -infinity'
            ELSE
                 CALL OUTFMT(YMATT(I,1),2,AUX1,NCAUX,'RIGHT')
            ENDIF
            IF(YMATT(I,4).NE.0)THEN
                 AUX2='    +infinity'
            ELSE
                 CALL OUTFMT(YMATT(I,2),2,AUX2,NCAUX,'RIGHT')
            ENDIF
            CALL OUTFMT(YMATT(I,5),2,AUX3,NCAUX,'RIGHT')
            WRITE(LUNOUT,'(10X,A1,A13,1X,A13,1X,A13)')
     -           'y',AUX1,AUX2,AUX3
60          CONTINUE
       ENDIF
*** Print cell size, type and various other things.
       WRITE(LUNOUT,'(/''  OTHER DATA'')')
       CALL OUTFMT(DOWN(1),2,AUX1,NC1,'LEFT')
       CALL OUTFMT(DOWN(2),2,AUX2,NC2,'LEFT')
       CALL OUTFMT(DOWN(3),2,AUX3,NC3,'LEFT')
       WRITE(LUNOUT,'(/''  Gravity vector: ('',A,'','',A,'','',A,
     -      '') g.'')') AUX1(1:NC1),AUX2(1:NC2),AUX3(1:NC3)
       CALL OUTFMT(VMIN,2,AUX7,NC7,'RIGHT')
       CALL OUTFMT(VMAX,2,AUX8,NC8,'LEFT')
       IF(.NOT.POLAR)THEN
            CALL OUTFMT(XMIN,2,AUX1,NC1,'RIGHT')
            CALL OUTFMT(XMAX,2,AUX2,NC2,'LEFT')
            CALL OUTFMT(YMIN,2,AUX3,NC3,'RIGHT')
            CALL OUTFMT(YMAX,2,AUX4,NC4,'LEFT')
            CALL OUTFMT(ZMIN,2,AUX5,NC5,'RIGHT')
            CALL OUTFMT(ZMAX,2,AUX6,NC6,'LEFT')
            LMIN=LEN(AUX1)-MAX(NC1,NC3,NC5,NC7)+1
            RMAX=MAX(NC2,NC4,NC6,NC8)
            WRITE(LUNOUT,'(/''  Cell dimensions: '',
     -           A,'' < x < '',A,'' cm,''/19X,
     -           A,'' < y < '',A,'' cm,''/19X,
     -           A,'' < z < '',A,'' cm.''//
     -           ''  Potential range: '',
     -           A,'' < V < '',A,'' V.'')')
     -           AUX1(LMIN:),AUX2(1:RMAX),AUX3(LMIN:),AUX4(1:RMAX),
     -           AUX5(LMIN:),AUX6(1:RMAX),AUX7(LMIN:),AUX8(1:RMAX)
       ELSE
            CALL CFMRTP(XMIN,YMIN,XMINP,YMINP,1)
            CALL CFMRTP(XMAX,YMAX,XMAXP,YMAXP,1)
            CALL OUTFMT(XMINP,2,AUX1,NC1,'RIGHT')
            CALL OUTFMT(XMAXP,2,AUX2,NC2,'LEFT')
            CALL OUTFMT(YMINP,2,AUX3,NC3,'RIGHT')
            CALL OUTFMT(YMAXP,2,AUX4,NC4,'LEFT')
            CALL OUTFMT(ZMIN,2,AUX5,NC5,'RIGHT')
            CALL OUTFMT(ZMAX,2,AUX6,NC6,'LEFT')
            LMIN=LEN(AUX1)-MAX(NC1,NC3,NC5,NC7)+1
            RMAX=MAX(NC2,NC4,NC6,NC8)
            WRITE(LUNOUT,'(/''  Cell dimensions: '',
     -           A,'' <  r  < '',A,'' cm,''/19X,
     -           A,'' < phi < '',A,'' degrees,''/19X,
     -           A,'' <  z  < '',A,'' cm.''//
     -           ''  Potential range: '',
     -           A,'' <  V  < '',A,'' V.'')')
     -           AUX1(LMIN:),AUX2(1:RMAX),AUX3(LMIN:),AUX4(1:RMAX),
     -           AUX5(LMIN:),AUX6(1:RMAX),AUX7(LMIN:),AUX8(1:RMAX)
       ENDIF
       WRITE(LUNOUT,'(/''  The cell is of type '',A3,'' (code '',I2,
     -      '', details can be found in the writeup.)'')') TYPE,ICTYPE
*   Print voltage shift in case no equipotential planes are present,
       IF(.NOT.(YNPLAN(1).OR.YNPLAN(2).OR.
     -      YNPLAN(3).OR.YNPLAN(4).OR.TUBE))THEN
            CALL OUTFMT(V0,2,AUX1,NC1,'LEFT')
            WRITE(LUNOUT,'(/''  All voltages have been shifted by '',
     -           A,'' V to avoid net wire charge.'')') AUX1(1:NC1)
       ELSE
*   else print the net charge on the wires.
            SUMCH=0.0
            DO 40 I=1,NWIRE
            SUMCH=SUMCH+E(I)
40          CONTINUE
            CALL OUTFMT(2.0E12*PI*EPS0*SUMCH,2,AUX1,NC1,'LEFT')
            WRITE(LUNOUT,'(/''  The net charge on the wires is '',A,
     -           '' pC/cm.'')') AUX1(1:NC1)
       ENDIF
*** Register the amount of CPU time used.
       CALL TIMLOG('Printing the cell properties:           ')
       END
