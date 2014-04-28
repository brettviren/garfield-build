CDECK  ID>, MAPPRT.
       SUBROUTINE MAPPRT
*-----------------------------------------------------------------------
*   MAPPRT - Prints a field map overview.
*   (Last changed on 24/11/13.)
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
       INTEGER I,NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,LMIN,RMAX
       CHARACTER*20 AUX1,AUX2,AUX3,AUX4,AUX5,AUX6,AUX7,AUX8
*** Make sure there is a field map.
       IF(NMAP.LE.1)THEN
            WRITE(LUNOUT,'(/''  There is currently no field'',
     -           '' map.'')')
            RETURN
       ENDIF
*** Print the elements that are present.
       CALL OUTFMT(REAL(NMAP),2,AUX1,NC1,'LEFT')
       IF(MAPTYP.EQ.1.OR.MAPTYP.EQ.2.OR.MAPTYP.EQ.3)THEN
            WRITE(LUNOUT,'(/''  The field is taken from a field map'',
     -           '' of '',A,'' triangles,''/''  at the vertices'',
     -           '' of which the following are known:'')') AUX1(1:NC1)
       ELSEIF(MAPTYP.EQ.4.OR.MAPTYP.EQ.5)THEN
            WRITE(LUNOUT,'(/''  The field is taken from a field map'',
     -           '' of '',A,'' quadrilaterals,''/''  at the vertices'',
     -           '' of which the following are known:'')') AUX1(1:NC1)
       ELSEIF(MAPTYP.EQ.11.OR.MAPTYP.EQ.12.OR.MAPTYP.EQ.13)THEN
            WRITE(LUNOUT,'(/''  The field is taken from a field map'',
     -           '' of '',A,'' tetrahedrons,''/''  at the vertices'',
     -           '' of which the following are known:'')') AUX1(1:NC1)
       ELSEIF(MAPTYP.EQ.14.OR.MAPTYP.EQ.15.OR.MAPTYP.EQ.16)THEN
            WRITE(LUNOUT,'(/''  The field is taken from a field map'',
     -           '' of '',A,'' hexahedra,''/
     -           ''  at the vertices of which the following'',
     -           '' are known:'')') AUX1(1:NC1)
       ELSE
            WRITE(LUNOUT,'(/''  The field is taken from a field map'',
     -           '' of '',I5,'' elements of unknown type,''/
     -           ''  at the vertices of which the following''/
     -           '' are known:'')') NMAP
       ENDIF
       IF(MAPFLG(2))WRITE(LUNOUT,'(''  - x-component of the'',
     -      '' electric field'')')
       IF(MAPFLG(3))WRITE(LUNOUT,'(''  - y-component of the'',
     -      '' electric field'')')
       IF(MAPFLG(4))WRITE(LUNOUT,'(''  - z-component of the'',
     -      '' electric field'')')
       IF(MAPFLG(5))WRITE(LUNOUT,'(''  - electrostatic'',
     -      '' potential'')')
       IF(MAPFLG(6))WRITE(LUNOUT,'(''  - x-component of the'',
     -      '' magnetic field'')')
       IF(MAPFLG(7))WRITE(LUNOUT,'(''  - y-component of the'',
     -      '' magnetic field'')')
       IF(MAPFLG(8))WRITE(LUNOUT,'(''  - z-component of the'',
     -      '' magnetic field'')')
       IF(MAPFLG(9))WRITE(LUNOUT,'(''  - dielectric constants'',
     -      '' of the materials'')')
       DO 10 I=1,NWMAP
       IF(MAPFLG(10+4*I-3))WRITE(LUNOUT,'(''  - x-component of a'',
     -      '' weighting field for solids with label '',A1)') EWSTYP(I)
       IF(MAPFLG(11+4*I-3))WRITE(LUNOUT,'(''  - y-component of a'',
     -      '' weighting field for solids with label '',A1)') EWSTYP(I)
       IF(MAPFLG(12+4*I-3))WRITE(LUNOUT,'(''  - z-component of a'',
     -      '' weighting field for solids with label '',A1)') EWSTYP(I)
       IF(MAPFLG(13+4*I-3))WRITE(LUNOUT,'(''  - weighting potential'',
     -      '' for solids with label '',A1)') EWSTYP(I)
10     CONTINUE
*** Print the ranges and periodicities.
       CALL OUTFMT(XMMIN,2,AUX1,NC1,'RIGHT')
       CALL OUTFMT(XMMAX,2,AUX2,NC2,'LEFT')
       CALL OUTFMT(YMMIN,2,AUX3,NC3,'RIGHT')
       CALL OUTFMT(YMMAX,2,AUX4,NC4,'LEFT')
       CALL OUTFMT(ZMMIN,2,AUX5,NC5,'RIGHT')
       CALL OUTFMT(ZMMAX,2,AUX6,NC6,'LEFT')
       IF(MAPFLG(5))THEN
            CALL OUTFMT(VMMIN,2,AUX7,NC7,'RIGHT')
            CALL OUTFMT(VMMAX,2,AUX8,NC8,'LEFT')
            LMIN=LEN(AUX1)-MAX(NC1,NC3,NC5,NC7)+1
            RMAX=MAX(NC2,NC4,NC6,NC8)
       ELSE
            LMIN=LEN(AUX1)-MAX(NC1,NC3,NC5)+1
            RMAX=MAX(NC2,NC4,NC6)
       ENDIF
       WRITE(LUNOUT,'(/''  Grid dimensions: '',
     -      A,'' < x < '',A,'' cm,''/19X,
     -      A,'' < y < '',A,'' cm,''/19X,
     -      A,'' < z < '',A,'' cm.'')')
     -      AUX1(LMIN:),AUX2(1:RMAX),AUX3(LMIN:),AUX4(1:RMAX),
     -      AUX5(LMIN:),AUX6(1:RMAX)
       IF(MAPFLG(5))WRITE(LUNOUT,'(''  Potential range: '',
     -           A,'' < V < '',A,'' V.'')') AUX7(LMIN:),AUX8(1:RMAX)
       IF(PERX)THEN
            CALL OUTFMT(SX,2,AUX1,NC1,'LEFT')
            WRITE(LUNOUT,'(/''  The cell is repeated in x, the'',
     -           '' length of a period is '',A,'' cm.'')') AUX1(1:NC1)
       ELSEIF(PERMX)THEN
            CALL OUTFMT(SX,2,AUX1,NC1,'LEFT')
            WRITE(LUNOUT,'(/''  The cell has mirror periodicity'',
     -           '' in x with a length of '',A,'' cm.'')') AUX1(1:NC1)
       ELSE
            WRITE(LUNOUT,'(/''  The cell has no translation'',
     -           '' periodicity in x.'')')
       ENDIF
       IF(PERAX)THEN
            CALL OUTFMT((XAMAX-XAMIN)*180/PI,2,AUX1,NC1,'LEFT')
            WRITE(LUNOUT,'(''  The cell has axial periodicity'',
     -           '' around the x-axis of '',A,'' degrees.'')')
     -           AUX1(1:NC1)
       ELSEIF(PERRX)THEN
            WRITE(LUNOUT,'(''  The cell is rotationally'',
     -           '' symmetric around the x-axis.'')')
       ELSE
            WRITE(LUNOUT,'(''  The cell has no axial periodicity'',
     -           '' around the x-axis.'')')
       ENDIF
*   In y.
       IF(PERY)THEN
            CALL OUTFMT(SY,2,AUX1,NC1,'LEFT')
            WRITE(LUNOUT,'(''  The cell is repeated in y, the'',
     -           '' length of a period is '',A,'' cm.'')') AUX1(1:NC1)
       ELSEIF(PERMY)THEN
            CALL OUTFMT(SY,2,AUX1,NC1,'LEFT')
            WRITE(LUNOUT,'(''  The cell has mirror periodicity'',
     -           '' in y with a length of '',A,'' cm.'')') AUX1(1:NC1)
       ELSE
            WRITE(LUNOUT,'(''  The cell has no translation'',
     -           '' periodicity in y.'')')
       ENDIF
       IF(PERAY)THEN
           CALL OUTFMT((YAMAX-YAMIN)*180/PI,2,AUX1,NC1,'LEFT')
           WRITE(LUNOUT,'(''  The cell has axial periodicity'',
     -           '' around the y-axis of '',A,'' degrees.'')')
     -           AUX1(1:NC1)
       ELSEIF(PERRY)THEN
            WRITE(LUNOUT,'(''  The cell is rotationally'',
     -           '' symmetric around the y-axis.'')')
       ELSE
            WRITE(LUNOUT,'(''  The cell has no axial periodicity'',
     -           '' around the y-axis.'')')
       ENDIF
*   In z.
       IF(PERZ)THEN
            CALL OUTFMT(SZ,2,AUX1,NC1,'LEFT')
            WRITE(LUNOUT,'(''  The cell is repeated in z, the'',
     -           '' length of a period is '',A,'' cm.'')') AUX1(1:NC1)
       ELSEIF(PERMZ)THEN
            CALL OUTFMT(SZ,2,AUX1,NC1,'LEFT')
            WRITE(LUNOUT,'(''  The cell has mirror periodicity'',
     -           '' in z with a length of '',A,'' cm.'')') AUX1(1:NC1)
       ELSE
            WRITE(LUNOUT,'(''  The cell has no translation'',
     -           '' periodicity in z.'')')
       ENDIF
       IF(PERAZ)THEN
            CALL OUTFMT((ZAMAX-ZAMIN)*180/PI,2,AUX1,NC1,'LEFT')
            WRITE(LUNOUT,'(''  The cell has axial periodicity'',
     -           '' around the z-axis of '',A,'' degrees.'')')
     -           AUX1(1:NC1)
       ELSEIF(PERRZ)THEN
            WRITE(LUNOUT,'(''  The cell is rotationally'',
     -           '' symmetric around the z-axis.'')')
       ELSE
            WRITE(LUNOUT,'(''  The cell has no axial periodicity'',
     -           '' around the z-axis.'')')
       ENDIF
*** List the materials.
       IF(NEPS.GE.1)THEN
            IF(MAPTYP.GT.10.AND.MATSRC.EQ.'SIGMA')THEN
                 WRITE(LUNOUT,'(/''  There are '',I5,'' materials'',
     -                '' which differ by conducivity: ''/''  Index'',
     -                ''         Sigma [S/m]        Volume [cm3]'')')
     -                NEPS
            ELSEIF(MAPTYP.GT.10)THEN
                 WRITE(LUNOUT,'(/''  There are '',I5,'' materials'',
     -                '' which differ by dielectric constant: ''/
     -                ''  Index'',
     -                ''             Epsilon        Volume [cm3]'')')
     -                NEPS
            ELSEIF(MATSRC.EQ.'SIGMA')THEN
                 WRITE(LUNOUT,'(/''  There are '',I5,'' materials'',
     -                '' which differ by conducivity: ''/''  Index'',
     -                ''         Sigma [S/m]       Surface [cm2]'')')
     -                NEPS
            ELSE
                 WRITE(LUNOUT,'(/''  There are '',I5,'' materials'',
     -                '' which differ by dielectric constant: ''/
     -                ''  Index'',
     -                ''             Epsilon       Surface [cm2]'')')
     -                NEPS
            ENDIF
            DO 70 I=1,NEPS
            CALL OUTFMT(EPSMAT(I),2,AUX1,NC1,'RIGHT')
            CALL OUTFMT(EPSSUR(I),2,AUX2,NC2,'RIGHT')
            IF(MATSRC.EQ.'SIGMA')THEN
                 IF(I.EQ.IDRMAT)THEN
                      WRITE(LUNOUT,'(''  '',I5,2A20,
     -                     '' (drift medium)'')') I,AUX1,AUX2
                 ELSE
                      WRITE(LUNOUT,'(''  '',I5,2A20)') I,AUX1,AUX2
                 ENDIF
            ELSE
                 IF(I.EQ.IDRMAT)THEN
                      WRITE(LUNOUT,'(''  '',I5,2A20,
     -                     '' (drift medium)'')') I,AUX1,AUX2
                 ELSE
                      WRITE(LUNOUT,'(''  '',I5,2A20)') I,AUX1,AUX2
                 ENDIF
            ENDIF
70          CONTINUE
       ELSE
            WRITE(LUNOUT,'(/''  No material properties available.'')')
       ENDIF
*** Print the interpolation order.
       IF(MAPORD.EQ.1)THEN
            WRITE(LUNOUT,'(/''  The field maps will be interpolated'',
     -           '' linearly.'')')
       ELSEIF(MAPORD.EQ.2)THEN
            WRITE(LUNOUT,'(/''  The field maps will be interpolated'',
     -           '' quadratically.'')')
       ELSE
            WRITE(LUNOUT,'(/''  The field maps will be interpolated'',
     -           '' to order '',I2,''.'')') MAPORD
       ENDIF
*** Print the state of the E field derivatives flag.
       IF(LSFDER)THEN
            WRITE(LUNOUT,'(/''  The field components are computed by'',
     -           '' taking derivatives of the potential.'')')
       ELSE
            WRITE(LUNOUT,'(/''  The field components are computed by'',
     -           '' interpolation in the field map.'')')
       ENDIF
       END
