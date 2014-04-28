CDECK  ID>, MAPFM2.
       SUBROUTINE MAPFM2(FMAP,NCMAP,IDATA,IWMAP,IFAIL)
*-----------------------------------------------------------------------
*   MAPFM2 - Reads a Maxwell 2D table of triangles.
*   (Last changed on 26/10/07.)
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
       INTEGER IMAP,IEPS,ICONT(3),IMAX,IWMAP,
     -      I,J,IREAD,NCMAP,IFAIL,IFAIL1,IOS,NC,
     -      INPCMP,IDATA,NDECL
       REAL TEMPRE,TEMPIM,XAUX(3),YAUX(3),ZAUX(3),SUM,
     -      T1,T2,T3,T4,ECOMP,DCOMP,DXCOMP,DYCOMP
       DOUBLE PRECISION JAC(4,4),DET
       CHARACTER*(*) FMAP
       CHARACTER*80 STRING
       CHARACTER*8 STRAUX
       LOGICAL SCALAR,NEWEPS
       EXTERNAL INPCMP
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MAPFM2 ///'
*** Assume that this will fail.
       IFAIL=1
*** First read the line with number of triangles.
       CALL INPCHK(5,1,IFAIL1)
       CALL INPRDI(5,NDECL,0)
       IF(IFAIL1.NE.0.OR.NDECL.LE.0)THEN
            PRINT *,' !!!!!! MAPFM2 WARNING : The file ',
     -           FMAP(1:NCMAP),' has an unreadable number'//
     -           ' of triangles; not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM2 DEBUG   : Number'',
     -      '' of triangles: '',I5)') NDECL
*** Progress printing.
       CALL PROFLD(2,'Triangles',REAL(NDECL))
*** See whether the data is scalar or vector.
       IF(INPCMP(2,'SCALAR').NE.0)THEN
            SCALAR=.TRUE.
       ELSEIF(INPCMP(2,'VECTOR').NE.0)THEN
            SCALAR=.FALSE.
       ELSE
            PRINT *,' !!!!!! MAPFM2 WARNING : The file ',
     -           FMAP(1:NCMAP),' contains neither scalar nor'//
     -           ' vectorial data; not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*** Next determine the contents of the file, read the next record.
       CALL INPGET
*   Set the expected word count.
       IF(SCALAR)THEN
            IMAX=1
       ELSE
            IMAX=3
       ENDIF
*   Initial contents flags.
       ICONT(1)=0
       ICONT(2)=0
       ICONT(3)=0
       NEWEPS=.FALSE.
*   Loop over the words.
       DO 40 I=1,IMAX
*   Ex or EWx or Er or EWr.
       IF(INPCMP(I,'smooth(E(x))')+INPCMP(I,'E(x)')+
     -      INPCMP(I,'smooth(E(r))')+INPCMP(I,'E(r)').NE.0)THEN
            ICONT(I)=2
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM2 DEBUG   :'',
     -           '' Field '',I1,'': x/r-component E field.'')') I
            IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                 IF(MAPFLG(2))PRINT *,' ------ MAPFM2 MESSAGE :'//
     -                ' Overwriting current Ex/r map.'
                 MAPFLG(2)=.FALSE.
            ELSEIF(IDATA.EQ.10)THEN
                 IF(MAPFLG(10+4*IWMAP-3))
     -                PRINT *,' ------ MAPFM2 MESSAGE :'//
     -                ' Overwriting current weighting Ex/r map.'
                 MAPFLG(10+4*IWMAP-3)=.FALSE.
            ENDIF
            IF(INPCMP(I,'smooth(E(r))')+INPCMP(I,'E(r)').NE.0)
     -           PERRZ=.TRUE.
*   Ey or EWy.
       ELSEIF(INPCMP(I,'smooth(E(y))')+INPCMP(I,'E(y)').NE.0)THEN
            ICONT(I)=3
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM2 DEBUG   :'',
     -           '' Field '',I1,'': y-component E field.'')') I
            IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                 IF(MAPFLG(3))PRINT *,' ------ MAPFM2 MESSAGE :'//
     -                ' Overwriting current Ey map.'
                 MAPFLG(3)=.FALSE.
            ELSEIF(IDATA.EQ.10)THEN
                 IF(MAPFLG(11+4*IWMAP-3))
     -                PRINT *,' ------ MAPFM2 MESSAGE :'//
     -                ' Overwriting current weighting Ey map.'
                 MAPFLG(11+4*IWMAP-3)=.FALSE.
            ENDIF
*   Ez or EWz.
       ELSEIF(INPCMP(I,'smooth(E(z))')+INPCMP(I,'E(z)').NE.0)THEN
            ICONT(I)=4
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM2 DEBUG   :'',
     -           '' Field '',I1,'': z-component E field.'')') I
            IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                 IF(MAPFLG(4))PRINT *,' ------ MAPFM2 MESSAGE :'//
     -                ' Overwriting current Ez map.'
                 MAPFLG(4)=.FALSE.
            ELSEIF(IDATA.EQ.10)THEN
                 IF(MAPFLG(12+4*IWMAP-3))
     -                PRINT *,' ------ MAPFM2 MESSAGE :'//
     -                ' Overwriting current weighting Ez map.'
                 MAPFLG(12+4*IWMAP-3)=.FALSE.
            ENDIF
*   Dx or Dr.
       ELSEIF(INPCMP(I,'smooth(D(x))')+INPCMP(I,'D(x)')+
     -      INPCMP(I,'smooth(D(r))')+INPCMP(I,'D(r)').NE.0)THEN
            ICONT(I)=-9
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM2 DEBUG   :'',
     -           '' Field '',I1,'': x-component D field.'')') I
            IF(MAPFLG(9))PRINT *,' ------ MAPFM2 MESSAGE :'//
     -           ' Overwriting current material map.'
            MAPFLG(9)=.FALSE.
            MATSRC='EPSILON'
*   Dy.
       ELSEIF(INPCMP(I,'smooth(D(y))')+INPCMP(I,'D(y)').NE.0)THEN
            ICONT(I)=-9
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM2 DEBUG   :'',
     -           '' Field '',I1,'': y-component D field.'')') I
            IF(MAPFLG(9))PRINT *,' ------ MAPFM2 MESSAGE :'//
     -           ' Overwriting current material map.'
            MAPFLG(9)=.FALSE.
            MATSRC='EPSILON'
*   Dz.
       ELSEIF(INPCMP(I,'smooth(D(z))')+INPCMP(I,'D(z)').NE.0)THEN
            ICONT(I)=-9
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM2 DEBUG   :'',
     -           '' Field '',I1,'': z-component D field.'')') I
            IF(MAPFLG(9))PRINT *,' ------ MAPFM2 MESSAGE :'//
     -           ' Overwriting current material map.'
            MAPFLG(9)=.FALSE.
            MATSRC='EPSILON'
*   V
       ELSEIF(INPCMP(I,'smooth(voltage)')+INPCMP(I,'voltage').NE.0)THEN
            ICONT(I)=5
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM2 DEBUG   :'',
     -           '' Field '',I1,'': potential.'')') I
            IF(MAPFLG(5))PRINT *,' ------ MAPFM2 MESSAGE :'//
     -           ' Overwriting current potential map.'
            MAPFLG(5)=.FALSE.
*   Bx
       ELSEIF(INPCMP(I,'smooth(B(x))')+INPCMP(I,'B(x)')+
     -      INPCMP(I,'smooth(B(r))')+INPCMP(I,'B(r)').NE.0)THEN
            ICONT(I)=6
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM2 DEBUG   :'',
     -           '' Field '',I1,'': x/r-component B field.'')') I
            IF(MAPFLG(6))PRINT *,' ------ MAPFM2 MESSAGE :'//
     -           ' Overwriting current Bx/r map.'
            MAPFLG(6)=.FALSE.
            IF(INPCMP(I,'smooth(B(r))')+INPCMP(I,'B(r)').NE.0)
     -           PERRZ=.TRUE.
*   By
       ELSEIF(INPCMP(I,'smooth(B(y))')+INPCMP(I,'B(y)').NE.0)THEN
            ICONT(I)=7
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM2 DEBUG   :'',
     -           '' Field '',I1,'': y-component B field.'')') I
            IF(MAPFLG(7))PRINT *,' ------ MAPFM2 MESSAGE :'//
     -           ' Overwriting current By map.'
            MAPFLG(7)=.FALSE.
*   Bz
       ELSEIF(INPCMP(I,'smooth(B(z))')+INPCMP(I,'B(z)').NE.0)THEN
            ICONT(I)=8
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM2 DEBUG   :'',
     -           '' Field '',I1,'': z-component B field.'')') I
            IF(MAPFLG(8))PRINT *,' ------ MAPFM2 MESSAGE :'//
     -           ' Overwriting current Bz map.'
            MAPFLG(8)=.FALSE.
*   epsilon
       ELSEIF(INPCMP(I,'(r(    1.00000e+00) * epsilon)')+
     -      INPCMP(I,'(r(   1.00000e+000) * epsilon)').NE.0)THEN
            ICONT(I)=9
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM2 DEBUG   :'',
     -           '' Field '',I1,'': dielectric constant.'')') I
            IF(MAPFLG(9))PRINT *,' ------ MAPFM2 MESSAGE :'//
     -           ' Overwriting material map.'
            MAPFLG(9)=.FALSE.
            MATSRC='EPSILON'
*   sigma.
       ELSEIF(INPCMP(I,'(r(    1.00000e+00) * sigma)')+
     -      INPCMP(I,'(r(   1.00000e+000) * sigma)').NE.0)THEN
            ICONT(I)=9
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM2 DEBUG   :'',
     -           '' Field '',I1,'': Conductivity.'')') I
            IF(MAPFLG(9))PRINT *,' ------ MAPFM2 MESSAGE :'//
     -           ' Overwriting material map.'
            MAPFLG(9)=.FALSE.
            MATSRC='SIGMA'
*   dummy field
       ELSEIF(INPCMP(I,'smooth(0)')+INPCMP(I,'0')+
     -      INPCMP(I,'r(    0.00000e+00)')+
     -      INPCMP(I,'(r(    1.00000e+00) *  )')+
     -      INPCMP(I,'(r(   0.00000e+000) * epsilon)')+
     -      INPCMP(I,'(r(   0.00000e+00) * epsilon)')+
     -      INPCMP(I,'(r(   0.00000e+000) * sigma)')+
     -      INPCMP(I,'(r(   0.00000e+00) * sigma)').NE.0)THEN
            ICONT(I)=0
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM2 DEBUG   :'',
     -           '' Field '',I1,'': dummy.'')') I
*   unrecognised items.
       ELSE
            CALL INPSTR(I,I,STRING,NC)
            PRINT *,' !!!!!! MAPFM2 WARNING : The file ',
     -           FMAP(1:NCMAP),' contains a "'//STRING(1:NC)//
     -           '" field which is not known; field ignored.'
            ICONT(I)=0
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM2 DEBUG   :'',
     -           '' Field '',I1,'': not recognised.'')') I
       ENDIF
*   Ensure that the data type matches the declared type.
       IF(((ICONT(I).EQ.2.OR.ICONT(I).EQ.3.OR.ICONT(I).EQ.4).AND.
     -      (IDATA.NE.0.AND.IDATA.NE.2.AND.IDATA.NE.10)).OR.
     -      (ICONT(I).EQ.5.AND.(IDATA.NE.0.AND.IDATA.NE.5)).OR.
     -      ((ICONT(I).EQ.6.OR.ICONT(I).EQ.7.OR.ICONT(I).EQ.8).AND.
     -      (IDATA.NE.0.AND.IDATA.NE.6)).OR.
     -      ((ICONT(I).EQ.9.OR.ICONT(I).EQ.-9).AND.
     -      (IDATA.NE.0.AND.IDATA.NE.9)))THEN
            PRINT *,' !!!!!! MAPFM2 WARNING : Field ',I,' of file ',
     -           FMAP(1:NCMAP),' does not contain the declared',
     -           ' kind of data; skipped.'
            ICONT(I)=0
       ENDIF
40     CONTINUE
*** Switch back to regular input.
       CALL INPSWI('RESTORE')
*** Loop over the triangles.
       I=0
       DO 10 IREAD=1,NDECL
       IF(IREAD.EQ.MAX(1,NDECL/100)*(IREAD/MAX(1,NDECL/100)))
     -      CALL PROSTA(2,REAL(IREAD))
*** Increment triangle count, checking there is space.
       IF(I+1.GT.MXMAP)THEN
            PRINT *,' !!!!!! MAPFM2 WARNING : Number of'//
     -           ' triangles in ',FMAP(1:NCMAP),
     -           ' exceeds compilation limit; file not read.'
            RETURN
       ELSE
            I=I+1
       ENDIF
*** Skip until the line with the word "Vertices".
21     CONTINUE
       READ(12,'(A8)',END=2000,ERR=2010,IOSTAT=IOS) STRAUX
       IF(STRAUX.NE.'Vertices')GOTO 21
*** Read vertex coordinates.
       DO 20 J=1,3
*   If the grid is already defined, merely store for check.
       IF(MAPFLG(1))THEN
            READ(12,'(E27.20,1X,E27.20,1X,E27.7)',END=2000,ERR=2010,
     -           IOSTAT=IOS) XAUX(J),YAUX(J),ZAUX(J)
            XAUX(J)=XAUX(J)*100
            YAUX(J)=YAUX(J)*100
            ZAUX(J)=ZAUX(J)*100
*   Otherwise store the grid, converting units from m to cm.
       ELSE
            READ(12,'(E27.20,1X,E27.20,1X,E27.7)',END=2000,ERR=2010,
     -           IOSTAT=IOS) XMAP(I,J),YMAP(I,J),ZMAP(I,J)
            XMAP(I,J)=XMAP(I,J)*100
            YMAP(I,J)=YMAP(I,J)*100
            ZMAP(I,J)=ZMAP(I,J)*100
*   Update maxima and minima.
            IF(I.EQ.1.AND.J.EQ.1)THEN
                 XMMIN=XMAP(I,J)
                 XMMAX=XMAP(I,J)
                 YMMIN=YMAP(I,J)
                 YMMAX=YMAP(I,J)
            ELSE
                 XMMIN=MIN(XMMIN,XMAP(I,J))
                 XMMAX=MAX(XMMAX,XMAP(I,J))
                 YMMIN=MIN(YMMIN,YMAP(I,J))
                 YMMAX=MAX(YMMAX,YMAP(I,J))
            ENDIF
*   Update angular range.
            IF(XMAP(I,J).NE.0.OR.YMAP(I,J).NE.0)THEN
                 IF(SETAZ)THEN
                      ZAMIN=MIN(ZAMIN,ATAN2(YMAP(I,J),XMAP(I,J)))
                      ZAMAX=MAX(ZAMAX,ATAN2(YMAP(I,J),XMAP(I,J)))
                 ELSE
                      ZAMIN=ATAN2(YMAP(I,J),XMAP(I,J))
                      ZAMAX=ATAN2(YMAP(I,J),XMAP(I,J))
                      SETAZ=.TRUE.
                 ENDIF
            ENDIF
       ENDIF
20     CONTINUE
*   Now check that the triangles fit.
       IF(MAPFLG(1))THEN
            CALL MAPIND((XAUX(1)+XAUX(2)+XAUX(3))/3,
     -           (YAUX(1)+YAUX(2)+YAUX(3))/3,
     -           (ZAUX(1)+ZAUX(2)+ZAUX(3))/3,T1,T2,T3,T4,JAC,DET,IMAP)
            IF(I.NE.IMAP)THEN
                 PRINT *,' !!!!!! MAPFM2 WARNING : The grid in ',
     -                FMAP(1:NCMAP),' does not match the current'//
     -                ' grid; file not read.'
                 RETURN
            ENDIF
       ENDIF
*** Read scalar field values over the triangle.
       IF(SCALAR)THEN
            SUM=0
**  Read field values over the triangle.
            DO 60 J=1,6
            READ(12,'(E27.20,1X,E27.20)',END=2000,ERR=2010,
     -           IOSTAT=IOS) TEMPRE,TEMPIM
*   Can be either a potential.
            IF(ICONT(1).EQ.5)THEN
                 IF(J.EQ.1)THEN
                      VMAP(I,1)=TEMPRE
                 ELSEIF(J.EQ.4)THEN
                      VMAP(I,2)=TEMPRE
                 ELSEIF(J.EQ.6)THEN
                      VMAP(I,3)=TEMPRE
                 ELSEIF(J.EQ.2)THEN
                      VMAP(I,4)=TEMPRE
                 ELSEIF(J.EQ.3)THEN
                      VMAP(I,5)=TEMPRE
                 ELSEIF(J.EQ.5)THEN
                      VMAP(I,6)=TEMPRE
                 ENDIF
*   Or a dielectricum.
            ELSEIF(ICONT(1).EQ.9)THEN
                 SUM=SUM+TEMPRE
            ENDIF
60          CONTINUE
**  If dielectricum, identify the material.
            IF(ICONT(1).EQ.9)THEN
                 SUM=SUM/(600*EPS0)
                 IEPS=-1
                 DO 100 J=1,NEPS
                 IF(ABS(SUM-EPSMAT(J)).LT.1E-4*(ABS(SUM)+
     -                ABS(EPSMAT(J))))IEPS=J
100              CONTINUE
                 IF(IEPS.LT.0.AND.NEPS.GE.MXEPS)THEN
                      PRINT *,' !!!!!! MAPFM2 WARNING : Unable'//
     -                     ' to store a dielectricum from file ',
     -                     FMAP(1:NCMAP),'; file not read.'
                      RETURN
                 ELSEIF(IEPS.LT.0)THEN
                      NEPS=NEPS+1
                      IEPS=NEPS
                      EPSMAT(IEPS)=SUM
                      IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM2'',
     -                     '' DEBUG   : Adding dielectricum with'',
     -                     '' eps='',E10.3,''.'')') EPSMAT(IEPS)
                 ENDIF
                 MATMAP(I)=IEPS
                 NEWEPS=.TRUE.
**  If a potential, keep track of potential range.
            ELSEIF(ICONT(1).EQ.5)THEN
                 IF(I.EQ.1)THEN
                      VMMIN=VMAP(I,1)
                      VMMAX=VMAP(I,1)
                 ENDIF
                 VMMIN=MIN(VMMIN,VMAP(I,1),VMAP(I,2),VMAP(I,3),
     -                VMAP(I,4),VMAP(I,5),VMAP(I,6))
                 VMMAX=MAX(VMMAX,VMAP(I,1),VMAP(I,2),VMAP(I,3),
     -                VMAP(I,4),VMAP(I,5),VMAP(I,6))
            ENDIF
*** Read vectorial field values over the triangle.
       ELSE
*   Take care of knowing |D| either from Ex or by summing.
            IF(ICONT(1).EQ.-9.AND.ICONT(2).EQ.-9)THEN
                 DXCOMP=0
                 DYCOMP=0
            ELSEIF(MAPFLG(10))THEN
                 DXCOMP=EXMAP(I,1)
                 DYCOMP=EYMAP(I,1)
            ENDIF
*   Prepare a summing scalar for epsilons entered as such.
            SUM=0
*   Read the various fields.
            DO 30 J=1,18
            READ(12,'(E27.20,1X,E27.20)',END=2000,ERR=2010,
     -           IOSTAT=IOS) TEMPRE,TEMPIM
*   Averaging of epsilons.
            IF(ICONT(1).EQ.9)THEN
                 IF(J.LE.6)SUM=SUM+TEMPRE
            ELSEIF(ICONT(2).EQ.9)THEN
                 IF(J.GT.6.AND.J.LE.12)SUM=SUM+TEMPRE
            ELSEIF(ICONT(3).EQ.9)THEN
                 IF(J.GT.13.AND.J.LE.18)SUM=SUM+TEMPRE
            ENDIF
*   Ex or Bx corner 1.
            IF(J.EQ.1)THEN
                 IF(ICONT(1).EQ.2)THEN
                      IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                           EXMAP(I,1)=TEMPRE/100
                      ELSEIF(IDATA.EQ.10)THEN
                           EWXMAP(I,1,IWMAP)=TEMPRE/100
                      ENDIF
                 ELSEIF(ICONT(1).EQ.6)THEN
                      BXMAP(I,1)=TEMPRE
                 ELSEIF(ICONT(1).EQ.-9)THEN
                      DXCOMP=DXCOMP+TEMPRE/100
                 ENDIF
*   Ex or Bx corner 2.
            ELSEIF(J.EQ.4)THEN
                 IF(ICONT(1).EQ.2)THEN
                      IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                           EXMAP(I,2)=TEMPRE/100
                      ELSEIF(IDATA.EQ.10)THEN
                           EWXMAP(I,2,IWMAP)=TEMPRE/100
                      ENDIF
                 ELSEIF(ICONT(1).EQ.6)THEN
                      BXMAP(I,2)=TEMPRE
                 ELSEIF(ICONT(1).EQ.-9)THEN
                      DXCOMP=DXCOMP+TEMPRE/100
                 ENDIF
*   Ex or Bx corner 3.
            ELSEIF(J.EQ.6)THEN
                 IF(ICONT(1).EQ.2)THEN
                      IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                           EXMAP(I,3)=TEMPRE/100
                      ELSEIF(IDATA.EQ.10)THEN
                           EWXMAP(I,3,IWMAP)=TEMPRE/100
                      ENDIF
                 ELSEIF(ICONT(1).EQ.6)THEN
                      BXMAP(I,3)=TEMPRE
                 ELSEIF(ICONT(1).EQ.-9)THEN
                      DXCOMP=DXCOMP+TEMPRE/100
                 ENDIF
*   Ex or Bx corner 4.
            ELSEIF(J.EQ.2)THEN
                 IF(ICONT(1).EQ.2)THEN
                      IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                           EXMAP(I,4)=TEMPRE/100
                      ELSEIF(IDATA.EQ.10)THEN
                           EWXMAP(I,4,IWMAP)=TEMPRE/100
                      ENDIF
                 ELSEIF(ICONT(1).EQ.6)THEN
                      BXMAP(I,4)=TEMPRE
                 ELSEIF(ICONT(1).EQ.-9)THEN
                      DXCOMP=DXCOMP+TEMPRE/100
                 ENDIF
*   Ex or Bx corner 5.
            ELSEIF(J.EQ.3)THEN
                 IF(ICONT(1).EQ.2)THEN
                      IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                           EXMAP(I,5)=TEMPRE/100
                      ELSEIF(IDATA.EQ.10)THEN
                           EWXMAP(I,5,IWMAP)=TEMPRE/100
                      ENDIF
                 ELSEIF(ICONT(1).EQ.6)THEN
                      BXMAP(I,5)=TEMPRE
                 ELSEIF(ICONT(1).EQ.-9)THEN
                      DXCOMP=DXCOMP+TEMPRE/100
                 ENDIF
*   Ex or Bx corner 6.
            ELSEIF(J.EQ.5)THEN
                 IF(ICONT(1).EQ.2)THEN
                      IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                           EXMAP(I,6)=TEMPRE/100
                      ELSEIF(IDATA.EQ.10)THEN
                           EWXMAP(I,6,IWMAP)=TEMPRE/100
                      ENDIF
                 ELSEIF(ICONT(1).EQ.6)THEN
                      BXMAP(I,6)=TEMPRE
                 ELSEIF(ICONT(1).EQ.-9)THEN
                      DXCOMP=DXCOMP+TEMPRE/100
                 ENDIF
*   Ey or By corner 1.
            ELSEIF(J.EQ.7)THEN
                 IF(ICONT(2).EQ.3)THEN
                      IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                           EYMAP(I,1)=TEMPRE/100
                      ELSEIF(IDATA.EQ.10)THEN
                           EWYMAP(I,1,IWMAP)=TEMPRE/100
                      ENDIF
                 ELSEIF(ICONT(2).EQ.7)THEN
                      BYMAP(I,1)=TEMPRE
                 ELSEIF(ICONT(2).EQ.-9)THEN
                      DYCOMP=DYCOMP+TEMPRE/100
                 ENDIF
*   Ey or By corner 2.
            ELSEIF(J.EQ.10)THEN
                 IF(ICONT(2).EQ.3)THEN
                      IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                           EYMAP(I,2)=TEMPRE/100
                      ELSEIF(IDATA.EQ.10)THEN
                           EWYMAP(I,2,IWMAP)=TEMPRE/100
                      ENDIF
                 ELSEIF(ICONT(2).EQ.7)THEN
                      BYMAP(I,2)=TEMPRE
                 ELSEIF(ICONT(2).EQ.-9)THEN
                      DYCOMP=DYCOMP+TEMPRE/100
                 ENDIF
*   Ey or By corner 3.
            ELSEIF(J.EQ.12)THEN
                 IF(ICONT(2).EQ.3)THEN
                      IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                           EYMAP(I,3)=TEMPRE/100
                      ELSEIF(IDATA.EQ.10)THEN
                           EWYMAP(I,3,IWMAP)=TEMPRE/100
                      ENDIF
                 ELSEIF(ICONT(2).EQ.7)THEN
                      BYMAP(I,3)=TEMPRE
                 ELSEIF(ICONT(2).EQ.-9)THEN
                      DYCOMP=DYCOMP+TEMPRE/100
                 ENDIF
*   Ey or By corner 4.
            ELSEIF(J.EQ.8)THEN
                 IF(ICONT(2).EQ.3)THEN
                      IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                           EYMAP(I,4)=TEMPRE/100
                      ELSEIF(IDATA.EQ.10)THEN
                           EWYMAP(I,4,IWMAP)=TEMPRE/100
                      ENDIF
                 ELSEIF(ICONT(2).EQ.7)THEN
                      BYMAP(I,4)=TEMPRE
                 ELSEIF(ICONT(2).EQ.-9)THEN
                      DYCOMP=DYCOMP+TEMPRE/100
                 ENDIF
*   Ey or By corner 5.
            ELSEIF(J.EQ.9)THEN
                 IF(ICONT(2).EQ.3)THEN
                      IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                           EYMAP(I,5)=TEMPRE/100
                      ELSEIF(IDATA.EQ.10)THEN
                           EWYMAP(I,5,IWMAP)=TEMPRE/100
                      ENDIF
                 ELSEIF(ICONT(2).EQ.7)THEN
                      BYMAP(I,5)=TEMPRE
                 ELSEIF(ICONT(2).EQ.-9)THEN
                      DYCOMP=DYCOMP+TEMPRE/100
                 ENDIF
*   Ey or By corner 6.
            ELSEIF(J.EQ.11)THEN
                 IF(ICONT(2).EQ.3)THEN
                      IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                           EYMAP(I,6)=TEMPRE/100
                      ELSEIF(IDATA.EQ.10)THEN
                           EWYMAP(I,6,IWMAP)=TEMPRE/100
                      ENDIF
                 ELSEIF(ICONT(2).EQ.7)THEN
                      BYMAP(I,6)=TEMPRE
                 ELSEIF(ICONT(2).EQ.-9)THEN
                      DYCOMP=DYCOMP+TEMPRE/100
                 ENDIF
*   Ez or Bz corner 1.
            ELSEIF(J.EQ.13)THEN
                 IF(ICONT(3).EQ.4)THEN
                      IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                           EZMAP(I,1)=TEMPRE/100
                      ELSEIF(IDATA.EQ.10)THEN
                           EWZMAP(I,1,IWMAP)=TEMPRE/100
                      ENDIF
                 ELSEIF(ICONT(3).EQ.8)THEN
                      BZMAP(I,1)=TEMPRE
                 ENDIF
*   Ez or Bz corner 2.
            ELSEIF(J.EQ.16)THEN
                 IF(ICONT(3).EQ.4)THEN
                      IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                           EZMAP(I,2)=TEMPRE/100
                      ELSEIF(IDATA.EQ.10)THEN
                           EWZMAP(I,2,IWMAP)=TEMPRE/100
                      ENDIF
                 ELSEIF(ICONT(3).EQ.8)THEN
                      BZMAP(I,2)=TEMPRE
                 ENDIF
*   Ez or Bz corner 3.
            ELSEIF(J.EQ.18)THEN
                 IF(ICONT(3).EQ.4)THEN
                      IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                           EZMAP(I,3)=TEMPRE/100
                      ELSEIF(IDATA.EQ.10)THEN
                           EWZMAP(I,3,IWMAP)=TEMPRE/100
                      ENDIF
                 ELSEIF(ICONT(3).EQ.8)THEN
                      BZMAP(I,3)=TEMPRE
                 ENDIF
*   Ez or Bz corner 4.
            ELSEIF(J.EQ.14)THEN
                 IF(ICONT(3).EQ.4)THEN
                      IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                           EZMAP(I,4)=TEMPRE/100
                      ELSEIF(IDATA.EQ.10)THEN
                           EWZMAP(I,4,IWMAP)=TEMPRE/100
                      ENDIF
                 ELSEIF(ICONT(3).EQ.8)THEN
                      BZMAP(I,4)=TEMPRE
                 ENDIF
*   Ez or Bz corner 5.
            ELSEIF(J.EQ.15)THEN
                 IF(ICONT(3).EQ.4)THEN
                      IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                           EZMAP(I,5)=TEMPRE/100
                      ELSEIF(IDATA.EQ.10)THEN
                           EWZMAP(I,5,IWMAP)=TEMPRE/100
                      ENDIF
                 ELSEIF(ICONT(3).EQ.8)THEN
                      BZMAP(I,5)=TEMPRE
                 ENDIF
*   Ez or Bz corner 6.
            ELSEIF(J.EQ.17)THEN
                 IF(ICONT(3).EQ.4)THEN
                      IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                           EZMAP(I,6)=TEMPRE/100
                      ELSEIF(IDATA.EQ.10)THEN
                           EWZMAP(I,6,IWMAP)=TEMPRE/100
                      ENDIF
                 ELSEIF(ICONT(3).EQ.8)THEN
                      BZMAP(I,6)=TEMPRE
                 ENDIF
            ENDIF
30          CONTINUE
**  If dielectricum, identify the material.
            IF(ICONT(1).EQ.9.OR.ICONT(2).EQ.9.OR.ICONT(3).EQ.9)THEN
                 SUM=SUM/(600*EPS0)
                 IEPS=-1
                 DO 160 J=1,NEPS
                 IF(ABS(SUM-EPSMAT(J)).LT.1E-4*(ABS(SUM)+
     -                ABS(EPSMAT(J))))IEPS=J
160              CONTINUE
                 IF(IEPS.LT.0.AND.NEPS.GE.MXEPS)THEN
                      PRINT *,' !!!!!! MAPFM2 WARNING : Unable'//
     -                     ' to store a dielectricum from file ',
     -                     FMAP(1:NCMAP),'; file not read.'
                      RETURN
                 ELSEIF(IEPS.LT.0)THEN
                      NEPS=NEPS+1
                      IEPS=NEPS
                      EPSMAT(IEPS)=SUM
                      IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM2'',
     -                     '' DEBUG   : Adding dielectricum with'',
     -                     '' eps='',E10.3,''.'')') EPSMAT(IEPS)
                 ENDIF
                 MATMAP(I)=IEPS
**  Dielectricum identification via D/E comparison.
            ELSEIF((MAPFLG(2).AND.MAPFLG(3).AND.
     -           (.NOT.MAPFLG(9)).AND.
     -           ICONT(1).EQ.-9.AND.ICONT(2).EQ.-9).OR.
     -           (MAPFLG(10).AND.(.NOT.MAPFLG(9)).AND.
     -           ICONT(1).EQ.2.AND.ICONT(2).EQ.3))THEN
                 IEPS=-1
                 DCOMP=DXCOMP**2+DYCOMP**2
                 ECOMP=(EXMAP(I,1)+EXMAP(I,2)+EXMAP(I,3)+
     -                EXMAP(I,4)+EXMAP(I,5)+EXMAP(I,6))**2+
     -                 (EYMAP(I,1)+EYMAP(I,2)+EYMAP(I,3)+
     -                EYMAP(I,4)+EYMAP(I,5)+EYMAP(I,6))**2
                 DO 170 J=1,NEPS
                 IF(ABS(ECOMP*(100*EPS0*EPSMAT(J))**2-DCOMP).LT.1E-4*
     -                (ABS(ECOMP*(100*EPS0*EPSMAT(J))**2)+
     -                ABS(DCOMP)))IEPS=J
170              CONTINUE
                 IF(ECOMP.LE.0.AND.DCOMP.GT.0)THEN
                      PRINT *,' !!!!!! MAPFM2 WARNING : Found'//
     -                     ' a dielectric constant of 0; skipped.'
                 ELSEIF(IEPS.LT.0.AND.NEPS.GE.MXEPS)THEN
                      PRINT *,' !!!!!! MAPFM2 WARNING : Unable'//
     -                     ' to store a dielectricum from file ',
     -                     FMAP(1:NCMAP),'; file not read.'
                      RETURN
                 ELSEIF(IEPS.LT.0)THEN
                      NEPS=NEPS+1
                      IEPS=NEPS
                      IF(ECOMP.LE.0)THEN
                           PRINT *,' ------ MAPFM2 MESSAGE : Unable'//
     -                          ' to determine epsilon in an E=0'//
     -                          ' tetrahedron; epsilon set to 0.'
                           EPSMAT(IEPS)=0
                      ELSE
                           EPSMAT(IEPS)=SQRT(DCOMP/ECOMP)/(100*EPS0)
                      ENDIF
                      IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM2'',
     -                     '' DEBUG   : Adding dielectricum with'',
     -                     '' eps='',E10.3,''.'')') EPSMAT(IEPS)
                 ENDIF
                 MATMAP(I)=IEPS
                 NEWEPS=.TRUE.
*  Otherwise store the field.
            ELSEIF(ICONT(1).EQ.-9.AND.ICONT(2).EQ.-9.AND.
     -           (.NOT.MAPFLG(2)).AND.(.NOT.MAPFLG(3)))THEN
                 EXMAP(I,1)=DXCOMP
                 EYMAP(I,1)=DYCOMP
            ENDIF
       ENDIF
*   Skip blank line at the end.
       READ(12,'()',END=2000,ERR=2010,IOSTAT=IOS)
10     CONTINUE
*** Assign triangle count.
       IF(MAPFLG(1))THEN
            IF(I.NE.NMAP)THEN
                 PRINT *,' !!!!!! MAPFM2 WARNING : Number of'//
     -                ' triangles in ',FMAP(1:NCMAP),' does'//
     -                ' not agree with previous files; not read.'
                 RETURN
            ENDIF
       ELSE
            IF(I.LE.0)THEN
                 PRINT *,' !!!!!! MAPFM2 WARNING : ',FMAP(1:NCMAP),
     -                ' contain no triangles; not read.'
                 RETURN
            ELSE
                 NMAP=I
            ENDIF
       ENDIF
*** Material has been defined is NEWEPS is set.
       IF(NEWEPS)MAPFLG(9)=.TRUE.
*** Flag those elements which have been defined.
       MAPFLG(1)=.TRUE.
       DO 70 I=1,3
       IF(ICONT(I).EQ.2.OR.ICONT(I).EQ.3.OR.ICONT(I).EQ.4)THEN
            IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                 MAPFLG(ICONT(I))=.TRUE.
            ELSEIF(IDATA.EQ.10)THEN
                 MAPFLG(8+ICONT(I)+4*IWMAP-3)=.TRUE.
            ENDIF
       ELSEIF(ICONT(I).GT.0)THEN
            MAPFLG(ICONT(I))=.TRUE.
       ELSEIF(ICONT(I).EQ.-9)THEN
            MAPFLG(10)=.TRUE.
       ENDIF
70     CONTINUE
*** Seems to have worked, set error flag to OK and return.
       IFAIL=0
       MAPTYP=2
       RETURN
*** Handle error conditions.
2000   CONTINUE
       PRINT *,' !!!!!! MAPFM2 WARNING : Premature end of file on ',
     -      FMAP(1:NCMAP),'; map not available.'
       RETURN
2010   CONTINUE
       PRINT *,' !!!!!! MAPFM2 WARNING : Error reading field map'//
     -      ' file ',FMAP(1:NCMAP),'; map not available.'
       RETURN
       END
