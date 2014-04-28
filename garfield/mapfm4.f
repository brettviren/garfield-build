CDECK  ID>, MAPFM4.
       SUBROUTINE MAPFM4(FMAP,NCMAP,IDATA,IWMAP,IFAIL)
*-----------------------------------------------------------------------
*   MAPFM4 - Reads a Maxwell 2D Field Simulator version xxx table of
*            triangles.
*   (Last changed on 14/ 3/07.)
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
       INTEGER IEPS,ICONT(3),IMAX,NUSE,IWMAP,
     -      I,J,K,NCMAP,IFAIL,IFAIL1,IFAIL2,IFAIL3,IFAIL4,
     -      IT1,IT2,IT3,ITRIAN,IOS,NC,INPCMP,
     -      IDATA,NWORD,NCAUX,IEND,IP,NTRIAN,NPOINT,
     -      MTRIAN,MPOINT,LOOKUP(6)
       REAL TEMP(6),SUM,ECOMP,DCOMP,DX,DY,DZ,XP,YP,
     -      XGMIN,XGMAX,YGMIN,YGMAX
       CHARACTER*(*) FMAP
       CHARACTER*(MXNAME) FAUX
       CHARACTER*80 STRING
       LOGICAL SCALAR,READ,NEWEPS,EXIST,FIRST
       EXTERNAL INPCMP
       SAVE NTRIAN,NPOINT,LOOKUP,XGMIN,XGMAX,YGMIN,YGMAX
       DATA NTRIAN/0/, NPOINT/0/
       DATA LOOKUP/1, 4, 5, 2, 6, 3/
C       DATA LOOKUP/1, 4, 2, 5, 6, 3/
       DATA XGMIN,XGMAX,YGMIN,YGMAX /-1.0, 1.0, -1.0, 1.0/
**** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MAPFM4 ///'
*** Assume that this will fail.
       IFAIL=1
*** Make sure the file names are not too long.
       IF(NCMAP.GT.MXNAME)THEN
            PRINT *,' !!!!!! MAPFM4 WARNING : Field map file name ',
     -           FMAP(1:NCMAP),' is too long; not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*** Model file - extract the dimensions of the domain.
       IF(IDATA.EQ.-1)THEN
*   Skip until the block with the settings.
100         CONTINUE
            CALL INPGET
            CALL INPNUM(NWORD)
            IF(INPCMP(1,'B_SETNGS').EQ.0)GOTO 100
*   Skip until the cell dimension field.
110         CONTINUE
            CALL INPGET
            CALL INPNUM(NWORD)
            IF(INPCMP(1,'EXTENT').EQ.0)GOTO 110
*   Read the box dimensions.
            CALL INPGET
            CALL INPNUM(NWORD)
            CALL INPCHK(1,2,IFAIL1)
            CALL INPCHK(2,2,IFAIL2)
            CALL INPRDR(1,XGMIN,-1.0)
            CALL INPRDR(2,YGMIN,-1.0)
            CALL INPGET
            CALL INPNUM(NWORD)
            CALL INPCHK(1,2,IFAIL3)
            CALL INPCHK(2,2,IFAIL4)
            CALL INPRDR(1,XGMAX,+1.0)
            CALL INPRDR(2,YGMAX,+1.0)
*   Check that reading worked well.
            IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.
     -           IFAIL3.EQ.0.AND.IFAIL4.EQ.0.AND.
     -           XGMAX.GT.XGMIN.AND.
     -           YGMAX.GT.YGMIN)THEN
                 XGMIN=XGMIN*100
                 XGMAX=XGMAX*100
                 YGMIN=YGMIN*100
                 YGMAX=YGMAX*100
                 IF(LDEBUG)WRITE(LUNOUT,'(
     -                ''  ++++++ MAPFM4 DEBUG   : Domain of the cell:''/
     -                26X,E15.8,'' <  x < '',E15.8,'' cm ''/
     -                26X,E15.8,'' <  y < '',E15.8,'' cm '')')
     -                XGMIN,XGMAX,YGMIN,YGMAX
                 IFAIL=0
            ELSE
                 PRINT *,' !!!!!! MAPFM4 WARNING : Did not find'//
     -                ' a correct domain size in the .sm2 file.'
                 XGMIN=-1.0
                 XGMAX=+1.0
                 YGMIN=-1.0
                 YGMAX=+1.0
                 IFAIL=1
            ENDIF
*   And return.
            CALL INPSWI('RESTORE')
            RETURN
*** Check for mesh files - or guess mesh name if there is no mesh yet.
       ELSEIF(IDATA.EQ.1)THEN
*   Get rid of extensions, if any.
            IEND=0
            DO 1050 I=NCMAP,1,-1
            IF(FMAP(I:I).NE.' '.AND.IEND.EQ.0)IEND=I
            IF(FMAP(I:I).EQ.'/')THEN
                 IF(IEND.GT.0)THEN
                      FAUX=FMAP(1:IEND)//'.'
                      NCAUX=IEND+1
                 ELSE
                      FAUX=FMAP
                      NCAUX=NCMAP
                 ENDIF
                 GOTO 1060
            ELSEIF(FMAP(I:I).EQ.'.')THEN
                 FAUX=FMAP(1:I)
                 NCAUX=I
                 GOTO 1060
            ENDIF
1050        CONTINUE
            IF(IEND.EQ.0)THEN
                 PRINT *,' !!!!!! MAPFM4 WARNING : Mesh file name'//
     -                ' empty ; not read.'
                 CALL INPSWI('RESTORE')
                 RETURN
            ENDIF
            FAUX=FMAP(1:IEND)//'.'
            NCAUX=IEND+1
1060        CONTINUE
*   Verify that the resulting file name is not too long.
            IF(NCAUX+3.GT.MXNAME)THEN
                 PRINT *,' !!!!!! MAPFM4 WARNING : Mesh file name'//
     -                ' too long after expansion ; not read.'
                 CALL INPSWI('RESTORE')
                 RETURN
            ENDIF
*   Check for the existence of the files.
            CALL DSNINQ(FAUX(1:NCAUX)//'tri',NCAUX+3,EXIST)
            IF(.NOT.EXIST)THEN
                 PRINT *,' !!!!!! MAPFM4 WARNING : Triangle file '//
     -                FAUX(1:NCAUX)//'tri not found; map not read.'
                 CALL INPSWI('RESTORE')
                 RETURN
            ENDIF
            CALL DSNINQ(FAUX(1:NCAUX)//'pts',NCAUX+3,EXIST)
            IF(.NOT.EXIST)THEN
                 PRINT *,' !!!!!! MAPFM4 WARNING : Point file '//
     -                FAUX(1:NCAUX)//'pts not found; map not read.'
                 CALL INPSWI('RESTORE')
                 RETURN
            ENDIF
**  If we didn't get a mesh file, try to guess the name.
       ELSEIF(.NOT.MAPFLG(1))THEN
*   Locate the directory name.
            IEND=0
            DO 1070 I=NCMAP,1,-1
            IF(FMAP(I:I).NE.' '.AND.IEND.EQ.0)IEND=I
            IF(FMAP(I:I).EQ.'/')THEN
                 FAUX=FMAP(1:I)
                 NCAUX=I
                 GOTO 1080
            ENDIF
1070        CONTINUE
            IF(IEND.EQ.0)THEN
                 PRINT *,' !!!!!! MAPFM4 WARNING : Field file name'//
     -                ' empty ; not read.'
                 CALL INPSWI('RESTORE')
                 RETURN
            ENDIF
            FAUX='./'
            NCAUX=2
1080        CONTINUE
*   Test for various files.
            CALL DSNINQ(FAUX(1:NCAUX)//'fileset2.tri',NCAUX+12,EXIST)
            IF(EXIST)THEN
                 CALL DSNINQ(FAUX(1:NCAUX)//'fileset2.pts',NCAUX+12,
     -                EXIST)
                 IF(EXIST)THEN
                      FAUX=FAUX(:NCAUX)//'fileset2.'
                      NCAUX=NCAUX+9
                      PRINT *,' ------ MAPFM4 MESSAGE : Taking the'//
     -                     ' "fileset2" mesh.'
                      GOTO 1090
                 ENDIF
            ENDIF
            CALL DSNINQ(FAUX(1:NCAUX)//'fileset1.tri',NCAUX+12,EXIST)
            IF(EXIST)THEN
                 CALL DSNINQ(FAUX(1:NCAUX)//'fileset1.pts',NCAUX+12,
     -                EXIST)
                 IF(EXIST)THEN
                      FAUX=FAUX(:NCAUX)//'fileset1.'
                      NCAUX=NCAUX+9
                      PRINT *,' ------ MAPFM4 MESSAGE : Taking the'//
     -                     ' "fileset1" mesh.'
                      GOTO 1090
                 ENDIF
            ENDIF
            PRINT *,' !!!!!! MAPFM4 WARNING : Triangle, point and'//
     -          ' solid files not found; specify mesh explicitely.'
            CALL INPSWI('RESTORE')
            RETURN
*   Verify that the resulting file name is not too long.
1090        CONTINUE
            IF(NCAUX+3.GT.MXNAME)THEN
                 PRINT *,' !!!!!! MAPFM4 WARNING : Mesh file name'//
     -                ' too long after expansion ; not read.'
                 CALL INPSWI('RESTORE')
                 RETURN
            ENDIF
       ENDIF
*** Skip the mesh decoding if this has already been done.
       IF(IDATA.NE.1.AND.MAPFLG(1))GOTO 1000
*   Close the current file, re-open later.
       CALL INPSWI('RESTORE')
       CLOSE(12,ERR=2030,IOSTAT=IOS)
*   Construct the triangle file name.
       FAUX=FAUX(1:NCAUX)//'tri'
       NCAUX=NCAUX+3
*   Open the triangle file.
       CALL DSNOPN(FAUX,NCAUX,12,'READ-FILE',IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! MAPFM4 WARNING : Unable to open the'//
     -           ' triangle file '//FAUX(1:NCAUX)//'; map not read.'
            RETURN
       ENDIF
*   Record the opening.
       CALL DSNLOG(FAUX(1:NCAUX),'Triangles ','Sequential',
     -      'Read only ')
*   Switch to reading the file.
       CALL INPSWI('UNIT12')
**  Loop over the triangles.
       CALL PROFLD(2,'Triangles',-1.0)
       CALL PROSTA(2,-1.0)
       NTRIAN=0
1030   CONTINUE
*   Read the data line.
       CALL INPGET
       CALL INPNUM(NWORD)
*   Check for end of file
       IF(INPCMP(1,'END').NE.0)THEN
            GOTO 1031
       ELSEIF(NWORD.NE.5)THEN
            PRINT *,' !!!!!! MAPFM4 WARNING : Unexpected number'//
     -           ' of words seen in '//FAUX(1:NCAUX)//
     -           '; map not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Increment the number of triangles, check limits.
       IF(NTRIAN+1.GT.MXMAP)THEN
            PRINT *,' !!!!!! MAPFM4 WARNING : Number of'//
     -           ' triangles in '//FAUX(1:NCAUX)//
     -           ' exceeds compilation limit; file not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ELSE
            NTRIAN=NTRIAN+1
       ENDIF
*   Find the pointers to the .pnt file.
       CALL INPCHK(2,1,IFAIL1)
       CALL INPCHK(3,1,IFAIL2)
       CALL INPCHK(4,1,IFAIL3)
       CALL INPCHK(5,1,IFAIL4)
       CALL INPRDI(2,ITRIAN,0)
       CALL INPRDI(3,IT1,0)
       CALL INPRDI(4,IT2,0)
       CALL INPRDI(5,IT3,0)
       IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.
     -      IFAIL3.NE.0.OR.IFAIL4.NE.0.OR.
     -      IT1.LT.0.OR.IT2.LT.0.OR.IT3.LT.0.OR.
     -      ITRIAN.NE.NTRIAN)THEN
            PRINT *,' !!!!!! MAPFM4 WARNING : Reference to points'//
     -           ' incorrect in '//FAUX(1:NCAUX)//'; map not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Store the reference pointers temporarily in Ex.
       EXMAP(NTRIAN,1)=IT1
       EXMAP(NTRIAN,2)=IT2
       EXMAP(NTRIAN,3)=IT3
*   Skip the 3 lines of additional information.
       READ(12,'(//)',ERR=2015,END=2005,IOSTAT=IOS)
       GOTO 1030
1031   CONTINUE
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM4 DEBUG   : Number'',
     -      '' of .tri triangles: '',I5)') NTRIAN
*   Switch back to regular input.
       CALL INPSWI('RESTORE')
*   Close the triangle file.
       CLOSE(12,ERR=2030,IOSTAT=IOS)
**  Construct the name of the .pts file.
       FAUX(NCAUX-2:NCAUX)='pts'
*   Open the points file.
       CALL DSNOPN(FAUX,NCAUX,12,'READ-FILE',IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! MAPFM4 WARNING : Unable to open the'//
     -           ' points file '//FAUX(1:NCAUX)//'; map not read.'
            RETURN
       ENDIF
*   Record the opening.
       CALL DSNLOG(FAUX(1:NCAUX),'Points    ','Sequential',
     -      'Read only ')
**  Switch to the data file.
       CALL INPSWI('UNIT12')
**  Loop over the points.
       CALL PROFLD(2,'Points',-1.0)
       CALL PROSTA(2,-1.0)
       FIRST=.TRUE.
       NPOINT=0
1040   CONTINUE
*   Read the data line.
       CALL INPGET
       CALL INPNUM(NWORD)
       IF(INPCMP(1,'END').NE.0)THEN
            GOTO 1041
       ELSEIF(INPCMP(1,'P').EQ.0)THEN
            GOTO 1040
       ELSEIF(NWORD.NE.5)THEN
            PRINT *,' !!!!!! MAPFM4 WARNING : The format of '//
     -           FAUX(1:NCAUX)//' is not known; map not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Increment point counter.
       NPOINT=NPOINT+1
*   Read the point coordinates and the reference to the .tri file.
       CALL INPCHK(2,1,IFAIL2)
       CALL INPCHK(3,2,IFAIL3)
       CALL INPCHK(4,2,IFAIL4)
       CALL INPRDI(2,IP,0)
       CALL INPRDR(3,XP,0.0)
       CALL INPRDR(4,YP,0.0)
       IF(IFAIL2.NE.0.OR.IFAIL3.NE.0.OR.IFAIL4.NE.0.OR.
     -      IP.NE.NPOINT-1)THEN
            PRINT *,' !!!!!! MAPFM4 WARNING : Point description'//
     -           ' invalid in '//FAUX(1:NCAUX)//'; map not read.'
            IF(IFAIL2.NE.0)PRINT *,'                        '//
     -           ' Point number is not a valid integer.'
            IF(IFAIL3.NE.0)PRINT *,'                        '//
     -           ' Point x-coordinate is not a valid real.'
            IF(IFAIL4.NE.0)PRINT *,'                        '//
     -           ' Point y-coordinate is not a valid real.'
            IF(IP.NE.NPOINT-1)PRINT *,'                        '//
     -           ' Point number does not match line number.'
            PRINT *,'                         ip = ',ip,', xp = ',
     -           xp,', yp = ',yp,', npoint = ',npoint,'.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Store the triangle parameters that refer to this point.
       NUSE=0
       DO 1100 K=1,NTRIAN
       DO 1110 J=1,3
       IF(NINT(EXMAP(K,J)).EQ.IP)THEN
            NUSE=NUSE+1
            XMAP(K,J)=XP
            YMAP(K,J)=YP
            ZMAP(K,J)=0
            EXMAP(K,J)=-1
       ENDIF
1110   CONTINUE
1100   CONTINUE
*   If this point was not used, skip the rest.
       IF(NUSE.LE.0)GOTO 1040
*   Update the chamber dimensions.
       IF(FIRST)THEN
            FIRST=.FALSE.
            XMMIN=XP
            XMMAX=XP
            YMMIN=YP
            YMMAX=YP
       ELSE
            XMMIN=MIN(XMMIN,XP)
            XMMAX=MAX(XMMAX,XP)
            YMMIN=MIN(YMMIN,YP)
            YMMAX=MAX(YMMAX,YP)
       ENDIF
*   Update angular ranges.
       IF(XP.NE.0.OR.YP.NE.0)THEN
            IF(SETAZ)THEN
                 ZAMIN=MIN(ZAMIN,ATAN2(YP,XP))
                 ZAMAX=MAX(ZAMAX,ATAN2(YP,XP))
            ELSE
                 ZAMIN=ATAN2(YP,XP)
                 ZAMAX=ATAN2(YP,XP)
                 SETAZ=.TRUE.
            ENDIF
       ENDIF
*   Next point.
       GOTO 1040
1041   CONTINUE
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM4 DEBUG   : Number'',
     -      '' of points: '',I5)') NPOINT
*   Ensure the grid dimensions are not singular.
       IF(XMMAX.LE.XMMIN.OR.YMMAX.LE.YMMIN)THEN
            PRINT *,' !!!!!! MAPFM4 WARNING : Grid size appears to'//
     -           ' be degenerate; set to (-1,-1) to (+1,+1).'
            XMMIN=-1
            XMMAX=-1
            YMMIN=-1
            YMMAX=-1
       ENDIF
*   Scale the grid to the proper dimensions.
       DO 1140 I=1,NTRIAN
       DO 1150 J=1,3
       XMAP(I,J)=XGMIN+(XGMAX-XGMIN)*(XMAP(I,J)-XMMIN)/(XMMAX-XMMIN)
       YMAP(I,J)=YGMIN+(YGMAX-YGMIN)*(YMAP(I,J)-YMMIN)/(YMMAX-YMMIN)
1150   CONTINUE
1140   CONTINUE
       XMMIN=XGMIN
       XMMAX=XGMAX
       YMMIN=YGMIN
       YMMAX=YGMAX
**  Switch back to regular input.
       CALL INPSWI('RESTORE')
*   End of reading, make sure that all triangle references are solved.
       DO 1120 I=1,NTRIAN
       DO 1130 J=1,3
       IF(EXMAP(I,J).GE.0)THEN
            PRINT *,' !!!!!! MAPFM4 WARNING : Unresolved references'//
     -           ' in triangle ',I,' ; map rejected.'
            RETURN
       ENDIF
1130   CONTINUE
1120   CONTINUE
*   Store the number of triangles.
       NMAP=NTRIAN
**  Set the flag that the mesh is now defined.
       MAPFLG(1)=.TRUE.
**  In case this was an explicit mesh, return with success status.
       IF(IDATA.EQ.1)THEN
            IFAIL=0
            RETURN
*   Otherwise, close the points file and re-open mesh file.
       ELSE
            CLOSE(12,ERR=2030,IOSTAT=IOS)
            CALL DSNOPN(FMAP,NCMAP,12,'READ-FILE',IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFM4 WARNING : Re-opening the'//
     -                ' field map failed ; map not read.'
                 RETURN
            ENDIF
*   Record the opening.
            CALL DSNLOG(FMAP(1:NCMAP),'Field map ','Sequential',
     -           'Re-read   ')
*   Read the header records, switch to the data file.
            CALL INPSWI('UNIT12')
            CALL INPGET
            CALL INPNUM(NWORD)
*   Check for empty files.
            IF(NWORD.EQ.0)THEN
                 PRINT *,' !!!!!! MAPFM4 WARNING : The file ',
     -                FMAP(1:NCMAP),' seems to be empty; not read.'
                 CALL INPSWI('RESTORE')
                 CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
                 RETURN
            ENDIF
       ENDIF
*** Read the field map.
1000   CONTINUE
*   See whether the data is scalar or vector.
       IF(INPCMP(1,'SCALAR').NE.0)THEN
            SCALAR=.TRUE.
       ELSEIF(INPCMP(1,'VECTOR').NE.0)THEN
            SCALAR=.FALSE.
       ELSE
            PRINT *,' !!!!!! MAPFM4 WARNING : The file ',
     -           FMAP(1:NCMAP),' contains neither scalar nor'//
     -           ' vectorial data; not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Initial contents flags.
       READ=.FALSE.
       NEWEPS=.FALSE.
*** Determine the contents of the file, first for scalar files.
       IF(SCALAR)THEN
*   They contain 1 data word per line.
            IMAX=1
*   Potentials.
            IF(INPCMP(3,'Phi')+INPCMP(3,'smh(Phi)').NE.0)THEN
                 ICONT(1)=5
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM4 DEBUG   :'',
     -                '' File contains a potential.'')')
                 IF(MAPFLG(5))PRINT *,' ------ MAPFM4 MESSAGE :'//
     -                ' Overwriting current potential map.'
                 MAPFLG(5)=.FALSE.
                 READ=.TRUE.
*   Dielectric constants.
            ELSEIF(INPCMP(3,'epsilon').NE.0)THEN
                 ICONT(1)=9
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM4 DEBUG   :'',
     -                '' File contains an epsilon map.'')')
                 IF(MAPFLG(9))PRINT *,' ------ MAPFM4 MESSAGE :'//
     -                ' Overwriting current material map.'
                 MAPFLG(9)=.FALSE.
                 NEWEPS=.TRUE.
                 READ=.TRUE.
                 MATSRC='EPSILON'
*   Conductivity.
            ELSEIF(INPCMP(3,'sigma').NE.0)THEN
                 ICONT(1)=9
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM4 DEBUG   :'',
     -                '' File contains a conductivity map.'')')
                 IF(MAPFLG(9))PRINT *,' ------ MAPFM4 MESSAGE :'//
     -                ' Overwriting current material map.'
                 MAPFLG(9)=.FALSE.
                 NEWEPS=.TRUE.
                 READ=.TRUE.
                 MATSRC='SIGMA'
*   All the rest is not known.
            ELSE
                 CALL INPSTR(3,3,STRING,NC)
                 PRINT *,' !!!!!! MAPFM4 WARNING : The file ',
     -                FMAP(1:NCMAP),' contains the unknown "'//
     -                STRING(1:NC)//'" field; ignored.'
                 ICONT(1)=0
                 READ=.TRUE.
            ENDIF
            ICONT(2)=0
            ICONT(3)=0
**  Next for vector files.
       ELSE
*   Which have 3 words per line.
            IMAX=3
*   Ex,y field, either main field or weighting field.
            IF(INPCMP(3,'<Ex,Ey,0>')+
     -           INPCMP(3,'<smh(Ex),smh(Ey),smh(0)>').NE.0)THEN
                 ICONT(1)=2
                 ICONT(2)=3
                 ICONT(3)=0
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM4 DEBUG   :'',
     -                '' File contains an E field.'')')
                 IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                      IF(MAPFLG(2).OR.MAPFLG(3))
     -                     PRINT *,' ------ MAPFM4 MESSAGE :'//
     -                     ' Overwriting current E field map.'
                      MAPFLG(2)=.FALSE.
                      MAPFLG(3)=.FALSE.
                 ELSEIF(IDATA.EQ.10)THEN
                      IF(MAPFLG(10+4*IWMAP-3).OR.MAPFLG(11+4*IWMAP-3))
     -                     PRINT *,' ------ MAPFM4 MESSAGE :'//
     -                     ' Overwriting current weighting field map.'
                      MAPFLG(10+4*IWMAP-3)=.FALSE.
                      MAPFLG(11+4*IWMAP-3)=.FALSE.
                 ENDIF
                 READ=.TRUE.
*   Er,z field, either main field or weighting field.
            ELSEIF(INPCMP(3,'<Er,Ez,0>')+
     -           INPCMP(3,'<smh(Er),smh(Ez),smh(0)>').NE.0)THEN
                 ICONT(1)=2
                 ICONT(2)=4
                 ICONT(3)=0
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM4 DEBUG   :'',
     -                '' File contains an E field.'')')
                 IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                      IF(MAPFLG(2).OR.MAPFLG(4))
     -                     PRINT *,' ------ MAPFM4 MESSAGE :'//
     -                     ' Overwriting current E field map.'
                      MAPFLG(2)=.FALSE.
                      MAPFLG(4)=.FALSE.
                 ELSEIF(IDATA.EQ.10)THEN
                      IF(MAPFLG(10+4*IWMAP-3).OR.MAPFLG(12+4*IWMAP-3))
     -                     PRINT *,' ------ MAPFM4 MESSAGE :'//
     -                     ' Overwriting current weighting field map.'
                      MAPFLG(10+4*IWMAP-3)=.FALSE.
                      MAPFLG(12+4*IWMAP-3)=.FALSE.
                 ENDIF
                 READ=.TRUE.
                 PERRZ=.TRUE.
*   B field in x,y coordinates.
            ELSEIF(INPCMP(3,'<Bx,By,Bz>')+
     -           INPCMP(3,'<smh(Bx),smh(By),smh(Bz)>').NE.0)THEN
                 ICONT(1)=6
                 ICONT(2)=7
                 ICONT(3)=8
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM4 DEBUG   :'',
     -                '' File contains a B field.'')')
                 IF(MAPFLG(6).OR.MAPFLG(7).OR.MAPFLG(8))
     -                PRINT *,' ------ MAPFM4 MESSAGE :'//
     -                ' Overwriting current E field map.'
                 MAPFLG(6)=.FALSE.
                 MAPFLG(7)=.FALSE.
                 MAPFLG(8)=.FALSE.
                 READ=.TRUE.
*   B field in r,z coordinates.
            ELSEIF(INPCMP(3,'<Br,Bz,0>')+
     -           INPCMP(3,'<smh(Br),smh(Bz),smh(0)>').NE.0)THEN
                 ICONT(1)=6
                 ICONT(2)=8
                 ICONT(3)=0
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM4 DEBUG   :'',
     -                '' File contains a B field.'')')
                 IF(MAPFLG(6).OR.MAPFLG(8))
     -                PRINT *,' ------ MAPFM4 MESSAGE :'//
     -                ' Overwriting current B field map.'
                 MAPFLG(6)=.FALSE.
                 MAPFLG(8)=.FALSE.
                 READ=.TRUE.
*   D field.
            ELSEIF(INPCMP(3,'<Dx,Dy,0>')+
     -           INPCMP(3,'<smh(Dx),smh(Dy),smh(0)>')+
     -           INPCMP(3,'<Dr,Dz,0>')+
     -           INPCMP(3,'<smh(Dr),smh(Dz),smh(0)>').NE.0)THEN
                 ICONT(1)=-9
                 ICONT(2)=-9
                 ICONT(3)=-9
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM4 DEBUG   :'',
     -                '' File contains a D field.'')')
                 IF(MAPFLG(9))PRINT *,' ------ MAPFM4 MESSAGE :'//
     -                ' Overwriting current material map.'
                 MAPFLG(9)=.FALSE.
                 READ=.TRUE.
                 MATSRC='EPSILON'
*   All the rest is not known.
            ELSE
                 CALL INPSTR(3,3,STRING,NC)
                 PRINT *,' !!!!!! MAPFM4 WARNING : The file ',
     -                FMAP(1:NCMAP),' contains the unknown "'//
     -                STRING(1:NC)//' field; ignored.'
                 ICONT(1)=0
                 ICONT(2)=0
                 ICONT(3)=0
                 READ=.TRUE.
            ENDIF
       ENDIF
**  Ensure that the data type matches the declared type.
       DO 40 I=1,IMAX
       IF(((ICONT(I).EQ.2.OR.ICONT(I).EQ.3.OR.ICONT(I).EQ.4).AND.
     -      (IDATA.NE.0.AND.IDATA.NE.2.AND.IDATA.NE.10)).OR.
     -      (ICONT(I).EQ.5.AND.(IDATA.NE.0.AND.IDATA.NE.5)).OR.
     -      ((ICONT(I).EQ.6.OR.ICONT(I).EQ.7.OR.ICONT(I).EQ.8).AND.
     -      (IDATA.NE.0.AND.IDATA.NE.6)).OR.
     -      ((ICONT(I).EQ.9.OR.ICONT(I).EQ.-9).AND.
     -      (IDATA.NE.0.AND.IDATA.NE.9)))THEN
            PRINT *,' !!!!!! MAPFM4 WARNING : Field ',I,' of file ',
     -           FMAP(1:NCMAP),' does not contain the declared',
     -           ' kind of data; skipped.'
            ICONT(I)=0
       ENDIF
40     CONTINUE
*** Read the number of points and number of triangles.
       CALL INPGET
       CALL INPNUM(NWORD)
*   Verify the triangle and points count.
       CALL INPCHK(2,1,IFAIL1)
       CALL INPCHK(4,1,IFAIL2)
       CALL INPRDI(2,MTRIAN,0)
       CALL INPRDI(4,MPOINT,0)
       IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.NWORD.NE.4.OR.
     -      MTRIAN.NE.NTRIAN)THEN
            PRINT *,' !!!!!! MAPFM4 WARNING : The file ',
     -           FMAP(1:NCMAP),' has an incorrect number'//
     -           ' of triangles or points; not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM4 DEBUG   : Number'',
     -      '' of field triangles: '',I5)') NTRIAN
*   Progress printing.
       CALL PROFLD(2,'Triangles',REAL(NTRIAN))
*** Switch back to regular input.
       CALL INPSWI('RESTORE')
*   See whether any item is left.
       IF(.NOT.READ)THEN
            PRINT *,' !!!!!! MAPFM4 WARNING : The file ',
     -           FMAP(1:NCMAP),' contains no useable'//
     -           ' information; file not read.'
            RETURN
       ENDIF
*** Loop over the triangles.
       DO 10 I=1,NTRIAN
       IF(I.EQ.MAX(1,NTRIAN/100)*(I/MAX(1,NTRIAN/100)))
     -      CALL PROSTA(2,REAL(I))
*** Read the line with the word "Tri".
20     CONTINUE
       READ(12,'(A80)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       IF(STRING(1:8).EQ.'EmptyTri')THEN
            READ(STRING,'(8X,BN,I10)',ERR=2010,IOSTAT=IOS) ITRIAN
            GOTO 20
       ELSEIF(STRING(1:3).NE.'Tri')THEN
            PRINT *,' !!!!!! MAPFM4 WARNING : Found an unexpected'//
     -           ' header line: ',STRING(1:20),' ...'
            GOTO 20
       ENDIF
*   Read the triangle number.
       READ(STRING,'(3X,BN,I10)',ERR=2010,IOSTAT=IOS) ITRIAN
*   Ensure this number is in range.
       IF(ITRIAN.LT.1.OR.ITRIAN.GT.NTRIAN)THEN
            PRINT *,' !!!!!! MAPFM4 WARNING : Triangle  number ',
     -           ITRIAN,' out of range in ',FMAP(1:NCMAP)
            READ(12,'(//////)',ERR=2010,END=2000,IOSTAT=IOS)
            GOTO 10
       ENDIF
*** Read scalar field values over the triangle.
       IF(SCALAR)THEN
**  Can be either a potential, first read.
            IF(ICONT(1).EQ.5)THEN
                 READ(12,*,END=2000,ERR=2010,IOSTAT=IOS)
     -                (VMAP(ITRIAN,LOOKUP(K)),K=1,6)
**   Then keep track of potential range.
                 IF(I.EQ.1)THEN
                      VMMIN=VMAP(ITRIAN,1)
                      VMMAX=VMAP(ITRIAN,1)
                 ENDIF
                 VMMIN=MIN(VMMIN,
     -                VMAP(ITRIAN,1),VMAP(ITRIAN,2),VMAP(ITRIAN,3),
     -                VMAP(ITRIAN,4),VMAP(ITRIAN,5),VMAP(ITRIAN,6))
                 VMMAX=MAX(VMMAX,
     -                VMAP(ITRIAN,1),VMAP(ITRIAN,2),VMAP(ITRIAN,3),
     -                VMAP(ITRIAN,4),VMAP(ITRIAN,5),VMAP(ITRIAN,6))
**  Or a dielectricum, first read.
            ELSEIF(ICONT(1).EQ.9)THEN
                 READ(12,*,END=2000,ERR=2010,IOSTAT=IOS)
     -                (TEMP(K),K=1,6)
*   Average the epsilons/conductivity.
                 SUM=0
                 DO 30 J=1,6
                 SUM=SUM+TEMP(J)
30               CONTINUE
                 SUM=SUM/(600*EPS0)
*   Identify the material.
                 IEPS=-1
                 DO 80 J=1,NEPS
                 IF(ABS(SUM-EPSMAT(J)).LT.1E-4*(ABS(SUM)+
     -                ABS(EPSMAT(J))))IEPS=J
80               CONTINUE
                 IF(IEPS.LT.0.AND.NEPS.GE.MXEPS)THEN
                      PRINT *,' !!!!!! MAPFM4 WARNING : Unable'//
     -                     ' to store a dielectricum from file ',
     -                     FMAP(1:NCMAP),'; file not read.'
                      RETURN
                 ELSEIF(IEPS.LT.0)THEN
                      NEPS=NEPS+1
                      IEPS=NEPS
                      EPSMAT(IEPS)=SUM
                      IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM4'',
     -                     '' DEBUG   : Adding dielectricum with'',
     -                     '' eps='',E10.3,''.'')') EPSMAT(IEPS)
                 ENDIF
                 MATMAP(ITRIAN)=IEPS
            ENDIF
*** Read vectorial field values over the triangle.
       ELSE
*   Take care of knowing |D| either from Ex or by summing.
            IF(MAPFLG(10))DCOMP=EXMAP(ITRIAN,1)
*   Initialise the D sums.
            DX=0
            DY=0
            DZ=0
*   Read each of the 6 nodes
            DO 120 K=1,6
            READ(12,*,END=2000,ERR=2010,IOSTAT=IOS) (TEMP(J),J=1,3)
*   Assign each of the 3 fields
            DO 130 J=1,3
*   E and EW fields
            IF(ICONT(J).EQ.2)THEN
                 IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                      EXMAP(ITRIAN,LOOKUP(K))=TEMP(J)
                 ELSEIF(IDATA.EQ.10)THEN
                      EWXMAP(ITRIAN,LOOKUP(K),IWMAP)=TEMP(J)
                 ENDIF
            ELSEIF(ICONT(J).EQ.3)THEN
                 IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                      EYMAP(ITRIAN,LOOKUP(K))=TEMP(J)
                 ELSEIF(IDATA.EQ.10)THEN
                      EWYMAP(ITRIAN,LOOKUP(K),IWMAP)=TEMP(J)
                 ENDIF
            ELSEIF(ICONT(J).EQ.4)THEN
                 IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                      EZMAP(ITRIAN,LOOKUP(K))=TEMP(J)
                 ELSEIF(IDATA.EQ.10)THEN
                      EWZMAP(ITRIAN,LOOKUP(K),IWMAP)=TEMP(J)
                 ENDIF
*   B field
            ELSEIF(ICONT(J).EQ.6)THEN
                 BXMAP(ITRIAN,LOOKUP(K))=TEMP(J)
            ELSEIF(ICONT(J).EQ.7)THEN
                 BYMAP(ITRIAN,LOOKUP(K))=TEMP(J)
            ELSEIF(ICONT(J).EQ.8)THEN
                 BZMAP(ITRIAN,LOOKUP(K))=TEMP(J)
*   D field.
            ELSEIF(ICONT(J).EQ.-9)THEN
                 IF(J.EQ.1.AND.LOOKUP(K).GE.1.AND.LOOKUP(K).LE.3)THEN
                      DX=DX+TEMP(1)
                      DY=DY+TEMP(2)
                      DZ=DZ+TEMP(3)
                 ENDIF
            ELSEIF(ICONT(J).NE.0)THEN
                 PRINT *,' !!!!!! MAPFM4 WARNING : Found an unknown'//
     -                ' contents flag; ignored.'
            ENDIF
*   Next field and node.
130         CONTINUE
120         CONTINUE
*   Compute the total D field.
            IF(ICONT(2).EQ.-9)DCOMP=DX**2+DY**2+DZ**2
**  Dielectricum identification via D/E comparison.
            IF(((MAPFLG(2).OR.MAPFLG(3).OR.MAPFLG(4)).AND.
     -           (.NOT.MAPFLG(9)).AND.
     -           (ICONT(1).EQ.-9.OR.ICONT(2).EQ.-9.OR.
     -                ICONT(3).EQ.-9)).OR.
     -           (MAPFLG(10).AND.(.NOT.MAPFLG(9)).AND.
     -           ICONT(1).GE.1.AND.ICONT(1).LE.3))THEN
                 IEPS=-1
                 ECOMP=(EXMAP(ITRIAN,1)+EXMAP(ITRIAN,2)+
     -                EXMAP(ITRIAN,3))**2+
     -                (EYMAP(ITRIAN,1)+EYMAP(ITRIAN,2)+
     -                EYMAP(ITRIAN,3))**2+
     -                (EZMAP(ITRIAN,1)+EZMAP(ITRIAN,2)+
     -                EZMAP(ITRIAN,3))**2
                 DO 60 J=1,NEPS
                 IF(ABS(ECOMP*(10000*EPS0*EPSMAT(J))**2-DCOMP).LE.1E-4*
     -                (ABS(ECOMP*(10000*EPS0*EPSMAT(J))**2)+
     -                ABS(DCOMP)))IEPS=J
60               CONTINUE
                 IF(ECOMP.LE.0.AND.DCOMP.GT.0)THEN
                      PRINT *,' !!!!!! MAPFM4 WARNING : Found'//
     -                     ' a dielectric constant of 0; skipped.'
                 ELSEIF(IEPS.LT.0.AND.NEPS.GE.MXEPS)THEN
                      PRINT *,' !!!!!! MAPFM4 WARNING : Unable'//
     -                     ' to store a dielectricum from file ',
     -                     FMAP(1:NCMAP),'; file not read.'
                      RETURN
                 ELSEIF(IEPS.LT.0)THEN
                      NEPS=NEPS+1
                      IEPS=NEPS
                      IF(ECOMP.LE.0)THEN
                           PRINT *,' ------ MAPFM4 MESSAGE : Unable'//
     -                          ' to determine epsilon in an E=0'//
     -                          ' triangle; epsilon set to 0.'
                           EPSMAT(IEPS)=0
                      ELSE
                           EPSMAT(IEPS)=SQRT(DCOMP/ECOMP)/(10000*EPS0)
                      ENDIF
                      IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM4'',
     -                     '' DEBUG   : Adding dielectricum with'',
     -                     '' eps='',E10.3,''.'')') EPSMAT(IEPS)
                 ENDIF
                 MATMAP(ITRIAN)=IEPS
                 NEWEPS=.TRUE.
*  Otherwise store the field.
            ELSEIF((ICONT(1).EQ.-9.OR.ICONT(2).EQ.-9.OR.
     -           ICONT(3).EQ.-9).AND.(.NOT.MAPFLG(2)))THEN
                 EXMAP(ITRIAN,1)=DCOMP
            ENDIF
       ENDIF
10     CONTINUE
*** Be sure something has been read.
2000   CONTINUE
       IF(ITRIAN.NE.NMAP)THEN
            PRINT *,' !!!!!! MAPFM4 WARNING : Number of'//
     -           ' triangles in ',FMAP(1:NCMAP),' does not'//
     -           ' match current mesh; not read.'
            RETURN
       ENDIF
*** Materials have been defined is NEWEPS is set.
       IF(NEWEPS)MAPFLG(9)=.TRUE.
*** Scale electric fields if they have been entered.
       IF(ICONT(1).EQ.2.AND.(IDATA.EQ.0.OR.IDATA.EQ.2))THEN
            DO 200 I=1,NMAP
            DO 210 J=1,6
            EXMAP(I,J)=EXMAP(I,J)/100
            EYMAP(I,J)=EYMAP(I,J)/100
            EZMAP(I,J)=EZMAP(I,J)/100
210         CONTINUE
200         CONTINUE
       ELSEIF(ICONT(1).EQ.2.AND.IDATA.EQ.10)THEN
            DO 220 I=1,NMAP
            DO 230 J=1,6
            EWXMAP(I,J,IWMAP)=EWXMAP(I,J,IWMAP)/100
            EWYMAP(I,J,IWMAP)=EWYMAP(I,J,IWMAP)/100
            EWZMAP(I,J,IWMAP)=EWZMAP(I,J,IWMAP)/100
230         CONTINUE
220         CONTINUE
       ENDIF
*** Flag those elements which have been defined.
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
2005   CONTINUE
       PRINT *,' !!!!!! MAPFM4 WARNING : Premature end of file'//
     -      ' reading a mesh file; map not available.'
       IF(LDEBUG)CALL INPIOS(IOS)
       CLOSE(12,ERR=2030)
       RETURN
2010   CONTINUE
       PRINT *,' !!!!!! MAPFM4 WARNING : Error reading field map'//
     -      ' file ',FMAP(1:NCMAP),'; map not available.'
       IF(LDEBUG)CALL INPIOS(IOS)
       RETURN
2015   CONTINUE
       PRINT *,' !!!!!! MAPFM4 WARNING : Error reading a mesh'//
     -      ' file ; map not available.'
       IF(LDEBUG)CALL INPIOS(IOS)
       CLOSE(12,ERR=2030)
       RETURN
2030   CONTINUE
       PRINT *,' !!!!!! MAPFM4 WARNING : Error closing field map'//
     -      ' file ',FMAP(1:NCMAP),'; map not available.'
       IF(LDEBUG)CALL INPIOS(IOS)
       RETURN
       END
