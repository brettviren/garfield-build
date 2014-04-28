CDECK  ID>, MAPFMS.
       SUBROUTINE MAPFMS
*-----------------------------------------------------------------------
*   MAPFMS - Writes the field map data in binary format.
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
       INTEGER I,J,K,IOS,NWORD,NC,IFAIL
       CHARACTER*(MXNAME) FILE
*** Make sure there is a field map.
       IF(NMAP.LE.0.OR..NOT.MAPFLG(1))THEN
            PRINT *,' !!!!!! MAPFMS WARNING : There is currently no'//
     -           ' valid field map in memory; map not saved.'
            RETURN
       ENDIF
*** Get hold of the file name.
       CALL INPNUM(NWORD)
*   Make sure there is at least one argument.
       IF(NWORD.NE.2)THEN
            PRINT *,' !!!!!! MAPFMS WARNING : SAVE-FIELD-MAP takes 1'//
     -           ' argument (a dataset name); map will not be saved.'
            RETURN
       ENDIF
       CALL INPSTR(2,2,FILE,NC)
*   Check the length.
       IF(NC.GT.MXNAME)PRINT *,' !!!!!! MAPFMS WARNING : The file'//
     -      ' name is truncated to MXNAME (=',MXNAME,') characters.'
       NC=MIN(NC,MXNAME)
*** Open the file for sequential binary write.
       CALL DSNOPN(FILE,NC,12,'WRITE-BINARY',IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! MAPFMS WARNING : Opening '//FILE(1:NC)//
     -           ' failed ; the field map will not be written.'
            RETURN
       ENDIF
*** Write the version number.
       WRITE(12,ERR=2010,IOSTAT=IOS) 8
*** Write # triangles, map order, availability, 3D, plot flag.
       WRITE(12,ERR=2010,IOSTAT=IOS) NMAP,MAPORD,
     -      (MAPFLG(I),I=1,10+4*MXWMAP),MAPTYP,LMAPPL,NWMAP
*** Write the triangles or tetrahedrons, dimensions and periodicities.
       IF(MAPFLG(1))WRITE(12,ERR=2010,IOSTAT=IOS)
     -      ((XMAP(I,J),I=1,NMAP),J=1,10),
     -      ((YMAP(I,J),I=1,NMAP),J=1,10),
     -      ((ZMAP(I,J),I=1,NMAP),J=1,10),
     -      (ELMDGN(I),I=1,NMAP),
     -      XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,
     -      XFMOFF,YFMOFF,ZFMOFF,
     -      SETAX,SETAY,SETAZ,SX,SY,SZ,PERX,PERY,PERZ,LSFDER,
     -      PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,PERRX,PERRY,PERRZ
*   The (Ex,Ey,Ez) field, if available.
       IF(MAPFLG(2))WRITE(12,ERR=2010,IOSTAT=IOS)
     -      ((EXMAP(I,J),I=1,NMAP),J=1,10)
       IF(MAPFLG(3))WRITE(12,ERR=2010,IOSTAT=IOS)
     -      ((EYMAP(I,J),I=1,NMAP),J=1,10)
       IF(MAPFLG(4))WRITE(12,ERR=2010,IOSTAT=IOS)
     -      ((EZMAP(I,J),I=1,NMAP),J=1,10)
*   The potential and potential range, if available.
       IF(MAPFLG(5))WRITE(12,ERR=2010,IOSTAT=IOS)
     -      ((VMAP(I,J),I=1,NMAP),J=1,10),VMMIN,VMMAX
*   The (Bx,By,Bz) field, if available.
       IF(MAPFLG(6))WRITE(12,ERR=2010,IOSTAT=IOS)
     -      ((BXMAP(I,J),I=1,NMAP),J=1,10)
       IF(MAPFLG(7))WRITE(12,ERR=2010,IOSTAT=IOS)
     -      ((BYMAP(I,J),I=1,NMAP),J=1,10)
       IF(MAPFLG(8))WRITE(12,ERR=2010,IOSTAT=IOS)
     -      ((BZMAP(I,J),I=1,NMAP),J=1,10)
*   The material map, if available.
       IF(MAPFLG(9))WRITE(12,ERR=2010,IOSTAT=IOS)
     -      (MATMAP(I),I=1,NMAP)
*   The weighting (Ex,Ey,Ez) field and label, if available.
       DO 10 K=1,NWMAP
       IF(MAPFLG(10+4*K-3))WRITE(12,ERR=2010,IOSTAT=IOS)
     -      ((EWXMAP(I,J,K),I=1,NMAP),J=1,10)
       IF(MAPFLG(11+4*K-3))WRITE(12,ERR=2010,IOSTAT=IOS)
     -      ((EWYMAP(I,J,K),I=1,NMAP),J=1,10)
       IF(MAPFLG(12+4*K-3))WRITE(12,ERR=2010,IOSTAT=IOS)
     -      ((EWZMAP(I,J,K),I=1,NMAP),J=1,10)
       IF(MAPFLG(13+4*K-3))WRITE(12,ERR=2010,IOSTAT=IOS)
     -      ((VWMAP(I,J,K),I=1,NMAP),J=1,10)
       IF(MAPFLG(10+4*K-3).OR.MAPFLG(11+4*K-3).OR.
     -      MAPFLG(12+4*K-3).OR.MAPFLG(13+4*K-3))
     -      WRITE(12,ERR=2010,IOSTAT=IOS) EWSTYP(K)
10     CONTINUE
*** Write the number of materials and the drift medium.
       WRITE(12,ERR=2010,IOSTAT=IOS) NEPS,IDRMAT
*** Write the material table.
       WRITE(12,ERR=2010,IOSTAT=IOS) (EPSMAT(I),I=1,NEPS),
     -      (EPSSUR(I),I=1,NEPS)
*** Close the file.
       CLOSE(UNIT=12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
*** Register file access.
       CALL DSNLOG(FILE(1:NC),'Field map ','Sequential',
     -      'Bin Write ')
       RETURN
*** Handle I/O errors.
2010   CONTINUE
       PRINT *,' !!!!!! MAPFMS WARNING : Error during binary write'//
     -      ' to file '//FILE(1:NC)//'; deleting file.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='DELETE',ERR=2030,IOSTAT=IOS)
       RETURN
2030   CONTINUE
       PRINT *,' !!!!!! MAPFMS WARNING : Error closing '//FILE(1:NC)//
     -      ' after binary write; effect not known.'
       CALL INPIOS(IOS)
       END
