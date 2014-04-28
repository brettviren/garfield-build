CDECK  ID>, MAPFMB.
       SUBROUTINE MAPFMB(FMAP,NCMAP,IDATA,IWMAP,DELBKG,UNITD,IFAIL)
*-----------------------------------------------------------------------
*   MAPFMB - Reads Ansys 2nd order serendipity quadrilaterals (which
*            do not need to have straight sides). Element 121.
*   (Last changed on  8/ 6/11.)
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
       INTEGER IEPS,NUSE,IWMAP,
     -      I,J,K,NCMAP,IFAIL,IFAIL1,
     -      IT1,IT2,IT3,IT4,IT5,IT6,IT7,IT8,
     -      ITETRA,IOS,INPCMP,NCDIR,INODE,
     -      IDATA,NCAUX,IEND,IP,NTETRA,NPOINT,NDEGEN,
     -      LOOKUP(8),NDELET,IMAT,IDUM,NMAT,NTEMP
       REAL XP,YP,ZP,VOLT,UNITD,VALUE,OHMMAT(MXEPS),DUMMY
       CHARACTER*4 FIELD
       CHARACTER*(*) FMAP
       CHARACTER*(MXNAME) FAUX,FDIR
       CHARACTER*80 STRING
       LOGICAL DELBKG,FIRST,LCOORD,EXIST,LMPFIL
       EXTERNAL INPCMP
       SAVE NTETRA,NPOINT,LOOKUP
       DATA NTETRA/0/, NPOINT/0/
*** Lookup table.
       DATA LOOKUP/1, 2, 3, 4, 5, 6, 7, 8/
**** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MAPFMB ///'
*** Assume that this will fail.
       IFAIL=1
*** Make sure the file names are not too long.
       IF(NCMAP.GT.MXNAME)THEN
            PRINT *,' !!!!!! MAPFMB WARNING : Field map file name ',
     -           FMAP(1:NCMAP),' is too long; not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*** Initialise.
       NDELET=0
*** Check for mesh files - or guess mesh name if there is no mesh yet.
       IF(IDATA.EQ.1.OR..NOT.MAPFLG(1))THEN
*   Locate the directory name.
            IEND=0
            DO 10 I=NCMAP,1,-1
            IF(FMAP(I:I).NE.' '.AND.IEND.EQ.0)IEND=I
            IF(FMAP(I:I).EQ.'/')THEN
                 FDIR=FMAP(1:I)
                 NCDIR=I
                 GOTO 20
            ENDIF
10          CONTINUE
            FDIR='./'
            NCDIR=2
20          CONTINUE
**  Skip the mesh decoding if this has already been done.
       ELSE
            GOTO 1000
       ENDIF
*   Close the current file, re-open later.
       CLOSE(12,ERR=2030,IOSTAT=IOS)
**  Construct the material properties file name.
       FAUX=FDIR(1:NCDIR)//'MPLIST.lis'
       NCAUX=NCDIR+10
*   Progress.
       CALL PROFLD(2,'Materials', -1.0)
       CALL PROSTA(2,-1.0)
*   Assume the file does not exist
       LMPFIL=.FALSE.
*   Assume we have to index the epsilons.
       MATSRC='INDEX'
*   Set the flag that the materials are defined.
       MAPFLG(9)=.FALSE.
*   Initialise the material table.
       DO 160 IEPS=1,MXEPS
       OHMMAT(IEPS)=-1
       EPSMAT(IEPS)=-1
160    CONTINUE
*   See whether the file exists.
       CALL DSNINQ(FAUX,NCAUX,EXIST)
*   Open the mesh file.
       IF(EXIST)THEN
            CALL DSNOPN(FAUX,NCAUX,12,'READ-FILE',IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFMB WARNING : Unable to open the'//
     -                ' material file '//FAUX(1:NCAUX)//'; not read.'
                 GOTO 1010
            ENDIF
*   Record the opening.
            CALL DSNLOG(FAUX(1:NCAUX),'Material  ','Sequential',
     -           'Read only ')
*   Read the number of materials.
            READ(12,'(/26X,I8)',END=2000,ERR=2010,IOSTAT=IOS) NMAT
            IF(NMAT.GT.MXEPS)THEN
                 PRINT *,' !!!!!! MAPFMB WARNING : Material file '//
     -                FAUX(1:NCAUX)//' contains more than MXEPS'//
     -                ' materials; file not read.'
                 GOTO 1010
            ENDIF
*   Loop over the materials
            IMAT=0
140         CONTINUE
            READ(12,'(A)',END=1020,ERR=2010,IOSTAT=IOS) STRING
*   Deal with version 11.x format.
            IF(INDEX(STRING,'PROPERTY TABLE').NE.0)THEN
                 READ(STRING,'(16X,A4,6X,BN,I8,14X,I10)',ERR=2010,
     -                IOSTAT=IOS) FIELD,IMAT,NTEMP
                 IF(LDEBUG)WRITE(LUNOUT,'(
     -                ''  ++++++ MAPFMA DEBUG   : Field: '',A,
     -                '', material: '',I3,'', points: '',I5)')
     -                FIELD,IMAT,NTEMP
                 IF(NTEMP.NE.1)THEN
                      PRINT *,' !!!!!! MAPFMA WARNING : Material'//
     -                     ' file '//FAUX(1:NCAUX)//' has temperature'//
     -                     ' dependent '//FIELD//' data for material ',
     -                     IMAT,'; not read.'
                      GOTO 1010
                 ELSEIF(IMAT.LT.1.OR.IMAT.GT.NMAT)THEN
                      PRINT *,' !!!!!! MAPFMA WARNING : Material'//
     -                     ' file '//FAUX(1:NCAUX)//' contains out of',
     -                     ' range index ',IMAT,'; file not read.'
                      GOTO 1010
                 ELSEIF(FIELD.NE.'RSVX'.AND.FIELD.NE.'PERX')THEN
                      GOTO 140
                 ENDIF
                 READ(12,'(/E15.8,E12.5)',END=2000,ERR=2010,IOSTAT=IOS)
     -                DUMMY,VALUE
                 IF(FIELD.EQ.'RSVX')THEN
                      OHMMAT(IMAT)=VALUE
                 ELSEIF(FIELD.EQ.'PERX')THEN
                      EPSMAT(IMAT)=VALUE
                 ELSE
                      PRINT *,' !!!!!! MAPFMA WARNING : Material'//
     -                     ' file '//FAUX(1:NCAUX)//' refers to the'//
     -                     ' unknown property '//FIELD//'; ignored.'
                 ENDIF
*   Deal with version 12.x format.
            ELSEIF(INDEX(STRING,'MATERIAL NUMBER').NE.0)THEN
                 READ(STRING,'(16X,BN,I8)') IMAT
            ELSEIF(STRING(1:10).EQ.'      TEMP')THEN
                 FIELD=STRING(19:22)
                 READ(12,'(A)',END=1020,ERR=2010,IOSTAT=IOS) STRING
                 READ(STRING,'(14X,E14.8)') VALUE
                 IF(FIELD.EQ.'RSVX')THEN
                      OHMMAT(IMAT)=VALUE
                 ELSEIF(FIELD.EQ.'PERX')THEN
                      EPSMAT(IMAT)=VALUE
                 ELSE
                      PRINT *,' !!!!!! MAPFMA WARNING : Material'//
     -                     ' file '//FAUX(1:NCAUX)//' refers to the'//
     -                     ' unknown property '//FIELD//'; ignored.'
                 ENDIF
            ENDIF
            GOTO 140
*   End of file reached.
1020        CONTINUE
*   Apparently, the epsilons are defined.
            NEPS=NMAT
            LMPFIL=.TRUE.
*   Register the origin of the materials.
            MATSRC='EPSILON'
*   Set the flag that the materials are defined.
            MAPFLG(9)=.TRUE.
*   Resume here if reading fails.
1010        CONTINUE
            CLOSE(12,ERR=2030,IOSTAT=IOS)
       ENDIF
**  Construct the mesh file name.
       FAUX=FDIR(1:NCDIR)//'ELIST.lis'
       NCAUX=NCDIR+9
*   Open the mesh file.
       CALL DSNOPN(FAUX,NCAUX,12,'READ-FILE',IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! MAPFMB WARNING : Unable to open the'//
     -           ' mesh file '//FAUX(1:NCAUX)//'; map not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Record the opening.
       CALL DSNLOG(FAUX(1:NCAUX),'Mesh      ','Sequential',
     -      'Read only ')
**  Loop over the quadrilaterals, with progress printing.
       CALL PROFLD(2,'Elements', -1.0)
       CALL PROSTA(2,-1.0)
       NTETRA=0
       NPOINT=0
       NDEGEN=0
30     CONTINUE
*   Get a line
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       CALL CRLF(STRING)
*   Skip empty and header lines (includes correction from Yang Zhang)
       IF(STRING.EQ.' '.OR.
     -      STRING.EQ.'1'.OR.
     -      INDEX(STRING,'ANSYS').GT.0.OR.
     -      INDEX(STRING,'VERSION').GT.0.OR.
     -      INDEX(STRING,'LIST').GT.0.OR.
     -      INDEX(STRING,'ELEM').GT.0)GOTO 30
*   Increment element counter
       IF(NTETRA.GE.MXMAP)THEN
            PRINT *,' !!!!!! MAPFMB WARNING : Number of'//
     -           ' quadrilaterals in '//FAUX(1:NCAUX)//
     -           ' exceeds compilation limit; file not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
       NTETRA=NTETRA+1
*   Read the element information
       BACKSPACE(12,ERR=2040)
       READ(12,*,END=2005,ERR=2010,IOSTAT=IOS)
     -      ITETRA,IMAT,IDUM,IDUM,IDUM,IDUM,
     -      IT1,IT2,IT3,IT4,IT5,IT6,IT7,IT8
       IF(ITETRA.NE.NTETRA.AND..NOT.DELBKG)THEN
            PRINT *,' !!!!!! MAPFMB WARNING : Numbering mismatch'//
     -           ' at quadrilateral ',NTETRA,' in '//FAUX(1:NCAUX)//
     -           ' ; file not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Store the material flag.
       IF(.NOT.LMPFIL)THEN
            IF(IMAT.LT.1.OR.IMAT.GT.MXEPS)THEN
                 PRINT *,' !!!!!! MAPFMB WARNING : Found out of range'//
     -                ' material identifier ',IMAT,' in mesh file '//
     -                FAUX(1:NCAUX)//'; file not read.'
                 CALL INPSWI('RESTORE')
                 RETURN
            ELSE
                 MATMAP(NTETRA)=IMAT
                 EPSMAT(IMAT)=REAL(IMAT)
                 OHMMAT(IMAT)=REAL(IMAT)
                 IF(NEPS.LT.IMAT)NEPS=IMAT
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFMB DEBUG   :'',
     -                '' Adding dielectricum '',I3,'' with eps='',E10.3,
     -                ''.'')') IMAT,EPSMAT(IMAT)
            ENDIF
       ELSE
            IF(IMAT.LT.1.OR.IMAT.GT.NEPS)THEN
                 PRINT *,' !!!!!! MAPFMB WARNING : Found out of range'//
     -                ' material identifier ',IMAT,' in mesh file '//
     -                FAUX(1:NCAUX)//'; file not read.'
                 CALL INPSWI('RESTORE')
                 RETURN
            ELSE
                 MATMAP(NTETRA)=IMAT
            ENDIF
       ENDIF
*   Skip quadrilaterals which are background.
       IF(DELBKG.AND.OHMMAT(MATMAP(NTETRA)).EQ.0.0)THEN
            NDELET=NDELET+1
            NTETRA=NTETRA-1
            GOTO 30
       ENDIF
*   Check for degeneracy.
       IF(IT3.EQ.IT4.AND.IT4.EQ.IT7)THEN
            NDEGEN=NDEGEN+1
            ELMDGN(NTETRA)=.TRUE.
       ELSE
            ELMDGN(NTETRA)=.FALSE.
       ENDIF
*   Store the reference pointers temporarily in Ex (point) and Ey (V).
       IF(ELMDGN(NTETRA))THEN
            EXMAP(NTETRA,LOOKUP(1))=IT1
            EXMAP(NTETRA,LOOKUP(2))=IT2
            EXMAP(NTETRA,LOOKUP(3))=IT3
            EXMAP(NTETRA,LOOKUP(4))=IT5
            EXMAP(NTETRA,LOOKUP(5))=IT8
            EXMAP(NTETRA,LOOKUP(6))=IT6
            EXMAP(NTETRA,LOOKUP(7))=IT4
            EXMAP(NTETRA,LOOKUP(8))=IT7
            EYMAP(NTETRA,LOOKUP(1))=IT1
            EYMAP(NTETRA,LOOKUP(2))=IT2
            EYMAP(NTETRA,LOOKUP(3))=IT3
            EYMAP(NTETRA,LOOKUP(4))=IT5
            EYMAP(NTETRA,LOOKUP(5))=IT8
            EYMAP(NTETRA,LOOKUP(6))=IT6
            EYMAP(NTETRA,LOOKUP(7))=IT4
            EYMAP(NTETRA,LOOKUP(8))=IT7
       ELSE
            EXMAP(NTETRA,LOOKUP(1))=IT1
            EXMAP(NTETRA,LOOKUP(2))=IT2
            EXMAP(NTETRA,LOOKUP(3))=IT3
            EXMAP(NTETRA,LOOKUP(4))=IT4
            EXMAP(NTETRA,LOOKUP(5))=IT5
            EXMAP(NTETRA,LOOKUP(6))=IT6
            EXMAP(NTETRA,LOOKUP(7))=IT7
            EXMAP(NTETRA,LOOKUP(8))=IT8
            EYMAP(NTETRA,LOOKUP(1))=IT1
            EYMAP(NTETRA,LOOKUP(2))=IT2
            EYMAP(NTETRA,LOOKUP(3))=IT3
            EYMAP(NTETRA,LOOKUP(4))=IT4
            EYMAP(NTETRA,LOOKUP(5))=IT5
            EYMAP(NTETRA,LOOKUP(6))=IT6
            EYMAP(NTETRA,LOOKUP(7))=IT7
            EYMAP(NTETRA,LOOKUP(8))=IT8
       ENDIF
*   Keep track of the number of points
       NPOINT=MAX(NPOINT,IT1,IT2,IT3,IT4,IT5,IT6,IT7,IT8)
*   Next block
       GOTO 30
*   Close the mesh file.
2000   CONTINUE
       CLOSE(12,ERR=2030,IOSTAT=IOS)
**  Construct the name of the points file.
       FAUX=FDIR(1:NCDIR)//'NLIST.lis'
       NCAUX=NCDIR+9
*   Open the points file.
       CALL DSNOPN(FAUX,NCAUX,12,'READ-FILE',IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! MAPFMB WARNING : Unable to open the'//
     -           ' points file '//FAUX(1:NCAUX)//'; map not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Record the opening.
       CALL DSNLOG(FAUX(1:NCAUX),'Points    ','Sequential',
     -      'Read only ')
**  Loop over the quadrilaterals, with progress printing.
       CALL PROFLD(2,'Nodes',REAL(NPOINT))
       FIRST=.TRUE.
       LCOORD=.TRUE.
40     CONTINUE
*   Read the data line
       READ(12,'(A)',END=2001,ERR=2010,IOSTAT=IOS) STRING
       CALL CRLF(STRING)
*   Two possible formats for this file: NLIST,,,,COORD or NLIST
       IF(INDEX(STRING,'THXY    THYZ    THZX').NE.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFMB DEBUG   :'',
     -           '' Switching LCOORD off.'')')
            LCOORD=.FALSE.
       ENDIF
*   Skip empty and header lines
       IF(STRING.EQ.' '.OR.STRING.EQ.''.OR.
     -      INDEX(STRING,'LIST').GT.0.OR.
     -      INDEX(STRING,'SORT').GT.0.OR.
     -      INDEX(STRING,'NODE').GT.0.OR.
     -      INDEX(STRING,'ANSYS').GT.0.OR.
     -      INDEX(STRING,'VERSION').GT.0.OR.
     -      INDEX(STRING,'FILE').GT.0.OR.
     -      INDEX(STRING,'Electric').GT.0)GOTO 40
*   Read the point coordinates and the reference to the mesh
       IF(LCOORD)THEN
            READ(STRING,'(I9,2X,3E20.12)',ERR=2010,IOSTAT=IOS)
     -           IP,XP,YP,ZP
       ELSE
            READ(STRING,'(I8,2X,3E12.5)',ERR=2010,IOSTAT=IOS)
     -           IP,XP,YP,ZP
       ENDIF
*   Progress printing
       IF(IP.EQ.MAX(1,NPOINT/100)*(IP/MAX(1,NPOINT/100)))
     -      CALL PROSTA(2,REAL(IP))
*   Convert from the a priori unknown Ansys unit to cm.
       XP=XP*UNITD
       YP=YP*UNITD
       ZP=ZP*UNITD
*   Store the quadrilateral parameters that refer to this point.
       NUSE=0
       DO 60 J=1,8
       DO 50 K=1,NTETRA
       IF(NINT(EXMAP(K,J)).EQ.IP)THEN
            NUSE=NUSE+1
            XMAP(K,J)=XP
            YMAP(K,J)=YP
            ZMAP(K,J)=ZP
            EXMAP(K,J)=-1
       ENDIF
50     CONTINUE
60     CONTINUE
*   If this point was not used, skip the rest.
       IF(NUSE.LE.0)GOTO 40
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
       GOTO 40
*   Close the file
2001   CONTINUE
       CLOSE(12,ERR=2030,IOSTAT=IOS)
*   End of reading, make sure that all node references are solved.
       CALL PROFLD(2,'Node check',-1.0)
       CALL PROSTA(2,0.0)
       DO 80 J=1,8
       DO 70 I=1,NTETRA
       IF(EXMAP(I,J).GE.0)THEN
            PRINT *,' !!!!!! MAPFMB WARNING : Unresolved references'//
     -           ' to points left in element table ; map rejected.'
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFMB DEBUG   :'',
     -           '' Element = '',I5,'', j = '',I2,'', point = '',I5)')
     -           I,J,NINT(EXMAP(I,J))
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
70     CONTINUE
80     CONTINUE
*   Store the number of quadrilaterals.
       NMAP=NTETRA
*   Set the flag that the mesh is now defined.
       MAPFLG(1)=.TRUE.
**  Print number of deleted quadrilaterals.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFMB DEBUG   :'',
     -      '' Quadrilaterals left: '',I6,'', deleted background: '',I6,
     -      '', nodes: '',I6)') NTETRA,NDELET,NPOINT
       IF(NDELET.NE.0)PRINT *,' ------ MAPFMB MESSAGE : Found ',NDELET,
     -      ' background quadrilaterals.'
       IF(NDEGEN.NE.0)PRINT *,' ------ MAPFMB MESSAGE : Found ',NDEGEN,
     -      ' degenerate quadrilaterals.'
**  In case this was an explicit mesh, return with success status.
       IF(IDATA.EQ.1)THEN
            IFAIL=0
            CALL INPSWI('RESTORE')
            RETURN
*   Otherwise, close the points file and re-open mesh file.
       ELSE
            CLOSE(12,ERR=2030,IOSTAT=IOS)
            CALL DSNOPN(FMAP,NCMAP,12,'READ-FILE',IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFMB WARNING : Re-opening the'//
     -                ' field map ',FMAP(1:NCMAP),
     -                ' failed ; map not read.'
                 CALL INPSWI('RESTORE')
                 RETURN
            ENDIF
*   Record the opening.
            CALL DSNLOG(FMAP(1:NCMAP),'Field map ','Sequential',
     -           'Re-read   ')
       ENDIF
*** Read the field map.
1000   CONTINUE
*   Progress printing.
       IF(IDATA.EQ.10)THEN
            CALL PROFLD(2,'Weighting field',REAL(NPOINT))
       ELSE
            CALL PROFLD(2,'Potentials',REAL(NPOINT))
       ENDIF
*   Loop over the nodes.
       FIRST=.TRUE.
90     CONTINUE
*   Read the data line.
       READ(12,'(A)',END=2002,ERR=2010,IOSTAT=IOS) STRING
       CALL CRLF(STRING)
*   Skip empty and header lines
       IF(STRING.EQ.' '.OR.
     -      STRING.EQ.'1'.OR.
     -      INDEX(STRING,'ANSYS').GT.0.OR.
     -      INDEX(STRING,'VERSION').GT.0.OR.
     -      INDEX(STRING,'NODAL').GT.0.OR.
     -      INDEX(STRING,'LOAD').GT.0.OR.
     -      INDEX(STRING,'MAXIMUM').GT.0.OR.
     -      INDEX(STRING,'VALUE').GT.0.OR.
     -      INDEX(STRING,'FILE').GT.0.OR.
     -      INDEX(STRING,'NODE').GT.0)GOTO 90
*   Read the name and the potential.
C       READ(STRING,'(I8,E13.5)',ERR=2010,IOSTAT=IOS)
C     -      INODE,VOLT
       READ(STRING,*,ERR=2010,IOSTAT=IOS) INODE,VOLT
*   Progress.
       IF(INODE.EQ.MAX(1,NPOINT/100)*(INODE/MAX(1,NPOINT/100)))
     -      CALL PROSTA(2,REAL(INODE))
*   Store the potential at the nodes that refer to this point.
       NUSE=0
       DO 110 J=1,8
       DO 100 K=1,NTETRA
       IF(NINT(EYMAP(K,J)).EQ.INODE)THEN
            NUSE=NUSE+1
            IF(IDATA.EQ.10)THEN
                 VWMAP(K,J,IWMAP)=VOLT
            ELSE
                 VMAP(K,J)=VOLT
            ENDIF
            EYMAP(K,J)=-EYMAP(K,J)
       ENDIF
100    CONTINUE
110    CONTINUE
*   If this point was not used, skip the rest.
       IF(NUSE.LE.0)GOTO 90
*   Then keep track of potential range.
       IF(FIRST)THEN
            FIRST=.FALSE.
            VMMIN=VOLT
            VMMAX=VOLT
       ELSE
            VMMIN=MIN(VMMIN,VOLT)
            VMMAX=MAX(VMMAX,VOLT)
       ENDIF
*   Next point.
       GOTO 90
*   Revert to normal reading.
2002   CONTINUE
       CALL INPSWI('RESTORE')
*   End of reading, make sure that all node references are solved.
       CALL PROFLD(2,'Potential check',-1.0)
       CALL PROSTA(2,0.0)
       DO 130 J=1,8
       DO 120 I=1,NTETRA
       IF(EYMAP(I,J).GE.0)THEN
            PRINT *,' !!!!!! MAPFMB WARNING : Unresolved references'//
     -           ' to potentials left in element table ; map rejected.'
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFMB DEBUG   :'',
     -           '' Element = '',I5,'', j = '',I2,'', point = '',I5)')
     -           I,J,NINT(EYMAP(I,J))
            RETURN
       ELSE
            EYMAP(I,J)=-EYMAP(I,J)
       ENDIF
120    CONTINUE
130    CONTINUE
*   Record that we've read a potential or a weighting potential.
       IF(IDATA.EQ.10)THEN
            MAPFLG(13+4*IWMAP-3)=.TRUE.
       ELSE
            MAPFLG(5)=.TRUE.
       ENDIF
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFMB DEBUG   :'',
     -      '' Potential range: '',E15.8,'' - '',E15.8,'' V.'')')
     -      VMMIN,VMMAX
*** Check that we have got all materials fully defined.
       DO 170 IEPS=1,NEPS
       IF(EPSMAT(IEPS).LT.0)THEN
            PRINT *,' !!!!!! MAPFMB WARNING : Material ',IEPS,
     -           ' has not been assigned a permittivity,'//
     -           '; file ',FMAP(1:NCMAP),' not read.'
            RETURN
       ENDIF
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFMB DEBUG   :'',
     -      '' Material '',I3,'': perx = '',E12.5,
     -      '', rsvx = '',E12.5)') IEPS,EPSMAT(IEPS),OHMMAT(IEPS)
170    CONTINUE
*** Seems to have worked, set error flag to OK and return.
       IFAIL=0
       MAPTYP=5
       RETURN
*** Handle error conditions.
2005   CONTINUE
       PRINT *,' !!!!!! MAPFMB WARNING : Premature end of file'//
     -      ' reading a field map file; map not available.'
       IF(LDEBUG)CALL INPIOS(IOS)
       CALL INPSWI('RESTORE')
       RETURN
2010   CONTINUE
       PRINT *,' !!!!!! MAPFMB WARNING : Error reading a field'//
     -      ' map file; map not available.'
       IF(LDEBUG)CALL INPIOS(IOS)
       CALL INPSWI('RESTORE')
       RETURN
2030   CONTINUE
       PRINT *,' !!!!!! MAPFMB WARNING : Error closing a field'//
     -      ' map file ; map not available.'
       IF(LDEBUG)CALL INPIOS(IOS)
       RETURN
2040   CONTINUE
       PRINT *,' !!!!!! MAPFMB WARNING : Error backspacing on a'//
     -      'field  map file ; map not available.'
       IF(LDEBUG)CALL INPIOS(IOS)
       CALL INPSWI('RESTORE')
       RETURN
       END
