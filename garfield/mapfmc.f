CDECK  ID>, MAPFMC.
       SUBROUTINE MAPFMC(FMAP,NCMAP,IDATA,IWMAP,DELBKG,IFAIL)
*-----------------------------------------------------------------------
*   MAPFMC - Reads Tosca 2nd order tetrahedrons (which do not need to
*            have straight sides).
*            File structure: sequence of blocks of the form
*            "    -1"
*            "    id"
*            (one or more lines of contents)
*            "    -1"
*            where id, called "dataset number" in Tosca-speak, specifies
*            the contents of the block:
*             151   Header (program version)
*             164   Units (only seems to have a length unit)
*            1700   Material Database Header (empty)
*            1703   Material Database Property (empty)
*            1705   Material Database Variable (empty)
*            1710   Material Database Material (mechanical data)
*             789   Physical Property Table (?)
*            2411   Nodes - Double Precision (node coordinates)
*            2412   Elements (mesh structure)
*             792   Boundary Conditions
*            2414   ? contains a potential solution
*   (Last changed on 27/10/11.)
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
       INTEGER IDATA,IEPS,NUSE,IWMAP,I,J,K,NCMAP,IFAIL,
     -      IT1,IT2,IT3,IT4,IT5,IT6,IT7,IT8,IT9,IT10,
     -      NTETRA,ITETRA,IOS,INPCMP,INPTYP,NPOINT,INODE,IELT,
     -      LOOKUP(10),NDELET,IMAT,IDUMMY
       REAL XP,YP,ZP,UNITD,OHMMAT(MXEPS),
     -      V1,V2,V3,V4,V5,V6,V7,V8,V9,V10
       CHARACTER*(*) FMAP
       CHARACTER*120 STRING
       LOGICAL DELBKG
       EXTERNAL INPCMP,INPTYP
       SAVE NTETRA,NPOINT,LOOKUP
       DATA NTETRA/0/, NPOINT/0/
*** Lookup table.
       DATA LOOKUP/1, 5, 2, 8, 3, 6, 7, 9, 10, 4/
**** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MAPFMC ///'
*** Assume that this will fail.
       IFAIL=1
*** Make sure the file names are not too long.
       IF(NCMAP.GT.MXNAME)THEN
            PRINT *,' !!!!!! MAPFMC WARNING : Field map file name ',
     -           FMAP(1:NCMAP),' is too long; not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*** Initialise.
       NDELET=0
*   Assume we have to index the epsilons.
       MATSRC='INDEX'
*   Set the flag that the materials are defined.
       MAPFLG(9)=.FALSE.
*   Initialise the material table.
       DO 160 IEPS=1,MXEPS
       OHMMAT(IEPS)=-1
       EPSMAT(IEPS)=-1
160    CONTINUE
*   Assume the unit is cm.
       UNITD=1.0
*** Move to the units: dataset 164
       CALL PROFLD(2,'Locate units', -1.0)
       CALL PROSTA(2,-1.0)
80     CONTINUE
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       IF(STRING(1:6).NE.'    -1')GOTO 80
81     CONTINUE
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       IF(STRING(1:6).EQ.'    -1')GOTO 81
       IF(STRING(1:6).NE.'   164')GOTO 80
*   Read the line
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       IF(INDEX(STRING,' M ').NE.0)THEN
            UNITD=100.0
       ELSEIF(INDEX(STRING,' CM ').NE.0)THEN
            UNITD=1.0
       ELSEIF(INDEX(STRING,' MM ').NE.0)THEN
            UNITD=0.1
       ELSEIF(INDEX(STRING,' MUM ').NE.0)THEN
            UNITD=0.0001
       ELSE
            PRINT *,' !!!!!! MAPFMC WARNING : Did not find a'//
     -           ' length unit in dataset 164; please report.'
            PRINT *,STRING(1:50)
       ENDIF
       PRINT *,' ------ MAPFMC MESSAGE : Length unit: ',UNITD,' cm.'
*** Rewind, then move to the points list.
       REWIND(UNIT=12,ERR=2050)
*** Move to the mesh structure part: dataset 2412
       CALL PROFLD(2,'Locate elements', -1.0)
       CALL PROSTA(2,-1.0)
10     CONTINUE
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       IF(STRING(1:6).NE.'    -1')GOTO 10
11     CONTINUE
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       IF(STRING(1:6).EQ.'    -1')GOTO 11
       IF(STRING(1:6).NE.'  2412')GOTO 10
**  Loop over the tetrahedrons, with progress printing.
       CALL PROFLD(2,'Elements', -1.0)
       CALL PROSTA(2,-1.0)
       NTETRA=0
       NPOINT=0
20     CONTINUE
*   Read the element information
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       IF(STRING(1:6).EQ.'    -1')GOTO 30
       READ(STRING,'(6I10)',ERR=2010,IOSTAT=IOS)
     -      ITETRA,IELT,IDUMMY,IMAT,IDUMMY,IDUMMY
       READ(12,'(8I10/2I10)',END=2000,ERR=2010,IOSTAT=IOS)
     -      IT1,IT2,IT3,IT4,IT5,IT6,IT7,IT8,
     -      IT9,IT10
*   Detect the end
       IF(ITETRA.EQ.-1)THEN
            GOTO 30
       ELSEIF(IELT.NE.118)THEN
            PRINT *,' !!!!!! MAPFMC WARNING : Found element ',
     -           NTETRA,' which is not a 118-tetrahedron in ',
     -           FMAP(1:NCMAP),' ; file not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ELSEIF(ITETRA.NE.NTETRA.AND..NOT.DELBKG)THEN
            PRINT *,' !!!!!! MAPFMC WARNING : Numbering mismatch'//
     -           ' at tetrahedron ',NTETRA,' in ',FMAP(1:NCMAP),
     -           ' ; file not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Increment element counter
       IF(NTETRA.GE.MXMAP)THEN
            PRINT *,' !!!!!! MAPFMC WARNING : Number of'//
     -           ' tetrahedrons in ',FMAP(1:NCMAP),
     -           ' exceeds compilation limit; file not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
       NTETRA=NTETRA+1
*   Store the material flag.
       IF(IMAT.LT.1.OR.IMAT.GT.MXEPS)THEN
            PRINT *,' !!!!!! MAPFMC WARNING : Found out of range'//
     -           ' material identifier ',IMAT,' in mesh file ',
     -           FMAP(1:NCMAP),'; file not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ELSE
            MATMAP(NTETRA)=IMAT
            EPSMAT(IMAT)=REAL(IMAT)
            OHMMAT(IMAT)=REAL(IMAT)
            IF(NEPS.LT.IMAT)NEPS=IMAT
       ENDIF
*   Skip tetrahedra which are background.
       IF(DELBKG.AND.OHMMAT(MATMAP(NTETRA)).EQ.0.0)THEN
            NDELET=NDELET+1
            NTETRA=NTETRA-1
            GOTO 20
       ENDIF
*   Store the reference pointers temporarily in Ex (point) and Ey (V).
       EXMAP(NTETRA,LOOKUP(1))=IT1
       EXMAP(NTETRA,LOOKUP(2))=IT2
       EXMAP(NTETRA,LOOKUP(3))=IT3
       EXMAP(NTETRA,LOOKUP(4))=IT4
       EXMAP(NTETRA,LOOKUP(5))=IT5
       EXMAP(NTETRA,LOOKUP(6))=IT6
       EXMAP(NTETRA,LOOKUP(7))=IT7
       EXMAP(NTETRA,LOOKUP(8))=IT8
       EXMAP(NTETRA,LOOKUP(9))=IT9
       EXMAP(NTETRA,LOOKUP(10))=IT10
*   Keep track of the number of points
       NPOINT=MAX(NPOINT,IT1,IT2,IT3,IT4,IT5,IT6,IT7,IT8,IT9,IT10)
*   Next element.
       GOTO 20
30     CONTINUE
*** Rewind, then move to the points list.
       REWIND(UNIT=12,ERR=2050)
*** Move to the mesh structure part: dataset 2411
       CALL PROFLD(2,'Locate points', -1.0)
       CALL PROSTA(2,-1.0)
40     CONTINUE
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       IF(STRING(1:6).NE.'    -1')GOTO 40
41     CONTINUE
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       IF(STRING(1:6).EQ.'    -1')GOTO 41
       IF(STRING(1:6).NE.'  2411')GOTO 40
**  Loop over the tetrahedrons, with progress printing.
       CALL PROFLD(2,'Nodes',REAL(NPOINT))
       DO 50 I=1,NPOINT
*   Progress printing
       IF(I.EQ.MAX(1,NPOINT/100)*(I/MAX(1,NPOINT/100)))
     -      CALL PROSTA(2,REAL(I))
*   Read the node coordinates
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       IF(STRING(1:6).EQ.'    -1')THEN
            PRINT *,' !!!!!! MAPFMC WARNING : Premature end of'//
     -           ' node list in ',FMAP(1:NCMAP),' ; file not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
       READ(STRING,'(4I10)',ERR=2010,IOSTAT=IOS)
     -      INODE,IDUMMY,IDUMMY,IDUMMY
       READ(12,'(3E25.16)',END=2000,ERR=2010,IOSTAT=IOS)
     -      XP,YP,ZP
*   Detect the end
       IF(INODE.EQ.-1)THEN
            PRINT *,' !!!!!! MAPFMC WARNING : Premature end of'//
     -           ' node list in ',FMAP(1:NCMAP),' ; file not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ELSEIF(INODE.NE.I)THEN
            PRINT *,' !!!!!! MAPFMC WARNING : Numbering mismatch'//
     -           ' at point ',I,'/',INODE,' in ',FMAP(1:NCMAP),
     -           ' ; file not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Convert from the a priori unknown Tosca unit to cm.
       XP=XP*UNITD
       YP=YP*UNITD
       ZP=ZP*UNITD
*   Store the tetrahedron parameters that refer to this point.
       NUSE=0
       DO 70 J=1,10
       DO 60 K=1,NTETRA
       IF(NINT(EXMAP(K,J)).EQ.I)THEN
            NUSE=NUSE+1
            XMAP(K,J)=XP
            YMAP(K,J)=YP
            ZMAP(K,J)=ZP
            EXMAP(K,J)=-1
       ENDIF
60     CONTINUE
70     CONTINUE
*   If this point was not used, skip the rest.
       IF(NUSE.LE.0)GOTO 50
*   Update the chamber dimensions.
       IF(I.EQ.1)THEN
            XMMIN=XP
            XMMAX=XP
            YMMIN=YP
            YMMAX=YP
            ZMMIN=ZP
            ZMMAX=ZP
       ELSE
            XMMIN=MIN(XMMIN,XP)
            XMMAX=MAX(XMMAX,XP)
            YMMIN=MIN(YMMIN,YP)
            YMMAX=MAX(YMMAX,YP)
            ZMMIN=MIN(ZMMIN,ZP)
            ZMMAX=MAX(ZMMAX,ZP)
       ENDIF
*   Update angular ranges.
       IF(YP.NE.0.OR.ZP.NE.0)THEN
            IF(SETAX)THEN
                 XAMIN=MIN(XAMIN,ATAN2(ZP,YP))
                 XAMAX=MAX(XAMAX,ATAN2(ZP,YP))
            ELSE
                 XAMIN=ATAN2(ZP,YP)
                 XAMAX=ATAN2(ZP,YP)
                 SETAX=.TRUE.
            ENDIF
       ENDIF
       IF(ZP.NE.0.OR.XP.NE.0)THEN
            IF(SETAY)THEN
                 YAMIN=MIN(YAMIN,ATAN2(XP,ZP))
                 YAMAX=MAX(YAMAX,ATAN2(XP,ZP))
            ELSE
                 YAMIN=ATAN2(XP,ZP)
                 YAMAX=ATAN2(XP,ZP)
                 SETAY=.TRUE.
            ENDIF
       ENDIF
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
50     CONTINUE
*** End of reading, make sure that all node references are solved.
       CALL PROFLD(2,'Node check',-1.0)
       CALL PROSTA(2,0.0)
       DO 110 J=1,10
       DO 100 I=1,NTETRA
       IF(EXMAP(I,J).GE.0)THEN
            PRINT *,' !!!!!! MAPFMC WARNING : Unresolved references'//
     -           ' to points left in element table ; map rejected.'
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFMC DEBUG   :'',
     -           '' Element = '',I5,'', j = '',I2,'', point = '',I5)')
     -           I,J,NINT(EXMAP(I,J))
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
100    CONTINUE
110    CONTINUE
*   Store the number of tetrahedrons.
       NMAP=NTETRA
*   Set the flag that the mesh is now defined.
       MAPFLG(1)=.TRUE.
**  Print number of deleted tetrahedrons.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFMC DEBUG   :'',
     -      '' Tetrahedra left: '',I6,'', deleted background: '',I6,
     -      '', nodes: '',I6)') NTETRA,NDELET,NPOINT
       IF(NDELET.NE.0)PRINT *,' ------ MAPFMC MESSAGE : Found ',NDELET,
     -      ' background tetrahedrons.'
*** Read the potentials, first find them: dataset 2414
       CALL PROFLD(2,'Locate potentials', -1.0)
       CALL PROSTA(2,-1.0)
120    CONTINUE
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       IF(STRING(1:6).NE.'    -1')GOTO 120
121    CONTINUE
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       IF(STRING(1:6).EQ.'    -1')GOTO 121
       IF(STRING(1:6).NE.'  2414')GOTO 120
*   Skip 6 header lines
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
*   Now expect to find the potential.
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       IF(STRING(1:12).NE.'Component: V')THEN
            PRINT *,' ------ MAPFMC MESSAGE : Found ',STRING(1:20)
            GOTO 120
       ENDIF
*   Skip a further 6 lines.
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       READ(12,'(A)',END=2000,ERR=2010,IOSTAT=IOS) STRING
*   Progress printing.
       IF(IDATA.EQ.10)THEN
            CALL PROFLD(2,'Weighting field',REAL(NTETRA))
       ELSE
            CALL PROFLD(2,'Potentials',REAL(NTETRA))
       ENDIF
*   Loop over the elements.
       DO 130 I=1,NTETRA
       IF(I.EQ.MAX(1,NTETRA/100)*(I/MAX(1,NTETRA/100)))
     -      CALL PROSTA(2,REAL(I))
*   Read an element at the time.
       READ(12,'(4I10,10(/E13.5))',END=2000,ERR=2010,IOSTAT=IOS)
     -      ITETRA,IDUMMY,IDUMMY,IDUMMY,
     -      V1,V2,V3,V4,V5,V6,V7,V8,V9,V10
*   Check the numbers.
       IF(ITETRA.EQ.-1)THEN
            PRINT *,' !!!!!! MAPFMC WARNING : Premature end of'//
     -           ' element list in ',FMAP(1:NCMAP),' ; not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ELSEIF(ITETRA.NE.I)THEN
            PRINT *,' !!!!!! MAPFMC WARNING : Numbering mismatch'//
     -           ' at element ',I,ITETRA,' in ',FMAP(1:NCMAP),
     -           ' ; file not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Store the potentials.
       IF(IDATA.EQ.10)THEN
            VWMAP(I,LOOKUP(1),IWMAP)=V1
            VWMAP(I,LOOKUP(2),IWMAP)=V2
            VWMAP(I,LOOKUP(3),IWMAP)=V3
            VWMAP(I,LOOKUP(4),IWMAP)=V4
            VWMAP(I,LOOKUP(5),IWMAP)=V5
            VWMAP(I,LOOKUP(6),IWMAP)=V6
            VWMAP(I,LOOKUP(7),IWMAP)=V7
            VWMAP(I,LOOKUP(8),IWMAP)=V8
            VWMAP(I,LOOKUP(9),IWMAP)=V9
            VWMAP(I,LOOKUP(10),IWMAP)=V10
       ELSE
            VMAP(I,LOOKUP(1))=V1
            VMAP(I,LOOKUP(2))=V2
            VMAP(I,LOOKUP(3))=V3
            VMAP(I,LOOKUP(4))=V4
            VMAP(I,LOOKUP(5))=V5
            VMAP(I,LOOKUP(6))=V6
            VMAP(I,LOOKUP(7))=V7
            VMAP(I,LOOKUP(8))=V8
            VMAP(I,LOOKUP(9))=V9
            VMAP(I,LOOKUP(10))=V10
       ENDIF
*   Keep track of potential range.
       IF(IDATA.NE.10)THEN
            IF(I.EQ.1)THEN
                 VMMIN=V1
                 VMMAX=V1
            ENDIF
            VMMIN=MIN(VMMIN,V1,V2,V3,V4,V5,V6,V7,V8,V9,V10)
            VMMAX=MAX(VMMAX,V1,V2,V3,V4,V5,V6,V7,V8,V9,V10)
       ENDIF
130    CONTINUE
*** Record that we've read a potential or a weighting potential.
       IF(IDATA.EQ.10)THEN
            MAPFLG(13+4*IWMAP-3)=.TRUE.
       ELSE
            MAPFLG(5)=.TRUE.
       ENDIF
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFMC DEBUG   :'',
     -      '' Potential range: '',E15.8,'' - '',E15.8,'' V.'')')
     -      VMMIN,VMMAX
*** Check that we have got all materials fully defined.
       DO 170 IEPS=1,NEPS
       IF(EPSMAT(IEPS).LT.0)THEN
            PRINT *,' !!!!!! MAPFMC WARNING : Material ',IEPS,
     -           ' has not been assigned a permittivity,'//
     -           '; file ',FMAP(1:NCMAP),' not read.'
            RETURN
       ENDIF
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFMC DEBUG   :'',
     -      '' Material '',I3,'': perx = '',E12.5,
     -      '', rsvx = '',E12.5)') IEPS,EPSMAT(IEPS),OHMMAT(IEPS)
170    CONTINUE
*** Seems to have worked, set error flag to OK and return.
       IFAIL=0
       MAPTYP=13
       RETURN
*** Handle error conditions.
2000   CONTINUE
       PRINT *,' !!!!!! MAPFMC WARNING : Premature end of file'//
     -      ' reading a field map file; map not available.'
       IF(LDEBUG)CALL INPIOS(IOS)
       CALL INPSWI('RESTORE')
       RETURN
2010   CONTINUE
       PRINT *,' !!!!!! MAPFMC WARNING : Error reading a field'//
     -      ' map file; map not available.'
       IF(LDEBUG)CALL INPIOS(IOS)
       CALL INPSWI('RESTORE')
       RETURN
2050   CONTINUE
       PRINT *,' !!!!!! MAPFMC WARNING : Error rewinding a'//
     -      ' field  map file ; map not available.'
       IF(LDEBUG)CALL INPIOS(IOS)
       CALL INPSWI('RESTORE')
       RETURN
       END
