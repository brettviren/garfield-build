CDECK  ID>, MAPFM8.
       SUBROUTINE MAPFM8(FMAP,NCMAP,IDATA,IWMAP,IORDER,UNITD,IFAIL)
*-----------------------------------------------------------------------
*   MAPFM8 - Reads a COMSOL 3D map of 1st order tetrahedra
*   (Last changed on 29/ 3/12.)
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
       INTEGER I,J,K,L,NCMAP,IFAIL,IORDER,IL,IEPS,IAUX,
     -      IH1,IH2,IH3,IH4,IH5,IH6,IH7,IH8,IH9,IH10,IDUM,
     -      IDATA,NTETRA,NPOINT,IWMAP,IMAP(MXMAP,10),IOS
       REAL XP,YP,ZP,VREAD,UNITD,EREAD
       LOGICAL SWITCH
       CHARACTER*(*) FMAP
       CHARACTER*75 LINE
       SAVE NTETRA,NPOINT,IMAP
       DATA NTETRA/0/, NPOINT/0/
**** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MAPFM8 ///'
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM8 DEBUG   : Data = '',
     -      I3,'', IWMAP = '',I3,'', IORDER = '',I3/26X,''File = "'',
     -      A,''"'')') IDATA,IWMAP,IORDER,FMAP(1:NCMAP)
*** Assume that this will fail.
       IFAIL=1
*** Set the number of fields.
       IF(IORDER.LT.1.OR.IORDER.GT.2)
     -      PRINT *,' !!!!!! MAPFM8 WARNING : Invalid order parameter'//
     -      ' received: ',IORDER,'; reading as 1st order.'
*** We will only do Fortran reads, from the start.
       CALL INPSWI('RESTORE')
       REWIND(UNIT=12,ERR=2040,IOSTAT=IOS)
       IL=0
*** The first part contains the coordinates, which we skip at first
       CALL PROFLD(2,'Skipping',-1.0)
       CALL PROSTA(2,0.0)
*   Check the first header.
       READ(12,'(A75)',END=2000,ERR=2010,IOSTAT=IOS) LINE
       IL=IL+1
       IF(LINE(1:13).NE.'% Coordinates')THEN
            PRINT *,' !!!!!! MAPFM8 WARNING : The first line of ',
     -           FMAP(1:NCMAP),' does not contain the expected'//
     -           ' COMSOL 3D header; file not read.'
            IFAIL=1
            RETURN
       ENDIF
*   Skip until the elements, counting the number of points.
       SWITCH=.FALSE.
 150   CONTINUE
       NPOINT=0
40     CONTINUE
       READ(12,'(A75)',END=2000,ERR=2010,IOSTAT=IOS) LINE
       IL=IL+1
       IF(LINE(1:10).NE.'% Elements')THEN
            NPOINT=NPOINT+1
            GOTO 40
       ENDIF
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM8 DEBUG   :'',
     -      '' Expecting '',I5,'' points.'')') NPOINT
*** Read the tetrahedron structure.
       CALL PROFLD(2,'Tetrahedrons',-1.0)
       CALL PROSTA(2,0.0)
*   Read lines with point references, until the next header
       NTETRA=0
70     CONTINUE
       READ(12,'(A40)',END=2000,ERR=2010,IOSTAT=IOS) LINE(1:40)
       IL=IL+1
       IF(LINE(1:40).EQ.' ')GOTO 70
       IF(LINE(1:1).NE.'%')THEN
*   Increment the tetrahedron counter.
            IF(NTETRA+1.GT.MXMAP)THEN
                 PRINT *,' !!!!!! MAPFM8 WARNING : Number of'//
     -                ' tetrahedrons in ',FMAP(1:NCMAP),
     -                ' exceeds compilation limit; file not read.'
                 IFAIL=1
                 RETURN
            ENDIF
            NTETRA=NTETRA+1
*   Decode a block of 8 line
            IF(IORDER.NE.2)THEN
                 READ(LINE,'(BN,4I10)',END=2000,ERR=2010,IOSTAT=IOS)
     -                IH1 ,IH2 ,IH3 ,IH4
                 IH5=0
                 IH6=0
                 IH7=0
                 IH8=0
                 IH9=0
                 IH10=0
            ELSEIF(IORDER.EQ.2.AND.SWITCH)THEN
                 READ(LINE,'(BN,4I10)',END=2000,ERR=2010,IOSTAT=IOS)
     -                IH5 ,IH8 ,IH9 ,IH6
                 DO 50 I=1,7
                 READ(12,'(A40)',END=2000,ERR=2010,IOSTAT=IOS)
     -                LINE(1:40)
                 IL=IL+1
                 IF(LINE(1:40).EQ.' ')GOTO 50
                 IF(I.EQ.1)THEN
                      READ(LINE,'(BN,4I10)',END=2000,ERR=2010,
     -                     IOSTAT=IOS) IDUM,IDUM,IH7 ,IDUM
                 ELSEIF(I.EQ.2)THEN
                      READ(LINE,'(BN,4I10)',END=2000,ERR=2010,
     -                     IOSTAT=IOS) IDUM,IDUM,IH10,IDUM
                 ELSEIF(I.EQ.3)THEN
                      READ(LINE,'(BN,4I10)',END=2000,ERR=2010,
     -                     IOSTAT=IOS) IDUM,IDUM,IDUM,IDUM
                 ELSEIF(I.EQ.4)THEN
                      READ(LINE,'(BN,4I10)',END=2000,ERR=2010,
     -                     IOSTAT=IOS) IDUM,IDUM,IH1 ,IDUM
                 ELSEIF(I.EQ.5)THEN
                      READ(LINE,'(BN,4I10)',END=2000,ERR=2010,
     -                     IOSTAT=IOS) IDUM,IDUM,IDUM,IH2
                 ELSEIF(I.EQ.6)THEN
                      READ(LINE,'(BN,4I10)',END=2000,ERR=2010,
     -                     IOSTAT=IOS) IDUM,IH3 ,IDUM,IDUM
                 ELSEIF(I.EQ.7)THEN
                      READ(LINE,'(BN,4I10)',END=2000,ERR=2010,
     -                     IOSTAT=IOS) IH4 ,IDUM,IDUM,IDUM
                 ENDIF
50               CONTINUE
            ELSEIF(IORDER.EQ.2)THEN
                 READ(LINE,'(BN,4I10)',END=2000,ERR=2010,IOSTAT=IOS)
     -                IH5 ,IH8 ,IH6 ,IH9
                 DO 160 I=1,7
                 READ(12,'(A40)',END=2000,ERR=2010,IOSTAT=IOS)
     -                LINE(1:40)
                 IL=IL+1
                 IF(LINE(1:40).EQ.' ')GOTO 160
                 IF(I.EQ.1)THEN
                      READ(LINE,'(BN,4I10)',END=2000,ERR=2010,
     -                     IOSTAT=IOS) IDUM,IDUM,IDUM,IH7
                 ELSEIF(I.EQ.2)THEN
                      READ(LINE,'(BN,4I10)',END=2000,ERR=2010,
     -                     IOSTAT=IOS) IDUM,IDUM,IDUM,IH10
                 ELSEIF(I.EQ.3)THEN
                      READ(LINE,'(BN,4I10)',END=2000,ERR=2010,
     -                     IOSTAT=IOS) IDUM,IDUM,IDUM,IDUM
                 ELSEIF(I.EQ.4)THEN
                      READ(LINE,'(BN,4I10)',END=2000,ERR=2010,
     -                     IOSTAT=IOS) IDUM,IDUM,IDUM,IH1
                 ELSEIF(I.EQ.5)THEN
                      READ(LINE,'(BN,4I10)',END=2000,ERR=2010,
     -                     IOSTAT=IOS) IDUM,IDUM,IH2 ,IDUM
                 ELSEIF(I.EQ.6)THEN
                      READ(LINE,'(BN,4I10)',END=2000,ERR=2010,
     -                     IOSTAT=IOS) IDUM,IH3 ,IDUM,IDUM
                 ELSEIF(I.EQ.7)THEN
                      READ(LINE,'(BN,4I10)',END=2000,ERR=2010,
     -                     IOSTAT=IOS) IH4 ,IDUM,IDUM,IDUM
                 ENDIF
160              CONTINUE
            ENDIF
*   Ensure there is no degeneracy
            IF(  (IORDER.NE.2.AND.(
     -           IH1.EQ.IH2.OR.IH1.EQ.IH3.OR.IH1.EQ.IH4.OR.
     -                         IH2.EQ.IH3.OR.IH2.EQ.IH4.OR.
     -                                       IH3.EQ.IH4)).OR.
     -           (IORDER.EQ.2.AND.(
     -           IH1.EQ.IH2.OR.IH1.EQ.IH3.OR.IH1.EQ.IH4 .OR.
     -           IH1.EQ.IH5.OR.IH1.EQ.IH6.OR.IH1.EQ.IH7 .OR.
     -           IH1.EQ.IH8.OR.IH1.EQ.IH9.OR.IH1.EQ.IH10.OR.
     -                         IH2.EQ.IH3.OR.IH2.EQ.IH4 .OR.
     -           IH2.EQ.IH5.OR.IH2.EQ.IH6.OR.IH2.EQ.IH7 .OR.
     -           IH2.EQ.IH8.OR.IH2.EQ.IH9.OR.IH2.EQ.IH10.OR.
     -                                       IH3.EQ.IH4 .OR.
     -           IH3.EQ.IH5.OR.IH3.EQ.IH6.OR.IH3.EQ.IH7 .OR.
     -           IH3.EQ.IH8.OR.IH3.EQ.IH9.OR.IH3.EQ.IH10.OR.
     -           IH4.EQ.IH5.OR.IH4.EQ.IH6.OR.IH4.EQ.IH7 .OR.
     -           IH4.EQ.IH8.OR.IH4.EQ.IH9.OR.IH4.EQ.IH10.OR.
     -                         IH5.EQ.IH6.OR.IH5.EQ.IH7 .OR.
     -           IH5.EQ.IH8.OR.IH5.EQ.IH9.OR.IH5.EQ.IH10.OR.
     -                                       IH6.EQ.IH7 .OR.
     -           IH6.EQ.IH8.OR.IH6.EQ.IH9.OR.IH6.EQ.IH10.OR.
     -           IH7.EQ.IH8.OR.IH7.EQ.IH9.OR.IH7.EQ.IH10.OR.
     -                         IH8.EQ.IH9.OR.IH8.EQ.IH10.OR.
     -                                       IH9.EQ.IH10)))THEN
                      IF(.NOT.SWITCH.AND.NTETRA.EQ.1)THEN
                           IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM8'',
     -                        '' DEBUG   : Switching.columns 3-4.'')')
                           SWITCH=.TRUE.
                           REWIND(UNIT=12,ERR=2040,IOSTAT=IOS)
                           READ(12,'(A75)',END=2000,ERR=2010,IOSTAT=IOS)
     -                          LINE
                           IL=1
                           GOTO 150
                      ELSE
                           PRINT *,' !!!!!! MAPFM8 WARNING : Element ',
     -                          NTETRA,' in file ',FMAP(1:NCMAP),
     -                          ' is degenerate; file not read.'
                      IFAIL=1
                      RETURN
                 ENDIF
            ENDIF
*   Ensure they all make sense.
            IF(  (IORDER.NE.2.AND.(
     -           IH1.LE.0.OR.IH2.LE.0.OR.IH3.LE.0.OR.IH4.LE.0)).OR.
     -           (IORDER.EQ.2.AND.(
     -           IH1.LE.0.OR.IH2.LE.0.OR.IH3.LE.0.OR.IH4.LE.0.OR.
     -           IH5.LE.0.OR.IH6.LE.0.OR.IH7.LE.0.OR.IH8.LE.0.OR.
     -           IH9.LE.0.OR.IH10.LE.0)))THEN
                 PRINT *,' !!!!!! MAPFM8 WARNING : Invalid point'//
     -                ' reference in ',FMAP(1:NCMAP),'; map not read.'
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM8 DEBUG   :'',
     -                '' tetrahedron '',I5/
     -                26X,''Vertices: '',10(2X,I5))')
     -                NTETRA,IH1,IH2,IH3,IH4,IH5,IH6,IH7,IH8,IH9,IH10
                 IFAIL=1
                 RETURN
            ENDIF
*   Store the reference pointers (negative sign for checks).
            IMAP(NTETRA,1) =-IH1
            IMAP(NTETRA,2) =-IH2
            IMAP(NTETRA,3) =-IH3
            IMAP(NTETRA,4) =-IH4
            IMAP(NTETRA,5) =-IH5
            IMAP(NTETRA,6) =-IH6
            IMAP(NTETRA,7) =-IH7
            IMAP(NTETRA,8) =-IH8
            IMAP(NTETRA,9) =-IH9
            IMAP(NTETRA,10)=-IH10
*   Next line
            GOTO 70
       ENDIF
*   Debugging
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM8 DEBUG   :'',
     -      '' Found '',I5,'' tetrahedra.'')') NTETRA
*** Read the points filling in the mesh file.
       CALL PROFLD(2,'Rewind',-1.0)
       CALL PROSTA(2,0.0)
       REWIND(UNIT=12,ERR=2040,IOSTAT=IOS)
       IL=0
*** Skip vertex reading if this has already been done.
       IF(MAPFLG(1))GOTO 1234
*   Skip the header
       READ(12,'()',END=2000,ERR=2010,IOSTAT=IOS)
       IL=IL+1
*   Loop over the points.
       CALL PROFLD(2,'Vertices',REAL(NPOINT))
       DO 80 I=1,NPOINT
       IF(I.EQ.MAX(1,NPOINT/100)*(I/MAX(1,NPOINT/100)))
     -      CALL PROSTA(2,REAL(I))
*   Read the line.
       READ(12,'(3F25.10)',END=2000,ERR=2010,IOSTAT=IOS) XP,YP,ZP
       IL=IL+1
*   Convert from the COMSOL unit to cm.
       XP=XP*UNITD
       YP=YP*UNITD
       ZP=ZP*UNITD
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
*   Find referring tetrahedrons, trace resolved references with sign.
       DO 90 J=1,4
       DO 100 K=1,NTETRA
       IF(I.EQ.ABS(IMAP(K,J)))THEN
            XMAP(K,J)=XP
            YMAP(K,J)=YP
            ZMAP(K,J)=ZP
            IMAP(K,J)=ABS(IMAP(K,J))
       ENDIF
100    CONTINUE
90     CONTINUE
*   Next point.
80     CONTINUE
*   End of reading, check reference resolution.
       CALL PROFLD(2,'Verifying',-1.0)
       CALL PROSTA(2,0.0)
       DO 110 J=1,4
       DO 120 I=1,NTETRA
       IF(IMAP(I,J).LE.0)THEN
            PRINT *,' !!!!!! MAPFM8 WARNING : Unresolved point'//
     -           ' references in mesh ; map rejected.'
            IFAIL=1
            RETURN
       ENDIF
120    CONTINUE
110    CONTINUE
*   Now set the number of elements.
       NMAP=NTETRA
*   Set the flag that the mesh is now defined.
       MAPFLG(1)=.TRUE.
*   Set the element type.
       MAPTYP=12
*** And read the potentials, first skip till we have the header.
1234   CONTINUE
       CALL PROFLD(2,'Skipping',-1.0)
       CALL PROSTA(2,0.0)
140    CONTINUE
       READ(12,'(A75)',END=2000,ERR=2010,IOSTAT=IOS) LINE
       IL=IL+1
       IF(LINE(1:10).NE.'% Data (V)'.AND.
     -    LINE(1:19).NE.'% Data ((V)[1/(V)])'.AND.
     -    LINE(1:14).NE.'% Data (V (V))')GOTO 140
*   Read the list of points with associated field values.
       IF(IDATA.EQ.10)THEN
            CALL PROFLD(2,'Weighting field',REAL(NPOINT))
       ELSE
            CALL PROFLD(2,'Potentials',REAL(NPOINT))
       ENDIF
       DO 10 I=1,NPOINT
       IF(I.EQ.MAX(1,NPOINT/100)*(I/MAX(1,NPOINT/100)))
     -      CALL PROSTA(2,REAL(I))
*   Read the data line.
       READ(12,'(A75)',END=2000,ERR=2010,IOSTAT=IOS) LINE
       IF(INDEX(LINE,'.').EQ.0)THEN
            READ(LINE,'(BN,I25)') IAUX
            VREAD=REAL(IAUX)
       ELSE
            READ(LINE,'(E25.10)') VREAD
       ENDIF
       IL=IL+1
*   Assign the potential to the mesh.
       DO 20 L=1,10
       DO 30 K=1,NTETRA
       IF(ABS(IMAP(K,L)).EQ.I)THEN
            IF(IDATA.EQ.10)THEN
                 VWMAP(K,L,IWMAP)=VREAD
            ELSE
                 VMAP(K,L)=VREAD
                 IF(I.EQ.1)THEN
                      VMMIN=VREAD
                      VMMAX=VREAD
                 ELSE
                      VMMIN=MIN(VMMIN,VREAD)
                      VMMAX=MAX(VMMAX,VREAD)
                 ENDIF
            ENDIF
       ENDIF
30     CONTINUE
20     CONTINUE
*   Next point.
10     CONTINUE
**  Flag that the potential is now defined.
       IF(IDATA.EQ.10)THEN
            MAPFLG(13+4*IWMAP-3)=.TRUE.
       ELSE
            MAPFLG(5)=.TRUE.
       ENDIF
*** Read the materials, if there are any.
       CALL PROFLD(2,'Rewind',-1.0)
       CALL PROSTA(2,0.0)
       REWIND(UNIT=12,ERR=2040,IOSTAT=IOS)
       IL=0
*   Preset the material flags.
       NEPS=0
       print *,' ntetra = ',ntetra
       DO 130 I=1,NTETRA
       MATMAP(I)=-1
130    CONTINUE
       CALL PROFLD(2,'Skipping',-1.0)
       CALL PROSTA(2,0.0)
170    CONTINUE
       READ(12,'(A75)',END=2001,ERR=2010,IOSTAT=IOS) LINE
       IL=IL+1
       IF(LINE(1:22).NE.'% Data (es.epsrAv (1))')GOTO 170
       CALL PROFLD(2,'Materials',REAL(NTETRA))
       DO 180 I=1,NTETRA
       IF(I.EQ.MAX(1,NTETRA/100)*(I/MAX(1,NTETRA/100)))
     -      CALL PROSTA(2,REAL(I))
*   Read the data line.
       READ(12,'(A75)',END=2000,ERR=2010,IOSTAT=IOS) LINE
       IF(INDEX(LINE,'.').EQ.0)THEN
            READ(LINE,'(BN,I25)') IAUX
            EREAD=REAL(IAUX)
       ELSE
            READ(LINE,'(E25.10)') EREAD
       ENDIF
C       print *,' eread = ',eread
       IL=IL+1
*   Match with existing epsilons.
       IEPS=-1
       DO 190 J=1,NEPS
       IF(ABS(EREAD-EPSMAT(J)).LE.
     -      1E-4*(ABS(EPSMAT(J))+ABS(EREAD)))IEPS=J
190    CONTINUE
*   Warn if we run out of spcae.
       IF(IEPS.LT.0.AND.NEPS.GE.MXEPS)THEN
            PRINT *,' !!!!!! MAPFM8 WARNING : More media than storage'//
     -           ' allows in ',FMAP(1:NCMAP),'; medium not assigned.'
            IFAIL=1
            RETURN
*   Add new epsilon to the table.
       ELSEIF(IEPS.LT.0)THEN
            NEPS=NEPS+1
            IEPS=NEPS
            EPSMAT(IEPS)=EREAD
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM8 DEBUG   :'',
     -           '' Adding dielectricum '',I3,'' with eps='',E10.3,
     -           ''.'')') IEPS,EPSMAT(IEPS)
       ENDIF
*   Assign the value.
       MATMAP(I)=IEPS
180    CONTINUE
*   Set the flag.
       MAPFLG(9)=.TRUE.
       GOTO 2002
*   No epsilon data present.
2001   CONTINUE
*   Set the flag that there are no epsilons.
       MAPFLG(9)=.FALSE.
**  Seems to have worked, set error flag to OK and return.
2002   CONTINUE
       IFAIL=0
       RETURN
*** Error handling.
2000   CONTINUE
       PRINT *,' !!!!!! MAPFM8 WARNING : Premature end of file on ',
     -      FMAP(1:NCMAP),' at line ',IL,'; file not read.'
       IF(LDEBUG)CALL INPIOS(IOS)
       RETURN
2010   CONTINUE
       PRINT *,' !!!!!! MAPFM8 WARNING : Error while reading ',
     -      FMAP(1:NCMAP),' at line ',IL,'; file not read.'
       print *,' Line ',il
       IF(LDEBUG)CALL INPIOS(IOS)
       RETURN
2040   CONTINUE
       PRINT *,' !!!!!! MAPFM8 WARNING : Error while rewinding ',
     -      FMAP(1:NCMAP),'; file not read.'
       IF(LDEBUG)CALL INPIOS(IOS)
       RETURN
       END
