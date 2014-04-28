CDECK  ID>, MAPFM6.
       SUBROUTINE MAPFM6(FMAP,NCMAP,IDATA,IWMAP,IFAIL)
*-----------------------------------------------------------------------
*   MAPFM6 - Reads a Tosca table of boxes (hexhedrons).
*   (Last changed on 28/ 2/04.)
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
       INTEGER MXCONT
       PARAMETER(MXCONT=20)
       INTEGER IEPS,ICONT(MXCONT),IMAX,I,J,K,L,M,NCMAP,IFAIL,
     -      IH1,IH2,IH3,IH4,IH5,IH6,IH7,IH8,IHEX,IOS,INPCMP,
     -      IDATA,IP,NHEX,NPOINT,IWMAP,IELT,IMATE,IPOT,
     -      MPOINT,IMAP(MXMAP,8),NAVER,NDEGEN
       REAL TEMP(MXCONT),ECOMP,DCOMP,XP,YP,ZP
       DOUBLE PRECISION EPSNOD(MXMAP,8),EPSVAL,EPSAVE,EPSRMS
       CHARACTER*(*) FMAP
       CHARACTER*8 DATA
       LOGICAL READ,BXOK,BYOK,BZOK,DXOK,DYOK,DZOK,
     -      EXOK,EYOK,EZOK
       EXTERNAL INPCMP
       SAVE NHEX,NPOINT,EPSNOD,EPSVAL,IMAP
       DATA NHEX/0/, NPOINT/0/
**** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MAPFM6 ///'
*** Assume that this will fail.
       IFAIL=1
*** We will only do Fortran reads, and want to read from the start.
       CALL INPSWI('RESTORE')
       REWIND(UNIT=12,ERR=2040,IOSTAT=IOS)
*** If this is a mesh file.
       IF(IDATA.EQ.1)THEN
*   Read the number of hexahedrons.
            READ(12,'(I10)',END=2000,ERR=2010,IOSTAT=IOS) NPOINT
*   Check the value.
            IF(NPOINT.LE.0)THEN
                 PRINT *,' !!!!!! MAPFM6 WARNING : The file ',
     -                FMAP(1:NCMAP),' contains 0 or fewer'//
     -                ' vertices; not read.'
                 RETURN
            ENDIF
*   Debugging output.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM6 DEBUG   :'',
     -           '' Number of vertices: '',I5)') NPOINT
**  Skip to the hexahedron composition.
            CALL PROFLD(2,'Skipping',-1.0)
            CALL PROSTA(2,0.0)
            DO 1010 I=1,NPOINT
            READ(12,'(1X)',END=2000,ERR=2010,IOSTAT=IOS)
1010        CONTINUE
*   Read the number of hexahedrons.
            READ(12,'(I10)',END=2000,ERR=2010,IOSTAT=IOS) NHEX
*   Check the value.
            IF(NHEX.LE.0)THEN
                 PRINT *,' !!!!!! MAPFM6 WARNING : The file ',
     -                FMAP(1:NCMAP),' contains 0 or fewer'//
     -                ' hexahedrons; not read.'
                 RETURN
            ELSEIF(NHEX.GT.MXMAP)THEN
                 PRINT *,' !!!!!! MAPFM6 WARNING : Number of'//
     -                ' hexahedrons in ',FMAP(1:NCMAP),
     -                ' exceeds compilation limit; file not read.'
                 RETURN
            ENDIF
*   Debugging output.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM6 DEBUG   :'',
     -           '' Number of hexahedrons: '',I5)') NHEX
*   Initialise the tetrahedron to vertex pointers.
            DO 1050 J=1,8
            DO 1060 I=1,NHEX
            IMAP(I,J)=0
1060        CONTINUE
1050        CONTINUE
**  Loop over the hexahedrons.
            CALL PROFLD(2,'Hexahedrons',REAL(NHEX))
            NDEGEN=0
            DO 1030 I=1,NHEX
            IF(I.EQ.MAX(1,NHEX/100)*(I/MAX(1,NHEX/100)))
     -           CALL PROSTA(2,REAL(I))
*   Read the hexahedron number, element type, material and potential
            READ(12,'(4I10)',END=2000,ERR=2010,IOSTAT=IOS)
     -           IHEX,IELT,IMATE,IPOT
            IF(IELT.NE.1.OR.
     -           (IMATE.LT.0.OR.IMATE.GT.MXEPS).OR.
     -           (IPOT.NE.0.AND.IPOT.NE.3))THEN
                 PRINT *, ' !!!!!! MAPFM6 WARNING : Description of'//
     -                ' element ',IHEX,' not as expected.'
            ENDIF
*   Read the pointers for its vertices.
            READ(12,'(8I10)',END=2000,ERR=2010,IOSTAT=IOS)
     -           IH1,IH2,IH3,IH4,IH5,IH6,IH7,IH8
            IF(IH1.EQ.IH2.OR.IH1.EQ.IH3.OR.IH2.EQ.IH3.OR.
     -           IH5.EQ.IH6.OR.IH5.EQ.IH7.OR.IH6.EQ.IH7.OR.
     -           (IH1.EQ.IH4.AND.IH5.NE.IH8).OR.
     -           (IH1.NE.IH4.AND.IH5.EQ.IH8))THEN
                 PRINT *,' !!!!!! MAPFM6 WARNING : Element ',IHEX,
     -                ' is degenerate in an unexpected manner.'
            ELSEIF(IH1.EQ.IH4.AND.IH5.EQ.IH8)THEN
                 NDEGEN=NDEGEN+1
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM6 DEBUG   :'',
     -                '' Element '',I5,'' is a pentahedron.'')') IHEX
                 ELMDGN(IHEX)=.TRUE.
            ELSE
                 ELMDGN(IHEX)=.FALSE.
            ENDIF
*   Ensure they all make sense.
            IF(IH1.LE.0.OR.IH2.LE.0.OR.IH3.LE.0.OR.IH4.LE.0.OR.
     -           IH5.LE.0.OR.IH6.LE.0.OR.IH7.LE.0.OR.IH8.LE.0.OR.
     -           IHEX.LE.0.OR.IHEX.GT.NHEX)THEN
                 PRINT *,' !!!!!! MAPFM6 WARNING : Invalid hexahedron'//
     -                ' reference in ',FMAP(1:NCMAP),'; map not read.'
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM6 DEBUG   :'',
     -                '' Hexahedron '',I5,'' / '',I5,'', Points: '',I5/
     -                26X,''Vertices: '',4(2X,I5)/37X,4(2X,I5))')
     -                IHEX,NHEX,NPOINT,IH1,IH2,IH3,IH4,IH5,IH6,IH7,IH8
                 RETURN
            ENDIF
*   Store the reference pointers (negative sign for checks).
            IMAP(IHEX,1)=-IH1
            IMAP(IHEX,2)=-IH2
            IMAP(IHEX,3)=-IH3
            IMAP(IHEX,4)=-IH4
            IMAP(IHEX,5)=-IH5
            IMAP(IHEX,6)=-IH6
            IMAP(IHEX,7)=-IH7
            IMAP(IHEX,8)=-IH8
1030        CONTINUE
**  Return to the start of the file.
            CALL PROFLD(2,'Rewind',-1.0)
            CALL PROSTA(2,0.0)
            REWIND(UNIT=12,ERR=2040,IOSTAT=IOS)
*   Skip the number of points.
            READ(12,'()',END=2000,ERR=2010,IOSTAT=IOS)
**  Loop over the points.
            CALL PROFLD(2,'Vertices',REAL(NPOINT))
            DO 1040 I=1,NPOINT
            IF(I.EQ.MAX(1,NPOINT/100)*(I/MAX(1,NPOINT/100)))
     -           CALL PROSTA(2,REAL(I))
*   Read the line.
            READ(12,'(I10,3F21.8)',END=2000,ERR=2010,IOSTAT=IOS)
     -           IP,XP,YP,ZP
            IF(IP.LE.0)THEN
                 PRINT *,' !!!!!! MAPFM6 WARNING : Invalid point'//
     -                ' reference in ',FMAP(1:NCMAP),'; map not read.'
                 RETURN
            ENDIF
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
*   Find referring hexahedrons, trace resolved references with sign.
            DO 1100 J=1,8
            DO 1110 K=1,NHEX
            IF(IP.EQ.ABS(IMAP(K,J)))THEN
                 XMAP(K,J)=XP
                 YMAP(K,J)=YP
                 ZMAP(K,J)=ZP
                 IMAP(K,J)=ABS(IMAP(K,J))
            ENDIF
1110        CONTINUE
1100        CONTINUE
*   Next point.
1040        CONTINUE
*   End of reading, check reference resolution.
            CALL PROFLD(2,'Verifying',-1.0)
            CALL PROSTA(2,0.0)
            DO 1120 J=1,8
            DO 1130 I=1,NHEX
            IF(IMAP(I,J).LE.0)THEN
                 PRINT *,' !!!!!! MAPFM6 WARNING : Unresolved point'//
     -                ' references in mesh ; map rejected.'
                 RETURN
            ENDIF
1130        CONTINUE
1120        CONTINUE
*   Preset Dx, Dy, Dz and the material.
            DO 1160 I=1,NHEX
            MATMAP(I)=-1
1160        CONTINUE
*   Now set the number of elements.
            NMAP=NHEX
*   Set the flag that the mesh is now defined.
            MAPFLG(1)=.TRUE.
*   Set the element type.
            MAPTYP=14
*** Read field map files.
       ELSE
*   Make sure that the mesh has been read.
            IF(.NOT.MAPFLG(1))THEN
                 PRINT *,' !!!!!! MAPFM6 WARNING : Attempt to read'//
     -                ' a field map before the mesh ; not read.'
                 RETURN
            ENDIF
*   Read the number of points in this file, check it matches the mesh.
            CALL PROFLD(2,'Contents',-1.0)
            CALL PROSTA(2,0.0)
            READ(12,'(BN,I13)',END=2000,ERR=2010,IOSTAT=IOS) MPOINT
            IF(MPOINT.NE.NPOINT)THEN
                 PRINT *,' !!!!!! MAPFM6 WARNING : Number of'//
     -                ' points in ',FMAP(1:NCMAP),' does not'//
     -                ' match current mesh; not read.'
                 RETURN
            ENDIF
**  Determine the contents of this field map.
            READ=.FALSE.
            IMAX=0
            BXOK=.FALSE.
            BYOK=.FALSE.
            BZOK=.FALSE.
            DXOK=.FALSE.
            DYOK=.FALSE.
            DZOK=.FALSE.
            EXOK=.FALSE.
            EYOK=.FALSE.
            EZOK=.FALSE.
*   Read up to the line stating the units.
100         CONTINUE
            READ(12,'(3X,A8)',END=2000,ERR=2010,IOSTAT=IOS) DATA
            IF(DATA.EQ.'[CGS]')THEN
                 GOTO 110
            ELSEIF(DATA.EQ.'[MICRON]')THEN
                 GOTO 110
            ELSEIF(DATA.EQ.'[MM]')THEN
                 GOTO 110
            ELSEIF(IMAX.GE.MXCONT)THEN
                 PRINT *,' !!!!!! MAPFM6 WARNING : Number of data'//
     -                ' fields in ',FMAP(1:NCMAP),' is too large;'//
     -                ' not read.'
                 RETURN
            ENDIF
            IMAX=IMAX+1
*   Coordinates.
            IF(DATA.EQ.'X'.OR.DATA.EQ.'Y'.OR.DATA.EQ.'Z')THEN
                 ICONT(IMAX)=1
*   Ex or Ewx.
            ELSEIF(DATA.EQ.'EX')THEN
                 IF(IDATA.EQ.10)THEN
                      IF(MAPFLG(10+4*IWMAP-3))
     -                     PRINT *,' ------ MAPFM6 MESSAGE :'//
     -                     ' Overwriting current weighting Ex map.'
                      MAPFLG(10+4*IWMAP-3)=.FALSE.
                      ICONT(IMAX)=10+4*IWMAP-3
                 ELSE
                      IF(MAPFLG(2))PRINT *,' ------ MAPFM6 MESSAGE :'//
     -                     ' Overwriting current Ex field map.'
                      MAPFLG(2)=.FALSE.
                      ICONT(IMAX)=2
                 ENDIF
                 EXOK=.TRUE.
                 READ=.TRUE.
*   Ey or Ewy.
            ELSEIF(DATA.EQ.'EY')THEN
                 IF(IDATA.EQ.10)THEN
                      IF(MAPFLG(11+4*IWMAP-3))
     -                     PRINT *,' ------ MAPFM6 MESSAGE :'//
     -                     ' Overwriting current weighting Ey map.'
                      MAPFLG(11+4*IWMAP-3)=.FALSE.
                      ICONT(IMAX)=11+4*IWMAP-3
                 ELSE
                      IF(MAPFLG(3))PRINT *,' ------ MAPFM6 MESSAGE :'//
     -                     ' Overwriting current Ey field map.'
                      MAPFLG(3)=.FALSE.
                      ICONT(IMAX)=3
                 ENDIF
                 EYOK=.TRUE.
                 READ=.TRUE.
*   Ez or Ewz.
            ELSEIF(DATA.EQ.'EZ')THEN
                 IF(IDATA.EQ.10)THEN
                      IF(MAPFLG(12+4*IWMAP-3))
     -                     PRINT *,' ------ MAPFM6 MESSAGE :'//
     -                     ' Overwriting current weighting Ez map.'
                      MAPFLG(12+4*IWMAP-3)=.FALSE.
                      ICONT(IMAX)=12+4*IWMAP-3
                 ELSE
                      IF(MAPFLG(4))PRINT *,' ------ MAPFM6 MESSAGE :'//
     -                     ' Overwriting current Ez field map.'
                      MAPFLG(4)=.FALSE.
                      ICONT(IMAX)=4
                 ENDIF
                 EZOK=.TRUE.
                 READ=.TRUE.
*   Potential or weighting potential.
            ELSEIF(DATA.EQ.'V')THEN
                 IF(IDATA.EQ.10)THEN
                      IF(MAPFLG(13+4*IWMAP-3))
     -                     PRINT *,' ------ MAPFM6 MESSAGE :'//
     -                     ' Overwriting current weighting V map.'
                      MAPFLG(13+4*IWMAP-3)=.FALSE.
                      ICONT(IMAX)=13+4*IWMAP-3
                 ELSE
                      IF(MAPFLG(5))PRINT *,' ------ MAPFM6 MESSAGE :'//
     -                     ' Overwriting current potential map.'
                      MAPFLG(5)=.FALSE.
                      ICONT(IMAX)=5
                 ENDIF
                 READ=.TRUE.
*   Bx, By and Bz.
            ELSEIF(DATA.EQ.'BX')THEN
                 IF(MAPFLG(6))PRINT *,' ------ MAPFM6 MESSAGE :'//
     -                ' Overwriting current Bx field map.'
                 MAPFLG(6)=.FALSE.
                 ICONT(IMAX)=6
                 BXOK=.TRUE.
                 READ=.TRUE.
            ELSEIF(DATA.EQ.'BY')THEN
                 IF(MAPFLG(7))PRINT *,' ------ MAPFM6 MESSAGE :'//
     -                ' Overwriting current Bx field map.'
                 MAPFLG(7)=.FALSE.
                 ICONT(IMAX)=7
                 BYOK=.TRUE.
                 READ=.TRUE.
            ELSEIF(DATA.EQ.'BZ')THEN
                 IF(MAPFLG(8))PRINT *,' ------ MAPFM6 MESSAGE :'//
     -                ' Overwriting current Bx field map.'
                 MAPFLG(8)=.FALSE.
                 ICONT(IMAX)=8
                 BZOK=.TRUE.
                 READ=.TRUE.
*   Dx, Dy and Dz.
            ELSEIF(DATA.EQ.'DX'.OR.DATA.EQ.'DY'.OR.DATA.EQ.'DZ')THEN
                 IF(DATA.EQ.'DX')THEN
                      ICONT(IMAX)=-2
                      DXOK=.TRUE.
                 ELSEIF(DATA.EQ.'DY')THEN
                      ICONT(IMAX)=-3
                      DYOK=.TRUE.
                 ELSEIF(DATA.EQ.'DZ')THEN
                      ICONT(IMAX)=-4
                      DZOK=.TRUE.
                 ENDIF
                 IF(DXOK.AND.DYOK.AND.DZOK)THEN
                      IF(MAPFLG(9))PRINT *,' ------ MAPFM6 MESSAGE :'//
     -                     ' Overwriting current material map.'
                      MAPFLG(9)=.FALSE.
                 ENDIF
                 READ=.TRUE.
                 MATSRC='EPSILON'
*   Other fields.
            ELSE
                 PRINT *,' !!!!!! MAPFM6 WARNING : The file ',
     -                FMAP(1:NCMAP),' contains data of the unknown'//
     -                ' kind '//DATA//'; item skipped.'
                 ICONT(IMAX)=0
            ENDIF
            GOTO 100
110         CONTINUE
**  See whether any item is to be read.
            IF(.NOT.READ)THEN
                 PRINT *,' !!!!!! MAPFM6 WARNING : The file ',
     -                FMAP(1:NCMAP),' contains no useable'//
     -                ' information; file not read.'
                 RETURN
*   Make sure all 3 components of a vector are present.
            ELSEIF((BXOK.AND..NOT.(BYOK.AND.BZOK)).OR.
     -           (BYOK.AND..NOT.(BXOK.AND.BZOK)).OR.
     -           (BZOK.AND..NOT.(BXOK.AND.BYOK)))THEN
                 PRINT *,' !!!!!! MAPFM6 WARNING : Bx, By and Bz must'//
     -                ' appear in one file; ',FMAP(1:NCMAP),' not read.'
                 RETURN
            ELSEIF((DXOK.AND..NOT.(DYOK.AND.DZOK)).OR.
     -           (DYOK.AND..NOT.(DXOK.AND.DZOK)).OR.
     -           (DZOK.AND..NOT.(DXOK.AND.DYOK)))THEN
                 PRINT *,' !!!!!! MAPFM6 WARNING : Dx, Dy and Dz must'//
     -                ' appear in one file; ',FMAP(1:NCMAP),' not read.'
                 RETURN
            ELSEIF((EXOK.AND..NOT.(EYOK.AND.EZOK)).OR.
     -           (EYOK.AND..NOT.(EXOK.AND.EZOK)).OR.
     -           (EZOK.AND..NOT.(EXOK.AND.EYOK)))THEN
                 PRINT *,' !!!!!! MAPFM6 WARNING : Ex, Ey and Ez must'//
     -                ' appear in one file; ',FMAP(1:NCMAP),' not read.'
                 RETURN
            ENDIF
**  Read the list of points with associated field values.
            CALL PROFLD(2,'Vertices',REAL(NPOINT))
            DO 10 I=1,NPOINT
            IF(I.EQ.MAX(1,NPOINT/100)*(I/MAX(1,NPOINT/100)))
     -           CALL PROSTA(2,REAL(I))
*   Read the data line.
            READ(12,'(10F13.4)',END=2000,ERR=2010,IOSTAT=IOS)
     -           (TEMP(J),J=1,IMAX)
*   See whether we can compute an epsilon for this node.
            EPSVAL=-1.0
            IF(EXOK.AND.EYOK.AND.EZOK.AND.
     -           DXOK.AND.DYOK.AND.DZOK)THEN
                 ECOMP=0
                 DCOMP=0
                 DO 90 J=1,IMAX
                 IF(ICONT(J).EQ.2.OR.ICONT(J).EQ.3.OR.ICONT(J).EQ.4)THEN
                      ECOMP=ECOMP+TEMP(J)**2
                 ELSEIF(ICONT(J).EQ.-2.OR.ICONT(J).EQ.-3.OR.
     -                ICONT(J).EQ.-4)THEN
                      DCOMP=DCOMP+TEMP(J)**2
                 ENDIF
90               CONTINUE
                 IF(DCOMP.GT.0.AND.ECOMP.GT.0)
     -                EPSVAL=SQRT(DCOMP/ECOMP)/EPS0
            ENDIF
*   Assign the data to the arrays.
            DO 20 L=1,8
            DO 30 K=1,NHEX
            DO 40 J=1,IMAX
            IF(ICONT(J).EQ.2.AND.IMAP(K,L).EQ.I)THEN
                 EXMAP(K,L)=TEMP(J)
            ELSEIF(ICONT(J).EQ.3.AND.IMAP(K,L).EQ.I)THEN
                 EYMAP(K,L)=TEMP(J)
            ELSEIF(ICONT(J).EQ.4.AND.IMAP(K,L).EQ.I)THEN
                 EZMAP(K,L)=TEMP(J)
            ELSEIF(ICONT(J).EQ.5.AND.IMAP(K,L).EQ.I)THEN
                 VMAP(K,L)=TEMP(J)
                 IF(I.EQ.1)THEN
                      VMMIN=TEMP(J)
                      VMMAX=TEMP(J)
                 ELSE
                      VMMIN=MIN(VMMIN,TEMP(J))
                      VMMAX=MAX(VMMAX,TEMP(J))
                 ENDIF
            ELSEIF(ICONT(J).EQ.6.AND.IMAP(K,L).EQ.I)THEN
                 BXMAP(K,L)=TEMP(J)
            ELSEIF(ICONT(J).EQ.7.AND.IMAP(K,L).EQ.I)THEN
                 BYMAP(K,L)=TEMP(J)
            ELSEIF(ICONT(J).EQ.8.AND.IMAP(K,L).EQ.I)THEN
                 BZMAP(K,L)=TEMP(J)
            ELSEIF(ICONT(J).EQ.-2.AND.IMAP(K,L).EQ.I)THEN
                 EPSNOD(K,L)=EPSVAL
            ELSEIF(ICONT(J).EQ.9.AND.IMAP(K,L).EQ.I.AND.L.EQ.1)THEN
                 IEPS=-1
                 DO 80 M=1,NEPS
                 IF(ABS(TEMP(J)-EPSMAT(M)).LT.1E-4*(ABS(TEMP(J))+
     -                ABS(EPSMAT(M))))IEPS=M
80               CONTINUE
                 IF(IEPS.LT.0.AND.NEPS.GE.MXEPS)THEN
                      PRINT *,' !!!!!! MAPFM6 WARNING : More media'//
     -                     ' than storage allows in file ',
     -                     FMAP(1:NCMAP),'; medium not assigned.'
                      RETURN
                 ELSEIF(IEPS.LT.0)THEN
                      NEPS=NEPS+1
                      IEPS=NEPS
                      EPSMAT(IEPS)=TEMP(J)
                      IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM6'',
     -                     '' DEBUG   : Adding dielectricum with'',
     -                     '' eps='',E10.3,''.'')') EPSMAT(IEPS)
                 ENDIF
                 MATMAP(K)=IEPS
            ELSEIF(ICONT(J).EQ.10+IWMAP*4-3.AND.IMAP(K,L).EQ.I)THEN
                 EWXMAP(K,L,IWMAP)=TEMP(J)
            ELSEIF(ICONT(J).EQ.11+IWMAP*4-3.AND.IMAP(K,L).EQ.I)THEN
                 EWYMAP(K,L,IWMAP)=TEMP(J)
            ELSEIF(ICONT(J).EQ.12+IWMAP*4-3.AND.IMAP(K,L).EQ.I)THEN
                 EWZMAP(K,L,IWMAP)=TEMP(J)
            ELSEIF(ICONT(J).EQ.13+IWMAP*4-3.AND.IMAP(K,L).EQ.I)THEN
                 VWMAP(K,L,IWMAP)=TEMP(J)
            ENDIF
40          CONTINUE
30          CONTINUE
20          CONTINUE
*   Next point.
10          CONTINUE
**  Flag those elements which have been defined.
            DO 70 I=1,IMAX
            IF(ICONT(I).GT.0)THEN
                 MAPFLG(ICONT(I))=.TRUE.
            ELSEIF(ICONT(I).EQ.-2.OR.ICONT(I).EQ.-3.OR.
     -           ICONT(I).EQ.-4)THEN
                 MAPFLG(10)=.TRUE.
            ENDIF
70          CONTINUE
**  Identify materials if both D and E are now available.
            IF(MAPFLG(2).AND.MAPFLG(3).AND.MAPFLG(4).AND.
     -           MAPFLG(10).AND..NOT.MAPFLG(9))THEN
*   Loop over the elements.
                 CALL PROFLD(2,'Epsilons',REAL(NMAP))
                 DO 50 I=1,NMAP
                 IF(I.EQ.MAX(1,NMAP/100)*(I/MAX(1,NMAP/100)))
     -           CALL PROSTA(2,REAL(I))
                 IEPS=-1
*   Try and find the epsilon from the node values.
                 EPSAVE=0
                 NAVER=0
                 DO 120 J=1,8
                 IF(EPSNOD(I,J).GT.0)THEN
                      EPSAVE=EPSAVE+EPSNOD(I,J)
                      NAVER=NAVER+1
                 ENDIF
120              CONTINUE
                 IF(NAVER.EQ.0)THEN
                      PRINT *,' !!!!!! MAPFM6 WARNING : No node'//
     -                     ' of element ',I,' has useable D/E data.'
                      EPSAVE=-1.0
                 ELSE
                      EPSAVE=EPSAVE/NAVER
                      EPSRMS=0
                      DO 130 J=1,8
                      EPSRMS=EPSRMS+(EPSNOD(I,J)-EPSAVE)**2
130                   CONTINUE
                      EPSRMS=SQRT(EPSRMS)
                      IF(EPSRMS.GT.1E-4*EPSAVE)THEN
                           PRINT *,' !!!!!! MAPFM6 WARNING : The'//
     -                          ' nodes of element ',I,' differ in'//
     -                          ' epsilon; picking one at random.'
                           DO 140 J=1,8
                           IF(EPSNOD(I,J).GT.0)EPSAVE=EPSNOD(I,J)
140                        CONTINUE
                      ENDIF
                 ENDIF
*   Match with existing epsilons.
                 DO 60 J=1,NEPS
                 IF(ABS(EPSAVE-EPSMAT(J)).LE.1E-4*
     -                (ABS(EPSMAT(J))+ABS(EPSAVE)))IEPS=J
60               CONTINUE
*   Check for |E|=|D|=0.
                 IF(EPSAVE.LE.0)PRINT *,' !!!!!! MAPFM6 WARNING :'//
     -                ' Assigning epsilon = -1 to element ',I,'.'
*   Warn if we run out of spcae.
                 IF(IEPS.LT.0.AND.NEPS.GE.MXEPS)THEN
                      PRINT *,' !!!!!! MAPFM6 WARNING : More media'//
     -                     ' than storage allows in file ',
     -                     FMAP(1:NCMAP),'; medium not assigned.'
                      RETURN
*   Add new epsilon to the table.
                 ELSEIF(IEPS.LT.0)THEN
                      NEPS=NEPS+1
                      IEPS=NEPS
                      EPSMAT(IEPS)=EPSAVE
                      IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM6'',
     -                     '' DEBUG   : Adding dielectricum with'',
     -                     '' eps='',E10.3,''.'')') EPSMAT(IEPS)
                 ENDIF
*   Assign the value.
                 MATMAP(I)=IEPS
50               CONTINUE
*   Set the flag.
                 MAPFLG(9)=.TRUE.
            ENDIF
       ENDIF
*** Seems to have worked, set error flag to OK and return.
       IFAIL=0
       RETURN
*** Error handling.
2000   CONTINUE
       PRINT *,' !!!!!! MAPFM6 WARNING : Premature end of file on ',
     -      FMAP(1:NCMAP),'; file not read.'
       IF(LDEBUG)CALL INPIOS(IOS)
       RETURN
2010   CONTINUE
       PRINT *,' !!!!!! MAPFM6 WARNING : Error while reading ',
     -      FMAP(1:NCMAP),'; file not read.'
       IF(LDEBUG)CALL INPIOS(IOS)
       RETURN
2040   CONTINUE
       PRINT *,' !!!!!! MAPFM6 WARNING : Error while rewinding ',
     -      FMAP(1:NCMAP),'; file not read.'
       IF(LDEBUG)CALL INPIOS(IOS)
       RETURN
       END
