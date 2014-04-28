CDECK  ID>, MAPFM9.
       SUBROUTINE MAPFM9(FMAP,NCMAP,IDATA,IWMAP,IORDER,UNITD,IFAIL)
*-----------------------------------------------------------------------
*   MAPFM9 - Reads a COMSOL 2D map of 1st or 2nd order triangles
*   (Last changed on 11/ 3/14.)
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
       INTEGER I,J,K,L,NCMAP,IFAIL,IORDER,
     -      IH1,IH2,IH3,IH4,IH5,IH6,IDUM,
     -      IDATA,NTRIAN,NPOINT,IWMAP,IMAP(MXMAP,10),IOS
       REAL XP,YP,VREAD,UNITD
       CHARACTER*(*) FMAP
       CHARACTER*75 LINE
       SAVE NTRIAN,NPOINT,IMAP
       DATA NTRIAN/0/, NPOINT/0/
**** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MAPFM9 ///'
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM9 DEBUG   : Data = '',
     -      I3,'', IWMAP = '',I3,'', IORDER = '',I3/26X,''File = "'',
     -      A,''"'')') IDATA,IWMAP,IORDER,FMAP(1:NCMAP)
*** Assume that this will fail.
       IFAIL=1
*** Set the number of fields.
       IF(IORDER.LT.1.OR.IORDER.GT.2)
     -      PRINT *,' !!!!!! MAPFM9 WARNING : Invalid order parameter'//
     -      ' received: ',IORDER,'; reading as 1st order.'
*** We will only do Fortran reads, from the start.
       CALL INPSWI('RESTORE')
       REWIND(UNIT=12,ERR=2040,IOSTAT=IOS)
*** The first part contains the coordinates, which we skip at first
       CALL PROFLD(2,'Skipping',-1.0)
       CALL PROSTA(2,0.0)
*   Check the first header.
       READ(12,'(A75)',END=2000,ERR=2010,IOSTAT=IOS) LINE
       IF(LINE(1:13).NE.'% Coordinates')THEN
            PRINT *,' !!!!!! MAPFM9 WARNING : The first line of ',
     -           FMAP(1:NCMAP),' does not contain the expected'//
     -           ' COMSOL 2D header; file not read.'
            IFAIL=1
            RETURN
       ENDIF
*   Skip until the elements, counting the number of points.
       NPOINT=0
40     CONTINUE
       READ(12,'(A75)',END=2000,ERR=2010,IOSTAT=IOS) LINE
       IF(LINE(1:10).NE.'% Elements')THEN
            NPOINT=NPOINT+1
            GOTO 40
       ENDIF
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM9 DEBUG   :'',
     -      '' Expecting '',I5,'' points.'')') NPOINT
*** Read the triangle structure.
       CALL PROFLD(2,'Triangles',-1.0)
       CALL PROSTA(2,0.0)
*   Read lines with point references, until the next header
       NTRIAN=0
70     CONTINUE
       READ(12,'(A30)',END=2000,ERR=2010,IOSTAT=IOS) LINE(1:30)
       IF(LINE(1:30).EQ.' ')GOTO 70
       IF(LINE(1:1).NE.'%')THEN
*   Increment the triangle counter.
            IF(NTRIAN+1.GT.MXMAP)THEN
                 PRINT *,' !!!!!! MAPFM9 WARNING : Number of'//
     -                ' triangles in ',FMAP(1:NCMAP),
     -                ' exceeds compilation limit; file not read.'
                 IFAIL=1
                 RETURN
            ENDIF
            NTRIAN=NTRIAN+1
*   Decode a block of 8 line
            IF(IORDER.NE.2)THEN
                 READ(LINE,'(BN,3I10)',END=2000,ERR=2010,IOSTAT=IOS)
     -                IH1 ,IH2 ,IH3
                 IH4=0
                 IH5=0
                 IH6=0
            ELSEIF(IORDER.EQ.2)THEN
                 READ(LINE,'(BN,3I10)',END=2000,ERR=2010,IOSTAT=IOS)
     -                IH1 ,IH4 ,IH5
                 DO 50 I=1,3
                 READ(12,'(A30)',END=2000,ERR=2010,IOSTAT=IOS)
     -                LINE(1:30)
                 IF(LINE(1:30).EQ.' ')GOTO 50
                 IF(I.EQ.1)THEN
                      READ(LINE,'(BN,3I10)',END=2000,ERR=2010,
     -                     IOSTAT=IOS) IDUM,IH6,IDUM
                 ELSEIF(I.EQ.2)THEN
                      READ(LINE,'(BN,3I10)',END=2000,ERR=2010,
     -                     IOSTAT=IOS) IDUM,IH2,IDUM
                 ELSEIF(I.EQ.3)THEN
                      READ(LINE,'(BN,3I10)',END=2000,ERR=2010,
     -                     IOSTAT=IOS) IDUM,IDUM,IH3
                 ENDIF
50               CONTINUE
            ENDIF
*   Ensure there is no degeneracy
            IF(  (IORDER.NE.2.AND.(
     -           IH1.EQ.IH2.OR.IH1.EQ.IH3.OR.
     -                         IH2.EQ.IH3)).OR.
     -           (IORDER.EQ.2.AND.(
     -           IH1.EQ.IH2.OR.IH1.EQ.IH3.OR.IH1.EQ.IH4 .OR.
     -           IH1.EQ.IH5.OR.IH1.EQ.IH6.OR.
     -                         IH2.EQ.IH3.OR.IH2.EQ.IH4 .OR.
     -           IH2.EQ.IH5.OR.IH2.EQ.IH6.OR.
     -                                       IH3.EQ.IH4 .OR.
     -           IH3.EQ.IH5.OR.IH3.EQ.IH6.OR.
     -           IH4.EQ.IH5.OR.IH4.EQ.IH6.OR.
     -                         IH5.EQ.IH6)))THEN
                 PRINT *,' !!!!!! MAPFM9 WARNING : Element ',NTRIAN,
     -                ' in file ',FMAP(1:NCMAP),' is degenerate;'//
     -                ' file not read.'
                 IFAIL=1
                 RETURN
            ENDIF
*   Ensure they all make sense.
            IF(  (IORDER.NE.2.AND.(
     -           IH1.LE.0.OR.IH2.LE.0.OR.IH3.LE.0)).OR.
     -           (IORDER.EQ.2.AND.(
     -           IH1.LE.0.OR.IH2.LE.0.OR.IH3.LE.0.OR.IH4.LE.0.OR.
     -           IH5.LE.0.OR.IH6.LE.0)))THEN
                 PRINT *,' !!!!!! MAPFM9 WARNING : Invalid point'//
     -                ' reference in ',FMAP(1:NCMAP),'; map not read.'
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM9 DEBUG   :'',
     -                '' triangle '',I5/
     -                26X,''Vertices: '',6(2X,I5))')
     -                NTRIAN,IH1,IH2,IH3,IH4,IH5,IH6
                 IFAIL=1
                 RETURN
            ENDIF
*   Store the reference pointers (negative sign for checks).
            IMAP(NTRIAN,1) =-IH1
            IMAP(NTRIAN,2) =-IH2
            IMAP(NTRIAN,3) =-IH3
            IMAP(NTRIAN,4) =-IH4
            IMAP(NTRIAN,5) =-IH5
            IMAP(NTRIAN,6) =-IH6
*   Next line
            GOTO 70
       ENDIF
*   Debugging
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM9 DEBUG   :'',
     -      '' Found '',I5,'' triangles.'')') NTRIAN
*** Read the points filling in the mesh file.
       CALL PROFLD(2,'Rewind',-1.0)
       CALL PROSTA(2,0.0)
       REWIND(UNIT=12,ERR=2040,IOSTAT=IOS)
*** Skip vertex reading if this has already been done.
       IF(MAPFLG(1))GOTO 1234
*   Skip the header
       READ(12,'()',END=2000,ERR=2010,IOSTAT=IOS)
*   Loop over the points.
       CALL PROFLD(2,'Vertices',REAL(NPOINT))
       DO 80 I=1,NPOINT
       IF(I.EQ.MAX(1,NPOINT/100)*(I/MAX(1,NPOINT/100)))
     -      CALL PROSTA(2,REAL(I))
*   Read the line.
       READ(12,'(2F25.10)',END=2000,ERR=2010,IOSTAT=IOS) XP,YP
*   Convert from the COMSOL unit to cm.
       XP=XP*UNITD
       YP=YP*UNITD
*   Update the chamber dimensions.
       IF(I.EQ.1)THEN
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
*   Find referring triangles, trace resolved references with sign.
       DO 90 J=1,3
       DO 100 K=1,NTRIAN
       IF(I.EQ.ABS(IMAP(K,J)))THEN
            XMAP(K,J)=XP
            YMAP(K,J)=YP
            IMAP(K,J)=ABS(IMAP(K,J))
       ENDIF
100    CONTINUE
90     CONTINUE
*   Next point.
80     CONTINUE
*   End of reading, check reference resolution.
       CALL PROFLD(2,'Verifying',-1.0)
       CALL PROSTA(2,0.0)
       DO 110 J=1,3
       DO 120 I=1,NTRIAN
       IF(IMAP(I,J).LE.0)THEN
            PRINT *,' !!!!!! MAPFM9 WARNING : Unresolved point'//
     -           ' references in mesh ; map rejected.'
            IFAIL=1
            RETURN
       ENDIF
120    CONTINUE
110    CONTINUE
*   Preset the material flags.
       DO 130 I=1,NTRIAN
       MATMAP(I)=-1
130    CONTINUE
*   Now set the number of elements.
       NMAP=NTRIAN
*   Set the flag that the mesh is now defined.
       MAPFLG(1)=.TRUE.
*   Set the element type.
       MAPTYP=2
*** And read the potentials, first skip till we have the header.
1234   CONTINUE
       CALL PROFLD(2,'Skipping',-1.0)
       CALL PROSTA(2,0.0)
140    CONTINUE
       READ(12,'(A75)',END=2000,ERR=2010,IOSTAT=IOS) LINE
       IF(LINE(1:10).NE.'% Data (V)'.AND.
     -    LINE(1:19).NE.'% Data ((V)[1/(V)])'.AND.
     -    LINE(1:19).NE.'% Data (V (V))')GOTO 140
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
       READ(12,'(F25.10)',END=2000,ERR=2010,IOSTAT=IOS) VREAD
*   Assign the potential to the mesh.
       DO 20 L=1,6
       DO 30 K=1,NTRIAN
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
*** Seems to have worked, set error flag to OK and return.
       IFAIL=0
       RETURN
*** Error handling.
2000   CONTINUE
       PRINT *,' !!!!!! MAPFM9 WARNING : Premature end of file on ',
     -      FMAP(1:NCMAP),'; file not read.'
       IF(LDEBUG)CALL INPIOS(IOS)
       RETURN
2010   CONTINUE
       PRINT *,' !!!!!! MAPFM9 WARNING : Error while reading ',
     -      FMAP(1:NCMAP),'; file not read.'
       IF(LDEBUG)CALL INPIOS(IOS)
       RETURN
2040   CONTINUE
       PRINT *,' !!!!!! MAPFM9 WARNING : Error while rewinding ',
     -      FMAP(1:NCMAP),'; file not read.'
       IF(LDEBUG)CALL INPIOS(IOS)
       RETURN
       END
