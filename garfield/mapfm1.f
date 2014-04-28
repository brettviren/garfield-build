CDECK  ID>, MAPFM1.
       SUBROUTINE MAPFM1(FMAP,NCMAP,IDATA,IWMAP,IFAIL)
*-----------------------------------------------------------------------
*   MAPFM1 - Reads a QuickField 2D map of 1st order triangles
*   (Last changed on 14/ 7/05.)
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
       INTEGER I,J,K,NCMAP,IFAIL,IT1,IT2,IT3,IMAT,IP,IDATA,
     -      NTRIAN,NVAL,NLABEL,NEDGE,NPOINT,IWMAP,IMAP(MXMAP,10),IOS
       REAL XP,YP,V1,EX1,EY1,V2,EX2,EY2,V3,EX3,EY3,EPSXX,EPSYY
       CHARACTER*(*) FMAP
       SAVE NTRIAN,NPOINT,IMAP
       DATA NTRIAN/0/, NPOINT/0/
**** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MAPFM1 ///'
*** Assume that this will fail.
       IFAIL=1
*** We will only do Fortran reads, from the start.
       CALL INPSWI('RESTORE')
       REWIND(UNIT=12,ERR=2040,IOSTAT=IOS)
*** The first line contains the number of elements, nodes etc.
       READ(12,'(BN,5I8)',END=2000,ERR=2010,IOSTAT=IOS)
     -      NPOINT,NTRIAN,NVAL,NLABEL,NEDGE
*   Be sure we can read this
       IF(NVAL.NE.3)THEN
            PRINT *,' !!!!!! MAPFM1 WARNING : Number of field values'//
     -           ' per element not as expected; file not read.'
            IFAIL=1
            RETURN
       ELSEIF(NLABEL.LT.0.OR.NLABEL+1.GT.MXEPS)THEN
            PRINT *,' !!!!!! MAPFM1 WARNING : Number of block labels'//
     -           ' not as expected; file not read.'
            IFAIL=1
            RETURN
       ENDIF
*** The first part contains the coordinates, which we skip at first
       CALL PROFLD(2,'Skipping',-1.0)
       CALL PROSTA(2,0.0)
       DO 10 I=1,NPOINT
       READ(12,'()',END=2000,ERR=2010,IOSTAT=IOS)
10     CONTINUE
*** Read the triangle structure.
       CALL PROFLD(2,'Triangles',REAL(NTRIAN))
*   Read the element table.
       DO 20 I=1,NTRIAN
       IF(I.EQ.MAX(1,NTRIAN/100)*(I/MAX(1,NTRIAN/100)))
     -      CALL PROSTA(2,REAL(I))
       READ(12,'(4I8,9E14.4)',END=2000,ERR=2010,IOSTAT=IOS)
     -      IT1,IT2,IT3,IMAT,V1,EX1,EY1,V2,EX2,EY2,V3,EX3,EY3
       IT1=IT1+1
       IT2=IT2+1
       IT3=IT3+1
       EX1=EX1/100
       EX2=EX2/100
       EX3=EX3/100
       EY1=EY1/100
       EY2=EY2/100
       EY3=EY3/100
*   Ensure the node references make sense.
       IF(IT1.LE.0.OR.IT1.GT.NPOINT.OR.
     -      IT2.LE.0.OR.IT2.GT.NPOINT.OR.
     -      IT3.LE.0.OR.IT3.GT.NPOINT)THEN
            PRINT *,' !!!!!! MAPFM1 WARNING : Invalid point'//
     -           ' reference in ',FMAP(1:NCMAP),'; map not read.'
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM1 DEBUG   :'',
     -           '' triangle '',I5/26X,''Vertices: '',6(2X,I5))')
     -           NTRIAN,IT1,IT2,IT3
            IFAIL=1
            RETURN
       ENDIF
*   Store the reference pointers (negative sign for checks).
       IMAP(I,1) =-IT1
       IMAP(I,2) =-IT2
       IMAP(I,3) =-IT3
*   Store field potential and field
       VMAP(I,1)=V1
       VMAP(I,2)=V2
       VMAP(I,3)=V3
       EXMAP(I,1)=EX1
       EXMAP(I,2)=EX2
       EXMAP(I,3)=EX3
       EYMAP(I,1)=EY1
       EYMAP(I,2)=EY2
       EYMAP(I,3)=EY3
*   Keep track of the potential range
       IF(I.EQ.1)THEN
            VMMIN=MIN(V1,V2,V3)
            VMMAX=MAX(V1,V2,V3)
       ELSE
            VMMIN=MIN(VMMIN,V1,V2,V3)
            VMMAX=MAX(VMMAX,V1,V2,V3)
       ENDIF
*   Store the material flag
       MATMAP(I)=IMAT+1
20     CONTINUE
*** Read the material properties
       CALL PROFLD(2,'Materials',REAL(NLABEL))
       DO 80 I=1,NLABEL
       IF(I.EQ.MAX(1,NLABEL/100)*(I/MAX(1,NLABEL/100)))
     -      CALL PROSTA(2,REAL(I))
       READ(12,'(16X,4X,2E14.5)',END=2000,ERR=2010,IOSTAT=IOS)
     -      EPSXX,EPSYY
       EPSMAT(I)=(EPSXX+EPSYY)/2
80     CONTINUE
*** Read the points filling in the mesh file.
       CALL PROFLD(2,'Rewind',-1.0)
       CALL PROSTA(2,0.0)
       REWIND(UNIT=12,ERR=2040,IOSTAT=IOS)
*   Skip the header
       READ(12,'()',END=2000,ERR=2010,IOSTAT=IOS)
*   Loop over the points.
       CALL PROFLD(2,'Vertices',REAL(NPOINT))
       DO 30 I=1,NPOINT
       IF(I.EQ.MAX(1,NPOINT/100)*(I/MAX(1,NPOINT/100)))
     -      CALL PROSTA(2,REAL(I))
*   Read the line.
       READ(12,'(I8,2E14.5)',END=2000,ERR=2010,IOSTAT=IOS) IP,XP,YP
       XP=XP/10000
       YP=YP/10000
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
       DO 40 J=1,3
       DO 50 K=1,NTRIAN
       IF(I.EQ.ABS(IMAP(K,J)))THEN
            XMAP(K,J)=XP
            YMAP(K,J)=YP
            IMAP(K,J)=ABS(IMAP(K,J))
       ENDIF
50     CONTINUE
40     CONTINUE
*   Next point.
30     CONTINUE
*   End of reading, check reference resolution.
       CALL PROFLD(2,'Verifying',-1.0)
       CALL PROSTA(2,0.0)
       DO 70 I=1,NTRIAN
       DO 60 J=1,3
       IF(IMAP(I,J).LE.0)THEN
            PRINT *,' !!!!!! MAPFM1 WARNING : Unresolved point'//
     -           ' references in mesh ; map rejected.'
            IFAIL=1
            RETURN
       ENDIF
60     CONTINUE
70     CONTINUE
*** Set the flag that the mesh is now defined.
       MAPFLG(1)=.TRUE.
*   Number of elements
       NMAP=NTRIAN
*   Set the element type
       MAPTYP=1
*   Flags that the field is now defined.
       MAPFLG(2)=.TRUE.
       MAPFLG(3)=.TRUE.
       MAPFLG(4)=.FALSE.
*   Flag that the potential is now defined.
       MAPFLG(5)=.TRUE.
*   Flag that the materials are set
       MAPFLG(9)=.TRUE.
*   Number of materials
       NEPS=NLABEL
*** Seems to have worked, set error flag to OK and return.
       IFAIL=0
       RETURN
*** Error handling.
2000   CONTINUE
       PRINT *,' !!!!!! MAPFM1 WARNING : Premature end of file on ',
     -      FMAP(1:NCMAP),'; file not read.'
       IF(LDEBUG)CALL INPIOS(IOS)
       RETURN
2010   CONTINUE
       PRINT *,' !!!!!! MAPFM1 WARNING : Error while reading ',
     -      FMAP(1:NCMAP),'; file not read.'
       IF(LDEBUG)CALL INPIOS(IOS)
       RETURN
2040   CONTINUE
       PRINT *,' !!!!!! MAPFM1 WARNING : Error while rewinding ',
     -      FMAP(1:NCMAP),'; file not read.'
       IF(LDEBUG)CALL INPIOS(IOS)
       RETURN
       END
