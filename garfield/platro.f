CDECK  ID>, PLATRO.
       SUBROUTINE PLATRO(IREF,NREFO,IREFO,IFAIL)
*-----------------------------------------------------------------------
*   PLATRO - Cuts a polygon into right-angled triangles.
*   (Last changed on 21/10/10.)
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
       INTEGER NBEM,IREFB1(MXPLAN),NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,
     -      BEMNEW,BEMINV,BEMSLV
       DOUBLE PRECISION BEMQTH,BEMSTH,BEMSSC,BEMTGT,BEMEPA,BEMEPD
       LOGICAL LBDUMP
       COMMON /BEMDAT/ BEMQTH,BEMSSC,BEMSTH,BEMTGT,BEMEPA,BEMEPD,
     -      IREFB1,NBEM,NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,BEMNEW,
     -      BEMINV,BEMSLV,LBDUMP
       DOUBLE PRECISION XPL1(MXEDGE),YPL1(MXEDGE),ZPL1(MXEDGE),ZMEAN,
     -      XPL(MXEDGE),YPL(MXEDGE),ZPL(MXEDGE),XC,YC,
     -      EPSANG,EPSXYZ,AN1,BN1,CN1,DN1,DIST,A1,A2,A3
C           ,PROD
       INTEGER I,IREF,IFAIL,IFAIL1,IFAIL2,NPL1,ICOL1,IP1,JP1,I1,I2,I3,
     -      IREFO(MXPLAN),NREFO
       LOGICAL CROSSD,INSIDE,EDGE
       EXTERNAL CROSSD
*** Assume failure.
       IFAIL=1
*** Establish tolerances.
       EPSANG = BEMEPA
       EPSXYZ = BEMEPD
       CALL EPSSET('SET',EPSXYZ,EPSXYZ,EPSXYZ)
*** Zero the output buffer.
       NREFO=0
*** Retrieve the polygon.
       CALL PLABU2('READ',IREF,NPL1,XPL1,YPL1,ZPL1,
     -      AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
C       print *,'    Polygon of ',npl1,' nodes'
*   Delete the original.
       CALL PLABU2('DELETE',IREF,NPL1,XPL1,YPL1,ZPL1,
     -      AN1,BN1,CN1,DN1,ICOL1,IFAIL2)
*   Resume for another pass.
100    CONTINUE
*   Check successful reading.
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! PLATRO WARNING : Panel ',IREF,
     -           ' does not exist.'
            IFAIL=1
            RETURN
*   Too few nodes
       ELSEIF(NPL1.LE.2)THEN
            IFAIL=0
            RETURN
*   See whether this is a right-angled triangle.
       ELSEIF(NPL1.EQ.3.AND.
     -      ABS((XPL1(1)-XPL1(2))*(XPL1(3)-XPL1(2))+
     -          (YPL1(1)-YPL1(2))*(YPL1(3)-YPL1(2))).LT.EPSANG*
     -      SQRT(((XPL1(1)-XPL1(2))**2+(YPL1(1)-YPL1(2))**2)*
     -           ((XPL1(3)-XPL1(2))**2+(YPL1(3)-YPL1(2))**2)))THEN
C            print *,' Right-angled triangle node 2'
            IF(NREFO+1.LE.MXPLAN)THEN
                 NREFO=NREFO+1
                 CALL PLABU2('STORE',IREFO(NREFO),NPL1,XPL1,YPL1,ZPL1,
     -                AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
            ELSE
                 PRINT *,' !!!!!! PLATRO WARNING : Output list'//
     -                     ' too long.'
                 RETURN
            ENDIF
            IFAIL=0
            RETURN
*   See whether this is a right-angled triangle.
       ELSEIF(NPL1.EQ.3.AND.
     -      ABS((XPL1(1)-XPL1(3))*(XPL1(2)-XPL1(3))+
     -          (YPL1(1)-YPL1(3))*(YPL1(2)-YPL1(3))).LT.EPSANG*
     -      SQRT(((XPL1(1)-XPL1(3))**2+(YPL1(1)-YPL1(3))**2)*
     -           ((XPL1(2)-XPL1(3))**2+(YPL1(2)-YPL1(3))**2)))THEN
C            print *,' Right-angled triangle node 3'
            XPL(1)=XPL1(2)
            YPL(1)=YPL1(2)
            ZPL(1)=ZPL1(2)
            XPL(2)=XPL1(3)
            YPL(2)=YPL1(3)
            ZPL(2)=ZPL1(3)
            XPL(3)=XPL1(1)
            YPL(3)=YPL1(1)
            ZPL(3)=ZPL1(1)
            IF(NREFO+1.LE.MXPLAN)THEN
                 NREFO=NREFO+1
                 CALL PLABU2('STORE',IREFO(NREFO),3,XPL,YPL,ZPL,
     -                AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
            ELSE
                 PRINT *,' !!!!!! PLATRO WARNING : Output list'//
     -                     ' too long.'
                 RETURN
            ENDIF
            IFAIL=0
            RETURN
*   See whether this is a right-angled triangle.
       ELSEIF(NPL1.EQ.3.AND.
     -      ABS((XPL1(3)-XPL1(1))*(XPL1(2)-XPL1(1))+
     -          (YPL1(3)-YPL1(1))*(YPL1(2)-YPL1(1))).LT.EPSANG*
     -      SQRT(((XPL1(3)-XPL1(1))**2+(YPL1(3)-YPL1(1))**2)*
     -           ((XPL1(2)-XPL1(1))**2+(YPL1(2)-YPL1(1))**2)))THEN
C            print *,' Right-angled triangle node 1'
            XPL(1)=XPL1(3)
            YPL(1)=YPL1(3)
            ZPL(1)=ZPL1(3)
            XPL(2)=XPL1(1)
            YPL(2)=YPL1(1)
            ZPL(2)=ZPL1(1)
            XPL(3)=XPL1(2)
            YPL(3)=YPL1(2)
            ZPL(3)=ZPL1(2)
            IF(NREFO+1.LE.MXPLAN)THEN
                 NREFO=NREFO+1
                 CALL PLABU2('STORE',IREFO(NREFO),3,XPL,YPL,ZPL,
     -                AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
            ELSE
                 PRINT *,' !!!!!! PLATRO WARNING : Output list'//
     -                     ' too long.'
                 RETURN
            ENDIF
            IFAIL=0
            RETURN
*   See whether this is a rectangle.
       ELSEIF(NPL1.EQ.4.AND.
     -      ABS((XPL1(1)-XPL1(2))*(XPL1(3)-XPL1(2))+
     -          (YPL1(1)-YPL1(2))*(YPL1(3)-YPL1(2))).LT.EPSANG*
     -      SQRT(((XPL1(1)-XPL1(2))**2+(YPL1(1)-YPL1(2))**2)*
     -           ((XPL1(3)-XPL1(2))**2+(YPL1(3)-YPL1(2))**2)).AND.
     -      ABS((XPL1(2)-XPL1(3))*(XPL1(4)-XPL1(3))+
     -          (YPL1(2)-YPL1(3))*(YPL1(4)-YPL1(3))).LT.EPSANG*
     -      SQRT(((XPL1(2)-XPL1(3))**2+(YPL1(2)-YPL1(3))**2)*
     -           ((XPL1(4)-XPL1(3))**2+(YPL1(4)-YPL1(3))**2)).AND.
     -      ABS((XPL1(1)-XPL1(4))*(XPL1(3)-XPL1(4))+
     -          (YPL1(1)-YPL1(4))*(YPL1(3)-YPL1(4))).LT.EPSANG*
     -      SQRT(((XPL1(1)-XPL1(4))**2+(YPL1(1)-YPL1(4))**2)*
     -           ((XPL1(3)-XPL1(4))**2+(YPL1(3)-YPL1(4))**2)))THEN
C            print *,' Rectangle'
            IF(NREFO+1.LE.MXPLAN)THEN
                 NREFO=NREFO+1
                 CALL PLABU2('STORE',IREFO(NREFO),NPL1,XPL1,YPL1,ZPL1,
     -                AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
            ELSE
                 PRINT *,' !!!!!! PLATRO WARNING : Output list'//
     -                     ' too long.'
                 RETURN
            ENDIF
            IFAIL=0
            RETURN
       ENDIF
*** Scan over the nodes, finding the right angles
       ZMEAN=0
       DO 10 IP1=1,NPL1
*   Check for zero-length edges.
       DIST=SQRT(((XPL1(1+MOD(IP1-2+NPL1,NPL1))-XPL1(IP1))**2+
     -            (YPL1(1+MOD(IP1-2+NPL1,NPL1))-YPL1(IP1))**2)*
     -           ((XPL1(1+MOD(IP1,NPL1))       -XPL1(IP1))**2+
     -            (YPL1(1+MOD(IP1,NPL1))       -YPL1(IP1))**2))
       IF(DIST.LE.0)THEN
            PRINT *,' !!!!!! PLATRO WARNING : Found zero-length',
     -           ' edge in panel ',IREF,'; rejected.'
            RETURN
       ENDIF
C       PROD=((XPL1(1+MOD(IP1-2+NPL1,NPL1))-XPL1(IP1))*
C     -       (XPL1(1+MOD(IP1,NPL1))       -XPL1(IP1))+
C     -       (YPL1(1+MOD(IP1-2+NPL1,NPL1))-YPL1(IP1))*
C     -       (YPL1(1+MOD(IP1,NPL1))       -YPL1(IP1)))/DIST
*   Track the mean z.
       ZMEAN=ZMEAN+ZPL1(IP1)
10     CONTINUE
       ZMEAN=ZMEAN/NPL1
*   Preset the ZPL array to the mean z value.
       DO 40 I=1,MXEDGE
       ZPL(I)=ZMEAN
40     CONTINUE
*** Find a corner we can cut off.
       DO 20 IP1=1,NPL1
*   Ensure the midpoint is internal.
       IF(NPL1.GT.3)THEN
            CALL INTERD(NPL1,XPL1,YPL1,
     -           (XPL1(1+MOD(IP1-2+NPL1,NPL1))+XPL1(1+MOD(IP1,NPL1)))/2,
     -           (YPL1(1+MOD(IP1-2+NPL1,NPL1))+YPL1(1+MOD(IP1,NPL1)))/2,
     -           INSIDE,EDGE)
            IF(.NOT.INSIDE)GOTO 20
       ENDIF
*** Check all vertex crossings.
       DO 30 JP1=1,NPL1
*   Accept immediate contact.
       IF(  JP1            .EQ.1+MOD(IP1-2+NPL1,NPL1).OR.
     -      JP1            .EQ.IP1.OR.
     -      JP1            .EQ.1+MOD(IP1,NPL1).OR.
     -      1+MOD(JP1,NPL1).EQ.1+MOD(IP1-2+NPL1,NPL1).OR.
     -      1+MOD(JP1,NPL1).EQ.IP1.OR.
     -      1+MOD(JP1,NPL1).EQ.1+MOD(IP1,NPL1))GOTO 30
*   Check crossing.
       IF(CROSSD(XPL1(1+MOD(IP1-2+NPL1,NPL1)),
     -           YPL1(1+MOD(IP1-2+NPL1,NPL1)),
     -           XPL1(1+MOD(IP1,NPL1)),
     -           YPL1(1+MOD(IP1,NPL1)),
     -           XPL1(JP1),
     -           YPL1(JP1),
     -           XPL1(1+MOD(JP1,NPL1)),
     -           YPL1(1+MOD(JP1,NPL1))))GOTO 20
30     CONTINUE
*** Found a triangle, introduce shorthand node references.
       I1=1+MOD(IP1-2+NPL1,NPL1)
       I2=IP1
       I3=1+MOD(IP1,NPL1)
*   Find the biggest opening angle.
       A1=  ((XPL1(I2)-XPL1(I1))*(XPL1(I3)-XPL1(I1))+
     -       (YPL1(I2)-YPL1(I1))*(YPL1(I3)-YPL1(I1)))/
     -      SQRT(((XPL1(I2)-XPL1(I1))**2+(YPL1(I2)-YPL1(I1))**2)*
     -           ((XPL1(I3)-XPL1(I1))**2+(YPL1(I3)-YPL1(I1))**2))
       A2=  ((XPL1(I3)-XPL1(I2))*(XPL1(I1)-XPL1(I2))+
     -       (YPL1(I3)-YPL1(I2))*(YPL1(I1)-YPL1(I2)))/
     -      SQRT(((XPL1(I3)-XPL1(I2))**2+(YPL1(I3)-YPL1(I2))**2)*
     -           ((XPL1(I1)-XPL1(I2))**2+(YPL1(I1)-YPL1(I2))**2))
       A3=  ((XPL1(I1)-XPL1(I3))*(XPL1(I2)-XPL1(I3))+
     -       (YPL1(I1)-YPL1(I3))*(YPL1(I2)-YPL1(I3)))/
     -      SQRT(((XPL1(I1)-XPL1(I3))**2+(YPL1(I1)-YPL1(I3))**2)*
     -           ((XPL1(I2)-XPL1(I3))**2+(YPL1(I2)-YPL1(I3))**2))
C       print *,'    Angles: ',
C     -      180.0*acos(a1)/pi,180*acos(a2)/pi,180*acos(a3)/pi,
C     -      ', sum = ',180.0*(acos(a1)+acos(a2)+acos(a3))/pi
*   See whether one angle is more or less right-angled
       IF(ABS(A1).LT.EPSANG.OR.ABS(A2).LT.EPSANG.OR.
     -      ABS(A3).LT.EPSANG)THEN
            IF(ABS(A1).LT.EPSANG)THEN
                 XPL(1)=XPL1(I3)
                 YPL(1)=YPL1(I3)
                 XPL(2)=XPL1(I1)
                 YPL(2)=YPL1(I1)
                 XPL(3)=XPL1(I2)
                 YPL(3)=YPL1(I2)
            ELSEIF(ABS(A2).LT.EPSANG)THEN
                 XPL(1)=XPL1(I1)
                 YPL(1)=YPL1(I1)
                 XPL(2)=XPL1(I2)
                 YPL(2)=YPL1(I2)
                 XPL(3)=XPL1(I3)
                 YPL(3)=YPL1(I3)
            ELSE
                 XPL(1)=XPL1(I2)
                 YPL(1)=YPL1(I2)
                 XPL(2)=XPL1(I3)
                 YPL(2)=YPL1(I3)
                 XPL(3)=XPL1(I1)
                 YPL(3)=YPL1(I1)
            ENDIF
            IF(NREFO+1.LE.MXPLAN)THEN
                 NREFO=NREFO+1
                 CALL PLABU2('STORE',IREFO(NREFO),3,XPL,YPL,ZPL,
     -                AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
            ELSE
                 PRINT *,' !!!!!! PLATRO WARNING : Output list'//
     -                     ' too long.'
                 RETURN
            ENDIF
       ELSEIF(A1.LE.A2.AND.A1.LE.A3)THEN
            XC=XPL1(I2)+A2*(XPL1(I3)-XPL1(I2))*SQRT(
     -          ((XPL1(I1)-XPL1(I2))**2+(YPL1(I1)-YPL1(I2))**2)/
     -          ((XPL1(I3)-XPL1(I2))**2+(YPL1(I3)-YPL1(I2))**2))
            YC=YPL1(I2)+A2*(YPL1(I3)-YPL1(I2))*SQRT(
     -          ((XPL1(I1)-XPL1(I2))**2+(YPL1(I1)-YPL1(I2))**2)/
     -          ((XPL1(I3)-XPL1(I2))**2+(YPL1(I3)-YPL1(I2))**2))
            XPL(1)=XPL1(I3)
            YPL(1)=YPL1(I3)
            XPL(2)=XC
            YPL(2)=YC
            XPL(3)=XPL1(I1)
            YPL(3)=YPL1(I1)
            IF(NREFO+1.LE.MXPLAN)THEN
                 NREFO=NREFO+1
                 CALL PLABU2('STORE',IREFO(NREFO),3,XPL,YPL,ZPL,
     -                AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
            ELSE
                 PRINT *,' !!!!!! PLATRO WARNING : Output list'//
     -                     ' too long.'
                 RETURN
            ENDIF
            XPL(1)=XPL1(I2)
            YPL(1)=YPL1(I2)
            XPL(2)=XC
            YPL(2)=YC
            XPL(3)=XPL1(I1)
            YPL(3)=YPL1(I1)
            IF(NREFO+1.LE.MXPLAN)THEN
                 NREFO=NREFO+1
                 CALL PLABU2('STORE',IREFO(NREFO),3,XPL,YPL,ZPL,
     -                AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
            ELSE
                 PRINT *,' !!!!!! PLATRO WARNING : Output list'//
     -                     ' too long.'
                 RETURN
            ENDIF
       ELSEIF(A2.LE.A1.AND.A2.LE.A3)THEN
            XC=XPL1(I3)+A3*(XPL1(I1)-XPL1(I3))*SQRT(
     -          ((XPL1(I2)-XPL1(I3))**2+(YPL1(I2)-YPL1(I3))**2)/
     -          ((XPL1(I1)-XPL1(I3))**2+(YPL1(I1)-YPL1(I3))**2))
            YC=YPL1(I3)+A3*(YPL1(I1)-YPL1(I3))*SQRT(
     -          ((XPL1(I2)-XPL1(I3))**2+(YPL1(I2)-YPL1(I3))**2)/
     -          ((XPL1(I1)-XPL1(I3))**2+(YPL1(I1)-YPL1(I3))**2))
            XPL(1)=XPL1(I1)
            YPL(1)=YPL1(I1)
            XPL(2)=XC
            YPL(2)=YC
            XPL(3)=XPL1(I2)
            YPL(3)=YPL1(I2)
            IF(NREFO+1.LE.MXPLAN)THEN
                 NREFO=NREFO+1
                 CALL PLABU2('STORE',IREFO(NREFO),3,XPL,YPL,ZPL,
     -                AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
            ELSE
                 PRINT *,' !!!!!! PLATRO WARNING : Output list'//
     -                     ' too long.'
                 RETURN
            ENDIF
            XPL(1)=XPL1(I3)
            YPL(1)=YPL1(I3)
            XPL(2)=XC
            YPL(2)=YC
            XPL(3)=XPL1(I2)
            YPL(3)=YPL1(I2)
            IF(NREFO+1.LE.MXPLAN)THEN
                 NREFO=NREFO+1
                 CALL PLABU2('STORE',IREFO(NREFO),3,XPL,YPL,ZPL,
     -                AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
            ELSE
                 PRINT *,' !!!!!! PLATRO WARNING : Output list'//
     -                     ' too long.'
                 RETURN
            ENDIF
       ELSE
            XC=XPL1(I1)+A1*(XPL1(I2)-XPL1(I1))*SQRT(
     -          ((XPL1(I3)-XPL1(I1))**2+(YPL1(I3)-YPL1(I1))**2)/
     -          ((XPL1(I2)-XPL1(I1))**2+(YPL1(I2)-YPL1(I1))**2))
            YC=YPL1(I1)+A1*(YPL1(I2)-YPL1(I1))*SQRT(
     -          ((XPL1(I3)-XPL1(I1))**2+(YPL1(I3)-YPL1(I1))**2)/
     -          ((XPL1(I2)-XPL1(I1))**2+(YPL1(I2)-YPL1(I1))**2))
            XPL(1)=XPL1(I1)
            YPL(1)=YPL1(I1)
            XPL(2)=XC
            YPL(2)=YC
            XPL(3)=XPL1(I3)
            YPL(3)=YPL1(I3)
            IF(NREFO+1.LE.MXPLAN)THEN
                 NREFO=NREFO+1
                 CALL PLABU2('STORE',IREFO(NREFO),3,XPL,YPL,ZPL,
     -                AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
            ELSE
                 PRINT *,' !!!!!! PLATRO WARNING : Output list'//
     -                     ' too long.'
                 RETURN
            ENDIF
            XPL(1)=XPL1(I2)
            YPL(1)=YPL1(I2)
            XPL(2)=XC
            YPL(2)=YC
            XPL(3)=XPL1(I3)
            YPL(3)=YPL1(I3)
            IF(NREFO+1.LE.MXPLAN)THEN
                 NREFO=NREFO+1
                 CALL PLABU2('STORE',IREFO(NREFO),3,XPL,YPL,ZPL,
     -                AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
            ELSE
                 PRINT *,' !!!!!! PLATRO WARNING : Output list'//
     -                     ' too long.'
                 RETURN
            ENDIF
       ENDIF
*** Eliminate this node from the polygon.
       DO 50 JP1=1,NPL1
       IF(JP1.GT.IP1)THEN
            XPL1(JP1-1)=XPL1(JP1)
            YPL1(JP1-1)=YPL1(JP1)
       ELSEIF(JP1.LT.IP1)THEN
            XPL1(JP1)=XPL1(JP1)
            YPL1(JP1)=YPL1(JP1)
       ENDIF
50     CONTINUE
       NPL1=NPL1-1
       GOTO 100
20     CONTINUE
*** Seems to have worked.
       IFAIL=0
       END
