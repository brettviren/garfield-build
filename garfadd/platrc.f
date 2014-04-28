CDECK  ID>, PLATRC.
       SUBROUTINE PLATRC(IREF,NREFO,IREFO,IFAIL)
*-----------------------------------------------------------------------
*   PLATRC - Cuts a polygon into right-angled triangles.
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
     -      EPSANG,EPSXYZ,AN1,BN1,CN1,DN1,A1,A2,A3,
     -      XL1, XL2, XL3, XL4
       INTEGER I,IREF,IFAIL,IFAIL1,IFAIL2,NPL1,ICOL1,IP1,JP1,I1,I2,I3,
     -      IREFO(MXPLAN),NREFO,JREFO,NNEW
       LOGICAL CROSSD,INSIDE,EDGE,CORNER
       EXTERNAL CROSSD
*** Assume failure.
       IFAIL=1
*** Establish tolerances.
       EPSANG = BEMEPA
       EPSXYZ = BEMEPD
       CALL EPSSET('SET',EPSXYZ,EPSXYZ,EPSXYZ)
*** Zero the output buffer.
       NREFO=1
       IREFO(NREFO)=IREF
       JREFO=1
*** Next polygon.
 1000  CONTINUE
C       print *,' At JREFo = ',jrefo,' / NREFo = ',nrefo
C       read '(a)'
*   If done, remove spurious elements from the list.
       IF(JREFO.GT.NREFO)THEN
            NNEW=0
            DO 1010 I=1,NREFO
            IF(IREFO(I).GT.0)THEN
                 NNEW=NNEW+1
                 IREFO(NNEW)=IREFO(I)
            ENDIF
 1010       CONTINUE
            NREFO=NNEW
C            print *,' Done, produced ',NREFO,' panels'
            IFAIL=0
            RETURN
       ENDIF
*** Retrieve the polygon.
       IF(IREFO(JREFO).LT.0)THEN
C            print *,' *** No polygon for JREFO = ',jrefo
            jrefo=jrefo+1
            goto 1000
       ENDIF
       CALL PLABU2('READ',IREFO(JREFO),NPL1,XPL1,YPL1,ZPL1,
     -      AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
C       print *,' Polygon of ',npl1,' nodes, IFAIL =',ifail1
*** Dump
       IF(LBDUMP.AND.JREFO.EQ.1)THEN
            OPEN(UNIT=12,FILE='platrc.dump',ACCESS='APPEND')
            WRITE(12,'(I5,4E15.8)') NPL1,AN1,BN1,CN1,DN1
            DO 1020 I=1,NPL1
            WRITE(12,'(3E15.8)') XPL1(I),YPL1(I),ZPL1(I)
 1020       CONTINUE
            CLOSE(UNIT=12)
       ENDIF
*   Check successful reading.
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! PLATRC WARNING : Panel ',IREFO(JREFO),
     -           ' does not exist.'
            JREFO=JREFO+1
            GOTO 1000
*   Too few nodes
       ELSEIF(NPL1.LE.2)THEN
C            print *,' *** Too few points: ',npl1
            CALL PLABU2('DELETE',IREFO(JREFO),NPL1,XPL1,YPL1,ZPL1,
     -           AN1,BN1,CN1,DN1,ICOL1,IFAIL2)
            IREFO(JREFO)=-1
            JREFO=JREFO+1
            GOTO 1000
       ENDIF
*   Track the mean z and preset ZPL to the mean z value
       ZMEAN=0
       DO 10 IP1=1,NPL1
       ZMEAN=ZMEAN+ZPL1(IP1)
10     CONTINUE
       ZMEAN=ZMEAN/NPL1
       DO 40 I=1,MXEDGE
       ZPL(I)=ZMEAN
40     CONTINUE
*** Resume for another pass with the same polygon.
100    CONTINUE
*   Fewer than 3 points: done
       IF(NPL1.LE.2)THEN
C            print *,' Only NPL = ',npl1,' points left - done'
            CALL PLABU2('DELETE',IREFO(JREFO),NPL1,XPL1,YPL1,ZPL1,
     -           AN1,BN1,CN1,DN1,ICOL1,IFAIL2)
            IREFO(JREFO)=-1
            JREFO=JREFO+1
            GOTO 1000
*   See whether this is a right-angled triangle.
       ELSEIF(NPL1.EQ.3.AND.
     -      ABS((XPL1(1)-XPL1(2))*(XPL1(3)-XPL1(2))+
     -          (YPL1(1)-YPL1(2))*(YPL1(3)-YPL1(2))).LT.EPSANG*
     -      SQRT(((XPL1(1)-XPL1(2))**2+(YPL1(1)-YPL1(2))**2)*
     -           ((XPL1(3)-XPL1(2))**2+(YPL1(3)-YPL1(2))**2)))THEN
C            print *,' Right-angled triangle node 2 - done'
            JREFO=JREFO+1
            GOTO 1000
*   See whether this is a right-angled triangle.
       ELSEIF(NPL1.EQ.3.AND.
     -      ABS((XPL1(1)-XPL1(3))*(XPL1(2)-XPL1(3))+
     -          (YPL1(1)-YPL1(3))*(YPL1(2)-YPL1(3))).LT.EPSANG*
     -      SQRT(((XPL1(1)-XPL1(3))**2+(YPL1(1)-YPL1(3))**2)*
     -           ((XPL1(2)-XPL1(3))**2+(YPL1(2)-YPL1(3))**2)))THEN
C            print *,' Right-angled triangle node 3 - rearrange'
            XPL(1)=XPL1(2)
            YPL(1)=YPL1(2)
            ZPL(1)=ZPL1(2)
            XPL(2)=XPL1(3)
            YPL(2)=YPL1(3)
            ZPL(2)=ZPL1(3)
            XPL(3)=XPL1(1)
            YPL(3)=YPL1(1)
            ZPL(3)=ZPL1(1)
            CALL PLABU2('DELETE',IREFO(JREFO),NPL1,XPL1,YPL1,ZPL1,
     -           AN1,BN1,CN1,DN1,ICOL1,IFAIL2)
            IREFO(JREFO)=-1
            IF(NREFO+1.LE.MXPLAN)THEN
                 NREFO=NREFO+1
                 CALL PLABU2('STORE',IREFO(NREFO),3,XPL,YPL,ZPL,
     -                AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
            ELSE
                 PRINT *,' !!!!!! PLATRC WARNING : Output list'//
     -                     ' too long.'
                 RETURN
            ENDIF
            JREFO=JREFO+1
            GOTO 1000
*   See whether this is a right-angled triangle.
       ELSEIF(NPL1.EQ.3.AND.
     -      ABS((XPL1(3)-XPL1(1))*(XPL1(2)-XPL1(1))+
     -          (YPL1(3)-YPL1(1))*(YPL1(2)-YPL1(1))).LT.EPSANG*
     -      SQRT(((XPL1(3)-XPL1(1))**2+(YPL1(3)-YPL1(1))**2)*
     -           ((XPL1(2)-XPL1(1))**2+(YPL1(2)-YPL1(1))**2)))THEN
C            print *,' Right-angled triangle node 1 - rearrange'
            XPL(1)=XPL1(3)
            YPL(1)=YPL1(3)
            ZPL(1)=ZPL1(3)
            XPL(2)=XPL1(1)
            YPL(2)=YPL1(1)
            ZPL(2)=ZPL1(1)
            XPL(3)=XPL1(2)
            YPL(3)=YPL1(2)
            ZPL(3)=ZPL1(2)
            CALL PLABU2('DELETE',IREFO(JREFO),NPL1,XPL1,YPL1,ZPL1,
     -           AN1,BN1,CN1,DN1,ICOL1,IFAIL2)
            IREFO(JREFO)=-1
            IF(NREFO+1.LE.MXPLAN)THEN
                 NREFO=NREFO+1
                 CALL PLABU2('STORE',IREFO(NREFO),3,XPL,YPL,ZPL,
     -                AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
            ELSE
                 PRINT *,' !!!!!! PLATRC WARNING : Output list'//
     -                     ' too long.'
                 RETURN
            ENDIF
            JREFO=JREFO+1
            GOTO 1000
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
            JREFO=JREFO+1
            GOTO 1000
**  See whether there are parallel sides, e.g. a trapezium (UK English).
       ELSEIF(NPL1.GE.4)THEN
            DO 200 IP1=1,NPL1
            DO 210 JP1=IP1+2,NPL1
*   Skip adjacent segments.
            IF(  IP1.EQ.JP1.OR.
     -           IP1.EQ.1+MOD(JP1,NPL1).OR.
     -           1+MOD(IP1,NPL1).EQ.JP1.OR.
     -           1+MOD(IP1,NPL1).EQ.1+MOD(JP1,NPL1))GOTO 210
*   Require parallelism.
            IF(ABS((XPL1(IP1)-XPL1(1+MOD(IP1,NPL1)))*
     -             (XPL1(JP1)-XPL1(1+MOD(JP1,NPL1)))+
     -             (YPL1(IP1)-YPL1(1+MOD(IP1,NPL1)))*
     -             (YPL1(JP1)-YPL1(1+MOD(JP1,NPL1)))+
     -       SQRT(((XPL1(IP1)-XPL1(1+MOD(IP1,NPL1)))**2+
     -             (YPL1(IP1)-YPL1(1+MOD(IP1,NPL1)))**2)*
     -            ((XPL1(JP1)-XPL1(1+MOD(JP1,NPL1)))**2+
     -             (YPL1(JP1)-YPL1(1+MOD(JP1,NPL1)))**2))).GT.EPSANG*
     -       SQRT(((XPL1(IP1)-XPL1(1+MOD(IP1,NPL1)))**2+
     -             (YPL1(IP1)-YPL1(1+MOD(IP1,NPL1)))**2)*
     -            ((XPL1(JP1)-XPL1(1+MOD(JP1,NPL1)))**2+
     -             (YPL1(JP1)-YPL1(1+MOD(JP1,NPL1)))**2)))GOTO 210
C      print *,' Found parallel sections: ',IP1,JP1
*   Avoid division by zero
            IF(  (XPL1(JP1)-XPL1(1+MOD(JP1,NPL1)))**2+
     -           (YPL1(JP1)-YPL1(1+MOD(JP1,NPL1)))**2.LE.0.OR.
     -           (XPL1(IP1)-XPL1(1+MOD(IP1,NPL1)))**2+
     -           (YPL1(IP1)-YPL1(1+MOD(IP1,NPL1)))**2.LE.0)THEN
                 PRINT *,' !!!!!! PLATRC WARNING : Zero norm'//
     -                ' segment found; skipped.'
                 GOTO 210
            ENDIF
*   Establish the cutting lines
            XL1 =((XPL1(IP1)            -XPL1(JP1))*
     -            (XPL1(1+MOD(JP1,NPL1))-XPL1(JP1))+
     -            (YPL1(IP1)            -YPL1(JP1))*
     -            (YPL1(1+MOD(JP1,NPL1))-YPL1(JP1)))/
     -           ((XPL1(JP1)-XPL1(1+MOD(JP1,NPL1)))**2+
     -            (YPL1(JP1)-YPL1(1+MOD(JP1,NPL1)))**2)
            XL2 =((XPL1(1+MOD(IP1,NPL1))-XPL1(JP1))*
     -            (XPL1(1+MOD(JP1,NPL1))-XPL1(JP1))+
     -            (YPL1(1+MOD(IP1,NPL1))-YPL1(JP1))*
     -            (YPL1(1+MOD(JP1,NPL1))-YPL1(JP1)))/
     -           ((XPL1(JP1)-XPL1(1+MOD(JP1,NPL1)))**2+
     -            (YPL1(JP1)-YPL1(1+MOD(JP1,NPL1)))**2)
            XL3 =((XPL1(JP1)            -XPL1(IP1))*
     -            (XPL1(1+MOD(IP1,NPL1))-XPL1(IP1))+
     -            (YPL1(JP1)            -YPL1(IP1))*
     -            (YPL1(1+MOD(IP1,NPL1))-YPL1(IP1)))/
     -           ((XPL1(IP1)-XPL1(1+MOD(IP1,NPL1)))**2+
     -            (YPL1(IP1)-YPL1(1+MOD(IP1,NPL1)))**2)
            XL4 =((XPL1(1+MOD(JP1,NPL1))-XPL1(IP1))*
     -            (XPL1(1+MOD(IP1,NPL1))-XPL1(IP1))+
     -            (YPL1(1+MOD(JP1,NPL1))-YPL1(IP1))*
     -            (YPL1(1+MOD(IP1,NPL1))-YPL1(IP1)))/
     -           ((XPL1(IP1)-XPL1(1+MOD(IP1,NPL1)))**2+
     -            (YPL1(IP1)-YPL1(1+MOD(IP1,NPL1)))**2)
C      print *,' xl1 = ',xl1,' xl2 = ',xl2,' xl3 = ',xl3,' xl4 = ',xl4
*   Check that there is at all a rectangle.
            IF(((XL1+EPSANG)*(1+EPSANG-XL1).LT.0.AND.
     -          (XL4+EPSANG)*(1+EPSANG-XL4).LT.0).OR.
     -         ((XL2+EPSANG)*(1+EPSANG-XL2).LT.0.AND.
     -          (XL3+EPSANG)*(1+EPSANG-XL3).LT.0))THEN
C      print *,' No rectangle'
                 GOTO 210
            ENDIF
*   Add the rectangular part.
            IF((XL1+EPSANG)*(1+EPSANG-XL1).GE.0)THEN
                 XPL(1)=XPL1(IP1)
                 YPL(1)=YPL1(IP1)
                 XPL(2)=XPL1(JP1)+XL1*(XPL1(1+MOD(JP1,NPL1))-XPL1(JP1))
                 YPL(2)=YPL1(JP1)+XL1*(YPL1(1+MOD(JP1,NPL1))-YPL1(JP1))
            ELSEIF((XL4+EPSANG)*(1+EPSANG-XL4).GE.0)THEN
                 XPL(1)=XPL1(IP1)+XL4*(XPL1(1+MOD(IP1,NPL1))-XPL1(IP1))
                 YPL(1)=YPL1(IP1)+XL4*(YPL1(1+MOD(IP1,NPL1))-YPL1(IP1))
                 XPL(2)=XPL1(1+MOD(JP1,NPL1))
                 YPL(2)=YPL1(1+MOD(JP1,NPL1))
            ENDIF
            IF((XL2+EPSANG)*(1+EPSANG-XL2).GE.0)THEN
                 XPL(3)=XPL1(JP1)+XL2*(XPL1(1+MOD(JP1,NPL1))-XPL1(JP1))
                 YPL(3)=YPL1(JP1)+XL2*(YPL1(1+MOD(JP1,NPL1))-YPL1(JP1))
                 XPL(4)=XPL1(1+MOD(IP1,NPL1))
                 YPL(4)=YPL1(1+MOD(IP1,NPL1))
            ELSEIF((XL3+EPSANG)*(1+EPSANG-XL3).GE.0)THEN
                 XPL(3)=XPL1(JP1)
                 YPL(3)=YPL1(JP1)
                 XPL(4)=XPL1(IP1)+XL3*(XPL1(1+MOD(IP1,NPL1))-XPL1(IP1))
                 YPL(4)=YPL1(IP1)+XL3*(YPL1(1+MOD(IP1,NPL1))-YPL1(IP1))
            ENDIF
*   Verify that the midpoints of these lines are internal
            CALL INTERD(NPL1,XPL1,YPL1,
     -           (XPL(1)+XPL(2))/2,(YPL(1)+YPL(2))/2,
     -           INSIDE,EDGE)
            IF(.NOT.(INSIDE.OR.EDGE))THEN
C                 print *,' *** Midpoint 1 not internal'
                 GOTO 210
            ENDIF
            CALL INTERD(NPL1,XPL1,YPL1,
     -           (XPL(3)+XPL(4))/2,(YPL(3)+YPL(4))/2,
     -           INSIDE,EDGE)
            IF(.NOT.(INSIDE.OR.EDGE))THEN
C                 print *,' *** Midpoint 2 not internal'
                 GOTO 210
            ENDIF
*   Ensure there are no crossings, accepting contact.
            DO 240 I=1,NPL1
            IF((I.EQ.1+MOD(IP1-2+NPL1,NPL1).AND.
     -          (XL1+EPSANG)*(1+EPSANG-XL1).GE.0)              .OR.
     -          I.EQ.IP1                                       .OR.
     -         (I.EQ.1+MOD(IP1,NPL1).AND.
     -          (XL2+EPSANG)*(1+EPSANG-XL2).GE.0)              .OR.
     -         (I.EQ.1+MOD(JP1-2+NPL1,NPL1).AND.
     -          (XL3+EPSANG)*(1+EPSANG-XL3).GE.0)              .OR.
     -          I.EQ.JP1                                       .OR.
     -         (I.EQ.1+MOD(JP1,NPL1).AND.
     -          (XL4+EPSANG)*(1+EPSANG-XL4).GE.0))GOTO 240
            IF(CROSSD(XPL1(I),YPL1(I),
     -                XPL1(1+MOD(I,NPL1)),YPL1(1+MOD(I,NPL1)),
     -                XPL(1),YPL(1),XPL(2),YPL(2)).OR.
     -         CROSSD(XPL1(I),YPL1(I),
     -                XPL1(1+MOD(I,NPL1)),YPL1(1+MOD(I,NPL1)),
     -                XPL(3),YPL(3),XPL(4),YPL(4)))THEN
C                 print *,' *** Crossing'
C                 print *,'     Edge I/I+1',I,I+1
C                 print *,'     IP1 = ',IP1,' JP1 = ',JP1
C                 do j=1,4
C                    print '(2x,i10,2f10.3)',j,xpl(j),ypl(j)
C                 enddo
                 GOTO 210
            ENDIF
240         CONTINUE
*   Add the rectangular part.
            IF(  (ABS(XL1).LT.EPSANG.AND.ABS(XL3).LT.EPSANG).OR.
     -           (ABS(1-XL2).LT.EPSANG.AND.ABS(1-XL4).LT.EPSANG))THEN
C                 print *,' *** Not stored, degenerate'
C                 do i=1,4
C                    print '(2x,i10,2f10.3)',i,xpl(i),ypl(i)
C                 enddo
            ELSEIF(NREFO+1.LE.MXPLAN)THEN
                 NREFO=NREFO+1
                 CALL PLABU2('STORE',IREFO(NREFO),4,XPL,YPL,ZPL,
     -                AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
C                 print *,' Adding rectangle'
C                 print *,' Ref = ',IREFO(NREFO)
C                 do i=1,4
C                    print '(2x,i10,2f10.3)',i,xpl(i),ypl(i)
C                 enddo
            ELSE
                 PRINT *,' !!!!!! PLATRC WARNING : Output list'//
     -                     ' too long.'
                 RETURN
            ENDIF
*   First non-rectangular section.
            DO 220 I=JP1+1,IP1+NPL1
            XPL(I-JP1)=XPL1(1+MOD(I-1+NPL1,NPL1))
            YPL(I-JP1)=YPL1(1+MOD(I-1+NPL1,NPL1))
 220        CONTINUE
            IF(  (XL1+EPSANG)*(1+EPSANG-XL1).GE.0.AND.
     -           (XL4+EPSANG)*(1+EPSANG-XL4).GE.0)THEN
C                 print *,' 1-4 degenerate'
                 NNEW=IP1+NPL1-JP1
            ELSEIF((XL1+EPSANG)*(1+EPSANG-XL1).GE.0)THEN
C                 print *,' Using 1'
                 NNEW=IP1+NPL1-JP1+1
                 XPL(NNEW)=
     -                XPL1(JP1)+XL1*(XPL1(1+MOD(JP1,NPL1))-XPL1(JP1))
                 YPL(NNEW)=
     -                YPL1(JP1)+XL1*(YPL1(1+MOD(JP1,NPL1))-YPL1(JP1))
            ELSEIF((XL4+EPSANG)*(1+EPSANG-XL4).GE.0)THEN
C                 print *,' Using 4'
                 NNEW=IP1+NPL1-JP1+1
                 XPL(NNEW)=
     -                XPL1(IP1)+XL4*(XPL1(1+MOD(IP1,NPL1))-XPL1(IP1))
                 YPL(NNEW)=
     -                YPL1(IP1)+XL4*(YPL1(1+MOD(IP1,NPL1))-YPL1(IP1))
            ELSE
C                 print *,' *** Neither 1 nor 4, should not happen'
            ENDIF
            IF(NNEW.LT.3)THEN
C                 print *,' *** Not stored, only ',nnew,' vertices'
            ELSEIF(NREFO+1.LE.MXPLAN)THEN
                 NREFO=NREFO+1
                 CALL PLABU2('STORE',IREFO(NREFO),NNEW,XPL,YPL,ZPL,
     -                AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
C                 print *,' Ref = ',IREFO(NREFO)
C                 do i=1,nnew
C                    print '(2x,i10,2f10.3)',i,xpl(i),ypl(i)
C                 enddo
            ELSE
                 PRINT *,' !!!!!! PLATRC WARNING : Output list'//
     -                     ' too long.'
                 RETURN
            ENDIF
*   Second non-rectangular section.
            DO 230 I=IP1+1,JP1
            XPL(I-IP1)=XPL1(1+MOD(I-1+NPL1,NPL1))
            YPL(I-IP1)=YPL1(1+MOD(I-1+NPL1,NPL1))
 230        CONTINUE
            IF(  (XL2+EPSANG)*(1+EPSANG-XL2).GE.0.AND.
     -           (XL3+EPSANG)*(1+EPSANG-XL3).GE.0)THEN
C                 print *,' 2-3 degenerate'
                 NNEW=JP1-IP1
            ELSEIF((XL2+EPSANG)*(1+EPSANG-XL2).GE.0)THEN
C                 print *,' Using 2'
                 NNEW=JP1-IP1+1
                 XPL(NNEW)=
     -                XPL1(JP1)+XL2*(XPL1(1+MOD(JP1,NPL1))-XPL1(JP1))
                 YPL(NNEW)=
     -                YPL1(JP1)+XL2*(YPL1(1+MOD(JP1,NPL1))-YPL1(JP1))
            ELSEIF((XL3+EPSANG)*(1+EPSANG-XL3).GE.0)THEN
C                 print *,' Using 3'
                 NNEW=JP1-IP1+1
                 XPL(NNEW)=
     -                XPL1(IP1)+XL3*(XPL1(1+MOD(IP1,NPL1))-XPL1(IP1))
                 YPL(NNEW)=
     -                YPL1(IP1)+XL3*(YPL1(1+MOD(IP1,NPL1))-YPL1(IP1))
            ELSE
C                 print *,' *** Neither 2 nor 3, should not happen'
            ENDIF
            IF(NNEW.LT.3)THEN
                 print *,' *** Not stored, only ',nnew,' vertices'
            ELSEIF(NREFO+1.LE.MXPLAN)THEN
                 NREFO=NREFO+1
                 CALL PLABU2('STORE',IREFO(NREFO),NNEW,XPL,YPL,ZPL,
     -                AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
C                 print *,' Ref = ',IREFO(NREFO)
C                 do i=1,nnew
C                    print '(2x,i10,2f10.3)',i,xpl(i),ypl(i)
C                 enddo
            ELSE
                 PRINT *,' !!!!!! PLATRC WARNING : Output list'//
     -                     ' too long.'
                 RETURN
            ENDIF
*   Delete the original and restart a cycle
            CALL PLABU2('DELETE',IREFO(JREFO),NPL1,XPL1,YPL1,ZPL1,
     -           AN1,BN1,CN1,DN1,ICOL1,IFAIL2)
            IREFO(JREFO)=-1
            JREFO=JREFO+1
            GOTO 1000
 210        CONTINUE
 200        CONTINUE
       ENDIF
*** Find a right-angled corner we can cut off.
C       print *,' Trying to find a right-angle'
       CORNER=.FALSE.
       DO 70 IP1=1,NPL1
*   Take only right angles
       IF(ABS((XPL1(1+MOD(IP1-2+NPL1,NPL1))-XPL1(IP1))*
     -        (XPL1(1+MOD(IP1,NPL1))-       XPL1(IP1))+
     -        (YPL1(1+MOD(IP1-2+NPL1,NPL1))-YPL1(IP1))*
     -        (YPL1(1+MOD(IP1,NPL1))-       YPL1(IP1))).GT.EPSANG*
     -      SQRT(((XPL1(1+MOD(IP1-2+NPL1,NPL1))-XPL1(IP1))**2+
     -            (YPL1(1+MOD(IP1-2+NPL1,NPL1))-YPL1(IP1))**2)*
     -           ((XPL1(1+MOD(IP1,NPL1))-XPL1(IP1))**2+
     -            (YPL1(1+MOD(IP1,NPL1))-YPL1(IP1))**2)))GOTO 70
*   Ensure the midpoint is internal.
       IF(NPL1.GT.3)THEN
            CALL INTERD(NPL1,XPL1,YPL1,
     -           (XPL1(1+MOD(IP1-2+NPL1,NPL1))+XPL1(1+MOD(IP1,NPL1)))/2,
     -           (YPL1(1+MOD(IP1-2+NPL1,NPL1))+YPL1(1+MOD(IP1,NPL1)))/2,
     -           INSIDE,EDGE)
            IF(.NOT.INSIDE)GOTO 70
       ENDIF
*** Check all vertex crossings.
       DO 80 JP1=1,NPL1
*   Accept immediate contact.
       IF(  JP1            .EQ.1+MOD(IP1-2+NPL1,NPL1).OR.
     -      JP1            .EQ.IP1.OR.
     -      JP1            .EQ.1+MOD(IP1,NPL1).OR.
     -      1+MOD(JP1,NPL1).EQ.1+MOD(IP1-2+NPL1,NPL1).OR.
     -      1+MOD(JP1,NPL1).EQ.IP1.OR.
     -      1+MOD(JP1,NPL1).EQ.1+MOD(IP1,NPL1))GOTO 80
*   Check crossing.
       IF(CROSSD(XPL1(1+MOD(IP1-2+NPL1,NPL1)),
     -           YPL1(1+MOD(IP1-2+NPL1,NPL1)),
     -           XPL1(1+MOD(IP1,NPL1)),
     -           YPL1(1+MOD(IP1,NPL1)),
     -           XPL1(JP1),
     -           YPL1(JP1),
     -           XPL1(1+MOD(JP1,NPL1)),
     -           YPL1(1+MOD(JP1,NPL1))))GOTO 70
80     CONTINUE
*** Found a triangle, introduce shorthand node references.
C       print *,' Cutting at right-angled corner ',IP1
       CORNER=.TRUE.
       XPL(1)=XPL1(1+MOD(IP1-2+NPL1,NPL1))
       YPL(1)=YPL1(1+MOD(IP1-2+NPL1,NPL1))
       XPL(2)=XPL1(IP1)
       YPL(2)=YPL1(IP1)
       XPL(3)=XPL1(1+MOD(IP1,NPL1))
       YPL(3)=YPL1(1+MOD(IP1,NPL1))
       IF(NREFO+1.LE.MXPLAN)THEN
            NREFO=NREFO+1
            CALL PLABU2('STORE',IREFO(NREFO),3,XPL,YPL,ZPL,
     -           AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
       ELSE
            PRINT *,' !!!!!! PLAXXX WARNING : Output list'//
     -                ' too long.'
            RETURN
       ENDIF
*** Eliminate this node from the polygon.
       DO 90 JP1=1,NPL1
       IF(JP1.GT.IP1)THEN
            XPL1(JP1-1)=XPL1(JP1)
            YPL1(JP1-1)=YPL1(JP1)
       ELSEIF(JP1.LT.IP1)THEN
            XPL1(JP1)=XPL1(JP1)
            YPL1(JP1)=YPL1(JP1)
       ENDIF
90     CONTINUE
       NPL1=NPL1-1
C       print *,' Going for another pass, NPL = ',npl1
       GOTO 100
70     CONTINUE
*** Find any corner we can cut off.
C       print *,' Trying to find a corner'
       CORNER=.FALSE.
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
C       print *,' Cutting at corner ',IP1
       CORNER=.TRUE.
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
C            print *,' Right-angled corner cut off'
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
                 PRINT *,' !!!!!! PLATRC WARNING : Output list'//
     -                     ' too long.'
                 RETURN
            ENDIF
       ELSEIF(A1.LE.A2.AND.A1.LE.A3)THEN
C            print *,' A1 < A2, A3 - adding 2 triangles'
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
                 PRINT *,' !!!!!! PLATRC WARNING : Output list'//
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
                 PRINT *,' !!!!!! PLATRC WARNING : Output list'//
     -                     ' too long.'
                 RETURN
            ENDIF
       ELSEIF(A2.LE.A1.AND.A2.LE.A3)THEN
C            print *,' A2 < A1, A3 - adding 2 triangles'
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
                 PRINT *,' !!!!!! PLATRC WARNING : Output list'//
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
                 PRINT *,' !!!!!! PLATRC WARNING : Output list'//
     -                     ' too long.'
                 RETURN
            ENDIF
       ELSE
C            print *,' A3 < A1, A2 - adding 2 triangles'
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
                 PRINT *,' !!!!!! PLATRC WARNING : Output list'//
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
                 PRINT *,' !!!!!! PLATRC WARNING : Output list'//
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
C       print *,' Going for another pass, NPL = ',npl1
       GOTO 100
20     CONTINUE
       IF(.NOT.CORNER)PRINT *,' !!!!!! PLATRC WARNING : Unable to'//
     -      ' identify a corner to cut, probably a degenerate polygon.'
*** Next stack element.
       CALL PLABU2('DELETE',IREFO(JREFO),NPL1,XPL1,YPL1,ZPL1,
     -      AN1,BN1,CN1,DN1,ICOL1,IFAIL2)
       IREFO(JREFO)=-1
       JREFO=JREFO+1
       GOTO 1000
       END
