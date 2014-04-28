CDECK  ID>, PLATRQ.
       SUBROUTINE PLATRQ(IREF,NREFO,IREFO,IFAIL)
*-----------------------------------------------------------------------
*   PLATRQ - Cuts a triangle into better quality pieces
*   (Last changed on 22/ 3/09.)
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
       INTEGER NBEM,IREFB1(MXPLAN),NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,
     -      BEMNEW,BEMINV,BEMSLV
       DOUBLE PRECISION BEMQTH,BEMSTH,BEMSSC,BEMTGT,BEMEPA,BEMEPD
       LOGICAL LBDUMP
       COMMON /BEMDAT/ BEMQTH,BEMSSC,BEMSTH,BEMTGT,BEMEPA,BEMEPD,
     -      IREFB1,NBEM,NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,BEMNEW,
     -      BEMINV,BEMSLV,LBDUMP
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
       DOUBLE PRECISION XPL1(MXEDGE),YPL1(MXEDGE),ZPL1(MXEDGE),ZMEAN,
     -      XPL(MXEDGE),YPL(MXEDGE),ZPL(MXEDGE),
     -      AN1,BN1,CN1,DN1,D12,D32,XL,XAUX,YAUX
       INTEGER I,IREF,IFAIL,IFAIL1,NPL1,ICOL1,IP1,IREFO(MXPLAN),NREFO
*** Assume failure.
       IFAIL=1
*** Zero the output buffer.
       NREFO=0
*** Retrieve the polygon.
       CALL PLABU2('READ',IREF,NPL1,XPL1,YPL1,ZPL1,
     -      AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
*   Check this is a triangle.
       IF(NPL1.NE.3)THEN
C            print *,' Not a triangle, n=',npl1
            NREFO=1
            IREFO(1)=IREF
            RETURN
       ENDIF
*** Work out aspect ratio
       D12=SQRT((XPL1(1)-XPL1(2))**2+(YPL1(1)-YPL1(2))**2)
       D32=SQRT((XPL1(3)-XPL1(2))**2+(YPL1(3)-YPL1(2))**2)
*   Anything to be done ?
       IF(D12.LT.BEMQTH*D32.AND.D32.LT.BEMQTH*D12)THEN
C            print *,' Initial aspect ratio below threshold'
            NREFO=1
            IREFO(1)=IREF
            RETURN
       ELSEIF(D12*D32/2.LT.BEMSTH)THEN
C            print *,' Initial size below threshold'
            NREFO=1
            IREFO(1)=IREF
            RETURN
       ENDIF
*   Rearrange to make corner 1 the sharp one.
       IF(D12.LT.D32)THEN
            XAUX=XPL1(3)
            YAUX=YPL1(3)
            XPL1(3)=XPL1(1)
            YPL1(3)=YPL1(1)
            XPL1(1)=XAUX
            YPL1(1)=YAUX
            XAUX=D12
            D12=D32
            D32=XAUX
       ENDIF
*   We'll need the average z
       ZMEAN=0
       DO 10 IP1=1,NPL1
       ZMEAN=ZMEAN+ZPL1(IP1)
10     CONTINUE
       ZMEAN=ZMEAN/NPL1
*   Delete the original.
       CALL PLABU2('DELETE',IREF,NPL1,XPL1,YPL1,ZPL1,
     -      AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
*   Preset the ZPL array to the mean z value.
       DO 20 I=1,MXEDGE
       ZPL(I)=ZMEAN
20     CONTINUE
*** Resume for another pass.
100    CONTINUE
C       print *,' *** New pass'
*   Chop off corner 2
       XL=MIN(0.5,BEMQTH*D32/D12)
       XPL(1)=XPL1(2)+(XPL1(1)-XPL1(2))*XL
       YPL(1)=YPL1(2)+(YPL1(1)-YPL1(2))*XL
       XPL(2)=XPL1(2)
       YPL(2)=YPL1(2)
       XPL(3)=XPL1(3)
       YPL(3)=YPL1(3)
       IF(NREFO+1.LE.MXPLAN)THEN
            NREFO=NREFO+1
            CALL PLABU2('STORE',IREFO(NREFO),3,XPL,YPL,ZPL,
     -           AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
       ELSE
            PRINT *,' !!!!!! PLATRQ WARNING : Output overflow.'
            RETURN
       ENDIF
*   Drop the perpendicular on the hypothenusa.
       XL=((XPL1(3)-XPL(1))*(XPL1(3)-XPL1(1))+
     -     (YPL1(3)-YPL(1))*(YPL1(3)-YPL1(1)))/
     -     ((XPL1(3)-XPL1(1))**2+(YPL1(3)-YPL1(1))**2)
       XPL(2)=XPL1(3)+XL*(XPL1(1)-XPL1(3))
       YPL(2)=YPL1(3)+XL*(YPL1(1)-YPL1(3))
       IF(NREFO+1.LE.MXPLAN)THEN
            NREFO=NREFO+1
            CALL PLABU2('STORE',IREFO(NREFO),3,XPL,YPL,ZPL,
     -           AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
       ELSE
            PRINT *,' !!!!!! PLATRQ WARNING : Output overflow.'
            RETURN
       ENDIF
*   Store the new corners.
       XPL1(2)=XPL(2)
       YPL1(2)=YPL(2)
       XPL1(3)=XPL(1)
       YPL1(3)=YPL(1)
*   Work out the new aspect ratios
       D12=SQRT((XPL1(1)-XPL1(2))**2+(YPL1(1)-YPL1(2))**2)
       D32=SQRT((XPL1(3)-XPL1(2))**2+(YPL1(3)-YPL1(2))**2)
*   Anything to be done ?
       IF(D12.LT.BEMQTH*D32.AND.D32.LT.BEMQTH*D12)THEN
C            print *,' Current aspect ratio below threshold'
            IF(NREFO+1.LE.MXPLAN)THEN
                 NREFO=NREFO+1
                 CALL PLABU2('STORE',IREFO(NREFO),3,XPL1,YPL1,ZPL1,
     -                AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
            ELSE
                 PRINT *,' !!!!!! PLATRQ WARNING : Output overflow.'
                 RETURN
            ENDIF
      print *,' Triangle cut in ',nrefo,' pieces (A/R).'
            RETURN
       ELSEIF(D12*D32/2.LT.BEMSTH)THEN
C            print *,' Current size below threshold'
            IF(NREFO+1.LE.MXPLAN)THEN
                 NREFO=NREFO+1
                 CALL PLABU2('STORE',IREFO(NREFO),3,XPL1,YPL1,ZPL1,
     -                AN1,BN1,CN1,DN1,ICOL1,IFAIL1)
            ELSE
                 PRINT *,' !!!!!! PLATRQ WARNING : Output overflow.'
                 RETURN
            ENDIF
      print *,' Triangle cut in ',nrefo,' pieces (QTH).'
            RETURN
       ENDIF
       GOTO 100
*** Seems to have worked.
       IFAIL=0
       END
