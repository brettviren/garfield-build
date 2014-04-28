CDECK  ID>, MATOBL.
       SUBROUTINE MATOBL(IRX,IRY,IREX1,IREY1,IREX2,IREY2,TYPE,SIZE)
*-----------------------------------------------------------------------
*   MATOBL - Plots oblique error bars.
*   (Last changed on  4/ 9/10.)
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
       PARAMETER (MXWIRE=   300,MXSW  =   50)
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
       PARAMETER (MXMAP =  5000,MXEPS =   10)
       PARAMETER (MXWMAP=     5)
       PARAMETER (MXSOLI=  1000)
       PARAMETER (MXPLAN= 50000, MXPOIN=100000,MXEDGE=100)
       PARAMETER (MXSBUF= 20000)
       PARAMETER (MXMCA = 50000)
*   The parameter MXNBMC must equal MXGNAM (sequence MAGBPARM) !
       INTEGER MXNBMC
       PARAMETER(MXNBMC=60)
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       REAL USERX0,USERX1,USERY0,USERY1,FRXMIN,FRXMAX,FRYMIN,FRYMAX,
     -      ARRANG,ARRLEN,BARFRC,DISPX0,DISPX1,DISPY0,DISPY1,
     -      GPXN,GPXN10,GPYN,GPYN10,GPXL,GPYL,GPXT
       LOGICAL LGRID,LGRALL,LOGX,LOGY,LSTAMP,LGCLRB,LGCLRA,
     -      LWAITA,LWAITB,LXCCH,LGLCLP,LGMCLP,LGACLP,LGTCLP,
     -      WKMULT(MXWKLS)
       INTEGER NWK,WKID(MXWKLS),WKCON(MXWKLS),WKFREF(MXWKLS),
     -         WKLUN(MXWKLS),WKSTAT(MXWKLS),WKSREQ(MXWKLS),
     -         NCWKNM(MXWKLS),NCSTMP,IGHIST,IGBAR,NCGKS
       CHARACTER*20 WKNAME(MXWKLS),WKATTR(MXWKLS)
       CHARACTER*80 STAMP
       CHARACTER*(MXNAME) GKSLOG
       COMMON /GRADAT/ USERX0,USERX1,USERY0,USERY1,ARRANG,ARRLEN,
     -      BARFRC,
     -      FRXMIN,FRXMAX,FRYMIN,FRYMAX,DISPX0,DISPX1,DISPY0,DISPY1,
     -      GPXN,GPXN10,GPYN,GPYN10,GPXL,GPYL,GPXT,
     -      LGRID,LGRALL,LOGX,LOGY,LSTAMP,LGCLRB,LGCLRA,LWAITA,LWAITB,
     -      LXCCH,LGLCLP,LGMCLP,LGACLP,LGTCLP,
     -      NWK,WKID,WKCON,WKFREF,WKLUN,WKSTAT,WKSREQ,NCWKNM,NCSTMP,
     -      IGHIST,IGBAR,NCGKS,WKMULT
       COMMON /GRACHR/ WKNAME,WKATTR,STAMP,GKSLOG
       REAL X0,X1,X2,Y0,Y1,Y2,SIZE,XPL(80),YPL(80),STFACT,XC,YC,
     -      EX1,EY1,EX2,EY2
       INTEGER IRX,IRY,IREX1,IREY1,IREX2,IREY2,I,J,NPOINT,
     -      ISX,ISY,ISEX1,ISEY1,ISEX2,ISEY2,MATSLT,IERR,NT
       CHARACTER*(*) TYPE
       EXTERNAL MATSLT
*** Locate the matrices.
       ISX=MATSLT(IRX)
       ISY=MATSLT(IRY)
       ISEX1=MATSLT(IREX1)
       ISEY1=MATSLT(IREY1)
       ISEX2=MATSLT(IREX2)
       ISEY2=MATSLT(IREY2)
*** Determine current NT.
       CALL GQCNTN(IERR,NT)
       IF(IERR.NE.0)THEN
            PRINT *,' !!!!!! MATOBL WARNING : Error from'//
     -           ' GQCNTN, code=',IERR,'; no error bars plotted.'
            RETURN
       ENDIF
*** Make sure that the marker type makes sense.
       IF(INDEX(TYPE,'CIRCLE')+INDEX(TYPE,'SQUARE')+
     -      INDEX(TYPE,'CROSS')+INDEX(TYPE,'PLUS')+
     -      INDEX(TYPE,'ELLIPSE')+INDEX(TYPE,'TRIANGLE')+
     -      INDEX(TYPE,'STAR')+INDEX(TYPE,'DAVID')+
     -      INDEX(TYPE,'HEXAGON').EQ.0)THEN
            PRINT *,' !!!!!! MATOBL WARNING : Error bar model ',
     -           TYPE,' not known ; no error bars plotted.'
            RETURN
       ENDIF
*** Check the size of the markers.
       IF(SIZE.LE.0.OR.SIZE.GT.1.0)THEN
            PRINT *,' !!!!!! MATOBL WARNING : Error bar size is'//
     -           ' out of range [0,1] ; no error bars plotted.'
            RETURN
       ENDIF
*** Set the appropriate representations.
       CALL GRATTS('ERROR-BAR','POLYLINE')
       CALL GRATTS('ERROR-BAR','AREA')
*** Loop over the points.
       DO 10 I=1,MLEN(ISX)
**  Translate the various reference points into NDC.
       XC=0
       YC=0
       EX1=0
       EY1=0
       EX2=0
       EY2=0
       IF(ISX.GT.0)XC=MVEC(MORG(ISX)+I)
       IF(ISY.GT.0)YC=MVEC(MORG(ISY)+I)
       IF(ISEX1.GT.0)EX1=MVEC(MORG(ISEX1)+I)
       IF(ISEY1.GT.0)EY1=MVEC(MORG(ISEY1)+I)
       IF(ISEX2.GT.0)EX2=MVEC(MORG(ISEX2)+I)
       IF(ISEY2.GT.0)EY2=MVEC(MORG(ISEY2)+I)
       CALL GRWCNC(XC,YC,X0,Y0)
       CALL GRWCNC(XC-EX1,YC-EY1,X1,Y1)
       CALL GRWCNC(XC+EX2,YC+EY2,X2,Y2)
*   Skip points which are outside the frame.
       IF(X0.LT.0.1.OR.X0.GT.0.9.OR.Y0.LT.0.1.OR.Y0.GT.0.9)GOTO 10
**  Move to NDC coordinates.
       CALL GSELNT(0)
**  Error bar type CIRCLE and SQUARE.
       IF(INDEX(TYPE,'CIRCLE')+INDEX(TYPE,'SQUARE').NE.0)THEN
*   Plot the error bars.
            XPL(1)=X1
            YPL(1)=Y1
            XPL(2)=X2
            YPL(2)=Y2
            CALL GPL(2,XPL,YPL)
            XPL(1)=X2-SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(1)=Y2-SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            XPL(2)=X2+SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(2)=Y2+SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            CALL GPL(2,XPL,YPL)
            XPL(1)=X1-SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(1)=Y1-SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            XPL(2)=X1+SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(2)=Y1+SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            CALL GPL(2,XPL,YPL)
*   Plot the marker.
            IF(INDEX(TYPE,'CIRCLE').NE.0)THEN
                 DO 20 J=1,20
                 XPL(J)=X0+COS(2*PI*REAL(J-1)/19.0)*SIZE
                 YPL(J)=Y0+SIN(2*PI*REAL(J-1)/19.0)*SIZE
20               CONTINUE
                 CALL GFA(20,XPL,YPL)
                 CALL GPL(20,XPL,YPL)
            ELSE
                 XPL(1)=X0-SIZE
                 YPL(1)=Y0-SIZE
                 XPL(2)=X0+SIZE
                 YPL(2)=Y0-SIZE
                 XPL(3)=X0+SIZE
                 YPL(3)=Y0+SIZE
                 XPL(4)=X0-SIZE
                 YPL(4)=Y0+SIZE
                 XPL(5)=X0-SIZE
                 YPL(5)=Y0-SIZE
                 CALL GFA(5,XPL,YPL)
                 CALL GPL(5,XPL,YPL)
            ENDIF
**  ELLIPSE shaped error bars.
       ELSEIF(INDEX(TYPE,'ELLIPSE').NE.0)THEN
            DO 30 J=1,20
            XPL(J)=X0+(X2-X0)*COS(PI*REAL(J-1)/38.0)
            YPL(J)=Y0+(Y2-Y0)*SIN(PI*REAL(J-1)/38.0)
30          CONTINUE
            DO 40 J=1,20
            XPL(20+J)=X0-(X1-X0)*COS(PI/2+PI*REAL(J-1)/38.0)
            YPL(20+J)=Y0+(Y2-Y0)*SIN(PI/2+PI*REAL(J-1)/38.0)
40          CONTINUE
            DO 50 J=1,20
            XPL(40+J)=X0-(X1-X0)*COS(PI+PI*REAL(J-1)/38.0)
            YPL(40+J)=Y0-(Y1-Y0)*SIN(PI+PI*REAL(J-1)/38.0)
50          CONTINUE
            DO 60 J=1,20
            XPL(60+J)=X0+(X2-X0)*COS(3*PI/2+PI*REAL(J-1)/38.0)
            YPL(60+J)=Y0-(Y1-Y0)*SIN(3*PI/2+PI*REAL(J-1)/38.0)
60          CONTINUE
            CALL GFA(80,XPL,YPL)
            CALL GPL(80,XPL,YPL)
**  CROSS and PLUS shaped error bars.
       ELSEIF(INDEX(TYPE,'CROSS')+INDEX(TYPE,'PLUS').NE.0)THEN
*   Plot the error bars.
            XPL(1)=X1
            YPL(1)=Y1
            XPL(2)=X2
            YPL(2)=Y2
            CALL GPL(2,XPL,YPL)
            XPL(1)=X2-SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(1)=Y2-SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            XPL(2)=X2+SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(2)=Y2+SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            CALL GPL(2,XPL,YPL)
            XPL(1)=X1-SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(1)=Y1-SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            XPL(2)=X1+SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(2)=Y1+SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            CALL GPL(2,XPL,YPL)
*   Plot the marker.
            IF(INDEX(TYPE,'CROSS').NE.0)THEN
                 XPL(1)=X0-SIZE
                 YPL(1)=Y0-SIZE
                 XPL(2)=X0+SIZE
                 YPL(2)=Y0+SIZE
                 CALL GPL(2,XPL,YPL)
                 XPL(1)=X0-SIZE
                 YPL(1)=Y0+SIZE
                 XPL(2)=X0+SIZE
                 YPL(2)=Y0-SIZE
                 CALL GPL(2,XPL,YPL)
            ELSEIF(INDEX(TYPE,'PLUS').NE.0)THEN
                 XPL(1)=X0-SIZE
                 YPL(1)=Y0
                 XPL(2)=X0+SIZE
                 YPL(2)=Y0
                 CALL GPL(2,XPL,YPL)
                 XPL(1)=X0
                 YPL(1)=Y0+SIZE
                 XPL(2)=X0
                 YPL(2)=Y0-SIZE
                 CALL GPL(2,XPL,YPL)
            ENDIF
**  Error bar of type HEXAGON.
       ELSEIF(INDEX(TYPE,'HEXAGON').NE.0)THEN
*   Plot the error bars.
            XPL(1)=X1
            YPL(1)=Y1
            XPL(2)=X2
            YPL(2)=Y2
            CALL GPL(2,XPL,YPL)
            XPL(1)=X2-SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(1)=Y2-SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            XPL(2)=X2+SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(2)=Y2+SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            CALL GPL(2,XPL,YPL)
            XPL(1)=X1-SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(1)=Y1-SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            XPL(2)=X1+SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(2)=Y1+SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            CALL GPL(2,XPL,YPL)
*   Plot the marker.
            XPL(1)=X0+SIZE*0.5*SQRT(3.0)
            YPL(1)=Y0+SIZE*0.5
            XPL(2)=X0
            YPL(2)=Y0+SIZE
            XPL(3)=X0-SIZE*0.5*SQRT(3.0)
            YPL(3)=Y0+SIZE*0.5
            XPL(4)=X0-SIZE*0.5*SQRT(3.0)
            YPL(4)=Y0-SIZE*0.5
            XPL(5)=X0
            YPL(5)=Y0-SIZE
            XPL(6)=X0+SIZE*0.5*SQRT(3.0)
            YPL(6)=Y0-SIZE*0.5
            XPL(7)=XPL(1)
            YPL(7)=YPL(1)
            CALL GFA(7,XPL,YPL)
            CALL GPL(7,XPL,YPL)
**  Error bar type RIGHT-TRIANGLE.
       ELSEIF(INDEX(TYPE,'RIGHT-TRIANGLE')+
     -      INDEX(TYPE,'TRIANGLE-RIGHT')+
     -      INDEX(TYPE,'EAST-TRIANGLE')+
     -      INDEX(TYPE,'TRIANGLE-EAST')+
     -      INDEX(TYPE,'E-TRIANGLE')+
     -      INDEX(TYPE,'TRIANGLE-E').NE.0)THEN
*   Plot the error bars.
            XPL(1)=X1
            YPL(1)=Y1
            XPL(2)=X2
            YPL(2)=Y2
            CALL GPL(2,XPL,YPL)
            XPL(1)=X2-SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(1)=Y2-SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            XPL(2)=X2+SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(2)=Y2+SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            CALL GPL(2,XPL,YPL)
            XPL(1)=X1-SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(1)=Y1-SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            XPL(2)=X1+SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(2)=Y1+SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            CALL GPL(2,XPL,YPL)
*   Plot the marker.
            XPL(1)=X0+SIZE
            YPL(1)=Y0
            XPL(2)=X0-0.5*SIZE
            YPL(2)=Y0+0.5*SQRT(3.0)*SIZE
            XPL(3)=X0-0.5*SIZE
            YPL(3)=Y0-0.5*SQRT(3.0)*SIZE
            XPL(4)=XPL(1)
            YPL(4)=YPL(1)
            CALL GFA(4,XPL,YPL)
            CALL GPL(4,XPL,YPL)
**  Error bar type LEFT-TRIANGLE.
       ELSEIF(INDEX(TYPE,'LEFT-TRIANGLE')+
     -      INDEX(TYPE,'TRIANGLE-LEFT')+
     -      INDEX(TYPE,'WEST-TRIANGLE')+
     -      INDEX(TYPE,'TRIANGLE-WEST')+
     -      INDEX(TYPE,'W-TRIANGLE')+
     -      INDEX(TYPE,'TRIANGLE-W').NE.0)THEN
*   Plot the error bars.
            XPL(1)=X1
            YPL(1)=Y1
            XPL(2)=X2
            YPL(2)=Y2
            CALL GPL(2,XPL,YPL)
            XPL(1)=X2-SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(1)=Y2-SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            XPL(2)=X2+SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(2)=Y2+SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            CALL GPL(2,XPL,YPL)
            XPL(1)=X1-SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(1)=Y1-SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            XPL(2)=X1+SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(2)=Y1+SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            CALL GPL(2,XPL,YPL)
*   Plot the marker.
            XPL(1)=X0-SIZE
            YPL(1)=Y0
            XPL(2)=X0+0.5*SIZE
            YPL(2)=Y0+0.5*SQRT(3.0)*SIZE
            XPL(3)=X0+0.5*SIZE
            YPL(3)=Y0-0.5*SQRT(3.0)*SIZE
            XPL(4)=XPL(1)
            YPL(4)=YPL(1)
            CALL GFA(4,XPL,YPL)
            CALL GPL(4,XPL,YPL)
**  Error bar type DOWN-TRIANGLE.
       ELSEIF(INDEX(TYPE,'DOWN-TRIANGLE')+
     -      INDEX(TYPE,'TRIANGLE-DOWN')+
     -      INDEX(TYPE,'SOUTH-TRIANGLE')+
     -      INDEX(TYPE,'TRIANGLE-SOUTH')+
     -      INDEX(TYPE,'S-TRIANGLE')+
     -      INDEX(TYPE,'TRIANGLE-S').NE.0)THEN
*   Plot the error bars.
            XPL(1)=X1
            YPL(1)=Y1
            XPL(2)=X2
            YPL(2)=Y2
            CALL GPL(2,XPL,YPL)
            XPL(1)=X2-SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(1)=Y2-SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            XPL(2)=X2+SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(2)=Y2+SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            CALL GPL(2,XPL,YPL)
            XPL(1)=X1-SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(1)=Y1-SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            XPL(2)=X1+SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(2)=Y1+SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            CALL GPL(2,XPL,YPL)
*   Plot the marker.
            XPL(1)=X0
            YPL(1)=Y0-SIZE
            XPL(2)=X0+0.5*SQRT(3.0)*SIZE
            YPL(2)=Y0+0.5*SIZE
            XPL(3)=X0-0.5*SQRT(3.0)*SIZE
            YPL(3)=Y0+0.5*SIZE
            XPL(4)=XPL(1)
            YPL(4)=YPL(1)
            CALL GFA(4,XPL,YPL)
            CALL GPL(4,XPL,YPL)
**  Error bar type UP-TRIANGLE.
       ELSEIF(INDEX(TYPE,'TRIANGLE').NE.0)THEN
*   Plot the error bars.
            XPL(1)=X1
            YPL(1)=Y1
            XPL(2)=X2
            YPL(2)=Y2
            CALL GPL(2,XPL,YPL)
            XPL(1)=X2-SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(1)=Y2-SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            XPL(2)=X2+SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(2)=Y2+SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            CALL GPL(2,XPL,YPL)
            XPL(1)=X1-SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(1)=Y1-SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            XPL(2)=X1+SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(2)=Y1+SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            CALL GPL(2,XPL,YPL)
*   Plot the marker.
            XPL(1)=X0
            YPL(1)=Y0+SIZE
            XPL(2)=X0+0.5*SQRT(3.0)*SIZE
            YPL(2)=Y0-0.5*SIZE
            XPL(3)=X0-0.5*SQRT(3.0)*SIZE
            YPL(3)=Y0-0.5*SIZE
            XPL(4)=XPL(1)
            YPL(4)=YPL(1)
            CALL GFA(4,XPL,YPL)
            CALL GPL(4,XPL,YPL)
**  Error bar of type STAR.
       ELSEIF(INDEX(TYPE,'STAR')+INDEX(TYPE,'DAVID').NE.0)THEN
*   Plot the error bars.
            XPL(1)=X1
            YPL(1)=Y1
            XPL(2)=X2
            YPL(2)=Y2
            CALL GPL(2,XPL,YPL)
            XPL(1)=X2-SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(1)=Y2-SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            XPL(2)=X2+SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(2)=Y2+SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            CALL GPL(2,XPL,YPL)
            XPL(1)=X1-SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(1)=Y1-SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            XPL(2)=X1+SIZE*(Y2-Y1)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            YPL(2)=Y1+SIZE*(X1-X2)/SQRT((X2-X1)**2+(Y2-Y1)**2)
            CALL GPL(2,XPL,YPL)
*   Plot the marker.
            IF(INDEX(TYPE,'4-STAR').NE.0)THEN
                 NPOINT=8
                 STFACT=0.3
            ELSEIF(INDEX(TYPE,'6-STAR').NE.0)THEN
                 NPOINT=12
                 STFACT=0.3
            ELSEIF(INDEX(TYPE,'DAVID').NE.0)THEN
                 NPOINT=12
                 STFACT=0.5/COS(PI/6)
            ELSEIF(INDEX(TYPE,'8-STAR').NE.0)THEN
                 NPOINT=16
                 STFACT=0.3
            ELSEIF(INDEX(TYPE,'10-STAR').NE.0)THEN
                 NPOINT=20
                 STFACT=0.3
            ELSE
                 NPOINT=12
                 STFACT=0.3
            ENDIF
            DO 70 J=1,NPOINT
            IF(J.EQ.2*(J/2))THEN
                 XPL(J)=X0+SIZE*COS(2*PI*J/REAL(NPOINT))
                 YPL(J)=Y0+SIZE*SIN(2*PI*J/REAL(NPOINT))
            ELSE
                 XPL(J)=X0+STFACT*SIZE*COS(2*PI*J/REAL(NPOINT))
                 YPL(J)=Y0+STFACT*SIZE*SIN(2*PI*J/REAL(NPOINT))
            ENDIF
70          CONTINUE
            XPL(NPOINT+1)=XPL(1)
            YPL(NPOINT+1)=YPL(1)
            CALL GFA(NPOINT+1,XPL,YPL)
            CALL GPL(NPOINT+1,XPL,YPL)
**  Unknown marker type.
       ELSE
            PRINT *,' !!!!!! MATOBL WARNING : Marker type not'//
     -           ' recognised; no markers plotted.'
       ENDIF
**  Move to the original normalisation transformation.
       CALL GSELNT(NT)
**  Next point.
10     CONTINUE
       END
