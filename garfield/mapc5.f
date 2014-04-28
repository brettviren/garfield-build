CDECK  ID>, MAPC5.
       SUBROUTINE MAPC5(X,Y,Z,T1,T2,T3,T4,JAC,DET,IMAP,IFAIL)
*-----------------------------------------------------------------------
*   MAPC5  - Finds the isoparametric cooordinates of point (X,Y) in an
*            8-point ("serendipity") quadratic quadrilateral
*   (Last changed on 16/ 7/08.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
C      logical ldebug,lident
C      parameter (ldebug=.true., lident=.false.)
C      integer lunout
C      parameter(lunout=6)
       REAL X,Y,Z,T1,T2,T3,T4
       DOUBLE PRECISION DETT,
     -      JACT11,JACT12,JACT21,JACT22,
     -      XR,YR,CORR(2),DIFF(2),
     -      JAC(4,4),DET,U,V,TD1,TD2,F
       INTEGER I,K,L,IMAP,IL,ITER,IFAIL
*   Determinant of the quadratic tetrahedron Jacobian
       DETT(U,V,IL)=
     -      (-2*U**3*((XMAP(IL,3)+XMAP(IL,4)-2*XMAP(IL,7))*
     -      (YMAP(IL,1)+YMAP(IL,2)-2*YMAP(IL,5))-(XMAP(IL,1)+XMAP(IL,2)-
     -      2*XMAP(IL,5))*(YMAP(IL,3)+YMAP(IL,4)-2*YMAP(IL,7)))+
     -      2*V**3*(-((XMAP(IL,1)+XMAP(IL,4)-2*XMAP(IL,8))*
     -      (YMAP(IL,2)+YMAP(IL,3)-2*YMAP(IL,6)))+
     -      (XMAP(IL,2)+XMAP(IL,3)-2*XMAP(IL,6))*(YMAP(IL,1)+YMAP(IL,4)-
     -      2*YMAP(IL,8)))+2*(-((XMAP(IL,6)-XMAP(IL,8))*
     -      (YMAP(IL,5)-YMAP(IL,7)))+(XMAP(IL,5)-XMAP(IL,7))*
     -      (YMAP(IL,6)-YMAP(IL,8)))+V*(-(XMAP(IL,7)*YMAP(IL,1))-
     -      2*XMAP(IL,8)*YMAP(IL,1)+XMAP(IL,7)*YMAP(IL,2)-
     -      2*XMAP(IL,8)*YMAP(IL,2)-XMAP(IL,7)*YMAP(IL,3)-
     -      2*XMAP(IL,8)*YMAP(IL,3)+XMAP(IL,5)*(YMAP(IL,1)-YMAP(IL,2)+
     -      YMAP(IL,3)-YMAP(IL,4))+XMAP(IL,7)*YMAP(IL,4)-
     -      2*XMAP(IL,8)*YMAP(IL,4)-XMAP(IL,1)*YMAP(IL,5)+
     -      XMAP(IL,2)*YMAP(IL,5)-XMAP(IL,3)*YMAP(IL,5)+
     -      XMAP(IL,4)*YMAP(IL,5)-2*XMAP(IL,1)*YMAP(IL,6)-
     -      2*XMAP(IL,2)*YMAP(IL,6)-2*XMAP(IL,3)*YMAP(IL,6)-
     -      2*XMAP(IL,4)*YMAP(IL,6)+8*XMAP(IL,8)*YMAP(IL,6)+
     -      XMAP(IL,1)*YMAP(IL,7)-XMAP(IL,2)*YMAP(IL,7)+
     -      XMAP(IL,3)*YMAP(IL,7)-XMAP(IL,4)*YMAP(IL,7)+
     -      2*XMAP(IL,6)*(YMAP(IL,1)+YMAP(IL,2)+YMAP(IL,3)+YMAP(IL,4)-
     -      4*YMAP(IL,8))+2*(XMAP(IL,1)+XMAP(IL,2)+XMAP(IL,3)+
     -      XMAP(IL,4))*YMAP(IL,8))+V**2*(-(XMAP(IL,5)*YMAP(IL,1))+
     -      2*XMAP(IL,6)*YMAP(IL,1)+XMAP(IL,7)*YMAP(IL,1)+
     -      2*XMAP(IL,8)*YMAP(IL,1)+XMAP(IL,5)*YMAP(IL,2)-
     -      2*XMAP(IL,6)*YMAP(IL,2)-XMAP(IL,7)*YMAP(IL,2)-
     -      2*XMAP(IL,8)*YMAP(IL,2)+XMAP(IL,5)*YMAP(IL,3)+
     -      2*XMAP(IL,6)*YMAP(IL,3)-XMAP(IL,7)*YMAP(IL,3)+
     -      2*XMAP(IL,8)*YMAP(IL,3)-XMAP(IL,5)*YMAP(IL,4)-
     -      2*XMAP(IL,6)*YMAP(IL,4)+XMAP(IL,7)*YMAP(IL,4)-
     -      2*XMAP(IL,8)*YMAP(IL,4)+
     -      2*XMAP(IL,3)*(YMAP(IL,2)+YMAP(IL,4))-XMAP(IL,3)*YMAP(IL,5)+
     -      2*XMAP(IL,6)*YMAP(IL,5)-2*XMAP(IL,8)*YMAP(IL,5)-
     -      2*XMAP(IL,3)*YMAP(IL,6)-2*XMAP(IL,5)*YMAP(IL,6)+
     -      2*XMAP(IL,7)*YMAP(IL,6)+XMAP(IL,3)*YMAP(IL,7)-
     -      2*XMAP(IL,6)*YMAP(IL,7)+2*XMAP(IL,8)*YMAP(IL,7)+
     -      XMAP(IL,1)*(2*YMAP(IL,2)+2*YMAP(IL,4)+YMAP(IL,5)-
     -      2*YMAP(IL,6)-YMAP(IL,7)-2*YMAP(IL,8))-
     -      2*(XMAP(IL,3)-XMAP(IL,5)+XMAP(IL,7))*YMAP(IL,8)+XMAP(IL,4)*
     -      (-2*YMAP(IL,1)-2*YMAP(IL,3)+YMAP(IL,5)+
     -      2*YMAP(IL,6)-YMAP(IL,7)+2*YMAP(IL,8))+
     -      XMAP(IL,2)*(-2*YMAP(IL,1)-2*YMAP(IL,3)-YMAP(IL,5)+
     -      2*YMAP(IL,6)+YMAP(IL,7)+2*YMAP(IL,8)))+
     -      U*(XMAP(IL,6)*YMAP(IL,1)-2*XMAP(IL,7)*YMAP(IL,1)-
     -      XMAP(IL,8)*YMAP(IL,1)-XMAP(IL,6)*YMAP(IL,2)-
     -      2*XMAP(IL,7)*YMAP(IL,2)+XMAP(IL,8)*YMAP(IL,2)+
     -      XMAP(IL,6)*YMAP(IL,3)-2*XMAP(IL,7)*YMAP(IL,3)-
     -      XMAP(IL,8)*YMAP(IL,3)-XMAP(IL,6)*YMAP(IL,4)-
     -      2*XMAP(IL,7)*YMAP(IL,4)+XMAP(IL,8)*YMAP(IL,4)-
     -      2*XMAP(IL,2)*YMAP(IL,5)-2*XMAP(IL,3)*YMAP(IL,5)-
     -      2*XMAP(IL,4)*YMAP(IL,5)+8*XMAP(IL,7)*YMAP(IL,5)+
     -      XMAP(IL,2)*YMAP(IL,6)-XMAP(IL,3)*YMAP(IL,6)+
     -      XMAP(IL,4)*YMAP(IL,6)+2*XMAP(IL,5)*(YMAP(IL,1)+
     -      YMAP(IL,2)+YMAP(IL,3)+YMAP(IL,4)-4*YMAP(IL,7))+
     -      2*XMAP(IL,2)*YMAP(IL,7)+2*XMAP(IL,3)*YMAP(IL,7)+
     -      2*XMAP(IL,4)*YMAP(IL,7)-(XMAP(IL,2)-XMAP(IL,3)+
     -      XMAP(IL,4))*YMAP(IL,8)+XMAP(IL,1)*
     -      (-2*YMAP(IL,5)-YMAP(IL,6)+2*YMAP(IL,7)+YMAP(IL,8))+
     -      V**2*(4*XMAP(IL,5)*YMAP(IL,1)-3*XMAP(IL,6)*YMAP(IL,1)-
     -      4*XMAP(IL,7)*YMAP(IL,1)-5*XMAP(IL,8)*YMAP(IL,1)+
     -      4*XMAP(IL,5)*YMAP(IL,2)-5*XMAP(IL,6)*YMAP(IL,2)-
     -      4*XMAP(IL,7)*YMAP(IL,2)-3*XMAP(IL,8)*YMAP(IL,2)+
     -      4*XMAP(IL,5)*YMAP(IL,3)+5*XMAP(IL,6)*YMAP(IL,3)-
     -      4*XMAP(IL,7)*YMAP(IL,3)+3*XMAP(IL,8)*YMAP(IL,3)+
     -      4*XMAP(IL,5)*YMAP(IL,4)+3*XMAP(IL,6)*YMAP(IL,4)-
     -      4*XMAP(IL,7)*YMAP(IL,4)+5*XMAP(IL,8)*YMAP(IL,4)+
     -      8*XMAP(IL,6)*YMAP(IL,5)+8*XMAP(IL,8)*YMAP(IL,5)-
     -      8*XMAP(IL,5)*YMAP(IL,6)+8*XMAP(IL,7)*YMAP(IL,6)-
     -      8*XMAP(IL,6)*YMAP(IL,7)-8*XMAP(IL,8)*YMAP(IL,7)+
     -      XMAP(IL,4)*(5*YMAP(IL,1)+3*YMAP(IL,2)-4*YMAP(IL,5)-
     -      3*YMAP(IL,6)+4*YMAP(IL,7)-5*YMAP(IL,8))+
     -      XMAP(IL,3)*(3*YMAP(IL,1)+5*YMAP(IL,2)-4*YMAP(IL,5)-
     -      5*YMAP(IL,6)+4*YMAP(IL,7)-3*YMAP(IL,8))-
     -      8*XMAP(IL,5)*YMAP(IL,8)+8*XMAP(IL,7)*YMAP(IL,8)+
     -      XMAP(IL,2)*(-5*YMAP(IL,3)-3*YMAP(IL,4)-4*YMAP(IL,5)+
     -      5*YMAP(IL,6)+4*YMAP(IL,7)+3*YMAP(IL,8))+
     -      XMAP(IL,1)*(-3*YMAP(IL,3)-5*YMAP(IL,4)-4*YMAP(IL,5)+
     -      3*YMAP(IL,6)+4*YMAP(IL,7)+5*YMAP(IL,8)))-
     -      2*V*(XMAP(IL,7)*YMAP(IL,1)-3*XMAP(IL,8)*YMAP(IL,1)+
     -      XMAP(IL,7)*YMAP(IL,2)-XMAP(IL,8)*YMAP(IL,2)+
     -      3*XMAP(IL,7)*YMAP(IL,3)-XMAP(IL,8)*YMAP(IL,3)+
     -      3*XMAP(IL,7)*YMAP(IL,4)-3*XMAP(IL,8)*YMAP(IL,4)-
     -      3*XMAP(IL,1)*YMAP(IL,5)-3*XMAP(IL,2)*YMAP(IL,5)-
     -      XMAP(IL,3)*YMAP(IL,5)-XMAP(IL,4)*YMAP(IL,5)+
     -      4*XMAP(IL,8)*YMAP(IL,5)+XMAP(IL,1)*YMAP(IL,6)+
     -      3*XMAP(IL,2)*YMAP(IL,6)+3*XMAP(IL,3)*YMAP(IL,6)+
     -      XMAP(IL,4)*YMAP(IL,6)-4*XMAP(IL,7)*YMAP(IL,6)-
     -      XMAP(IL,1)*YMAP(IL,7)-XMAP(IL,2)*YMAP(IL,7)-
     -      3*XMAP(IL,3)*YMAP(IL,7)-3*XMAP(IL,4)*YMAP(IL,7)+
     -      4*XMAP(IL,8)*YMAP(IL,7)-XMAP(IL,6)*(YMAP(IL,1)+
     -      3*YMAP(IL,2)+3*YMAP(IL,3)+YMAP(IL,4)-
     -      4*(YMAP(IL,5)+YMAP(IL,7)))+(3*XMAP(IL,1)+XMAP(IL,2)+
     -      XMAP(IL,3)+3*XMAP(IL,4)-4*XMAP(IL,7))*YMAP(IL,8)+
     -      XMAP(IL,5)*(3*YMAP(IL,1)+3*YMAP(IL,2)+YMAP(IL,3)+
     -      YMAP(IL,4)-4*(YMAP(IL,6)+YMAP(IL,8)))))+
     -      U**2*(2*XMAP(IL,4)*YMAP(IL,1)-2*XMAP(IL,5)*YMAP(IL,1)-
     -      XMAP(IL,6)*YMAP(IL,1)-2*XMAP(IL,7)*YMAP(IL,1)+
     -      XMAP(IL,8)*YMAP(IL,1)-2*XMAP(IL,1)*YMAP(IL,2)+
     -      2*XMAP(IL,5)*YMAP(IL,2)-XMAP(IL,6)*YMAP(IL,2)+
     -      2*XMAP(IL,7)*YMAP(IL,2)+XMAP(IL,8)*YMAP(IL,2)+
     -      2*XMAP(IL,4)*YMAP(IL,3)-2*XMAP(IL,5)*YMAP(IL,3)+
     -      XMAP(IL,6)*YMAP(IL,3)-2*XMAP(IL,7)*YMAP(IL,3)-
     -      XMAP(IL,8)*YMAP(IL,3)+2*XMAP(IL,5)*YMAP(IL,4)+
     -      XMAP(IL,6)*YMAP(IL,4)+2*XMAP(IL,7)*YMAP(IL,4)-
     -      XMAP(IL,8)*YMAP(IL,4)-2*XMAP(IL,4)*YMAP(IL,5)+
     -      2*XMAP(IL,6)*YMAP(IL,5)-2*XMAP(IL,8)*YMAP(IL,5)-
     -      XMAP(IL,4)*YMAP(IL,6)-2*XMAP(IL,5)*YMAP(IL,6)+
     -      2*XMAP(IL,7)*YMAP(IL,6)-2*XMAP(IL,4)*YMAP(IL,7)-
     -      2*XMAP(IL,6)*YMAP(IL,7)+2*XMAP(IL,8)*YMAP(IL,7)+
     -      XMAP(IL,1)*(-2*YMAP(IL,4)+2*YMAP(IL,5)+YMAP(IL,6)+
     -      2*YMAP(IL,7)-YMAP(IL,8))+(XMAP(IL,4)+2*XMAP(IL,5)-
     -      2*XMAP(IL,7))*YMAP(IL,8)+XMAP(IL,3)*(-2*YMAP(IL,2)-
     -      2*YMAP(IL,4)+2*YMAP(IL,5)-YMAP(IL,6)+
     -      2*YMAP(IL,7)+YMAP(IL,8))-3*V**2*(XMAP(IL,6)*YMAP(IL,1)-
     -      XMAP(IL,7)*YMAP(IL,1)-XMAP(IL,8)*YMAP(IL,1)+
     -      XMAP(IL,6)*YMAP(IL,2)+XMAP(IL,7)*YMAP(IL,2)-
     -      XMAP(IL,8)*YMAP(IL,2)-XMAP(IL,6)*YMAP(IL,3)+
     -      XMAP(IL,7)*YMAP(IL,3)+XMAP(IL,8)*YMAP(IL,3)-
     -      XMAP(IL,6)*YMAP(IL,4)-XMAP(IL,7)*YMAP(IL,4)+
     -      XMAP(IL,8)*YMAP(IL,4)-2*XMAP(IL,6)*YMAP(IL,5)+
     -      2*XMAP(IL,8)*YMAP(IL,5)-2*XMAP(IL,7)*YMAP(IL,6)+
     -      2*XMAP(IL,6)*YMAP(IL,7)-2*XMAP(IL,8)*YMAP(IL,7)+
     -      XMAP(IL,5)*(YMAP(IL,1)-YMAP(IL,2)-YMAP(IL,3)+
     -      YMAP(IL,4)+2*YMAP(IL,6)-2*YMAP(IL,8))+
     -      XMAP(IL,4)*(YMAP(IL,1)-YMAP(IL,3)-YMAP(IL,5)+
     -      YMAP(IL,6)+YMAP(IL,7)-YMAP(IL,8))+2*XMAP(IL,7)*YMAP(IL,8)+
     -      (XMAP(IL,1)-XMAP(IL,3))*(YMAP(IL,2)-YMAP(IL,4)-YMAP(IL,5)-
     -      YMAP(IL,6)+YMAP(IL,7)+YMAP(IL,8)))+
     -      V*(4*XMAP(IL,6)*YMAP(IL,1)+3*XMAP(IL,7)*YMAP(IL,1)-
     -      4*XMAP(IL,8)*YMAP(IL,1)+4*XMAP(IL,6)*YMAP(IL,2)-
     -      3*XMAP(IL,7)*YMAP(IL,2)-4*XMAP(IL,8)*YMAP(IL,2)+
     -      4*XMAP(IL,6)*YMAP(IL,3)-5*XMAP(IL,7)*YMAP(IL,3)-
     -      4*XMAP(IL,8)*YMAP(IL,3)+4*XMAP(IL,6)*YMAP(IL,4)+
     -      5*XMAP(IL,7)*YMAP(IL,4)-4*XMAP(IL,8)*YMAP(IL,4)-
     -      8*XMAP(IL,6)*YMAP(IL,5)+8*XMAP(IL,8)*YMAP(IL,5)+
     -      8*XMAP(IL,7)*YMAP(IL,6)-8*XMAP(IL,6)*YMAP(IL,7)+
     -      8*XMAP(IL,8)*YMAP(IL,7)+XMAP(IL,5)*(5*YMAP(IL,1)-
     -      5*YMAP(IL,2)-3*YMAP(IL,3)+3*YMAP(IL,4)+8*YMAP(IL,6)-
     -      8*YMAP(IL,8))-8*XMAP(IL,7)*YMAP(IL,8)+
     -      XMAP(IL,4)*(3*YMAP(IL,2)+5*YMAP(IL,3)-3*YMAP(IL,5)-
     -      4*YMAP(IL,6)-5*YMAP(IL,7)+4*YMAP(IL,8))+
     -      XMAP(IL,1)*(5*YMAP(IL,2)+3*YMAP(IL,3)-5*YMAP(IL,5)-
     -      4*YMAP(IL,6)-3*YMAP(IL,7)+4*YMAP(IL,8))+XMAP(IL,3)*
     -      (-3*YMAP(IL,1)-5*YMAP(IL,4)+3*YMAP(IL,5)-4*YMAP(IL,6)+
     -      5*YMAP(IL,7)+4*YMAP(IL,8)))+XMAP(IL,2)*((-1+V)*(-2+3*V)*
     -      YMAP(IL,1)+2*YMAP(IL,3)-2*YMAP(IL,5)+YMAP(IL,6)-
     -      2*YMAP(IL,7)-YMAP(IL,8)+V*(-3*YMAP(IL,4)+5*YMAP(IL,5)-
     -      4*YMAP(IL,6)+3*YMAP(IL,7)+4*YMAP(IL,8)-
     -      3*V*(YMAP(IL,3)+YMAP(IL,5)-YMAP(IL,6)-YMAP(IL,7)+
     -      YMAP(IL,8))))))/8
*   Terms of the serendipity Jacobian.
       JACT11(U,V,IL)=(U**2*(-YMAP(IL,1)-YMAP(IL,2)+YMAP(IL,3)+
     -      YMAP(IL,4)+2*YMAP(IL,5)-2*YMAP(IL,7))+2*(-YMAP(IL,5)+
     -      YMAP(IL,7)+V*(YMAP(IL,1)+YMAP(IL,2)+YMAP(IL,3)+YMAP(IL,4)-
     -      2*YMAP(IL,6)-2*YMAP(IL,8)))+U*(YMAP(IL,1)-2*V*YMAP(IL,1)-
     -      YMAP(IL,2)+2*V*YMAP(IL,2)+YMAP(IL,3)+2*V*YMAP(IL,3)-
     -      YMAP(IL,4)-2*V*YMAP(IL,4)-4*V*YMAP(IL,6)+4*V*YMAP(IL,8)))/4
       JACT12(U,V,IL)=(U**2*(XMAP(IL,1)+XMAP(IL,2)-XMAP(IL,3)-
     -      XMAP(IL,4)-2*XMAP(IL,5)+2*XMAP(IL,7))-2*(-XMAP(IL,5)+
     -      XMAP(IL,7)+V*(XMAP(IL,1)+XMAP(IL,2)+XMAP(IL,3)+XMAP(IL,4)-
     -      2*XMAP(IL,6)-2*XMAP(IL,8)))+U*((-1+2*V)*XMAP(IL,1)+
     -      XMAP(IL,2)-2*V*XMAP(IL,2)-XMAP(IL,3)-2*V*XMAP(IL,3)+
     -      XMAP(IL,4)+2*V*XMAP(IL,4)+4*V*XMAP(IL,6)-4*V*XMAP(IL,8)))/4
       JACT21(U,V,IL)=(V*(-YMAP(IL,1)+YMAP(IL,2)-YMAP(IL,3)+YMAP(IL,4))-
     -      2*YMAP(IL,6)+2*U*((-1+V)*YMAP(IL,1)+(-1+V)*YMAP(IL,2)-
     -      YMAP(IL,3)-V*YMAP(IL,3)-YMAP(IL,4)-V*YMAP(IL,4)+
     -      2*YMAP(IL,5)-2*V*YMAP(IL,5)+2*YMAP(IL,7)+2*V*YMAP(IL,7))+
     -      V**2*(YMAP(IL,1)-YMAP(IL,2)-YMAP(IL,3)+YMAP(IL,4)+
     -      2*YMAP(IL,6)-2*YMAP(IL,8))+2*YMAP(IL,8))/4
       JACT22(U,V,IL)=(V*(XMAP(IL,1)-XMAP(IL,2)+XMAP(IL,3)-XMAP(IL,4))+
     -      2*U*(XMAP(IL,1)-V*XMAP(IL,1)+XMAP(IL,2)-V*XMAP(IL,2)+
     -      XMAP(IL,3)+V*XMAP(IL,3)+XMAP(IL,4)+V*XMAP(IL,4)-
     -      2*XMAP(IL,5)+2*V*XMAP(IL,5)-2*XMAP(IL,7)-2*V*XMAP(IL,7))+
     -      2*(XMAP(IL,6)-XMAP(IL,8))+V**2*(-XMAP(IL,1)+XMAP(IL,2)+
     -      XMAP(IL,3)-XMAP(IL,4)-2*XMAP(IL,6)+2*XMAP(IL,8)))/4
*** Debugging.
       IF(LIDENT)PRINT *,' /// ROUTINE MAPC5 ///'
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC5  DEBUG   : Input'',
     -      '' point:       (x, y) = ('',E12.5,'' , '',E12.5,'')'')')
     -      X,Y
*** This may fail.
       IFAIL=1
*** For now, do not consider degenerate elements.
       IF(ELMDGN(IMAP))THEN
            T1=0
            T2=0
            IFAIL=1
            RETURN
       ENDIF
*** Set tolerance parameter.
       F=0.5
*** Make a first order approximation.
       CALL MAPC4(X,Y,Z,T1,T2,T3,T4,JAC,DET,IMAP,IFAIL)
*   Failure to do this, e.g. due to degenerate elements.
       IF(IFAIL.NE.0)THEN
C            PRINT *,' !!!!!! MAPC5  WARNING : Failure to obtain'//
C     -           ' linear estimate of isoparametric coordinates'//
C     -           ' in element ',IMAP
            T1=0
            T2=0
            RETURN
*   Check whether the point is far outside.
       ELSEIF(T1.LT.-1.0-F.OR.T1.GT.1.0+F.OR.
     -        T2.LT.-1.0-F.OR.T2.GT.1.0+F)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC5  DEBUG   :'',
     -           '' Point far outside: T1 = '',F12.9,'', T2 = '',
     -           F12.9)') T1,T2
            IFAIL=1
            RETURN
       ENDIF
*   Convert to double precision for subsequent calculations.
       TD1=DBLE(T1)
       TD2=DBLE(T2)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC5  DEBUG   :'',
     -      '' Linear estimate:   (u, v) = ('',F12.9,'' , '',F12.9,
     -      '')'')') TD1,TD2
*** Iterate to refine the estimate.
       DO 210 ITER=1,10
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC5  DEBUG   :'',
     -      '' Iteration '',I5,'':   (u, v) = ('',F12.9,'' , '',F12.9,
     -      '')'')') ITER,TD1,TD2
*   Re-compute the (x,y,z) position for this coordinate.
       XR=XMAP(IMAP, 1)*(-(1-TD1)*(1-TD2)*(1+TD1+TD2))/4+
     -    XMAP(IMAP, 2)*(-(1+TD1)*(1-TD2)*(1-TD1+TD2))/4+
     -    XMAP(IMAP, 3)*(-(1+TD1)*(1+TD2)*(1-TD1-TD2))/4+
     -    XMAP(IMAP, 4)*(-(1-TD1)*(1+TD2)*(1+TD1-TD2))/4+
     -    XMAP(IMAP, 5)*(1-TD1)*(1+TD1)*(1-TD2)/2+
     -    XMAP(IMAP, 6)*(1+TD1)*(1+TD2)*(1-TD2)/2+
     -    XMAP(IMAP, 7)*(1-TD1)*(1+TD1)*(1+TD2)/2+
     -    XMAP(IMAP, 8)*(1-TD1)*(1+TD2)*(1-TD2)/2
       YR=YMAP(IMAP, 1)*(-(1-TD1)*(1-TD2)*(1+TD1+TD2))/4+
     -    YMAP(IMAP, 2)*(-(1+TD1)*(1-TD2)*(1-TD1+TD2))/4+
     -    YMAP(IMAP, 3)*(-(1+TD1)*(1+TD2)*(1-TD1-TD2))/4+
     -    YMAP(IMAP, 4)*(-(1-TD1)*(1+TD2)*(1+TD1-TD2))/4+
     -    YMAP(IMAP, 5)*(1-TD1)*(1+TD1)*(1-TD2)/2+
     -    YMAP(IMAP, 6)*(1+TD1)*(1+TD2)*(1-TD2)/2+
     -    YMAP(IMAP, 7)*(1-TD1)*(1+TD1)*(1+TD2)/2+
     -    YMAP(IMAP, 8)*(1-TD1)*(1+TD2)*(1-TD2)/2
*   Store the Jacobian
       JAC(1,1)=JACT11(TD1,TD2,IMAP)
       JAC(1,2)=JACT12(TD1,TD2,IMAP)
       JAC(2,1)=JACT21(TD1,TD2,IMAP)
       JAC(2,2)=JACT22(TD1,TD2,IMAP)
       DET=DETT(TD1,TD2,IMAP)
*   Compute the difference vector
       DIFF(1)=X-XR
       DIFF(2)=Y-YR
*   Update the estimate
       DO 220 L=1,2
       CORR(L)=0.0D0
       DO 230 K=1,2
       CORR(L)=CORR(L)+JAC(L,K)*DIFF(K)/DET
230    CONTINUE
220    CONTINUE
*   Debugging
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC5  DEBUG   : '',
     -      ''Difference vector: (x, y) = ('',E12.5,
     -      '' , '',E12.5,''), max = '',E12.5/
     -      26X,''Correction vector: (u, v) = ('',F12.9,
     -      '' , '',F12.9,''), max = '',F12.9)')
     -      (DIFF(I),I=1,2),
     -      MAX(ABS(DIFF(1)),ABS(DIFF(2))),
     -      (CORR(I),I=1,2),
     -      MAX(ABS(CORR(1)),ABS(CORR(2)))
*   Update the vector.
       TD1=TD1+CORR(1)
       TD2=TD2+CORR(2)
*   Check for convergence.
       IF(MAX(ABS(CORR(1)),ABS(CORR(2))).LT.1E-5)GOTO 240
210    CONTINUE
       IF(X.GT.MIN(XMAP(IMAP,1),XMAP(IMAP,2),XMAP(IMAP,3),
     -      XMAP(IMAP,4)).AND.X.LT.MAX(XMAP(IMAP,1),XMAP(IMAP,2),
     -      XMAP(IMAP,3),XMAP(IMAP,4)).AND.
     -      Y.GT.MIN(YMAP(IMAP,1),YMAP(IMAP,2),YMAP(IMAP,3),
     -      YMAP(IMAP,4)).AND.Y.LT.MAX(YMAP(IMAP,1),YMAP(IMAP,2),
     -      YMAP(IMAP,3),YMAP(IMAP,4)))
     -      PRINT *,' !!!!!! MAPC5  WARNING : No convergence'//
     -           ' achieved when refining internal isoparametric'//
     -           ' coordinates in element ',IMAP,' at position ',
     -           X,Y
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ MAPC5  DEBUG   : Convergence'',
     -           '' failure in element '',I6,'' for:'')') IMAP
            WRITE(LUNOUT,'(2X,''Pos: '',2(F12.5,2X))') X,Y
            WRITE(LUNOUT,'(2X,''P1:  '',2(F12.5,2X))') XMAP(IMAP,1),
     -           YMAP(IMAP,1)
            WRITE(LUNOUT,'(2X,''P2:  '',2(F12.5,2X))') XMAP(IMAP,2),
     -           YMAP(IMAP,2)
            WRITE(LUNOUT,'(2X,''P3:  '',2(F12.5,2X))') XMAP(IMAP,3),
     -           YMAP(IMAP,3)
            WRITE(LUNOUT,'(2X,''P4:  '',2(F12.5,2X))') XMAP(IMAP,4),
     -           YMAP(IMAP,4)
       ENDIF
       T1=0
       T2=0
       IFAIL=1
       RETURN
*   Convergence reached.
240    CONTINUE
       T1=TD1
       T2=TD2
       T3=0
       T4=0
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC5  DEBUG   :'',
     -      '' Convergence reached.''/
     -      26X,''Final coordinates: (u, v) = ('',F12.9,'' , '',
     -      F12.9,'')'')') T1,T2
*   Success.
       IFAIL=0
*** Re-compute the (x,y,z) position for this coordinate.
       IF(LDEBUG)THEN
            XR=XMAP(IMAP, 1)*(-(1-TD1)*(1-TD2)*(1+TD1+TD2))/4+
     -         XMAP(IMAP, 2)*(-(1+TD1)*(1-TD2)*(1-TD1+TD2))/4+
     -         XMAP(IMAP, 3)*(-(1+TD1)*(1+TD2)*(1-TD1-TD2))/4+
     -         XMAP(IMAP, 4)*(-(1-TD1)*(1+TD2)*(1+TD1-TD2))/4+
     -         XMAP(IMAP, 5)*(1-TD1)*(1+TD1)*(1-TD2)/2+
     -         XMAP(IMAP, 6)*(1+TD1)*(1+TD2)*(1-TD2)/2+
     -         XMAP(IMAP, 7)*(1-TD1)*(1+TD1)*(1+TD2)/2+
     -         XMAP(IMAP, 8)*(1-TD1)*(1+TD2)*(1-TD2)/2
            YR=YMAP(IMAP, 1)*(-(1-TD1)*(1-TD2)*(1+TD1+TD2))/4+
     -         YMAP(IMAP, 2)*(-(1+TD1)*(1-TD2)*(1-TD1+TD2))/4+
     -         YMAP(IMAP, 3)*(-(1+TD1)*(1+TD2)*(1-TD1-TD2))/4+
     -         YMAP(IMAP, 4)*(-(1-TD1)*(1+TD2)*(1+TD1-TD2))/4+
     -         YMAP(IMAP, 5)*(1-TD1)*(1+TD1)*(1-TD2)/2+
     -         YMAP(IMAP, 6)*(1+TD1)*(1+TD2)*(1-TD2)/2+
     -         YMAP(IMAP, 7)*(1-TD1)*(1+TD1)*(1+TD2)/2+
     -         YMAP(IMAP, 8)*(1-TD1)*(1+TD2)*(1-TD2)/2
            WRITE(LUNOUT,'(''  ++++++ MAPC5  DEBUG   : Position'',
     -           '' requested:     ('',E12.5,'' , '',E12.5,'')''/
     -           26X,''Reconstructed: ('',E12.5,'' , '',E12.5,'')''/
     -           26X,''Difference:    ('',E12.5,'' , '',E12.5,'')'')')
     -           X,Y,XR,YR,X-XR,Y-YR
       ENDIF
       END
