CDECK  ID>, MAPC13.
       SUBROUTINE MAPC13(X,Y,Z,T1,T2,T3,T4,JAC,DET,IMAP,IFAIL)
*-----------------------------------------------------------------------
*   MAPC13 - Finds the tetrahedral cooordinates of point (X,Y,Z) in a
*            quadratic tetrahedron.
*   (Last changed on  6/ 3/08.)
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
     -      JACT11,JACT12,JACT13,JACT14,
     -      JACT21,JACT22,JACT23,JACT24,
     -      JACT31,JACT32,JACT33,JACT34,
     -      JACT41,JACT42,JACT43,JACT44,
     -      XR,YR,ZR,SR,CORR(4),DIFF(4),
     -      JAC(4,4),DET,T,U,V,W,TD1,TD2,TD3,TD4
       INTEGER I,K,L,IMAP,IL,ITER,IFAIL
*   Determinant of the quadratic tetrahedron Jacobian
       DETT(T,U,V,W,IL)=
     -      -(((-4*V*XMAP(IL,10)-XMAP(IL,2)+4*U*XMAP(IL,2)+XMAP(IL,4)-
     -      4*W*XMAP(IL,4)+4*T*XMAP(IL,5)-4*T*XMAP(IL,7)+4*V*XMAP(IL,8)-
     -      4*U*XMAP(IL,9)+4*W*XMAP(IL,9))*
     -      (4*W*YMAP(IL,10)-YMAP(IL,3)+4*V*YMAP(IL,3)+4*T*YMAP(IL,6)+
     -      4*U*YMAP(IL,8))+
     -      (XMAP(IL,2)-4*U*XMAP(IL,2)-XMAP(IL,3)+4*V*XMAP(IL,3)-
     -      4*T*XMAP(IL,5)+4*T*XMAP(IL,6)+4*U*XMAP(IL,8)-4*V*XMAP(IL,8)+
     -      4*W*(XMAP(IL,10)-XMAP(IL,9)))*
     -       (4*V*YMAP(IL,10)-YMAP(IL,4)+4*W*YMAP(IL,4)+4*T*YMAP(IL,7)+
     -      4*U*YMAP(IL,9))+
     -      (-4*W*XMAP(IL,10)+4*V*(XMAP(IL,10)-XMAP(IL,3))+XMAP(IL,3)-
     -      XMAP(IL,4)+4*W*XMAP(IL,4)-4*T*XMAP(IL,6)+4*T*XMAP(IL,7)-
     -      4*U*XMAP(IL,8)+4*U*XMAP(IL,9))*
     -       ((-1+4*U)*YMAP(IL,2)+4*(T*YMAP(IL,5)+V*YMAP(IL,8)+
     -      W*YMAP(IL,9))))*((-1+4*T)*ZMAP(IL,1)+4*(U*ZMAP(IL,5)+
     -      V*ZMAP(IL,6)+W*ZMAP(IL,7))))-
     - ((XMAP(IL,2)-4*U*XMAP(IL,2)-XMAP(IL,4)+4*W*XMAP(IL,4)-
     -      4*T*XMAP(IL,5)+4*T*XMAP(IL,7)+4*V*(XMAP(IL,10)-XMAP(IL,8))+
     -      4*U*XMAP(IL,9)-4*W*XMAP(IL,9))*
     -     ((-1+4*T)*YMAP(IL,1)+4*(U*YMAP(IL,5)+V*YMAP(IL,6)+
     -      W*YMAP(IL,7)))-
     -    ((-1+4*T)*XMAP(IL,1)+XMAP(IL,2)-4*U*XMAP(IL,2)+
     -      4*(-(T*XMAP(IL,5))+U*XMAP(IL,5)+V*XMAP(IL,6)+W*XMAP(IL,7)-
     -      V*XMAP(IL,8)-W*XMAP(IL,9)))*
     -     (4*V*YMAP(IL,10)-YMAP(IL,4)+4*W*YMAP(IL,4)+4*T*YMAP(IL,7)+
     -      4*U*YMAP(IL,9))+
     -    ((-1+4*T)*XMAP(IL,1)-4*V*XMAP(IL,10)+XMAP(IL,4)-
     -      4*W*XMAP(IL,4)+4*U*XMAP(IL,5)+4*V*XMAP(IL,6)-4*T*XMAP(IL,7)+
     -      4*W*XMAP(IL,7)-4*U*XMAP(IL,9))*
     -     ((-1+4*U)*YMAP(IL,2)+4*(T*YMAP(IL,5)+V*YMAP(IL,8)+
     -      W*YMAP(IL,9))))*(4*W*ZMAP(IL,10)-ZMAP(IL,3)+4*V*ZMAP(IL,3)+
     -      4*T*ZMAP(IL,6)+4*U*ZMAP(IL,8))+
     - ((XMAP(IL,2)-4*U*XMAP(IL,2)-XMAP(IL,3)+4*V*XMAP(IL,3)-
     -      4*T*XMAP(IL,5)+4*T*XMAP(IL,6)+4*U*XMAP(IL,8)-4*V*XMAP(IL,8)+
     -      4*W*(XMAP(IL,10)-XMAP(IL,9)))*
     -     ((-1+4*T)*YMAP(IL,1)+4*(U*YMAP(IL,5)+V*YMAP(IL,6)+
     -      W*YMAP(IL,7)))-
     -    ((-1+4*T)*XMAP(IL,1)+XMAP(IL,2)-4*U*XMAP(IL,2)+
     -      4*(-(T*XMAP(IL,5))+U*XMAP(IL,5)+V*XMAP(IL,6)+W*XMAP(IL,7)-
     -      V*XMAP(IL,8)-W*XMAP(IL,9)))*
     -     (4*W*YMAP(IL,10)-YMAP(IL,3)+4*V*YMAP(IL,3)+4*T*YMAP(IL,6)+
     -      4*U*YMAP(IL,8))+
     -    ((-1+4*T)*XMAP(IL,1)-4*W*XMAP(IL,10)+XMAP(IL,3)-
     -      4*V*XMAP(IL,3)+4*U*XMAP(IL,5)-4*T*XMAP(IL,6)+4*V*XMAP(IL,6)+
     -      4*W*XMAP(IL,7)-4*U*XMAP(IL,8))*
     -     ((-1+4*U)*YMAP(IL,2)+4*(T*YMAP(IL,5)+V*YMAP(IL,8)+
     -      W*YMAP(IL,9))))*(4*V*ZMAP(IL,10)-ZMAP(IL,4)+4*W*ZMAP(IL,4)+
     -      4*T*ZMAP(IL,7)+4*U*ZMAP(IL,9))+
     - ((-4*W*XMAP(IL,10)+4*V*(XMAP(IL,10)-XMAP(IL,3))+XMAP(IL,3)-
     -      XMAP(IL,4)+4*W*XMAP(IL,4)-4*T*XMAP(IL,6)+4*T*XMAP(IL,7)-
     -      4*U*XMAP(IL,8)+4*U*XMAP(IL,9))*
     -     ((-1+4*T)*YMAP(IL,1)+4*(U*YMAP(IL,5)+V*YMAP(IL,6)+
     -      W*YMAP(IL,7)))+
     -    ((-1+4*T)*XMAP(IL,1)-4*V*XMAP(IL,10)+XMAP(IL,4)-
     -      4*W*XMAP(IL,4)+4*U*XMAP(IL,5)+4*V*XMAP(IL,6)-4*T*XMAP(IL,7)+
     -      4*W*XMAP(IL,7)-4*U*XMAP(IL,9))*
     -     (4*W*YMAP(IL,10)-YMAP(IL,3)+4*V*YMAP(IL,3)+4*T*YMAP(IL,6)+
     -      4*U*YMAP(IL,8))-
     -    ((-1+4*T)*XMAP(IL,1)-4*W*XMAP(IL,10)+XMAP(IL,3)-
     -      4*V*XMAP(IL,3)+4*U*XMAP(IL,5)-4*T*XMAP(IL,6)+4*V*XMAP(IL,6)+
     -      4*W*XMAP(IL,7)-4*U*XMAP(IL,8))*
     -     (4*V*YMAP(IL,10)-YMAP(IL,4)+4*W*YMAP(IL,4)+4*T*YMAP(IL,7)+
     -      4*U*YMAP(IL,9)))*((-1+4*U)*ZMAP(IL,2)+4*(T*ZMAP(IL,5)+
     -      V*ZMAP(IL,8)+W*ZMAP(IL,9)))
*   Terms of the quadratic tetrahedral Jacobian
       JACT11(T,U,V,W,IL)= -((((-1+4*U)*XMAP(IL,2)+4*(T*XMAP(IL,5)+
     -      V*XMAP(IL,8)+W*XMAP(IL,9)))*
     -         (4*V*YMAP(IL,10)-YMAP(IL,4)+4*W*YMAP(IL,4)+
     -      4*T*YMAP(IL,7)+4*U*YMAP(IL,9))-
     -        (4*V*XMAP(IL,10)-XMAP(IL,4)+4*W*XMAP(IL,4)+4*T*XMAP(IL,7)+
     -      4*U*XMAP(IL,9))*((-1+4*U)*YMAP(IL,2)+4*(T*YMAP(IL,5)+
     -      V*YMAP(IL,8)+W*YMAP(IL,9))))*
     -      (4*W*ZMAP(IL,10)-ZMAP(IL,3)+4*V*ZMAP(IL,3)+4*T*ZMAP(IL,6)+
     -      4*U*ZMAP(IL,8)))+
     -   (((-1+4*U)*XMAP(IL,2)+4*(T*XMAP(IL,5)+V*XMAP(IL,8)+
     -      W*XMAP(IL,9)))*(4*W*YMAP(IL,10)-YMAP(IL,3)+4*V*YMAP(IL,3)+
     -      4*T*YMAP(IL,6)+4*U*YMAP(IL,8))-
     -      (4*W*XMAP(IL,10)-XMAP(IL,3)+4*V*XMAP(IL,3)+4*T*XMAP(IL,6)+
     -      4*U*XMAP(IL,8))*((-1+4*U)*YMAP(IL,2)+4*(T*YMAP(IL,5)+
     -      V*YMAP(IL,8)+W*YMAP(IL,9))))*
     -    (4*V*ZMAP(IL,10)-ZMAP(IL,4)+4*W*ZMAP(IL,4)+4*T*ZMAP(IL,7)+
     -      4*U*ZMAP(IL,9))+
     -   (-((4*V*XMAP(IL,10)-XMAP(IL,4)+4*W*XMAP(IL,4)+4*T*XMAP(IL,7)+
     -      4*U*XMAP(IL,9))*(4*W*YMAP(IL,10)-YMAP(IL,3)+4*V*YMAP(IL,3)+
     -      4*T*YMAP(IL,6)+4*U*YMAP(IL,8)))+
     -      (4*W*XMAP(IL,10)-XMAP(IL,3)+4*V*XMAP(IL,3)+4*T*XMAP(IL,6)+
     -      4*U*XMAP(IL,8))*(4*V*YMAP(IL,10)-YMAP(IL,4)+4*W*YMAP(IL,4)+
     -      4*T*YMAP(IL,7)+4*U*YMAP(IL,9)))*
     -    ((-1+4*U)*ZMAP(IL,2)+4*(T*ZMAP(IL,5)+V*ZMAP(IL,8)+
     -      W*ZMAP(IL,9)))
       JACT12(T,U,V,W,IL)=
     -  (YMAP(IL,2)-4*U*YMAP(IL,2)-YMAP(IL,4)+4*W*YMAP(IL,4)-
     -      4*T*YMAP(IL,5)+4*T*YMAP(IL,7)+4*V*(YMAP(IL,10)-YMAP(IL,8))+
     -      4*U*YMAP(IL,9)-4*W*YMAP(IL,9))*
     -    (4*W*ZMAP(IL,10)-ZMAP(IL,3)+4*V*ZMAP(IL,3)+4*T*ZMAP(IL,6)+
     -      4*U*ZMAP(IL,8))+
     -   (-4*W*YMAP(IL,10)-YMAP(IL,2)+4*U*YMAP(IL,2)+YMAP(IL,3)-
     -      4*V*YMAP(IL,3)+4*T*YMAP(IL,5)-4*T*YMAP(IL,6)-4*U*YMAP(IL,8)+
     -      4*V*YMAP(IL,8)+4*W*YMAP(IL,9))*
     -    (4*V*ZMAP(IL,10)-ZMAP(IL,4)+4*W*ZMAP(IL,4)+4*T*ZMAP(IL,7)+
     -      4*U*ZMAP(IL,9))+
     -   (-4*V*YMAP(IL,10)+4*W*YMAP(IL,10)-YMAP(IL,3)+4*V*YMAP(IL,3)+
     -      YMAP(IL,4)-4*W*YMAP(IL,4)+4*T*YMAP(IL,6)-4*T*YMAP(IL,7)+
     -      4*U*YMAP(IL,8)-4*U*YMAP(IL,9))*
     -    ((-1+4*U)*ZMAP(IL,2)+4*(T*ZMAP(IL,5)+V*ZMAP(IL,8)+
     -      W*ZMAP(IL,9)))
       JACT13(T,U,V,W,IL)=
     -  (-4*V*XMAP(IL,10)-XMAP(IL,2)+4*U*XMAP(IL,2)+XMAP(IL,4)-
     -      4*W*XMAP(IL,4)+4*T*XMAP(IL,5)-4*T*XMAP(IL,7)+4*V*XMAP(IL,8)-
     -      4*U*XMAP(IL,9)+4*W*XMAP(IL,9))*
     -    (4*W*ZMAP(IL,10)-ZMAP(IL,3)+4*V*ZMAP(IL,3)+4*T*ZMAP(IL,6)+
     -      4*U*ZMAP(IL,8))+
     -   (XMAP(IL,2)-4*U*XMAP(IL,2)-XMAP(IL,3)+4*V*XMAP(IL,3)-
     -      4*T*XMAP(IL,5)+4*T*XMAP(IL,6)+4*U*XMAP(IL,8)-4*V*XMAP(IL,8)+
     -      4*W*(XMAP(IL,10)-XMAP(IL,9)))*
     -    (4*V*ZMAP(IL,10)-ZMAP(IL,4)+4*W*ZMAP(IL,4)+4*T*ZMAP(IL,7)+
     -      4*U*ZMAP(IL,9))+
     -   (-4*W*XMAP(IL,10)+4*V*(XMAP(IL,10)-XMAP(IL,3))+XMAP(IL,3)-
     -      XMAP(IL,4)+4*W*XMAP(IL,4)-4*T*XMAP(IL,6)+4*T*XMAP(IL,7)-
     -      4*U*XMAP(IL,8)+4*U*XMAP(IL,9))*
     -    ((-1+4*U)*ZMAP(IL,2)+4*(T*ZMAP(IL,5)+V*ZMAP(IL,8)+
     -      W*ZMAP(IL,9)))
       JACT14(T,U,V,W,IL)=
     -  (XMAP(IL,2)-4*U*XMAP(IL,2)-XMAP(IL,4)+4*W*XMAP(IL,4)-
     -      4*T*XMAP(IL,5)+4*T*XMAP(IL,7)+4*V*(XMAP(IL,10)-XMAP(IL,8))+
     -      4*U*XMAP(IL,9)-4*W*XMAP(IL,9))*
     -    (4*W*YMAP(IL,10)-YMAP(IL,3)+4*V*YMAP(IL,3)+4*T*YMAP(IL,6)+
     -      4*U*YMAP(IL,8))+
     -   (-4*W*XMAP(IL,10)-XMAP(IL,2)+4*U*XMAP(IL,2)+XMAP(IL,3)-
     -      4*V*XMAP(IL,3)+4*T*XMAP(IL,5)-4*T*XMAP(IL,6)-4*U*XMAP(IL,8)+
     -      4*V*XMAP(IL,8)+4*W*XMAP(IL,9))*
     -    (4*V*YMAP(IL,10)-YMAP(IL,4)+4*W*YMAP(IL,4)+4*T*YMAP(IL,7)+
     -      4*U*YMAP(IL,9))+
     -   (-4*V*XMAP(IL,10)+4*W*XMAP(IL,10)-XMAP(IL,3)+4*V*XMAP(IL,3)+
     -      XMAP(IL,4)-4*W*XMAP(IL,4)+4*T*XMAP(IL,6)-4*T*XMAP(IL,7)+
     -      4*U*XMAP(IL,8)-4*U*XMAP(IL,9))*
     -    ((-1+4*U)*YMAP(IL,2)+4*(T*YMAP(IL,5)+V*YMAP(IL,8)+
     -      W*YMAP(IL,9)))
       JACT21(T,U,V,W,IL)= -((-((4*V*XMAP(IL,10)-XMAP(IL,4)+
     -      4*W*XMAP(IL,4)+4*T*XMAP(IL,7)+4*U*XMAP(IL,9))*
     -           (4*W*YMAP(IL,10)-YMAP(IL,3)+4*V*YMAP(IL,3)+
     -      4*T*YMAP(IL,6)+4*U*YMAP(IL,8)))+
     -        (4*W*XMAP(IL,10)-XMAP(IL,3)+4*V*XMAP(IL,3)+4*T*XMAP(IL,6)+
     -      4*U*XMAP(IL,8))*(4*V*YMAP(IL,10)-YMAP(IL,4)+4*W*YMAP(IL,4)+
     -      4*T*YMAP(IL,7)+4*U*YMAP(IL,9)))*
     -      ((-1+4*T)*ZMAP(IL,1)+4*(U*ZMAP(IL,5)+V*ZMAP(IL,6)+
     -      W*ZMAP(IL,7))))+
     -   (-((4*V*XMAP(IL,10)-XMAP(IL,4)+4*W*XMAP(IL,4)+4*T*XMAP(IL,7)+
     -      4*U*XMAP(IL,9))*((-1+4*T)*YMAP(IL,1)+4*(U*YMAP(IL,5)+
     -      V*YMAP(IL,6)+W*YMAP(IL,7))))+
     -      ((-1+4*T)*XMAP(IL,1)+4*(U*XMAP(IL,5)+V*XMAP(IL,6)+
     -      W*XMAP(IL,7)))*(4*V*YMAP(IL,10)-YMAP(IL,4)+4*W*YMAP(IL,4)+
     -      4*T*YMAP(IL,7)+4*U*YMAP(IL,9)))*
     -    (4*W*ZMAP(IL,10)-ZMAP(IL,3)+4*V*ZMAP(IL,3)+4*T*ZMAP(IL,6)+
     -      4*U*ZMAP(IL,8))-
     -   (-((4*W*XMAP(IL,10)-XMAP(IL,3)+4*V*XMAP(IL,3)+4*T*XMAP(IL,6)+
     -      4*U*XMAP(IL,8))*((-1+4*T)*YMAP(IL,1)+4*(U*YMAP(IL,5)+
     -      V*YMAP(IL,6)+W*YMAP(IL,7))))+
     -      ((-1+4*T)*XMAP(IL,1)+4*(U*XMAP(IL,5)+V*XMAP(IL,6)+
     -      W*XMAP(IL,7)))*(4*W*YMAP(IL,10)-YMAP(IL,3)+4*V*YMAP(IL,3)+
     -      4*T*YMAP(IL,6)+4*U*YMAP(IL,8)))*
     -    (4*V*ZMAP(IL,10)-ZMAP(IL,4)+4*W*ZMAP(IL,4)+4*T*ZMAP(IL,7)+
     -      4*U*ZMAP(IL,9))
       JACT22(T,U,V,W,IL)=
     -  (-4*W*YMAP(IL,10)+4*V*(YMAP(IL,10)-YMAP(IL,3))+YMAP(IL,3)-
     -      YMAP(IL,4)+4*W*YMAP(IL,4)-4*T*YMAP(IL,6)+4*T*YMAP(IL,7)-
     -      4*U*YMAP(IL,8)+4*U*YMAP(IL,9))*
     -    ((-1+4*T)*ZMAP(IL,1)+4*(U*ZMAP(IL,5)+V*ZMAP(IL,6)+
     -      W*ZMAP(IL,7)))+
     -   ((-1+4*T)*YMAP(IL,1)-4*V*YMAP(IL,10)+YMAP(IL,4)-4*W*YMAP(IL,4)+
     -      4*U*YMAP(IL,5)+4*V*YMAP(IL,6)-4*T*YMAP(IL,7)+4*W*YMAP(IL,7)-
     -      4*U*YMAP(IL,9))*
     -    (4*W*ZMAP(IL,10)-ZMAP(IL,3)+4*V*ZMAP(IL,3)+4*T*ZMAP(IL,6)+
     -      4*U*ZMAP(IL,8))-
     -   ((-1+4*T)*YMAP(IL,1)-4*W*YMAP(IL,10)+YMAP(IL,3)-4*V*YMAP(IL,3)+
     -      4*U*YMAP(IL,5)-4*T*YMAP(IL,6)+4*V*YMAP(IL,6)+4*W*YMAP(IL,7)-
     -      4*U*YMAP(IL,8))*
     -    (4*V*ZMAP(IL,10)-ZMAP(IL,4)+4*W*ZMAP(IL,4)+4*T*ZMAP(IL,7)+
     -      4*U*ZMAP(IL,9))
       JACT23(T,U,V,W,IL)=
     -  (-4*V*XMAP(IL,10)+4*W*XMAP(IL,10)-XMAP(IL,3)+4*V*XMAP(IL,3)+
     -      XMAP(IL,4)-4*W*XMAP(IL,4)+4*T*XMAP(IL,6)-4*T*XMAP(IL,7)+
     -      4*U*XMAP(IL,8)-4*U*XMAP(IL,9))*
     -    ((-1+4*T)*ZMAP(IL,1)+4*(U*ZMAP(IL,5)+V*ZMAP(IL,6)+
     -      W*ZMAP(IL,7)))-
     -   ((-1+4*T)*XMAP(IL,1)-4*V*XMAP(IL,10)+XMAP(IL,4)-4*W*XMAP(IL,4)+
     -      4*U*XMAP(IL,5)+4*V*XMAP(IL,6)-4*T*XMAP(IL,7)+4*W*XMAP(IL,7)-
     -      4*U*XMAP(IL,9))*
     -    (4*W*ZMAP(IL,10)-ZMAP(IL,3)+4*V*ZMAP(IL,3)+4*T*ZMAP(IL,6)+
     -      4*U*ZMAP(IL,8))+
     -   ((-1+4*T)*XMAP(IL,1)-4*W*XMAP(IL,10)+XMAP(IL,3)-4*V*XMAP(IL,3)+
     -      4*U*XMAP(IL,5)-4*T*XMAP(IL,6)+4*V*XMAP(IL,6)+4*W*XMAP(IL,7)-
     -      4*U*XMAP(IL,8))*
     -    (4*V*ZMAP(IL,10)-ZMAP(IL,4)+4*W*ZMAP(IL,4)+4*T*ZMAP(IL,7)+
     -      4*U*ZMAP(IL,9))
       JACT24(T,U,V,W,IL)=
     -  (-4*W*XMAP(IL,10)+4*V*(XMAP(IL,10)-XMAP(IL,3))+XMAP(IL,3)-
     -      XMAP(IL,4)+4*W*XMAP(IL,4)-4*T*XMAP(IL,6)+4*T*XMAP(IL,7)-
     -      4*U*XMAP(IL,8)+4*U*XMAP(IL,9))*
     -    ((-1+4*T)*YMAP(IL,1)+4*(U*YMAP(IL,5)+V*YMAP(IL,6)+
     -      W*YMAP(IL,7)))+
     -   ((-1+4*T)*XMAP(IL,1)-4*V*XMAP(IL,10)+XMAP(IL,4)-4*W*XMAP(IL,4)+
     -      4*U*XMAP(IL,5)+4*V*XMAP(IL,6)-4*T*XMAP(IL,7)+4*W*XMAP(IL,7)-
     -      4*U*XMAP(IL,9))*
     -    (4*W*YMAP(IL,10)-YMAP(IL,3)+4*V*YMAP(IL,3)+4*T*YMAP(IL,6)+
     -      4*U*YMAP(IL,8))-
     -   ((-1+4*T)*XMAP(IL,1)-4*W*XMAP(IL,10)+XMAP(IL,3)-4*V*XMAP(IL,3)+
     -      4*U*XMAP(IL,5)-4*T*XMAP(IL,6)+4*V*XMAP(IL,6)+4*W*XMAP(IL,7)-
     -      4*U*XMAP(IL,8))*
     -    (4*V*YMAP(IL,10)-YMAP(IL,4)+4*W*YMAP(IL,4)+4*T*YMAP(IL,7)+
     -      4*U*YMAP(IL,9))
       JACT31(T,U,V,W,IL)=
     -   (((-1+4*U)*XMAP(IL,2)+4*(T*XMAP(IL,5)+V*XMAP(IL,8)+
     -      W*XMAP(IL,9)))*(4*V*YMAP(IL,10)-YMAP(IL,4)+4*W*YMAP(IL,4)+
     -      4*T*YMAP(IL,7)+4*U*YMAP(IL,9))-
     -      (4*V*XMAP(IL,10)-XMAP(IL,4)+4*W*XMAP(IL,4)+4*T*XMAP(IL,7)+
     -      4*U*XMAP(IL,9))*((-1+4*U)*YMAP(IL,2)+4*(T*YMAP(IL,5)+
     -      V*YMAP(IL,8)+W*YMAP(IL,9))))*
     -    ((-1+4*T)*ZMAP(IL,1)+4*(U*ZMAP(IL,5)+V*ZMAP(IL,6)+
     -      W*ZMAP(IL,7)))+
     -   (-(((-1+4*U)*XMAP(IL,2)+4*(T*XMAP(IL,5)+V*XMAP(IL,8)+
     -      W*XMAP(IL,9)))*((-1+4*T)*YMAP(IL,1)+4*(U*YMAP(IL,5)+
     -      V*YMAP(IL,6)+W*YMAP(IL,7))))+
     -      ((-1+4*T)*XMAP(IL,1)+4*(U*XMAP(IL,5)+V*XMAP(IL,6)+
     -      W*XMAP(IL,7)))*((-1+4*U)*YMAP(IL,2)+4*(T*YMAP(IL,5)+
     -      V*YMAP(IL,8)+W*YMAP(IL,9))))*
     -    (4*V*ZMAP(IL,10)-ZMAP(IL,4)+4*W*ZMAP(IL,4)+4*T*ZMAP(IL,7)+
     -      4*U*ZMAP(IL,9))-
     -   (-((4*V*XMAP(IL,10)-XMAP(IL,4)+4*W*XMAP(IL,4)+4*T*XMAP(IL,7)+
     -      4*U*XMAP(IL,9))*((-1+4*T)*YMAP(IL,1)+4*(U*YMAP(IL,5)+
     -      V*YMAP(IL,6)+W*YMAP(IL,7))))+
     -      ((-1+4*T)*XMAP(IL,1)+4*(U*XMAP(IL,5)+V*XMAP(IL,6)+
     -      W*XMAP(IL,7)))*(4*V*YMAP(IL,10)-YMAP(IL,4)+4*W*YMAP(IL,4)+
     -      4*T*YMAP(IL,7)+4*U*YMAP(IL,9)))*
     -    ((-1+4*U)*ZMAP(IL,2)+4*(T*ZMAP(IL,5)+V*ZMAP(IL,8)+
     -      W*ZMAP(IL,9)))
       JACT32(T,U,V,W,IL)=
     -  (-4*V*YMAP(IL,10)-YMAP(IL,2)+4*U*YMAP(IL,2)+YMAP(IL,4)-
     -      4*W*YMAP(IL,4)+4*T*YMAP(IL,5)-4*T*YMAP(IL,7)+4*V*YMAP(IL,8)-
     -      4*U*YMAP(IL,9)+4*W*YMAP(IL,9))*
     -    ((-1+4*T)*ZMAP(IL,1)+4*(U*ZMAP(IL,5)+V*ZMAP(IL,6)+
     -      W*ZMAP(IL,7)))+
     -   ((-1+4*T)*YMAP(IL,1)+YMAP(IL,2)-4*U*YMAP(IL,2)+
     -      4*(-(T*YMAP(IL,5))+U*YMAP(IL,5)+V*YMAP(IL,6)+W*YMAP(IL,7)-
     -      V*YMAP(IL,8)-W*YMAP(IL,9)))*
     -    (4*V*ZMAP(IL,10)-ZMAP(IL,4)+4*W*ZMAP(IL,4)+4*T*ZMAP(IL,7)+
     -      4*U*ZMAP(IL,9))-
     -   ((-1+4*T)*YMAP(IL,1)-4*V*YMAP(IL,10)+YMAP(IL,4)-4*W*YMAP(IL,4)+
     -      4*U*YMAP(IL,5)+4*V*YMAP(IL,6)-4*T*YMAP(IL,7)+4*W*YMAP(IL,7)-
     -      4*U*YMAP(IL,9))*
     -    ((-1+4*U)*ZMAP(IL,2)+4*(T*ZMAP(IL,5)+V*ZMAP(IL,8)+
     -      W*ZMAP(IL,9)))
       JACT33(T,U,V,W,IL)=
     -  (XMAP(IL,2)-4*U*XMAP(IL,2)-XMAP(IL,4)+4*W*XMAP(IL,4)-
     -      4*T*XMAP(IL,5)+4*T*XMAP(IL,7)+4*V*(XMAP(IL,10)-XMAP(IL,8))+
     -      4*U*XMAP(IL,9)-4*W*XMAP(IL,9))*
     -    ((-1+4*T)*ZMAP(IL,1)+4*(U*ZMAP(IL,5)+V*ZMAP(IL,6)+
     -      W*ZMAP(IL,7)))-
     -   ((-1+4*T)*XMAP(IL,1)+XMAP(IL,2)-4*U*XMAP(IL,2)+
     -      4*(-(T*XMAP(IL,5))+U*XMAP(IL,5)+V*XMAP(IL,6)+W*XMAP(IL,7)-
     -      V*XMAP(IL,8)-W*XMAP(IL,9)))*
     -    (4*V*ZMAP(IL,10)-ZMAP(IL,4)+4*W*ZMAP(IL,4)+4*T*ZMAP(IL,7)+
     -      4*U*ZMAP(IL,9))+
     -   ((-1+4*T)*XMAP(IL,1)-4*V*XMAP(IL,10)+XMAP(IL,4)-4*W*XMAP(IL,4)+
     -      4*U*XMAP(IL,5)+4*V*XMAP(IL,6)-4*T*XMAP(IL,7)+4*W*XMAP(IL,7)-
     -      4*U*XMAP(IL,9))*
     -    ((-1+4*U)*ZMAP(IL,2)+4*(T*ZMAP(IL,5)+V*ZMAP(IL,8)+
     -      W*ZMAP(IL,9)))
       JACT34(T,U,V,W,IL)=
     -  (-4*V*XMAP(IL,10)-XMAP(IL,2)+4*U*XMAP(IL,2)+XMAP(IL,4)-
     -      4*W*XMAP(IL,4)+4*T*XMAP(IL,5)-4*T*XMAP(IL,7)+4*V*XMAP(IL,8)-
     -      4*U*XMAP(IL,9)+4*W*XMAP(IL,9))*
     -    ((-1+4*T)*YMAP(IL,1)+4*(U*YMAP(IL,5)+V*YMAP(IL,6)+
     -      W*YMAP(IL,7)))+
     -   ((-1+4*T)*XMAP(IL,1)+XMAP(IL,2)-4*U*XMAP(IL,2)+
     -      4*(-(T*XMAP(IL,5))+U*XMAP(IL,5)+V*XMAP(IL,6)+W*XMAP(IL,7)-
     -      V*XMAP(IL,8)-W*XMAP(IL,9)))*
     -    (4*V*YMAP(IL,10)-YMAP(IL,4)+4*W*YMAP(IL,4)+4*T*YMAP(IL,7)+
     -      4*U*YMAP(IL,9))-
     -   ((-1+4*T)*XMAP(IL,1)-4*V*XMAP(IL,10)+XMAP(IL,4)-4*W*XMAP(IL,4)+
     -      4*U*XMAP(IL,5)+4*V*XMAP(IL,6)-4*T*XMAP(IL,7)+4*W*XMAP(IL,7)-
     -      4*U*XMAP(IL,9))*
     -    ((-1+4*U)*YMAP(IL,2)+4*(T*YMAP(IL,5)+V*YMAP(IL,8)+
     -      W*YMAP(IL,9)))
       JACT41(T,U,V,W,IL)=
     -     -((((-1+4*U)*XMAP(IL,2)+4*(T*XMAP(IL,5)+V*XMAP(IL,8)+
     -      W*XMAP(IL,9)))*
     -         (4*W*YMAP(IL,10)-YMAP(IL,3)+4*V*YMAP(IL,3)+
     -      4*T*YMAP(IL,6)+4*U*YMAP(IL,8))-
     -        (4*W*XMAP(IL,10)-XMAP(IL,3)+4*V*XMAP(IL,3)+4*T*XMAP(IL,6)+
     -      4*U*XMAP(IL,8))*((-1+4*U)*YMAP(IL,2)+4*(T*YMAP(IL,5)+
     -      V*YMAP(IL,8)+W*YMAP(IL,9))))*
     -      ((-1+4*T)*ZMAP(IL,1)+4*(U*ZMAP(IL,5)+V*ZMAP(IL,6)+
     -      W*ZMAP(IL,7))))-
     -   (-(((-1+4*U)*XMAP(IL,2)+4*(T*XMAP(IL,5)+V*XMAP(IL,8)+
     -      W*XMAP(IL,9)))*((-1+4*T)*YMAP(IL,1)+4*(U*YMAP(IL,5)+
     -      V*YMAP(IL,6)+W*YMAP(IL,7))))+
     -      ((-1+4*T)*XMAP(IL,1)+4*(U*XMAP(IL,5)+V*XMAP(IL,6)+
     -      W*XMAP(IL,7)))*((-1+4*U)*YMAP(IL,2)+4*(T*YMAP(IL,5)+
     -      V*YMAP(IL,8)+W*YMAP(IL,9))))*
     -    (4*W*ZMAP(IL,10)-ZMAP(IL,3)+4*V*ZMAP(IL,3)+4*T*ZMAP(IL,6)+
     -      4*U*ZMAP(IL,8))+
     -   (-((4*W*XMAP(IL,10)-XMAP(IL,3)+4*V*XMAP(IL,3)+4*T*XMAP(IL,6)+
     -      4*U*XMAP(IL,8))*((-1+4*T)*YMAP(IL,1)+4*(U*YMAP(IL,5)+
     -      V*YMAP(IL,6)+W*YMAP(IL,7))))+
     -      ((-1+4*T)*XMAP(IL,1)+4*(U*XMAP(IL,5)+V*XMAP(IL,6)+
     -      W*XMAP(IL,7)))*(4*W*YMAP(IL,10)-YMAP(IL,3)+4*V*YMAP(IL,3)+
     -      4*T*YMAP(IL,6)+4*U*YMAP(IL,8)))*
     -    ((-1+4*U)*ZMAP(IL,2)+4*(T*ZMAP(IL,5)+V*ZMAP(IL,8)+
     -      W*ZMAP(IL,9)))
       JACT42(T,U,V,W,IL)=
     -  (YMAP(IL,2)-4*U*YMAP(IL,2)-YMAP(IL,3)+4*V*YMAP(IL,3)-
     -      4*T*YMAP(IL,5)+4*T*YMAP(IL,6)+4*U*YMAP(IL,8)-4*V*YMAP(IL,8)+
     -      4*W*(YMAP(IL,10)-YMAP(IL,9)))*
     -    ((-1+4*T)*ZMAP(IL,1)+4*(U*ZMAP(IL,5)+V*ZMAP(IL,6)+
     -      W*ZMAP(IL,7)))-
     -   ((-1+4*T)*YMAP(IL,1)+YMAP(IL,2)-4*U*YMAP(IL,2)+
     -      4*(-(T*YMAP(IL,5))+U*YMAP(IL,5)+V*YMAP(IL,6)+W*YMAP(IL,7)-
     -      V*YMAP(IL,8)-W*YMAP(IL,9)))*
     -    (4*W*ZMAP(IL,10)-ZMAP(IL,3)+4*V*ZMAP(IL,3)+4*T*ZMAP(IL,6)+
     -      4*U*ZMAP(IL,8))+
     -   ((-1+4*T)*YMAP(IL,1)-4*W*YMAP(IL,10)+YMAP(IL,3)-4*V*YMAP(IL,3)+
     -      4*U*YMAP(IL,5)-4*T*YMAP(IL,6)+4*V*YMAP(IL,6)+4*W*YMAP(IL,7)-
     -      4*U*YMAP(IL,8))*
     -    ((-1+4*U)*ZMAP(IL,2)+4*(T*ZMAP(IL,5)+V*ZMAP(IL,8)+
     -      W*ZMAP(IL,9)))
       JACT43(T,U,V,W,IL)=
     -  (-4*W*XMAP(IL,10)-XMAP(IL,2)+4*U*XMAP(IL,2)+XMAP(IL,3)-
     -      4*V*XMAP(IL,3)+4*T*XMAP(IL,5)-4*T*XMAP(IL,6)-4*U*XMAP(IL,8)+
     -      4*V*XMAP(IL,8)+4*W*XMAP(IL,9))*
     -    ((-1+4*T)*ZMAP(IL,1)+4*(U*ZMAP(IL,5)+V*ZMAP(IL,6)+
     -      W*ZMAP(IL,7)))+
     -   ((-1+4*T)*XMAP(IL,1)+XMAP(IL,2)-4*U*XMAP(IL,2)+
     -      4*(-(T*XMAP(IL,5))+U*XMAP(IL,5)+V*XMAP(IL,6)+W*XMAP(IL,7)-
     -      V*XMAP(IL,8)-W*XMAP(IL,9)))*
     -    (4*W*ZMAP(IL,10)-ZMAP(IL,3)+4*V*ZMAP(IL,3)+4*T*ZMAP(IL,6)+
     -      4*U*ZMAP(IL,8))-
     -   ((-1+4*T)*XMAP(IL,1)-4*W*XMAP(IL,10)+XMAP(IL,3)-4*V*XMAP(IL,3)+
     -      4*U*XMAP(IL,5)-4*T*XMAP(IL,6)+4*V*XMAP(IL,6)+
     -      4*W*XMAP(IL,7)-4*U*XMAP(IL,8))*
     -    ((-1+4*U)*ZMAP(IL,2)+4*(T*ZMAP(IL,5)+V*ZMAP(IL,8)+
     -      W*ZMAP(IL,9)))
       JACT44(T,U,V,W,IL)=
     -  (XMAP(IL,2)-4*U*XMAP(IL,2)-XMAP(IL,3)+4*V*XMAP(IL,3)-
     -      4*T*XMAP(IL,5)+4*T*XMAP(IL,6)+4*U*XMAP(IL,8)-4*V*XMAP(IL,8)+
     -      4*W*(XMAP(IL,10)-XMAP(IL,9)))*
     -    ((-1+4*T)*YMAP(IL,1)+4*(U*YMAP(IL,5)+V*YMAP(IL,6)+
     -      W*YMAP(IL,7)))-
     -   ((-1+4*T)*XMAP(IL,1)+XMAP(IL,2)-4*U*XMAP(IL,2)+
     -      4*(-(T*XMAP(IL,5))+U*XMAP(IL,5)+V*XMAP(IL,6)+W*XMAP(IL,7)-
     -      V*XMAP(IL,8)-W*XMAP(IL,9)))*
     -    (4*W*YMAP(IL,10)-YMAP(IL,3)+4*V*YMAP(IL,3)+4*T*YMAP(IL,6)+
     -      4*U*YMAP(IL,8))+
     -   ((-1+4*T)*XMAP(IL,1)-4*W*XMAP(IL,10)+XMAP(IL,3)-4*V*XMAP(IL,3)+
     -      4*U*XMAP(IL,5)-4*T*XMAP(IL,6)+4*V*XMAP(IL,6)+4*W*XMAP(IL,7)-
     -      4*U*XMAP(IL,8))*
     -    ((-1+4*U)*YMAP(IL,2)+4*(T*YMAP(IL,5)+V*YMAP(IL,8)+
     -      W*YMAP(IL,9)))
*** Debugging.
       IF(LIDENT)PRINT *,' /// ROUTINE MAPC13 ///'
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC13 DEBUG   : Input'',
     -      '' point:       (   x, y, z) = ('',15X,E12.5,'' , '',E12.5,
     -      '' , '',E12.5,'')'')') X,Y,Z
*** This may fail.
       IFAIL=1
*** Make a first order approximation, using the linear tetrahedron.
       CALL MAPC12(X,Y,Z,T1,T2,T3,T4,JAC,DET,IMAP,IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! MAPC13 WARNING : Calculation of initial'//
     -           ' coordinates failed; abandoned.'
            RETURN
       ENDIF
*   Convert to double precision for subsequent calculations.
       TD1=DBLE(T1)
       TD2=DBLE(T2)
       TD3=DBLE(T3)
       TD4=DBLE(T4)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC13 DEBUG   :'',
     -      '' Linear estimate:   (t, u, v, w) = ('',F12.9,
     -      '' , '',F12.9,'' , '',F12.9,'' , '',F12.9,''), sum = '',
     -      F12.9)') TD1,TD2,TD3,TD4,TD1+TD2+TD3+TD4
*** Iterate to refine the estimate.
       DO 210 ITER=1,10
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC13 DEBUG   :'',
     -      '' Iteration '',I5,'':   (t, u, v, w) = ('',F12.9,
     -      '' , '',F12.9,'' , '',F12.9,'' , '',F12.9,''), sum = '',
     -      F12.9)') ITER,TD1,TD2,TD3,TD4,TD1+TD2+TD3+TD4
*   Re-compute the (x,y,z) position for this coordinate.
       XR=XMAP(IMAP, 1)*TD1*(2*TD1-1)+
     -    XMAP(IMAP, 2)*TD2*(2*TD2-1)+
     -    XMAP(IMAP, 3)*TD3*(2*TD3-1)+
     -    XMAP(IMAP, 4)*TD4*(2*TD4-1)+
     -    XMAP(IMAP, 5)*4*TD1*TD2+
     -    XMAP(IMAP, 6)*4*TD1*TD3+
     -    XMAP(IMAP, 7)*4*TD1*TD4+
     -    XMAP(IMAP, 8)*4*TD2*TD3+
     -    XMAP(IMAP, 9)*4*TD2*TD4+
     -    XMAP(IMAP,10)*4*TD3*TD4
       YR=YMAP(IMAP, 1)*TD1*(2*TD1-1)+
     -    YMAP(IMAP, 2)*TD2*(2*TD2-1)+
     -    YMAP(IMAP, 3)*TD3*(2*TD3-1)+
     -    YMAP(IMAP, 4)*TD4*(2*TD4-1)+
     -    YMAP(IMAP, 5)*4*TD1*TD2+
     -    YMAP(IMAP, 6)*4*TD1*TD3+
     -    YMAP(IMAP, 7)*4*TD1*TD4+
     -    YMAP(IMAP, 8)*4*TD2*TD3+
     -    YMAP(IMAP, 9)*4*TD2*TD4+
     -    YMAP(IMAP,10)*4*TD3*TD4
       ZR=ZMAP(IMAP, 1)*TD1*(2*TD1-1)+
     -    ZMAP(IMAP, 2)*TD2*(2*TD2-1)+
     -    ZMAP(IMAP, 3)*TD3*(2*TD3-1)+
     -    ZMAP(IMAP, 4)*TD4*(2*TD4-1)+
     -    ZMAP(IMAP, 5)*4*TD1*TD2+
     -    ZMAP(IMAP, 6)*4*TD1*TD3+
     -    ZMAP(IMAP, 7)*4*TD1*TD4+
     -    ZMAP(IMAP, 8)*4*TD2*TD3+
     -    ZMAP(IMAP, 9)*4*TD2*TD4+
     -    ZMAP(IMAP,10)*4*TD3*TD4
       SR=TD1+TD2+TD3+TD4
*   Store the Jacobian
       JAC(1,1)=JACT11(TD1,TD2,TD3,TD4,IMAP)
       JAC(1,2)=JACT12(TD1,TD2,TD3,TD4,IMAP)
       JAC(1,3)=JACT13(TD1,TD2,TD3,TD4,IMAP)
       JAC(1,4)=JACT14(TD1,TD2,TD3,TD4,IMAP)
       JAC(2,1)=JACT21(TD1,TD2,TD3,TD4,IMAP)
       JAC(2,2)=JACT22(TD1,TD2,TD3,TD4,IMAP)
       JAC(2,3)=JACT23(TD1,TD2,TD3,TD4,IMAP)
       JAC(2,4)=JACT24(TD1,TD2,TD3,TD4,IMAP)
       JAC(3,1)=JACT31(TD1,TD2,TD3,TD4,IMAP)
       JAC(3,2)=JACT32(TD1,TD2,TD3,TD4,IMAP)
       JAC(3,3)=JACT33(TD1,TD2,TD3,TD4,IMAP)
       JAC(3,4)=JACT34(TD1,TD2,TD3,TD4,IMAP)
       JAC(4,1)=JACT41(TD1,TD2,TD3,TD4,IMAP)
       JAC(4,2)=JACT42(TD1,TD2,TD3,TD4,IMAP)
       JAC(4,3)=JACT43(TD1,TD2,TD3,TD4,IMAP)
       JAC(4,4)=JACT44(TD1,TD2,TD3,TD4,IMAP)
       DET=DETT(TD1,TD2,TD3,TD4,IMAP)
*   Compute the difference vector
       DIFF(1)=1.0D0-SR
       DIFF(2)=X-XR
       DIFF(3)=Y-YR
       DIFF(4)=Z-ZR
*   Update the estimate
       DO 220 L=1,4
       CORR(L)=0.0D0
       DO 230 K=1,4
       CORR(L)=CORR(L)+JAC(L,K)*DIFF(K)/DET
230    CONTINUE
220    CONTINUE
*   Debugging
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC13 DEBUG   : '',
     -      ''Difference vector: (1, x, y, z) = ('',E12.5,
     -      '' , '',E12.5,'' , '',E12.5,'' , '',E12.5,''),'',
     -      '' max = '',E12.5/
     -      26X,''Correction vector: (t, u, v, w) = ('',F12.9,
     -      '' , '',F12.9,'' , '',F12.9,'' , '',F12.9,''),'',
     -      '' max = '',F12.9)')
     -      (DIFF(I),I=1,4),
     -      MAX(ABS(DIFF(1)),ABS(DIFF(2)),ABS(DIFF(3)),ABS(DIFF(4))),
     -      (CORR(I),I=1,4),
     -      MAX(ABS(CORR(1)),ABS(CORR(2)),ABS(CORR(3)),ABS(CORR(4)))
*   Update the vector.
       TD1=TD1+CORR(1)
       TD2=TD2+CORR(2)
       TD3=TD3+CORR(3)
       TD4=TD4+CORR(4)
*   Check for convergence.
       IF(MAX(ABS(CORR(1)),ABS(CORR(2)),
     -        ABS(CORR(3)),ABS(CORR(4))).LT.1E-5)GOTO 240
210    CONTINUE
       IF(X.GT.MIN(XMAP(IMAP,1),XMAP(IMAP,2),XMAP(IMAP,3),
     -      XMAP(IMAP,4)).AND.X.LT.MAX(XMAP(IMAP,1),XMAP(IMAP,2),
     -      XMAP(IMAP,3),XMAP(IMAP,4)).AND.
     -      Y.GT.MIN(YMAP(IMAP,1),YMAP(IMAP,2),YMAP(IMAP,3),
     -      YMAP(IMAP,4)).AND.Y.LT.MAX(YMAP(IMAP,1),YMAP(IMAP,2),
     -      YMAP(IMAP,3),YMAP(IMAP,4)).AND.
     -      Z.GT.MIN(ZMAP(IMAP,1),ZMAP(IMAP,2),ZMAP(IMAP,3),
     -      ZMAP(IMAP,4)).AND.Z.LT.MAX(ZMAP(IMAP,1),ZMAP(IMAP,2),
     -      ZMAP(IMAP,3),ZMAP(IMAP,4)))
     -      PRINT *,' !!!!!! MAPC13 WARNING : No convergence'//
     -           ' achieved when refining internal isoparametric'//
     -           ' coordinates in element ',IMAP,'.'
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ MAPC13 DEBUG   : Convergence'',
     -           '' failure in element '',I6,'' for:'')') IMAP
            WRITE(LUNOUT,'(2X,''Pos: '',3(F12.5,2X))') X,Y,Z
            WRITE(LUNOUT,'(2X,''P1:  '',3(F12.5,2X))') XMAP(IMAP,1),
     -           YMAP(IMAP,1),ZMAP(IMAP,1)
            WRITE(LUNOUT,'(2X,''P2:  '',3(F12.5,2X))') XMAP(IMAP,2),
     -           YMAP(IMAP,2),ZMAP(IMAP,2)
            WRITE(LUNOUT,'(2X,''P3:  '',3(F12.5,2X))') XMAP(IMAP,3),
     -           YMAP(IMAP,3),ZMAP(IMAP,3)
            WRITE(LUNOUT,'(2X,''P4:  '',3(F12.5,2X))') XMAP(IMAP,4),
     -           YMAP(IMAP,4),ZMAP(IMAP,4)
       ENDIF
       TD1=1.0
       TD2=-1.0
       TD3=1.0
       TD4=-1.0
       IFAIL=1
       RETURN
*   Convergence reached.
240    CONTINUE
       T1=TD1
       T2=TD2
       T3=TD3
       T4=TD4
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC13 DEBUG   :'',
     -      '' Convergence reached.''/
     -      26X,''Final coordinates: (t, u, v, w) = ('',F12.9,
     -      '' , '',F12.9,'' , '',F12.9,'' , '',F12.9,''),'',
     -      '' sum = '',F12.9)') T1,T2,T3,T4,T1+T2+T3+T4
*   Success.
       IFAIL=0
*** Re-compute the (x,y,z) position for this coordinate.
       IF(LDEBUG)THEN
            XR=XMAP(IMAP, 1)*TD1*(2*TD1-1)+
     -         XMAP(IMAP, 2)*TD2*(2*TD2-1)+
     -         XMAP(IMAP, 3)*TD3*(2*TD3-1)+
     -         XMAP(IMAP, 4)*TD4*(2*TD4-1)+
     -         XMAP(IMAP, 5)*4*TD1*TD2+
     -         XMAP(IMAP, 6)*4*TD1*TD3+
     -         XMAP(IMAP, 7)*4*TD1*TD4+
     -         XMAP(IMAP, 8)*4*TD2*TD3+
     -         XMAP(IMAP, 9)*4*TD2*TD4+
     -         XMAP(IMAP,10)*4*TD3*TD4
            YR=YMAP(IMAP, 1)*TD1*(2*TD1-1)+
     -         YMAP(IMAP, 2)*TD2*(2*TD2-1)+
     -         YMAP(IMAP, 3)*TD3*(2*TD3-1)+
     -         YMAP(IMAP, 4)*TD4*(2*TD4-1)+
     -         YMAP(IMAP, 5)*4*TD1*TD2+
     -         YMAP(IMAP, 6)*4*TD1*TD3+
     -         YMAP(IMAP, 7)*4*TD1*TD4+
     -         YMAP(IMAP, 8)*4*TD2*TD3+
     -         YMAP(IMAP, 9)*4*TD2*TD4+
     -         YMAP(IMAP,10)*4*TD3*TD4
            ZR=ZMAP(IMAP, 1)*TD1*(2*TD1-1)+
     -         ZMAP(IMAP, 2)*TD2*(2*TD2-1)+
     -         ZMAP(IMAP, 3)*TD3*(2*TD3-1)+
     -         ZMAP(IMAP, 4)*TD4*(2*TD4-1)+
     -         ZMAP(IMAP, 5)*4*TD1*TD2+
     -         ZMAP(IMAP, 6)*4*TD1*TD3+
     -         ZMAP(IMAP, 7)*4*TD1*TD4+
     -         ZMAP(IMAP, 8)*4*TD2*TD3+
     -         ZMAP(IMAP, 9)*4*TD2*TD4+
     -         ZMAP(IMAP,10)*4*TD3*TD4
            SR=TD1+TD2+TD3+TD4
            WRITE(LUNOUT,'(''  ++++++ MAPC13 DEBUG   : Position'',
     -           '' requested:     ('',E12.5,'' , '',E12.5,'' , '',
     -           E12.5,'')''/
     -           26X,''Position reconstructed: ('',E12.5,'' , '',E12.5,
     -           '' , '',E12.5,'')''/
     -           26X,''Difference:             ('',E12.5,'' , '',E12.5,
     -           '' , '',E12.5,'')''/
     -           26X,''Checksum-1: '',13X,F12.9)')
     -           X,Y,Z,XR,YR,ZR,X-XR,Y-YR,Z-ZR,SR-1
       ENDIF
       END
