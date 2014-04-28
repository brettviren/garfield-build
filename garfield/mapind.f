CDECK  ID>, MAPIND.
       SUBROUTINE MAPIND(X,Y,Z,T1,T2,T3,T4,JAC,DET,IMAP)
*-----------------------------------------------------------------------
*   MAPIND - Finds the index of the triangle or tetrahedron in which
*            (X,Y,Z) is located and returns the triangle / tetrahedron
*            coordinates of the point.
*   (Last changed on 11/ 2/11.)
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
       INTEGER MXFOUN
       PARAMETER(MXFOUN=10)
       REAL X,Y,Z,T1,T2,T3,T4,TT1,TT2,TT3,XMIN,XMAX,YMIN,YMAX,
     -      ZMIN,ZMAX,F,T1BAK,T2BAK,T3BAK,T4BAK
       DOUBLE PRECISION DETH,JACH11,JACH12,JACH13,JACH21,JACH22,JACH23,
     -      JACH31,JACH32,JACH33,DETP,JACP11,JACP12,JACP13,JACP21,
     -      JACP22,JACP23,JACP31,JACP32,JACP33,XR,YR,ZR,CORR(3),DIFF(3),
     -      JAC(4,4),DET,U,V,W,VECX,VECY,VECZ,DIST(6),VNORM,TD1,TD2,TD3,
     -      JACBAK(4,4),DETBAK
       INTEGER I,J,K,L,IMAP,IL,NFOUND,IFOUND(MXFOUN),IFAIL,NODES(6,3),
     -      IEL,ITER,IMPBAK
       SAVE IL,NODES
*** Statement functions: determinant of hexahedral Jacobian
       DETH(U,V,W,IEL)=
     -   (((-((-1+V)*(-1+W)*XMAP(IEL,1))+(-1+V)*(-1+W)*XMAP(IEL,2)-
     -         (1+V)*(-1+W)*XMAP(IEL,3) + (1+V)*(-1+W)*XMAP(IEL,4)+
     -        (-1+V)* (1+W)*XMAP(IEL,5) -(-1+V)* (1+W)*XMAP(IEL,6)+
     -         (1+V)* (1+W)*XMAP(IEL,7) - (1+V)* (1+W)*XMAP(IEL,8))*
     -     (-((-1+U)*(-1+W)*YMAP(IEL,1))+ (1+U)*(-1+W)*YMAP(IEL,2)-
     -         (1+U)*(-1+W)*YMAP(IEL,3) +(-1+U)*(-1+W)*YMAP(IEL,4)+
     -        (-1+U)* (1+W)*YMAP(IEL,5) - (1+U)* (1+W)*YMAP(IEL,6)+
     -         (1+U)* (1+W)*YMAP(IEL,7) -(-1+U)* (1+W)*YMAP(IEL,8))-
     -     (-((-1+U)*(-1+W)*XMAP(IEL,1))+ (1+U)*(-1+W)*XMAP(IEL,2)-
     -         (1+U)*(-1+W)*XMAP(IEL,3) +(-1+U)*(-1+W)*XMAP(IEL,4)+
     -        (-1+U)* (1+W)*XMAP(IEL,5) - (1+U)* (1+W)*XMAP(IEL,6)+
     -         (1+U)* (1+W)*XMAP(IEL,7) -(-1+U)* (1+W)*XMAP(IEL,8))*
     -     (-((-1+V)*(-1+W)*YMAP(IEL,1))+(-1+V)*(-1+W)*YMAP(IEL,2)-
     -         (1+V)*(-1+W)*YMAP(IEL,3) + (1+V)*(-1+W)*YMAP(IEL,4)+
     -        (-1+V)* (1+W)*YMAP(IEL,5) -(-1+V)* (1+W)*YMAP(IEL,6)+
     -         (1+V)* (1+W)*YMAP(IEL,7) - (1+V)* (1+W)*YMAP(IEL,8)))*
     -     (-((-1+U)*(-1+V)*ZMAP(IEL,1))+ (1+U)*(-1+V)*ZMAP(IEL,2)-
     -         (1+U)* (1+V)*ZMAP(IEL,3) +(-1+U)* (1+V)*ZMAP(IEL,4)+
     -        (-1+U)*(-1+V)*ZMAP(IEL,5) - (1+U)*(-1+V)*ZMAP(IEL,6)+
     -         (1+U)* (1+V)*ZMAP(IEL,7) -(-1+U)* (1+V)*ZMAP(IEL,8))-
     -    ((-((-1+V)*(-1+W)*XMAP(IEL,1))+(-1+V)*(-1+W)*XMAP(IEL,2)-
     -         (1+V)*(-1+W)*XMAP(IEL,3) + (1+V)*(-1+W)*XMAP(IEL,4)+
     -        (-1+V)* (1+W)*XMAP(IEL,5) -(-1+V)* (1+W)*XMAP(IEL,6)+
     -         (1+V)* (1+W)*XMAP(IEL,7) - (1+V)* (1+W)*XMAP(IEL,8))*
     -     (-((-1+U)*(-1+V)*YMAP(IEL,1))+ (1+U)*(-1+V)*YMAP(IEL,2)-
     -         (1+U)* (1+V)*YMAP(IEL,3) +(-1+U)* (1+V)*YMAP(IEL,4)+
     -        (-1+U)*(-1+V)*YMAP(IEL,5) - (1+U)*(-1+V)*YMAP(IEL,6)+
     -         (1+U)* (1+V)*YMAP(IEL,7) -(-1+U)* (1+V)*YMAP(IEL,8))-
     -     (-((-1+U)*(-1+V)*XMAP(IEL,1))+ (1+U)*(-1+V)*XMAP(IEL,2)-
     -         (1+U)* (1+V)*XMAP(IEL,3) +(-1+U)* (1+V)*XMAP(IEL,4)+
     -        (-1+U)*(-1+V)*XMAP(IEL,5) - (1+U)*(-1+V)*XMAP(IEL,6)+
     -         (1+U)* (1+V)*XMAP(IEL,7) -(-1+U)* (1+V)*XMAP(IEL,8))*
     -     (-((-1+V)*(-1+W)*YMAP(IEL,1))+(-1+V)*(-1+W)*YMAP(IEL,2)-
     -         (1+V)*(-1+W)*YMAP(IEL,3) + (1+V)*(-1+W)*YMAP(IEL,4)+
     -        (-1+V)* (1+W)*YMAP(IEL,5) -(-1+V)* (1+W)*YMAP(IEL,6)+
     -         (1+V)* (1+W)*YMAP(IEL,7) - (1+V)* (1+W)*YMAP(IEL,8)))*
     -     (-((-1+U)*(-1+W)*ZMAP(IEL,1))+ (1+U)*(-1+W)*ZMAP(IEL,2)-
     -         (1+U)*(-1+W)*ZMAP(IEL,3) +(-1+U)*(-1+W)*ZMAP(IEL,4)+
     -        (-1+U)* (1+W)*ZMAP(IEL,5) - (1+U)* (1+W)*ZMAP(IEL,6)+
     -         (1+U)* (1+W)*ZMAP(IEL,7) -(-1+U)* (1+W)*ZMAP(IEL,8))+
     -    ((-((-1+U)*(-1+W)*XMAP(IEL,1))+ (1+U)*(-1+W)*XMAP(IEL,2)-
     -         (1+U)*(-1+W)*XMAP(IEL,3) +(-1+U)*(-1+W)*XMAP(IEL,4)+
     -        (-1+U)* (1+W)*XMAP(IEL,5) - (1+U)* (1+W)*XMAP(IEL,6)+
     -         (1+U)* (1+W)*XMAP(IEL,7) -(-1+U)* (1+W)*XMAP(IEL,8))*
     -     (-((-1+U)*(-1+V)*YMAP(IEL,1))+ (1+U)*(-1+V)*YMAP(IEL,2)-
     -         (1+U)* (1+V)*YMAP(IEL,3) +(-1+U)* (1+V)*YMAP(IEL,4)+
     -        (-1+U)*(-1+V)*YMAP(IEL,5) - (1+U)*(-1+V)*YMAP(IEL,6)+
     -         (1+U)* (1+V)*YMAP(IEL,7) -(-1+U)* (1+V)*YMAP(IEL,8))-
     -     (-((-1+U)*(-1+V)*XMAP(IEL,1))+ (1+U)*(-1+V)*XMAP(IEL,2)-
     -         (1+U)* (1+V)*XMAP(IEL,3) +(-1+U)* (1+V)*XMAP(IEL,4)+
     -        (-1+U)*(-1+V)*XMAP(IEL,5) - (1+U)*(-1+V)*XMAP(IEL,6)+
     -         (1+U)* (1+V)*XMAP(IEL,7) -(-1+U)* (1+V)*XMAP(IEL,8))*
     -     (-((-1+U)*(-1+W)*YMAP(IEL,1))+ (1+U)*(-1+W)*YMAP(IEL,2)-
     -         (1+U)*(-1+W)*YMAP(IEL,3) +(-1+U)*(-1+W)*YMAP(IEL,4)+
     -        (-1+U)* (1+W)*YMAP(IEL,5) - (1+U)* (1+W)*YMAP(IEL,6)+
     -         (1+U)* (1+W)*YMAP(IEL,7) -(-1+U)* (1+W)*YMAP(IEL,8)))*
     -     (-((-1+V)*(-1+W)*ZMAP(IEL,1))+(-1+V)*(-1+W)*ZMAP(IEL,2)-
     -         (1+V)*(-1+W)*ZMAP(IEL,3) + (1+V)*(-1+W)*ZMAP(IEL,4)+
     -        (-1+V)* (1+W)*ZMAP(IEL,5) -(-1+V)* (1+W)*ZMAP(IEL,6)+
     -         (1+V)* (1+W)*ZMAP(IEL,7) - (1+V)* (1+W)*ZMAP(IEL,8)))/512
*   Various terms of the hexahedral Jacobian.
       JACH11(U,V,W,IEL)=
     -    ((-((-1+U)*(-1+W)*YMAP(IEL,1))+ (1+U)*(-1+W)*YMAP(IEL,2)-
     -         (1+U)*(-1+W)*YMAP(IEL,3) +(-1+U)*(-1+W)*YMAP(IEL,4)+
     -        (-1+U)* (1+W)*YMAP(IEL,5) - (1+U)* (1+W)*YMAP(IEL,6)+
     -         (1+U)* (1+W)*YMAP(IEL,7) -(-1+U)* (1+W)*YMAP(IEL,8))*
     -     (-((-1+U)*(-1+V)*ZMAP(IEL,1))+ (1+U)*(-1+V)*ZMAP(IEL,2)-
     -         (1+U)* (1+V)*ZMAP(IEL,3) +(-1+U)* (1+V)*ZMAP(IEL,4)+
     -        (-1+U)*(-1+V)*ZMAP(IEL,5) - (1+U)*(-1+V)*ZMAP(IEL,6)+
     -         (1+U)* (1+V)*ZMAP(IEL,7) -(-1+U)* (1+V)*ZMAP(IEL,8))-
     -     (-((-1+U)*(-1+V)*YMAP(IEL,1))+ (1+U)*(-1+V)*YMAP(IEL,2)-
     -         (1+U)* (1+V)*YMAP(IEL,3) +(-1+U)* (1+V)*YMAP(IEL,4)+
     -        (-1+U)*(-1+V)*YMAP(IEL,5) - (1+U)*(-1+V)*YMAP(IEL,6)+
     -         (1+U)* (1+V)*YMAP(IEL,7) -(-1+U)* (1+V)*YMAP(IEL,8))*
     -     (-((-1+U)*(-1+W)*ZMAP(IEL,1))+ (1+U)*(-1+W)*ZMAP(IEL,2)-
     -         (1+U)*(-1+W)*ZMAP(IEL,3) +(-1+U)*(-1+W)*ZMAP(IEL,4)+
     -        (-1+U)* (1+W)*ZMAP(IEL,5) - (1+U)* (1+W)*ZMAP(IEL,6)+
     -         (1+U)* (1+W)*ZMAP(IEL,7) -(-1+U)* (1+W)*ZMAP(IEL,8)))/64
       JACH12(U,V,W,IEL)=
     -  (-((-((-1+U)*(-1+W)*XMAP(IEL,1))+ (1+U)*(-1+W)*XMAP(IEL,2)-
     -         (1+U)*(-1+W)*XMAP(IEL,3) +(-1+U)*(-1+W)*XMAP(IEL,4)+
     -        (-1+U)* (1+W)*XMAP(IEL,5) - (1+U)* (1+W)*XMAP(IEL,6)+
     -         (1+U)* (1+W)*XMAP(IEL,7) -(-1+U)* (1+W)*XMAP(IEL,8))*
     -     (-((-1+U)*(-1+V)*ZMAP(IEL,1))+ (1+U)*(-1+V)*ZMAP(IEL,2)-
     -         (1+U)* (1+V)*ZMAP(IEL,3) +(-1+U)* (1+V)*ZMAP(IEL,4)+
     -        (-1+U)*(-1+V)*ZMAP(IEL,5) - (1+U)*(-1+V)*ZMAP(IEL,6)+
     -         (1+U)* (1+V)*ZMAP(IEL,7) -(-1+U)* (1+V)*ZMAP(IEL,8)))+
     -     (-((-1+U)*(-1+V)*XMAP(IEL,1))+ (1+U)*(-1+V)*XMAP(IEL,2)-
     -         (1+U)* (1+V)*XMAP(IEL,3) +(-1+U)* (1+V)*XMAP(IEL,4)+
     -        (-1+U)*(-1+V)*XMAP(IEL,5) - (1+U)*(-1+V)*XMAP(IEL,6)+
     -         (1+U)* (1+V)*XMAP(IEL,7) -(-1+U)* (1+V)*XMAP(IEL,8))*
     -     (-((-1+U)*(-1+W)*ZMAP(IEL,1))+ (1+U)*(-1+W)*ZMAP(IEL,2)-
     -         (1+U)*(-1+W)*ZMAP(IEL,3) +(-1+U)*(-1+W)*ZMAP(IEL,4)+
     -        (-1+U)* (1+W)*ZMAP(IEL,5) - (1+U)* (1+W)*ZMAP(IEL,6)+
     -         (1+U)* (1+W)*ZMAP(IEL,7) -(-1+U)* (1+W)*ZMAP(IEL,8)))/64
       JACH13(U,V,W,IEL)=
     -    ((-((-1+U)*(-1+W)*XMAP(IEL,1))+ (1+U)*(-1+W)*XMAP(IEL,2)-
     -         (1+U)*(-1+W)*XMAP(IEL,3) +(-1+U)*(-1+W)*XMAP(IEL,4)+
     -        (-1+U)* (1+W)*XMAP(IEL,5) - (1+U)* (1+W)*XMAP(IEL,6)+
     -         (1+U)* (1+W)*XMAP(IEL,7) -(-1+U)* (1+W)*XMAP(IEL,8))*
     -     (-((-1+U)*(-1+V)*YMAP(IEL,1))+ (1+U)*(-1+V)*YMAP(IEL,2)-
     -         (1+U)* (1+V)*YMAP(IEL,3) +(-1+U)* (1+V)*YMAP(IEL,4)+
     -        (-1+U)*(-1+V)*YMAP(IEL,5) - (1+U)*(-1+V)*YMAP(IEL,6)+
     -         (1+U)* (1+V)*YMAP(IEL,7) -(-1+U)* (1+V)*YMAP(IEL,8))-
     -     (-((-1+U)*(-1+V)*XMAP(IEL,1))+ (1+U)*(-1+V)*XMAP(IEL,2)-
     -         (1+U)* (1+V)*XMAP(IEL,3) +(-1+U)* (1+V)*XMAP(IEL,4)+
     -        (-1+U)*(-1+V)*XMAP(IEL,5) - (1+U)*(-1+V)*XMAP(IEL,6)+
     -         (1+U)* (1+V)*XMAP(IEL,7) -(-1+U)* (1+V)*XMAP(IEL,8))*
     -     (-((-1+U)*(-1+W)*YMAP(IEL,1))+ (1+U)*(-1+W)*YMAP(IEL,2)-
     -         (1+U)*(-1+W)*YMAP(IEL,3) +(-1+U)*(-1+W)*YMAP(IEL,4)+
     -        (-1+U)* (1+W)*YMAP(IEL,5) - (1+U)* (1+W)*YMAP(IEL,6)+
     -         (1+U)* (1+W)*YMAP(IEL,7) -(-1+U)* (1+W)*YMAP(IEL,8)))/64
       JACH21(U,V,W,IEL)=
     -  (-((-((-1+V)*(-1+W)*YMAP(IEL,1))+(-1+V)*(-1+W)*YMAP(IEL,2)-
     -         (1+V)*(-1+W)*YMAP(IEL,3) + (1+V)*(-1+W)*YMAP(IEL,4)+
     -        (-1+V)* (1+W)*YMAP(IEL,5) -(-1+V)* (1+W)*YMAP(IEL,6)+
     -         (1+V)* (1+W)*YMAP(IEL,7) - (1+V)* (1+W)*YMAP(IEL,8))*
     -     (-((-1+U)*(-1+V)*ZMAP(IEL,1))+ (1+U)*(-1+V)*ZMAP(IEL,2)-
     -         (1+U)* (1+V)*ZMAP(IEL,3) +(-1+U)* (1+V)*ZMAP(IEL,4)+
     -        (-1+U)*(-1+V)*ZMAP(IEL,5) - (1+U)*(-1+V)*ZMAP(IEL,6)+
     -         (1+U)* (1+V)*ZMAP(IEL,7) -(-1+U)* (1+V)*ZMAP(IEL,8)))+
     -     (-((-1+U)*(-1+V)*YMAP(IEL,1))+ (1+U)*(-1+V)*YMAP(IEL,2)-
     -         (1+U)* (1+V)*YMAP(IEL,3) +(-1+U)* (1+V)*YMAP(IEL,4)+
     -        (-1+U)*(-1+V)*YMAP(IEL,5) - (1+U)*(-1+V)*YMAP(IEL,6)+
     -         (1+U)* (1+V)*YMAP(IEL,7) -(-1+U)* (1+V)*YMAP(IEL,8))*
     -     (-((-1+V)*(-1+W)*ZMAP(IEL,1))+(-1+V)*(-1+W)*ZMAP(IEL,2)-
     -         (1+V)*(-1+W)*ZMAP(IEL,3) + (1+V)*(-1+W)*ZMAP(IEL,4)+
     -        (-1+V)* (1+W)*ZMAP(IEL,5) -(-1+V)* (1+W)*ZMAP(IEL,6)+
     -         (1+V)* (1+W)*ZMAP(IEL,7) - (1+V)* (1+W)*ZMAP(IEL,8)))/64
       JACH22(U,V,W,IEL)=
     -    ((-((-1+V)*(-1+W)*XMAP(IEL,1))+(-1+V)*(-1+W)*XMAP(IEL,2)-
     -         (1+V)*(-1+W)*XMAP(IEL,3) + (1+V)*(-1+W)*XMAP(IEL,4)+
     -        (-1+V)* (1+W)*XMAP(IEL,5) -(-1+V)* (1+W)*XMAP(IEL,6)+
     -         (1+V)* (1+W)*XMAP(IEL,7) - (1+V)* (1+W)*XMAP(IEL,8))*
     -     (-((-1+U)*(-1+V)*ZMAP(IEL,1))+ (1+U)*(-1+V)*ZMAP(IEL,2)-
     -         (1+U)* (1+V)*ZMAP(IEL,3) +(-1+U)* (1+V)*ZMAP(IEL,4)+
     -        (-1+U)*(-1+V)*ZMAP(IEL,5) - (1+U)*(-1+V)*ZMAP(IEL,6)+
     -         (1+U)* (1+V)*ZMAP(IEL,7) -(-1+U)* (1+V)*ZMAP(IEL,8))-
     -     (-((-1+U)*(-1+V)*XMAP(IEL,1))+ (1+U)*(-1+V)*XMAP(IEL,2)-
     -         (1+U)* (1+V)*XMAP(IEL,3) +(-1+U)* (1+V)*XMAP(IEL,4)+
     -        (-1+U)*(-1+V)*XMAP(IEL,5) - (1+U)*(-1+V)*XMAP(IEL,6)+
     -         (1+U)* (1+V)*XMAP(IEL,7) -(-1+U)* (1+V)*XMAP(IEL,8))*
     -     (-((-1+V)*(-1+W)*ZMAP(IEL,1))+(-1+V)*(-1+W)*ZMAP(IEL,2)-
     -         (1+V)*(-1+W)*ZMAP(IEL,3) + (1+V)*(-1+W)*ZMAP(IEL,4)+
     -        (-1+V)* (1+W)*ZMAP(IEL,5) -(-1+V)* (1+W)*ZMAP(IEL,6)+
     -         (1+V)* (1+W)*ZMAP(IEL,7) - (1+V)* (1+W)*ZMAP(IEL,8)))/64
       JACH23(U,V,W,IEL)=
     -  (-((-((-1+V)*(-1+W)*XMAP(IEL,1))+(-1+V)*(-1+W)*XMAP(IEL,2)-
     -         (1+V)*(-1+W)*XMAP(IEL,3) + (1+V)*(-1+W)*XMAP(IEL,4)+
     -        (-1+V)* (1+W)*XMAP(IEL,5) -(-1+V)* (1+W)*XMAP(IEL,6)+
     -         (1+V)* (1+W)*XMAP(IEL,7) - (1+V)* (1+W)*XMAP(IEL,8))*
     -     (-((-1+U)*(-1+V)*YMAP(IEL,1))+ (1+U)*(-1+V)*YMAP(IEL,2)-
     -         (1+U)* (1+V)*YMAP(IEL,3) +(-1+U)* (1+V)*YMAP(IEL,4)+
     -        (-1+U)*(-1+V)*YMAP(IEL,5) - (1+U)*(-1+V)*YMAP(IEL,6)+
     -         (1+U)* (1+V)*YMAP(IEL,7) -(-1+U)* (1+V)*YMAP(IEL,8)))+
     -     (-((-1+U)*(-1+V)*XMAP(IEL,1))+ (1+U)*(-1+V)*XMAP(IEL,2)-
     -         (1+U)* (1+V)*XMAP(IEL,3) +(-1+U)* (1+V)*XMAP(IEL,4)+
     -        (-1+U)*(-1+V)*XMAP(IEL,5) - (1+U)*(-1+V)*XMAP(IEL,6)+
     -         (1+U)* (1+V)*XMAP(IEL,7) -(-1+U)* (1+V)*XMAP(IEL,8))*
     -     (-((-1+V)*(-1+W)*YMAP(IEL,1))+(-1+V)*(-1+W)*YMAP(IEL,2)-
     -         (1+V)*(-1+W)*YMAP(IEL,3) + (1+V)*(-1+W)*YMAP(IEL,4)+
     -        (-1+V)* (1+W)*YMAP(IEL,5) -(-1+V)* (1+W)*YMAP(IEL,6)+
     -         (1+V)* (1+W)*YMAP(IEL,7) - (1+V)* (1+W)*YMAP(IEL,8)))/64
       JACH31(U,V,W,IEL)=
     -    ((-((-1+V)*(-1+W)*YMAP(IEL,1))+(-1+V)*(-1+W)*YMAP(IEL,2)-
     -         (1+V)*(-1+W)*YMAP(IEL,3) + (1+V)*(-1+W)*YMAP(IEL,4)+
     -        (-1+V)* (1+W)*YMAP(IEL,5) -(-1+V)* (1+W)*YMAP(IEL,6)+
     -         (1+V)* (1+W)*YMAP(IEL,7) - (1+V)* (1+W)*YMAP(IEL,8))*
     -     (-((-1+U)*(-1+W)*ZMAP(IEL,1))+ (1+U)*(-1+W)*ZMAP(IEL,2)-
     -         (1+U)*(-1+W)*ZMAP(IEL,3) +(-1+U)*(-1+W)*ZMAP(IEL,4)+
     -        (-1+U)* (1+W)*ZMAP(IEL,5) - (1+U)* (1+W)*ZMAP(IEL,6)+
     -         (1+U)* (1+W)*ZMAP(IEL,7) -(-1+U)* (1+W)*ZMAP(IEL,8))-
     -     (-((-1+U)*(-1+W)*YMAP(IEL,1))+ (1+U)*(-1+W)*YMAP(IEL,2)-
     -         (1+U)*(-1+W)*YMAP(IEL,3) +(-1+U)*(-1+W)*YMAP(IEL,4)+
     -        (-1+U)* (1+W)*YMAP(IEL,5) - (1+U)* (1+W)*YMAP(IEL,6)+
     -         (1+U)* (1+W)*YMAP(IEL,7) -(-1+U)* (1+W)*YMAP(IEL,8))*
     -     (-((-1+V)*(-1+W)*ZMAP(IEL,1))+(-1+V)*(-1+W)*ZMAP(IEL,2)-
     -         (1+V)*(-1+W)*ZMAP(IEL,3) + (1+V)*(-1+W)*ZMAP(IEL,4)+
     -        (-1+V)* (1+W)*ZMAP(IEL,5) -(-1+V)* (1+W)*ZMAP(IEL,6)+
     -         (1+V)* (1+W)*ZMAP(IEL,7) - (1+V)* (1+W)*ZMAP(IEL,8)))/64
       JACH32(U,V,W,IEL)=
     -  (-((-((-1+V)*(-1+W)*XMAP(IEL,1))+(-1+V)*(-1+W)*XMAP(IEL,2)-
     -         (1+V)*(-1+W)*XMAP(IEL,3) + (1+V)*(-1+W)*XMAP(IEL,4)+
     -        (-1+V)* (1+W)*XMAP(IEL,5) -(-1+V)* (1+W)*XMAP(IEL,6)+
     -         (1+V)* (1+W)*XMAP(IEL,7) - (1+V)* (1+W)*XMAP(IEL,8))*
     -     (-((-1+U)*(-1+W)*ZMAP(IEL,1))+ (1+U)*(-1+W)*ZMAP(IEL,2)-
     -         (1+U)*(-1+W)*ZMAP(IEL,3) +(-1+U)*(-1+W)*ZMAP(IEL,4)+
     -        (-1+U)* (1+W)*ZMAP(IEL,5) - (1+U)* (1+W)*ZMAP(IEL,6)+
     -         (1+U)* (1+W)*ZMAP(IEL,7) -(-1+U)* (1+W)*ZMAP(IEL,8)))+
     -     (-((-1+U)*(-1+W)*XMAP(IEL,1))+ (1+U)*(-1+W)*XMAP(IEL,2)-
     -         (1+U)*(-1+W)*XMAP(IEL,3) +(-1+U)*(-1+W)*XMAP(IEL,4)+
     -        (-1+U)* (1+W)*XMAP(IEL,5) - (1+U)* (1+W)*XMAP(IEL,6)+
     -         (1+U)* (1+W)*XMAP(IEL,7) -(-1+U)* (1+W)*XMAP(IEL,8))*
     -     (-((-1+V)*(-1+W)*ZMAP(IEL,1))+(-1+V)*(-1+W)*ZMAP(IEL,2)-
     -         (1+V)*(-1+W)*ZMAP(IEL,3) + (1+V)*(-1+W)*ZMAP(IEL,4)+
     -        (-1+V)* (1+W)*ZMAP(IEL,5) -(-1+V)* (1+W)*ZMAP(IEL,6)+
     -         (1+V)* (1+W)*ZMAP(IEL,7) - (1+V)* (1+W)*ZMAP(IEL,8)))/64
       JACH33(U,V,W,IEL)=
     -    ((-((-1+V)*(-1+W)*XMAP(IEL,1))+(-1+V)*(-1+W)*XMAP(IEL,2)-
     -         (1+V)*(-1+W)*XMAP(IEL,3) + (1+V)*(-1+W)*XMAP(IEL,4)+
     -        (-1+V)* (1+W)*XMAP(IEL,5) -(-1+V)* (1+W)*XMAP(IEL,6)+
     -         (1+V)* (1+W)*XMAP(IEL,7) - (1+V)* (1+W)*XMAP(IEL,8))*
     -     (-((-1+U)*(-1+W)*YMAP(IEL,1))+ (1+U)*(-1+W)*YMAP(IEL,2)-
     -         (1+U)*(-1+W)*YMAP(IEL,3) +(-1+U)*(-1+W)*YMAP(IEL,4)+
     -        (-1+U)* (1+W)*YMAP(IEL,5) - (1+U)* (1+W)*YMAP(IEL,6)+
     -         (1+U)* (1+W)*YMAP(IEL,7) -(-1+U)* (1+W)*YMAP(IEL,8))-
     -     (-((-1+U)*(-1+W)*XMAP(IEL,1))+ (1+U)*(-1+W)*XMAP(IEL,2)-
     -         (1+U)*(-1+W)*XMAP(IEL,3) +(-1+U)*(-1+W)*XMAP(IEL,4)+
     -        (-1+U)* (1+W)*XMAP(IEL,5) - (1+U)* (1+W)*XMAP(IEL,6)+
     -         (1+U)* (1+W)*XMAP(IEL,7) -(-1+U)* (1+W)*XMAP(IEL,8))*
     -     (-((-1+V)*(-1+W)*YMAP(IEL,1))+(-1+V)*(-1+W)*YMAP(IEL,2)-
     -         (1+V)*(-1+W)*YMAP(IEL,3) + (1+V)*(-1+W)*YMAP(IEL,4)+
     -        (-1+V)* (1+W)*YMAP(IEL,5) -(-1+V)* (1+W)*YMAP(IEL,6)+
     -         (1+V)* (1+W)*YMAP(IEL,7) - (1+V)* (1+W)*YMAP(IEL,8)))/64
*   Determinant of the pentahedral (degenerate hexahedral) Jacobian
       DETP(U,V,W,IEL)=
     -    ((-(((-1+U+V)*XMAP(IEL,1)-V*XMAP(IEL,3)+XMAP(IEL,5)-
     -    V*XMAP(IEL,5)-U*(XMAP(IEL,2)+XMAP(IEL,5)-XMAP(IEL,6))+
     -    V*XMAP(IEL,7))*
     -    ((-1+W)*YMAP(IEL,1)+YMAP(IEL,3)-W*YMAP(IEL,3)-
     -    (1+W)*(YMAP(IEL,5)-YMAP(IEL,7))))+
     -    ((-1+W)*XMAP(IEL,1)+XMAP(IEL,3)-W*XMAP(IEL,3)-
     -    (1+W)*(XMAP(IEL,5)-XMAP(IEL,7)))*
     -    ((-1+U+V)*YMAP(IEL,1)-V*YMAP(IEL,3)+YMAP(IEL,5)-V*YMAP(IEL,5)-
     -    U*(YMAP(IEL,2)+YMAP(IEL,5)-YMAP(IEL,6))+V*YMAP(IEL,7)))*
     -    ((-1+W)*ZMAP(IEL,1)+ZMAP(IEL,2)-W*ZMAP(IEL,2)-
     -    (1+W)*(ZMAP(IEL,5)-ZMAP(IEL,6)))-
     -    (-(((-1+U+V)*XMAP(IEL,1)-V*XMAP(IEL,3)+XMAP(IEL,5)-
     -    V*XMAP(IEL,5)-U*(XMAP(IEL,2)+XMAP(IEL,5)-XMAP(IEL,6))+
     -    V*XMAP(IEL,7))*
     -    ((-1+W)*YMAP(IEL,1)+YMAP(IEL,2)-W*YMAP(IEL,2)-
     -    (1+W)*(YMAP(IEL,5)-YMAP(IEL,6))))+
     -    ((-1+W)*XMAP(IEL,1)+XMAP(IEL,2)-W*XMAP(IEL,2)-
     -    (1+W)*(XMAP(IEL,5)-XMAP(IEL,6)))*
     -    ((-1+U+V)*YMAP(IEL,1)-V*YMAP(IEL,3)+YMAP(IEL,5)-V*YMAP(IEL,5)-
     -    U*(YMAP(IEL,2)+YMAP(IEL,5)-YMAP(IEL,6))+V*YMAP(IEL,7)))*
     -    ((-1+W)*ZMAP(IEL,1)+ZMAP(IEL,3)-W*ZMAP(IEL,3)-
     -    (1+W)*(ZMAP(IEL,5)-ZMAP(IEL,7)))+
     -    (-(((-1+W)*XMAP(IEL,1)+XMAP(IEL,3)-W*XMAP(IEL,3)-
     -    (1+W)*(XMAP(IEL,5)-XMAP(IEL,7)))*
     -    ((-1+W)*YMAP(IEL,1)+YMAP(IEL,2)-W*YMAP(IEL,2)-
     -    (1+W)*(YMAP(IEL,5)-YMAP(IEL,6))))+
     -    ((-1+W)*XMAP(IEL,1)+XMAP(IEL,2)-W*XMAP(IEL,2)-
     -    (1+W)*(XMAP(IEL,5)-XMAP(IEL,6)))*
     -    ((-1+W)*YMAP(IEL,1)+YMAP(IEL,3)-W*YMAP(IEL,3)-
     -    (1+W)*(YMAP(IEL,5)-YMAP(IEL,7))))*
     -    ((-1+U+V)*ZMAP(IEL,1)-V*ZMAP(IEL,3)+ZMAP(IEL,5)-V*ZMAP(IEL,5)-
     -    U*(ZMAP(IEL,2)+ZMAP(IEL,5)-ZMAP(IEL,6))+V*ZMAP(IEL,7)))/8
*   Terms of the pentahedral Jacobian
       JACP11(U,V,W,IEL)=
     -      (-(((-1+U+V)*YMAP(IEL,1)-V*YMAP(IEL,3)+YMAP(IEL,5)-
     -      V*YMAP(IEL,5)-U*(YMAP(IEL,2)+YMAP(IEL,5)-YMAP(IEL,6))+
     -      V*YMAP(IEL,7))*((-1+W)*ZMAP(IEL,1)+ZMAP(IEL,3)-
     -      W*ZMAP(IEL,3)-(1+W)*(ZMAP(IEL,5)-ZMAP(IEL,7))))+
     -      ((-1+W)*YMAP(IEL,1)+YMAP(IEL,3)-W*YMAP(IEL,3)-
     -      (1+W)*(YMAP(IEL,5)-YMAP(IEL,7)))*
     -      ((-1+U+V)*ZMAP(IEL,1)-V*ZMAP(IEL,3)+ZMAP(IEL,5)-
     -      V*ZMAP(IEL,5)-
     -      U*(ZMAP(IEL,2)+ZMAP(IEL,5)-ZMAP(IEL,6))+V*ZMAP(IEL,7)))/4
       JACP12(U,V,W,IEL)=
     -      (((-1+U+V)*XMAP(IEL,1)-V*XMAP(IEL,3)+XMAP(IEL,5)-
     -      V*XMAP(IEL,5)-U*(XMAP(IEL,2)+XMAP(IEL,5)-XMAP(IEL,6))+
     -      V*XMAP(IEL,7))*((-1+W)*ZMAP(IEL,1)+ZMAP(IEL,3)-
     -      W*ZMAP(IEL,3)-(1+W)*(ZMAP(IEL,5)-ZMAP(IEL,7)))-
     -      ((-1+W)*XMAP(IEL,1)+XMAP(IEL,3)-W*XMAP(IEL,3)-
     -      (1+W)*(XMAP(IEL,5)-XMAP(IEL,7)))*
     -      ((-1+U+V)*ZMAP(IEL,1)-V*ZMAP(IEL,3)+ZMAP(IEL,5)-
     -      V*ZMAP(IEL,5)-
     -      U*(ZMAP(IEL,2)+ZMAP(IEL,5)-ZMAP(IEL,6))+V*ZMAP(IEL,7)))/4
       JACP13(U,V,W,IEL)=
     -      (-(((-1+U+V)*XMAP(IEL,1)-V*XMAP(IEL,3)+
     -      XMAP(IEL,5)-V*XMAP(IEL,5)-U*(XMAP(IEL,2)+XMAP(IEL,5)-
     -      XMAP(IEL,6))+V*XMAP(IEL,7))*
     -      ((-1+W)*YMAP(IEL,1)+YMAP(IEL,3)-W*YMAP(IEL,3)-
     -      (1+W)*(YMAP(IEL,5)-YMAP(IEL,7))))+
     -      ((-1+W)*XMAP(IEL,1)+XMAP(IEL,3)-W*XMAP(IEL,3)-
     -      (1+W)*(XMAP(IEL,5)-XMAP(IEL,7)))*
     -      ((-1+U+V)*YMAP(IEL,1)-V*YMAP(IEL,3)+YMAP(IEL,5)-
     -      V*YMAP(IEL,5)-
     -      U*(YMAP(IEL,2)+YMAP(IEL,5)-YMAP(IEL,6))+V*YMAP(IEL,7)))/4
       JACP21(U,V,W,IEL)=
     -      (((-1+U+V)*YMAP(IEL,1)-V*YMAP(IEL,3)+YMAP(IEL,5)-
     -      V*YMAP(IEL,5)-U*(YMAP(IEL,2)+YMAP(IEL,5)-YMAP(IEL,6))+
     -      V*YMAP(IEL,7))*
     -      ((-1+W)*ZMAP(IEL,1)+ZMAP(IEL,2)-W*ZMAP(IEL,2)-
     -      (1+W)*(ZMAP(IEL,5)-ZMAP(IEL,6)))-
     -      ((-1+W)*YMAP(IEL,1)+YMAP(IEL,2)-W*YMAP(IEL,2)-
     -      (1+W)*(YMAP(IEL,5)-YMAP(IEL,6)))*
     -      ((-1+U+V)*ZMAP(IEL,1)-V*ZMAP(IEL,3)+ZMAP(IEL,5)-
     -      V*ZMAP(IEL,5)-
     -      U*(ZMAP(IEL,2)+ZMAP(IEL,5)-ZMAP(IEL,6))+V*ZMAP(IEL,7)))/4
       JACP22(U,V,W,IEL)=
     -      (-(((-1+U+V)*XMAP(IEL,1)-V*XMAP(IEL,3)+
     -      XMAP(IEL,5)-V*XMAP(IEL,5)-U*(XMAP(IEL,2)+XMAP(IEL,5)-
     -      XMAP(IEL,6))+V*XMAP(IEL,7))*
     -      ((-1+W)*ZMAP(IEL,1)+ZMAP(IEL,2)-W*ZMAP(IEL,2)-
     -      (1+W)*(ZMAP(IEL,5)-ZMAP(IEL,6))))+
     -      ((-1+W)*XMAP(IEL,1)+XMAP(IEL,2)-W*XMAP(IEL,2)-
     -      (1+W)*(XMAP(IEL,5)-XMAP(IEL,6)))*
     -      ((-1+U+V)*ZMAP(IEL,1)-V*ZMAP(IEL,3)+ZMAP(IEL,5)-
     -      V*ZMAP(IEL,5)-
     -      U*(ZMAP(IEL,2)+ZMAP(IEL,5)-ZMAP(IEL,6))+V*ZMAP(IEL,7)))/4
       JACP23(U,V,W,IEL)=
     -      (((-1+U+V)*XMAP(IEL,1)-V*XMAP(IEL,3)+XMAP(IEL,5)-
     -      V*XMAP(IEL,5)-U*(XMAP(IEL,2)+XMAP(IEL,5)-XMAP(IEL,6))+
     -      V*XMAP(IEL,7))*
     -      ((-1+W)*YMAP(IEL,1)+YMAP(IEL,2)-W*YMAP(IEL,2)-
     -      (1+W)*(YMAP(IEL,5)-YMAP(IEL,6)))-
     -      ((-1+W)*XMAP(IEL,1)+XMAP(IEL,2)-W*XMAP(IEL,2)-
     -      (1+W)*(XMAP(IEL,5)-XMAP(IEL,6)))*
     -      ((-1+U+V)*YMAP(IEL,1)-V*YMAP(IEL,3)+YMAP(IEL,5)-
     -      V*YMAP(IEL,5)-
     -      U*(YMAP(IEL,2)+YMAP(IEL,5)-YMAP(IEL,6))+V*YMAP(IEL,7)))/4
       JACP31(U,V,W,IEL)=
     -      (-(((-1+W)*YMAP(IEL,1)+YMAP(IEL,3)-W*YMAP(IEL,3)-
     -      (1+W)*(YMAP(IEL,5)-YMAP(IEL,7)))*
     -      ((-1+W)*ZMAP(IEL,1)+ZMAP(IEL,2)-W*ZMAP(IEL,2)-
     -      (1+W)*(ZMAP(IEL,5)-ZMAP(IEL,6))))+
     -      ((-1+W)*YMAP(IEL,1)+YMAP(IEL,2)-W*YMAP(IEL,2)-
     -      (1+W)*(YMAP(IEL,5)-YMAP(IEL,6)))*
     -      ((-1+W)*ZMAP(IEL,1)+ZMAP(IEL,3)-W*ZMAP(IEL,3)-
     -      (1+W)*(ZMAP(IEL,5)-ZMAP(IEL,7))))/4
       JACP32(U,V,W,IEL)=
     -      (((-1+W)*XMAP(IEL,1)+XMAP(IEL,3)-W*XMAP(IEL,3)-
     -      (1+W)*(XMAP(IEL,5)-XMAP(IEL,7)))*
     -      ((-1+W)*ZMAP(IEL,1)+ZMAP(IEL,2)-W*ZMAP(IEL,2)-
     -      (1+W)*(ZMAP(IEL,5)-ZMAP(IEL,6)))-
     -      ((-1+W)*XMAP(IEL,1)+XMAP(IEL,2)-W*XMAP(IEL,2)-
     -      (1+W)*(XMAP(IEL,5)-XMAP(IEL,6)))*
     -      ((-1+W)*ZMAP(IEL,1)+ZMAP(IEL,3)-W*ZMAP(IEL,3)-
     -      (1+W)*(ZMAP(IEL,5)-ZMAP(IEL,7))))/4
       JACP33(U,V,W,IEL)=
     -      (-(((-1+W)*XMAP(IEL,1)+XMAP(IEL,3)-W*XMAP(IEL,3)-
     -      (1+W)*(XMAP(IEL,5)-XMAP(IEL,7)))*
     -      ((-1+W)*YMAP(IEL,1)+YMAP(IEL,2)-W*YMAP(IEL,2)-
     -      (1+W)*(YMAP(IEL,5)-YMAP(IEL,6))))+
     -      ((-1+W)*XMAP(IEL,1)+XMAP(IEL,2)-W*XMAP(IEL,2)-
     -      (1+W)*(XMAP(IEL,5)-XMAP(IEL,6)))*
     -      ((-1+W)*YMAP(IEL,1)+YMAP(IEL,3)-W*YMAP(IEL,3)-
     -      (1+W)*(YMAP(IEL,5)-YMAP(IEL,7))))/4
*** Node numbering scheme for non-degenerate hexahedrals.
       DATA (NODES(1,J),J=1,3) / 1, 5, 4/
       DATA (NODES(2,J),J=1,3) / 2, 3, 6/
       DATA (NODES(3,J),J=1,3) / 1, 2, 5/
       DATA (NODES(4,J),J=1,3) / 4, 8, 3/
       DATA (NODES(5,J),J=1,3) / 1, 4, 2/
       DATA (NODES(6,J),J=1,3) / 5, 6, 8/
*** Last known node.
       DATA IL/0/
*** Initial values.
       T1=0
       T2=0
       T3=0
       T4=0
       IMAP=0
*** Tolerance for a point being inside the curved element.
       F=0.2
*** Verify the count of volumes that contain the point.
       NFOUND=0
*** Last volume out of range ?
       IF(IL.LT.1.OR.IL.GT.NMAP)GOTO 1000
*** Check the last volume, straight triangles ...
       IF((MAPTYP.EQ.1.OR.MAPTYP.EQ.2).AND..NOT.LMAPCH)THEN
            TT1=(X-XMAP(IL,2))*(YMAP(IL,3)-YMAP(IL,2))-
     -           (Y-YMAP(IL,2))*(XMAP(IL,3)-XMAP(IL,2))
            TT2=(X-XMAP(IL,3))*(YMAP(IL,1)-YMAP(IL,3))-
     -           (Y-YMAP(IL,3))*(XMAP(IL,1)-XMAP(IL,3))
            TT3=(X-XMAP(IL,1))*(YMAP(IL,2)-YMAP(IL,1))-
     -           (Y-YMAP(IL,1))*(XMAP(IL,2)-XMAP(IL,1))
            IF((TT1.GE.0.AND.TT2.GE.0.AND.TT3.GE.0).OR.
     -           (TT1.LE.0.AND.TT2.LE.0.AND.TT3.LE.0))THEN
                 T1=TT1/
     -               ((XMAP(IL,1)-XMAP(IL,2))*(YMAP(IL,3)-YMAP(IL,2))-
     -                (XMAP(IL,3)-XMAP(IL,2))*(YMAP(IL,1)-YMAP(IL,2)))
                 T2=TT2/
     -               ((XMAP(IL,2)-XMAP(IL,3))*(YMAP(IL,1)-YMAP(IL,3))-
     -                (XMAP(IL,1)-XMAP(IL,3))*(YMAP(IL,2)-YMAP(IL,3)))
                 T3=TT3/
     -               ((XMAP(IL,3)-XMAP(IL,1))*(YMAP(IL,2)-YMAP(IL,1))-
     -                (XMAP(IL,2)-XMAP(IL,1))*(YMAP(IL,3)-YMAP(IL,1)))
                 T4=0
                 IMAP=IL
                 RETURN
            ENDIF
**  curved quadratic triangles
       ELSEIF((MAPTYP.EQ.3.AND..NOT.LMAPCH).OR.
     -        (MAPTYP.EQ.5.AND.ELMDGN(IL).AND..NOT.LMAPCH))THEN
            CALL MAPC3(X,Y,Z,T1,T2,T3,T4,JAC,DET,IL,IFAIL)
            IF(IFAIL.EQ.0.AND.
     -           T1.GE.0.AND.T2.GE.0.AND.T3.GE.0.AND.
     -           T1.LE.1.AND.T2.LE.1.AND.T3.LE.1)THEN
                 IMAP=IL
                 RETURN
            ENDIF
**  linear quadrilaterals.
       ELSEIF(MAPTYP.EQ.4.AND..NOT.LMAPCH)THEN
            CALL MAPC4(X,Y,Z,T1,T2,T3,T4,JAC,DET,IL,IFAIL)
            IF(IFAIL.EQ.0.AND.
     -           T1.GE.-1.AND.T2.GE.-1.AND.T1.LE.+1.AND.T2.LE.+1)THEN
                 IMAP=IL
                 RETURN
            ENDIF
**  quadratic, curved "serendipity" quadrilaterals.
       ELSEIF(MAPTYP.EQ.5.AND..NOT.LMAPCH)THEN
            CALL MAPC5(X,Y,Z,T1,T2,T3,T4,JAC,DET,IL,IFAIL)
            IF(IFAIL.EQ.0.AND.
     -           T1.GE.-1.AND.T2.GE.-1.AND.T1.LE.+1.AND.T2.LE.+1)THEN
                 IMAP=IL
                 RETURN
            ENDIF
**  linear tetrahedra.
       ELSEIF((MAPTYP.EQ.11.OR.MAPTYP.EQ.12).AND..NOT.LMAPCH)THEN
            CALL MAPC12(X,Y,Z,T1,T2,T3,T4,JAC,DET,IL,IFAIL)
            IF(IFAIL.EQ.0.AND.
     -           ((T1.GE.0.AND.T2.GE.0.AND.T3.GE.0.AND.T4.GE.0).OR.
     -            (T1.LE.0.AND.T2.LE.0.AND.T3.LE.0.AND.T4.LE.0)))THEN
                 IMAP=IL
                 RETURN
            ENDIF
**  Quadratic tetrahedra.
       ELSEIF(MAPTYP.EQ.13.AND..NOT.LMAPCH)THEN
            XMIN=MIN(XMAP(IL,1),XMAP(IL,2),XMAP(IL,3),XMAP(IL,4))
            XMAX=MAX(XMAP(IL,1),XMAP(IL,2),XMAP(IL,3),XMAP(IL,4))
            YMIN=MIN(YMAP(IL,1),YMAP(IL,2),YMAP(IL,3),YMAP(IL,4))
            YMAX=MAX(YMAP(IL,1),YMAP(IL,2),YMAP(IL,3),YMAP(IL,4))
            ZMIN=MIN(ZMAP(IL,1),ZMAP(IL,2),ZMAP(IL,3),ZMAP(IL,4))
            ZMAX=MAX(ZMAP(IL,1),ZMAP(IL,2),ZMAP(IL,3),ZMAP(IL,4))
            IF(X.GE.XMIN-F*(XMAX-XMIN).AND.X.LE.XMAX+F*(XMAX-XMIN).AND.
     -         Y.GE.YMIN-F*(YMAX-YMIN).AND.Y.LE.YMAX+F*(YMAX-YMIN).AND.
     -         Z.GE.ZMIN-F*(ZMAX-ZMIN).AND.Z.LE.ZMAX+F*(ZMAX-ZMIN))THEN
                 CALL MAPC13(X,Y,Z,T1,T2,T3,T4,JAC,DET,IL,IFAIL)
                 IF(IFAIL.EQ.0.AND.
     -                ((T1.GE.0.AND.T2.GE.0.AND.
     -                  T3.GE.0.AND.T4.GE.0).OR.
     -                 (T1.LE.0.AND.T2.LE.0.AND.
     -                  T3.LE.0.AND.T4.LE.0)))THEN
                      IMAP=IL
                      RETURN
                 ENDIF
            ENDIF
**  General (non-)degenerate hexahedrons.
       ELSEIF((MAPTYP.EQ.14.OR.MAPTYP.EQ.15.OR.MAPTYP.EQ.16).AND.
     -      .NOT.LMAPCH.AND.
     -      X.GE.MIN(XMAP(IL,1),XMAP(IL,2),XMAP(IL,3),XMAP(IL,4),
     -               XMAP(IL,5),XMAP(IL,6),XMAP(IL,7),XMAP(IL,8)).AND.
     -      X.LE.MAX(XMAP(IL,1),XMAP(IL,2),XMAP(IL,3),XMAP(IL,4),
     -               XMAP(IL,5),XMAP(IL,6),XMAP(IL,7),XMAP(IL,8)).AND.
     -      Y.GE.MIN(YMAP(IL,1),YMAP(IL,2),YMAP(IL,3),YMAP(IL,4),
     -               YMAP(IL,5),YMAP(IL,6),YMAP(IL,7),YMAP(IL,8)).AND.
     -      Y.LE.MAX(YMAP(IL,1),YMAP(IL,2),YMAP(IL,3),YMAP(IL,4),
     -               YMAP(IL,5),YMAP(IL,6),YMAP(IL,7),YMAP(IL,8)).AND.
     -      Z.GE.MIN(ZMAP(IL,1),ZMAP(IL,2),ZMAP(IL,3),ZMAP(IL,4),
     -               ZMAP(IL,5),ZMAP(IL,6),ZMAP(IL,7),ZMAP(IL,8)).AND.
     -      Z.LE.MAX(ZMAP(IL,1),ZMAP(IL,2),ZMAP(IL,3),ZMAP(IL,4),
     -               ZMAP(IL,5),ZMAP(IL,6),ZMAP(IL,7),ZMAP(IL,8)))THEN
*   Determine the distances to each of the side planes.
            IF(.NOT.ELMDGN(IL))THEN
                 DO 200 J=1,6
                 VECX=(YMAP(IL,NODES(J,2))-YMAP(IL,NODES(J,1)))*
     -                (ZMAP(IL,NODES(J,3))-ZMAP(IL,NODES(J,1)))-
     -                (YMAP(IL,NODES(J,3))-YMAP(IL,NODES(J,1)))*
     -                (ZMAP(IL,NODES(J,2))-ZMAP(IL,NODES(J,1)))
                 VECY=(ZMAP(IL,NODES(J,2))-ZMAP(IL,NODES(J,1)))*
     -                (XMAP(IL,NODES(J,3))-XMAP(IL,NODES(J,1)))-
     -                (ZMAP(IL,NODES(J,3))-ZMAP(IL,NODES(J,1)))*
     -                (XMAP(IL,NODES(J,2))-XMAP(IL,NODES(J,1)))
                 VECZ=(XMAP(IL,NODES(J,2))-XMAP(IL,NODES(J,1)))*
     -                (YMAP(IL,NODES(J,3))-YMAP(IL,NODES(J,1)))-
     -                (XMAP(IL,NODES(J,3))-XMAP(IL,NODES(J,1)))*
     -                (YMAP(IL,NODES(J,2))-YMAP(IL,NODES(J,1)))
                 VNORM=VECX**2+VECY**2+VECZ**2
                 IF(VNORM.GT.0)THEN
                      VNORM=SQRT(VNORM)
                      VECX=VECX/VNORM
                      VECY=VECY/VNORM
                      VECZ=VECZ/VNORM
                      DIST(J)=VECX*(X-XMAP(IL,NODES(J,1)))+
     -                     VECY*(Y-YMAP(IL,NODES(J,1)))+
     -                     VECZ*(Z-ZMAP(IL,NODES(J,1)))
                 ELSE
                      PRINT *,' !!!!!! MAPIND WARNING : Normal vector'//
     -                     ' with zero norm found in element ',il,
     -                     ' ; element ignored.'
                 ENDIF
200              CONTINUE
*   Initialise coordinate iteration with the distance ratio.
                 IF(DIST(1)+DIST(2).GT.0.AND.
     -                DIST(3)+DIST(4).GT.0.AND.
     -                DIST(5)+DIST(6).GT.0)THEN
                      TD1=(DIST(1)-DIST(2))/(DIST(1)+DIST(2))
                      TD2=(DIST(3)-DIST(4))/(DIST(3)+DIST(4))
                      TD3=(DIST(5)-DIST(6))/(DIST(5)+DIST(6))
                 ELSE
                      PRINT *,' !!!!!! MAPIND WARNING : Point'//
     -                     ' located on an intersection; no'//
     -                     ' initial coordinate estimate.'
                      TD1=0
                      TD2=0
                      TD3=0
                 ENDIF
*   For degenerate elements, initialise at the centre.
            ELSE
                 TD1=0.5
                 TD2=0.5
                 TD3=0
            ENDIF
*   Iterate to refine the estimate.
            DO 210 ITER=1,10
*   Re-compute the (x,y,z) position for this coordinate.
            IF(.NOT.ELMDGN(IL))THEN
                 XR=  XMAP(IL,1)*(1 - TD1)*(1 - TD2)*(1 - TD3)/8 +
     -                XMAP(IL,2)*(1 + TD1)*(1 - TD2)*(1 - TD3)/8 +
     -                XMAP(IL,3)*(1 + TD1)*(1 + TD2)*(1 - TD3)/8 +
     -                XMAP(IL,4)*(1 - TD1)*(1 + TD2)*(1 - TD3)/8 +
     -                XMAP(IL,5)*(1 - TD1)*(1 - TD2)*(1 + TD3)/8 +
     -                XMAP(IL,6)*(1 + TD1)*(1 - TD2)*(1 + TD3)/8 +
     -                XMAP(IL,7)*(1 + TD1)*(1 + TD2)*(1 + TD3)/8 +
     -                XMAP(IL,8)*(1 - TD1)*(1 + TD2)*(1 + TD3)/8
                 YR=  YMAP(IL,1)*(1 - TD1)*(1 - TD2)*(1 - TD3)/8 +
     -                YMAP(IL,2)*(1 + TD1)*(1 - TD2)*(1 - TD3)/8 +
     -                YMAP(IL,3)*(1 + TD1)*(1 + TD2)*(1 - TD3)/8 +
     -                YMAP(IL,4)*(1 - TD1)*(1 + TD2)*(1 - TD3)/8 +
     -                YMAP(IL,5)*(1 - TD1)*(1 - TD2)*(1 + TD3)/8 +
     -                YMAP(IL,6)*(1 + TD1)*(1 - TD2)*(1 + TD3)/8 +
     -                YMAP(IL,7)*(1 + TD1)*(1 + TD2)*(1 + TD3)/8 +
     -                YMAP(IL,8)*(1 - TD1)*(1 + TD2)*(1 + TD3)/8
                 ZR=  ZMAP(IL,1)*(1 - TD1)*(1 - TD2)*(1 - TD3)/8 +
     -                ZMAP(IL,2)*(1 + TD1)*(1 - TD2)*(1 - TD3)/8 +
     -                ZMAP(IL,3)*(1 + TD1)*(1 + TD2)*(1 - TD3)/8 +
     -                ZMAP(IL,4)*(1 - TD1)*(1 + TD2)*(1 - TD3)/8 +
     -                ZMAP(IL,5)*(1 - TD1)*(1 - TD2)*(1 + TD3)/8 +
     -                ZMAP(IL,6)*(1 + TD1)*(1 - TD2)*(1 + TD3)/8 +
     -                ZMAP(IL,7)*(1 + TD1)*(1 + TD2)*(1 + TD3)/8 +
     -                ZMAP(IL,8)*(1 - TD1)*(1 + TD2)*(1 + TD3)/8
*   Store the Jacobian
                 JAC(1,1)=JACH11(TD1,TD2,TD3,IL)
                 JAC(1,2)=JACH12(TD1,TD2,TD3,IL)
                 JAC(1,3)=JACH13(TD1,TD2,TD3,IL)
                 JAC(2,1)=JACH21(TD1,TD2,TD3,IL)
                 JAC(2,2)=JACH22(TD1,TD2,TD3,IL)
                 JAC(2,3)=JACH23(TD1,TD2,TD3,IL)
                 JAC(3,1)=JACH31(TD1,TD2,TD3,IL)
                 JAC(3,2)=JACH32(TD1,TD2,TD3,IL)
                 JAC(3,3)=JACH33(TD1,TD2,TD3,IL)
                 DET=DETH(TD1,TD2,TD3,IL)
            ELSE
*   Compute the (x,y,z) position for this coordinate.
                 XR=  XMAP(IL,1)*(1-TD1-TD2)*(1-TD3)/2 +
     -                XMAP(IL,2)*TD1        *(1-TD3)/2 +
     -                XMAP(IL,3)*TD2        *(1-TD3)/2 +
     -                XMAP(IL,5)*(1-TD1-TD2)*(1+TD3)/2 +
     -                XMAP(IL,6)*TD1        *(1+TD3)/2 +
     -                XMAP(IL,7)*TD2        *(1+TD3)/2
                 YR=  YMAP(IL,1)*(1-TD1-TD2)*(1-TD3)/2 +
     -                YMAP(IL,2)*TD1        *(1-TD3)/2 +
     -                YMAP(IL,3)*TD2        *(1-TD3)/2 +
     -                YMAP(IL,5)*(1-TD1-TD2)*(1+TD3)/2 +
     -                YMAP(IL,6)*TD1        *(1+TD3)/2 +
     -                YMAP(IL,7)*TD2        *(1+TD3)/2
                 ZR=  ZMAP(IL,1)*(1-TD1-TD2)*(1-TD3)/2 +
     -                ZMAP(IL,2)*TD1        *(1-TD3)/2 +
     -                ZMAP(IL,3)*TD2        *(1-TD3)/2 +
     -                ZMAP(IL,5)*(1-TD1-TD2)*(1+TD3)/2 +
     -                ZMAP(IL,6)*TD1        *(1+TD3)/2 +
     -                ZMAP(IL,7)*TD2        *(1+TD3)/2
*   Store the Jacobian
                 JAC(1,1)=JACP11(TD1,TD2,TD3,IL)
                 JAC(1,2)=JACP12(TD1,TD2,TD3,IL)
                 JAC(1,3)=JACP13(TD1,TD2,TD3,IL)
                 JAC(2,1)=JACP21(TD1,TD2,TD3,IL)
                 JAC(2,2)=JACP22(TD1,TD2,TD3,IL)
                 JAC(2,3)=JACP23(TD1,TD2,TD3,IL)
                 JAC(3,1)=JACP31(TD1,TD2,TD3,IL)
                 JAC(3,2)=JACP32(TD1,TD2,TD3,IL)
                 JAC(3,3)=JACP33(TD1,TD2,TD3,IL)
                 DET=DETP(TD1,TD2,TD3,IL)
            ENDIF
*   Compute the difference vector
            DIFF(1)=XR-X
            DIFF(2)=YR-Y
            DIFF(3)=ZR-Z
*   Update the estimate
            DO 220 L=1,3
            CORR(L)=0
            DO 230 K=1,3
            CORR(L)=CORR(L)+JAC(L,K)*DIFF(K)/DET
230         CONTINUE
220         CONTINUE
            TD1=TD1-CORR(1)
            TD2=TD2-CORR(2)
            TD3=TD3-CORR(3)
            IF(MAX(ABS(CORR(1)),ABS(CORR(2)),ABS(CORR(3))).LT.
     -           1E-6)GOTO 240
210         CONTINUE
            PRINT *,' !!!!!! MATIND WARNING : No convergence'//
     -           ' achieved when refining the isoparamatric'//
     -           ' coordinates in element ',IL
240         CONTINUE
*   Return the element with its coordinates if inside the element.
            IF((.NOT.ELMDGN(IL).AND.
     -           TD1.GE.-1.AND.TD1.LE.+1.AND.
     -           TD2.GE.-1.AND.TD2.LE.+1.AND.
     -           TD3.GE.-1.AND.TD3.LE.+1).OR.
     -           (ELMDGN(IL).AND.
     -           TD1.GE.0.AND.TD1.LE.+1.AND.
     -           TD2.GE.0.AND.TD2.LE.+1.AND.
     -           TD3.GE.-1.AND.TD3.LE.+1))THEN
                 T1=REAL(TD1)
                 T2=REAL(TD2)
                 T3=REAL(TD3)
                 T4=0
                 IMAP=IL
                 RETURN
            ENDIF
       ENDIF
*** Loop over the volumes, triangles.
1000   CONTINUE
       IF(MAPTYP.EQ.1.OR.MAPTYP.EQ.2.OR.MAPTYP.EQ.3)THEN
            DO 20 I=1,NMAP
            IF(X.LT.MIN(XMAP(I,1),XMAP(I,2),XMAP(I,3)).OR.
     -           X.GT.MAX(XMAP(I,1),XMAP(I,2),XMAP(I,3)).OR.
     -           Y.LT.MIN(YMAP(I,1),YMAP(I,2),YMAP(I,3)).OR.
     -           Y.GT.MAX(YMAP(I,1),YMAP(I,2),YMAP(I,3)))GOTO 20
            TT1=(X-XMAP(I,2))*(YMAP(I,3)-YMAP(I,2))-
     -           (Y-YMAP(I,2))*(XMAP(I,3)-XMAP(I,2))
            TT2=(X-XMAP(I,3))*(YMAP(I,1)-YMAP(I,3))-
     -           (Y-YMAP(I,3))*(XMAP(I,1)-XMAP(I,3))
            TT3=(X-XMAP(I,1))*(YMAP(I,2)-YMAP(I,1))-
     -           (Y-YMAP(I,1))*(XMAP(I,2)-XMAP(I,1))
            IF((TT1.GE.0.AND.TT2.GE.0.AND.TT3.GE.0).OR.
     -           (TT1.LE.0.AND.TT2.LE.0.AND.TT3.LE.0))THEN
                 T1=TT1/((XMAP(I,1)-XMAP(I,2))*(YMAP(I,3)-YMAP(I,2))-
     -                (XMAP(I,3)-XMAP(I,2))*(YMAP(I,1)-YMAP(I,2)))
                 T2=TT2/((XMAP(I,2)-XMAP(I,3))*(YMAP(I,1)-YMAP(I,3))-
     -                (XMAP(I,1)-XMAP(I,3))*(YMAP(I,2)-YMAP(I,3)))
                 T3=TT3/((XMAP(I,3)-XMAP(I,1))*(YMAP(I,2)-YMAP(I,1))-
     -                (XMAP(I,2)-XMAP(I,1))*(YMAP(I,3)-YMAP(I,1)))
                 T4=0
                 IMAP=I
                 IL=I
                 NFOUND=NFOUND+1
                 IF(NFOUND.LE.MXFOUN)IFOUND(NFOUND)=IMAP
                 IF(.NOT.LMAPCH)RETURN
                 DO 1001 K=1,4
                 DO 1002 L=1,4
                 JACBAK(K,L)=JAC(K,L)
1002             CONTINUE
1001             CONTINUE
                 DETBAK=DET
                 T1BAK=T1
                 T2BAK=T2
                 T3BAK=T3
                 T4BAK=T4
                 IMPBAK=IMAP
            ENDIF
20          CONTINUE
**  linear quadrilaterals.
       ELSEIF(MAPTYP.EQ.4)THEN
            DO 50 I=1,NMAP
            CALL MAPC4(X,Y,Z,T1,T2,T3,T4,JAC,DET,I,IFAIL)
            IF(IFAIL.EQ.0.AND.
     -           T1.GE.-1.AND.T2.GE.-1.AND.T1.LE.+1.AND.T2.LE.+1)THEN
                 IMAP=I
                 IL=I
                 NFOUND=NFOUND+1
                 IF(NFOUND.LE.MXFOUN)IFOUND(NFOUND)=IMAP
                 IF(.NOT.LMAPCH)RETURN
                 DO 1003 K=1,4
                 DO 1004 L=1,4
                 JACBAK(K,L)=JAC(K,L)
1004             CONTINUE
1003             CONTINUE
                 DETBAK=DET
                 T1BAK=T1
                 T2BAK=T2
                 T3BAK=T3
                 T4BAK=T4
                 IMPBAK=IMAP
            ENDIF
50          CONTINUE
**  quadratic, curved "serendipity" quadrilaterals.
       ELSEIF(MAPTYP.EQ.5)THEN
            DO 60 I=1,NMAP
            IF(ELMDGN(I))THEN
                 CALL MAPC3(X,Y,Z,T1,T2,T3,T4,JAC,DET,I,IFAIL)
                 IF(IFAIL.EQ.0.AND.
     -                T1.GE.0.AND.T2.GE.0.AND.T3.GE.0.AND.
     -                T1.LE.1.AND.T2.LE.1.AND.T3.LE.1)THEN
                      IMAP=I
                      IL=I
                      NFOUND=NFOUND+1
                      IF(NFOUND.LE.MXFOUN)IFOUND(NFOUND)=IMAP
                      IF(.NOT.LMAPCH)RETURN
                      DO 1005 K=1,4
                      DO 1006 L=1,4
                      JACBAK(K,L)=JAC(K,L)
1006                  CONTINUE
1005                  CONTINUE
                      DETBAK=DET
                      T1BAK=T1
                      T2BAK=T2
                      T3BAK=T3
                      T4BAK=T4
                      IMPBAK=IMAP
                 ENDIF
            ELSE
                 CALL MAPC5(X,Y,Z,T1,T2,T3,T4,JAC,DET,I,IFAIL)
                 IF(IFAIL.EQ.0.AND.T1.GE.-1.AND.T2.GE.-1.AND.
     -                T1.LE.+1.AND.T2.LE.+1)THEN
                      IMAP=I
                      IL=I
                      NFOUND=NFOUND+1
                      IF(NFOUND.LE.MXFOUN)IFOUND(NFOUND)=IMAP
                      IF(.NOT.LMAPCH)RETURN
                      DO 1007 K=1,4
                      DO 1008 L=1,4
                      JACBAK(K,L)=JAC(K,L)
1008                  CONTINUE
1007                  CONTINUE
                      DETBAK=DET
                      T1BAK=T1
                      T2BAK=T2
                      T3BAK=T3
                      T4BAK=T4
                      IMPBAK=IMAP
                 ENDIF
            ENDIF
60          CONTINUE
**  linear tetrahedra
       ELSEIF(MAPTYP.EQ.11.OR.MAPTYP.EQ.12)THEN
            DO 10 I=1,NMAP
            IF(X.LT.MIN(XMAP(I,1),XMAP(I,2),XMAP(I,3),XMAP(I,4)).OR.
     -           X.GT.MAX(XMAP(I,1),XMAP(I,2),XMAP(I,3),XMAP(I,4)).OR.
     -           Y.LT.MIN(YMAP(I,1),YMAP(I,2),YMAP(I,3),YMAP(I,4)).OR.
     -           Y.GT.MAX(YMAP(I,1),YMAP(I,2),YMAP(I,3),YMAP(I,4)).OR.
     -           Z.LT.MIN(ZMAP(I,1),ZMAP(I,2),ZMAP(I,3),ZMAP(I,4)).OR.
     -           Z.GT.MAX(ZMAP(I,1),ZMAP(I,2),ZMAP(I,3),ZMAP(I,4)))
     -           GOTO 10
            CALL MAPC12(X,Y,Z,T1,T2,T3,T4,JAC,DET,I,IFAIL)
            IF(IFAIL.EQ.0.AND.
     -           ((T1.GE.0.AND.T2.GE.0.AND.T3.GE.0.AND.T4.GE.0).OR.
     -            (T1.LE.0.AND.T2.LE.0.AND.T3.LE.0.AND.T4.LE.0)))THEN
                 IMAP=I
                 IL=I
                 NFOUND=NFOUND+1
                 IF(NFOUND.LE.MXFOUN)IFOUND(NFOUND)=IMAP
                 IF(.NOT.LMAPCH)RETURN
                 DO 1009 K=1,4
                 DO 1010 L=1,4
                 JACBAK(K,L)=JAC(K,L)
1010             CONTINUE
1009             CONTINUE
                 DETBAK=DET
                 T1BAK=T1
                 T2BAK=T2
                 T3BAK=T3
                 T4BAK=T4
                 IMPBAK=IMAP
            ENDIF
10          CONTINUE
**  quadratic tetrahedra.
       ELSEIF(MAPTYP.EQ.13)THEN
            DO 11 I=1,NMAP
            XMIN=MIN(XMAP(I,1),XMAP(I,2),XMAP(I,3),XMAP(I,4))
            XMAX=MAX(XMAP(I,1),XMAP(I,2),XMAP(I,3),XMAP(I,4))
            YMIN=MIN(YMAP(I,1),YMAP(I,2),YMAP(I,3),YMAP(I,4))
            YMAX=MAX(YMAP(I,1),YMAP(I,2),YMAP(I,3),YMAP(I,4))
            ZMIN=MIN(ZMAP(I,1),ZMAP(I,2),ZMAP(I,3),ZMAP(I,4))
            ZMAX=MAX(ZMAP(I,1),ZMAP(I,2),ZMAP(I,3),ZMAP(I,4))
            IF(X.LT.XMIN-F*(XMAX-XMIN).OR.X.GT.XMAX+F*(XMAX-XMIN).OR.
     -         Y.LT.YMIN-F*(YMAX-YMIN).OR.Y.GT.YMAX+F*(YMAX-YMIN).OR.
     -         Z.LT.ZMIN-F*(ZMAX-ZMIN).OR.Z.GT.ZMAX+F*(ZMAX-ZMIN))
     -         GOTO 11
            CALL MAPC13(X,Y,Z,T1,T2,T3,T4,JAC,DET,I,IFAIL)
            IF(IFAIL.EQ.0.AND.
     -           ((T1.GE.0.AND.T2.GE.0.AND.T3.GE.0.AND.T4.GE.0).OR.
     -            (T1.LE.0.AND.T2.LE.0.AND.T3.LE.0.AND.T4.LE.0)))THEN
                 IMAP=I
                 IL=I
                 NFOUND=NFOUND+1
                 IF(NFOUND.LE.MXFOUN)IFOUND(NFOUND)=IMAP
                 IF(.NOT.LMAPCH)RETURN
                 DO 1011 K=1,4
                 DO 1012 L=1,4
                 JACBAK(K,L)=JAC(K,L)
1012             CONTINUE
1011             CONTINUE
                 DETBAK=DET
                 T1BAK=T1
                 T2BAK=T2
                 T3BAK=T3
                 T4BAK=T4
                 IMPBAK=IMAP
            ENDIF
11          CONTINUE
**  General (non-)degenerate hexahedrons.
       ELSEIF(MAPTYP.EQ.14.OR.MAPTYP.EQ.15.OR.MAPTYP.EQ.16)THEN
            DO 30 I=1,NMAP
*   Verify that the element is roughly in the correct area
            IF(  X.LT.MIN(XMAP(I,1),XMAP(I,2),XMAP(I,3),XMAP(I,4),
     -                    XMAP(I,5),XMAP(I,6),XMAP(I,7),XMAP(I,8)).OR.
     -           X.GT.MAX(XMAP(I,1),XMAP(I,2),XMAP(I,3),XMAP(I,4),
     -                    XMAP(I,5),XMAP(I,6),XMAP(I,7),XMAP(I,8)).OR.
     -           Y.LT.MIN(YMAP(I,1),YMAP(I,2),YMAP(I,3),YMAP(I,4),
     -                    YMAP(I,5),YMAP(I,6),YMAP(I,7),YMAP(I,8)).OR.
     -           Y.GT.MAX(YMAP(I,1),YMAP(I,2),YMAP(I,3),YMAP(I,4),
     -                    YMAP(I,5),YMAP(I,6),YMAP(I,7),YMAP(I,8)).OR.
     -           Z.LT.MIN(ZMAP(I,1),ZMAP(I,2),ZMAP(I,3),ZMAP(I,4),
     -                    ZMAP(I,5),ZMAP(I,6),ZMAP(I,7),ZMAP(I,8)).OR.
     -           Z.GT.MAX(ZMAP(I,1),ZMAP(I,2),ZMAP(I,3),ZMAP(I,4),
     -                    ZMAP(I,5),ZMAP(I,6),ZMAP(I,7),ZMAP(I,8)))
     -           GOTO 30
*   For non-degenerate elements, make an initial estimate
            IF(.NOT.ELMDGN(I))THEN
*   Determine the distances to each of the side planes.
                 DO 250 J=1,6
                 VECX=(YMAP(I,NODES(J,2))-YMAP(I,NODES(J,1)))*
     -                (ZMAP(I,NODES(J,3))-ZMAP(I,NODES(J,1)))-
     -                (YMAP(I,NODES(J,3))-YMAP(I,NODES(J,1)))*
     -                (ZMAP(I,NODES(J,2))-ZMAP(I,NODES(J,1)))
                 VECY=(ZMAP(I,NODES(J,2))-ZMAP(I,NODES(J,1)))*
     -                (XMAP(I,NODES(J,3))-XMAP(I,NODES(J,1)))-
     -                (ZMAP(I,NODES(J,3))-ZMAP(I,NODES(J,1)))*
     -                (XMAP(I,NODES(J,2))-XMAP(I,NODES(J,1)))
                 VECZ=(XMAP(I,NODES(J,2))-XMAP(I,NODES(J,1)))*
     -                (YMAP(I,NODES(J,3))-YMAP(I,NODES(J,1)))-
     -                (XMAP(I,NODES(J,3))-XMAP(I,NODES(J,1)))*
     -                (YMAP(I,NODES(J,2))-YMAP(I,NODES(J,1)))
                 VNORM=VECX**2+VECY**2+VECZ**2
                 IF(VNORM.GT.0)THEN
                      VNORM=SQRT(VNORM)
                      VECX=VECX/VNORM
                      VECY=VECY/VNORM
                      VECZ=VECZ/VNORM
                      DIST(J)=VECX*(X-XMAP(I,NODES(J,1)))+
     -                     VECY*(Y-YMAP(I,NODES(J,1)))+
     -                     VECZ*(Z-ZMAP(I,NODES(J,1)))
                 ELSE
                      PRINT *,' !!!!!! MAPIND WARNING : Normal vector'//
     -                     ' with zero norm found in element ',i,
     -                     ' ; element ignored.'
                 ENDIF
250              CONTINUE
*   Initialise coordinate iteration with the distance ratio.
                 IF(DIST(1)+DIST(2).GT.0.AND.
     -                DIST(3)+DIST(4).GT.0.AND.
     -                DIST(5)+DIST(6).GT.0)THEN
                      TD1=(DIST(1)-DIST(2))/(DIST(1)+DIST(2))
                      TD2=(DIST(3)-DIST(4))/(DIST(3)+DIST(4))
                      TD3=(DIST(5)-DIST(6))/(DIST(5)+DIST(6))
                 ELSE
                      PRINT *,' !!!!!! MAPIND WARNING : Point'//
     -                     ' located on an intersection; no'//
     -                     ' initial estimate.'
                      TD1=0
                      TD2=0
                      TD3=0
                 ENDIF
*   For degenerate elements, start with the centre.
            ELSE
                 TD1=0.5
                 TD2=0.5
                 TD3=0
            ENDIF
*   Iterate to refine the estimate.
            DO 260 ITER=1,10
*   Re-compute the (x,y,z) position for this coordinate.
            IF(.NOT.ELMDGN(I))THEN
                 XR=  XMAP(I,1)*(1 - TD1)*(1 - TD2)*(1 - TD3)/8 +
     -                XMAP(I,2)*(1 + TD1)*(1 - TD2)*(1 - TD3)/8 +
     -                XMAP(I,3)*(1 + TD1)*(1 + TD2)*(1 - TD3)/8 +
     -                XMAP(I,4)*(1 - TD1)*(1 + TD2)*(1 - TD3)/8 +
     -                XMAP(I,5)*(1 - TD1)*(1 - TD2)*(1 + TD3)/8 +
     -                XMAP(I,6)*(1 + TD1)*(1 - TD2)*(1 + TD3)/8 +
     -                XMAP(I,7)*(1 + TD1)*(1 + TD2)*(1 + TD3)/8 +
     -                XMAP(I,8)*(1 - TD1)*(1 + TD2)*(1 + TD3)/8
                 YR=  YMAP(I,1)*(1 - TD1)*(1 - TD2)*(1 - TD3)/8 +
     -                YMAP(I,2)*(1 + TD1)*(1 - TD2)*(1 - TD3)/8 +
     -                YMAP(I,3)*(1 + TD1)*(1 + TD2)*(1 - TD3)/8 +
     -                YMAP(I,4)*(1 - TD1)*(1 + TD2)*(1 - TD3)/8 +
     -                YMAP(I,5)*(1 - TD1)*(1 - TD2)*(1 + TD3)/8 +
     -                YMAP(I,6)*(1 + TD1)*(1 - TD2)*(1 + TD3)/8 +
     -                YMAP(I,7)*(1 + TD1)*(1 + TD2)*(1 + TD3)/8 +
     -                YMAP(I,8)*(1 - TD1)*(1 + TD2)*(1 + TD3)/8
                 ZR=  ZMAP(I,1)*(1 - TD1)*(1 - TD2)*(1 - TD3)/8 +
     -                ZMAP(I,2)*(1 + TD1)*(1 - TD2)*(1 - TD3)/8 +
     -                ZMAP(I,3)*(1 + TD1)*(1 + TD2)*(1 - TD3)/8 +
     -                ZMAP(I,4)*(1 - TD1)*(1 + TD2)*(1 - TD3)/8 +
     -                ZMAP(I,5)*(1 - TD1)*(1 - TD2)*(1 + TD3)/8 +
     -                ZMAP(I,6)*(1 + TD1)*(1 - TD2)*(1 + TD3)/8 +
     -                ZMAP(I,7)*(1 + TD1)*(1 + TD2)*(1 + TD3)/8 +
     -                ZMAP(I,8)*(1 - TD1)*(1 + TD2)*(1 + TD3)/8
*   Store the Jacobian
                 JAC(1,1)=JACH11(TD1,TD2,TD3,I)
                 JAC(1,2)=JACH12(TD1,TD2,TD3,I)
                 JAC(1,3)=JACH13(TD1,TD2,TD3,I)
                 JAC(2,1)=JACH21(TD1,TD2,TD3,I)
                 JAC(2,2)=JACH22(TD1,TD2,TD3,I)
                 JAC(2,3)=JACH23(TD1,TD2,TD3,I)
                 JAC(3,1)=JACH31(TD1,TD2,TD3,I)
                 JAC(3,2)=JACH32(TD1,TD2,TD3,I)
                 JAC(3,3)=JACH33(TD1,TD2,TD3,I)
                 DET=DETH(TD1,TD2,TD3,I)
            ELSE
                 XR=  XMAP(I,1)*(1-TD1-TD2)*(1-TD3)/2 +
     -                XMAP(I,2)*TD1        *(1-TD3)/2 +
     -                XMAP(I,3)*TD2        *(1-TD3)/2 +
     -                XMAP(I,5)*(1-TD1-TD2)*(1+TD3)/2 +
     -                XMAP(I,6)*TD1        *(1+TD3)/2 +
     -                XMAP(I,7)*TD2        *(1+TD3)/2
                 YR=  YMAP(I,1)*(1-TD1-TD2)*(1-TD3)/2 +
     -                YMAP(I,2)*TD1        *(1-TD3)/2 +
     -                YMAP(I,3)*TD2        *(1-TD3)/2 +
     -                YMAP(I,5)*(1-TD1-TD2)*(1+TD3)/2 +
     -                YMAP(I,6)*TD1        *(1+TD3)/2 +
     -                YMAP(I,7)*TD2        *(1+TD3)/2
                 ZR=  ZMAP(I,1)*(1-TD1-TD2)*(1-TD3)/2 +
     -                ZMAP(I,2)*TD1        *(1-TD3)/2 +
     -                ZMAP(I,3)*TD2        *(1-TD3)/2 +
     -                ZMAP(I,5)*(1-TD1-TD2)*(1+TD3)/2 +
     -                ZMAP(I,6)*TD1        *(1+TD3)/2 +
     -                ZMAP(I,7)*TD2        *(1+TD3)/2
*   Store the Jacobian
                 JAC(1,1)=JACP11(TD1,TD2,TD3,I)
                 JAC(1,2)=JACP12(TD1,TD2,TD3,I)
                 JAC(1,3)=JACP13(TD1,TD2,TD3,I)
                 JAC(2,1)=JACP21(TD1,TD2,TD3,I)
                 JAC(2,2)=JACP22(TD1,TD2,TD3,I)
                 JAC(2,3)=JACP23(TD1,TD2,TD3,I)
                 JAC(3,1)=JACP31(TD1,TD2,TD3,I)
                 JAC(3,2)=JACP32(TD1,TD2,TD3,I)
                 JAC(3,3)=JACP33(TD1,TD2,TD3,I)
                 DET=DETP(TD1,TD2,TD3,I)
            ENDIF
*   Compute the difference vector
            DIFF(1)=XR-X
            DIFF(2)=YR-Y
            DIFF(3)=ZR-Z
*   Update the estimate
            DO 270 L=1,3
            CORR(L)=0
            DO 280 K=1,3
            CORR(L)=CORR(L)+JAC(L,K)*DIFF(K)/DET
280         CONTINUE
270         CONTINUE
            TD1=TD1-CORR(1)
            TD2=TD2-CORR(2)
            TD3=TD3-CORR(3)
            IF(MAX(ABS(CORR(1)),ABS(CORR(2)),ABS(CORR(3))).LT.
     -           1E-6)GOTO 290
260         CONTINUE
            PRINT *,' !!!!!! MATIND WARNING : No convergence'//
     -           ' achieved when refining the isoparamatric'//
     -           ' coordinates in element ',I
290         CONTINUE
*   Return the element with its coordinates if inside the element.
            IF((.NOT.ELMDGN(I).AND.
     -           TD1.GE.-1.AND.TD1.LE.+1.AND.
     -           TD2.GE.-1.AND.TD2.LE.+1.AND.
     -           TD3.GE.-1.AND.TD3.LE.+1).OR.
     -           (ELMDGN(I).AND.
     -           TD1.GE. 0.AND.TD1.LE.+1.AND.
     -           TD2.GE. 0.AND.TD2.LE.+1.AND.
     -           TD3.GE.-1.AND.TD3.LE.+1))THEN
                 IMAP=I
                 IL=I
                 T1=REAL(TD1)
                 T2=REAL(TD2)
                 T3=REAL(TD3)
                 T4=0
                 NFOUND=NFOUND+1
                 IF(NFOUND.LE.MXFOUN)IFOUND(NFOUND)=IMAP
                 IF(.NOT.LMAPCH)RETURN
                 DO 1013 K=1,4
                 DO 1014 L=1,4
                 JACBAK(K,L)=JAC(K,L)
1014             CONTINUE
1013             CONTINUE
                 DETBAK=DET
                 T1BAK=T1
                 T2BAK=T2
                 T3BAK=T3
                 T4BAK=T4
                 IMPBAK=IMAP
            ENDIF
30          CONTINUE
*   Not a known map type.
       ELSE
            PRINT *,' !!!!!! MAPIND WARNING : Unknown element type ',
     -           MAPTYP,' no map index returned.'
            IMAP=-1
            T1=-1
            T2=-1
            T3=-1
            T4=-1
            RETURN
       ENDIF
*** In checking mode, verify the tetrahedron/triangle count.
       IF(LMAPCH)THEN
*   Nothing found.
            IF(NFOUND.LE.0)THEN
                 IMAP=0
                 IL=0
*   More than 1 element found.
            ELSEIF(NFOUND.GT.1)THEN
                 PRINT *,' ------ MAPIND MESSAGE : Found ',NFOUND,
     -                ' elements for point ',X,Y,Z
                 DO 40 I=1,MIN(NFOUND,MXFOUN)
                 IF(MAPTYP.EQ.1.OR.MAPTYP.EQ.2.OR.MAPTYP.EQ.3)THEN
                      WRITE(LUNOUT,'('' Triangle '',I2,
     -                     '', index '',I5,'':''/
     -                     '' (x1,y1)='',2(E15.8,2X)/
     -                     '' (x2,y2)='',2(E15.8,2X)/
     -                     '' (x3,y3)='',2(E15.8,2X))')
     -                     I,IFOUND(I),
     -                     XMAP(IFOUND(I),1),YMAP(IFOUND(I),1),
     -                     XMAP(IFOUND(I),2),YMAP(IFOUND(I),2),
     -                     XMAP(IFOUND(I),3),YMAP(IFOUND(I),3)
                 ELSEIF(MAPTYP.EQ.4.OR.MAPTYP.EQ.5)THEN
                      WRITE(LUNOUT,'('' Quadrilateral '',I2,
     -                     '', index '',I5,'':''/
     -                     '' (x1,y1)='',2(E15.8,2X)/
     -                     '' (x2,y2)='',2(E15.8,2X)/
     -                     '' (x3,y3)='',2(E15.8,2X)/
     -                     '' (x4,y4)='',2(E15.8,2X))')
     -                     I,IFOUND(I),
     -                     XMAP(IFOUND(I),1),YMAP(IFOUND(I),1),
     -                     XMAP(IFOUND(I),2),YMAP(IFOUND(I),2),
     -                     XMAP(IFOUND(I),3),YMAP(IFOUND(I),3),
     -                     XMAP(IFOUND(I),4),YMAP(IFOUND(I),4)
                 ELSEIF(MAPTYP.EQ.11.OR.MAPTYP.EQ.12.OR.
     -                MAPTYP.EQ.13)THEN
                      WRITE(LUNOUT,'('' Tetrahedron '',I2,
     -                     '', index '',I5,'':''/
     -                     '' (x1,y1,z1)='',3(E15.8,2X)/
     -                     '' (x2,y2,z2)='',3(E15.8,2X)/
     -                     '' (x3,y3,z3)='',3(E15.8,2X)/
     -                     '' (x4,y4,z4)='',3(E15.8,2X))')
     -                     I,IFOUND(I),
     -                     XMAP(IFOUND(I),1),YMAP(IFOUND(I),1),
     -                     ZMAP(IFOUND(I),1),
     -                     XMAP(IFOUND(I),2),YMAP(IFOUND(I),2),
     -                     ZMAP(IFOUND(I),2),
     -                     XMAP(IFOUND(I),3),YMAP(IFOUND(I),3),
     -                     ZMAP(IFOUND(I),3),
     -                     XMAP(IFOUND(I),4),YMAP(IFOUND(I),4),
     -                     ZMAP(IFOUND(I),4)
                 ELSEIF(MAPTYP.EQ.14.OR.MAPTYP.EQ.15.OR.
     -                MAPTYP.EQ.16)THEN
                      WRITE(LUNOUT,'('' Hexahedron '',I2,
     -                     '', index '',I5,'':''/
     -                     '' (x1,y1,z1)='',3(E15.8,2X)/
     -                     '' (x2,y2,z2)='',3(E15.8,2X)/
     -                     '' (x3,y3,z3)='',3(E15.8,2X)/
     -                     '' (x4,y4,z4)='',3(E15.8,2X))')
     -                     I,IFOUND(I),
     -                     XMAP(IFOUND(I),1),YMAP(IFOUND(I),1),
     -                     ZMAP(IFOUND(I),1),
     -                     XMAP(IFOUND(I),2),YMAP(IFOUND(I),2),
     -                     ZMAP(IFOUND(I),2),
     -                     XMAP(IFOUND(I),3),YMAP(IFOUND(I),3),
     -                     ZMAP(IFOUND(I),3),
     -                     XMAP(IFOUND(I),4),YMAP(IFOUND(I),4),
     -                     ZMAP(IFOUND(I),4)
                 ENDIF
40               CONTINUE
            ENDIF
*   Check triangular/tetrahedral elements.
            IF((MAPTYP.EQ.1.OR.MAPTYP.EQ.2.OR.MAPTYP.EQ.3.OR.
     -           MAPTYP.EQ.11.OR.MAPTYP.EQ.12.OR.MAPTYP.EQ.13).AND.
     -           NFOUND.GE.1.AND.ABS(T1+T2+T3+T4-1).GT.1E-3)THEN
                 PRINT *,' !!!!!! MAPIND WARNING : Triangular'//
     -                ' coordinates do not add up to 1.'
                 PRINT *,' T1=',T1,', T2=',T2,', T3=',T3,', T4=',T4
                 PRINT *,' X= ',X ,', Y= ',Y ,', Z= ',Z
            ENDIF
*   Transfer if found.
            IF(NFOUND.GE.1)THEN
                 DO 1015 K=1,4
                 DO 1016 L=1,4
                 JAC(K,L)=JACBAK(K,L)
1016             CONTINUE
1015             CONTINUE
                 DET=DETBAK
                 T1=T1BAK
                 T2=T2BAK
                 T3=T3BAK
                 T4=T4BAK
                 IMAP=IMPBAK
                 IL=IMAP
            ENDIF
*** No volume found.
       ELSE
            IMAP=0
            IL=0
       ENDIF
       RETURN
*** Reset of volume.
       ENTRY MAPINR
       IL=0
       END
