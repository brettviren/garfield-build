CDECK  ID>, MAPC12.
       SUBROUTINE MAPC12(X,Y,Z,T1,T2,T3,T4,JAC,DET,IMAP,IFAIL)
*-----------------------------------------------------------------------
*   MAPC12 - Finds the tetrahedral cooordinates of point (X,Y,Z) in a
*            linear tetrahedron.
*   (Last changed on  5/12/09.)
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
       REAL X,Y,Z,T1,T2,T3,T4
       DOUBLE PRECISION JAC(4,4),DET,XR,YR,ZR,SR
       INTEGER IMAP,IFAIL
*** Debugging.
       IF(LIDENT)PRINT *,' /// ROUTINE MAPC12 ///'
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC12 DEBUG   : Input'',
     -      '' point:       (   x, y, z) = ('',15X,E12.5,'' , '',E12.5,
     -      '' , '',E12.5,'')'')') X,Y,Z
*** Compute coordinates.
       T1=(X-XMAP(IMAP,2))*(
     -      (YMAP(IMAP,3)-YMAP(IMAP,2))*(ZMAP(IMAP,4)-ZMAP(IMAP,2))-
     -      (YMAP(IMAP,4)-YMAP(IMAP,2))*(ZMAP(IMAP,3)-ZMAP(IMAP,2)))+
     -      (Y-YMAP(IMAP,2))*(
     -      (ZMAP(IMAP,3)-ZMAP(IMAP,2))*(XMAP(IMAP,4)-XMAP(IMAP,2))-
     -      (ZMAP(IMAP,4)-ZMAP(IMAP,2))*(XMAP(IMAP,3)-XMAP(IMAP,2)))+
     -      (Z-ZMAP(IMAP,2))*(
     -      (XMAP(IMAP,3)-XMAP(IMAP,2))*(YMAP(IMAP,4)-YMAP(IMAP,2))-
     -      (XMAP(IMAP,4)-XMAP(IMAP,2))*(YMAP(IMAP,3)-YMAP(IMAP,2)))
       T2=(X-XMAP(IMAP,3))*(
     -      (YMAP(IMAP,1)-YMAP(IMAP,3))*(ZMAP(IMAP,4)-ZMAP(IMAP,3))-
     -      (YMAP(IMAP,4)-YMAP(IMAP,3))*(ZMAP(IMAP,1)-ZMAP(IMAP,3)))+
     -      (Y-YMAP(IMAP,3))*(
     -      (ZMAP(IMAP,1)-ZMAP(IMAP,3))*(XMAP(IMAP,4)-XMAP(IMAP,3))-
     -      (ZMAP(IMAP,4)-ZMAP(IMAP,3))*(XMAP(IMAP,1)-XMAP(IMAP,3)))+
     -      (Z-ZMAP(IMAP,3))*(
     -      (XMAP(IMAP,1)-XMAP(IMAP,3))*(YMAP(IMAP,4)-YMAP(IMAP,3))-
     -      (XMAP(IMAP,4)-XMAP(IMAP,3))*(YMAP(IMAP,1)-YMAP(IMAP,3)))
       T3=(X-XMAP(IMAP,4))*(
     -      (YMAP(IMAP,1)-YMAP(IMAP,4))*(ZMAP(IMAP,2)-ZMAP(IMAP,4))-
     -      (YMAP(IMAP,2)-YMAP(IMAP,4))*(ZMAP(IMAP,1)-ZMAP(IMAP,4)))+
     -      (Y-YMAP(IMAP,4))*(
     -      (ZMAP(IMAP,1)-ZMAP(IMAP,4))*(XMAP(IMAP,2)-XMAP(IMAP,4))-
     -      (ZMAP(IMAP,2)-ZMAP(IMAP,4))*(XMAP(IMAP,1)-XMAP(IMAP,4)))+
     -      (Z-ZMAP(IMAP,4))*(
     -      (XMAP(IMAP,1)-XMAP(IMAP,4))*(YMAP(IMAP,2)-YMAP(IMAP,4))-
     -      (XMAP(IMAP,2)-XMAP(IMAP,4))*(YMAP(IMAP,1)-YMAP(IMAP,4)))
       T4=(X-XMAP(IMAP,1))*(
     -      (YMAP(IMAP,3)-YMAP(IMAP,1))*(ZMAP(IMAP,2)-ZMAP(IMAP,1))-
     -      (YMAP(IMAP,2)-YMAP(IMAP,1))*(ZMAP(IMAP,3)-ZMAP(IMAP,1)))+
     -      (Y-YMAP(IMAP,1))*(
     -      (ZMAP(IMAP,3)-ZMAP(IMAP,1))*(XMAP(IMAP,2)-XMAP(IMAP,1))-
     -      (ZMAP(IMAP,2)-ZMAP(IMAP,1))*(XMAP(IMAP,3)-XMAP(IMAP,1)))+
     -      (Z-ZMAP(IMAP,1))*(
     -      (XMAP(IMAP,3)-XMAP(IMAP,1))*(YMAP(IMAP,2)-YMAP(IMAP,1))-
     -      (XMAP(IMAP,2)-XMAP(IMAP,1))*(YMAP(IMAP,3)-YMAP(IMAP,1)))
       T1=T1/((XMAP(IMAP,1)-XMAP(IMAP,2))*(
     -      (YMAP(IMAP,3)-YMAP(IMAP,2))*(ZMAP(IMAP,4)-ZMAP(IMAP,2))-
     -      (YMAP(IMAP,4)-YMAP(IMAP,2))*(ZMAP(IMAP,3)-ZMAP(IMAP,2)))+
     -      (YMAP(IMAP,1)-YMAP(IMAP,2))*(
     -      (ZMAP(IMAP,3)-ZMAP(IMAP,2))*(XMAP(IMAP,4)-XMAP(IMAP,2))-
     -      (ZMAP(IMAP,4)-ZMAP(IMAP,2))*(XMAP(IMAP,3)-XMAP(IMAP,2)))+
     -      (ZMAP(IMAP,1)-ZMAP(IMAP,2))*(
     -      (XMAP(IMAP,3)-XMAP(IMAP,2))*(YMAP(IMAP,4)-YMAP(IMAP,2))-
     -      (XMAP(IMAP,4)-XMAP(IMAP,2))*(YMAP(IMAP,3)-YMAP(IMAP,2))))
       T2=T2/((XMAP(IMAP,2)-XMAP(IMAP,3))*(
     -      (YMAP(IMAP,1)-YMAP(IMAP,3))*(ZMAP(IMAP,4)-ZMAP(IMAP,3))-
     -      (YMAP(IMAP,4)-YMAP(IMAP,3))*(ZMAP(IMAP,1)-ZMAP(IMAP,3)))+
     -      (YMAP(IMAP,2)-YMAP(IMAP,3))*(
     -      (ZMAP(IMAP,1)-ZMAP(IMAP,3))*(XMAP(IMAP,4)-XMAP(IMAP,3))-
     -      (ZMAP(IMAP,4)-ZMAP(IMAP,3))*(XMAP(IMAP,1)-XMAP(IMAP,3)))+
     -      (ZMAP(IMAP,2)-ZMAP(IMAP,3))*(
     -      (XMAP(IMAP,1)-XMAP(IMAP,3))*(YMAP(IMAP,4)-YMAP(IMAP,3))-
     -      (XMAP(IMAP,4)-XMAP(IMAP,3))*(YMAP(IMAP,1)-YMAP(IMAP,3))))
       T3=T3/((XMAP(IMAP,3)-XMAP(IMAP,4))*(
     -      (YMAP(IMAP,1)-YMAP(IMAP,4))*(ZMAP(IMAP,2)-ZMAP(IMAP,4))-
     -      (YMAP(IMAP,2)-YMAP(IMAP,4))*(ZMAP(IMAP,1)-ZMAP(IMAP,4)))+
     -      (YMAP(IMAP,3)-YMAP(IMAP,4))*(
     -      (ZMAP(IMAP,1)-ZMAP(IMAP,4))*(XMAP(IMAP,2)-XMAP(IMAP,4))-
     -      (ZMAP(IMAP,2)-ZMAP(IMAP,4))*(XMAP(IMAP,1)-XMAP(IMAP,4)))+
     -      (ZMAP(IMAP,3)-ZMAP(IMAP,4))*(
     -      (XMAP(IMAP,1)-XMAP(IMAP,4))*(YMAP(IMAP,2)-YMAP(IMAP,4))-
     -      (XMAP(IMAP,2)-XMAP(IMAP,4))*(YMAP(IMAP,1)-YMAP(IMAP,4))))
       T4=T4/((XMAP(IMAP,4)-XMAP(IMAP,1))*(
     -      (YMAP(IMAP,3)-YMAP(IMAP,1))*(ZMAP(IMAP,2)-ZMAP(IMAP,1))-
     -      (YMAP(IMAP,2)-YMAP(IMAP,1))*(ZMAP(IMAP,3)-ZMAP(IMAP,1)))+
     -      (YMAP(IMAP,4)-YMAP(IMAP,1))*(
     -      (ZMAP(IMAP,3)-ZMAP(IMAP,1))*(XMAP(IMAP,2)-XMAP(IMAP,1))-
     -      (ZMAP(IMAP,2)-ZMAP(IMAP,1))*(XMAP(IMAP,3)-XMAP(IMAP,1)))+
     -      (ZMAP(IMAP,4)-ZMAP(IMAP,1))*(
     -      (XMAP(IMAP,3)-XMAP(IMAP,1))*(YMAP(IMAP,2)-YMAP(IMAP,1))-
     -      (XMAP(IMAP,2)-XMAP(IMAP,1))*(YMAP(IMAP,3)-YMAP(IMAP,1))))
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC12 DEBUG   :'',
     -      '' Tetrahedral coord. (t, u, v, w) = ('',F12.9,
     -      '' , '',F12.9,'' , '',F12.9,'' , '',F12.9,''), sum = '',
     -      F12.9)') T1,T2,T3,T4,T1+T2+T3+T4
*   Re-compute the (x,y,z) position for this coordinate.
       IF(LDEBUG)THEN
            XR=XMAP(IMAP, 1)*T1+
     -         XMAP(IMAP, 2)*T2+
     -         XMAP(IMAP, 3)*T3+
     -         XMAP(IMAP, 4)*T4
            YR=YMAP(IMAP, 1)*T1+
     -         YMAP(IMAP, 2)*T2+
     -         YMAP(IMAP, 3)*T3+
     -         YMAP(IMAP, 4)*T4
            ZR=ZMAP(IMAP, 1)*T1+
     -         ZMAP(IMAP, 2)*T2+
     -         ZMAP(IMAP, 3)*T3+
     -         ZMAP(IMAP, 4)*T4
            SR=T1+T2+T3+T4
            WRITE(LUNOUT,'(''  ++++++ MAPC12 DEBUG   : Position'',
     -           '' requested:     ('',E12.5,'' , '',E12.5,'' , '',
     -           E12.5,'')''/
     -           26X,''Position reconstructed: ('',E12.5,'' , '',E12.5,
     -           '' , '',E12.5,'')''/
     -           26X,''Difference:             ('',E12.5,'' , '',E12.5,
     -           '' , '',E12.5,'')''/
     -           26X,''Checksum-1: '',13X,F12.9)')
     -           X,Y,Z,XR,YR,ZR,X-XR,Y-YR,Z-ZR,SR-1
       ENDIF
*** This should always work
       IFAIL=0
       END
