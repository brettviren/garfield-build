CDECK  ID>, MAPC3.
       SUBROUTINE MAPC3(X,Y,Z,T1,T2,T3,T4,JAC,DET,IMAP,IFAIL)
*-----------------------------------------------------------------------
*   MAPC3  - Finds the triangle cooordinates of point (X,Y) in a
*            curved quadratic triangle.
*   (Last changed on  9/ 1/09.)
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
       REAL X,Y,Z,T1,T2,T3,T4,TT1,TT2,TT3
       DOUBLE PRECISION DETT,
     -      JACT11,JACT12,JACT13,
     -      JACT21,JACT22,JACT23,
     -      JACT31,JACT32,JACT33,
     -      XR,YR,SR,CORR(3),DIFF(3),
     -      JAC(4,4),DET,U,V,W,TD1,TD2,TD3
       INTEGER I,K,L,IMAP,IL,ITER,IFAIL
*   Determinant of the quadratic triangular Jacobian
       DETT(U,V,W,IL)=
     -      -(((-1+4*V)*XMAP(IL,2)+XMAP(IL,3)-4*W*XMAP(IL,3)+
     -      4*U*XMAP(IL,4)-
     -      4*U*XMAP(IL,5)-4*V*XMAP(IL,6)+4*W*XMAP(IL,6))*
     -      (-YMAP(IL,1)+4*U*YMAP(IL,1)+4*V*YMAP(IL,4)+4*W*YMAP(IL,5)))-
     -      ((-1+4*U)*XMAP(IL,1)+XMAP(IL,2)-4*V*XMAP(IL,2)-
     -      4*U*XMAP(IL,4)+
     -      4*V*XMAP(IL,4)+4*W*XMAP(IL,5)-4*W*XMAP(IL,6))*
     -      (-YMAP(IL,3)+4*W*YMAP(IL,3)+4*U*YMAP(IL,5)+4*V*YMAP(IL,6))+
     -      ((-1+4*U)*XMAP(IL,1)+XMAP(IL,3)-4*W*XMAP(IL,3)+
     -      4*V*XMAP(IL,4)-
     -      4*U*XMAP(IL,5)+4*W*XMAP(IL,5)-4*V*XMAP(IL,6))*
     -      (-YMAP(IL,2)+4*V*YMAP(IL,2)+4*U*YMAP(IL,4)+4*W*YMAP(IL,6))
*   Terms of the quadratic triangular Jacobian
       JACT11(U,V,W,IL)=
     -      (-XMAP(IL,2)+4*V*XMAP(IL,2)+4*U*XMAP(IL,4)+4*W*XMAP(IL,6))*
     -      (-YMAP(IL,3)+4*W*YMAP(IL,3)+4*U*YMAP(IL,5)+4*V*YMAP(IL,6))-
     -      (-XMAP(IL,3)+4*W*XMAP(IL,3)+4*U*XMAP(IL,5)+4*V*XMAP(IL,6))*
     -      (-YMAP(IL,2)+4*V*YMAP(IL,2)+4*U*YMAP(IL,4)+4*W*YMAP(IL,6))
       JACT12(U,V,W,IL)=
     -      (-1+4*V)*YMAP(IL,2)+YMAP(IL,3)-4*W*YMAP(IL,3)+
     -      4*U*YMAP(IL,4)-4*U*YMAP(IL,5)-4*V*YMAP(IL,6)+4*W*YMAP(IL,6)
       JACT13(U,V,W,IL)=
     -      XMAP(IL,2)-4*V*XMAP(IL,2)+(-1+4*W)*XMAP(IL,3)-
     -      4*U*XMAP(IL,4)+4*U*XMAP(IL,5)+4*V*XMAP(IL,6)-4*W*XMAP(IL,6)
       JACT21(U,V,W,IL)=
     -      (-XMAP(IL,3)+4*W*XMAP(IL,3)+4*U*XMAP(IL,5)+4*V*XMAP(IL,6))*
     -      (-YMAP(IL,1)+4*U*YMAP(IL,1)+4*V*YMAP(IL,4)+4*W*YMAP(IL,5))-
     -      (-XMAP(IL,1)+4*U*XMAP(IL,1)+4*V*XMAP(IL,4)+4*W*XMAP(IL,5))*
     -      (-YMAP(IL,3)+4*W*YMAP(IL,3)+4*U*YMAP(IL,5)+4*V*YMAP(IL,6))
       JACT22(U,V,W,IL)=
     -      YMAP(IL,1)-4*U*YMAP(IL,1)-YMAP(IL,3)+4*W*YMAP(IL,3)-
     -      4*V*YMAP(IL,4)+4*U*YMAP(IL,5)-4*W*YMAP(IL,5)+4*V*YMAP(IL,6)
       JACT23(U,V,W,IL)=
     -      (-1+4*U)*XMAP(IL,1)+XMAP(IL,3)-4*W*XMAP(IL,3)+
     -      4*V*XMAP(IL,4)-4*U*XMAP(IL,5)+4*W*XMAP(IL,5)-4*V*XMAP(IL,6)
       JACT31(U,V,W,IL)=-(
     -      (-XMAP(IL,2)+4*V*XMAP(IL,2)+4*U*XMAP(IL,4)+4*W*XMAP(IL,6))*
     -      (-YMAP(IL,1)+4*U*YMAP(IL,1)+4*V*YMAP(IL,4)+4*W*YMAP(IL,5)))+
     -      (-XMAP(IL,1)+4*U*XMAP(IL,1)+4*V*XMAP(IL,4)+4*W*XMAP(IL,5))*
     -      (-YMAP(IL,2)+4*V*YMAP(IL,2)+4*U*YMAP(IL,4)+4*W*YMAP(IL,6))
       JACT32(U,V,W,IL)=
     -      (-1+4*U)*YMAP(IL,1)+YMAP(IL,2)-4*V*YMAP(IL,2)-
     -      4*U*YMAP(IL,4)+4*V*YMAP(IL,4)+4*W*YMAP(IL,5)-4*W*YMAP(IL,6)
       JACT33(U,V,W,IL)=
     -      XMAP(IL,1)-4*U*XMAP(IL,1)-XMAP(IL,2)+4*V*XMAP(IL,2)+
     -      4*U*XMAP(IL,4)-4*V*XMAP(IL,4)-4*W*XMAP(IL,5)+4*W*XMAP(IL,6)
*** Debugging.
       IF(LIDENT)PRINT *,' /// ROUTINE MAPC3 ///'
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC3  DEBUG   : Input'',
     -      '' point:       (x, y)    = ('',E12.5,'' , '',E12.5,
     -      '')'')') X,Y
*** This may fail.
       IFAIL=1
*** Make a first order approximation, using the linear triangle.
       TT1=(X-XMAP(IMAP,2))*(YMAP(IMAP,3)-YMAP(IMAP,2))-
     -      (Y-YMAP(IMAP,2))*(XMAP(IMAP,3)-XMAP(IMAP,2))
       TT2=(X-XMAP(IMAP,3))*(YMAP(IMAP,1)-YMAP(IMAP,3))-
     -      (Y-YMAP(IMAP,3))*(XMAP(IMAP,1)-XMAP(IMAP,3))
       TT3=(X-XMAP(IMAP,1))*(YMAP(IMAP,2)-YMAP(IMAP,1))-
     -      (Y-YMAP(IMAP,1))*(XMAP(IMAP,2)-XMAP(IMAP,1))
       IF(  (XMAP(IMAP,1)-XMAP(IMAP,2))*(YMAP(IMAP,3)-YMAP(IMAP,2))-
     -      (XMAP(IMAP,3)-XMAP(IMAP,2))*(YMAP(IMAP,1)-YMAP(IMAP,2)).EQ.
     -      0.OR.
     -      (XMAP(IMAP,2)-XMAP(IMAP,3))*(YMAP(IMAP,1)-YMAP(IMAP,3))-
     -      (XMAP(IMAP,1)-XMAP(IMAP,3))*(YMAP(IMAP,2)-YMAP(IMAP,3)).EQ.
     -      0.OR.
     -      (XMAP(IMAP,3)-XMAP(IMAP,1))*(YMAP(IMAP,2)-YMAP(IMAP,1))-
     -      (XMAP(IMAP,2)-XMAP(IMAP,1))*(YMAP(IMAP,3)-YMAP(IMAP,1)).EQ.
     -      0)THEN
            PRINT *,' !!!!!! MAPC3  WARNING : Calculation of initial'//
     -           ' coordinates failed; abandoned.'
            print *,tt1,tt2,tt3
            RETURN
       ELSE
            T1=TT1/
     -         ((XMAP(IMAP,1)-XMAP(IMAP,2))*(YMAP(IMAP,3)-YMAP(IMAP,2))-
     -          (XMAP(IMAP,3)-XMAP(IMAP,2))*(YMAP(IMAP,1)-YMAP(IMAP,2)))
            T2=TT2/
     -         ((XMAP(IMAP,2)-XMAP(IMAP,3))*(YMAP(IMAP,1)-YMAP(IMAP,3))-
     -          (XMAP(IMAP,1)-XMAP(IMAP,3))*(YMAP(IMAP,2)-YMAP(IMAP,3)))
            T3=TT3/
     -         ((XMAP(IMAP,3)-XMAP(IMAP,1))*(YMAP(IMAP,2)-YMAP(IMAP,1))-
     -          (XMAP(IMAP,2)-XMAP(IMAP,1))*(YMAP(IMAP,3)-YMAP(IMAP,1)))
            T4=0
       ENDIF
*   Convert to double precision for subsequent calculations.
       TD1=DBLE(T1)
       TD2=DBLE(T2)
       TD3=DBLE(T3)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC3  DEBUG   :'',
     -      '' Linear estimate:   (u, v, w) = ('',F12.9,
     -      '' , '',F12.9,'' , '',F12.9,''), sum = '',F12.9)')
     -      TD1,TD2,TD3,TD1+TD2+TD3
*** Iterate to refine the estimate.
       DO 210 ITER=1,10
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC3  DEBUG   :'',
     -      '' Iteration '',I5,'':   (u, v, w) = ('',F12.9,
     -      '' , '',F12.9,'' , '',F12.9,''), sum = '',F12.9)')
     -      ITER,TD1,TD2,TD3,TD1+TD2+TD3
*   Re-compute the (x,y,z) position for this coordinate.
       XR=XMAP(IMAP, 1)*TD1*(2*TD1-1)+
     -    XMAP(IMAP, 2)*TD2*(2*TD2-1)+
     -    XMAP(IMAP, 3)*TD3*(2*TD3-1)+
     -    XMAP(IMAP, 4)*4*TD1*TD2+
     -    XMAP(IMAP, 5)*4*TD1*TD3+
     -    XMAP(IMAP, 6)*4*TD2*TD3
       YR=YMAP(IMAP, 1)*TD1*(2*TD1-1)+
     -    YMAP(IMAP, 2)*TD2*(2*TD2-1)+
     -    YMAP(IMAP, 3)*TD3*(2*TD3-1)+
     -    YMAP(IMAP, 4)*4*TD1*TD2+
     -    YMAP(IMAP, 5)*4*TD1*TD3+
     -    YMAP(IMAP, 6)*4*TD2*TD3
       SR=TD1+TD2+TD3
*   Store the Jacobian
       JAC(1,1)=JACT11(TD1,TD2,TD3,IMAP)
       JAC(1,2)=JACT12(TD1,TD2,TD3,IMAP)
       JAC(1,3)=JACT13(TD1,TD2,TD3,IMAP)
       JAC(2,1)=JACT21(TD1,TD2,TD3,IMAP)
       JAC(2,2)=JACT22(TD1,TD2,TD3,IMAP)
       JAC(2,3)=JACT23(TD1,TD2,TD3,IMAP)
       JAC(3,1)=JACT31(TD1,TD2,TD3,IMAP)
       JAC(3,2)=JACT32(TD1,TD2,TD3,IMAP)
       JAC(3,3)=JACT33(TD1,TD2,TD3,IMAP)
       DET=DETT(TD1,TD2,TD3,IMAP)
*   Compute the difference vector
       DIFF(1)=1.0D0-SR
       DIFF(2)=X-XR
       DIFF(3)=Y-YR
*   Update the estimate
       DO 220 L=1,3
       CORR(L)=0.0D0
       DO 230 K=1,3
       CORR(L)=CORR(L)+JAC(L,K)*DIFF(K)/DET
230    CONTINUE
220    CONTINUE
*   Debugging
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC3  DEBUG   : '',
     -      ''Difference vector: (1, x, y) = ('',E12.5,
     -      '' , '',E12.5,'' , '',E12.5,''),'',
     -      '' max = '',E12.5/
     -      26X,''Correction vector: (u, v, w) = ('',F12.9,
     -      '' , '',F12.9,'' , '',F12.9,''),'',
     -      '' max = '',F12.9)')
     -      (DIFF(I),I=1,3),
     -      MAX(ABS(DIFF(1)),ABS(DIFF(2)),ABS(DIFF(3))),
     -      (CORR(I),I=1,3),
     -      MAX(ABS(CORR(1)),ABS(CORR(2)),ABS(CORR(3)))
*   Update the vector.
       TD1=TD1+CORR(1)
       TD2=TD2+CORR(2)
       TD3=TD3+CORR(3)
*   Check for convergence.
       IF(MAX(ABS(CORR(1)),ABS(CORR(2)),
     -        ABS(CORR(3))).LT.1E-5)GOTO 240
210    CONTINUE
       IF(X.GT.MIN(XMAP(IMAP,1),XMAP(IMAP,2),XMAP(IMAP,3)).AND.
     -      X.LT.MAX(XMAP(IMAP,1),XMAP(IMAP,2),XMAP(IMAP,3)).AND.
     -      Y.GT.MIN(YMAP(IMAP,1),YMAP(IMAP,2),YMAP(IMAP,3)).AND.
     -      Y.LT.MAX(YMAP(IMAP,1),YMAP(IMAP,2),YMAP(IMAP,3)))
     -      PRINT *,' !!!!!! MAPC3  WARNING : No convergence'//
     -           ' achieved when refining internal isoparametric'//
     -           ' coordinates in element ',IMAP,'.'
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ MAPC3  DEBUG   : Convergence'',
     -           '' failure in element '',I6,'' for:'')') IMAP
            WRITE(LUNOUT,'(2X,''Pos: '',2(F12.5,2X))') X,Y
            WRITE(LUNOUT,'(2X,''P1:  '',2(F12.5,2X))') XMAP(IMAP,1),
     -           YMAP(IMAP,1)
            WRITE(LUNOUT,'(2X,''P2:  '',2(F12.5,2X))') XMAP(IMAP,2),
     -           YMAP(IMAP,2)
            WRITE(LUNOUT,'(2X,''P3:  '',2(F12.5,2X))') XMAP(IMAP,3),
     -           YMAP(IMAP,3)
       ENDIF
       TD1=0
       TD2=0
       TD3=0
       IFAIL=1
       RETURN
*   Convergence reached.
240    CONTINUE
       T1=TD1
       T2=TD2
       T3=TD3
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC3  DEBUG   :'',
     -      '' Convergence reached.''/
     -      26X,''Final coordinates: (u, v, w) = ('',F12.9,
     -      '' , '',F12.9,'' , '',F12.9,''),'',
     -      '' sum = '',F12.9)') T1,T2,T3,T1+T2+T3
*   Success.
       IFAIL=0
*** Re-compute the (x,y,z) position for this coordinate.
       IF(LDEBUG)THEN
            XR=XMAP(IMAP, 1)*TD1*(2*TD1-1)+
     -         XMAP(IMAP, 2)*TD2*(2*TD2-1)+
     -         XMAP(IMAP, 3)*TD3*(2*TD3-1)+
     -         XMAP(IMAP, 4)*4*TD1*TD2+
     -         XMAP(IMAP, 5)*4*TD1*TD3+
     -         XMAP(IMAP, 6)*4*TD2*TD3
            YR=YMAP(IMAP, 1)*TD1*(2*TD1-1)+
     -         YMAP(IMAP, 2)*TD2*(2*TD2-1)+
     -         YMAP(IMAP, 3)*TD3*(2*TD3-1)+
     -         YMAP(IMAP, 4)*4*TD1*TD2+
     -         YMAP(IMAP, 5)*4*TD1*TD3+
     -         YMAP(IMAP, 6)*4*TD2*TD3
            SR=TD1+TD2+TD3
            WRITE(LUNOUT,'(''  ++++++ MAPC3  DEBUG   : Position'',
     -           '' requested:     ('',E12.5,'' , '',E12.5,'')''/
     -           26X,''Position reconstructed: ('',E12.5,'' , '',E12.5,
     -           '')''/
     -           26X,''Difference:             ('',E12.5,'' , '',E12.5,
     -           '')''/
     -           26X,''Checksum-1: '',13X,F12.9)')
     -           X,Y,XR,YR,X-XR,Y-YR,SR-1
       ENDIF
       END
