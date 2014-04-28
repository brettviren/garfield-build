CDECK  ID>, MAPC4.
       SUBROUTINE MAPC4(X,Y,Z,T1,T2,T3,T4,JAC,DET,IMAP,IFAIL)
*-----------------------------------------------------------------------
*   MAPC4  - Finds the isoparametric cooordinates of point (X,Y) in a
*            linear quadrilateral.
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
       REAL X,Y,Z,T1,T2,T3,T4
       DOUBLE PRECISION JAC(4,4),DET,PROD,XR,YR,XP,YP,
     -      DN,DBOX,DPOINT,T,XT1,YT1,XT2,YT2
       INTEGER IMAP,IFAIL
C       integer i
*** Debugging.
       IF(LIDENT)PRINT *,' /// ROUTINE MAPC4  ///'
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC4  DEBUG   : Input'',
     -      '' point (x, y):     ('',E12.5,'' , '',E12.5,'')'')') X,Y
*** Set values which are not going to be determined.
       T1=0
       T2=0
       T3=0
       T4=0
*** Compute determinant.
       DET = -(-((XMAP(IMAP,1)-XMAP(IMAP,4))*
     -      (YMAP(IMAP,2)-YMAP(IMAP,3)))+
     -      (XMAP(IMAP,2)-XMAP(IMAP,3))*(YMAP(IMAP,1)-YMAP(IMAP,4)))*
     -      (2*X*(-YMAP(IMAP,1)+YMAP(IMAP,2)+YMAP(IMAP,3)-YMAP(IMAP,4))-
     -      (XMAP(IMAP,1)+XMAP(IMAP,4))*(YMAP(IMAP,2)+YMAP(IMAP,3)-2*Y)+
     -      XMAP(IMAP,2)*(YMAP(IMAP,1)+YMAP(IMAP,4)-2*Y)+XMAP(IMAP,3)*
     -      (YMAP(IMAP,1)+YMAP(IMAP,4)-2*Y))+
     -      (-(XMAP(IMAP,1)*YMAP(IMAP,2))+
     -      XMAP(IMAP,4)*YMAP(IMAP,3)-XMAP(IMAP,3)*YMAP(IMAP,4)+
     -      X*(-YMAP(IMAP,1)+
     -      YMAP(IMAP,2)-YMAP(IMAP,3)+YMAP(IMAP,4))+
     -      XMAP(IMAP,2)*(YMAP(IMAP,1)-Y)+(XMAP(IMAP,1)+XMAP(IMAP,3)-
     -      XMAP(IMAP,4))*Y)**2
*   Check that the determinant is non-negative.
       IF(DET.LT.0)THEN
C            PRINT *,' !!!!!! MAPC4  WARNING : No solution found for'//
C     -           ' isoparametric coordinates in element ',IMAP,
C     -           ' because the determinant is < 0.'
C      print *,' Location: ',x,y
C      do i=1,4
C      print '(2(f12.5,2x))',xmap(imap,i),ymap(imap,i)
C      enddo
            T1=0
            T2=0
            IFAIL=1
            RETURN
       ENDIF
*   Vector products for evaluation of T1.
       PROD=((XMAP(IMAP,3)-XMAP(IMAP,4))*(YMAP(IMAP,1)-YMAP(IMAP,2))-
     -       (XMAP(IMAP,1)-XMAP(IMAP,2))*(YMAP(IMAP,3)-YMAP(IMAP,4)))
       IF(PROD**2.GT.1E-12*
     -      ((XMAP(IMAP,1)-XMAP(IMAP,2))**2+
     -       (YMAP(IMAP,1)-YMAP(IMAP,2))**2)*
     -      ((XMAP(IMAP,3)-XMAP(IMAP,4))**2+
     -       (YMAP(IMAP,3)-YMAP(IMAP,4))**2))THEN
            T1=(-(XMAP(IMAP,4)*YMAP(IMAP,1))+X*YMAP(IMAP,1)+
     -           XMAP(IMAP,3)*YMAP(IMAP,2)-X*YMAP(IMAP,2)-
     -           XMAP(IMAP,2)*YMAP(IMAP,3)+
     -           X*YMAP(IMAP,3)+XMAP(IMAP,1)*YMAP(IMAP,4)-
     -           X*YMAP(IMAP,4)-XMAP(IMAP,1)*Y+XMAP(IMAP,2)*Y-
     -           XMAP(IMAP,3)*Y+XMAP(IMAP,4)*Y+SQRT(DET))/PROD
C            T1=(-(XMAP(IMAP,4)*YMAP(IMAP,1))+X*YMAP(IMAP,1)+
C     -           XMAP(IMAP,3)*YMAP(IMAP,2)-X*YMAP(IMAP,2)-
C     -           XMAP(IMAP,2)*YMAP(IMAP,3)+
C     -           X*YMAP(IMAP,3)+XMAP(IMAP,1)*YMAP(IMAP,4)-
C     -           X*YMAP(IMAP,4)-XMAP(IMAP,1)*Y+XMAP(IMAP,2)*Y-
C     -           XMAP(IMAP,3)*Y+XMAP(IMAP,4)*Y-SQRT(DET))/PROD
C       print *,' Non-parallel, 2nd estimate: T1 = ',T1
       ELSE
            XP=YMAP(IMAP,1)-YMAP(IMAP,2)
            YP=XMAP(IMAP,2)-XMAP(IMAP,1)
            DN=SQRT(XP**2+YP**2)
            IF(DN.LE.0)THEN
                 PRINT *,' !!!!!! MAPC4  WARNING : Element ',IMAP,
     -                ' appears to be degenerate in the 1-2 axis.'
                 T1=0
                 T2=0
                 IFAIL=1
                 RETURN
            ENDIF
            XP=XP/DN
            YP=YP/DN
            DPOINT=XP*(X-XMAP(IMAP,1))+YP*(Y-YMAP(IMAP,1))
            DBOX=  XP*(XMAP(IMAP,4)-XMAP(IMAP,1))+
     -             YP*(YMAP(IMAP,4)-YMAP(IMAP,1))
            IF(DBOX.EQ.0)THEN
                 PRINT *,' !!!!!! MAPC4  WARNING : Element ',IMAP,
     -                ' appears to be degenerate in the 1-3 axis.'
                 T1=0
                 T2=0
                 IFAIL=1
                 RETURN
            ENDIF
            T=-1+2*DPOINT/DBOX
            XT1=XMAP(IMAP,1)+0.5*(T+1)*(XMAP(IMAP,4)-XMAP(IMAP,1))
            YT1=YMAP(IMAP,1)+0.5*(T+1)*(YMAP(IMAP,4)-YMAP(IMAP,1))
            XT2=XMAP(IMAP,2)+0.5*(T+1)*(XMAP(IMAP,3)-XMAP(IMAP,2))
            YT2=YMAP(IMAP,2)+0.5*(T+1)*(YMAP(IMAP,3)-YMAP(IMAP,2))
            DN=(XT1-XT2)**2+(YT1-YT2)**2
            IF(DN.LE.0)THEN
                 PRINT *,' !!!!!! MAPC4  WARNING : Coordinate'//
     -                ' requested at convergence point of element ',
     -                IMAP
       print *,' parallel for t1: t2 = ',t
       print *,' xt1, yt1 = ',xt1,yt1
       print *,' x  , y   = ',x,y
       print *,' xt2, yt2 = ',xt2,yt2
                 T1=0
                 T2=0
                 IFAIL=1
                 RETURN
            ENDIF
            T1=-1+2*((X-XT1)*(XT2-XT1)+(Y-YT1)*(YT2-YT1))/DN
       ENDIF
*   Vector products for evaluation of T2.
       PROD=((XMAP(IMAP,1)-XMAP(IMAP,4))*(YMAP(IMAP,2)-YMAP(IMAP,3))-
     -       (XMAP(IMAP,2)-XMAP(IMAP,3))*(YMAP(IMAP,1)-YMAP(IMAP,4)))
       IF(PROD**2.GT.1E-12*
     -      ((XMAP(IMAP,1)-XMAP(IMAP,4))**2+
     -       (YMAP(IMAP,1)-YMAP(IMAP,4))**2)*
     -      ((XMAP(IMAP,2)-XMAP(IMAP,3))**2+
     -       (YMAP(IMAP,2)-YMAP(IMAP,3))**2))THEN
            T2 = (-(XMAP(IMAP,2)*YMAP(IMAP,1))+X*YMAP(IMAP,1)+
     -           XMAP(IMAP,1)*YMAP(IMAP,2)-X*YMAP(IMAP,2)-
     -           XMAP(IMAP,4)*YMAP(IMAP,3)+
     -           X*YMAP(IMAP,3)+XMAP(IMAP,3)*YMAP(IMAP,4)-
     -           X*YMAP(IMAP,4)-XMAP(IMAP,1)*Y+XMAP(IMAP,2)*Y-
     -           XMAP(IMAP,3)*Y+XMAP(IMAP,4)*Y-SQRT(DET))/PROD
C            T2 = (-(XMAP(IMAP,2)*YMAP(IMAP,1))+X*YMAP(IMAP,1)+
C     -           XMAP(IMAP,1)*YMAP(IMAP,2)-X*YMAP(IMAP,2)-
C     -           XMAP(IMAP,4)*YMAP(IMAP,3)+
C     -           X*YMAP(IMAP,3)+XMAP(IMAP,3)*YMAP(IMAP,4)-
C     -           X*YMAP(IMAP,4)-XMAP(IMAP,1)*Y+XMAP(IMAP,2)*Y-
C     -           XMAP(IMAP,3)*Y+XMAP(IMAP,4)*Y+SQRT(DET))/PROD
C       print *,' Non-parallel, 2nd estimate: T2 = ',T2
       ELSE
            XP=YMAP(IMAP,1)-YMAP(IMAP,4)
            YP=XMAP(IMAP,4)-XMAP(IMAP,1)
            DN=SQRT(XP**2+YP**2)
            IF(DN.LE.0)THEN
                 PRINT *,' !!!!!! MAPC4  WARNING : Element ',IMAP,
     -                ' appears to be degenerate in the 1-4 axis.'
                 T1=0
                 T2=0
                 IFAIL=1
                 RETURN
            ENDIF
            XP=XP/DN
            YP=YP/DN
            DPOINT=XP*(X-XMAP(IMAP,1))+YP*(Y-YMAP(IMAP,1))
            DBOX=  XP*(XMAP(IMAP,2)-XMAP(IMAP,1))+
     -             YP*(YMAP(IMAP,2)-YMAP(IMAP,1))
            IF(DBOX.EQ.0)THEN
                 PRINT *,' !!!!!! MAPC4  WARNING : Element ',IMAP,
     -                ' appears to be degenerate in the 1-2 axis.'
                 T1=0
                 T2=0
                 IFAIL=1
                 RETURN
            ENDIF
            T=-1+2*DPOINT/DBOX
            XT1=XMAP(IMAP,1)+0.5*(T+1)*(XMAP(IMAP,2)-XMAP(IMAP,1))
            YT1=YMAP(IMAP,1)+0.5*(T+1)*(YMAP(IMAP,2)-YMAP(IMAP,1))
            XT2=XMAP(IMAP,4)+0.5*(T+1)*(XMAP(IMAP,3)-XMAP(IMAP,4))
            YT2=YMAP(IMAP,4)+0.5*(T+1)*(YMAP(IMAP,3)-YMAP(IMAP,4))
            DN=(XT1-XT2)**2+(YT1-YT2)**2
            IF(DN.LE.0)THEN
                 PRINT *,' !!!!!! MAPC4  WARNING : Coordinate'//
     -                ' requested at convergence point of element ',
     -                IMAP
       print *,' parallel for t2: t1 = ',t
       print *,' xt1, yt1 = ',xt1,yt1
       print *,' x  , y   = ',x,y
       print *,' xt2, yt2 = ',xt2,yt2
                 T1=0
                 T2=0
                 IFAIL=1
                 RETURN
            ENDIF
            T2=-1+2*((X-XT1)*(XT2-XT1)+(Y-YT1)*(YT2-YT1))/DN
       ENDIF
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPC4  DEBUG   :'',
     -      '' Isoparametric (u, v):   ('',F12.9,'' , '',F12.9,'')'')')
     -      T1,T2
*   Re-compute the (x,y,z) position for this coordinate.
       IF(LDEBUG)THEN
            XR=XMAP(IMAP, 1)*(1-T1)*(1-T2)/4+
     -         XMAP(IMAP, 2)*(1+T1)*(1-T2)/4+
     -         XMAP(IMAP, 3)*(1+T1)*(1+T2)/4+
     -         XMAP(IMAP, 4)*(1-T1)*(1+T2)/4
            YR=YMAP(IMAP, 1)*(1-T1)*(1-T2)/4+
     -         YMAP(IMAP, 2)*(1+T1)*(1-T2)/4+
     -         YMAP(IMAP, 3)*(1+T1)*(1+T2)/4+
     -         YMAP(IMAP, 4)*(1-T1)*(1+T2)/4
            WRITE(LUNOUT,'(''  ++++++ MAPC4  DEBUG   : Position'',
     -           '' requested:     ('',E12.5,'' , '',E12.5,'')''/
     -           26X,''Position reconstructed: ('',E12.5,'' , '',E12.5,
     -           '')''/
     -           26X,''Difference:             ('',E12.5,'' , '',E12.5,
     -           '')'')') X,Y,XR,YR,X-XR,Y-YR
       ENDIF
*** This should have worked if we get this far.
       IFAIL=0
       END
