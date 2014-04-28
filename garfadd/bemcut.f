CDECK  ID>, BEMCUT.
       SUBROUTINE BEMCUT(IREF, NREFO, IREFO, A,B,C, X0,Y0,Z0)
*-----------------------------------------------------------------------
*   BEMCUT - Cuts one plane with another and keeps only a part.
*            Part kept: a (x-x0) + b (y-y0) + c (z-z0) > 0.
*            Works only for convex areas.
*   (Last changed on 16/ 4/10.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER NPL,I,J,NOUT,IREF,NREFO,IREFO(*),IFAIL1,ICOL,IVOL,
     -      CLASS(MXEDGE)
       DOUBLE PRECISION XPL(MXEDGE),YPL(MXEDGE),ZPL(MXEDGE),
     -      A,B,C, X0,Y0,Z0, APL,BPL,CPL,DIST,DIST1,EPSD,
     -      PROD1,PROD2,XLAM,XOUT(MXEDGE),YOUT(MXEDGE),ZOUT(MXEDGE)
*** Number of output planes.
       NREFO=0
*** Check normal vector of plane
       IF(A**2+B**2+C**2.LE.0)THEN
            PRINT *,' !!!!!! BEMCUT WARNING : Normal vector of plane'//
     -           ' has zero norm; no cut applied.'
            RETURN
       ENDIF
*** Retrieve the panel.
       CALL PLABU1('READ',IREF,NPL,XPL,YPL,ZPL,
     -      APL,BPL,CPL,ICOL,IVOL,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! BEMCUT WARNING : Unable to retrieve the'//
     -           ' panel; no cut applied.'
            RETURN
*   Check dimensions.
       ELSEIF(NPL.GT.MXEDGE)THEN
            PRINT *,' !!!!!! BEMCUT WARNING : Array dimensions not'//
     -           ' sufficient; no cut applied.'
            RETURN
       ENDIF
*** Debugging.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ BEMCUT DEBUG   : '',
     -               ''Panel identifier: '',I5/
     -           26X,''Panel normal:     '',3F15.8/
     -           26X,''Cut plane normal: '',3F15.8/
     -           26X,''Cut plane point:  '',3F15.8/
     -           26X,''Polygon:'')') IREF,APL,BPL,CPL,A,B,C,X0,Y0,Z0
            DO 60 I=1,NPL
            WRITE(LUNOUT,'(26X,3F15.8)') XPL(I),YPL(I),ZPL(I)
60          CONTINUE
       ENDIF
*** Set a tolerance for lambda.
       DIST=0
       DO 20 I=1,NPL
       DIST1=ABS((X0-XPL(I))*A+(Y0-YPL(I))*B+(Z0-ZPL(I))*C)/
     -      SQRT(A**2+B**2+C**2)
       IF(DIST1.GT.DIST)DIST=DIST1
       DO 30 J=I+1,NPL
       DIST1=SQRT((XPL(I)-XPL(J))**2+(YPL(I)-YPL(J))**2+
     -      (ZPL(I)-ZPL(J))**2)
       IF(DIST1.GT.DIST)DIST=DIST1
30     CONTINUE
20     CONTINUE
       IF(DIST.LE.0)THEN
            PRINT *,' !!!!!! BEMCUT WARNING : Maximum distance'//
     -           ' equal to zero; no cut applied.'
            RETURN
       ENDIF
       EPSD=1.0D-8*DIST
*** Check for coincident cut plane and panel.
       IF(ABS(ABS(A*APL+B*BPL+C*CPL)-SQRT((A**2+B**2+C**2)*
     -      (APL**2+BPL**2+CPL**2))).LE.1.0D-6*SQRT((A**2+B**2+C**2)*
     -      (APL**2+BPL**2+CPL**2)))THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ BEMCUT DEBUG   :'',
     -           '' Parallel cut-plane and panel.'')')
            DIST=0
            DO 50 I=1,NPL
            DIST=DIST+A*(XPL(I)-X0)+B*(YPL(I)-Y0)+C*(ZPL(I)-Z0)
50          CONTINUE
            IF(DIST.GE.-EPSD*SQRT(A**2+B**2+C**2))THEN
                 NREFO=1
                 CALL PLABU1('STORE',IREFO(NREFO),NPL,XPL,YPL,ZPL,
     -                APL,BPL,CPL,ICOL,IVOL,IFAIL1)
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ BEMCUT DEBUG   :'',
     -                '' Accepted as a whole.'')')
            ELSE
                 NREFO=0
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ BEMCUT DEBUG   :'',
     -                '' Rejected as a whole.'')')
            ENDIF
            RETURN
       ENDIF
*** Loop over the points to classify them
       DO 10 I=1,NPL
       PROD1=((X0-XPL(I))*A+(Y0-YPL(I))*B+(Z0-ZPL(I))*C)/
     -      SQRT(A**2+B**2+C**2)
       IF(ABS(PROD1).LE.EPSD)THEN
            CLASS(I)=0
       ELSEIF(PROD1.LT.0)THEN
            CLASS(I)=+1
       ELSE
            CLASS(I)=-1
       ENDIF
C      print *,' Point ',I,': class: ',class(i)
10     CONTINUE
*** Add points as appropriate.
       NOUT=0
       DO 40 I=1,NPL
C      print *,' *** Point ',I
*   If inside or on the edge, add point 1
       IF(CLASS(I).GE.0)THEN
            NOUT=NOUT+1
            XOUT(NOUT)=XPL(I)
            YOUT(NOUT)=YPL(I)
            ZOUT(NOUT)=ZPL(I)
       ENDIF
*   If going in or out, add the crossing point.
       IF(CLASS(I)*CLASS(1+MOD(I,NPL)).EQ.-1)THEN
            PROD1=(X0               -XPL(I))*A+
     -            (Y0               -YPL(I))*B+
     -            (Z0               -ZPL(I))*C
            PROD2=(XPL(1+MOD(I,NPL))-XPL(I))*A+
     -            (YPL(1+MOD(I,NPL))-YPL(I))*B+
     -            (ZPL(1+MOD(I,NPL))-ZPL(I))*C
*   Check for midpoint crossing
            IF(PROD2**2.GT.1.0D-12*(A**2+B**2+C**2)*(
     -           (XPL(1+MOD(I,NPL))-XPL(I))**2+
     -           (YPL(1+MOD(I,NPL))-YPL(I))**2+
     -           (ZPL(1+MOD(I,NPL))-ZPL(I))**2))THEN
                 XLAM=PROD1/PROD2
            ELSE
      print *,' !!! prod2 = 0'
                 XLAM=0.5
            ENDIF
C      print *,'     lambda = ',xlam
            XLAM=MAX(0.0D0,MIN(1.0D0,XLAM))
            NOUT=NOUT+1
            XOUT(NOUT)=XPL(I)+XLAM*(XPL(1+MOD(I,NPL))-XPL(I))
            YOUT(NOUT)=YPL(I)+XLAM*(YPL(1+MOD(I,NPL))-YPL(I))
            ZOUT(NOUT)=ZPL(I)+XLAM*(ZPL(1+MOD(I,NPL))-ZPL(I))
       ENDIF
40     CONTINUE
*** Copy back the result.
       CALL BEMCHK(NOUT,XOUT,YOUT,ZOUT,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ BEMCUT DEBUG   :'',
     -           '' Did not pass BEMCHK.'')')
       ELSE
            NREFO=NREFO+1
            CALL PLABU1('STORE',IREFO(NREFO),NOUT,XOUT,YOUT,ZOUT,
     -           APL,BPL,CPL,ICOL,IVOL,IFAIL1)
            IF(LDEBUG)THEN
                 WRITE(LUNOUT,'(''  ++++++ BEMCUT DEBUG   : '',
     -                ''Stored after applying cuts.''/
     -                26X,''Panel identifier: '',I5/
     -                26X,''Polygon:'')') IREFO(NREFO)
                 DO 70 I=1,NOUT
                 WRITE(LUNOUT,'(26X,3F15.8)') XOUT(I),YOUT(I),ZOUT(I)
70               CONTINUE
            ENDIF
       ENDIF
       END
