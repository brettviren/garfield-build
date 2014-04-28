CDECK  ID>, GRSPLN.
       SUBROUTINE GRSPLN(NU,XU,YU)
*-----------------------------------------------------------------------
*   GRSPLN - Plots a smooth line through a set of points.
*   (Last changed on 12/ 8/96.)
*-----------------------------------------------------------------------
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
       INTEGER NU,IFAIL,I
       REAL XU(*),YU(*),XPL(MXLIST),YPL(MXLIST),Z(MXLIST),C(MXLIST)
*** Check number of points.
       IF(NU.LE.1)THEN
            WRITE(10,'(''  !!!!!! GRSPLN WARNING : Insufficient'',
     -           '' number ('',I3,'') of points on line; not'',
     -           '' plotted.'')') NU
            RETURN
       ELSEIF(NU.GT.MXLIST)THEN
            WRITE(10,'(''  !!!!!! GRSPLN WARNING : Too many points'',
     -           '' ('',I3,'') on line; not plotted.'')') NU
            RETURN
       ENDIF
*** Prepare interpolation vector.
       DO 10 I=1,NU
       Z(I)=1+REAL(MXLIST-1)*REAL(I-1)/REAL(NU-1)
10     CONTINUE
*** Prepare x-spline interpolation.
       CALL SPLINE(Z,XU,C,NU,IFAIL)
       IF(IFAIL.NE.0)THEN
            WRITE(10,'(''  !!!!!! GRSPLN WARNING : Preparation of'',
     -           '' x-spline failed; line not plotted.'')')
            RETURN
       ENDIF
*** Perform x-spline interpolation.
       DO 20 I=1,MXLIST
       IF(I.EQ.1)THEN
            XPL(I)=XU(1)
       ELSEIF(I.EQ.MXLIST)THEN
            XPL(I)=XU(NU)
       ELSE
            CALL INTERP(Z,XU,C,N,REAL(I),XPL(I),IFAIL)
            IF(IFAIL.NE.0)THEN
                 WRITE(10,'(''  !!!!!! GRSPLN WARNING : Interpolating'',
     -                '' x-spline failed; line not plotted.'')')
                 RETURN
            ENDIF
       ENDIF
20     CONTINUE
*** Prepare y-spline interpolation.
       CALL SPLINE(Z,YU,C,NU,IFAIL)
       IF(IFAIL.NE.0)THEN
            WRITE(10,'(''  !!!!!! GRSPLN WARNING : Preparation of'',
     -           '' y-spline failed; line not plotted.'')')
            RETURN
       ENDIF
*** Perform x-spline interpolation.
       DO 30 I=1,MXLIST
       IF(I.EQ.1)THEN
            YPL(I)=YU(1)
       ELSEIF(I.EQ.MXLIST)THEN
            YPL(I)=YU(NU)
       ELSE
            CALL INTERP(Z,YU,C,N,REAL(I),YPL(I),IFAIL)
            IF(IFAIL.NE.0)THEN
                 WRITE(10,'(''  !!!!!! GRSPLN WARNING : Interpolating'',
     -                '' y-spline failed; line not plotted.'')')
                 RETURN
            ENDIF
       ENDIF
30     CONTINUE
*** Plot the curve.
       CALL GRLINE(MXLIST,XPL,YPL)
       END
