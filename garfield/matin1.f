CDECK  ID>, MATIN1.
       SUBROUTINE MATIN1(IRVEC1,IRVEC2,N,X,Y,ISVEC1,ISVEC2,IORD,IFAIL)
*-----------------------------------------------------------------------
*   MATIN1 - Interpolates two vectors.
*   (Last changed on 19/ 9/00.)
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
       INTEGER IRVEC1,IRVEC2,ISVEC1,ISVEC2,IFAIL,I,N,MATSLT,IORD
       REAL X(*),Y(*)
       EXTERNAL MATSLT
       REAL DIVDIF
       EXTERNAL DIVDIF
*** Indentify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE MATIN1 (CERNLIB) ///'
*** Debugging information.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MATIN1 DEBUG   : Order '',I2,
     -      '' interpolation for '',I5,'' vs '',I5,'' with '',I5,
     -      '' points.'')') IORD,IRVEC1,IRVEC2,N
*** Assume the routine will fail.
       IFAIL=1
*** Check the interpolation order.
       IF(IORD.LT.1)THEN
            PRINT *,' !!!!!! MATIN1 WARNING : Interpolation order'//
     -           ' is not at least 1; no interpolation.'
            RETURN
       ENDIF
*** Locate slots if not already done.
       IF(ISVEC1.LE.0.OR.ISVEC2.LE.0)THEN
*   Find the slot numbers.
            ISVEC1=MATSLT(IRVEC1)
            ISVEC2=MATSLT(IRVEC2)
*   Ensure that the vectors exist.
            IF(ISVEC1.LE.0.OR.ISVEC2.LE.0)THEN
                 PRINT *,' !!!!!! MATIN1 WARNING : Unable to locate'//
     -                ' one of the 2 vectors; no interpolation.'
                 RETURN
            ENDIF
*   Make sure they are indeed vectors.
            IF(MDIM(ISVEC1).NE.1.OR.MDIM(ISVEC2).NE.1.OR.
     -           MLEN(ISVEC1).NE.MLEN(ISVEC2).OR.
     -           MLEN(ISVEC1).LT.2.OR.MLEN(ISVEC2).LT.2)THEN
                 PRINT *,' !!!!!! MATIN1 WARNING : The 2 vectors are'//
     -                ' not 1-dimensional, too short or not compatible.'
                 ISVEC1=-1
                 ISVEC2=-1
                 RETURN
            ENDIF
*   Check that they are properly ordered.
            IF(MVEC(MORG(ISVEC1)+2).GT.MVEC(MORG(ISVEC1)+1))THEN
                 DO 10 I=2,MLEN(ISVEC1)
                 IF(MVEC(MORG(ISVEC1)+I).LE.MVEC(MORG(ISVEC1)+I-1))THEN
                      PRINT *,' !!!!!! MATIN1 WARNING : The ordinate'//
     -                     ' vector is not strictly ordered.'
                      ISVEC1=-1
                      ISVEC2=-1
                      RETURN
                 ENDIF
10               CONTINUE
            ELSEIF(MVEC(MORG(ISVEC1)+2).LT.MVEC(MORG(ISVEC1)+1))THEN
                 DO 20 I=2,MLEN(ISVEC1)
                 IF(MVEC(MORG(ISVEC1)+I).GE.MVEC(MORG(ISVEC1)+I-1))THEN
                      PRINT *,' !!!!!! MATIN1 WARNING : The ordinate'//
     -                     ' vector is not strictly ordered.'
                      ISVEC1=-1
                      ISVEC2=-1
                      RETURN
                 ENDIF
20               CONTINUE
            ELSE
                 PRINT *,' !!!!!! MATIN1 WARNING : The ordinate'//
     -                ' vector is not strictly ordered.'
                 ISVEC1=-1
                 ISVEC2=-1
                 RETURN
            ENDIF
       ENDIF
*** Carry out the interpolation.
       IF(N.LT.1)THEN
            PRINT *,' !!!!!! MATIN1 WARNING : Invalid number of'//
     -           ' points received ; no interpolation.'
            RETURN
       ENDIF
       DO 30 I=1,N
*   Avoid extrapolation.
       IF((MVEC(MORG(ISVEC1)+1)-X(I))*
     -      (MVEC(MORG(ISVEC1)+MLEN(ISVEC1))-X(I)).GT.0)THEN
            Y(I)=0
*   Interpolation.
       ELSE
            Y(I)=DIVDIF(MVEC(MORG(ISVEC2)+1),MVEC(MORG(ISVEC1)+1),
     -           MLEN(ISVEC1),X(I),MIN(IORD,MLEN(ISVEC1)-1))
       ENDIF
30     CONTINUE
*** Seems to have worked.
       IFAIL=0
       END
