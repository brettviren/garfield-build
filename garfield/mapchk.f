CDECK  ID>, MAPCHK.
       SUBROUTINE MAPCHK(IFAIL)
*-----------------------------------------------------------------------
*   MAPCHK - Checks the element aspect ratio and measure range.
*   (Last changed on 14/07/08)
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
       REAL DMIN,DMAX,DIST,SMIN,SMAX,SURF,RMIN,RMAX,STEP
       INTEGER I,J,K,NP,NCHA,IFAIL1,IFAIL2,IASP,IVOL,IFAIL
*** By default, this should work.
       IFAIL=0
*** Ensure there are some triangles / tetrahedrons.
       IF(NMAP.LE.0)THEN
            PRINT *,' !!!!!! MAPCHK WARNING : No elements in the'//
     -           ' current map ; histograms not made, map rejected.'
            IFAIL=1
            RETURN
       ENDIF
*** Compute the range of volumes.
       DO 40 I=1,NMAP
       IF(MAPTYP.EQ.1.OR.MAPTYP.EQ.2.OR.MAPTYP.EQ.3)THEN
            SURF=ABS(
     -           (XMAP(I,3)-XMAP(I,1))*(YMAP(I,2)-YMAP(I,1))-
     -           (YMAP(I,3)-YMAP(I,1))*(XMAP(I,2)-XMAP(I,1)))/2
       ELSEIF(MAPTYP.EQ.4.OR.MAPTYP.EQ.5)THEN
            SURF=(ABS((XMAP(I,2)-XMAP(I,1))*
     -                (YMAP(I,3)-YMAP(I,1))-
     -                (XMAP(I,3)-XMAP(I,1))*
     -                (YMAP(I,2)-YMAP(I,1)))+
     -            ABS((XMAP(I,4)-XMAP(I,1))*
     -                (YMAP(I,3)-YMAP(I,1))-
     -                (XMAP(I,3)-XMAP(I,1))*
     -                (YMAP(I,4)-YMAP(I,1))))/2
       ELSEIF(MAPTYP.EQ.11.OR.MAPTYP.EQ.12.OR.MAPTYP.EQ.13)THEN
            SURF=ABS(
     -           (XMAP(I,4)-XMAP(I,1))*(
     -           (YMAP(I,2)-YMAP(I,1))*(ZMAP(I,3)-ZMAP(I,1))-
     -           (YMAP(I,3)-YMAP(I,1))*(ZMAP(I,2)-ZMAP(I,1)))+
     -           (YMAP(I,4)-YMAP(I,1))*(
     -           (ZMAP(I,2)-ZMAP(I,1))*(XMAP(I,3)-XMAP(I,1))-
     -           (ZMAP(I,3)-ZMAP(I,1))*(XMAP(I,2)-XMAP(I,1)))+
     -           (ZMAP(I,4)-ZMAP(I,1))*(
     -           (XMAP(I,2)-XMAP(I,1))*(YMAP(I,3)-YMAP(I,1))-
     -           (XMAP(I,3)-XMAP(I,1))*(YMAP(I,2)-YMAP(I,1))))/6
       ELSEIF(MAPTYP.EQ.14.OR.MAPTYP.EQ.15.OR.MAPTYP.EQ.16)THEN
            SURF=ABS(
     -           (XMAP(I,4)-XMAP(I,1))*(
     -           (YMAP(I,2)-YMAP(I,1))*(ZMAP(I,3)-ZMAP(I,1))-
     -           (YMAP(I,3)-YMAP(I,1))*(ZMAP(I,2)-ZMAP(I,1)))+
     -           (YMAP(I,4)-YMAP(I,1))*(
     -           (ZMAP(I,2)-ZMAP(I,1))*(XMAP(I,3)-XMAP(I,1))-
     -           (ZMAP(I,3)-ZMAP(I,1))*(XMAP(I,2)-XMAP(I,1)))+
     -           (ZMAP(I,4)-ZMAP(I,1))*(
     -           (XMAP(I,2)-XMAP(I,1))*(YMAP(I,3)-YMAP(I,1))-
     -           (XMAP(I,3)-XMAP(I,1))*(YMAP(I,2)-YMAP(I,1))))
       ELSE
            SURF=0
       ENDIF
       IF(I.EQ.0)THEN
            SMIN=SURF
            SMAX=SURF
       ELSE
            SMIN=MIN(SMIN,SURF)
            SMAX=MAX(SMAX,SURF)
       ENDIF
40     CONTINUE
*** Number of bins.
       NCHA=MIN(100,MXCHA)
*** Check we do have a range and round it.
       SMIN=MAX(0.0,SMIN-0.1*(SMAX-SMIN))
       SMAX=SMAX+0.1*(SMAX-SMIN)
       IF(SMIN.EQ.SMAX)THEN
            SMIN=SMIN-(1+ABS(SMIN))
            SMAX=SMAX+(1+ABS(SMAX))
       ENDIF
       CALL ROUND(SMIN,SMAX,NCHA,'LARGER,COARSER',STEP)
*** Book histograms.
       CALL HISADM('ALLOCATE',IASP,NCHA,0.0,100.0,.FALSE.,IFAIL1)
       CALL HISADM('ALLOCATE',IVOL,NCHA,SMIN,SMAX,.FALSE.,IFAIL2)
       IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)THEN
            PRINT *,' !!!!!! MAPCHK WARNING : Unable to allocate'//
     -           ' histograms ; no check done.'
            CALL HISADM('DELETE',IASP,NCHA,0.0,100.0,.FALSE.,IFAIL1)
            CALL HISADM('DELETE',IVOL,NCHA,0.0,100.0,.FALSE.,IFAIL2)
            RETURN
       ENDIF
*** Set the number of vertices.
       IF(MAPTYP.EQ.1.OR.MAPTYP.EQ.2.OR.MAPTYP.EQ.3)THEN
            NP=3
       ELSEIF(MAPTYP.EQ.4)THEN
            NP=4
       ELSEIF(MAPTYP.EQ.5)THEN
            NP=8
       ELSEIF(MAPTYP.EQ.11.OR.MAPTYP.EQ.12)THEN
            NP=4
       ELSEIF(MAPTYP.EQ.13)THEN
            NP=10
       ELSEIF(MAPTYP.EQ.14.OR.MAPTYP.EQ.15.OR.MAPTYP.EQ.16)THEN
            NP=8
       ELSE
            NP=0
       ENDIF
*** Loop over all triangles or tetrahedrons.
       DO 10 I=1,NMAP
*   And over all pairs of vertices.
       DO 20 J=1,NP-1
       DO 30 K=J+1,NP
*   Compute distance.
       DIST=SQRT((XMAP(I,J)-XMAP(I,K))**2+(YMAP(I,J)-YMAP(I,K))**2+
     -      (ZMAP(I,J)-ZMAP(I,K))**2)
*   And update maximum/minimum.
       IF(K.EQ.2)THEN
            DMIN=DIST
            DMAX=DIST
       ELSE
            DMIN=MIN(DMIN,DIST)
            DMAX=MAX(DMAX,DIST)
       ENDIF
*   Next vertex pair.
30     CONTINUE
20     CONTINUE
*   Check for null-sizes.
       IF(DMIN.LE.0.AND..NOT.ELMDGN(I))THEN
            PRINT *,' !!!!!! MAPCHK WARNING : Found a shape with a'//
     -           ' zero-length vertex separation; map rejected.'
            IFAIL=1
            GOTO 10
       ENDIF
*   Histogramming.
       CALL HISENT(IASP,DMAX/DMIN,1.0)
**  Compute the surface or volume.
       IF(MAPTYP.EQ.1.OR.MAPTYP.EQ.2.OR.MAPTYP.EQ.3)THEN
            SURF=ABS(
     -           (XMAP(I,3)-XMAP(I,1))*(YMAP(I,2)-YMAP(I,1))-
     -           (YMAP(I,3)-YMAP(I,1))*(XMAP(I,2)-XMAP(I,1)))/2
       ELSEIF(MAPTYP.EQ.4.OR.MAPTYP.EQ.5)THEN
            SURF=(ABS((XMAP(I,2)-XMAP(I,1))*
     -                (YMAP(I,3)-YMAP(I,1))-
     -                (XMAP(I,3)-XMAP(I,1))*
     -                (YMAP(I,2)-YMAP(I,1)))+
     -            ABS((XMAP(I,4)-XMAP(I,1))*
     -                (YMAP(I,3)-YMAP(I,1))-
     -                (XMAP(I,3)-XMAP(I,1))*
     -                (YMAP(I,4)-YMAP(I,1))))/2
       ELSEIF(MAPTYP.EQ.11.OR.MAPTYP.EQ.12.OR.MAPTYP.EQ.13)THEN
            SURF=ABS(
     -           (XMAP(I,4)-XMAP(I,1))*(
     -           (YMAP(I,2)-YMAP(I,1))*(ZMAP(I,3)-ZMAP(I,1))-
     -           (YMAP(I,3)-YMAP(I,1))*(ZMAP(I,2)-ZMAP(I,1)))+
     -           (YMAP(I,4)-YMAP(I,1))*(
     -           (ZMAP(I,2)-ZMAP(I,1))*(XMAP(I,3)-XMAP(I,1))-
     -           (ZMAP(I,3)-ZMAP(I,1))*(XMAP(I,2)-XMAP(I,1)))+
     -           (ZMAP(I,4)-ZMAP(I,1))*(
     -           (XMAP(I,2)-XMAP(I,1))*(YMAP(I,3)-YMAP(I,1))-
     -           (XMAP(I,3)-XMAP(I,1))*(YMAP(I,2)-YMAP(I,1))))/6
       ELSEIF(MAPTYP.EQ.14.OR.MAPTYP.EQ.15.OR.MAPTYP.EQ.16)THEN
            SURF=ABS(
     -           (XMAP(I,4)-XMAP(I,1))*(
     -           (YMAP(I,2)-YMAP(I,1))*(ZMAP(I,3)-ZMAP(I,1))-
     -           (YMAP(I,3)-YMAP(I,1))*(ZMAP(I,2)-ZMAP(I,1)))+
     -           (YMAP(I,4)-YMAP(I,1))*(
     -           (ZMAP(I,2)-ZMAP(I,1))*(XMAP(I,3)-XMAP(I,1))-
     -           (ZMAP(I,3)-ZMAP(I,1))*(XMAP(I,2)-XMAP(I,1)))+
     -           (ZMAP(I,4)-ZMAP(I,1))*(
     -           (XMAP(I,2)-XMAP(I,1))*(YMAP(I,3)-YMAP(I,1))-
     -           (XMAP(I,3)-XMAP(I,1))*(YMAP(I,2)-YMAP(I,1))))
       ELSE
            SURF=0
       ENDIF
*   Check for null-sizes.
       IF(SURF.LE.0)THEN
            PRINT *,' !!!!!! MAPCHK WARNING : Found a shape with a'//
     -           ' zero surface or volume; map rejected.'
            IFAIL=1
            GOTO 10
       ENDIF
*   Histogramming.
       CALL HISENT(IVOL,SURF,1.0)
*   Update maxima and minima.
       IF(I.EQ.1)THEN
            SMIN=SURF
            SMAX=SURF
            RMIN=DMAX/DMIN
            RMAX=DMAX/DMIN
       ELSE
            SMIN=MIN(SMIN,SURF)
            SMAX=MAX(SMAX,SURF)
            RMIN=MIN(RMIN,DMAX/DMIN)
            RMAX=MAX(RMAX,DMAX/DMIN)
       ENDIF
*   Next triangle or tetrahedron.
10     CONTINUE
*** Final output, aspect ratio plot.
C       CALL GRAOPT('LOG-Y')
       CALL HISPLT(IASP,'Largest / smallest vertex distance',
     -      'Aspect ratio',.TRUE.)
       CALL GRNEXT
       CALL GRALOG('Aspect ratio histogram')
       CALL HISADM('DELETE',IASP,0,0.0,0.0,.FALSE.,IFAIL1)
C       CALL GRAOPT('LIN-Y')
*   Volumes.
       CALL GRAOPT('LOG-Y')
       CALL HISPLT(IVOL,'Surface [cm2] or Volume [cm3]',
     -      'Element measure',.TRUE.)
       CALL GRNEXT
       CALL GRALOG('Element measure')
       CALL HISADM('DELETE',IVOL,0,0.0,0.0,.FALSE.,IFAIL2)
       CALL GRAOPT('LIN-Y')
*   Printout.
       WRITE(LUNOUT,'(''  Aspect ratios: ''/
     -      5X,''Smallest: '',F10.3/5X,''Largest:  '',F10.3/
     -      ''  Volumes or Surfaces: ''/
     -      5X,''Smallest: '',E10.3/5X,''Largest:  '',E10.3)')
     -      RMIN,RMAX,SMIN,SMAX
*** Record the time needed.
       CALL TIMLOG('Checking the mesh')
       END
