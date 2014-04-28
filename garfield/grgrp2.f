CDECK  ID>, GRGRP2.
       SUBROUTINE GRGRP2(X,Y,N,XTEXT,YTEXT,TITLE)
*-----------------------------------------------------------------------
*   GRGRP2 - Routine plotting a graph of the points (X,Y).
*   GRGRS2 - Sets the scale of the next graph to be plotted.
*   VARIABLES : X         : x-coordinates of plot points.
*               Y         : y-coordinates of plot points.
*               N         : Number of plot points.
*               XTEXT     : Text along the x-axis.
*               YTEXT     : Text along the y-axis.
*   (Last changed on  4/10/99.)
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
       REAL USERX0,USERX1,USERY0,USERY1,FRXMIN,FRXMAX,FRYMIN,FRYMAX,
     -      ARRANG,ARRLEN,BARFRC,DISPX0,DISPX1,DISPY0,DISPY1,
     -      GPXN,GPXN10,GPYN,GPYN10,GPXL,GPYL,GPXT
       LOGICAL LGRID,LGRALL,LOGX,LOGY,LSTAMP,LGCLRB,LGCLRA,
     -      LWAITA,LWAITB,LXCCH,LGLCLP,LGMCLP,LGACLP,LGTCLP,
     -      WKMULT(MXWKLS)
       INTEGER NWK,WKID(MXWKLS),WKCON(MXWKLS),WKFREF(MXWKLS),
     -         WKLUN(MXWKLS),WKSTAT(MXWKLS),WKSREQ(MXWKLS),
     -         NCWKNM(MXWKLS),NCSTMP,IGHIST,IGBAR,NCGKS
       CHARACTER*20 WKNAME(MXWKLS),WKATTR(MXWKLS)
       CHARACTER*80 STAMP
       CHARACTER*(MXNAME) GKSLOG
       COMMON /GRADAT/ USERX0,USERX1,USERY0,USERY1,ARRANG,ARRLEN,
     -      BARFRC,
     -      FRXMIN,FRXMAX,FRYMIN,FRYMAX,DISPX0,DISPX1,DISPY0,DISPY1,
     -      GPXN,GPXN10,GPYN,GPYN10,GPXL,GPYL,GPXT,
     -      LGRID,LGRALL,LOGX,LOGY,LSTAMP,LGCLRB,LGCLRA,LWAITA,LWAITB,
     -      LXCCH,LGLCLP,LGMCLP,LGACLP,LGTCLP,
     -      NWK,WKID,WKCON,WKFREF,WKLUN,WKSTAT,WKSREQ,NCWKNM,NCSTMP,
     -      IGHIST,IGBAR,NCGKS,WKMULT
       COMMON /GRACHR/ WKNAME,WKATTR,STAMP,GKSLOG
       CHARACTER*(*) XTEXT,YTEXT,TITLE
       DOUBLE PRECISION X(*),Y(*),XMIN,YMIN,XMAX,YMAX,
     -      SCMIN,SCMAX,SCMINI,SCMAXI
       REAL XMINR,YMINR,XMAXR,YMAXR
       LOGICAL FORCE,XSET,YSET,XFLAG,YFLAG
       INTEGER I,N
       SAVE FORCE,SCMIN,SCMAX
       DATA FORCE/.FALSE./
       DATA SCMIN/0.0D0/,SCMAX/0.0D0/
*** Determine boundaries of plots.
       XSET=.FALSE.
       YSET=.FALSE.
       XFLAG=.FALSE.
       YFLAG=.FALSE.
       DO 10 I=1,N
       IF((LOGX.AND.X(I).GT.0).OR..NOT.LOGX)THEN
            IF(XSET)THEN
                 XMIN=MIN(XMIN,X(I))
                 XMAX=MAX(XMAX,X(I))
            ELSE
                 XMIN=X(I)
                 XMAX=X(I)
                 XSET=.TRUE.
            ENDIF
       ELSE
            XFLAG=.TRUE.
       ENDIF
       IF((LOGY.AND.Y(I).GT.0).OR..NOT.LOGY)THEN
            IF(YSET)THEN
                 YMIN=MIN(YMIN,Y(I))
                 YMAX=MAX(YMAX,Y(I))
            ELSE
                 YMIN=Y(I)
                 YMAX=Y(I)
                 YSET=.TRUE.
            ENDIF
       ELSE
            YFLAG=.TRUE.
       ENDIF
10     CONTINUE
*** Make the scale a bit bigger so that the curve fits nicely.
       IF(LOGX)THEN
            IF(XFLAG)WRITE(10,'(''  !!!!!! GRGRP2 WARNING : Non-pos'',
     -           ''itive x-values found on an x-log plot; ignored.'')')
            IF(.NOT.XSET)THEN
                 PRINT *,' !!!!!! GRGRP2 WARNING : x-Range is'//
     -                ' entirely non-positive although logarithmic'
                 PRINT *,'                         x-scaling'//
     -                ' has been requested; range set to [1,10].'
                 XMIN=1
                 XMAX=10
            ENDIF
            XMINR=REAL(10.0D0**(LOG10(XMIN)-LOG10(XMAX/XMIN)/20.0D0))
            XMAXR=REAL(10.0D0**(LOG10(XMAX)+LOG10(XMAX/XMIN)/20.0D0))
       ELSE
            XMINR=REAL(XMIN-(XMAX-XMIN)/20.0D0)
            XMAXR=REAL(XMAX+(XMAX-XMIN)/20.0D0)
       ENDIF
*** Verify the automatic scaling request.
       IF(FORCE.AND.LOGY.AND.(SCMIN.LE.0.OR.SCMAX.LE.0))THEN
            PRINT *,' !!!!!! GRGRP2 WARNING : The specified y-scale'//
     -           ' is not valid as a log scale; using default.'
            FORCE=.FALSE.
       ENDIF
       IF(FORCE.AND.SCMIN.EQ.SCMAX)THEN
            PRINT *,' !!!!!! GRGRP2 WARNING : The specified y-scale'//
     -           ' has zero range; using default.'
            FORCE=.FALSE.
       ENDIF
*** Override default scale by forced scale if applicable.
       IF(FORCE)THEN
            YMINR=SCMIN
            YMAXR=SCMAX
            FORCE=.FALSE.
*   And handle the y range the same way as the x range
       ELSEIF(LOGY)THEN
            IF(YFLAG)WRITE(10,'(''  !!!!!! GRGRP2 WARNING : Non-pos'',
     -           ''itive y-values found on a y-log plot; ignored.'')')
            IF(.NOT.YSET)THEN
                 PRINT *,' !!!!!! GRGRP2 WARNING : y-Range is'//
     -                ' entirely non-positive although logarithmic'
                 PRINT *,'                         y-scaling'//
     -                ' has been requested; range set to [1,10].'
                 YMIN=1
                 YMAX=10
            ENDIF
            YMINR=REAL(10.0D0**(LOG10(YMIN)-LOG10(YMAX/YMIN)/20.0D0))
            YMAXR=REAL(10.0D0**(LOG10(YMAX)+LOG10(YMAX/YMIN)/20.0D0))
       ELSE
            YMINR=REAL(YMIN-(YMAX-YMIN)/20.0)
            YMAXR=REAL(YMAX+(YMAX-YMIN)/20.0)
       ENDIF
*** Plot the coordinate axes.
       CALL GRCART(XMINR,YMINR,XMAXR,YMAXR,XTEXT,YTEXT,TITLE)
*** Plot the line.
       CALL GRATTS('FUNCTION-1','POLYLINE')
       IF(N.GT.1)CALL GRLIN2(N,X,Y)
       RETURN
*** Entry point to force a scale.
       ENTRY GRGRS2(SCMINI,SCMAXI)
       FORCE=.TRUE.
       SCMIN=MIN(SCMINI,SCMAXI)
       SCMAX=MAX(SCMINI,SCMAXI)
       END
