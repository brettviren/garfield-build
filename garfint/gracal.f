CDECK  ID>, GRACAL.
       SUBROUTINE GRACAL(INSTR,IFAIL)
*-----------------------------------------------------------------------
*   GRACAL - Handles graphics related calls.
*   (Last changed on  2/ 8/09.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER INS(MXINS,4),ALGENT(MXALGE,10),MODREG(MXCONS:MXREG),
     -      ISYNCH,IINS0,ICONS0,ARGREF(MXARG,2),MODARG(MXARG),
     -      NREG,NCONS,NINS,NERR,NRES,NALGE,IENTRL,NAERR(100)
       REAL REG(MXCONS:MXREG),ARG(MXARG),EXPMAX
       PARAMETER(EXPMAX=40.0)
       LOGICAL EXEC(MXINS),LIGUND,LINUND
       COMMON /ALGDAT/ REG,ARG,MODARG,ARGREF,INS,MODREG,ALGENT,
     -      NREG,NCONS,NINS,NERR,NAERR,
     -      NRES,NALGE,IENTRL,ISYNCH,IINS0,ICONS0,EXEC,LIGUND,LINUND
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,MEV2KG,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      MEV2KG = 1.782661845E-30,
     -      BOLTZ=1.380658E-23)
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
       CHARACTER*256 XTXT,YTXT,TITLE
       REAL XPL(MXARG),YPL(MXARG),SIZE,UPX,UPY,
     -      CPX,CPY,XBOX(5),YBOX(5),YSHIFT
       INTEGER INPCMX,IFAIL,INSTR,IPROC,NARG,IREF(6),ISLOT(6),ISIZ(1),
     -      IFAIL1,IFAIL2,IFAIL3,NC,ILEN,IFORM,MATSLT,NCXTXT,NCYTXT,
     -      NCTIT,I,J,IALHOR,IALVER,IUD,ILR,IVERT,IHOR,ICOL,IPREC,IERR,
     -      IWK
       EXTERNAL INPCMX,MATSLT
*** Indentify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE GRACAL ///'
*** Set a workstation for box size inquiries.
       IWK=1
*** Assume the CALL will fail.
       IFAIL=1
*** Some easy reference variables.
       NARG=INS(INSTR,3)
       IPROC=INS(INSTR,1)
*** Open a plot frame.
       IF(IPROC.EQ.-801)THEN
*   Check number of arguments.
            IF(NARG.LT.4.OR.NARG.GT.7)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect number'//
     -                ' of arguments for PLOT_FRAME.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           MODARG(3).NE.2.OR.MODARG(4).NE.2.OR.
     -           (NARG.GE.5.AND.MODARG(5).NE.1).OR.
     -           (NARG.GE.6.AND.MODARG(6).NE.1).OR.
     -           (NARG.GE.7.AND.MODARG(7).NE.1))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Some arguments of'//
     -                ' PLOT_FRAME are of incorrect type.'
                 RETURN
            ENDIF
*   Carry out the calculation.
            IF(NARG.GE.5)THEN
                 CALL STRBUF('READ',NINT(ARG(5)),XTXT,NCXTXT,IFAIL1)
                 IF(NCXTXT.LT.1)THEN
                      XTXT=' '
                      NCXTXT=1
                 ENDIF
            ELSE
                 XTXT='x'
                 NCXTXT=1
                 IFAIL1=0
            ENDIF
            IF(NARG.GE.6)THEN
                 CALL STRBUF('READ',NINT(ARG(6)),YTXT,NCYTXT,IFAIL2)
                 IF(NCYTXT.LT.1)THEN
                      YTXT=' '
                      NCYTXT=1
                 ENDIF
            ELSE
                 YTXT='y'
                 NCYTXT=1
                 IFAIL2=0
            ENDIF
            IF(NARG.GE.7)THEN
                 CALL STRBUF('READ',NINT(ARG(7)),TITLE,NCTIT,IFAIL3)
                 IF(NCTIT.LT.1)THEN
                      TITLE=' '
                      NCTIT=1
                 ENDIF
            ELSE
                 TITLE=' '
                 NCTIT=1
                 IFAIL3=0
            ENDIF
            CALL GRCART(ARG(1),ARG(2),ARG(3),ARG(4),
     -           XTXT(1:NCXTXT),YTXT(1:NCYTXT),TITLE(1:NCTIT))
*   Switch back to normal screen.
            CALL GRALPH
*   Error processing.
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0)
     -           PRINT *,' !!!!!! GRACAL WARNING : Error'//
     -           ' retrieving a string for PLOT_FRAME.'
*** Close a plot frame.
      ELSEIF(IPROC.EQ.-802)THEN
*   Check number of arguments.
            IF(NARG.GT.1)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect number'//
     -                ' of arguments for PLOT_END.'
                 RETURN
            ENDIF
*   If the last argument is present, fetch it (log record).
            IF(NARG.GE.1)THEN
                 CALL STRBUF('READ',NINT(ARG(1)),TITLE,NCTIT,IFAIL1)
            ELSE
                 TITLE='< User plot >'
                 NCTIT=13
            ENDIF
*   Log the plot.
            IF(NCTIT.GE.1)CALL GRALOG(TITLE(1:NCTIT))
*   Switch to graphics.
            CALL GRGRAF(.FALSE.)
*   Close graphics.
            CALL GRNEXT
*** Plot a marker.
      ELSEIF(IPROC.EQ.-803)THEN
*   Check number of arguments.
            IF(NARG.EQ.1.OR.
     -           (NARG.NE.2*(NARG/2).AND.MODARG(NARG).NE.1))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect set of'//
     -                ' arguments for PLOT_MARKERS.'
                 RETURN
            ENDIF
*   Check argument mode.
            IF(MODARG(1).NE.5)THEN
                 DO 45 I=1,2*(NARG/2)
                 IF(MODARG(I).NE.2)THEN
                      PRINT *,' !!!!!! GRACAL WARNING : Incorrect'//
     -                     ' argument type in PLOT_MARKERS call.'
                      RETURN
                 ENDIF
45               CONTINUE
            ELSEIF(MODARG(2).NE.5)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect'//
     -                ' argument type in PLOT_MARKERS call.'
                 RETURN
            ENDIF
*   Switch to graphics screen.
            CALL GRGRAF(.FALSE.)
*   If there is a 3rd argument, set the polymarker type.
            IF(NARG.NE.2*(NARG/2))THEN
                 CALL STRBUF('READ',NINT(ARG(NARG)),TITLE,NCTIT,IFAIL1)
                 IF(NCTIT.GE.1)THEN
                      CALL CLTOU(TITLE(1:NCTIT))
                      CALL GRATTS(TITLE(1:NCTIT),'POLYMARKER')
                 ENDIF
            ELSE
                 CALL GRATTS('CIRCLE','POLYMARKER')
                 IFAIL1=0
            ENDIF
*   Plot the markers.
            IF(MODARG(1).NE.5)THEN
                 DO 55 I=1,NARG/2
                 XPL(I)=ARG(2*I-1)
                 YPL(I)=ARG(2*I)
55               CONTINUE
                 CALL GRMARK(NARG/2,XPL,YPL)
            ELSE
                 CALL MATMRK(NINT(ARG(1)),NINT(ARG(2)),' ')
            ENDIF
*   Switch back to alphanumeric screen.
            CALL GRALPH
*   Error processing.
            IF(IFAIL1.NE.0)PRINT *,' !!!!!! GRACAL WARNING : Error'//
     -           ' retrieving a string for PLOT_MARKERS.'
*** Plot a polyline.
      ELSEIF(IPROC.EQ.-804)THEN
*   Check number of arguments.
            IF(NARG.EQ.1.OR.
     -           (NARG.LE.3.AND.(MODARG(1).NE.5.OR.MODARG(2).NE.5)).OR.
     -           (NARG.NE.2*(NARG/2).AND.MODARG(NARG).NE.1))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect set of'//
     -                ' arguments for PLOT_LINE.'
                 RETURN
            ENDIF
*   Check argument mode.
            IF(NARG.GE.4)THEN
                 DO 40 I=1,2*(NARG/2)
                 IF(MODARG(I).NE.2)THEN
                      PRINT *,' !!!!!! GRACAL WARNING : Incorrect'//
     -                     ' argument type in PLOT_LINE call.'
                      RETURN
                 ENDIF
40               CONTINUE
            ENDIF
*   Switch to graphics screen.
            CALL GRGRAF(.FALSE.)
*   If there is a 3rd argument, set the polyline type.
            IF(NARG.NE.2*(NARG/2))THEN
                 CALL STRBUF('READ',NINT(ARG(NARG)),TITLE,NCTIT,IFAIL1)
                 IF(NCTIT.LT.1)THEN
                      TITLE=' '
                      NCTIT=1
                 ENDIF
                 CALL CLTOU(TITLE(1:NCTIT))
            ELSE
                 TITLE='SOLID'
                 NCTIT=5
                 IFAIL1=0
            ENDIF
            IF(INDEX(TITLE(1:NCTIT),'SOLID').NE.0)THEN
                 CALL GRATTS('SOLID','POLYLINE')
            ELSEIF(INDEX(TITLE(1:NCTIT),'COMMENT').NE.0)THEN
                 CALL GRATTS('COMMENT','POLYLINE')
            ELSEIF(INDEX(TITLE(1:NCTIT),'DASHED').NE.0)THEN
                 CALL GRATTS('DASHED','POLYLINE')
            ELSEIF(INDEX(TITLE(1:NCTIT),'DOTTED').NE.0)THEN
                 CALL GRATTS('DOTTED','POLYLINE')
            ELSEIF(INDEX(TITLE(1:NCTIT),'DASH-DOTTED').NE.0)THEN
                 CALL GRATTS('DASH-DOTTED','POLYLINE')
            ELSEIF(INDEX(TITLE(1:NCTIT),'FUNCTION-1').NE.0)THEN
                 CALL GRATTS('FUNCTION-1','POLYLINE')
            ELSEIF(INDEX(TITLE(1:NCTIT),'FUNCTION-2').NE.0)THEN
                 CALL GRATTS('FUNCTION-2','POLYLINE')
            ELSEIF(INDEX(TITLE(1:NCTIT),'FUNCTION-3').NE.0)THEN
                 CALL GRATTS('FUNCTION-3','POLYLINE')
            ELSEIF(INDEX(TITLE(1:NCTIT),'FUNCTION-4').NE.0)THEN
                 CALL GRATTS('FUNCTION-4','POLYLINE')
            ELSEIF(INDEX(TITLE(1:NCTIT),'FUNCTION-5').NE.0)THEN
                 CALL GRATTS('FUNCTION-5','POLYLINE')
            ELSEIF(INDEX(TITLE(1:NCTIT),'FUNCTION-6').NE.0)THEN
                 CALL GRATTS('FUNCTION-6','POLYLINE')
            ELSEIF(INDEX(TITLE(1:NCTIT),'FUNCTION-7').NE.0)THEN
                 CALL GRATTS('FUNCTION-7','POLYLINE')
            ELSEIF(INDEX(TITLE(1:NCTIT),'ERROR-BAR').NE.0)THEN
                 CALL GRATTS('ERROR-BAR','POLYLINE')
            ELSEIF(INDEX(TITLE(1:NCTIT),'BOX-TICKMARKS').NE.0)THEN
                 CALL GRATTS('BOX-TICKMARKS','POLYLINE')
            ELSE
                 CALL GRATTS('SOLID','POLYLINE')
            ENDIF
*   Plot the line segment.
            IF(NARG.GE.4)THEN
                 DO 50 I=1,NARG/2
                 XPL(I)=ARG(2*I-1)
                 YPL(I)=ARG(2*I)
50               CONTINUE
                 IF(INDEX(TITLE(1:NCTIT),'SMOOTH').NE.0.AND.
     -                INDEX(TITLE(1:NCTIT),'NOSMOOTH').EQ.0)THEN
                      CALL GRSPLN(NARG/2,XPL,YPL)
                 ELSE
                      CALL GRLINE(NARG/2,XPL,YPL)
                 ENDIF
            ELSE
                 IF(INDEX(TITLE(1:NCTIT),'SMOOTH').NE.0.AND.
     -                INDEX(TITLE(1:NCTIT),'NOSMOOTH').EQ.0)THEN
                      CALL MATLIN(NINT(ARG(1)),NINT(ARG(2)),'SMOOTH')
                 ELSE
                      CALL MATLIN(NINT(ARG(1)),NINT(ARG(2)),' ')
                 ENDIF
            ENDIF
*   Switch back to alphanumeric screen.
            CALL GRALPH
*   Error processing.
            IF(IFAIL1.NE.0)PRINT *,' !!!!!! GRACAL WARNING : Error'//
     -           ' retrieving a string for PLOT_LINE.'
*** Plot a string.
      ELSEIF(IPROC.EQ.-805)THEN
*   Check number of arguments.
            IF(NARG.LT.3.OR.NARG.GT.6)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect number'//
     -                ' of arguments for PLOT_TEXT.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           MODARG(3).NE.1.OR.
     -           (NARG.GE.4.AND.MODARG(4).NE.1).OR.
     -           (NARG.GE.5.AND.MODARG(5).NE.1).OR.
     -           (NARG.GE.6.AND.MODARG(6).NE.2))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Some arguments of'//
     -                ' PLOT_TEXT are of incorrect type.'
                 RETURN
            ENDIF
*   Switch to graphics screen.
            CALL GRGRAF(.FALSE.)
*   If there is a 4th argument, set the text type.
            IF(NARG.GE.4)THEN
                 CALL STRBUF('READ',NINT(ARG(4)),TITLE,NCTIT,IFAIL1)
                 IF(NCTIT.GE.1)THEN
                      CALL CLTOU(TITLE(1:NCTIT))
                      CALL GRATTS(TITLE(1:NCTIT),'TEXT')
                 ENDIF
            ELSE
                 CALL GRATTS('COMMENT','TEXT')
                 IFAIL1=0
            ENDIF
*   If there is a 5th argument, set the text alignment.
            IF(NARG.GE.5)THEN
                 CALL STRBUF('READ',NINT(ARG(5)),TITLE,NCTIT,IFAIL2)
                 IF(NCTIT.LT.1)THEN
                      TITLE=' '
                      NCTIT=1
                 ENDIF
                 CALL CLTOU(TITLE(1:NCTIT))
                 IF(INDEX(TITLE(1:NCTIT),'LEFT').NE.0)THEN
                      IALHOR=1
                 ELSEIF(INDEX(TITLE(1:NCTIT),'CENTER')+
     -                INDEX(TITLE(1:NCTIT),'CENTRE').NE.0)THEN
                      IALHOR=2
                 ELSEIF(INDEX(TITLE(1:NCTIT),'RIGHT').NE.0)THEN
                      IALHOR=3
                 ELSEIF(INDEX(TITLE(1:NCTIT),'NORMAL').NE.0)THEN
                      IALHOR=0
                 ELSE
                      IALHOR=0
                 ENDIF
                 IF(INDEX(TITLE(1:NCTIT),'TOP').NE.0)THEN
                      IALVER=1
                 ELSEIF(INDEX(TITLE(1:NCTIT),'CAP').NE.0)THEN
                      IALVER=2
                 ELSEIF(INDEX(TITLE(1:NCTIT),'HALF').NE.0)THEN
                      IALVER=3
                 ELSEIF(INDEX(TITLE(1:NCTIT),'BASE').NE.0)THEN
                      IALVER=4
                 ELSEIF(INDEX(TITLE(1:NCTIT),'BOTTOM').NE.0)THEN
                      IALVER=5
                 ELSEIF(INDEX(TITLE(1:NCTIT),'NORMAL').NE.0)THEN
                      IALVER=0
                 ELSE
                      IALVER=0
                 ENDIF
                 CALL GSTXAL(IALHOR,IALVER)
            ELSE
                 CALL GSTXAL(0,0)
                 IFAIL2=0
            ENDIF
*   If there is a 6th argument, set the text orientation.
            IF(NARG.GE.5)THEN
                 UPX=COS(PI*(ARG(6)+90.0)/180.0)
                 UPY=SIN(PI*(ARG(6)+90.0)/180.0)
                 CALL GSCHUP(UPX,UPY)
            ELSE
                 CALL GSCHUP(0.0,1.0)
            ENDIF
*   Plot the string.
            CALL STRBUF('READ',NINT(ARG(3)),TITLE,NCTIT,IFAIL3)
            IF(NCTIT.GE.1)CALL GRTEXT(ARG(1),ARG(2),TITLE(1:NCTIT))
*   Switch back to alphanumeric screen.
            CALL GRALPH
*   Error processing.
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0)
     -           PRINT *,' !!!!!! GRACAL WARNING : Error'//
     -           ' retrieving a string for PLOT_TEXT.'
*** Plot a comment string.
      ELSEIF(IPROC.EQ.-806)THEN
*   Check number of arguments and argument type.
            IF(NARG.NE.2.OR.MODARG(1).NE.1.OR.MODARG(2).NE.1)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect set of'//
     -                ' arguments for PLOT_COMMENT.'
                 RETURN
            ENDIF
*   Figure out where the comment should be placed.
            CALL STRBUF('READ',NINT(ARG(1)),TITLE,NCTIT,IFAIL1)
            IF(NCTIT.LT.1)THEN
                 TITLE=' '
                 NCTIT=1
            ENDIF
            CALL CLTOU(TITLE(1:NCTIT))
            IF(INDEX(TITLE(1:NCTIT),'UP')+
     -           INDEX(TITLE(1:NCTIT),'HIGH').NE.0)THEN
                 IUD=1
            ELSEIF(INDEX(TITLE(1:NCTIT),'DOWN')+
     -           INDEX(TITLE(1:NCTIT),'LOW').NE.0)THEN
                 IUD=2
            ELSE
                 PRINT *,' !!!!!! GRACAL WARNING : Up/down'//
     -                ' location missing; comment not plotted.'
                 RETURN
            ENDIF
            IF(INDEX(TITLE(1:NCTIT),'LEFT').NE.0)THEN
                 ILR=0
            ELSEIF(INDEX(TITLE(1:NCTIT),'RIGHT').NE.0)THEN
                 ILR=2
            ELSE
                 PRINT *,' !!!!!! GRACAL WARNING : Left/right'//
     -                ' location missing; comment not plotted.'
                 RETURN
            ENDIF
*   Fetch the string to be plotted.
            CALL STRBUF('READ',NINT(ARG(2)),TITLE,NCTIT,IFAIL2)
*   Switch to graphics screen.
            CALL GRGRAF(.FALSE.)
*   Plot the comment.
            IF(NCTIT.GE.1)CALL GRCOMM(IUD+ILR,TITLE(1:NCTIT))
*   Switch back to alphanumeric screen.
            CALL GRALPH
*   Error processing.
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)
     -           PRINT *,' !!!!!! GRACAL WARNING : Error'//
     -           ' retrieving a string for PLOT_COMMENT.'
*** Plot an area.
      ELSEIF(IPROC.EQ.-807)THEN
*   Check number of arguments.
            IF(NARG.EQ.1.OR.
     -           (NARG.LE.3.AND.(MODARG(1).NE.5.OR.MODARG(2).NE.5)).OR.
     -           (NARG.NE.2*(NARG/2).AND.MODARG(NARG).NE.1))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect set of'//
     -                ' arguments for PLOT_AREA.'
                 RETURN
            ENDIF
*   Check argument mode.
            IF(NARG.GE.4)THEN
                 DO 60 I=1,2*(NARG/2)
                 IF(MODARG(I).NE.2)THEN
                      PRINT *,' !!!!!! GRACAL WARNING : Incorrect'//
     -                     ' argument type in a PLOT_AREA call.'
                      RETURN
                 ENDIF
60               CONTINUE
                 IF(NARG.LT.6)THEN
                      PRINT *,' !!!!!! GRACAL WARNING : Insufficient'//
     -                     ' number of points in a PLOT_AREA call.'
                      RETURN
                 ENDIF
            ENDIF
*   Switch to graphics screen.
            CALL GRGRAF(.FALSE.)
*   If there is a 3rd argument, set the area type.
            IF(NARG.NE.2*(NARG/2))THEN
                 CALL STRBUF('READ',NINT(ARG(NARG)),TITLE,NCTIT,IFAIL1)
                 IF(NCTIT.LT.1)THEN
                      TITLE=' '
                      NCTIT=1
                 ENDIF
                 CALL CLTOU(TITLE(1:NCTIT))
            ELSE
                 TITLE='FUNCTION-1'
                 NCTIT=10
                 IFAIL1=0
            ENDIF
            CALL GRATTS(TITLE(1:NCTIT),'AREA')
*   Plot the area.
            IF(NARG.GE.4)THEN
                 DO 70 I=1,NARG/2
                 XPL(I)=ARG(2*I-1)
                 YPL(I)=ARG(2*I)
70               CONTINUE
                 CALL GRAREA(NARG/2,XPL,YPL)
            ELSE
                 CALL MATFAR(NINT(ARG(1)),NINT(ARG(2)),' ')
            ENDIF
*   Switch back to alphanumeric screen.
            CALL GRALPH
*   Error processing.
            IF(IFAIL1.NE.0)PRINT *,' !!!!!! GRACAL WARNING : Error'//
     -           ' retrieving a string for PLOT_AREA.'
*** Plot a graph.
      ELSEIF(IPROC.EQ.-808)THEN
*   Check number of arguments.
            IF(NARG.LT.2.OR.NARG.GT.5)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect number'//
     -                ' of arguments for PLOT_GRAPH.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.5.OR.MODARG(2).NE.5.OR.
     -           (NARG.GE.3.AND.MODARG(3).NE.1).OR.
     -           (NARG.GE.4.AND.MODARG(4).NE.1).OR.
     -           (NARG.GE.5.AND.MODARG(5).NE.1))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Some arguments of'//
     -                ' PLOT_GRAPH are of incorrect type.'
                 RETURN
            ENDIF
*   Fetch the x-axis label.
            IF(NARG.GE.3)THEN
                 CALL STRBUF('READ',NINT(ARG(3)),XTXT,NCXTXT,IFAIL1)
                 IF(NCXTXT.LT.1)THEN
                      XTXT=' '
                      NCXTXT=1
                 ENDIF
            ELSE
                 DO 71 J=1,NGLB
                 IF(GLBMOD(J).NE.5)GOTO 71
                 IF(NINT(GLBVAL(J)).EQ.NINT(ARG(1)))THEN
                      XTXT=GLBVAR(J)
                      NCXTXT=10
                      GOTO 72
                 ENDIF
71               CONTINUE
                 XTXT='x-axis'
                 NCXTXT=6
72               CONTINUE
                 IFAIL1=0
            ENDIF
*   Fetch the y-axis label.
            IF(NARG.GE.4)THEN
                 CALL STRBUF('READ',NINT(ARG(4)),YTXT,NCYTXT,IFAIL2)
                 IF(NCYTXT.LT.1)THEN
                      YTXT=' '
                      NCYTXT=1
                 ENDIF
            ELSE
                 DO 73 J=1,NGLB
                 IF(GLBMOD(J).NE.5)GOTO 73
                 IF(NINT(GLBVAL(J)).EQ.NINT(ARG(2)))THEN
                      YTXT=GLBVAR(J)
                      NCYTXT=10
                      GOTO 74
                 ENDIF
73               CONTINUE
                 YTXT='y-axis'
                 NCYTXT=6
74               CONTINUE
                 IFAIL2=0
            ENDIF
*   Fetch the global title.
            IF(NARG.GE.5)THEN
                 CALL STRBUF('READ',NINT(ARG(5)),TITLE,NCTIT,IFAIL3)
                 IF(NCTIT.LT.1)THEN
                      TITLE=' '
                      NCTIT=1
                 ENDIF
            ELSEIF(XTXT.NE.'x-axis'.AND.XTXT.NE.' '.AND.
     -           YTXT.NE.'y-axis'.AND.YTXT.NE.' '.AND.
     -           NARG.LT.3)THEN
                 TITLE=XTXT(1:NCXTXT)//' vs '//YTXT(1:NCYTXT)
                 NCTIT=MIN(LEN(TITLE),NCXTXT+4+NCYTXT)
            ELSE
                 TITLE=' '
                 NCTIT=1
                 IFAIL3=0
            ENDIF
*   Plot the graph.
            CALL MATGRA(NINT(ARG(1)),NINT(ARG(2)),
     -           XTXT(1:NCXTXT),YTXT(1:NCYTXT),TITLE(1:NCTIT))
*   Switch back to normal screen.
            CALL GRALPH
*   Error processing.
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0)
     -           PRINT *,' !!!!!! GRACAL WARNING : Error'//
     -           ' retrieving a string for PLOT_GRAPH.'
*** Plotting error bars.
       ELSEIF(IPROC.EQ.-809)THEN
*   Identify provisionally the chosen format.
            IF(NARG.GE.7.OR.(NARG.EQ.6.AND.MODARG(5).NE.1))THEN
                 IFORM=3
            ELSEIF(NARG.GE.5.OR.(NARG.EQ.4.AND.MODARG(3).NE.1))THEN
                 IFORM=2
            ELSEIF(NARG.GE.2)THEN
                 IFORM=1
            ELSE
                 PRINT *,' !!!!!! GRACAL WARNING : Not a recognised'//
     -                ' format of PLOT_ERROR_BARS; no error bars.'
                 RETURN
            ENDIF
*   Verify the types for each format.
            IF((MODARG(1).NE.2.AND.MODARG(1).NE.5).OR.
     -           (MODARG(2).NE.2.AND.MODARG(2).NE.5))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : PLOT_ERROR_BARS'//
     -                ' needs at least an (x,y) pair; no error bars.'
                 RETURN
            ELSEIF(IFORM.EQ.1.AND.(
     -           NARG.GT.4.OR.
     -           (NARG.GE.3.AND.MODARG(3).NE.1).OR.
     -           (NARG.GE.4.AND.MODARG(4).NE.2)))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect option'//
     -                ' list for PLOT_ERROR_BARS; no error bars.'
                 RETURN
            ELSEIF(IFORM.GT.1.AND.(
     -           (MODARG(3).NE.2.AND.MODARG(3).NE.5).OR.
     -           (MODARG(4).NE.2.AND.MODARG(4).NE.5)))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect set of'//
     -                ' (ex-,ey-) in PLOT_ERROR_BARS; no error bars.'
                 RETURN
            ELSEIF(IFORM.EQ.2.AND.(
     -           NARG.GT.6.OR.
     -           (NARG.GE.5.AND.MODARG(5).NE.1).OR.
     -           (NARG.GE.6.AND.MODARG(6).NE.2)))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect option'//
     -                ' list for PLOT_ERROR_BARS; no error bars.'
                 RETURN
            ELSEIF(IFORM.GT.2.AND.(
     -           (MODARG(5).NE.2.AND.MODARG(5).NE.5).OR.
     -           (MODARG(6).NE.2.AND.MODARG(6).NE.5)))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect set of'//
     -                ' (ex+,ey+) in PLOT_ERROR_BARS; no error bars.'
                 RETURN
            ELSEIF(IFORM.EQ.3.AND.(
     -           NARG.GT.8.OR.
     -           (NARG.GE.7.AND.MODARG(7).NE.1).OR.
     -           (NARG.GE.8.AND.MODARG(8).NE.2)))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect option'//
     -                ' list for PLOT_ERROR_BARS; no error bars.'
                 RETURN
            ENDIF
*   Fetch the option string, if present.
            IF(IFORM.EQ.1.AND.NARG.GE.3.AND.MODARG(3).EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(3)),TITLE,NC,IFAIL1)
                 IF(NC.LT.1)THEN
                      TITLE=' '
                      NC=1
                 ENDIF
                 CALL CLTOU(TITLE(1:NC))
            ELSEIF(IFORM.EQ.2.AND.NARG.GE.5.AND.MODARG(5).EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(5)),TITLE,NC,IFAIL1)
                 IF(NC.LT.1)THEN
                      TITLE=' '
                      NC=1
                 ENDIF
                 CALL CLTOU(TITLE(1:NC))
            ELSEIF(IFORM.EQ.3.AND.NARG.GE.7.AND.MODARG(7).EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(7)),TITLE,NC,IFAIL1)
                 IF(NC.LT.1)THEN
                      TITLE=' '
                      NC=1
                 ENDIF
                 CALL CLTOU(TITLE(1:NC))
            ELSE
                 TITLE='CIRCLE'
                 NC=6
                 IFAIL1=0
            ENDIF
*   Fetch the character size if present.
            IF(IFORM.EQ.1.AND.NARG.GE.4.AND.MODARG(4).EQ.2)THEN
                 SIZE=ARG(4)
            ELSEIF(IFORM.EQ.2.AND.NARG.GE.6.AND.MODARG(6).EQ.2)THEN
                 SIZE=ARG(6)
            ELSEIF(IFORM.EQ.3.AND.NARG.GE.8.AND.MODARG(8).EQ.2)THEN
                 SIZE=ARG(8)
            ELSE
                 SIZE=0.01
            ENDIF
*   Locate the arrays, get hold of and check dimensions.
            ILEN=0
            DO 301 I=1,NARG
            IF(MODARG(I).EQ.5)THEN
                 IREF(I)=NINT(ARG(I))
                 ISLOT(I)=MATSLT(IREF(I))
                 IF(ISLOT(I).NE.0)THEN
                      IF(MDIM(ISLOT(I)).NE.1)PRINT *,' ------ GRACAL'//
     -                     ' MESSAGE : Non 1-dimensional vector'//
     -                     ' found; unraveled.'
                      IF(ILEN.EQ.0)THEN
                           ILEN=MLEN(ISLOT(I))
                      ELSEIF(ILEN.NE.MLEN(ISLOT(I)))THEN
                           PRINT *,' !!!!!! GRACAL WARNING : Vectors'//
     -                         ' have different lengths; no error bars.'
                           RETURN
                      ENDIF
                 ELSE
                      PRINT *,' !!!!!! GRACAL WARNING : Vector'//
     -                    ' not found; no error bars.'
                      RETURN
                 ENDIF
            ENDIF
301         CONTINUE
*   If none are arrays, then assign a size of 1.
            IF(ILEN.EQ.0)THEN
                 ISIZ(1)=1
            ELSE
                 ISIZ(1)=ILEN
            ENDIF
*   Expand those numbers that are not matrices.
            DO 302 I=1,6
            IF((I.EQ.5.OR.I.EQ.6).AND.(IFORM.EQ.1.OR.IFORM.EQ.2))THEN
                 IREF(I)=IREF(I-2)
            ELSEIF((I.EQ.3.OR.I.EQ.4).AND.IFORM.EQ.1)THEN
                 CALL MATADM('ALLOCATE',IREF(I),1,ISIZ,2,IFAIL2)
                 IF(IFAIL2.NE.0)THEN
                      PRINT *,' !!!!!! GRACAL WARNING : Unable to'//
     -                     ' create a null-vector; no error bars.'
                      RETURN
                 ENDIF
                 ISLOT(I)=MATSLT(IREF(I))
                 IF(ISLOT(I).LE.0)THEN
                      PRINT *,' !!!!!! GRACAL WARNING : Unable to'//
     -                    ' locate a null-vector; no error bars.'
                      RETURN
                 ENDIF
                 DO 303 J=1,ISIZ(1)
                 MVEC(MORG(ISLOT(I))+J)=0
303              CONTINUE
            ELSEIF(MODARG(I).EQ.2)THEN
                 CALL MATADM('ALLOCATE',IREF(I),1,ISIZ,2,IFAIL2)
                 IF(IFAIL2.NE.0)THEN
                      PRINT *,' !!!!!! GRACAL WARNING : Unable to'//
     -                     ' expand a number; no error bars.'
                      RETURN
                 ENDIF
                 ISLOT(I)=MATSLT(IREF(I))
                 IF(ISLOT(I).LE.0)THEN
                      PRINT *,' !!!!!! GRACAL WARNING : Unable to'//
     -                    ' locate an expanded number; no error bars.'
                      RETURN
                 ENDIF
                 DO 305 J=1,ISIZ(1)
                 MVEC(MORG(ISLOT(I))+J)=ARG(I)
305              CONTINUE
            ENDIF
302         CONTINUE
*   Switch to graphics screen.
            CALL GRGRAF(.FALSE.)
*   Plot the error bars.
            CALL MATERR(IREF(1),IREF(2),IREF(3),
     -           IREF(4),IREF(5),IREF(6),TITLE(1:NC),SIZE)
*   Switch to alpha screen.
            CALL GRALPH
*   Get rid of temporary arrays.
            DO 304 I=1,6
            IF((I.EQ.1.OR.I.EQ.2).AND.MODARG(I).EQ.2)THEN
                 CALL MATADM('DELETE',IREF(I),1,ISIZ,2,IFAIL3)
            ELSEIF((I.EQ.3.OR.I.EQ.4).AND.(
     -           IFORM.EQ.1.OR.
     -           (MODARG(I).EQ.2.AND.IFORM.GT.1)))THEN
                 CALL MATADM('DELETE',IREF(I),1,ISIZ,2,IFAIL3)
            ELSEIF((I.EQ.5.OR.I.EQ.6).AND.
     -           (MODARG(I).EQ.2.AND.IFORM.GT.2))THEN
                 CALL MATADM('DELETE',IREF(I),1,ISIZ,2,IFAIL3)
            ENDIF
304         CONTINUE
*** Plotting oblique error bars.
       ELSEIF(IPROC.EQ.-831)THEN
*   Identify provisionally the chosen format.
            IF(NARG.GE.7.OR.(NARG.EQ.6.AND.MODARG(5).NE.1))THEN
                 IFORM=3
            ELSEIF(NARG.GE.5.OR.(NARG.EQ.4.AND.MODARG(3).NE.1))THEN
                 IFORM=2
            ELSEIF(NARG.GE.2)THEN
                 IFORM=1
            ELSE
                 PRINT *,' !!!!!! GRACAL WARNING : Not a recognised'//
     -                ' format of PLOT_ERROR_BARS; no error bars.'
                 RETURN
            ENDIF
*   Verify the types for each format.
            IF((MODARG(1).NE.2.AND.MODARG(1).NE.5).OR.
     -           (MODARG(2).NE.2.AND.MODARG(2).NE.5))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : PLOT_ERROR_BARS'//
     -                ' needs at least an (x,y) pair; no error bars.'
                 RETURN
            ELSEIF(IFORM.EQ.1.AND.(
     -           NARG.GT.4.OR.
     -           (NARG.GE.3.AND.MODARG(3).NE.1).OR.
     -           (NARG.GE.4.AND.MODARG(4).NE.2)))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect option'//
     -                ' list for PLOT_ERROR_BARS; no error bars.'
                 RETURN
            ELSEIF(IFORM.GT.1.AND.(
     -           (MODARG(3).NE.2.AND.MODARG(3).NE.5).OR.
     -           (MODARG(4).NE.2.AND.MODARG(4).NE.5)))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect set of'//
     -                ' (ex-,ey-) in PLOT_ERROR_BARS; no error bars.'
                 RETURN
            ELSEIF(IFORM.EQ.2.AND.(
     -           NARG.GT.6.OR.
     -           (NARG.GE.5.AND.MODARG(5).NE.1).OR.
     -           (NARG.GE.6.AND.MODARG(6).NE.2)))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect option'//
     -                ' list for PLOT_ERROR_BARS; no error bars.'
                 RETURN
            ELSEIF(IFORM.GT.2.AND.(
     -           (MODARG(5).NE.2.AND.MODARG(5).NE.5).OR.
     -           (MODARG(6).NE.2.AND.MODARG(6).NE.5)))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect set of'//
     -                ' (ex+,ey+) in PLOT_ERROR_BARS; no error bars.'
                 RETURN
            ELSEIF(IFORM.EQ.3.AND.(
     -           NARG.GT.8.OR.
     -           (NARG.GE.7.AND.MODARG(7).NE.1).OR.
     -           (NARG.GE.8.AND.MODARG(8).NE.2)))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect option'//
     -                ' list for PLOT_ERROR_BARS; no error bars.'
                 RETURN
            ENDIF
*   Fetch the option string, if present.
            IF(IFORM.EQ.1.AND.NARG.GE.3.AND.MODARG(3).EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(3)),TITLE,NC,IFAIL1)
                 IF(NC.LT.1)THEN
                      TITLE=' '
                      NC=1
                 ENDIF
                 CALL CLTOU(TITLE(1:NC))
            ELSEIF(IFORM.EQ.2.AND.NARG.GE.5.AND.MODARG(5).EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(5)),TITLE,NC,IFAIL1)
                 IF(NC.LT.1)THEN
                      TITLE=' '
                      NC=1
                 ENDIF
                 CALL CLTOU(TITLE(1:NC))
            ELSEIF(IFORM.EQ.3.AND.NARG.GE.7.AND.MODARG(7).EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(7)),TITLE,NC,IFAIL1)
                 IF(NC.LT.1)THEN
                      TITLE=' '
                      NC=1
                 ENDIF
                 CALL CLTOU(TITLE(1:NC))
            ELSE
                 TITLE='CIRCLE'
                 NC=6
                 IFAIL1=0
            ENDIF
*   Fetch the character size if present.
            IF(IFORM.EQ.1.AND.NARG.GE.4.AND.MODARG(4).EQ.2)THEN
                 SIZE=ARG(4)
            ELSEIF(IFORM.EQ.2.AND.NARG.GE.6.AND.MODARG(6).EQ.2)THEN
                 SIZE=ARG(6)
            ELSEIF(IFORM.EQ.3.AND.NARG.GE.8.AND.MODARG(8).EQ.2)THEN
                 SIZE=ARG(8)
            ELSE
                 SIZE=0.01
            ENDIF
*   Locate the arrays, get hold of and check dimensions.
            ILEN=0
            DO 306 I=1,NARG
            IF(MODARG(I).EQ.5)THEN
                 IREF(I)=NINT(ARG(I))
                 ISLOT(I)=MATSLT(IREF(I))
                 IF(ISLOT(I).NE.0)THEN
                      IF(MDIM(ISLOT(I)).NE.1)PRINT *,' ------ GRACAL'//
     -                     ' MESSAGE : Non 1-dimensional vector'//
     -                     ' found; unraveled.'
                      IF(ILEN.EQ.0)THEN
                           ILEN=MLEN(ISLOT(I))
                      ELSEIF(ILEN.NE.MLEN(ISLOT(I)))THEN
                           PRINT *,' !!!!!! GRACAL WARNING : Vectors'//
     -                         ' have different lengths; no error bars.'
                           RETURN
                      ENDIF
                 ELSE
                      PRINT *,' !!!!!! GRACAL WARNING : Vector'//
     -                    ' not found; no error bars.'
                      RETURN
                 ENDIF
            ENDIF
306         CONTINUE
*   If none are arrays, then assign a size of 1.
            IF(ILEN.EQ.0)THEN
                 ISIZ(1)=1
            ELSE
                 ISIZ(1)=ILEN
            ENDIF
*   Expand those numbers that are not matrices.
            DO 307 I=1,6
            IF((I.EQ.5.OR.I.EQ.6).AND.(IFORM.EQ.1.OR.IFORM.EQ.2))THEN
                 IREF(I)=IREF(I-2)
            ELSEIF((I.EQ.3.OR.I.EQ.4).AND.IFORM.EQ.1)THEN
                 CALL MATADM('ALLOCATE',IREF(I),1,ISIZ,2,IFAIL2)
                 IF(IFAIL2.NE.0)THEN
                      PRINT *,' !!!!!! GRACAL WARNING : Unable to'//
     -                     ' create a null-vector; no error bars.'
                      RETURN
                 ENDIF
                 ISLOT(I)=MATSLT(IREF(I))
                 IF(ISLOT(I).LE.0)THEN
                      PRINT *,' !!!!!! GRACAL WARNING : Unable to'//
     -                    ' locate a null-vector; no error bars.'
                      RETURN
                 ENDIF
                 DO 308 J=1,ISIZ(1)
                 MVEC(MORG(ISLOT(I))+J)=0
308              CONTINUE
            ELSEIF(MODARG(I).EQ.2)THEN
                 CALL MATADM('ALLOCATE',IREF(I),1,ISIZ,2,IFAIL2)
                 IF(IFAIL2.NE.0)THEN
                      PRINT *,' !!!!!! GRACAL WARNING : Unable to'//
     -                     ' expand a number; no error bars.'
                      RETURN
                 ENDIF
                 ISLOT(I)=MATSLT(IREF(I))
                 IF(ISLOT(I).LE.0)THEN
                      PRINT *,' !!!!!! GRACAL WARNING : Unable to'//
     -                    ' locate an expanded number; no error bars.'
                      RETURN
                 ENDIF
                 DO 309 J=1,ISIZ(1)
                 MVEC(MORG(ISLOT(I))+J)=ARG(I)
309              CONTINUE
            ENDIF
307         CONTINUE
*   Switch to graphics screen.
            CALL GRGRAF(.FALSE.)
*   Plot the error bars.
            CALL MATOBL(IREF(1),IREF(2),IREF(3),
     -           IREF(4),IREF(5),IREF(6),TITLE(1:NC),SIZE)
*   Switch to alpha screen.
            CALL GRALPH
*   Get rid of temporary arrays.
            DO 310 I=1,6
            IF((I.EQ.1.OR.I.EQ.2).AND.MODARG(I).EQ.2)THEN
                 CALL MATADM('DELETE',IREF(I),1,ISIZ,2,IFAIL3)
            ELSEIF((I.EQ.3.OR.I.EQ.4).AND.(
     -           IFORM.EQ.1.OR.
     -           (MODARG(I).EQ.2.AND.IFORM.GT.1)))THEN
                 CALL MATADM('DELETE',IREF(I),1,ISIZ,2,IFAIL3)
            ELSEIF((I.EQ.5.OR.I.EQ.6).AND.
     -           (MODARG(I).EQ.2.AND.IFORM.GT.2))THEN
                 CALL MATADM('DELETE',IREF(I),1,ISIZ,2,IFAIL3)
            ENDIF
310         CONTINUE
*** Project a line.
      ELSEIF(IPROC.EQ.-810)THEN
*   Check number of arguments.
            IF(NARG.LT.1.OR.NARG.GT.4.OR.
     -           MODARG(1).NE.5.OR.MODARG(2).NE.5.OR.MODARG(2).NE.5.OR.
     -           (NARG.GE.4.AND.MODARG(4).NE.1))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect set of'//
     -                ' arguments for PROJECT_LINE.'
                 RETURN
            ENDIF
*   Switch to graphics screen.
            CALL GRGRAF(.FALSE.)
*   If there is a 3rd argument, set the polyline type.
            IF(NARG.GE.4)THEN
                 CALL STRBUF('READ',NINT(ARG(4)),TITLE,NCTIT,IFAIL1)
                 IF(NCTIT.LT.1)THEN
                      TITLE=' '
                      NCTIT=1
                 ENDIF
                 CALL CLTOU(TITLE(1:NCTIT))
            ELSE
                 TITLE='SOLID'
                 NCTIT=5
                 IFAIL1=0
            ENDIF
            CALL GRATTS(TITLE(1:NCTIT),'POLYLINE')
*   Plot the line segment.
            CALL MATPLN(NINT(ARG(1)),NINT(ARG(2)),NINT(ARG(3)))
*   Switch back to alphanumeric screen.
            CALL GRALPH
*   Error processing.
            IF(IFAIL1.NE.0)PRINT *,' !!!!!! GRACAL WARNING : Error'//
     -           ' retrieving a string for PROJECT_LINE.'
*** Project a set of markers.
      ELSEIF(IPROC.EQ.-811)THEN
*   If there is a 4th argument, set the polymarker type.
            IF(NARG.GE.4)THEN
                 CALL STRBUF('READ',NINT(ARG(4)),TITLE,NCTIT,IFAIL1)
                 IF(NCTIT.LT.1)THEN
                      TITLE=' '
                      NCTIT=1
                 ENDIF
                 CALL CLTOU(TITLE(1:NCTIT))
            ELSE
                 TITLE='CROSS'
                 NCTIT=5
                 IFAIL1=0
            ENDIF
            CALL GRATTS(TITLE(1:NCTIT),'POLYMARKER')
            IF(IFAIL1.NE.0)PRINT *,' !!!!!! GRACAL WARNING : Error'//
     -           ' retrieving a string for PLOT_MARKERS.'
*   Vector type arguments.
            IF((NARG.EQ.3.OR.NARG.EQ.4).AND.
     -           MODARG(1).EQ.5.AND.MODARG(2).EQ.5.AND.
     -           MODARG(2).EQ.5.AND.
     -           (NARG.EQ.3.OR.MODARG(4).EQ.1))THEN
*   Switch to graphics screen.
                 CALL GRGRAF(.FALSE.)
*   Plot the markers.
                 CALL MATPMK(NINT(ARG(1)),NINT(ARG(2)),NINT(ARG(3)))
*   Switch back to alphanumeric screen.
                 CALL GRALPH
*   Scalar arguments.
            ELSEIF((NARG.EQ.3.OR.NARG.EQ.4).AND.
     -           MODARG(1).EQ.2.AND.MODARG(2).EQ.2.AND.
     -           MODARG(2).EQ.2.AND.
     -           (NARG.EQ.3.OR.MODARG(4).EQ.1))THEN
*   Switch to graphics screen.
                 CALL GRGRAF(.FALSE.)
*   Plot the markers.
                 CALL PLAGPM(1,DBLE(ARG(1)),DBLE(ARG(2)),DBLE(ARG(3)))
*   Switch back to alphanumeric screen.
                 CALL GRALPH
*   Other argument types.
            ELSE
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect set of'//
     -                ' arguments for PROJECT_MARKERS.'
                 RETURN
            ENDIF
*** Open a plot, doing nothing else.
       ELSEIF(IPROC.EQ.-812)THEN
            IF(NARG.NE.0)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect number'//
     -                ' of arguments for PLOT_START.'
                 RETURN
            ENDIF
            CALL GRGRAF(.TRUE.)
*   Reset the bar chart and histogram counters.
            IGBAR=0
            IGHIST=0
*** Set a window.
       ELSEIF(IPROC.EQ.-813)THEN
*   Check the arguments.
            IF(NARG.NE.5.OR.
     -           MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.MODARG(3).NE.2.OR.
     -           MODARG(4).NE.2.OR.MODARG(5).NE.2)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect argument'//
     -                ' list for GKS_WINDOW; not executed.'
                 RETURN
            ELSE
*   Set the window.
                 CALL GSWN(NINT(ARG(1)),ARG(2),ARG(3),ARG(4),ARG(5))
*   Make sure lines will not be cut.
                 FRXMIN=ARG(2)
                 FRXMAX=ARG(3)
                 FRYMIN=ARG(4)
                 FRYMAX=ARG(5)
            ENDIF
*** Set a viewport.
       ELSEIF(IPROC.EQ.-814)THEN
            IF(NARG.NE.5.OR.
     -           MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.MODARG(3).NE.2.OR.
     -           MODARG(4).NE.2.OR.MODARG(5).NE.2)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect argument'//
     -                ' list for GKS_VIEWPORT; not executed.'
                 RETURN
            ELSE
                 CALL GSVP(NINT(ARG(1)),ARG(2),ARG(3),ARG(4),ARG(5))
            ENDIF
*** Select a normalisation transformation.
       ELSEIF(IPROC.EQ.-815)THEN
            IF(NARG.NE.1.OR.MODARG(1).NE.2)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect argument'//
     -                ' list for GKS_SELECT_NT; not executed.'
                 RETURN
            ELSE
                 CALL GSELNT(NINT(ARG(1)))
            ENDIF
*** Plot a polyline.
      ELSEIF(IPROC.EQ.-816)THEN
*   Check number of arguments.
            IF(NARG.NE.2.OR.MODARG(1).NE.5.OR.MODARG(2).NE.5)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect set of'//
     -                ' arguments for GKS_POLYLINE.'
                 RETURN
            ENDIF
*   Switch to graphics screen.
            CALL GRGRAF(.FALSE.)
*   Plot the line.
            CALL MATLIN(NINT(ARG(1)),NINT(ARG(2)),'GKS')
*   Switch back to alphanumeric screen.
            CALL GRALPH
*** Plot polymarkers.
      ELSEIF(IPROC.EQ.-817)THEN
*   Check number of arguments.
            IF(NARG.NE.2.OR.MODARG(1).NE.5.OR.MODARG(2).NE.5)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect set of'//
     -                ' arguments for GKS_POLYMARKER.'
                 RETURN
            ENDIF
*   Switch to graphics screen.
            CALL GRGRAF(.FALSE.)
*   Plot the line.
            CALL MATMRK(NINT(ARG(1)),NINT(ARG(2)),'GKS')
*   Switch back to alphanumeric screen.
            CALL GRALPH
*** Set attributes.
       ELSEIF(IPROC.EQ.-818.OR.IPROC.EQ.-819.OR.
     -      IPROC.EQ.-820.OR.IPROC.EQ.-821)THEN
*   Check argument types.
            IF(NARG.NE.1.OR.MODARG(1).NE.1)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect argument'//
     -                ' received by SET_x_ATTRIBUTES.'
                 RETURN
            ENDIF
*   Pick up the representation.
            CALL STRBUF('READ',NINT(ARG(1)),TITLE,NCTIT,IFAIL1)
            IF(IFAIL1.NE.0.OR.NCTIT.LE.0)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Unable to retrieve'//
     -                ' the representation name.'
                 RETURN
            ENDIF
            CALL CLTOU(TITLE(1:NCTIT))
*   Set the representation.
            IF(IPROC.EQ.-818)THEN
                 CALL GRATTS(TITLE(1:NCTIT),'POLYLINE')
            ELSEIF(IPROC.EQ.-819)THEN
                 CALL GRATTS(TITLE(1:NCTIT),'POLYMARKER')
            ELSEIF(IPROC.EQ.-820)THEN
                 CALL GRATTS(TITLE(1:NCTIT),'TEXT')
            ELSEIF(IPROC.EQ.-821)THEN
                 CALL GRATTS(TITLE(1:NCTIT),'AREA')
            ENDIF
*** Plot a text string.
      ELSEIF(IPROC.EQ.-822)THEN
*   Check number of arguments.
            IF(NARG.NE.3.OR.MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           MODARG(3).NE.1)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect set of'//
     -                ' arguments for GKS_TEXT.'
                 RETURN
            ENDIF
*   Pick up the representation.
            CALL STRBUF('READ',NINT(ARG(3)),TITLE,NCTIT,IFAIL1)
            IF(IFAIL1.NE.0.OR.NCTIT.LE.0)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Unable to retrieve'//
     -                ' the text string.'
                 RETURN
            ENDIF
*   Switch to graphics screen.
            CALL GRGRAF(.FALSE.)
*   Plot the text.
            CALL GTX(ARG(1),ARG(2),TITLE(1:NCTIT))
*   Switch back to alphanumeric screen.
            CALL GRALPH
*** Plot an area.
      ELSEIF(IPROC.EQ.-823)THEN
*   Check number of arguments.
            IF(NARG.NE.2.OR.MODARG(1).NE.5.OR.MODARG(2).NE.5)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect set of'//
     -                ' arguments for GKS_AREA.'
                 RETURN
            ENDIF
*   Switch to graphics screen.
            CALL GRGRAF(.FALSE.)
*   Plot the line.
            CALL MATFAR(NINT(ARG(1)),NINT(ARG(2)),'GKS')
*   Switch back to alphanumeric screen.
            CALL GRALPH
*** Set the text alignment.
       ELSEIF(IPROC.EQ.-824)THEN
            IF(NARG.NE.2.OR.MODARG(1).NE.1.OR.MODARG(2).NE.1)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect set of'//
     -                ' arguments for GKS_SET_TEXT_ALIGNMENT.'
                 RETURN
            ENDIF
*   Fetch the horizontal alignment.
            CALL STRBUF('READ',NINT(ARG(1)),TITLE,NCTIT,IFAIL1)
            IF(IFAIL1.NE.0.OR.NCTIT.LE.0)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Unable to retrieve'//
     -                ' the horizontal alignment.'
                 RETURN
            ENDIF
            CALL CLTOU(TITLE(1:NCTIT))
            IF(TITLE(1:NCTIT).EQ.'NORMAL')THEN
                 IHOR=0
            ELSEIF(TITLE(1:NCTIT).EQ.'LEFT')THEN
                 IHOR=1
            ELSEIF(TITLE(1:NCTIT).EQ.'CENTER'.OR.
     -           TITLE(1:NCTIT).EQ.'CENTRE')THEN
                 IHOR=2
            ELSEIF(TITLE(1:NCTIT).EQ.'RIGHT')THEN
                 IHOR=3
            ELSE
                 PRINT *,' !!!!!! GRACAL WARNING : Invalid horizontal'//
     -                ' alignment; using NORMAL.'
                 IHOR=0
            ENDIF
*   Fetch the vertical alignment.
            CALL STRBUF('READ',NINT(ARG(2)),TITLE,NCTIT,IFAIL1)
            IF(IFAIL1.NE.0.OR.NCTIT.LE.0)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Unable to retrieve'//
     -                ' the vertical alignment.'
                 RETURN
            ENDIF
            CALL CLTOU(TITLE(1:NCTIT))
            IF(TITLE(1:NCTIT).EQ.'NORMAL')THEN
                 IVERT=0
            ELSEIF(TITLE(1:NCTIT).EQ.'TOP')THEN
                 IVERT=1
            ELSEIF(TITLE(1:NCTIT).EQ.'CAP')THEN
                 IVERT=2
            ELSEIF(TITLE(1:NCTIT).EQ.'HALF')THEN
                 IVERT=3
            ELSEIF(TITLE(1:NCTIT).EQ.'BASE')THEN
                 IVERT=0
            ELSEIF(TITLE(1:NCTIT).EQ.'BOTTOM')THEN
                 IVERT=0
            ELSE
                 PRINT *,' !!!!!! GRACAL WARNING : Invalid vertical'//
     -                ' alignment; using NORMAL.'
                 IVERT=0
            ENDIF
*   Issue the GKS call.
            CALL GSTXAL(IHOR,IVERT)
*** Text colour.
       ELSEIF(IPROC.EQ.-825)THEN
*   Check arguments.
            IF(NARG.NE.1.OR.MODARG(1).NE.1)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect argument'//
     -                ' list for GKS_SET_TEXT_COLOUR'
                 RETURN
            ENDIF
*   Retrieve the colour name.
            CALL STRBUF('READ',NINT(ARG(1)),TITLE,NCTIT,IFAIL1)
            IF(IFAIL1.NE.0.OR.NCTIT.LT.1)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Unable to retrieve'//
     -                ' the GKS_SET_TEXT_COLOUR colour.'
                 RETURN
            ENDIF
*   Locate the colour in the table.
            CALL GRCOLQ(1,TITLE(1:NCTIT),ICOL)
            IF(ICOL.LT.0)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : The colour '//
     -                TITLE(1:NCTIT)//' is not known; not set.'
                 RETURN
            ENDIF
*   Set the colour.
            CALL GSTXCI(ICOL)
*** Character height.
       ELSEIF(IPROC.EQ.-826)THEN
*   Check the argument list.
            IF(NARG.NE.1.OR.MODARG(1).NE.2)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect argument'//
     -                ' list for GKS_SET_CHARACTER_HEIGHT'
                 RETURN
            ENDIF
*   Issue the GKS call.
            CALL GSCHH(ARG(1))
*** Character expansion.
       ELSEIF(IPROC.EQ.-827)THEN
*   Check the argument list.
            IF(NARG.NE.1.OR.MODARG(1).NE.2)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect argument'//
     -                ' list for GKS_SET_CHARACTER_EXPANSION'
                 RETURN
            ENDIF
*   Issue the GKS call.
            CALL GSCHXP(ARG(1))
*** Character spacing.
       ELSEIF(IPROC.EQ.-828)THEN
*   Check the argument list.
            IF(NARG.NE.1.OR.MODARG(1).NE.2)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect argument'//
     -                ' list for GKS_SET_CHARACTER_SPACING'
                 RETURN
            ENDIF
*   Issue the GKS call.
            CALL GSCHSP(ARG(1))
*** Character up vector.
       ELSEIF(IPROC.EQ.-829)THEN
*   Check the argument list.
            IF(NARG.NE.2.OR.MODARG(1).NE.2.OR.MODARG(2).NE.2)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect argument'//
     -                ' list for GKS_SET_CHARACTER_UP_VECTOR'
                 RETURN
            ENDIF
*   Issue the GKS call.
            CALL GSCHUP(ARG(1),ARG(2))
*** Text font and precision.
       ELSEIF(IPROC.EQ.-830)THEN
*   Check the argument list.
            IF(NARG.NE.2.OR.MODARG(1).NE.2.OR.MODARG(2).NE.1)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect argument'//
     -                ' list for GKS_SET_CHARACTER_UP_VECTOR'
                 RETURN
            ENDIF
*   Extract the precision.
            CALL STRBUF('READ',NINT(ARG(2)),TITLE,NCTIT,IFAIL1)
            CALL CLTOU(TITLE(1:MIN(1,NCTIT)))
            IF(NCTIT.LT.1.OR.IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Invalid character'//
     -                ' precision ; font and precision not set.'
                 RETURN
            ELSEIF(TITLE(1:NCTIT).EQ.'STROKE')THEN
                 IPREC=2
            ELSEIF(TITLE(1:NCTIT).EQ.'CHARACTER')THEN
                 IPREC=1
            ELSEIF(TITLE(1:NCTIT).EQ.'STRING')THEN
                 IPREC=0
            ELSE
                 PRINT *,' !!!!!! GRACAL WARNING : Character'//
     -                ' precision '//TITLE(1:NCTIT)//
     -                ' is not know; assuming CHARACTER.'
                 IPREC=1
            ENDIF
*   Issue the GKS call.
            CALL GSTXFP(NINT(ARG(1)),IPREC)
*** Plot an arrow.
       ELSEIF(IPROC.EQ.-850)THEN
*   Check number of arguments.
            IF(NARG.LT.4.OR.NARG.GT.5.OR.
     -           MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           MODARG(3).NE.2.OR.MODARG(4).NE.2.OR.
     -           (NARG.GE.5.AND.MODARG(5).NE.1))THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect set of'//
     -                ' arguments for PLOT_ARROW.'
                 RETURN
            ENDIF
*   Pick up the representation, if present.
            IF(NARG.GE.5)THEN
                 CALL STRBUF('READ',NINT(ARG(5)),TITLE,NCTIT,IFAIL1)
                 IF(NCTIT.LT.1)THEN
                      TITLE='SOLID'
                      NCTIT=5
                 ENDIF
                 CALL CLTOU(TITLE(1:NCTIT))
            ELSE
                 TITLE='SOLID'
                 NCTIT=5
                 IFAIL1=0
            ENDIF
*   Switch to graphics screen.
            CALL GRGRAF(.FALSE.)
*   Plot the arrow with the requested representation.
            CALL GRATTS(TITLE(1:NCTIT),'POLYLINE')
            CALL GRARRO(ARG(1),ARG(2),ARG(3),ARG(4))
*   Switch back to alphanumeric screen.
            CALL GRALPH
*   Print error message.
            IF(IFAIL1.NE.0)PRINT *,' !!!!!! GRACAL WARNING : Unable'//
     -           ' to retrieve the arrow representation; set to SOLID.'
*** Plot a title.
       ELSEIF(IPROC.EQ.-851)THEN
*   Check number of arguments and argument type.
            IF(NARG.NE.1.OR.MODARG(1).NE.1)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect set of'//
     -                ' arguments for PLOT_TITLE.'
                 RETURN
            ENDIF
*   Retrieve the title string.
            CALL STRBUF('READ',NINT(ARG(1)),TITLE,NCTIT,IFAIL1)
            IF(IFAIL1.NE.0.OR.NCTIT.LE.0)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Unable to retrieve'//
     -                ' the title.'
                 RETURN
            ENDIF
*   Plot the title.
            CALL GSELNT(0)
            CALL GSCHUP(0.0,1.0)
            CALL GSTXAL(1,1)
            CALL GRATTS('TITLE','TEXT')
            CALL GRTX(DISPX0+0.1,DISPY1-GPXT,TITLE(1:NCTIT))
*   Restore.
            CALL GSELNT(1)
            CALL GSTXAL(0,0)
            CALL GSCHUP(0.0,1.0)
*** Plot an x-label.
       ELSEIF(IPROC.EQ.-852)THEN
*   Check number of arguments and argument type.
            IF(NARG.NE.1.OR.MODARG(1).NE.1)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect set of'//
     -                ' arguments for PLOT_X_LABEL.'
                 RETURN
            ENDIF
*   Retrieve the title string.
            CALL STRBUF('READ',NINT(ARG(1)),XTXT,NCXTXT,IFAIL1)
            IF(IFAIL1.NE.0.OR.NCXTXT.LE.0)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Unable to retrieve'//
     -                ' the x-label.'
                 RETURN
            ENDIF
*   Label the x-axis.
            CALL GSELNT(0)
            CALL GSTXAL(3,0)
            CALL GSCHUP(0.0,1.0)
            CALL GRATTS('LABELS','TEXT')
            CALL GQTXX(IWK,0.5,0.5,XTXT(1:NCXTXT),IERR,CPX,CPY,
     -           XBOX,YBOX)
            YSHIFT=0.5-MIN(YBOX(1),YBOX(2),YBOX(3),YBOX(4))
            CALL GRTX(DISPX1-0.1,DISPY0+GPXL+YSHIFT,XTXT(1:NCXTXT))
*   Restore.
            CALL GSELNT(1)
            CALL GSTXAL(0,0)
            CALL GSCHUP(0.0,1.0)
*** Plot a y-label.
       ELSEIF(IPROC.EQ.-853)THEN
*   Check number of arguments and argument type.
            IF(NARG.NE.1.OR.MODARG(1).NE.1)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect set of'//
     -                ' arguments for PLOT_Y_LABEL.'
                 RETURN
            ENDIF
*   Retrieve the title string.
            CALL STRBUF('READ',NINT(ARG(1)),YTXT,NCYTXT,IFAIL1)
            IF(IFAIL1.NE.0.OR.NCYTXT.LE.0)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Unable to retrieve'//
     -                ' the y-label.'
                 RETURN
            ENDIF
*   Label the y-axis.
            CALL GSELNT(0)
            CALL GSTXAL(3,1)
            CALL GSCHUP(-1.0,0.0)
            CALL GRATTS('LABELS','TEXT')
            CALL GRTX(DISPX0+GPYL,DISPY1-0.1,YTXT(1:NCYTXT))
*   Restore.
            CALL GSELNT(1)
            CALL GSTXAL(0,0)
            CALL GSCHUP(0.0,1.0)
*** Rainbow colours
       ELSEIF(IPROC.EQ.-854)THEN
*   Check argument list.
            IF(NARG.NE.4.OR.
     -           MODARG(1).NE.2.OR.
     -           ARGREF(2,1).GE.2.OR.ARGREF(3,1).GE.2.OR.
     -           ARGREF(4,1).GE.2.OR.ARGREF(5,1).GE.2)THEN
                 PRINT *,' !!!!!! GRACAL WARNING : Incorrect'//
     -                ' argument list for RAINBOW; not called.'
                 RETURN
            ENDIF
*   Clear up old values.
            CALL ALGREU(NINT(ARG(2)),MODARG(3),ARGREF(2,1))
            CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
*   Call the procedure.
            CALL RAIN(ARG(1),ARG(2),ARG(3),ARG(4))
            MODARG(2)=2
            MODARG(3)=2
            MODARG(4)=2
*** Unknown graphics operation.
       ELSE
            PRINT *,' !!!!!! GRACAL WARNING : Unknown procedure code'//
     -           ' received; nothing done.'
            IFAIL=1
            RETURN
       ENDIF
*** Seems to have worked.
       IFAIL=0
       END
