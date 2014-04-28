CDECK  ID>, GRACWK.
       SUBROUTINE GRACWK(NAME,OPTION)
*-----------------------------------------------------------------------
*   GRACWK - Activates a workstation - GKS version.
*   (Last changed on  6/ 3/04.)
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
       EXTERNAL INPCMX
       INTEGER INPCMX,IWK,IFAIL,IERR,ISTATE
       CHARACTER*(*) NAME,OPTION
*** Locate workstation.
       CALL GRQIWK(NAME,IWK,IFAIL)
       IF(IFAIL.NE.0)RETURN
*** Delayed processing.
       IF(OPTION.EQ.'DELAY'.AND..NOT.WKMULT(IWK))THEN
            WKSREQ(IWK)=3
            RETURN
       ENDIF
*** Check the current state of the workstation.
       IF(WKSTAT(IWK).LT.2)THEN
            PRINT *,' ------ GRACWK MESSAGE : Workstation ',NAME,
     -           ' is not yet open; trying to open ...'
            CALL GROPWK(NAME,'IMMEDIATE')
            IF(WKSTAT(IWK).EQ.2)THEN
                 PRINT *,'                         Opening the'//
     -                ' workstation was successful.'
            ELSE
                 PRINT *,' !!!!!! GRACWK WARNING : Opening failed'//
     -                ' ; workstation not activated.'
                 RETURN
            ENDIF
       ENDIF
       CALL GQWKS(IWK,IERR,ISTATE)
       IF(IERR.NE.0)THEN
            PRINT *,' !!!!!! GRACWK WARNING : Inquiry error for'//
     -           ' state of ',NAME,' ; assumed inactive.'
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GRACWK DEBUG   :'',
     -           '' GQWKS Error code '',I3,'' state '',I1,'' for'',
     -           '' workstation '',A,''.'')') IERR,ISTATE,NAME
       ELSEIF(ISTATE.EQ.1)THEN
            PRINT *,' !!!!!! GRACWK WARNING : Workstation ',
     -           NAME,' is already active.'
            WKSTAT(IWK)=3
            RETURN
       ENDIF
*** And at last activate the workstation.
       CALL GACWK(IWK)
       WKSTAT(IWK)=3
       CALL SGFLAG
       IF(WKFREF(IWK).GT.0)CALL IGRNG(19.0,19.0)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GRACWK DEBUG   :'',
     -      '' Workstation '',A,'' has been activated.'')') NAME
*** Check that the workstation is really open.
       CALL GQWKS(IWK,IERR,ISTATE)
       IF(IERR.EQ.7.OR.IERR.EQ.25)THEN
            PRINT *,' !!!!!! GRACWK WARNING : Cannot activate ',NAME,
     -           ' because the workstation is not open.'
            WKSTAT(IWK)=1
            RETURN
       ELSEIF(IERR.EQ.20)THEN
            PRINT *,' !!!!!! GRACWK WARNING : Cannot activate ',NAME,
     -           ' because the workstation identifier is not valid.'
            WKSTAT(IWK)=1
            RETURN
       ELSEIF(ISTATE.NE.1)THEN
            PRINT *,' !!!!!! GRACWK WARNING : Workstation ',NAME,
     -           ' could not be activated.'
            WKSTAT(IWK)=1
            RETURN
       ENDIF
       END
