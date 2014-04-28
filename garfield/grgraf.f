CDECK  ID>, GRGRAF.
       SUBROUTINE GRGRAF(WAIT)
*-----------------------------------------------------------------------
*   GRGRAF - Clears the screen, preparing it for graphics.
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
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       EXTERNAL STDSTR
       CHARACTER*80 DUMMY
       LOGICAL STDSTR,WAIT
       INTEGER IOPSTA,IWKREQ,IERR,NACT,IWK,I,IDUM,IERR1,ICONID,IWKTYP,
     -      IERR2,IWKCAT
*** Bring the workstation table up to date.
       IF(WAIT)THEN
*   Increment the frame counter.
            IF(GLBMOD(9).NE.2)THEN
                 PRINT *,' !!!!!! GRGRAF WARNING : Global variable'//
     -                ' FRAME has changed type; reset to Number 1.'
                 GLBMOD(9)=0
                 GLBMOD(9)=2
            ENDIF
            GLBVAL(9)=GLBVAL(9)+1
*   Loop over the workstations.
            DO 30 IWK=1,NWK
*   Skip non-file and multiple frame workstations.
            IF(WKFREF(IWK).EQ.0.OR.WKMULT(IWK))GOTO 30
*   Open if the station should be open but isn't.
            IF(WKSTAT(IWK).LT.2.AND.WKSREQ(IWK).GE.2)
     -           CALL GROPWK(WKNAME(IWK)(1:NCWKNM(IWK)),'IMMEDIATE')
*   Activate if the station should be active but isn't.
            IF(WKSTAT(IWK).LT.3.AND.WKSREQ(IWK).GE.3)
     -           CALL GRACWK(WKNAME(IWK)(1:NCWKNM(IWK)),'IMMEDIATE')
30          CONTINUE
       ENDIF
*** See whether there is a workstation with input facilities.
       CALL GQOPS(IOPSTA)
       IF(IOPSTA.LT.3)THEN
            IF(LDEBUG)WRITE(10,'(''  ++++++ GRGRAF DEBUG   :'',
     -           '' No active workstations.'')')
            RETURN
       ENDIF
*** Try to find a workstation with input facilities.
       CALL GQACWK(0,IERR,NACT,IWK)
       IWKREQ=-1
       DO 20 I=1,NACT
       CALL GQACWK(I,IERR,IDUM,IWK)
*   Locate one that has input facilities.
       CALL GQWKC(IWK,IERR1,ICONID,IWKTYP)
       CALL GQWKCA(IWKTYP,IERR2,IWKCAT)
       IF(IWKCAT.EQ.2)IWKREQ=IWK
20     CONTINUE
*** Only debugging output if there isn't one.
       IF(IWKREQ.EQ.-1)THEN
            IF(LDEBUG)WRITE(10,'(''  ++++++ GRGRAF DEBUG   :'',
     -           '' No active in-out workstation found.'')')
*** Warn if there is one while running in batch.
       ELSEIF(.NOT.STDSTR('INPUT'))THEN
            WRITE(10,'(''  ###### GRGRAF ERROR   : Workstation with'',
     -           '' input found in a batch job; please report.'')')
*** Otherwise wait for user response.
       ELSE
            IF(WAIT.AND.LWAITB)THEN
                 PRINT *,' '
                 PRINT *,' ----------------------------------'
                 PRINT *,' Graphics output - waiting for (CR)'
                 PRINT *,' ----------------------------------'
                 PRINT *,' '
                 READ(5,'(A80)',END=10) DUMMY
10               CONTINUE
            ENDIF
            IF(IWKREQ.NE.-1)CALL IGSG(IWKREQ)
       ENDIF
*** Clear screen if requested.
       IF(LGCLRB.AND.WAIT)THEN
*   Determine Operating State value.
            CALL GQOPS(IOPSTA)
            IF(LDEBUG)WRITE(10,'(''  ++++++ GRGRAF DEBUG   : Current'',
     -           '' GKS operating state: '',I1,''.'')') IOPSTA
*   Close current segment if open.
            IF(IOPSTA.EQ.4)CALL GCLSG
*   Do a clear on all active workstations, if there are any open.
            IF(IOPSTA.GE.3)THEN
                 CALL GQACWK(0,IERR,NACT,IWK)
                 IF(LDEBUG)WRITE(10,'(''  ++++++ GRGRAF DEBUG   :'',
     -                '' Number of active WS: '',I3,'', inq err: '',
     -                I3,''.'')') NACT,IERR
                 DO 40 I=1,NACT
                 CALL GQACWK(I,IERR,IDUM,IWK)
                 CALL GCLRWK(IWK,1)
                 IF(LDEBUG)WRITE(10,'(''  ++++++ GRGRAF DEBUG   :'',
     -                '' Clear sent to WS '',I3,'', inq err: '',
     -                I3,''.'')') IWK,IERR
40               CONTINUE
            ENDIF
*   Debugging information ?
       ELSEIF(LDEBUG)THEN
            WRITE(10,'(''  ++++++ GRGRAF DEBUG   : No clear'',
     -           '' of WS done because LGCLRB & WAIT=F.'')')
       ENDIF
       END
