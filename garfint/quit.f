CDECK  ID>, QUIT.
       SUBROUTINE QUIT
*-----------------------------------------------------------------------
*   QUIT   - This routines calls some routines that print information
*            collected during the run and closes in batch mode the
*            display file.
*   (Last changed on 26/10/09.)
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
       LOGICAL OPEN
       CHARACTER*20 OPSTR
*** Close neBEM.
       CALL BEMEND
*** Switch to graphics mode.
       CALL GRGRAF(.TRUE.)
*** Keep track of statistics, inquiry errors.
       IERSUM=0
       NOP=0
       NOP0=0
       NACT=0
       NACT0=0
*** Determine Operating State value.
       CALL GQOPS(IOPSTA)
*** Close current segment if open.
       IF(IOPSTA.EQ.4)CALL GCLSG
*** Deactivate all active workstations, if appropriate.
       IF(IOPSTA.GE.3)THEN
*   Get number of open workstations.
            CALL GQACWK(0,IERR,NACT,IWK)
            IF(IERR.NE.0)IERSUM=IERSUM+1
*   Loop over the open workstations.
            DO 10 I=NACT,1,-1
            CALL GQACWK(I,IERR,IDUM,IWK)
            IF(IERR.NE.0)IERSUM=IERSUM+1
            CALL GDAWK(IWK)
            WKSTAT(IWK)=2
            WKSREQ(IWK)=2
            CALL SGFLAG
10          CONTINUE
*   Count the number of still active workstations.
            NACT0=NACT
            CALL GQACWK(0,IERR,NACT,IWK)
            IF(IERR.NE.0)IERSUM=IERSUM+1
       ENDIF
*** Close all open workstations.
       IF(IOPSTA.GE.2)THEN
*   Get number of active workstations.
            CALL GQOPWK(0,IERR,NOP,IWK)
            IF(IERR.NE.0)IERSUM=IERSUM+1
*   Loop over the active workstations.
            DO 20 I=NOP,1,-1
*   Get workstation identifier.
            CALL GQOPWK(I,IERR,IDUM,IWK)
            IF(IERR.NE.0)IERSUM=IERSUM+1
*   Close the workstation.
            CALL GCLWK(IWK)
            WKSTAT(IWK)=1
            WKSREQ(IWK)=1
*   Check whether there is a file.
            IF(WKLUN(IWK).GT.0)THEN
                 CLOSE(UNIT=WKLUN(IWK),STATUS='KEEP',
     -                ERR=2034,IOSTAT=IOS)
                 GOTO 90
2034             CONTINUE
                 CALL INPIOS(IOS)
                 PRINT *,' !!!!!! QUIT   WARNING : Error closing'//
     -                ' file associated to workstation ',IWK,'.'
90               CONTINUE
            ENDIF
20          CONTINUE
*   Count the number of still active workstations.
            NOP0=NOP
            CALL GQOPWK(0,IERR,NOP,IWK)
            IF(IERR.NE.0)IERSUM=IERSUM+1
       ENDIF
*** And print error messages if any.
       IF(NACT.NE.0)PRINT *,' !!!!!! QUIT   WARNING : Unable to'//
     -      ' deactivate all workstations.'
       IF(NOP.NE.0)PRINT *,' !!!!!! QUIT   WARNING : Unable to'//
     -      ' close all workstations.'
       IF(IERSUM.NE.0)PRINT *,' !!!!!! QUIT   WARNING : Number of'//
     -      ' inquiry errors during GKS close-down: ',IERSUM
*** Print statistics if requested.
       IF(LDEBUG)THEN
            OPSTR='< unknown code >'
            IF(IOPSTA.EQ.0)OPSTR='GKS closed'
            IF(IOPSTA.EQ.1)OPSTR='GKS open'
            IF(IOPSTA.EQ.2)OPSTR='workstation open'
            IF(IOPSTA.EQ.3)OPSTR='workstation active'
            IF(IOPSTA.EQ.4)OPSTR='segment open'
            WRITE(LUNOUT,'(2X,''++++++ QUIT   DEBUG   : '',
     -           ''GKS state was '',A20/26X,
     -           ''Active workstations: '',I3,'' (was '',I3,'')''/26X,
     -           ''Open workstations:   '',I3,'' (was '',I3,'')''/26X,
     -           ''Inquiry errors:      '',I3)')
     -           OPSTR,NACT,NACT0,NOP,NOP0,IERSUM
       ENDIF
*** Close HIGZ.
       CALL IGTERM
       CALL IGEND
*** Close the GKS log file.
       INQUIRE(UNIT=10,OPENED=OPEN)
       IF(OPEN)CLOSE(UNIT=10,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
       GOTO 50
*   Error handling.
2030   CONTINUE
       CALL INPIOS(IOS)
       PRINT *,' !!!!!! QUIT   WARNING : Error closing the'//
     -      ' GKS error logging file during program termination.'
50     CONTINUE
*** Close the main metafiles.
       INQUIRE(UNIT=11,OPENED=OPEN)
       IF(OPEN)THEN
            PRINT *,' !!!!!! QUIT   WARNING : Found a metafile'//
     -           ' left open on unit 11; closing the file.'
            CLOSE(UNIT=11,STATUS='KEEP',ERR=2031,IOSTAT=IOS)
       ENDIF
       GOTO 60
*   Error handling.
2031   CONTINUE
       CALL INPIOS(IOS)
       PRINT *,' !!!!!! QUIT   WARNING : Error closing a'//
     -      ' graphics metafile during program termination.'
60     CONTINUE
*** Close additional metafiles, there shouldn't be any.
       DO 30 I=40,49
       INQUIRE(UNIT=I,OPENED=OPEN)
       IF(OPEN)THEN
            PRINT *,' !!!!!! QUIT   WARNING : Found a metafile'//
     -           ' left open on unit ',I,'; closing the file.'
            CLOSE(UNIT=I,STATUS='KEEP',ERR=2032,IOSTAT=IOS)
       ENDIF
       GOTO 30
*   Error handling.
2032   CONTINUE
       CALL INPIOS(IOS)
       PRINT *,' !!!!!! QUIT   WARNING : Error closing a'//
     -      ' graphics metafile during program termination.'
30     CONTINUE
*** Close the recording file.
       INQUIRE(UNIT=18,OPENED=OPEN)
       IF(OPEN)CLOSE(UNIT=18,STATUS='KEEP',ERR=2033,IOSTAT=IOS)
       GOTO 70
*   Error handling.
2033   CONTINUE
       CALL INPIOS(IOS)
       PRINT *,' !!!!!! QUIT   WARNING : Error closing the'//
     -      ' input recording file during program termination.'
70     CONTINUE
*** Print the graphics, dataset and timing log.
       CALL GRAPRT
       CALL DSNPRT
       CALL TIMLOG(' ')
*** List objects still in memory.
       IF(LDEBUG)THEN
            PRINT *,' ++++++ QUIT   DEBUG   : Histograms ...'
            CALL HISADM('LIST',IREF,0,0.0,0.0,.TRUE.,IFAIL)
            PRINT *,' ++++++ QUIT   DEBUG   : Matrices ...'
            CALL MATADM('LIST',IDUM,NDUM,NDUM,NDUM,IFAIL1)
            PRINT *,' ++++++ QUIT   DEBUG   : Booked objects ...'
            CALL BOOK('LIST',' ',' ',IFAIL)
            PRINT *,' ++++++ QUIT   DEBUG   : Strings ...'
            CALL STRBUF('DUMP',IREF,' ',1,IFAIL)
       ENDIF
*** Inform synchronisation.
       IF(LSYNCH)WRITE(6,'(''  >>>>>> quit'')')
*** And stop program execution.
       STOP
       END
