CDECK  ID>, GROPWK.
       SUBROUTINE GROPWK(NAME,OPTION)
*-----------------------------------------------------------------------
*   GROPWK - Opens a workstation - version for GKS.
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
       INTEGER INPCMX,IFAIL,IFAIL1,IWK,I,NC,IERR,ISTATE,IOS
       CHARACTER*(*) NAME,OPTION
       CHARACTER*(MXNAME) AUX
       LOGICAL OPENED
*** Locate workstation.
       CALL GRQIWK(NAME,IWK,IFAIL)
       IF(IFAIL.NE.0)RETURN
*** Delayed processing.
       IF(OPTION.EQ.'DELAY'.AND..NOT.WKMULT(IWK))THEN
            WKSREQ(IWK)=2
            RETURN
       ENDIF
*** Check the current state of the workstation.
       IF(WKSTAT(IWK).GE.2)THEN
            PRINT *,' !!!!!! GROPWK WARNING : Workstation ',NAME,
     -           ' is already open ; not opened.'
            RETURN
       ENDIF
*** Open the workstation: case of a workstation associated with a file.
       IF(WKFREF(IWK).GT.0)THEN
*   Find a free logical unit.
            WKLUN(IWK)=0
            INQUIRE(UNIT=11,OPENED=OPENED)
            IF(OPENED)THEN
                 DO 20 I=40,49
                 INQUIRE(UNIT=I,OPENED=OPENED)
                 IF(.NOT.OPENED)THEN
                      WKLUN(IWK)=I
                      GOTO 30
                 ENDIF
20               CONTINUE
                 PRINT *,' !!!!!! GROPWK WARNING : All logical units'//
     -                ' reserved for metafiles are in use ; not opened.'
                 RETURN
30               CONTINUE
            ELSE
                 WKLUN(IWK)=11
            ENDIF
*   Retrieve the file name.
            CALL STRBUF('READ',WKFREF(IWK),AUX,NC,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! GROPWK WARNING : Unable to retrieve'//
     -                ' the file name of the workstation; not opened.'
                 RETURN
            ENDIF
*   Perform global variable substitution.
            CALL INPSUB(AUX(1:NC),NC,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! GROPWK WARNING : Substitution of'//
     -                ' global variables in the file name of the'//
     -                ' workstation failed; not opened.'
                 RETURN
            ENDIF
*   And open the file.
            CALL DSNOPN(AUX(1:NC),NC,WKLUN(IWK),'WRITE-FILE',IFAIL)
            IF(OPENED)THEN
                 PRINT *,' !!!!!! GROPWK WARNING : Unable to open '//
     -                AUX(1:NC)//' as metafile for workstation ',
     -                NAME,'; left in "defined" state.'
                 RETURN
            ENDIF
            CALL DSNLOG(AUX(1:NC),'Metafile  ','Sequential',
     -            'Write     ')
*   And open the workstation.
            CALL GOPWK(IWK,WKLUN(IWK)+WKCON(IWK),WKID(IWK))
            WKSTAT(IWK)=2
*   Debugging output.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GROPWK DEBUG   :'',
     -           '' File '',A,'' opened on unit '',I2,'' for'',
     -           '' workstation '',A,'' of type '',I5,''.'')')
     -           AUX(1:NC),WKLUN(IWK),NAME,WKID(IWK)
*** Open the workstation: no associated file.
       ELSE
            CALL GOPWK(IWK,WKCON(IWK),WKID(IWK))
            WKSTAT(IWK)=2
*   Debugging output.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GROPWK DEBUG   :'',
     -           '' Workstation '',A,'' of type '',I5,'' opened'',
     -           '' without associated file.'')') NAME,WKID(IWK)
       ENDIF
*** Check that the workstation is really open.
       CALL GQWKS(IWK,IERR,ISTATE)
       IF(IERR.EQ.7.OR.IERR.EQ.25)THEN
            PRINT *,' !!!!!! GROPWK WARNING : Workstation ',NAME,
     -           ' could not be opened.'
            WKSTAT(IWK)=1
            RETURN
       ELSEIF(IERR.EQ.20)THEN
            PRINT *,' !!!!!! GROPWK WARNING : Cannot open ',NAME,
     -           ' because the workstation identifier is not valid.'
            WKSTAT(IWK)=1
            RETURN
       ENDIF
*** Set the workstation window.
       CALL GSWKWN(IWK,0.0,1.0,0.0,1.0)
*** End of normal processing.
       RETURN
       END
