CDECK  ID>, GRINIT.
       SUBROUTINE GRINIT
*-----------------------------------------------------------------------
*   GRINIT - Initialises the graphics system.
*   (Last changed on 15/ 9/08.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       CHARACTER*8 DATE,TIME
       CHARACTER*(MXNAME) FILE
       INTEGER NWORDS
       REAL RPAW
       PARAMETER (NWORDS=50000)
       COMMON /PAWC/ RPAW(NWORDS)
       EXTERNAL STDSTR
       LOGICAL STDSTR
       INTEGER IASF(13),IFAIL1,IFAIL2,IFAIL3,NCFILE,IOS
       DATA IASF /13*1/
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE GRINIT ///'
*** Fetch date and time.
       CALL DATTIM(DATE,TIME)
*** Open a file for GKS error messages.
       OPEN(UNIT=10,FILE=GKSLOG(1:NCGKS),STATUS='UNKNOWN',
     -      ACCESS='APPEND',IOSTAT=IOS,ERR=2020)
       CALL DSNLOG(GKSLOG(1:NCGKS),'GKS errors','Sequential',
     -      'Append    ')
       WRITE(10,'('' ========== New run on '',A8,'' at '',A8,
     -      '' ========== '')',ERR=2010,IOSTAT=IOS) DATE,TIME
*** Initialise HIGZ.
       CALL HLIMIT(NWORDS)
       CALL HPLINT(0)
C       CALL MZEBRA(-3)
C       CALL MZPAW(NWORDS,' ')
C       CALL IGINIT(0)
C       CALL IOPKS(10)
       CALL IGSET('PASS',1.0)
*** Set aspect-source flags.
       CALL GSASF(IASF)
*** Initialise the workstation table.
       NWK=0
*   First the terminal.
       IF(STDSTR('INPUT'))THEN
            NWK=NWK+1
            WKNAME(NWK)='TERMINAL'
            NCWKNM(NWK)=8
            CALL GRTERM(WKID(NWK),WKCON(NWK),WKSTAT(NWK),IFAIL1)
            WKFREF(NWK)=0
            WKLUN(NWK)=-1
            WKMULT(NWK)=.TRUE.
*   Open and activate.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! GRINIT WARNING : Terminal graphics'//
     -                ' graphics is currently disabled because of the'//
     -                ' above error.'
                 NWK=NWK-1
            ELSEIF(WKSTAT(NWK).GT.0.OR.WKSTAT(NWK).EQ.0)THEN
                 CALL GROPWK(WKNAME(NWK)(1:NCWKNM(NWK)),
     -                'IMMEDIATE')
                 CALL GRACWK(WKNAME(NWK)(1:NCWKNM(NWK)),
     -                'IMMEDIATE')
                 CALL GSDS(NWK,1,1)
            ELSE
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GRINIT DEBUG   :'',
     -                '' TERMINAL not defined at your request.'')')
                 NWK=NWK-1
            ENDIF
       ENDIF
*   Then the metafile.
       NWK=NWK+1
       WKNAME(NWK)='METAFILE'
       NCWKNM(NWK)=8
       CALL GRMETA(WKID(NWK),WKCON(NWK),FILE,NCFILE,WKSTAT(NWK),
     -      WKMULT(NWK),IFAIL2)
       CALL STRBUF('STORE',WKFREF(NWK),FILE,NCFILE,IFAIL3)
       WKLUN(2)=0
*   Open and activate.
       IF(IFAIL2.NE.0.OR.IFAIL3.NE.0)THEN
            PRINT *,' !!!!!! GRINIT WARNING : Metafile output'//
     -           ' is currently disabled because of the above error.'
            NWK=NWK-1
       ELSEIF(WKSTAT(NWK).GT.0.OR.
     -      (WKSTAT(NWK).EQ.0.AND..NOT.STDSTR('INPUT')))THEN
            CALL GROPWK(WKNAME(NWK)(1:NCWKNM(NWK)),
     -           'DELAY')
            CALL GRACWK(WKNAME(NWK)(1:NCWKNM(NWK)),
     -           'DELAY')
            CALL GSDS(NWK,3,1)
       ELSE
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GRINIT DEBUG   :'',
     -           '' METAFILE not defined at your request.'')')
            NWK=NWK-1
       ENDIF
*** Switch terminal to alpha-numeric mode.
       CALL GRALPH
*** Graphics options.
       LGRID=.FALSE.
       LGRALL=.TRUE.
       LOGX=.FALSE.
       LOGY=.FALSE.
       LSTAMP=.TRUE.
       LWAITA=.TRUE.
       LWAITB=.FALSE.
       LGCLRB=.TRUE.
       LGCLRA=.FALSE.
       LXCCH=.FALSE.
       LGLCLP=.TRUE.
       LGMCLP=.TRUE.
       LGACLP=.TRUE.
       LGTCLP=.TRUE.
       STAMP=' with Garfield version 7.45.'
       NCSTMP=28
*** Display size.
       DISPX0=0.0
       DISPX1=1.0
       DISPY0=0.0
       DISPY1=1.0
*** Window layout.
       GPXN  =0.007
       GPXN10=0.015
       GPYN  =0.007
       GPYN10=0.015
       GPXL  =0.01
       GPYL  =0.01
       GPXT  =0.01
*** Arrow top angle.
       ARRANG=30.0*PI/180.0
       ARRLEN=0.3
*** Bar chart width
       BARFRC=0.9
*** Histogram and bar chart sequence number
       IGHIST=0
       IGBAR =0
*** Handle problems when opening various files
       RETURN
2010   CONTINUE
       PRINT *,' ###### GRINIT ERROR   : Unable to write the graphics'//
     -      ' error logging file ; end of program execution.'
       CALL INPIOS(IOS)
       STOP
2020   CONTINUE
       PRINT *,' ###### GRINIT ERROR   : Unable to open the graphics'//
     -      ' error logging file ; end of program execution.'
       CALL INPIOS(IOS)
       STOP
       END
