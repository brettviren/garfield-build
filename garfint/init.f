CDECK  ID>, INIT.
       subroutine init
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
       DOUBLE PRECISION WGT,FPRMAT,
     -      FPROJ,FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX,GYBOX,GZBOX
       REAL PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM
       INTEGER NLINED,NGRIDX,NGRIDY,ITRTYP,NTRLIN,NTRSAM,INDPOS,NCTRW,
     -      NTRFLX,NINORD,
     -      NCPNAM,NCXLAB,NCYLAB,NCFPRO,IPRMAT,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,ITFSRM,NTRERR
       LOGICAL LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG,LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       COMMON /PARMS / WGT(MXLIST),FPRMAT(3,3),
     -      FPROJ(3,3),FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX(12),GYBOX(12),GZBOX(12),
     -      PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM,
     -      INDPOS(11000),IPRMAT(3),NCTRW,NCPNAM,
     -      ITRTYP,NTRLIN,NTRSAM,NTRFLX,ITFSRM,NTRERR(10),
     -      NLINED,NINORD,NGRIDX,NGRIDY,NCXLAB,NCYLAB,NCFPRO,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,
     -      LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG(10),LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       CHARACTER*80 PARTID,PXLAB,PYLAB,PROLAB
       CHARACTER*10 PNAME
       CHARACTER*5  PRVIEW
       CHARACTER*(MXCHAR) FCNTRW
       COMMON /PARCHR/ PARTID,FCNTRW,PNAME,PXLAB,PYLAB,PROLAB,PRVIEW
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       INTEGER DOREF,IFREF,LINREF,CURLIN,CDOLVL,CIFLVL,TRACDO,TRACIF,
     -      ISTATE,NDOLIN,NLOOP,NIF
       COMMON /DODAT/ LINREF(MXDLIN,8),DOREF(MXDLVL,12),IFREF(MXILVL,5),
     -      TRACDO(0:MXDLVL),TRACIF(0:MXILVL),CURLIN,CDOLVL,CIFLVL,
     -      NDOLIN,NLOOP,NIF,ISTATE
       LOGICAL XDONE(0:MXGRID,0:MXGRID),YDONE(0:MXGRID,0:MXGRID),
     -      TRANS,CLAB
       REAL GRID(0:MXGRID,0:MXGRID),EPSTRA,EPSGRA,CXMIN,CXMAX,CYMIN,
     -      CYMAX,STINIT,DNTHR,DXGRA,DYGRA
       INTEGER ILOCGR(0:MXGRID,0:MXGRID),NBITER,NNITER,NFC,NGCMAX
       COMMON /CONDAT/ GRID,XDONE,YDONE,ILOCGR,
     -      NBITER,NNITER,EPSTRA,EPSGRA,DXGRA,DYGRA,
     -      STINIT,DNTHR,CXMIN,CXMAX,CYMIN,CYMAX,NFC,NGCMAX,TRANS,CLAB
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
       CHARACTER*(MXINCH+1) STRING
       CHARACTER*(MXINCH)   ARGSTR
       CHARACTER*30         ERRCDE(MXWORD)
       CHARACTER*(MXCHAR)   WORD(MXWORD)
       CHARACTER*80         PROMPT,EOFSTR,SHELL
       CHARACTER            ESCAPE
       CHARACTER*(MXNAME)   FNINP,FNOUT
       INTEGER NCHAR(MXWORD),INDWRD(MXWORD),ICHSET,LUNSTR(5:MXLUN,3),
     -      NWORD,LUN,NCPROM,NCEOF,NCSH,NCARG,NCFNI,NCFNO
       LOGICAL ERRPRT(MXWORD),LPROM,DOEXEC,DOREAD,LINREC
       COMMON /INPCOM/ NCHAR,INDWRD,LUNSTR,NWORD,LUN,ICHSET,NCPROM,
     -      ERRPRT,LPROM,DOEXEC,DOREAD,NCEOF,LINREC,NCSH,NCARG,
     -      NCFNI,NCFNO
       COMMON /INPCHR/ ERRCDE,STRING,WORD,PROMPT,EOFSTR,ESCAPE,SHELL,
     -      ARGSTR,FNINP,FNOUT
       integer inpcmx,arg_length,iarg,nargs,inext,j,iref,ifail
       character*128 args
       external inpcmx
       EXTERNAL STDSTR
       LOGICAL STDSTR
*** I/O init.
       LUNOUT   =6
*** Graphics data.
       DISPX0   =0.0
       DISPX1   =1.0
       DISPY0   =0.0
       DISPY1   =1.0
       LGCLRB   =.TRUE.
       LGCLRA   =.FALSE.
*** Parameters for contour plotting in /CONDAT/.
       NBITER=10
       NNITER=10
       EPSTRA=1.0E-3
       EPSGRA=1.0E-3
       STINIT=0.174123
       DNTHR=0.1
       NGCMAX=500
*** Initial data statements for the /PRTPLT/ common block.
       JFAIL    =1
       JEXMEM   =2
       LINPUT   =.NOT.STDSTR('INPUT')
       LCELPR   =.FALSE.
       LCELPL   =.FALSE.
       LWRMRK   =.FALSE.
       LISOCL   =.FALSE.
       LCHGCH   =.FALSE.
       LDRPLT   =.FALSE.
       LDRPRT   =.FALSE.
       LCLPRT   =.TRUE.
       LCLPLT   =.TRUE.
       LIDENT   =.FALSE.
       LDEBUG   =.FALSE.
       LRNDMI   =.TRUE.
       LPROPR   =.TRUE.
       LPROF    =.TRUE.
       LMAPCH   =.FALSE.
       LCNTAM   =.TRUE.
       LINREC   =STDSTR('INPUT')
       LGSTOP   =.FALSE.
       LSYNCH   =.FALSE.
*** GKS error logging file name.
       GKSLOG   ='GKS_error.log'
       NCGKS    =13
*** Parameters.
       NGRIDX=25
       NGRIDY=25
*   Count the number of arguments, pointer vector will not be used.
       nargs=iargc()
*   Loop over arguments, deleting those we recognise.
       inext=1
       do 30 iarg=1,nargs
       if(iarg.lt.inext)goto 30
       call argget(iarg,args,arg_length)
*   Debugging options.
       if(inpcmx(args(1:arg_length),'-deb#ug').ne.0)then
            ldebug=.true.
       elseif(inpcmx(args(1:arg_length),'-nodeb#ug').ne.0)then
            ldebug=.false.
*   Tracing options.
       elseif(inpcmx(args(1:arg_length),'-id#entification').ne.0)then
            lident=.true.
       elseif(inpcmx(args(1:arg_length),'-noid#entification').ne.0)then
            lident=.false.
*   Input listing.
       elseif(inpcmx(args(1:arg_length),'-in#put_listing').ne.0)then
            linput=.true.
       elseif(inpcmx(args(1:arg_length),'-noin#put_listing').ne.0)then
            linput=.false.
*   Random number initialisation.
       elseif(inpcmx(args(1:arg_length),
     -      '-RNDM#_initialisation').ne.0)then
            lrndmi=.true.
       elseif(inpcmx(args(1:arg_length),
     -      '-noRNDM#_initialisation').ne.0)then
            lrndmi=.false.
*   Progress printing.
       elseif(inpcmx(args(1:arg_length),'-pro#gress_print').ne.0)then
            lpropr=.true.
       elseif(inpcmx(args(1:arg_length),'-nopro#gress_print').ne.0)then
            lpropr=.false.
*   Input recording.
       elseif(inpcmx(args(1:arg_length),'-rec#ording').ne.0)then
            if(STDSTR('INPUT'))linrec=.true.
       elseif(inpcmx(args(1:arg_length),'-norec#ording').ne.0)then
            linrec=.false.
*   Reading of profile file.
       elseif(inpcmx(args(1:arg_length),'-pr#ofile').ne.0)then
            lprof=.true.
       elseif(inpcmx(args(1:arg_length),'-nopr#ofile').ne.0)then
            lprof=.false.
*   Synchronisation prompt.
       elseif(inpcmx(args(1:arg_length),'-synch#ronise').ne.0)then
            lsynch=.true.
       elseif(inpcmx(args(1:arg_length),'-nosynch#ronise').ne.0)then
            lsynch=.false.
*   GKS error logging file.
       elseif(inpcmx(args(1:arg_length),'-GKSlog').ne.0)then
            if(iarg+1.le.nargs)then
                 call argget(iarg+1,args,arg_length)
                 gkslog=args(1:arg_length)
                 ncgks=min(mxname,arg_length)
                 if(arg_length.gt.mxname)
     -                print *,' !!!!!! INIT   WARNING : Name of GKS'//
     -                ' error logging file too long; truncated.'
                 inext=iarg+2
            else
                 print *,' !!!!!! INIT   WARNING : File name missing'//
     -                ' following the -GKSlog option.'
            endif
*   Terminal and metafile type.
       elseif(inpcmx(args(1:arg_length),'-term#inal')+
     -      inpcmx(args(1:arg_length),'-meta#file')+
     -      inpcmx(args(1:arg_length),'-interact#ive')+
     -      inpcmx(args(1:arg_length),'-batch').ne.0)then
            do 50 j=iarg+1,nargs
            call argget(j,args,arg_length)
            if(args(1:1).eq.'-'.and.arg_length.gt.1)then
                 inext=j
                 goto 30
            endif
50          continue
            inext=nargs+1
*   Command line arguments.
       elseif(inpcmx(args(1:arg_length),'-arg#uments').ne.0)then
            ncarg=0
            do 60 j=iarg+1,nargs
            call argget(j,args,arg_length)
            if(inpcmx(args(1:arg_length),'-batch')+
     -           inpcmx(args(1:arg_length),'-interact#ive')+
     -           inpcmx(args(1:arg_length),'-GKSlog')+
     -           inpcmx(args(1:arg_length),'-deb#ug')+
     -           inpcmx(args(1:arg_length),'-nodeb#ug')+
     -           inpcmx(args(1:arg_length),'-id#entification')+
     -           inpcmx(args(1:arg_length),'-noid#entification')+
     -           inpcmx(args(1:arg_length),'-in#put_listing')+
     -           inpcmx(args(1:arg_length),'-noin#put_listing')+
     -           inpcmx(args(1:arg_length),'-meta#file')+
     -           inpcmx(args(1:arg_length),'-nometa#file')+
     -           inpcmx(args(1:arg_length),'-pr#ofile')+
     -           inpcmx(args(1:arg_length),'-nopr#ofile')+
     -           inpcmx(args(1:arg_length),'-pro#gress_print')+
     -           inpcmx(args(1:arg_length),'-nopro#gress_print')+
     -           inpcmx(args(1:arg_length),'-rec#ording')+
     -           inpcmx(args(1:arg_length),'-norec#ording')+
     -           inpcmx(args(1:arg_length),
     -                '-RNDM#_initialisation')+
     -           inpcmx(args(1:arg_length),
     -                '-noRNDM#_initialisation')+
     -           inpcmx(args(1:arg_length),'-synch#ronise')+
     -           inpcmx(args(1:arg_length),'-nosynch#ronise')+
     -           inpcmx(args(1:arg_length),'-term#inal')+
     -           inpcmx(args(1:arg_length),'-noterm#inal').eq.0)then
                 if(ncarg+1.le.len(argstr))then
                      argstr(ncarg+1:)=args(1:arg_length)//' '
                      ncarg=min(len(argstr),ncarg+arg_length+1)
                 else
                      print *,' !!!!!! INIT   WARNING : Command'//
     -                     ' line arguments too long; truncated.'
                 endif
                 inext=j+1
            else
                 goto 70
            endif
60          continue
70          continue
            if(ncarg.gt.1)ncarg=ncarg-1
            if(ncarg.lt.1)then
                 argstr=' '
                 ncarg=1
            endif
*   Anything else is not valid.
       elseif(inpcmx(args(1:arg_length),'-noterm#inal')+
     -      inpcmx(args(1:arg_length),'-nometa#file').eq.0)then
            print *,' !!!!!! INIT   WARNING : Unrecognised option "'//
     -           args(1:arg_length)//'" found on the command line.'
       endif
30     continue
*** Algebra, histogram, matrix and graphics.
       call algint
       call hisint
       call grinit
       call matint
*** Global variable initialisation.
       GLBVAR(1)='TIME_LEFT '
       GLBMOD(1)=2
       CALL TIMEL(GLBVAL(1))
       GLBVAR(2)='MACHINE   '
       IREF=-1
       CALL STRBUF('STORE',IREF,'Unix',4,IFAIL)
       IF(IREF.LT.0)
     -      CALL STRBUF('STORE',IREF,'< not known >',13,IFAIL)
       GLBMOD(2)=1
       GLBVAL(2)=IREF
       GLBVAR(3)='INTERACT  '
       GLBVAR(4)='BATCH     '
       GLBMOD(3)=3
       GLBMOD(4)=3
       IF(STDSTR('INPUT'))THEN
            GLBVAL(3)=1
            GLBVAL(4)=0
       ELSE
            GLBVAL(3)=0
            GLBVAL(4)=1
       ENDIF
       GLBVAR(5)='OK        '
       GLBMOD(5)=3
       GLBVAL(5)=1
       GLBVAR(7)='OUTPUT    '
       CALL STRBUF('STORE',IREF,'Standard output',15,IFAIL)
       GLBMOD(7)=1
       GLBVAL(7)=IREF
       GLBVAR(8)='X         '
       GLBMOD(8)=2
       GLBVAL(8)=0
*   Plot frame number
       GLBVAR(9)='FRAME     '
       GLBMOD(9)=2
       GLBVAL(9)=0
       NGLB=9
*** DO loop initialisation.
       ISTATE=-2
*** Options.
       LDEBUG=.FALSE.
       LIDENT=.FALSE.
*** Input initialisation.
       LPROF=.TRUE.
       LPROPR=.TRUE.
       call inpint
       call plaint
       end
