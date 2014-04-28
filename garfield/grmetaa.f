CDECK  ID>, GRMETAA.
       SUBROUTINE GRMETA(IWKTYP,IOFF,FILE,NCFILE,IFLAG,LMULT,IFAIL)
*-----------------------------------------------------------------------
*   GRMETA - Returns the workstation identifier from the command line.
*   (Last changed on 19/12/10.)
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
       integer nargs,iarg
       character*(*) file
       character*128 args
       integer iwktyp,ioff,ncfile,iflag,ifail,arg_length,inpcmx,istart,
     -      iend,ionoff,icat,idum,inext,iwkr,ioffr,ifail1,ierr,kmult
       logical lmult
       external inpcmx
*** Default settings.
       call grwkid('*batch_default',iwktyp,ioff,icat,idum)
       file='garfield.metafile'
       ncfile=17
       ifail=1
       kmult=0
       lmult=.true.
*** Pick up the value from the command line, count arguments.
       nargs=iargc()
*** Find the area devoted to the -metafile option.
       istart=0
       iend=nargs
       ionoff=0
       iflag=0
       do iarg=1,nargs
       call argget(iarg,args,arg_length)
       if(args(1:1).eq.'-'.and.arg_length.gt.1.and.istart.ne.0)then
            iend=iarg-1
            goto 10
       elseif(inpcmx(args(1:arg_length),'-meta#file').ne.0)then
            istart=iarg+1
            ionoff=1
       elseif(inpcmx(args(1:arg_length),'-nometa#file').ne.0)then
            ionoff=-1
       endif
       enddo
10     continue
*** Return here if there is a -nometafile or no -metafile.
       if(ionoff.eq.0)then
            ifail=0
            if(ldebug)write(lunout,'(''  ++++++ GRMETA DEBUG   :'',
     -           '' No -metafile qualifier present.'')')
            iflag=0
            return
       elseif(ionoff.eq.-1)then
            ifail=0
            iflag=-1
            if(ldebug)write(lunout,'(''  ++++++ GRMETA DEBUG   :'',
     -           '' Request not to produce a metafile.'')')
            return
       else
            iflag=+1
       endif
*** Decode the part about the metafile.
       inext=istart
       do 20 iarg=istart,iend
       if(iarg.lt.inext)goto 20
**  Retrieve the sub-keyword.
       call argget(iarg,args,arg_length)
**  Metafile type.
       if(inpcmx(args(1:arg_length),'t#ype').ne.0)then
*   Check there indeed is an argument.
            if(iarg.eq.iend)then
                 PRINT *,' !!!!!! GRMETA WARNING : The argument'//
     -                ' for "type" is missing.'
                 ifail=1
                 return
            endif
*   Retrieve the argument.
            call argget(iarg+1,args,arg_length)
*   Compare with standard lists.
            call grwkid(args(1:arg_length),iwkr,ioffr,icat,ifail1)
            if((icat.ne.0.and.icat.ne.4).or.ifail1.ne.0)then
                 PRINT *,' !!!!!! GRMETA WARNING : Metafile type '//
     -                args(1:arg_length)//' not valid or only for'//
     -                ' interactive use.'
                 ifail=1
                 return
            endif
            iwktyp=iwkr
            ioff=ioffr
*   Debugging output.
            if(ldebug)write(lunout,'(''  ++++++ GRMETA DEBUG   :'',
     -           '' Metafile type '',A,'', GKS id '',I5,''.'')')
     -           args(1:arg_length),iwktyp
            inext=iarg+2
**  Metafile type via GKS identifier.
       elseif(inpcmx(args(1:arg_length),'GKS#_identifier').ne.0)then
*   Check there indeed is an argument.
            if(iarg.eq.iend)then
                 PRINT *,' !!!!!! GRMETA WARNING : The argument'//
     -                ' for "GKS_identifier" is missing.'
                 ifail=1
                 return
            endif
*   Retrieve the argument.
            call argget(iarg+1,args,arg_length)
*   Attempt to read the integer.
            call inpric(args(1:arg_length),iwkr,0,ifail1)
            if(ifail1.ne.0)then
                 print *,' !!!!!! GRMETA WARNING : The metafile'//
     -                ' GKS identifier is not a valid integer.'
                 ifail=1
                 return
            endif
*   Check workstation category.
            call gqwkca(iwkr,ierr,icat)
            if((icat.ne.0.and.icat.ne.4).or.ierr.ne.0)then
                 PRINT *,' !!!!!! GRMETA WARNING : Metafile type '//
     -                args(1:arg_length)//' not valid or only for'//
     -                ' interactive use.'
                 ifail=1
                 return
            endif
*   Store the workstation type.
            iwktyp=iwkr
*   Debugging output.
            if(ldebug)write(lunout,'(''  ++++++ GRMETA DEBUG   :'',
     -           '' GKS identifier '',I5,'' given for metafile'',
     -           '' type.'')') iwktyp
            inext=iarg+2
**  Connection offset.
       elseif(inpcmx(args(1:arg_length),'o#ffset').ne.0)then
*   Check there indeed is an argument.
            if(iarg.eq.iend)then
                 PRINT *,' !!!!!! GRMETA WARNING : The argument'//
     -                ' for "offset" is missing.'
                 ifail=1
                 return
            endif
*   Retrieve the argument.
            call argget(iarg+1,args,arg_length)
*   Attempt to read the number.
            call inpric(args(1:arg_length),ioffr,0,ifail1)
            if(ifail1.ne.0)then
                 print *,' !!!!!! GRMETA WARNING : The metafile'//
     -                ' connection offset is not a valid integer.'
                 ifail=1
                 return
            endif
            ioff=ioffr
*   Debugging output.
            if(ldebug)write(lunout,'(''  ++++++ GRMETA DEBUG   :'',
     -           '' Metafile connection offset '',I3,''.'')')
     -           ioff
            inext=iarg+2
**  Metafile file-name.
       elseif(inpcmx(args(1:arg_length),'n#ame').ne.0)then
*   Check there indeed is an argument.
            if(iarg.eq.iend)then
                 PRINT *,' !!!!!! GRMETA WARNING : The argument'//
     -                ' for "name" is missing.'
                 ifail=1
                 return
            endif
*   Retrieve the argument.
            call argget(iarg+1,args,arg_length)
*   Check the length.
            if(arg_length.gt.mxname)then
                 print *,' !!!!!! GRMETA WARNING : The file name'//
     -                ' of the metafile is too long.'
                 ifail=1
                 return
            else
                 file=args
                 ncfile=arg_length
            endif
*   Debugging output.
            if(ldebug)write(lunout,'(''  ++++++ GRMETA DEBUG   :'',
     -           '' Metafile file-name '',A,''.'')') FILE(1:NCFILE)
            inext=iarg+2
**  Single or multiple frame.
       elseif(inpcmx(args(1:arg_length),
     -      's#ingle-fr#ame-#file').ne.0)then
            kmult=-1
       elseif(inpcmx(args(1:arg_length),
     -      'm#ultiple-fr#ame-#file').ne.0)then
            kmult=+1
**  Anything else is not valid.
       else
            print *,' !!!!!! GRMETA WARNING : The keyword '//
     -           args(1:arg_length)//' is not valid within'//
     -           ' -metafile; is ignored.'
       endif
20     continue
*** Set to single-frame-file if there are { } in the file name.
       if(kmult.eq.-1)then
            lmult=.false.
       elseif(kmult.eq.+1)then
            lmult=.true.
       elseif(index(file(1:ncfile),'{').ne.0.and.
     -      index(file(1:ncfile),'}').ne.0)then
            lmult=.false.
       else
            lmult=.true.
       endif
*** Things worked fine.
       ifail=0
       end
