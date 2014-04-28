CDECK  ID>, GRTERMA.
       SUBROUTINE GRTERM(IWKTYP,ICON,IFLAG,IFAIL)
*-----------------------------------------------------------------------
*   GRTERM - Returns the workstation identifier from the command line.
*            Version for GKS.
*   (Last changed on 19/12/10.)
*-----------------------------------------------------------------------
       implicit none
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       integer nargs, arg_length
       character*128 args
       integer istart,iend,ionoff,iflag,iarg,iwktyp,icon,ifail,
     -      iwkr,iconr,icat,ifail1,inext,ierr,idum,inpcmx
       external inpcmx
*** Default settings.
       call grwkid('*interactive_default',iwktyp,icon,icat,idum)
       ifail=1
*** Pick up the value from the command line, count arguments.
       nargs=iargc()
*** Find the area devoted to the -terminal option.
       istart=0
       iend=nargs
       ionoff=0
       iflag=0
       do iarg=1,nargs
       call argget(iarg,args,arg_length)
       if(args(1:1).eq.'-'.and.arg_length.gt.1.and.istart.ne.0)then
            iend=iarg-1
            goto 10
       elseif(inpcmx(args(1:arg_length),'-term#inal').ne.0)then
            istart=iarg+1
            ionoff=1
       elseif(inpcmx(args(1:arg_length),'-noterm#inal').ne.0)then
            ionoff=-1
       endif
       enddo
10     continue
*** Return here if there is a -noterminal or no -terminal.
       if(ionoff.eq.0)then
            ifail=0
            if(ldebug)write(lunout,'(''  ++++++ GRTERM DEBUG   :'',
     -           '' No -terminal qualifier present.'')')
            iflag=0
            goto 100
       elseif(ionoff.eq.-1)then
            ifail=0
            if(ldebug)write(lunout,'(''  ++++++ GRTERM DEBUG   :'',
     -           '' Request not to produce terminal graphics.'')')
            iflag=0
            iwktyp=0
            return
       else
            iflag=+1
       endif
*** Decode the part about the terminal.
       inext=istart
       do 20 iarg=istart,iend
       if(iarg.lt.inext)goto 20
**  Retrieve the sub-keyword.
       call argget(iarg,args,arg_length)
**  Terminal type.
       if(inpcmx(args(1:arg_length),'t#ype').ne.0)then
*   Check there indeed is an argument.
            if(iarg.eq.iend)then
                 PRINT *,' !!!!!! GRTERM WARNING : The argument'//
     -                ' for "type" is missing.'
                 ifail=1
                 goto 100
            endif
*   Retrieve the argument.
            call argget(iarg+1,args,arg_length)
*   Compare with the workstation type list.
            call grwkid(args(1:arg_length),iwkr,iconr,icat,ifail1)
*   Check that this is a good interactive workstation type.
            if(icat.ne.2.or.ifail1.ne.0)then
                 PRINT *,' !!!!!! GRTERM WARNING : Terminal type '//
     -                args(1:arg_length)//' not valid or not for'//
     -                ' interactive use.'
                 ifail=1
                 return
            endif
            iwktyp=iwkr
            icon=iconr
*   Debugging output.
            if(ldebug)write(lunout,'(''  ++++++ GRTERM DEBUG   :'',
     -           '' Terminal type '',A,'', GKS id '',I5,''.'')')
     -           args(1:arg_length),iwktyp
            inext=iarg+2
**  Terminal type via GKS identifier.
       elseif(inpcmx(args(1:arg_length),'GKS#_identifier').ne.0)then
*   Check there indeed is an argument.
            if(iarg.eq.iend)then
                 PRINT *,' !!!!!! GRTERM WARNING : The argument'//
     -                ' for "GKS_identifier" is missing.'
                 ifail=1
                 goto 100
            endif
*   Retrieve the argument.
            call argget(iarg+1,args,arg_length)
*   Attempt to read the integer.
            call inpric(args(1:arg_length),iwkr,0,ifail1)
            if(ifail1.ne.0)then
                 print *,' !!!!!! GRTERM WARNING : The terminal'//
     -                ' GKS identifier is not a valid integer.'
                 ifail=1
                 goto 100
            endif
*   Check workstation category.
            call gqwkca(iwkr,ierr,icat)
            if(icat.ne.2.or.ierr.ne.0)then
                 PRINT *,' !!!!!! GRTERM WARNING : Terminal type '//
     -                args(1:arg_length)//' not valid or not for'//
     -                ' interactive use.'
                 ifail=1
                 goto 100
            endif
*   Store the workstation type.
            iwktyp=iwkr
*   Debugging output.
            if(ldebug)write(lunout,'(''  ++++++ GRTERM DEBUG   :'',
     -           '' GKS identifier '',I5,'' given for terminal'',
     -           '' type.'')') iwktyp
            inext=iarg+2
**  Connection identifier.
       elseif(inpcmx(args(1:arg_length),
     -      'c#onnection_identifier').ne.0)then
*   Check there indeed is an argument.
            if(iarg.eq.iend)then
                 PRINT *,' !!!!!! GRTERM WARNING : The argument'//
     -                ' for "connection_identifier" is missing.'
                 ifail=1
                 return
            endif
*   Retrieve the argument.
            call argget(iarg+1,args,arg_length)
*   Attempt to read the number.
            call inpric(args(1:arg_length),iconr,0,ifail1)
            if(ifail1.ne.0)then
                 print *,' !!!!!! GRTERM WARNING : The terminal'//
     -                ' connection identifier is not a valid integer.'
                 ifail=1
                 return
            endif
            icon=iconr
*   Debugging output.
            if(ldebug)write(lunout,'(''  ++++++ GRTERM DEBUG   :'',
     -           '' Terminal connection identifier '',I3,''.'')')
     -           icon
            inext=iarg+2
**  Anything else is not valid.
       else
            print *,' !!!!!! GRTERM WARNING : The keyword '//
     -           args(1:arg_length)//' is not valid within'//
     -           ' -terminal; is ignored.'
       endif
20     continue
*** Continue here in case of errors.
100    continue
*** Check whether an inquiry is required.
       if(iwktyp.eq.-1.and.iflag.ge.0)then
            call igwkty(iwktyp)
            icon=0
       endif
*** Things worked fine.
       ifail=0
       end
