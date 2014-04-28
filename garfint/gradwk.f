CDECK  ID>, GRADWK.
       SUBROUTINE GRADWK
*-----------------------------------------------------------------------
*   GRADWK - Adds a workstation to the workstation table.
*   (Last changed on  5/ 3/04.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*(MXCHAR) STR
       CHARACTER*(MXNAME) FILE
       CHARACTER*20 NAME
       LOGICAL KTYPE,KCONID,KOFF,KFILE,KGKSID
       INTEGER NC,IKEY,INEXT,NNWORD,NCFILE,IOFF,ICO,ICONID,IWKTYP,ICAT,
     -      I,IFAIL1,INPCMP,NCNAME,IERR,KMULT
       EXTERNAL INPCMP
*** Determine position of keyword.
       CALL INPSTR(1,1,STR,NC)
       IF(STR(1:1).EQ.'!'.AND.NC.EQ.1)THEN
            IKEY=2
       ELSE
            IKEY=1
       ENDIF
*** Warn if there are no arguments.
       CALL INPNUM(NNWORD)
       IF(NNWORD.EQ.IKEY)THEN
            PRINT *,' !!!!!! GRADWK WARNING : ADD-WORKSTATION needs'//
     -           ' arguments ; nothing done.'
            RETURN
       ENDIF
*** Initial values.
       FILE='GARFIELD.METAFILE'
       NCFILE=17
       IOFF=0
       ICONID=1
       IWKTYP=0
       ICAT=-1
       KMULT=0
*** First argument is the name of the workstation.
       CALL INPSTR(IKEY+1,IKEY+1,NAME,NCNAME)
*   Preset flags.
       KFILE=.FALSE.
       KGKSID=.FALSE.
       KTYPE=.FALSE.
       KCONID=.FALSE.
       KOFF=.FALSE.
*   Match with existing names.
       DO 10 I=1,NWK
       IF(NAME(1:NCNAME).EQ.WKNAME(I)(1:NCWKNM(I)))THEN
            PRINT *,' !!!!!! GRADWK WARNING : '//NAME(1:NCNAME)//
     -           ' is already defined ; not redefined.'
            RETURN
       ENDIF
10     CONTINUE
*** Loop over the rest of the string.
       INEXT=1
       DO 20 I=IKEY+2,NNWORD
       IF(I.LT.INEXT)GOTO 20
*   Type specification.
       IF(INPCMP(I,'TY#PE').NE.0)THEN
            IF(I+1.GT.NNWORD)THEN
                 CALL INPMSG(I,'Argument is missing.')
            ELSE
                 CALL INPSTR(I+1,I+1,STR,NC)
                 CALL GRWKID(STR(1:NC),IWKTYP,ICO,ICAT,IFAIL1)
                 IF(IFAIL1.EQ.0.AND.ICAT.EQ.2)THEN
                      ICONID=ICO
                 ELSEIF(IFAIL1.EQ.0)THEN
                      IOFF=ICO
                 ELSE
                      CALL INPMSG(I+1,'Not a valid workstation type.')
                 ENDIF
                 INEXT=I+2
                 KTYPE=.TRUE.
            ENDIF
*   GKS identifier.
       ELSEIF(INPCMP(I,'GKS-ID#ENTIFIER').NE.0)THEN
            IF(I+1.GT.NNWORD)THEN
                 CALL INPMSG(I,'Argument is missing.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,IWKTYP,0)
                 CALL GQWKCA(IWKTYP,IERR,ICAT)
                 IF(IERR.NE.0)CALL INPMSG(I+1,'GKS inquiry error.')
                 INEXT=I+2
                 KGKSID=.TRUE.
            ENDIF
*   Connection identifier.
       ELSEIF(INPCMP(I,'CON#NECTION-ID#ENTIFIER').NE.0)THEN
            IF(I+1.GT.NNWORD)THEN
                 CALL INPMSG(I,'Argument is missing.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,ICONID,0)
                 INEXT=I+2
                 KCONID=.TRUE.
            ENDIF
*   Logical unit offset.
       ELSEIF(INPCMP(I,'OFF#SET').NE.0)THEN
            IF(I+1.GT.NNWORD)THEN
                 CALL INPMSG(I,'Argument is missing.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,IOFF,0)
                 INEXT=I+2
                 KOFF=.TRUE.
            ENDIF
*   File name.
       ELSEIF(INPCMP(I,'F#ILE-NAME')+INPCMP(I,'NAME').NE.0)THEN
            IF(I+1.GT.NNWORD)THEN
                 CALL INPMSG(I,'Argument is missing.')
            ELSE
                 CALL INPSTR(I+1,I+1,FILE,NCFILE)
                 INEXT=I+2
                 KFILE=.TRUE.
            ENDIF
*   Multiple of single frame.
       ELSEIF(INPCMP(I,'M#ULTIPLE-FR#AME-#FILE').NE.0)THEN
            KMULT=+1
       ELSEIF(INPCMP(I,'S#INGLE-FR#AME-#FILE').NE.0)THEN
            KMULT=-1
*   Anything else is not valid.
       ELSE
            CALL INPMSG(I,'Not a valid keyword.')
       ENDIF
20     CONTINUE
*** Print error messages
       CALL INPERR
*** Check for invalid combinations.
       IF((ICAT.EQ.2.AND.KFILE).OR.
     -      ((ICAT.EQ.0.OR.ICAT.EQ.4).AND..NOT.KFILE).OR.
     -      (KFILE.AND.KCONID).OR.
     -      (.NOT.KFILE.AND.KOFF).OR.
     -      (.NOT.KTYPE.AND..NOT.KGKSID))THEN
            PRINT *,' !!!!!! GRADWK WARNING : Incomplete'//
     -           ' specification or, illegal combination of keywords'
            PRINT *,'                         or keywords used that'//
     -           ' are not appropriate for the workstation; ignored.'
            RETURN
       ELSEIF(ICAT.EQ.-1)THEN
            PRINT *,' !!!!!! GRADWK WARNING : No valid workstation'//
     -           ' type found; ignored.'
            RETURN
       ENDIF
       IF((.NOT.KFILE).AND.KMULT.NE.0)
     -      PRINT *,' !!!!!! GRADWK WARNING : Only workstations with'//
     -           ' output to a file can be single/multiple frame;'//
     -           ' option ignored.'
*** Store the information, increment workstation counter.
       IF(NWK.GE.MXWKLS)THEN
             PRINT *,' !!!!!! GRADWK WARNING : No storage left for'//
     -            ' workstations; ignored.'
             RETURN
       ENDIF
       NWK=NWK+1
*   Store the name.
       WKNAME(NWK)=NAME(1:NCNAME)
       NCWKNM(NWK)=NCNAME
*   GKS identifier.
       WKID(NWK)=IWKTYP
*   File attributes.
       IF(KFILE)THEN
            CALL STRBUF('STORE',WKFREF(NWK),FILE,NCFILE,IFAIL1)
            WKCON(NWK)=IOFF
            IF(KMULT.EQ.-1)THEN
                 WKMULT(NWK)=.FALSE.
            ELSEIF(KMULT.EQ.+1)THEN
                 WKMULT(NWK)=.TRUE.
            ELSEIF(INDEX(FILE(1:NCFILE),'{').NE.0.AND.
     -           INDEX(FILE(1:NCFILE),'}').NE.0)THEN
                 WKMULT(NWK)=.FALSE.
            ELSE
                 WKMULT(NWK)=.TRUE.
            ENDIF
       ELSE
            WKFREF(NWK)=-1
            WKCON(NWK)=ICONID
       ENDIF
*   Status.
       WKSTAT(NWK)=1
       WKSREQ(NWK)=1
       END
