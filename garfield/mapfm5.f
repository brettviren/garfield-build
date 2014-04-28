CDECK  ID>, MAPFM5.
       SUBROUTINE MAPFM5(FMAP,NCMAP,IDATA,IWMAP,DELBKG,IFAIL)
*-----------------------------------------------------------------------
*   MAPFM5 - Reads a Maxwell 3D Field Simulator version 4.0 table of
*            tetrahedrons.
*   (Last changed on  7/10/07.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL EXMAP,EYMAP,EZMAP,VMAP,EWXMAP,EWYMAP,EWZMAP,VWMAP,
     -      BXMAP,BYMAP,BZMAP,
     -      XMAP,YMAP,ZMAP,XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,
     -      VMMIN,VMMAX,EPSMAT,EPSSUR,XFMOFF,YFMOFF,ZFMOFF
       INTEGER MATMAP,NMAP,NEPS,MAPORD,MAPTYP,IDRMAT,INDEWS,
     -      NWMAP
       LOGICAL MAPFLG,LMAPPL,SETAX,SETAY,SETAZ,ELMDGN,LSFDER
       CHARACTER EWSTYP
       CHARACTER*10 MATSRC
       COMMON /FLDMAP/ VMAP(MXMAP,10),VWMAP(MXMAP,10,MXWMAP),
     -      EXMAP(MXMAP,10),EYMAP(MXMAP,10),EZMAP(MXMAP,10),
     -      EWXMAP(MXMAP,10,MXWMAP),EWYMAP(MXMAP,10,MXWMAP),
     -      EWZMAP(MXMAP,10,MXWMAP),
     -      BXMAP(MXMAP,10),BYMAP(MXMAP,10),BZMAP(MXMAP,10),
     -      XMAP(MXMAP,10),YMAP(MXMAP,10),ZMAP(MXMAP,10),
     -      XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,VMMIN,VMMAX,
     -      XFMOFF,YFMOFF,ZFMOFF,
     -      EPSMAT(MXEPS),EPSSUR(MXEPS),MATMAP(MXMAP),
     -      NMAP,NEPS,MAPORD,MAPTYP,IDRMAT,INDEWS(MXWMAP),NWMAP,
     -      MAPFLG(10+4*MXWMAP),ELMDGN(MXMAP),
     -      LMAPPL,SETAX,SETAY,SETAZ,LSFDER
       COMMON /FLDCHR/ EWSTYP(MXWMAP),MATSRC
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
       INTEGER IEPS,ICONT(3),IMAX,NUSE,IWMAP,
     -      I,J,K,NCMAP,IFAIL,IFAIL1,IFAIL2,IFAIL3,IFAIL4,IFAIL5,
     -      IT1,IT2,IT3,IT4,ITETRA,IOS,NC,INPCMP,
     -      IDATA,NWORD,NCAUX,IEND,IP,NTETRA,NTOTAL,NPOINT,
     -      MTETRA,LOOKUP(10),NDELET,INEAR
C       INTEGER MPOINT
       REAL TEMP(10),SUM,ECOMP,DCOMP,DX,DY,DZ,XP,YP,ZP,DNEAR
       CHARACTER*(*) FMAP
       CHARACTER*(MXNAME) FAUX
       CHARACTER*80 STRING
       LOGICAL SCALAR,READ,NEWEPS,EXIST,EXIST2,DELBKG,
     -      DELFLG(MXMAP),FIRST
       EXTERNAL INPCMP
       SAVE NTETRA,NPOINT,LOOKUP
       DATA NTETRA/0/, NPOINT/0/
       DATA LOOKUP/1, 5, 6, 7, 2, 8, 9, 3, 10, 4/
**** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MAPFM5 ///'
*** Assume that this will fail.
       IFAIL=1
*** Make sure the file names are not too long.
       IF(NCMAP.GT.MXNAME)THEN
            PRINT *,' !!!!!! MAPFM5 WARNING : Field map file name ',
     -           FMAP(1:NCMAP),' is too long; not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*** Check for mesh files - or guess mesh name if there is no mesh yet.
       IF(IDATA.EQ.1)THEN
*   Get rid of extensions, if any.
            IEND=0
            DO 1050 I=NCMAP,1,-1
            IF(FMAP(I:I).NE.' '.AND.IEND.EQ.0)IEND=I
            IF(FMAP(I:I).EQ.'/')THEN
                 IF(IEND.GT.0)THEN
                      FAUX=FMAP(1:IEND)//'.'
                      NCAUX=IEND+1
                 ELSE
                      FAUX=FMAP
                      NCAUX=NCMAP
                 ENDIF
                 GOTO 1060
            ELSEIF(FMAP(I:I).EQ.'.')THEN
                 FAUX=FMAP(1:I)
                 NCAUX=I
                 GOTO 1060
            ENDIF
1050        CONTINUE
            IF(IEND.EQ.0)THEN
                 PRINT *,' !!!!!! MAPFM5 WARNING : Mesh file name'//
     -                ' empty ; not read.'
                 CALL INPSWI('RESTORE')
                 RETURN
            ENDIF
            FAUX=FMAP(1:IEND)//'.'
            NCAUX=IEND+1
1060        CONTINUE
*   Verify that the resulting file name is not too long.
            IF(NCAUX+3.GT.MXNAME)THEN
                 PRINT *,' !!!!!! MAPFM5 WARNING : Mesh file name'//
     -                ' too long after expansion ; not read.'
                 CALL INPSWI('RESTORE')
                 RETURN
            ENDIF
*   Check for the existence of the files.
            CALL DSNINQ(FAUX(1:NCAUX)//'hyd',NCAUX+3,EXIST)
            IF(.NOT.EXIST)THEN
                 PRINT *,' !!!!!! MAPFM5 WARNING : Hydra file '//
     -                FAUX(1:NCAUX)//'hyd not found; map not read.'
                 CALL INPSWI('RESTORE')
                 RETURN
            ENDIF
            CALL DSNINQ(FAUX(1:NCAUX)//'pnt',NCAUX+3,EXIST)
            IF(.NOT.EXIST)THEN
                 PRINT *,' !!!!!! MAPFM5 WARNING : Point file '//
     -                FAUX(1:NCAUX)//'pnt not found; map not read.'
                 CALL INPSWI('RESTORE')
                 RETURN
            ENDIF
            IF(DELBKG)THEN
                 CALL DSNINQ(FAUX(1:NCAUX)//'shd',NCAUX+3,EXIST)
                 IF(.NOT.EXIST)THEN
                      PRINT *,' !!!!!! MAPFM5 WARNING : Solid file '//
     -                     FAUX(1:NCAUX)//'shd not found; map not read.'
                      CALL INPSWI('RESTORE')
                      RETURN
                 ENDIF
            ENDIF
**  If we didn't get a mesh file, try to guess the name.
       ELSEIF(.NOT.MAPFLG(1))THEN
*   Locate the directory name.
            IEND=0
            DO 1070 I=NCMAP,1,-1
            IF(FMAP(I:I).NE.' '.AND.IEND.EQ.0)IEND=I
            IF(FMAP(I:I).EQ.'/')THEN
                 FAUX=FMAP(1:I)
                 NCAUX=I
                 GOTO 1080
            ENDIF
1070        CONTINUE
            IF(IEND.EQ.0)THEN
                 PRINT *,' !!!!!! MAPFM5 WARNING : Field file name'//
     -                ' empty ; not read.'
                 CALL INPSWI('RESTORE')
                 RETURN
            ENDIF
            FAUX='./'
            NCAUX=2
1080        CONTINUE
*   Test for various files.
            CALL DSNINQ(FAUX(1:NCAUX)//'fileset2.hyd',NCAUX+12,EXIST)
            IF(EXIST)THEN
                 CALL DSNINQ(FAUX(1:NCAUX)//'fileset2.pnt',NCAUX+12,
     -                EXIST)
                 IF(DELBKG)THEN
                      CALL DSNINQ(FAUX(1:NCAUX)//'fileset2.shd',
     -                     NCAUX+12,EXIST2)
                 ELSE
                      EXIST2=.TRUE.
                 ENDIF
                 IF(EXIST.AND.EXIST2)THEN
                      FAUX=FAUX(:NCAUX)//'fileset2.'
                      NCAUX=NCAUX+9
                      PRINT *,' ------ MAPFM5 MESSAGE : Taking the'//
     -                     ' "fileset2" mesh.'
                      GOTO 1090
                 ENDIF
            ENDIF
            CALL DSNINQ(FAUX(1:NCAUX)//'fileset1.hyd',NCAUX+12,EXIST)
            IF(EXIST)THEN
                 CALL DSNINQ(FAUX(1:NCAUX)//'fileset1.pnt',NCAUX+12,
     -                EXIST)
                 IF(DELBKG)THEN
                      CALL DSNINQ(FAUX(1:NCAUX)//'fileset1.shd',
     -                     NCAUX+12,EXIST2)
                 ELSE
                      EXIST2=.TRUE.
                 ENDIF
                 IF(EXIST.AND.EXIST2)THEN
                      FAUX=FAUX(:NCAUX)//'fileset1.'
                      NCAUX=NCAUX+9
                      PRINT *,' ------ MAPFM5 MESSAGE : Taking the'//
     -                     ' "fileset1" mesh.'
                      GOTO 1090
                 ENDIF
            ENDIF
            CALL DSNINQ(FAUX(1:NCAUX)//'current.hyd',NCAUX+11,EXIST)
            IF(EXIST)THEN
                 CALL DSNINQ(FAUX(1:NCAUX)//'current.pnt',NCAUX+11,
     -                EXIST)
                 IF(DELBKG)THEN
                      CALL DSNINQ(FAUX(1:NCAUX)//'current.shd',
     -                     NCAUX+11,EXIST2)
                 ELSE
                      EXIST2=.TRUE.
                 ENDIF
                 IF(EXIST.AND.EXIST2)THEN
                      FAUX=FAUX(:NCAUX)//'current.'
                      NCAUX=NCAUX+8
                      PRINT *,' ------ MAPFM5 MESSAGE : Taking the'//
     -                     ' "current" mesh.'
                      GOTO 1090
                 ENDIF
            ENDIF
            CALL DSNINQ(FAUX(1:NCAUX)//'efs3d.hyd',NCAUX+9,EXIST)
            IF(EXIST)THEN
                 CALL DSNINQ(FAUX(1:NCAUX)//'efs3d.pnt',NCAUX+9,
     -                EXIST)
                 IF(DELBKG)THEN
                      CALL DSNINQ(FAUX(1:NCAUX)//'efs3d.shd',
     -                     NCAUX+9,EXIST2)
                 ELSE
                      EXIST2=.TRUE.
                 ENDIF
                 IF(EXIST.AND.EXIST2)THEN
                      FAUX=FAUX(:NCAUX)//'efs3d.'
                      NCAUX=NCAUX+6
                      PRINT *,' ------ MAPFM5 MESSAGE : Taking the'//
     -                     ' "efs3d" mesh.'
                      GOTO 1090
                 ENDIF
            ENDIF
            CALL DSNINQ(FAUX(1:NCAUX)//'previous.hyd',NCAUX+12,EXIST)
            IF(EXIST)THEN
                 CALL DSNINQ(FAUX(1:NCAUX)//'previous.pnt',NCAUX+12,
     -                EXIST)
                 IF(DELBKG)THEN
                      CALL DSNINQ(FAUX(1:NCAUX)//'previous.shd',
     -                     NCAUX+12,EXIST2)
                 ELSE
                      EXIST2=.TRUE.
                 ENDIF
                 IF(EXIST.AND.EXIST2)THEN
                      FAUX=FAUX(:NCAUX)//'previous.'
                      NCAUX=NCAUX+9
                      PRINT *,' ------ MAPFM5 MESSAGE : Taking the'//
     -                     ' "previous" mesh.'
                      GOTO 1090
                 ENDIF
            ENDIF
            CALL DSNINQ(FAUX(1:NCAUX)//'initial.hyd',NCAUX+11,EXIST)
            IF(EXIST)THEN
                 CALL DSNINQ(FAUX(1:NCAUX)//'initial.pnt',NCAUX+11,
     -                EXIST)
                 IF(DELBKG)THEN
                      CALL DSNINQ(FAUX(1:NCAUX)//'initial.shd',
     -                     NCAUX+11,EXIST2)
                 ELSE
                      EXIST2=.TRUE.
                 ENDIF
                 IF(EXIST.AND.EXIST2)THEN
                      FAUX=FAUX(:NCAUX)//'initial.'
                      NCAUX=NCAUX+8
                      PRINT *,' ------ MAPFM5 MESSAGE : Taking the'//
     -                     ' "initial" mesh.'
                      GOTO 1090
                 ENDIF
            ENDIF
            PRINT *,' !!!!!! MAPFM5 WARNING : Hydra, point and'//
     -          ' solid files not found; specify mesh explicitely.'
            CALL INPSWI('RESTORE')
            RETURN
*   Verify that the resulting file name is not too long.
1090        CONTINUE
            IF(NCAUX+3.GT.MXNAME)THEN
                 PRINT *,' !!!!!! MAPFM5 WARNING : Mesh file name'//
     -                ' too long after expansion ; not read.'
                 CALL INPSWI('RESTORE')
                 RETURN
            ENDIF
       ENDIF
*** Skip the mesh decoding if this has already been done.
       IF(IDATA.NE.1.AND.MAPFLG(1))GOTO 1000
*   Close the current file, re-open later.
       CALL INPSWI('RESTORE')
       CLOSE(12,ERR=2030,IOSTAT=IOS)
*** If background suppression has been requested, read .shd file.
       DO 1150 I=1,MXMAP
       DELFLG(I)=.FALSE.
1150   CONTINUE
       IF(DELBKG)THEN
*   Construct the hydra file name.
            FAUX=FAUX(1:NCAUX)//'shd'
            NCAUX=NCAUX+3
*   Open the hydra file.
            CALL DSNOPN(FAUX,NCAUX,12,'READ-FILE',IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFM5 WARNING : Unable to open the'//
     -                ' solid file '//FAUX(1:NCAUX)//'; map not read.'
                 RETURN
            ENDIF
*   Record the opening.
            CALL DSNLOG(FAUX(1:NCAUX),'Solids    ','Sequential',
     -           'Read only ')
*   Switch to reading the file.
            CALL INPSWI('UNIT12')
**  Read the header records, switch to the data file.
            CALL INPGET
            CALL INPNUM(NWORD)
*   Check for empty files.
            IF(NWORD.EQ.0)THEN
                 PRINT *,' !!!!!! MAPFM5 WARNING : The file '//
     -                FAUX(1:NCAUX)//' seems to be empty; map not read.'
                 CALL INPSWI('RESTORE')
                 CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
                 RETURN
            ENDIF
*   Read the number of tetrahedrons.
            CALL INPNUM(NWORD)
            CALL INPCHK(2,1,IFAIL1)
            CALL INPRDI(2,NTOTAL,0)
            IF(IFAIL1.NE.0.OR.NTOTAL.LE.0.OR.NWORD.NE.2)THEN
                 PRINT *,' !!!!!! MAPFM5 WARNING : The file '//
     -                FAUX(1:NCAUX)//' has an unreadable number'//
     -                ' of tetrahedrons; not read.'
                 CALL INPSWI('RESTORE')
                 RETURN
            ELSEIF(NTOTAL.GT.MXMAP)THEN
                 PRINT *,' !!!!!! MAPFM5 WARNING : Number of'//
     -                ' tetrahedrons in '//FAUX(1:NCAUX)//
     -                ' exceeds compilation limit; file not read.'
                 CALL INPSWI('RESTORE')
                 RETURN
            ENDIF
*   Debugging output.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM5 DEBUG   :'',
     -           '' Number of .shd tetrahedrons (incl bkg): '',I5)')
     -           NTOTAL
**  Loop over the tetrahedrons, with progress printing.
            CALL PROFLD(2,'Volumes',REAL(NTOTAL))
            NDELET=0
            DO 1160 I=1,NTOTAL
            IF(I.EQ.MAX(1,NTOTAL/100)*(I/MAX(1,NTOTAL/100)))
     -           CALL PROSTA(2,REAL(I))
*   Read the data line.
            CALL INPGET
            CALL INPNUM(NWORD)
            IF(NWORD.EQ.3)THEN
                 DELFLG(I)=.TRUE.
                 NDELET=NDELET+1
            ENDIF
1160        CONTINUE
*   Switch back to regular input.
            CALL INPSWI('RESTORE')
*   Close the solids file.
            CLOSE(12,ERR=2030,IOSTAT=IOS)
*   Reestablish the root name length.
            NCAUX=NCAUX-3
       ENDIF
*   Construct the hydra file name.
       FAUX=FAUX(1:NCAUX)//'hyd'
       NCAUX=NCAUX+3
*   Open the hydra file.
       CALL DSNOPN(FAUX,NCAUX,12,'READ-FILE',IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! MAPFM5 WARNING : Unable to open the'//
     -           ' hydra file '//FAUX(1:NCAUX)//'; map not read.'
            RETURN
       ENDIF
*   Record the opening.
       CALL DSNLOG(FAUX(1:NCAUX),'Hydra     ','Sequential',
     -      'Read only ')
*   Switch to reading the file.
       CALL INPSWI('UNIT12')
**  Read the header records, switch to the data file.
       CALL INPGET
       CALL INPNUM(NWORD)
*   Check for empty files.
       IF(NWORD.EQ.0)THEN
            PRINT *,' !!!!!! MAPFM5 WARNING : The file '//
     -           FAUX(1:NCAUX)//' seems to be empty; map not read.'
            CALL INPSWI('RESTORE')
            CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
            RETURN
       ENDIF
*   Read the number of tetrahedrons.
       CALL INPNUM(NWORD)
       CALL INPCHK(2,1,IFAIL1)
       CALL INPRDI(2,NTOTAL,0)
       IF(IFAIL1.NE.0.OR.NTOTAL.LE.0.OR.NWORD.NE.2)THEN
            PRINT *,' !!!!!! MAPFM5 WARNING : The file '//
     -           FAUX(1:NCAUX)//' has an unreadable number'//
     -           ' of tetrahedrons; not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ELSEIF(NTOTAL.GT.MXMAP)THEN
            PRINT *,' !!!!!! MAPFM5 WARNING : Number of'//
     -           ' tetrahedrons in '//FAUX(1:NCAUX)//
     -           ' exceeds compilation limit; file not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM5 DEBUG   : Number'',
     -      '' of .hyd tetrahedrons (incl bkg): '',I5)') NTOTAL
**  Loop over the tetrahedrons, with progress printing.
       CALL PROFLD(2,'Hydra',REAL(NTOTAL))
       NTETRA=0
       DO 1030 I=1,NTOTAL
       IF(I.EQ.MAX(1,NTOTAL/100)*(I/MAX(1,NTOTAL/100)))
     -      CALL PROSTA(2,REAL(I))
*   Skip tetrahedron or increment counter.
       IF(DELBKG.AND.DELFLG(I))THEN
            READ(12,'(/////)',ERR=2015,END=2005,IOSTAT=IOS)
            GOTO 1030
       ELSE
            NTETRA=NTETRA+1
       ENDIF
*   Skip the blank header.
       CALL INPGET
*   Read the data line.
       CALL INPGET
       CALL INPNUM(NWORD)
       IF(NWORD.NE.6)THEN
            PRINT *,' !!!!!! MAPFM5 WARNING : The format of '//
     -           FAUX(1:NCAUX)//' is not known; map not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Find the pointers to the .pnt file.
       CALL INPCHK(2,1,IFAIL1)
       CALL INPCHK(3,1,IFAIL2)
       CALL INPCHK(4,1,IFAIL3)
       CALL INPCHK(5,1,IFAIL4)
       CALL INPCHK(6,1,IFAIL5)
       CALL INPRDI(2,ITETRA,0)
       CALL INPRDI(3,IT1,0)
       CALL INPRDI(4,IT2,0)
       CALL INPRDI(5,IT3,0)
       CALL INPRDI(6,IT4,0)
       IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.
     -      IFAIL3.NE.0.OR.IFAIL4.NE.0.OR.IFAIL5.NE.0.OR.
     -      IT1.LE.0.OR.IT2.LE.0.OR.IT3.LE.0.OR.IT4.LE.0.OR.
     -      ITETRA.LE.0.OR.ITETRA.GT.NTOTAL)THEN
            PRINT *,' !!!!!! MAPFM5 WARNING : Reference to points'//
     -           ' unreadable in '//FAUX(1:NCAUX)//'; map not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Store the reference pointers temporarily in Ex.
       EXMAP(NTETRA,1)=IT1
       EXMAP(NTETRA,2)=IT2
       EXMAP(NTETRA,3)=IT3
       EXMAP(NTETRA,4)=IT4
*   Skip the 4 lines of additional information.
       READ(12,'(///)',ERR=2015,END=2005,IOSTAT=IOS)
1030   CONTINUE
*   Make sure we're at the end.
       READ(12,'(A9)',ERR=2015,END=2005,IOSTAT=IOS) STRING(1:9)
       IF(STRING(1:9).NE.'end_hydra')PRINT *,' !!!!!! MAPFM5 WARNING'//
     -      ' : Didn''t find the hydra EOF marker ; map probably'//
     -      ' incomplete.'
*   Switch back to regular input.
       CALL INPSWI('RESTORE')
*   Close the hydra file.
       CLOSE(12,ERR=2030,IOSTAT=IOS)
**  Construct the name of the .pnt file.
       FAUX(NCAUX-2:NCAUX)='pnt'
*   Open the points file.
       CALL DSNOPN(FAUX,NCAUX,12,'READ-FILE',IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! MAPFM5 WARNING : Unable to open the'//
     -           ' points file '//FAUX(1:NCAUX)//'; map not read.'
            RETURN
       ENDIF
*   Record the opening.
       CALL DSNLOG(FAUX(1:NCAUX),'Points    ','Sequential',
     -      'Read only ')
**  Read the header records, switch to the data file.
       CALL INPSWI('UNIT12')
*   Read the number of points.
       CALL INPGET
       CALL INPNUM(NWORD)
       CALL INPCHK(2,1,IFAIL1)
       CALL INPRDI(2,NPOINT,0)
       IF(IFAIL1.NE.0.OR.NPOINT.LE.0.OR.NWORD.NE.2)THEN
            PRINT *,' !!!!!! MAPFM5 WARNING : The file '//
     -           FAUX(1:NCAUX)//' has an unreadable number'//
     -           ' of points; not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM5 DEBUG   : Number'',
     -      '' of points: '',I5)') NPOINT
**  Loop over the tetrahedrons, with progress printing.
       CALL PROFLD(2,'Points',REAL(NPOINT))
       FIRST=.TRUE.
       DO 1040 I=1,NPOINT
       IF(I.EQ.MAX(1,NPOINT/100)*(I/MAX(1,NPOINT/100)))
     -      CALL PROSTA(2,REAL(I))
*   Read the data line.
       CALL INPGET
       CALL INPNUM(NWORD)
       IF(NWORD.NE.5)THEN
            PRINT *,' !!!!!! MAPFM5 WARNING : The format of '//
     -           FAUX(1:NCAUX)//' is not known; map not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Read the point coordinates and the reference to the .hyd file.
       CALL INPCHK(2,1,IFAIL2)
       CALL INPCHK(3,2,IFAIL3)
       CALL INPCHK(4,2,IFAIL4)
       CALL INPCHK(5,2,IFAIL5)
       CALL INPRDI(2,IP,0)
       CALL INPRDR(3,XP,0.0)
       CALL INPRDR(4,YP,0.0)
       CALL INPRDR(5,ZP,0.0)
       IF(IFAIL2.NE.0.OR.IFAIL3.NE.0.OR.IFAIL4.NE.0.OR.IFAIL5.NE.0.OR.
     -      IP.LE.0)THEN
            PRINT *,' !!!!!! MAPFM5 WARNING : Reference to hydra'//
     -           ' unreadable in '//FAUX(1:NCAUX)//'; map not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Convert from m to cm.
       XP=XP*100
       YP=YP*100
       ZP=ZP*100
*   Store the tetrahedron parameters that refer to this point.
       NUSE=0
       DO 1100 K=1,NTETRA
       DO 1110 J=1,4
       IF(NINT(EXMAP(K,J)).EQ.IP)THEN
            NUSE=NUSE+1
            XMAP(K,J)=XP
            YMAP(K,J)=YP
            ZMAP(K,J)=ZP
            EXMAP(K,J)=-1
       ENDIF
1110   CONTINUE
1100   CONTINUE
*   If this point was not used, skip the rest.
       IF(NUSE.LE.0)GOTO 1040
*   Update the chamber dimensions.
       IF(FIRST)THEN
            FIRST=.FALSE.
            XMMIN=XP
            XMMAX=XP
            YMMIN=YP
            YMMAX=YP
            ZMMIN=ZP
            ZMMAX=ZP
       ELSE
            XMMIN=MIN(XMMIN,XP)
            XMMAX=MAX(XMMAX,XP)
            YMMIN=MIN(YMMIN,YP)
            YMMAX=MAX(YMMAX,YP)
            ZMMIN=MIN(ZMMIN,ZP)
            ZMMAX=MAX(ZMMAX,ZP)
       ENDIF
*   Update angular ranges.
       IF(YP.NE.0.OR.ZP.NE.0)THEN
            IF(SETAX)THEN
                 XAMIN=MIN(XAMIN,ATAN2(ZP,YP))
                 XAMAX=MAX(XAMAX,ATAN2(ZP,YP))
            ELSE
                 XAMIN=ATAN2(ZP,YP)
                 XAMAX=ATAN2(ZP,YP)
                 SETAX=.TRUE.
            ENDIF
       ENDIF
       IF(ZP.NE.0.OR.XP.NE.0)THEN
            IF(SETAY)THEN
                 YAMIN=MIN(YAMIN,ATAN2(XP,ZP))
                 YAMAX=MAX(YAMAX,ATAN2(XP,ZP))
            ELSE
                 YAMIN=ATAN2(XP,ZP)
                 YAMAX=ATAN2(XP,ZP)
                 SETAY=.TRUE.
            ENDIF
       ENDIF
       IF(XP.NE.0.OR.YP.NE.0)THEN
            IF(SETAZ)THEN
                 ZAMIN=MIN(ZAMIN,ATAN2(YP,XP))
                 ZAMAX=MAX(ZAMAX,ATAN2(YP,XP))
            ELSE
                 ZAMIN=ATAN2(YP,XP)
                 ZAMAX=ATAN2(YP,XP)
                 SETAZ=.TRUE.
            ENDIF
       ENDIF
*   Next point.
1040   CONTINUE
*   Make sure we're at the end.
       READ(12,'(A11)',ERR=2015,END=2005,IOSTAT=IOS) STRING(1:11)
       IF(STRING(1:11).NE.'end_points')PRINT *,' !!!!!! MAPFM5'//
     -      ' WARNING : Didn''t find the points EOF marker ; map'//
     -      ' probably incomplete.'
**  Switch back to regular input.
       CALL INPSWI('RESTORE')
*   End of reading, make sure that all hydra references are solved.
       DO 1120 I=1,NTETRA
       DO 1130 J=1,4
       IF(EXMAP(I,J).GE.0)THEN
            PRINT *,' !!!!!! MAPFM5 WARNING : Unresolved references'//
     -           ' in hydra ; map rejected.'
            RETURN
       ENDIF
1130   CONTINUE
1120   CONTINUE
*   Store the number of tetrahedrons.
       NMAP=NTETRA
**  Set the flag that the mesh is now defined.
       MAPFLG(1)=.TRUE.
**  Print number of deletec tetrahedrons.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM5 DEBUG   :'',
     -      '' Tetrahedrons: '',I5,'' Background: '',I5)')
     -      NTETRA,NDELET
       IF(NDELET.NE.0)PRINT *,' ------ MAPFM5 MESSAGE : Found ',NDELET,
     -      ' background tetrahedrons.'
**  In case this was an explicit mesh, return with success status.
       IF(IDATA.EQ.1)THEN
            IFAIL=0
            RETURN
*   Otherwise, close the points file and re-open mesh file.
       ELSE
            CLOSE(12,ERR=2030,IOSTAT=IOS)
            CALL DSNOPN(FMAP,NCMAP,12,'READ-FILE',IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFM5 WARNING : Re-opening the'//
     -                ' field map failed ; map not read.'
                 RETURN
            ENDIF
*   Record the opening.
            CALL DSNLOG(FMAP(1:NCMAP),'Field map ','Sequential',
     -           'Re-read   ')
*   Read the header records, switch to the data file.
            CALL INPSWI('UNIT12')
            CALL INPGET
            CALL INPNUM(NWORD)
*   Check for empty files.
            IF(NWORD.EQ.0)THEN
                 PRINT *,' !!!!!! MAPFM5 WARNING : The file ',
     -                FMAP(1:NCMAP),' seems to be empty; not read.'
                 CALL INPSWI('RESTORE')
                 CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
                 RETURN
            ENDIF
       ENDIF
*** Read the field map.
1000   CONTINUE
*   See whether the data is scalar or vector.
       IF(INPCMP(1,'SCALAR').NE.0)THEN
            SCALAR=.TRUE.
       ELSEIF(INPCMP(1,'VECTOR').NE.0)THEN
            SCALAR=.FALSE.
       ELSEIF(INPCMP(1,'HYDRAS')+INPCMP(1,'POINTS').NE.0)THEN
            PRINT *,' !!!!!! MAPFM5 WARNING : The file ',
     -           FMAP(1:NCMAP),' contains a mesh, the mesh is'//
     -           ' already defined ; file not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ELSE
            PRINT *,' !!!!!! MAPFM5 WARNING : The file ',
     -           FMAP(1:NCMAP),' contains neither scalar nor'//
     -           ' vectorial data; not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Initial contents flags.
       READ=.FALSE.
       NEWEPS=.FALSE.
*** Determine the contents of the file, first for scalar files.
       IF(SCALAR)THEN
*   They contain 1 data word per line.
            IMAX=1
*   Potentials.
            IF(INPCMP(3,'Phi')+INPCMP(3,'smh(Phi)')+
     -           INPCMP(3,'VOLTAGE').NE.0)THEN
                 ICONT(1)=5
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM5 DEBUG   :'',
     -                '' File contains a potential.'')')
                 IF(MAPFLG(5))PRINT *,' ------ MAPFM5 MESSAGE :'//
     -                ' Overwriting current potential map.'
                 MAPFLG(5)=.FALSE.
                 READ=.TRUE.
*   Dielectric constants.
            ELSEIF(INPCMP(3,'epsilon').NE.0)THEN
                 ICONT(1)=9
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM5 DEBUG   :'',
     -                '' File contains an epsilon map.'')')
                 IF(MAPFLG(9))PRINT *,' ------ MAPFM5 MESSAGE :'//
     -                ' Overwriting current material map.'
                 MAPFLG(9)=.FALSE.
                 NEWEPS=.TRUE.
                 READ=.TRUE.
                 MATSRC='EPSILON'
*   Conductivity.
            ELSEIF(INPCMP(3,'sigma').NE.0)THEN
                 ICONT(1)=9
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM5 DEBUG   :'',
     -                '' File contains a conductivity map.'')')
                 IF(MAPFLG(9))PRINT *,' ------ MAPFM5 MESSAGE :'//
     -                ' Overwriting current material map.'
                 MAPFLG(9)=.FALSE.
                 NEWEPS=.TRUE.
                 READ=.TRUE.
                 MATSRC='SIGMA'
*   All the rest is not known.
            ELSE
                 CALL INPSTR(3,3,STRING,NC)
                 PRINT *,' !!!!!! MAPFM5 WARNING : The file ',
     -                FMAP(1:NCMAP),' contains the unknown "'//
     -                STRING(1:NC)//'" field; ignored.'
                 ICONT(1)=0
                 READ=.TRUE.
            ENDIF
            ICONT(2)=0
            ICONT(3)=0
**  Next for vector files.
       ELSE
*   Which have 3 words per line.
            IMAX=3
*   E field, either main field or weighting field.
            IF(INPCMP(3,'<Ex,Ey,Ez>')+
     -           INPCMP(3,'<smh(Ex),smh(Ey),smh(Ez)>').NE.0)THEN
                 ICONT(1)=2
                 ICONT(2)=3
                 ICONT(3)=4
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM5 DEBUG   :'',
     -                '' File contains an E field.'')')
                 IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                      IF(MAPFLG(2).OR.MAPFLG(3).OR.MAPFLG(4))
     -                     PRINT *,' ------ MAPFM5 MESSAGE :'//
     -                     ' Overwriting current E field map.'
                      MAPFLG(2)=.FALSE.
                      MAPFLG(3)=.FALSE.
                      MAPFLG(4)=.FALSE.
                 ELSEIF(IDATA.EQ.10)THEN
                      IF(MAPFLG(10+4*IWMAP-3).OR.
     -                     MAPFLG(11+4*IWMAP-3).OR.
     -                     MAPFLG(12+4*IWMAP-3))
     -                     PRINT *,' ------ MAPFM5 MESSAGE :'//
     -                     ' Overwriting current weighting field map.'
                      MAPFLG(10+4*IWMAP-3)=.FALSE.
                      MAPFLG(11+4*IWMAP-3)=.FALSE.
                      MAPFLG(12+4*IWMAP-3)=.FALSE.
                 ENDIF
                 READ=.TRUE.
*   B field.
            ELSEIF(INPCMP(3,'<Bx,By,Bz>')+
     -           INPCMP(3,'<smh(Bx),smh(By),smh(Bz)>').NE.0)THEN
                 ICONT(1)=6
                 ICONT(2)=7
                 ICONT(3)=8
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM5 DEBUG   :'',
     -                '' File contains a B field.'')')
                 IF(MAPFLG(6).OR.MAPFLG(7).OR.MAPFLG(8))
     -                PRINT *,' ------ MAPFM5 MESSAGE :'//
     -                ' Overwriting current E field map.'
                 MAPFLG(6)=.FALSE.
                 MAPFLG(7)=.FALSE.
                 MAPFLG(8)=.FALSE.
                 READ=.TRUE.
*   D field.
            ELSEIF(INPCMP(3,'<Dx,Dy,Dz>')+
     -           INPCMP(3,'<smh(Dx),smh(Dy),smh(Dz)>').NE.0)THEN
                 ICONT(1)=-9
                 ICONT(2)=-9
                 ICONT(3)=-9
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM5 DEBUG   :'',
     -                '' File contains a D field.'')')
                 IF(MAPFLG(9))PRINT *,' ------ MAPFM5 MESSAGE :'//
     -                ' Overwriting current material map.'
                 MAPFLG(9)=.FALSE.
                 READ=.TRUE.
                 MATSRC='EPSILON'
*   All the rest is not known.
            ELSE
                 CALL INPSTR(3,3,STRING,NC)
                 PRINT *,' !!!!!! MAPFM5 WARNING : The file ',
     -                FMAP(1:NCMAP),' contains the unknown "'//
     -                STRING(1:NC)//' field; ignored.'
                 ICONT(1)=0
                 ICONT(2)=0
                 ICONT(3)=0
                 READ=.TRUE.
            ENDIF
       ENDIF
**  Ensure that the data type matches the declared type.
       DO 40 I=1,IMAX
       IF(((ICONT(I).EQ.2.OR.ICONT(I).EQ.3.OR.ICONT(I).EQ.4).AND.
     -      (IDATA.NE.0.AND.IDATA.NE.2.AND.IDATA.NE.10)).OR.
     -      (ICONT(I).EQ.5.AND.(IDATA.NE.0.AND.IDATA.NE.5)).OR.
     -      ((ICONT(I).EQ.6.OR.ICONT(I).EQ.7.OR.ICONT(I).EQ.8).AND.
     -      (IDATA.NE.0.AND.IDATA.NE.6)).OR.
     -      ((ICONT(I).EQ.9.OR.ICONT(I).EQ.-9).AND.
     -      (IDATA.NE.0.AND.IDATA.NE.9)))THEN
            PRINT *,' !!!!!! MAPFM5 WARNING : Field ',I,' of file ',
     -           FMAP(1:NCMAP),' does not contain the declared',
     -           ' kind of data; skipped.'
            ICONT(I)=0
       ENDIF
40     CONTINUE
*** Read the number of points and number of tetrahedrons.
       CALL INPGET
       CALL INPNUM(NWORD)
*   Verify the tetrahedron and points count.
       CALL INPCHK(2,1,IFAIL1)
C       CALL INPCHK(4,1,IFAIL2)
       CALL INPRDI(2,MTETRA,0)
C       CALL INPRDI(4,MPOINT,0)
C       IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.NWORD.NE.4)THEN
       IF(IFAIL1.NE.0.OR.NWORD.LT.2)THEN
            PRINT *,' !!!!!! MAPFM5 WARNING : The file ',
     -           FMAP(1:NCMAP),' has an unreadable number'//
     -           ' of tetrahedrons or points; not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM5 DEBUG   : Number'',
     -      '' of field tetrahedrons (incl bkg): '',I5)') NTOTAL
*   Progress printing.
       CALL PROFLD(2,'Tetrahedrons',REAL(NTOTAL))
*** Switch back to regular input.
       CALL INPSWI('RESTORE')
*   See whether any item is left.
       IF(.NOT.READ)THEN
            PRINT *,' !!!!!! MAPFM5 WARNING : The file ',
     -           FMAP(1:NCMAP),' contains no useable'//
     -           ' information; file not read.'
            RETURN
       ENDIF
*** Loop over the tetrahedrons.
       NTETRA=0
       DO 10 I=1,NTOTAL
       IF(I.EQ.MAX(1,NTOTAL/100)*(I/MAX(1,NTOTAL/100)))
     -      CALL PROSTA(2,REAL(I))
*** Read the line with the word "Tet" or "Elem".
20     CONTINUE
       READ(12,'(A80)',END=2000,ERR=2010,IOSTAT=IOS) STRING
*   Tetrahedron number in Earlier formats
       IF(STRING(1:3).EQ.'Tet')THEN
            READ(STRING,'(3X,BN,I10)',ERR=2010,IOSTAT=IOS) ITETRA
       ELSEIF(STRING(1:4).EQ.'Elem')THEN
            READ(STRING,'(4X,BN,I10)',ERR=2010,IOSTAT=IOS) ITETRA
       ELSE
            GOTO 20
       ENDIF
*   Ensure this number is in range.
       IF(ITETRA.LE.0.OR.ITETRA.GT.NTOTAL)THEN
            PRINT *,' !!!!!! MAPFM5 WARNING : Tetrahedron number ',
     -           ITETRA,' out of range in ',FMAP(1:NCMAP)
            READ(12,'(/////////)',ERR=2010,END=2000,IOSTAT=IOS)
            GOTO 10
*   Skip tetrahedron or increment counter.
       ELSEIF(DELBKG.AND.DELFLG(ITETRA))THEN
            READ(12,'(/////////)',ERR=2010,END=2000,IOSTAT=IOS)
            GOTO 10
       ELSE
            NTETRA=NTETRA+1
       ENDIF
*** Read scalar field values over the tetrahedron.
       IF(SCALAR)THEN
**  Can be either a potential, first read.
            IF(ICONT(1).EQ.5)THEN
                 READ(12,*,END=2000,ERR=2010,IOSTAT=IOS)
     -                (VMAP(NTETRA,LOOKUP(K)),K=1,10)
*   Then keep track of potential range.
                 IF(I.EQ.1)THEN
                      VMMIN=VMAP(NTETRA,1)
                      VMMAX=VMAP(NTETRA,1)
                 ENDIF
                 VMMIN=MIN(VMMIN,
     -                VMAP(NTETRA,1),VMAP(NTETRA,2),VMAP(NTETRA,3),
     -                VMAP(NTETRA,4),VMAP(NTETRA,5),VMAP(NTETRA,6),
     -                VMAP(NTETRA,7),VMAP(NTETRA,8),VMAP(NTETRA,9),
     -                VMAP(NTETRA,10))
                 VMMAX=MAX(VMMAX,
     -                VMAP(NTETRA,1),VMAP(NTETRA,2),VMAP(NTETRA,3),
     -                VMAP(NTETRA,4),VMAP(NTETRA,5),VMAP(NTETRA,6),
     -                VMAP(NTETRA,7),VMAP(NTETRA,8),VMAP(NTETRA,9),
     -                VMAP(NTETRA,10))
**  Or a dielectricum, first read.
            ELSEIF(ICONT(1).EQ.9)THEN
                 READ(12,*,END=2000,ERR=2010,IOSTAT=IOS)
     -                (TEMP(K),K=1,10)
*   Average the epsilons/conductivity.
                 SUM=0
                 DO 30 J=1,10
                 SUM=SUM+TEMP(J)
30               CONTINUE
                 SUM=SUM/(1000*EPS0)
*   Identify the material.
                 IF(NEPS.GE.1)THEN
                      INEAR=1
                      DNEAR=ABS(SUM-EPSMAT(1))
                 ELSE
                      INEAR=-1
                      DNEAR=0
                 ENDIF
                 DO 80 J=1,NEPS
                 IF(ABS(SUM-EPSMAT(J)).LT.DNEAR)THEN
                      DNEAR=ABS(SUM-EPSMAT(J))
                      INEAR=J
                 ENDIF
80               CONTINUE
                 IEPS=-1
                 IF(INEAR.GT.0)THEN
                      IF(DNEAR.LT.1E-4*(ABS(SUM)+ABS(EPSMAT(INEAR))))
     -                     IEPS=INEAR
                 ENDIF
                 IF(IEPS.LT.0.AND.NEPS.LT.MXEPS)THEN
                      NEPS=NEPS+1
                      IEPS=NEPS
                      EPSMAT(IEPS)=SUM
                      IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM5'',
     -                     '' DEBUG   : Adding dielectricum with'',
     -                     '' eps='',E10.3,''.'')') EPSMAT(IEPS)
                 ELSEIF(IEPS.LT.0.AND.INEAR.GT.0)THEN
                      IF(JFAIL.EQ.1)THEN
                           PRINT *,' !!!!!! MAPFM5 WARNING :'//
     -                          ' Dielectrica table is full; storing'//
     -                          ' new material at nearest epsilon.'
                           IEPS=INEAR
                      ELSEIF(JFAIL.EQ.2)THEN
                           PRINT *,' !!!!!! MAPFM5 WARNING :'//
     -                          ' Dielectrica table is full; file ',
     -                          FMAP(1:NCMAP),' not read.'
                           RETURN
                      ELSE
                           PRINT *,' !!!!!! MAPFM5 WARNING :'//
     -                          ' Dielectrica table is full; exiting.'
                           CALL QUIT
                           RETURN
                      ENDIF
                 ELSE
                      PRINT *,' ###### MAPFM5 ERROR   : Error'//
     -                     ' processing dielectrica; file ',
     -                     FMAP(1:NCMAP),' not read.'
                      RETURN
                 ENDIF
                 MATMAP(NTETRA)=IEPS
            ENDIF
*** Read vectorial field values over the tetrahedron.
       ELSE
*   Take care of knowing |D| either from Ex or by summing.
            IF(MAPFLG(10))DCOMP=EXMAP(NTETRA,1)
*   E or EW.
            IF(ICONT(1).EQ.2.AND.ICONT(2).EQ.3.AND.ICONT(3).EQ.4)THEN
                 IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                      READ(12,*,END=2000,ERR=2010,IOSTAT=IOS)
     -                     (EXMAP(NTETRA,LOOKUP(K)),
     -                      EYMAP(NTETRA,LOOKUP(K)),
     -                      EZMAP(NTETRA,LOOKUP(K)),K=1,10)
                 ELSEIF(IDATA.EQ.10)THEN
                      READ(12,*,END=2000,ERR=2010,IOSTAT=IOS)
     -                     (EWXMAP(NTETRA,LOOKUP(K),IWMAP),
     -                      EWYMAP(NTETRA,LOOKUP(K),IWMAP),
     -                      EWZMAP(NTETRA,LOOKUP(K),IWMAP),K=1,10)
                 ENDIF
*   B.
            ELSEIF(ICONT(1).EQ.6.AND.ICONT(2).EQ.7.AND.
     -           ICONT(3).EQ.8)THEN
                 READ(12,*,END=2000,ERR=2010,IOSTAT=IOS)
     -                (BXMAP(NTETRA,LOOKUP(K)),
     -                 BYMAP(NTETRA,LOOKUP(K)),
     -                 BZMAP(NTETRA,LOOKUP(K)),K=1,10)
*   D field.
            ELSEIF(ICONT(1).EQ.-9.AND.ICONT(2).EQ.-9.AND.
     -           ICONT(3).EQ.-9)THEN
                 DX=0
                 DY=0
                 DZ=0
                 DO 50 J=1,10
                 READ(12,*,END=2000,ERR=2010,IOSTAT=IOS)
     -                (TEMP(K),K=1,3)
                 IF(LOOKUP(J).LE.4)THEN
                      DX=DX+TEMP(1)
                      DY=DY+TEMP(2)
                      DZ=DZ+TEMP(3)
                 ENDIF
50               CONTINUE
                 DCOMP=(DX**2+DY**2+DZ**2)/160000
            ENDIF
**  Dielectricum identification via D/E comparison.
            IF((MAPFLG(2).AND.MAPFLG(3).AND.MAPFLG(4).AND.
     -           (.NOT.MAPFLG(9)).AND.ICONT(1).EQ.-9.AND.
     -           ICONT(2).EQ.-9.AND.ICONT(3).EQ.-9).OR.
     -           (MAPFLG(10).AND.(.NOT.MAPFLG(9)).AND.
     -           ICONT(1).EQ.2.AND.ICONT(2).EQ.3.AND.
     -           ICONT(3).EQ.4))THEN
                 IEPS=-1
                 ECOMP=((EXMAP(NTETRA,1)+EXMAP(NTETRA,2)+
     -                EXMAP(NTETRA,3)+EXMAP(NTETRA,4))**2+
     -                (EYMAP(NTETRA,1)+EYMAP(NTETRA,2)+
     -                EYMAP(NTETRA,3)+EYMAP(NTETRA,4))**2+
     -                (EZMAP(NTETRA,1)+EZMAP(NTETRA,2)+
     -                EZMAP(NTETRA,3)+EZMAP(NTETRA,4))**2)/16
                 IF(ICONT(1).EQ.2.AND.ICONT(2).EQ.3.AND.
     -                ICONT(3).EQ.4)ECOMP=ECOMP/10000
                 DO 60 J=1,NEPS
                 IF(ABS(ECOMP*(100*EPS0*EPSMAT(J))**2-DCOMP).LE.1E-4*
     -                (ABS(ECOMP*(100*EPS0*EPSMAT(J))**2)+
     -                ABS(DCOMP)))IEPS=J
60               CONTINUE
                 IF(ECOMP.LE.0.AND.DCOMP.GT.0)THEN
                      PRINT *,' !!!!!! MAPFM5 WARNING : Found'//
     -                     ' a dielectric constant of 0; skipped.'
                 ELSEIF(IEPS.LT.0.AND.NEPS.GE.MXEPS)THEN
                      IF(NEPS.GE.1)THEN
                           DNEAR=ABS(EPSMAT(1)-SQRT(DCOMP/ECOMP)/
     -                          (100*EPS0))
                           IEPS=1
                      ELSE
                           DNEAR=0
                           IEPS=-1
                      ENDIF
                      DO 61 J=2,NEPS
                      IF(ABS(EPSMAT(J)-SQRT(DCOMP/ECOMP)/(100*EPS0)).LT.
     -                     DNEAR)THEN
                           IEPS=J
                      ENDIF
61                    CONTINUE
                      IF(NEPS.LT.1.OR.IEPS.LT.1)THEN
                           PRINT *,' ###### MAPFM5 ERROR   : Error'//
     -                          ' processing dielectrica; file ',
     -                          FMAP(1:NCMAP),' not read.'
                           RETURN
                      ELSEIF(JFAIL.EQ.1)THEN
                           PRINT *,' !!!!!! MAPFM5 WARNING :'//
     -                          ' Dielectrica table is full; storing'//
     -                          ' new material at nearest epsilon.'
                      ELSEIF(JFAIL.EQ.2)THEN
                           PRINT *,' !!!!!! MAPFM5 WARNING :'//
     -                          ' Dielectrica table is full; file ',
     -                          FMAP(1:NCMAP),' not read.'
                           RETURN
                      ELSE
                           PRINT *,' !!!!!! MAPFM5 WARNING :'//
     -                          ' Dielectrica table is full; exiting.'
                           CALL QUIT
                           RETURN
                      ENDIF
                 ELSEIF(IEPS.LT.0)THEN
                      NEPS=NEPS+1
                      IEPS=NEPS
                      IF(ECOMP.LE.0)THEN
                           PRINT *,' ------ MAPFM5 MESSAGE : Unable'//
     -                          ' to determine epsilon in an E=0'//
     -                          ' tetrahedron; epsilon set to 0.'
                           EPSMAT(IEPS)=0
                      ELSE
                           EPSMAT(IEPS)=SQRT(DCOMP/ECOMP)/(100*EPS0)
                      ENDIF
                      IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM5'',
     -                     '' DEBUG   : Adding dielectricum with'',
     -                     '' eps='',E10.3,''.'')') EPSMAT(IEPS)
                 ENDIF
                 MATMAP(NTETRA)=IEPS
                 NEWEPS=.TRUE.
*  Otherwise store the field.
            ELSEIF(ICONT(1).EQ.-9.AND.ICONT(2).EQ.-9.AND.
     -           ICONT(3).EQ.-9.AND.(.NOT.MAPFLG(2)))THEN
                 EXMAP(NTETRA,1)=DCOMP
            ENDIF
       ENDIF
10     CONTINUE
*** Be sure something has been read.
2000   CONTINUE
       IF(NTETRA.NE.NMAP)THEN
            PRINT *,' !!!!!! MAPFM5 WARNING : Number of'//
     -           ' tetrahedrons in ',FMAP(1:NCMAP),' does not'//
     -           ' match current mesh; not read.'
            RETURN
       ENDIF
*** Materials have been defined is NEWEPS is set.
       IF(NEWEPS)MAPFLG(9)=.TRUE.
*** Scale electric fields if they have been entered.
       IF(ICONT(1).EQ.2.AND.(IDATA.EQ.0.OR.IDATA.EQ.2))THEN
            DO 200 I=1,NMAP
            DO 210 J=1,10
            EXMAP(I,J)=EXMAP(I,J)/100
            EYMAP(I,J)=EYMAP(I,J)/100
            EZMAP(I,J)=EZMAP(I,J)/100
210         CONTINUE
200         CONTINUE
       ELSEIF(ICONT(1).EQ.2.AND.IDATA.EQ.10)THEN
            DO 220 I=1,NMAP
            DO 230 J=1,10
            EWXMAP(I,J,IWMAP)=EWXMAP(I,J,IWMAP)/100
            EWYMAP(I,J,IWMAP)=EWYMAP(I,J,IWMAP)/100
            EWZMAP(I,J,IWMAP)=EWZMAP(I,J,IWMAP)/100
230         CONTINUE
220         CONTINUE
       ENDIF
*** Flag those elements which have been defined.
       DO 70 I=1,3
       IF(ICONT(I).EQ.2.OR.ICONT(I).EQ.3.OR.ICONT(I).EQ.4)THEN
            IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                 MAPFLG(ICONT(I))=.TRUE.
            ELSEIF(IDATA.EQ.10)THEN
                 MAPFLG(8+ICONT(I)+4*IWMAP-3)=.TRUE.
            ENDIF
       ELSEIF(ICONT(I).GT.0)THEN
            MAPFLG(ICONT(I))=.TRUE.
       ELSEIF(ICONT(I).EQ.-9)THEN
            MAPFLG(10)=.TRUE.
       ENDIF
70     CONTINUE
*** Seems to have worked, set error flag to OK and return.
       IFAIL=0
       MAPTYP=12
       RETURN
*** Handle error conditions.
2005   CONTINUE
       PRINT *,' !!!!!! MAPFM5 WARNING : Premature end of file'//
     -      ' reading a mesh file; map not available.'
       IF(LDEBUG)CALL INPIOS(IOS)
       CLOSE(12,ERR=2030)
       RETURN
2010   CONTINUE
       PRINT *,' !!!!!! MAPFM5 WARNING : Error reading field map'//
     -      ' file ',FMAP(1:NCMAP),'; map not available.'
       IF(LDEBUG)CALL INPIOS(IOS)
       RETURN
2015   CONTINUE
       PRINT *,' !!!!!! MAPFM5 WARNING : Error reading a mesh'//
     -      ' file ; map not available.'
       IF(LDEBUG)CALL INPIOS(IOS)
       CLOSE(12,ERR=2030)
       RETURN
2030   CONTINUE
       PRINT *,' !!!!!! MAPFM5 WARNING : Error closing field map'//
     -      ' file ',FMAP(1:NCMAP),'; map not available.'
       IF(LDEBUG)CALL INPIOS(IOS)
       RETURN
       END
