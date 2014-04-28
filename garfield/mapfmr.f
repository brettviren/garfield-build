CDECK  ID>, MAPFMR.
       SUBROUTINE MAPFMR(FMAP,NCMAP,IFORM,IDATA,IWMAP,DELBKG,
     -      MAPMAX,UNITD,IFAIL)
*-----------------------------------------------------------------------
*   MAPFMR - Reads one interpolation table.
*   (Last changed on 24/ 3/12.)
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
       CHARACTER*(*) FMAP
       INTEGER NCMAP,IFAIL,IOS,I,IFORM,IDATA,IFAIL1,INPCMP,MAPMAX,
     -      NWORD,IWMAP,IORDER
       REAL UNITD
       LOGICAL EXIST,DELBKG,OPEN
       EXTERNAL INPCMP
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MAPFMR ///'
*** Assume the routine will fail.
       IFAIL=1
*** Reset search for volumes.
       CALL MAPINR
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFMR DEBUG   : Field'',
     -      '' map: '',A/26X,''Format:    '',I2/26X,''Contents:  '',
     -      I2)') FMAP(1:NCMAP),IFORM,IDATA
*** Check for an empty file name.
       IF(NCMAP.LT.1)THEN
            PRINT *,' !!!!!! MAPFMR WARNING : Field map file'//
     -           ' is empty; field map not read.'
            RETURN
       ENDIF
*** Check the existence of the field map or mesh file.
       CALL DSNINQ(FMAP,NCMAP,EXIST)
       IF(.NOT.EXIST)THEN
            PRINT *,' !!!!!! MAPFMR WARNING : Field map file ',
     -           FMAP(1:NCMAP),' not found; field map not read.'
            RETURN
       ENDIF
*   Open the field map file.
       CALL DSNOPN(FMAP,NCMAP,12,'READ-FILE',IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! MAPFMR WARNING : Unable to open the'//
     -           ' field map file ',FMAP(1:NCMAP),'; not read.'
            RETURN
       ENDIF
*   Record the opening.
       CALL DSNLOG(FMAP(1:NCMAP),'Field map ','Sequential',
     -      'Read only ')
*   Read the header records, switch to the data file.
       CALL INPSWI('UNIT12')
       CALL INPGET
       CALL INPNUM(NWORD)
*** In case of planar and real data, try Maxwell Parameter Extractor 2D.
       IF(INPCMP(1,'PLANE').NE.0.AND.INPCMP(3,'REAL').NE.0.AND.
     -      INPCMP(4,'SIZE').NE.0)THEN
            IF(IFORM.NE.1.AND.IFORM.NE.0)PRINT *,' !!!!!! MAPFMR'//
     -           ' WARNING : File ',FMAP(1:NCMAP),' seems to be in'//
     -           ' Maxwell Parameter Extractor 2D format,'//
     -           ' contrary to your indications.'
            CALL MAPFM2(FMAP,NCMAP,IDATA,IWMAP,IFAIL1)
            MAPMAX=2
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFMR WARNING : File ',FMAP(1:NCMAP),
     -                ' could not successfully be read as Maxwell 2D.'
                 CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
                 RETURN
            ENDIF
*   Non-planar real data could be Maxwell Parameter Extractor 3D.
       ELSEIF(INPCMP(2,'REAL').NE.0.AND.INPCMP(3,'SIZE').NE.0)THEN
            IF(IFORM.NE.2.AND.IFORM.NE.0)PRINT *,' !!!!!! MAPFMR'//
     -           ' WARNING : File ',FMAP(1:NCMAP),' seems to be in'//
     -           ' Maxwell Parameter Extractor 3D format,'//
     -           ' contrary to your indications.'
            CALL MAPFM3(FMAP,NCMAP,IDATA,IWMAP,IFAIL1)
            MAPMAX=2
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFMR WARNING : File ',FMAP(1:NCMAP),
     -                ' could not successfully be read as Maxwell 3D.'
                 CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
                 RETURN
            ENDIF
*   Try Maxwell 2D SV.
       ELSEIF(IFORM.EQ.6)THEN
            CALL MAPFM4(FMAP,NCMAP,IDATA,IWMAP,IFAIL1)
            MAPMAX=2
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFMR WARNING : File ',FMAP(1:NCMAP),
     -                ' could not be read as Maxwell 2D SV.'
                 CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
                 RETURN
            ENDIF
*   Try Maxwell Version 11.
       ELSEIF(IFORM.EQ.9)THEN
            CALL MAPFM0(FMAP,NCMAP,IDATA,IWMAP,DELBKG,IFAIL1)
            MAPMAX=2
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFMR WARNING : File ',FMAP(1:NCMAP),
     -                ' could not successfully be read as Maxwell 11.'
                 CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
                 RETURN
            ENDIF
*   Try Maxwell Field Simulator 3D.
       ELSEIF(INPCMP(1,'HYDRAS').NE.0.OR.
     -      INPCMP(1,'POINTS').NE.0.OR.
     -      (INPCMP(1,'SCALAR').NE.0.AND.INPCMP(2,'DATA').NE.0.AND.
     -      NWORD.EQ.3).OR.
     -      (INPCMP(1,'VECTOR').NE.0.AND.INPCMP(2,'DATA').NE.0.AND.
     -      NWORD.EQ.3))THEN
            IF(IFORM.NE.4.AND.IFORM.NE.0)PRINT *,' !!!!!! MAPFMR'//
     -           ' WARNING : File ',FMAP(1:NCMAP),' seems to be in'//
     -           ' Maxwell Field Simulator 3D format,'//
     -           ' contrary to your indications.'
            IF(INPCMP(1,'HYDRAS')+INPCMP(1,'POINTS').NE.0)IDATA=1
            CALL MAPFM5(FMAP,NCMAP,IDATA,IWMAP,DELBKG,IFAIL1)
            MAPMAX=2
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFMR WARNING : File ',FMAP(1:NCMAP),
     -                ' could not successfully be read as Maxwell 3D.'
                 CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
                 RETURN
            ENDIF
*   Try COMSOL 3D (2nd order tetrahedra).
       ELSEIF(IFORM.EQ.7)THEN
            IORDER=2
            CALL MAPFM8(FMAP,NCMAP,IDATA,IWMAP,IORDER,UNITD,IFAIL1)
            MAPMAX=IORDER
            IF(.NOT.LSFDER)PRINT *,' ------ MAPFMR MESSAGE :'//
     -           ' Switching COMPUTE-ELECTRIC-FIELD on.'
            LSFDER=.TRUE.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFMR WARNING : File ',FMAP(1:NCMAP),
     -                ' could not read as COMSOL 2nd order 3D.'
                 CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
                 RETURN
            ENDIF
*   Try COMSOL 3D (1st order tetrahedra).
       ELSEIF(IFORM.EQ.15)THEN
            IORDER=1
            CALL MAPFM8(FMAP,NCMAP,IDATA,IWMAP,IORDER,UNITD,IFAIL1)
            MAPMAX=IORDER
            IF(.NOT.LSFDER)PRINT *,' ------ MAPFMR MESSAGE :'//
     -           ' Switching COMPUTE-ELECTRIC-FIELD on.'
            LSFDER=.TRUE.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFMR WARNING : File ',FMAP(1:NCMAP),
     -                ' could not read as COMSOL 2nd order 3D.'
                 CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
                 RETURN
            ENDIF
*   Try COMSOL 2D (2nd order triangles)
       ELSEIF(IFORM.EQ.8)THEN
            IORDER=2
            CALL MAPFM9(FMAP,NCMAP,IDATA,IWMAP,IORDER,UNITD,IFAIL1)
            MAPMAX=IORDER
            IF(.NOT.LSFDER)PRINT *,' ------ MAPFMR MESSAGE :'//
     -           ' Switching COMPUTE-ELECTRIC-FIELD on.'
            LSFDER=.TRUE.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFMR WARNING : File ',FMAP(1:NCMAP),
     -                ' could not be read as COMSOL 2nd order 2D.'
                 CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
                 RETURN
            ENDIF
*   Try COMSOL 2D (first order triangles)
       ELSEIF(IFORM.EQ.14)THEN
            IORDER=1
            CALL MAPFM9(FMAP,NCMAP,IDATA,IWMAP,IORDER,UNITD,IFAIL1)
            MAPMAX=IORDER
            IF(.NOT.LSFDER)PRINT *,' ------ MAPFMR MESSAGE :'//
     -           ' Switching COMPUTE-ELECTRIC-FIELD on.'
            LSFDER=.TRUE.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFMR WARNING : File ',FMAP(1:NCMAP),
     -                ' could not be read as COMSOL 1st order 2D.'
                 CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
                 RETURN
            ENDIF
*   Tosca.
       ELSEIF(IFORM.EQ.5)THEN
            CALL MAPFM6(FMAP,NCMAP,IDATA,IWMAP,IFAIL1)
            MAPMAX=1
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFMR WARNING : File ',FMAP(1:NCMAP),
     -                ' could not successfully be read as Tosca.'
                 CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
                 RETURN
            ENDIF
*   QuickField.
       ELSEIF(IFORM.EQ.10)THEN
            CALL MAPFM1(FMAP,NCMAP,IDATA,IWMAP,IFAIL1)
            MAPMAX=1
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFMR WARNING : File ',FMAP(1:NCMAP),
     -                ' could not successfully be read as QuickField.'
                 CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
                 RETURN
            ENDIF
*   Ansys element solid123.
       ELSEIF(IFORM.EQ.11)THEN
            CALL MAPFMA(FMAP,NCMAP,IDATA,IWMAP,DELBKG,UNITD,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFMR WARNING : File ',FMAP(1:NCMAP),
     -                ' could not successfully be read as Ansys'//
     -                ' with element solid123.'
                 CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
                 RETURN
            ENDIF
            MAPMAX=2
            IF(.NOT.LSFDER)PRINT *,' ------ MAPFMR MESSAGE :'//
     -           ' Switching COMPUTE-ELECTRIC-FIELD on.'
            LSFDER=.TRUE.
*   Ansys element plane121.
       ELSEIF(IFORM.EQ.12)THEN
            CALL MAPFMB(FMAP,NCMAP,IDATA,IWMAP,DELBKG,UNITD,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFMR WARNING : File ',FMAP(1:NCMAP),
     -                ' could not successfully be read as Ansys'//
     -                ' with element plane121.'
                 CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
                 RETURN
            ENDIF
            MAPMAX=2
            IF(.NOT.LSFDER)PRINT *,' ------ MAPFMR MESSAGE :'//
     -           ' Switching COMPUTE-ELECTRIC-FIELD on.'
            LSFDER=.TRUE.
*   Tosca element 118
       ELSEIF(IFORM.EQ.13)THEN
            CALL MAPFMC(FMAP,NCMAP,IDATA,IWMAP,DELBKG,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFMR WARNING : File ',FMAP(1:NCMAP),
     -                ' could not successfully be read as Tosca'//
     -                ' with element 118.'
                 CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
                 RETURN
            ENDIF
            MAPMAX=2
            IF(.NOT.LSFDER)PRINT *,' ------ MAPFMR MESSAGE :'//
     -           ' Switching COMPUTE-ELECTRIC-FIELD on.'
            LSFDER=.TRUE.
*   Other formats are not currently known.
       ELSE
            PRINT *,' !!!!!! MAPFMR WARNING : Data in ',FMAP(1:NCMAP),
     -           ' is in an unknown format; not read.'
            CALL INPSWI('RESTORE')
            CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
            RETURN
       ENDIF
*** We should have read everything now.
       CALL INPSWI('RESTORE-QUIET')
       INQUIRE(UNIT=12,OPENED=OPEN)
       IF(OPEN)CLOSE(12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
*** Debugging output.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ MAPFMR DEBUG   :'',
     -           '' Current set of flags: '',13L1)')
     -           (MAPFLG(I),I=1,13)
            IF(MAPFLG(1))WRITE(LUNOUT,'(''  ++++++ MAPFMR'',
     -           '' DEBUG   : Grid covers: ''/
     -           26X,E15.8,'' < x < '',E15.8/
     -           26X,E15.8,'' < y < '',E15.8/
     -           26X,E15.8,'' < z < '',E15.8)')
     -           XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX
            IF(MAPFLG(5))WRITE(LUNOUT,'(''  ++++++ MAPFMR'',
     -           '' DEBUG   : Potential range: ''/
     -           26X,E15.8,'' < V < '',E15.8)') VMMIN,VMMAX
       ENDIF
*** Seems to have worked, set error flag to OK and return.
       IFAIL=0
       RETURN
*** Handle error conditions.
2030   CONTINUE
       PRINT *,' !!!!!! MAPFMR WARNING : Error closing field map'//
     -      ' file ',FMAP(1:NCMAP),'; map not available.'
       RETURN
       END
