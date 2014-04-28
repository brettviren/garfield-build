CDECK  ID>, MAPFM3.
       SUBROUTINE MAPFM3(FMAP,NCMAP,IDATA,IWMAP,IFAIL)
*-----------------------------------------------------------------------
*   MAPFM3 - Reads a Maxwell 3D table of tetrahedrons.
*   (Last changed on 26/10/07.)
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
       INTEGER NDECL,IMAP,IEPS,ICONT(3),IMAX,IWMAP,
     -      I,J,K,NCMAP,IFAIL,IFAIL1,IOS,NC,INPCMP,
     -      NREAD,IDATA
       REAL TEMP(10),XAUX(4),YAUX(4),ZAUX(4),SUM,ECOMP,DCOMP,
     -      T1,T2,T3,T4
       DOUBLE PRECISION JAC(4,4),DET
       CHARACTER*(*) FMAP
       CHARACTER*80 STRING
       LOGICAL SCALAR,READ,NEWEPS
       EXTERNAL INPCMP
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MAPFM3 ///'
*** Assume that this will fail.
       IFAIL=1
*** First read the line with number of tetrahedrons.
       CALL INPCHK(4,1,IFAIL1)
       CALL INPRDI(4,NDECL,0)
       IF(IFAIL1.NE.0.OR.NDECL.LE.0)THEN
            PRINT *,' !!!!!! MAPFM3 WARNING : The file ',
     -           FMAP(1:NCMAP),' has an unreadable number'//
     -           ' of tetrahedrons; not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM3 DEBUG   : Number'',
     -      '' of tetrahedrons: '',I5)') NDECL
*   Progress printing.
       CALL PROFLD(2,'Tetrahedrons',REAL(NDECL))
*** See whether the data is scalar or vector.
       IF(INPCMP(1,'SCALAR').NE.0)THEN
            SCALAR=.TRUE.
       ELSEIF(INPCMP(1,'VECTOR').NE.0)THEN
            SCALAR=.FALSE.
       ELSE
            PRINT *,' !!!!!! MAPFM3 WARNING : The file ',
     -           FMAP(1:NCMAP),' contains neither scalar nor'//
     -           ' vectorial data; not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*** Next determine the contents of the file, read the next record.
       CALL INPGET
*   Set the expected word count.
       IF(SCALAR)THEN
            IMAX=1
       ELSE
            IMAX=3
       ENDIF
*   Initial contents flags.
       ICONT(1)=0
       ICONT(2)=0
       ICONT(3)=0
       READ=.FALSE.
       NEWEPS=.FALSE.
*   Loop over the words.
       DO 40 I=1,IMAX
*   Ex.
       IF(INPCMP(I,'smh(E(x))').NE.0)THEN
            ICONT(I)=2
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM3 DEBUG   :'',
     -           '' Field '',I1,'': x-component E field.'')') I
            IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                 IF(MAPFLG(2))PRINT *,' ------ MAPFM3 MESSAGE :'//
     -                ' Overwriting current Ex map.'
                 MAPFLG(2)=.FALSE.
            ELSEIF(IDATA.EQ.10)THEN
                 IF(MAPFLG(10+4*IWMAP-3))
     -                PRINT *,' ------ MAPFM3 MESSAGE :'//
     -                ' Overwriting current weighting Ex map.'
                 MAPFLG(10+4*IWMAP-3)=.FALSE.
            ENDIF
*   Ey.
       ELSEIF(INPCMP(I,'smh(E(y))').NE.0)THEN
            ICONT(I)=3
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM3 DEBUG   :'',
     -           '' Field '',I1,'': y-component E field.'')') I
            IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                 IF(MAPFLG(3))PRINT *,' ------ MAPFM3 MESSAGE :'//
     -                ' Overwriting current Ey map.'
                 MAPFLG(3)=.FALSE.
            ELSEIF(IDATA.EQ.10)THEN
                 IF(MAPFLG(11+4*IWMAP-3))
     -                PRINT *,' ------ MAPFM3 MESSAGE :'//
     -                ' Overwriting current weighting Ey map.'
                 MAPFLG(11+4*IWMAP-3)=.FALSE.
            ENDIF
*   Ez.
       ELSEIF(INPCMP(I,'smh(E(z))').NE.0)THEN
            ICONT(I)=4
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM3 DEBUG   :'',
     -           '' Field '',I1,'': z-component E field.'')') I
            IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                 IF(MAPFLG(4))PRINT *,' ------ MAPFM3 MESSAGE :'//
     -                ' Overwriting current Ez map.'
                 MAPFLG(4)=.FALSE.
            ELSEIF(IDATA.EQ.10)THEN
                 IF(MAPFLG(12+4*IWMAP-3))
     -                PRINT *,' ------ MAPFM3 MESSAGE :'//
     -                ' Overwriting current weighting Ez map.'
                 MAPFLG(12+4*IWMAP-3)=.FALSE.
            ENDIF
*   Dx.
       ELSEIF(INPCMP(I,'smh(D(x))').NE.0)THEN
            ICONT(I)=-9
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM3 DEBUG   :'',
     -           '' Field '',I1,'': x-component D field.'')') I
            IF(MAPFLG(9))PRINT *,' ------ MAPFM3 MESSAGE :'//
     -           ' Overwriting current material map.'
            MAPFLG(9)=.FALSE.
            MATSRC='EPSILON'
*   Dy.
       ELSEIF(INPCMP(I,'smh(D(y))').NE.0)THEN
            ICONT(I)=-9
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM3 DEBUG   :'',
     -           '' Field '',I1,'': y-component D field.'')') I
            IF(MAPFLG(9))PRINT *,' ------ MAPFM3 MESSAGE :'//
     -           ' Overwriting current material map.'
            MAPFLG(9)=.FALSE.
            MATSRC='EPSILON'
*   Dz.
       ELSEIF(INPCMP(I,'smh(D(z))').NE.0)THEN
            ICONT(I)=-9
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM3 DEBUG   :'',
     -           '' Field '',I1,'': z-component D field.'')') I
            IF(MAPFLG(9))PRINT *,' ------ MAPFM3 MESSAGE :'//
     -           ' Overwriting current material map.'
            MAPFLG(9)=.FALSE.
            MATSRC='EPSILON'
*   Unsmoothed electric fields.
       ELSEIF(INPCMP(I,'E(x)')+INPCMP(I,'E(y)')+INPCMP(I,'E(z)')+
     -      INPCMP(I,'D(x)')+INPCMP(I,'D(y)')+INPCMP(I,'D(z)').NE.
     -      0)THEN
            ICONT(I)=0
            PRINT *,' !!!!!! MAPFM3 WARNING : Maxwell 3D fields must'//
     -           ' be smoothed; field not read.'
*   V.
       ELSEIF(INPCMP(I,'smh(phi)')+INPCMP(I,'phi').NE.0)THEN
            ICONT(I)=5
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM3 DEBUG   :'',
     -           '' Field '',I1,'': potential.'')') I
            IF(MAPFLG(5))PRINT *,' ------ MAPFM3 MESSAGE :'//
     -           ' Overwriting current potential map.'
            MAPFLG(5)=.FALSE.
*   Bx.
       ELSEIF(INPCMP(I,'smh(B(x))')+INPCMP(I,'B(x)').NE.0)THEN
            ICONT(I)=6
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM3 DEBUG   :'',
     -           '' Field '',I1,'': x-component B field.'')') I
            IF(MAPFLG(6))PRINT *,' ------ MAPFM3 MESSAGE :'//
     -           ' Overwriting current Bx map.'
            MAPFLG(6)=.FALSE.
*   By.
       ELSEIF(INPCMP(I,'smh(B(y))')+INPCMP(I,'B(y)').NE.0)THEN
            ICONT(I)=7
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM3 DEBUG   :'',
     -           '' Field '',I1,'': y-component B field.'')') I
            IF(MAPFLG(7))PRINT *,' ------ MAPFM3 MESSAGE :'//
     -           ' Overwriting current By map.'
            MAPFLG(7)=.FALSE.
*   Bz.
       ELSEIF(INPCMP(I,'smh(B(z))')+INPCMP(I,'B(z)').NE.0)THEN
            ICONT(I)=8
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM3 DEBUG   :'',
     -           '' Field '',I1,'': z-component B field.'')') I
            IF(MAPFLG(8))PRINT *,' ------ MAPFM3 MESSAGE :'//
     -           ' Overwriting current Bz map.'
            MAPFLG(8)=.FALSE.
*   epsilon.
       ELSEIF(INPCMP(I,'(r(    1.00000e+00) * epsilon)')+
     -      INPCMP(I,'(r(   1.00000e+000) * epsilon)').NE.0)THEN
            ICONT(I)=9
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM3 DEBUG   :'',
     -           '' Field '',I1,'': dielectric constant.'')') I
            IF(MAPFLG(9))PRINT *,' ------ MAPFM3 MESSAGE :'//
     -           ' Overwriting material map.'
            MAPFLG(9)=.FALSE.
            MATSRC='EPSILON'
*   sigma.
       ELSEIF(INPCMP(I,'(r(    1.00000e+00) * sigma)')+
     -      INPCMP(I,'(r(   1.00000e+000) * sigma)').NE.0)THEN
            ICONT(I)=9
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM3 DEBUG   :'',
     -           '' Field '',I1,'': conductivity.'')') I
            IF(MAPFLG(9))PRINT *,' ------ MAPFM3 MESSAGE :'//
     -           ' Overwriting material map.'
            MAPFLG(9)=.FALSE.
            MATSRC='SIGMA'
*   dummy field.
       ELSEIF(INPCMP(I,'smh(0)')+INPCMP(I,'0')+
     -      INPCMP(I,'r(    0.00000e+00)')+
     -      INPCMP(I,'(r(    1.00000e+00) *  )')+
     -      INPCMP(I,'(r(   0.00000e+000) * epsilon)')+
     -      INPCMP(I,'(r(   0.00000e+00) * epsilon)')+
     -      INPCMP(I,'(r(   0.00000e+000) * sigma)')+
     -      INPCMP(I,'(r(   0.00000e+00) * sigma)').NE.0)THEN
            ICONT(I)=0
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM3 DEBUG   :'',
     -           '' Field '',I1,'': dummy.'')') I
*   unrecognised items.
       ELSE
            CALL INPSTR(I,I,STRING,NC)
            PRINT *,' !!!!!! MAPFM3 WARNING : The file ',
     -           FMAP(1:NCMAP),' contains a "'//STRING(1:NC)//
     -           '" field which is not known; field ignored.'
            ICONT(I)=0
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM3 DEBUG   :'',
     -           '' Field '',I1,'': not recognised.'')') I
       ENDIF
*   Check whether reading is required.
       IF(ICONT(I).NE.0)READ=.TRUE.
*   Ensure that the data type matches the declared type.
       IF(((ICONT(I).EQ.2.OR.ICONT(I).EQ.3.OR.ICONT(I).EQ.4).AND.
     -      (IDATA.NE.0.AND.IDATA.NE.2.AND.IDATA.NE.10)).OR.
     -      (ICONT(I).EQ.5.AND.(IDATA.NE.0.AND.IDATA.NE.5)).OR.
     -      ((ICONT(I).EQ.6.OR.ICONT(I).EQ.7.OR.ICONT(I).EQ.8).AND.
     -      (IDATA.NE.0.AND.IDATA.NE.6)).OR.
     -      ((ICONT(I).EQ.9.OR.ICONT(I).EQ.-9).AND.
     -      (IDATA.NE.0.AND.IDATA.NE.9)))THEN
            PRINT *,' !!!!!! MAPFM3 WARNING : Field ',I,' of file ',
     -           FMAP(1:NCMAP),' does not contain the declared',
     -           ' kind of data; skipped.'
            ICONT(I)=0
       ENDIF
40     CONTINUE
*** Switch back to regular input.
       CALL INPSWI('RESTORE')
*   See whether any item is left.
       IF(.NOT.READ)THEN
            PRINT *,' !!!!!! MAPFM3 WARNING : The file ',
     -           FMAP(1:NCMAP),' contains no useable'//
     -           ' information; file not read.'
            RETURN
       ENDIF
*** Loop over the tetrahedrons.
       NREAD=0
       DO 10 I=1,NDECL
       IF(I.EQ.MAX(1,NDECL/100)*(I/MAX(1,NDECL/100)))
     -      CALL PROSTA(2,REAL(I))
*** Read the line with the word "Vertices" or with "x".
50     CONTINUE
       READ(12,'(A80)',END=2000,ERR=2010,IOSTAT=IOS) STRING
       IF(STRING(1:8).NE.'Vertices')GOTO 50
*   Ensure there is still space in memory.
       IF(.NOT.MAPFLG(1))THEN
            IF(NREAD+1.GT.MXMAP)THEN
                 PRINT *,' !!!!!! MAPFM3 WARNING : Number of'//
     -                ' tetrahedrons in ',FMAP(1:NCMAP),
     -                ' exceeds compilation limit; file not read.'
                 RETURN
            ENDIF
       ENDIF
*** Read vertex coordinates.
       DO 20 J=1,4
*   If the grid is already defined, merely store for check.
       IF(MAPFLG(1))THEN
            READ(12,'(E27.20,1X,E27.20,1X,E27.7)',END=2000,ERR=2010,
     -           IOSTAT=IOS) XAUX(J),YAUX(J),ZAUX(J)
            XAUX(J)=XAUX(J)*100
            YAUX(J)=YAUX(J)*100
            ZAUX(J)=ZAUX(J)*100
*   Otherwise store the grid, converting units from m to cm.
       ELSE
            READ(12,'(E27.20,1X,E27.20,1X,E27.7)',END=2000,ERR=2010,
     -           IOSTAT=IOS) XMAP(I,J),YMAP(I,J),ZMAP(I,J)
            XMAP(I,J)=XMAP(I,J)*100
            YMAP(I,J)=YMAP(I,J)*100
            ZMAP(I,J)=ZMAP(I,J)*100
            IF(I.EQ.1.AND.J.EQ.1)THEN
                 XMMIN=XMAP(I,J)
                 XMMAX=XMAP(I,J)
                 YMMIN=YMAP(I,J)
                 YMMAX=YMAP(I,J)
                 ZMMIN=ZMAP(I,J)
                 ZMMAX=ZMAP(I,J)
            ELSE
                 XMMIN=MIN(XMMIN,XMAP(I,J))
                 XMMAX=MAX(XMMAX,XMAP(I,J))
                 YMMIN=MIN(YMMIN,YMAP(I,J))
                 YMMAX=MAX(YMMAX,YMAP(I,J))
                 ZMMIN=MIN(ZMMIN,ZMAP(I,J))
                 ZMMAX=MAX(ZMMAX,ZMAP(I,J))
            ENDIF
*   Update angular range.
            IF(YMAP(I,J).NE.0.OR.ZMAP(I,J).NE.0)THEN
                 IF(SETAX)THEN
                      XAMIN=MIN(XAMIN,ATAN2(ZMAP(I,J),YMAP(I,J)))
                      XAMAX=MAX(XAMAX,ATAN2(ZMAP(I,J),YMAP(I,J)))
                 ELSE
                      XAMIN=ATAN2(ZMAP(I,J),YMAP(I,J))
                      XAMAX=ATAN2(ZMAP(I,J),YMAP(I,J))
                      SETAX=.TRUE.
                 ENDIF
            ENDIF
            IF(ZMAP(I,J).NE.0.OR.XMAP(I,J).NE.0)THEN
                 IF(SETAY)THEN
                      YAMIN=MIN(YAMIN,ATAN2(XMAP(I,J),ZMAP(I,J)))
                      YAMAX=MAX(YAMAX,ATAN2(XMAP(I,J),ZMAP(I,J)))
                 ELSE
                      YAMIN=ATAN2(XMAP(I,J),ZMAP(I,J))
                      YAMAX=ATAN2(XMAP(I,J),ZMAP(I,J))
                      SETAY=.TRUE.
                 ENDIF
            ENDIF
            IF(XMAP(I,J).NE.0.OR.YMAP(I,J).NE.0)THEN
                 IF(SETAZ)THEN
                      ZAMIN=MIN(ZAMIN,ATAN2(YMAP(I,J),XMAP(I,J)))
                      ZAMAX=MAX(ZAMAX,ATAN2(YMAP(I,J),XMAP(I,J)))
                 ELSE
                      ZAMIN=ATAN2(YMAP(I,J),XMAP(I,J))
                      ZAMAX=ATAN2(YMAP(I,J),XMAP(I,J))
                      SETAZ=.TRUE.
                 ENDIF
            ENDIF
       ENDIF
20     CONTINUE
*   Now check that the tetrahedrons fit.
       IF(MAPFLG(1))THEN
            CALL MAPIND((XAUX(1)+XAUX(2)+XAUX(3)+XAUX(4))/4,
     -           (YAUX(1)+YAUX(2)+YAUX(3)+YAUX(4))/4,
     -           (ZAUX(1)+ZAUX(2)+ZAUX(3)+ZAUX(4))/4,
     -           T1,T2,T3,T4,JAC,DET,IMAP)
            IF(IMAP.NE.I)THEN
                 PRINT *,' !!!!!! MAPFM3 WARNING : The grid in ',
     -                FMAP(1:NCMAP),' does not match the current'//
     -                ' grid; file not read.'
            WRITE(LUNOUT,'(''  Read tetrahedron  '',I6,
     -           4(/''  (x,y,z) = '',3F15.6)/
     -           ''  Found tetrahedron '',I6,
     -           4(/''  (x,y,z) = '',3F15.6))')
     -           I,(XAUX(J),YAUX(J),ZAUX(J),J=1,4),
     -           IMAP,(XMAP(IMAP,J),YMAP(IMAP,J),ZMAP(IMAP,J),J=1,4)
C                 RETURN
            ENDIF
       ENDIF
*** Read scalar field values over the tetrahedron.
       IF(SCALAR)THEN
**  Read field values over the tetrahedron.
            READ(12,'(10(E27.20,1X))',END=2000,ERR=2010,
     -           IOSTAT=IOS) (TEMP(J),J=1,10)
*   Can be either a potential.
            IF(ICONT(1).EQ.5)THEN
                 VMAP(I,1)=TEMP(1)
                 VMAP(I,2)=TEMP(5)
                 VMAP(I,3)=TEMP(8)
                 VMAP(I,4)=TEMP(10)
                 VMAP(I,5)=TEMP(2)
                 VMAP(I,6)=TEMP(3)
                 VMAP(I,7)=TEMP(4)
                 VMAP(I,8)=TEMP(6)
                 VMAP(I,9)=TEMP(7)
                 VMAP(I,10)=TEMP(9)
*   Or a dielectricum.
            ELSEIF(ICONT(1).EQ.9)THEN
                 SUM=0
                 DO 60 J=1,10
                 SUM=SUM+TEMP(J)
60               CONTINUE
                 SUM=SUM/(1000*EPS0)
            ENDIF
**  If dielectricum, identify the material.
            IF(ICONT(1).EQ.9)THEN
                 IEPS=-1
                 DO 100 J=1,NEPS
                 IF(ABS(SUM-EPSMAT(J)).LT.1E-4*(ABS(SUM)+
     -                ABS(EPSMAT(J))))IEPS=J
100              CONTINUE
                 IF(IEPS.LT.0.AND.NEPS.GE.MXEPS)THEN
                      PRINT *,' !!!!!! MAPFM3 WARNING : Unable'//
     -                     ' to store a dielectricum from file ',
     -                     FMAP(1:NCMAP),'; file not read.'
                      RETURN
                 ELSEIF(IEPS.LT.0)THEN
                      NEPS=NEPS+1
                      IEPS=NEPS
                      EPSMAT(IEPS)=SUM
                      IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM3'',
     -                     '' DEBUG   : Adding dielectricum with'',
     -                     '' eps='',E10.3,''.'')') EPSMAT(IEPS)
                 ENDIF
                 MATMAP(I)=IEPS
**  If a potential, keep track of potential range.
            ELSEIF(ICONT(1).EQ.5)THEN
                 IF(I.EQ.1)THEN
                      VMMIN=VMAP(I,1)
                      VMMAX=VMAP(I,1)
                 ENDIF
                 VMMIN=MIN(VMMIN,VMAP(I,1),VMAP(I,2),VMAP(I,3),
     -                VMAP(I,4),VMAP(I,5),VMAP(I,6),VMAP(I,7),
     -                VMAP(I,8),VMAP(I,9),VMAP(I,10))
                 VMMAX=MAX(VMMAX,VMAP(I,1),VMAP(I,2),VMAP(I,3),
     -                VMAP(I,4),VMAP(I,5),VMAP(I,6),VMAP(I,7),
     -                VMAP(I,8),VMAP(I,9),VMAP(I,10))
            ENDIF
*** Read vectorial field values over the tetrahedron.
       ELSE
*   Take care of knowing |D| either from Ex or by summing.
            IF(ICONT(1).EQ.-9.AND.ICONT(2).EQ.-9.AND.
     -           ICONT(3).EQ.-9)THEN
                 DCOMP=0
            ELSEIF(MAPFLG(10))THEN
                 DCOMP=EXMAP(I,1)
            ENDIF
*   Loop over the vectors.
            DO 30 J=1,3
            READ(12,'(10(E27.20,1X))',END=2000,ERR=2010,
     -           IOSTAT=IOS) (TEMP(K),K=1,10)
*   Averaging of epsilons.
            IF(ICONT(J).EQ.9)THEN
                 SUM=0
                 DO 80 K=1,10
                 SUM=SUM+TEMP(K)
80               CONTINUE
                 SUM=SUM/(1000*EPS0)
            ELSEIF(ICONT(J).EQ.-9)THEN
                 DCOMP=DCOMP+(TEMP(1)+TEMP(5)+TEMP(8)+TEMP(10))**2/
     -                160000
            ENDIF
*   Ex or EWx
            IF(ICONT(J).EQ.2)THEN
                 IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                      EXMAP(I,1)=TEMP(1)/100
                      EXMAP(I,2)=TEMP(5)/100
                      EXMAP(I,3)=TEMP(8)/100
                      EXMAP(I,4)=TEMP(10)/100
                      EXMAP(I,5)=TEMP(2)/100
                      EXMAP(I,6)=TEMP(3)/100
                      EXMAP(I,7)=TEMP(4)/100
                      EXMAP(I,8)=TEMP(6)/100
                      EXMAP(I,9)=TEMP(7)/100
                      EXMAP(I,10)=TEMP(9)/100
                 ELSEIF(IDATA.EQ.10)THEN
                      EWXMAP(I,1,IWMAP)=TEMP(1)/100
                      EWXMAP(I,2,IWMAP)=TEMP(5)/100
                      EWXMAP(I,3,IWMAP)=TEMP(8)/100
                      EWXMAP(I,4,IWMAP)=TEMP(10)/100
                      EWXMAP(I,5,IWMAP)=TEMP(2)/100
                      EWXMAP(I,6,IWMAP)=TEMP(3)/100
                      EWXMAP(I,7,IWMAP)=TEMP(4)/100
                      EWXMAP(I,8,IWMAP)=TEMP(6)/100
                      EWXMAP(I,9,IWMAP)=TEMP(7)/100
                      EWXMAP(I,10,IWMAP)=TEMP(9)/100
                 ENDIF
*   Bx.
            ELSEIF(ICONT(J).EQ.6)THEN
                 BXMAP(I,1)=TEMP(1)
                 BXMAP(I,2)=TEMP(5)
                 BXMAP(I,3)=TEMP(8)
                 BXMAP(I,4)=TEMP(10)
                 BXMAP(I,5)=TEMP(2)
                 BXMAP(I,6)=TEMP(3)
                 BXMAP(I,7)=TEMP(4)
                 BXMAP(I,8)=TEMP(6)
                 BXMAP(I,9)=TEMP(7)
                 BXMAP(I,10)=TEMP(9)
*   Ey or EWy.
            ELSEIF(ICONT(J).EQ.3)THEN
                 IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                      EYMAP(I,1)=TEMP(1)/100
                      EYMAP(I,2)=TEMP(5)/100
                      EYMAP(I,3)=TEMP(8)/100
                      EYMAP(I,4)=TEMP(10)/100
                      EYMAP(I,5)=TEMP(2)/100
                      EYMAP(I,6)=TEMP(3)/100
                      EYMAP(I,7)=TEMP(4)/100
                      EYMAP(I,8)=TEMP(6)/100
                      EYMAP(I,9)=TEMP(7)/100
                      EYMAP(I,10)=TEMP(9)/100
                 ELSEIF(IDATA.EQ.10)THEN
                      EWYMAP(I,1,IWMAP)=TEMP(1)/100
                      EWYMAP(I,2,IWMAP)=TEMP(5)/100
                      EWYMAP(I,3,IWMAP)=TEMP(8)/100
                      EWYMAP(I,4,IWMAP)=TEMP(10)/100
                      EWYMAP(I,5,IWMAP)=TEMP(2)/100
                      EWYMAP(I,6,IWMAP)=TEMP(3)/100
                      EWYMAP(I,7,IWMAP)=TEMP(4)/100
                      EWYMAP(I,8,IWMAP)=TEMP(6)/100
                      EWYMAP(I,9,IWMAP)=TEMP(7)/100
                      EWYMAP(I,10,IWMAP)=TEMP(9)/100
                 ENDIF
*   By.
            ELSEIF(ICONT(J).EQ.7)THEN
                 BYMAP(I,1)=TEMP(1)
                 BYMAP(I,2)=TEMP(5)
                 BYMAP(I,3)=TEMP(8)
                 BYMAP(I,4)=TEMP(10)
                 BYMAP(I,5)=TEMP(2)
                 BYMAP(I,6)=TEMP(3)
                 BYMAP(I,7)=TEMP(4)
                 BYMAP(I,8)=TEMP(6)
                 BYMAP(I,9)=TEMP(7)
                 BYMAP(I,10)=TEMP(9)
*   Ez or EWz.
            ELSEIF(ICONT(J).EQ.4)THEN
                 IF(IDATA.EQ.0.OR.IDATA.EQ.2)THEN
                      EZMAP(I,1)=TEMP(1)/100
                      EZMAP(I,2)=TEMP(5)/100
                      EZMAP(I,3)=TEMP(8)/100
                      EZMAP(I,4)=TEMP(10)/100
                      EZMAP(I,5)=TEMP(2)/100
                      EZMAP(I,6)=TEMP(3)/100
                      EZMAP(I,7)=TEMP(4)/100
                      EZMAP(I,8)=TEMP(6)/100
                      EZMAP(I,9)=TEMP(7)/100
                      EZMAP(I,10)=TEMP(9)/100
                 ELSEIF(IDATA.EQ.10)THEN
                      EWZMAP(I,1,IWMAP)=TEMP(1)/100
                      EWZMAP(I,2,IWMAP)=TEMP(5)/100
                      EWZMAP(I,3,IWMAP)=TEMP(8)/100
                      EWZMAP(I,4,IWMAP)=TEMP(10)/100
                      EWZMAP(I,5,IWMAP)=TEMP(2)/100
                      EWZMAP(I,6,IWMAP)=TEMP(3)/100
                      EWZMAP(I,7,IWMAP)=TEMP(4)/100
                      EWZMAP(I,8,IWMAP)=TEMP(6)/100
                      EWZMAP(I,9,IWMAP)=TEMP(7)/100
                      EWZMAP(I,10,IWMAP)=TEMP(9)/100
                 ENDIF
*   Bz.
            ELSEIF(ICONT(J).EQ.8)THEN
                 BZMAP(I,1)=TEMP(1)
                 BZMAP(I,2)=TEMP(5)
                 BZMAP(I,3)=TEMP(8)
                 BZMAP(I,4)=TEMP(10)
                 BZMAP(I,5)=TEMP(2)
                 BZMAP(I,6)=TEMP(3)
                 BZMAP(I,7)=TEMP(4)
                 BZMAP(I,8)=TEMP(6)
                 BZMAP(I,9)=TEMP(7)
                 BZMAP(I,10)=TEMP(9)
            ENDIF
30          CONTINUE
**  If dielectricum, identify the material.
            IF(ICONT(1).EQ.9.OR.ICONT(2).EQ.9.OR.ICONT(3).EQ.9)THEN
                 IEPS=-1
                 DO 160 J=1,NEPS
                 IF(ABS(SUM-EPSMAT(J)).LT.1E-4*(ABS(SUM)+
     -                ABS(EPSMAT(J))))IEPS=J
160              CONTINUE
                 IF(IEPS.LT.0.AND.NEPS.GE.MXEPS)THEN
                      PRINT *,' !!!!!! MAPFM3 WARNING : Unable'//
     -                     ' to store a dielectricum from file ',
     -                     FMAP(1:NCMAP),'; file not read.'
                      RETURN
                 ELSEIF(IEPS.LT.0)THEN
                      NEPS=NEPS+1
                      IEPS=NEPS
                      EPSMAT(IEPS)=SUM
                      IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM3'',
     -                     '' DEBUG   : Adding dielectricum with'',
     -                     '' eps='',E10.3,''.'')') EPSMAT(IEPS)
                 ENDIF
                 MATMAP(I)=IEPS
                 NEWEPS=.TRUE.
**  Dielectricum identification via D/E comparison.
            ELSEIF((MAPFLG(2).AND.MAPFLG(3).AND.MAPFLG(4).AND.
     -           (.NOT.MAPFLG(9)).AND.ICONT(1).EQ.-9.AND.
     -           ICONT(2).EQ.-9.AND.ICONT(3).EQ.-9).OR.
     -           (MAPFLG(10).AND.(.NOT.MAPFLG(9)).AND.
     -           ICONT(1).EQ.2.AND.ICONT(2).EQ.3.AND.
     -           ICONT(3).EQ.4))THEN
                 IEPS=-1
                 ECOMP=((EXMAP(I,1)+EXMAP(I,2)+EXMAP(I,3)+
     -                EXMAP(I,4))**2+(EYMAP(I,1)+EYMAP(I,2)+
     -                EYMAP(I,3)+EYMAP(I,4))**2+(EZMAP(I,1)+
     -                EZMAP(I,2)+EZMAP(I,3)+EZMAP(I,4))**2)/16
                 DO 170 J=1,NEPS
                 IF(ABS(ECOMP*(100*EPS0*EPSMAT(J))**2-DCOMP).LT.1E-4*
     -                (ABS(ECOMP*(100*EPS0*EPSMAT(J))**2)+
     -                ABS(DCOMP)))IEPS=J
170              CONTINUE
                 IF(ECOMP.LE.0.AND.DCOMP.GT.0)THEN
                      PRINT *,' !!!!!! MAPFM3 WARNING : Found'//
     -                     ' a dielectric constant of 0; skipped.'
                 ELSEIF(IEPS.LT.0.AND.NEPS.GE.MXEPS)THEN
                      PRINT *,' !!!!!! MAPFM3 WARNING : Unable'//
     -                     ' to store a dielectricum from file ',
     -                     FMAP(1:NCMAP),'; file not read.'
                      RETURN
                 ELSEIF(IEPS.LT.0)THEN
                      NEPS=NEPS+1
                      IEPS=NEPS
                      IF(ECOMP.LE.0)THEN
                           PRINT *,' ------ MAPFM3 MESSAGE : Unable'//
     -                          ' to determine epsilon in an E=0'//
     -                          ' tetrahedron; epsilon set to 0.'
                           EPSMAT(IEPS)=0
                      ELSE
                           EPSMAT(IEPS)=SQRT(DCOMP/ECOMP)/(100*EPS0)
                      ENDIF
                      IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFM3'',
     -                     '' DEBUG   : Adding dielectricum with'',
     -                     '' eps='',E10.3,''.'')') EPSMAT(IEPS)
                 ENDIF
                 MATMAP(I)=IEPS
                 NEWEPS=.TRUE.
*  Otherwise store the field.
            ELSEIF(ICONT(1).EQ.-9.AND.ICONT(2).EQ.-9.AND.
     -           ICONT(3).EQ.-9.AND.(.NOT.MAPFLG(2)))THEN
                 EXMAP(I,1)=DCOMP
            ENDIF
       ENDIF
*   Update the count.
       NREAD=NREAD+1
*   Skip the line with "h" at the end.
       READ(12,'()',END=2000,ERR=2010,IOSTAT=IOS)
10     CONTINUE
*** Be sure something has been read.
2000   CONTINUE
       IF(MAPFLG(1))THEN
            IF(NREAD.NE.NMAP)THEN
                 PRINT *,' !!!!!! MAPFM3 WARNING : Number of'//
     -                ' tetrahedrons in ',FMAP(1:NCMAP),' does'//
     -                ' not agree with previous files; not read.'
                 RETURN
            ENDIF
       ELSE
            IF(NREAD.LE.0)THEN
                 PRINT *,' !!!!!! MAPFM3 WARNING : ',FMAP(1:NCMAP),
     -                ' contain no tetrahedrons; not read.'
                 RETURN
            ELSE
                 NMAP=NREAD
            ENDIF
       ENDIF
*** Materials have been defined is NEWEPS is set.
       IF(NEWEPS)MAPFLG(9)=.TRUE.
*** Flag those elements which have been defined.
       MAPFLG(1)=.TRUE.
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
2010   CONTINUE
       PRINT *,' !!!!!! MAPFM3 WARNING : Error reading field map'//
     -      ' file ',FMAP(1:NCMAP),'; map not available.'
       RETURN
       END
