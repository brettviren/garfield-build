CDECK  ID>, ALGEX5.
       SUBROUTINE ALGEX5(I,IFAIL)
*-----------------------------------------------------------------------
*   ALGEX5 - Routine executing instruction I (produced by ALGPRE).
*            This routine takes care of arithmetic operations between
*            histograms.
*   (Last changed on 19/11/10.)
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
       INTEGER INS(MXINS,4),ALGENT(MXALGE,10),MODREG(MXCONS:MXREG),
     -      ISYNCH,IINS0,ICONS0,ARGREF(MXARG,2),MODARG(MXARG),
     -      NREG,NCONS,NINS,NERR,NRES,NALGE,IENTRL,NAERR(100)
       REAL REG(MXCONS:MXREG),ARG(MXARG),EXPMAX
       PARAMETER(EXPMAX=40.0)
       LOGICAL EXEC(MXINS),LIGUND,LINUND
       COMMON /ALGDAT/ REG,ARG,MODARG,ARGREF,INS,MODREG,ALGENT,
     -      NREG,NCONS,NINS,NERR,NAERR,
     -      NRES,NALGE,IENTRL,ISYNCH,IINS0,ICONS0,EXEC,LIGUND,LINUND
       DOUBLE PRECISION CONTEN(MXHIST,0:MXCHA+1)
       REAL XMIN(MXHIST),XMAX(MXHIST)
       DOUBLE PRECISION SX0(MXHIST),SX1(MXHIST),SX2(MXHIST)
       INTEGER NCHA(MXHIST),NENTRY(MXHIST)
       LOGICAL SET(MXHIST),HISUSE(MXHIST),HISLIN(MXHIST)
       COMMON /HISDAT/ SX0,SX1,SX2,CONTEN,XMIN,XMAX,HISUSE,HISLIN,NCHA,
     -      NENTRY,SET
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*(MXINCH) STR1
       REAL RNDUNI,RANLAN,RNDEXP,RNDNOR,RNDPOL,RNDFUN,DENLAN,EPS,
     -      XXMIN,XXMAX,XX,XAUX,AVER,RMS,DGAMMA,DLGAMA,RNGAMA,RNDLAP
       INTEGER IFAIL,IFAIL1,IHIST1,IHIST3,IHIST4,NNCHA,I,J,NPOIS,IREF,
     -      IERR,NC1,NNENTR
       LOGICAL HEXIST,HSET
       EXTERNAL RNDUNI,RANLAN,RNDEXP,RNDNOR,RNDPOL,RNDFUN,DENLAN,DGAMMA,
     -      DLGAMA,RNGAMA,RNDLAP
*** Set IFAIL to 1 and EPS.
       IFAIL=1
       EPS=1.0E-5
*** For easier reference, define histogram references.
       IHIST1=NINT(REG(INS(I,1)))
       IHIST3=NINT(REG(INS(I,3)))
       IHIST4=NINT(REG(INS(I,4)))
*** Verify that the objects are indeed valid, set histograms.
       IF(INS(I,2).NE.6.AND.MODREG(INS(I,1)).EQ.4.AND.
     -      MODREG(INS(I,3)).EQ.4)THEN
*   Validity of reference number.
            IF(IHIST1.LE.0.OR.IHIST3.LE.0.OR.
     -           IHIST1.GT.MXHIST.OR.IHIST3.GT.MXHIST)THEN
                 MODREG(INS(I,4))=0
                 REG(INS(I,4))=0
                 IF(LDEBUG)PRINT *,' ++++++ ALGEX5 DEBUG   : Invalid'//
     -                ' histogram reference ',IHIST1,IHIST3
                 NAERR(51)=NAERR(51)+1
                 RETURN
*   Histograms must have been declared.
            ELSEIF(.NOT.(HISUSE(IHIST1).AND.HISUSE(IHIST3)))THEN
                 MODREG(INS(I,4))=0
                 REG(INS(I,4))=0
                 IF(LDEBUG)THEN
                      PRINT *,' ++++++ ALGEX5 DEBUG   : Histogram'//
     -                     ' not yet in use'
                      PRINT *,'                         Arg 1: ref=',
     -                     IHIST1,' use=',HISUSE(IHIST1),
     -                     ', Arg 3: ref=',
     -                     IHIST3,' use=',HISUSE(IHIST3)
                 ENDIF
                 NAERR(52)=NAERR(52)+1
                 RETURN
*   If autoranged, then the range must have been set.
            ELSEIF(.NOT.(SET(IHIST1).AND.SET(IHIST3)))THEN
                 MODREG(INS(I,4))=0
                 REG(INS(I,4))=0
                 IF(LDEBUG)THEN
                      PRINT *,' ++++++ ALGEX5 DEBUG   : Histogram'//
     -                     ' not yet autoscaled'
                      PRINT *,'                         Arg 1: ref=',
     -                     IHIST1,' set=',SET(IHIST1),', Arg 3: ref=',
     -                     IHIST3,' set=',SET(IHIST3)
                 ENDIF
                 NAERR(53)=NAERR(53)+1
                 RETURN
*   The range and the number of bins must agree.
            ELSEIF(ABS(XMIN(IHIST1)-XMIN(IHIST3)).GT.
     -           EPS*(1+ABS(XMIN(IHIST1))+ABS(XMIN(IHIST3))).OR.
     -           ABS(XMAX(IHIST1)-XMAX(IHIST3)).GT.
     -           EPS*(1+ABS(XMAX(IHIST1))+ABS(XMAX(IHIST3))).OR.
     -           NCHA(IHIST1).NE.NCHA(IHIST3))THEN
                 MODREG(INS(I,4))=0
                 REG(INS(I,4))=0
                 IF(LDEBUG)THEN
                      PRINT *,' ++++++ ALGEX5 DEBUG   : Histograms'//
     -                     ' not compatible.'
                      PRINT *,'                         Arg 1: ref=',
     -                     IHIST1,' range=',XMIN(IHIST1),XMAX(IHIST1),
     -                     ' bins=',NCHA(IHIST1)
                      PRINT *,'                         Arg 3: ref=',
     -                     IHIST3,' range=',XMIN(IHIST3),XMAX(IHIST3),
     -                     ' bins=',NCHA(IHIST3)
                 ENDIF
                 NAERR(54)=NAERR(54)+1
                 RETURN
            ENDIF
       ELSEIF(MODREG(INS(I,3)).EQ.4)THEN
*   Validity of reference number.
            IF(IHIST3.LE.0.OR.IHIST3.GT.MXHIST)THEN
                 MODREG(INS(I,4))=0
                 REG(INS(I,4))=0
                 IF(LDEBUG)PRINT *,' ++++++ ALGEX5 DEBUG   : Invalid'//
     -                ' histogram reference ',IHIST3
                 NAERR(51)=NAERR(51)+1
                 RETURN
*   Histogram must have been declared.
            ELSEIF(.NOT.HISUSE(IHIST3))THEN
                 MODREG(INS(I,4))=0
                 REG(INS(I,4))=0
                 IF(LDEBUG)THEN
                      PRINT *,' ++++++ ALGEX5 DEBUG   : Histogram'//
     -                     ' not yet in use'
                      PRINT *,'                         Arg 3: ref=',
     -                     IHIST3,' use=',HISUSE(IHIST3)
                 ENDIF
                 NAERR(52)=NAERR(52)+1
                 RETURN
*   If autoranged, then the range must have been set.
            ELSEIF(.NOT.SET(IHIST3))THEN
                 MODREG(INS(I,4))=0
                 REG(INS(I,4))=0
                 IF(LDEBUG)THEN
                      PRINT *,' ++++++ ALGEX5 DEBUG   : Histogram'//
     -                     ' not yet autoscaled'
                      PRINT *,'                         Arg 3: ref=',
     -                     IHIST3,' set=',SET(IHIST3)
                 ENDIF
                 NAERR(53)=NAERR(53)+1
                 RETURN
            ENDIF
*   Check nothing else than numbers and histograms appear.
       ELSEIF((INS(I,2).EQ.6.AND.MODREG(INS(I,3)).NE.4).OR.
     -      (INS(I,2).NE.6.AND.((MODREG(INS(I,1)).NE.2.AND.
     -      MODREG(INS(I,1)).NE.4).OR.(MODREG(INS(I,3)).NE.2.AND.
     -      MODREG(INS(I,3)).NE.4))))THEN
            MODREG(INS(I,4))=0
            REG(INS(I,4))=0
            IF(LDEBUG)THEN
                 PRINT *,' ++++++ ALGEX5 DEBUG   : Unable to'//
     -                ' handle received modes'
                 PRINT *,'                         Arg 1: ref=',
     -                IHIST1,' mode=',MODREG(INS(I,1))
                 PRINT *,'                         Arg 3: ref=',
     -                IHIST3,' mode=',MODREG(INS(I,3))
            ENDIF
            RETURN
       ENDIF
*** Establish parameters of the resulting histogram.
       IF(INS(I,2).EQ.6)THEN
            IF(MODREG(INS(I,3)).EQ.4)THEN
                 XXMIN=XMIN(IHIST3)
                 XXMAX=XMAX(IHIST3)
                 NNCHA=NCHA(IHIST3)
            ELSE
                 RETURN
            ENDIF
       ELSE
            IF(MODREG(INS(I,1)).EQ.4)THEN
                 XXMIN=XMIN(IHIST1)
                 XXMAX=XMAX(IHIST1)
                 NNCHA=NCHA(IHIST1)
            ELSEIF(MODREG(INS(I,3)).EQ.4)THEN
                 XXMIN=XMIN(IHIST3)
                 XXMAX=XMAX(IHIST3)
                 NNCHA=NCHA(IHIST3)
            ELSE
                 RETURN
            ENDIF
       ENDIF
*** If one of the arguments is scalar, turn into a histogram.
       IF(INS(I,2).NE.6.AND.MODREG(INS(I,1)).EQ.2)THEN
            CALL HISADM('ALLOCATE',IHIST1,NNCHA,XXMIN,XXMAX,
     -           .FALSE.,IFAIL1)
            IF(IFAIL1.NE.0)RETURN
            DO 500 J=1,NNCHA
            CONTEN(IHIST1,J)=REG(INS(I,1))
500         CONTINUE
       ENDIF
       IF(MODREG(INS(I,3)).EQ.2)THEN
            CALL HISADM('ALLOCATE',IHIST3,NNCHA,XXMIN,XXMAX,
     -           .FALSE.,IFAIL1)
            IF(IFAIL1.NE.0)RETURN
            DO 510 J=1,NNCHA
            CONTEN(IHIST3,J)=REG(INS(I,3))
510         CONTINUE
       ENDIF
*** Allocate a histogram for the result.
       CALL HISADM('ALLOCATE',IHIST4,NNCHA,XXMIN,XXMAX,.FALSE.,IFAIL1)
       IF(IFAIL1.NE.0)RETURN
*** Perform the actual calculation: binary numerical operators.
       IF(INS(I,2).EQ.1)THEN
            DO 10 J=1,NNCHA
            CONTEN(IHIST4,J)=CONTEN(IHIST1,J)+CONTEN(IHIST3,J)
10          CONTINUE
            MODREG(INS(I,4))=4
       ELSEIF(INS(I,2).EQ.2)THEN
            DO 20 J=1,NNCHA
            CONTEN(IHIST4,J)=CONTEN(IHIST1,J)-CONTEN(IHIST3,J)
20          CONTINUE
            MODREG(INS(I,4))=4
       ELSEIF(INS(I,2).EQ.3)THEN
            DO 30 J=1,NNCHA
            CONTEN(IHIST4,J)=CONTEN(IHIST1,J)*CONTEN(IHIST3,J)
30          CONTINUE
            MODREG(INS(I,4))=4
       ELSEIF(INS(I,2).EQ.4)THEN
            DO 40 J=1,NNCHA
            IF(CONTEN(IHIST3,J).NE.0)THEN
                 CONTEN(IHIST4,J)=CONTEN(IHIST1,J)/CONTEN(IHIST3,J)
            ELSE
                 CONTEN(IHIST4,J)=0.0
            ENDIF
40          CONTINUE
            MODREG(INS(I,4))=4
       ELSEIF(INS(I,2).EQ.5)THEN
            DO 50 J=1,NNCHA
            IF(ABS(CONTEN(IHIST3,J)-NINT(CONTEN(IHIST3,J))).LT.EPS)THEN
                 IF(NINT(CONTEN(IHIST3,J)).LE.0.AND.
     -                CONTEN(IHIST1,J).EQ.0)THEN
                      CONTEN(IHIST4,J)=0.0
                 ELSEIF(2*(NINT(CONTEN(IHIST3,J))/2).EQ.
     -                NINT(CONTEN(IHIST3,J)))THEN
                      CONTEN(IHIST4,J)=ABS(CONTEN(IHIST1,J))**
     -                     NINT(CONTEN(IHIST3,J))
                 ELSE
                      CONTEN(IHIST4,J)=SIGN(ABS(CONTEN(IHIST1,J))**
     -                     NINT(CONTEN(IHIST3,J)),CONTEN(IHIST1,J))
                 ENDIF
            ELSEIF(CONTEN(IHIST1,J).EQ.0.AND.CONTEN(IHIST3,J).GT.0)THEN
                 CONTEN(IHIST4,J)=0
            ELSEIF(CONTEN(IHIST1,J).GT.0)THEN
                 CONTEN(IHIST4,J)=CONTEN(IHIST1,J)**CONTEN(IHIST3,J)
            ELSE
                 NAERR(11)=NAERR(11)+1
                 CONTEN(IHIST4,J)=0.0
            ENDIF
50          CONTINUE
            MODREG(INS(I,4))=4
*   Numerical function calls.
       ELSEIF(INS(I,2).EQ.6)THEN
            MODREG(INS(I,4))=4
            DO 60 J=1,NNCHA
            IF(INS(I,1).EQ. 1)THEN
                 IF(CONTEN(IHIST3,J).GT.EXPMAX)THEN
                      NAERR(2)=NAERR(2)+1
                      CONTEN(IHIST4,J)=EXP(EXPMAX)
                      RETURN
                 ELSEIF(CONTEN(IHIST3,J).LT.-EXPMAX)THEN
                      IF(LIGUND)THEN
                           CONTEN(IHIST4,J)=0
                      ELSE
                           NAERR(3)=NAERR(3)+1
                           RETURN
                      ENDIF
                 ELSE
                      CONTEN(IHIST4,J)=EXP(CONTEN(IHIST3,J))
                 ENDIF
            ELSEIF(INS(I,1).EQ.-1)THEN
                 IF(CONTEN(IHIST3,J).LE.0.0)RETURN
                 CONTEN(IHIST4,J)=LOG(CONTEN(IHIST3,J))
            ENDIF
            IF((INS(I,1).EQ.-2.OR.INS(I,1).EQ.-3).AND.
     -           ABS(CONTEN(IHIST3,J)).GT.1.0)THEN
                 CONTEN(IHIST4,J)=0.0
            ELSE
                 IF(INS(I,1).EQ.-2)CONTEN(IHIST4,J)=
     -                ASIN(CONTEN(IHIST3,J))
                 IF(INS(I,1).EQ.-3)CONTEN(IHIST4,J)=
     -                ACOS(CONTEN(IHIST3,J))
            ENDIF
            IF(INS(I,1).EQ. 2)CONTEN(IHIST4,J)=  SIN(CONTEN(IHIST3,J))
            IF(INS(I,1).EQ. 3)CONTEN(IHIST4,J)=  COS(CONTEN(IHIST3,J))
            IF(INS(I,1).EQ. 4)CONTEN(IHIST4,J)=  TAN(CONTEN(IHIST3,J))
            IF(INS(I,1).EQ.-4)CONTEN(IHIST4,J)= ATAN(CONTEN(IHIST3,J))
            IF(INS(I,1).EQ. 5)CONTEN(IHIST4,J)=  ABS(CONTEN(IHIST3,J))
            IF(INS(I,1).EQ.-5)THEN
                 IF(CONTEN(IHIST3,J).LT.0.0)THEN
                      CONTEN(IHIST4,J)=-1.0
                 ELSE
                      CONTEN(IHIST4,J)=SQRT(CONTEN(IHIST3,J))
                 ENDIF
            ENDIF
            IF(INS(I,1).EQ. 6)CONTEN(IHIST4,J)=      CONTEN(IHIST3,J)
            IF(INS(I,1).EQ.-6)CONTEN(IHIST4,J)=     -CONTEN(IHIST3,J)
            IF(INS(I,1).EQ. 7)CONTEN(IHIST4,J)= SINH(CONTEN(IHIST3,J))
            IF(INS(I,1).EQ.-7)CONTEN(IHIST4,J)=LOG(CONTEN(IHIST3,J)+
     -           SQRT(1+CONTEN(IHIST3,J)**2))
            IF(INS(I,1).EQ. 8)CONTEN(IHIST4,J)= COSH(CONTEN(IHIST3,J))
            IF(INS(I,1).EQ.-8)THEN
                 IF(CONTEN(IHIST3,J).LT.1)THEN
                      CONTEN(IHIST4,J)=0.0
                 ELSE
                      CONTEN(IHIST4,J)=LOG(CONTEN(IHIST3,J)+
     -                     SQRT(CONTEN(IHIST3,J)**2-1))
                 ENDIF
            ENDIF
            IF(INS(I,1).EQ. 9)CONTEN(IHIST4,J)= TANH(CONTEN(IHIST3,J))
            IF(INS(I,1).EQ.-9)THEN
                 IF(CONTEN(IHIST3,J).LE.-1.0.OR.
     -                CONTEN(IHIST3,J).GE.1.0)THEN
                      CONTEN(IHIST4,J)=0.0
                 ELSE
                      CONTEN(IHIST4,J)=0.5*LOG((1+CONTEN(IHIST3,J))/
     -                     (1-CONTEN(IHIST3,J)))
                 ENDIF
            ENDIF
*   Truncation of a real number.
            IF(INS(I,1).EQ.11)THEN
                 CONTEN(IHIST4,J)=INT(CONTEN(IHIST3,J))
                 IF(CONTEN(IHIST3,J).LT.0)CONTEN(IHIST4,J)=
     -                CONTEN(IHIST4,J)-1.0
            ELSEIF(INS(I,1).EQ.-11)THEN
                 CONTEN(IHIST4,J)=CONTEN(IHIST3,J)-INT(CONTEN(IHIST3,J))
                 IF(CONTEN(IHIST3,J).LT.0)CONTEN(IHIST4,J)=
     -                CONTEN(IHIST4,J)+1.0
            ENDIF
*   Landau density.
            IF(INS(I,1).EQ.18)CONTEN(IHIST4,J)=
     -           DENLAN(REAL(CONTEN(IHIST3,J)))
*   Gamma function.
            IF(INS(I,1).EQ.47)THEN
                 IF(CONTEN(IHIST3,J).LT.1E-20)THEN
                      NAERR(12)=NAERR(12)+1
                      CONTEN(IHIST4,J)=0.0
                 ELSEIF(CONTEN(IHIST3,J).GT.25.0)THEN
                      NAERR(13)=NAERR(13)+1
                      CONTEN(IHIST4,J)=0.0
                 ELSE
                      CONTEN(IHIST4,J)=DGAMMA(CONTEN(IHIST3,J))
                 ENDIF
*   log-Gamma function.
            ELSEIF(INS(I,1).EQ.48)THEN
                 IF(CONTEN(IHIST3,J).LE.0)THEN
                      NAERR(14)=NAERR(14)+1
                      CONTEN(IHIST4,J)=0.0
                 ELSE
                      CONTEN(IHIST4,J)=DLGAMA(CONTEN(IHIST3,J))
                 ENDIF
            ENDIF
60          CONTINUE
*   Make a string from a number.
            IF(INS(I,1).EQ.12)THEN
                 CALL STRBUF('STORE',IREF,'Histogram',9,IFAIL)
                 IF(IFAIL.NE.0)RETURN
                 MODREG(INS(I,4))=1
                 REG(INS(I,4))=IREF
*   Sum and product.
            ELSEIF(INS(I,1).EQ.13)THEN
                 REG(INS(I,4))=0
                 MODREG(INS(I,4))=2
                 DO 90 J=1,NNCHA
                 REG(INS(I,4))=REG(INS(I,4))+CONTEN(IHIST3,J)
90               CONTINUE
            ELSEIF(INS(I,1).EQ.14)THEN
                 REG(INS(I,4))=1
                 MODREG(INS(I,4))=2
                 DO 100 J=1,NNCHA
                 REG(INS(I,4))=REG(INS(I,4))*CONTEN(IHIST3,J)
100              CONTINUE
*   Reference of an histogram.
            ELSEIF(INS(I,1).EQ.15)THEN
                 REG(INS(I,4))=IHIST3
                 MODREG(INS(I,4))=2
*   Maximum and minimum.
            ELSEIF(INS(I,1).EQ.19)THEN
                 REG(INS(I,4))=CONTEN(IHIST3,1)
                 MODREG(INS(I,4))=2
                 DO 95 J=2,NNCHA
                 REG(INS(I,4))=MIN(REG(INS(I,4)),CONTEN(IHIST3,J))
95               CONTINUE
            ELSEIF(INS(I,1).EQ.20)THEN
                 REG(INS(I,4))=CONTEN(IHIST3,1)
                 MODREG(INS(I,4))=2
                 DO 96 J=2,NNCHA
                 REG(INS(I,4))=MAX(REG(INS(I,4)),CONTEN(IHIST3,J))
96               CONTINUE
*   Mean and RMS.
            ELSEIF(INS(I,1).EQ.41)THEN
                 CALL HISINQ(IHIST3,HEXIST,HSET,NNCHA,XXMIN,XXMAX,
     -                NNENTR,AVER,RMS)
                 REG(INS(I,4))=AVER
                 MODREG(INS(I,4))=2
            ELSEIF(INS(I,1).EQ.42)THEN
                 CALL HISINQ(IHIST3,HEXIST,HSET,NNCHA,XXMIN,XXMAX,
     -                NNENTR,AVER,RMS)
                 REG(INS(I,4))=RMS
                 MODREG(INS(I,4))=2
*   Locate a global variable from its name.
            ELSEIF(INS(I,1).EQ.16)THEN
                 CALL STRBUF('READ',NINT(REG(INS(I,3))),STR1,NC1,IFAIL1)
                 DO 101 J=1,NGLB
                 IF(STR1(1:NC1).EQ.GLBVAR(J))THEN
                      MODREG(INS(I,4))=GLBMOD(J)
                      REG(INS(I,4))=GLBVAL(J)
                      GOTO 102
                 ENDIF
101              CONTINUE
                 MODREG(INS(I,4))=0
                 REG(INS(I,4))=0
102              CONTINUE
*   Return the type of the argument.
            ELSEIF(INS(I,1).EQ.17)THEN
                 CALL STRBUF('STORE',IREF,'Histogram',9,IFAIL1)
                 IF(IFAIL1.NE.0)RETURN
                 REG(INS(I,4))=IREF
                 MODREG(INS(I,4))=1
*   Random number according to a histogram.
            ELSEIF(INS(I,1).EQ.28)THEN
                 CALL RNDHIS(IHIST3,XAUX)
                 REG(INS(I,4))=XAUX
                 MODREG(INS(I,4))=2
            ENDIF
*   Random number generators.
            DO 110 J=1,NNCHA
            IF(INS(I,1).EQ.21)THEN
                 CONTEN(IHIST4,J)=DBLE(RNDUNI(REG(INS(I,3))))
            ELSEIF(INS(I,1).EQ.22)THEN
                 CONTEN(IHIST4,J)=DBLE(RNDNOR(0.0,1.0))
            ELSEIF(INS(I,1).EQ.23)THEN
                 CONTEN(IHIST4,J)=DBLE(RNDEXP(REAL(CONTEN(IHIST3,J))))
            ELSEIF(INS(I,1).EQ.24)THEN
                 CALL RNPSSN(REAL(CONTEN(IHIST3,J)),NPOIS,IERR)
                 CONTEN(IHIST4,J)=DBLE(NPOIS)
            ELSEIF(INS(I,1).EQ.25)THEN
                 CONTEN(IHIST4,J)=DBLE(RANLAN(REAL(RNDUNI(1.0))))
            ELSEIF(INS(I,1).EQ.26)THEN
                 CONTEN(IHIST4,J)=DBLE(RNDPOL(REAL(CONTEN(IHIST3,J))))
            ELSEIF(INS(I,1).EQ.27)THEN
                 CONTEN(IHIST4,J)=DBLE(RNDFUN(REAL(CONTEN(IHIST3,J))))
            ELSEIF(INS(I,1).EQ.29)THEN
                 IF(CONTEN(IHIST3,J).GT.0)THEN
                      CONTEN(IHIST4,J)=
     -                     DBLE(RNGAMA(REAL(CONTEN(IHIST3,J))))
                 ELSE
                      CONTEN(IHIST4,J)=0
                      NAERR(14)=NAERR(14)+1
                 ENDIF
            ELSEIF(INS(I,1).EQ.30)THEN
                 CONTEN(IHIST4,J)=DBLE(RNDLAP(REAL(CONTEN(IHIST3,J))))
            ENDIF
110         CONTINUE
*   Binary logical operators between real type arguments.
       ELSEIF(INS(I,2).EQ.10)THEN
            MODREG(INS(I,4))=3
            REG(INS(I,4))=1.0
            DO 120 J=1,NNCHA
            IF(ABS(CONTEN(IHIST1,J)-CONTEN(IHIST3,J)).GT.EPS)
     -           REG(INS(I,4))=0.0
120         CONTINUE
       ELSEIF(INS(I,2).EQ.11)THEN
            MODREG(INS(I,4))=3
            REG(INS(I,4))=0.0
            DO 130 J=1,NNCHA
            IF(ABS(CONTEN(IHIST1,J)-CONTEN(IHIST3,J)).GT.EPS)
     -           REG(INS(I,4))=1.0
130         CONTINUE
       ELSEIF(INS(I,2).EQ.12)THEN
            MODREG(INS(I,4))=3
            REG(INS(I,4))=1.0
            DO 140 J=1,NNCHA
            IF(CONTEN(IHIST1,J).GE.CONTEN(IHIST3,J))REG(INS(I,4))=0.0
140         CONTINUE
       ELSEIF(INS(I,2).EQ.13)THEN
            MODREG(INS(I,4))=3
            REG(INS(I,4))=1.0
            DO 150 J=1,NNCHA
            IF(CONTEN(IHIST1,J).GT.CONTEN(IHIST3,J))REG(INS(I,4))=0.0
150         CONTINUE
       ELSEIF(INS(I,2).EQ.14)THEN
            MODREG(INS(I,4))=3
            REG(INS(I,4))=1.0
            DO 160 J=1,NNCHA
            IF(CONTEN(IHIST1,J).LE.CONTEN(IHIST3,J))REG(INS(I,4))=0.0
160         CONTINUE
       ELSEIF(INS(I,2).EQ.15)THEN
            MODREG(INS(I,4))=3
            REG(INS(I,4))=1.0
            DO 170 J=1,NNCHA
            IF(CONTEN(IHIST1,J).LT.CONTEN(IHIST3,J))REG(INS(I,4))=0.0
170         CONTINUE
*   Unidentified operation code.
       ELSE
            MODREG(INS(I,4))=0
            RETURN
       ENDIF
*** Delete auxiliary histograms.
       IF(INS(I,2).NE.6)THEN
            IF(MODREG(INS(I,1)).EQ.2)
     -           CALL HISADM('DELETE',IHIST1,NNCHA,XXMIN,XXMAX,
     -           .FALSE.,IFAIL1)
       ENDIF
       IF(MODREG(INS(I,3)).EQ.2)
     -      CALL HISADM('DELETE',IHIST3,NNCHA,XXMIN,XXMAX,
     -      .FALSE.,IFAIL1)
*** Delete output histogram if not used.
       IF(MODREG(INS(I,4)).NE.4)THEN
            CALL HISADM('DELETE',IHIST4,NNCHA,XXMIN,XXMAX,
     -           .FALSE.,IFAIL1)
       ELSE
*   Make visible if used.
            REG(INS(I,4))=IHIST4
*   And provide the various sums.
            SX0(IHIST4)=0.0
            SX1(IHIST4)=0.0
            SX2(IHIST4)=0.0
            DO 200 J=1,NNCHA
            XX=XXMIN+REAL(J-0.5)*(XXMAX-XXMIN)/REAL(NNCHA)
            SX0(IHIST4)=SX0(IHIST4)+CONTEN(IHIST4,J)
            SX1(IHIST4)=SX1(IHIST4)+CONTEN(IHIST4,J)*XX
            SX2(IHIST4)=SX2(IHIST4)+CONTEN(IHIST4,J)*XX**2
200         CONTINUE
            NENTRY(IHIST4)=1
       ENDIF
*** Reset IFAIL to 0 because the calculations were probably successful.
       IFAIL=0
       END
