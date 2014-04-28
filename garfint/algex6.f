CDECK  ID>, ALGEX6.
       SUBROUTINE ALGEX6(I,IFAIL)
*-----------------------------------------------------------------------
*   ALGEX6 - Routine executing instruction I (produced by ALGPRE).
*            This routine takes care of arithmetic operations between
*            matrices.
*   (Last changed on 17/ 2/12.)
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
       INTEGER INS(MXINS,4),ALGENT(MXALGE,10),MODREG(MXCONS:MXREG),
     -      ISYNCH,IINS0,ICONS0,ARGREF(MXARG,2),MODARG(MXARG),
     -      NREG,NCONS,NINS,NERR,NRES,NALGE,IENTRL,NAERR(100)
       REAL REG(MXCONS:MXREG),ARG(MXARG),EXPMAX
       PARAMETER(EXPMAX=40.0)
       LOGICAL EXEC(MXINS),LIGUND,LINUND
       COMMON /ALGDAT/ REG,ARG,MODARG,ARGREF,INS,MODREG,ALGENT,
     -      NREG,NCONS,NINS,NERR,NAERR,
     -      NRES,NALGE,IENTRL,ISYNCH,IINS0,ICONS0,EXEC,LIGUND,LINUND
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
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
       REAL DENLAN,RNDUNI,RANLAN,RNDEXP,RNDNOR,RNDPOL,RNDFUN,EPS,GAMMA,
     -      ALGAMA,RNGAMA,RNDLAP
       DOUBLE PRECISION SX1,SX2
       INTEGER IFAIL,IFAIL1,IMAT1,IMAT3,IMAT4,IREF1,IREF3,IREF4,I,J,
     -      NDIM,IMOD,IDIM(MXMDIM),MATSLT,NPOIS,IREF,IERR,NC1,NCAUX,NOUT
       CHARACTER*(MXINCH) STR1,AUXSTR
       EXTERNAL RNDUNI,RANLAN,RNDEXP,RNDNOR,RNDPOL,RNDFUN,MATSLT,DENLAN,
     -      GAMMA,ALGAMA,RNGAMA,RNDLAP
*** Set IFAIL to 1 and EPS.
       IFAIL=1
       EPS=1.0E-5
*** For easier reference, define matrix references.
       IREF1=NINT(REG(INS(I,1)))
       IREF3=NINT(REG(INS(I,3)))
       IREF4=NINT(REG(INS(I,4)))
       IMAT1=MATSLT(IREF1)
       IMAT3=MATSLT(IREF3)
       IMAT4=MATSLT(IREF4)
*** Verify that the objects are indeed valid matrices.
       IF(INS(I,2).NE.6.AND.MODREG(INS(I,1)).EQ.5.AND.
     -      MODREG(INS(I,3)).EQ.5)THEN
*   Check that the matrices do indeed exist.
            IF(IMAT1.LE.0.OR.IMAT3.LE.0)THEN
                 MODREG(INS(I,4))=0
                 REG(INS(I,4))=0
                 IF(LDEBUG)THEN
                      PRINT *,' ++++++ ALGEX6 DEBUG   : Reference to'//
     -                     ' an unbooked matrix.'
                      PRINT *,'                         Arg 1: ref=',
     -                     IMAT1,', Arg 3: ref=',IMAT3
                 ENDIF
                 NAERR(61)=NAERR(61)+1
                 RETURN
*   The matrices must have the same overall size.
            ELSEIF(INS(I,2).NE.16.AND.MLEN(IMAT1).NE.MLEN(IMAT3))THEN
                 MODREG(INS(I,4))=0
                 REG(INS(I,4))=0
                 IF(LDEBUG)THEN
                      PRINT *,' ++++++ ALGEX6 DEBUG   : Matrices'//
     -                     ' have differing length.'
                      PRINT *,'                         Arg 1: ref=',
     -                     IMAT1,' length=',MLEN(IMAT1)
                      PRINT *,'                         Arg 3: ref=',
     -                     IMAT3,' length=',MLEN(IMAT3)
                 ENDIF
                 NAERR(62)=NAERR(62)+1
                 RETURN
            ENDIF
*   Validity of reference number.
       ELSEIF(INS(I,2).NE.6.AND.MODREG(INS(I,1)).EQ.5.AND.
     -      IMAT1.LE.0)THEN
            MODREG(INS(I,4))=0
            REG(INS(I,4))=0
            IF(LDEBUG)PRINT *,' ++++++ ALGEX6 DEBUG   :'//
     -           ' Refering to unbooked matrix ',IMAT1
            NAERR(61)=NAERR(61)+1
            RETURN
       ELSEIF(MODREG(INS(I,3)).EQ.5.AND.IMAT3.LE.0)THEN
            MODREG(INS(I,4))=0
            REG(INS(I,4))=0
            IF(LDEBUG)PRINT *,' ++++++ ALGEX6 DEBUG   :'//
     -           ' Refering to unbooked matrix ',IMAT3
            NAERR(61)=NAERR(61)+1
            RETURN
*   Check nothing else than numbers and matrices appear.
       ELSEIF((INS(I,2).EQ.6.AND.MODREG(INS(I,3)).NE.5).OR.
     -      (INS(I,2).NE.6.AND.((MODREG(INS(I,1)).NE.2.AND.
     -      MODREG(INS(I,1)).NE.5).OR.(MODREG(INS(I,3)).NE.2.AND.
     -      MODREG(INS(I,3)).NE.5))))THEN
            MODREG(INS(I,4))=0
            REG(INS(I,4))=0
            IF(LDEBUG)THEN
                 PRINT *,' ++++++ ALGEX6 DEBUG   : Unable to'//
     -                ' handle received modes'
                 PRINT *,'                         Arg 1: ref=',
     -                IMAT1,' mode=',MODREG(INS(I,1))
                 PRINT *,'                         Arg 3: ref=',
     -                IMAT3,' mode=',MODREG(INS(I,3))
            ENDIF
            RETURN
       ENDIF
*** Set parameters of resulting matrix: function calls.
       IF(INS(I,2).EQ.6)THEN
            IF(MODREG(INS(I,3)).EQ.5)THEN
                 DO 340 J=1,MDIM(IMAT3)
                 IDIM(J)=MSIZ(IMAT3,J)
340              CONTINUE
                 NDIM=MDIM(IMAT3)
                 IMOD=MMOD(IMAT3)
            ELSE
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ ALGEX6 DEBUG  :'',
     -                '' Unable to get output matrix format.'')')
                 RETURN
            ENDIF
*   Concatenation.
       ELSEIF(INS(I,2).EQ.16)THEN
            IF(MODREG(INS(I,1)).EQ.5.AND.MODREG(INS(I,3)).EQ.5)THEN
                 NDIM=1
                 IDIM(1)=MLEN(IMAT1)+MLEN(IMAT3)
                 IMOD=MMOD(IMAT1)
            ELSEIF(MODREG(INS(I,1)).EQ.5)THEN
                 NDIM=1
                 IDIM(1)=MLEN(IMAT1)+1
                 IMOD=MMOD(IMAT1)
            ELSEIF(MODREG(INS(I,3)).EQ.5)THEN
                 NDIM=1
                 IDIM(1)=MLEN(IMAT3)+1
                 IMOD=MMOD(IMAT3)
            ELSE
                 NDIM=1
                 IDIM(1)=2
                 IMOD=2
            ENDIF
*   Numeric calls.
       ELSE
            IF(MODREG(INS(I,1)).EQ.5)THEN
                 DO 350 J=1,MDIM(IMAT1)
                 IDIM(J)=MSIZ(IMAT1,J)
350              CONTINUE
                 NDIM=MDIM(IMAT1)
                 IMOD=MMOD(IMAT1)
            ELSEIF(MODREG(INS(I,3)).EQ.5)THEN
                 DO 360 J=1,MDIM(IMAT3)
                 IDIM(J)=MSIZ(IMAT3,J)
360              CONTINUE
                 NDIM=MDIM(IMAT3)
                 IMOD=MMOD(IMAT3)
            ELSE
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ ALGEX6 DEBUG  :'',
     -                '' Unable to get output matrix format.'')')
                 RETURN
            ENDIF
       ENDIF
*** If one of the arguments is scalar, turn into a matrix.
       IF(INS(I,2).NE.6.AND.INS(I,2).NE.16.AND.
     -      MODREG(INS(I,1)).EQ.2)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ ALGEX6 DEBUG   :'',
     -           '' Creating a replacement matrix for INS(I,1).'')')
            CALL MATADM('ALLOCATE',IREF1,NDIM,IDIM,IMOD,IFAIL1)
            IF(IFAIL1.NE.0)RETURN
            IMAT1=MATSLT(IREF1)
            IF(IMAT1.LE.0)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ ALGEX6'',
     -                '' DEBUG   : Unable to locate scalar'',
     -                '' replacement matrix 1.'')')
                 RETURN
            ENDIF
            DO 380 J=1,MLEN(IMAT1)
            MVEC(MORG(IMAT1)+J)=REG(INS(I,1))
380         CONTINUE
       ENDIF
       IF(INS(I,2).NE.16.AND.MODREG(INS(I,3)).EQ.2)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ ALGEX6 DEBUG   :'',
     -           '' Creating a replacement matrix for INS(I,3).'')')
            CALL MATADM('ALLOCATE',IREF3,NDIM,IDIM,IMOD,IFAIL1)
            IF(IFAIL1.NE.0)RETURN
            IMAT3=MATSLT(IREF3)
            IF(IMAT3.LE.0)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ ALGEX6'',
     -                '' DEBUG   : Unable to locate scalar'',
     -                '' replacement matrix 3.'')')
                 RETURN
            ENDIF
            DO 400 J=1,MLEN(IMAT3)
            MVEC(MORG(IMAT3)+J)=REG(INS(I,3))
400         CONTINUE
       ENDIF
*** Allocate a matrix for the result.
       CALL MATADM('ALLOCATE',IREF4,NDIM,IDIM,IMOD,IFAIL1)
       IF(IFAIL1.NE.0)RETURN
       REG(INS(I,4))=IREF4
       MODREG(INS(I,4))=5
*** Establish final locations for the various matrices, first word.
       IF(INS(I,2).NE.6.AND.
     -      (INS(I,2).NE.16.OR.MODREG(INS(I,1)).EQ.5))THEN
            IMAT1=MATSLT(IREF1)
            IF(IMAT1.LE.0)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ ALGEX6'',
     -                '' DEBUG   : Unable to locate scalar'',
     -                '' replacement matrix 1.'')')
                 RETURN
            ENDIF
       ENDIF
*   Third word.
       IF(INS(I,2).NE.16.OR.MODREG(INS(I,3)).EQ.5)THEN
            IMAT3=MATSLT(IREF3)
            IF(IMAT3.LE.0)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ ALGEX6'',
     -                '' DEBUG   : Unable to locate scalar'',
     -                '' replacement matrix 3.'')')
                 RETURN
            ENDIF
       ENDIF
*   Result.
       IMAT4=MATSLT(IREF4)
       IF(IMAT4.LE.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ ALGEX6'',
     -           '' DEBUG   : Unable to locate scalar'',
     -           '' replacement result matrix.'')')
            RETURN
       ENDIF
*** Perform the actual calculation: binary numerical operators.
       IF(INS(I,2).EQ.1)THEN
            DO 10 J=1,MLEN(IMAT4)
            MVEC(MORG(IMAT4)+J)=MVEC(MORG(IMAT1)+J)+MVEC(MORG(IMAT3)+J)
10          CONTINUE
            MODREG(INS(I,4))=5
       ELSEIF(INS(I,2).EQ.2)THEN
            DO 20 J=1,MLEN(IMAT4)
            MVEC(MORG(IMAT4)+J)=MVEC(MORG(IMAT1)+J)-MVEC(MORG(IMAT3)+J)
20          CONTINUE
            MODREG(INS(I,4))=5
       ELSEIF(INS(I,2).EQ.3)THEN
            DO 30 J=1,MLEN(IMAT4)
            MVEC(MORG(IMAT4)+J)=MVEC(MORG(IMAT1)+J)*MVEC(MORG(IMAT3)+J)
30          CONTINUE
            MODREG(INS(I,4))=5
       ELSEIF(INS(I,2).EQ.4)THEN
            DO 40 J=1,MLEN(IMAT4)
            IF(MVEC(MORG(IMAT3)+J).NE.0)THEN
                 MVEC(MORG(IMAT4)+J)=MVEC(MORG(IMAT1)+J)/
     -                MVEC(MORG(IMAT3)+J)
            ELSE
                 MVEC(MORG(IMAT4)+J)=0.0
            ENDIF
40          CONTINUE
            MODREG(INS(I,4))=5
       ELSEIF(INS(I,2).EQ.5)THEN
            DO 50 J=1,MLEN(IMAT4)
            IF(ABS(MVEC(MORG(IMAT3)+J)-
     -           NINT(MVEC(MORG(IMAT3)+J))).LT.EPS)THEN
                 IF(NINT(MVEC(MORG(IMAT3)+J)).LE.0.AND.
     -                MVEC(MORG(IMAT1)+J).EQ.0)THEN
                      MVEC(MORG(IMAT4)+J)=0.0
                 ELSEIF(2*(NINT(MVEC(MORG(IMAT3)+J))/2).EQ.
     -                NINT(MVEC(MORG(IMAT3)+J)))THEN
                      MVEC(MORG(IMAT4)+J)=ABS(MVEC(MORG(IMAT1)+J))**
     -                     NINT(MVEC(MORG(IMAT3)+J))
                 ELSE
                      MVEC(MORG(IMAT4)+J)=
     -                     SIGN(ABS(MVEC(MORG(IMAT1)+J))**
     -                     NINT(MVEC(MORG(IMAT3)+J)),
     -                     MVEC(MORG(IMAT1)+J))
                 ENDIF
            ELSEIF(MVEC(MORG(IMAT1)+J).EQ.0.AND.
     -           MVEC(MORG(IMAT3)+J).GT.0)THEN
                 MVEC(MORG(IMAT4)+J)=0
            ELSEIF(MVEC(MORG(IMAT1)+J).GT.0)THEN
                 MVEC(MORG(IMAT4)+J)=MVEC(MORG(IMAT1)+J)**
     -                MVEC(MORG(IMAT3)+J)
            ELSE
                 MVEC(MORG(IMAT4)+J)=0.0
                 NAERR(11)=NAERR(11)+1
            ENDIF
50          CONTINUE
            MODREG(INS(I,4))=5
*   Numerical function calls.
       ELSEIF(INS(I,2).EQ.6)THEN
            MODREG(INS(I,4))=5
            DO 60 J=1,MLEN(IMAT4)
            IF(INS(I,1).EQ. 1)THEN
                 IF(MVEC(MORG(IMAT3)+J).GT.EXPMAX)THEN
                      NAERR(2)=NAERR(2)+1
                      MVEC(MORG(IMAT4)+J)=EXP(EXPMAX)
                      RETURN
                 ELSEIF(MVEC(MORG(IMAT3)+J).LT.-EXPMAX)THEN
                      IF(LIGUND)THEN
                           MVEC(MORG(IMAT4)+J)=0
                      ELSE
                           NAERR(3)=NAERR(3)+1
                           RETURN
                      ENDIF
                 ELSE
                      MVEC(MORG(IMAT4)+J)=EXP(MVEC(MORG(IMAT3)+J))
                 ENDIF
            ELSEIF(INS(I,1).EQ.-1)THEN
                 IF(MVEC(MORG(IMAT3)+J).LE.0.0)RETURN
                 MVEC(MORG(IMAT4)+J)=LOG(MVEC(MORG(IMAT3)+J))
            ENDIF
            IF((INS(I,1).EQ.-2.OR.INS(I,1).EQ.-3).AND.
     -           ABS(MVEC(MORG(IMAT3)+J)).GT.1.0)THEN
                 MVEC(MORG(IMAT4)+J)=0.0
            ELSE
                 IF(INS(I,1).EQ.-2)MVEC(MORG(IMAT4)+J)=
     -                ASIN(MVEC(MORG(IMAT3)+J))
                 IF(INS(I,1).EQ.-3)MVEC(MORG(IMAT4)+J)=
     -                ACOS(MVEC(MORG(IMAT3)+J))
            ENDIF
            IF(INS(I,1).EQ. 2)MVEC(MORG(IMAT4)+J)=
     -           SIN(MVEC(MORG(IMAT3)+J))
            IF(INS(I,1).EQ. 3)MVEC(MORG(IMAT4)+J)=
     -           COS(MVEC(MORG(IMAT3)+J))
            IF(INS(I,1).EQ. 4)MVEC(MORG(IMAT4)+J)=
     -           TAN(MVEC(MORG(IMAT3)+J))
            IF(INS(I,1).EQ.-4)MVEC(MORG(IMAT4)+J)=
     -           ATAN(MVEC(MORG(IMAT3)+J))
            IF(INS(I,1).EQ. 5)MVEC(MORG(IMAT4)+J)=
     -           ABS(MVEC(MORG(IMAT3)+J))
            IF(INS(I,1).EQ.-5)THEN
                 IF(MVEC(MORG(IMAT3)+J).LT.0.0)THEN
                      MVEC(MORG(IMAT4)+J)=-1.0
                 ELSE
                      MVEC(MORG(IMAT4)+J)=SQRT(MVEC(MORG(IMAT3)+J))
                 ENDIF
            ENDIF
            IF(INS(I,1).EQ. 6)MVEC(MORG(IMAT4)+J)=
     -           MVEC(MORG(IMAT3)+J)
            IF(INS(I,1).EQ.-6)MVEC(MORG(IMAT4)+J)=
     -           -MVEC(MORG(IMAT3)+J)
            IF(INS(I,1).EQ. 7)MVEC(MORG(IMAT4)+J)=
     -           SINH(MVEC(MORG(IMAT3)+J))
            IF(INS(I,1).EQ.-7)MVEC(MORG(IMAT4)+J)=
     -           LOG(MVEC(MORG(IMAT3)+J)+
     -           SQRT(1+MVEC(MORG(IMAT3)+J)**2))
            IF(INS(I,1).EQ. 8)MVEC(MORG(IMAT4)+J)=
     -           COSH(MVEC(MORG(IMAT3)+J))
            IF(INS(I,1).EQ.-8)THEN
                 IF(MVEC(MORG(IMAT3)+J).LT.1)THEN
                      MVEC(MORG(IMAT4)+J)=0.0
                 ELSE
                      MVEC(MORG(IMAT4)+J)=LOG(MVEC(MORG(IMAT3)+J)+
     -                     SQRT(MVEC(MORG(IMAT3)+J)**2-1))
                 ENDIF
            ENDIF
            IF(INS(I,1).EQ. 9)MVEC(MORG(IMAT4)+J)=
     -           TANH(MVEC(MORG(IMAT3)+J))
            IF(INS(I,1).EQ.-9)THEN
                 IF(MVEC(MORG(IMAT3)+J).LE.-1.0.OR.
     -                MVEC(MORG(IMAT3)+J).GE.1.0)THEN
                      MVEC(MORG(IMAT4)+J)=0.0
                 ELSE
                      MVEC(MORG(IMAT4)+J)=
     -                     0.5*LOG((1+MVEC(MORG(IMAT3)+J))/
     -                     (1-MVEC(MORG(IMAT3)+J)))
                 ENDIF
            ENDIF
*   Truncation of a real number.
            IF(INS(I,1).EQ.11)THEN
                 MVEC(MORG(IMAT4)+J)=INT(MVEC(MORG(IMAT3)+J))
                 IF(MVEC(MORG(IMAT3)+J).LT.0)MVEC(MORG(IMAT4)+J)=
     -                MVEC(MORG(IMAT4)+J)-1.0
            ELSEIF(INS(I,1).EQ.-11)THEN
                 MVEC(MORG(IMAT4)+J)=
     -                MVEC(MORG(IMAT3)+J)-INT(MVEC(MORG(IMAT3)+J))
                 IF(MVEC(MORG(IMAT3)+J).LT.0)MVEC(MORG(IMAT4)+J)=
     -                MVEC(MORG(IMAT4)+J)+1.0
            ENDIF
*   Landau density.
            IF(INS(I,1).EQ.18)MVEC(MORG(IMAT4)+J)=
     -           DENLAN(MVEC(MORG(IMAT3)+J))
*   Gamma function.
            IF(INS(I,1).EQ.47)THEN
                 IF(MVEC(MORG(IMAT3)+J).LT.1E-20)THEN
                      NAERR(12)=NAERR(12)+1
                      MVEC(MORG(IMAT4)+J)=0.0
                 ELSEIF(MVEC(MORG(IMAT3)+J).GT.25.0)THEN
                      NAERR(13)=NAERR(13)+1
                      MVEC(MORG(IMAT4)+J)=0.0
                 ELSE
                      MVEC(MORG(IMAT4)+J)=GAMMA(MVEC(MORG(IMAT3)+J))
                 ENDIF
*   log-Gamma function.
            ELSEIF(INS(I,1).EQ.48)THEN
                 IF(MVEC(MORG(IMAT3)+J).LE.0)THEN
                      NAERR(14)=NAERR(14)+1
                      MVEC(MORG(IMAT4)+J)=0.0
                 ELSE
                      MVEC(MORG(IMAT4)+J)=ALGAMA(MVEC(MORG(IMAT3)+J))
                 ENDIF
            ENDIF
60          CONTINUE
*   Make a string from a matrix.
            IF(INS(I,1).EQ.12)THEN
                 CALL OUTFMT(REG(INS(I,3)),MODREG(INS(I,3)),
     -                AUXSTR,NCAUX,'LEFT')
                 CALL STRBUF('STORE',IREF,AUXSTR(1:NCAUX),NCAUX,IFAIL)
                 IF(IFAIL.NE.0)RETURN
                 MODREG(INS(I,4))=1
                 REG(INS(I,4))=IREF
*   Return the real number of the matrix.
            ELSEIF(INS(I,1).EQ.-12)THEN
                 REG(INS(I,4))=MVEC(MORG(IMAT3)+1)
                 MODREG(INS(I,4))=2
*   Sum and product.
            ELSEIF(INS(I,1).EQ.13)THEN
                 REG(INS(I,4))=0
                 MODREG(INS(I,4))=2
                 DO 90 J=1,MLEN(IMAT4)
                 REG(INS(I,4))=REG(INS(I,4))+MVEC(MORG(IMAT3)+J)
90               CONTINUE
            ELSEIF(INS(I,1).EQ.14)THEN
                 REG(INS(I,4))=1
                 MODREG(INS(I,4))=2
                 DO 100 J=1,MLEN(IMAT4)
                 REG(INS(I,4))=REG(INS(I,4))*MVEC(MORG(IMAT3)+J)
100              CONTINUE
*   Maximum and minimum.
            ELSEIF(INS(I,1).EQ.19)THEN
                 REG(INS(I,4))=MVEC(MORG(IMAT3)+1)
                 MODREG(INS(I,4))=2
                 DO 180 J=2,MLEN(IMAT3)
                 REG(INS(I,4))=MIN(REG(INS(I,4)),MVEC(MORG(IMAT3)+J))
180              CONTINUE
            ELSEIF(INS(I,1).EQ.20)THEN
                 REG(INS(I,4))=MVEC(MORG(IMAT3)+1)
                 MODREG(INS(I,4))=2
                 DO 190 J=2,MLEN(IMAT3)
                 REG(INS(I,4))=MAX(REG(INS(I,4)),MVEC(MORG(IMAT3)+J))
190              CONTINUE
*   Mean and RMS.
            ELSEIF(INS(I,1).EQ.41.OR.INS(I,1).EQ.42)THEN
                 SX1=0
                 SX2=0
                 DO 200 J=1,MLEN(IMAT3)
                 SX1=SX1+MVEC(MORG(IMAT3)+J)
                 SX2=SX2+MVEC(MORG(IMAT3)+J)**2
200              CONTINUE
                 IF(MLEN(IMAT3).LT.1)RETURN
                 IF(INS(I,1).EQ.41)THEN
                      REG(INS(I,4))=SX1/MLEN(IMAT3)
                 ELSE
                      REG(INS(I,4))=SQRT(MAX(0.0D0,
     -                     (SX2-SX1**2/MLEN(IMAT3))/MLEN(IMAT3)))
                 ENDIF
                 MODREG(INS(I,4))=2
*   Overall size of a matrix.
            ELSEIF(INS(I,1).EQ.43)THEN
                 REG(INS(I,4))=MLEN(IMAT3)
                 MODREG(INS(I,4))=2
*   Return the reference of the matrix.
            ELSEIF(INS(I,1).EQ.15)THEN
                 REG(INS(I,4))=IMAT3
                 MODREG(INS(I,4))=2
*   Locate a global variable from its name.
            ELSEIF(INS(I,1).EQ.16)THEN
                 CALL STRBUF('READ',NINT(REG(INS(I,3))),STR1,NC1,IFAIL1)
                 DO 70 J=1,NGLB
                 IF(STR1(1:NC1).EQ.GLBVAR(J))THEN
                      MODREG(INS(I,4))=GLBMOD(J)
                      REG(INS(I,4))=GLBVAL(J)
                      GOTO 75
                 ENDIF
70               CONTINUE
                 MODREG(INS(I,4))=0
                 REG(INS(I,4))=0
75               CONTINUE
*   Return the type of the argument.
            ELSEIF(INS(I,1).EQ.17)THEN
                 CALL STRBUF('STORE',IREF,'Matrix',6,IFAIL1)
                 IF(IFAIL1.NE.0)RETURN
                 REG(INS(I,4))=IREF
                 MODREG(INS(I,4))=1
*   Reverse a matrix.
            ELSEIF(INS(I,1).EQ.49)THEN
                 DO 76 J=1,MLEN(IMAT3)
                 MVEC(MORG(IMAT4)+J)=MVEC(MORG(IMAT3)+MLEN(IMAT3)-J+1)
76               CONTINUE
            ENDIF
*   Random number generators.
            DO 110 J=1,MLEN(IMAT4)
            IF(INS(I,1).EQ.21)THEN
                 MVEC(MORG(IMAT4)+J)=RNDUNI(REG(INS(I,3)))
            ELSEIF(INS(I,1).EQ.22)THEN
                 MVEC(MORG(IMAT4)+J)=RNDNOR(0.0,1.0)
            ELSEIF(INS(I,1).EQ.23)THEN
                 MVEC(MORG(IMAT4)+J)=RNDEXP(MVEC(MORG(IMAT3)+J))
            ELSEIF(INS(I,1).EQ.24)THEN
                 CALL RNPSSN(MVEC(MORG(IMAT3)+J),NPOIS,IERR)
                 MVEC(MORG(IMAT4)+J)=REAL(NPOIS)
            ELSEIF(INS(I,1).EQ.25)THEN
                 MVEC(MORG(IMAT4)+J)=RANLAN(RNDUNI(1.0))
            ELSEIF(INS(I,1).EQ.26)THEN
                 MVEC(MORG(IMAT4)+J)=RNDPOL(MVEC(MORG(IMAT3)+J))
            ELSEIF(INS(I,1).EQ.27)THEN
                 MVEC(MORG(IMAT4)+J)=RNDFUN(MVEC(MORG(IMAT3)+J))
            ELSEIF(INS(I,1).EQ.29)THEN
                 IF(MVEC(MORG(IMAT3)+J).GT.0)THEN
                      MVEC(MORG(IMAT4)+J)=RNGAMA(MVEC(MORG(IMAT3)+J))
                 ELSE
                      NAERR(14)=NAERR(14)+1
                      MVEC(MORG(IMAT4)+J)=0
                 ENDIF
            ELSEIF(INS(I,1).EQ.30)THEN
                 MVEC(MORG(IMAT4)+J)=RNDLAP(MVEC(MORG(IMAT3)+J))
            ENDIF
110         CONTINUE
*   Random number generators not to be called.
            IF(INS(I,1).EQ.28)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ ALGEX6 DEBUG   :'',
     -                '' Generator '',I2,'' does not apply to'',
     -                '' Matrix.'')') INS(I,1)
                 RETURN
            ENDIF
*   Binary logical operators between real type arguments.
       ELSEIF(INS(I,2).EQ.10)THEN
            MODREG(INS(I,4))=3
            REG(INS(I,4))=1.0
            DO 120 J=1,MLEN(IMAT4)
            IF(ABS(MVEC(MORG(IMAT1)+J)-MVEC(MORG(IMAT3)+J)).GT.EPS)
     -           REG(INS(I,4))=0.0
120         CONTINUE
       ELSEIF(INS(I,2).EQ.11)THEN
            MODREG(INS(I,4))=3
            REG(INS(I,4))=0.0
            DO 130 J=1,MLEN(IMAT4)
            IF(ABS(MVEC(MORG(IMAT1)+J)-MVEC(MORG(IMAT3)+J)).GT.EPS)
     -           REG(INS(I,4))=1.0
130         CONTINUE
       ELSEIF(INS(I,2).EQ.12)THEN
            MODREG(INS(I,4))=3
            REG(INS(I,4))=1.0
            DO 140 J=1,MLEN(IMAT4)
            IF(MVEC(MORG(IMAT1)+J).GE.MVEC(MORG(IMAT3)+J))
     -           REG(INS(I,4))=0.0
140         CONTINUE
       ELSEIF(INS(I,2).EQ.13)THEN
            MODREG(INS(I,4))=3
            REG(INS(I,4))=1.0
            DO 150 J=1,MLEN(IMAT4)
            IF(MVEC(MORG(IMAT1)+J).GT.MVEC(MORG(IMAT3)+J))
     -           REG(INS(I,4))=0.0
150         CONTINUE
       ELSEIF(INS(I,2).EQ.14)THEN
            MODREG(INS(I,4))=3
            REG(INS(I,4))=1.0
            DO 160 J=1,MLEN(IMAT4)
            IF(MVEC(MORG(IMAT1)+J).LE.MVEC(MORG(IMAT3)+J))
     -           REG(INS(I,4))=0.0
160         CONTINUE
       ELSEIF(INS(I,2).EQ.15)THEN
            MODREG(INS(I,4))=3
            REG(INS(I,4))=1.0
            DO 170 J=1,MLEN(IMAT4)
            IF(MVEC(MORG(IMAT1)+J).LT.MVEC(MORG(IMAT3)+J))
     -           REG(INS(I,4))=0.0
170         CONTINUE
*   Concatenation.
       ELSEIF(INS(I,2).EQ.16)THEN
            NOUT=0
            IF(MODREG(INS(I,1)).EQ.2)THEN
                 NOUT=NOUT+1
                 MVEC(MORG(IMAT4)+NOUT)=REG(INS(I,1))
            ELSE
                 DO 210 J=1,MLEN(IMAT1)
                 NOUT=NOUT+1
                 MVEC(MORG(IMAT4)+NOUT)=MVEC(MORG(IMAT1)+J)
210              CONTINUE
            ENDIF
            IF(MODREG(INS(I,3)).EQ.2)THEN
                 NOUT=NOUT+1
                 MVEC(MORG(IMAT4)+NOUT)=REG(INS(I,3))
            ELSE
                 DO 220 J=1,MLEN(IMAT3)
                 NOUT=NOUT+1
                 MVEC(MORG(IMAT4)+NOUT)=MVEC(MORG(IMAT3)+J)
220              CONTINUE
            ENDIF
            MODREG(INS(I,4))=5
*   Unidentified operation code.
       ELSE
            MODREG(INS(I,4))=0
            RETURN
       ENDIF
*** Delete auxiliary matrices.
       IF(INS(I,2).NE.6.AND.INS(I,2).NE.16)THEN
            IF(MODREG(INS(I,1)).EQ.2)
     -           CALL MATADM('DELETE',IREF1,NDIM,IDIM,IMOD,IFAIL1)
       ENDIF
       IF(MODREG(INS(I,3)).EQ.2.AND.INS(I,2).NE.16)
     -      CALL MATADM('DELETE',IREF3,NDIM,IDIM,IMOD,IFAIL1)
*** Delete output matrix if not used.
       IF(MODREG(INS(I,4)).NE.5)
     -      CALL MATADM('DELETE',IREF4,NDIM,IDIM,IMOD,IFAIL1)
*** Reset IFAIL to 0 because the calculations were probably successful.
       IFAIL=0
       END
