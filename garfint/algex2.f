CDECK  ID>, ALGEX2.
       SUBROUTINE ALGEX2(I,IFAIL)
*-----------------------------------------------------------------------
*   ALGEX2 - Routine executing instruction I (produced by ALGPRE).
*            This routine takes care of arithmetic operations between
*            reals (and for the time being also of logicals).
*   (Last changed on  9/11/07.)
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
       EXTERNAL RNDUNI,RANLAN,RNDEXP,RNDNOR,RNDPOL,RNDFUN,DENLAN,RNDLAP
       REAL RNDUNI,RANLAN,RNDEXP,RNDNOR,RNDPOL,RNDFUN,DENLAN,EPS,GAMMA,
     -      ALGAMA,RNGAMA,RNDLAP
       INTEGER I,J,IFAIL,IFAIL1,NPOIS,IERR,NCAUX,IREF,ISIZ(1),ISLOT,
     -      MATSLT
       CHARACTER*20 AUXSTR
       EXTERNAL MATSLT,GAMMA,ALGAMA,RNGAMA
*** Set IFAIL to 1 and EPS.
       IFAIL=1
       EPS=1.0E-5
*** Initial value is zero for any result.
       REG(INS(I,4))=0.0
*** Perform the actual calculation: binary numerical operators.
       IF(INS(I,2).EQ.1)THEN
            REG(INS(I,4))=REG(INS(I,1))+REG(INS(I,3))
            MODREG(INS(I,4))=2
       ELSEIF(INS(I,2).EQ.2)THEN
            REG(INS(I,4))=REG(INS(I,1))-REG(INS(I,3))
            MODREG(INS(I,4))=2
       ELSEIF(INS(I,2).EQ.3)THEN
            REG(INS(I,4))=REG(INS(I,1))*REG(INS(I,3))
            MODREG(INS(I,4))=2
       ELSEIF(INS(I,2).EQ.4)THEN
            MODREG(INS(I,4))=2
            IF(REG(INS(I,3)).EQ.0.0)THEN
                 NAERR(1)=NAERR(1)+1
                 RETURN
            ENDIF
            REG(INS(I,4))=REG(INS(I,1))/REG(INS(I,3))
       ELSEIF(INS(I,2).EQ.5)THEN
            MODREG(INS(I,4))=2
            IF(ABS(REG(INS(I,3))-NINT(REG(INS(I,3)))).LT.EPS)THEN
                 IF(NINT(REG(INS(I,3))).LE.0.AND.REG(INS(I,1)).EQ.0)THEN
                      RETURN
                 ELSEIF(2*(NINT(REG(INS(I,3)))/2).EQ.
     -                NINT(REG(INS(I,3))))THEN
                      REG(INS(I,4))=ABS(REG(INS(I,1)))**
     -                     NINT(REG(INS(I,3)))
                 ELSE
                      REG(INS(I,4))=SIGN(ABS(REG(INS(I,1)))**
     -                     NINT(REG(INS(I,3))),REG(INS(I,1)))
                 ENDIF
            ELSEIF(REG(INS(I,1)).EQ.0.AND.REG(INS(I,3)).GT.0)THEN
                 REG(INS(I,4))=0
            ELSEIF(REG(INS(I,1)).GT.0)THEN
                 REG(INS(I,4))=REG(INS(I,1))**REG(INS(I,3))
            ELSE
                 NAERR(11)=NAERR(11)+1
                 RETURN
            ENDIF
*** Numerical function calls.
       ELSEIF(INS(I,2).EQ.6)THEN
*   Exponential and log.
            MODREG(INS(I,4))=2
            IF(INS(I,1).EQ. 1)THEN
                 IF(REG(INS(I,3)).GT.EXPMAX)THEN
                      NAERR(2)=NAERR(2)+1
                      REG(INS(I,4))=EXP(EXPMAX)
                      RETURN
                 ELSEIF(REG(INS(I,3)).LT.-EXPMAX)THEN
                      IF(LIGUND)THEN
                           REG(INS(I,4))=0
                      ELSE
                           NAERR(3)=NAERR(3)+1
                           RETURN
                      ENDIF
                 ELSE
                      REG(INS(I,4))=EXP(REG(INS(I,3)))
                 ENDIF
            ELSEIF(INS(I,1).EQ.-1)THEN
                 IF(REG(INS(I,3)).LE.0.0)THEN
                      NAERR(4)=NAERR(4)+1
                      RETURN
                 ENDIF
                 REG(INS(I,4))=LOG(REG(INS(I,3)))
*   Trigonometric.
            ELSEIF(INS(I,1).EQ. 2)THEN
                 REG(INS(I,4))=  SIN(REG(INS(I,3)))
            ELSEIF(INS(I,1).EQ.-2)THEN
                 IF(ABS(REG(INS(I,3))).GT.1.0)THEN
                      NAERR(5)=NAERR(5)+1
                      RETURN
                 ENDIF
                 REG(INS(I,4))= ASIN(REG(INS(I,3)))
            ELSEIF(INS(I,1).EQ. 3)THEN
                 REG(INS(I,4))=  COS(REG(INS(I,3)))
            ELSEIF(INS(I,1).EQ.-3)THEN
                 IF(ABS(REG(INS(I,3))).GT.1.0)THEN
                      NAERR(5)=NAERR(5)+1
                      RETURN
                 ENDIF
                 REG(INS(I,4))= ACOS(REG(INS(I,3)))
            ELSEIF(INS(I,1).EQ. 4)THEN
                 REG(INS(I,4))=  TAN(REG(INS(I,3)))
            ELSEIF(INS(I,1).EQ.-4)THEN
                 REG(INS(I,4))= ATAN(REG(INS(I,3)))
*   Absolute value.
            ELSEIF(INS(I,1).EQ. 5)THEN
                 REG(INS(I,4))=  ABS(REG(INS(I,3)))
*   Square root.
            ELSEIF(INS(I,1).EQ.-5)THEN
                 IF(REG(INS(I,3)).LT.0.0)THEN
                      NAERR(6)=NAERR(6)+1
                      RETURN
                 ENDIF
                 REG(INS(I,4))=SQRT(REG(INS(I,3)))
*   Assignments and negatives.
            ELSEIF(INS(I,1).EQ. 6)THEN
                 REG(INS(I,4))=      REG(INS(I,3))
            ELSEIF(INS(I,1).EQ.-6)THEN
                 REG(INS(I,4))=     -REG(INS(I,3))
*   Hyperbolic trigonometry.
            ELSEIF(INS(I,1).EQ. 7)THEN
                 REG(INS(I,4))= SINH(REG(INS(I,3)))
            ELSEIF(INS(I,1).EQ.-7)THEN
                 REG(INS(I,4))=LOG(REG(INS(I,3))+
     -                SQRT(1+REG(INS(I,3))**2))
            ELSEIF(INS(I,1).EQ. 8)THEN
                 REG(INS(I,4))= COSH(REG(INS(I,3)))
            ELSEIF(INS(I,1).EQ.-8)THEN
                 IF(REG(INS(I,3)).LT.1)THEN
                      NAERR(7)=NAERR(7)+1
                      RETURN
                 ENDIF
                 REG(INS(I,4))=LOG(REG(INS(I,3))+
     -                SQRT(REG(INS(I,3))**2-1))
            ELSEIF(INS(I,1).EQ. 9)THEN
                 REG(INS(I,4))= TANH(REG(INS(I,3)))
            ELSEIF(INS(I,1).EQ.-9)THEN
                 IF(REG(INS(I,3)).LE.-1.0.OR.REG(INS(I,3)).GE.1.0)THEN
                      NAERR(8)=NAERR(8)+1
                      RETURN
                 ENDIF
                 REG(INS(I,4))=0.5*LOG((1+REG(INS(I,3)))/
     -                (1-REG(INS(I,3))))
*   Landau distribution.
            ELSEIF(INS(I,1).EQ.18)THEN
                 REG(INS(I,4))=DENLAN(REG(INS(I,3)))
*   Make a string from a number.
            ELSEIF(INS(I,1).EQ.12)THEN
                 CALL OUTFMT(REG(INS(I,3)),MODREG(INS(I,3)),
     -                AUXSTR,NCAUX,'LEFT')
                 CALL STRBUF('STORE',IREF,AUXSTR(1:NCAUX),NCAUX,IFAIL)
                 IF(IFAIL.NE.0)THEN
                      NAERR(9)=NAERR(9)+1
                      RETURN
                 ENDIF
                 MODREG(INS(I,4))=1
                 REG(INS(I,4))=IREF
*   Return the type of the argument.
            ELSEIF(INS(I,1).EQ.17)THEN
                 CALL STRBUF('STORE',IREF,'Number',6,IFAIL)
                 IF(IFAIL.NE.0)THEN
                      NAERR(9)=NAERR(9)+1
                      RETURN
                 ENDIF
                 REG(INS(I,4))=IREF
                 MODREG(INS(I,4))=1
*   Make a number from a number.
            ELSEIF(INS(I,1).EQ.-12)THEN
                 REG(INS(I,4))=REG(INS(I,3))
                 MODREG(INS(I,4))=2
*   Truncation of a real number.
            ELSEIF(INS(I,1).EQ.11)THEN
                 REG(INS(I,4))=INT(REG(INS(I,3)))
                 IF(REG(INS(I,3)).LT.0)REG(INS(I,4))=REG(INS(I,4))-1.0
            ELSEIF(INS(I,1).EQ.-11)THEN
                 REG(INS(I,4))=REG(INS(I,3))-INT(REG(INS(I,3)))
                 IF(REG(INS(I,3)).LT.0)REG(INS(I,4))=REG(INS(I,4))+1.0
*   Return strings by reference.
            ELSEIF(INS(I,1).EQ.51)THEN
                 REG(INS(I,4))=REG(INS(I,3))
                 MODREG(INS(I,4))=1
*   Return histograms by reference.
            ELSEIF(INS(I,1).EQ.54)THEN
                 REG(INS(I,4))=REG(INS(I,3))
                 MODREG(INS(I,4))=4
*   Return matrices by reference.
            ELSEIF(INS(I,1).EQ.55)THEN
                 REG(INS(I,4))=REG(INS(I,3))
                 MODREG(INS(I,4))=5
*   Random number generators.
            ELSEIF(INS(I,1).EQ.21)THEN
                 REG(INS(I,4))=RNDUNI(REG(INS(I,3)))
            ELSEIF(INS(I,1).EQ.22)THEN
                 REG(INS(I,4))=RNDNOR(0.0,1.0)
            ELSEIF(INS(I,1).EQ.23)THEN
                 REG(INS(I,4))=RNDEXP(REG(INS(I,3)))
            ELSEIF(INS(I,1).EQ.24)THEN
                 CALL RNPSSN(REG(INS(I,3)),NPOIS,IERR)
                 REG(INS(I,4))=REAL(NPOIS)
            ELSEIF(INS(I,1).EQ.25)THEN
                 REG(INS(I,4))=RANLAN(RNDUNI(1.0))
            ELSEIF(INS(I,1).EQ.26)THEN
                 REG(INS(I,4))=RNDPOL(REG(INS(I,3)))
            ELSEIF(INS(I,1).EQ.27)THEN
                 REG(INS(I,4))=RNDFUN(REG(INS(I,3)))
            ELSEIF(INS(I,1).EQ.29)THEN
                 IF(REG(INS(I,3)).GT.0)THEN
                      REG(INS(I,4))=RNGAMA(REG(INS(I,3)))
                 ELSE
                      REG(INS(I,4))=-1
                      NAERR(14)=NAERR(14)+1
                 ENDIF
            ELSEIF(INS(I,1).EQ.30)THEN
                 REG(INS(I,4))=RNDLAP(REG(INS(I,3)))
*   A row of integers.
            ELSEIF(INS(I,1).EQ.40)THEN
                 ISIZ(1)=NINT(REG(INS(I,3)))
                 CALL MATADM('ALLOCATE',IREF,1,ISIZ,2,IFAIL1)
                 IF(IFAIL1.NE.0)RETURN
                 REG(INS(I,4))=REAL(IREF)
                 MODREG(INS(I,4))=5
*   A row of zeroes.
            ELSEIF(INS(I,1).EQ.44)THEN
                 ISIZ(1)=NINT(REG(INS(I,3)))
                 CALL MATADM('ALLOCATE',IREF,1,ISIZ,2,IFAIL1)
                 IF(IFAIL1.NE.0)RETURN
                 ISLOT=MATSLT(IREF)
                 IF(ISLOT.LE.0)THEN
                      RETURN
                 ELSE
                      DO 10 J=1,MLEN(ISLOT)
                      MVEC(MORG(ISLOT)+J)=0.0
10                    CONTINUE
                      REG(INS(I,4))=REAL(IREF)
                      MODREG(INS(I,4))=5
                 ENDIF
*   A row of ones.
            ELSEIF(INS(I,1).EQ.45)THEN
                 ISIZ(1)=NINT(REG(INS(I,3)))
                 CALL MATADM('ALLOCATE',IREF,1,ISIZ,2,IFAIL1)
                 IF(IFAIL1.NE.0)RETURN
                 ISLOT=MATSLT(IREF)
                 IF(ISLOT.LE.0)THEN
                      RETURN
                 ELSE
                      DO 20 J=1,MLEN(ISLOT)
                      MVEC(MORG(ISLOT)+J)=1.0
20                    CONTINUE
                      REG(INS(I,4))=REAL(IREF)
                      MODREG(INS(I,4))=5
                 ENDIF
*   Gamma function.
            ELSEIF(INS(I,1).EQ.47)THEN
                 IF(REG(INS(I,3)).LT.1E-20)THEN
                      NAERR(12)=NAERR(12)+1
                      RETURN
                 ELSEIF(REG(INS(I,3)).GT.25.0)THEN
                      NAERR(13)=NAERR(13)+1
                      RETURN
                 ELSE
                      REG(INS(I,4))=GAMMA(REG(INS(I,3)))
                      MODREG(INS(I,4))=2
                 ENDIF
*   log-Gamma function.
            ELSEIF(INS(I,1).EQ.48)THEN
                 IF(REG(INS(I,3)).LE.0)THEN
                      NAERR(14)=NAERR(14)+1
                      RETURN
                 ELSE
                      REG(INS(I,4))=ALGAMA(REG(INS(I,3)))
                      MODREG(INS(I,4))=2
                 ENDIF
*   Unidentified.
            ELSE
                 MODREG(INS(I,4))=0
                 NAERR(10)=NAERR(10)+1
                 RETURN
            ENDIF
*** Binary logical operators between real type arguments.
       ELSEIF(INS(I,2).EQ.10)THEN
            MODREG(INS(I,4))=3
            REG(INS(I,4))=0.0
            IF(ABS(REG(INS(I,1))-REG(INS(I,3))).LT.EPS)REG(INS(I,4))=1.0
       ELSEIF(INS(I,2).EQ.11)THEN
            MODREG(INS(I,4))=3
            REG(INS(I,4))=0.0
            IF(ABS(REG(INS(I,1))-REG(INS(I,3))).GT.EPS)REG(INS(I,4))=1.0
       ELSEIF(INS(I,2).EQ.12)THEN
            MODREG(INS(I,4))=3
            REG(INS(I,4))=0.0
            IF(REG(INS(I,1)).LT.REG(INS(I,3)))REG(INS(I,4))=1.0
       ELSEIF(INS(I,2).EQ.13)THEN
            MODREG(INS(I,4))=3
            REG(INS(I,4))=0.0
            IF(REG(INS(I,1)).LE.REG(INS(I,3)))REG(INS(I,4))=1.0
       ELSEIF(INS(I,2).EQ.14)THEN
            MODREG(INS(I,4))=3
            REG(INS(I,4))=0.0
            IF(REG(INS(I,1)).GT.REG(INS(I,3)))REG(INS(I,4))=1.0
       ELSEIF(INS(I,2).EQ.15)THEN
            MODREG(INS(I,4))=3
            REG(INS(I,4))=0.0
            IF(REG(INS(I,1)).GE.REG(INS(I,3)))REG(INS(I,4))=1.0
*** Concatenate the 2 arguments to form a Matrix.
       ELSEIF(INS(I,2).EQ.16)THEN
            ISIZ(1)=2
            CALL MATADM('ALLOCATE',IREF,1,ISIZ,2,IFAIL1)
            IF(IFAIL1.NE.0)RETURN
            REG(INS(I,4))=REAL(IREF)
            MODREG(INS(I,4))=5
            ISLOT=MATSLT(IREF)
            IF(ISLOT.LE.0)RETURN
            MVEC(MORG(ISLOT)+1)=REG(INS(I,1))
            MVEC(MORG(ISLOT)+2)=REG(INS(I,3))
*** Unidentified operation code.
       ELSE
            MODREG(INS(I,4))=0
            NAERR(10)=NAERR(10)+1
            RETURN
       ENDIF
*** Reset IFAIL to 0 because the calculations were probably successful.
       IFAIL=0
       END
