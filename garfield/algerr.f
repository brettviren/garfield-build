CDECK  ID>, ALGERR.
       SUBROUTINE ALGERR
*-----------------------------------------------------------------------
*   ALGERR - Routine printing the number of arithmetic errors since the
*            last call from ALGPRE.
*   (Last changed on 22/11/06.)
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
       CHARACTER*20 AUX
       INTEGER I,NC,NATOT
*** Count the errors.
       NATOT=0
       DO 20 I=1,100
       NATOT=NATOT+NAERR(I)
20     CONTINUE
       IF(NERR.LT.NATOT)NERR=NATOT
*** One error.
       IF(NERR.EQ.1)THEN
            PRINT *,' !!!!!! ALGERR WARNING : One arithmetic error'//
     -           ' has been detected.'
*** Two errors.
       ELSEIF(NERR.EQ.2)THEN
            PRINT *,' !!!!!! ALGERR WARNING : Two arithmetic errors'//
     -           ' have been detected.'
*** More errors, format the number and print.
       ELSEIF(NERR.GT.2)THEN
            CALL OUTFMT(REAL(NERR),2,AUX,NC,'LEFT')
            PRINT *,' !!!!!! ALGERR WARNING : '//AUX(1:NC)//
     -           ' arithmetic errors have been detected.'
       ENDIF
*** Print detailed error messages.
       IF(NAERR(1).GT.0)WRITE(*,'(26X,
     -      ''Division by zero:                    '',I5)') NAERR(1)
       IF(NAERR(2).GT.0)WRITE(*,'(26X,
     -      ''Exponential overflow:                '',I5)') NAERR(2)
       IF(NAERR(3).GT.0)WRITE(*,'(26X,
     -      ''Exponential underflow:               '',I5)') NAERR(3)
       IF(NAERR(4).GT.0)WRITE(*,'(26X,
     -      ''Log of a non-positive number:        '',I5)') NAERR(4)
       IF(NAERR(5).GT.0)WRITE(*,'(26X,
     -      ''Arcsin or Arccos of a number > 1:    '',I5)') NAERR(5)
       IF(NAERR(6).GT.0)WRITE(*,'(26X,
     -      ''Square root of a negative number:    '',I5)') NAERR(6)
       IF(NAERR(7).GT.0)WRITE(*,'(26X,
     -      ''Arccosh of a number < 1:             '',I5)') NAERR(7)
       IF(NAERR(8).GT.0)WRITE(*,'(26X,
     -      ''Arctanh of a number outside <-1,1>:  '',I5)') NAERR(8)
       IF(NAERR(9).GT.0)WRITE(*,'(26X,
     -      ''Failure to store a string:           '',I5)') NAERR(9)
       IF(NAERR(10).GT.0)WRITE(*,'(26X,
     -      ''Unidentified operator code:          '',I5)') NAERR(10)
       IF(NAERR(11).GT.0)WRITE(*,'(26X,
     -      ''Undefined power raising:             '',I5)') NAERR(11)
       IF(NAERR(12).GT.0)WRITE(*,'(26X,
     -      ''Gamma of a number not > 0:           '',I5)') NAERR(12)
       IF(NAERR(13).GT.0)WRITE(*,'(26X,
     -      ''Gamma of a number > 25:              '',I5)') NAERR(13)
       IF(NAERR(14).GT.0)WRITE(*,'(26X,
     -      ''Random gamma requested for p <= 0:   '',I5)') NAERR(14)
       IF(NAERR(15).GT.0)WRITE(*,'(26X,
     -      ''Vavilov parameters out of range:     '',I5)') NAERR(15)
*   Histogram related errors
       IF(NAERR(51).GT.0)WRITE(*,'(26X,
     -      ''Invalid histogram reference:         '',I5)') NAERR(51)
       IF(NAERR(52).GT.0)WRITE(*,'(26X,
     -      ''Histogram not in use:                '',I5)') NAERR(52)
       IF(NAERR(53).GT.0)WRITE(*,'(26X,
     -      ''Histogram not yet auto-scaled:       '',I5)') NAERR(53)
       IF(NAERR(54).GT.0)WRITE(*,'(26X,
     -      ''Histograms have incompatible range:  '',I5)') NAERR(54)
*   Matrix related errors
       IF(NAERR(61).GT.0)WRITE(*,'(26X,
     -      ''Invalid matrix reference:            '',I5)') NAERR(61)
       IF(NAERR(62).GT.0)WRITE(*,'(26X,
     -      ''Matrices differ in dimension:        '',I5)') NAERR(62)
*** Whatever happens, reset the error counter.
       NERR=0
       DO 10 I=1,100
       NAERR(I)=0
10     CONTINUE
       END
