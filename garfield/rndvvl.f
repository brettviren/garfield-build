CDECK  ID>, RNDVVL.
       REAL FUNCTION RNDVVL(RKAPPA,BETA2)
*-----------------------------------------------------------------------
*   RNDVVL - Function generating a Vavilov random number, providing
*            better precision than RNDVAV
*   VARIABLES : RKAPPA     : xi/emax
*               BETA2      : velocity2
*   (Last changed on 21/ 3/07.)
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
       REAL RKAPPA,BETA2,RKOLD,BETOLD,FSPACE(200),XLOW,XHIGH,VVIDEN,
     -      XRAN(1)
       INTEGER LENGTH,IFAIL
       EXTERNAL VVIDEN
       SAVE RKOLD,BETOLD,FSPACE
*** Initial parameter values (not acceptable)
       DATA RKOLD/-1/, BETOLD/-1/
*** Check values for applicable range
       IF(RKAPPA.LT.0.01.OR.RKAPPA.GT.10.OR.
     -      BETA2.LT.0.OR.BETA2.GT.1)THEN
            NAERR(15)=NAERR(15)+1
            RNDVVL=0
            RETURN
       ENDIF
*** See whether the initialisation needs to be called.
       IF(ABS(RKAPPA-RKOLD).GT.1E-5.OR.ABS(BETA2-BETOLD).GT.1E-5)THEN
*   Prepare the Vavilov function.
            CALL VVISET(RKAPPA,BETA2,0,XLOW,XHIGH)
*   Prepare for random number generation.
            CALL FUGLXP(VVIDEN,FSPACE,XLOW,XHIGH,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! RNDVVL WARNING : Error preparing'//
     -                ' the Vavilov function for kappa = ',RKAPPA,
     -                ', beta2 = ',BETA2,'; returning 0.'
                 RNDVVL=0
                 RETURN
            ENDIF
*   If successful, store old kappa and beta2.
            RKOLD=RKAPPA
            BETOLD=BETA2
       ENDIF
*** Return a value
       LENGTH=1
       CALL FUGLUX(FSPACE,XRAN,LENGTH)
       RNDVVL=XRAN(1)
       END
