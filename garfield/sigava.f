CDECK  ID>, SIGAVA.
       SUBROUTINE SIGAVA(QCL,ACL)
*-----------------------------------------------------------------------
*   SIGAVA - Returns a random number for the multiplication factor: the
*            number of electrons produced in the avalanche + the
*            electron that started the avalanche, including electrons
*            lost in attachment.
*   (Last changed on  8/12/01.)
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
       LOGICAL FPERX,FPERY,LCROSS,TRASET,TRAFLG,LITAIL,LDTAIL,LRTAIL,
     -      LEPULS,LIPULS,SIGSET,RESSET
       INTEGER NPAIR,ICLUST,NFOUR,MFEXP,MXMIN,MXMAX,
     -      MYMIN,MYMAX,NTRBNK,ITRMAJ,NTIME,NORIA,
     -      NASIMP,JIORD,NISIMP,NMQUAD,NCANG,IENANG
       REAL TIMSIG,SIGNAL,TCLUST,SCLUST,ACLUST,BCLUST,FCLUST,
     -      AVALAN,TSTART,TDEV,PRSTHR,
     -      TRABNK,TRAVEC
       CHARACTER*(MXCHAR) FCNANG
       CHARACTER*12 AVATYP
       CHARACTER*3 FCELTP
       COMMON /SIGDAT/ TIMSIG(MXLIST),SIGNAL(MXLIST,MXSW,2),
     -      AVALAN(2),TRAVEC(MXLIST),
     -      TRABNK(MXLIST,9),TSTART,TDEV,PRSTHR,
     -      TCLUST,SCLUST,ACLUST,BCLUST,FCLUST,ICLUST,NPAIR,
     -      NFOUR,ITRMAJ,JIORD,NISIMP,NMQUAD,IENANG,NTIME,NORIA,
     -      MFEXP,MXMIN,MXMAX,MYMIN,MYMAX,NTRBNK,NASIMP,NCANG,
     -      TRASET,TRAFLG(9),FPERX,FPERY,LCROSS,LITAIL,LDTAIL,LRTAIL,
     -      LEPULS,LIPULS,SIGSET,RESSET
       COMMON /SIGCHR/ FCELTP,AVATYP,FCNANG
       REAL QCL,ACL,RNDEXP,RNDNOR,RNDPOL
       INTEGER MXRNDM,IRNDM
       PARAMETER(MXRNDM=100)
       EXTERNAL RNDEXP,RNDNOR,RNDPOL
*** Exponential type.
       IF(AVATYP.EQ.'EXPONENTIAL')THEN
10          CONTINUE
            QCL=RNDEXP(AVALAN(1))
*   Ensure the multiplication factor equals at least 1.
            IRNDM=0
            IF(QCL.LT.1.0)THEN
                 IRNDM=IRNDM+1
                 IF(IRNDM.LE.MXRNDM)THEN
                      GOTO 10
                 ELSE
                      PRINT *,' !!!!!! SIGAVA WARNING : Drawn ',MXRNDM,
     -                     ' times an exponential random number < 1;'//
     -                     ' forced to 1.'
                 ENDIF
            ENDIF
*** Fixed factor.
       ELSEIF(AVATYP.EQ.'FIXED')THEN
            QCL=AVALAN(1)
*** Gaussian distribution.
       ELSEIF(AVATYP.EQ.'GAUSSIAN')THEN
20          CONTINUE
            QCL=RNDNOR(AVALAN(1),AVALAN(1)*AVALAN(2))
*   Ensure the multiplication factor equals at least 1.
            IRNDM=0
            IF(QCL.LT.1.0)THEN
                 IRNDM=IRNDM+1
                 IF(IRNDM.LE.MXRNDM)THEN
                      GOTO 20
                 ELSE
                      PRINT *,' !!!!!! SIGAVA WARNING : Drawn ',MXRNDM,
     -                     ' times a Gaussian random number < 1;'//
     -                     ' forced to 1.'
                 ENDIF
            ENDIF
*** No multiplication.
       ELSEIF(AVATYP.EQ.'NONE')THEN
            QCL=1
*** Townsend based exponential distribution.
       ELSEIF(AVATYP.EQ.'TOWNSEND')THEN
30          CONTINUE
            QCL=RNDEXP(ACL)
*   Ensure the multiplication factor equals at least 1.
            IRNDM=0
            IF(QCL.LT.1.0)THEN
                 IRNDM=IRNDM+1
                 IF(IRNDM.LE.MXRNDM)THEN
                      GOTO 30
                 ELSE
                      PRINT *,' !!!!!! SIGAVA WARNING : Drawn ',MXRNDM,
     -                     ' times an exponential random number < 1;'//
     -                     ' forced to 1.'
                 ENDIF
            ENDIF
*** Townsend without fluctuations.
       ELSEIF(AVATYP.EQ.'TOWN-FIXED')THEN
            QCL=ACL
*** Polya distributed with fixed mean.
       ELSEIF(AVATYP.EQ.'POLYA-FIXED')THEN
40          CONTINUE
            QCL=AVALAN(1)*RNDPOL(AVALAN(2))
*   Ensure the multiplication factor equals at least 1.
            IRNDM=0
            IF(QCL.LT.1.0)THEN
                 IRNDM=IRNDM+1
                 IF(IRNDM.LE.MXRNDM)THEN
                      GOTO 40
                 ELSE
                      PRINT *,' !!!!!! SIGAVA WARNING : Drawn ',MXRNDM,
     -                     ' times a Polya random number < 1;'//
     -                     ' forced to 1.'
                 ENDIF
            ENDIF
*** Polya distributed with Townsend mean.
       ELSEIF(AVATYP.EQ.'POLYA-TOWN')THEN
50          CONTINUE
            QCL=ACL*RNDPOL(AVALAN(1))
*   Ensure the multiplication factor equals at least 1.
            IRNDM=0
            IF(QCL.LT.1.0)THEN
                 IRNDM=IRNDM+1
                 IF(IRNDM.LE.MXRNDM)THEN
                      GOTO 50
                 ELSE
                      PRINT *,' !!!!!! SIGAVA WARNING : Drawn ',MXRNDM,
     -                     ' times a Polya random number < 1;'//
     -                     ' forced to 1.'
                 ENDIF
            ENDIF
*** Anything else not known, take the (hopefully meanigful) default.
       ELSE
            PRINT *,' !!!!!! SIGAVA WARNING : Unknown avalanche type'//
     -           ' received: '//AVATYP//'; program bug, please report.'
            QCL=AVALAN(1)
       ENDIF
*** Never accept a multiplication smaller than 1.
       IF(QCL.LT.1.0)QCL=1.0
       END
