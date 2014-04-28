CDECK  ID>, OUTEI7.
       SUBROUTINE OUTEI7(EXBOL,IOBOL)
*-----------------------------------------------------------------------
*   OUTEI7 - Extracts the ionisation/excitation rates from Magboltz.
*   (Last changed on 25/ 8/09.)
*-----------------------------------------------------------------------
       implicit none
*   Array dimensions.
       integer mxngas
       parameter(mxngas=6)
*-----------------------------------------------------------------------
*   MAGPAR - Interface parameters for gas mixing with Magboltz.
*   (Last changed on  2/ 3/08.)
*-----------------------------------------------------------------------
       INTEGER MXGNAM
       PARAMETER(MXGNAM=60)
       DOUBLE PRECISION FRAMIX
       LOGICAL LF0PLT,LCSPLT,LGKEEP,LBMCPR
       COMMON /MAGPAR/ FRAMIX(MXGNAM),LF0PLT,LCSPLT,LGKEEP,LBMCPR
       DOUBLE PRECISION TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX,RSTART,
     -      EMAG
       INTEGER NMAX
       COMMON/SETP/TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX(8),RSTART,
     -      EMAG,NMAX
       DOUBLE PRECISION ZTOT,TTOT,ZTOTS,TTOTS
       COMMON/TTRM/ZTOT,TTOT,ZTOTS,TTOTS
       INTEGER NGAS,NSTEP,IDBG
       DOUBLE PRECISION EFINAL,ESTEP,AKT,ARY,TEMPC,TORR
       PARAMETER(ARY=13.60569172)
       COMMON/INPT/NGAS,NSTEP,EFINAL,ESTEP,AKT,TEMPC,TORR,IDBG
*   Adjusted size of ICOLL
       DOUBLE PRECISION TIME,SPEC,TMAX1,AVE,DEN,XID,X,Y,Z,ST
       INTEGER ICOLL,NNULL,ICOLN
       COMMON/OUTPT/TIME(300),ICOLL(5*mxngas),SPEC(2048),TMAX1,
     -      AVE,DEN,XID,X,Y,Z,ST,NNULL,ICOLN(512)
       CHARACTER*30 DSCRPT
       COMMON/SCRIP/DSCRPT(512)
*   Sometimes IPLAST is called LAST
       DOUBLE PRECISION CF,EIN,TCF,RGAS,WPL
       INTEGER IARRY,IPN,IPLAST,ISIZE
       COMMON/LARGE/CF(2048,512),EIN(512),TCF(2048),IARRY(512),
     -      RGAS(512),IPN(512),WPL(512),IPLAST,ISIZE
*   Changed name of common from /NAMES/ to /MBGNAM/ for Mac OS X
       CHARACTER*15 NAMEG
       COMMON /MBGNAM/ NAMEG(mxngas)
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
       REAL EXBOL(MXEXG),IOBOL(MXIOG),FREQ,FRELV,ERRFRE
       INTEGER NREAL,I,J,IEXC,IION,IFAIL
*** Initialise the vectors
       DO 30 I=1,MXEXG
       EXBOL(I)=0
30     CONTINUE
       DO 40 I=1,MXIOG
       IOBOL(I)=0
40     CONTINUE
*** Number of collisions and frequency scaling factor.
       IF(TTOTS.EQ.0.0D0) THEN
            NREAL=NMAX
            TTOTS=ST
       ELSE
            NREAL=INT(XID)
       ENDIF
       FREQ=REAL(NREAL)/TTOTS
*** Loop over the gases and states, selecting
       DO 10 I=1,NGAS
       DO 20 J=1,IPLAST
       IF(IARRY(J).LE.5*I .AND. IARRY(J).GT.5*(I-1))THEN
**  Compute the rates.
            FRELV=FREQ*ICOLN(J)/NREAL
            IF(ICOLN(J).EQ.0) THEN
                 ERRFRE=0.0
            ELSE
                 ERRFRE=100.0/SQRT(REAL(ICOLN(J)))
            ENDIF
**  Select the excitation frequencies
            IF(DSCRPT(J)(1:4).EQ.' EXC')THEN
*   Identify and store the rate.
                 CALL GASIDE(IEXC,NAMEG(I)//DSCRPT(J),IFAIL)
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' !!!!!! OUTEI7 WARNING: Excitation'//
     -                     ' buffer is full; unable to add '//
     -                     NAMEG(I)//DSCRPT(J)
                 ELSE
                      EXBOL(IEXC)=FRELV/TORR
                 ENDIF
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ OUTEI7 DEBUG   :'',
     -                '' Gas '',A,'', state '',A,'', exc id '',I3,
     -                '', rate: '',E12.5,'' +/- '',F8.4,'' %'')')
     -                NAMEG(I),DSCRPT(J),IEXC,FRELV,ERRFRE
**  Select the ionisation frequencies
            ELSEIF(DSCRPT(J)(1:4).EQ.' ION')THEN
*   Identify and store the rate.
                 CALL GASIDI(IION,NAMEG(I)//DSCRPT(J),IFAIL)
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' !!!!!! OUTEI7 WARNING: Ionisation'//
     -                     ' buffer is full; unable to add '//
     -                     NAMEG(I)//DSCRPT(J)
                 ELSE
                      IOBOL(IION)=FRELV/TORR
                 ENDIF
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ OUTEI7 DEBUG   :'',
     -                '' Gas '',A,'', state '',A,'', ion id '',I3,
     -                '', rate: '',E12.5,'' +/- '',F8.4,'' %'')')
     -                NAMEG(I),DSCRPT(J),IION,FRELV,ERRFRE
            ENDIF
       ENDIF
20     CONTINUE
10     CONTINUE
       END
