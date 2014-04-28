CDECK  ID>, MATLOC.
       SUBROUTINE MATLOC(IREF,IMAX,NMAX,MAXMIN,IFAIL)
*-----------------------------------------------------------------------
*   MATLOC - Locates maximum or minimum of a matrix.
*   Variables: IREF      - Reference of matrix
*              IMAX      - Vector with location of max/min
*              NMAX      - Number of indices returned.
*              MAXMIN    - -1: Search minimum, +1: search maximum
*              IFAIL     - Indicator of success (=0) or failure (=1)
*   (Last changed on  1/12/02.)
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
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER IREF,ISLOT,IFAIL,MATSLT,MATADR,IA(MXMDIM),IMAX(*),I,J,
     -      IADDR,MAXMIN,NMAX
       REAL EXTR
       EXTERNAL MATSLT,MATADR
       LOGICAL FIRST
*** Indentify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MATLOC ///'
*** Set an initial, invalid response.
       DO 100 I=1,MXMDIM
       IMAX(I)=0
100    CONTINUE
       NMAX=0
       IFAIL=1
*** Locate the vector.
       ISLOT=MATSLT(IREF)
       IF(ISLOT.LE.0)THEN
            PRINT *,' !!!!!! MATLOC WARNING : Unable to locate the'//
     -           ' matrix to be sorted; no maximum/minimum search.'
            RETURN
       ENDIF
*** Debugging information.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MATLOC DEBUG   : Looking'',
     -      '' for max(+1) or min(-1) '',I2,'' of matrix ref='',I5,
     -      '', slot='',I5,'' with '',I5,'' elements.'')') MAXMIN,IREF,
     -      ISLOT,MLEN(ISLOT)
*** Loop over the sub matrix, initialiase.
       FIRST=.TRUE.
       EXTR=0
*   Initial address vector.
       DO 20 I=1,MDIM(ISLOT)
       IA(I)=1
20     CONTINUE
*   Return here for next element.
10     CONTINUE
*   Find the address for this element.
       IADDR=MATADR(ISLOT,IA)
*   See whether this is the largest.
       IF(FIRST.OR.
     -      (MAXMIN.GT.0.AND.MVEC(IADDR).GT.EXTR).OR.
     -      (MAXMIN.LT.0.AND.MVEC(IADDR).LT.EXTR))THEN
            DO 30 I=1,MDIM(ISLOT)
            IMAX(I)=IA(I)
30          CONTINUE
            EXTR=MVEC(IADDR)
            FIRST=.FALSE.
       ENDIF
*   Increment the address vector.
       DO 40 I=1,MDIM(ISLOT)
       IF(IA(I).LT.MSIZ(ISLOT,I))THEN
            IA(I)=IA(I)+1
            DO 50 J=1,I-1
            IA(J)=1
50          CONTINUE
            GOTO 10
       ENDIF
40     CONTINUE
*** See whether a point has been found.
       IF(.NOT.FIRST)THEN
            IFAIL=0
            NMAX=MDIM(ISLOT)
       ELSE
            PRINT *,' !!!!!! MATLOC WARNING : No maximum/minimum'//
     -           ' has been found.'
       ENDIF
       END
