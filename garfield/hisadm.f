CDECK  ID>, HISADM.
       SUBROUTINE HISADM(ACTION,IREF,NNCHA,XXMIN,XXMAX,AUTO,IFAIL)
*-----------------------------------------------------------------------
*   HISADM - Takes care of histogram booking.
*            range setting if requested.
*   (Last changed on 20/11/01.)
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
       DOUBLE PRECISION CONTEN(MXHIST,0:MXCHA+1)
       REAL XMIN(MXHIST),XMAX(MXHIST)
       DOUBLE PRECISION SX0(MXHIST),SX1(MXHIST),SX2(MXHIST)
       INTEGER NCHA(MXHIST),NENTRY(MXHIST)
       LOGICAL SET(MXHIST),HISUSE(MXHIST),HISLIN(MXHIST)
       COMMON /HISDAT/ SX0,SX1,SX2,CONTEN,XMIN,XMAX,HISUSE,HISLIN,NCHA,
     -      NENTRY,SET
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       CHARACTER*(*) ACTION
       CHARACTER*10 NAME
       INTEGER IREF,IFAIL,NNCHA,I,J,NLIST
       REAL XXMIN,XXMAX
       LOGICAL AUTO
*** Allocate a new histogram.
       IF(ACTION.EQ.'ALLOCATE'.OR.ACTION.EQ.'INTEGER')THEN
*   Check the request is reasonable.
            IF(NNCHA.GT.MXCHA.OR.NNCHA.LE.0.OR.
     -           (XXMIN.GE.XXMAX.AND..NOT.AUTO))THEN
                 PRINT *,' !!!!!! HISADM WARNING : Unreasonable'//
     -                ' allocation request refused.'
                 IFAIL=1
                 RETURN
            ENDIF
*   Look for a free slot.
            DO 10 I=1,MXHIST
*   Found a free slot.
            IF(.NOT.HISUSE(I))THEN
                 IREF=I
                 DO 20 J=0,MXCHA+1
                 CONTEN(IREF,J)=0.0
20               CONTINUE
                 SX0(IREF)=0.0D0
                 SX1(IREF)=0.0D0
                 SX2(IREF)=0.0D0
                 NENTRY(IREF)=0
                 XMIN(IREF)=XXMIN
                 XMAX(IREF)=XXMAX
                 NCHA(IREF)=NNCHA
                 SET(IREF)=.NOT.AUTO
                 HISUSE(IREF)=.TRUE.
                 IF(ACTION.EQ.'INTEGER')THEN
                      HISLIN(IREF)=.TRUE.
                 ELSE
                      HISLIN(IREF)=.FALSE.
                 ENDIF
                 IFAIL=0
                 IF(LDEBUG)PRINT *,' ++++++ HISADM DEBUG   :'//
     -                ' Histogram ',IREF,' allocated.'
                 RETURN
            ENDIF
10          CONTINUE
*   No free slot found.
            PRINT *,' !!!!!! HISADM WARNING : No free histogram'//
     -           ' storage available; no slot allocated.'
            IREF=0
            IFAIL=1
*** Release an allocated histogram.
       ELSEIF(ACTION.EQ.'DELETE')THEN
            IF(IREF.GE.1.AND.IREF.LE.MXHIST)THEN
                 HISUSE(IREF)=.FALSE.
                 IF(LDEBUG)PRINT *,' ++++++ HISADM DEBUG   :'//
     -                ' Histogram ',IREF,' deallocated.'
                 DO 45 J=1,NGLB
                 IF(GLBMOD(J).EQ.4.AND.NINT(GLBVAL(J)).EQ.IREF)
     -                GLBMOD(J)=0
45               CONTINUE
                 IFAIL=0
            ELSE
                 PRINT *,' !!!!!! HISADM WARNING : Histogram to be'//
     -                ' deleted not found.'
                 IFAIL=1
            ENDIF
*** List of histograms.
       ELSEIF(ACTION.EQ.'LIST')THEN
*   Print a header.
            WRITE(LUNOUT,'(/''  OVERVIEW OF EXISTING HISTOGRAMS''//
     -           ''  Number Global       Integral    Average'',
     -           ''        RMS    Minimum    Maximum''/)')
*   Loop over all histograms.
            NLIST=0
            DO 30 I=1,MXHIST
*   Case 1: histogram slot not in use.
            IF(.NOT.HISUSE(I))GOTO 30
*   Locate the global variable name that goes with the histogram.
            NAME='(none)'
            DO 40 J=1,NGLB
            IF(GLBMOD(J).EQ.4.AND.NINT(GLBVAL(J)).EQ.I)
     -           NAME=GLBVAR(J)
40          CONTINUE
*   Case 2: histogram in use but still empty, range set.
            IF(NENTRY(I).EQ.0.AND.SET(I))THEN
                 WRITE(LUNOUT,'(2X,I6,1X,A10,''   No entries yet'',16X,
     -                2(1X,E10.3))') I,NAME,XMIN(I),XMAX(I)
*   Case 3: histogram in use but still empty, range not yet set.
            ELSEIF(NENTRY(I).EQ.0)THEN
                 WRITE(LUNOUT,'(2X,I6,1X,A10,''   Autorange histogram'',
     -                '' without entries sofar'')') I,NAME
*   Case 4: entries available.
            ELSEIF(SET(I))THEN
                 IF(SX0(I).LE.0)THEN
                      WRITE(LUNOUT,'(2X,I6,1X,A10,1X,E10.3,
     -                     ''     No statistics yet'',2(1X,E10.3))')
     -                     I,NAME,SX0(I),XMIN(I),XMAX(I)
                 ELSEIF(SX0(I).LT.2)THEN
                      WRITE(LUNOUT,'(2X,I6,1X,A10,2(1X,E10.3),
     -                     ''  Undefined'',2(1X,E10.3))')
     -                     I,NAME,SX0(I),SX1(I)/SX0(I),XMIN(I),XMAX(I)
                 ELSE
                      WRITE(LUNOUT,'(2X,I6,1X,A10,5(1X,E10.3))')
     -                     I,NAME,SX0(I),SX1(I)/SX0(I),
     -                     SQRT((SX2(I)-SX1(I)**2/SX0(I))/
     -                     (SX0(I)-1)),XMIN(I),XMAX(I)
                 ENDIF
            ELSE
                 WRITE(LUNOUT,'(2X,I6,1X,A10,3(1X,E10.3),
     -                '' Range not yet set'')') I,NAME,
     -                SX0(I),SX1(I)/SX0(I),
     -                SQRT((SX2(I)-SX1(I)**2/SX0(I))/
     -                (SX0(I)-1))
            ENDIF
*   Increment the counter.
            NLIST=NLIST+1
30          CONTINUE
*   Say how many histograms are currently known.
            WRITE(LUNOUT,'(/''  Number of histograms booked: '',I5/)')
     -           NLIST
*** Unknown action.
       ELSE
            PRINT *,' !!!!!! HISADM WARNING : Invalid action requested.'
            IFAIL=1
       ENDIF
       END
