CDECK  ID>, MATZRO.
       SUBROUTINE MATZRO(IREF1,IREF2,NZERO,ZERO,IFAIL)
*-----------------------------------------------------------------------
*   MATZRO - Finds the zeroes of one matrix vs another.
*   (Last changed on 21/11/01.)
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
       INTEGER IREF1,IREF2,ISLOT1,ISLOT2,I,J,MATSLT,NZERO,NVEC,IFAIL
       REAL ZERO(MXLIST),XVEC(4),YVEC(4),DIVDIF
       EXTERNAL MATSLT,DIVDIF
*** Indentify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MATZRO ///'
*** Assume this will fail.
       IFAIL=1
*** Preset number of zeroes.
       NZERO=0
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MATZRO DEBUG   : Searching'',
     -      '' for zero crossings of vectors '',2I5)') IREF1,IREF2
*** Locate the 3 vectors.
       ISLOT1=MATSLT(IREF1)
       ISLOT2=MATSLT(IREF2)
       IF(ISLOT1.EQ.0.OR.ISLOT2.EQ.0)THEN
            PRINT *,' !!!!!! MATZRO WARNING : One or more of the'//
     -           ' vectors has not been found; no zero search.'
            RETURN
       ENDIF
*** Verify that the 2 have the same length.
       IF(MLEN(ISLOT1).NE.MLEN(ISLOT2))THEN
            PRINT *,' !!!!!! MATZRO WARNING : The vectors do not'//
     -           ' have the same length; no zero search.'
            RETURN
       ENDIF
*** Scan the vectors.
       DO 10 I=1,MLEN(ISLOT1)-1
**  See whether the starting point is a zero.
       IF(MVEC(MORG(ISLOT2)+I).EQ.0)THEN
            NZERO=NZERO+1
            ZERO(NZERO)=MVEC(MORG(ISLOT1)+I)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MATZRO DEBUG   :'',
     -           '' Point '',I4,'': '',E15.8,'' is a zero.'')')
     -           I,ZERO(NZERO)
**  Look for crossings in the interval.
       ELSEIF(MVEC(MORG(ISLOT2)+I)*MVEC(MORG(ISLOT2)+I+1).LT.0)THEN
*   Add the point below, if in the same order.
            NVEC=0
            IF(I-1.GE.1)THEN
                 IF((MVEC(MORG(ISLOT2)+I-1).GT.
     -                MVEC(MORG(ISLOT2)+I).AND.
     -                MVEC(MORG(ISLOT2)+I).GT.0).OR.
     -                (MVEC(MORG(ISLOT2)+I-1).LT.
     -                MVEC(MORG(ISLOT2)+I).AND.
     -                MVEC(MORG(ISLOT2)+I).LT.0))THEN
                      NVEC=NVEC+1
                      XVEC(NVEC)=MVEC(MORG(ISLOT1)+I-1)
                      YVEC(NVEC)=MVEC(MORG(ISLOT2)+I-1)
                 ENDIF
            ENDIF
*   Add the 2 points around the crossing.
            NVEC=NVEC+1
            XVEC(NVEC)=MVEC(MORG(ISLOT1)+I)
            YVEC(NVEC)=MVEC(MORG(ISLOT2)+I)
            NVEC=NVEC+1
            XVEC(NVEC)=MVEC(MORG(ISLOT1)+I+1)
            YVEC(NVEC)=MVEC(MORG(ISLOT2)+I+1)
*   Add the point above, if in the same order.
            IF(I+2.LE.MLEN(ISLOT1))THEN
                 IF((MVEC(MORG(ISLOT2)+I+2).GT.
     -                MVEC(MORG(ISLOT2)+I+1).AND.
     -                MVEC(MORG(ISLOT2)+I+1).GT.0).OR.
     -                (MVEC(MORG(ISLOT2)+I+2).LT.
     -                MVEC(MORG(ISLOT2)+I+1).AND.
     -                MVEC(MORG(ISLOT2)+I+1).LT.0))THEN
                      NVEC=NVEC+1
                      XVEC(NVEC)=MVEC(MORG(ISLOT1)+I+2)
                      YVEC(NVEC)=MVEC(MORG(ISLOT2)+I+2)
                 ENDIF
            ENDIF
*   Interpolate.
            IF(LDEBUG)THEN
                 WRITE(LUNOUT,'(''  ++++++ MATZRO DEBUG   :'',
     -                '' Zero search over the area: '')')
                 DO 20 J=1,NVEC
                 WRITE(LUNOUT,'(26X,2E15.8)') XVEC(J),YVEC(J)
20               CONTINUE
            ENDIF
*   Increment zero count if possible.
            NZERO=NZERO+1
            IF(NZERO.LE.MXLIST)THEN
*   Try a 2nd order interpolation.
                 ZERO(NZERO)=DIVDIF(XVEC,YVEC,NVEC,0.0,MIN(2,NVEC-1))
*   If this lies outside the search range, replace by 1st order.
                 IF(ZERO(NZERO).LT.XVEC(1).OR.
     -                ZERO(NZERO).GT.XVEC(NVEC))
     -                ZERO(NZERO)=DIVDIF(XVEC,YVEC,NVEC,0.0,1)
*   Debugging output.
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MATZRO DEBUG   :'',
     -                '' Zero at '',E15.8)') ZERO(NZERO)
*   Don't compute nor add if the buffer is full.
            ELSE
                 WRITE(LUNOUT,'(''  ++++++ MATZRO DEBUG   : Zero not'',
     -                '' added, buffer is full.'')')
            ENDIF
       ENDIF
10     CONTINUE
*** Check the last point.
       IF(MVEC(MORG(ISLOT2)+MLEN(ISLOT1)).EQ.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MATZRO DEBUG   :'',
     -           '' Final point '',E15.8,'' is a zero.'')') ZERO(NZERO)
            NZERO=NZERO+1
            IF(NZERO.LE.MXLIST)ZERO(NZERO)=
     -           MVEC(MORG(ISLOT1)+MLEN(ISLOT1))
       ENDIF
*** Check the total zero count.
       IF(NZERO.GT.MXLIST)THEN
            PRINT *,' !!!!!! MATZRO WARNING : Number of zeroes'//
     -           ' exceeds MXLIST; list truncated.'
            NZERO=MXLIST
       ENDIF
*** Sort the zeroes.
       CALL FLPSOR(ZERO,NZERO)
*** Seems to have worked.
       IFAIL=0
       END
