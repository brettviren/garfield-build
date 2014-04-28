CDECK  ID>, MATPMK.
       SUBROUTINE MATPMK(IREF1,IREF2,IREF3)
*-----------------------------------------------------------------------
*   MATPMK - Plots a set of markers.
*   (Last changed on 14/12/00.)
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
       INTEGER IREF1,IREF2,IREF3,ISLOT1,ISLOT2,ISLOT3,I,MATSLT
       DOUBLE PRECISION XPL(MXLIST),YPL(MXLIST),ZPL(MXLIST)
       EXTERNAL MATSLT
*** Indentify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MATPMK ///'
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MATPMK DEBUG   : Plotting'',
     -      '' marker vectors '',3I5)') IREF1,IREF2,IREF3
*** Locate the 3 vectors.
       ISLOT1=MATSLT(IREF1)
       ISLOT2=MATSLT(IREF2)
       ISLOT3=MATSLT(IREF3)
       IF(ISLOT1.EQ.0.OR.ISLOT2.EQ.0.OR.ISLOT3.EQ.0)THEN
            PRINT *,' !!!!!! MATPMK WARNING : One or more of the'//
     -           ' plot vectors has not been found; not plotted.'
            RETURN
       ENDIF
*** Verify that the 3 have the same length.
       IF(MLEN(ISLOT1).NE.MLEN(ISLOT2).OR.
     -      MLEN(ISLOT2).NE.MLEN(ISLOT3))THEN
            PRINT *,' !!!!!! MATPMK WARNING : The 3 vectors do not'//
     -           ' have the same length; not plotted.'
            RETURN
       ENDIF
*** Verify that the length is in the range [1,MXLIST].
       IF(MLEN(ISLOT1).LT.1.OR.MLEN(ISLOT1).GT.MXLIST)THEN
            PRINT *,' !!!!!! MATPMK WARNING : The length of the'//
     -           ' vectors is not in the range [1,MXLIST]; not plotted.'
            RETURN
       ENDIF
*** Make a double precision copy of the vector.
       DO 10 I=1,MLEN(ISLOT1)
       XPL(I)=DBLE(MVEC(MORG(ISLOT1)+I))
       YPL(I)=DBLE(MVEC(MORG(ISLOT2)+I))
       ZPL(I)=DBLE(MVEC(MORG(ISLOT3)+I))
10     CONTINUE
*** Plot the markers.
       CALL PLAGPM(MLEN(ISLOT1),XPL,YPL,ZPL)
       END
