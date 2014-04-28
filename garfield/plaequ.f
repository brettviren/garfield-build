CDECK  ID>, PLAEQU.
       SUBROUTINE PLAEQU(IREF1,IREF2,EPSX,EPSY,EQUAL)
*-----------------------------------------------------------------------
*   PLAEQU - Determines whether 2 planes are equal.
*   (Last changed on  8/ 5/10.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       LOGICAL EQUAL,ONLIND
       INTEGER IREF1,IREF2,NPL1,NPL2,ICOL1,IFAIL1,ICOL2,IFAIL2,I,J
       DOUBLE PRECISION EPSX,EPSY,
     -      XPL1(MXEDGE),YPL1(MXEDGE),ZPL1(MXEDGE),APL1,BPL1,CPL1,DPL1,
     -      XPL2(MXEDGE),YPL2(MXEDGE),ZPL2(MXEDGE),APL2,BPL2,CPL2,DPL2
       EXTERNAL ONLIND
*** Initial value.
       EQUAL=.FALSE.
*** Retrieve both planes.
       CALL PLABU2('READ',IREF1,NPL1,XPL1,YPL1,ZPL1,APL1,BPL1,CPL1,DPL1,
     -      ICOL1,IFAIL1)
       CALL PLABU2('READ',IREF2,NPL2,XPL2,YPL2,ZPL2,APL2,BPL2,CPL2,DPL2,
     -      ICOL2,IFAIL2)
       IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)THEN
            PRINT *,' !!!!!! PLAEQU WARNING : Unable to retrieve a'//
     -           ' projected polygon; declared not equal.'
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLAEQU DEBUG   :''//
     -           '' Reference numbers: '',2I4)') IREF1,IREF2
            EQUAL=.FALSE.
            RETURN
       ENDIF
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ PLAEQU DEBUG   : Curve 1'',
     -           '' has '',I5,'' points.'')') NPL1
            DO 50 I=1,NPL1
            WRITE(LUNOUT,'(2X,I3,F13.6,2X,F13.6)') I,XPL1(I),YPL1(I)
50          CONTINUE
            WRITE(LUNOUT,'(''                          Curve 2'',
     -           '' has '',I5,'' points.'')') NPL2
            DO 60 I=1,NPL2
            WRITE(LUNOUT,'(2X,I3,F13.6,2X,F13.6)') I,XPL2(I),YPL2(I)
60          CONTINUE
       ENDIF
*** If 0 length, simply return.
       IF(NPL1.LE.0.OR.NPL2.LE.0)THEN
            EQUAL=.FALSE.
            RETURN
       ENDIF
*** Compare all points of 1 with all points of 2.
       DO 10 I=1,NPL1
*   Loop over 2 until a match is found.
       DO 20 J=1,NPL2
       IF(ABS(XPL2(J)-XPL1(I)).LT.EPSX.AND.
     -    ABS(YPL2(J)-YPL1(I)).LT.EPSY)GOTO 10
       IF(ONLIND(XPL2(1+MOD(J-1,NPL2)),YPL2(1+MOD(J-1,NPL2)),
     -           XPL2(1+MOD(J  ,NPL2)),YPL2(1+MOD(J  ,NPL2)),
     -           XPL1(I              ),YPL1(I)             ))GOTO 10
20     CONTINUE
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLAEQU DEBUG   :'',
     -      '' No match on 2 for point '',I5,'' of 1.'')') I
       EQUAL=.FALSE.
       RETURN
10     CONTINUE
*** Compare all points of 2 with all points of 1.
       DO 30 I=1,NPL2
*   Loop over 2 until a match is found.
       DO 40 J=1,NPL1
       IF(ABS(XPL2(J)-XPL1(I)).LT.EPSX.AND.
     -    ABS(YPL2(J)-YPL1(I)).LT.EPSY)GOTO 30
       IF(ONLIND(XPL1(1+MOD(J-1,NPL1)),YPL1(1+MOD(J-1,NPL1)),
     -           XPL1(1+MOD(J  ,NPL1)),YPL1(1+MOD(J  ,NPL1)),
     -           XPL2(I              ),YPL2(I)             ))GOTO 30
40     CONTINUE
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLAEQU DEBUG   :'',
     -      '' No match on 1 for point '',I5,'' of 2.'')') I
       EQUAL=.FALSE.
       RETURN
30     CONTINUE
*** If we get this far, the curves are the same.
       EQUAL=.TRUE.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLAEQU DEBUG   :'',
     -      '' Curves match.'')')
       END
