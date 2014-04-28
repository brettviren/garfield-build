CDECK  ID>, ALGCLR.
       SUBROUTINE ALGCLR(IENTRY)
*-----------------------------------------------------------------------
*   ALGCLR - Clears an entry point, marking the storage space it
*            occupied as available - only effective after a gbc.
*   (Last changed on  1/ 2/01.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       LOGICAL FOUND
       INTEGER I,IENTRY
*** Scan the entry point table to find the entry.
       FOUND=.FALSE.
       DO 10 I=1,NALGE
       IF(ALGENT(I,1).EQ.IENTRY)THEN
            FOUND=.TRUE.
            IF(ALGENT(I,2).EQ.0.AND.LDEBUG)THEN
                 WRITE(LUNOUT,'(''  ++++++ ALGCLR DEBUG   : Entry'',
     -                '' point '',I4,'' was already cleared.'')') IENTRY
            ELSEIF(LDEBUG)THEN
                 WRITE(LUNOUT,'(''  ++++++ ALGCLR DEBUG   : Entry'',
     -                '' point '',I4,'' cleared.'')') IENTRY
            ENDIF
            ALGENT(I,2)=0
       ENDIF
10     CONTINUE
*** Make sure the entry was indeed found.
       IF(.NOT.FOUND)PRINT *,' !!!!!! ALGCLR WARNING : The entry'//
     -      ' point to be cleared does not exist; program bug.'
       END
