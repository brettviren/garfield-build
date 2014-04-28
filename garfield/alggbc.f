CDECK  ID>, ALGGBC.
       SUBROUTINE ALGGBC
*-----------------------------------------------------------------------
*   ALGGBC - Performs a garbage collect in the algebra memory.
*   (Last changed on  1/11/98.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER NEOLD,NIOLD,NCOLD,I,J,K
*** Clean up the entry point list.
       NEOLD=NALGE
       NALGE=0
       ICONS0=-7
       NCOLD=NCONS
       NCONS=-6
       NIOLD=NINS
       NINS=0
*** Loop over the entry points that are to be kept.
       DO 10 I=1,NEOLD
*   But kill constant strings associated with dropped entry points.
       IF(ALGENT(I,2).EQ.0)THEN
            DO 15 J=ALGENT(I,8),ALGENT(I,8)-ALGENT(I,9)+1,-1
            CALL ALGREU(NINT(REG(J)),MODREG(J),1)
15          CONTINUE
            GOTO 10
       ENDIF
*   Shift the constants.
       ICONS0=NCONS-1
       DO 70 J=ALGENT(I,8),ALGENT(I,8)-ALGENT(I,9)+1,-1
       NCONS=NCONS-1
       REG(NCONS)=REG(J)
       MODREG(NCONS)=MODREG(J)
       DO 80 K=ALGENT(I,5),ALGENT(I,5)+ALGENT(I,6)-1
       IF(INS(K,1).EQ.J.AND.INS(K,2).NE.0.AND.INS(K,2).NE.6.AND.
     -      INS(K,2).NE.8.AND.INS(K,2).NE.9)INS(K,1)=NCONS
       IF(INS(K,3).EQ.J.AND.ABS(INS(K,2)).NE.9)INS(K,3)=NCONS
80     CONTINUE
70     CONTINUE
*   Shift the instructions.
       IINS0=NINS+1
       DO 40 J=ALGENT(I,5),ALGENT(I,5)+ALGENT(I,6)-1
       NINS=NINS+1
       DO 50 K=1,4
       INS(NINS,K)=INS(J,K)
50     CONTINUE
       EXEC(NINS)=EXEC(J)
40     CONTINUE
*   Update the entry point record.
       NALGE=NALGE+1
       DO 20 J=1,10
       ALGENT(NALGE,J)=ALGENT(I,J)
20     CONTINUE
       ALGENT(NALGE,5)=IINS0
       ALGENT(NALGE,8)=ICONS0
10     CONTINUE
*** Set suitable starting points for additions.
       ICONS0=NCONS-1
       IINS0=NINS+1
*** Print statistics if requested.
       IF(LDEBUG)WRITE(LUNOUT,'(/''  ++++++ ALGGBC DEBUG   : Garbage'',
     -      '' collection statistics:''//
     -      26X,''Entry points in use: '',I4,'' (was: '',I4,'')''/
     -      26X,''Instructions in use: '',I4,'' (was: '',I4,'')''/
     -      26X,''Constant registers:  '',I4,'' (was: '',I4,'')''/)')
     -      NALGE,NEOLD,NINS,NIOLD,-5-NCONS,-5-NCOLD
*** Reset unused portion of the instruction and constants storage.
       DO 90 I=IINS0,MXINS
       EXEC(I)=.TRUE.
       INS(I,1)=0
       INS(I,2)=0
       INS(I,3)=0
       INS(I,4)=0
90     CONTINUE
       DO 100 I=ICONS0,MXCONS,-1
       REG(I)=0.0
100    CONTINUE
       END
