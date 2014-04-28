CDECK  ID>, ALGREU.
       SUBROUTINE ALGREU(IREG,IMOD,IUSAGE)
*-----------------------------------------------------------------------
*   ALGREU - Clears storage associated with strings and the like that
*            are being reused.
*   VARIABLES: IUSAGE      : Flag with the same meaning as ARGREF(I,1).
*   (Last changed on 20/ 1/00.)
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
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER IUSAGE,NUSEG,NUSEC,IDUM(1),IREG,IMOD,I,IFAIL
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE ALGREU ///'
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ ALGREU DEBUG   : Reuse'',
     -      '' request for ref='',I5,'', mode='',I2,'' usage='',I2)')
     -      IREG,IMOD,IUSAGE
*** If not String, Histogram or Matrix, simply return.
       IF(IMOD.NE.1.AND.IMOD.NE.4.AND.IMOD.NE.5)THEN
            IMOD=0
            RETURN
       ENDIF
*** Count references from globals.
       NUSEG=0
       DO 10 I=1,NGLB
       IF(GLBMOD(I).EQ.IMOD.AND.NINT(GLBVAL(I)).EQ.IREG)NUSEG=NUSEG+1
10     CONTINUE
*** Count references from constants in active instruction lists.
       NUSEC=0
       DO 20 I=-6,NCONS,-1
       IF(MODREG(I).EQ.IMOD.AND.NINT(REG(I)).EQ.IREG)NUSEC=NUSEC+1
20     CONTINUE
*** Delete the String, Histogram or Matrix if not needed anymore.
       IF((IUSAGE.EQ.0.AND.NUSEG+NUSEC.LE.1).OR.
     -      (IUSAGE.EQ.1.AND.NUSEG+NUSEC.LE.0))THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ ALGREU DEBUG   :'',
     -           '' Deleting, global ref: '',I5,'' const ref: '',I5)')
     -           NUSEG,NUSEC
            IF(IMOD.EQ.1)THEN
                 CALL STRBUF('DELETE',IREG,' ',1,IFAIL)
            ELSEIF(IMOD.EQ.4)THEN
                 CALL HISADM('DELETE',IREG,0,0.0,0.0,.FALSE.,IFAIL)
            ELSEIF(IMOD.EQ.5)THEN
                 CALL MATADM('DELETE',IREG,0,IDUM,0,IFAIL)
            ENDIF
            IMOD=0
       ELSEIF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ ALGREU DEBUG   : Not'',
     -           '' deleting, global ref: '',I5,'' const ref: '',I5)')
     -           NUSEG,NUSEC
       ENDIF
       END
