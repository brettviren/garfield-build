CDECK  ID>, INPCDO.
       SUBROUTINE INPCDO
*-----------------------------------------------------------------------
*   INPCDO - Cleans up the current DO loop.
*   (Last changed on  1/11/01.)
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
       INTEGER DOREF,IFREF,LINREF,CURLIN,CDOLVL,CIFLVL,TRACDO,TRACIF,
     -      ISTATE,NDOLIN,NLOOP,NIF
       COMMON /DODAT/ LINREF(MXDLIN,8),DOREF(MXDLVL,12),IFREF(MXILVL,5),
     -      TRACDO(0:MXDLVL),TRACIF(0:MXILVL),CURLIN,CDOLVL,CIFLVL,
     -      NDOLIN,NLOOP,NIF,ISTATE
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       INTEGER I,J,NC,IFAIL
       CHARACTER STRING
*** Clean up entry points.
       DO 10 I=1,NLOOP
       IF(DOREF(I,9).GT.0)THEN
            DO 20 J=1,5
            IF(DOREF(I,J).GT.0)CALL ALGCLR(DOREF(I,J))
20          CONTINUE
            IF(DOREF(I,11).GT.0)CALL ALGCLR(DOREF(I,11))
       ELSE
            DO 30 J=3,4
            IF(DOREF(I,J).GT.0)CALL ALGCLR(DOREF(I,J))
30          CONTINUE
       ENDIF
10     CONTINUE
*** Remove the lines from the string buffer and entries for IF's.
       DO 40 I=1,NDOLIN
*   Global statements.
       IF(LINREF(I,1).EQ.21.AND.LINREF(I,8).GT.0)
     -      CALL ALGCLR(LINREF(I,8))
*   Call statements.
       IF(LINREF(I,1).EQ.22.AND.LINREF(I,8).GT.0)
     -      CALL ALGCLR(LINREF(I,8))
*   Leading IF ... THEN ... parts.
       IF(LINREF(I,4).GT.0)CALL ALGCLR(LINREF(I,4))
*   Strings associated with instructions.
       CALL STRBUF('DELETE',LINREF(I,2),STRING,NC,IFAIL)
       IF(IFAIL.NE.0)PRINT *,' !!!!!! INPCDO WARNING : Unable to'//
     -      ' delete a line from the string buffer; bug - no problem.'
40     CONTINUE
*** Reset the number of DO lines to disallow reexecution.
       NDOLIN=-1
       NLOOP=-1
       ISTATE=-1
       END
