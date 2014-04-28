CDECK  ID>, GREBND.
      SUBROUTINE GREBND(N,X,Y1,Y2)
*-----------------------------------------------------------------------
*   GREBND - Plots error bars.
*   (Last changed on 14/12/09.)
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
       INTEGER N,ISIZ(1),IMOD,I,MATSLT,
     -      IFAIL1,IFAIL2,IFAIL3,
     -      IRX,IRY1,IRY2,
     -      ISX,ISY1,ISY2
       REAL X(N),Y1(N),Y2(N)
       EXTERNAL MATSLT
*** Allocate matrix space
       ISIZ(1)=N
       IMOD=2
       CALL MATADM('ALLOCATE',IRX,1,ISIZ,IMOD,IFAIL1)
       CALL MATADM('ALLOCATE',IRY1,1,ISIZ,IMOD,IFAIL2)
       CALL MATADM('ALLOCATE',IRY2,1,ISIZ,IMOD,IFAIL3)
       IF(IFAIL1+IFAIL2+IFAIL3.NE.0)THEN
            PRINT *,' !!!!!! GREBND WARNING : Allocating memory'//
     -           ' failed; error band not plotted.'
            GOTO 1000
       ENDIF
*** Locate the matrices.
       ISX=MATSLT(IRX)
       ISY1=MATSLT(IRY1)
       ISY2=MATSLT(IRY2)
       IF(ISX*ISY1*ISY2.EQ.0)THEN
            PRINT *,' !!!!!! GREBND WARNING : Locating memory'//
     -           ' failed; error band not plotted.'
            GOTO 1000
       ENDIF
*** Copy the data.
       DO 10 I=1,N
       MVEC(MORG(ISX)+I)=X(I)
       MVEC(MORG(ISY1)+I)=Y1(I)
       MVEC(MORG(ISY2)+I)=Y2(I)
10     CONTINUE
*** Plot the error bars.
       CALL MATBND(IRX,IRY1,IRY2)
*** Clean up memory.
1000   CONTINUE
       ISIZ(1)=N
       IMOD=2
       CALL MATADM('DELETE',IRX,1,ISIZ,IMOD,IFAIL1)
       CALL MATADM('DELETE',IRY1,1,ISIZ,IMOD,IFAIL2)
       CALL MATADM('DELETE',IRY2,1,ISIZ,IMOD,IFAIL3)
       END
