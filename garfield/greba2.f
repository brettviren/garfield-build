CDECK  ID>, GREBA2.
      SUBROUTINE GREBA2(N,X,Y,EX1,EX2,EY1,EY2,TYPE,SIZE)
*-----------------------------------------------------------------------
*   GREBA2 - Plots error bars.
*   (Last changed on 15/ 7/09.)
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
       INTEGER N,ISIZ(1),IMOD,I,MATSLT,
     -      IFAIL1,IFAIL2,IFAIL3,IFAIL4,IFAIL5,IFAIL6,
     -      IRX,IRY,IREX1,IREX2,IREY1,IREY2,
     -      ISX,ISY,ISEX1,ISEX2,ISEY1,ISEY2
C       REAL X(N),Y(N),EX1(N),EX2(N),EY1(N),EY2(N),SIZE
       DOUBLE PRECISION X(N),Y(N),EX1(N),EX2(N),EY1(N),EY2(N),SIZE
       CHARACTER*(*) TYPE
       EXTERNAL MATSLT
*** Allocate matrix space
       ISIZ(1)=N
       IMOD=2
       CALL MATADM('ALLOCATE',IRX,1,ISIZ,IMOD,IFAIL1)
       CALL MATADM('ALLOCATE',IRY,1,ISIZ,IMOD,IFAIL2)
       CALL MATADM('ALLOCATE',IREX1,1,ISIZ,IMOD,IFAIL3)
       CALL MATADM('ALLOCATE',IREX2,1,ISIZ,IMOD,IFAIL4)
       CALL MATADM('ALLOCATE',IREY1,1,ISIZ,IMOD,IFAIL5)
       CALL MATADM('ALLOCATE',IREY2,1,ISIZ,IMOD,IFAIL6)
       IF(IFAIL1+IFAIL2+IFAIL3+IFAIL4+IFAIL5+IFAIL6.NE.0)THEN
            PRINT *,' !!!!!! GREBA2 WARNING : Allocating memory'//
     -           ' failed; error bars not plotted.'
            GOTO 1000
       ENDIF
*** Locate the matrices.
       ISX=MATSLT(IRX)
       ISY=MATSLT(IRY)
       ISEX1=MATSLT(IREX1)
       ISEY1=MATSLT(IREY1)
       ISEX2=MATSLT(IREX2)
       ISEY2=MATSLT(IREY2)
       IF(ISX*ISY*ISEX1*ISEX2*ISEY1*ISEY2.EQ.0)THEN
            PRINT *,' !!!!!! GREBA2 WARNING : Locating memory'//
     -           ' failed; error bars not plotted.'
            GOTO 1000
       ENDIF
*** Copy the data.
       DO 10 I=1,N
       MVEC(MORG(ISX)+I)=X(I)
       MVEC(MORG(ISY)+I)=Y(I)
       MVEC(MORG(ISEX1)+I)=EX1(I)
       MVEC(MORG(ISEX2)+I)=EX2(I)
       MVEC(MORG(ISEY1)+I)=EY1(I)
       MVEC(MORG(ISEY2)+I)=EY2(I)
10     CONTINUE
*** Plot the error bars.
       CALL MATERR(IRX,IRY,IREX1,IREY1,IREX2,IREY2,TYPE,REAL(SIZE))
*** Clean up memory.
1000   CONTINUE
       ISIZ(1)=N
       IMOD=2
       CALL MATADM('DELETE',IRX,1,ISIZ,IMOD,IFAIL1)
       CALL MATADM('DELETE',IRY,1,ISIZ,IMOD,IFAIL2)
       CALL MATADM('DELETE',IREX1,1,ISIZ,IMOD,IFAIL3)
       CALL MATADM('DELETE',IREX2,1,ISIZ,IMOD,IFAIL4)
       CALL MATADM('DELETE',IREY1,1,ISIZ,IMOD,IFAIL5)
       CALL MATADM('DELETE',IREY2,1,ISIZ,IMOD,IFAIL6)
       END
