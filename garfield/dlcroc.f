CDECK  ID>, DLCROC.
       SUBROUTINE DLCROC(X1,Y1,Z1,Q,ITYPE,IRRVC,IRRMC,IRRVM,IFAIL)
*-----------------------------------------------------------------------
*   DLCROC - Debugging interface to DLCROT
*   (Last changed on 18/ 6/02.)
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
       DOUBLE PRECISION RVC(3,3),RMC(3,3),RVM(3,3),X0,Y0,Z0,VD(3)
       REAL X1,Y1,Z1,Q
       INTEGER ITYPE,ISRVC,ISRMC,ISRVM,IRRVC,IRRMC,IRRVM,IFAIL,ILOC,
     -      IFAIL1,IFAIL2,IFAIL3,ISIZ(2),MATSLT,I,J
       EXTERNAL MATSLT
*** Initial return code.
       IFAIL=1
*** Convert location to double precision.
       X0=DBLE(X1)
       Y0=DBLE(Y1)
       Z0=DBLE(Z1)
*** Compute the velocity vector.
       CALL DLCVEL(X0,Y0,Z0,VD,Q,ITYPE,ILOC)
*** Compute the matrices.
       CALL DLCROT(X0,Y0,Z0,VD,RVC,RMC,RVM,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! DLCROC WARNING : Error computing the'//
     -           ' rotation matrices; matrices not returned.'
            RETURN
       ENDIF
*** Book space for the matrices.
       ISIZ(1)=3
       ISIZ(2)=3
       CALL MATADM('ALLOCATE',IRRVC,2,ISIZ,2,IFAIL1)
       CALL MATADM('ALLOCATE',IRRMC,2,ISIZ,2,IFAIL2)
       CALL MATADM('ALLOCATE',IRRVM,2,ISIZ,2,IFAIL3)
       IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0)THEN
            PRINT *,' !!!!!! DLCROC WARNING : Unable to locate the'//
     -           ' rotation matrices; matrices not returned.'
            RETURN
       ENDIF
*** Locate the matrices.
       ISRVC=MATSLT(IRRVC)
       ISRMC=MATSLT(IRRMC)
       ISRVM=MATSLT(IRRVM)
*** Ensure the references exist.
       IF(ISRVC.LE.0.OR.ISRMC.LE.0.OR.ISRVM.LE.0)THEN
            PRINT *,' !!!!!! DLCROC WARNING : Unable to locate the'//
     -           ' rotation matrices; matrices not returned.'
            RETURN
       ENDIF
*** Transfer the matrices.
       DO 10 I=1,3
       DO 20 J=1,3
       MVEC(MORG(ISRVC)+3*(I-1)+J)=REAL(RVC(I,J))
       MVEC(MORG(ISRMC)+3*(I-1)+J)=REAL(RMC(I,J))
       MVEC(MORG(ISRVM)+3*(I-1)+J)=REAL(RVM(I,J))
20     CONTINUE
10     CONTINUE
*** Seems to have worked.
       IFAIL=0
       END
