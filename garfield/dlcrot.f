CDECK  ID>, DLCROT.
       SUBROUTINE DLCROT(X0,Y0,Z0,VD,RVC,RMC,RVM,IFAIL)
*-----------------------------------------------------------------------
*   DLCROT - Computes the rotation matrices at a given point.
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
       DOUBLE PRECISION RVC(3,3),RMC(3,3),RVM(3,3),X0,Y0,Z0,VD(3),RNORM
       REAL EX,EY,EZ,ETOT,VOLT,BX,BY,BZ,BTOT
       INTEGER ILOC,I,J,K,IFAIL
*** Set the failure flag.
       IFAIL=1
*** Rotation matrix from "c" to "v" frame, axis 1: effective velocity.
       RVC(1,1)=VD(1)
       RVC(1,2)=VD(2)
       RVC(1,3)=VD(3)
       RNORM=SQRT(RVC(1,1)**2+RVC(1,2)**2+RVC(1,3)**2)
       IF(RNORM.LE.0)THEN
            PRINT *,' !!!!!! DLCROT WARNING : Found a zero norm when'//
     -           ' computing Rvc; no rotation matrices returned.'
            RETURN
       ENDIF
       RVC(1,1)=RVC(1,1)/RNORM
       RVC(1,2)=RVC(1,2)/RNORM
       RVC(1,3)=RVC(1,3)/RNORM
*   Axis 2: orthogonal in the 2 largest components.
       IF(ABS(VD(1)).GE.ABS(VD(3)).AND.ABS(VD(2)).GE.ABS(VD(3)))THEN
            RVC(2,1)=-VD(2)
            RVC(2,2)=VD(1)
            RVC(2,3)=0
       ELSEIF(ABS(VD(1)).GE.ABS(VD(2)).AND.ABS(VD(3)).GE.ABS(VD(2)))THEN
            RVC(2,1)=-VD(3)
            RVC(2,2)=0
            RVC(2,3)=VD(1)
       ELSE
            RVC(2,1)=0
            RVC(2,2)=VD(3)
            RVC(2,3)=-VD(2)
       ENDIF
       RNORM=SQRT(RVC(2,1)**2+RVC(2,2)**2+RVC(2,3)**2)
       IF(RNORM.LE.0)THEN
            PRINT *,' !!!!!! DLCROT WARNING : Found a zero norm when'//
     -           ' computing Rvc; no rotation matrices returned.'
            RETURN
       ENDIF
       RVC(2,1)=RVC(2,1)/RNORM
       RVC(2,2)=RVC(2,2)/RNORM
       RVC(2,3)=RVC(2,3)/RNORM
*   Axis 3: vectorial product of axes 1 and 2.
       RVC(3,1)=RVC(1,2)*RVC(2,3)-RVC(1,3)*RVC(2,2)
       RVC(3,2)=RVC(1,3)*RVC(2,1)-RVC(1,1)*RVC(2,3)
       RVC(3,3)=RVC(1,1)*RVC(2,2)-RVC(1,2)*RVC(2,1)
       RNORM=SQRT(RVC(3,1)**2+RVC(3,2)**2+RVC(3,3)**2)
       IF(RNORM.LE.0)THEN
            PRINT *,' !!!!!! DLCROT WARNING : Found a zero norm when'//
     -           ' computing Rvc; no rotation matrices returned.'
            RETURN
       ENDIF
       RVC(3,1)=RVC(3,1)/RNORM
       RVC(3,2)=RVC(3,2)/RNORM
       RVC(3,3)=RVC(3,3)/RNORM
*** Compute the fields.
       CALL EFIELD(REAL(X0),REAL(Y0),REAL(Z0),EX,EY,EZ,ETOT,VOLT,0,ILOC)
       CALL BFIELD(REAL(X0),REAL(Y0),REAL(Z0),BX,BY,BZ,BTOT)
*** Rotation matrix from "c" frame to "m" frame, axis 1: E
       RMC(1,1)=EX
       RMC(1,2)=EY
       RMC(1,3)=EZ
       RNORM=SQRT(RMC(1,1)**2+RMC(1,2)**2+RMC(1,3)**2)
       IF(RNORM.LE.0)THEN
            PRINT *,' !!!!!! DLCROT WARNING : Found a zero norm when'//
     -           ' computing Rmc; no rotation matrices returned.'
            RETURN
       ENDIF
       RMC(1,1)=RMC(1,1)/RNORM
       RMC(1,2)=RMC(1,2)/RNORM
       RMC(1,3)=RMC(1,3)/RNORM
*   Axis 2: ExB
       IF(BTOT.GT.0.AND.ABS(EX*BX+EY*BY+EZ*BZ).LT.0.99*ETOT*BTOT)THEN
            RMC(2,1)=EY*BZ-EZ*BY
            RMC(2,2)=EZ*BX-EX*BZ
            RMC(2,3)=EX*BY-EY*BX
       ELSEIF(ABS(EX).GE.ABS(EZ).AND.ABS(EY).GE.ABS(EZ))THEN
            RMC(2,1)=-EY
            RMC(2,2)=EX
            RMC(2,3)=0
       ELSEIF(ABS(EX).GE.ABS(EY).AND.ABS(EZ).GE.ABS(EY))THEN
            RMC(2,1)=-EZ
            RMC(2,2)=0
            RMC(2,3)=EX
       ELSE
            RMC(3,1)=0
            RMC(3,2)=EZ
            RMC(3,3)=-EY
       ENDIF
       RNORM=SQRT(RMC(2,1)**2+RMC(2,2)**2+RMC(2,3)**2)
       IF(RNORM.LE.0)THEN
            PRINT *,' !!!!!! DLCROT WARNING : Found a zero norm when'//
     -           ' computing Rmc; no rotation matrices returned.'
            RETURN
       ENDIF
       RMC(2,1)=RMC(2,1)/RNORM
       RMC(2,2)=RMC(2,2)/RNORM
       RMC(2,3)=RMC(2,3)/RNORM
*   Axis 3: E-Transverse part of B, obtained by vectorial product.
       RMC(3,1)=RMC(1,2)*RMC(2,3)-RMC(1,3)*RMC(2,2)
       RMC(3,2)=RMC(1,3)*RMC(2,1)-RMC(1,1)*RMC(2,3)
       RMC(3,3)=RMC(1,1)*RMC(2,2)-RMC(1,2)*RMC(2,1)
       RNORM=SQRT(RMC(3,1)**2+RMC(3,2)**2+RMC(3,3)**2)
       IF(RNORM.LE.0)THEN
            PRINT *,' !!!!!! DLCROT WARNING : Found a zero norm when'//
     -           ' computing Rmc; no rotation matrices returned.'
            RETURN
       ENDIF
       RMC(3,1)=RMC(3,1)/RNORM
       RMC(3,2)=RMC(3,2)/RNORM
       RMC(3,3)=RMC(3,3)/RNORM
*** Rotation from "m" to "v" frame.
       DO 30 I=1,3
       DO 40 J=1,3
       RVM(I,J)=0
       DO 50 K=1,3
       RVM(I,J)=RVM(I,J)+RVC(I,K)*RMC(J,K)
50     CONTINUE
40     CONTINUE
30     CONTINUE
*** Seems to have worked.
       IFAIL=0
       END
