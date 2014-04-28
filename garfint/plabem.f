CDECK  ID>, PLABEM.
       SUBROUTINE PLABEM(IFAIL)
*-----------------------------------------------------------------------
*   PLABEM - Prepares panels for BEM applications: removes the contacts
*            and cuts polygons to rectangles and right-angle triangles.
*   (Last changed on 21/10/10.)
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
       INTEGER NBEM,IREFB1(MXPLAN),NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,
     -      BEMNEW,BEMINV,BEMSLV
       DOUBLE PRECISION BEMQTH,BEMSTH,BEMSSC,BEMTGT,BEMEPA,BEMEPD
       LOGICAL LBDUMP
       COMMON /BEMDAT/ BEMQTH,BEMSSC,BEMSTH,BEMTGT,BEMEPA,BEMEPD,
     -      IREFB1,NBEM,NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,BEMNEW,
     -      BEMINV,BEMSLV,LBDUMP
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       DOUBLE PRECISION XPL1(MXEDGE),YPL1(MXEDGE),ZPL1(MXEDGE),
     -      XPL2(MXEDGE),YPL2(MXEDGE),ZPL2(MXEDGE),
     -      XPL(MXEDGE),YPL(MXEDGE),ZPL(MXEDGE),
     -      EPSANG,EPSXYZ,AN1,BN1,CN1,DN1,AN2,BN2,CN2,DN2,ROT(3,3),ZM,
     -      VLIST(MXPLAN,4),EPSX,EPSY
       INTEGER I,J,K,L,NREF,NPL1,NPL2,NREFO,IREFO(MXPLAN),ITYPO(MXPLAN),
     -      ICOL1,ICOL2,IVOL1,IVOL2,IREF1,REFOLD(MXPLAN),NOLD,M,
     -      IFAIL,IFAIL1,LIST(MXPLAN,4),NLIST,JMIN,NREFOO,IREFOO(MXPLAN)
       LOGICAL MARK(MXPLAN),EQUAL1,EQUAL2,EQUAL3,EQUAL4,CHANGE
*** Assume failure.
       IFAIL=1
*** Establish tolerances.
       EPSANG = BEMEPA
       EPSXYZ = BEMEPD
       CALL EPSSET('SET',EPSXYZ,EPSXYZ,EPSXYZ)
*** Count panels.
       CALL PLABU1('QUERY',NREF,NPL1,XPL1,YPL1,ZPL1,AN1,BN1,CN1,
     -      ICOL1,IVOL1,IFAIL1)
C       print *,' Found ',nref,' panels.'
*   Set the flags.
       DO 60 I=1,NREF
       MARK(I)=.FALSE.
60     CONTINUE
*   Keep track of the existing panels.
       NOLD=0
*** Pick up panels which coincide potentially.
       DO 10 I=1,NREF
*   Fetch one panel.
       CALL PLABU1('READ',I,NPL1,XPL1,YPL1,ZPL1,AN1,BN1,CN1,
     -      ICOL1,IVOL1,IFAIL1)
       IF(IFAIL1.NE.0)GOTO 10
C       print *,' *** Checking from panel ',i,' with ',npl1,' nodes'
*   Keep the references
       NOLD=NOLD+1
       REFOLD(NOLD)=I
*   Skip panels already done.
       IF(MARK(I))GOTO 10
*   Clear buffer 2.
       CALL PLABU2('RESET',IREF1,NPL1,XPL2,YPL2,ZPL2,
     -      AN2,BN2,CN2,DN2,ICOL2,IFAIL1)
*   Establish its norm and offset.
       DN1=AN1*XPL1(1)+BN1*YPL1(1)+CN1*ZPL1(1)
C       print *,' Norm vector: ',AN1,BN1,CN1,DN1
*   Rotation matrix.
       IF(ABS(CN1).LE.ABS(AN1).AND.ABS(CN1).LE.ABS(BN1))THEN
C            print *,' Rotation: removing C'
            ROT(1,1)= BN1/SQRT(AN1*AN1+BN1*BN1)
            ROT(2,1)=-AN1/SQRT(AN1*AN1+BN1*BN1)
            ROT(3,1)= 0.0
       ELSEIF(ABS(BN1).LE.ABS(AN1).AND.ABS(BN1).LE.ABS(CN1))THEN
C            print *,' Rotation: removing B'
            ROT(1,1)= CN1/SQRT(AN1*AN1+CN1*CN1)
            ROT(2,1)= 0.0
            ROT(3,1)=-AN1/SQRT(AN1*AN1+CN1*CN1)
       ELSE
C            print *,' Rotation: removing A'
            ROT(1,1)= 0.0
            ROT(2,1)= CN1/SQRT(BN1*BN1+CN1*CN1)
            ROT(3,1)=-BN1/SQRT(BN1*BN1+CN1*CN1)
       ENDIF
       ROT(1,3)=AN1
       ROT(2,3)=BN1
       ROT(3,3)=CN1
       ROT(1,2)=ROT(2,3)*ROT(3,1)-ROT(3,3)*ROT(2,1)
       ROT(2,2)=ROT(3,3)*ROT(1,1)-ROT(1,3)*ROT(3,1)
       ROT(3,2)=ROT(1,3)*ROT(2,1)-ROT(2,3)*ROT(1,1)
C       do k=1,3
C       print '(3f10.3)',(rot(k,l),l=1,3)
C       enddo
*   Rotate it in place.
       ZM=0
       DO 30 K=1,NPL1
       XPL(K)=ROT(1,1)*XPL1(K)+ROT(2,1)*YPL1(K)+ROT(3,1)*ZPL1(K)
       YPL(K)=ROT(1,2)*XPL1(K)+ROT(2,2)*YPL1(K)+ROT(3,2)*ZPL1(K)
       ZPL(K)=ROT(1,3)*XPL1(K)+ROT(2,3)*YPL1(K)+ROT(3,3)*ZPL1(K)
       ZM=ZM+ZPL(K)
30     CONTINUE
       ZM=ZM/NPL1
*   Store it and keep reference data.
       NLIST=1
       CALL PLABU2('STORE',LIST(NLIST,3),NPL1,XPL,YPL,ZPL,
     -      0.0D0,0.0D0,1.0D0,ZM,ICOL1,IFAIL1)
       LIST(NLIST,1)=IVOL1
       LIST(NLIST,2)=0
       VLIST(NLIST,1)=AN1
       VLIST(NLIST,2)=BN1
       VLIST(NLIST,3)=CN1
       VLIST(NLIST,4)=DN1
*** Pick up all matching planes.
       DO 20 J=I+1,NREF
       IF(MARK(J))GOTO 20
C       print *,' Checking against ',j
       CALL PLABU1('READ',J,NPL2,XPL2,YPL2,ZPL2,AN2,BN2,CN2,
     -      ICOL2,IVOL2,IFAIL1)
*   See whether this matches the first.
       DN2=AN2*XPL2(1)+BN2*YPL2(1)+CN2*ZPL2(1)
C       print *,' Norm vector: ',AN2,BN2,CN2,DN2
C       print *,' inner product = ',AN1*AN2+BN1*BN2+CN1*CN2
C       print *,' plane offset  = ',
C     -      DN1-DN2*(AN1*AN2+BN1*BN2+CN1*CN2)
       IF(ABS(ABS(AN1*AN2+BN1*BN2+CN1*CN2)-1.0).GT.EPSANG.OR.
     -      ABS(DN1-DN2*(AN1*AN2+BN1*BN2+CN1*CN2)).GT.EPSXYZ)
     -      GOTO 20
*   Found a match
C       print *,' Found a matching plane'
       MARK(J)=.TRUE.
*   Rotate this plane too.
       ZM=0
       DO 40 K=1,NPL2
       XPL(K)=ROT(1,1)*XPL2(K)+ROT(2,1)*YPL2(K)+ROT(3,1)*ZPL2(K)
       YPL(K)=ROT(1,2)*XPL2(K)+ROT(2,2)*YPL2(K)+ROT(3,2)*ZPL2(K)
       ZPL(K)=ROT(1,3)*XPL2(K)+ROT(2,3)*YPL2(K)+ROT(3,3)*ZPL2(K)
       ZM=ZM+ZPL(K)
40     CONTINUE
       ZM=ZM/NPL2
*   Store it.
       IF(NLIST+1.GT.MXPLAN)THEN
            PRINT *,' !!!!!! PLABEM WARNING : Too many panels.'
            RETURN
       ENDIF
       NLIST=NLIST+1
       CALL PLABU2('STORE',LIST(NLIST,3),NPL2,XPL,YPL,ZPL,
     -      0.0D0,0.0D0,1.0D0,ZM,ICOL2,IFAIL1)
       LIST(NLIST,1)=IVOL2
       LIST(NLIST,2)=0
       VLIST(NLIST,1)=AN2
       VLIST(NLIST,2)=BN2
       VLIST(NLIST,3)=CN2
       VLIST(NLIST,4)=DN2
20     CONTINUE
C       print *,' Listing of panels before cutting:'
C       do j=1,nlist
C       print *,' Panel ',j,': volumes ',list(j,1),list(j,2)
C       enddo
*** Cut them as long as needed till no contacts remain.
       JMIN=1
100    CONTINUE
       DO 110 J=1,NLIST
       IF(LIST(J,3).LE.0.OR.
     -      J.LT.JMIN.OR.
     -      (LIST(J,1).GT.0.AND.LIST(J,2).GT.0))GOTO 110
       DO 120 K=J+1,NLIST
       IF(LIST(K,3).LE.0.OR.
     -      (LIST(K,1).GT.0.AND.LIST(K,2).GT.0))GOTO 120
C       print *,' Cutting ',list(j,3),list(k,3)
*   Separate contact and non-contact areas.
       CALL PLAOVL(LIST(J,3),LIST(K,3),NREFO,IREFO,ITYPO,
     -      EPSX,EPSY,IFAIL1)
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'('' Input volumes: '',2I4,4X,2I4)')
     -          LIST(J,1),LIST(J,2),LIST(K,1),LIST(K,2)
            WRITE(LUNOUT,'('' Norm 1: '',4F10.3)')
     -          VLIST(J,1),VLIST(J,2),VLIST(J,3),VLIST(J,4)
            WRITE(LUNOUT,'('' Norm 2: '',4F10.3)')
     -          VLIST(K,1),VLIST(K,2),VLIST(K,3),VLIST(K,4)
            WRITE(LUNOUT,'('' Output panels: '',I4)') NREFO
            DO 310 L=1,NREFO
            WRITE(LUNOUT,'('' Primitive '',I2,'' type '',I2)')
     -           IREFO(L),ITYPO(L)
            CALL PLABU2('READ',IREFO(L),NPL2,XPL2,YPL2,ZPL2,
     -                 AN2,BN2,CN2,DN2,ICOL2,IFAIL1)
            DO 320 M=1,NPL2
            WRITE(LUNOUT,'(2X,I2,'': '',3F10.3)')
     -           M,XPL2(M),YPL2(M),ZPL2(M)
320         CONTINUE
310         CONTINUE
       ENDIF
*   If there are just 2 panels, see whether there is a new one
       IF(NREFO.EQ.2)THEN
            CALL PLAEQU(LIST(J,3),IREFO(1),EPSX,EPSY,EQUAL1)
            CALL PLAEQU(LIST(J,3),IREFO(2),EPSX,EPSY,EQUAL2)
            CALL PLAEQU(LIST(K,3),IREFO(1),EPSX,EPSY,EQUAL3)
            CALL PLAEQU(LIST(K,3),IREFO(2),EPSX,EPSY,EQUAL4)
            IF((EQUAL1.OR.EQUAL3).AND.(EQUAL2.OR.EQUAL4))THEN
                 CHANGE=.FALSE.
            ELSE
                 CHANGE=.TRUE.
            ENDIF
       ELSE
            CHANGE=.TRUE.
       ENDIF
C      print *,' Change flag: ',change
*   If there is no change, delete them.
       IF(.NOT.CHANGE)THEN
C            print *,' 2 output planes - removing them'
            CALL PLABU2('DELETE',IREFO(1),NPL2,XPL2,YPL2,ZPL2,
     -           AN2,BN2,CN2,DN2,ICOL2,IFAIL1)
            CALL PLABU2('DELETE',IREFO(2),NPL2,XPL2,YPL2,ZPL2,
     -           AN2,BN2,CN2,DN2,ICOL2,IFAIL1)
*   Buffer overflow
       ELSEIF(NLIST+NREFO.GT.MXPLAN)THEN
            PRINT *,' !!!!!! PLABEM WARNING : Overflow of panel'//
     -           ' buffer; abandoned.'
            IFAIL=1
            RETURN
*   Otherwise delete the existing planes and restart the loops
       ELSE
C            print *,' ',nrefo,' output planes, removing old planes'
            CALL PLABU2('DELETE',LIST(J,3),NPL2,XPL2,YPL2,ZPL2,
     -           AN2,BN2,CN2,DN2,ICOL2,IFAIL1)
            CALL PLABU2('DELETE',LIST(K,3),NPL2,XPL2,YPL2,ZPL2,
     -           AN2,BN2,CN2,DN2,ICOL2,IFAIL1)
            LIST(J,3)=-ABS(LIST(J,3))
            LIST(K,3)=-ABS(LIST(K,3))
            DO 130 L=1,NREFO
            IF(ITYPO(L).EQ.1)THEN
                 LIST(NLIST+L,1)=MAX(LIST(J,1),LIST(J,2))
                 LIST(NLIST+L,2)=0
                 VLIST(NLIST+L,1)=VLIST(J,1)
                 VLIST(NLIST+L,2)=VLIST(J,2)
                 VLIST(NLIST+L,3)=VLIST(J,3)
                 VLIST(NLIST+L,4)=VLIST(J,4)
            ELSEIF(ITYPO(L).EQ.2)THEN
                 LIST(NLIST+L,1)=MAX(LIST(K,1),LIST(K,2))
                 LIST(NLIST+L,2)=0
                 VLIST(NLIST+L,1)=VLIST(K,1)
                 VLIST(NLIST+L,2)=VLIST(K,2)
                 VLIST(NLIST+L,3)=VLIST(K,3)
                 VLIST(NLIST+L,4)=VLIST(K,4)
            ELSE
                 LIST(NLIST+L,1)=MAX(LIST(J,1),LIST(J,2))
                 LIST(NLIST+L,2)=MAX(LIST(K,1),LIST(K,2))
                 VLIST(NLIST+L,1)=VLIST(J,1)
                 VLIST(NLIST+L,2)=VLIST(J,2)
                 VLIST(NLIST+L,3)=VLIST(J,3)
                 VLIST(NLIST+L,4)=VLIST(J,4)
            ENDIF
            LIST(NLIST+L,3)=IREFO(L)
            IF(LDEBUG)WRITE(LUNOUT,'('' Panel '',I3,'' volume flags: '',
     -           2I4,'' NORM: '',4F10.3)')
     -           L,LIST(NLIST+L,1),LIST(NLIST+L,2),
     -           VLIST(NLIST+L,1),VLIST(NLIST+L,2),
     -           VLIST(NLIST+L,3),VLIST(NLIST+L,4)
130         CONTINUE
            NLIST=NLIST+NREFO
            JMIN=J+1
            GOTO 100
       ENDIF
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'('' Listing of panels after cutting:'')')
            DO 330 L=1,NLIST
            IF(LIST(L,3).GT.0)THEN
                 CALL PLABU2('READ',LIST(L,3),NPL2,XPL2,YPL2,ZPL2,
     -                AN2,BN2,CN2,DN2,ICOL2,IFAIL1)
                 WRITE(LUNOUT,'('' Panel '',I3,'': with volumes '',2I4,
     -                '', buf 2 ref = '',I4,'' corners: '',I4)')
     -                L,LIST(L,1),LIST(L,2),LIST(L,3),NPL2
            ENDIF
330         CONTINUE
       ENDIF
120    CONTINUE
110    CONTINUE
*** And rotate the panels back in place.
       DO 200 J=1,NLIST
       IF(LIST(J,3).LE.0)GOTO 200
*   Reduce to rectangles and right-angle triangles
       CALL PLATRC(LIST(J,3),NREFO,IREFO,IFAIL1)
       IF(IFAIL1.NE.0)PRINT *,' !!!!!! PLABEM WARNING : Reduction of'//
     -      ' polygons to rectangles and triangles failed.'
*   Loop over the rectangles and triangles.
       DO 220 L=1,NREFO
       if(.false.)then
*   Improve their quality by further cutting.
            CALL PLATRQ(IREFO(L),NREFOO,IREFOO,IFAIL1)
            DO 230 M=1,NREFOO
*   Fetch
            CALL PLABU2('READ',IREFOO(M),NPL2,XPL2,YPL2,ZPL2,
     -           AN2,BN2,CN2,DN2,ICOL2,IFAIL1)
            IF(IFAIL1.NE.0)PRINT *,' !!!!!! PLABEM WARNING : Failed'//
     -           ' to find a panel.'
*   Rotate.
C            print *,' After rotating back'
            DO 210 K=1,NPL2
            XPL(K)=ROT(1,1)*XPL2(K)+ROT(1,2)*YPL2(K)+ROT(1,3)*ZPL2(K)
            YPL(K)=ROT(2,1)*XPL2(K)+ROT(2,2)*YPL2(K)+ROT(2,3)*ZPL2(K)
            ZPL(K)=ROT(3,1)*XPL2(K)+ROT(3,2)*YPL2(K)+ROT(3,3)*ZPL2(K)
C            print '(3f10.3)',xpl1(k),ypl1(k),zpl1(k)
210         CONTINUE
*   Store.
            CALL PLABU1('STORE',IREF1,NPL2,XPL,YPL,ZPL,AN1,BN1,CN1,
     -           LIST(J,2),LIST(J,1),IFAIL1)
C            print *,' Buf 1 ref = ',iref1,', Buf 2 ref = ',list(j,3),
C     -           ', vol: ',LIST(J,1),LIST(J,2),' length: ',npl2
230          CONTINUE
       else
            CALL PLABU2('READ',IREFO(L),NPL2,XPL2,YPL2,ZPL2,
     -           AN2,BN2,CN2,DN2,ICOL2,IFAIL1)
            IF(IFAIL1.NE.0)PRINT *,' !!!!!! PLABEM WARNING : Failed'//
     -           ' to find a panel.'
*   Rotate.
C            print *,' After rotating back'
            DO 211 K=1,NPL2
            XPL(K)=ROT(1,1)*XPL2(K)+ROT(1,2)*YPL2(K)+ROT(1,3)*ZPL2(K)
            YPL(K)=ROT(2,1)*XPL2(K)+ROT(2,2)*YPL2(K)+ROT(2,3)*ZPL2(K)
            ZPL(K)=ROT(3,1)*XPL2(K)+ROT(3,2)*YPL2(K)+ROT(3,3)*ZPL2(K)
C            print '(3f10.3)',xpl1(k),ypl1(k),zpl1(k)
211         CONTINUE
*   Store.
            CALL PLABU1('STORE',IREF1,NPL2,XPL,YPL,ZPL,
     -           VLIST(J,1),VLIST(J,2),VLIST(J,3),
     -           LIST(J,2),LIST(J,1),IFAIL1)
C            print *,' Buf 1 ref = ',iref1,', Buf 2 ref = ',IREFO(L),
C     -           ', vol: ',LIST(J,1),LIST(J,2),' length: ',npl2
       endif
220    CONTINUE
200    CONTINUE
*   Transfer the new panels.
10     CONTINUE
*** Delete the original panels.
       DO 300 I=1,NOLD
       CALL PLABU1('DELETE',REFOLD(I),NPL2,XPL,YPL,ZPL,
     -      AN2,BN2,CN2,ICOL2,IVOL2,IFAIL1)
C       if(ifail1.ne.0)print *,' Deleting old ',refold(i),' failed.'
300    CONTINUE
*** Seems to have worked.
       IFAIL=0
       END
