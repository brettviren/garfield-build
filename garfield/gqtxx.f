CDECK  ID>, GQTXX.
       SUBROUTINE GQTXX(IWK,X,Y,TEXT,IERR,CPX,CPY,XBOX,YBOX)
*-----------------------------------------------------------------------
*   GQTXX  - Returns the text extent, HIGZ version. Currently not able
*            to get the box directly from HIGZ, but try to do something
*            reasonable using the character height.
*   (Last changed on 28/ 1/00.)
*-----------------------------------------------------------------------
       implicit none
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL X,Y,CPX,CPY,XBOX(*),YBOX(*),CHH,CHW,XUP,YUP,XOFF,YOFF,PHI,
     -      XNEW,YNEW
       INTEGER IWK,IERR,ITXALH,ITXALV,I
       CHARACTER*(*) TEXT
*** Try to get some reasonable estimate of the character size.
       CALL GQCHH(IERR,CHH)
       IF(IERR.NE.0)CHH=0.02
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GQTXX  DEBUG   : Height: '',
     -      F10.3,'', ierr='',I5)') CHH,IERR
       CALL GQCHW(IERR,CHW)
       IF(IERR.NE.0)CHW=0.8*CHH
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GQTXX  DEBUG   : Width:  '',
     -      F10.3,'', ierr='',I5)') CHW,IERR
*** Find out what the alignment is like.
       CALL GQTXAL(IERR,ITXALH,ITXALV)
*** Compute from this what the x and y offsets are.
       IF(ITXALH.EQ.2)THEN
            XOFF=0.5*CHW*LEN(TEXT)
       ELSEIF(ITXALH.EQ.3)THEN
            XOFF=CHW*LEN(TEXT)
       ELSE
            XOFF=0
       ENDIF
       IF(ITXALV.EQ.1.OR.ITXALV.EQ.2)THEN
            YOFF=CHH
       ELSEIF(ITXALV.EQ.3)THEN
            YOFF=0.5*CHH
       ELSE
            YOFF=0
       ENDIF
*** Construct a first box.
       XBOX(1)=-XOFF
       XBOX(2)=-XOFF
       XBOX(3)=CHW*LEN(TEXT)-XOFF
       XBOX(4)=CHW*LEN(TEXT)-XOFF
       YBOX(1)=-YOFF-0.2*CHH
       YBOX(2)=CHH-YOFF
       YBOX(3)=CHH-YOFF
       YBOX(4)=-YOFF-0.2*CHH
*** Determine the character up vector.
       CALL GQCHUP(IERR,XUP,YUP)
       IF(IERR.NE.0.OR.XUP**2+YUP**2.LE.0)THEN
            XUP=0
            YUP=1
       ENDIF
       PHI=ATAN2(YUP,XUP)
*** And rotate the box in place, translating it too.
       DO 10 I=1,4
       XNEW=+SIN(PHI)*XBOX(I)+COS(PHI)*YBOX(I)
       YNEW=-COS(PHI)*XBOX(I)+SIN(PHI)*YBOX(I)
       XBOX(I)=XNEW+X
       YBOX(I)=YNEW+Y
10     CONTINUE
*** Definre the concatenation point.
       CPX=XBOX(4)+XOFF
       CPY=YBOX(4)+YOFF
*** And set the error flag to "success".
       IERR=0
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GQTXX  DEBUG   :'',
     -      '' String: "'',A,''"''/
     -      26X,''x-box: '',4F10.3/26X,''y-box: '',4F10.3)')
     -      TEXT,(XBOX(I),I=1,4),(YBOX(I),I=1,4)
       END
