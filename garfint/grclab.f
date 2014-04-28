CDECK  ID>, GRCLAB.
       SUBROUTINE GRCLAB(NPL,XPL,YPL,FC)
*-----------------------------------------------------------------------
*   GRCLAB - Plots the contour and adds labels if requested.
*   (Last changed on 16/ 5/97.)
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
       LOGICAL XDONE(0:MXGRID,0:MXGRID),YDONE(0:MXGRID,0:MXGRID),
     -      TRANS,CLAB
       REAL GRID(0:MXGRID,0:MXGRID),EPSTRA,EPSGRA,CXMIN,CXMAX,CYMIN,
     -      CYMAX,STINIT,DNTHR,DXGRA,DYGRA
       INTEGER ILOCGR(0:MXGRID,0:MXGRID),NBITER,NNITER,NFC,NGCMAX
       COMMON /CONDAT/ GRID,XDONE,YDONE,ILOCGR,
     -      NBITER,NNITER,EPSTRA,EPSGRA,DXGRA,DYGRA,
     -      STINIT,DNTHR,CXMIN,CXMAX,CYMIN,CYMAX,NFC,NGCMAX,TRANS,CLAB
       REAL XPL(*),YPL(*),WINDOW(4),VIEWPT(4),XBOX(5),YBOX(5),FC,
     -      TXTLEN,CHH,CPX,CPY
       INTEGER NPL,IWK,NTOLD,ITXALH,ITXALV,IERR,IERR1,IERR2,I,NC,
     -      IMID,ITXT
       CHARACTER*20 TEXT
*** Skip label plotting if not requested.
       IF(.NOT.CLAB)THEN
            CALL GPL(NPL,XPL,YPL)
            RETURN
       ENDIF
*** Label plotting, set workstation to 1 (only one workstation).
       IWK=1
       NTOLD=-1
       ITXALH=-1
       ITXALV=-1
*   Transform the curve to NT=0.
       CALL GQCNTN(IERR1,NTOLD)
       CALL GQNT(NTOLD,IERR2,WINDOW,VIEWPT)
       IF(IERR1.NE.0.OR.IERR2.NE.0.OR.WINDOW(1).EQ.WINDOW(2).OR.
     -      WINDOW(3).EQ.WINDOW(4))THEN
            WRITE(10,'(''  !!!!!! GRCLAB WARNING : Window/viewport/nt'',
     -           '' inquiry failed, IERR='',2I3)') IERR1,IERR2
            GOTO 1000
       ENDIF
       DO 10 I=1,NPL
       XPL(I)=(XPL(I)-WINDOW(1))/(WINDOW(2)-WINDOW(1))
       YPL(I)=(YPL(I)-WINDOW(3))/(WINDOW(4)-WINDOW(3))
10     CONTINUE
       CALL GSELNT(0)
*   Set the attributes of the contour labels.
       CALL GRATTS('CONTOUR-LABELS','TEXT')
*   Format the label.
       CALL OUTFMT(FC,2,TEXT,NC,'LEFT')
*   Compute horizontal length of the text.
       CALL GSCHUP(0.0,1.0)
       CALL GQTXAL(IERR,ITXALH,ITXALV)
       IF(IERR.NE.0)THEN
            WRITE(10,'(''  !!!!!! GRCLAB WARNING : Text alignments'',
     -           '' inquiry failed, IERR='',I3)') IERR
            GOTO 1000
       ENDIF
       CALL GSTXAL(2,3)
       CALL GQTXX(IWK,0.5,0.5,TEXT(1:NC),IERR,CPX,CPY,XBOX,YBOX)
       IF(IERR.EQ.0)THEN
            TXTLEN=MAX(MAX(XBOX(1),XBOX(2),XBOX(3),XBOX(4))-
     -           MIN(XBOX(1),XBOX(2),XBOX(3),XBOX(4)),
     -           MAX(YBOX(1),YBOX(2),YBOX(3),YBOX(4))-
     -           MIN(YBOX(1),YBOX(2),YBOX(3),YBOX(4)))
       ELSE
            CALL GQCHW(IERR,CHH)
            IF(IERR.NE.0)CALL GQCHH(IERR,CHH)
            IF(IERR.NE.0)CHH=0.01
            TXTLEN=NC*CHH
       ENDIF
*   Make the space a bit bigger to make the label more legible.
       TXTLEN=TXTLEN*1.1
*   Determine a piece of the curve that will hold the text.
       IMID=NPL/2
       DO 20 I=1,IMID
       IF(IMID-I.LE.0.OR.IMID+I.GT.NPL)GOTO 20
       IF((XPL(IMID-I)-XPL(IMID+I))**2+
     -      (YPL(IMID-I)-YPL(IMID+I))**2.GT.TXTLEN**2)THEN
            ITXT=I
            GOTO 30
       ENDIF
20     CONTINUE
       GOTO 1000
*   Plot the text.
30     CONTINUE
       IF(XPL(IMID+ITXT)-XPL(IMID-ITXT).LT.0.0.AND.
     -      YPL(IMID-ITXT)-YPL(IMID+ITXT).LT.0.0)THEN
            CALL GSCHUP(YPL(IMID+ITXT)-YPL(IMID-ITXT),
     -           XPL(IMID-ITXT)-XPL(IMID+ITXT))
       ELSE
            CALL GSCHUP(YPL(IMID-ITXT)-YPL(IMID+ITXT),
     -           XPL(IMID+ITXT)-XPL(IMID-ITXT))
       ENDIF
       CALL GRTX((XPL(IMID-ITXT)+XPL(IMID+ITXT))/2.0,
     -      (YPL(IMID-ITXT)+YPL(IMID+ITXT))/2.0,TEXT(1:NC))
*   Plot the two line segments.
       IF(IMID-ITXT.GE.2)CALL GPL(IMID-ITXT,XPL,YPL)
       IF(NPL-IMID-ITXT+1.GE.2)CALL GPL(NPL-IMID-ITXT+1,
     -      XPL(IMID+ITXT),YPL(IMID+ITXT))
*   Restore the old situation.
       IF(NTOLD.GE.0)CALL GSELNT(NTOLD)
       IF(ITXALH.GE.0.AND.ITXALV.GE.0)CALL GSTXAL(ITXALH,ITXALV)
       CALL GSCHUP(0.0,1.0)
       RETURN
*** Simple line drawing.
1000   CONTINUE
       CALL GPL(NPL,XPL,YPL)
*   Restore the old situation.
       IF(NTOLD.GE.0)CALL GSELNT(NTOLD)
       IF(ITXALH.GE.0.AND.ITXALV.GE.0)CALL GSTXAL(ITXALH,ITXALV)
       CALL GSCHUP(0.0,1.0)
       END
