CDECK  ID>, BEMBU1.
       SUBROUTINE BEMBU1(ACTION,IREF,NPL,XPL,YPL,ZPL,APL,BPL,CPL,
     -      IVOL1,IVOL2,IFAIL)
*-----------------------------------------------------------------------
*   BEMBU1 - Stores planes of surfaces. Almost a replica of PLABU1.
*   (Last changed on  5/ 4/10.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       DOUBLE PRECISION XBUF(MXPOIN),YBUF(MXPOIN),ZBUF(MXPOIN),
     -      ABUF(MXPLAN),BBUF(MXPLAN),CBUF(MXPLAN),
     -      XPL(MXEDGE),YPL(MXEDGE),ZPL(MXEDGE),APL,BPL,CPL
C       double precision dpl
       INTEGER ICBUF(MXPLAN),IVBUF(MXPLAN),NBUF(MXPLAN),ISTART(MXPLAN),
     -      ICURR,IND(MXPLAN),II,IREF,NPL,IVOL1,IVOL2,IFAIL,I,J
       LOGICAL USE(MXPLAN)
       CHARACTER*(*) ACTION
       SAVE NBUF,XBUF,YBUF,ZBUF,ABUF,BBUF,CBUF,IVBUF,ICBUF,
     -      ISTART,ICURR,USE
       DATA ICURR/0/,USE/MXPLAN*.FALSE./,ISTART/MXPLAN*-1/
*** Assume failure.
       IFAIL=1
*** Store a new plane.
       IF(ACTION.EQ.'STORE')THEN
*   Basic check on the data.
            IF(NPL.LT.0.OR.NPL.GT.MXEDGE)THEN
                 PRINT *,' !!!!!! BEMBU1 WARNING : Number of points'//
     -                ' on polygon < 0 or > MXEDGE ; not stored.'
                 RETURN
            ENDIF
*   See whether there is a free slot.
            IREF=0
            DO 10 I=1,MXPLAN
            IF(.NOT.USE(I))THEN
                 IREF=I
                 GOTO 20
            ENDIF
10          CONTINUE
            PRINT *,' !!!!!! BEMBU1 WARNING : No room to store'//
     -           ' further polygons ; increase MXPLAN.'
            RETURN
20          CONTINUE
*   See whether there is free space, garbage collect if not.
            IF(ICURR+NPL.GT.MXPOIN)THEN
C                 CALL SORTZV(ISTART,IND,MXPLAN,-1,0,0)
                 DO 15 I=1,MXPLAN
                 IND(I)=I
15               CONTINUE
                 CALL SORTTI(ISTART,IND,MXPLAN)
                 ICURR=0
                 DO 30 II=1,MXPLAN
                 I=IND(II)
                 IF(ISTART(I).LT.0.OR..NOT.USE(I))GOTO 30
                 DO 40 J=1,NBUF(I)
                 XBUF(ICURR+J)=XBUF(ISTART(I)+J)
                 YBUF(ICURR+J)=YBUF(ISTART(I)+J)
                 ZBUF(ICURR+J)=ZBUF(ISTART(I)+J)
40               CONTINUE
                 ISTART(I)=ICURR
                 ICURR=ICURR+NBUF(I)
30               CONTINUE
            ENDIF
*   See whether there now is enough space.
            IF(ICURR+NPL.GT.MXPOIN)THEN
                 PRINT *,' !!!!!! BEMBU1 WARNING : No room to store'//
     -                ' further points; increase MXPOIN.'
                 RETURN
            ENDIF
*   Store the polygon.
            ISTART(IREF)=ICURR
            USE(IREF)=.TRUE.
            NBUF(IREF)=NPL
            ABUF(IREF)=APL
            BBUF(IREF)=BPL
            CBUF(IREF)=CPL
            ICBUF(IREF)=IVOL1
            IVBUF(IREF)=IVOL2
C            dpl=0
            DO 50 I=1,NPL
            XBUF(ISTART(IREF)+I)=XPL(I)
            YBUF(ISTART(IREF)+I)=YPL(I)
            ZBUF(ISTART(IREF)+I)=ZPL(I)
C            dpl=dpl+apl*xpl(i)+bpl*ypl(i)+cpl*zpl(i)
50          CONTINUE
C            dpl=dpl/npl
C            do i=1,npl
C            if(abs(dpl-xpl(i)*apl-ypl(i)*bpl-zpl(i)*cpl).gt.1e-4)then
C                 print *,' BEMBU1 Offset Error: '
C                 print *,' Point:   ',xpl(i),ypl(i),zpl(i)
C                 print *,' Error:   ',dpl-xpl(i)*apl-ypl(i)*bpl-
C     -                cpl*zpl(i)
C            endif
C            enddo
            ICURR=ICURR+NPL
*** Read back a plane.
       ELSEIF(ACTION.EQ.'READ')THEN
*   Basic checks of the index.
            IF(IREF.LT.1.OR.IREF.GT.MXPLAN)THEN
                 PRINT *,' !!!!!! BEMBU1 WARNING : Polygon reference'//
     -                ' number out of range; not read.'
                 RETURN
            ELSEIF(.NOT.USE(IREF))THEN
                 IF(LDEBUG)PRINT *,' ++++++ BEMBU1 DEBUG   :'//
     -                ' Requested polygon is not defined; not read.'
                 RETURN
            ENDIF
*   Return the polygon.
            DO 100 I=1,NBUF(IREF)
            XPL(I)=XBUF(ISTART(IREF)+I)
            YPL(I)=YBUF(ISTART(IREF)+I)
            ZPL(I)=ZBUF(ISTART(IREF)+I)
100         CONTINUE
            APL=ABUF(IREF)
            BPL=BBUF(IREF)
            CPL=CBUF(IREF)
            IVOL1=ICBUF(IREF)
            IVOL2=IVBUF(IREF)
            NPL=NBUF(IREF)
*** Delete a plane.
       ELSEIF(ACTION.EQ.'DELETE')THEN
*   Basic checks of the index.
            IF(IREF.LT.1.OR.IREF.GT.MXPLAN)THEN
                 PRINT *,' !!!!!! BEMBU1 WARNING : Polygon reference'//
     -                ' number out of range; not deleted.'
                 RETURN
            ELSEIF(.NOT.USE(IREF))THEN
                 PRINT *,' ------ BEMBU1 MESSAGE : Requested polygon'//
     -                ' is currently not defined.'
                 RETURN
            ENDIF
*   Delete the polygon.
            USE(IREF)=.FALSE.
            ISTART(IREF)=-1
*** Reset the buffer.
       ELSEIF(ACTION.EQ.'RESET'.OR.ACTION.EQ.'INITIALISE')THEN
            ICURR=0
            DO 200 I=1,MXPLAN
            NBUF(I)=0
            USE(I)=.FALSE.
            ISTART(I)=-1
200         CONTINUE
*** List the buffer.
       ELSEIF(ACTION.EQ.'LIST'.OR.ACTION.EQ.'PRINT')THEN
            DO 300 I=1,MXPLAN
            IF(USE(I))THEN
                 WRITE(LUNOUT,'(2X,''Polygon '',I4,'' is stored '',
     -                '' from '',I4)') I,ISTART(I)
                 WRITE(LUNOUT,'(2X,''Colour index:     '',I5)') ICBUF(I)
                 WRITE(LUNOUT,'(2X,''Volume index:     '',I5)') IVBUF(I)
                 WRITE(LUNOUT,'(2X,''Plane parameters: '',3E15.8)')
     -                ABUF(I),BBUF(I),CBUF(I)
                 WRITE(LUNOUT,'(2X,''Number of points: '',I5)')
     -                NBUF(I)
                 DO 310 J=1,NBUF(I)
                 WRITE(LUNOUT,'(10X,3E15.8)') XBUF(ISTART(I)+J),
     -                YBUF(ISTART(I)+J),ZBUF(ISTART(I)+J)
310              CONTINUE
            ENDIF
300         CONTINUE
*** Query of maximum numbers.
       ELSEIF(ACTION.EQ.'QUERY')THEN
            DO 400 I=MXPLAN,1,-1
            IF(USE(I))THEN
                 IREF=I
                 GOTO 410
            ENDIF
400         CONTINUE
            IREF=0
410         CONTINUE
*** Other actions not known.
       ELSE
            PRINT *,' !!!!!! BEMBU1 WARNING : Unknown action ',
     -           ACTION,' received ; nothing done.'
            IFAIL=1
            RETURN
       ENDIF
*** Seems to have worked.
       IFAIL=0
       END
