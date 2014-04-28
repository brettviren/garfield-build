CDECK  ID>, MATADM.
       SUBROUTINE MATADM(ACTION,IREF,NDIM,IDIM,IMOD,IFAIL)
*-----------------------------------------------------------------------
*   MATADM - Takes care of matrix booking.
*   (Last changed on 17/ 1/12.)
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
       CHARACTER*(*) ACTION
       CHARACTER*78 STRING
       CHARACTER*20 STRAUX
       CHARACTER*10 TYPE,NAME
       INTEGER IREF,NDIM,IDIM(*),IMOD,IFAIL,NLEN,ISLOT,ILAST,ISTART,
     -      IFREE,NFREE,INEW,IORG,NUSED,I,J,NC,NCAUX
*** Allocate a new matrix.
       IF(ACTION.EQ.'ALLOCATE')THEN
**  Assign a provision reference in case of error.
            IREF=0
**  Set a provisional error flag.
            IFAIL=1
**  Check the number of dimensions.
            IF(NDIM.GT.MXMDIM)THEN
                 PRINT *,' !!!!!! MATADM WARNING : Matrix has more'//
     -                ' than MXMDIM dimensions; matrix not booked.'
                 RETURN
            ENDIF
**  See how large the new matrix is.
            NLEN=1
            DO 10 I=1,NDIM
            IF(IDIM(I).LE.0)THEN
                 PRINT *,' !!!!!! MATADM WARNING : Dimension ',I,' of',
     -                ' the matrix is non-positive; matrix not booked.'
                 RETURN
            ENDIF
            NLEN=NLEN*IDIM(I)
10          CONTINUE
**  Debugging output.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MATADM DEBUG   :'',
     -           '' Booking matrix, length '',I5,'', dimension '',I5,
     -           '', mode '',I1,''.'')') NLEN,NDIM,IMOD
**  See whether we've space without garbage collect.
            ILAST=0
            IFREE=0
            DO 20 I=1,MXMAT+1
*   If slot free, register and find next free slot.
            IF(MREF(I).EQ.0)THEN
                 IF(IFREE.EQ.0)IFREE=I
*   Sufficient space ? Try to get a slot with it.
            ELSEIF(MORG(I)-ILAST.GE.NLEN)THEN
                 IF(IFREE.NE.0)THEN
                      ISLOT=IFREE
                      ISTART=ILAST
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,''Fits in slot '',
     -                     I5,'' with origin '',I5,''.'')')
     -                     ISLOT,ISTART
                      GOTO 300
                 ELSE
                      ISLOT=I-1
                      ISTART=ILAST
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,''Trying to put'',
     -                     '' in slot '',I5,'' at origin '',I5,''.'')')
     -                     ISLOT,ISTART
                      GOTO 100
                 ENDIF
*   Not enough space ? Re-start searching from here.
            ELSE
                 ILAST=MORG(I)+MLEN(I)
                 IFREE=0
            ENDIF
20          CONTINUE
**  If we get here, there is no free space without garbage collect.
            GOTO 200
**  Resume here is there is free space, but no slot in the right place.
100         CONTINUE
*   Eliminate empty entries below the slot to be assigned.
            IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -           ''Removing empty slots below target slot.'')')
            INEW=0
            DO 30 I=1,ISLOT
            IF(MREF(I).NE.0)THEN
                 INEW=INEW+1
                 MREF(INEW)=MREF(I)
                 MORG(INEW)=MORG(I)
                 DO 40 J=1,MXMDIM
                 MSIZ(INEW,J)=MSIZ(I,J)
40               CONTINUE
                 MDIM(INEW)=MDIM(I)
                 MLEN(INEW)=MLEN(I)
                 MMOD(INEW)=MMOD(I)
                 IF(I.NE.INEW)MREF(I)=0
            ENDIF
30          CONTINUE
*   Is there a free slot now ?
            IF(INEW.LT.ISLOT)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''Fits in slot '',
     -                I5,'' with origin '',I5,''.'')')
     -                ISLOT,ISTART
                 GOTO 300
            ENDIF
*   Still no free slot, try to get the next higher slot.
            IF(ISLOT+1.LE.MXMAT)THEN
                 ISLOT=ISLOT+1
            ELSE
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''Unable to get place'',
     -                '' by shifting, trying garbage collect.'')')
                 GOTO 200
            ENDIF
*   And move the pointers ahead of the slot up.
            INEW=MXMAT+1
            IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -           ''Removing empty slots above target slot.'')')
            DO 50 I=MXMAT,ISLOT,-1
            IF(MREF(I).NE.0)THEN
                 INEW=INEW-1
                 MREF(INEW)=MREF(I)
                 MORG(INEW)=MORG(I)
                 DO 60 J=1,MXMDIM
                 MSIZ(INEW,J)=MSIZ(I,J)
60               CONTINUE
                 MDIM(INEW)=MDIM(I)
                 MLEN(INEW)=MLEN(I)
                 MMOD(INEW)=MMOD(I)
                 IF(I.NE.INEW)MREF(I)=0
            ENDIF
50          CONTINUE
*   Is there a free slot now ?
            IF(ISLOT.LT.INEW)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''Fits in slot '',
     -                I5,'' with origin '',I5,''.'')')
     -                ISLOT,ISTART
                 GOTO 300
            ENDIF
**  If all failed, try a garbage collect.
200         CONTINUE
            INEW=0
            IORG=0
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Garbage collection.'')')
            DO 70 I=1,MXMAT
*   Skip empty matrices.
            IF(MREF(I).EQ.0)GOTO 70
*   Copy the matrix itself.
            DO 90 J=1,MLEN(I)
            MVEC(IORG+J)=MVEC(MORG(I)+J)
90          CONTINUE
*   Increment matrix counter.
            INEW=INEW+1
*   Copy the reference information.
            MREF(INEW)=MREF(I)
            MORG(INEW)=IORG
            IORG=IORG+MLEN(I)
            DO 80 J=1,MXMDIM
            MSIZ(INEW,J)=MSIZ(I,J)
80          CONTINUE
            MDIM(INEW)=MDIM(I)
            MLEN(INEW)=MLEN(I)
            MMOD(INEW)=MMOD(I)
70          CONTINUE
*   Reset the pointers for the rest of the list.
            DO 110 I=INEW+1,MXMAT
            MREF(I)=0
            MORG(I)=0
            MLEN(I)=0
            MDIM(I)=0
            MMOD(I)=0
            DO 120 J=1,MXMDIM
            MSIZ(I,J)=0
120         CONTINUE
110         CONTINUE
*   Is there a free slot ?
            IF(INEW.GE.MXMAT.OR.INEW.LE.0)THEN
                 PRINT *,' !!!!!! MATADM WARNING : No free slot'//
     -                ' found; matrix not booked.'
                 RETURN
            ENDIF
*   Is there enough space now ?
            IF(MORG(INEW)+MLEN(INEW)+NLEN.LE.MXEMAT)THEN
                 ISLOT=INEW+1
                 ISTART=MORG(INEW)+MLEN(INEW)
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''Fits in slot '',
     -                I5,'' with origin '',I5,''.'')')
     -                ISLOT,ISTART
                 GOTO 300
            ENDIF
*   Not enough room.
            PRINT *,' !!!!!! MATADM WARNING : Not enough matrix'//
     -           ' space; matrix not booked.'
            RETURN
**  We got a slot with enough space, save matrix information.
300         CONTINUE
            NREFL=NREFL+1
            IF (NINT(REAL(NREFL)).EQ.NINT(REAL(NREFL+1)))THEN
                 PRINT *,' ###### MATADM ERROR   : Reached end of'//
     -                ' reference addressing space.'
                 IF(JFAIL.EQ.2)THEN
                      PRINT *,'                         The matrix'//
     -                     ' has not been booked.'
                 ELSEIF(JFAIL.EQ.3)THEN
                      PRINT *,'                         Program'//
     -                     'terminated because of the above.'
                      CALL QUIT
                 ENDIF
            ENDIF
            MREF(ISLOT)=NREFL
            IREF=NREFL
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Assigning reference '',I5)')
     -           IREF
            MDIM(ISLOT)=NDIM
            MORG(ISLOT)=ISTART
            MMOD(ISLOT)=IMOD
            MLEN(ISLOT)=NLEN
            DO 130 I=1,NDIM
            MSIZ(ISLOT,I)=IDIM(I)
130         CONTINUE
*** Initialise the matrix.
            DO 140 I=1,MLEN(ISLOT)
            MVEC(MORG(ISLOT)+I)=REAL(I)
140         CONTINUE
**  Debugging output.
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Matrix allocation done.'')')
**  Remember that this worked.
            IFAIL=0
*** Release an allocated matrix.
       ELSEIF(ACTION.EQ.'DELETE')THEN
*   Check whether there is a global associated with this matrix.
            DO 505 J=1,NGLB
            IF(GLBMOD(J).EQ.5.AND.NINT(GLBVAL(J)).EQ.IREF)
     -           GLBMOD(J)=0
505         CONTINUE
*   Locate the matrix.
            DO 500 I=1,MXMAT
            IF(MREF(I).EQ.IREF)THEN
                 MREF(I)=0
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''Matrix with'',
     -                '' reference '',I5,'' cleared.'')') IREF
                 IFAIL=0
                 RETURN
            ENDIF
500         CONTINUE
*   Warn if the matrix is not found.
            PRINT *,' !!!!!! MATADM WARNING : Matrix to be deleted'//
     -           ' has not been found.'
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Reference: '',I5)') IREF
            IFAIL=1
*** List of matrices.
       ELSEIF(ACTION.EQ.'LIST')THEN
*   Print a header.
            WRITE(LUNOUT,'(/''  OVERVIEW OF EXISTING MATRICES''//
     -           ''  Reference n-Dim Type       Global    '',
     -           '' Dimensions ... ''/)')
*   Keep track of free space and number of matrices in use.
            NFREE=0
            ILAST=0
            NUSED=0
*   Loop over the matrices.
            DO 700 I=1,MXMAT
            IF(MREF(I).EQ.0)THEN
                 NFREE=NFREE+1
            ELSE
                 NUSED=NUSED+1
                 IF(NFREE.GT.0.OR.MORG(I).NE.ILAST)THEN
                      STRING(1:1)='('
                      NC=1
                      CALL OUTFMT(REAL(NFREE),2,STRAUX,NCAUX,'LEFT')
                      STRING(NC+1:NC+NCAUX)=STRAUX(1:NCAUX)
                      NC=NC+NCAUX
                      STRING(NC+1:NC+16)=' free slots for '
                      NC=NC+16
                      CALL OUTFMT(REAL(MORG(I)-ILAST),2,
     -                     STRAUX,NCAUX,'LEFT')
                      STRING(NC+1:NC+NCAUX)=STRAUX(1:NCAUX)
                      NC=NC+NCAUX
                      STRING(NC+1:NC+13)=' free words.)'
                      NC=NC+13
                      WRITE(LUNOUT,'(2X,A)') STRING(1:NC)
                 ENDIF
                 NFREE=0
                 ILAST=MORG(I)+MLEN(I)
                 IF(MMOD(I).EQ.0)THEN
                      TYPE='Undefined'
                 ELSEIF(MMOD(I).EQ.1)THEN
                      TYPE='String'
                 ELSEIF(MMOD(I).EQ.2)THEN
                      TYPE='Number'
                 ELSEIF(MMOD(I).EQ.3)THEN
                      TYPE='Logical'
                 ELSEIF(MMOD(I).EQ.4)THEN
                      TYPE='Histogram'
                 ENDIF
                 NAME='< none >'
                 DO 710 J=1,NGLB
                 IF(GLBMOD(J).EQ.5.AND.NINT(GLBVAL(J)).EQ.MREF(I))
     -                NAME=GLBVAR(J)
710              CONTINUE
                 DO 720 J=1,MDIM(I)
                 CALL OUTFMT(REAL(MSIZ(I,J)),2,STRAUX,NCAUX,'LEFT')
                 IF(J.EQ.1)THEN
                      IF(NCAUX.GT.LEN(STRING))GOTO 720
                      STRING=STRAUX(1:NCAUX)
                      NC=NCAUX
                 ELSE
                      IF(NC+NCAUX.GT.LEN(STRING))GOTO 720
                      STRING(NC+1:NC+NCAUX)=STRAUX(1:NCAUX)
                      NC=NC+NCAUX
                 ENDIF
                 IF(J.LT.MDIM(I))THEN
                      IF(NC+3.GT.LEN(STRING))GOTO 720
                      STRING(NC+1:NC+3)=' x '
                      NC=NC+3
                 ENDIF
720              CONTINUE
                 WRITE(LUNOUT,'(2X,I9,1X,I5,1X,A10,1X,A10,1X,A)')
     -                MREF(I),MDIM(I),TYPE,NAME,STRING(1:NC)
            ENDIF
700         CONTINUE
            IF(NFREE.GT.0.OR.MXEMAT.GT.ILAST)THEN
                 STRING(1:1)='('
                 NC=1
                 CALL OUTFMT(REAL(NFREE),2,STRAUX,NCAUX,'LEFT')
                 STRING(NC+1:NC+NCAUX)=STRAUX(1:NCAUX)
                 NC=NC+NCAUX
                 STRING(NC+1:NC+16)=' free slots for '
                 NC=NC+16
                 CALL OUTFMT(REAL(MXEMAT-ILAST),2,
     -                STRAUX,NCAUX,'LEFT')
                 STRING(NC+1:NC+NCAUX)=STRAUX(1:NCAUX)
                 NC=NC+NCAUX
                 STRING(NC+1:NC+13)=' free words.)'
                 NC=NC+13
                 WRITE(LUNOUT,'(2X,A)') STRING(1:NC)
            ENDIF
*   Print number of matrices in use.
            WRITE(LUNOUT,'(/''  Number of matrices booked: '',I5/)')
     -           NUSED
*** Unknown action.
       ELSE
            PRINT *,' !!!!!! MATADM WARNING : Invalid action requested.'
            IFAIL=1
       ENDIF
       END
