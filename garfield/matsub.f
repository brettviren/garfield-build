CDECK  ID>, MATSUB.
       SUBROUTINE MATSUB(ACTION,ISEL,IRSUB,IRMAT,IFAIL)
*-----------------------------------------------------------------------
*   MATSUB - Stores in or extracts from a sub-matrix.
*   Variables: ACTION    - Either STORE to save the matrix IRMAT in a
*              IRMAT       submatrix of IRSUB, or EXTRACT to save a
*              IRSUB       submatrix of IRSUB in matrix IRMAT.
*              ISEL      - Sub matrix selection (#dim, #sel in dim1,
*                          #sel in dim2 ..., sel dim1, sel dim2, ...
*   (Last changed on 12/ 4/96.)
*-----------------------------------------------------------------------
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER IRSUB,IRMAT,ISSUB,ISMAT,ISEL(*),IA(MXMDIM),MATADR,ILEN,
     -      ISIZ(MXMDIM),IOFF(MXMDIM),IASUB(MXMDIM),IADDR,MATSLT
       CHARACTER*(*) ACTION
       EXTERNAL MATADR,MATSLT
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE MATSUB ///'
*** Assume this will fail.
       IFAIL=1
*** Check the ACTION flag.
       IF(ACTION.NE.'STORE'.AND.ACTION.NE.'EXTRACT')THEN
            PRINT *,' !!!!!! MATSUB WARNING : Unknown action'//
     -           ' received; nothing done.'
            RETURN
       ENDIF
*** Locate the matrix of which a sub-matrix is to be formed.
       ISSUB=MATSLT(IRSUB)
       IF(ISSUB.LE.0)THEN
            PRINT *,' !!!!!! MATSUB WARNING : Indexed matrix not found.'
            RETURN
       ENDIF
*** Check that the number of dimensions matches the selection vector.
       IF(MDIM(ISSUB).NE.ISEL(1).OR.ISEL(1).LE.0)THEN
            PRINT *,' !!!!!! MATSUB WARNING : Matrix dimension and'//
     -           ' indexing do not match.'
            RETURN
       ENDIF
*** Prepare sub-matrix addressing vectors, check the dimensions.
       DO 90 I=1,ISEL(1)
       ISIZ(I)=ISEL(I+1)
       IF(ISIZ(I).EQ.0)ISIZ(I)=MSIZ(ISSUB,I)
       IF(I.EQ.1)THEN
            IOFF(I)=ISEL(1)+1
       ELSE
            IOFF(I)=IOFF(I-1)+ISEL(I)
       ENDIF
       DO 100 J=IOFF(I)+1,IOFF(I)+ISEL(I+1)
       IF(ISEL(J).LT.1.OR.ISEL(J).GT.MSIZ(ISSUB,I))THEN
            PRINT *,' !!!!!! MATSUB WARNING : Indexing out of bounds'//
     -           ' of the matrix; no sub-matrix.'
            RETURN
       ENDIF
100    CONTINUE
90     CONTINUE
*** Locate the input matrix when STORE'ing.
       IF(ACTION.EQ.'STORE')THEN
*   Find the matrix.
            ISMAT=MATSLT(IRMAT)
            IF(ISMAT.LE.0)THEN
                 PRINT *,' !!!!!! MATSUB WARNING : Input matrix has'//
     -                ' not been found.'
                 RETURN
            ENDIF
*   See whether the size is the same as that of the sub-matrix.
            ILEN=1
            DO 50 I=1,ISEL(1)
            ILEN=ILEN*ISIZ(I)
50          CONTINUE
            IF(MLEN(ISMAT).NE.1.AND.ILEN.NE.MLEN(ISMAT))THEN
                 PRINT *,' !!!!!! MATSUB WARNING : Mismatch in matrix'//
     -                ' sizes; matrix not assigned.'
                 RETURN
            ENDIF
*** Or allocate a matrix when EXTRACT'ing.
       ELSE
*   Set the mode for the new matrix.
            IMOD=MMOD(ISSUB)
*   Allocate.
            CALL MATADM('ALLOCATE',IRMAT,ISEL(1),ISIZ,IMOD,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MATSUB WARNING : Unable to allocate'//
     -                ' space for the sub-matrix; not extracted.'
                 RETURN
            ENDIF
*   Find where the new matrix sits.
            ISMAT=MATSLT(IRMAT)
            IF(ISMAT.LE.0)THEN
                 PRINT *,' !!!!!! MATSUB WARNING : New matrix not'//
     -                ' found; program bug - please report.'
                 RETURN
            ENDIF
       ENDIF
*** Re-locate the matrix of which a sub-matrix is to be formed.
       ISSUB=MATSLT(IRSUB)
       IF(ISSUB.LE.0)THEN
            PRINT *,' !!!!!! MATSUB WARNING : Indexed matrix not found.'
            RETURN
       ENDIF
*** Loop over the sub matrix, initial address vector.
       DO 200 I=1,MDIM(ISSUB)
       IA(I)=1
200    CONTINUE
*   Initial pointer in the matrix vector.
       IELEM=MORG(ISMAT)
*   Return here for the next element.
210    CONTINUE
       IF(MLEN(ISMAT).EQ.1)THEN
            IELEM=MORG(ISMAT)+1
       ELSE
            IELEM=IELEM+1
       ENDIF
*   Convert the address in a true sub-matrix address.
       DO 240 I=1,MDIM(ISSUB)
       IF(ISEL(I+1).EQ.0)THEN
            IASUB(I)=IA(I)
       ELSE
            IASUB(I)=ISEL(IOFF(I)+IA(I))
       ENDIF
240    CONTINUE
*   Carry out the assignments.
       IADDR=MATADR(ISSUB,IASUB)
       IF(ACTION.EQ.'STORE')THEN
            MVEC(IADDR)=MVEC(IELEM)
       ELSE
            MVEC(IELEM)=MVEC(IADDR)
       ENDIF
*   Increment the address vector.
       DO 220 I=1,MDIM(ISSUB)
       IF(IA(I).LT.ISIZ(I))THEN
            IA(I)=IA(I)+1
            DO 230 J=1,I-1
            IA(J)=1
230         CONTINUE
            GOTO 210
       ENDIF
220    CONTINUE
*** Seems to have worked.
       IFAIL=0
       END
