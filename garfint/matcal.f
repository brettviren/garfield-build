CDECK  ID>, MATCAL.
       SUBROUTINE MATCAL(INSTR,IFAIL)
*-----------------------------------------------------------------------
*   MATCAL - Handles matrix procedure calls.
*   (Last changed on 10/06/12.)
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
       INTEGER INS(MXINS,4),ALGENT(MXALGE,10),MODREG(MXCONS:MXREG),
     -      ISYNCH,IINS0,ICONS0,ARGREF(MXARG,2),MODARG(MXARG),
     -      NREG,NCONS,NINS,NERR,NRES,NALGE,IENTRL,NAERR(100)
       REAL REG(MXCONS:MXREG),ARG(MXARG),EXPMAX
       PARAMETER(EXPMAX=40.0)
       LOGICAL EXEC(MXINS),LIGUND,LINUND
       COMMON /ALGDAT/ REG,ARG,MODARG,ARGREF,INS,MODREG,ALGENT,
     -      NREG,NCONS,NINS,NERR,NAERR,
     -      NRES,NALGE,IENTRL,ISYNCH,IINS0,ICONS0,EXEC,LIGUND,LINUND
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
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
       CHARACTER*80 XTXT,YTXT,ZTXT,TITLE,FILE,OPTION
       CHARACTER*30 NAME
       CHARACTER*29 REMARK
       CHARACTER*8  MEMBER
       REAL AUX,ZERO(MXLIST),PAD,THETA,PHI,SIGW
       INTEGER MXISEL
       PARAMETER(MXISEL=25000)
       INTEGER ISIZ(MXMDIM),ISEL(MXISEL),NSEL,MATSLT,NCXTXT,NCYTXT,
     -      NCZTXT,NCTIT,IREFX,IREFY,ISDUM1,ISDUM2,NARG,IPROC,INSTR,
     -      IFAIL,IFAIL1,IFAIL2,IFAIL3,IFAIL4,IFAIL5,NCOPT,NCONT,NZERO,
     -      I,J,K,NIN,IMAX(MXMDIM),NMAX,
     -      ISLOT,IRMAT,ISMAT,NDIM,NDUM,NCFILE,NCMEMB,NCREM,LORD,NORD,
     -      ISORD,ISORDI,IROUT,IWRONG,IDUM,IREF,IREFD,ISLOTD,ISDIM,
     -      IRORD,ISLOTX,ISLOTY,ISLOTW,NW
       LOGICAL FRAME
       EXTERNAL MATSLT
*** Assume that this will fail.
       IFAIL=1
*** Some easy reference variables.
       NARG=INS(INSTR,3)
       IPROC=INS(INSTR,1)
*** Extract a sub-matrix.
       IF(IPROC.EQ.-80)THEN
*   Check the format of the argument list.
            IF(NARG.LT.3.OR.ARGREF(NARG,1).GE.2.OR.
     -           MODARG(NARG-1).NE.5.OR.MODARG(1).NE.2.OR.
     -           NINT(ARG(1)).LT.1)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : EXTRACT_SUBMATRIX'//
     -                ' received an invalid argument list.'
                 RETURN
            ENDIF
*   Copy the selection vector, expanding any vectors used as address.
            IF(1+NINT(ARG(1)).GT.MXISEL)GOTO 69
            ISEL(1)=NINT(ARG(1))
            NSEL=1+NINT(ARG(1))
            NIN=1+NINT(ARG(1))
            DO 60 I=2,1+NINT(ARG(1))
            IF(MODARG(I).NE.2)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Incorrectly'//
     -                ' specified selection size; no sub-matrix.'
                 RETURN
            ENDIF
            ISEL(I)=0
            DO 61 J=1,NINT(ARG(I))
            IF(MODARG(NIN+J).EQ.2)THEN
                 IF(NSEL+1.GT.MXISEL)GOTO 69
                 NSEL=NSEL+1
                 ISEL(NSEL)=NINT(ARG(NIN+J))
                 ISEL(I)=ISEL(I)+1
            ELSEIF(MODARG(NIN+J).EQ.5)THEN
                 DO 62 K=1,MXMAT
                 IF(MREF(K).EQ.NINT(ARG(NIN+J)))THEN
                      ISLOT=K
                      GOTO 63
                 ENDIF
62               CONTINUE
                 PRINT *,' !!!!!! MATCAL WARNING : Matrix not found;'//
     -                ' program bug - please report.'
                 RETURN
63               CONTINUE
                 DO 64 K=1,MLEN(ISLOT)
                 IF(NSEL+1.GT.MXISEL)GOTO 69
                 NSEL=NSEL+1
                 ISEL(NSEL)=NINT(MVEC(MORG(ISLOT)+K))
                 ISEL(I)=ISEL(I)+1
64               CONTINUE
            ELSE
                 PRINT *,' !!!!!! MATCAL WARNING : Incorrectly'//
     -                ' specified selection item; no sub-matrix.'
                 RETURN
            ENDIF
61          CONTINUE
            NIN=NIN+NINT(ARG(I))
60          CONTINUE
*   Store the sub-matrix in the matrix.
            CALL MATSUB('EXTRACT',ISEL,NINT(ARG(NARG-1)),IRMAT,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Extracting the'//
     -                ' submatrix failed.'
                 RETURN
            ENDIF
*   Free the receiving argument.
            CALL ALGREU(NINT(ARG(NARG)),MODARG(NARG),ARGREF(NARG,1))
*   Update the argument list.
            ARG(NARG)=IRMAT
            MODARG(NARG)=5
*   In case of failure.
            GOTO 68
69          CONTINUE
            PRINT *,' !!!!!! MATCAL WARNING : Insufficient memory'//
     -           ' to expand matrix selection vector; no sub-matrix.'
            RETURN
68          CONTINUE
*** Store a sub-matrix.
       ELSEIF(IPROC.EQ.-81)THEN
*   Check the format of the argument list.
            IF(NARG.LT.3.OR.ARGREF(NARG-1,1).GE.2.OR.
     -           MODARG(NARG-1).NE.5.OR.
     -           (MODARG(NARG).NE.2.AND.MODARG(NARG).NE.5))THEN
                 PRINT *,' !!!!!! MATCAL WARNING : STORE_SUBMATRIX'//
     -                ' received an invalid argument list.'
                 RETURN
            ENDIF
*   Process the case that we've to store a scalar in a matrix.
            IF(MODARG(NARG).EQ.2)THEN
                 ISIZ(1)=1
                 CALL MATADM('ALLOCATE',IRMAT,1,ISIZ,2,IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! MATCAL WARNING : Unable to'//
     -                     ' allocate a temporary matrix for a scalar.'
                      RETURN
                 ENDIF
                 DO 80 I=1,MXMAT
                 IF(MREF(I).EQ.IRMAT)THEN
                      ISMAT=I
                      GOTO 90
                 ENDIF
80               CONTINUE
                 PRINT *,' !!!!!! MATCAL WARNING : Scalar not found;'//
     -                ' program bug - please report.'
                 RETURN
90               CONTINUE
                 MVEC(MORG(ISMAT)+1)=ARG(NARG)
            ELSE
                 IRMAT=NINT(ARG(NARG))
            ENDIF
*   Copy the selection vector, expanding any vectors used as address.
            IF(1+NINT(ARG(1)).GT.MXISEL)GOTO 79
            ISEL(1)=NINT(ARG(1))
            NSEL=1+NINT(ARG(1))
            NIN=1+NINT(ARG(1))
            DO 70 I=2,1+NINT(ARG(1))
            IF(MODARG(I).NE.2)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Incorrectly'//
     -                ' specified selection size; no sub-matrix.'
                 IF(MODARG(NARG).EQ.2)
     -                CALL MATADM('DELETE',IRMAT,1,ISIZ,2,IFAIL2)
                 RETURN
            ENDIF
            ISEL(I)=0
            DO 71 J=1,NINT(ARG(I))
            IF(MODARG(NIN+J).EQ.2)THEN
                 IF(NSEL+1.GT.MXISEL)GOTO 79
                 NSEL=NSEL+1
                 ISEL(NSEL)=NINT(ARG(NIN+J))
                 ISEL(I)=ISEL(I)+1
            ELSEIF(MODARG(NIN+J).EQ.5)THEN
                 DO 72 K=1,MXMAT
                 IF(MREF(K).EQ.NINT(ARG(NIN+J)))THEN
                      ISLOT=K
                      GOTO 73
                 ENDIF
72               CONTINUE
                 PRINT *,' !!!!!! MATCAL WARNING : Matrix not found;'//
     -                ' program bug - please report.'
                 RETURN
73               CONTINUE
                 DO 74 K=1,MLEN(ISLOT)
                 IF(NSEL+1.GT.MXISEL)GOTO 79
                 NSEL=NSEL+1
                 ISEL(NSEL)=NINT(MVEC(MORG(ISLOT)+K))
                 ISEL(I)=ISEL(I)+1
74               CONTINUE
            ELSE
                 PRINT *,' !!!!!! MATCAL WARNING : Incorrectly'//
     -                ' specified selection item; no sub-matrix.'
                 IF(MODARG(NARG).EQ.2)
     -                CALL MATADM('DELETE',IRMAT,1,ISIZ,2,IFAIL2)
                 RETURN
            ENDIF
71          CONTINUE
            NIN=NIN+NINT(ARG(I))
70          CONTINUE
*   Store the matrix in the sub-matrix.
            CALL MATSUB('STORE',ISEL,NINT(ARG(NARG-1)),IRMAT,IFAIL1)
*   Remove the temporary matrix if we assigned a scalar.
            IF(MODARG(NARG).EQ.2)
     -           CALL MATADM('DELETE',IRMAT,1,ISIZ,2,IFAIL2)
*   Check error flags.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Storing in the'//
     -                ' submatrix failed.'
                 RETURN
            ENDIF
*   Failure.
            GOTO 78
79          CONTINUE
            PRINT *,' !!!!!! MATCAL WARNING : Insufficient memory'//
     -           ' to expand matrix selection vector; no sub-matrix.'
            RETURN
78          CONTINUE
*** Print a matrix.
       ELSEIF(IPROC.EQ.-82)THEN
*   There should be at least 1 argument.
            IF(NARG.LE.0)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Incorrect number'//
     -                ' arguments received by PRINT_MATRIX.'
*   Print all matrices provided as arguments, find their names.
            ELSE
                 DO 10 I=1,NARG
                 IF(MODARG(I).NE.5)THEN
                      PRINT *,' !!!!!! MATCAL WARNING : An argument'//
     -                     ' is not of type matrix ; ignored.'
                      GOTO 10
                 ENDIF
                 NAME='(temporary matrix)'
                 DO 20 J=1,NGLB
                 IF(GLBMOD(J).EQ.5.AND.NINT(GLBVAL(J)).EQ.NINT(ARG(I)))
     -                NAME=GLBVAR(J)
20               CONTINUE
                 WRITE(LUNOUT,'(2X,A)') NAME
                 CALL MATPRT(NINT(ARG(I)))
10               CONTINUE
            ENDIF
*** Create a matrix.
       ELSEIF(IPROC.EQ.-83)THEN
*   Check number of arguments.
            IF(NARG.LE.1.OR.ARGREF(1,1).GE.2)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Incorrect argument'//
     -                ' list received by BOOK_MATRIX.'
                 RETURN
            ELSE
*   Get the matrix dimensions.
                 NDIM=0
                 DO 30 I=2,NARG
                 IF(MODARG(I).EQ.2)THEN
                      IF(NDIM+1.GT.MXMDIM)THEN
                           PRINT *,' !!!!!! MATCAL WARNING : Too'//
     -                          ' many dimensions; matrix not booked.'
                           RETURN
                      ENDIF
                      NDIM=NDIM+1
                      ISIZ(NDIM)=NINT(ARG(I))
                 ELSEIF(MODARG(I).EQ.5)THEN
                      ISDIM=MATSLT(NINT(ARG(I)))
                      IF(ISDIM.LE.0)THEN
                           PRINT *,' !!!!!! MATCAL WARNING : Size'//
     -                          ' not found; matrix not booked.'
                           RETURN
                      ENDIF
                      DO 35 J=1,MLEN(ISDIM)
                      IF(NDIM+1.GT.MXMDIM)THEN
                           PRINT *,' !!!!!! MATCAL WARNING : Too'//
     -                          ' many dimensions; matrix not booked.'
                           RETURN
                      ENDIF
                      NDIM=NDIM+1
                      ISIZ(NDIM)=NINT(MVEC(MORG(ISDIM)+J))
35                    CONTINUE
                 ELSE
                      PRINT *,' !!!!!! MATCAL WARNING : Incorrect'//
     -                     ' data type in array dimensions.'
                      RETURN
                 ENDIF
30               CONTINUE
*   Create the matrix.
                 CALL MATADM('ALLOCATE',IREF,NDIM,ISIZ,2,IFAIL1)
*   See whether this worked.
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! MATCAL WARNING : Unable to'//
     -                     ' create the requested matrix.'
                      RETURN
                 ENDIF
*   Clear the variable.
                 CALL ALGREU(NINT(ARG(1)),MODARG(1),ARGREF(1,1))
*   Assign the result to the variable.
                 ARG(1)=REAL(IREF)
                 MODARG(1)=5
            ENDIF
*** Resize a matrix.
       ELSEIF(IPROC.EQ.-84)THEN
*   Check number of arguments.
            IF(NARG.LE.2.OR.NARG.GT.MXMDIM+2.OR.
     -           ARGREF(1,1).GE.2.OR.MODARG(1).NE.5.OR.
     -           MODARG(NARG).NE.2)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Incorrect argument'//
     -                ' list received by RESHAPE_MATRIX.'
                 RETURN
            ELSE
*   Get padding.
                 PAD=ARG(NARG)
*   Get the matrix dimensions.
                 NDIM=NARG-2
                 DO 40 I=1,NDIM
                 IF(MODARG(I+1).NE.2)THEN
                      PRINT *,' !!!!!! MATCAL WARNING : Incorrect'//
     -                     ' data type in array dimensions.'
                      RETURN
                 ENDIF
                 ISIZ(I)=NINT(ARG(I+1))
40               CONTINUE
*   Resize the matrix.
                 CALL MATCHS(NINT(ARG(1)),NDIM,ISIZ,PAD,IFAIL1)
*   See whether this worked.
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! MATCAL WARNING : Unable to'//
     -                     ' re-shape the matrix.'
                      RETURN
                 ENDIF
            ENDIF
*** Adjust a matrix.
       ELSEIF(IPROC.EQ.-85)THEN
*   Check number of arguments.
            IF(NARG.LE.2.OR.NARG.GT.MXMDIM+2.OR.
     -           ARGREF(1,1).GE.2.OR.MODARG(1).NE.5.OR.
     -           MODARG(NARG).NE.2)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Incorrect argument'//
     -                ' list received by ADJUST_MATRIX.'
                 RETURN
            ELSE
*   Get padding.
                 PAD=ARG(NARG)
*   Get the matrix dimensions.
                 NDIM=NARG-2
                 DO 45 I=1,NDIM
                 IF(MODARG(I+1).NE.2)THEN
                      PRINT *,' !!!!!! MATCAL WARNING : Incorrect'//
     -                     ' data type in array dimensions.'
                      RETURN
                 ENDIF
                 ISIZ(I)=NINT(ARG(I+1))
45               CONTINUE
*   Resize the matrix.
                 CALL MATADJ(NINT(ARG(1)),NDIM,ISIZ,PAD,IFAIL1)
*   See whether this worked.
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! MATCAL WARNING : Unable to'//
     -                     ' adjust the matrix.'
                      RETURN
                 ENDIF
            ENDIF
*** Delete a matrix.
       ELSEIF(IPROC.EQ.-86)THEN
*   Check number of arguments.
            IF(NARG.LT.1)THEN
                 DO 55 I=1,NGLB
                 IF(GLBMOD(I).EQ.5)THEN
                      CALL MATADM('DELETE',NINT(GLBVAL(I)),
     -                     0,ISIZ,2,IFAIL1)
                      GLBVAL(I)=0
                      GLBMOD(I)=0
                 ENDIF
55               CONTINUE
                 CALL MATINT
*   Delete all the matrices in the arguments.
            ELSE
                 DO 50 I=1,NARG
                 IF(MODARG(I).NE.5)THEN
C                      PRINT *,' !!!!!! MATCAL WARNING : Incorrect'//
C     -                     ' data type in DELETE_MATRIX call.'
                      GOTO 50
                 ENDIF
                 CALL MATADM('DELETE',NINT(ARG(I)),0,ISIZ,2,IFAIL1)
                 ARG(I)=0
                 MODARG(I)=0
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! MATCAL WARNING :'//
     -                ' Deleting a matrix failed.'
50               CONTINUE
            ENDIF
*** List matrices in memory.
       ELSEIF(IPROC.EQ.-87)THEN
*   Check number and type of arguments.
            IF(NARG.NE.0)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Incorrect argument'//
     -                ' list provided for LIST_MATRICES.'
                 RETURN
            ENDIF
*   List.
            CALL MATADM('LIST',IDUM,NDUM,ISIZ,NDUM,IFAIL1)
*** Write a matrix to a library.
       ELSEIF(IPROC.EQ.-88)THEN
*   Check number and type of arguments.
            IF(MODARG(1).NE.5.OR.MODARG(2).NE.1.OR.
     -           (NARG.GE.3.AND.MODARG(3).NE.1).OR.
     -           (NARG.GE.4.AND.MODARG(4).NE.1).OR.
     -           NARG.LT.2.OR.NARG.GT.4)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Incorrect argument'//
     -                ' list provided for WRITE_MATRIX.'
                 RETURN
            ENDIF
*   Fetch file name.
            CALL STRBUF('READ',NINT(ARG(2)),FILE,NCFILE,IFAIL1)
*   Member name.
            IF(NARG.GE.3)THEN
                 CALL STRBUF('READ',NINT(ARG(3)),MEMBER,NCMEMB,IFAIL2)
                 IF(NCMEMB.GT.8)PRINT *,' !!!!!! MATCAL WARNING :'//
     -                ' Member name truncated to first 8 characters'
                 NCMEMB=MIN(8,NCMEMB)
            ELSE
                 DO 120 J=1,NGLB
                 IF(GLBMOD(J).NE.5)GOTO 120
                 IF(NINT(GLBVAL(J)).EQ.NINT(ARG(1)))THEN
                      MEMBER=GLBVAR(J)
                      NCMEMB=8
                      GOTO 130
                 ENDIF
120              CONTINUE
                 MEMBER='< none >'
                 NCMEMB=8
130              CONTINUE
                 IFAIL2=0
            ENDIF
*   Remark.
            IF(NARG.GE.4)THEN
                 CALL STRBUF('READ',NINT(ARG(4)),REMARK,NCREM,IFAIL3)
                 IF(NCREM.GT.29)PRINT *,' !!!!!! MATCAL WARNING :'//
     -                ' Remark truncated to first 29 characters'
                 NCREM=MIN(29,NCREM)
            ELSE
                 REMARK='none'
                 NCREM=4
                 IFAIL3=0
            ENDIF
*  Write the matrix.
            IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                 CALL MATWRT(NINT(ARG(1)),FILE(1:NCFILE),
     -                MEMBER(1:NCMEMB),REMARK(1:NCREM),IFAIL2)
                 IF(IFAIL2.NE.0)THEN
                      PRINT *,' !!!!!! MATCAL WARNING :'//
     -                     ' Writing matrix to disk failed.'
                      RETURN
                 ENDIF
            ELSE
                 PRINT *,' !!!!!! MATCAL WARNING :'//
     -                ' Not able to obtain a name; matrix'//
     -                ' not written to disk.'
                 RETURN
            ENDIF
*** Get a matrix from a library.
       ELSEIF(IPROC.EQ.-89)THEN
*   Check number and type of arguments.
            IF(ARGREF(1,1).GE.2.OR.
     -           MODARG(2).NE.1.OR.
     -           (NARG.GE.3.AND.MODARG(3).NE.1).OR.
     -           NARG.LT.2.OR.NARG.GT.3)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Incorrect argument'//
     -                ' list provided for GET_MATRIX.'
                 RETURN
            ENDIF
*  Fetch file name.
            CALL STRBUF('READ',NINT(ARG(2)),FILE,NCFILE,IFAIL1)
*  Fetch member name, if any.
            IF(NARG.GE.3)THEN
                 CALL STRBUF('READ',NINT(ARG(3)),MEMBER,NCMEMB,IFAIL2)
                 IF(NCMEMB.GT.8)PRINT *,' !!!!!! MATCAL WARNING :'//
     -                ' Member name truncated to first 8 characters'
                 NCMEMB=MIN(8,NCMEMB)
            ELSEIF(ARGREF(1,2).GE.1.AND.ARGREF(1,2).LE.NGLB)THEN
                 MEMBER=GLBVAR(ARGREF(1,2))
                 NCMEMB=8
                 IFAIL2=0
            ELSE
                 MEMBER='*'
                 NCMEMB=1
                 IFAIL2=0
            ENDIF
*  Read the matrix.
            IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0)THEN
                 CALL ALGREU(NINT(ARG(1)),MODARG(1),ARGREF(1,1))
                 CALL MATGET(IREF,FILE(1:NCFILE),
     -                MEMBER(1:NCMEMB),IFAIL3)
                 IF(IFAIL3.NE.0)THEN
                      PRINT *,' !!!!!! MATCAL WARNING :'//
     -                     ' Reading matrix from disk failed.'
                      ARG(1)=0
                      MODARG(1)=0
                      RETURN
                 ELSE
                      ARG(1)=IREF
                      MODARG(1)=5
                 ENDIF
            ELSE
                 PRINT *,' !!!!!! MATCAL WARNING :'//
     -                ' Not able to obtain a name; matrix'//
     -                ' not read from disk.'
                 RETURN
            ENDIF
*** Matrix multiplication.
       ELSEIF(IPROC.EQ.-90)THEN
*** Solve linear equation.
       ELSEIF(IPROC.EQ.-91)THEN
*** Return matrix dimensions.
       ELSEIF(IPROC.EQ.-92)THEN
*   Check number and type of arguments.
            IF(NARG.NE.3.OR.
     -           ARGREF(2,1).GE.2.OR.ARGREF(3,1).GE.2.OR.
     -           MODARG(1).NE.5)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Incorrect argument'//
     -                ' list provided for DIMENSIONS.'
                 RETURN
            ENDIF
*   Locate the matrix.
            DO 180 I=1,MXMAT
            IF(MREF(I).EQ.NINT(ARG(1)))THEN
                 ISLOT=I
                 NDIM=MDIM(I)
                 GOTO 140
            ENDIF
180         CONTINUE
            PRINT *,' !!!!!! MATCAL WARNING : Matrix not found;'//
     -           ' no dimensions returned.'
            RETURN
140         CONTINUE
*   Clear the output arguments.
            CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
            CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
*   Store the dimension.
            ARG(2)=NDIM
            MODARG(2)=2
*   Get a matrix for the dimensions.
            ISIZ(1)=NDIM
            CALL MATADM('ALLOCATE',IREFD,1,ISIZ,2,IFAIL1)
            IF(IFAIL1.NE.0)RETURN
            ISLOT=-1
            ISLOTD=-1
            DO 150 I=1,MXMAT
            IF(MREF(I).EQ.NINT(ARG(1)))THEN
                 ISLOT=I
            ELSEIF(MREF(I).EQ.IREFD)THEN
                 ISLOTD=I
            ENDIF
            IF(ISLOT.GT.0.AND.ISLOTD.GT.0)GOTO 160
150         CONTINUE
            PRINT *,' !!!!!! MATCAL WARNING : Matrix not found;'//
     -           ' no dimensions returned.'
            RETURN
160         CONTINUE
*   Store the dimensions.
            DO 170 J=1,NDIM
            MVEC(MORG(ISLOTD)+J)=MSIZ(ISLOT,J)
170         CONTINUE
*   Save the output.
            ARG(3)=IREFD
            MODARG(3)=5
*** Matrix interpolation.
       ELSEIF(IPROC.EQ.-93)THEN
*   Check the format of the argument list.
            IF(NARG.LT.4.OR.ARGREF(NARG,1).GE.2.OR.
     -           MODARG(1).NE.5.OR.MODARG(NARG-1).NE.5)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : INTERPOLATE'//
     -                ' received an invalid argument list.'
                 RETURN
            ENDIF
*   Locate the matrix.
            ISMAT=MATSLT(NINT(ARG(1)))
            IF(ISMAT.LE.0)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Matrix to be'//
     -                ' interpolated has not been found.'
                 RETURN
            ENDIF
*   Determine the size of the combined ordinate vector.
            LORD=0
            DO 203 I=1,MDIM(ISMAT)
            LORD=LORD+MSIZ(ISMAT,I)
203         CONTINUE
*   Allocate a matrix for the combined ordinate vector.
            ISIZ(1)=LORD
            CALL MATADM('ALLOCATE',IRORD,1,ISIZ,2,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Unable to allocate'//
     -                ' an ordinate vector.'
                 RETURN
            ENDIF
*   And find this matrix.
            ISORD=MATSLT(IRORD)
            IF(ISORD.LE.0)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Combined ordinate'//
     -                ' vector not found.'
                 RETURN
            ENDIF
*   Find the matrix again.
            ISMAT=MATSLT(NINT(ARG(1)))
            IF(ISMAT.LE.0)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Matrix to be'//
     -                ' interpolated has not been found.'
                 RETURN
            ENDIF
*   Loop over the ordinate vectors.
            NORD=0
            DO 200 I=2,NARG-2
            IF(MODARG(I).NE.5)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : An'//
     -                ' ordinate vector is not a declared matrix.'
                 CALL MATADM('DELETE',IRORD,1,ISIZ,2,IFAIL1)
                 RETURN
            ENDIF
*   Locate the vector.
            ISORDI=MATSLT(NINT(ARG(I)))
            IF(ISORDI.LE.0)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : An ordinate'//
     -                ' vector has not been found.'
                 CALL MATADM('DELETE',IRORD,1,ISIZ,2,IFAIL1)
                 RETURN
            ENDIF
*   Ensure it is a 1-dimensional vector and the right size.
            IF(MDIM(ISORDI).NE.1.OR.
     -           MSIZ(ISORDI,1).NE.MSIZ(ISMAT,I-1))THEN
                 PRINT *,' !!!!!! MATCAL WARNING : An'//
     -                ' ordinate vector is not of the right size.'
                 CALL MATADM('DELETE',IRORD,1,ISIZ,2,IFAIL1)
                 RETURN
            ENDIF
*   Copy this vector to the large ordinate vector, checking ordering.
            DO 230 J=1,MSIZ(ISORDI,1)
            NORD=NORD+1
            MVEC(MORG(ISORD)+NORD)=MVEC(MORG(ISORDI)+J)
            IF(J.GT.1)THEN
                 IF(MVEC(MORG(ISORDI)+J).LE.MVEC(MORG(ISORDI)+J-1))THEN
                      PRINT *,' !!!!!! MATCAL WARNING : An ordinate'//
     -                     ' vector is not well ordered.'
                      CALL MATADM('DELETE',IRORD,1,ISIZ,2,IFAIL1)
                      RETURN
                 ENDIF
            ENDIF
230         CONTINUE
*   Next dimension.
200         CONTINUE
*   Output argument.
            IF(MODARG(NARG).EQ.5)THEN
                 IROUT=NINT(ARG(NARG))
            ELSE
                 IROUT=-1
            ENDIF
*   Call the interpolation routine.
            CALL MATINN(NINT(ARG(1)),IRORD,NINT(ARG(NARG-1)),
     -           IROUT,IFAIL2)
            IF(IFAIL2.NE.0)PRINT *,' !!!!!! MATCAL WARNING :'//
     -           ' Matrix interpolation failed.'
*   Assign the output.
            ARG(NARG)=IROUT
            MODARG(NARG)=5
*   Remove the ordinate vector.
            CALL MATADM('DELETE',IRORD,1,ISIZ,2,IFAIL1)
            IF(IFAIL2.NE.0)RETURN
*** Surface plots.
       ELSEIF(IPROC.EQ.-94)THEN
*   Check argument list.
            IF(NARG.LT.1.OR.NARG.GT.8.OR.
     -           MODARG(1).NE.5.OR.
     -           (NARG.GE.2.AND.MODARG(2).NE.2).OR.
     -           (NARG.GE.3.AND.MODARG(3).NE.2).OR.
     -           (NARG.GE.4.AND.MODARG(4).NE.5).OR.
     -           (NARG.GE.5.AND.MODARG(5).NE.5).OR.
     -           (NARG.GE.6.AND.MODARG(6).NE.1).OR.
     -           (NARG.GE.7.AND.MODARG(7).NE.1).OR.
     -           (NARG.GE.8.AND.MODARG(8).NE.1))THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Some arguments of'//
     -                ' PLOT_SURFACE are of incorrect type.'
                 RETURN
            ENDIF
*   Plotting angles.
            IF(NARG.GE.2)THEN
                 THETA=ARG(2)
            ELSE
                 THETA=60
            ENDIF
            IF(NARG.GE.3)THEN
                 PHI=ARG(3)
            ELSE
                 PHI=60
            ENDIF
*   Axis ranges.
            IF(NARG.GE.4)THEN
                 IREFX=NINT(ARG(4))
            ELSE
                 IREFX=-1
            ENDIF
            IF(NARG.GE.5)THEN
                 IREFY=NINT(ARG(5))
            ELSE
                 IREFY=-1
            ENDIF
*   Fetch the x-axis label.
            IF(NARG.GE.6)THEN
                 CALL STRBUF('READ',NINT(ARG(6)),XTXT,NCXTXT,IFAIL1)
            ELSEIF(NARG.GE.4)THEN
                 DO 171 J=1,NGLB
                 IF(GLBMOD(J).NE.5)GOTO 171
                 IF(NINT(GLBVAL(J)).EQ.IREFX)THEN
                      XTXT=GLBVAR(J)
                      NCXTXT=10
                      GOTO 172
                 ENDIF
171              CONTINUE
                 XTXT='x-axis'
                 NCXTXT=6
172              CONTINUE
                 IFAIL1=0
            ELSE
                 XTXT='x-axis'
                 NCXTXT=6
                 IFAIL1=0
            ENDIF
*   Fetch the y-axis label.
            IF(NARG.GE.7)THEN
                 CALL STRBUF('READ',NINT(ARG(7)),YTXT,NCYTXT,IFAIL2)
            ELSEIF(NARG.GE.5)THEN
                 DO 173 J=1,NGLB
                 IF(GLBMOD(J).NE.5)GOTO 173
                 IF(NINT(GLBVAL(J)).EQ.IREFY)THEN
                      YTXT=GLBVAR(J)
                      NCYTXT=10
                      GOTO 174
                 ENDIF
173              CONTINUE
                 YTXT='y-axis'
                 NCYTXT=6
174              CONTINUE
                 IFAIL2=0
            ELSE
                 YTXT='y-axis'
                 NCYTXT=6
                 IFAIL2=0
            ENDIF
*   Fetch the global title.
            IF(NARG.GE.8)THEN
                 CALL STRBUF('READ',NINT(ARG(8)),TITLE,NCTIT,IFAIL3)
            ELSE
                 DO 175 J=1,NGLB
                 IF(GLBMOD(J).NE.5)GOTO 175
                 IF(NINT(GLBVAL(J)).EQ.NINT(ARG(1)))THEN
                      TITLE=GLBVAR(J)
                      NCTIT=10
                      GOTO 176
                 ENDIF
175              CONTINUE
                 TITLE=' '
                 NCTIT=1
176              CONTINUE
                 IFAIL3=0
            ENDIF
*   Plot the surface.
            CALL MATSUR(NINT(ARG(1)),IREFX,IREFY,XTXT(1:NCXTXT),
     -           YTXT(1:NCYTXT),TITLE(1:NCTIT),PHI,THETA)
*   Switch back to normal screen.
            CALL GRALPH
*   Error processing.
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Error'//
     -                ' retrieving a string for PLOT_SURFACE.'
                 RETURN
            ENDIF
*** Contour plots.
       ELSEIF(IPROC.EQ.-100)THEN
*   Check argument list.
            IF(NARG.LT.1.OR.NARG.GT.8.OR.
     -           MODARG(1).NE.5.OR.
     -           (NARG.GE.2.AND.MODARG(2).NE.2).OR.
     -           (NARG.GE.3.AND.MODARG(3).NE.1).OR.
     -           (NARG.GE.4.AND.MODARG(4).NE.5).OR.
     -           (NARG.GE.5.AND.MODARG(5).NE.5).OR.
     -           (NARG.GE.6.AND.MODARG(6).NE.1).OR.
     -           (NARG.GE.7.AND.MODARG(7).NE.1).OR.
     -           (NARG.GE.8.AND.MODARG(8).NE.1))THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Some arguments of'//
     -                ' PLOT_CONTOURS are of incorrect type.'
                 RETURN
            ENDIF
*   Fetch the options, if present.
            IF(NARG.GE.2)THEN
                 NCONT=NINT(ARG(2))
            ELSE
                 NCONT=20
            ENDIF
            IF(NARG.GE.3)THEN
                 CALL STRBUF('READ',NINT(ARG(3)),OPTION,NCOPT,IFAIL4)
                 CALL CLTOU(OPTION(1:NCOPT))
            ELSE
                 OPTION=' '
                 NCOPT=1
                 IFAIL4=0
            ENDIF
*   Axis ranges.
            IF(NARG.GE.4)THEN
                 IREFX=NINT(ARG(4))
            ELSE
                 IREFX=-1
            ENDIF
            IF(NARG.GE.5)THEN
                 IREFY=NINT(ARG(5))
            ELSE
                 IREFY=-1
            ENDIF
*   Fetch the x-axis label.
            IF(NARG.GE.6)THEN
                 CALL STRBUF('READ',NINT(ARG(6)),XTXT,NCXTXT,IFAIL1)
            ELSEIF(NARG.GE.4.AND.
     -           ARGREF(4,2).GE.1.AND.ARGREF(4,2).LE.NGLB)THEN
                 XTXT=GLBVAR(ARGREF(4,2))
                 NCXTXT=LEN(GLBVAR(ARGREF(4,2)))
                 IFAIL1=0
            ELSE
                 XTXT='x-axis'
                 NCXTXT=6
                 IFAIL1=0
            ENDIF
*   Fetch the y-axis label.
            IF(NARG.GE.7)THEN
                 CALL STRBUF('READ',NINT(ARG(7)),YTXT,NCYTXT,IFAIL2)
            ELSEIF(NARG.GE.5.AND.
     -           ARGREF(5,2).GE.1.AND.ARGREF(5,2).LE.NGLB)THEN
                 YTXT=GLBVAR(ARGREF(5,2))
                 NCYTXT=LEN(GLBVAR(ARGREF(5,2)))
                 IFAIL2=0
            ELSE
                 YTXT='y-axis'
                 NCYTXT=6
                 IFAIL2=0
            ENDIF
*   Fetch the global title.
            IF(NARG.GE.8)THEN
                 CALL STRBUF('READ',NINT(ARG(8)),TITLE,NCTIT,IFAIL3)
            ELSEIF(NARG.GE.1.AND.
     -           ARGREF(1,2).GE.1.AND.ARGREF(1,2).LE.NGLB)THEN
                 TITLE=GLBVAR(ARGREF(1,2))
                 NCTIT=LEN(GLBVAR(ARGREF(1,2)))
                 IFAIL3=0
            ELSE
                 TITLE='Matrix contours'
                 NCTIT=15
                 IFAIL3=0
            ENDIF
*   Plot the surface.
            CALL MATCON(NINT(ARG(1)),IREFX,IREFY,XTXT(1:NCXTXT),
     -           YTXT(1:NCYTXT),TITLE(1:NCTIT),NCONT,OPTION(1:NCOPT))
*   Switch back to normal screen.
            CALL GRALPH
*   Error processing.
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0.OR.
     -           IFAIL4.NE.0)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Error'//
     -                ' retrieving a string for PLOT_CONTOURS.'
                 RETURN
            ENDIF
*** Derivative.
       ELSEIF(IPROC.EQ.-95)THEN
*   Check argument list.
            IF(NARG.LT.4.OR.NARG.GT.5.OR.
     -           MODARG(1).NE.5.OR.MODARG(2).NE.5.OR.MODARG(3).NE.2.OR.
     -           ARGREF(4,1).GE.2.OR.
     -           (NARG.GE.5.AND.MODARG(5).NE.1))THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Some arguments of'//
     -                ' DERIVATIVE are of incorrect type.'
                 RETURN
            ENDIF
*   Get hold of the option string.
            IF(NARG.GE.5)THEN
                 CALL STRBUF('READ',NINT(ARG(5)),TITLE,NCTIT,IFAIL1)
                 CALL CLTOU(TITLE(1:NCTIT))
            ELSE
                 TITLE=' '
                 NCTIT=1
                 IFAIL1=0
            ENDIF
*   Calculate the derivative.
            CALL MATDER(NINT(ARG(1)),NINT(ARG(2)),ARG(3),AUX,
     -           TITLE(1:NCTIT),IFAIL2)
*   Clear the memory associated with the return argument.
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
*   Return the result.
            ARG(4)=AUX
            MODARG(4)=2
*   Error processing.
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Error'//
     -                ' processing a DERIVATIVE call.'
                 RETURN
            ENDIF
*** Interpolation of various orders.
       ELSEIF(IPROC.EQ.-96.OR.IPROC.EQ.-97.OR.IPROC.EQ.-98.OR.
     -      IPROC.EQ.-99)THEN
*   Check argument list.
            IF(NARG.NE.4.OR.ARGREF(4,1).GE.2.OR.
     -           MODARG(1).NE.5.OR.MODARG(2).NE.5)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Some arguments of'//
     -                ' INTERPOLATE_i are of incorrect type.'
                 RETURN
            ENDIF
*   Clear the output argument.
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
**  Number as point to be interpolated at.
            IF(MODARG(3).EQ.2)THEN
*   Interpolate.
                 ISDUM1=-1
                 ISDUM2=-1
                 IF(IPROC.EQ.-96)THEN
                      CALL MATIN1(NINT(ARG(1)),NINT(ARG(2)),1,
     -                     ARG(3),ARG(4),ISDUM1,ISDUM2,1,IFAIL1)
                 ELSEIF(IPROC.EQ.-97)THEN
                      CALL MATIN1(NINT(ARG(1)),NINT(ARG(2)),1,
     -                     ARG(3),ARG(4),ISDUM1,ISDUM2,2,IFAIL1)
                 ELSEIF(IPROC.EQ.-98)THEN
                      CALL MATIN1(NINT(ARG(1)),NINT(ARG(2)),1,
     -                     ARG(3),ARG(4),ISDUM1,ISDUM2,3,IFAIL1)
                 ELSEIF(IPROC.EQ.-99)THEN
                      CALL MATIN1(NINT(ARG(1)),NINT(ARG(2)),1,
     -                     ARG(3),ARG(4),ISDUM1,ISDUM2,4,IFAIL1)
                 ENDIF
*   Return the result.
                 MODARG(4)=2
*   Check the error condition.
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! MATCAL WARNING : INTERPOLATE_n'//
     -                     ' did not work correctly; no interpolation.'
                      RETURN
                 ENDIF
**  A Matrix as argument.
            ELSEIF(MODARG(3).EQ.5)THEN
*   Locate the input matrix.
                 ISLOTX=MATSLT(NINT(ARG(3)))
                 IF(ISLOTX.LE.0)THEN
                      PRINT *,' !!!!!! MATCAL WARNING : Unable to'//
     -                     ' locate the ordinate matrix;'//
     -                     ' no interpolation.'
                      RETURN
                 ENDIF
*   Allocate an output matrix with the same dimensions as the input.
                 NDIM=MDIM(ISLOTX)
                 DO 193 I=1,NDIM
                 ISIZ(I)=MSIZ(ISLOTX,I)
193              CONTINUE
                 CALL MATADM('ALLOCATE',IREFY,NDIM,ISIZ,2,IFAIL1)
*   Check the error condition.
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! MATCAL WARNING : INTERPOLATE_n'//
     -                     ' could not allocate the output matrix;'//
     -                     ' no interpolation.'
                      RETURN
                 ENDIF
*   Re-locate the input matrix.
                 ISLOTX=MATSLT(NINT(ARG(3)))
                 IF(ISLOTX.LE.0)THEN
                      PRINT *,' !!!!!! MATCAL WARNING : Unable to'//
     -                     ' locate the ordinate matrix;'//
     -                     ' no interpolation.'
                      RETURN
                 ENDIF
*   Locate the output matrix.
                 ISLOTY=MATSLT(IREFY)
                 IF(ISLOTY.LE.0)THEN
                      PRINT *,' !!!!!! MATCAL WARNING : Unable to'//
     -                     ' locate the output matrix;'//
     -                     ' no interpolation.'
                      RETURN
                 ENDIF
*   Interpolate.
                 ISDUM1=-1
                 ISDUM2=-1
                 IF(IPROC.EQ.-96)THEN
                      CALL MATIN1(
     -                     NINT(ARG(1)),NINT(ARG(2)),MLEN(ISLOTX),
     -                     MVEC(MORG(ISLOTX)+1),MVEC(MORG(ISLOTY)+1),
     -                     ISDUM1,ISDUM2,1,IFAIL1)
                 ELSEIF(IPROC.EQ.-97)THEN
                      CALL MATIN1(
     -                     NINT(ARG(1)),NINT(ARG(2)),MLEN(ISLOTX),
     -                     MVEC(MORG(ISLOTX)+1),MVEC(MORG(ISLOTY)+1),
     -                     ISDUM1,ISDUM2,2,IFAIL1)
                 ELSEIF(IPROC.EQ.-98)THEN
                      CALL MATIN1(
     -                     NINT(ARG(1)),NINT(ARG(2)),MLEN(ISLOTX),
     -                     MVEC(MORG(ISLOTX)+1),MVEC(MORG(ISLOTY)+1),
     -                     ISDUM1,ISDUM2,3,IFAIL1)
                 ELSEIF(IPROC.EQ.-99)THEN
                      CALL MATIN1(
     -                     NINT(ARG(1)),NINT(ARG(2)),MLEN(ISLOTX),
     -                     MVEC(MORG(ISLOTX)+1),MVEC(MORG(ISLOTY)+1),
     -                     ISDUM1,ISDUM2,4,IFAIL1)
                 ENDIF
*   Return the results.
                 ARG(4)=IREFY
                 MODARG(4)=5
**  Anything else.
            ELSE
                 PRINT *,' !!!!!! MATCAL WARNING : The ordinates'//
     -                ' should either be a Number of a Matrix;'//
     -                ' no interpolation.'
                 RETURN
            ENDIF
*** Plot an error band.
       ELSEIF(IPROC.EQ.-101)THEN
*   Check number of arguments.
            IF(NARG.NE.3.OR.MODARG(1).NE.5.OR.MODARG(2).NE.5.OR.
     -           MODARG(3).NE.5)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Incorrect set of'//
     -                ' arguments for ERROR_BAND.'
                 RETURN
            ENDIF
*   Switch to graphics screen.
            CALL GRGRAF(.FALSE.)
*   Plot the error band.
            CALL MATBND(NINT(ARG(1)),NINT(ARG(2)),NINT(ARG(3)))
*   Switch back to alphanumeric screen.
            CALL GRALPH
*** Find zeroes of a matrix vs another matrix.
       ELSEIF(IPROC.EQ.-102)THEN
*   Check the arguments.
            IWRONG=0
            DO 240 I=4,NARG
            IF(ARGREF(I,1).GE.2)IWRONG=IWRONG+1
240         CONTINUE
            IF(NARG.LT.3.OR.MODARG(1).NE.5.OR.MODARG(2).NE.5.OR.
     -           ARGREF(3,1).GE.2.OR.IWRONG.GT.0)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Incorrect set of'//
     -                ' arguments for ZEROES; no zero search.'
                 RETURN
            ENDIF
*   Get the zero crossings.
            CALL MATZRO(NINT(ARG(1)),NINT(ARG(2)),NZERO,ZERO,IFAIL1)
            ARG(3)=REAL(NZERO)
            MODARG(3)=2
            DO 190 I=4,MXARG
            CALL ALGREU(NINT(ARG(I)),MODARG(I),ARGREF(I,1))
            IF(I-3.LE.NZERO)THEN
                 ARG(I)=ZERO(I-3)
                 MODARG(I)=2
            ELSE
                 ARG(I)=0
                 MODARG(I)=0
            ENDIF
190         CONTINUE
*** Plot a bar chart.
       ELSEIF(IPROC.EQ.-103)THEN
*   Check number and type of arguments.
            IF(MODARG(1).NE.5.OR.MODARG(2).NE.5.OR.
     -           (NARG.GE.3.AND.MODARG(3).NE.1).OR.
     -           (NARG.GE.4.AND.MODARG(4).NE.1).OR.
     -           (NARG.GE.5.AND.MODARG(5).NE.1).OR.
     -           (NARG.GE.6.AND.MODARG(6).NE.1).OR.
     -           NARG.LT.2.OR.NARG.GT.6)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Incorrect argument'//
     -                ' list provided for PLOT_BARCHART.'
                 RETURN
            ENDIF
*   Ensure the matrices have the same length.
            ISLOTX=MATSLT(NINT(ARG(1)))
            ISLOTY=MATSLT(NINT(ARG(2)))
            IF(ISLOTX.LE.0.OR.ISLOTY.LE.0)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Unable to locate'//
     -                ' one of the matrices to be plotted; bar chart'//
     -                ' not plotted.'
                 RETURN
            ELSEIF(MLEN(ISLOTX).NE.MLEN(ISLOTY).OR.
     -           MLEN(ISLOTX).LT.1)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : The matrices have'//
     -                ' different size or insufficient total length;'//
     -                ' bar chart not plotted.'
                 RETURN
            ENDIF
*   Check option.
            FRAME=.TRUE.
            IF(NARG.GE.6)THEN
                 CALL STRBUF('READ',NINT(ARG(6)),TITLE,NCTIT,IFAIL1)
                 IF(NCTIT.LT.1)THEN
                      TITLE=' '
                      NCTIT=1
                 ENDIF
                 CALL CLTOU(TITLE(1:NCTIT))
                 IF(INDEX(TITLE(1:NCTIT),'NOFRAME').NE.0)THEN
                      FRAME=.FALSE.
                 ELSEIF(INDEX(TITLE(1:NCTIT),'FRAME').NE.0)THEN
                      FRAME=.TRUE.
                 ENDIF
            ENDIF
*   Fetch titles.
            XTXT='*'
            NCXTXT=1
            IF(NARG.GE.3)THEN
                 CALL STRBUF('READ',NINT(ARG(3)),XTXT,NCXTXT,IFAIL1)
                 IF(IFAIL1.NE.0)XTXT='*'
                 IF(IFAIL1.NE.0)NCXTXT=1
            ENDIF
            IF((NARG.LT.3.OR.XTXT(1:NCXTXT).EQ.'*').AND.
     -           ARGREF(1,2).GE.1.AND.ARGREF(1,2).LE.NGLB)THEN
                 XTXT=GLBVAR(ARGREF(1,2))
                 NCXTXT=LEN(GLBVAR(ARGREF(1,2)))
            ENDIF
            IF(XTXT(1:NCXTXT).EQ.'*')THEN
                 XTXT='Ordinate'
                 NCXTXT=8
            ENDIF
            YTXT='*'
            NCYTXT=1
            IF(NARG.GE.4)THEN
                 CALL STRBUF('READ',NINT(ARG(4)),YTXT,NCYTXT,IFAIL1)
                 IF(IFAIL1.NE.0)YTXT='*'
                 IF(IFAIL1.NE.0)NCYTXT=1
            ENDIF
            IF((NARG.LT.4.OR.YTXT(1:NCYTXT).EQ.'*').AND.
     -           ARGREF(2,2).GE.1.AND.ARGREF(2,2).LE.NGLB)THEN
                 YTXT=GLBVAR(ARGREF(2,2))
                 NCYTXT=LEN(GLBVAR(ARGREF(2,2)))
            ENDIF
            IF(YTXT(1:NCYTXT).EQ.'*')THEN
                 YTXT='Coordinate'
                 NCYTXT=10
            ENDIF
            TITLE='*'
            NCTIT=1
            IF(NARG.GE.5)THEN
                 CALL STRBUF('READ',NINT(ARG(5)),TITLE,NCTIT,IFAIL1)
                 IF(IFAIL1.NE.0)TITLE='*'
                 IF(IFAIL1.NE.0)NCTIT=1
            ENDIF
            IF(NARG.LT.5.OR.TITLE(1:NCTIT).EQ.'*')THEN
                 TITLE=XTXT(1:NCXTXT)//' vs '//YTXT(1:NCYTXT)
                 NCTIT=MIN(LEN(TITLE),NCXTXT+4+NCYTXT)
            ENDIF
*   Plot.
            CALL GRBAR(MVEC(MORG(ISLOTX)+1),MVEC(MORG(ISLOTY)+1),
     -           MLEN(ISLOTX),XTXT(1:NCXTXT),YTXT(1:NCYTXT),
     -           TITLE(1:NCTIT),FRAME)
*** Sort a matrix.
       ELSEIF(IPROC.EQ.-104)THEN
*   Check number and type of arguments.
            IF(MODARG(1).NE.5.OR.ARGREF(1,1).GE.2.OR.NARG.NE.1)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Incorrect'//
     -                ' argument list for SORT_MATRIX; not called.'
                 RETURN
            ENDIF
*   Perform the sort.
            CALL MATSRT(NINT(ARG(1)),IFAIL1)
*   Check the error indicator.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Error processing'//
     -                ' a SORT_MATRIX call.'
                 RETURN
            ENDIF
*** Rainbow 3D.
       ELSEIF(IPROC.EQ.-105)THEN
*   Check argument list.
            IF(NARG.LT.1.OR.NARG.GT.10.OR.
     -           MODARG(1).NE.5.OR.
     -           (NARG.GE.2.AND.MODARG(2).NE.2).OR.
     -           (NARG.GE.3.AND.MODARG(3).NE.2).OR.
     -           (NARG.GE.4.AND.MODARG(4).NE.5).OR.
     -           (NARG.GE.5.AND.MODARG(5).NE.5).OR.
     -           (NARG.GE.6.AND.MODARG(6).NE.1).OR.
     -           (NARG.GE.7.AND.MODARG(7).NE.1).OR.
     -           (NARG.GE.8.AND.MODARG(8).NE.1).OR.
     -           (NARG.GE.9.AND.MODARG(9).NE.1).OR.
     -           (NARG.GE.10.AND.MODARG(10).NE.1))THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Some arguments of'//
     -                ' PLOT_3D are of incorrect type.'
                 RETURN
            ENDIF
*   Plotting angles.
            IF(NARG.GE.2)THEN
                 THETA=ARG(2)
            ELSE
                 THETA=60
            ENDIF
            IF(NARG.GE.3)THEN
                 PHI=ARG(3)
            ELSE
                 PHI=60
            ENDIF
*   Axis ranges.
            IF(NARG.GE.4)THEN
                 IREFX=NINT(ARG(4))
            ELSE
                 IREFX=-1
            ENDIF
            IF(NARG.GE.5)THEN
                 IREFY=NINT(ARG(5))
            ELSE
                 IREFY=-1
            ENDIF
*   Fetch the x-axis label.
            IF(NARG.GE.6)THEN
                 CALL STRBUF('READ',NINT(ARG(6)),XTXT,NCXTXT,IFAIL1)
            ELSEIF(NARG.GE.4)THEN
                 DO 300 J=1,NGLB
                 IF(GLBMOD(J).NE.5)GOTO 300
                 IF(NINT(GLBVAL(J)).EQ.IREFX)THEN
                      XTXT=GLBVAR(J)
                      NCXTXT=10
                      GOTO 310
                 ENDIF
300              CONTINUE
                 XTXT='x-axis'
                 NCXTXT=6
310              CONTINUE
                 IFAIL1=0
            ELSE
                 XTXT='x-axis'
                 NCXTXT=6
                 IFAIL1=0
            ENDIF
*   Fetch the y-axis label.
            IF(NARG.GE.7)THEN
                 CALL STRBUF('READ',NINT(ARG(7)),YTXT,NCYTXT,IFAIL2)
            ELSEIF(NARG.GE.5)THEN
                 DO 320 J=1,NGLB
                 IF(GLBMOD(J).NE.5)GOTO 320
                 IF(NINT(GLBVAL(J)).EQ.IREFY)THEN
                      YTXT=GLBVAR(J)
                      NCYTXT=10
                      GOTO 330
                 ENDIF
320              CONTINUE
                 YTXT='y-axis'
                 NCYTXT=6
330              CONTINUE
                 IFAIL2=0
            ELSE
                 YTXT='y-axis'
                 NCYTXT=6
                 IFAIL2=0
            ENDIF
*   Fetch the z-axis label.
            IF(NARG.GE.8)THEN
                 CALL STRBUF('READ',NINT(ARG(8)),ZTXT,NCZTXT,IFAIL3)
            ELSE
                 ZTXT='z-axis'
                 NCZTXT=6
                 IFAIL3=0
            ENDIF
*   Fetch the global title.
            IF(NARG.GE.9)THEN
                 CALL STRBUF('READ',NINT(ARG(9)),TITLE,NCTIT,IFAIL4)
            ELSE
                 DO 340 J=1,NGLB
                 IF(GLBMOD(J).NE.5)GOTO 340
                 IF(NINT(GLBVAL(J)).EQ.NINT(ARG(1)))THEN
                      TITLE=GLBVAR(J)
                      NCTIT=10
                      GOTO 350
                 ENDIF
350              CONTINUE
                 TITLE=' '
                 NCTIT=1
340              CONTINUE
                 IFAIL4=0
            ENDIF
*   Fetch the options
            IF(NARG.GE.10)THEN
                 CALL STRBUF('READ',NINT(ARG(10)),OPTION,NCOPT,IFAIL5)
                 IF(NCOPT.LE.0)THEN
                      OPTION=' '
                      NCOPT=1
                 ENDIF
                 CALL CLTOU(OPTION(1:NCOPT))
            ELSE
                 OPTION=' '
                 NCOPT=1
                 IFAIL5=0
            ENDIF
*   Plot the surface.
            CALL MAT3D(NINT(ARG(1)),IREFX,IREFY,XTXT(1:NCXTXT),
     -           YTXT(1:NCYTXT),ZTXT(1:NCZTXT),TITLE(1:NCTIT),
     -           PHI,THETA,OPTION(1:NCOPT))
*   Switch back to normal screen.
            CALL GRALPH
*   Error processing.
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0.OR.
     -           IFAIL4.NE.0.OR.IFAIL5.NE.0)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Error'//
     -                ' retrieving a string for PLOT_3D.'
                 RETURN
            ENDIF
*** Locate maximum or minimum.
       ELSEIF(IPROC.EQ.-106.OR.IPROC.EQ.-107)THEN
*   Check argument list.
            IF(NARG.LT.2)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Number of'//
     -                ' arguments of LOCATE_MAXIMUM or'//
     -                ' LOCATE_MINIMUM not correct.'
                 RETURN
            ENDIF
*   Clear up old arguments.
            DO 360 I=2,NARG
            CALL ALGREU(NINT(ARG(I)),MODARG(I),ARGREF(I,1))
360         CONTINUE
*   Compute the maximum or minimum.
            IF(IPROC.EQ.-106)THEN
                 CALL MATLOC(NINT(ARG(1)),IMAX,NMAX,+1,IFAIL1)
            ELSE
                 CALL MATLOC(NINT(ARG(1)),IMAX,NMAX,-1,IFAIL1)
            ENDIF
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Error computing'//
     -                ' the maximum/minimum; no result returned.'
                 RETURN
            ENDIF
*   Return the results.
            IF(NMAX.GT.MXARG-1.OR.NMAX.GT.NARG-1)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Not enough'//
     -                ' arguments to return all indices; remainder'//
     -                ' not returned.'
            ELSEIF(NARG.GT.NMAX+1)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : More arguments'//
     -                ' than indices to return; remainder set to Nill.'
            ENDIF
            DO 370 I=1,MIN(NMAX,MXARG-1,NARG-1)
            ARG(I+1)=IMAX(I)
            MODARG(I+1)=2
370         CONTINUE
*** Smoothen
       ELSEIF(IPROC.EQ.-108)THEN
*   Check argument list.
            IF(NARG.LT.1.OR.NARG.GT.3.OR.
     -           MODARG(1).NE.5.OR.
     -           ARGREF(1,1).GE.2.OR.
     -           (NARG.GE.2.AND.MODARG(2).NE.2).OR.
     -           (NARG.GE.3.AND.MODARG(3).NE.2))THEN
                 PRINT *,' !!!!!! MATCAL WARNING : SMOOTH takes'//
     -                ' 1 Matrix and up to 2 Number arguments.'
                 RETURN
            ENDIF
*   Locate the matrix.
            ISLOTW=MATSLT(NINT(ARG(1)))
            IF(ISLOTW.LE.0)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : Unable to'//
     -                ' locate matrix to be smoothed.'
                 RETURN
            ENDIF
*   Default values
            IF(NARG.GE.2)THEN
                 NW=NINT(ARG(2))
            ELSE
                 NW=NINT(REAL(MLEN(ISLOTW))/100.0)
            ENDIF
            IF(NARG.GE.3)THEN
                 SIGW=ARG(3)
            ELSE
                 SIGW=REAL(NW)/3
            ENDIF
*   Smoothen
            CALL SMOOTH(MVEC(MORG(ISLOTW)+1), MLEN(ISLOTW), NW, SIGW)
*** Cumulate
       ELSEIF(IPROC.EQ.-109)THEN
*   Check argument list.
            IF(NARG.NE.2.OR.
     -           MODARG(1).NE.5.OR.
     -           ARGREF(2,1).GE.2)THEN
                 PRINT *,' !!!!!! MATCAL WARNING : CUMULATE_MATRIX'//
     -                ' takes 2 Matrix arguments, the 2nd modifiable.'
                 RETURN
            ENDIF
*   Clean up
            CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
*   Cumulate
            CALL MATCUM(NINT(ARG(1)), IREF, IFAIL1)
            IF(IFAIL1.NE.0)RETURN
            ARG(2)=IREF
            MODARG(2)=5
*** Unknown matrix operation.
       ELSE
            PRINT *,' !!!!!! MATCAL WARNING : Unknown procedure code'//
     -           ' received; nothing done.'
            IFAIL=1
            RETURN
       ENDIF
*** Seems to have worked.
       IFAIL=0
       END
