CDECK  ID>, HISCAL.
       SUBROUTINE HISCAL(INSTR,IFAIL)
*-----------------------------------------------------------------------
*   HISCAL - Processes histogram related procedure calls.
*   (Last changed on 28/ 8/02.)
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
       DOUBLE PRECISION CONTEN(MXHIST,0:MXCHA+1)
       REAL XMIN(MXHIST),XMAX(MXHIST)
       DOUBLE PRECISION SX0(MXHIST),SX1(MXHIST),SX2(MXHIST)
       INTEGER NCHA(MXHIST),NENTRY(MXHIST)
       LOGICAL SET(MXHIST),HISUSE(MXHIST),HISLIN(MXHIST)
       COMMON /HISDAT/ SX0,SX1,SX2,CONTEN,XMIN,XMAX,HISUSE,HISLIN,NCHA,
     -      NENTRY,SET
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       CHARACTER*(MXINCH) STRING
       CHARACTER*80 XTXT,TITLE
       CHARACTER*8 MEMBER
       REAL  XXMIN,XXMAX,AVER,SIGMA,HMIN,HMAX,ENTRY,WEIGHT,XX
       LOGICAL HAUTO,HEXIST,HSET,FRAME,HINT
       INTEGER INPCMX,IPROC,NARG,IFAIL,INSTR,NNCHA,NNENTR,IREF,IFAIL1,
     -      IFAIL2,IFAIL3,NCMEMB,NCREM,NCXTXT,NC,NCTITL,I,J,ISTR,NBIN,
     -      IHISRF,IMATRF,IMATSL,MATSLT,ISIZ(1),NHIST,ISENT,ISWGT,
     -      NSIZE
       EXTERNAL INPCMX,MATSLT
*** Assume the CALL will fail.
       IFAIL=1
*** Some easy reference variables.
       NARG=INS(INSTR,3)
       IPROC=INS(INSTR,1)
*** Book a histogram.
       IF(IPROC.EQ.-602)THEN
*   Check number and type of arguments.
            IF(NARG.LT.1.OR.NARG.GT.5.OR.
     -           (NARG.EQ.2.AND.MODARG(2).NE.2.AND.MODARG(2).NE.1).OR.
     -           (NARG.GT.2.AND.MODARG(2).NE.2).OR.
     -           (NARG.EQ.3.AND.MODARG(3).NE.1).OR.
     -           (NARG.GE.4.AND.(MODARG(3).NE.2.OR.MODARG(4).NE.2)).OR.
     -           (NARG.GE.5.AND.MODARG(5).NE.1))THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Incorrect argument'//
     -                ' list provided for BOOK_HISTOGRAM.'
                 RETURN
            ENDIF
*   Check that the reference number can be transferred back.
            IF(ARGREF(1,1).GE.2)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Unable to return'//
     -                ' the histogram reference to calling procedure.'
                 RETURN
            ENDIF
*   Free memory associated with argument.
            CALL ALGREU(NINT(ARG(1)),MODARG(1),ARGREF(1,1))
*   Store arguments.
            IF(NARG.GE.2)THEN
                 NNCHA=NINT(ARG(2))
            ELSE
                 NNCHA=100
            ENDIF
            IF(NARG.GE.4)THEN
                 HMIN=ARG(3)
                 HMAX=ARG(4)
                 HAUTO=.FALSE.
            ELSE
                 HMIN=-1
                 HMAX=+1
                 HAUTO=.TRUE.
            ENDIF
            HINT=.FALSE.
            IF(MODARG(NARG).EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(NARG)),XTXT,NCXTXT,IFAIL1)
                 IF(NCXTXT.LE.0)THEN
                      XTXT=' '
                      NCXTXT=1
                 ENDIF
                 CALL CLTOU(XTXT(1:NCXTXT))
                 IF(INDEX(XTXT(1:MAX(1,NCXTXT)),'MANUAL').NE.0)THEN
                      IF(NARG.LT.5)THEN
                           PRINT *,' !!!!!! HISCAL WARNING : The'//
     -                          ' MANUAL option requires the range'//
     -                          ' to be specified; assuming AUTO.'
                      ELSE
                           HAUTO=.FALSE.
                      ENDIF
                 ELSEIF(INDEX(XTXT(1:MAX(1,NCXTXT)),'AUTO').NE.0)THEN
                      HAUTO=.TRUE.
                 ENDIF
                 IF(INDEX(XTXT(1:MAX(1,NCXTXT)),'INTEGER').NE.0)THEN
                      HINT=.TRUE.
                 ELSEIF(INDEX(XTXT(1:MAX(1,NCXTXT)),'REAL').NE.0)THEN
                      HINT=.FALSE.
                 ENDIF
            ENDIF
*   Book the histogram.
            IF(HINT)THEN
                 CALL HISADM('INTEGER',IHISRF,NNCHA,HMIN,HMAX,
     -                HAUTO,IFAIL1)
            ELSE
                 CALL HISADM('ALLOCATE',IHISRF,NNCHA,HMIN,HMAX,
     -                HAUTO,IFAIL1)
            ENDIF
*   Back-transfer the reference number.
            IF(IFAIL1.EQ.0)THEN
                 ARG(1)=IHISRF
                 MODARG(1)=4
            ELSE
                 PRINT *,' !!!!!! HISCAL WARNING : Unable to allocate'//
     -                ' the histogram.'
                 ARG(1)=0
                 MODARG(1)=0
                 RETURN
            ENDIF
*** Fill histogram.
       ELSEIF(IPROC.EQ.-603)THEN
*   Check number and type of arguments.
            IF(MODARG(1).NE.4.OR.
     -           (MODARG(2).NE.2.AND.MODARG(2).NE.5).OR.
     -           (NARG.GE.3.AND.MODARG(3).NE.2.AND.MODARG(3).NE.5).OR.
     -           NARG.LT.2.OR.NARG.GT.3)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Incorrect argument'//
     -                ' list provided for FILL_HISTOGRAM.'
                 RETURN
            ENDIF
*   Locate entries.
            IF(MODARG(2).EQ.5)THEN
                 ISENT=MATSLT(NINT(ARG(2)))
                 IF(ISENT.LE.0)THEN
                      PRINT *,' !!!!!! HISCAL WARNING : Unable'//
     -                     ' to locate the entries; no filling.'
                      RETURN
                 ENDIF
            ELSE
                 ISENT=0
            ENDIF
*   Locate weights.
            IF(MODARG(3).EQ.5.AND.NARG.GE.3)THEN
                 ISWGT=MATSLT(NINT(ARG(3)))
                 IF(ISWGT.LE.0)THEN
                      PRINT *,' !!!!!! HISCAL WARNING : Unable'//
     -                     ' to locate the weights; no filling.'
                      RETURN
                 ENDIF
            ELSE
                 ISWGT=0
            ENDIF
*   Verify compatibility.
            IF(ISENT.NE.0.AND.ISWGT.NE.0)THEN
                 IF(MLEN(ISENT).NE.MLEN(ISWGT))THEN
                      PRINT *,' !!!!!! HISCAL WARNING : Entry'//
     -                     ' and weight vectors are not'//
     -                     ' compatible; no filling.'
                      RETURN
                 ELSE
                      NSIZE=MLEN(ISENT)
                 ENDIF
            ELSEIF(ISENT.NE.0)THEN
                 NSIZE=MLEN(ISENT)
            ELSEIF(ISWGT.NE.0)THEN
                 NSIZE=MLEN(ISWGT)
            ELSE
                 NSIZE=1
            ENDIF
*   Perform filling.
            DO 110 I=1,NSIZE
            IF(ISENT.EQ.0)THEN
                 ENTRY=ARG(2)
            ELSE
                 ENTRY=MVEC(MORG(ISENT)+I)
            ENDIF
            IF(ISWGT.EQ.0)THEN
                 IF(NARG.GE.3)THEN
                      WEIGHT=ARG(3)
                 ELSE
                      WEIGHT=1.0
                 ENDIF
            ELSE
                 WEIGHT=MVEC(MORG(ISWGT)+I)
            ENDIF
            CALL HISENT(NINT(ARG(1)),ENTRY,WEIGHT)
110         CONTINUE
*** Plot a histogram.
       ELSEIF(IPROC.EQ.-604)THEN
*   Check number and type of arguments.
            IF(MODARG(1).NE.4.OR.
     -           (NARG.GE.2.AND.MODARG(2).NE.1).OR.
     -           (NARG.GE.3.AND.MODARG(3).NE.1).OR.
     -           (NARG.GE.4.AND.MODARG(4).NE.1).OR.
     -           NARG.LT.1.OR.NARG.GT.4)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Incorrect argument'//
     -                ' list provided for PLOT_HISTOGRAM.'
                 RETURN
            ENDIF
*   Check option.
            FRAME=.TRUE.
            IF(NARG.GE.4)THEN
                 CALL STRBUF('READ',NINT(ARG(4)),TITLE,NCTITL,IFAIL1)
                 IF(NCTITL.LT.1)THEN
                      TITLE=' '
                      NCTITL=1
                 ENDIF
                 CALL CLTOU(TITLE(1:NCTITL))
                 IF(INDEX(TITLE(1:NCTITL),'NOFRAME').NE.0)THEN
                      FRAME=.FALSE.
                 ELSEIF(INDEX(TITLE(1:NCTITL),'FRAME').NE.0)THEN
                      FRAME=.TRUE.
                 ENDIF
            ENDIF
*   Fetch titles.
            IF(NARG.GE.2)THEN
                 CALL STRBUF('READ',NINT(ARG(2)),XTXT,NCXTXT,IFAIL1)
                 IF(IFAIL1.NE.0)XTXT=' '
                 IF(IFAIL1.NE.0)NCXTXT=1
            ELSE
                 XTXT='Coordinate'
                 NCXTXT=10
            ENDIF
            IF(NARG.GE.3)THEN
                 CALL STRBUF('READ',NINT(ARG(3)),TITLE,NCTITL,IFAIL2)
                 IF(IFAIL2.NE.0)TITLE=' '
                 IF(IFAIL2.NE.0)NCTITL=1
            ELSE
                 TITLE='Title'
                 NCTITL=5
            ENDIF
            IF((NARG.LT.3.OR.TITLE(1:NCTITL).EQ.'*').AND.
     -           ARGREF(1,2).GE.1.AND.ARGREF(1,2).LE.NGLB)THEN
                 TITLE=GLBVAR(ARGREF(1,2))
                 NCTITL=LEN(GLBVAR(ARGREF(1,2)))
            ENDIF
*   Plot.
            CALL HISPLT(NINT(ARG(1)),XTXT(1:NCXTXT),TITLE(1:NCTITL),
     -           FRAME)
*** Print a histogram.
       ELSEIF(IPROC.EQ.-605)THEN
*   Check number and type of arguments.
            IF(MODARG(1).NE.4.OR.
     -           (NARG.GE.2.AND.MODARG(2).NE.1).OR.
     -           (NARG.GE.3.AND.MODARG(3).NE.1).OR.
     -           NARG.LT.1.OR.NARG.GT.3)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Incorrect argument'//
     -                ' list provided for PRINT_HISTOGRAM.'
                 RETURN
            ENDIF
*   Fetch strings.
            IF(NARG.GE.2)THEN
                 CALL STRBUF('READ',NINT(ARG(2)),XTXT,NCXTXT,IFAIL1)
                 IF(IFAIL1.NE.0)XTXT=' '
                 IF(IFAIL1.NE.0)NCXTXT=1
            ELSE
                 XTXT='Coordinate'
                 NCXTXT=10
            ENDIF
            IF(NARG.GE.3)THEN
                 CALL STRBUF('READ',NINT(ARG(3)),TITLE,NCTITL,IFAIL2)
                 IF(IFAIL2.NE.0)TITLE=' '
                 IF(IFAIL2.NE.0)NCTITL=1
            ELSE
                 TITLE='Title'
                 NCTITL=5
            ENDIF
            IF((NARG.LT.3.OR.TITLE(1:NCTITL).EQ.'*').AND.
     -           ARGREF(1,2).GE.1.AND.ARGREF(1,2).LE.NGLB)THEN
                 TITLE=GLBVAR(ARGREF(1,2))
                 NCTITL=LEN(GLBVAR(ARGREF(1,2)))
            ENDIF
*   Print.
            CALL HISPRT(NINT(ARG(1)),XTXT(1:NCXTXT),TITLE(1:NCTITL))
*** Delete a histogram.
       ELSEIF(IPROC.EQ.-606)THEN
*   Without arguments, delete all histograms.
            IF(NARG.LT.1)THEN
                 DO 10 I=1,NGLB
                 IF(GLBMOD(I).EQ.4)THEN
                      CALL HISADM('DELETE',NINT(GLBVAL(I)),
     -                     0,0.0,0.0,.FALSE.,IFAIL1)
                      GLBVAL(I)=0
                      GLBMOD(I)=0
                 ENDIF
10               CONTINUE
                 CALL HISINT
*   Delete all the matrices in the arguments.
            ELSE
                 DO 40 I=1,NARG
                 IF(MODARG(I).NE.4)THEN
                      PRINT *,' !!!!!! HISCAL WARNING : Argument ',I,
     -                     ' is not an histogram; not deleted.'
                      GOTO 40
                 ENDIF
                 CALL HISADM('DELETE',NINT(ARG(I)),
     -                0,0.0,0.0,.FALSE.,IFAIL1)
                 ARG(I)=0
                 MODARG(I)=0
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! HISCAL WARNING :'//
     -                ' Deleting an histogram failed.'
40               CONTINUE
            ENDIF
*** List histograms.
       ELSEIF(IPROC.EQ.-607)THEN
*   Check number and type of arguments.
            IF(NARG.NE.0)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Incorrect argument'//
     -                ' list provided for LIST_HISTOGRAMS.'
                 RETURN
            ENDIF
*   List.
            CALL HISADM('LIST',0,0,0.0,0.0,.FALSE.,IFAIL1)
*** Write a histogram to disk.
       ELSEIF(IPROC.EQ.-608)THEN
*   Check number and type of arguments.
            IF(MODARG(1).NE.4.OR.MODARG(2).NE.1.OR.
     -           (NARG.GE.3.AND.MODARG(3).NE.1).OR.
     -           (NARG.GE.4.AND.MODARG(4).NE.1).OR.
     -           NARG.LT.2.OR.NARG.GT.4)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Incorrect argument'//
     -                ' list provided for WRITE_HISTOGRAM.'
                 RETURN
            ENDIF
*   Fetch file name.
            CALL STRBUF('READ',NINT(ARG(2)),STRING,NC,IFAIL1)
*   Member name.
            IF(NARG.GE.3)THEN
                 CALL STRBUF('READ',NINT(ARG(3)),MEMBER,NCMEMB,IFAIL2)
                 IF(NCMEMB.GT.8)PRINT *,' !!!!!! HISCAL WARNING :'//
     -                ' Member name truncated to first 8 characters'
                 NCMEMB=MIN(8,NCMEMB)
            ELSE
                 DO 20 J=1,NGLB
                 IF(GLBMOD(J).NE.4)GOTO 20
                 IF(NINT(GLBVAL(J)).EQ.NINT(ARG(1)))THEN
                      MEMBER=GLBVAR(J)
                      NCMEMB=8
                      GOTO 30
                 ENDIF
20               CONTINUE
                 MEMBER='< none >'
                 NCMEMB=8
30               CONTINUE
                 IFAIL2=0
            ENDIF
*   Remark.
            IF(NARG.GE.4)THEN
                 CALL STRBUF('READ',NINT(ARG(4)),TITLE,NCREM,IFAIL3)
                 IF(NCREM.GT.29)PRINT *,' !!!!!! HISCAL WARNING :'//
     -                ' Remark truncated to first 29 characters'
                 NCREM=MIN(29,NCREM)
            ELSE
                 TITLE='none'
                 NCREM=4
                 IFAIL3=0
            ENDIF
*  Write the histogram.
            IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                 CALL HISWRT(NINT(ARG(1)),STRING(1:NC),MEMBER(1:NCMEMB),
     -                TITLE(1:NCREM),IFAIL2)
                 IF(IFAIL2.NE.0)THEN
                      PRINT *,' !!!!!! HISCAL WARNING :'//
     -                     ' Writing histogram to disk failed.'
                      RETURN
                 ENDIF
            ELSE
                 PRINT *,' !!!!!! HISCAL WARNING :'//
     -                ' Not able to obtain a name; histogram'//
     -                ' not written to disk.'
                 RETURN
            ENDIF
*** Read a histogram from disk.
       ELSEIF(IPROC.EQ.-609)THEN
*   Check number and type of arguments.
            IF(ARGREF(1,1).GE.2.OR.
     -           MODARG(2).NE.1.OR.
     -           (NARG.GE.3.AND.MODARG(3).NE.1).OR.
     -           NARG.LT.2.OR.NARG.GT.3)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Incorrect argument'//
     -                ' list provided for GET_HISTOGRAM.'
                 RETURN
            ENDIF
*  Fetch file name.
            CALL STRBUF('READ',NINT(ARG(2)),STRING,NC,IFAIL1)
*  Fetch the member name, if present.
            IF(NARG.GE.3)THEN
                 CALL STRBUF('READ',NINT(ARG(3)),MEMBER,NCMEMB,IFAIL2)
                 IF(NCMEMB.GT.8)PRINT *,' !!!!!! HISCAL WARNING :'//
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
*  Read the histogram.
            IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0)THEN
                 CALL ALGREU(NINT(ARG(1)),MODARG(1),ARGREF(1,1))
                 CALL HISGET(IREF,STRING(1:NC),MEMBER(1:NCMEMB),IFAIL3)
                 IF(IFAIL3.NE.0)THEN
                      PRINT *,' !!!!!! HISCAL WARNING :'//
     -                     ' Reading histogram from disk failed.'
                      ARG(1)=0
                      MODARG(1)=0
                      RETURN
                 ELSE
                      ARG(1)=IREF
                      MODARG(1)=4
                 ENDIF
            ELSE
                 PRINT *,' !!!!!! HISCAL WARNING :'//
     -                ' Not able to obtain a name; histogram'//
     -                ' not read from disk.'
                 RETURN
            ENDIF
*** Obtain information about an histogram.
       ELSEIF(IPROC.EQ.-610)THEN
*   Check number and type of arguments.
            IF(MODARG(1).NE.4.OR.NARG.LT.2.OR.
     -           (NARG.GE.2.AND.ARGREF(2,1).GE.2).OR.
     -           (NARG.GE.3.AND.ARGREF(3,1).GE.2).OR.
     -           (NARG.GE.4.AND.ARGREF(4,1).GE.2).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2).OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2).OR.
     -           (NARG.GE.8.AND.ARGREF(8,1).GE.2).OR.
     -           (NARG.GE.9.AND.ARGREF(9,1).GE.2))THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Incorrect argument'//
     -                ' list provided for INQUIRE_HISTOGRAM.'
                 RETURN
            ENDIF
*  Obtain the information.
            CALL HISINQ(NINT(ARG(1)),HEXIST,HSET,NNCHA,XXMIN,XXMAX,
     -           NNENTR,AVER,SIGMA)
*   Variables already in use ?
            DO 250 ISTR=2,NARG
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
250         CONTINUE
*   Transfer information.
            IF(NARG.GE.2)THEN
                 IF(HEXIST)THEN
                      ARG(2)=1
                 ELSE
                      ARG(2)=0
                 ENDIF
                 MODARG(2)=3
            ENDIF
            IF(NARG.GE.3)THEN
                 MODARG(2)=3
                 IF(HSET)THEN
                      ARG(3)=1
                 ELSE
                      ARG(3)=0
                 ENDIF
                 MODARG(3)=3
            ENDIF
            IF(NARG.GE.4)THEN
                 ARG(4)=REAL(NNCHA)
                 MODARG(4)=2
            ENDIF
            IF(NARG.GE.5)THEN
                 ARG(5)=XXMIN
                 MODARG(5)=2
            ENDIF
            IF(NARG.GE.6)THEN
                 ARG(6)=XXMAX
                 MODARG(6)=2
            ENDIF
            IF(NARG.GE.7)THEN
                 ARG(7)=REAL(NNENTR)
                 MODARG(7)=2
            ENDIF
            IF(NARG.GE.8)THEN
                 ARG(8)=AVER
                 MODARG(8)=2
            ENDIF
            IF(NARG.GE.9)THEN
                 ARG(9)=SIGMA
                 MODARG(9)=2
            ENDIF
*** Convolute 2 histograms.
       ELSEIF(IPROC.EQ.-611)THEN
*   Check argument list.
            IF(NARG.NE.3.OR.MODARG(1).NE.4.OR.MODARG(2).NE.4.OR.
     -           ARGREF(3,1).GE.2)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Incorrect set of'//
     -                ' arguments given to CONVOLUTE; nothing done.'
                 RETURN
            ENDIF
*   Free memory associated with the return argument.
            CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
*   Perform the convolution.
            CALL HISCNV(NINT(ARG(1)),NINT(ARG(2)),IREF,IFAIL1)
*   Check the return code.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Convolution'//
     -                ' failed; no histogram returned.'
                 RETURN
            ELSE
                 ARG(3)=IREF
                 MODARG(3)=4
            ENDIF
*** Compute the barycentre of a histogram.
       ELSEIF(IPROC.EQ.-612)THEN
*   Check the argument list.
            IF(NARG.LT.2.OR.MODARG(1).NE.4.OR.ARGREF(2,1).GE.2.OR.
     -           (NARG.GE.3.AND.MODARG(3).NE.2).OR.
     -           NARG.GT.3)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Incorrect set of'//
     -                ' arguments given to BARYCENTRE; nothing done.'
                 RETURN
            ENDIF
*   Free memory associated with the return argument.
            CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
*   Pick up the number of bins.
            NBIN=3
            IF(NARG.GE.3)NBIN=NINT(ARG(3))
*   Compute the barycentre.
            CALL HISBAR(NINT(ARG(1)),NBIN,ARG(2),IFAIL1)
*   Check the return code.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Barycentre'//
     -                ' calculation failed; no value returned.'
                 MODARG(2)=0
                 RETURN
            ELSE
                 MODARG(2)=2
            ENDIF
*** Copy a histogram to a matrix.
       ELSEIF(IPROC.EQ.-613)THEN
*   Check argument list.
            IF(NARG.LT.2.OR.NARG.GT.4.OR.
     -           MODARG(1).NE.4.OR.
     -           ARGREF(2,1).GE.2.OR.
     -           (NARG.GE.3.AND.ARGREF(3,1).GE.2).OR.
     -           (NARG.GE.4.AND.ARGREF(4,1).GE.2))THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Incorrect set of'//
     -                ' arguments for HISTOGRAM_TO_MATRIX;'//
     -                ' nothing done.'
                 RETURN
            ENDIF
*   Check the histogram.
            IHISRF=NINT(ARG(1))
            IF(IHISRF.LE.0.OR.IHISRF.GT.MXHIST)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Invalid histogram'//
     -                ' reference; no copied to a matrix.'
                 RETURN
            ELSEIF((.NOT.HISUSE(IHISRF)).OR.(.NOT.SET(IHISRF)))THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Histogram not in'//
     -                ' use or range not set; no copied to a matrix.'
                 RETURN
            ENDIF
*   Free memory associated with the return argument.
            CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
            IF(NARG.GE.3)CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
            IF(NARG.GE.4)CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
*   Book a matrix for the contents.
            ISIZ(1)=NCHA(IHISRF)
            CALL MATADM('ALLOCATE',IMATRF,1,ISIZ,2,IFAIL1)
*   Locate the matrix.
            IMATSL=MATSLT(IMATRF)
            IF(IFAIL1.NE.0.OR.IMATSL.LE.0)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Unable to obtain'//
     -                ' matrix space ; histogram not copied.'
                 RETURN
            ENDIF
*   Copy the histogram to a matrix.
            DO 50 I=1,NCHA(IHISRF)
            MVEC(MORG(IMATSL)+I)=CONTEN(IHISRF,I)
50          CONTINUE
            ARG(2)=IMATRF
            MODARG(2)=5
*   And copy the ranges if requested.
            IF(NARG.GE.3)THEN
                 ARG(3)=XMIN(IHISRF)
                 MODARG(3)=2
            ENDIF
            IF(NARG.GE.4)THEN
                 ARG(4)=XMAX(IHISRF)
                 MODARG(4)=2
            ENDIF
*** Copy a matrix to a histogram.
       ELSEIF(IPROC.EQ.-614)THEN
*   Check argument list.
            IF(NARG.GT.4.OR.
     -           MODARG(1).NE.5.OR.MODARG(2).NE.2.OR.MODARG(3).NE.2.OR.
     -           ARGREF(4,1).GE.2)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Incorrect set of'//
     -                ' arguments for MATRIX_TO_HISTOGRAM;'//
     -                ' nothing done.'
                 RETURN
            ENDIF
*   Check the matrix.
            IMATRF=NINT(ARG(1))
            IMATSL=MATSLT(IMATRF)
            IF(IMATSL.LE.0)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Invalid matrix'//
     -                ' reference; no copied to a histogram.'
                 RETURN
            ELSEIF(MDIM(IMATSL).NE.1)THEN
                 PRINT *,' ------ HISCAL MESSAGE : Matrix is not'//
     -                ' 1-dimensional; unfolded.'
            ENDIF
*   Free memory associated with the return argument.
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
*   Book a histogram for the contents.
            CALL HISADM('ALLOCATE',IHISRF,MLEN(IMATSL),ARG(2),ARG(3),
     -           .FALSE.,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Unable to obtain'//
     -                ' histogram space ; matrix not copied.'
                 RETURN
            ENDIF
*   Copy the histogram to a matrix.
            SX0(IHISRF)=0
            SX1(IHISRF)=0
            SX2(IHISRF)=0
            DO 60 I=1,NCHA(IHISRF)
            CONTEN(IHISRF,I)=MVEC(MORG(IMATSL)+I)
            XX=XMIN(IHISRF)+REAL(I-0.5)*(XMAX(IHISRF)-XMIN(IHISRF))/
     -           REAL(NCHA(IHISRF))
            SX0(IHISRF)=SX0(IHISRF)+CONTEN(IHISRF,I)
            SX1(IHISRF)=SX1(IHISRF)+CONTEN(IHISRF,I)*XX
            SX2(IHISRF)=SX2(IHISRF)+CONTEN(IHISRF,I)*XX**2
60          CONTINUE
            NENTRY(IHISRF)=0
            ARG(4)=IHISRF
            MODARG(4)=4
*** RZ output of an histogram.
       ELSEIF(IPROC.EQ.-615)THEN
*   Check argument list.
            IF(NARG.GT.3.OR.
     -           (NARG.GE.1.AND.MODARG(1).NE.4.AND.MODARG(1).NE.1).OR.
     -           (NARG.GE.2.AND.MODARG(2).NE.1).OR.
     -           (NARG.GE.3.AND.MODARG(3).NE.1))THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Incorrect argument'//
     -                ' list for WRITE_HISTOGRAM_RZ; not written.'
                 RETURN
            ENDIF
*   Check the histogram number.
            IF(NARG.LE.0)THEN
                 IREF=0
            ELSEIF(MODARG(1).EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(1)),XTXT,NCXTXT,IFAIL1)
                 IF(NCXTXT.LE.0)NCXTXT=1
                 CALL CLTOU(XTXT(1:NCXTXT))
                 IF(XTXT(1:NCXTXT).EQ.'ALL')THEN
                      IREF=0
                 ELSE
                      PRINT *,' !!!!!! HISCAL WARNING : Invalid'//
     -                     ' histogram identifier; nothing written.'
                      RETURN
                 ENDIF
            ELSEIF(MODARG(1).EQ.4)THEN
                 IREF=NINT(ARG(1))
            ELSE
                 PRINT *,' !!!!!! HISCAL WARNING : Invalid'//
     -                ' histogram identifier; nothing written.'
                 RETURN
            ENDIF
*   Fetch the file name.
            IF(NARG.GE.2)THEN
                 CALL STRBUF('READ',NINT(ARG(2)),STRING,NC,IFAIL1)
            ELSE
                 STRING='garfield.rz'
                 NC=11
                 IFAIL1=0
            ENDIF
*   Fetch the title.
            IF(NARG.GE.3)THEN
                 CALL STRBUF('READ',NINT(ARG(3)),TITLE,NCTITL,IFAIL2)
                 IF(NCTITL.LE.0)THEN
                      TITLE=' '
                      NCTITL=1
                 ENDIF
            ELSE
                 TITLE=' '
                 NCTITL=1
                 IFAIL2=0
            ENDIF
*   Check fetches.
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Unable to fetch an'//
     -                ' argument of WRITE_HISTOGRAM_RZ; no write.'
                 RETURN
            ENDIF
*   Write all histograms.
            IF(IREF.EQ.0)THEN
                 NHIST=0
                 DO 70 I=1,MXHIST
                 IF(.NOT.HISUSE(I))GOTO 70
                 TITLE='Histogram '
                 CALL OUTFMT(REAL(I),2,TITLE(11:),NCTITL,'LEFT')
                 NCTITL=NCTITL+11
                 DO 80 J=1,NGLB
                 IF(GLBMOD(J).EQ.4.AND.NINT(GLBVAL(J)).EQ.I)THEN
                      TITLE=GLBVAR(J)
                      NCTITL=LEN(GLBVAR(J))
                 ENDIF
80               CONTINUE
                 IF(.NOT.SET(I))THEN
                      PRINT *,' !!!!!! HISCAL WARNING : '//
     -                     TITLE(1:NCTITL)//' not written to the RZ'//
     -                     ' file because the range is not set.'
                 ELSE
                      PRINT *,' ------ HISCAL MESSAGE : Writing '//
     -                     TITLE(1:NCTITL)
                      CALL HISRZO(I,STRING(1:NC),TITLE(1:NCTITL),
     -                     IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! HISCAL WARNING : Writing '//
     -                          TITLE(1:NCTITL)//' failed.'
                      ELSE
                           NHIST=NHIST+1
                      ENDIF
                 ENDIF
70               CONTINUE
                 PRINT *,' ------ HISCAL MESSAGE : ',NHIST,
     -                ' Histograms written to the RZ file.'
*   Write only 1 histogram.
            ELSE
                 IF(NARG.LT.3.AND.ARGREF(1,2).GE.1.AND.
     -                ARGREF(1,2).LE.NGLB)THEN
                      TITLE=GLBVAR(ARGREF(1,2))
                      NCTITL=LEN(GLBVAR(ARGREF(1,2)))
                 ENDIF
                 CALL HISRZO(IREF,STRING(1:NC),TITLE(1:NCTITL),IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! HISCAL WARNING : Writing in RZ'//
     -                     ' format failed.'
                      RETURN
                 ENDIF
            ENDIF
*** Cut an histogram.
       ELSEIF(IPROC.EQ.-616)THEN
*   Check argument list.
            IF(NARG.NE.4.OR.MODARG(1).NE.4.OR.
     -           MODARG(2).NE.2.OR.MODARG(3).NE.2.OR.
     -           ARGREF(4,1).GE.2)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Incorrect argument'//
     -                ' list for CUT_HISTOGRAM; no sub-range.'
                 RETURN
            ENDIF
*   Take the sub-range.
            CALL HISCUT(NINT(ARG(1)),ARG(2),ARG(3),IREF,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Cutting the'//
     -                ' histogram failed; no sub-range.'
                 RETURN
            ENDIF
*   Free memory associated with the return argument.
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
*   Return the histogram.
            ARG(4)=REAL(IREF)
            MODARG(4)=4
*** Rebin an histogram.
       ELSEIF(IPROC.EQ.-617)THEN
*   Check argument list.
            IF(NARG.NE.3.OR.MODARG(1).NE.4.OR.
     -           MODARG(2).NE.2.OR.
     -           ARGREF(3,1).GE.2)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Incorrect argument'//
     -                ' list for REBIN_HISTOGRAM; not sub-range.'
                 RETURN
            ENDIF
*   Take the sub-range.
            CALL HISREB(NINT(ARG(1)),NINT(ARG(2)),IREF,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Rebinning the'//
     -                ' histogram failed; no rebinned histogram.'
                 RETURN
            ENDIF
*   Free memory associated with the return argument.
            CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
*   Return the histogram.
            ARG(3)=REAL(IREF)
            MODARG(3)=4
*** Reset the contents of an histogram.
       ELSEIF(IPROC.EQ.-618)THEN
*   Without arguments, reset all histograms.
            IF(NARG.LT.1)THEN
                 DO 100 I=1,NGLB
                 IF(GLBMOD(I).EQ.4)CALL HISRES(NINT(GLBVAL(I)),IFAIL1)
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! HISCAL WARNING :'//
     -                ' Failed to delete histogram '//GLBVAR(I)
100              CONTINUE
            ELSE
                 DO 90 I=1,NARG
                 IF(MODARG(I).NE.4)THEN
                      PRINT *,' !!!!!! HISCAL WARNING : Argument ',I,
     -                      ' of RESET_HISTOGRAM is not an histogram;'//
     -                      ' not reset.'
                 ELSEIF(ARGREF(I,1).GE.2)THEN
                      PRINT *,' !!!!!! HISCAL WARNING : Argument ',I,
     -                      ' of RESET_HISTOGRAM is not modifiable;'//
     -                      ' not reset.'
                 ELSE
                      CALL HISRES(NINT(ARG(I)),IFAIL1)
                      IF(IFAIL1.NE.0)PRINT *,' !!!!!! HISCAL'//
     -                     ' WARNING : Failed to delete histogram '//
     -                     ' for argument ',I
                 ENDIF
90               CONTINUE
            ENDIF
*** Cumulate an histogram.
       ELSEIF(IPROC.EQ.-619)THEN
*   Check argument list.
            IF(NARG.NE.2.OR.MODARG(1).NE.4)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Incorrect argument'//
     -                ' list for CUMULATE_HISTOGRAM; not output.'
                 RETURN
            ENDIF
*   Take the sub-range.
            CALL HISCUM(NINT(ARG(1)),IREF,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! HISCAL WARNING : Unable to create'//
     -                ' a cumulative histogram; no output.'
                 RETURN
            ENDIF
*   Free memory associated with the return argument.
            CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
*   Return the histogram.
            ARG(2)=REAL(IREF)
            MODARG(2)=4
*** Skip an histogram.
       ELSEIF(IPROC.EQ.-620)THEN
*   Skip the specified number of histograms.
            IF(NARG.EQ.1.AND.MODARG(1).EQ.2)THEN
                 CALL HISSKP(NINT(ARG(1)))
*   Or by default 1
            ELSEIF(NARG.EQ.0)THEN
                 CALL HISSKP(1)
*   Or print an warning
            ELSE
                 PRINT *,' !!!!!! HISCAL WARNING : Incorrect argument'//
     -                ' list for SKIP_HISTOGRAM; not action taken.'
                 RETURN
            ENDIF
*** Unknown matrix operation.
       ELSE
            PRINT *,' !!!!!! HISCAL WARNING : Unknown procedure code'//
     -           ' received; nothing done.'
            IFAIL=1
            RETURN
       ENDIF
*** Seems to have worked.
       IFAIL=0
       END
