CDECK  ID>, PROINT.
       SUBROUTINE PROINT(NAME,NFIELD,LUN)
*-----------------------------------------------------------------------
*   PROINT - Initialises progress printing.
*   PROFLD - Sets field names
*   PRORED - Changes the number of fields.
*   PROSTA - Prints current status.
*   PROEND - Ends progress printing.
*   (Last changed on  7/ 1/09).
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
       INTEGER MXFLD
       PARAMETER(MXFLD=10)
       CHARACTER*130 OUT
       CHARACTER*(*) NAME
       CHARACTER*20 FLD(MXFLD),FAC
       INTEGER NFIELD,NCFLD(MXFLD),MFLD,NFLD,NCFAC,NCOUT,IXFLD(MXFLD),
     -      LUN,LUNPRO,I,IFLD
       REAL X,XRNG(MXFLD),RANGE
       LOGICAL CHANGE
       SAVE FLD,NCFLD,MFLD,NFLD,IXFLD,FAC,NCFAC,LUNPRO,XRNG
       DATA NFLD/0/,MFLD/0/,LUNPRO/6/,NCFAC/7/
       DATA FAC/'Unknown             '/
*** Check setting of nfield.
       IF(NFIELD.LT.1.OR.NFIELD.GT.MXFLD)THEN
            PRINT *,' !!!!!! PROINT WARNING : Received an incorrect'//
     -           ' number of fields ; program bug - please report.'
            RETURN
       ENDIF
*   Keep the routine name.
       FAC=NAME
       NCFAC=MIN(LEN(NAME),LEN(FAC))
*   Initialise the field names.
       DO 10 I=1,NFIELD
       FLD(I)=' '
       NCFLD(I)=0
       IXFLD(I)=0
10     CONTINUE
       NFLD=NFIELD
       MFLD=0
*   Keep the logical unit number.
       LUNPRO=LUN
*   Write out a blank line or a synchronisation record.
       IF(LPROPR)THEN
            IF(LSYNCH)THEN
                 WRITE(6,'(''  >>>>>> progress init '',I5,'' '',A)')
     -               NFLD,FAC(1:NCFAC)
            ELSE
                 WRITE(LUNPRO,'(''  '')')
            ENDIF
       ENDIF
*   That's it for this entry.
       RETURN
*** Update a field.
       ENTRY PROFLD(IFLD,NAME,RANGE)
*   Check validity of the field index.
       IF(IFLD.LT.1.OR.IFLD.GT.NFLD.OR.NFLD.LT.1)THEN
            PRINT *,' !!!!!! PROFLD WARNING : Received an incorrect'//
     -           ' field index; program bug - please report.'
            RETURN
       ENDIF
*   Update the latest received field.
       MFLD=MAX(MFLD,IFLD)
*   Otherwise store this field name.
       FLD(IFLD)=NAME
       NCFLD(IFLD)=MIN(LEN(NAME),LEN(FLD(IFLD)))
       XRNG(IFLD)=RANGE
*   Reset the progress counter for this field to 0.
       IXFLD(IFLD)=0
*   Synchronisation records.
       IF(LSYNCH)WRITE(6,'(''  >>>>>> progress field '',I5,'' '',
     -      E15.8,'' '',A)') IFLD,XRNG(IFLD),FLD(IFLD)(1:NCFLD(IFLD))
*   That's it for this entry.
       RETURN
*** Reduce or increase the number of fields.
       ENTRY PRORED(NFIELD)
*   Check validity of the field index.
       IF(NFIELD.LE.0)THEN
            PRINT *,' !!!!!! PRORED WARNING : Received an incorrect'//
     -           ' new number of fields; program bug - please report.'
       ELSE
            IF(LSYNCH)WRITE(6,'(''  >>>>>> progress count '',I5,
     -           '' '',I4)') NFIELD,NFLD
            DO 15 I=NFLD+1,NFIELD
            FLD(I)=' '
            NCFLD(I)=0
            IXFLD(I)=0
15          CONTINUE
            NFLD=NFIELD
            MFLD=MIN(MFLD,NFIELD)
       ENDIF
*   All for this entry.
       RETURN
*** Print current status.
       ENTRY PROSTA(IFLD,X)
*   Assume no change.
       CHANGE=.FALSE.
*   Check validity of the field index.
       IF(IFLD.LT.1.OR.IFLD.GT.NFLD.OR.NFLD.LT.1)THEN
            PRINT *,' !!!!!! PROSTA WARNING : Received an incorrect'//
     -           ' field index; program bug - please report.'
            RETURN
       ENDIF
*   Update the counter for the field and check for changes.
       IF(XRNG(IFLD).GT.0)THEN
            IF(IXFLD(IFLD).NE.
     -           MAX(0,MIN(10,INT(10*X/XRNG(IFLD)+0.0001))))
     -           CHANGE=.TRUE.
            IXFLD(IFLD)=MAX(0,MIN(10,INT(10*X/XRNG(IFLD)+0.0001)))
            IF(LSYNCH)WRITE(6,'(''  >>>>>> progress set '',I5,
     -           '' '',E15.8)') IFLD,X/XRNG(IFLD)
       ELSE
            CHANGE=.TRUE.
            IXFLD(IFLD)=-1
            IF(LSYNCH)WRITE(6,'(''  >>>>>> progress set '',I5,
     -           '' working'')') IFLD
       ENDIF
*   Reset all lower counters.
       DO 20 I=IFLD+1,NFLD
       IF(XRNG(I).GT.0)THEN
            IXFLD(I)=0
            IF(LSYNCH)WRITE(6,'(''  >>>>>> progress set '',I5,
     -           '' 0'')') IFLD
       ELSE
            IXFLD(I)=-1
            IF(LSYNCH)WRITE(6,'(''  >>>>>> progress set '',I5,
     -           '' working'')') IFLD
       ENDIF
20     CONTINUE
*   In case of synchronisation output, this is all.
       IF(LSYNCH)RETURN
*   Also return if there is no change.
       IF(.NOT.CHANGE)RETURN
*   Print the current status.
       OUT=FAC(1:NCFAC)//': '
       NCOUT=NCFAC+2
       DO 30 I=1,MFLD
       IF(NCOUT+14.GT.LEN(OUT))THEN
            IF(NCOUT+2.LE.LEN(OUT))THEN
                 OUT(NCOUT-1:NCOUT+2)=' ...'
                 NCOUT=NCOUT+3
            ENDIF
            GOTO 40
       ENDIF
       IF(NCFLD(I).GT.0)THEN
            OUT(NCOUT+1:NCOUT+NCFLD(I))=FLD(I)(1:NCFLD(I))//' '
            NCOUT=NCOUT+NCFLD(I)+1
       ENDIF
       IF(IXFLD(I).EQ.-1)THEN
            IF(NCOUT.GT.1)NCOUT=NCOUT-1
            OUT(NCOUT+1:NCOUT+13)=', '
            NCOUT=NCOUT+2
       ELSEIF(IXFLD(I).EQ.0)THEN
            OUT(NCOUT+1:NCOUT+14)='[ Starting ], '
            NCOUT=NCOUT+14
       ELSEIF(IXFLD(I).EQ.20)THEN
            OUT(NCOUT+1:NCOUT+14)='[ Finished ], '
            NCOUT=NCOUT+14
       ELSE
            OUT(NCOUT+1:NCOUT+14)='[..........], '
            IF(IXFLD(I).GE.2)
     -           OUT(NCOUT+2:NCOUT+IXFLD(I))='--------------------'
            OUT(NCOUT+IXFLD(I)+1:NCOUT+IXFLD(I)+1)='>'
            NCOUT=NCOUT+14
       ENDIF
30     CONTINUE
       IF(NCOUT.GT.2)THEN
            OUT(NCOUT-1:NCOUT)='. '
            NCOUT=NCOUT-1
       ENDIF
40     CONTINUE
       IF(LPROPR)WRITE(LUNPRO,'(A1,''  '',A,$)')
     -      CHAR(13),OUT(1:MAX(78,NCOUT))
*   That's all for this entry.
       RETURN
*** Say that we're done.
       ENTRY PROEND
       IF(LSYNCH)THEN
            WRITE(6,'(''  >>>>>> progress end'')')
       ELSE
            OUT=' '
            OUT(1:NCFAC)=FAC(1:NCFAC)
            OUT(NCFAC+1:NCFAC+12)=': Completed.'
            IF(LPROPR)WRITE(LUNPRO,'(A1,''  '',A)')
     -           CHAR(13),OUT(1:MAX(78,NCFAC+12))
       ENDIF
*** Reset the fields flag.
       NFLD=0
       END
