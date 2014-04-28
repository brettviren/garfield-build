CDECK  ID>, CRNERR.
       SUBROUTINE CRNERR
*-----------------------------------------------------------------------
*   CRNERR - Error handling
*-----------------------------------------------------------------------
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       EXTERNAL INPCMP
       CHARACTER*6 ER
       INTEGER LM,LR
*** Default value.
       ER='??????'
       LM=100
       LR=100
       IER=0
       ILM=0
       ILR=0
*** Decode the argument string
       CALL INPNUM(NWORD)
       INEXT=2
       DO 10 I=2,NWORD
*   Skip arguments etc.
       IF(I.LT.INEXT)GOTO 10
*   Message string.
       IF(INPCMP(I,'M#ESSAGE').NE.0)THEN
            CALL INPSTR(I+1,I+1,ER,NCH)
            IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ ',ER(1:1)).EQ.0.OR.
     -           INDEX('0123456789 ',              ER(2:2)).EQ.0.OR.
     -           INDEX('0123456789 ',              ER(3:3)).EQ.0.OR.
     -           INDEX('0123456789 ',              ER(4:4)).EQ.0.OR.
     -           INDEX('. ',                       ER(5:5)).EQ.0.OR.
     -           INDEX('0123456789 ',              ER(6:6)).EQ.0)THEN
                 CALL INPMSG(I+1,'Not correctly formatted.      ')
                 ER='??????'
                 IER=0
            ELSE
                 IER=1
            ENDIF
            INEXT=I+2
*   Number of times to print.
       ELSEIF(INPCMP(I,'PR#INT').NE.0)THEN
            IF(INPCMP(I+1,'A#LWAYS').NE.0)THEN
                 LM=100
            ELSEIF(INPCMP(I+1,'N#EVER').NE.0)THEN
                 LM=0
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,LM,100)
            ENDIF
            INEXT=I+2
            ILM=1
*   Number of occurences before ABEND.
       ELSEIF(INPCMP(I,'AB#END').NE.0)THEN
            IF(INPCMP(I+1,'N#EVER').NE.0)THEN
                 LR=100
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,LR,100)
            ENDIF
            INEXT=I+2
            ILR=1
*   Anything not valid.
       ELSE
            CALL INPMSG(I,'Keyword not recognised.       ')
       ENDIF
10     CONTINUE
*** Dump error messages.
       CALL INPERR
*** Check at least the message id was specified.
       IF(IER.EQ.0)THEN
            PRINT *,' !!!!!! CRNERR WARNING : Error message id not'//
     -           ' specified ; no call to KERSET.'
            RETURN
       ENDIF
*** Register request with KERSET.
       CALL KERSET(ER,0,LM,LR)
       IF(LDEBUG)PRINT *,' ++++++ CRNERR DEBUG   : KERSET called for'//
     -      ' message '//ER//': printing ',LM,' times, ABEND after ',
     -      LR,' occurences.'
       END
