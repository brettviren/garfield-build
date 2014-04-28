CDECK  ID>, DSNLOC.
       SUBROUTINE DSNLOC(MEMBER,NC,TYPE,LUN,EXIS,OPER)
*-----------------------------------------------------------------------
*   DSNLOC - Places the pointer in a Garfield file on the header record
*            of the requested member.
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
       CHARACTER*80  STRING
       CHARACTER     CHAR
       CHARACTER*8   MEMBER,TYPE
       CHARACTER*(*) OPER
       LOGICAL       EXIS,OPEN,MATCH
*** Print some debugging information.
       IF(LIDENT)PRINT *,' /// ROUTINE DSNLOC ///'
       IF(LDEBUG)PRINT *,' ++++++ DSNLOC DEBUG   : Request to locate ',
     -      MEMBER(1:NC),' on unit ',LUN,' in mode ',OPER,'.'
*** First set EXIS to .FALSE. ie not yet found.
       EXIS=.FALSE.
*** Check that unit LUN is indeed open.
       INQUIRE(UNIT=LUN,OPENED=OPEN)
       IF(.NOT.OPEN)THEN
            PRINT *,' ###### DSNLOC ERROR   : Unit ',LUN,' should be'//
     -           ' open but is not; program bug, member not located.'
            RETURN
       ENDIF
*** Rewind the file.
       REWIND(UNIT=LUN,ERR=2050,IOSTAT=IOS)
*** Loop until EOF or until the member has been located.
10     CONTINUE
       READ(LUN,'(A1)',END=20,IOSTAT=IOS,ERR=2010) CHAR
       IF(CHAR.EQ.'%')THEN
            BACKSPACE(UNIT=LUN,IOSTAT=IOS,ERR=2040)
            READ(LUN,'(A80)',END=20,IOSTAT=IOS,ERR=2010) STRING
            IF(LDEBUG)PRINT *,' ++++++ DSNLOC DEBUG   : Found member '//
     -           STRING(32:39)//', type '//STRING(41:48)//
     -           ', delete flag "'//STRING(2:2)//'".'
*   Skip members of the wrong type and deleted members unless IGNORE.
            IF((OPER.NE.'IGNORE'.AND.STRING(2:2).EQ.'X').OR.
     -           STRING(41:48).NE.TYPE)GOTO 10
*   Wildcard check for the actual member name.
            CALL WLDCRD(STRING(32:39),MEMBER(1:NC),.FALSE.,MATCH)
*   Member found, make sure the next read sees the header and return.
            IF(MATCH)THEN
                 EXIS=.TRUE.
                 BACKSPACE(UNIT=LUN,IOSTAT=IOS,ERR=2040)
                 RETURN
            ENDIF
       ENDIF
*   Next line.
       GOTO 10
*** EOF seen on the dataset, member apparently not found.
20     CONTINUE
       REWIND(UNIT=LUN,IOSTAT=IOS,ERR=2050)
       RETURN
*** Handle error conditions.
2010   CONTINUE
       PRINT *,' ###### DSNLOC ERROR   : I/O error reading a dataset'//
     -      ' for dataset manipulation via LUN ',LUN,'; no action.'
       CALL INPIOS(IOS)
       RETURN
2040   CONTINUE
       PRINT *,' ###### DSNLOC ERROR   : Error during backspace on'//
     -      ' a dataset connected to LUN ',LUN,' ; no action.'
       CALL INPIOS(IOS)
       RETURN
2050   CONTINUE
       PRINT *,' ###### DSNLOC ERROR   : Error during rewind on'//
     -      ' a dataset connected to LUN ',LUN,' ; no action.'
       CALL INPIOS(IOS)
       END
