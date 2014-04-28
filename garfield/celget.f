CDECK  ID>, CELGET.
       SUBROUTINE CELGET(IFAIL)
*-----------------------------------------------------------------------
*   CELGET - This routine reads all cell information from an external
*            dataset. It checks that the dataset exists and that it is
*            of the correct type.
*   VARIABLES : STRING      : Character string that should contain a
*                             description of the dataset being read.
*   (Last changed on 25/ 2/11.)
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
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL,
     -      BEMSET
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL,BEMSET
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*(MXINCH) STRING
       CHARACTER*8 MEMBER
       CHARACTER*(MXNAME) FILE
       CHARACTER*1 DUMMY
       INTEGER IFAIL,IFAIL1,NCFILE,NCMEMB,NWORD,I,J,K,IOS
       LOGICAL DSNCMP,EXIS
       EXTERNAL DSNCMP
*** Identify the routine, if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE CELGET ///'
*** Initialise IFAIL on 1 (i.e. fail).
       IFAIL=1
       FILE=' '
       MEMBER='*'
       NCFILE=8
       NCMEMB=1
*** First decode the argument string, setting file name + member name.
       CALL INPNUM(NWORD)
*   If there's only one argument, it's the dataset name.
       IF(NWORD.GE.2)THEN
            CALL INPSTR(2,2,STRING,NCFILE)
            FILE=STRING
       ENDIF
*   If there's a second argument, it is the member name.
       IF(NWORD.GE.3)THEN
            CALL INPSTR(3,3,STRING,NCMEMB)
            MEMBER=STRING
       ENDIF
*   Check the various lengths.
       IF(NCFILE.GT.MXNAME)THEN
            PRINT *,' !!!!!! CELGET WARNING : The file name is'//
     -           ' truncated to MXNAME (=',MXNAME,') characters.'
            NCFILE=MIN(NCFILE,MXNAME)
       ENDIF
       IF(NCMEMB.GT.8)THEN
            PRINT *,' !!!!!! CELGET WARNING : The member name is'//
     -           ' shortened to ',MEMBER,', first 8 characters.'
            NCMEMB=MIN(NCMEMB,8)
       ELSEIF(NCMEMB.LE.0)THEN
            PRINT *,' !!!!!! CELGET WARNING : The member'//
     -           ' name has zero length, replaced by "*".'
            MEMBER='*'
            NCMEMB=1
       ENDIF
*   Reject the empty file name case.
       IF(FILE.EQ.' '.OR.NWORD.EQ.1)THEN
            PRINT *,' !!!!!! CELGET WARNING : GET must be at least'//
     -           ' followed by a dataset name ; no data are read.'
            RETURN
       ENDIF
*   If there are even more args, warn because they are ignored.
       IF(NWORD.GT.3)PRINT *,' !!!!!! CELGET WARNING : GET takes'//
     -     ' at most two arguments (dataset and member); rest ignored.'
*** Open the dataset and inform DSNLOG.
       CALL DSNOPN(FILE(1:NCFILE),NCFILE,12,'READ-LIBRARY',IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! CELGET WARNING : Opening ',FILE(1:NCFILE),
     -           ' failed ; cell data are not read.'
            RETURN
       ENDIF
       CALL DSNLOG(FILE,'Cell data ','Sequential','Read only ')
       IF(LDEBUG)PRINT *,' ++++++ CELGET DEBUG   : Dataset ',
     -      FILE(1:NCFILE),' opened on unit 12 for seq read.'
*   Locate the pointer on the header of the requested member.
       CALL DSNLOC(MEMBER,NCMEMB,'CELL    ',12,EXIS,'RESPECT')
       IF(.NOT.EXIS)THEN
            CALL DSNLOC(MEMBER,NCMEMB,'CELL    ',12,EXIS,'IGNORE')
            IF(EXIS)THEN
                 PRINT *,' ###### CELGET ERROR   : Cell description ',
     -                MEMBER(1:NCMEMB),' has been deleted from ',
     -                FILE(1:NCFILE),'; not read.'
            ELSE
                 PRINT *,' ###### CELGET ERROR   : Cell description ',
     -                MEMBER(1:NCMEMB),' not found on ',FILE(1:NCFILE)
            ENDIF
            CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
*** Check that the member is acceptable.
       READ(12,'(A80)',END=2000,IOSTAT=IOS,ERR=2010) STRING
       IF(LDEBUG)THEN
            PRINT *,' ++++++ CELGET DEBUG   : Dataset header',
     -              ' record follows:'
            PRINT *,STRING
       ENDIF
*   Print member information.
       WRITE(LUNOUT,'(''  Member '',A8,'' was created on '',A8,
     -      '' at '',A8/''  Remarks: '',A29)')
     -      STRING(32:39),STRING(11:18),STRING(23:30),STRING(51:79)
*   Check the version.
       READ(12,'(A14)',END=2000,IOSTAT=IOS,ERR=2010) STRING
       IF(STRING(1:14).NE.' Version   : 3')THEN
            PRINT *,' !!!!!! CELGET WARNING : This member can not'//
     -           ' be read because of a change in format.'
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
*** Execute read operations if a valid name is available.
       READ(12,'(9X,A)',END=2000,IOSTAT=IOS,ERR=2010) CELLID
       READ(12,'(9X,I10,7X,A3,I2,8X,L1,7X,L1)',
     -      END=2000,IOSTAT=IOS,ERR=2010)
     -      NWIRE,TYPE,ICTYPE,POLAR,TUBE
*   Cell-data cannot be used if MXWIRE < NWIRE.
       IF(NWIRE.GT.MXWIRE)THEN
            PRINT *,' ###### CELGET ERROR   : Program not suitably',
     -              ' compiled to use member ',MEMBER(1:NCMEMB),' on ',
     -              FILE(1:NCFILE),' ; increase MXWIRE to ',NWIRE
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
       READ(12,'(7X,6E15.8,/,10X,2E15.8)',END=2000,IOSTAT=IOS,ERR=2010)
     -      XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,VMIN,VMAX
       READ(12,'(A1)',END=2000,IOSTAT=IOS,ERR=2010) DUMMY
       DO 210 I=1,NWIRE
       READ(12,'(1X,A1,6E15.8/2X,5E15.8)',END=2000,IOSTAT=IOS,ERR=2010)
     -      WIRTYP(I),X(I),Y(I),V(I),E(I),D(I),W(I),U(I),DENS(I),
     -      B2SIN(I),WMAP(I)
210    CONTINUE
       READ(12,'(10X,3E15.8)',END=2000,IOSTAT=IOS,ERR=2010)
     -      (DOWN(I),I=1,3)
       READ(12,'(8X,3E15.8,5X,E15.8)',END=2000,IOSTAT=IOS,ERR=2010)
     -      CORVTA,CORVTB,CORVTC,V0
       READ(12,'(11X,2(L1,2E15.8,A1))',END=2000,IOSTAT=IOS,ERR=2010)
     -      (YNPLAN(I),COPLAN(I),VTPLAN(I),PLATYP(I),I=1,2)
       READ(12,'(11X,2(L1,2E15.8,A1))',END=2000,IOSTAT=IOS,ERR=2010)
     -      (YNPLAN(I),COPLAN(I),VTPLAN(I),PLATYP(I),I=3,4)
       READ(12,'(21X,2L1,2E15.8)',END=2000,IOSTAT=IOS,ERR=2010)
     -      YNPLAX,YNPLAY,COPLAX,COPLAY
       READ(12,'(9X,5I10/9X,5I10)',END=2000,IOSTAT=IOS,ERR=2010)
     -       (NPSTR1(I),NPSTR2(I),I=1,5)
       DO 240 I=1,5
       DO 250 J=1,NPSTR1(I)
       READ(12,'(1X,A1,1X,3E15.8)',END=2000,IOSTAT=IOS,ERR=2010)
     -      PSLAB1(I,J),(PLSTR1(I,J,K),K=1,3)
250    CONTINUE
       DO 260 J=1,NPSTR2(I)
       READ(12,'(1X,A1,1X,3E15.8)',END=2000,IOSTAT=IOS,ERR=2010)
     -      PSLAB2(I,J),(PLSTR2(I,J,K),K=1,3)
260    CONTINUE
240    CONTINUE
       READ(12,'(15X,2(L1,E15.8))',END=2000,IOSTAT=IOS,ERR=2010)
     -      PERX,SX,PERY,SY
       IF(TYPE(1:1).EQ.'C')READ(12,'(14X,5E15.8,I10)',END=2000,
     -      IOSTAT=IOS,ERR=2010) ZMULT,P1,P2,C1,MODE
       IF(TYPE.EQ.'D3 '.OR.TYPE.EQ.'D4 ')
     -      READ(12,'(13X,E15.8)',END=2000,IOSTAT=IOS,ERR=2010) KAPPA
       READ(12,'(17X,I3,5X,I3)',END=2000,IOSTAT=IOS,ERR=2010)
     -      NXMATT,NYMATT
       IF(NXMATT.GT.MXMATT.OR.NYMATT.GT.MXMATT)THEN
            PRINT *,' ###### CELGET ERROR   : Program not suitably',
     -              ' compiled to use member ',MEMBER(1:NCMEMB),' on ',
     -              FILE(1:NCFILE),' ; increase MXMATT to ',
     -              MAX(NXMATT,NYMATT)
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
       DO 220 I=1,NXMATT
       READ(12,'(1X,5E15.8)',END=2000,IOSTAT=IOS,ERR=2010)
     -      (XMATT(I,J),J=1,5)
220    CONTINUE
       DO 230 I=1,NYMATT
       READ(12,'(1X,5E15.8)',END=2000,IOSTAT=IOS,ERR=2010)
     -      (YMATT(I,J),J=1,5)
230    CONTINUE
       IF(TUBE)READ(12,'(7X,2E15.8,2I10,A1)',END=2000,IOSTAT=IOS,
     -      ERR=2010) COTUBE,VTTUBE,NTUBE,MTUBE,PLATYP(5)
       READ(12,'(9X,2I10,17X,L1)',END=2000,IOSTAT=IOS,ERR=2010)
     -      NSOLID,ICCURR,BEMSET
       IF(NSOLID.GT.0)
     -      READ(12,'(1X,3I10,1X,A1)',END=2000,IOSTAT=IOS,ERR=2010)
     -      (ISTART(I),ISOLTP(I),ISOLMT(I),SOLTYP(I),I=1,NSOLID)
       IF(ICCURR.GT.0)READ(12,'(1X,4E25.18)',END=2000,IOSTAT=IOS,
     -      ERR=2010) (CBUF(I),I=1,ICCURR)
*   Close the file after the operation.
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       IFAIL=0
*** Register the amount of CPU time used for reading.
       CALL TIMLOG('Reading the cell data from a dataset:   ')
       RETURN
*** Handle the I/O error conditions.
2000   CONTINUE
       PRINT *,' ###### CELGET ERROR   : EOF encountered while reading',
     -         ' ',FILE(1:NCFILE),' from unit 12 ; no cell data read.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2010   CONTINUE
       PRINT *,' ###### CELGET ERROR   : Error while reading',
     -         ' ',FILE(1:NCFILE),' from unit 12 ; no cell data read.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2030   CONTINUE
       PRINT *,' ###### CELGET ERROR   : Dataset ',FILE(1:NCFILE),' on',
     -         ' unit 12 cannot be closed ; results not predictable.'
       CALL INPIOS(IOS)
       END
