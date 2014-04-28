CDECK  ID>, CELWRT.
       SUBROUTINE CELWRT(IMODE)
*-----------------------------------------------------------------------
*   CELWRT - This routine writes all cell information on a dataset.
*   VARIABLES : IMODE       : If 1 : find name, if 2 write cell.
*               IACC        : If 0 no name specified, no write.
*                             If 1 name OK, write will be executed.
*                             If 2 name rejected no write.
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
       CHARACTER*(MXNAME) FILE
       CHARACTER*29 REMARK
       CHARACTER*8 TIME,DATE,MEMBER
       INTEGER IMODE,IACC,NCFILE,NCMEMB,NCREM,I,J,K,IFAIL,INEXT,IOS,
     -      INPCMP,NWORD
       LOGICAL EXMEMB
       EXTERNAL INPCMP
       SAVE IACC,FILE,NCFILE,MEMBER,NCMEMB,REMARK,NCREM
       DATA IACC/0/
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE CELWRT ///'
*** Goto 200 if a write is requested.
       IF(IMODE.EQ.2)GOTO 200
*   Set the file name etc.
       IACC=0
       FILE=' '
       NCFILE=1
       MEMBER='< none >'
       NCMEMB=8
       REMARK='none'
       NCREM=4
*   First decode the argument string.
       CALL INPNUM(NWORD)
*   Make sure there is at least one argument.
       IF(NWORD.EQ.1)THEN
            PRINT *,' !!!!!! CELWRT WARNING : WRITE takes at least one',
     -           ' argument (a dataset name); data will not be written.'
            RETURN
*   Check whether keywords have been used.
       ELSEIF(INPCMP(2,'D#ATASET')+INPCMP(2,'R#EMARK').NE.0)THEN
            INEXT=2
            DO 10 I=2,NWORD
            IF(I.LT.INEXT)GOTO 10
            IF(INPCMP(I,'D#ATASET').NE.0)THEN
                 IF(INPCMP(I+1,'R#EMARK').NE.0.OR.I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'The dataset name is missing.  ')
                      INEXT=I+1
                 ELSE
                      CALL INPSTR(I+1,I+1,STRING,NCFILE)
                      FILE=STRING
                      INEXT=I+2
                      IF(INPCMP(I+2,'R#EMARK').EQ.0.AND.
     -                     I+2.LE.NWORD)THEN
                           CALL INPSTR(I+2,I+2,STRING,NCMEMB)
                           MEMBER=STRING
                           INEXT=I+3
                      ENDIF
                 ENDIF
            ELSEIF(INPCMP(I,'R#EMARK').NE.0)THEN
                 IF(INPCMP(I+1,'D#ATASET').NE.0.OR.I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'The remark is missing.        ')
                      INEXT=I+1
                 ELSE
                      CALL INPSTR(I+1,I+1,STRING,NCREM)
                      REMARK=STRING
                      INEXT=I+2
                 ENDIF
            ELSE
                 CALL INPMSG(I,'The parameter is not known.   ')
            ENDIF
10          CONTINUE
*   Otherwise the string is interpreted as a file name (+ member name).
       ELSE
            CALL INPSTR(2,2,STRING,NCFILE)
            FILE=STRING
            IF(NWORD.GE.3)THEN
                 CALL INPSTR(3,3,STRING,NCMEMB)
                 MEMBER=STRING
            ENDIF
            IF(NWORD.GE.4)THEN
                 CALL INPSTR(4,NWORD,STRING,NCREM)
                 REMARK=STRING
            ENDIF
       ENDIF
*   Print error messages.
       CALL INPERR
       IF(NCFILE.GT.MXNAME)PRINT *,' !!!!!! CELWRT WARNING : The file',
     -      ' name is truncated to MXNAME (=',MXNAME,') characters.'
       IF(NCMEMB.GT.8)PRINT *,' !!!!!! CELWRT WARNING : The member',
     -      ' name is shortened to ',MEMBER,', first 8 characters.'
       IF(NCREM.GT.29)PRINT *,' !!!!!! CELWRT WARNING : The remark',
     -      ' shortened to ',REMARK,', first 29 characters.'
       NCFILE=MIN(NCFILE,MXNAME)
       NCMEMB=MIN(NCMEMB,8)
       NCREM=MIN(NCREM,29)
*   Check whether the member already exists.
       CALL DSNREM(FILE(1:NCFILE),MEMBER(1:NCMEMB),'CELL',EXMEMB)
       IF(JEXMEM.EQ.2.AND.EXMEMB)THEN
            PRINT *,' ------ CELWRT MESSAGE : A copy of the member'//
     -           ' exists; new member will be appended.'
       ELSEIF(JEXMEM.EQ.3.AND.EXMEMB)THEN
            PRINT *,' !!!!!! CELWRT WARNING : A copy of the member'//
     -           ' exists already; member will not be written.'
            RETURN
       ENDIF
*   Everything seems to be OK, the accept flag can be set to 'accept'.
       IACC=1
*   Print some debugging output if requested.
       IF(LDEBUG)THEN
            PRINT *,' ++++++ CELWRT DEBUG   : File= '//FILE(1:NCFILE)//
     -           ', member= '//MEMBER(1:NCMEMB),' IACC=',IACC
            PRINT *,'                         Remark= ',REMARK(1:NCREM)
       ENDIF
       RETURN
*** Execute write operation if a valid name is available.
200    CONTINUE
       IF(IACC.EQ.0)RETURN
       IACC=0
*** Open the dataset for sequential write and inform DSNLOG.
       CALL DSNOPN(FILE,NCFILE,12,'WRITE-LIBRARY',IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! CELWRT WARNING : Opening '//FILE(1:NCFILE),
     -           ' failed ; the cell data will not be written.'
            RETURN
       ENDIF
       CALL DSNLOG(FILE,'Cell data ','Sequential','Write     ')
       IF(LDEBUG)PRINT *,' ++++++ CELWRT DEBUG   : Dataset '//
     -      FILE(1:NCFILE)//' opened on unit 12 for seq write.'
*   Now write a heading record to the file.
       CALL DATTIM(DATE,TIME)
       WRITE(STRING,'(''% Created '',A8,'' At '',A8,1X,A8,'' CELL    '',
     -      1X,''"'',A29,''"'')') DATE,TIME,MEMBER,REMARK
       WRITE(12,'(A80)',IOSTAT=IOS,ERR=2010) STRING
       IF(LDEBUG)THEN
            PRINT *,' ++++++ CELWRT DEBUG   : Dataset heading record:'
            PRINT *,STRING
       ENDIF
*   Write a version number.
       WRITE(12,'('' Version   : 3'')')
*   Write the cell on the dataset.
       WRITE(12,'('' CELLID: '',A)',IOSTAT=IOS,ERR=2010) CELLID
       WRITE(12,'('' Wires:  '',I10,'' Type: '',A3,I2,
     -      '' Polar: '',L1,'' Tube: '',L1)',IOSTAT=IOS,ERR=2010)
     -      NWIRE,TYPE,ICTYPE,POLAR,TUBE
       WRITE(12,'('' Area: '',6E15.8,/,'' V-RANGE: '',2E15.8)',
     -      IOSTAT=IOS,ERR=2010) XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,VMIN,VMAX
       WRITE(12,'('' Wire table follows: '')',IOSTAT=IOS,ERR=2010)
       DO 210 I=1,NWIRE
       WRITE(12,'(1X,A1,6E15.8/2X,5E15.8)',IOSTAT=IOS,ERR=2010)
     -      WIRTYP(I),X(I),Y(I),V(I),E(I),D(I),W(I),U(I),DENS(I),
     -      B2SIN(I),WMAP(I)
210    CONTINUE
       WRITE(12,'('' Gravity: '',3E15.8)',IOSTAT=IOS,
     -      ERR=2010) (DOWN(I),I=1,3)
       WRITE(12,'('' CORVT: '',3E15.8,'' V0: '',E15.8)',IOSTAT=IOS,
     -      ERR=2010) CORVTA,CORVTB,CORVTC,V0
       WRITE(12,'('' x-Planes: '',2(L1,2E15.8,A1))',IOSTAT=IOS,ERR=2010)
     -      (YNPLAN(I),COPLAN(I),VTPLAN(I),PLATYP(I),I=1,2)
       WRITE(12,'('' y-Planes: '',2(L1,2E15.8,A1))',IOSTAT=IOS,ERR=2010)
     -      (YNPLAN(I),COPLAN(I),VTPLAN(I),PLATYP(I),I=3,4)
       WRITE(12,'('' Plane summary data: '',2L1,2E15.8)',IOSTAT=IOS,
     -      ERR=2010) YNPLAX,YNPLAY,COPLAX,COPLAY
       WRITE(12,'('' Strips: '',5I10/9X,5I10)',IOSTAT=IOS,ERR=2010)
     -      (NPSTR1(I),NPSTR2(I),I=1,5)
       DO 240 I=1,5
       DO 250 J=1,NPSTR1(I)
       WRITE(12,'(1X,A1,1X,3E15.8)',IOSTAT=IOS,ERR=2010)
     -      PSLAB1(I,J),(PLSTR1(I,J,K),K=1,3)
250    CONTINUE
       DO 260 J=1,NPSTR2(I)
       WRITE(12,'(1X,A1,1X,3E15.8)',IOSTAT=IOS,ERR=2010)
     -      PSLAB2(I,J),(PLSTR2(I,J,K),K=1,3)
260    CONTINUE
240    CONTINUE
       WRITE(12,'('' Periodicity : '',2(L1,E15.8))',IOSTAT=IOS,
     -      ERR=2010) PERX,SX,PERY,SY
       IF(TYPE(1:1).EQ.'C')WRITE(12,'('' C cell data: '',5E15.8,I10)',
     -      IOSTAT=IOS,ERR=2010) ZMULT,P1,P2,C1,MODE
       IF(TYPE.EQ.'D3 '.OR.TYPE.EQ.'D4 ')
     -      WRITE(12,'('' D3-D4 data: '',E15.8)',
     -      IOSTAT=IOS,ERR=2010) KAPPA
       WRITE(12,'('' Dielectrica: nx='',I3,'', ny='',I3)',IOSTAT=IOS,
     -      ERR=2010) NXMATT,NYMATT
       DO 220 I=1,NXMATT
       WRITE(12,'(1X,5E15.8)',IOSTAT=IOS,ERR=2010) (XMATT(I,J),J=1,5)
220    CONTINUE
       DO 230 I=1,NYMATT
       WRITE(12,'(1X,5E15.8)',IOSTAT=IOS,ERR=2010) (YMATT(I,J),J=1,5)
230    CONTINUE
       IF(TUBE)WRITE(12,'('' Tube: '',2E15.8,2I10,A1)',IOSTAT=IOS,
     -      ERR=2010) COTUBE,VTTUBE,NTUBE,MTUBE,PLATYP(5)
       WRITE(12,'('' Solids: '',2I10,'' neBEM eligible: '',L1)',
     -      IOSTAT=IOS,ERR=2010) NSOLID,ICCURR,BEMSET
       IF(NSOLID.GT.0)WRITE(12,'(1X,3I10,1X,A1)',IOSTAT=IOS,ERR=2010)
     -      (ISTART(I),ISOLTP(I),ISOLMT(I),SOLTYP(I),I=1,NSOLID)
       IF(ICCURR.GT.0)WRITE(12,'(1X,4E25.18)',IOSTAT=IOS,ERR=2010)
     -      (CBUF(I),I=1,ICCURR)
*   Close the file after the operation.
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       CALL TIMLOG('Writing the cell data to a dataset:     ')
       RETURN
*** Handle the error conditions.
2010   CONTINUE
       PRINT *,' ###### CELWRT ERROR   : Error while writing'//
     -      ' to ',FILE(1:NCFILE),' via unit 12 ; no cell data written.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2030   CONTINUE
       PRINT *,' ###### CELWRT ERROR   : Dataset '//FILE(1:NCFILE)//
     -      ' unit 12 cannot be closed ; results not predictable'
       CALL INPIOS(IOS)
       END
