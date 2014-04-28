CDECK  ID>, GRCOLR.
       SUBROUTINE GRCOLR(IKEY,IFAIL)
*-----------------------------------------------------------------------
*   GRCOLR - Reads colour descriptions and stores them.
*   GRCOLQ - Returns the index for a given colour name.
*   GRCOLD - Returns the name for a colour with a given index.
*   GRCOLW - Writes a colour table to a library.
*   GRCOLG - Retrieves a colour table from a library.
*   GRCOLM - Plots a colour map.
*   GRCOLS - Resets the colour table.
*   (Last changed on 14/11/09.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER MXCOL
       PARAMETER (MXCOL=25)
       CHARACTER*(*) COLCMP,OPTION
       CHARACTER*(MXINCH) STRING
       CHARACTER*(MXNAME) FILE
       CHARACTER*80 DESCR,AUX
       CHARACTER*29 REMARK
       CHARACTER*20 COLNAM(0:MXCOL),AUX1,AUX2,AUX3
       CHARACTER*8 TIME,DATE,MEMBER
       LOGICAL EXIS,DSNCMP,EXMEMB
       INTEGER INPTYP,INPCMP,INPCMX,NC,NC1,NC2,NC3,ICOL,NCOL,IKEY,
     -      IOPSTA,NWORD,IWK,IERR,IDUM,IWKID,ITYPE,IWKTYP,
     -      IERR0,IERR1,IERR2,MPL,MPM,MTX,MFA,MPA,MXCOLI,
     -      IWKCAT,INEXT,IFAIL,IFAIL1,IFLAG,IC,NCC,IWKDUM,ICIND,NCD,
     -      NCFILE,NCMEMB,NCREM,I,II,IOS,ICONID,IWKDES,NACT,IRGB
       REAL XPL(5),YPL(5),BLUE,GREEN,RED,BLUES,GREENS,REDS,BLUER,GREENR,
     -      REDR
       EXTERNAL INPTYP,INPCMP,INPCMX
       SAVE COLNAM,NCOL
       DATA NCOL /1/
       DATA (COLNAM(I),I=0,1) /
     -      'BACKGROUND          ',
     -      'FOREGROUND          '/
*** Assume the command fails.
       IFAIL=1
*** Pick up the name of the colour.
       CALL INPNUM(NWORD)
       ICOL=-1
       IF(IKEY+1.LE.NWORD)THEN
            CALL INPSTR(IKEY+1,IKEY+1,STRING,NC)
            IF(NC.GT.20)THEN
                 PRINT *,' !!!!!! GRCOLR WARNING : The name of the'//
     -                ' colour is longer than 20 chars ; truncated.'
                 NC=20
            ENDIF
            DO 10 I=0,NCOL
            IF(STRING(1:NC).EQ.COLNAM(I))THEN
                 ICOL=I
                 GOTO 20
            ENDIF
10          CONTINUE
            ICOL=NCOL+1
20          CONTINUE
       ELSE
            STRING=' '
            NC=1
       ENDIF
*** Default workstation (find one that has output).
       CALL GQOPS(IOPSTA)
*   No active workstations.
       IF(IOPSTA.LT.3)THEN
            PRINT *,' !!!!!! GRCOLR WARNING : No active workstations'//
     -           ' ; COLOUR not executed.'
            RETURN
       ENDIF
*   Determine number of active workstations.
       CALL GQACWK(0,IERR,NACT,IWK)
       IWKID=-1
       ITYPE=0
       DO 30 I=1,NACT
       CALL GQACWK(I,IERR,IDUM,IWK)
*   Locate one an out/in ws, if not existing one of type out.
       CALL GQWKC(IWK,IERR1,ICONID,IWKTYP)
       CALL GQWKCA(IWKTYP,IERR2,IWKCAT)
       IF(IWKCAT.EQ.2.AND.ITYPE.LT.2)THEN
            IWKID=IWK
            ITYPE=2
       ELSEIF((IWKCAT.EQ.0.OR.IWKCAT.EQ.4).AND.ITYPE.LT.1)THEN
            IWKID=IWK
            ITYPE=1
       ENDIF
30     CONTINUE
*   Issue an string request to an input workstation.
       IF(IWKID.EQ.-1)THEN
            PRINT *,' !!!!!! GRCOLR WARNING : No active workstations'//
     -           ' with output facilities ; COLOUR not executed.'
            RETURN
       ENDIF
*** Default colour.
       BLUE=-1.0
       GREEN=-1.0
       RED=-1.0
*** Read the various components of the colour description.
       INEXT=IKEY+2
       DO 100 I=IKEY+2,NWORD
       IF(I.LT.INEXT)GOTO 100
       IF(INPCMP(I,'BL#UE').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Blue value missing or not real')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,BLUE,-1.0)
                 IF(IFAIL1.EQ.0.AND.(BLUE.LT.0.0.OR.BLUE.GT.1.0))
     -                CALL INPMSG(I+1,'Blue value not in range [0,1].')
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'GR#EEN').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Green is missing or not real. ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,GREEN,-1.0)
                 IF(IFAIL1.EQ.0.AND.(GREEN.LT.0.0.OR.GREEN.GT.1.0))
     -                CALL INPMSG(I+1,'Green value not in range [0,1]')
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'RED').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Red value missing or not real.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,RED,-1.0)
                 IF(IFAIL1.EQ.0.AND.(RED.LT.0.0.OR.RED.GT.1.0))
     -                CALL INPMSG(I+1,'Red value not in range [0,1]. ')
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'HEX#ADECIMAL')+INPCMP(I,'RGB').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Red value missing or not real.')
            ELSE
                 CALL INPCHK(I+1,3,IFAIL1)
                 CALL INPRDH(I+1,IRGB,-1)
                 IF(IFAIL1.EQ.0.AND.(IRGB.LT.0.OR.IRGB.GT.16**6-1))THEN
                      CALL INPMSG(I+1,'RGB value not in range [0,1]. ')
                 ELSEIF(IFAIL1.EQ.0)THEN
                      BLUE=REAL(IRGB-256*(IRGB/256))/255.0
                      IRGB=IRGB/256
                      GREEN=REAL(IRGB-256*(IRGB/256))/255.0
                      IRGB=IRGB/256
                      RED=REAL(IRGB-256*(IRGB/256))/255.0
                 ENDIF
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'WORK#STATION').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Workstation missing or invalid')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,IWKID,1)
                 INEXT=I+2
            ENDIF
       ELSE
            CALL INPMSG(I,'This is not a known keyword.  ')
       ENDIF
100    CONTINUE
*** Dump the error messages.
       CALL INPERR
*** Now check whether the workstation has at all colour facilities.
       CALL GRCOLC(IWKID,IWKTYP,IFLAG)
       IF(IFLAG.GT.0)THEN
            PRINT *,' !!!!!! GRCOLR WARNING : The workstation does'//
     -           ' not have colour facilities.'
            RETURN
       ENDIF
*** Check validity of the request in terms of intensities.
       IF(NWORD.GT.IKEY+1.AND.(BLUE.LT.0.OR.BLUE.GT.1.OR.RED.LT.0.OR.
     -      RED.GT.1.OR.GREEN.LT.0.OR.GREEN.GT.1))THEN
            PRINT *,' !!!!!! GRCOLR WARNING : Your update request is'//
     -           ' not carried out because the'
            PRINT *,'                         colour is either'//
     -           ' incompletely or incorrectly specified.'
            RETURN
       ENDIF
*** Try incrementing the number of colours if update is requested.
       IF(ICOL.GT.NCOL.AND.NWORD.GT.IKEY+1)THEN
            IF(IFLAG.LT.0)GOTO 1010
            CALL GQLWK(IWKTYP,IERR,MPL,MPM,MTX,MFA,MPA,MXCOLI)
            IF(LDEBUG)WRITE(LUNOUT,*) ' ++++++ GRCOLR DEBUG   : Max.'//
     -           ' number of colours on this workstation: ',MXCOLI
            IF(IERR.NE.0)THEN
                 PRINT *,' !!!!!! GRCOLR WARNING : Unable to obtain'//
     -               ' the wk state table length; nothing done.'
                 RETURN
            ENDIF
            IF(ICOL+1.GT.MXCOLI)THEN
                 PRINT *,' !!!!!! GRCOLR WARNING : Workstation table'//
     -               ' of colours is full; new colour not defined.'
                 RETURN
            ENDIF
1010        CONTINUE
            IF(ICOL+1.GT.MXCOL)THEN
                 PRINT *,' !!!!!! GRCOLR WARNING : Internal colour'//
     -               ' name table is full; increase MXCOL, not defined.'
                 RETURN
            ENDIF
            NCOL=ICOL
            COLNAM(ICOL)=STRING(1:NC)
*** Failing inquiry because the colour is not known.
       ELSEIF(ICOL.GT.NCOL.AND.NWORD.EQ.IKEY+1)THEN
            PRINT *,' !!!!!! GRCOLR WARNING : The colour is not known.'
            RETURN
       ENDIF
*** Inquiry and update.
       DO 200 I=0,NCOL
       IF(ICOL.EQ.-1.OR.(IKEY+1.EQ.NWORD.AND.
     -      STRING(1:NC).EQ.COLNAM(I)))THEN
            CALL GRQCR(IWKID,I,0,IERR0,REDS,GREENS,BLUES)
            CALL GRQCR(IWKID,I,1,IERR1,REDR,GREENR,BLUER)
            DO 210 IC=20,1,-1
            IF(COLNAM(I)(IC:IC).NE.' ')THEN
                 NCC=IC
                 GOTO 220
            ENDIF
210         CONTINUE
            NCC=1
220         CONTINUE
            IF(IERR0.NE.0.OR.IERR1.NE.0)THEN
                 WRITE(LUNOUT,'(/''  Unable to retrieve the current'',
     -                '' representation of colour '',A,''.''/)')
     -                COLNAM(I)(1:NCC)
            ELSE
                 WRITE(LUNOUT,'(/''  Current representation of'',
     -                '' colour '',A,'' on workstation '',I3,'':''//
     -                2X,'' Blue:  '',F10.3,'' (set), '',
     -                F10.3,'' (realised),''/
     -                2X,'' Green: '',F10.3,'' (set), '',
     -                F10.3,'' (realised),''/
     -                2X,'' Red:   '',F10.3,'' (set), '',
     -                F10.3,'' (realised).''/)') COLNAM(I)(1:NCC),
     -                IWKID,BLUES,BLUER,GREENS,GREENR,REDS,REDR
            ENDIF
       ELSEIF(NWORD.GT.IKEY+1.AND.STRING(1:NC).EQ.COLNAM(I))THEN
            CALL GRSCR(IWKID,ICOL,RED,GREEN,BLUE)
       ENDIF
200    CONTINUE
*** If we get here, things are probably OK.
       IFAIL=0
       RETURN
*** GRCOLQ: Return the table index corresponding to a colour name.
       ENTRY GRCOLQ(IWKDUM,COLCMP,ICIND)
*   Try to locate the colour in the table.
       DO 300 I=0,NCOL
       IF(INPCMX(COLCMP,COLNAM(I)).NE.0)THEN
            ICIND=I
            GOTO 320
       ENDIF
300    CONTINUE
*   Set to -1 if not found.
       ICIND=-1
320    CONTINUE
       RETURN
*** GRCOLD: Return a string containing the description.
       ENTRY GRCOLD(IWKDES,ICIND,DESCR,NCD,OPTION)
*   Reject invalid colour reference numbers.
       IF(ICIND.LT.0.OR.ICIND.GT.NCOL)THEN
            DESCR='# Not a known colour.'
            NCD=21
            RETURN
       ENDIF
*   Inquire GKS about the intensities.
       CALL GRQCR(IWKDES,ICIND,1,IERR,RED,GREEN,BLUE)
*   And format the colour description.
       IF(IERR.NE.0)THEN
            DESCR='# Error retrieving the data.'
            NCD=28
            CALL INPFIX(COLNAM(ICIND),AUX,NC)
            DESCR=AUX(1:NC)//' (Unable to retrieve the description)'
            NCD=NC+37
       ELSE
            IF(OPTION.EQ.'RAW')THEN
                 DESCR=COLNAM(ICIND)
                 NCD=20
            ELSE
                 CALL INPFIX(COLNAM(ICIND),AUX,NC)
                 CALL OUTFMT(RED,2,AUX1,NC1,'LEFT')
                 CALL OUTFMT(BLUE,2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(GREEN,2,AUX3,NC3,'LEFT')
                 DESCR=AUX(1:NC)//' (Red '//AUX1(1:NC1)//', Blue '//
     -                AUX2(1:NC2)//', Green '//AUX3(1:NC3)//')'
                 NCD=NC+NC1+NC2+NC3+22
            ENDIF
       ENDIF
       RETURN
*** Write the settings to a file.
       ENTRY GRCOLW(IKEY,IFAIL)
*   Initial settings.
       FILE=' '
       NCFILE=1
       MEMBER='< none >'
       NCMEMB=8
       REMARK='none'
       NCREM=4
       IFAIL=1
       IWKID=1
*   Make sure there are colours.
       CALL GRCOLC(IWKID,IWKTYP,IFLAG)
       IF(IFLAG.GT.0)THEN
            PRINT *,' !!!!!! GRCOLW WARNING : The workstation does'//
     -           ' not have colour facilities.'
            RETURN
       ENDIF
*   First decode the argument string.
       CALL INPNUM(NWORD)
*   Make sure there is at least one argument.
       IF(NWORD.EQ.IKEY)THEN
            PRINT *,' !!!!!! GRCOLW WARNING : WRITE takes at least one',
     -           ' argument (a dataset name); data will not be written.'
            RETURN
*   Check whether keywords have been used.
       ELSEIF(INPCMP(IKEY+1,'D#ATASET')+
     -      INPCMP(IKEY+1,'R#EMARK').NE.0)THEN
            INEXT=IKEY+1
            DO 410 I=IKEY+1,NWORD
            IF(I.LT.INEXT)GOTO 410
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
                 CALL INPMSG(I,'The parameter is not known.    ')
            ENDIF
410         CONTINUE
*   Otherwise the string is interpreted as a file name (+ member name).
       ELSE
            CALL INPSTR(IKEY+1,IKEY+1,STRING,NCFILE)
            FILE=STRING
            IF(NWORD.GE.IKEY+2)THEN
                 CALL INPSTR(IKEY+2,IKEY+2,STRING,NCMEMB)
                 MEMBER=STRING
            ENDIF
            IF(NWORD.GE.IKEY+3)THEN
                 CALL INPSTR(IKEY+3,NWORD,STRING,NCREM)
                 REMARK=STRING
            ENDIF
       ENDIF
*   Print error messages.
       CALL INPERR
       IF(NCFILE.GT.MXNAME)PRINT *,' !!!!!! GRCOLW WARNING : The file',
     -      ' name is truncated to MXNAME (=',MXNAME,') characters.'
       IF(NCMEMB.GT.8)PRINT *,' !!!!!! GRCOLW WARNING : The member',
     -      ' name is shortened to ',MEMBER,', first 8 characters.'
       IF(NCREM.GT.29)PRINT *,' !!!!!! GRCOLW WARNING : The remark',
     -      ' shortened to ',REMARK,', first 29 characters.'
       NCFILE=MIN(NCFILE,MXNAME)
       NCMEMB=MIN(NCMEMB,8)
       NCREM=MIN(NCREM,29)
*   Check whether the member already exists.
       CALL DSNREM(FILE(1:NCFILE),MEMBER(1:NCMEMB),'GRAPHCOL',EXMEMB)
       IF(JEXMEM.EQ.2.AND.EXMEMB)THEN
            PRINT *,' ------ GRCOLW MESSAGE : A copy of the member'//
     -           ' exists; new member will be appended.'
       ELSEIF(JEXMEM.EQ.3.AND.EXMEMB)THEN
            PRINT *,' !!!!!! GRCOLW WARNING : A copy of the member'//
     -           ' exists already; member will not be written.'
            RETURN
       ENDIF
*   Print some debugging output if requested.
       IF(LDEBUG)THEN
            PRINT *,' ++++++ GRCOLW DEBUG   : File= '//FILE(1:NCFILE)//
     -           ', member= '//MEMBER(1:NCMEMB)
            PRINT *,'                         Remark= '//REMARK(1:NCREM)
       ENDIF
**  Open the dataset for sequential write and inform DSNLOG.
       CALL DSNOPN(FILE,NCFILE,12,'WRITE-LIBRARY',IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! GRCOLW WARNING : Opening '//FILE(1:NCFILE),
     -              ' failed ; the colour data will not be written.'
            RETURN
       ENDIF
       CALL DSNLOG(FILE,'Colours   ','Sequential','Write     ')
       IF(LDEBUG)PRINT *,' ++++++ GRCOLW DEBUG   : Dataset ',
     -      FILE(1:NCFILE),' opened on unit 12 for seq write.'
*   Now write a heading record to the file.
       CALL DATTIM(DATE,TIME)
       WRITE(STRING,'(''% Created '',A8,'' At '',A8,1X,A8,'' GRAPHCOL'',
     -      1X,''"'',A29,''"'')') DATE,TIME,MEMBER,REMARK
       WRITE(12,'(A80)',IOSTAT=IOS,ERR=2010) STRING
       IF(LDEBUG)THEN
            PRINT *,' ++++++ GRCOLW DEBUG   : Dataset heading record:'
            PRINT *,STRING
       ENDIF
*   Write the actual data, start with the number of colours.
       WRITE(12,'('' NCOL='',I3)',ERR=2010,IOSTAT=IOS) NCOL
*   Next a list of Polyline attributes.
       DO 420 I=0,NCOL
       CALL GRQCR(IWKID,I,1,IERR,RED,GREEN,BLUE)
       IF(IERR.NE.0)THEN
            PRINT *,' !!!!!! GRCOLW WARNING : Unable to retrieve data'//
     -           ' about colour ',I
            GOTO 420
       ENDIF
       WRITE(12,'(A20,3E15.8)',ERR=2010,IOSTAT=IOS)
     -      COLNAM(I),RED,BLUE,GREEN
420    CONTINUE
**  Close the file after the operation.
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       CALL TIMLOG('Writing out a list of colours:          ')
       IFAIL=0
       RETURN
*** Read the presentation from dataset.
       ENTRY GRCOLG(IKEY,IFAIL)
*   Initial values.
       FILE=' '
       MEMBER='*'
       NCFILE=8
       NCMEMB=1
       IFAIL=1
       IWKID=1
*   Make sure there are colours.
       CALL GRCOLC(IWKID,IWKTYP,IFLAG)
       IF(IFLAG.GT.0)THEN
            PRINT *,' !!!!!! GRCOLW WARNING : The workstation does'//
     -           ' not have colour facilities.'
            RETURN
       ENDIF
**  First decode the argument string, setting file name + member name.
       CALL INPNUM(NWORD)
*   If there's only one argument, it's the dataset name.
       IF(NWORD.GE.IKEY+1)THEN
            CALL INPSTR(IKEY+1,IKEY+1,STRING,NCFILE)
            FILE=STRING
       ENDIF
*   If there's a second argument, it is the member name.
       IF(NWORD.GE.IKEY+2)THEN
            CALL INPSTR(IKEY+2,IKEY+2,STRING,NCMEMB)
            MEMBER=STRING
       ENDIF
*   Check the various lengths.
       IF(NCFILE.GT.MXNAME)THEN
            PRINT *,' !!!!!! GRCOLG WARNING : The file name is'//
     -           ' truncated to MXNAME (=',MXNAME,') characters.'
            NCFILE=MIN(NCFILE,MXNAME)
       ENDIF
       IF(NCMEMB.GT.8)THEN
            PRINT *,' !!!!!! GRCOLG WARNING : The member name is'//
     -           ' shortened to ',MEMBER,', first 8 characters.'
            NCMEMB=MIN(NCMEMB,8)
       ELSEIF(NCMEMB.LE.0)THEN
            PRINT *,' !!!!!! GRCOLG WARNING : The member'//
     -           ' name has zero length, replaced by "*".'
            MEMBER='*'
            NCMEMB=1
       ENDIF
*   Reject the empty file name case.
       IF(FILE.EQ.' '.OR.NWORD.EQ.1)THEN
            PRINT *,' !!!!!! GRCOLG WARNING : GET must be at least'//
     -           ' followed by a dataset name ; no data are read.'
            RETURN
       ENDIF
*   If there are even more args, warn they are ignored.
       IF(NWORD.GT.IKEY+2)PRINT *,' !!!!!! GRCOLG WARNING : GET takes'//
     -     ' at most two arguments (dataset and member); rest ignored.'
**  Open the dataset and inform DSNLOG.
       CALL DSNOPN(FILE,NCFILE,12,'READ-LIBRARY',IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! GRCOLG WARNING : Opening ',FILE(1:NCFILE),
     -           ' failed ; colour data are not read.'
            RETURN
       ENDIF
       CALL DSNLOG(FILE,'Colours   ','Sequential','Read only ')
       IF(LDEBUG)PRINT *,' ++++++ GRCOLG DEBUG   : Dataset',
     -      FILE(1:NCFILE),' opened on unit 12 for seq read.'
*   Locate the pointer on the header of the requested member.
       CALL DSNLOC(MEMBER,NCMEMB,'GRAPHCOL',12,EXIS,'RESPECT')
       IF(.NOT.EXIS)THEN
            CALL DSNLOC(MEMBER,NCMEMB,'GRAPHCOL',12,EXIS,'IGNORE')
            IF(EXIS)THEN
                 PRINT *,' ###### GRCOLG ERROR   : Colour data ',
     -                MEMBER(1:NCMEMB),' has been deleted from ',
     -                FILE(1:NCFILE),'; not read.'
            ELSE
                 PRINT *,' ###### GRCOLG ERROR   : Colour data ',
     -                MEMBER(1:NCMEMB),' not found on ',FILE(1:NCFILE)
            ENDIF
            CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
**  Check that the member is acceptable date wise.
       READ(12,'(A80)',END=2000,IOSTAT=IOS,ERR=2010) STRING
       IF(LDEBUG)THEN
            PRINT *,' ++++++ GRCOLG DEBUG   : Dataset header'//
     -              ' record follows:'
            PRINT *,STRING
       ENDIF
       IF(DSNCMP('14-07-89',STRING(11:18)))THEN
            PRINT *,' !!!!!! GRCOLG WARNING : Member '//STRING(32:39)//
     -           ' can not be read because of a change in format.'
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
       WRITE(LUNOUT,'(''  Member '',A8,'' was created on '',A8,
     -      '' at '',A8/''  Remarks: '',A29)')
     -      STRING(32:39),STRING(11:18),STRING(23:30),STRING(51:79)
*   Read the actual data, start with the number of items of each type.
       READ(12,'(6X,I3)',END=2000,ERR=2010,IOSTAT=IOS) NCOL
*   Make sure none of these exceeds the maximum numbers.
       CALL GQLWK(IWKTYP,IERR,MPL,MPM,MTX,MFA,MPA,MXCOLI)
       IF(NCOL.GT.MXCOLI.OR.NCOL.GT.MXCOL)THEN
            PRINT *,' !!!!!! GRCOLG WARNING : The number of colours'//
     -           ' is larger than either the GKS or'
            PRINT *,'                         the compilation maxima;'//
     -           ' increase these and recompile.'
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
*   Read the list of colours.
       DO 430 I=0,NCOL
       READ(12,'(A20,3E15.8)',END=2000,ERR=2010,IOSTAT=IOS)
     -      COLNAM(I),RED,BLUE,GREEN
       CALL GRSCR(IWKID,I,RED,GREEN,BLUE)
430    CONTINUE
**  Close the file after the operation.
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       CALL TIMLOG('Reading in a list of colours:           ')
       IFAIL=0
       RETURN
*** Plot a colour map.
       ENTRY GRCOLM
**  Loop over the colours, first the loop over the pages.
       DO 510 II=0,NCOL,20
*   Switch to graphics mode.
       CALL GRGRAF(.TRUE.)
*   Switch to normalised device coordinates.
       CALL GSELNT(0)
*   Switch to solid interior style.
       CALL GSFAIS(1)
*   Set reasonable character attributes.
       CALL GSTXFP(0,2)
       CALL GSCHXP(1.0)
       CALL GSCHSP(0.0)
       CALL GSCHH(0.02)
       CALL GSTXAL(1,3)
       CALL GSCHUP(0.0,1.0)
       CALL GSTXCI(1)
*   Put some bands over the screen to compare colours, first white.
       XPL(1)=0.25
       YPL(1)=0
       XPL(2)=0.25
       YPL(2)=1
       XPL(3)=0.375
       YPL(3)=1
       XPL(4)=0.375
       YPL(4)=0
       XPL(5)=0.25
       YPL(5)=0
       CALL GSFACI(0)
       CALL GFA(5,XPL,YPL)
*   Then a black band.
       XPL(1)=0.375
       YPL(1)=0
       XPL(2)=0.375
       YPL(2)=1
       XPL(3)=0.5
       YPL(3)=1
       XPL(4)=0.5
       YPL(4)=0
       XPL(5)=0.375
       YPL(5)=0
       CALL GSFACI(1)
       CALL GFA(5,XPL,YPL)
*   If there are lots of colours, another white band.
       IF(MIN(19,NCOL-II).GE.10)THEN
            XPL(1)=0.75
            YPL(1)=0
            XPL(2)=0.75
            YPL(2)=1
            XPL(3)=0.875
            YPL(3)=1
            XPL(4)=0.875
            YPL(4)=0
            XPL(5)=0.75
            YPL(5)=0
            CALL GSFACI(0)
            CALL GFA(5,XPL,YPL)
*   And another black band.
            XPL(1)=0.875
            YPL(1)=0
            XPL(2)=0.875
            YPL(2)=1
            XPL(3)=1
            YPL(3)=1
            XPL(4)=1
            YPL(4)=0
            XPL(5)=0.875
            YPL(5)=0
            CALL GSFACI(1)
            CALL GFA(5,XPL,YPL)
       ENDIF
**  Then the loop over the colours on this page.
       DO 520 I=0,MIN(19,NCOL-II)
*   Plot the colour name.
       CALL INPFIX(COLNAM(II+I),AUX,NC)
       IF(I.LE.9)THEN
            CALL GTX(0.02,0.95-0.1*I,AUX(1:NC))
       ELSE
            CALL GTX(0.52,1.95-0.1*I,AUX(1:NC))
       ENDIF
*   Set the colour.
       CALL GSFACI(II+I)
*   Plot a box with the colour.
       IF(I.LE.9)THEN
            XPL(1)=0.26
            YPL(1)=0.99-0.1*I
            XPL(2)=0.26
            YPL(2)=0.91-0.1*I
            XPL(3)=0.49
            YPL(3)=0.91-0.1*I
            XPL(4)=0.49
            YPL(4)=0.99-0.1*I
            XPL(5)=0.26
            YPL(5)=0.99-0.1*I
       ELSE
            XPL(1)=0.76
            YPL(1)=1.99-0.1*I
            XPL(2)=0.76
            YPL(2)=1.91-0.1*I
            XPL(3)=0.99
            YPL(3)=1.91-0.1*I
            XPL(4)=0.99
            YPL(4)=1.99-0.1*I
            XPL(5)=0.76
            YPL(5)=1.99-0.1*I
       ENDIF
       CALL GFA(5,XPL,YPL)
*   Next colour.
520    CONTINUE
*   Next page.
       CALL GRALOG('Colour map:')
       CALL GRNEXT
510    CONTINUE
*   Keep track of CPU time consumption.
       CALL TIMLOG('Producing a colour map:                 ')
       RETURN
*** Colour table reset.
       ENTRY GRCOLS
       NCOL=1
       RETURN
*** Handle the error conditions.
2000   CONTINUE
       PRINT *,' ###### GRCOLG ERROR   : Premature EOF ecountered on '//
     -      FILE(1:NCFILE)//' read via unit 12 ; no valid data read.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2010   CONTINUE
       PRINT *,' ###### GRCOLW ERROR   : I/O error accessing '//
     -      FILE(1:NCFILE)//' via unit 12 ; no data read or written.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2030   CONTINUE
       PRINT *,' ###### GRCOLW ERROR   : Dataset '//FILE(1:NCFILE)//
     -      ' unit 12 cannot be closed ; results not predictable'
       CALL INPIOS(IOS)
       END
