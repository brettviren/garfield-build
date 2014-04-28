CDECK  ID>, GASWRT.
       SUBROUTINE GASWRT(IMODE)
*-----------------------------------------------------------------------
*   GASWRT - This routine writes all gas information on an external
*            dataset.
*   VARIABLES : IMODE       : If 1 : find name, if 2 write gas data
*               IACC        : If 0 no name specified, no write
*                             If 1 name OK, write will be executed
*   (Last changed on 18/09/11.)
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
       DOUBLE PRECISION CLSDIS,CLSAVE
       REAL EGAS,VGAS,XGAS,YGAS,DGAS,AGAS,BGAS,HGAS,MGAS,WGAS,OGAS,SGAS,
     -      EXGAS,IOGAS,
     -      CVGAS,CXGAS,CYGAS,CDGAS,CAGAS,CBGAS,CHGAS,CMGAS,CWGAS,COGAS,
     -      CSGAS,CEXGAS,CIOGAS,
     -      VGAS2,XGAS2,YGAS2,DGAS2,AGAS2,BGAS2,HGAS2,MGAS2,WGAS2,OGAS2,
     -      SGAS2,EXGAS2,IOGAS2,
     -      AORIG,AORIG2,PENPRB,PENRMS,PENDT,ENIOG,ENEXG,
     -      BANG,BTAB,
     -      VEXTR1,VEXTR2,VEXTR3,VEXTR4,
     -      XEXTR1,XEXTR2,XEXTR3,XEXTR4,
     -      YEXTR1,YEXTR2,YEXTR3,YEXTR4,
     -      DEXTR1,DEXTR2,DEXTR3,DEXTR4,
     -      AEXTR1,AEXTR2,AEXTR3,AEXTR4,
     -      BEXTR1,BEXTR2,BEXTR3,BEXTR4,
     -      HEXTR1,HEXTR2,HEXTR3,HEXTR4,
     -      MEXTR1,MEXTR2,MEXTR3,MEXTR4,
     -      WEXTR1,WEXTR2,WEXTR3,WEXTR4,
     -      OEXTR1,OEXTR2,OEXTR3,OEXTR4,
     -      SEXTR1,SEXTR2,SEXTR3,SEXTR4,
     -      EEXTR1,EEXTR2,EEXTR3,EEXTR4,
     -      ZEXTR1,ZEXTR2,ZEXTR3,ZEXTR4,
     -      GASRNG,
     -      Z,A,RHO,CMEAN,EMPROB,EPAIR,PGAS,TGAS,GASDEN,
     -      DTION,DLION,GASFRM,ELOSCS
       LOGICAL GASOK,TAB2D,GASOPT,HEEDOK,SRIMOK,TRIMOK,GASSET
       INTEGER NGAS,NCLS,NBANG,NBTAB,NFTAB,NFCLS,
     -      IVMETH,IXMETH,IYMETH,IDMETH,IAMETH,IBMETH,IHMETH,IMMETH,
     -      IWMETH,IOMETH,ISMETH,IEMETH,IZMETH,
     -      IVEXTR,IXEXTR,IYEXTR,IDEXTR,IAEXTR,IBEXTR,IHEXTR,IMEXTR,
     -      IWEXTR,IOEXTR,ISEXTR,IEEXTR,IZEXTR,
     -      JVEXTR,JXEXTR,JYEXTR,JDEXTR,JAEXTR,JBEXTR,JHEXTR,JMEXTR,
     -      JWEXTR,JOEXTR,JSEXTR,JEEXTR,JZEXTR,
     -      IATHR,IBTHR,IHTHR,
     -      NEXGAS,NIOGAS,NCSGAS,ICSTYP
       CHARACTER*80 GASID
       CHARACTER*(MXCHAR) FCNTAB,FCNCLS
       CHARACTER*10 CLSTYP
       CHARACTER*45 DSCEXG(MXEXG),DSCIOG(MXIOG),DSCCSG(MXCSG)
       COMMON /GASDAT/ CLSDIS(MXPAIR),CLSAVE,
     -      EGAS(MXLIST),
     -      VGAS(MXLIST),XGAS(MXLIST),YGAS(MXLIST),WGAS(MXLIST),
     -      DGAS(MXLIST),OGAS(MXLIST),AGAS(MXLIST),BGAS(MXLIST),
     -      HGAS(MXLIST),MGAS(MXLIST),SGAS(MXLIST,6),
     -      EXGAS(MXLIST,MXEXG),IOGAS(MXLIST,MXIOG),
     -      CVGAS(MXLIST),CXGAS(MXLIST),CYGAS(MXLIST),CWGAS(MXLIST),
     -      CDGAS(MXLIST),COGAS(MXLIST),CAGAS(MXLIST),CBGAS(MXLIST),
     -      CHGAS(MXLIST),CMGAS(MXLIST),CSGAS(MXLIST,6),
     -      CEXGAS(MXLIST,MXEXG),CIOGAS(MXLIST,MXIOG),
     -      VGAS2(MXLIST,MXBANG,MXBTAB),WGAS2(MXLIST,MXBANG,MXBTAB),
     -      XGAS2(MXLIST,MXBANG,MXBTAB),YGAS2(MXLIST,MXBANG,MXBTAB),
     -      AGAS2(MXLIST,MXBANG,MXBTAB),BGAS2(MXLIST,MXBANG,MXBTAB),
     -      DGAS2(MXLIST,MXBANG,MXBTAB),OGAS2(MXLIST,MXBANG,MXBTAB),
     -      HGAS2(MXLIST,MXBANG,MXBTAB),MGAS2(MXLIST,MXBANG,MXBTAB),
     -      SGAS2(MXLIST,MXBANG,MXBTAB,6),
     -      EXGAS2(MXLIST,MXBANG,MXBTAB,MXEXG),
     -      IOGAS2(MXLIST,MXBANG,MXBTAB,MXIOG),
     -      AORIG(MXLIST),AORIG2(MXLIST,MXBANG,MXBTAB),
     -      PENPRB(MXEXG),PENRMS(MXEXG),PENDT(MXEXG),
     -      ENIOG(MXIOG),ENEXG(MXEXG),
     -      BANG(MXBANG),BTAB(MXBTAB),
     -      GASRNG(20,2),GASFRM(MXNBMC),ELOSCS(MXCSG),
     -      Z,A,RHO,CMEAN,EMPROB,EPAIR,PGAS,TGAS,GASDEN,
     -      DTION,DLION,
     -      VEXTR1,VEXTR2,VEXTR3,VEXTR4,
     -      XEXTR1,XEXTR2,XEXTR3,XEXTR4,
     -      YEXTR1,YEXTR2,YEXTR3,YEXTR4,
     -      DEXTR1,DEXTR2,DEXTR3,DEXTR4,
     -      AEXTR1,AEXTR2,AEXTR3,AEXTR4,
     -      BEXTR1,BEXTR2,BEXTR3,BEXTR4,
     -      HEXTR1,HEXTR2,HEXTR3,HEXTR4,
     -      MEXTR1,MEXTR2,MEXTR3,MEXTR4,
     -      WEXTR1,WEXTR2,WEXTR3,WEXTR4,
     -      OEXTR1,OEXTR2,OEXTR3,OEXTR4,
     -      SEXTR1(6),SEXTR2(6),SEXTR3(6),SEXTR4(6),
     -      EEXTR1(MXEXG),EEXTR2(MXEXG),EEXTR3(MXEXG),EEXTR4(MXEXG),
     -      ZEXTR1(MXIOG),ZEXTR2(MXIOG),ZEXTR3(MXIOG),ZEXTR4(MXIOG),
     -      IVMETH,IXMETH,IYMETH,IDMETH,IAMETH,IBMETH,IHMETH,IMMETH,
     -      IWMETH,IOMETH,ISMETH,IEMETH,IZMETH,
     -      IVEXTR,IXEXTR,IYEXTR,IDEXTR,IAEXTR,IBEXTR,IHEXTR,IMEXTR,
     -      IWEXTR,IOEXTR,ISEXTR,IEEXTR,IZEXTR,
     -      JVEXTR,JXEXTR,JYEXTR,JDEXTR,JAEXTR,JBEXTR,JHEXTR,JMEXTR,
     -      JWEXTR,JOEXTR,JSEXTR,JEEXTR,JZEXTR,
     -      NGAS,NCLS,NBANG,NBTAB,NFTAB,NFCLS,
     -      IATHR,IBTHR,IHTHR,
     -      NEXGAS,NIOGAS,NCSGAS,ICSTYP(MXCSG),
     -      GASOK(20),GASOPT(20,4),
     -      TAB2D,HEEDOK,SRIMOK,TRIMOK,GASSET
       COMMON /GASCHR/ FCNTAB,FCNCLS,CLSTYP,GASID,DSCEXG,DSCIOG,DSCCSG
       LOGICAL MAGOK
       REAL ALFA,B0X,B0Y,B0Z,SUSWIR,SUSGAS,BSCALE,BFMIN,BFMAX,
     -      BFXMIN,BFYMIN,BFZMIN,BFXMAX,BFYMAX,BFZMAX
       INTEGER MAGSRC,
     -      IBXTYP,IBYTYP,IBZTYP,
     -      IRB0X,IRB0Y,IRB0Z,IRV0X,IRV0Y,IRV0Z,
     -      IENB0X,IENB0Y,IENB0Z,IBXDIR,IBYDIR,IBZDIR,
     -      NCB0X,NCB0Y,NCB0Z
       CHARACTER*(MXCHAR) FUNB0X,FUNB0Y,FUNB0Z
       COMMON /MAGDAT/ ALFA,SUSWIR,SUSGAS,
     -      B0X,B0Y,B0Z,BSCALE,BFMIN,BFMAX,
     -      BFXMIN,BFYMIN,BFZMIN,BFXMAX,BFYMAX,BFZMAX,
     -      MAGSRC,IBXTYP,IBYTYP,IBZTYP,
     -      IRB0X,IRB0Y,IRB0Z,IRV0X,IRV0Y,IRV0Z,
     -      IENB0X,IENB0Y,IENB0Z,IBXDIR,IBYDIR,IBZDIR,
     -      NCB0X,NCB0Y,NCB0Z,
     -      MAGOK
       COMMON /MAGCHR/ FUNB0X,FUNB0Y,FUNB0Z
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*(MXCHAR) STRING
       CHARACTER*29 REMARK
       CHARACTER*8 TIME,DATE,MEMBER
       CHARACTER*(MXNAME) FILE
       LOGICAL EXMEMB
       INTEGER IACC,IMODE,NCFILE,NCMEMB,NCREM,INPCMP,NWORD,INEXT,I,J,K,
     -      L,II,IOS,IFAIL
       EXTERNAL INPCMP
       SAVE IACC,FILE,NCFILE,MEMBER,NCMEMB,REMARK,NCREM
       DATA IACC/0/
*** Identify the routine, if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE GASWRT ///'
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
            PRINT *,' !!!!!! GASWRT WARNING : WRITE takes at least one',
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
       IF(NCFILE.GT.MXNAME)PRINT *,' !!!!!! GASWRT WARNING : The file',
     -      ' name is truncated to MXNAME (=',MXNAME,') characters.'
       IF(NCMEMB.GT.8)PRINT *,' !!!!!! GASWRT WARNING : The member',
     -      ' name is shortened to ',MEMBER,', first 8 characters.'
       IF(NCREM.GT.29)PRINT *,' !!!!!! GASWRT WARNING : The remark',
     -      ' shortened to ',REMARK,', first 29 characters.'
       NCFILE=MIN(NCFILE,MXNAME)
       NCMEMB=MIN(NCMEMB,8)
       NCREM=MIN(NCREM,29)
*   Check whether the member already exists.
       CALL DSNREM(FILE(1:NCFILE),MEMBER(1:NCMEMB),'GAS',EXMEMB)
       IF(JEXMEM.EQ.2.AND.EXMEMB)THEN
            PRINT *,' ------ GASWRT MESSAGE : A copy of the member'//
     -           ' exists; new member will be appended.'
       ELSEIF(JEXMEM.EQ.3.AND.EXMEMB)THEN
            PRINT *,' !!!!!! GASWRT WARNING : A copy of the member'//
     -           ' exists already; member will not be written.'
            RETURN
       ENDIF
*   Everything seems to be OK, the accept flag can be set to 'accept'.
       IACC=1
*   Print some debugging output if requested.
       IF(LDEBUG)THEN
            PRINT *,' ++++++ GASWRT DEBUG   : File= ',FILE(1:NCFILE),
     -           ', member= ',MEMBER(1:NCMEMB),' IACC=',IACC
            PRINT *,'                         Remark= ',REMARK(1:NCREM)
       ENDIF
       RETURN
*** Execute write operation if a valid name is available.
200    CONTINUE
       IF(IACC.EQ.0)RETURN
       IACC=0
*   Open a dataset and inform DSNLOG.
       CALL DSNOPN(FILE,NCFILE,12,'WRITE-LIBRARY',IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! GASWRT WARNING : Opening ',FILE(1:NCFILE),
     -           ' failed ; gas data will not be written'
            RETURN
       ENDIF
       CALL DSNLOG(FILE,'Gas data  ','Sequential','Write     ')
       IF(LDEBUG)PRINT *,' ++++++ GASWRT DEBUG   : Dataset ',
     -      FILE(1:NCFILE),' opened on unit 12 for seq write.'
*   Now write a heading record to the file.
       CALL DATTIM(DATE,TIME)
       WRITE(STRING,'(''% Created '',A8,'' at '',A8,1X,A8,'' GAS     '',
     -      1X,''"'',A29,''"'')') DATE,TIME,MEMBER,REMARK
       WRITE(12,'(A80)',IOSTAT=IOS,ERR=2010) STRING
*   Write a version number.
       WRITE(12,'('' Version   : 12'')')
*   Write the gas to the dataset.
       WRITE(12,'('' GASOK bits: '',20L1)',IOSTAT=IOS,ERR=2010)
     -      (GASOK(I),I=1,20)
       WRITE(12,'('' Identifier: '',A)',IOSTAT=IOS,ERR=2010) GASID
       WRITE(12,'('' Clusters  : '',A80)',IOSTAT=IOS,ERR=2010) FCNTAB
       WRITE(12,'('' Dimension : '',L1,5I10)',IOSTAT=IOS,ERR=2010)
     -      TAB2D,NGAS,NBANG,NBTAB,NEXGAS,NIOGAS
       WRITE(12,'('' E fields   ''/(5E15.8))',IOSTAT=IOS,ERR=2010)
     -      (EGAS(I),I=1,NGAS)
       WRITE(12,'('' E-B angles ''/(5E15.8))',IOSTAT=IOS,ERR=2010)
     -      (BANG(I),I=1,NBANG)
       WRITE(12,'('' B fields   ''/(5E15.8))',IOSTAT=IOS,ERR=2010)
     -      (BTAB(I),I=1,NBTAB)
*   Composition
       WRITE(12,'('' Mixture:   ''/(5E15.8))',IOSTAT=IOS,ERR=2010)
     -      (GASFRM(I),I=1,MXNBMC)
*   Excitation and ionisation identification strings.
       DO 260 I=1,NEXGAS
       WRITE(12,'('' Excitation '',I5,'': '',A45,2X,4E15.8)',
     -       IOSTAT=IOS,ERR=2010) I,DSCEXG(I),ENEXG(I),PENPRB(I),
     -       PENRMS(I),PENDT(I)
260    CONTINUE
       DO 270 I=1,NIOGAS
       WRITE(12,'('' Ionisation '',I5,'': '',A45,2X,E15.8)',
     -       IOSTAT=IOS,ERR=2010) I,DSCIOG(I),ENIOG(I)
270    CONTINUE
*   Gas tables.
       WRITE(12,'('' The gas tables follow:'')',IOSTAT=IOS,ERR=2010)
       IF(TAB2D)THEN
            DO 210 I=1,NGAS
            DO 220 J=1,NBANG
            DO 230 K=1,NBTAB
            WRITE(12,'(8E15.8)',IOSTAT=IOS,ERR=2010)
     -           VGAS2(I,J,K),XGAS2(I,J,K),YGAS2(I,J,K),DGAS2(I,J,K),
     -           OGAS2(I,J,K),AGAS2(I,J,K),AORIG2(I,J,K),BGAS2(I,J,K),
     -           MGAS2(I,J,K),WGAS2(I,J,K),HGAS2(I,J,K),
     -           (SGAS2(I,J,K,L),L=1,6),
     -           (EXGAS2(I,J,K,L),L=1,NEXGAS),
     -           (IOGAS2(I,J,K,L),L=1,NIOGAS)
230         CONTINUE
220         CONTINUE
210         CONTINUE
       ELSE
            DO 240 I=1,NGAS
            WRITE(12,'(8E15.8)',IOSTAT=IOS,ERR=2010)
     -           VGAS(I),CVGAS(I),XGAS(I),CXGAS(I),YGAS(I),CYGAS(I),
     -           DGAS(I),CDGAS(I),OGAS(I),COGAS(I),
     -           AGAS(I),CAGAS(I),AORIG(I),BGAS(I),CBGAS(I),
     -           MGAS(I),CMGAS(I),WGAS(I),CWGAS(I),HGAS(I),CHGAS(I),
     -           (SGAS(I,L),CSGAS(I,L),L=1,6),
     -           (EXGAS(I,L),CEXGAS(I,L),L=1,NEXGAS),
     -           (IOGAS(I,L),CIOGAS(I,L),L=1,NIOGAS)
240         CONTINUE
            WRITE(12,'('' H Extr: '',13I5)',IOSTAT=IOS,ERR=2010)
     -           IVEXTR,IXEXTR,IYEXTR,IDEXTR,IAEXTR,IBEXTR,IMEXTR,
     -           IWEXTR,IOEXTR,IHEXTR,ISEXTR,IEEXTR,IZEXTR
            WRITE(12,'('' L Extr: '',13I5)',IOSTAT=IOS,ERR=2010)
     -           JVEXTR,JXEXTR,JYEXTR,JDEXTR,JAEXTR,JBEXTR,JMEXTR,
     -           JWEXTR,JOEXTR,JHEXTR,JSEXTR,JEEXTR,JZEXTR
       ENDIF
*   Thresholds for Townsend, attachment and dissociation.
       WRITE(12,'('' Thresholds: '',3I10)',IOSTAT=IOS,ERR=2010)
     -      IATHR,IBTHR,IHTHR
*   Interpolation methods.
       WRITE(12,'('' Interp: '',13I5)',IOSTAT=IOS,ERR=2010)
     -      IVMETH,IXMETH,IYMETH,IDMETH,IAMETH,IBMETH,IMMETH,
     -      IWMETH,IOMETH,IHMETH,ISMETH,IEMETH,IZMETH
       WRITE(12,'('' A     ='',E15.8,'', Z     ='',E15.8,
     -      '', EMPROB='',E15.8,'', EPAIR ='',E15.8)',IOSTAT=IOS,
     -      ERR=2010) A,Z,EMPROB,EPAIR
       WRITE(12,'('' Ion diffusion: '',2E15.8)') DLION,DTION
       WRITE(12,'('' CMEAN ='',E15.8,'', RHO   ='',E15.8,
     -      '', PGAS  ='',E15.8,'', TGAS  ='',E15.8)',IOSTAT=IOS,
     -      ERR=2010) CMEAN,RHO,PGAS,TGAS
       WRITE(12,'('' CLSTYP    : '',A10)',IOSTAT=IOS,ERR=2010) CLSTYP
       WRITE(12,'('' FCNCLS    : '',A80)',IOSTAT=IOS,ERR=2010) FCNCLS
       WRITE(12,'('' NCLS      : '',2I10)',IOSTAT=IOS,ERR=2010) NCLS
       WRITE(12,'('' Average   : '',D25.18)',IOSTAT=IOS,ERR=2010) CLSAVE
       DO 250 II=1,NCLS,5
       WRITE(12,'(5D25.18)',IOSTAT=IOS,ERR=2010)
     -      (CLSDIS(I),I=II,MIN(II+4,NCLS))
250    CONTINUE
*   Write the Heed data to the file.
       CALL GASHWR(IFAIL)
       IF(IFAIL.NE.0)PRINT *,' !!!!!! GASWRT WARNING : Writing the'//
     -      ' Heed data failed ; gas data unuseable.'
*   Write the SRIM data to the file.
       CALL GASSWR(IFAIL)
       IF(IFAIL.NE.0)PRINT *,' !!!!!! GASWRT WARNING : Writing the'//
     -      ' SRIM data failed ; gas data unuseable.'
*   Close the file after the operation.
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       IACC=0
       CALL TIMLOG('Writing the gas data to a dataset:      ')
       RETURN
*** Handle the I/O error conditions.
2010   CONTINUE
       PRINT *,' ###### GASWRT ERROR   : Error while writing'//
     -      ' to '//FILE(1:NCFILE)//' via unit 12 ; gas data unuseable.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2030   CONTINUE
       PRINT *,' ###### GASWRT ERROR   : Dataset '//FILE(1:NCFILE)//
     -      ' on unit 12 cannot be closed ; results not predictable'
       CALL INPIOS(IOS)
       END
