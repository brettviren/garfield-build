CDECK  ID>, GRATTR.
       SUBROUTINE GRATTR(IKEY,IFAIL)
*-----------------------------------------------------------------------
*   GRATTR - Updates the attribute list for the various sorts of output.
*   (Last changed on 22/ 3/12.)
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
       REAL USERX0,USERX1,USERY0,USERY1,FRXMIN,FRXMAX,FRYMIN,FRYMAX,
     -      ARRANG,ARRLEN,BARFRC,DISPX0,DISPX1,DISPY0,DISPY1,
     -      GPXN,GPXN10,GPYN,GPYN10,GPXL,GPYL,GPXT
       LOGICAL LGRID,LGRALL,LOGX,LOGY,LSTAMP,LGCLRB,LGCLRA,
     -      LWAITA,LWAITB,LXCCH,LGLCLP,LGMCLP,LGACLP,LGTCLP,
     -      WKMULT(MXWKLS)
       INTEGER NWK,WKID(MXWKLS),WKCON(MXWKLS),WKFREF(MXWKLS),
     -         WKLUN(MXWKLS),WKSTAT(MXWKLS),WKSREQ(MXWKLS),
     -         NCWKNM(MXWKLS),NCSTMP,IGHIST,IGBAR,NCGKS
       CHARACTER*20 WKNAME(MXWKLS),WKATTR(MXWKLS)
       CHARACTER*80 STAMP
       CHARACTER*(MXNAME) GKSLOG
       COMMON /GRADAT/ USERX0,USERX1,USERY0,USERY1,ARRANG,ARRLEN,
     -      BARFRC,
     -      FRXMIN,FRXMAX,FRYMIN,FRYMAX,DISPX0,DISPX1,DISPY0,DISPY1,
     -      GPXN,GPXN10,GPYN,GPYN10,GPXL,GPYL,GPXT,
     -      LGRID,LGRALL,LOGX,LOGY,LSTAMP,LGCLRB,LGCLRA,LWAITA,LWAITB,
     -      LXCCH,LGLCLP,LGMCLP,LGACLP,LGTCLP,
     -      NWK,WKID,WKCON,WKFREF,WKLUN,WKSTAT,WKSREQ,NCWKNM,NCSTMP,
     -      IGHIST,IGBAR,NCGKS,WKMULT
       COMMON /GRACHR/ WKNAME,WKATTR,STAMP,GKSLOG
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
       CHARACTER*80 AUX,AUX1,AUX2
       CHARACTER*(*) ITEM,TYPE,COLOUR
       LOGICAL POLYL,POLYM,TEXT,AREA,EXIS,DSNCMP,EXMEMB
       INTEGER INPTYP,INPCMP,INPCMX,IKEY,IFAIL,IFAIL1,IFAIL2,I,IWKID,
     -      INEXT,NWORD,NCSTR,NITEM,NSEEN,NUPDAT,NC,NCMEMB,NCFILE,NCREM,
     -      IOS,NC1,NC2,ICOL,FSTY,FONT,PREC
       REAL WIDTH,FPAS1,FPAS2,FREF1,FREF2,EXPAN,HEIGHT,SPACE,SIZE
       EXTERNAL INPTYP,INPCMP,INPCMX,DSNCMP
*** Buffer declarations, first the sizes.
       INTEGER MXPLBU,MXPMBU,MXTXBU,MXFABU
       PARAMETER(MXPLBU=60,MXPMBU=60,MXTXBU=60,MXFABU=60)
*   PolyLine attributes.
       REAL LINWID(MXPLBU),LWR
       INTEGER LINTYP(MXPLBU),LINCOL(MXPLBU),LTR,LCR,NLIN
       CHARACTER*20 LINNAM(MXPLBU)
*   PolyMarker attributes.
       REAL MRKSIZ(MXPMBU),MSR
       INTEGER MRKTYP(MXPMBU),MRKCOL(MXPMBU),MTR,MCR,NMRK
       CHARACTER*20 MRKNAM(MXPMBU)
*   Text attributes.
       REAL TXTEXP(MXTXBU),TXTSPA(MXTXBU),TXTHGT(MXTXBU),TER,TSR,THR
       INTEGER TXTFNT(MXTXBU),TXTPRC(MXTXBU),TXTCOL(MXTXBU),TFR,TPR,TCR,
     -      NTXT
       CHARACTER*20 TXTNAM(MXTXBU)
*   Fill Area attributes.
       REAL FARPAS(2,MXFABU),FARREF(2,MXFABU),FPXR,FPYR,FRXR,FRYR
       INTEGER FARINT(MXFABU),FARSTY(MXFABU),FARCOL(MXFABU),FIR,FSR,FCR,
     -      NFAR
       CHARACTER*20 FARNAM(MXFABU)
*   Ensure the contents is kept across routine calls.
       SAVE NLIN,LINNAM,LINWID,LINTYP,LINCOL,
     -      NMRK,MRKNAM,MRKSIZ,MRKTYP,MRKCOL,
     -      NTXT,TXTNAM,TXTEXP,TXTSPA,TXTHGT,TXTFNT,TXTPRC,TXTCOL,
     -      NFAR,FARNAM,FARPAS,FARREF,FARINT,FARSTY,FARCOL
*** Initial values for the attributes, start with polyline.
       DATA NLIN /42/
       DATA (LINNAM(I),LINWID(I),LINTYP(I),LINCOL(I),I=1,42) /
     -      'AUGER-#ELECTRON     ', 1.00, 2, 1,
     -      'BOX-#TICKMARKS      ', 1.00, 1, 1,
     -      'COM#MENT            ', 1.00, 2, 1,
     -      'CON#TOUR-HIGH#LIGHT ', 1.00, 1, 1,
     -      'CON#TOUR-NORM#AL    ', 1.00, 1, 1,
     -      'DASH-DOT#TED        ', 1.00, 4, 1,
     -      'DASH#ED             ', 1.00, 2, 1,
     -      'DELTA-#ELECTRON     ', 1.00, 2, 1,
     -      'DOT#TED             ', 1.00, 3, 1,
     -      'DR#IFT-L#INE        ', 1.00, 1, 1,
     -      'E-DR#IFT-L#INE      ', 1.00, 1, 1,
     -      'ERR#OR-BAR          ', 1.00, 1, 1,
     -      'ERR#OR-BAND         ', 1.00, 1, 1,
     -      'FAT2                ', 2.00, 1, 1,
     -      'FAT3                ', 3.00, 1, 1,
     -      'FAT4                ', 4.00, 1, 1,
     -      'FAT5                ', 5.00, 1, 1,
     -      'FAT6                ', 6.00, 1, 1,
     -      'F#UNCTION-1         ', 1.00, 1, 1,
     -      'F#UNCTION-2         ', 1.00, 2, 1,
     -      'F#UNCTION-3         ', 1.00, 3, 1,
     -      'F#UNCTION-4         ', 1.00, 4, 1,
     -      'F#UNCTION-5         ', 1.00, 1, 1,
     -      'F#UNCTION-6         ', 1.00, 2, 1,
     -      'F#UNCTION-7         ', 1.00, 3, 1,
     -      'GR#ID               ', 1.00, 3, 1,
     -      'HIST#OGRAM-1        ', 1.00, 1, 1,
     -      'HIST#OGRAM-2        ', 1.00, 2, 1,
     -      'HIST#OGRAM-3        ', 1.00, 3, 1,
     -      'HIST#OGRAM-4        ', 1.00, 4, 1,
     -      'HIST#OGRAM-5        ', 1.00, 1, 1,
     -      'HIST#OGRAM-6        ', 1.00, 2, 1,
     -      'HIST#OGRAM-7        ', 1.00, 3, 1,
     -      'ION-DR#IFT-L#INE    ', 1.00, 1, 1,
     -      'ISO#CHRONS          ', 1.00, 2, 1,
     -      'OUT#LINE            ', 1.00, 1, 1,
     -      'PHOTON              ', 1.00, 3, 1,
     -      'PL#ANES             ', 1.00, 1, 1,
     -      'SOLID               ', 1.00, 1, 1,
     -      'STR#IPS             ', 3.00, 1, 1,
     -      'TR#ACK              ', 1.00, 2, 1,
     -      'TUBE                ', 1.00, 1, 1/
*   Next the polymarkers.
       DATA NMRK /27/
       DATA (MRKNAM(I),MRKSIZ(I),MRKTYP(I),MRKCOL(I),I=1,27) /
     -      'S-WIRE              ', 1.00, 4, 1,
     -      'P-WIRE              ', 1.00, 5, 1,
     -      'C-WIRE              ', 1.00, 2, 1,
     -      'OTH#ER-WIRE         ', 1.00, 3, 1,
     -      'ISO#CHRONS          ', 1.00, 3, 1,
     -      'F#UNCTION-1         ', 1.00, 3, 1,
     -      'F#UNCTION-2         ', 1.00, 4, 1,
     -      'F#UNCTION-3         ', 1.00, 2, 1,
     -      'F#UNCTION-4         ', 1.00, 1, 1,
     -      'F#UNCTION-5         ', 1.00, 3, 1,
     -      'F#UNCTION-6         ', 1.00, 4, 1,
     -      'F#UNCTION-7         ', 1.00, 2, 1,
     -      'TR#ACK              ', 1.00, 3, 1,
     -      'PHOTON              ', 1.00, 3, 1,
     -      'DELTA-#ELECTRON     ', 0.25, 4, 1,
     -      'AUGER-#ELECTRON     ', 0.25, 2, 1,
     -      'DOT                 ', 1.00, 1, 1,
     -      'PLUS                ', 1.00, 2, 1,
     -      'AST#ERISK           ', 1.00, 3, 1,
     -      'CIRC#LE             ', 1.00, 4, 1,
     -      'CR#OSS              ', 1.00, 5, 1,
     -      'EL#ASTIC            ', 1.00, 1, 1,
     -      'SUP#ER-#ELASTIC     ', 1.00, 2, 1,
     -      'INEL#ASTIC          ', 1.00, 5, 1,
     -      'EXC#ITATION         ', 1.00, 6, 1,
     -      'ION#ISATION         ', 1.00, 4, 1,
     -      'ATT#ACHMENT         ', 1.00, 3, 1/
*   Next the text.
       DATA NTXT /28/
       DATA (TXTNAM(I),TXTEXP(I),TXTSPA(I),TXTHGT(I),TXTFNT(I),
     -      TXTPRC(I),TXTCOL(I),I=1,28) /
     -      'COM#MENT            ', 1.00, 0.00, 0.013,    0, 2, 1,
     -      'CONT#OUR-#LABELS    ', 1.00, 0.00, 0.010,    0, 2, 1,
     -      'LAB#ELS             ', 1.00, 0.00, 0.025,    0, 2, 1,
     -      'F#UNCTION-1         ', 1.00, 0.00, 0.025,    0, 2, 1,
     -      'F#UNCTION-2         ', 1.00, 0.00, 0.025,    0, 2, 1,
     -      'F#UNCTION-3         ', 1.00, 0.00, 0.025,    0, 2, 1,
     -      'F#UNCTION-4         ', 1.00, 0.00, 0.025,    0, 2, 1,
     -      'F#UNCTION-5         ', 1.00, 0.00, 0.025,    0, 2, 1,
     -      'F#UNCTION-6         ', 1.00, 0.00, 0.025,    0, 2, 1,
     -      'F#UNCTION-7         ', 1.00, 0.00, 0.025,    0, 2, 1,
     -      'MES#SAGE            ', 1.00, 0.00, 0.010,    0, 2, 1,
     -      'NUM#BERS            ', 1.00, 0.00, 0.015,    0, 2, 1,
     -      'TIT#LE              ', 1.00, 0.00, 0.025,    0, 2, 1,
     -      'HIGZ-#SOFTWARE      ', 1.00, 0.00, 0.020,    0, 2, 1,
     -      'TIM#ES-RO#MAN       ', 1.00, 0.00, 0.020,  -13, 2, 1,
     -      'TIM#ES-IT#ALIC      ', 1.00, 0.00, 0.020,   -1, 2, 1,
     -      'TIM#ES-BOLD-R#OMAN  ', 1.00, 0.00, 0.020,   -2, 2, 1,
     -      'TIM#ES-BOLD-I#TALIC ', 1.00, 0.00, 0.020,   -3, 2, 1,
     -      'HELV#ETICA          ', 1.00, 0.00, 0.020,   -4, 2, 1,
     -      'HELV#ETICA-O#BLIQUE ', 1.00, 0.00, 0.020,   -5, 2, 1,
     -      'HELV#ETICA-B#OLD    ', 1.00, 0.00, 0.020,   -6, 2, 1,
     -      'HELV#ETICA-B#OLD-O#B', 1.00, 0.00, 0.020,   -7, 2, 1,
     -      'COUR#IER            ', 1.00, 0.00, 0.020,   -8, 2, 1,
     -      'COUR#IER-O#BLIQUE   ', 1.00, 0.00, 0.020,   -9, 2, 1,
     -      'COUR#IER-B#OLD      ', 1.00, 0.00, 0.020,  -10, 2, 1,
     -      'COUR#IER-B#OLD-O#BLI', 1.00, 0.00, 0.020,  -11, 2, 1,
     -      'SYM#BOL             ', 1.00, 0.00, 0.020,  -12, 2, 1,
     -      'ZAPF#DINGBATS       ', 1.00, 0.00, 0.020,  -14, 2, 1/
*   And finally the fill area.
       DATA NFAR /34/
       DATA (FARNAM(I),FARPAS(1,I),FARPAS(2,I),FARREF(1,I),FARREF(2,I),
     -      FARINT(I),FARSTY(I),FARCOL(I),I=1,34) /
     -      'COND#UCTORS-1       ', 1.00, 1.00, 0.00, 0.00, 3, 354, 1,
     -      'COND#UCTORS-2       ', 1.00, 1.00, 0.00, 0.00, 3, 354, 1,
     -      'COND#UCTORS-3       ', 1.00, 1.00, 0.00, 0.00, 3, 354, 1,
     -      'DIEL#ECTRICA-1      ', 1.00, 1.00, 0.00, 0.00, 3, 345, 1,
     -      'DIEL#ECTRICA-2      ', 1.00, 1.00, 0.00, 0.00, 3, 345, 1,
     -      'DIEL#ECTRICA-3      ', 1.00, 1.00, 0.00, 0.00, 3, 345, 1,
     -      'OUT#SIDE-AREA       ', 1.00, 1.00, 0.00, 0.00, 3, 305, 1,
     -      'PLA#NES             ', 1.00, 1.00, 0.00, 0.00, 3, 354, 1,
     -      'STR#IPS             ', 1.00, 1.00, 0.00, 0.00, 3, 304, 1,
     -      'TUBE                ', 1.00, 1.00, 0.00, 0.00, 3, 357, 1,
     -      'BAR#CHART-1         ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'BAR#CHART-2         ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'BAR#CHART-3         ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'BAR#CHART-4         ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'BAR#CHART-5         ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'BAR#CHART-6         ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'BAR#CHART-7         ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'BOX-#TICKMARKS      ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'WIR#ES              ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'ERR#OR-BAR          ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'ERR#OR-BAND         ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'LABEL               ', 1.00, 1.00, 0.00, 0.00, 3,   0, 1,
     -      'MATERIAL-1          ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'MATERIAL-2          ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'MATERIAL-3          ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'MATERIAL-4          ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'MATERIAL-5          ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'F#UNCTION-1         ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'F#UNCTION-2         ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'F#UNCTION-3         ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'F#UNCTION-4         ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'F#UNCTION-5         ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'F#UNCTION-6         ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1,
     -      'F#UNCTION-7         ', 1.00, 1.00, 0.00, 0.00, 0,   0, 1/
*** Assume the routine fails.
       IFAIL=1
*** Get the number of words.
       CALL INPNUM(NWORD)
*** Workstation id.
       IWKID=1
*** Starting values.
       LWR=-1.0
       LTR=0
       LCR=-1
       MSR=-1.0
       MTR=0
       MCR=-1
       TER=-1.0
       TSR=-1.0
       THR=-1.0
       TFR=12345678
       TPR=-1
       TCR=-1
       FPXR=-1.0
       FPYR=-1.0
       FRXR=0.0
       FRYR=0.0
       FIR=-1
       FSR=0
       FCR=-1
       POLYL=.FALSE.
       POLYM=.FALSE.
       TEXT=.FALSE.
       AREA=.FALSE.
*** Decode the parameter list.
       INEXT=IKEY+2
       DO 10 I=IKEY+2,NWORD
       IF(I.LT.INEXT)GOTO 10
*   Polyline items.
       IF(INPCMP(I,'LINET#YPE')+
     -      INPCMP(I,'LINE-T#YPE')+
     -      INPCMP(I,'POLYLINE-T#YPE')+
     -      INPCMP(I,'POLYLINET#YPE').NE.0)THEN
            POLYL=.TRUE.
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'The linetype is not specified.')
            ELSEIF(INPCMP(I+1,'SOL#ID').NE.0)THEN
                 LTR=1
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'DASH#ED').NE.0)THEN
                 LTR=2
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'DOT#TED').NE.0)THEN
                 LTR=3
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'DASH-DOT#TED').NE.0)THEN
                 LTR=4
                 INEXT=I+2
            ELSEIF(INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I+1,'Not recognised as a linetype. ')
                 INEXT=I+2
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,LTR,0)
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'LINEW#IDTH-SC#ALE-#FACTOR').NE.0)THEN
            POLYL=.TRUE.
            IF(INPTYP(I+1).LE.0.OR.I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'Value missing or not real.    ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,LWR,-1.0)
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'POLYL#INE-COL#OUR')+
     -      INPCMP(I,'L#INE-COL#OUR').NE.0)THEN
            POLYL=.TRUE.
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'The colour is not specified.  ')
            ELSE
                 CALL INPSTR(I+1,I+1,STRING,NCSTR)
                 CALL GRCOLQ(IWKID,STRING(1:NCSTR),LCR)
                 IF(LCR.LT.0)
     -                CALL INPMSG(I+1,'This colour is not known.     ')
                 INEXT=I+2
            ENDIF
*   Polymarker items.
       ELSEIF(INPCMP(I,'M#ARKER-T#YPE')+
     -      INPCMP(I,'POLYM#ARKER-T#YPE').NE.0)THEN
            POLYM=.TRUE.
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'The marker is not specified.  ')
            ELSEIF(INPCMP(I+1,'DOT').NE.0)THEN
                 MTR=1
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'PL#US').NE.0)THEN
                 MTR=2
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'AST#ERISK').NE.0)THEN
                 MTR=3
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'CIRC#LE').NE.0)THEN
                 MTR=4
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'CR#OSS').NE.0)THEN
                 MTR=5
                 INEXT=I+2
            ELSEIF(INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I+1,'Not recognised as a marker.   ')
                 INEXT=I+2
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,MTR,0)
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'M#ARKER-SIZ#E-#SCALE-#FACTOR').NE.0)THEN
            POLYM=.TRUE.
            IF(INPTYP(I+1).LE.0.OR.I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'Value missing or not real.    ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,MSR,-1.0)
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'POLYM#ARKER-COL#OUR')+
     -      INPCMP(I,'M#ARKER-COL#OUR').NE.0)THEN
            POLYM=.TRUE.
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'The colour is not specified.  ')
            ELSE
                 CALL INPSTR(I+1,I+1,STRING,NCSTR)
                 CALL GRCOLQ(IWKID,STRING(1:NCSTR),MCR)
                 IF(MCR.LT.0)
     -                CALL INPMSG(I+1,'This colour is not known.     ')
                 INEXT=I+2
            ENDIF
*   Text items.
       ELSEIF(INPCMP(I,'CH#ARACTER-EXP#ANSION-#FACTOR').NE.0)THEN
            TEXT=.TRUE.
            IF(INPTYP(I+1).LE.0.OR.I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'Value missing or not real.    ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,TER,-1.0)
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'CH#ARACTER-SP#ACING').NE.0)THEN
            TEXT=.TRUE.
            IF(INPTYP(I+1).LE.0.OR.I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'Value missing or not real.    ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,TSR,-1.0)
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'CH#ARACTER-H#EIGHT').NE.0)THEN
            TEXT=.TRUE.
            IF(INPTYP(I+1).LE.0.OR.I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'Value missing or not real.    ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,THR,-1.0)
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'T#EXT-F#ONT').NE.0)THEN
            TEXT=.TRUE.
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'Value missing.')
            ELSEIF(INPCMP(I+1,'HIGZ-#SOFTWARE').NE.0)THEN
                 TFR=0
            ELSEIF(INPCMP(I+1,'T#IMES-I#TALIC').NE.0)THEN
                 TFR=-1
            ELSEIF(INPCMP(I+1,'T#IMES-B#OLD').NE.0)THEN
                 TFR=-2
            ELSEIF(INPCMP(I+1,'T#IMES-B#OLD-I#TALIC').NE.0)THEN
                 TFR=-3
            ELSEIF(INPCMP(I+1,'HELV#ETICA').NE.0)THEN
                 TFR=-4
            ELSEIF(INPCMP(I+1,'HELV#ETICA-O#BLIQUE').NE.0)THEN
                 TFR=-5
            ELSEIF(INPCMP(I+1,'HELV#ETICA-B#OLD').NE.0)THEN
                 TFR=-6
            ELSEIF(INPCMP(I+1,'HELV#ETICA-B#OLD-O#BLIQUE').NE.0)THEN
                 TFR=-7
            ELSEIF(INPCMP(I+1,'C#OURIER').NE.0)THEN
                 TFR=-8
            ELSEIF(INPCMP(I+1,'C#OURIER-O#BLIQUE').NE.0)THEN
                 TFR=-9
            ELSEIF(INPCMP(I+1,'C#OURIER-B#OLD').NE.0)THEN
                 TFR=-10
            ELSEIF(INPCMP(I+1,'C#OURIER-B#OLD-O#BLIQUE').NE.0)THEN
                 TFR=-11
            ELSEIF(INPCMP(I+1,'S#YMBOL').NE.0)THEN
                 TFR=-12
            ELSEIF(INPCMP(I+1,'T#IMES-R#OMAN').NE.0)THEN
                 TFR=-13
            ELSEIF(INPCMP(I+1,'ZAPF-#DINGBAT').NE.0)THEN
                 TFR=-14
            ELSEIF(INPCMP(I+1,'HO#LLOW-T#IMES-I#TALIC').NE.0)THEN
                 TFR=-15
            ELSEIF(INPCMP(I+1,'HO#LLOW-T#IMES-B#OLD').NE.0)THEN
                 TFR=-16
            ELSEIF(INPCMP(I+1,'HO#LLOW-T#IMES-B#OLD-I#TALIC').NE.0)THEN
                 TFR=-17
            ELSEIF(INPCMP(I+1,'HO#LLOW-HELV#ETICA').NE.0)THEN
                 TFR=-18
            ELSEIF(INPCMP(I+1,'HO#LLOW-HELV#ETICA-O#BLIQUE').NE.0)THEN
                 TFR=-19
            ELSEIF(INPCMP(I+1,'HO#LLOW-HELV#ETICA-B#OLD').NE.0)THEN
                 TFR=-20
            ELSEIF(INPCMP(I+1,'HO#LLOW-HELV#ETICA-B#OLD-O#BLIQUE').NE.
     -           0)THEN
                 TFR=-21
            ELSEIF(INPCMP(I+1,'HO#LLOW-S#YMBOL').NE.0)THEN
                 TFR=-22
            ELSEIF(INPCMP(I+1,'HO#LLOW-T#IMES-R#OMAN').NE.0)THEN
                 TFR=-23
            ELSEIF(INPCMP(I+1,'HO#LLOW-ZAPF-#DINGBAT').NE.0)THEN
                 TFR=-24
            ELSEIF(INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Value unknown.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,TFR,-1)
            ENDIF
            INEXT=I+2
       ELSEIF(INPCMP(I,'T#EXT-PR#ECISION').NE.0)THEN
            TEXT=.TRUE.
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'Character quality missing.    ')
            ELSEIF(INPCMP(I+1,'STRI#NG')+INPCMP(I+1,'LOW').NE.0)THEN
                 TPR=0
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'CH#ARACTER')+
     -           INPCMP(I+1,'MED#IUM').NE.0)THEN
                 TPR=1
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'STRO#KE')+INPCMP(I+1,'HIGH').NE.0)THEN
                 TPR=2
                 INEXT=I+2
            ELSE
                 CALL INPMSG(I,'Not in STRING/CHARACTER/STROKE')
            ENDIF
       ELSEIF(INPCMP(I,'T#EXT-COL#OUR').NE.0)THEN
            TEXT=.TRUE.
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'The colour is not specified.  ')
            ELSE
                 CALL INPSTR(I+1,I+1,STRING,NCSTR)
                 CALL GRCOLQ(IWKID,STRING(1:NCSTR),TCR)
                 IF(TCR.LT.0)
     -                CALL INPMSG(I+1,'This colour is not known.     ')
                 INEXT=I+2
            ENDIF
*   Fill area items.
       ELSEIF(INPCMP(I,'F#ILL-A#REA-INT#ERIOR-#STYLE').NE.0)THEN
            AREA=.TRUE.
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'Interior style missing.       ')
            ELSEIF(INPCMP(I+1,'HOLL#OW').NE.0)THEN
                 FIR=0
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'SOL#ID').NE.0)THEN
                 FIR=1
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'PATT#ERN').NE.0)THEN
                 FIR=2
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'HAT#CHED').NE.0)THEN
                 FIR=3
                 INEXT=I+2
            ELSE
                 CALL INPMSG(I+1,'Not HOLLOW/SOLID/PATTERN/HATCH')
            ENDIF
       ELSEIF(INPCMP(I,'F#ILL-A#REA-ST#YLE-#INDEX').NE.0)THEN
            AREA=.TRUE.
            IF(INPTYP(I+1).LE.0.OR.I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'Value missing or not integer. ')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,FSR,0)
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'F#ILL-A#REA-COL#OUR').NE.0)THEN
            AREA=.TRUE.
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'The colour is not specified.  ')
            ELSE
                 CALL INPSTR(I+1,I+1,STRING,NCSTR)
                 CALL GRCOLQ(IWKID,STRING(1:NCSTR),FCR)
                 IF(FCR.LT.0)
     -                CALL INPMSG(I+1,'This colour is not known.     ')
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'PA#TTERN-SIZ#E').NE.0)THEN
            AREA=.TRUE.
            IF(INPTYP(I+1).LE.0.OR.INPTYP(I+2).LE.0.OR.
     -           I+2.GT.NWORD)THEN
                 CALL INPMSG(I,'Values missing or not real.   ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,FPXR,-1.0)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+2,FPYR,-1.0)
                 INEXT=I+3
            ENDIF
       ELSEIF(INPCMP(I,'PA#TTERN-REF#ERENCE-#POINT').NE.0)THEN
            AREA=.TRUE.
            IF(INPTYP(I+1).LE.0.OR.INPTYP(I+2).LE.0.OR.
     -           I+2.GT.NWORD)THEN
                 CALL INPMSG(I,'Values missing or not real.   ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,FRXR,-1.0)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+2,FRYR,-1.0)
                 INEXT=I+3
            ENDIF
*   Unknown item.
       ELSE
            CALL INPMSG(I,'Not a known item.             ')
       ENDIF
10     CONTINUE
*** Dump the error messages.
       CALL INPERR
*** Check whether conflicting items were presented.
       NITEM=0
       IF(POLYL)NITEM=NITEM+1
       IF(POLYM)NITEM=NITEM+1
       IF(TEXT)NITEM=NITEM+1
       IF(AREA)NITEM=NITEM+1
       IF(NITEM.GT.1)THEN
            PRINT *,' ###### GRATTR ERROR   : Items belonging to more'//
     -           ' than one primitive seen ; command not processed.'
            RETURN
       ELSEIF(NITEM.EQ.0.AND.IKEY+1.LT.NWORD)THEN
            PRINT *,' ###### GRATTR ERROR   : Invalid attributes'//
     -           ' seen ; neither inquiry nor update performed.'
            RETURN
       ENDIF
*** Loop over the items, start with the polylines.
       NUPDAT=0
       NSEEN=0
       DO 20 I=1,NLIN
       IF(IKEY.EQ.NWORD.OR.INPCMP(IKEY+1,LINNAM(I))+
     -      INPCMP(IKEY+1,'!'//LINNAM(I)).NE.0)THEN
            NSEEN=NSEEN+1
            IF(IKEY+1.GE.NWORD)THEN
                 CALL INPFIX(LINNAM(I),AUX,NC)
                 WRITE(LUNOUT,'(/''  Current representation of the'',
     -                '' polyline item '',A,'':''/)') AUX(1:NC)
                 IF(LINTYP(I).EQ.1)THEN
                      AUX='Solid (--------)'
                 ELSEIF(LINTYP(I).EQ.2)THEN
                      AUX='Dashed (- - - - )'
                 ELSEIF(LINTYP(I).EQ.3)THEN
                      AUX='Dotted (........)'
                 ELSEIF(LINTYP(I).EQ.4)THEN
                      AUX='Dash-dotted (-.-.-.-.)'
                 ELSE
                      WRITE(AUX,'(I10)') LINTYP(I)
                 ENDIF
                 WRITE(LUNOUT,'(''  Linetype:                 '',A)')
     -                AUX(1:25)
                 CALL OUTFMT(LINWID(I),2,AUX,NC,'LEFT')
                 WRITE(LUNOUT,'(''  Linewidth scale factor:   '',A)')
     -                AUX(1:NC)
                 CALL GRCOLD(IWKID,LINCOL(I),AUX,NC,'FORMATTED')
                 WRITE(LUNOUT,'(''  Polyline colour:          '',A)')
     -                AUX(1:NC)
                 WRITE(LUNOUT,'('' '')')
            ELSEIF(POLYL)THEN
                 NUPDAT=NUPDAT+1
                 IF(LTR.NE.0)LINTYP(I)=LTR
                 IF(LWR.GT.0.0)LINWID(I)=LWR
                 IF(LCR.GE.0)LINCOL(I)=LCR
            ENDIF
       ENDIF
20     CONTINUE
*** Next the polymarkers.
       DO 30 I=1,NMRK
       IF(IKEY.EQ.NWORD.OR.INPCMP(IKEY+1,MRKNAM(I))+
     -      INPCMP(IKEY+1,'!'//MRKNAM(I)).NE.0)THEN
            NSEEN=NSEEN+1
            IF(IKEY+1.GE.NWORD)THEN
                 CALL INPFIX(MRKNAM(I),AUX,NC)
                 WRITE(LUNOUT,'(/''  Current representation of the'',
     -                '' polymarker item '',A,'':''/)') AUX(1:NC)
                 IF(MRKTYP(I).EQ.1)THEN
                      AUX='Dot (.)'
                 ELSEIF(MRKTYP(I).EQ.2)THEN
                      AUX='Plus (+)'
                 ELSEIF(MRKTYP(I).EQ.3)THEN
                      AUX='Asterisk (*)'
                 ELSEIF(MRKTYP(I).EQ.4)THEN
                      AUX='Circle (o)'
                 ELSEIF(MRKTYP(I).EQ.5)THEN
                      AUX='Cross (x)'
                 ELSE
                      WRITE(AUX,'(I10)') MRKTYP(I)
                 ENDIF
                 WRITE(LUNOUT,'(''  Marker type:              '',A)')
     -                AUX(1:20)
                 CALL OUTFMT(MRKSIZ(I),2,AUX,NC,'LEFT')
                 WRITE(LUNOUT,'(''  Marker size scale factor: '',A)')
     -                AUX(1:NC)
                 CALL GRCOLD(IWKID,MRKCOL(I),AUX,NC,'FORMATTED')
                 WRITE(LUNOUT,'(''  Polymarker colour:        '',A/)')
     -                AUX(1:NC)
            ELSEIF(POLYM)THEN
                 NUPDAT=NUPDAT+1
                 IF(MTR.NE.0)MRKTYP(I)=MTR
                 IF(MSR.GT.0.0)MRKSIZ(I)=MSR
                 IF(MCR.GE.0)MRKCOL(I)=MCR
            ENDIF
       ENDIF
30     CONTINUE
*** Next the text.
       DO 40 I=1,NTXT
       IF(IKEY.EQ.NWORD.OR.INPCMP(IKEY+1,TXTNAM(I))+
     -      INPCMP(IKEY+1,'!'//TXTNAM(I)).NE.0)THEN
            NSEEN=NSEEN+1
            IF(IKEY+1.GE.NWORD)THEN
                 CALL INPFIX(TXTNAM(I),AUX,NC)
                 WRITE(LUNOUT,'(/''  Current representation of the'',
     -                '' text item '',A,'':''/)') AUX(1:NC)
                 CALL OUTFMT(REAL(TXTFNT(I)),2,AUX,NC,'LEFT')
                 WRITE(LUNOUT,'(''  Text font:                '',A)')
     -                AUX(1:NC)
                 IF(TXTPRC(I).EQ.0)THEN
                      AUX='String (low quality)'
                 ELSEIF(TXTPRC(I).EQ.1)THEN
                      AUX='Character (medium quality)'
                 ELSEIF(TXTPRC(I).EQ.2)THEN
                      AUX='Stroke (high quality)'
                 ELSE
                      WRITE(AUX,'(''# Invalid: '',I10)') TXTPRC(I)
                 ENDIF
                 WRITE(LUNOUT,'(''  Text precision:           '',A)')
     -                AUX(1:30)
                 CALL OUTFMT(TXTEXP(I),2,AUX,NC,'LEFT')
                 WRITE(LUNOUT,'(''  Character expansion:      '',A)')
     -                AUX(1:NC)
                 CALL OUTFMT(TXTHGT(I),2,AUX,NC,'LEFT')
                 WRITE(LUNOUT,'(''  Character height:         '',A)')
     -                AUX(1:NC)
                 CALL OUTFMT(TXTSPA(I),2,AUX,NC,'LEFT')
                 WRITE(LUNOUT,'(''  Character spacing:        '',A)')
     -                AUX(1:NC)
                 CALL GRCOLD(IWKID,TXTCOL(I),AUX,NC,'FORMATTED')
                 WRITE(LUNOUT,'(''  Text colour:              '',A/)')
     -                AUX(1:NC)
            ELSEIF(TEXT)THEN
                 NUPDAT=NUPDAT+1
                 IF(TER.GT.0.0)TXTEXP(I)=TER
                 IF(TSR.GE.0.0)TXTSPA(I)=TSR
                 IF(THR.GT.0.0)TXTHGT(I)=THR
                 IF(TPR.GE.0)TXTPRC(I)=TPR
                 IF(TFR.NE.12345678)TXTFNT(I)=TFR
                 IF(TCR.GE.0)TXTCOL(I)=TCR
            ENDIF
       ENDIF
40     CONTINUE
*** Next the fill area.
       DO 50 I=1,NFAR
       IF(IKEY.EQ.NWORD.OR.INPCMP(IKEY+1,FARNAM(I))+
     -      INPCMP(IKEY+1,'!'//FARNAM(I)).NE.0)THEN
            NSEEN=NSEEN+1
            IF(IKEY+1.GE.NWORD)THEN
                 CALL INPFIX(FARNAM(I),AUX,NC)
                 WRITE(LUNOUT,'(/''  Current representation of the'',
     -                '' fill area item '',A,'':''/)') AUX(1:NC)
                 IF(FARINT(I).EQ.0)THEN
                      AUX='Hollow (boundaries only)'
                 ELSEIF(FARINT(I).EQ.1)THEN
                      AUX='Solid (area filled with colour)'
                 ELSEIF(FARINT(I).EQ.2)THEN
                      AUX='Pattern (area filled with pattern)'
                 ELSEIF(FARINT(I).EQ.3)THEN
                      AUX='Hatch (area hatched)'
                 ELSE
                      CALL OUTFMT(REAL(FARINT(I)),2,AUX1,NC1,'LEFT')
                      AUX='# Invalid: '//AUX1(1:NC1)
                 ENDIF
                 WRITE(LUNOUT,'(''  Fill area interior style: '',A)')
     -                AUX(1:40)
                 IF(FARINT(I).EQ.2.OR.FARINT(I).EQ.3)THEN
                      CALL OUTFMT(REAL(FARSTY(I)),2,AUX,NC,'LEFT')
                      WRITE(LUNOUT,'(''  Fill area style index:    '',
     -                     A)') AUX(1:NC)
                 ENDIF
                 IF(FARINT(I).EQ.2)THEN
                      CALL OUTFMT(FARPAS(1,I),2,AUX1,NC1,'LEFT')
                      CALL OUTFMT(FARPAS(2,I),2,AUX2,NC2,'LEFT')
                      WRITE(LUNOUT,'(''  Fill area pattern sizes:  ('',
     -                     A,'','',A,'')'')') AUX1(1:NC1),AUX2(1:NC2)
                      CALL OUTFMT(FARREF(1,I),2,AUX1,NC1,'LEFT')
                      CALL OUTFMT(FARREF(2,I),2,AUX2,NC2,'LEFT')
                      WRITE(LUNOUT,'(''  Fill area reference:      ('',
     -                     A,'','',A,'')'')') AUX1(1:NC1),AUX2(1:NC2)
                 ENDIF
                 CALL GRCOLD(IWKID,FARCOL(I),AUX,NC,'FORMATTED')
                 WRITE(LUNOUT,'(''  Fill area colour:         '',A/)')
     -                AUX(1:NC)
            ELSEIF(AREA)THEN
                 NUPDAT=NUPDAT+1
                 IF(FPXR.GT.0.0)FARPAS(1,I)=FPXR
                 IF(FPYR.GT.0.0)FARPAS(2,I)=FPYR
                 FARREF(1,I)=FRXR
                 FARREF(2,I)=FRYR
                 IF(FIR.GE.0)FARINT(I)=FIR
                 IF(FSR.NE.0)FARSTY(I)=FSR
                 IF(FCR.GE.0)FARCOL(I)=FCR
            ENDIF
       ENDIF
50     CONTINUE
*** Check that an item was found.
       CALL INPSTR(IKEY+1,IKEY+1,STRING,NC)
       IF(NC.LE.0)THEN
            STRING='# Unable to read #'
            NC=18
       ENDIF
       IF(NSEEN.EQ.0)THEN
            PRINT *,' !!!!!! GRATTR WARNING : '//STRING(1:NC)//' is'//
     -           ' not a known item.'
       ELSEIF(NITEM.GT.0.AND.NUPDAT.EQ.0)THEN
            PRINT *,' !!!!!! GRATTR WARNING : The representation of '//
     -           STRING(1:NC)//' is left unaltered since'
            PRINT *,'                         the attributes you'//
     -           ' specified are not of the proper type.'
       ELSE
            IFAIL=0
       ENDIF
       RETURN
*** Secondary entry to set the proper attributes.
       ENTRY GRATTS(ITEM,TYPE)
       NSEEN=0
*   Scan the list of polyline items if appropriate.
       IF(TYPE.EQ.'POLYLINE')THEN
            DO 110 I=1,NLIN
            IF(INPCMX(ITEM,LINNAM(I)).EQ.0)GOTO 110
            NSEEN=NSEEN+1
            CALL GSLN(LINTYP(I))
            CALL GSLWSC(LINWID(I))
            CALL GSPLCI(LINCOL(I))
110         CONTINUE
*   The list of polymarker items.
       ELSEIF(TYPE.EQ.'POLYMARKER')THEN
            DO 120 I=1,NMRK
            IF(INPCMX(ITEM,MRKNAM(I)).EQ.0)GOTO 120
            NSEEN=NSEEN+1
            CALL GSMK(MRKTYP(I))
            CALL GSMKSC(MRKSIZ(I))
            CALL GSPMCI(MRKCOL(I))
120         CONTINUE
*   The list of text items.
       ELSEIF(TYPE.EQ.'TEXT')THEN
            DO 130 I=1,NTXT
            IF(INPCMX(ITEM,TXTNAM(I)).EQ.0)GOTO 130
            NSEEN=NSEEN+1
            CALL GSTXFP(TXTFNT(I),TXTPRC(I))
            CALL GSCHXP(TXTEXP(I))
            CALL GSCHSP(TXTSPA(I))
            CALL GSCHH(TXTHGT(I))
            CALL GSTXCI(TXTCOL(I))
130         CONTINUE
*   The list of fill area items.
       ELSEIF(TYPE.EQ.'AREA')THEN
            DO 140 I=1,NFAR
            IF(INPCMX(ITEM,FARNAM(I)).EQ.0)GOTO 140
            NSEEN=NSEEN+1
            CALL GSFAIS(FARINT(I))
            IF(FARINT(I).EQ.2.OR.FARINT(I).EQ.3)
     -           CALL GSFASI(FARSTY(I))
            CALL GSPA(FARPAS(1,I),FARPAS(2,I))
            CALL GSPARF(FARREF(1,I),FARREF(2,I))
            CALL GSFACI(FARCOL(I))
140         CONTINUE
*   Anything else: invalid.
       ELSE
            WRITE (10,'(''  ###### GRATTS ERROR   : Invalid primitive'',
     -           '' type '',A,'' received; program bug.'')') TYPE
            RETURN
       ENDIF
*** Make sure the item has been found.
       IF(NSEEN.EQ.0)THEN
            WRITE (10,'(''  !!!!!! GRATTS ERROR   : Unknown item '',A,
     -           '' received; no update.'')') ITEM
            RETURN
       ENDIF
       RETURN
*** Write the settings to a file.
       ENTRY GRATTW(IKEY,IFAIL)
*   Initial settings.
       FILE=' '
       NCFILE=1
       MEMBER='< none >'
       NCMEMB=8
       REMARK='none'
       NCREM=4
       IFAIL=1
       IWKID=1
*   First decode the argument string.
       CALL INPNUM(NWORD)
*   Make sure there is at least one argument.
       IF(NWORD.EQ.IKEY)THEN
            PRINT *,' !!!!!! GRATTW WARNING : WRITE takes at least one',
     -           ' argument (a dataset name); data will not be written.'
            RETURN
*   Check whether keywords have been used.
       ELSEIF(INPCMP(IKEY+1,'D#ATASET')+
     -      INPCMP(IKEY+1,'R#EMARK').NE.0)THEN
            INEXT=2
            DO 210 I=IKEY+1,NWORD
            IF(I.LT.INEXT)GOTO 210
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
210         CONTINUE
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
       IF(NCFILE.GT.MXNAME)PRINT *,' !!!!!! GRATTW WARNING : The file',
     -      ' name is truncated to MXNAME (=',MXNAME,') characters.'
       IF(NCMEMB.GT.8)PRINT *,' !!!!!! GRATTW WARNING : The member',
     -      ' name is shortened to ',MEMBER,', first 8 characters.'
       IF(NCREM.GT.29)PRINT *,' !!!!!! GRATTW WARNING : The remark',
     -      ' shortened to ',REMARK,', first 29 characters.'
       NCFILE=MIN(NCFILE,MXNAME)
       NCMEMB=MIN(NCMEMB,8)
       NCREM=MIN(NCREM,29)
*   Check whether the member already exists.
       CALL DSNREM(FILE(1:NCFILE),MEMBER(1:NCMEMB),'GRAPHREP',EXMEMB)
       IF(JEXMEM.EQ.2.AND.EXMEMB)THEN
            PRINT *,' ------ GRATTW MESSAGE : A copy of the member'//
     -           ' exists; new member will be appended.'
       ELSEIF(JEXMEM.EQ.3.AND.EXMEMB)THEN
            PRINT *,' !!!!!! GRATTW WARNING : A copy of the member'//
     -           ' exists already; member will not be written.'
            RETURN
       ENDIF
*   Print some debugging output if requested.
       IF(LDEBUG)THEN
            PRINT *,' ++++++ GRATTW DEBUG   : File= ',FILE(1:NCFILE),
     -           ', member= ',MEMBER(1:NCMEMB)
            PRINT *,'                         Remark= ',REMARK(1:NCREM)
       ENDIF
**  Open the dataset for sequential write and inform DSNLOG.
       CALL DSNOPN(FILE,NCFILE,12,'WRITE-LIBRARY',IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! GRATTW WARNING : Opening ',FILE(1:NCFILE),
     -              ' failed ; the data will not be written.'
            RETURN
       ENDIF
       CALL DSNLOG(FILE,'Graphics  ','Sequential','Write     ')
       IF(LDEBUG)PRINT *,' ++++++ GRATTW DEBUG   : Dataset ',
     -      FILE(1:NCFILE),' opened on unit 12 for seq write.'
*   Now write a heading record to the file.
       CALL DATTIM(DATE,TIME)
       WRITE(STRING,'(''% Created '',A8,'' At '',A8,1X,A8,'' GRAPHREP'',
     -      1X,''"'',A29,''"'')') DATE,TIME,MEMBER,REMARK
       WRITE(12,'(A80)',IOSTAT=IOS,ERR=2010) STRING
       IF(LDEBUG)THEN
            PRINT *,' ++++++ GRATTW DEBUG   : Dataset heading record:'
            PRINT *,STRING
       ENDIF
*   Information line about the graphics system beging used.
       WRITE(12,'('' GKS flavour: MGKS'')',ERR=2010,IOSTAT=IOS)
*   Write the actual data, start with the number of items of each type.
       WRITE(12,'('' NLIN='',I3,'', NMRK='',I3,'', NTXT='',I3,
     -      '', NFAR='',I3)',ERR=2010,IOSTAT=IOS) NLIN,NMRK,NTXT,NFAR
*   Next a list of Polyline attributes.
       DO 230 I=1,NLIN
       CALL GRCOLD(IWKID,LINCOL(I),AUX,NC,'RAW')
       WRITE(12,'(A20,I10,E15.8,A20)',ERR=2010,IOSTAT=IOS)
     -      LINNAM(I),LINTYP(I),LINWID(I),AUX(1:20)
230    CONTINUE
*   Next a list of Polymarker attributes.
       DO 240 I=1,NMRK
       CALL GRCOLD(IWKID,MRKCOL(I),AUX,NC,'RAW')
       WRITE(12,'(A20,I10,E15.8,A20)',ERR=2010,IOSTAT=IOS)
     -      MRKNAM(I),MRKTYP(I),MRKSIZ(I),AUX(1:20)
240    CONTINUE
*   Next a list of Text attributes.
       DO 250 I=1,NTXT
       CALL GRCOLD(IWKID,TXTCOL(I),AUX,NC,'RAW')
       WRITE(12,'(A20,2I10,3E15.8,A20)',ERR=2010,IOSTAT=IOS)
     -      TXTNAM(I),TXTFNT(I),TXTPRC(I),TXTEXP(I),TXTSPA(I),
     -      TXTHGT(I),AUX(1:20)
250    CONTINUE
*   Next a list of Fill Area attributes.
       DO 260 I=1,NFAR
       CALL GRCOLD(IWKID,FARCOL(I),AUX,NC,'RAW')
       WRITE(12,'(A20,2I10,4E15.8,A20)',ERR=2010,IOSTAT=IOS)
     -      FARNAM(I),FARINT(I),FARSTY(I),FARPAS(1,I),FARPAS(2,I),
     -      FARREF(1,I),FARREF(2,I),AUX(1:20)
260    CONTINUE
**  Close the file after the operation.
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       CALL TIMLOG('Writing out graphics representations:   ')
       IFAIL=0
       RETURN
*** Read the presentation from dataset.
       ENTRY GRATTG(IKEY,IFAIL)
*   Initial values.
       FILE=' '
       MEMBER='*'
       NCFILE=8
       NCMEMB=1
       IFAIL=1
       IWKID=1
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
            PRINT *,' !!!!!! GRATTG WARNING : The file name is'//
     -           ' truncated to MXNAME (=',MXNAME,') characters.'
            NCFILE=MIN(NCFILE,MXNAME)
       ENDIF
       IF(NCMEMB.GT.8)THEN
            PRINT *,' !!!!!! GRATTG WARNING : The member name is'//
     -           ' shortened to ',MEMBER,', first 8 characters.'
            NCMEMB=MIN(NCMEMB,8)
       ELSEIF(NCMEMB.LE.0)THEN
            PRINT *,' !!!!!! GRATTG WARNING : The member'//
     -           ' name has zero length, replaced by "*".'
            MEMBER='*'
            NCMEMB=1
       ENDIF
*   Reject the empty file name case.
       IF(FILE.EQ.' '.OR.NWORD.EQ.1)THEN
            PRINT *,' !!!!!! GRATTG WARNING : GET must be at least'//
     -           ' followed by a dataset name ; no data are read.'
            RETURN
       ENDIF
*   If there are even more args, warn they are ignored.
       IF(NWORD.GT.IKEY+2)PRINT *,' !!!!!! GRATTG WARNING : GET takes'//
     -     ' at most two arguments (dataset and member); rest ignored.'
**  Open the dataset and inform DSNLOG.
       CALL DSNOPN(FILE,NCFILE,12,'READ-LIBRARY',IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! GRATTG WARNING : Opening ',FILE(1:NCFILE),
     -           ' failed ; graphics representation data are not read.'
            RETURN
       ENDIF
       CALL DSNLOG(FILE,'Graphics  ','Sequential','Read only ')
       IF(LDEBUG)PRINT *,' ++++++ GRATTG DEBUG   : Dataset',
     -      FILE(1:NCFILE),' opened on unit 12 for seq read.'
*   Locate the pointer on the header of the requested member.
       CALL DSNLOC(MEMBER,NCMEMB,'GRAPHREP',12,EXIS,'RESPECT')
       IF(.NOT.EXIS)THEN
            CALL DSNLOC(MEMBER,NCMEMB,'GRAPHREP',12,EXIS,'IGNORE')
            IF(EXIS)THEN
                 PRINT *,' ###### GRATTG ERROR   : Graphics data ',
     -                MEMBER(1:NCMEMB),' has been deleted from ',
     -                FILE(1:NCFILE),'; not read.'
            ELSE
                 PRINT *,' ###### GRATTG ERROR   : Graphics data ',
     -                MEMBER(1:NCMEMB),' not found on ',FILE(1:NCFILE)
            ENDIF
            CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
**  Check that the member is acceptable date wise.
       READ(12,'(A80)',END=2000,IOSTAT=IOS,ERR=2010) STRING
       IF(LDEBUG)THEN
            PRINT *,' ++++++ GRATTG DEBUG   : Dataset header',
     -              ' record follows:'
            PRINT *,STRING
       ENDIF
       IF(DSNCMP('14-07-89',STRING(11:18)))THEN
            PRINT *,' !!!!!! GRATTG WARNING : Member ',STRING(32:39),
     -           ' can not be read because of a change in format.'
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
       WRITE(LUNOUT,'(''  Member '',A8,'' was created on '',A8,
     -      '' at '',A8/''  Remarks: '',A29)')
     -      STRING(32:39),STRING(11:18),STRING(23:30),STRING(51:79)
**  Carry out the actual reading, check the GKS flavour.
       READ(12,'(A80)',END=2000,ERR=2010,IOSTAT=IOS) AUX
       IF(AUX(15:30).NE.'MGKS            ')PRINT *,' !!!!!! GRATTG'//
     -      ' WARNING : This member was created with another GKS than'//
     -      ' the one you are running with now.'
*   Read the actual data, start with the number of items of each type.
       READ(12,'(6X,I3,7X,I3,7X,I3,7X,I3)',END=2000,ERR=2010,
     -      IOSTAT=IOS) NLIN,NMRK,NTXT,NFAR
*   Make sure none of these exceeds the maximum numbers.
       IF(NLIN.GT.MXPLBU.OR.NMRK.GT.MXPMBU.OR.NTXT.GT.MXTXBU.OR.
     -      NFAR.GT.MXFABU)THEN
            PRINT *,' !!!!!! GRATTG WARNING : The number of items'//
     -           ' for one or more atributes, exceeds'
            PRINT *,'                         the compilation maxima;'//
     -           ' increase these and recompile.'
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
*   Next a list of Polyline attributes.
       DO 330 I=1,NLIN
       READ(12,'(A20,I10,E15.8,A20)',END=2000,ERR=2010,IOSTAT=IOS)
     -      LINNAM(I),LINTYP(I),LINWID(I),AUX(1:20)
       CALL GRCOLQ(IWKID,AUX(1:20),LCR)
       IF(LCR.GE.0)THEN
            LINCOL(I)=LCR
       ELSE
            PRINT *,' !!!!!! GRATTG WARNING : The member contains a'//
     -           ' colour absent in the colour tables: '//AUX(1:20)
            CALL INPFIX(LINNAM(I),AUX,NC)
            PRINT *,'                         The FOREGROUND colour'//
     -           ' will be used to represent item '//AUX(1:NC)//'.'
            LINCOL(I)=1
       ENDIF
330    CONTINUE
*   Next a list of Polymarker attributes.
       DO 340 I=1,NMRK
       READ(12,'(A20,I10,E15.8,A20)',END=2000,ERR=2010,IOSTAT=IOS)
     -      MRKNAM(I),MRKTYP(I),MRKSIZ(I),AUX(1:20)
       CALL GRCOLQ(IWKID,AUX(1:20),MCR)
       IF(MCR.GE.0)THEN
            MRKCOL(I)=MCR
       ELSE
            PRINT *,' !!!!!! GRATTG WARNING : The member contains a'//
     -           ' colour absent in the colour tables: '//AUX(1:20)
            CALL INPFIX(MRKNAM(I),AUX,NC)
            PRINT *,'                         The FOREGROUND colour'//
     -           ' will be used to represent item '//AUX(1:NC)//'.'
            MRKCOL(I)=1
       ENDIF
340    CONTINUE
*   Next a list of Text attributes.
       DO 350 I=1,NTXT
       READ(12,'(A20,2I10,3E15.8,A20)',END=2000,ERR=2010,IOSTAT=IOS)
     -      TXTNAM(I),TXTFNT(I),TXTPRC(I),TXTEXP(I),TXTSPA(I),
     -      TXTHGT(I),AUX(1:20)
       CALL GRCOLQ(IWKID,AUX(1:20),TCR)
       IF(TCR.GE.0)THEN
            TXTCOL(I)=TCR
       ELSE
            PRINT *,' !!!!!! GRATTG WARNING : The member contains a'//
     -           ' colour absent in the colour tables: '//AUX(1:20)
            CALL INPFIX(TXTNAM(I),AUX,NC)
            PRINT *,'                         The FOREGROUND colour'//
     -           ' will be used to represent item '//AUX(1:NC)//'.'
            TXTCOL(I)=1
       ENDIF
350    CONTINUE
*   Next a list of Fill Area attributes.
       DO 360 I=1,NFAR
       READ(12,'(A20,2I10,4E15.8,A20)',END=2000,ERR=2010,IOSTAT=IOS)
     -      FARNAM(I),FARINT(I),FARSTY(I),FARPAS(1,I),FARPAS(2,I),
     -      FARREF(1,I),FARREF(2,I),AUX(1:20)
       CALL GRCOLQ(IWKID,AUX(1:20),FCR)
       IF(FCR.GE.0)THEN
            FARCOL(I)=FCR
       ELSE
            PRINT *,' !!!!!! GRATTG WARNING : The member contains a'//
     -           ' colour absent in the colour tables: '//AUX(1:20)
            CALL INPFIX(FARNAM(I),AUX,NC)
            PRINT *,'                         The FOREGROUND colour'//
     -           ' will be used to represent item '//AUX(1:NC)//'.'
            FARCOL(I)=1
       ENDIF
360    CONTINUE
**  Close the file after the operation.
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       CALL TIMLOG('Reading in graphics representations:    ')
       IFAIL=0
       RETURN
*** Polyline update.
       ENTRY GRATTL(ITEM,TYPE,WIDTH,COLOUR)
       NSEEN=0
       DO 410 I=1,NLIN
       IF(INPCMX(ITEM,LINNAM(I)).EQ.0)GOTO 410
       NSEEN=NSEEN+1
*   Line type
       IF(TYPE.EQ.' ')THEN
            LTR=0
       ELSEIF(INPCMX(TYPE,'SOL#ID').NE.0)THEN
            LTR=1
       ELSEIF(INPCMX(TYPE,'DASH#ED').NE.0)THEN
            LTR=2
       ELSEIF(INPCMX(TYPE,'DOT#TED').NE.0)THEN
            LTR=3
       ELSEIF(INPCMX(TYPE,'DASH-DOT#TED').NE.0)THEN
            LTR=4
       ELSE
            WRITE (10,'(''  !!!!!! GRATTL ERROR   : Line type '',A,
     -           '' not known; no update.'')') TYPE
            LTR=0
       ENDIF
       IF(LTR.GT.0)LINTYP(I)=LTR
*   Line width scale factor
       IF(WIDTH.GT.0)LINWID(I)=WIDTH
*   Line colour
       IF(COLOUR.NE.' ')THEN
            CALL GRCOLQ(1,COLOUR,ICOL)
            IF(ICOL.GT.0)THEN
                 LINCOL(I)=ICOL
            ELSE
                 WRITE (10,'(''  !!!!!! GRATTL ERROR   : Colour '',A,
     -                '' not known, attribute not set.'')') COLOUR
            ENDIF
       ENDIF
410    CONTINUE
*   Check the thing has been found.
       IF(NSEEN.EQ.0)THEN
            WRITE (10,'(''  !!!!!! GRATTL ERROR   : Unknown item '',A,
     -           '' received; no update.'')') ITEM
            RETURN
       ENDIF
       RETURN
*** Fill area update.
       ENTRY GRATTA(ITEM,TYPE,FSTY,FPAS1,FPAS2,FREF1,FREF2,COLOUR)
       NSEEN=0
       DO 420 I=1,NFAR
       IF(INPCMX(ITEM,FARNAM(I)).EQ.0)GOTO 420
       NSEEN=NSEEN+1
*   Area type
       IF(TYPE.EQ.' ')THEN
            FIR=0
       ELSEIF(INPCMX(TYPE,'HOLL#OW').NE.0)THEN
            FIR=0
       ELSEIF(INPCMX(TYPE,'SOL#ID').NE.0)THEN
            FIR=1
       ELSEIF(INPCMX(TYPE,'PATT#ERN').NE.0)THEN
            FIR=2
       ELSEIF(INPCMX(TYPE,'HAT#CHED').NE.0)THEN
            FIR=3
       ELSE
            WRITE (10,'(''  !!!!!! GRATTA ERROR   : Area type '',A,
     -           '' not known; no update.'')') TYPE
            FIR=0
       ENDIF
       IF(FIR.GT.0)FARINT(I)=FIR
*   Area style index
       IF(FSTY.GT.0)FARSTY(I)=FSTY
*   Area pattern style
       IF(FPAS1.GT.0)FARPAS(1,I)=FPAS1
       IF(FPAS2.GT.0)FARPAS(2,I)=FPAS2
*   Area pattern reference
       FARREF(1,I)=FREF1
       FARREF(2,I)=FREF2
*   Area colour
       IF(COLOUR.NE.' ')THEN
            CALL GRCOLQ(1,COLOUR,ICOL)
            IF(ICOL.GT.0)THEN
                 FARCOL(I)=ICOL
            ELSE
                 WRITE (10,'(''  !!!!!! GRATTA ERROR   : Colour '',A,
     -                '' not known, attribute not set.'')') COLOUR
            ENDIF
       ENDIF
420    CONTINUE
*   Check the thing has been found.
       IF(NSEEN.EQ.0)THEN
            WRITE (10,'(''  !!!!!! GRATTA ERROR   : Unknown item '',A,
     -           '' received; no update.'')') ITEM
            RETURN
       ENDIF
       RETURN
*** Text update.
       ENTRY GRATTT(ITEM,FONT,PREC,EXPAN,HEIGHT,SPACE,COLOUR)
       NSEEN=0
       DO 430 I=1,NTXT
       IF(INPCMX(ITEM,TXTNAM(I)).EQ.0)GOTO 430
       NSEEN=NSEEN+1
*   Text font
       IF(FONT.GT.-1)TXTFNT(I)=FONT
*   Text precision
       IF(PREC.GT.-1)TXTPRC(I)=PREC
*   Text expansion
       IF(EXPAN.GT.0.0)TXTEXP(I)=EXPAN
*   Text height
       IF(HEIGHT.GT.0.0)TXTHGT(I)=HEIGHT
*   Text spacing
       IF(SPACE.GT.0.0)TXTSPA(I)=SPACE
*   Text colour
       IF(COLOUR.NE.' ')THEN
            CALL GRCOLQ(1,COLOUR,ICOL)
            IF(ICOL.GT.0)THEN
                 TXTCOL(I)=ICOL
            ELSE
                 WRITE (10,'(''  !!!!!! GRATTT ERROR   : Colour '',A,
     -                '' not known, attribute not set.'')') COLOUR
            ENDIF
       ENDIF
430    CONTINUE
*   Check the thing has been found.
       IF(NSEEN.EQ.0)THEN
            WRITE (10,'(''  !!!!!! GRATTT ERROR   : Unknown item '',A,
     -           '' received; no update.'')') ITEM
            RETURN
       ENDIF
       RETURN
*** Marker update.
       ENTRY GRATTM(ITEM,TYPE,SIZE,COLOUR)
       NSEEN=0
       DO 440 I=1,NMRK
       IF(INPCMX(ITEM,MRKNAM(I)).EQ.0)GOTO 440
       NSEEN=NSEEN+1
*   Marker type
       IF(TYPE.EQ.' ')THEN
            MTR=0
       ELSEIF(INPCMX(TYPE,'DOT').NE.0)THEN
            MTR=1
       ELSEIF(INPCMX(TYPE,'PL#US').NE.0)THEN
            MTR=2
       ELSEIF(INPCMX(TYPE,'AST#ERISK').NE.0)THEN
            MTR=3
       ELSEIF(INPCMX(TYPE,'CIRC#LE').NE.0)THEN
            MTR=4
       ELSEIF(INPCMX(TYPE,'CR#OSS').NE.0)THEN
            MTR=5
       ELSE
            WRITE (10,'(''  !!!!!! GRATTA ERROR   : Marker type '',A,
     -           '' not known; no update.'')') TYPE
            MTR=0
       ENDIF
       IF(MTR.GT.0)MRKTYP=MTR
*   Marker size
       IF(SIZE.GT.0.0)MRKSIZ(I)=SIZE
*   Marker colour
       IF(COLOUR.NE.' ')THEN
            CALL GRCOLQ(1,COLOUR,ICOL)
            IF(ICOL.GT.0)THEN
                 MRKCOL(I)=ICOL
            ELSE
                 WRITE (10,'(''  !!!!!! GRATTT ERROR   : Colour '',A,
     -                '' not known, attribute not set.'')') COLOUR
            ENDIF
       ENDIF
440    CONTINUE
*   Check the thing has been found.
       IF(NSEEN.EQ.0)THEN
            WRITE (10,'(''  !!!!!! GRATTT ERROR   : Unknown item '',A,
     -           '' received; no update.'')') ITEM
            RETURN
       ENDIF
       RETURN
*** Handle the error conditions.
2000   CONTINUE
       PRINT *,' ###### GRATTG ERROR   : Premature EOF ecountered on '//
     -      FILE(1:NCFILE)//' read via unit 12 ; no valid data read.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2010   CONTINUE
       PRINT *,' ###### GRATTW ERROR   : I/O error accessing '//
     -      FILE(1:NCFILE)//' via unit 12 ; no data read or written.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2030   CONTINUE
       PRINT *,' ###### GRATTW ERROR   : Dataset '//FILE(1:NCFILE)//
     -      ' unit 12 cannot be closed ; results not predictable'
       CALL INPIOS(IOS)
       END
