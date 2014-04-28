CDECK  ID>, GRTXHIGZ.
       SUBROUTINE GRTX(X,Y,STRING)
*-----------------------------------------------------------------------
*   GRTX   - Calls ITX, version for HIGZ.
*   (Last changed on 24/ 1/12.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       CHARACTER*(*) STRING
       CHARACTER*256 STROUT
       LOGICAL UNIT
       INTEGER NOUT,INEXT,I
       REAL X,Y
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE GRTX (HIGZ version) ///'
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GRTX   DEBUG   : In:  "'',A,
     -      ''",'')') STRING
*** Do not process empty strings.
       IF(LEN(STRING).LT.1)RETURN
*** Simply copy the string if control characters are to be executed.
       IF(LXCCH)THEN
            NOUT=MIN(256,LEN(STRING))
            STROUT=STRING
*** Convert the control characters in the string if requested.
       ELSE
            NOUT=0
            UNIT=.FALSE.
*   Loop over the string.
            INEXT=1
            DO 10 I=1,LEN(STRING)
*   Skip a few characters.
            IF(I.LT.INEXT)GOTO 10
*   Check for excessive length.
            IF(NOUT+9.GT.256)GOTO 20
*   Fix SGML controls.
            IF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'<SUB>'.OR.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'<sub>')THEN
                 STROUT(NOUT+1:NOUT+1)='?'
                 INEXT=I+5
                 NOUT=NOUT+1
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'<SUP>'.OR.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'<sup>')THEN
                 STROUT(NOUT+1:NOUT+1)='^'
                 INEXT=I+5
                 NOUT=NOUT+1
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'</SUB>'.OR.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'</sub>')THEN
                 STROUT(NOUT+1:NOUT+1)='!'
                 INEXT=I+6
                 NOUT=NOUT+1
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'</SUP>'.OR.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'</sup>')THEN
                 STROUT(NOUT+1:NOUT+1)='!'
                 INEXT=I+6
                 NOUT=NOUT+1
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'<BACK>'.OR.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'<back>')THEN
                 STROUT(NOUT+1:NOUT+1)='&'
                 INEXT=I+6
                 NOUT=NOUT+1
*   Fix a series of control characters.
            ELSEIF(STRING(I:I).EQ.'|')THEN
                 STROUT(NOUT+1:NOUT+3)='"B#'
                 NOUT=NOUT+3
            ELSEIF(STRING(I:I).EQ.'$')THEN
                 STROUT(NOUT+1:NOUT+3)='"D#'
                 NOUT=NOUT+3
            ELSEIF(STRING(I:I).EQ.'!')THEN
                 STROUT(NOUT+1:NOUT+3)='"E#'
                 NOUT=NOUT+3
            ELSEIF(STRING(I:I).EQ.'#')THEN
                 STROUT(NOUT+1:NOUT+3)='"F#'
                 NOUT=NOUT+3
            ELSEIF(STRING(I:I).EQ.'>')THEN
                 STROUT(NOUT+1:NOUT+3)='"G#'
                 NOUT=NOUT+3
            ELSEIF(STRING(I:I).EQ.'?')THEN
                 STROUT(NOUT+1:NOUT+3)='"H#'
                 NOUT=NOUT+3
            ELSEIF(STRING(I:I).EQ.':')THEN
                 STROUT(NOUT+1:NOUT+3)='"J#'
                 NOUT=NOUT+3
            ELSEIF(STRING(I:I).EQ.'<')THEN
                 STROUT(NOUT+1:NOUT+3)='"L#'
                 NOUT=NOUT+3
            ELSEIF(STRING(I:I).EQ.'[')THEN
                 STROUT(NOUT+1:NOUT+3)='"M#'
                 UNIT=.TRUE.
                 NOUT=NOUT+3
            ELSEIF(STRING(I:I).EQ.']')THEN
                 STROUT(NOUT+1:NOUT+3)='"N#'
                 UNIT=.FALSE.
                 NOUT=NOUT+3
            ELSEIF(STRING(I:I).EQ.'{')THEN
                 STROUT(NOUT+1:NOUT+3)='"P#'
                 NOUT=NOUT+3
            ELSEIF(STRING(I:I).EQ.'}')THEN
                 STROUT(NOUT+1:NOUT+3)='"Q#'
                 NOUT=NOUT+3
            ELSEIF(STRING(I:I).EQ.'%')THEN
                 STROUT(NOUT+1:NOUT+3)='"Y#'
                 NOUT=NOUT+3
            ELSEIF(STRING(I:I).EQ.'''')THEN
                 STROUT(NOUT+1:NOUT+5)='"<9>#'
                 NOUT=NOUT+5
            ELSEIF(STRING(I:I).EQ.'"')THEN
                 STROUT(NOUT+1:NOUT+6)='"<99>#'
                 NOUT=NOUT+6
*   SGML entities, first accented letters "a" and "A".
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&aacute;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\366'
                 INEXT=I+8
                 NOUT=NOUT+4
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&Aacute;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\367'
                 INEXT=I+8
                 NOUT=NOUT+4
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&acirc;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\276'
                 INEXT=I+7
                 NOUT=NOUT+4
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&Acirc;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\300'
                 INEXT=I+7
                 NOUT=NOUT+4
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&agrave;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\260'
                 INEXT=I+8
                 NOUT=NOUT+4
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&Agrave;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\265'
                 INEXT=I+8
                 NOUT=NOUT+4
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&aring;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\357'
                 INEXT=I+7
                 NOUT=NOUT+4
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&Aring;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\362'
                 INEXT=I+7
                 NOUT=NOUT+4
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&auml;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\311'
                 INEXT=I+6
                 NOUT=NOUT+4
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&Auml;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\314'
                 INEXT=I+6
                 NOUT=NOUT+4
*   Accented letters "c" and "C".
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&ccedil;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\321'
                 INEXT=I+8
                 NOUT=NOUT+4
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&Ccedil;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\322'
                 INEXT=I+8
                 NOUT=NOUT+4
*   Accented letters "e" and "E".
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&eacute;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\323'
                 INEXT=I+8
                 NOUT=NOUT+4
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&Eacute;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\324'
                 INEXT=I+8
                 NOUT=NOUT+4
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&ecirc;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\327'
                 INEXT=I+7
                 NOUT=NOUT+4
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&Ecirc;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\330'
                 INEXT=I+7
                 NOUT=NOUT+4
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&egrave;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\325'
                 INEXT=I+8
                 NOUT=NOUT+4
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&Egrave;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\326'
                 INEXT=I+8
                 NOUT=NOUT+4
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&euml;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\331'
                 INEXT=I+6
                 NOUT=NOUT+4
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&Euml;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\332'
                 INEXT=I+6
                 NOUT=NOUT+4
*   Accented letters "i" and "I".
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&icirc;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\333'
                 INEXT=I+7
                 NOUT=NOUT+4
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&Icirc;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\334'
                 INEXT=I+7
                 NOUT=NOUT+4
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&iuml;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\335'
                 INEXT=I+6
                 NOUT=NOUT+4
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&Iuml;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\336'
                 INEXT=I+6
                 NOUT=NOUT+4
*   Accented letters "l" and "L".
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&lstrok;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\370'
                 INEXT=I+8
                 NOUT=NOUT+4
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&Lstrok;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\350'
                 INEXT=I+8
                 NOUT=NOUT+4
*   Accented letters "n" and "N".
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&ntilde;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\337'
                 INEXT=I+8
                 NOUT=NOUT+4
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&Ntilde;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\340'
                 INEXT=I+8
                 NOUT=NOUT+4
*   Accented letters "o" and "O".
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&ocirc;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\342'
                 INEXT=I+7
                 NOUT=NOUT+4
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&Ocirc;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\344'
                 INEXT=I+7
                 NOUT=NOUT+4
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&oslash;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\371'
                 INEXT=I+8
                 NOUT=NOUT+4
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&Oslash;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\351'
                 INEXT=I+8
                 NOUT=NOUT+4
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&ouml;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\345'
                 INEXT=I+6
                 NOUT=NOUT+4
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&Ouml;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\346'
                 INEXT=I+6
                 NOUT=NOUT+4
*   Accented letters "u" and "U".
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&ucirc;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\347'
                 INEXT=I+7
                 NOUT=NOUT+4
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&Ucirc;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\354'
                 INEXT=I+7
                 NOUT=NOUT+4
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&ugrave;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\374'
                 INEXT=I+8
                 NOUT=NOUT+4
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&Ugrave;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\375'
                 INEXT=I+8
                 NOUT=NOUT+4
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&uuml;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\355'
                 INEXT=I+6
                 NOUT=NOUT+4
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&Uuml;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\356'
                 INEXT=I+6
                 NOUT=NOUT+4
*   Ligatures.
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&aelig;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\361'
                 INEXT=I+7
                 NOUT=NOUT+4
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&AElig;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\341'
                 INEXT=I+7
                 NOUT=NOUT+4
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&filig;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\256'
                 INEXT=I+7
                 NOUT=NOUT+4
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&fllig;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\257'
                 INEXT=I+7
                 NOUT=NOUT+4
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&oelig;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\372'
                 INEXT=I+7
                 NOUT=NOUT+4
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&OElig;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\352'
                 INEXT=I+7
                 NOUT=NOUT+4
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&szlig;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\373'
                 INEXT=I+7
                 NOUT=NOUT+4
*   Lower case Greek characters.
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&alpha;')THEN
                 STROUT(NOUT+1:NOUT+3)='[a]'
                 INEXT=I+7
                 NOUT=NOUT+3
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&beta;')THEN
                 STROUT(NOUT+1:NOUT+3)='[b]'
                 INEXT=I+6
                 NOUT=NOUT+3
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'&eta;')THEN
                 STROUT(NOUT+1:NOUT+3)='[c]'
                 INEXT=I+5
                 NOUT=NOUT+3
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&delta;')THEN
                 STROUT(NOUT+1:NOUT+3)='[d]'
                 INEXT=I+7
                 NOUT=NOUT+3
            ELSEIF(I+8.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+8)).EQ.'&epsilon;')THEN
                 STROUT(NOUT+1:NOUT+3)='[e]'
                 INEXT=I+9
                 NOUT=NOUT+3
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'&phi;')THEN
                 STROUT(NOUT+1:NOUT+3)='[f]'
                 INEXT=I+5
                 NOUT=NOUT+3
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&gamma;')THEN
                 STROUT(NOUT+1:NOUT+3)='[g]'
                 INEXT=I+7
                 NOUT=NOUT+3
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'&chi;')THEN
                 STROUT(NOUT+1:NOUT+3)='[h]'
                 INEXT=I+5
                 NOUT=NOUT+3
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&iota;')THEN
                 STROUT(NOUT+1:NOUT+3)='[i]'
                 INEXT=I+6
                 NOUT=NOUT+3
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&kappa;')THEN
                 STROUT(NOUT+1:NOUT+3)='[k]'
                 INEXT=I+7
                 NOUT=NOUT+3
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&lambda;')THEN
                 STROUT(NOUT+1:NOUT+3)='[l]'
                 INEXT=I+8
                 NOUT=NOUT+3
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'&mu;')THEN
                 STROUT(NOUT+1:NOUT+3)='[m]'
                 INEXT=I+4
                 NOUT=NOUT+3
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'&nu;')THEN
                 STROUT(NOUT+1:NOUT+3)='[n]'
                 INEXT=I+4
                 NOUT=NOUT+3
            ELSEIF(I+8.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+8)).EQ.'&omicron;')THEN
                 STROUT(NOUT+1:NOUT+3)='[o]'
                 INEXT=I+9
                 NOUT=NOUT+3
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'&pi;')THEN
                 STROUT(NOUT+1:NOUT+3)='[p]'
                 INEXT=I+4
                 NOUT=NOUT+3
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&theta;')THEN
                 STROUT(NOUT+1:NOUT+3)='[q]'
                 INEXT=I+7
                 NOUT=NOUT+3
            ELSEIF(I+9.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+9)).EQ.'&thetasym;')THEN
                 STROUT(NOUT+1:NOUT+6)='[\\112]'
                 INEXT=I+10
                 NOUT=NOUT+6
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'&rho;')THEN
                 STROUT(NOUT+1:NOUT+3)='[r]'
                 INEXT=I+5
                 NOUT=NOUT+3
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&sigma;')THEN
                 STROUT(NOUT+1:NOUT+3)='[s]'
                 INEXT=I+7
                 NOUT=NOUT+3
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&sigmaf;')THEN
                 STROUT(NOUT+1:NOUT+6)='[\\126]'
                 INEXT=I+8
                 NOUT=NOUT+6
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'&tau;')THEN
                 STROUT(NOUT+1:NOUT+3)='[t]'
                 INEXT=I+5
                 NOUT=NOUT+3
            ELSEIF(I+8.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+8)).EQ.'&upsilon;')THEN
                 STROUT(NOUT+1:NOUT+3)='[u]'
                 INEXT=I+9
                 NOUT=NOUT+3
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&omega;')THEN
                 STROUT(NOUT+1:NOUT+3)='[w]'
                 INEXT=I+7
                 NOUT=NOUT+3
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'&ksi;')THEN
                 STROUT(NOUT+1:NOUT+3)='[x]'
                 INEXT=I+5
                 NOUT=NOUT+3
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'&xi;')THEN
                 STROUT(NOUT+1:NOUT+3)='[x]'
                 INEXT=I+4
                 NOUT=NOUT+3
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'&psi;')THEN
                 STROUT(NOUT+1:NOUT+3)='[y]'
                 INEXT=I+5
                 NOUT=NOUT+3
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&zeta;')THEN
                 STROUT(NOUT+1:NOUT+3)='[z]'
                 INEXT=I+6
                 NOUT=NOUT+3
*   Upper case Greek characters.
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&Alpha;')THEN
                 STROUT(NOUT+1:NOUT+3)='[A]'
                 INEXT=I+7
                 NOUT=NOUT+3
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&Beta;')THEN
                 STROUT(NOUT+1:NOUT+3)='[B]'
                 INEXT=I+6
                 NOUT=NOUT+3
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'&Eta;')THEN
                 STROUT(NOUT+1:NOUT+3)='[E]'
                 INEXT=I+5
                 NOUT=NOUT+3
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&Delta;')THEN
                 STROUT(NOUT+1:NOUT+3)='[D]'
                 INEXT=I+7
                 NOUT=NOUT+3
            ELSEIF(I+8.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+8)).EQ.'&Epsilon;')THEN
                 STROUT(NOUT+1:NOUT+3)='[E]'
                 INEXT=I+9
                 NOUT=NOUT+3
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'&Phi;')THEN
                 STROUT(NOUT+1:NOUT+3)='[F]'
                 INEXT=I+5
                 NOUT=NOUT+3
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&Gamma;')THEN
                 STROUT(NOUT+1:NOUT+3)='[G]'
                 INEXT=I+7
                 NOUT=NOUT+3
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'&Chi;')THEN
                 STROUT(NOUT+1:NOUT+3)='[H]'
                 INEXT=I+5
                 NOUT=NOUT+3
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&Iota;')THEN
                 STROUT(NOUT+1:NOUT+3)='[I]'
                 INEXT=I+6
                 NOUT=NOUT+3
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&Kappa;')THEN
                 STROUT(NOUT+1:NOUT+3)='[K]'
                 INEXT=I+7
                 NOUT=NOUT+3
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&Lambda;')THEN
                 STROUT(NOUT+1:NOUT+3)='[L]'
                 INEXT=I+8
                 NOUT=NOUT+3
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'&Mu;')THEN
                 STROUT(NOUT+1:NOUT+3)='[M]'
                 INEXT=I+4
                 NOUT=NOUT+3
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'&Nu;')THEN
                 STROUT(NOUT+1:NOUT+3)='[N]'
                 INEXT=I+4
                 NOUT=NOUT+3
            ELSEIF(I+8.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+8)).EQ.'&Omicron;')THEN
                 STROUT(NOUT+1:NOUT+3)='[O]'
                 INEXT=I+9
                 NOUT=NOUT+3
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'&Pi;')THEN
                 STROUT(NOUT+1:NOUT+3)='[P]'
                 INEXT=I+4
                 NOUT=NOUT+3
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&Theta;')THEN
                 STROUT(NOUT+1:NOUT+3)='[Q]'
                 INEXT=I+7
                 NOUT=NOUT+3
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'&Rho;')THEN
                 STROUT(NOUT+1:NOUT+3)='[R]'
                 INEXT=I+5
                 NOUT=NOUT+3
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&Sigma;')THEN
                 STROUT(NOUT+1:NOUT+3)='[S]'
                 INEXT=I+7
                 NOUT=NOUT+3
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'&Tau;')THEN
                 STROUT(NOUT+1:NOUT+3)='[T]'
                 INEXT=I+5
                 NOUT=NOUT+3
            ELSEIF(I+8.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+8)).EQ.'&Upsilon;')THEN
                 STROUT(NOUT+1:NOUT+3)='[U]'
                 INEXT=I+9
                 NOUT=NOUT+3
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&Omega;')THEN
                 STROUT(NOUT+1:NOUT+3)='[W]'
                 INEXT=I+7
                 NOUT=NOUT+3
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'&Ksi;')THEN
                 STROUT(NOUT+1:NOUT+3)='[X]'
                 INEXT=I+5
                 NOUT=NOUT+3
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'&Xi;')THEN
                 STROUT(NOUT+1:NOUT+3)='[X]'
                 INEXT=I+4
                 NOUT=NOUT+3
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'&Psi;')THEN
                 STROUT(NOUT+1:NOUT+3)='[Y]'
                 INEXT=I+5
                 NOUT=NOUT+3
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&Zeta;')THEN
                 STROUT(NOUT+1:NOUT+3)='[Z]'
                 INEXT=I+6
                 NOUT=NOUT+3
*   Some special symbols.
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'&amp;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\046'
                 INEXT=I+5
                 NOUT=NOUT+4
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&commat;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\100'
                 INEXT=I+8
                 NOUT=NOUT+4
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&copy;')THEN
                 STROUT(NOUT+1:NOUT+6)='[\\323]'
                 INEXT=I+6
                 NOUT=NOUT+6
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&darr;')THEN
                 STROUT(NOUT+1:NOUT+6)='"\\257#'
                 INEXT=I+6
                 NOUT=NOUT+6
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'&deg;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\312'
                 INEXT=I+5
                 NOUT=NOUT+4
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&dollar;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\044'
                 INEXT=I+8
                 NOUT=NOUT+4
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&euro;')THEN
                 STROUT(NOUT+1:NOUT+6)='[\\360]'
                 INEXT=I+6
                 NOUT=NOUT+6
            ELSEIF(I+1.LE.LEN(STRING).AND.
     -           (STRING(I:MIN(LEN(STRING),I+1)).EQ.'>='.OR.
     -            STRING(I:MIN(LEN(STRING),I+1)).EQ.'=>'))THEN
                 STROUT(NOUT+1:NOUT+3)='"O#'
                 INEXT=I+2
                 NOUT=NOUT+3
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'&ge;')THEN
                 STROUT(NOUT+1:NOUT+6)='"\\263#'
                 INEXT=I+4
                 NOUT=NOUT+6
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'&gt;')THEN
                 STROUT(NOUT+1:NOUT+6)='"\\076#'
                 INEXT=I+4
                 NOUT=NOUT+6
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&harr;')THEN
                 STROUT(NOUT+1:NOUT+6)='"\\071#'
                 INEXT=I+6
                 NOUT=NOUT+6
            ELSEIF(I+8.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+8)).EQ.'_integral')THEN
                 STROUT(NOUT+1:NOUT+3)='"I#'
                 INEXT=I+9
                 NOUT=NOUT+3
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'&int;')THEN
                 STROUT(NOUT+1:NOUT+6)='"\\111#'
                 INEXT=I+5
                 NOUT=NOUT+6
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&larr;')THEN
                 STROUT(NOUT+1:NOUT+6)='"\\067#'
                 INEXT=I+6
                 NOUT=NOUT+6
            ELSEIF(I+1.LE.LEN(STRING).AND.
     -           (STRING(I:MIN(LEN(STRING),I+1)).EQ.'<='.OR.
     -            STRING(I:MIN(LEN(STRING),I+1)).EQ.'=<'))THEN
                 STROUT(NOUT+1:NOUT+3)='"o#'
                 INEXT=I+2
                 NOUT=NOUT+3
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'&le;')THEN
                 STROUT(NOUT+1:NOUT+6)='"\\243#'
                 INEXT=I+4
                 NOUT=NOUT+6
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&lsqb;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\133'
                 INEXT=I+6
                 NOUT=NOUT+4
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'&lt;')THEN
                 STROUT(NOUT+1:NOUT+6)='"\\074#'
                 INEXT=I+4
                 NOUT=NOUT+6
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'&num;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\043'
                 INEXT=I+5
                 NOUT=NOUT+4
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&percnt;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\045'
                 INEXT=I+8
                 NOUT=NOUT+4
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&permil;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\275'
                 INEXT=I+8
                 NOUT=NOUT+4
            ELSEIF(I+2.LE.LEN(STRING).AND.
     -           (STRING(I:MIN(LEN(STRING),I+2)).EQ.'_+-'.OR.
     -            STRING(I:MIN(LEN(STRING),I+2)).EQ.'_pm'))THEN
                 STROUT(NOUT+1:NOUT+3)='"A#'
                 INEXT=I+3
                 NOUT=NOUT+3
            ELSEIF(I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&plusmn;')THEN
                 STROUT(NOUT+1:NOUT+6)='"\\261#'
                 INEXT=I+8
                 NOUT=NOUT+6
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&rarr;')THEN
                 STROUT(NOUT+1:NOUT+6)='"\\065#'
                 INEXT=I+6
                 NOUT=NOUT+6
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'&reg;')THEN
                 STROUT(NOUT+1:NOUT+8)='^[\\322]!'
                 INEXT=I+5
                 NOUT=NOUT+8
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&rsqb;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\135'
                 INEXT=I+6
                 NOUT=NOUT+4
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&radic;')THEN
                 STROUT(NOUT+1:NOUT+6)='"\\122#'
                 INEXT=I+7
                 NOUT=NOUT+6
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&sqrt;')THEN
                 STROUT(NOUT+1:NOUT+6)='"\\122#'
                 INEXT=I+6
                 NOUT=NOUT+6
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&trade;')THEN
                 STROUT(NOUT+1:NOUT+8)='^[\\324]!'
                 INEXT=I+7
                 NOUT=NOUT+8
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+8)).EQ.'&partial;')THEN
                 STROUT(NOUT+1:NOUT+6)='"\\144#'
                 INEXT=I+9
                 NOUT=NOUT+6
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&part;')THEN
                 STROUT(NOUT+1:NOUT+6)='"\\144#'
                 INEXT=I+6
                 NOUT=NOUT+6
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'&approx;')THEN
                 STROUT(NOUT+1:NOUT+6)='"\\145#'
                 INEXT=I+8
                 NOUT=NOUT+6
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&asymp;')THEN
                 STROUT(NOUT+1:NOUT+6)='"\\145#'
                 INEXT=I+7
                 NOUT=NOUT+6
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'_sum')THEN
                 STROUT(NOUT+1:NOUT+3)='[S]'
                 INEXT=I+4
                 NOUT=NOUT+3
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&times;')THEN
                 STROUT(NOUT+1:NOUT+6)='"\\264#'
                 INEXT=I+7
                 NOUT=NOUT+6
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&uarr;')THEN
                 STROUT(NOUT+1:NOUT+6)='"\\255#'
                 INEXT=I+6
                 NOUT=NOUT+6
*   Punctuation and accents.
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&excl;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\041'
                 INEXT=I+6
                 NOUT=NOUT+4
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'&apos;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\047'
                 INEXT=I+6
                 NOUT=NOUT+4
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&grave;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\301'
                 INEXT=I+7
                 NOUT=NOUT+4
            ELSEIF(I+6.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+6)).EQ.'&acute;')THEN
                 STROUT(NOUT+1:NOUT+4)='\\302'
                 INEXT=I+7
                 NOUT=NOUT+4
*   Particle names.
            ELSEIF(I+8.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+8)).EQ.'electron-')THEN
                 STROUT(NOUT+1:NOUT+4)='e^-!'
                 INEXT=I+9
                 NOUT=NOUT+4
            ELSEIF(I+8.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+8)).EQ.'electron+')THEN
                 STROUT(NOUT+1:NOUT+4)='e^+!'
                 INEXT=I+9
                 NOUT=NOUT+4
            ELSEIF(I+2.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+2)).EQ.'mu-')THEN
                 STROUT(NOUT+1:NOUT+6)='[m]^-!'
                 INEXT=I+3
                 NOUT=NOUT+6
            ELSEIF(I+2.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+2)).EQ.'mu+')THEN
                 STROUT(NOUT+1:NOUT+6)='[m]^+!'
                 INEXT=I+3
                 NOUT=NOUT+6
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'tau-')THEN
                 STROUT(NOUT+1:NOUT+6)='[t]^-!'
                 INEXT=I+4
                 NOUT=NOUT+6
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'tau+')THEN
                 STROUT(NOUT+1:NOUT+6)='[t]^+!'
                 INEXT=I+4
                 NOUT=NOUT+6
            ELSEIF(I+2.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+2)).EQ.'pi-')THEN
                 STROUT(NOUT+1:NOUT+6)='[p]^-!'
                 INEXT=I+3
                 NOUT=NOUT+6
            ELSEIF(I+2.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+2)).EQ.'pi0')THEN
                 STROUT(NOUT+1:NOUT+6)='[p]^0!'
                 INEXT=I+3
                 NOUT=NOUT+6
            ELSEIF(I+2.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+2)).EQ.'pi+')THEN
                 STROUT(NOUT+1:NOUT+6)='[p]^+!'
                 INEXT=I+3
                 NOUT=NOUT+6
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'photon')THEN
                 STROUT(NOUT+1:NOUT+3)='[g]'
                 INEXT=I+7
                 NOUT=NOUT+3
*   Names of chemical compounds.
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'He-3')THEN
                 STROUT(NOUT+1:NOUT+5)='^3!He'
                 INEXT=I+4
                 NOUT=NOUT+5
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'He-4')THEN
                 STROUT(NOUT+1:NOUT+5)='^4!He'
                 INEXT=I+4
                 NOUT=NOUT+5
            ELSEIF(I+2.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+2)).EQ.'CO2')THEN
                 STROUT(NOUT+1:NOUT+5)='CO?2!'
                 INEXT=I+3
                 NOUT=NOUT+5
            ELSEIF(I+2.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+2)).EQ.'CS2')THEN
                 STROUT(NOUT+1:NOUT+5)='CS?2!'
                 INEXT=I+3
                 NOUT=NOUT+5
            ELSEIF(I+2.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+2)).EQ.'CH4')THEN
                 STROUT(NOUT+1:NOUT+5)='CH?4!'
                 INEXT=I+3
                 NOUT=NOUT+5
            ELSEIF(I+2.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+2)).EQ.'CD4')THEN
                 STROUT(NOUT+1:NOUT+5)='CD?4!'
                 INEXT=I+3
                 NOUT=NOUT+5
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'CH3OH')THEN
                 STROUT(NOUT+1:NOUT+7)='CH?3!OH'
                 INEXT=I+5
                 NOUT=NOUT+7
            ELSEIF(I+2.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+2)).EQ.'CF4')THEN
                 STROUT(NOUT+1:NOUT+5)='CF?4!'
                 INEXT=I+3
                 NOUT=NOUT+5
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'CF3Br')THEN
                 STROUT(NOUT+1:NOUT+7)='CF?3!Br'
                 INEXT=I+5
                 NOUT=NOUT+7
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'CHF3')THEN
                 STROUT(NOUT+1:NOUT+6)='CHF?3!'
                 INEXT=I+4
                 NOUT=NOUT+6
            ELSEIF(I+2.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+2)).EQ.'BF3')THEN
                 STROUT(NOUT+1:NOUT+5)='BF?3!'
                 INEXT=I+3
                 NOUT=NOUT+5
            ELSEIF(I+2.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+2)).EQ.'SF6')THEN
                 STROUT(NOUT+1:NOUT+5)='SF?6!'
                 INEXT=I+3
                 NOUT=NOUT+5
            ELSEIF(I+2.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+2)).EQ.'H2O')THEN
                 STROUT(NOUT+1:NOUT+5)='H?2!O'
                 INEXT=I+3
                 NOUT=NOUT+5
            ELSEIF(I+2.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+2)).EQ.'N2O')THEN
                 STROUT(NOUT+1:NOUT+5)='N?2!O'
                 INEXT=I+3
                 NOUT=NOUT+5
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'C2H6')THEN
                 STROUT(NOUT+1:NOUT+8)='C?2!H?6!'
                 INEXT=I+4
                 NOUT=NOUT+8
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'C2F6')THEN
                 STROUT(NOUT+1:NOUT+8)='C?2!F?6!'
                 INEXT=I+4
                 NOUT=NOUT+8
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'C2F4H2')THEN
                 STROUT(NOUT+1:NOUT+12)='C?2!F?4!H?2!'
                 INEXT=I+6
                 NOUT=NOUT+12
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'C2H2F4')THEN
                 STROUT(NOUT+1:NOUT+12)='C?2!H?2!F?4!'
                 INEXT=I+6
                 NOUT=NOUT+12
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'C2F5H')THEN
                 STROUT(NOUT+1:NOUT+9)='C?2!F?5!H'
                 INEXT=I+5
                 NOUT=NOUT+9
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'C2HF5')THEN
                 STROUT(NOUT+1:NOUT+9)='C?2!HF?5!'
                 INEXT=I+5
                 NOUT=NOUT+9
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'C2H5OH')THEN
                 STROUT(NOUT+1:NOUT+10)='C?2!H?5!OH'
                 INEXT=I+6
                 NOUT=NOUT+10
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'C2H4')THEN
                 STROUT(NOUT+1:NOUT+8)='C?2!H?4!'
                 INEXT=I+4
                 NOUT=NOUT+8
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'C2H2')THEN
                 STROUT(NOUT+1:NOUT+8)='C?2!H?2!'
                 INEXT=I+4
                 NOUT=NOUT+8
            ELSEIF(I+3.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+3)).EQ.'C3H8')THEN
                 STROUT(NOUT+1:NOUT+8)='C?3!H?8!'
                 INEXT=I+4
                 NOUT=NOUT+8
            ELSEIF(I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'C3H7OH')THEN
                 STROUT(NOUT+1:NOUT+10)='C?3!H?7!OH'
                 INEXT=I+6
                 NOUT=NOUT+10
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'C4H10')THEN
                 STROUT(NOUT+1:NOUT+9)='C?4!H?10!'
                 INEXT=I+5
                 NOUT=NOUT+9
            ELSEIF(I+4.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+4)).EQ.'C5H12')THEN
                 STROUT(NOUT+1:NOUT+9)='C?5!H?12!'
                 INEXT=I+5
                 NOUT=NOUT+9
            ELSEIF(I+1.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+1)).EQ.'O2')THEN
                 STROUT(NOUT+1:NOUT+4)='O?2!'
                 INEXT=I+2
                 NOUT=NOUT+4
*   Units which need special formatting.
            ELSEIF(UNIT.AND.I+2.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+2)).EQ.'cm2')THEN
                 STROUT(NOUT+1:NOUT+5)='cm^2!'
                 INEXT=I+3
                 NOUT=NOUT+5
            ELSEIF(UNIT.AND.I+2.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+2)).EQ.'cm3')THEN
                 STROUT(NOUT+1:NOUT+5)='cm^3!'
                 INEXT=I+3
                 NOUT=NOUT+5
            ELSEIF(UNIT.AND.I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'microsec')THEN
                 STROUT(NOUT+1:NOUT+6)='[m]sec'
                 INEXT=I+8
                 NOUT=NOUT+6
            ELSEIF(UNIT.AND.I+5.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+5)).EQ.'micron')THEN
                 STROUT(NOUT+1:NOUT+4)='[m]m'
                 INEXT=I+6
                 NOUT=NOUT+4
            ELSEIF(UNIT.AND.I+7.LE.LEN(STRING).AND.
     -           STRING(I:MIN(LEN(STRING),I+7)).EQ.'microamp')THEN
                 STROUT(NOUT+1:NOUT+4)='[m]A'
                 INEXT=I+8
                 NOUT=NOUT+4
*   Now also replace underscores and ampersands that remain.
            ELSEIF(STRING(I:I).EQ.'_')THEN
                 STROUT(NOUT+1:NOUT+3)='[-]'
                 NOUT=NOUT+3
            ELSEIF(STRING(I:I).EQ.'&')THEN
                 STROUT(NOUT+1:NOUT+3)='"W#'
                 NOUT=NOUT+3
*   Copy all other characters as such.
            ELSE
                 STROUT(NOUT+1:NOUT+1)=STRING(I:I)
                 NOUT=NOUT+1
            ENDIF
10          CONTINUE
       ENDIF
*** Now plot the converted string.
20     CONTINUE
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''Out: "'',A,''"''/26X,
     -      ''Plot location: '',2E10.3)') STROUT(1:NOUT),X,Y
*   Plot the string.
       CALL ITX(X,Y,STROUT(1:NOUT))
       END
