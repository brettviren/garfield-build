CDECK  ID>, OPTBGF.
       SUBROUTINE OPTBGF
*-----------------------------------------------------------------------
*   OPTBGF - Adds a background field.
*   (Last changed on  5/ 4/98.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*(MXCHAR) STRV,STREX,STREY,STREZ
       CHARACTER*10 VARLIS(MXVAR),USER
       INTEGER NWORD,I,INEXT,INPCMP,NVAR,NCFV,NCFEX,NCFEY,NCFEZ,NRES,
     -      IFAIL1
       LOGICAL USE(MXVAR),OK
       EXTERNAL INPCMP
       SAVE STRV,STREX,STREY,STREZ,NCFV,NCFEX,NCFEY,NCFEZ
       DATA NCFV,NCFEX,NCFEY,NCFEZ /0,0,0,0/
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE OPTBGF ///'
*** Count words.
       CALL INPNUM(NWORD)
*** Display current state if there are no arguments.
       IF(NWORD.EQ.1)THEN
            IF(IENBGF.LE.0)THEN
                 WRITE(LUNOUT,'(''  Currently no background field.'')')
            ELSE
                 WRITE(LUNOUT,'(''  Currently the background field'',
     -                '' is:''//''  potential: '',A/''  Ex:        '',
     -                A/''  Ey:        '',A/''  Ez:        '',A)')
     -                STRV(1:MAX(1,NCFV)),STREX(1:MAX(1,NCFEX)),
     -                STREY(1:MAX(1,NCFEY)),STREZ(1:MAX(1,NCFEZ))
            ENDIF
            RETURN
       ENDIF
*** Set the list of variables.
       IF(POLAR)THEN
            VARLIS(1)='R'
            VARLIS(2)='PHI'
            VARLIS(3)='Z'
            VARLIS(4)='EXMAP'
            VARLIS(5)='EYMAP'
            VARLIS(6)='EZMAP'
            VARLIS(7)='VMAP'
       ELSE
            VARLIS(1)='X'
            VARLIS(2)='Y'
            VARLIS(3)='Z'
            VARLIS(4)='EXMAP'
            VARLIS(5)='EYMAP'
            VARLIS(6)='EZMAP'
            VARLIS(7)='VMAP'
       ENDIF
       NVAR=7
*** Preset the strings.
       STRV=' '
       NCFV=0
       STREX=' '
       NCFEX=0
       STREY=' '
       NCFEY=0
       STREZ=' '
       NCFEZ=0
*** Delete old entry points if present.
       IF(IENBGF.NE.0)CALL ALGCLR(IENBGF)
       IENBGF=0
       LBGFMP=.FALSE.
*** Loop over the components.
       OK=.TRUE.
       INEXT=2
       DO 10 I=2,NWORD
       IF(I.LT.INEXT)GOTO 10
*** Pick up the field components.
       IF(INPCMP(I,'V#OLTAGE')+INPCMP(I,'POT#ENTIAL').NE.0)THEN
            CALL INPSTR(I+1,I+1,STRV,NCFV)
            INEXT=I+2
       ELSEIF(INPCMP(I,'EX').NE.0)THEN
            CALL INPSTR(I+1,I+1,STREX,NCFEX)
            INEXT=I+2
       ELSEIF(INPCMP(I,'EY').NE.0)THEN
            CALL INPSTR(I+1,I+1,STREY,NCFEY)
            INEXT=I+2
       ELSEIF(INPCMP(I,'EZ').NE.0)THEN
            CALL INPSTR(I+1,I+1,STREZ,NCFEZ)
            INEXT=I+2
       ELSE
            CALL INPMSG(I,'Not a known field.')
            OK=.FALSE.
       ENDIF
10     CONTINUE
*** Dump error messages.
       CALL INPERR
*** Check that all fields are present.
       IF(NCFV.LE.0)THEN
            PRINT *,' ------ OPTBGF MESSAGE : Potential of the'//
     -           ' background field is missing; set to 0.'
            STRV='0'
            NCFV=1
            OK=.FALSE.
       ENDIF
       IF(NCFEX.LE.0)THEN
            PRINT *,' ------ OPTBGF MESSAGE  : Ex of the'//
     -           ' background field is missing; set to 0.'
            STREX='0'
            NCFEX=1
            OK=.FALSE.
       ENDIF
       IF(NCFEY.LE.0)THEN
            PRINT *,' ------ OPTBGF MESSAGE : Ey of the'//
     -           ' background field is missing; set to 0.'
            STREY='0'
            NCFEY=1
            OK=.FALSE.
       ENDIF
       IF(NCFEZ.LE.0)THEN
            PRINT *,' ------ OPTBGF MESSAGE : Ez of the'//
     -           ' background field is missing; set to 0.'
            STREZ='0'
            NCFEZ=1
       ENDIF
*** See whether we continue.
       IF(JFAIL.EQ.2.AND..NOT.OK)THEN
            PRINT *,' ###### OPTBGF ERROR   : No background field'//
     -           ' because of the above errors.'
            IENBGF=0
            RETURN
       ELSEIF(JFAIL.EQ.3.AND..NOT.OK)THEN
            PRINT *,' ###### OPTBGF ERROR   : Program terminated'//
     -           ' because of the above errors.'
            CALL QUIT
            RETURN
       ENDIF
*** Reset the error flag.
       OK=.TRUE.
*** Translate the background field.
       IF(INDEX(STRV(1:NCFV)//','//STREX(1:NCFEX)//','//STREY(1:NCFEY)
     -      //','//STREZ(1:NCFEZ),'@').NE.0)THEN
            NRES=4
            PRINT *,' ------ OPTBGF MESSAGE : Please edit the'//
     -           ' function.'
            CALL ALGEDT(VARLIS,NVAR,IENBGF,USE,NRES)
            IFAIL1=0
*   Usual function translation if not.
       ELSE
            CALL ALGPRE(STRV(1:NCFV)//','//STREX(1:NCFEX)//','//
     -           STREY(1:NCFEY)//','//STREZ(1:NCFEZ),
     -           NCFV+NCFEX+NCFEY+NCFEZ+3,VARLIS,NVAR,NRES,USE,
     -                IENBGF,IFAIL1)
       ENDIF
*   Check return code of translation.
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! OPTBGF WARNING : Error translating the'//
     -           ' field functions.'
            OK=.FALSE.
            CALL ALGCLR(IENBGF)
       ENDIF
*   Check number of results returned by the function.
       IF(NRES.NE.4)THEN
            PRINT *,' !!!!!! OPTBGF WARNING : The field functions do'//
     -           ' not return 4 results.'
            OK=.FALSE.
            CALL ALGCLR(IENBGF)
       ENDIF
*   Check use of field map.
       IF(USE(4).OR.USE(5).OR.USE(6).OR.USE(7))THEN
            CALL BOOK('INQUIRE','MAP',USER,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! OPTBGF WARNING : Unable to'//
     -                ' find out who owns the field map; background'//
     -                ' field rejected.'
                 OK=.FALSE.
                 CALL ALGCLR(IENBGF)
            ELSEIF(USER.EQ.'CELL')THEN
                 PRINT *,' !!!!!! OPTBGF WARNING : Field map is used'//
     -                ' as main field; background field rejected.'
                 OK=.FALSE.
                 CALL ALGCLR(IENBGF)
            ELSEIF(USER.NE.'OPTIMISE')THEN
                 PRINT *,' !!!!!! OPTBGF WARNING : No background'//
     -                ' field map available ; background field'//
     -                ' rejected.'
                 OK=.FALSE.
                 CALL ALGCLR(IENBGF)
            ELSEIF(POLAR)THEN
                 PRINT *,' !!!!!! OPTBGF WARNING : Background fields'//
     -                ' no available in polar cells; background field'//
     -                ' rejected.'
                 OK=.FALSE.
                 CALL ALGCLR(IENBGF)
            ELSE
                 LBGFMP=.TRUE.
            ENDIF
       ENDIF
*** See whether we continue.
       IF(JFAIL.EQ.2.AND..NOT.OK)THEN
            PRINT *,' ###### OPTBGF ERROR   : No background field'//
     -           ' because of the above errors.'
            IENBGF=0
            RETURN
       ELSEIF(JFAIL.EQ.3.AND..NOT.OK)THEN
            PRINT *,' ###### OPTBGF ERROR   : Program terminated'//
     -           ' because of the above errors.'
            CALL QUIT
            RETURN
       ENDIF
       END
