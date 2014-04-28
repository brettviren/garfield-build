CDECK  ID>, OPTCHV.
       SUBROUTINE OPTCHV
*-----------------------------------------------------------------------
*   OPTCHV - Changes voltages.
*   (Last changed on 20/10/99.)
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
       INTEGER INPCMP,INPTYP,I,J,NWORD,INEXT,IWIRE,IFAIL,IFAIL1,IFAIL2,
     -      NC,IPLANE,NFOUND
       REAL VNEW(MXWIRE),VPLNEW(5),VREAD
       CHARACTER*(MXCHAR) CODE
       LOGICAL OK
       EXTERNAL INPCMP,INPTYP
*** Original settings for wires, planes and tube.
       DO 20 I=1,NWIRE
       VNEW(I)=V(I)
20     CONTINUE
       DO 30 I=1,4
       VPLNEW(I)=VTPLAN(I)
30     CONTINUE
       VPLNEW(5)=VTTUBE
*** Decode the argument string.
       CALL INPNUM(NWORD)
*   Check there are at least some words on the line.
       IF(NWORD.LE.1)THEN
            PRINT *,' !!!!!! OPTCHV WARNING : This instruction needs'//
     -           ' arguments; nothing done.'
            RETURN
       ENDIF
**  Keep track of errors.
       OK=.TRUE.
**  Loop over the arguments.
       INEXT=2
       DO 10 I=1,NWORD
       IF(I.LT.INEXT)GOTO 10
**  Wire selection.
       IF(INPCMP(I,'W#IRE').NE.0)THEN
*   Ensure the wire is specified.
            IF(NWORD.LT.I+1)THEN
                 CALL INPMSG(I,'The wire should be specified.')
                 IWIRE=0
*   Read the wire number.
            ELSEIF(INPTYP(I+1).EQ.1)THEN
                 CALL INPCHK(I+1,1,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDI(I+1,IWIRE,0)
                      IF(IWIRE.LE.0.OR.IWIRE.GT.NWIRE)THEN
                           CALL INPMSG(I+1,'Wire number out of range.')
                           IWIRE=0
                           OK=.FALSE.
                      ENDIF
                 ELSE
                      IWIRE=0
                 ENDIF
*   Read the wire code.
            ELSE
                 CALL INPSTR(I+1,I+1,CODE,NC)
                 IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',CODE(1:1))
     -                .EQ.0)THEN
                      CALL INPMSG(I+1,'Not a valid wire code.')
                      OK=.FALSE.
                      IWIRE=0
                 ELSE
                      IWIRE=-1
                 ENDIF
                 IF(NC.GT.1)
     -                CALL INPMSG(I+1,'Only first character used.    ')
            ENDIF
**  Read the new voltage.
            IF(INPCMP(I+2,'V#OLTAGE').EQ.0.OR.NWORD.LT.I+3)THEN
                 CALL INPMSG(I,'The voltage is missing.')
                 OK=.FALSE.
                 INEXT=I+2
                 GOTO 10
            ELSE
                 CALL INPCHK(I+3,2,IFAIL2)
                 CALL INPRDR(I+3,VREAD,0.0)
            ENDIF
*   Store the result in the proper location.
            IF(IWIRE.GT.0.AND.IWIRE.LE.NWIRE)THEN
                 VNEW(IWIRE)=VREAD
            ELSEIF(IWIRE.EQ.-1)THEN
                 NFOUND=0
                 DO 40 J=1,NWIRE
                 IF(WIRTYP(J).EQ.CODE(1:1))THEN
                      VNEW(J)=VREAD
                      NFOUND=NFOUND+1
                 ENDIF
40               CONTINUE
                 IF(NFOUND.EQ.0)THEN
                      CALL INPMSG(I+1,'No such wire.')
                      OK=.FALSE.
                 ENDIF
            ENDIF
*   And increment the word.
            INEXT=I+4
**  Plane selection.
       ELSEIF(INPCMP(I,'PL#ANE').NE.0)THEN
*   Ensure the plane is specified.
            IF(NWORD.LT.I+1)THEN
                 CALL INPMSG(I,'The plane should be specified.')
                 IPLANE=0
*   Read the plane number.
            ELSEIF(INPTYP(I+1).EQ.1)THEN
                 CALL INPCHK(I+1,1,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDI(I+1,IPLANE,0)
                      IPLANE=ABS(IPLANE)
                      IF(IPLANE.LE.0.OR.IPLANE.GT.5)THEN
                           CALL INPMSG(I+1,'Plane number out of range.')
                           IPLANE=0
                           OK=.FALSE.
                      ENDIF
                 ELSE
                      IPLANE=0
                 ENDIF
*   Plane selection by name.
            ELSEIF(INPCMP(I+1,'LOW#ER-X')+INPCMP(I+1,'L#EFT').NE.0)THEN
                 IF(.NOT.YNPLAN(1))THEN
                      CALL INPMSG(I+1,'No such plane.')
                      OK=.FALSE.
                 ELSE
                      IPLANE=1
                 ENDIF
            ELSEIF(INPCMP(I+1,'UP#PER-X')+INPCMP(I+1,'R#IGHT').NE.0)THEN
                 IF(.NOT.YNPLAN(2))THEN
                      CALL INPMSG(I+1,'No such plane.')
                      OK=.FALSE.
                 ELSE
                      IPLANE=2
                 ENDIF
            ELSEIF(INPCMP(I+1,'LOW#ER-Y')+
     -           INPCMP(I+1,'B#OTTOM').NE.0)THEN
                 IF(.NOT.YNPLAN(3))THEN
                      CALL INPMSG(I+1,'No such plane.')
                      OK=.FALSE.
                 ELSE
                      IPLANE=3
                 ENDIF
            ELSEIF(INPCMP(I+1,'UP#PER-X')+INPCMP(I+1,'T#OP').NE.0)THEN
                 IF(.NOT.YNPLAN(4))THEN
                      CALL INPMSG(I+1,'No such plane.')
                      OK=.FALSE.
                 ELSE
                      IPLANE=4
                 ENDIF
            ELSEIF(INPCMP(I+1,'TUBE').NE.0)THEN
                 IF(.NOT.TUBE)THEN
                      CALL INPMSG(I+1,'No tube in this cell.')
                      OK=.FALSE.
                 ELSE
                      IPLANE=5
                 ENDIF
*   Read the PLANE code.
            ELSE
                 CALL INPSTR(I+1,I+1,CODE,NC)
                 IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',CODE(1:1))
     -                .EQ.0)THEN
                      CALL INPMSG(I+1,'Not a valid plane code.')
                      IPLANE=0
                      OK=.FALSE.
                 ELSE
                      IPLANE=-1
                 ENDIF
                 IF(NC.GT.1)
     -                CALL INPMSG(I+1,'Only first character used.')
            ENDIF
**  Read the new voltage.
            IF(INPCMP(I+2,'V#OLTAGE').EQ.0.OR.NWORD.LT.I+3)THEN
                 CALL INPMSG(I,'The voltage is missing.')
                 INEXT=I+2
                 OK=.FALSE.
                 GOTO 10
            ELSE
                 CALL INPCHK(I+3,2,IFAIL2)
                 CALL INPRDR(I+3,VREAD,0.0)
            ENDIF
*   Store the result in the proper location.
            IF(IPLANE.GE.1.AND.IPLANE.LE.5)THEN
                 VPLNEW(IPLANE)=VREAD
            ELSEIF(IPLANE.EQ.-1)THEN
                 NFOUND=0
                 DO 50 J=1,5
                 IF(PLATYP(J).EQ.CODE(1:1))THEN
                      VPLNEW(J)=VREAD
                      NFOUND=NFOUND+1
                 ENDIF
50               CONTINUE
                 IF(NFOUND.EQ.0)THEN
                      CALL INPMSG(I+1,'No such plane.')
                      OK=.FALSE.
                 ENDIF
            ENDIF
*   And increment the word.
            INEXT=I+4
**  Tube selection.
       ELSEIF(INPCMP(I,'TUBE').NE.0)THEN
**  Read the new voltage.
            IF(INPCMP(I+1,'V#OLTAGE').EQ.0.OR.NWORD.LT.I+2)THEN
                 CALL INPMSG(I,'The voltage is missing.')
                 INEXT=I+1
                 OK=.FALSE.
                 GOTO 10
            ELSE
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+2,VREAD,0.0)
            ENDIF
*   Store the result in the proper location.
            IF(TUBE)THEN
                 VPLNEW(5)=VREAD
            ELSE
                 CALL INPMSG(I,'No tube in this cell.')
                 OK=.FALSE.
            ENDIF
*   And increment the word.
            INEXT=I+3
**  Valid keyword out of context.
       ELSEIF(INPCMP(I,'V#OLTAGE').NE.0)THEN
            CALL INPMSG(I,'Valid keyword out of context. ')
            OK=.FALSE.
*   Invalid keywords.
       ELSE
            CALL INPMSG(I,'Not a valid keyword.          ')
            OK=.FALSE.
       ENDIF
10     CONTINUE
*** Dump error messages.
       CALL INPERR
*** Take action depending on the state of OK.
       IF(.NOT.OK)THEN
            IF(JFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! OPTCHV WARNING : Errors found in'//
     -                ' the command; performing a partial update.'
            ELSEIF(JFAIL.EQ.2)THEN
                 PRINT *,' !!!!!! OPTCHV WARNING : Errors found in'//
     -                ' the command; not changing any voltages.'
                 RETURN
            ELSE
                 PRINT *,' !!!!!! OPTCHV WARNING : Errors found in'//
     -                ' the command; terminating program execution.'
                 CALL QUIT
            ENDIF
       ENDIF
*** Set new voltages.
       CALL SETNEW(VNEW,VPLNEW,IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' ###### OPTCHV ERROR   : Voltage change failed;'//
     -           ' cell deleted.'
            CELSET=.FALSE.
       ENDIF
       END
