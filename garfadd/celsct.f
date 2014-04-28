CDECK  ID>, CELSCT.
       SUBROUTINE CELSCT(OPTION)
*-----------------------------------------------------------------------
*   CELSCT - Reads solid cutting instructions.
*   (Last changed on 13/ 4/10.)
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
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
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
       INTEGER NWORD,I,J,K,L,INEXT,JNEXT,NC,IEQ,NC1,NC2,INPTYP,INPCMP,
     -     MODVAR(MXVAR),MODRES(1),NRES,IENTRY,NSEL,ICOL,IVOL,NREF,NPL,
     -     ISOL,IFAIL1,NREFO,IREFO(MXPLAN)
       REAL VAR(MXVAR),RES(1),FRES(3,3,3)
       DOUBLE PRECISION FXR,FYR,FZR,FNORM,X0,Y0,Z0,
     -      XPL(MXEDGE),YPL(MXEDGE),ZPL(MXEDGE),APL,BPL,CPL
       CHARACTER*(MXCHAR) STRING
       CHARACTER*10 VARLIS(MXVAR)
       CHARACTER*(*) OPTION
       LOGICAL OK,SOLSEL(MXSOLI),FLAG(MXWORD+1),USE(MXVAR),MARK(MXPLAN)
       EXTERNAL INPTYP,INPCMP
*   Cut list
       INTEGER MXCUT
       PARAMETER(MXCUT=20)
       INTEGER NCUT,NCSEL(MXCUT),NCPLA(MXCUT)
       CHARACTER*80 SELSTR(MXCUT),PLASTR(MXCUT),STR1,STR2
       SAVE NCUT, SELSTR, NCSEL, PLASTR, NCPLA
*** Debugging.
       IF(LIDENT)PRINT *,' /// ROUTINE CELSCT ///'
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ CELSCT DEBUG   : Option: '',
     -      A)') OPTION
*** Reset number of cuts.
       IF(OPTION.EQ.'RESET')THEN
            NCUT=0
            RETURN
*** Store a new cut.
       ELSEIF(OPTION.EQ.'STORE')THEN
*   See whether we still have room.
            IF(NCUT+1.GT.MXCUT)THEN
                 PRINT *,' !!!!!! CELSCT WARNING : No room to store'//
     -                ' further cuts; cut will not be applied.'
                 RETURN
            ENDIF
*   Initial settings
            NC1=0
            STR1=' '
            NC2=0
            STR2=' '
*   Find current number of arguments.
            CALL INPNUM(NWORD)
*   Flag keywords.
            DO 10 I=1,NWORD+1
            IF(I.GT.NWORD)THEN
                 FLAG(I)=.TRUE.
            ELSEIF(INPCMP(I,'PL#ANE').NE.0.OR.
     -           INPCMP(I,'SOL#IDS').NE.0)THEN
                 FLAG(I)=.TRUE.
            ELSE
                 FLAG(I)=.FALSE.
            ENDIF
10          CONTINUE
*   Search for further arguments.
            INEXT=2
            OK=.TRUE.
            DO 20 I=2,NWORD
            IF(I.LT.INEXT)GOTO 20
*   Viewing plane.
            IF(INPCMP(I,'PL#ANE').NE.0)THEN
                 STRING=' '
                 NC=0
                 DO 30 J=I+1,NWORD+1
                 IF(FLAG(J))THEN
                      CALL INPSTR(I+1,J-1,STRING,NC)
                      INEXT=J
                      GOTO 40
                 ENDIF
30               CONTINUE
                 NC=0
40               CONTINUE
*   Ensure a definition is present.
                 IF(NC.LE.0)THEN
                      PRINT *,' !!!!!! CELSCT WARNING : The cut plane'//
     -                     ' is not correctly specified.'
                      RETURN
                 ELSEIF(NC+2.GT.LEN(STR1))THEN
                      PRINT *,' !!!!!! CELSCT WARNING : The cut plane'//
     -                     ' specification is too long; not applied.'
                      RETURN
                 ENDIF
*   Check the format.
                 IF(INDEX(STRING(1:NC),'>').EQ.0.AND.
     -              INDEX(STRING(1:NC),'<').EQ.0)THEN
                      PRINT *,' !!!!!! CELSCT WARNING : Did not find'//
     -                     ' a < or > sign in the plane description.'
                      RETURN
                 ENDIF
                 IEQ=INDEX(STRING(1:NC),'>')
                 IF(IEQ.GE.1.AND.IEQ.LT.NC)THEN
                      STR1=STRING(1:IEQ-1)//'-('//STRING(IEQ+1:NC)//')'
                      NC1=NC+2
                 ELSE
                      IEQ=INDEX(STRING(1:NC),'<')
                      IF(IEQ.GE.1.AND.IEQ.LT.NC)THEN
                           STR1=STRING(IEQ+1:NC)//'-('//
     -                          STRING(1:IEQ-1)//')'
                           NC1=NC+2
                      ELSE
                           PRINT *,' !!!!!! CELSCT WARNING : The <'//
     -                          ' or > sign is missing or misplaced.'
                           RETURN
                      ENDIF
                 ENDIF
**  Solid selection.
            ELSEIF(INPCMP(I,'SOL#IDS').NE.0)THEN
                 DO 50 J=I+1,NWORD+1
                 IF(FLAG(J))THEN
                      CALL INPSTR(I+1,J-1,STRING,NC)
                      INEXT=J
                      GOTO 60
                 ENDIF
50               CONTINUE
                 NC=0
60               CONTINUE
*   Check length.
                 IF(NC.LE.0)THEN
                      PRINT *,' !!!!!! CELSCT WARNING : The solid'//
     -                     ' selection is not correctly specified.'
                      RETURN
                 ELSEIF(NC.GT.LEN(STR2))THEN
                      PRINT *,' !!!!!! CELSCT WARNING : The solid'//
     -                     ' selection is too long; not applied.'
                      RETURN
                 ENDIF
*   Store
                 STR2=STRING
                 NC2=NC
**  Anything else
            ELSE
                 PRINT *,' !!!!!! CELSCT WARNING : Unknown option'//
     -                ' ignored.'
            ENDIF
20          CONTINUE
*   Check that something has been entered
            IF(NC1.LE.0.OR.STR1.EQ.' ')THEN
                 PRINT *,' !!!!!! CELSCT WARNING : No cut plane is'//
     -                ' specified; cut is not stored.'
                 RETURN
            ENDIF
            IF(NC2.EQ.0)THEN
                 STR2='ALL'
                 NC2=3
            ENDIF
*   Store as a new cut.
            NCUT=NCUT+1
            PLASTR(NCUT)=STR1
            NCPLA(NCUT)=NC1
            SELSTR(NCUT)=STR2
            NCSEL(NCUT)=NC2
*   Debugging
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ CELSCT DEBUG   :'',
     -           '' Cut '',I5/26X,''Plane:  '',A/26X,''Solids: '',A)')
     -            NCUT,PLASTR(NCUT)(1:NCPLA(NCUT)),
     -            SELSTR(NCUT)(1:NCSEL(NCUT))
*   Done
            RETURN
**  Only other option is applying..
       ELSEIF(OPTION.NE.'APPLY')THEN
            PRINT *,' !!!!!! CELSCT WARNING : Unknown option, ',
     -           OPTION
            RETURN
       ENDIF
*** Apply the cuts in sequence.
       DO 70 I=1,NCUT
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ CELSCT DEBUG   :'',
     -      '' Processing cut '',I5)') I
*   Initial solid selection setting
       DO 80 J=1,NSOLID
       SOLSEL(J)=.FALSE.
80     CONTINUE
       NSEL=0
*   Decode the string.
       JNEXT=1
       DO 90 J=1,NCSEL(I)
       IF(J.LT.JNEXT)GOTO 90
       IF(INDEX('0123456789',SELSTR(I)(J:J)).GT.0)THEN
            K=J+1
            ISOL=0
            IFAIL1=0
100         CONTINUE
            IF(K.GT.NCSEL(I))THEN
                 CALL INPRIC(SELSTR(I)(J:NCSEL(I)),ISOL,0,IFAIL1)
                 JNEXT=NCSEL(I)+1
                 GOTO 110
            ELSEIF(INDEX('0123456789',SELSTR(I)(K:K)).NE.0)THEN
                 K=K+1
                 GOTO 100
            ENDIF
            CALL INPRIC(SELSTR(I)(J:K-1),ISOL,0,IFAIL1)
            JNEXT=K
110         CONTINUE
            IF(IFAIL1.NE.0.OR.ISOL.LE.0.OR.ISOL.GT.NSOLID)THEN
                 PRINT *,' !!!!!! CELSCT WARNING : Solid number ',
     -                ISOL,' not valid in cut ',I
                 GOTO 70
            ENDIF
            SOLSEL(ISOL)=.TRUE.
            NSEL=NSEL+1
       ELSEIF(SELSTR(I)(J:MIN(NCSEL(I),J+2)).EQ.'ALL')THEN
            DO 120 K=1,NSOLID
            SOLSEL(K)=.TRUE.
120         CONTINUE
            NSEL=NSOLID
            JNEXT=J+3
       ELSEIF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',SELSTR(I)(J:J)).GT.
     -      0)THEN
            DO 130 K=1,NSOLID
            IF(SOLTYP(K).EQ.SELSTR(I)(J:J))SOLSEL(K)=.TRUE.
            NSEL=NSEL+1
130         CONTINUE
       ELSEIF(INDEX(' ,',SELSTR(I)(J:J)).EQ.0)THEN
            PRINT *,' !!!!!! CELSCT WARNING : Unrecognised character "',
     -           SELSTR(I)(J:J),'" in solid selection; skipped.'
            GOTO 70
       ENDIF
90     CONTINUE
*   If the selection is empty, select all.
       IF(NSEL.EQ.0)THEN
            DO 140 K=1,NSOLID
            SOLSEL(K)=.TRUE.
140         CONTINUE
       ENDIF
*   Debugging
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ CELSCT DEBUG   : Selected'',
     -           '' solids: '')')
            DO 210 J=1,NSOLID
            IF(SOLSEL(J))WRITE(LUNOUT,'(26X,I5,5X,A1)') J,SOLTYP(J)
210         CONTINUE
       ENDIF
*** Translate the formula.
       VARLIS(1)='X'
       VARLIS(2)='Y'
       VARLIS(3)='Z'
       CALL ALGPRE(PLASTR(I)(1:NCPLA(I)),NCPLA(I),VARLIS,3,NRES,USE,
     -      IENTRY,IFAIL1)
*   Check the results.
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! CELSCT WARNING : Plane description ',I,
     -           ' formula not translatable.'
            CALL ALGCLR(IENTRY)
            GOTO 70
       ELSEIF(NRES.NE.1)THEN
            PRINT *,' !!!!!! CELSCT WARNING : Plane description ',I,
     -           ' formula does not return 1 result.'
            CALL ALGCLR(IENTRY)
            GOTO 70
       ELSEIF(.NOT.(USE(1).OR.USE(2).OR.USE(3)))THEN
            PRINT *,' !!!!!! CELSCT WARNING : Plane description ',I,
     -           ' formula does not depend on x, y or z.'
            CALL ALGCLR(IENTRY)
            GOTO 70
       ENDIF
*** Compute function values at a (3x3) set of points.
       OK=.TRUE.
       MODVAR(1)=2
       MODVAR(2)=2
       MODVAR(3)=2
       DO 150 L=-1,1
       DO 160 J=-1,1
       DO 170 K=-1,1
       VAR(1)=0.5*(XMIN+XMAX)+L*(1+ABS(XMIN)+ABS(XMAX))
       VAR(2)=0.5*(YMIN+YMAX)+J*(1+ABS(YMIN)+ABS(YMAX))
       VAR(3)=0.5*(ZMIN+ZMAX)+K*(1+ABS(ZMIN)+ABS(ZMAX))
       CALL ALGEXE(IENTRY,VAR,MODVAR,3,RES,MODRES,1,IFAIL1)
       IF(IFAIL1.NE.0.OR.MODRES(1).NE.2)OK=.FALSE.
       FRES(2+L,2+J,2+K)=RES(1)
170    CONTINUE
160    CONTINUE
150    CONTINUE
*   Ensure that all function evaluations worked.
       IF(.NOT.OK)THEN
            PRINT *,' !!!!!! CELSCT WARNING : Plane description ',I,
     -           ' formula can not be evaluated.'
            CALL ALGCLR(IENTRY)
            GOTO 70
       ENDIF
*** Extract parameters.
       FXR=((FRES(3,1,1)-FRES(1,1,1))+
     -      (FRES(3,1,2)-FRES(1,1,2))+
     -      (FRES(3,2,1)-FRES(1,2,1))+
     -      (FRES(3,2,2)-FRES(1,2,2)))/(8*(1+ABS(XMIN)+ABS(XMAX)))
       FYR=((FRES(1,3,1)-FRES(1,1,1))+
     -      (FRES(1,3,2)-FRES(1,1,2))+
     -      (FRES(2,3,1)-FRES(2,1,1))+
     -      (FRES(2,3,2)-FRES(2,1,2)))/(8*(1+ABS(YMIN)+ABS(YMAX)))
       FZR=((FRES(1,1,3)-FRES(1,1,1))+
     -      (FRES(1,2,3)-FRES(1,2,1))+
     -      (FRES(2,1,3)-FRES(2,1,1))+
     -      (FRES(2,2,3)-FRES(2,2,1)))/(8*(1+ABS(ZMIN)+ABS(ZMAX)))
*   Check for cross-terms.
       IF(ABS(FXR-0.5*(FRES(3,1,1)-FRES(1,1,1))/
     -      (1+ABS(XMIN)+ABS(XMAX))).GT.1E-4*(1+ABS(FXR)).OR.
     -      ABS(FXR-0.5*(FRES(3,1,2)-FRES(1,1,2))/
     -      (1+ABS(XMIN)+ABS(XMAX))).GT.1E-4*(1+ABS(FXR)).OR.
     -      ABS(FXR-0.5*(FRES(3,2,1)-FRES(1,2,1))/
     -      (1+ABS(XMIN)+ABS(XMAX))).GT.1E-4*(1+ABS(FXR)).OR.
     -      ABS(FXR-0.5*(FRES(3,2,2)-FRES(1,2,2))/
     -      (1+ABS(XMIN)+ABS(XMAX))).GT.1E-4*(1+ABS(FXR)).OR.
     -      ABS(FYR-0.5*(FRES(1,3,1)-FRES(1,1,1))/
     -      (1+ABS(YMIN)+ABS(YMAX))).GT.1E-4*(1+ABS(FYR)).OR.
     -      ABS(FYR-0.5*(FRES(1,3,2)-FRES(1,1,2))/
     -      (1+ABS(YMIN)+ABS(YMAX))).GT.1E-4*(1+ABS(FYR)).OR.
     -      ABS(FYR-0.5*(FRES(2,3,1)-FRES(2,1,1))/
     -      (1+ABS(YMIN)+ABS(YMAX))).GT.1E-4*(1+ABS(FYR)).OR.
     -      ABS(FYR-0.5*(FRES(2,3,2)-FRES(2,1,2))/
     -      (1+ABS(YMIN)+ABS(YMAX))).GT.1E-4*(1+ABS(FYR)).OR.
     -      ABS(FZR-0.5*(FRES(1,1,3)-FRES(1,1,1))/
     -      (1+ABS(ZMIN)+ABS(ZMAX))).GT.1E-4*(1+ABS(FZR)).OR.
     -      ABS(FZR-0.5*(FRES(1,2,3)-FRES(1,2,1))/
     -      (1+ABS(ZMIN)+ABS(ZMAX))).GT.1E-4*(1+ABS(FZR)).OR.
     -      ABS(FZR-0.5*(FRES(2,1,3)-FRES(2,1,1))/
     -      (1+ABS(ZMIN)+ABS(ZMAX))).GT.1E-4*(1+ABS(FZR)).OR.
     -      ABS(FZR-0.5*(FRES(2,2,3)-FRES(2,2,1))/
     -      (1+ABS(ZMIN)+ABS(ZMAX))).GT.1E-4*(1+ABS(FZR)))THEN
            PRINT *,' !!!!!! CELSCT WARNING : Plane description ',I,
     -           ' formula probably contains cross terms.'
            CALL ALGCLR(IENTRY)
            GOTO 70
       ENDIF
*   Check for linearity in x, y and z.
       IF(  ABS(FRES(3,1,1)-2*FRES(2,1,1)+FRES(1,1,1)).GT.1E-4*(1+
     -      MAX(ABS(FRES(1,1,1)),ABS(FRES(2,1,1)),ABS(FRES(3,1,1)))).OR.
     -      ABS(FRES(3,1,2)-2*FRES(2,1,2)+FRES(1,1,2)).GT.1E-4*(1+
     -      MAX(ABS(FRES(1,1,2)),ABS(FRES(2,1,2)),ABS(FRES(3,1,2)))).OR.
     -      ABS(FRES(3,2,1)-2*FRES(2,2,1)+FRES(1,2,1)).GT.1E-4*(1+
     -      MAX(ABS(FRES(1,2,1)),ABS(FRES(2,2,1)),ABS(FRES(3,2,1)))).OR.
     -      ABS(FRES(3,2,2)-2*FRES(2,2,2)+FRES(1,2,2)).GT.1E-4*(1+
     -      MAX(ABS(FRES(1,2,2)),ABS(FRES(2,2,2)),ABS(FRES(3,2,2)))))
     -      THEN
            PRINT *,' !!!!!! CELSCT WARNING : Plane description ',I,
     -           ' formula is not linear in x.'
            CALL ALGCLR(IENTRY)
            GOTO 70
       ENDIF
       IF(  ABS(FRES(1,3,1)-2*FRES(1,2,1)+FRES(1,1,1)).GT.1E-4*(1+
     -      MAX(ABS(FRES(1,1,1)),ABS(FRES(1,2,1)),ABS(FRES(1,3,1)))).OR.
     -      ABS(FRES(1,3,2)-2*FRES(1,2,2)+FRES(1,1,2)).GT.1E-4*(1+
     -      MAX(ABS(FRES(1,1,2)),ABS(FRES(1,2,2)),ABS(FRES(1,3,2)))).OR.
     -      ABS(FRES(2,3,1)-2*FRES(2,2,1)+FRES(2,1,1)).GT.1E-4*(1+
     -      MAX(ABS(FRES(2,1,1)),ABS(FRES(2,2,1)),ABS(FRES(2,3,1)))).OR.
     -      ABS(FRES(2,3,2)-2*FRES(2,2,2)+FRES(2,1,2)).GT.1E-4*(1+
     -      MAX(ABS(FRES(2,1,2)),ABS(FRES(2,2,2)),ABS(FRES(2,3,2)))))
     -      THEN
            PRINT *,' !!!!!! CELSCT WARNING : Plane description ',I,
     -           ' formula is not linear in y.'
            CALL ALGCLR(IENTRY)
            GOTO 70
       ENDIF

       IF(  ABS(FRES(1,1,3)-2*FRES(1,1,2)+FRES(1,1,1)).GT.1E-4*(1+
     -      MAX(ABS(FRES(1,1,1)),ABS(FRES(1,1,2)),ABS(FRES(1,1,3)))).OR.
     -      ABS(FRES(1,2,3)-2*FRES(1,2,2)+FRES(1,2,1)).GT.1E-4*(1+
     -      MAX(ABS(FRES(1,2,1)),ABS(FRES(1,2,2)),ABS(FRES(1,2,3)))).OR.
     -      ABS(FRES(2,1,3)-2*FRES(2,1,2)+FRES(2,1,1)).GT.1E-4*(1+
     -      MAX(ABS(FRES(2,1,1)),ABS(FRES(2,1,2)),ABS(FRES(2,1,3)))).OR.
     -      ABS(FRES(2,2,3)-2*FRES(2,2,2)+FRES(2,2,1)).GT.1E-4*(1+
     -      MAX(ABS(FRES(2,2,1)),ABS(FRES(2,2,2)),ABS(FRES(2,2,3)))))
     -      THEN
            PRINT *,' !!!!!! CELSCT WARNING : Plane description ',I,
     -           ' formula is not linear in z.'
            CALL ALGCLR(IENTRY)
            GOTO 70
       ENDIF
*   Check normalisation
       FNORM=SQRT(FXR**2+FYR**2+FZR**2)
       IF(FNORM.LE.0)THEN
            PRINT *,' !!!!!! CELSCT WARNING : Formula ',I,' does not',
     -           ' describe a plane (zero norm normal vector).'
            CALL ALGCLR(IENTRY)
            GOTO 70
       ENDIF
*** Normalise the in-plane vector.
       VAR(1)=0
       VAR(2)=0
       VAR(3)=0
       CALL ALGEXE(IENTRY,VAR,MODVAR,3,RES,MODRES,1,IFAIL1)
       IF(IFAIL1.NE.0.OR.MODRES(1).NE.2)THEN
            PRINT *,' !!!!!! CELSCT WARNING : Unable to compute'//
     -           ' an in-plane vector for cut ',I,'.'
            CALL ALGCLR(IENTRY)
            RETURN
       ENDIF
       X0=-RES(1)*FXR/FNORM**2
       Y0=-RES(1)*FYR/FNORM**2
       Z0=-RES(1)*FZR/FNORM**2
*** Clear the formula.
       CALL ALGCLR(IENTRY)
*** Debugging.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ CELSCT DEBUG   : Cut plane''/
     -      26X,''In plane point: '',F10.3,2X,F10.3,2X,F10.3/
     -      26X,''Normal vector:  '',F10.3,2X,F10.3,2X,F10.3)')
     -      X0,Y0,Z0,FXR,FYR,FZR
*** Loop over all panels to cut.
       CALL PLABU1('QUERY',NREF,NPL,XPL,YPL,ZPL,APL,BPL,CPL,
     -      ICOL,IVOL,IFAIL1)
       DO 200 J=1,MXPLAN
       MARK(J)=.FALSE.
200    CONTINUE
       DO 180 J=1,NREF
       CALL PLABU1('READ',J,NPL,XPL,YPL,ZPL,
     -      APL,BPL,CPL,ICOL,IVOL,IFAIL1)
       IF(MARK(J).OR.IFAIL1.NE.0)THEN
            GOTO 180
       ELSEIF(IVOL.LE.0.OR.IVOL.GT.NSOLID)THEN
            PRINT *,' !!!!!! CELSCT WARNING : Invalid volume'//
     -           ' reference ',IVOL,' on panel ',I,'; skipped.'
            GOTO 180
       ELSEIF(SOLSEL(IVOL))THEN
            CALL BEMCUT(J, NREFO, IREFO, FXR,FYR,FZR, X0,Y0,Z0)
            DO 190 K=1,NREFO
            MARK(IREFO(K))=.TRUE.
190         CONTINUE
            CALL PLABU1('DELETE',J,NPL,XPL,YPL,ZPL,APL,BPL,CPL,
     -           ICOL,IVOL,IFAIL1)
       ENDIF
180    CONTINUE
*** Next cut
70     CONTINUE
       END
