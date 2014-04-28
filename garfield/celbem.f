CDECK  ID>, CELBEM.
       SUBROUTINE CELBEM
*-----------------------------------------------------------------------
*   CELBEM - neBEM parameters.
*   VARIABLES :
*   (Last changed on 15/ 4/12.)
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
       INTEGER NBEM,IREFB1(MXPLAN),NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,
     -      BEMNEW,BEMINV,BEMSLV
       DOUBLE PRECISION BEMQTH,BEMSTH,BEMSSC,BEMTGT,BEMEPA,BEMEPD
       LOGICAL LBDUMP
       COMMON /BEMDAT/ BEMQTH,BEMSSC,BEMSTH,BEMTGT,BEMEPA,BEMEPD,
     -      IREFB1,NBEM,NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,BEMNEW,
     -      BEMINV,BEMSLV,LBDUMP
       CHARACTER*20 AUX1,AUX2,AUX3,AUX4,AUX5,AUX6,AUX7,AUX8,AUX9,AUX10,
     -      AUX11,AUX12,AUX13
       INTEGER INPCMP,INPTYP,INEXT,I,NWORD,IFAIL1,IAUX,
     -      NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8,NC9,NC10,NC11,NC12,NC13
       REAL AUX
       EXTERNAL INPCMP,INPTYP
*** Get the number of words on the line.
       CALL INPNUM(NWORD)
*** If there is only one argument.
       IF(NWORD.EQ.1)THEN
            CALL OUTFMT(REAL(BEMQTH),2,AUX1,NC1,'LEFT')
            CALL OUTFMT(REAL(BEMSSC),2,AUX2,NC2,'LEFT')
            CALL OUTFMT(REAL(BEMTGT),2,AUX3,NC3,'LEFT')
            CALL OUTFMT(REAL(NBEMMN),2,AUX4,NC4,'LEFT')
            CALL OUTFMT(REAL(NBEMMX),2,AUX5,NC5,'LEFT')
            CALL OUTFMT(REAL(NBEMPX),2,AUX6,NC6,'LEFT')
            CALL OUTFMT(REAL(NBEMPY),2,AUX11,NC11,'LEFT')
            CALL OUTFMT(REAL(NBEMPZ),2,AUX12,NC12,'LEFT')
            CALL OUTFMT(REAL(BEMNEW),2,AUX7,NC7,'LEFT')
            CALL OUTFMT(REAL(BEMINV),2,AUX8,NC8,'LEFT')
            CALL OUTFMT(REAL(BEMEPD),2,AUX9,NC9,'LEFT')
            CALL OUTFMT(REAL(BEMEPA),2,AUX10,NC10,'LEFT')
            CALL OUTFMT(REAL(BEMSLV),2,AUX13,NC13,'LEFT')
            WRITE(LUNOUT,'(
     -         ''  CURRENT NEBEM PARAMETERS:''//
     -         ''  Panel quality threshold:              '',A/
     -         ''  Panel size threshold (relative):      '',A/
     -         ''  Target element size:                  '',A,'' cm''/
     -         ''  Distance tolerance:                   '',A,'' cm''/
     -         ''  Angular tolerance:                    '',A,'' rad''/
     -         ''  Minimum number of elements per panel: '',A/
     -         ''  Maximum number of elements per panel: '',A/
     -         ''  Periodic repetitions in x, y, z:      '',A,1X,A,1X,A/
     -         ''  New model (1) or reload model (0):    '',A/
     -         ''  Keep (1) inverted matrix or not (0):  '',A/
     -         ''  Matrix inversion LU (0) or SVD (1):   '',A)')
     -         AUX1(1:NC1),AUX2(1:NC2),AUX3(1:NC3),AUX9(1:NC9),
     -         AUX10(1:NC10),AUX4(1:NC4),
     -         AUX5(1:NC5),AUX6(1:NC6),AUX11(1:NC11),AUX12(1:NC12),
     -         AUX7(1:NC7),AUX8(1:NC8),AUX13(1:NC13)
*** Otherwise decode the argument list.
       ELSE
            INEXT=2
            DO 10 I=2,NWORD
            IF(I.LT.INEXT)GOTO 10
*   Quality threshold.
            IF(INPCMP(I,'Q#UALITY-THR#ESHOLD').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.
     -                (INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2))THEN
                      CALL INPMSG(I,'Should have a number as arg.')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,AUX,REAL(BEMQTH))
                      IF(AUX.GT.1)THEN
                           BEMQTH=AUX
                      ELSE
                           CALL INPMSG(I+1,
     -                          'Value not at least equal to 1')
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Size threshold.
            ELSEIF(INPCMP(I,'SIZE-THR#ESHOLD').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.
     -                (INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2))THEN
                      CALL INPMSG(I,'Should have a number as arg.')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,AUX,REAL(BEMSSC))
                      IF(AUX.GT.0)THEN
                           BEMSSC=AUX
                      ELSE
                           CALL INPMSG(I+1,
     -                          'Value not at least equal to 0')
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Target size.
            ELSEIF(INPCMP(I,'TARG#ET-ELEM#ENT-#SIZE')+
     -           INPCMP(I,'TARG#ET-#SIZE').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.
     -                (INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2))THEN
                      CALL INPMSG(I,'Should have a number as arg.')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,AUX,REAL(BEMTGT))
                      IF(AUX.GT.0)THEN
                           BEMTGT=AUX
                      ELSE
                           CALL INPMSG(I+1,
     -                          'Value not at least equal to 0')
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Minimum and maximum number of elements.
            ELSEIF(INPCMP(I,'MIN#IMUM-#NUMBER-#OF-#ELEMENTS')+
     -           INPCMP(I,'MIN#IMUM-#ELEMENTS').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).NE.1)THEN
                      CALL INPMSG(I,'Should have a integer as arg.')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,1,IFAIL1)
                      CALL INPRDI(I+1,IAUX,NBEMMN)
                      IF(IAUX.GE.1)THEN
                           NBEMMN=IAUX
                      ELSE
                           CALL INPMSG(I+1,
     -                          'Value not at least equal to 1')
                      ENDIF
                      INEXT=I+2
                 ENDIF
            ELSEIF(INPCMP(I,'MAX#IMUM-#NUMBER-#OF-#ELEMENTS')+
     -           INPCMP(I,'MAX#IMUM-#ELEMENTS').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).NE.1)THEN
                      CALL INPMSG(I,'Should have a integer as arg.')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,1,IFAIL1)
                      CALL INPRDI(I+1,IAUX,NBEMMX)
                      IF(IAUX.GE.1)THEN
                           NBEMMX=IAUX
                      ELSE
                           CALL INPMSG(I+1,
     -                          'Value not at least equal to 1')
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Periodic copies.
            ELSEIF(INPCMP(I,'PER#IODIC-COPIES').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).NE.1)THEN
                      CALL INPMSG(I,'Should have a integer as arg.')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,1,IFAIL1)
                      CALL INPRDI(I+1,IAUX,1+(NBEMPX+NBEMPY+NBEMPZ)/3)
                      IF(IAUX.GE.1)THEN
                           NBEMPX=IAUX
                           NBEMPY=IAUX
                           NBEMPZ=IAUX
                      ELSE
                           CALL INPMSG(I+1,
     -                          'Value not at least equal to 1')
                      ENDIF
                      INEXT=I+2
                 ENDIF
            ELSEIF(INPCMP(I,'X-PER#IODIC-COPIES').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).NE.1)THEN
                      CALL INPMSG(I,'Should have a integer as arg.')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,1,IFAIL1)
                      CALL INPRDI(I+1,IAUX,NBEMPX)
                      IF(IAUX.GE.1)THEN
                           NBEMPX=IAUX
                      ELSE
                           CALL INPMSG(I+1,
     -                          'Value not at least equal to 1')
                      ENDIF
                      INEXT=I+2
                 ENDIF
            ELSEIF(INPCMP(I,'Y-PER#IODIC-COPIES').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).NE.1)THEN
                      CALL INPMSG(I,'Should have a integer as arg.')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,1,IFAIL1)
                      CALL INPRDI(I+1,IAUX,NBEMPY)
                      IF(IAUX.GE.1)THEN
                           NBEMPY=IAUX
                      ELSE
                           CALL INPMSG(I+1,
     -                          'Value not at least equal to 1')
                      ENDIF
                      INEXT=I+2
                 ENDIF
            ELSEIF(INPCMP(I,'Z-PER#IODIC-COPIES').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.INPTYP(I+1).NE.1)THEN
                      CALL INPMSG(I,'Should have a integer as arg.')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,1,IFAIL1)
                      CALL INPRDI(I+1,IAUX,NBEMPZ)
                      IF(IAUX.GE.1)THEN
                           NBEMPZ=IAUX
                      ELSE
                           CALL INPMSG(I+1,
     -                          'Value not at least equal to 1')
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Start a new model.
            ELSEIF(INPCMP(I,'NEW-#MODEL').NE.0)THEN
                 BEMNEW=1
            ELSEIF(INPCMP(I,'REUSE-#MODEL').NE.0)THEN
                 BEMNEW=0
*   Store inverted matrix ?
            ELSEIF(INPCMP(I,'KEEP-INV#ERTED-#MATRIX').NE.0)THEN
                 BEMINV=1
            ELSEIF(INPCMP(I,'NOKEEP-INV#ERTED-#MATRIX').NE.0)THEN
                 BEMINV=0
*   Angular tolerance.
            ELSEIF(INPCMP(I,'ANG#ULAR-TOL#ERANCE')+
     -           INPCMP(I,'ANG#LE-TOL#ERANCE')+
     -           INPCMP(I,'TOL#ERANCE-ANG#LE')+
     -           INPCMP(I,'TOL#ERANCE-ANG#ULAR').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.
     -                (INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2))THEN
                      CALL INPMSG(I,'Should have a number as arg.')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,AUX,REAL(BEMEPA))
                      IF(AUX.GT.0)THEN
                           BEMEPA=AUX
                      ELSE
                           CALL INPMSG(I+1,
     -                          'Value not at least equal to 0')
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Distance tolerance.
            ELSEIF(INPCMP(I,'DIS#TANCE-TOL#ERANCE')+
     -           INPCMP(I,'TOL#ERANCE-DIS#TANCE').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.
     -                (INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2))THEN
                      CALL INPMSG(I,'Should have a number as arg.')
                      INEXT=I+1
                 ELSE
                      CALL INPCHK(I+1,2,IFAIL1)
                      CALL INPRDR(I+1,AUX,REAL(BEMEPD))
                      IF(AUX.GT.0)THEN
                           BEMEPD=AUX
                      ELSE
                           CALL INPMSG(I+1,
     -                          'Value not at least equal to 0')
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Dump primitives.
            ELSEIF(INPCMP(I,'DUMP-#PRIMITIVES').NE.0)THEN
                 LBDUMP=.TRUE.
            ELSEIF(INPCMP(I,'NODUMP-#PRIMITIVES').NE.0)THEN
                 LBDUMP=.FALSE.
*   Solving method.
            ELSEIF(INPCMP(I,'LU-#INVERSION').NE.0)THEN
                 BEMSLV=0
            ELSEIF(INPCMP(I,'SVD-#INVERSION')+
     -             INPCMP(I,'S#INGULAR-V#ALUE-D#ECOMPOSITION-'//
     -                      '#INVERSION').NE.0)THEN
                 BEMSLV=1
*   Anything else.
            ELSE
                 CALL INPMSG(I,'Not a valid keyword; ignored. ')
            ENDIF
10          CONTINUE
       ENDIF
       CALL INPERR
*** Sort range if needed.
       IF(NBEMMN.GT.NBEMMX)THEN
            IAUX=NBEMMN
            NBEMMN=NBEMMX
            NBEMMX=IAUX
       ENDIF
       END
