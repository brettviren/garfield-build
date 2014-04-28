CDECK  ID>, BEMPRI.
       SUBROUTINE BEMPRI(IELEM, NVERTEX,
     -      XVERT, YVERT, ZVERT, XNORM, YNORM, ZNORM,
     -      IVOL1, IVOL2, IFAIL)
*-----------------------------------------------------------------------
*   BEMPRI - Returns one surface panel at the time
*   (Last changed on 14/10/11.)
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
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
       INTEGER IELEM, NVERTEX, IVOL1, IVOL2, IFAIL, NC1,NC2,NC3,I,IWIRE,
     -      IVOL,IREF
       DOUBLE PRECISION XVERT(*), YVERT(*), ZVERT(*),
     -      XNORM, YNORM, ZNORM,R,ZL,X0,Y0,Z0,FNORM,A,B,C
       CHARACTER*15 AUX1,AUX2,AUX3
*** Check in which domain we are.
       IF(IELEM.GT.NBEM)THEN
*** Count the wire-shaped cylinders.
            IWIRE=0
            DO 10 IVOL=1,NSOLID
            IF(ISOLTP(IVOL).NE.1)GOTO 10
            IF(CBUF(ISTART(IVOL)+9).LT.-0.5)IWIRE=IWIRE+1
            IF(IWIRE.EQ.IELEM-NBEM)THEN
                 IREF=ISTART(IVOL)
                 R= CBUF(IREF+1)
                 ZL=ABS(CBUF(IREF+2))
                 X0=CBUF(IREF+3)
                 Y0=CBUF(IREF+4)
                 Z0=CBUF(IREF+5)
                 FNORM=SQRT(CBUF(IREF+6)**2+CBUF(IREF+7)**2+
     -                CBUF(IREF+8)**2)
                 IF(FNORM.LE.0)THEN
                      PRINT *,' !!!!!! BEMPRI WARNING : Cylinder ',IVOL,
     -                     ' has a zero norm direction vector; skipped.'
                      RETURN
                 ENDIF
                 A= CBUF(IREF+6)/FNORM
                 B= CBUF(IREF+7)/FNORM
                 C= CBUF(IREF+8)/FNORM
                 NVERTEX=2
                 XVERT(1)=X0-ZL*A
                 YVERT(1)=Y0-ZL*B
                 ZVERT(1)=Z0-ZL*C
                 XVERT(2)=X0+ZL*A
                 YVERT(2)=Y0+ZL*B
                 ZVERT(2)=Z0+ZL*C
                 XNORM=R
                 YNORM=0
                 ZNORM=0
                 IVOL1=IVOL
                 IVOL2=0
                 IFAIL=0
                 RETURN
            ENDIF
10          CONTINUE
            PRINT *,' !!!!!! BEMPRI WARNING : Did not find the'//
     -           ' cylinder corresponding to element ',IELEM
            RETURN
       ELSEIF(IELEM.LT.1)THEN
            PRINT *,' !!!!!! BEMPRI WARNING : Element reference ',
     -           IELEM,' out of range.'
            RETURN
       ELSEIF(IREFB1(IELEM).EQ.0)THEN
            PRINT *,' !!!!!! BEMPRI WARNING : Element reference ',
     -           IELEM,' points to non-existent panel.'
            RETURN
       ENDIF
*** Retrieve the element.
       CALL BEMBU1('READ',IREFB1(IELEM),NVERTEX,XVERT,YVERT,ZVERT,
     -      XNORM,YNORM,ZNORM,
     -      IVOL2,IVOL1,IFAIL)
*** Debugging output.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ BEMPRI DEBUG   : '',
     -           ''Primitive '',I5,'' with '',I3,'' vertices''/
     -           26X,''Volume references: '',I5,2X,I5/
     -           26X,9X,''x [cm]'',11X,''y [cm]'',11X,''z [cm]'')')
     -           IELEM,NVERTEX,IVOL1,IVOL2
            DO 20 I=1,NVERTEX
            CALL OUTFMT(REAL(XVERT(I)),2,AUX1,NC1,'RIGHT')
            CALL OUTFMT(REAL(YVERT(I)),2,AUX2,NC2,'RIGHT')
            CALL OUTFMT(REAL(ZVERT(I)),2,AUX3,NC3,'RIGHT')
            WRITE(LUNOUT,'(26X,A15,2X,A15,2X,A15)') AUX1,AUX2,AUX3
20          CONTINUE
            WRITE(LUNOUT,'(26X,8X,''xn [cm]'',10X,''yn [cm]'',
     -           10X,''zn [cm]'')')
            CALL OUTFMT(REAL(XNORM),2,AUX1,NC1,'RIGHT')
            CALL OUTFMT(REAL(YNORM),2,AUX2,NC2,'RIGHT')
            CALL OUTFMT(REAL(ZNORM),2,AUX3,NC3,'RIGHT')
            WRITE(LUNOUT,'(26X,A15,2X,A15,2X,A15)') AUX1,AUX2,AUX3
       ENDIF
*** Dump
       IF(LBDUMP)THEN
            OPEN(UNIT=12,FILE='bempri.dump',ACCESS='APPEND')
            WRITE(12,'(4I5)') IELEM,NVERTEX,IVOL1,IVOL2
            DO 30 I=1,NVERTEX
            WRITE(12,'(3E12.5)') XVERT(I),YVERT(I),ZVERT(I)
30          CONTINUE
            WRITE(12,'(3E12.5)') XNORM,YNORM,ZNORM
            CLOSE(UNIT=12)
       ENDIF
       END
