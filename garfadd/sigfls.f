CDECK  ID>, SIGFLS.
       SUBROUTINE SIGFLS(XPOS,YPOS,ZPOS,EXSUM,EYSUM,EZSUM,ISW)
*-----------------------------------------------------------------------
*   SIGFLS - Sums the weighting field components at (XPOS,YPOS,ZPOS).
*   (Last changed on 13/ 1/11.)
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
       REAL EXMAP,EYMAP,EZMAP,VMAP,EWXMAP,EWYMAP,EWZMAP,VWMAP,
     -      BXMAP,BYMAP,BZMAP,
     -      XMAP,YMAP,ZMAP,XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,
     -      VMMIN,VMMAX,EPSMAT,EPSSUR,XFMOFF,YFMOFF,ZFMOFF
       INTEGER MATMAP,NMAP,NEPS,MAPORD,MAPTYP,IDRMAT,INDEWS,
     -      NWMAP
       LOGICAL MAPFLG,LMAPPL,SETAX,SETAY,SETAZ,ELMDGN,LSFDER
       CHARACTER EWSTYP
       CHARACTER*10 MATSRC
       COMMON /FLDMAP/ VMAP(MXMAP,10),VWMAP(MXMAP,10,MXWMAP),
     -      EXMAP(MXMAP,10),EYMAP(MXMAP,10),EZMAP(MXMAP,10),
     -      EWXMAP(MXMAP,10,MXWMAP),EWYMAP(MXMAP,10,MXWMAP),
     -      EWZMAP(MXMAP,10,MXWMAP),
     -      BXMAP(MXMAP,10),BYMAP(MXMAP,10),BZMAP(MXMAP,10),
     -      XMAP(MXMAP,10),YMAP(MXMAP,10),ZMAP(MXMAP,10),
     -      XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,VMMIN,VMMAX,
     -      XFMOFF,YFMOFF,ZFMOFF,
     -      EPSMAT(MXEPS),EPSSUR(MXEPS),MATMAP(MXMAP),
     -      NMAP,NEPS,MAPORD,MAPTYP,IDRMAT,INDEWS(MXWMAP),NWMAP,
     -      MAPFLG(10+4*MXWMAP),ELMDGN(MXMAP),
     -      LMAPPL,SETAX,SETAY,SETAZ,LSFDER
       COMMON /FLDCHR/ EWSTYP(MXWMAP),MATSRC
       LOGICAL FPERX,FPERY,LCROSS,TRASET,TRAFLG,LITAIL,LDTAIL,LRTAIL,
     -      LEPULS,LIPULS,SIGSET,RESSET
       INTEGER NPAIR,ICLUST,NFOUR,MFEXP,MXMIN,MXMAX,
     -      MYMIN,MYMAX,NTRBNK,ITRMAJ,NTIME,NORIA,
     -      NASIMP,JIORD,NISIMP,NMQUAD,NCANG,IENANG
       REAL TIMSIG,SIGNAL,TCLUST,SCLUST,ACLUST,BCLUST,FCLUST,
     -      AVALAN,TSTART,TDEV,PRSTHR,
     -      TRABNK,TRAVEC
       CHARACTER*(MXCHAR) FCNANG
       CHARACTER*12 AVATYP
       CHARACTER*3 FCELTP
       COMMON /SIGDAT/ TIMSIG(MXLIST),SIGNAL(MXLIST,MXSW,2),
     -      AVALAN(2),TRAVEC(MXLIST),
     -      TRABNK(MXLIST,9),TSTART,TDEV,PRSTHR,
     -      TCLUST,SCLUST,ACLUST,BCLUST,FCLUST,ICLUST,NPAIR,
     -      NFOUR,ITRMAJ,JIORD,NISIMP,NMQUAD,IENANG,NTIME,NORIA,
     -      MFEXP,MXMIN,MXMAX,MYMIN,MYMAX,NTRBNK,NASIMP,NCANG,
     -      TRASET,TRAFLG(9),FPERX,FPERY,LCROSS,LITAIL,LDTAIL,LRTAIL,
     -      LEPULS,LIPULS,SIGSET,RESSET
       COMMON /SIGCHR/ FCELTP,AVATYP,FCNANG
       COMPLEX SIGMAT
       REAL QPLANE,EWXCOR,EWYCOR
       INTEGER IWORK,DUMMY
       COMMON /MATRIX/ SIGMAT(MXWIRE,MXWIRE),QPLANE(5,MXWIRE),
     -      IWORK(MXWIRE),DUMMY(2*MXWIRE+6)
       COMMON /SPLDAT/ EWXCOR(5),EWYCOR(5)
       REAL XPOS,YPOS,ZPOS,EX,EY,EZ,EXSUM,EYSUM,EZSUM
       INTEGER MX,MY,IFAIL,IW,ISW,IPLANE,IWMAP,ISTRIP
*** Preset the sums.
       EXSUM=0
       EYSUM=0
       EZSUM=0
*** Make sure that the signal matrices are present.
       IF(.NOT.SIGSET)THEN
            PRINT *,' !!!!!! SIGFLS WARNING : Initialisation of'//
     -           ' signal calculation not yet done; no field.'
            RETURN
       ENDIF
*** Loop over the signal layers.
       DO 10 MX=MXMIN,MXMAX
       DO 20 MY=MYMIN,MYMAX
*** Load the layers of the wire matrices.
       CALL IONIO(MX,MY,2,0,IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! SIGFLS WARNING : Wire matrix'//
     -           ' store error; no weighting field returned.'
            EXSUM=0
            EYSUM=0
            EZSUM=0
            RETURN
       ENDIF
*** Loop over all wires.
       DO 30 IW=1,NWIRE
*   Pick out those wires that are part of this read out group.
       IF(INDSW(IW).EQ.ISW)THEN
            EX=0
            EY=0
            EZ=0
            IF(FCELTP.EQ.'A  ')THEN
                 CALL IONA00(XPOS,YPOS,EX,EY,MX,MY,IW)
            ELSEIF(FCELTP.EQ.'B2X')THEN
                 CALL IONB2X(XPOS,YPOS,EX,EY   ,MY,IW)
            ELSEIF(FCELTP.EQ.'B2Y')THEN
                 CALL IONB2Y(XPOS,YPOS,EX,EY,MX   ,IW)
            ELSEIF(FCELTP.EQ.'C2X')THEN
                 CALL IONC2X(XPOS,YPOS,EX,EY      ,IW)
            ELSEIF(FCELTP.EQ.'C2Y')THEN
                 CALL IONC2Y(XPOS,YPOS,EX,EY      ,IW)
            ELSEIF(FCELTP.EQ.'C3 ')THEN
                 CALL IONC30(XPOS,YPOS,EX,EY      ,IW)
            ELSEIF(FCELTP.EQ.'D1 ')THEN
                 CALL IOND10(XPOS,YPOS,EX,EY      ,IW)
            ELSEIF(FCELTP.EQ.'D3 ')THEN
                 CALL IOND30(XPOS,YPOS,EX,EY      ,IW)
            ELSE
                 PRINT *,' ###### SIGFLS ERROR   : Unknown signal'//
     -                ' field type ',FCELTP,' received; program error.'
                 PRINT *,'                         Encountered for'//
     -                ' Wire ',IW,' indsw=',INDSW(IW)
                 EXSUM=0
                 EYSUM=0
                 EZSUM=0
                 RETURN
            ENDIF
            EXSUM=EXSUM+EX
            EYSUM=EYSUM+EY
            EZSUM=EZSUM+EZ
       ENDIF
30     CONTINUE
*** Load the layers of the plane matrices.
       CALL IPLIO(MX,MY,2,IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! SIGFLS WARNING : Plane matrix'//
     -           ' store error; no weighting field returned.'
            EXSUM=0
            EYSUM=0
            EZSUM=0
            RETURN
       ENDIF
*** Loop over all planes.
       DO 40 IPLANE=1,5
*   Pick out those wires that are part of this read out group.
       IF(INDPLA(IPLANE).EQ.ISW)THEN
            EX=0
            EY=0
            EZ=0
            IF(FCELTP.EQ.'A  ')THEN
                 CALL IPLA00(XPOS,YPOS,EX,EY,MX,MY,IPLANE)
            ELSEIF(FCELTP.EQ.'B2X')THEN
                 CALL IPLB2X(XPOS,YPOS,EX,EY   ,MY,IPLANE)
            ELSEIF(FCELTP.EQ.'B2Y')THEN
                 CALL IPLB2Y(XPOS,YPOS,EX,EY,MX   ,IPLANE)
            ELSEIF(FCELTP.EQ.'C2X')THEN
                 CALL IPLC2X(XPOS,YPOS,EX,EY      ,IPLANE)
            ELSEIF(FCELTP.EQ.'C2Y')THEN
                 CALL IPLC2Y(XPOS,YPOS,EX,EY      ,IPLANE)
            ELSEIF(FCELTP.EQ.'C3 ')THEN
                 CALL IPLC30(XPOS,YPOS,EX,EY      ,IPLANE)
            ELSEIF(FCELTP.EQ.'D1 ')THEN
                 CALL IPLD10(XPOS,YPOS,EX,EY      ,IPLANE)
            ELSEIF(FCELTP.EQ.'D3 ')THEN
                 CALL IPLD30(XPOS,YPOS,EX,EY      ,IPLANE)
            ELSE
                 PRINT *,' ###### SIGFLS ERROR   : Unknown signal'//
     -                ' field type ',FCELTP,' received; program error.'
                 PRINT *,'                         Encountered for'//
     -                ' Plane ',IPLANE,' indpla=',INDPLA(IPLANE)
                 EXSUM=0
                 EYSUM=0
                 EZSUM=0
                 RETURN
            ENDIF
            EXSUM=EXSUM+EX
            EYSUM=EYSUM+EY
            EZSUM=EZSUM+EZ
       ENDIF
40     CONTINUE
*** Next signal layer.
20     CONTINUE
10     CONTINUE
*** Add the field due to the planes themselves.
       DO 50 IPLANE=1,5
       IF(INDPLA(IPLANE).EQ.ISW)THEN
            EXSUM=EXSUM+EWXCOR(IPLANE)
            EYSUM=EYSUM+EWYCOR(IPLANE)
       ENDIF
50     CONTINUE
*** Add the field map, if appropriate.
       DO 60 IWMAP=1,NWMAP
       IF(INDEWS(IWMAP).EQ.ISW)THEN
            CALL IONFMP(XPOS,YPOS,ZPOS,EX,EY,EZ,IWMAP)
            EXSUM=EXSUM+EX
            EYSUM=EYSUM+EY
            EZSUM=EZSUM+EZ
       ENDIF
60     CONTINUE
*** Add BEM fields, if appropriate.
       IF(BEMSET)THEN
            CALL IONBEM(XPOS,YPOS,ZPOS,EX,EY,EZ,IWFBEM(ISW))
            EXSUM=EXSUM+EX
            EYSUM=EYSUM+EY
            EZSUM=EZSUM+EZ
       ENDIF
*** Add strips, if there are any.
       DO 70 IPLANE=1,5
       DO 80 ISTRIP=1,NPSTR1(IPLANE)
       IF(INDST1(IPLANE,ISTRIP).EQ.ISW)THEN
            CALL IONEST(XPOS,YPOS,ZPOS,EX,EY,EZ,IPLANE,ISTRIP,1)
            EXSUM=EXSUM+EX
            EYSUM=EYSUM+EY
            EZSUM=EZSUM+EZ
       ENDIF
80     CONTINUE
       DO 90 ISTRIP=1,NPSTR2(IPLANE)
       IF(INDST2(IPLANE,ISTRIP).EQ.ISW)THEN
            CALL IONEST(XPOS,YPOS,ZPOS,EX,EY,EZ,IPLANE,ISTRIP,2)
            EXSUM=EXSUM+EX
            EYSUM=EYSUM+EY
            EZSUM=EZSUM+EZ
       ENDIF
90     CONTINUE
70     CONTINUE
       END
