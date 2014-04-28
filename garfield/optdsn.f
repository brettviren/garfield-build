CDECK  ID>, OPTDSN.
       SUBROUTINE OPTDSN(ACTION,IREFNO)
*-----------------------------------------------------------------------
*   OPTDSN - Saves and restores those parts of the cell description
*            that are modified during optimisation.
*   VARIABLES : ACTION      : Type of dataset operation.
*               IREFNO      : Record reference number.
*   (Last changed on  9/10/90.)
*-----------------------------------------------------------------------
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
       LOGICAL OPENED
       SAVE IREF
       CHARACTER*(*) ACTION
*   Open the dataset and log the file.
       IF(ACTION.EQ.'OPEN')THEN
            OPEN(UNIT=13,STATUS='SCRATCH',ACCESS='DIRECT',
     -           FORM='UNFORMATTED',RECL=8+4*NWIRE,IOSTAT=IOS,ERR=2020)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ OPTDSN DEBUG   :'',
     -           '' Optimisation file opened on lun 13, lrecl='',I5)')
     -           8+4*NWIRE
            CALL DSNLOG('< Optimisation auxilliary file >','Scratch   ',
     -           'Direct    ','Read/Write')
            IREF=0
*   Close the dataset.
       ELSEIF(ACTION.EQ.'CLOSE')THEN
            INQUIRE(UNIT=13,OPENED=OPENED)
            IF(.NOT.OPENED)THEN
                 PRINT *,' ###### OPTDSN ERROR   : Dataset not opened;',
     -                ' program bug - please report.'
            ELSE
                 CLOSE(UNIT=13,IOSTAT=IOS,ERR=2030)
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ OPTDSN DEBUG   :'',
     -                '' The optimisation file has been closed.'')')
            ENDIF
*   Save a record.
       ELSEIF(ACTION.EQ.'SAVE')THEN
            IREFNO=0
            INQUIRE(UNIT=13,OPENED=OPENED)
            IF(.NOT.OPENED)THEN
                 PRINT *,' ###### OPTDSN ERROR   : Dataset not;'//
     -                ' opened; program bug - please report.'
            ELSE
                 IREF=IREF+1
                 IF(IREF.GT.1000)THEN
                      PRINT *,' !!!!!! OPTDSN WARNING   : Cannot'//
     -                     ' be saved because the dataset if full.'
                      RETURN
                 ENDIF
                 WRITE(13,REC=IREF,IOSTAT=IOS,ERR=2010)
     -                V0,(V(I),I=1,NWIRE)
                 IREFNO=IREF
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ OPTDSN DEBUG   :'',
     -                '' Record '',I3,'' has been saved.'')') IREF
            ENDIF
*   Retrieve a record.
       ELSEIF(ACTION.EQ.'RESTORE')THEN
            INQUIRE(UNIT=13,OPENED=OPENED)
            IF(.NOT.OPENED)THEN
                 PRINT *,' ###### OPTDSN ERROR   : Dataset not yet;'//
     -                ' opened program bug - please report.'
            ELSE
                 IF(IREFNO.LE.0.OR.IREFNO.GT.IREF)THEN
                      PRINT *,' !!!!!! OPTDSN WARNING : Illegal'//
     -                     ' reference number.'
                 ELSE
                      READ(13,REC=IREFNO,IOSTAT=IOS,ERR=2010)
     -                     V0,(V(I),I=1,NWIRE)
                      IF(LDEBUG)WRITE(LUNOUT,
     -                     '(''  ++++++ OPTDSN DEBUG   : Record '',I3,
     -                     '' has been retrieved.'')') IREF
                 ENDIF
            ENDIF
*   Invalid instruction.
       ELSE
            PRINT *,' ###### OPTDSN ERROR   : Invalid action arg ',
     -           ACTION,' received; program bug - please report.'
       ENDIF
       RETURN
*** Handle I/O problems.
2010   CONTINUE
       PRINT *,' !!!!!! OPTDSN WARNING : I/O error while saving or'//
     -      ' restoring a modification record.'
       CALL INPIOS(IOS)
       RETURN
2020   CONTINUE
       PRINT *,' !!!!!! OPTDSN WARNING : Error opening a modification'//
     -      ' dataset ; do not use SAVE and RESTORE.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=13,IOSTAT=IOS,ERR=2030)
       RETURN
2030   CONTINUE
       PRINT *,' !!!!!! OPTDSN WARNING : Error closing a modification'//
     -      ' dataset ; probably harmless.'
       CALL INPIOS(IOS)
       END
