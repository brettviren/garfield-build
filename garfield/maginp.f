CDECK  ID>, MAGINP.
       SUBROUTINE MAGINP
*-----------------------------------------------------------------------
*   MAGINP - Routine reading the magnetic field data from input file.
*   VARIABLES : IUNIT       : Unit system (0: internal, 1: Gauss, 2: T)
*   (Last changed on 13/ 2/00.)
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
       INTEGER INPCMP,NWORD,NC,IFAIL1,IFAIL2,I
       REAL SUSGSR,SUSWRR
       EXTERNAL INPCMP
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE MAGINP ///'
*** Print a heading for this section.
       WRITE(*,'(''1'')')
       PRINT *,' ================================================'
       PRINT *,' ==========  Start of B-field input    =========='
       PRINT *,' ================================================'
       PRINT *,' '
*** Start the input loop.
       CALL INPPRM('B field','NEW-PRINT')
10     CONTINUE
       CALL INPWRD(NWORD)
       CALL INPSTR(1,1,STRING,NC)
       IF(NWORD.EQ.0)GOTO 10
*** Return to calling routine if the instruction contains an &.
       IF(STRING(1:1).EQ.'&')THEN
            GOTO 20
*** Magnetic field components.
       ELSEIF(INPCMP(1,'COMP#ONENTS').NE.0)THEN
            CALL MAGCMP
*** Reset the magnetic field.
       ELSEIF(INPCMP(1,'RES#ET').NE.0)THEN
            IF(NWORD.EQ.1)THEN
                 CALL MAGINT
            ELSE
                 DO 30 I=2,NWORD
                 IF(INPCMP(I,'COMP#ONENTS').NE.0)THEN
                      MAGSRC=0
                 ELSEIF(INPCMP(I,'PERM#EABILITY').NE.0)THEN
                      ALFA=0
                 ELSE
                      CALL INPMSG(I,'Not a known item.')
                 ENDIF
30               CONTINUE
            ENDIF
*** Permeability.
       ELSEIF(INPCMP(1,'PERM#EABILITY').NE.0)THEN
**  No arguments, print current values.
            IF(NWORD.EQ.1)THEN
                 WRITE(LUNOUT,'(/''  Currently, the permeabilities'',
     -                '' are set to the following values:'',/,
     -                5X,''Wires: '',E15.8,'' [Arbitrary units],'',/,
     -                5X,''Gas  : '',E15.8,'' [Arbitrary units]'',/,
     -                ''  which leads to Alpha = '',E15.8/)')
     -                SUSWIR,SUSGAS,ALFA
**  One argument: look for the IGNORE keyword, otherwise reject.
            ELSEIF(NWORD.EQ.2)THEN
                 IF(INPCMP(2,'IGN#ORE').NE.0)THEN
                      ALFA=0.0
                      SUSWIR=1.0
                      SUSGAS=1.0
                 ELSE
                      CALL INPMSG(2,'Not a valid option.           ')
                 ENDIF
**  Two arguments: specification of new values.
            ELSEIF(NWORD.EQ.3)THEN
                 CALL INPCHK(2,2,IFAIL1)
                 CALL INPCHK(3,2,IFAIL2)
                 CALL INPRDR(2,SUSWRR,1.0)
                 CALL INPRDR(3,SUSGSR,1.0)
*   Reject negative permeabilities.
                 IF(SUSWRR.LT.0.0.AND.IFAIL1.EQ.0)THEN
                      CALL INPMSG(2,'Wire permeability not > 0.')
                      IFAIL1=1
                 ELSE
                      SUSWIR=SUSWRR
                 ENDIF
                 IF(SUSGSR.LT.0.0.AND.IFAIL2.EQ.0)THEN
                      CALL INPMSG(2,'Gas permeability not > 0.')
                      IFAIL2=1
                 ELSE
                      SUSGAS=SUSGSR
                 ENDIF
*   Calculate ALFA, the coefficient needed in the rest of the program.
                 IF(SUSWIR.LE.0.AND.SUSGAS.LE.0)THEN
                      ALFA=0
                 ELSE
                      ALFA=(SUSWIR-SUSGAS)/(SUSWIR+SUSGAS)
                 ENDIF
**  Strange number of arguments.
            ELSE
                 PRINT *,' !!!!!! MAGINP WARNING : PERMEABILITY'//
     -                ' needs up to 2 arguments ; line is ignored.'
            ENDIF
*** It is not possible to get here if the option was recognised.
       ELSE
            CALL INPSTR(1,1,STRING,NC)
            PRINT *,' !!!!!! MAGINP WARNING : '//STRING(1:NC)//' is'//
     -           ' not a valid instruction ; line is ignored.'
       ENDIF
*** Dump error messages.
       CALL INPERR
*** And return for a new input line.
       GOTO 10
20     CONTINUE
*** Register the amount of CPU time used for reading these data.
       CALL TIMLOG('Reading the magnetic field section:     ')
       END
