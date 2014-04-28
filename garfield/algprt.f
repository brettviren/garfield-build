CDECK  ID>, ALGPRT.
       SUBROUTINE ALGPRT(ISTART,IEND)
*-----------------------------------------------------------------------
*   ALGPRT - Routine printing the instructions produced by ALGPRE in a
*            somewhat legible manner.
*   (Last changed on 15/11/08.)
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
       INTEGER INS(MXINS,4),ALGENT(MXALGE,10),MODREG(MXCONS:MXREG),
     -      ISYNCH,IINS0,ICONS0,ARGREF(MXARG,2),MODARG(MXARG),
     -      NREG,NCONS,NINS,NERR,NRES,NALGE,IENTRL,NAERR(100)
       REAL REG(MXCONS:MXREG),ARG(MXARG),EXPMAX
       PARAMETER(EXPMAX=40.0)
       LOGICAL EXEC(MXINS),LIGUND,LINUND
       COMMON /ALGDAT/ REG,ARG,MODARG,ARGREF,INS,MODREG,ALGENT,
     -      NREG,NCONS,NINS,NERR,NAERR,
     -      NRES,NALGE,IENTRL,ISYNCH,IINS0,ICONS0,EXEC,LIGUND,LINUND
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*132 AUX
       CHARACTER*(MXINCH) OUTPUT
       INTEGER ISTART,IEND,NO,NNO,I,J,NCAUX
       REAL EPS
*** Identify the routine
       IF(LIDENT)PRINT *,' /// ROUTINE ALGPRT ///'
       EPS=1.0E-5
*** Loop over the instructions.
       DO 10 I=ISTART,IEND
*   Write the instruction number to the output string
       WRITE(OUTPUT,'(''Ins%'',I4,'':%'')') I
       NO=10
*   Do nothing
       IF(INS(I,2).EQ.-1)THEN
            OUTPUT(NO+1:NO+10)='Do%nothing'
            NO=NO+10
*   The instruction is a RESULT type statement
       ELSEIF(INS(I,2).EQ.0)THEN
            IF(INS(I,3).GT.0)THEN
                 WRITE(AUX,'(''Result%'',I4,''%=%R'',I4)')
     -                INS(I,4),INS(I,3)
                 OUTPUT(NO+1:NO+19)=AUX(1:19)
                 NO=NO+19
            ELSE
                 WRITE(AUX,'(''Result%'',I4,''%=%'')') INS(I,4)
                 OUTPUT(NO+1:NO+14)=AUX(1:14)
                 NO=NO+14
                 CALL OUTFMT(REG(INS(I,3)),MODREG(INS(I,3)),
     -                AUX,NCAUX,'LEFT')
                 OUTPUT(NO+1:NO+NCAUX)=AUX(1:NCAUX)
                 NO=NO+NCAUX
            ENDIF
*   The instruction is a real- or logical-arithmetic expression
       ELSEIF((INS(I,2).GE.1.AND.INS(I,2).LE.5).OR.
     -      (INS(I,2).GE.10.AND.INS(I,2).LE.17))THEN
            IF(INS(I,4).GE.0)THEN
                 WRITE(AUX,'(''R'',I4,''%:=%'')') INS(I,4)
                 OUTPUT(NO+1:NO+9)=AUX(1:9)
                 NO=NO+9
            ELSE
                 WRITE(AUX,'(''R('',I4,'')%:=%'')') INS(I,4)
                 OUTPUT(NO+1:NO+11)=AUX(1:11)
                 NO=NO+11
            ENDIF
            IF(INS(I,1).GT.0)THEN
                 WRITE(AUX,'(''R'',I4,''%'')') INS(I,1)
                 OUTPUT(NO+1:NO+6)=AUX(1:6)
                 NO=NO+6
            ELSE
                 CALL OUTFMT(REG(INS(I,1)),MODREG(INS(I,1)),
     -                AUX,NCAUX,'LEFT')
                 OUTPUT(NO+1:NO+NCAUX+1)=AUX(1:NCAUX)//'%'
                 NO=NO+NCAUX+1
            ENDIF
            IF(INS(I,2).EQ.1) OUTPUT(NO+1:NO+2)='+%'
            IF(INS(I,2).EQ.2) OUTPUT(NO+1:NO+2)='-%'
            IF(INS(I,2).EQ.3) OUTPUT(NO+1:NO+2)='*%'
            IF(INS(I,2).EQ.4) OUTPUT(NO+1:NO+2)='/%'
            IF(INS(I,2).EQ.5) OUTPUT(NO+1:NO+3)='**%'
            IF(INS(I,2).EQ.10)OUTPUT(NO+1:NO+2)='=%'
            IF(INS(I,2).EQ.11)OUTPUT(NO+1:NO+2)='#%'
            IF(INS(I,2).EQ.12)OUTPUT(NO+1:NO+2)='<%'
            IF(INS(I,2).EQ.13)OUTPUT(NO+1:NO+3)='<=%'
            IF(INS(I,2).EQ.14)OUTPUT(NO+1:NO+2)='>%'
            IF(INS(I,2).EQ.15)OUTPUT(NO+1:NO+3)='>=%'
            IF(INS(I,2).EQ.16)OUTPUT(NO+1:NO+2)='&%'
            IF(INS(I,2).EQ.17)OUTPUT(NO+1:NO+2)='|%'
            NO=NO+2
            IF(INS(I,2).EQ.5.OR.INS(I,2).EQ.13.OR.INS(I,2).EQ.15)NO=NO+1
            IF(INS(I,3).GT.0)THEN
                 WRITE(AUX,'(''R'',I4)') INS(I,3)
                 OUTPUT(NO+1:NO+5)=AUX(1:5)
                 NO=NO+5
            ELSE
                 CALL OUTFMT(REG(INS(I,3)),MODREG(INS(I,3)),
     -                AUX,NCAUX,'LEFT')
                 OUTPUT(NO+1:NO+NCAUX+1)=AUX(1:NCAUX)//'%'
                 NO=NO+NCAUX+1
            ENDIF
*   The instruction is a function
       ELSEIF(INS(I,2).EQ.6)THEN
            IF(INS(I,4).GE.0)THEN
                 WRITE(AUX,'(''R'',I4,''%:=%'')') INS(I,4)
                 OUTPUT(NO+1:NO+9)=AUX(1:9)
                 NO=NO+9
            ELSE
                 WRITE(AUX,'(''R('',I4,'')%:=%'')') INS(I,4)
                 OUTPUT(NO+1:NO+11)=AUX(1:11)
                 NO=NO+11
            ENDIF
            IF(INS(I,1).EQ.-12)THEN
                 OUTPUT(NO+1:NO+7)='Number('
                 NO=NO+7
            ELSEIF(INS(I,1).EQ.-11)THEN
                 OUTPUT(NO+1:NO+9)='Trailing('
                 NO=NO+9
            ELSEIF(INS(I,1).EQ.-9)THEN
                 OUTPUT(NO+1:NO+8)='arctanh('
                 NO=NO+8
            ELSEIF(INS(I,1).EQ.-8)THEN
                 OUTPUT(NO+1:NO+8)='arccosh('
                 NO=NO+8
            ELSEIF(INS(I,1).EQ.-7)THEN
                 OUTPUT(NO+1:NO+8)='arcsinh('
                 NO=NO+8
            ELSEIF(INS(I,1).EQ.-6)THEN
                 OUTPUT(NO+1:NO+1)='-'
                 NO=NO+1
            ELSEIF(INS(I,1).EQ.-5)THEN
                 OUTPUT(NO+1:NO+5)='sqrt('
                 NO=NO+5
            ELSEIF(INS(I,1).EQ.-4)THEN
                 OUTPUT(NO+1:NO+6)='arctan('
                 NO=NO+6
            ELSEIF(INS(I,1).EQ.-3)THEN
                 OUTPUT(NO+1:NO+6)='arccos('
                 NO=NO+6
            ELSEIF(INS(I,1).EQ.-2)THEN
                 OUTPUT(NO+1:NO+6)='arcsin('
                 NO=NO+6
            ELSEIF(INS(I,1).EQ.-1)THEN
                 OUTPUT(NO+1:NO+4)='log('
                 NO=NO+4
            ELSEIF(INS(I,1).EQ.+1)THEN
                 OUTPUT(NO+1:NO+4)='exp('
                 NO=NO+4
            ELSEIF(INS(I,1).EQ.+2)THEN
                 OUTPUT(NO+1:NO+4)='sin('
                 NO=NO+4
            ELSEIF(INS(I,1).EQ.+3)THEN
                 OUTPUT(NO+1:NO+4)='cos('
                 NO=NO+4
            ELSEIF(INS(I,1).EQ.+4)THEN
                 OUTPUT(NO+1:NO+4)='tan('
                 NO=NO+4
            ELSEIF(INS(I,1).EQ.+5)THEN
                 OUTPUT(NO+1:NO+1)='|'
                 NO=NO+1
            ELSEIF(INS(I,1).EQ.+6)THEN
                 OUTPUT(NO+1:NO+1)='+'
                 NO=NO+1
            ELSEIF(INS(I,1).EQ.+7)THEN
                 OUTPUT(NO+1:NO+5)='sinh('
                 NO=NO+5
            ELSEIF(INS(I,1).EQ.+8)THEN
                 OUTPUT(NO+1:NO+5)='cosh('
                 NO=NO+5
            ELSEIF(INS(I,1).EQ.+9)THEN
                 OUTPUT(NO+1:NO+5)='tanh('
                 NO=NO+5
            ELSEIF(INS(I,1).EQ.+10)THEN
                 OUTPUT(NO+1:NO+4)='not('
                 NO=NO+4
            ELSEIF(INS(I,1).EQ.+11)THEN
                 OUTPUT(NO+1:NO+7)='Entier('
                 NO=NO+7
            ELSEIF(INS(I,1).EQ.+12)THEN
                 OUTPUT(NO+1:NO+7)='String('
                 NO=NO+7
            ELSEIF(INS(I,1).EQ.+13)THEN
                 OUTPUT(NO+1:NO+4)='Sum('
                 NO=NO+4
            ELSEIF(INS(I,1).EQ.+14)THEN
                 OUTPUT(NO+1:NO+8)='Product('
                 NO=NO+8
            ELSEIF(INS(I,1).EQ.+15)THEN
                 OUTPUT(NO+1:NO+10)='Reference('
                 NO=NO+10
            ELSEIF(INS(I,1).EQ.+16)THEN
                 OUTPUT(NO+1:NO+7)='Global('
                 NO=NO+7
            ELSEIF(INS(I,1).EQ.+17)THEN
                 OUTPUT(NO+1:NO+5)='Type('
                 NO=NO+5
            ELSEIF(INS(I,1).EQ.+18)THEN
                 OUTPUT(NO+1:NO+7)='Landau('
                 NO=NO+7
            ELSEIF(INS(I,1).EQ.+19)THEN
                 OUTPUT(NO+1:NO+8)='Minimum('
                 NO=NO+8
            ELSEIF(INS(I,1).EQ.+20)THEN
                 OUTPUT(NO+1:NO+8)='Maximum('
                 NO=NO+8
            ELSEIF(INS(I,1).EQ.+21)THEN
                 OUTPUT(NO+1:NO+19)='Random_uniform[0,1]'
                 NO=NO+19
                 GOTO 30
            ELSEIF(INS(I,1).EQ.+22)THEN
                 OUTPUT(NO+1:NO+20)='Random_Gaussian(0,1)'
                 NO=NO+20
                 GOTO 30
            ELSEIF(INS(I,1).EQ.+23)THEN
                 OUTPUT(NO+1:NO+19)='Random_exponential('
                 NO=NO+19
            ELSEIF(INS(I,1).EQ.+24)THEN
                 OUTPUT(NO+1:NO+15)='Random_Poisson('
                 NO=NO+15
            ELSEIF(INS(I,1).EQ.+25)THEN
                 OUTPUT(NO+1:NO+13)='Random_Landau'
                 NO=NO+13
                 GOTO 30
            ELSEIF(INS(I,1).EQ.+26)THEN
                 OUTPUT(NO+1:NO+13)='Random_Polya('
                 NO=NO+13
            ELSEIF(INS(I,1).EQ.+27)THEN
                 OUTPUT(NO+1:NO+15)='Random_function'
                 NO=NO+15
                 GOTO 30
            ELSEIF(INS(I,1).EQ.+28)THEN
                 OUTPUT(NO+1:NO+17)='Random_histogram('
                 NO=NO+17
            ELSEIF(INS(I,1).EQ.+29)THEN
                 OUTPUT(NO+1:NO+13)='Random_gamma('
                 NO=NO+13
            ELSEIF(INS(I,1).EQ.+30)THEN
                 OUTPUT(NO+1:NO+13)='Random_Laplace('
                 NO=NO+15
            ELSEIF(INS(I,1).EQ.+40)THEN
                 OUTPUT(NO+1:NO+4)='Row('
                 NO=NO+4
            ELSEIF(INS(I,1).EQ.+41)THEN
                 OUTPUT(NO+1:NO+5)='Mean('
                 NO=NO+5
            ELSEIF(INS(I,1).EQ.+42)THEN
                 OUTPUT(NO+1:NO+4)='RMS('
                 NO=NO+4
            ELSEIF(INS(I,1).EQ.+43)THEN
                 OUTPUT(NO+1:NO+5)='Size('
                 NO=NO+5
            ELSEIF(INS(I,1).EQ.+44)THEN
                 OUTPUT(NO+1:NO+7)='Zeroes('
                 NO=NO+7
            ELSEIF(INS(I,1).EQ.+45)THEN
                 OUTPUT(NO+1:NO+5)='Ones('
                 NO=NO+5
            ELSEIF(INS(I,1).EQ.+46)THEN
                 OUTPUT(NO+1:NO+6)='Exist('
                 NO=NO+6
            ELSEIF(INS(I,1).EQ.+47)THEN
                 OUTPUT(NO+1:NO+6)='Gamma('
                 NO=NO+6
            ELSEIF(INS(I,1).EQ.+48)THEN
                 OUTPUT(NO+1:NO+9)='LogGamma('
                 NO=NO+9
            ELSEIF(INS(I,1).EQ.+49)THEN
                 OUTPUT(NO+1:NO+8)='Reverse('
                 NO=NO+8
            ELSEIF(INS(I,1).EQ.+51)THEN
                 OUTPUT(NO+1:NO+17)='String_reference('
                 NO=NO+17
            ELSEIF(INS(I,1).EQ.+54)THEN
                 OUTPUT(NO+1:NO+20)='Histogram_reference('
                 NO=NO+20
            ELSEIF(INS(I,1).EQ.+55)THEN
                 OUTPUT(NO+1:NO+17)='Matrix_reference('
                 NO=NO+17
            ELSE
                 OUTPUT(NO+1:NO+20)='<Unknown%function>%('
                 NO=NO+20
            ENDIF
            IF(INS(I,3).GT.0)THEN
                 WRITE(AUX,'(''R'',I4)') INS(I,3)
                 OUTPUT(NO+1:NO+6)=AUX(1:5)
                 NO=NO+5
            ELSE
                 CALL OUTFMT(REG(INS(I,3)),MODREG(INS(I,3)),
     -                AUX,NCAUX,'LEFT')
                 OUTPUT(NO+1:NO+NCAUX)=AUX(1:NCAUX)
                 NO=NO+NCAUX
            ENDIF
            IF(INS(I,1).NE.+5.AND.ABS(INS(I,1)).NE.+6)THEN
                 OUTPUT(NO+1:NO+1)=')'
                 NO=NO+1
            ELSEIF(INS(I,1).EQ.+5)THEN
                 OUTPUT(NO+1:NO+1)='|'
                 NO=NO+1
            ENDIF
30          CONTINUE
*   The instruction is an (un)conditional RETURN, EXIT or QUIT.
       ELSEIF(INS(I,2).EQ.-9)THEN
            IF(INS(I,1).GT.0)THEN
                 WRITE(AUX,'(''If%R'',I4,''%Then%'')') INS(I,1)
                 OUTPUT(NO+1:NO+14)=AUX(1:14)
                 NO=NO+14
            ELSEIF(ABS(REG(INS(I,1))).LT.EPS)THEN
                 OUTPUT(NO+1:NO+6)='Never%'
                 NO=NO+6
            ELSEIF(ABS(REG(INS(I,1))-1.0).LT.EPS)THEN
                 OUTPUT(NO+1:NO+7)='Always%'
                 NO=NO+7
            ELSE
                 OUTPUT(NO+1:NO+35)=
     -                'If%<invalid%logical%constant>%Then%'
                 NO=NO+35
            ENDIF
            IF(INS(I,3).EQ.0)THEN
                 OUTPUT(NO+1:NO+6)='Return'
                 NO=NO+6
            ELSEIF(INS(I,3).EQ.1)THEN
                 OUTPUT(NO+1:NO+4)='Exit'
                 NO=NO+4
            ELSEIF(INS(I,3).EQ.2)THEN
                 OUTPUT(NO+1:NO+4)='Stop'
                 NO=NO+4
            ELSE
                 OUTPUT(NO+1:NO+27)='Return%with%invalid%operand'
                 NO=NO+27
            ENDIF
*   The instruction is a RETURN by means of a GOTO.
       ELSEIF(INS(I,1).EQ.-1.AND.INS(I,2).EQ.7.AND.INS(I,3).EQ.0)THEN
            OUTPUT(NO+1:NO+28)='Return%by%out-of-bounds%Goto'
            NO=NO+28
*   The instruction is an (un)conditional GOTO
       ELSEIF(INS(I,2).EQ.7)THEN
            IF(INS(I,1).GT.0)THEN
                 WRITE(AUX,'(''If%R'',I4,''%Then%Goto%Ins%'')') INS(I,1)
                 OUTPUT(NO+1:NO+23)=AUX(1:23)
                 NO=NO+23
            ELSEIF(ABS(REG(INS(I,1))).LT.EPS)THEN
                 OUTPUT(NO+1:NO+15)='Never%Goto%Ins%'
                 NO=NO+15
            ELSEIF(ABS(REG(INS(I,1))-1.0).LT.EPS)THEN
                 OUTPUT(NO+1:NO+16)='Always%Goto%Ins%'
                 NO=NO+16
            ELSE
                 OUTPUT(NO+1:NO+39)=
     -                'If%<invalid%logical%constant>%Goto%Ins%'
                 NO=NO+39
            ENDIF
            IF(INS(I,3).GE.0)THEN
                 WRITE(AUX,'(''R'',I4)') INS(I,3)
                 OUTPUT(NO+1:NO+5)=AUX(1:5)
                 NO=NO+5
            ELSE
                 WRITE(AUX,'(I4)') NINT(REG(INS(I,3)))
                 OUTPUT(NO+1:NO+4)=AUX(1:4)
                 NO=NO+4
            ENDIF
*   Instruction is an argument building function.
       ELSEIF(INS(I,2).EQ.8)THEN
            IF(INS(I,3).GT.0)THEN
                 WRITE(AUX,'(''Arg'',I4,''%:=%R'',I4)')
     -                INS(I,4),INS(I,3)
                 OUTPUT(NO+1:NO+16)=AUX(1:16)
                 NO=NO+16
            ELSE
                 WRITE(AUX,'(''Arg'',I4,''%:=%'')') INS(I,4)
                 OUTPUT(NO+1:NO+11)=AUX(1:11)
                 NO=NO+11
                 CALL OUTFMT(REG(INS(I,3)),MODREG(INS(I,3)),
     -                AUX,NCAUX,'LEFT')
                 OUTPUT(NO+1:NO+NCAUX)=AUX(1:NCAUX)
                 NO=NO+NCAUX
            ENDIF
            IF(INS(I,1).EQ.0)THEN
                 OUTPUT(NO+1:NO+21)=',%modifiable,%global.'
                 NO=NO+21
            ELSEIF(INS(I,1).EQ.1)THEN
                 OUTPUT(NO+1:NO+25)=',%modifiable,%non-global.'
                 NO=NO+25
            ELSEIF(INS(I,1).EQ.2)THEN
                 OUTPUT(NO+1:NO+25)=',%non-modifiable,%global.'
                 NO=NO+25
            ELSEIF(INS(I,1).EQ.3)THEN
                 OUTPUT(NO+1:NO+29)=',%non-modifiable,%non-global.'
                 NO=NO+29
            ELSE
                 OUTPUT(NO+1:NO+28)=',%invalid%modification%flag.'
                 NO=NO+28
            ENDIF
*   Instruction is an external function call.
       ELSEIF(INS(I,2).EQ.9)THEN
            WRITE(AUX,'(''Call%procedure%'',I4,''%with%'',I4,
     -           ''%arguments.'')') INS(I,1),INS(I,3)
            OUTPUT(NO+1:NO+40)=AUX(1:40)
            NO=NO+40
*   Instruction not identified
       ELSE
            OUTPUT(NO+1:NO+37)='Unidentified,%unexecutable%statement.'
            NO=NO+37
       ENDIF
*   Remove blanks
       NNO=0
       DO 20 J=1,NO
       IF(OUTPUT(J:J).NE.' ')THEN
            NNO=NNO+1
            IF(OUTPUT(J:J).EQ.'%')OUTPUT(NNO:NNO)=' '
            IF(OUTPUT(J:J).NE.'%')OUTPUT(NNO:NNO)=OUTPUT(J:J)
       ENDIF
20     CONTINUE
*   Add the string '(deleted)' if marked not executable
       IF(.NOT.EXEC(I))OUTPUT(56:64)='(deleted)'
*   And write the string to the output
       WRITE(LUNOUT,'(26X,A)') OUTPUT(1:NNO)
10     CONTINUE
*** Add a blank line to make the output more legible
       WRITE(LUNOUT,'('' '')')
       END
