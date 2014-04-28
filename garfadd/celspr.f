CDECK  ID>, CELSPR.
       SUBROUTINE CELSPR
*-----------------------------------------------------------------------
*   CELSPR - Prints an overview of the solids.
*   (Last changed on 13/10/11.)
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
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,MEV2KG,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      MEV2KG = 1.782661845E-30,
     -      BOLTZ=1.380658E-23)
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER I,NCYL,NHOLE,NBOX,NSPHER,NTOBL,NEXT,
     -      NC1,NC2,NC3,NC4,NC5,NC6,
     -      NC7,NC8,NC9,NC10,NC11,NC12,NC13,NC14,NC15,NC16,NC17,NC18,
     -      NC19,NC20,NC21,NC22,NC23,NC24,NCNUM
       CHARACTER*30 MAT,AUX1,AUX2,AUX3,AUX4,AUX5,AUX6,AUX7,AUX8,AUX9,
     -      AUX10,AUX11,AUX12,AUX13,AUX14,AUX15,AUX16,AUX17,AUX18,AUX19,
     -      AUX20,AUX21,AUX22,AUX23,AUX24,AUXNUM
       DOUBLE PRECISION QVOL
*** See whether there are any solids.
       IF(NSOLID.LT.1)THEN
            WRITE(LUNOUT,'(''  There are currently no solids.'')')
            RETURN
       ELSE
            WRITE(LUNOUT,'(/''  SOLIDS'')')
       ENDIF
*** Count the various types of solids.
       NCYL=0
       NHOLE=0
       NBOX=0
       NSPHER=0
       NTOBL=0
       NEXT=0
       DO 10 I=1,NSOLID
       IF(ISOLTP(I).EQ.1)THEN
            NCYL=NCYL+1
       ELSEIF(ISOLTP(I).EQ.2)THEN
            NHOLE=NHOLE+1
       ELSEIF(ISOLTP(I).EQ.3)THEN
            NBOX=NBOX+1
       ELSEIF(ISOLTP(I).EQ.4)THEN
            NSPHER=NSPHER+1
       ELSEIF(ISOLTP(I).EQ.5)THEN
            NTOBL=NTOBL+1
       ELSEIF(ISOLTP(I).EQ.6)THEN
            NEXT=NEXT+1
       ELSE
            PRINT *,' !!!!!! CELSPR WARNING : Found a solid of'//
     -           ' unknown type ',ISOLTP(I),'; ignored.'
       ENDIF
10     CONTINUE
*** Print the cylinders.
       IF(NCYL.GE.1)THEN
            WRITE(LUNOUT,'(/''  Cylinders:'')')
            DO 20 I=1,NSOLID
            IF(ISOLTP(I).NE.1)GOTO 20
            IF(ISOLMT(I).EQ.1)THEN
                 MAT='Conductor 1'
            ELSEIF(ISOLMT(I).EQ.2)THEN
                 MAT='Conductor 2'
            ELSEIF(ISOLMT(I).EQ.3)THEN
                 MAT='Conductor 3'
            ELSEIF(ISOLMT(I).EQ.11)THEN
                 MAT='Dielectricum 1'
            ELSEIF(ISOLMT(I).EQ.12)THEN
                 MAT='Dielectricum 2'
            ELSEIF(ISOLMT(I).EQ.13)THEN
                 MAT='Dielectricum 3'
            ELSE
                 MAT='# Unknown'
            ENDIF
            CALL OUTFMT(REAL(I),2,AUXNUM,NCNUM,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+1)),2,AUX1,NC1,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+2)),2,AUX2,NC2,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+3)),2,AUX3,NC3,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+4)),2,AUX4,NC4,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+5)),2,AUX5,NC5,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+6)),2,AUX6,NC6,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+7)),2,AUX7,NC7,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+8)),2,AUX8,NC8,'LEFT')
            IF(NINT(CBUF(ISTART(I)+9)).EQ.0)THEN
                 AUX9='Default'
                 NC9=7
            ELSEIF(NINT(CBUF(ISTART(I)+9)).EQ.-1)THEN
                 AUX9='Thin wire approximation'
                 NC9=23
            ELSE
                 CALL OUTFMT(REAL(NINT(CBUF(ISTART(I)+9))),2,
     -                AUX9,NC9,'LEFT')
            ENDIF
            CALL OUTFMT(REAL(CBUF(ISTART(I)+15)),2,AUX10,NC10,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+16)),2,AUX11,NC11,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+18)),2,AUX12,NC12,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+19)),2,AUX13,NC13,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+20)),2,AUX14,NC14,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+21)),2,AUX15,NC15,'LEFT')
            IF(CBUF(ISTART(I)+19).LT.0)THEN
                AUX13='automatic'
                NC13=9
            ENDIF
            IF(CBUF(ISTART(I)+20).LT.0)THEN
                AUX14='automatic'
                NC14=9
            ENDIF
            IF(CBUF(ISTART(I)+21).LT.0)THEN
                AUX15='automatic'
                NC15=9
            ENDIF
            IF(SOLTYP(I).EQ.'?')THEN
                 AUX16='Not labeled'
                 NC16=11
            ELSE
                 AUX16=SOLTYP(I)
                 NC16=1
            ENDIF
            WRITE(LUNOUT,'(2X,A1,A4,'' - '',
     -               ''Radius:         '',A,'' cm''/
     -           10X,''Half-length:    '',A,'' cm''/
     -           10X,''Centre:         ('',A,'', '',A,'', '',A,'') cm''/
     -           10X,''Axis:           ('',A,'', '',A,'', '',A,'')''/
     -           10X,''Material:       '',A/
     -           10X,''Corners:        '',A/
     -           10X,''Discretisation: '',A,'' cm (top), '',
     -               A,'' cm (bottom), '',A,'' cm (body)''/
     -           10X,''Label:          '',A)')
     -           SOLTYP(I),AUXNUM(1:4),
     -           AUX1(1:NC1),AUX2(1:NC2),AUX3(1:NC3),
     -           AUX4(1:NC4),AUX5(1:NC5),AUX6(1:NC6),AUX7(1:NC7),
     -           AUX8(1:NC8),MAT,AUX9(1:NC9),
     -           AUX13(1:NC13),AUX14(1:NC14),AUX15(1:NC15),AUX16(1:NC16)
            IF(NINT(CBUF(ISTART(I)+17)).EQ.0)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''None specified.'')')
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.1)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Conductor held at '',A,'' V'')')
     -                AUX10(1:NC10)
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.2)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Conductor with a surface charge'',
     -                '' of '',A,'' Qe'')') AUX12(1:NC12)
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.3)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Uncharged floating conductor'')')
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.4)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Uncharged dielectric medium'',
     -                '' with dielectric constant '',A)') AUX11(1:NC11)
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.5)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Dielectric medium with'',
     -                '' epsilon '',A,'' and charge '',A,'' Qe'')')
     -                AUX11(1:NC11),AUX12(1:NC12)
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.6)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Symmetry: E-field parallel'',
     -                '' with the surface'')')
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.7)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Symmetry: E-field perpendicular'',
     -                '' to the surface'')')
            ELSE
                 WRITE(LUNOUT,'(10X,''Boundary:       Unknown.'')')
            ENDIF
            IF(NINT(CBUF(ISTART(I)+17)).NE.0)THEN
                 CALL BEMVOQ(I,QVOL)
                 CALL OUTFMT(REAL(QVOL*1.0D12),2,AUX24,NC24,'LEFT')
                 WRITE(LUNOUT,'(10X,''Surface charge: '',A,'' pC.'')')
     -                AUX24(1:NC24)
            ENDIF
20          CONTINUE
       ENDIF
*** Print the holes.
       IF(NHOLE.GE.1)THEN
            WRITE(LUNOUT,'(/''  Holes:'')')
            DO 30 I=1,NSOLID
            IF(ISOLTP(I).NE.2)GOTO 30
            IF(ISOLMT(I).EQ.1)THEN
                 MAT='Conductor 1'
            ELSEIF(ISOLMT(I).EQ.2)THEN
                 MAT='Conductor 2'
            ELSEIF(ISOLMT(I).EQ.3)THEN
                 MAT='Conductor 3'
            ELSEIF(ISOLMT(I).EQ.11)THEN
                 MAT='Dielectricum 1'
            ELSEIF(ISOLMT(I).EQ.12)THEN
                 MAT='Dielectricum 2'
            ELSEIF(ISOLMT(I).EQ.13)THEN
                 MAT='Dielectricum 3'
            ELSE
                 MAT='# Unknown'
            ENDIF
            CALL OUTFMT(REAL(I),2,AUXNUM,NCNUM,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+ 1)),2,AUX1, NC1, 'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+ 2)),2,AUX2, NC2, 'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+ 3)),2,AUX3, NC3, 'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+ 4)),2,AUX4, NC4, 'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+ 5)),2,AUX5, NC5, 'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+ 6)),2,AUX6, NC6, 'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+ 7)),2,AUX7, NC7, 'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+ 8)),2,AUX8, NC8, 'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+ 9)),2,AUX9, NC9, 'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+10)),2,AUX10,NC10,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+11)),2,AUX11,NC11,'LEFT')
            CALL OUTFMT(REAL(NINT(CBUF(ISTART(I)+12))),2,AUX12,NC12,
     -           'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+17)),2,AUX13,NC13,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+18)),2,AUX14,NC14,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+20)),2,AUX15,NC15,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+21)),2,AUX16,NC16,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+22)),2,AUX17,NC17,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+23)),2,AUX18,NC18,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+24)),2,AUX19,NC19,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+25)),2,AUX20,NC20,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+26)),2,AUX21,NC21,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+27)),2,AUX22,NC22,'LEFT')
            IF(CBUF(ISTART(I)+21).LT.0)THEN
                AUX16='automatic'
                NC16=9
            ENDIF
            IF(CBUF(ISTART(I)+22).LT.0)THEN
                AUX17='automatic'
                NC17=9
            ENDIF
            IF(CBUF(ISTART(I)+23).LT.0)THEN
                AUX18='automatic'
                NC18=9
            ENDIF
            IF(CBUF(ISTART(I)+24).LT.0)THEN
                AUX19='automatic'
                NC19=9
            ENDIF
            IF(CBUF(ISTART(I)+25).LT.0)THEN
                AUX20='automatic'
                NC20=9
            ENDIF
            IF(CBUF(ISTART(I)+26).LT.0)THEN
                AUX21='automatic'
                NC21=9
            ENDIF
            IF(CBUF(ISTART(I)+27).LT.0)THEN
                AUX22='automatic'
                NC22=9
            ENDIF
            IF(SOLTYP(I).EQ.'?')THEN
                 AUX23='Not labeled'
                 NC23=11
            ELSE
                 AUX23=SOLTYP(I)
                 NC23=1
            ENDIF
            WRITE(LUNOUT,'(2X,A1,A4,'' - '',
     -           ''Radii:          '',A,'' cm and '',A,'' cm''/
     -           10X,''Half-lengths:   ('',A,'', '',A,'', '',A,'') cm''/
     -           10X,''Centre:         ('',A,'', '',A,'', '',A,'') cm''/
     -           10X,''Axis:           ('',A,'', '',A,'', '',A,'')''/
     -           10X,''Material:       '',A/
     -           10X,''Corners:        '',A/
     -           10X,''Discretisation: '',
     -                A,''/'',A,'' cm (front/back), '',
     -                A,''/'',A,'' cm (right/left),''/26X,
     -                A,''/'',A,'' cm (top/bottom), '',
     -                A,'' cm (hole)''/
     -           10X,''Label:          '',A)')
     -           SOLTYP(I),AUXNUM(1:4),
     -           AUX1(1:NC1),AUX2(1:NC2),AUX3(1:NC3),
     -           AUX4(1:NC4),AUX5(1:NC5),AUX6(1:NC6),AUX7(1:NC7),
     -           AUX8(1:NC8),AUX9(1:NC9),AUX10(1:NC10),AUX11(1:NC11),
     -           MAT,AUX12(1:NC12),
     -           AUX16(1:NC16),AUX17(1:NC17),AUX18(1:NC18),
     -           AUX19(1:NC19),AUX20(1:NC20),AUX21(1:NC21),
     -           AUX22(1:NC22),AUX23(1:NC23)
            IF(NINT(CBUF(ISTART(I)+19)).EQ.0)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''None specified.'')')
            ELSEIF(NINT(CBUF(ISTART(I)+19)).EQ.1)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Conductor held at '',A,'' V'')')
     -                AUX13(1:NC13)
            ELSEIF(NINT(CBUF(ISTART(I)+19)).EQ.2)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Conductor with a surface charge'',
     -                '' of '',A,'' Qe'')') AUX15(1:NC15)
            ELSEIF(NINT(CBUF(ISTART(I)+19)).EQ.3)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Uncharged floating conductor'')')
            ELSEIF(NINT(CBUF(ISTART(I)+19)).EQ.4)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Uncharged dielectric medium'',
     -                '' with dielectric constant '',A)') AUX14(1:NC14)
            ELSEIF(NINT(CBUF(ISTART(I)+19)).EQ.5)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Dielectric medium with'',
     -                '' epsilon '',A,'' and charge '',A,'' Qe'')')
     -                AUX14(1:NC14),AUX15(1:NC15)
            ELSEIF(NINT(CBUF(ISTART(I)+19)).EQ.6)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Symmetry: E-field parallel'',
     -                '' with the surface'')')
            ELSEIF(NINT(CBUF(ISTART(I)+19)).EQ.7)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Symmetry: E-field perpendicular'',
     -                '' to the surface'')')
            ELSE
                 WRITE(LUNOUT,'(10X,''Boundary:       Unknown.'')')
            ENDIF
            IF(NINT(CBUF(ISTART(I)+19)).NE.0)THEN
                 CALL BEMVOQ(I,QVOL)
                 CALL OUTFMT(REAL(QVOL*1.0D12),2,AUX24,NC24,'LEFT')
                 WRITE(LUNOUT,'(10X,''Surface charge: '',A,'' pC.'')')
     -                AUX24(1:NC24)
            ENDIF
30          CONTINUE
       ENDIF
*** Print the boxes.
       IF(NBOX.GE.1)THEN
            WRITE(LUNOUT,'(/''  Boxes:'')')
            DO 40 I=1,NSOLID
            IF(ISOLTP(I).NE.3)GOTO 40
            IF(ISOLMT(I).EQ.1)THEN
                 MAT='Conductor 1'
            ELSEIF(ISOLMT(I).EQ.2)THEN
                 MAT='Conductor 2'
            ELSEIF(ISOLMT(I).EQ.3)THEN
                 MAT='Conductor 3'
            ELSEIF(ISOLMT(I).EQ.11)THEN
                 MAT='Dielectricum 1'
            ELSEIF(ISOLMT(I).EQ.12)THEN
                 MAT='Dielectricum 2'
            ELSEIF(ISOLMT(I).EQ.13)THEN
                 MAT='Dielectricum 3'
            ELSE
                 MAT='# Unknown'
            ENDIF
            CALL OUTFMT(REAL(I),2,AUXNUM,NCNUM,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+1)),2,AUX1,NC1,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+2)),2,AUX2,NC2,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+3)),2,AUX3,NC3,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+4)),2,AUX4,NC4,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+5)),2,AUX5,NC5,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+6)),2,AUX6,NC6,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+7)),2,AUX7,NC7,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+8)),2,AUX8,NC8,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+9)),2,AUX9,NC9,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+14)),2,AUX10,NC10,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+15)),2,AUX11,NC11,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+17)),2,AUX12,NC12,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+18)),2,AUX13,NC13,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+19)),2,AUX14,NC14,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+20)),2,AUX15,NC15,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+21)),2,AUX16,NC16,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+22)),2,AUX17,NC17,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+23)),2,AUX18,NC18,'LEFT')
            IF(CBUF(ISTART(I)+18).LT.0)THEN
                AUX13='automatic'
                NC13=9
            ENDIF
            IF(CBUF(ISTART(I)+19).LT.0)THEN
                AUX14='automatic'
                NC14=9
            ENDIF
            IF(CBUF(ISTART(I)+20).LT.0)THEN
                AUX15='automatic'
                NC15=9
            ENDIF
            IF(CBUF(ISTART(I)+21).LT.0)THEN
                AUX16='automatic'
                NC16=9
            ENDIF
            IF(CBUF(ISTART(I)+22).LT.0)THEN
                AUX17='automatic'
                NC17=9
            ENDIF
            IF(CBUF(ISTART(I)+23).LT.0)THEN
                AUX18='automatic'
                NC18=9
            ENDIF
            IF(SOLTYP(I).EQ.'?')THEN
                 AUX19='Not labeled'
                 NC19=11
            ELSE
                 AUX19=SOLTYP(I)
                 NC19=1
            ENDIF
            WRITE(LUNOUT,'(2X,A1,A4,'' - '',
     -               ''Half-lengths:   ('',A,'', '',A,'', '',A,'') cm''/
     -           10X,''Centre:         ('',A,'', '',A,'', '',A,'') cm''/
     -           10X,''Axis:           ('',A,'', '',A,'', '',A,'')''/
     -           10X,''Material:       '',A/
     -           10X,''Discretisation: '',
     -                A,''/'',A,'' cm (front/back), '',
     -                A,''/'',A,'' cm (right/left), '',
     -                A,''/'',A,'' cm (top/bottom)''/
     -           10X,''Label:          '',A)')
     -           SOLTYP(I),AUXNUM(1:4),
     -           AUX1(1:NC1),AUX2(1:NC2),AUX3(1:NC3),
     -           AUX4(1:NC4),AUX5(1:NC5),AUX6(1:NC6),AUX7(1:NC7),
     -           AUX8(1:NC8),AUX9(1:NC9),MAT,
     -           AUX13(1:NC13),AUX14(1:NC14),AUX15(1:NC15),
     -           AUX16(1:NC16),AUX17(1:NC17),AUX18(1:NC18),
     -           AUX19(1:NC19)
            IF(NINT(CBUF(ISTART(I)+16)).EQ.0)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''None specified.'')')
            ELSEIF(NINT(CBUF(ISTART(I)+16)).EQ.1)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Conductor held at '',A,'' V'')')
     -                AUX10(1:NC10)
            ELSEIF(NINT(CBUF(ISTART(I)+16)).EQ.2)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Conductor with a surface charge'',
     -                '' of '',A,'' Qe'')') AUX12(1:NC12)
            ELSEIF(NINT(CBUF(ISTART(I)+16)).EQ.3)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Uncharged floating conductor'')')
            ELSEIF(NINT(CBUF(ISTART(I)+16)).EQ.4)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Uncharged dielectric medium'',
     -                '' with dielectric constant '',A)') AUX11(1:NC11)
            ELSEIF(NINT(CBUF(ISTART(I)+16)).EQ.5)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Dielectric medium with'',
     -                '' epsilon '',A,'' and charge '',A,'' Qe'')')
     -                AUX11(1:NC11),AUX12(1:NC12)
            ELSEIF(NINT(CBUF(ISTART(I)+16)).EQ.6)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Symmetry: E-field parallel'',
     -                '' with the surface'')')
            ELSEIF(NINT(CBUF(ISTART(I)+16)).EQ.7)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Symmetry: E-field perpendicular'',
     -                '' to the surface'')')
            ELSE
                 WRITE(LUNOUT,'(10X,''Boundary:       Unknown.'')')
            ENDIF
            IF(NINT(CBUF(ISTART(I)+16)).NE.0)THEN
                 CALL BEMVOQ(I,QVOL)
                 CALL OUTFMT(REAL(QVOL*1.0D12),2,AUX24,NC24,'LEFT')
                 WRITE(LUNOUT,'(10X,''Surface charge: '',A,'' pC.'')')
     -                AUX24(1:NC24)
            ENDIF
40          CONTINUE
       ENDIF
*** Print the spheres.
       IF(NSPHER.GE.1)THEN
            WRITE(LUNOUT,'(/''  Spheres:'')')
            DO 50 I=1,NSOLID
            IF(ISOLTP(I).NE.4)GOTO 50
            IF(ISOLMT(I).EQ.1)THEN
                 MAT='Conductor 1'
            ELSEIF(ISOLMT(I).EQ.2)THEN
                 MAT='Conductor 2'
            ELSEIF(ISOLMT(I).EQ.3)THEN
                 MAT='Conductor 3'
            ELSEIF(ISOLMT(I).EQ.11)THEN
                 MAT='Dielectricum 1'
            ELSEIF(ISOLMT(I).EQ.12)THEN
                 MAT='Dielectricum 2'
            ELSEIF(ISOLMT(I).EQ.13)THEN
                 MAT='Dielectricum 3'
            ELSE
                 MAT='# Unknown'
            ENDIF
            CALL OUTFMT(REAL(I),2,AUXNUM,NCNUM,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+1)),2,AUX1,NC1,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+2)),2,AUX2,NC2,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+3)),2,AUX3,NC3,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+4)),2,AUX4,NC4,'LEFT')
            CALL OUTFMT(REAL(NINT(CBUF(ISTART(I)+5))),2,AUX5,NC5,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+6)),2,AUX6,NC6,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+7)),2,AUX7,NC7,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+9)),2,AUX8,NC8,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+10)),2,AUX9,NC9,'LEFT')
            IF(CBUF(ISTART(I)+10).LT.0)THEN
                AUX9='automatic'
                NC9=9
            ENDIF
            IF(SOLTYP(I).EQ.'?')THEN
                 AUX16='Not labeled'
                 NC16=11
            ELSE
                 AUX16=SOLTYP(I)
                 NC16=1
            ENDIF
            WRITE(LUNOUT,'(2X,A1,A4,'' - '',
     -               ''Radius:         '',A,'' cm''/
     -           10X,''Centre:         ('',A,'', '',A,'', '',A,'') cm''/
     -           10X,''Material:       '',A/
     -           10X,''Corners:        '',A/
     -           10X,''Discretisation: '',A,'' cm''/
     -           10X,''Label:          '',A)')
     -           SOLTYP(I),AUXNUM(1:4),
     -           AUX1(1:NC1),AUX2(1:NC2),AUX3(1:NC3),
     -           AUX4(1:NC4),MAT,AUX5(1:NC5),AUX9(1:NC9),AUX16(1:NC16)
            IF(NINT(CBUF(ISTART(I)+8)).EQ.0)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''None specified.'')')
            ELSEIF(NINT(CBUF(ISTART(I)+8)).EQ.1)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Conductor held at '',A,'' V'')')
     -                AUX6(1:NC6)
            ELSEIF(NINT(CBUF(ISTART(I)+8)).EQ.2)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Conductor with a surface charge'',
     -                '' of '',A,'' Qe'')') AUX8(1:NC8)
            ELSEIF(NINT(CBUF(ISTART(I)+8)).EQ.3)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Uncharged floating conductor'')')
            ELSEIF(NINT(CBUF(ISTART(I)+8)).EQ.4)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Uncharged dielectric medium'',
     -                '' with dielectric constant '',A)') AUX7(1:NC7)
            ELSEIF(NINT(CBUF(ISTART(I)+8)).EQ.5)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Dielectric medium with'',
     -                '' epsilon '',A,'' and charge '',A,'' Qe'')')
     -                AUX7(1:NC7),AUX8(1:NC8)
            ELSEIF(NINT(CBUF(ISTART(I)+8)).EQ.6)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Symmetry: E-field parallel'',
     -                '' with the surface'')')
            ELSEIF(NINT(CBUF(ISTART(I)+8)).EQ.7)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Symmetry: E-field perpendicular'',
     -                '' to the surface'')')
            ELSE
                 WRITE(LUNOUT,'(10X,''Boundary:       Unknown.'')')
            ENDIF
            IF(NINT(CBUF(ISTART(I)+8)).NE.0)THEN
                 CALL BEMVOQ(I,QVOL)
                 CALL OUTFMT(REAL(QVOL*1.0D12),2,AUX24,NC24,'LEFT')
                 WRITE(LUNOUT,'(10X,''Surface charge: '',A,'' pC.'')')
     -                AUX24(1:NC24)
            ENDIF
50          CONTINUE
       ENDIF
*** Print the Ridge's.
       IF(NTOBL.GE.1)THEN
            WRITE(LUNOUT,'(/''  Ridges:'')')
            DO 60 I=1,NSOLID
            IF(ISOLTP(I).NE.5)GOTO 60
            IF(ISOLMT(I).EQ.1)THEN
                 MAT='Conductor 1'
            ELSEIF(ISOLMT(I).EQ.2)THEN
                 MAT='Conductor 2'
            ELSEIF(ISOLMT(I).EQ.3)THEN
                 MAT='Conductor 3'
            ELSEIF(ISOLMT(I).EQ.11)THEN
                 MAT='Dielectricum 1'
            ELSEIF(ISOLMT(I).EQ.12)THEN
                 MAT='Dielectricum 2'
            ELSEIF(ISOLMT(I).EQ.13)THEN
                 MAT='Dielectricum 3'
            ELSE
                 MAT='# Unknown'
            ENDIF
            CALL OUTFMT(REAL(I),2,AUXNUM,NCNUM,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+1)),2,AUX1,NC1,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+2)),2,AUX2,NC2,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+3)),2,AUX3,NC3,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+4)),2,AUX4,NC4,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+5)),2,AUX5,NC5,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+6)),2,AUX6,NC6,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+7)),2,AUX7,NC7,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+8)),2,AUX8,NC8,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+9)),2,AUX9,NC9,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+14)),2,AUX10,NC10,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+15)),2,AUX11,NC11,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+16)),2,AUX12,NC12,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+18)),2,AUX13,NC13,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+19)),2,AUX14,NC14,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+20)),2,AUX15,NC15,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+21)),2,AUX16,NC16,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+22)),2,AUX17,NC17,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+23)),2,AUX18,NC18,'LEFT')
            IF(CBUF(ISTART(I)+19).LT.0)THEN
                AUX14='automatic'
                NC14=9
            ENDIF
            IF(CBUF(ISTART(I)+20).LT.0)THEN
                AUX15='automatic'
                NC15=9
            ENDIF
            IF(CBUF(ISTART(I)+21).LT.0)THEN
                AUX16='automatic'
                NC16=9
            ENDIF
            IF(CBUF(ISTART(I)+22).LT.0)THEN
                AUX17='automatic'
                NC17=9
            ENDIF
            IF(CBUF(ISTART(I)+23).LT.0)THEN
                AUX18='automatic'
                NC18=9
            ENDIF
            IF(SOLTYP(I).EQ.'?')THEN
                 AUX19='Not labeled'
                 NC19=11
            ELSE
                 AUX19=SOLTYP(I)
                 NC19=1
            ENDIF
            WRITE(LUNOUT,'(2X,A1,A4,'' - '',
     -               ''Base half-size: ('',A,'', '',A,'') cm''/
     -           10X,''Ridge (x,z):    ('',A,'', '',A,'') cm''/
     -           10X,''Axis:           ('',A,'', '',A,'', '',A,'')''/
     -           10X,''Material:       '',A/
     -           10X,''Discretisation: '',
     -                A,''/'',A,'' cm (front/back), '',
     -                A,''/'',A,'' cm (right/left), '',
     -                A,'' cm (floor)''/
     -           10X,''Label:          '',A)')
     -           SOLTYP(I),AUXNUM(1:4),
     -           AUX1(1:NC1),AUX2(1:NC2),
     -           AUX10(1:NC10),AUX3(1:NC3),
     -           AUX7(1:NC7),AUX8(1:NC8),AUX9(1:NC9),
     -           MAT,
     -           AUX14(1:NC14),AUX15(1:NC15),AUX16(1:NC16),
     -           AUX17(1:NC17),AUX18(1:NC18),
     -           AUX19(1:NC19)
            IF(NINT(CBUF(ISTART(I)+17)).EQ.0)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''None specified.'')')
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.1)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Conductor held at '',A,'' V'')')
     -                AUX11(1:NC11)
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.2)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Conductor with a surface charge'',
     -                '' of '',A,'' Qe'')') AUX13(1:NC13)
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.3)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Uncharged floating conductor'')')
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.4)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Uncharged dielectric medium'',
     -                '' with dielectric constant '',A)') AUX12(1:NC12)
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.5)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Dielectric medium with'',
     -                '' epsilon '',A,'' and charge '',A,'' Qe'')')
     -                AUX12(1:NC12),AUX13(1:NC13)
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.6)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Symmetry: E-field parallel'',
     -                '' with the surface'')')
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.7)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Symmetry: E-field perpendicular'',
     -                '' to the surface'')')
            ELSE
                 WRITE(LUNOUT,'(10X,''Boundary:       Unknown.'')')
            ENDIF
            IF(NINT(CBUF(ISTART(I)+17)).NE.0)THEN
                 CALL BEMVOQ(I,QVOL)
                 CALL OUTFMT(REAL(QVOL*1.0D12),2,AUX24,NC24,'LEFT')
                 WRITE(LUNOUT,'(10X,''Surface charge: '',A,'' pC.'')')
     -                AUX24(1:NC24)
            ENDIF
60          CONTINUE
       ENDIF
*** Print the extrusions.
       IF(NEXT.GE.1)THEN
            WRITE(LUNOUT,'(/''  Extrusions:'')')
            DO 70 I=1,NSOLID
            IF(ISOLTP(I).NE.6)GOTO 70
            IF(ISOLMT(I).EQ.1)THEN
                 MAT='Conductor 1'
            ELSEIF(ISOLMT(I).EQ.2)THEN
                 MAT='Conductor 2'
            ELSEIF(ISOLMT(I).EQ.3)THEN
                 MAT='Conductor 3'
            ELSEIF(ISOLMT(I).EQ.11)THEN
                 MAT='Dielectricum 1'
            ELSEIF(ISOLMT(I).EQ.12)THEN
                 MAT='Dielectricum 2'
            ELSEIF(ISOLMT(I).EQ.13)THEN
                 MAT='Dielectricum 3'
            ELSE
                 MAT='# Unknown'
            ENDIF
            CALL OUTFMT(REAL(I),2,AUXNUM,NCNUM,'LEFT')
            IF(CBUF(ISTART(I)+1).LT.0)THEN
                 AUX1='Clockwise'
                 NC1=9
            ELSE
                 AUX1='Counter-clockwise'
                 NC1=17
            ENDIF
            CALL OUTFMT(REAL(CBUF(ISTART(I)+2)),2,AUX2,NC2,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+3)),2,AUX3,NC3,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+4)),2,AUX4,NC4,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+5)),2,AUX5,NC5,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+6)),2,AUX6,NC6,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+7)),2,AUX7,NC7,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+8)),2,AUX8,NC8,'LEFT')
            CALL OUTFMT(REAL(NINT(CBUF(ISTART(I)+9))),2,AUX9,NC9,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+15)),2,AUX10,NC10,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+16)),2,AUX11,NC11,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+18)),2,AUX12,NC12,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+19)),2,AUX13,NC13,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+20)),2,AUX14,NC14,'LEFT')
            CALL OUTFMT(REAL(CBUF(ISTART(I)+21)),2,AUX15,NC15,'LEFT')
            IF(CBUF(ISTART(I)+19).LT.0)THEN
                AUX13='automatic'
                NC13=9
            ENDIF
            IF(CBUF(ISTART(I)+20).LT.0)THEN
                AUX14='automatic'
                NC14=9
            ENDIF
            IF(CBUF(ISTART(I)+21).LT.0)THEN
                AUX15='automatic'
                NC15=9
            ENDIF
            IF(SOLTYP(I).EQ.'?')THEN
                 AUX16='Not labeled'
                 NC16=11
            ELSE
                 AUX16=SOLTYP(I)
                 NC16=1
            ENDIF
            WRITE(LUNOUT,'(2X,A1,A4,'' - '',
     -               ''Half-length:    '',A,'' cm''/
     -           10X,''Centre:         ('',A,'', '',A,'', '',A,'') cm''/
     -           10X,''Axis:           ('',A,'', '',A,'', '',A,'')''/
     -           10X,''Material:       '',A/
     -           10X,''Profile nodes:  '',A/
     -           10X,''Orientation:    '',A/
     -           10X,''Discretisation: '',A,'' cm (top), '',
     -               A,'' cm (bottom), '',A,'' cm (body)''/
     -           10X,''Label:          '',A)')
     -           SOLTYP(I),AUXNUM(1:4),
     -           AUX2(1:NC2),AUX3(1:NC3),
     -           AUX4(1:NC4),AUX5(1:NC5),AUX6(1:NC6),AUX7(1:NC7),
     -           AUX8(1:NC8),MAT,AUX9(1:NC9),AUX1(1:NC1),
     -           AUX13(1:NC13),AUX14(1:NC14),AUX15(1:NC15),AUX16(1:NC16)
            IF(NINT(CBUF(ISTART(I)+17)).EQ.0)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''None specified.'')')
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.1)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Conductor held at '',A,'' V'')')
     -                AUX10(1:NC10)
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.2)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Conductor with a surface charge'',
     -                '' of '',A,'' Qe'')') AUX12(1:NC12)
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.3)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Uncharged floating conductor'')')
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.4)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Uncharged dielectric medium'',
     -                '' with dielectric constant '',A)') AUX11(1:NC11)
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.5)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Dielectric medium with'',
     -                '' epsilon '',A,'' and charge '',A,'' Qe'')')
     -                AUX11(1:NC11),AUX12(1:NC12)
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.6)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Symmetry: E-field parallel'',
     -                '' with the surface'')')
            ELSEIF(NINT(CBUF(ISTART(I)+17)).EQ.7)THEN
                 WRITE(LUNOUT,'(10X,''Boundary:       '',
     -                ''Symmetry: E-field perpendicular'',
     -                '' to the surface'')')
            ELSE
                 WRITE(LUNOUT,'(10X,''Boundary:       Unknown.'')')
            ENDIF
            IF(NINT(CBUF(ISTART(I)+17)).NE.0)THEN
                 CALL BEMVOQ(I,QVOL)
                 CALL OUTFMT(REAL(QVOL*1.0D12),2,AUX24,NC24,'LEFT')
                 WRITE(LUNOUT,'(10X,''Surface charge: '',A,'' pC.'')')
     -                AUX24(1:NC24)
            ENDIF
70          CONTINUE
       ENDIF
       END
