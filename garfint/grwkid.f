CDECK  ID>, GRWKID.
       SUBROUTINE GRWKID(NAME,IWKID,LUNOFF,ICAT,IFAIL)
*-----------------------------------------------------------------------
*   GRWKID - Associates a workstation name with an identifier.
*   VARIABLES : NAME        : Input name of the workstation.
*               IWKID       : Will be set to the workstation identifier.
*               LUNOFF      : Offset between conid and lun.
*   (Last changed on 23/ 4/96.)
*-----------------------------------------------------------------------
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*(*) NAME
       INTEGER IWKID,IFAIL,LUNOFF
       PARAMETER(NTYP=33)
       INTEGER ITYP(NTYP),IOFF(NTYP),INOUT(NTYP)
       CHARACTER*20 TYPE(NTYP)
       DATA (TYPE(I),ITYP(I),IOFF(I),INOUT(I),I=1,NTYP)/
     -      '0                   ',     0,    0,  1,
     -      'NONE                ',     0,    0,  1,
     -      'none                ',     0,    0,  1,
     -      'INQUIRE             ',    -1,    0,  1,
     -      'inquire             ',    -1,    0,  1,
     -      '*interactive_default',    -1,    0,  1,
     -      '1                   ',     1,    0,  1,
     -      '2                   ',     2,    0,  1,
     -      '3                   ',     3,    0,  1,
     -      '4                   ',     4,    0,  1,
     -      '5                   ',     5,    0,  1,
     -      '6                   ',     6,    0,  1,
     -      '7                   ',     7,    0,  1,
     -      '8                   ',     8,    0,  1,
     -      '9                   ',     9,    0,  1,
     -      '7878                ',  7878,    0,  1,
     -      'FALCO               ',  7878,    0,  1,
     -      'Falco               ',  7878,    0,  1,
     -      'XTERM               ',  7879,    0,  1,
     -      'PS_LANDSCAPE        ',  -112,    0, -1,
     -      'PS_landscape        ',  -112,    0, -1,
     -      'PS_PORTRAIT         ',  -111,    0, -1,
     -      'PS_portrait         ',  -111,    0, -1,
     -      'POSTSCRIPT          ',  -111,    0, -1,
     -      'PostScript          ',  -111,    0, -1,
     -      '*batch_default      ',  -111,    0, -1,
     -      'EPS                 ',  -113,    0, -1,
     -      'ENCAPSULATED_PS     ',  -113,    0, -1,
     -      'encapsulated_PS     ',  -113,    0, -1,
     -      'ENCAPSULATED_POSTSCR',  -113,    0, -1,
     -      'encapsulated_PostScr',  -113,    0, -1,
     -      'LATEX               ',  -777,    0, -1,
     -      'LaTeX               ',  -777,    0, -1/
*** Preset the workstation and logical unit offset to 0.
       IWKID=0
       LUNOFF=0
*** Assume the routine will fail.
       IFAIL=1
*** If NTYP has been set to 0, we don't recognise anything.
       IF(NTYP.EQ.0)THEN
            PRINT *,' !!!!!! GRWKID WARNING : No workstation type'//
     -           ' list is available; no identifier returned.'
            IFAIL=1
            RETURN
       ENDIF
*** Calculate the length of the workstation name.
       LENNAM=0
       DO 30 I=1,LEN(NAME)
       IF(NAME(I:I).NE.' ')LENNAM=I
30     CONTINUE
*** Warn if the name is blank.
       IF(LENNAM.EQ.0)THEN
            PRINT *,' !!!!!! GRWKID WARNING : The workstation type'//
     -           ' is blank; no identifier returned.'
            IFAIL=1
            RETURN
       ENDIF
*** Scan the list of known workstaion names.
       IFOUND=0
       NFOUND=0
       DO 10 I=1,NTYP
       IF(NAME(1:LENNAM).EQ.TYPE(I)(1:LENNAM))THEN
            IFOUND=I
            NFOUND=NFOUND+1
       ENDIF
10     CONTINUE
*** Warn if not known.
       IF(NFOUND.EQ.0)THEN
            PRINT *,' !!!!!! GRWKID WARNING : ',NAME(1:LENNAM),
     -           ' is not a known workstation type.'
            IFAIL=1
            RETURN
*** Inform about the choice if ambiguous.
       ELSEIF(NFOUND.GT.1)THEN
            NCPRT=1
            DO 20 J=20,1,-1
            IF(TYPE(IFOUND)(J:J).NE.' '.AND.NCPRT.EQ.1)NCPRT=J
20          CONTINUE
            PRINT *,' ------ GRWKID MESSAGE : ',NAME(1:LENNAM),
     -           ' is an ambiguous workstation type; choosing '//
     -           TYPE(IFOUND)(1:NCPRT)//'.'
       ENDIF
*** Normal assignment.
       IWKID=ITYP(IFOUND)
       LUNOFF=IOFF(IFOUND)
*** Determine the workstation category.
       CALL GQWKCA(IWKID,IERR,ICAT)
       IF(IERR.EQ.8)THEN
            IF(INOUT(IFOUND).EQ.1)THEN
                 ICAT=2
            ELSE
                 ICAT=4
            ENDIF
       ELSEIF(IERR.NE.0)THEN
            PRINT *,' !!!!!! GRWKID WARNING : ',NAME(1:LENNAM),
     -           ' is not recognised by GKS as a valid workstation.'
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GRWKID DEBUG   :'',
     -           '' GQWKCA Error code '',I3,'', category '',I1,
     -           '' for wktype '',I5,''.'')') IERR,ICAT,IWKID
            ICAT=-1
            IFAIL=1
       ENDIF
*** Things seem to have worked.
       IFAIL=0
       END
