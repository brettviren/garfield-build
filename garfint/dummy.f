CDECK  ID>, DUMMY.
       SUBROUTINE EFIELD(X,Y,EX,EY,EZ,E,V,IOPT,ILOC)
       REAL TCR(*),SCR
       DOUBLE PRECISION VXMIN,VYMIN,VXMAX,VYMAX
       INTEGER IW,NCR,INSTR,IFAIL
       CHARACTER*(*) OPTION,TITLE,XTXT,YTXT,ZTXT
       ENTRY DLCALC(X,Y,Q,I)
       ENTRY DLCATT(X)
       ENTRY DLCTWN(X)
       ENTRY DLCDIF(X)
       ENTRY SIGTHC(IW,SCR,OPTION,NCR,TCR,IFAIL)
       ENTRY CELCAL(INSTR,IFAIL)
       ENTRY GASCAL(INSTR,IFAIL)
       ENTRY DLCCAL(INSTR,IFAIL)
       ENTRY EFCCAL(INSTR,IFAIL)
       ENTRY SIGCAL(INSTR,IFAIL)
       entry graxi3(VXMIN,VYMIN,VXMAX,VYMAX,
     -      XTXT,YTXT,ZTXT,TITLE,OPTION)
       entry bemend
       END
       real function ranfl()
       ranfl=0
       end
