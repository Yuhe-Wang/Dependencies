CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C    PPPPP   EEEEEE  N    N  EEEEEE  L        OOOO   PPPPP   EEEEEE    C
C    P    P  E       NN   N  E       L       O    O  P    P  E         C
C    P    P  E       N N  N  E       L       O    O  P    P  E         C
C    PPPPP   EEEE    N  N N  EEEE    L       O    O  PPPPP   EEEE      C
C    P       E       N   NN  E       L       O    O  P       E         C
C    P       EEEEEE  N    N  EEEEEE  LLLLLL   OOOO   P       EEEEEE    C
C                                                                      C
C                                                   (version 2005).    C
C                                                                      C
C  Subroutine package for Monte Carlo simulation of coupled electron-  C
C  photon transport in homogeneous media.                              C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  This the interfaces that can be called by C/C++                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      
C
C  *********************************************************************
C                       FUNCTION SETPARA
C  *********************************************************************

	SUBROUTINE SETPARA(M,EABS1,EABS2,EABS3,C1M,C2M,WCCM,WCRM)
	
	!DEC$ ATTRIBUTES DLLEXPORT, ALIAS:'setpara' :: SETPARA

	IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER*4 (I-N)
      PARAMETER (MAXMAT=10)
C  ****  Simulation parameters.
      COMMON/CSIMPA/EABS(3,MAXMAT),C1(MAXMAT),C2(MAXMAT),WCC(MAXMAT),
     1  WCR(MAXMAT)
	 
	EABS(1,M)=EABS1
	EABS(2,M)=EABS2
	EABS(3,M)=EABS3
	C1(M)=C1M
	C2(M)=C2M
	WCC(M)=WCCM
	WCR(M)=WCRM
	  
	RETURN
	END
C  *********************************************************************
C                       FUNCTION SETSTATE
C  *********************************************************************

	SUBROUTINE SETSTATE(EI,XI,YI,ZI,UI,VI,WI,KPARI,MI)
	!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'setstate'::SETSTATE
	IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER*4 (I-N)
C  ****  Simulation parameters.
      COMMON/TRACK/E,X,Y,Z,U,V,W,WGHT,KPAR,IBODY,M,ILB(5)
	 
	E=EI
      X=XI
      Y=YI
      Z=ZI
	U=UI
	V=VI
	W=WI
	KPAR=KPARI
	M=MI
	  
	RETURN
      END
C  *********************************************************************
C                       FUNCTION SETPOS
C  *********************************************************************

	SUBROUTINE SETPOSDIR(XI,YI,ZI,UI,VI,WI)
	!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'setposdir'::SETPOSDIR
	IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER*4 (I-N)
C  ****  Simulation parameters.
      COMMON/TRACK/E,X,Y,Z,U,V,W,WGHT,KPAR,IBODY,M,ILB(5)
      
      X=XI
      Y=YI
      Z=ZI
      U=UI
      V=VI
      W=WI
  
	RETURN
	END
C  *********************************************************************
C                       FUNCTION GETSTATE
C  *********************************************************************

	SUBROUTINE GETSTATE(EI,XI,YI,ZI,UI,VI,WI,KPARI,MI,DSI)
	!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:'getstate'::GETSTATE
	IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER*4 (I-N)
C  ****  Simulation parameters.
      COMMON/TRACK/E,X,Y,Z,U,V,W,WGHT,KPAR,IBODY,M,ILB(5)
	COMMON/CJUMP0/P(8),ST,DST,DS1,W1,W2,T1,T2
	 
	EI=E
      XI=X
      YI=Y
      ZI=Z
	UI=U
	VI=V
	WI=W
	KPARI=KPAR
	MI=M
	DSI=DS1
	RETURN
      END
C  *********************************************************************
C                       FUNCTION INITRNG
C  *********************************************************************
	SUBROUTINE INITRNG(I,J)
      !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:'initrng'::INITRNG
	IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER*4 (I-N)
C  ****  Simulation parameters.
      COMMON/RSEED/ISEED1,ISEED2
C
      ISEED1=I
      ISEED2=J
	RETURN
      END
C  *********************************************************************
C                       FUNCTION PENRNG
C  *********************************************************************
	SUBROUTINE PENRNG(R)
      !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:'penrng'::PENRNG
	IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER*4 (I-N)
C    
      EXTERNAL RAND
C
      R=RAND(1.0D0)
	RETURN
	END
C  *********************************************************************
C                       SUBROUTINE PEINIT
C  *********************************************************************
      SUBROUTINE PINIT(EMAX,NMAT,INFO,IBINARY)

	!DEC$ ATTRIBUTES DLLEXPORT, ALIAS:'pinit'::PINIT
C
C  Input of material data and initialization of simulation routines.
C
C  Each material is defined through the input file (unit=IRD), which is
C  created by the program 'material' using information contained in the
C  database. This file can be modified by the user if more accurate in-
C  teraction data are available. Data files for different materials must
C  be concatenated in a single input file, the M-th material in this
C  file is identified by the index M.
C
C  Input arguments:
C    EMAX ... maximum particle energy (kinetic energy for electrons and
C             positrons) used in the simulation. Note: Positrons with
C             energy E may produce photons with energy E+1.022E6.
C    NMAT ... number of materials in the geometry.
C    IRD .... input unit.
C    IWD .... output unit.
C    INFO ... determines the amount of information that is written on
C             the output file,
C               INFO=1, minimal (composition data only).
C               INFO=2, medium (same information as in the material
C                 definition data file, useful to check that the struc-
C                 ture of the latter is correct).
C               INFO=3 or larger, full information, including tables of
C                 interaction properties used in the simulation.
C
C  For the preliminary computations, PEINIT needs to know the absorption
C  energies EABS(KPAR,M) and the simulation parameters C1(M), C2(M),
C  WCC(M) and WCR(M). This information is introduced through the named
C  common block /CSIMPA/ that has to be loaded before invoking the
C  PEINIT subroutine.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER*4 (I-N)
      CHARACTER*3 LIT
      CHARACTER*120 FNAME
      CHARACTER*60 DIR
      COMMON/OUTDIR/DIR
C  ****  Main-PENELOPE common.
      COMMON/TRACK/E,X,Y,Z,U,V,W,WGHT,KPAR,IBODY,M,ILB(5)
	COMMON/RSEED/ISEED1,ISEED2
C  ****  Simulation parameters.
      PARAMETER (MAXMAT=10)
      COMMON/CSIMPA/EABS(3,MAXMAT),C1(MAXMAT),C2(MAXMAT),WCC(MAXMAT),
     1  WCR(MAXMAT)
      COMMON/CECUTR/ECUTR(MAXMAT)
C
      COMMON/CERSEC/IERSEC
      EXTERNAL PEMATR,EGRID,ESI0,PSI0,GPH0,RELAX0
      
      IERSEC=0
C     default random number seed
	ISEED1=12345
	ISEED2=54321
	IWR=6
	OPEN(2055,FILE="temp_filename.txt")
	READ(2055,'(A60)') DIR
	DO M=1,NMAT
        READ(2055,'(A60)') FNAME
	  WRITE(*,*) FNAME
	  OPEN(M+2014,FILE=FNAME)
      ENDDO
	CLOSE(2055)
C  ****  Lower limit of the energy grid.
C
      EMIN=1.0D35
      DO M=1,NMAT
        EMIN=MIN(EMIN,EABS(1,M),EABS(2,M),EABS(3,M))
      ENDDO
      IF(EMIN.LT.50.0D0) EMIN=50.0D0
C
      WRITE(IWR,2000)
 2000 FORMAT(/1X,34('*'),/1X,'**   PENELOPE  (version 2005)   **',
     1  /1X,34('*')) 
      WRITE(IWR,2001) EMIN,EMAX
 2001 FORMAT(/1X,'EMIN =',1P,E11.4,' eV,  EMAX =',E11.4,' eV')
      IF(EMAX.LT.EMIN+10.0D0) STOP 'The energy interval is too narrow.'
      IF(NMAT.GT.MAXMAT) THEN
        WRITE(IWR,2002) NMAT,MAXMAT,NMAT
 2002   FORMAT(/1X,'*** PENELOPE cannot handle ',I2,' different mater',
     1  'ials.'/5X,'Edit the source file and change the parameter ',
     2  'MAXMAT = ',I2,' to MAXMAT = ',I2)
        STOP 'PEINIT. Too many materials.'
      ENDIF
      IF(INFO.GT.2) WRITE(IWR,2102)
 2102 FORMAT(/1X,'NOTE: 1 mtu = 1 g/cm**2')
C
      CALL EGRID(EMIN,EMAX)  ! Defines the simulation energy grid.
      CALL ESI0  ! Initializes electron impact ionization routines.
      CALL PSI0  ! Initializes positron impact ionization routines.
      CALL GPH0  ! Initializes photoelectric routines.
      CALL RELAX0  ! Initializes atomic relaxation routines.
C
      DO M=1,NMAT
        IF(M.EQ.1) LIT='st'
        IF(M.EQ.2) LIT='nd'
        IF(M.EQ.3) LIT='rd'
        IF(M.GT.3) LIT='th'
        WRITE(IWR,2003) M,LIT
 2003   FORMAT(//1X,22('*')/1X,'**  ',I2,A2,' material   **',
     1    /1X,22('*'))
C
C  ****  Energy limits and thresholds.
C
        WRITE(IWR,2004)
 2004   FORMAT(/1X,'*** Simulation parameters:')
        IF(EABS(1,M).LT.50.0D0) THEN
          EABS(1,M)=50.0D0
          WRITE(IWR,2005)
 2005     FORMAT(1X,'*** Warning: electron absorption energy has ',
     1      'been set to 50 eV')
        ENDIF
        WRITE(IWR,2006) EABS(1,M)
 2006   FORMAT(5X,'Electron absorption energy =',1P,E11.4,' eV')
C
        IF(EABS(2,M).LT.50.0D0) THEN
          EABS(2,M)=50.0D0
          WRITE(IWR,2007)
 2007     FORMAT(1X,'*** Warning: photon absorption energy has ',
     1    'been set to 50 eV')
        ENDIF
        WRITE(IWR,2008) EABS(2,M)
 2008   FORMAT(7X,'Photon absorption energy =',1P,E11.4,' eV')
C
        IF(EABS(3,M).LT.50.0D0) THEN
          EABS(3,M)=50.0D0
          WRITE(IWR,2009)
 2009     FORMAT(1X,'*** Warning: positron absorption energy has ',
     1      'been set to 50 eV')
        ENDIF
        WRITE(IWR,2010) EABS(3,M)
 2010   FORMAT(5X,'Positron absorption energy =',1P,E11.4,' eV')
C
        C1(M)=MIN(0.2D0,ABS(C1(M)))
        C2(M)=MIN(0.2D0,ABS(C2(M)))
        WCC(M)=MIN(ABS(WCC(M)),EMAX)
        IF(WCR(M).LT.0.0D0) WRITE(IWR,2011)
 2011   FORMAT(1X,'*** Warning: soft radiative losses are switched off')
        WRITE(IWR,2012) C1(M),C2(M),WCC(M),MAX(WCR(M),10.0D0)
 2012   FORMAT(6X,'C1 =',1P,E11.4,',       C2 =',E11.4,/5X,'WCC =',
     2    E11.4,' eV,   WCR =',E11.4,' eV',/)
C
        ECUTR(M)=MIN(EABS(1,M),EABS(2,M))

        CALL PEMATR(M,M+2014,IWR,INFO)
	  CALL MATDATA(M,IBINARY)
	  CLOSE(M+2014)
      ENDDO
	  CALL COMDATA(IBINARY)
      RETURN
      END
C  *********************************************************************
C                       SUBROUTINE COMDATA
C  *********************************************************************
      SUBROUTINE COMDATA(IBINARY)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER*4 (I-N)
C	  
      CHARACTER*60 DIR
      COMMON/OUTDIR/DIR
      PARAMETER (NEGP=200)
	COMMON/CEGRID/EL,EU,EYT(NEGP),DLEMP(NEGP),DLEMP1,DLFC,
     1  XEL,XE,XEK,KE 
	PARAMETER (MAXMAT=10)
	COMMON/CGRA/X2COH(MAXMAT,241),PDCOH(MAXMAT,241),FLCOH
      PARAMETER (NTP=8000) 
      COMMON/CGPH00/EPH(NTP),XPH(NTP,10),IPHF(99),IPHL(99),NPHS(99)
      PARAMETER (NRX=15000)
      COMMON/CRELAX/P(NRX),ET(NRX),F(NRX),IAL(NRX),IS1(NRX),IS2(NRX),
     1	     IFIRST(99,9),ILAST(99,9),NCUR,KS,MODER ! may have some conflicts
      PARAMETER (NRP=6000)
      COMMON/CESI0/XESI(NRP,9),IESIF(99),IESIL(99),NSESI(99),NCURE
      COMMON/CPSI0/XPSI(NRP,9),IPSIF(99),IPSIL(99),NSPSI(99),NCURP
      COMMON/CADATA/ATW(99),EPX(99),RA1(99),RA2(99),RA3(99),RA4(99),
     1  RA5(99),RSCR(99),ETA(99),EB(99,30)
C	
      
	IOUT=112
      IF(IBINARY.EQ.1) THEN
C     BINARY OUTPUT
          OPEN(IOUT,FILE=TRIM(DIR)//'common.dat',ACCESS="STREAM")
	    WRITE(IOUT) EL, EU, FLCOH
C	photoelectric effect	  
	    WRITE(IOUT) (EPH(K),K=1,NTP)
	    DO I=1,10
		    WRITE(IOUT) (XPH(K,I),K=1,NTP)
	    ENDDO
	    WRITE(IOUT) (IPHF(K),K=1,99)
	    WRITE(IOUT) (IPHL(K),K=1,99)
	    WRITE(IOUT) (NPHS(K),K=1,99)
C	relax
	    WRITE(IOUT) (ET(K),K=1,NRX)
	    WRITE(IOUT) (F(K),K=1,NRX)
	    WRITE(IOUT) (IAL(K),K=1,NRX)
	    WRITE(IOUT) (IS1(K),K=1,NRX)
	    WRITE(IOUT) (IS2(K),K=1,NRX)
	    DO I=1,9
		    WRITE(IOUT) (IFIRST(K,I),K=1,99)
	    ENDDO
	    DO I=1,9
		    WRITE(IOUT) (ILAST(K,I),K=1,99)
	    ENDDO
C	Inner shell impact
	    DO I=1,9
		    WRITE(IOUT) (XESI(K,I),K=1,6000)
	    ENDDO
	    WRITE(IOUT) (IESIF(K),K=1,99)
	    WRITE(IOUT) (NSESI(K),K=1,99)

	    DO I=1,9
		    WRITE(IOUT) (XPSI(K,I),K=1,6000)
	    ENDDO
	    WRITE(IOUT) (IPSIF(K),K=1,99)
	    WRITE(IOUT) (NSPSI(K),K=1,99)
C	Common data
	    DO I=1,30
		    WRITE(IOUT) (EB(K,I),K=1,99)
	    ENDDO
      ELSE
          OPEN(IOUT,FILE='common.dat')
          WRITE(IOUT,'(''data independent of material type'')')  
	    WRITE(IOUT,'('' ***EL, EU, FLCOH'')')
	    WRITE(IOUT,'(3(ES22.14))') EL, EU, FLCOH
C	photoelectric effect	  
	    WRITE(IOUT,'('' ***EPH[8000]'')')
	    WRITE(IOUT,'(8000(ES22.14))') (EPH(K),K=1,NTP)
	    WRITE(IOUT,'('' ***XPH[8000][10], 10 lines'')')
	    DO I=1,10
		    WRITE(IOUT,'(8000(ES22.14))') (XPH(K,I),K=1,NTP)
	    ENDDO
	    WRITE(IOUT,'('' ***IPHF[99]'')')
	    WRITE(IOUT,'(99(I6))') (IPHF(K),K=1,99)
	    WRITE(IOUT,'('' ***IPHL[99]'')')
	    WRITE(IOUT,'(99(I6))') (IPHL(K),K=1,99)
	    WRITE(IOUT,'('' ***NPHS[99]'')')
	    WRITE(IOUT,'(99(I6))') (NPHS(K),K=1,99)
C	relax
	    WRITE(IOUT,'('' ***ET[15000]'')')
	    WRITE(IOUT,'(15000(ES22.14))') (ET(K),K=1,NRX)
	    WRITE(IOUT,'('' ***FR[15000]'')')
	    WRITE(IOUT,'(15000(ES22.14))') (F(K),K=1,NRX)
	    WRITE(IOUT,'('' ***IAL[15000]'')')
	    WRITE(IOUT,'(15000(I6))') (IAL(K),K=1,NRX)
	    WRITE(IOUT,'('' ***IS1[15000]'')')
	    WRITE(IOUT,'(15000(I6))') (IS1(K),K=1,NRX)
	    WRITE(IOUT,'('' ***IS2[15000]'')')
	    WRITE(IOUT,'(15000(I6))') (IS2(K),K=1,NRX)
	    WRITE(IOUT,'('' ***IFIRST[99][9]'')')
	    DO I=1,9
		    WRITE(IOUT,'(99(I6))') (IFIRST(K,I),K=1,99)
	    ENDDO
	    WRITE(IOUT,'('' ***ILAST[99][9]'')')
	    DO I=1,9
		    WRITE(IOUT,'(99(I6))') (ILAST(K,I),K=1,99)
	    ENDDO
C	Inner shell impact
	    WRITE(IOUT,'('' ***XESI[6000][9]'')')
	    DO I=1,9
		    WRITE(IOUT,'(6000(ES22.14))') (XESI(K,I),K=1,6000)
	    ENDDO
	    WRITE(IOUT,'('' ***IESIF[99]'')')
	    WRITE(IOUT,'(99(I6))') (IESIF(K),K=1,99)
	    WRITE(IOUT,'('' ***NSESI[99]'')')
	    WRITE(IOUT,'(99(I6))') (NSESI(K),K=1,99)

	    WRITE(IOUT,'('' ***XPSI[6000][9]'')')
	    DO I=1,9
		    WRITE(IOUT,'(6000(ES22.14))') (XPSI(K,I),K=1,6000)
	    ENDDO
	    WRITE(IOUT,'('' ***IPSIF[99]'')')
	    WRITE(IOUT,'(99(I6))') (IPSIF(K),K=1,99)
	    WRITE(IOUT,'('' ***NSPSI[99]'')')
	    WRITE(IOUT,'(99(I6))') (NSPSI(K),K=1,99)
C	Common data
	    WRITE(IOUT,'('' ***EB[99][30]'')')
	    DO I=1,30
		    WRITE(IOUT,'(99(ES22.14))') (EB(K,I),K=1,99)
	    ENDDO
      ENDIF
	
	
	  
	CLOSE(IOUT)
      RETURN
      END 
      
C  *********************************************************************
C                       SUBROUTINE COMDATA
C  *********************************************************************
      SUBROUTINE MATDATA(M,IBINARY)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER*4 (I-N)
      CHARACTER*2 LASYMB
      CHARACTER*60 NAME,LNAME
      CHARACTER*60 DIR
      COMMON/OUTDIR/DIR
      PARAMETER (A0B=5.291772083D-9)  ! Bohr radius (cm)
      PARAMETER (HREV=27.2113834D0)  ! Hartree energy (eV)
      PARAMETER (AVOG=6.02214199D23)  ! Avogadro's number
      PARAMETER (REV=5.10998902D5)  ! Electron rest energy (eV)
      PARAMETER (SL=137.03599976D0)  ! Speed of light (1/alpha)
      PARAMETER (PI=3.1415926535897932D0, FOURPI=4.0D0*PI)
C  ****  Composition data.
      PARAMETER (MAXMAT=10)
      COMMON/COMPOS/STF(MAXMAT,30),ZT(MAXMAT),AT(MAXMAT),RHO(MAXMAT),
     1  VMOL(MAXMAT),IZ(MAXMAT,30),NELEM(MAXMAT)
C  ****  Element data.
      COMMON/CADATA/ATW(99),EPX(99),RA1(99),RA2(99),RA3(99),RA4(99),
     1  RA5(99),RSCR(99),ETA(99),EB(99,30),IFI(99,30),IKS(99,30),
     2  NSHT(99),LASYMB(99)
C  ****  Simulation parameters.
      COMMON/CSIMPA/EABS(3,MAXMAT),C1(MAXMAT),C2(MAXMAT),WCC(MAXMAT),
     1  WCR(MAXMAT)
      COMMON/CECUTR/ECUTR(MAXMAT)
C  ****  Energy grid and interpolation constants for the current energy.
      PARAMETER (NEGP=200)
      COMMON/CEGRID/EL,EU,ET(NEGP),DLEMP(NEGP),DLEMP1,DLFC,
     1  XEL,XE,XEK,KE
      COMMON/CRANGE/RANGE(3,MAXMAT,NEGP),RANGEL(3,MAXMAT,NEGP)
C  ****  E/P inelastic collisions.
      PARAMETER (NO=64)
      COMMON/CEIN/EXPOT(MAXMAT),OP2(MAXMAT),F(MAXMAT,NO),UI(MAXMAT,NO),
     1  WRI(MAXMAT,NO),KZ(MAXMAT,NO),KS(MAXMAT,NO),NOSC(MAXMAT)
      COMMON/CEINAC/EINAC(MAXMAT,NEGP,NO),PINAC(MAXMAT,NEGP,NO)
      COMMON/CEINTF/T1EI(NEGP),T2EI(NEGP),T1PI(NEGP),T2PI(NEGP)
C  ****  Partial cross sections of individual shells/oscillators.
      COMMON/CEIN00/SXH0(NO),SXH1(NO),SXH2(NO),SXS0(NO),SXS1(NO),
     1              SXS2(NO),SXT0(NO),SXT1(NO),SXT2(NO)
      COMMON/CPIN00/SYH0(NO),SYH1(NO),SYH2(NO),SYS0(NO),SYS1(NO),
     1              SYS2(NO),SYT0(NO),SYT1(NO),SYT2(NO)
C  ****  Compton scattering.
      PARAMETER (NOCO=64)
      COMMON/CGCO/FCO(MAXMAT,NOCO),UICO(MAXMAT,NOCO),FJ0(MAXMAT,NOCO),
     2  KZCO(MAXMAT,NOCO),KSCO(MAXMAT,NOCO),NOSCCO(MAXMAT)
C  ****  Electron simulation tables.
      COMMON/CEIMFP/SEHEL(MAXMAT,NEGP),SEHIN(MAXMAT,NEGP),
     1  SEISI(MAXMAT,NEGP),SEHBR(MAXMAT,NEGP),SEAUX(MAXMAT,NEGP),
     2  SETOT(MAXMAT,NEGP),CSTPE(MAXMAT,NEGP),RSTPE(MAXMAT,NEGP),
     3  DEL(MAXMAT,NEGP),W1E(MAXMAT,NEGP),W2E(MAXMAT,NEGP),
     4  RNDCE(MAXMAT,NEGP),AE(MAXMAT,NEGP),BE(MAXMAT,NEGP),
     5  T1E(MAXMAT,NEGP),T2E(MAXMAT,NEGP)
C  ****  Positron simulation tables.
      COMMON/CPIMFP/SPHEL(MAXMAT,NEGP),SPHIN(MAXMAT,NEGP),
     1  SPISI(MAXMAT,NEGP),SPHBR(MAXMAT,NEGP),SPAN(MAXMAT,NEGP),
     2  SPAUX(MAXMAT,NEGP),SPTOT(MAXMAT,NEGP),CSTPP(MAXMAT,NEGP),
     3  RSTPP(MAXMAT,NEGP),W1P(MAXMAT,NEGP),W2P(MAXMAT,NEGP),
     4  RNDCP(MAXMAT,NEGP),AP(MAXMAT,NEGP),BP(MAXMAT,NEGP),
     5  T1P(MAXMAT,NEGP),T2P(MAXMAT,NEGP)
C  ****  Photon simulation tables.
      COMMON/CGIMFP/SGRA(MAXMAT,NEGP),SGCO(MAXMAT,NEGP),
     1  SGPH(MAXMAT,NEGP),SGPP(MAXMAT,NEGP),SGAUX(MAXMAT,NEGP)
      PARAMETER (NDIM=1500)
      COMMON/CGPH01/ER(NDIM),XSR(NDIM),NPHD
C  ****  Auxiliary arrays.
      DIMENSION EIT(NEGP),EITL(NEGP),FL(NEGP),F1(NEGP),F2(NEGP),
     1  F3(NEGP),F4(NEGP),A(NEGP),B(NEGP),C(NEGP),D(NEGP),RADY(NEGP)
C  ****  Inner shell ionization by electron and positron impact.
      PARAMETER (NRP=6000)
      COMMON/CESI0/XESI(NRP,9),IESIF(99),IESIL(99),NSESI(99),NCURE
      COMMON/CPSI0/XPSI(NRP,9),IPSIF(99),IPSIL(99),NSPSI(99),NCURP
C
	COMMON/CGRA/X2COH(MAXMAT,241),PDCOH(MAXMAT,241),FLCOH
	COMMON/CGPP00/ZEQPP(MAXMAT),F0(MAXMAT,2),BCB(MAXMAT)
	COMMON/CELSEP/EELMAX(MAXMAT),PELMAX(MAXMAT)
	PARAMETER (NPEL=128)
	COMMON/CEELDB/XSE(NPEL,NEGP,MAXMAT),PSE(NPEL,NEGP,MAXMAT),
	1			  ASE(NPEL,NEGP,MAXMAT),BSE(NPEL,NEGP,MAXMAT),
	2			  ITLE(NPEL,NEGP,MAXMAT),ITUE(NPEL,NEGP,MAXMAT)
	COMMON/CPELDB/XSP(NPEL,NEGP,MAXMAT),PSP(NPEL,NEGP,MAXMAT),
	1			  ASP(NPEL,NEGP,MAXMAT),BSP(NPEL,NEGP,MAXMAT),
	2			  ITLP(NPEL,NEGP,MAXMAT),ITUP(NPEL,NEGP,MAXMAT)
	PARAMETER (NBW=32)
	COMMON/CEBR/WB(NBW),PBCUT(MAXMAT,NEGP),WBCUT(MAXMAT,NEGP),
     1  PDFB(MAXMAT,NEGP,NBW),PACB(MAXMAT,NEGP,NBW),ZBR2(MAXMAT)
	COMMON/CBRANG/BET(6),BK(21),BP1(MAXMAT,6,21,4),BP2(MAXMAT,6,21,4),
     1              ZBEQ(MAXMAT)
C	
C	output the data tables
	WRITE(LNAME,'(A,''mat'',i0,''.dat'')' ) TRIM(DIR), M
      IOUT=111
      IF(IBINARY.EQ.1) THEN
          OPEN(IOUT,FILE=LNAME, ACCESS="STREAM")
          WRITE(IOUT) M
          WRITE(IOUT) EABS(1,M), EABS(2,M), EABS(3,M)
          WRITE(IOUT) C1(M), C2(M), WCC(M), WCR(M), ECUTR(M)
          WRITE(IOUT) ZT(M), AT(M), RHO(M), VMOL(M), NELEM(M)
          WRITE(IOUT) (STF(M,K),K=1,30)
          WRITE(IOUT) (IZ(M,K),K=1,30)
C	photon simulation data
	    WRITE(IOUT) (SGRA(M,K),K=1,NEGP)
	    WRITE(IOUT) (SGCO(M,K),K=1,NEGP)
	    WRITE(IOUT) (SGPH(M,K),K=1,NEGP)
	    WRITE(IOUT) (SGPP(M,K),K=1,NEGP)
C	Rayleigh scattering
	    WRITE(IOUT) (X2COH(M,K),K=1,241)
	    WRITE(IOUT) (PDCOH(M,K),K=1,241)
	
C	Compton scattering
	    WRITE(IOUT) (FCO(M,K),K=1,64)
	    WRITE(IOUT) (UICO(M,K),K=1,64)
	    WRITE(IOUT) (FJ0(M,K),K=1,64)
	    WRITE(IOUT) (KZCO(M,K),K=1,64)
	    WRITE(IOUT) (KSCO(M,K),K=1,64)
	    WRITE(IOUT) NOSCCO(M)
C	pair production
	    WRITE(IOUT) ZEQPP(M),F0(M,2),BCB(M)
C	electron simulation table
	    WRITE(IOUT) (SEHEL(M,K),K=1,NEGP)
	    WRITE(IOUT) (SEHIN(M,K),K=1,NEGP)
	    WRITE(IOUT) (SEISI(M,K),K=1,NEGP)
	    WRITE(IOUT) (SEHBR(M,K),K=1,NEGP)
	    WRITE(IOUT) (SETOT(M,K),K=1,NEGP)
	    WRITE(IOUT) (CSTPE(M,K),K=1,NEGP)
	    WRITE(IOUT) (RSTPE(M,K),K=1,NEGP)
	    WRITE(IOUT) (DEL(M,K),K=1,NEGP)
	    WRITE(IOUT) (W1E(M,K),K=1,NEGP)
	    WRITE(IOUT) (W2E(M,K),K=1,NEGP)
	    WRITE(IOUT) (T1E(M,K),K=1,NEGP)
	    WRITE(IOUT) (T2E(M,K),K=1,NEGP)
	    WRITE(IOUT) (RNDCE(M,K),K=1,NEGP)
	    WRITE(IOUT) (AE(M,K),K=1,NEGP)
	    WRITE(IOUT) (BE(M,K),K=1,NEGP)
C	positron simulation table
	    WRITE(IOUT) (SPHEL(M,K),K=1,NEGP)
	    WRITE(IOUT) (SPHIN(M,K),K=1,NEGP)
	    WRITE(IOUT) (SPISI(M,K),K=1,NEGP)
	    WRITE(IOUT) (SPHBR(M,K),K=1,NEGP)
	    WRITE(IOUT) (SPAN(M,K),K=1,NEGP)
	    WRITE(IOUT) (SPTOT(M,K),K=1,NEGP)
	    WRITE(IOUT) (CSTPP(M,K),K=1,NEGP)
	    WRITE(IOUT) (RSTPP(M,K),K=1,NEGP)
	    WRITE(IOUT) (W1P(M,K),K=1,NEGP)
	    WRITE(IOUT) (W2P(M,K),K=1,NEGP)
	    WRITE(IOUT) (T1P(M,K),K=1,NEGP)
	    WRITE(IOUT) (T2P(M,K),K=1,NEGP)
	    WRITE(IOUT) (RNDCP(M,K),K=1,NEGP)
	    WRITE(IOUT) (AP(M,K),K=1,NEGP)
	    WRITE(IOUT) (BP(M,K),K=1,NEGP)
C	electron/position elastic collision
	    WRITE(IOUT) EELMAX(M), PELMAX(M)
C	electron
	    DO I=1,128
		    WRITE(IOUT) (XSE(I,K,M),K=1,NEGP)
	    ENDDO
	    DO I=1,128
		    WRITE(IOUT) (PSE(I,K,M),K=1,NEGP)
	    ENDDO
	    DO I=1,128
		    WRITE(IOUT) (ASE(I,K,M),K=1,NEGP)
	    ENDDO
	    DO I=1,128
		    WRITE(IOUT) (BSE(I,K,M),K=1,NEGP)
	    ENDDO
	    DO I=1,128
		    WRITE(IOUT) (ITLE(I,K,M),K=1,NEGP)
	    ENDDO
	    DO I=1,128
		    WRITE(IOUT) (ITUE(I,K,M),K=1,NEGP)
	    ENDDO
C	positron
	    DO I=1,128
		    WRITE(IOUT) (XSP(I,K,M),K=1,NEGP)
	    ENDDO
	    DO I=1,128
		    WRITE(IOUT) (PSP(I,K,M),K=1,NEGP)
	    ENDDO
	    DO I=1,128
		    WRITE(IOUT) (ASP(I,K,M),K=1,NEGP)
	    ENDDO
	    DO I=1,128
		    WRITE(IOUT) (BSP(I,K,M),K=1,NEGP)
	    ENDDO
	    DO I=1,128
		    WRITE(IOUT) (ITLP(I,K,M),K=1,NEGP)
	    ENDDO
	    DO I=1,128
		    WRITE(IOUT) (ITUP(I,K,M),K=1,NEGP)
	    ENDDO
C	Electron/Positron inelastic collision
	    WRITE(IOUT) EXPOT(M), OP2(M)
	    WRITE(IOUT) (F(M,K),K=1,64)
	    WRITE(IOUT) (UI(M,K),K=1,64)
	    WRITE(IOUT) (WRI(M,K),K=1,64)
	    WRITE(IOUT) (KZ(M,K),K=1,64)
	    WRITE(IOUT) (KS(M,K),K=1,64)
	    WRITE(IOUT) NOSC(M)
	    DO I=1,64
		    WRITE(IOUT) (EINAC(M,K,I),K=1,NEGP)
	    ENDDO
	    DO I=1,64
		    WRITE(IOUT) (PINAC(M,K,I),K=1,NEGP)
	    ENDDO
C	Bremsstrahlung emission
	    WRITE(IOUT) (PBCUT(M,K),K=1,NEGP)
	    WRITE(IOUT) (WBCUT(M,K),K=1,NEGP)
	    DO I=1,32
		    WRITE(IOUT) (PDFB(M,K,I),K=1,NEGP)
	    ENDDO
	    DO I=1,32
		    WRITE(IOUT) (PACB(M,K,I),K=1,NEGP)
	    ENDDO
	    DO I=1,6
		    DO J=1,21
			    WRITE(IOUT) (BP1(M,I,J,K),K=1,4)
		    ENDDO
	    ENDDO
	    DO I=1,6
		    DO J=1,21
			    WRITE(IOUT) (BP2(M,I,J,K),K=1,4)
		    ENDDO
	    ENDDO
		DO K=1,3
		    WRITE(IOUT) (RANGE(K,M,I),I=1,NEGP)
	    ENDDO
      ELSE
          OPEN(111,FILE=LNAME)
	    WRITE(IOUT,'('' Material Index:'',i10)') M
C     simulation parameters
	    WRITE(IOUT,'('' ***EABS[3]'')')
	    WRITE(IOUT,'(3(ES22.14))') EABS(1,M), EABS(2,M), EABS(3,M)
	    WRITE(IOUT,'('' ***C1, C2, WCC, WCR, ECUTR'')')
	    WRITE(IOUT,'(5(ES22.14))') C1(M), C2(M), WCC(M), WCR(M), ECUTR(M)
C     composition data
	    WRITE(IOUT,'('' ***ZT, AT, RHO, VMOL, NELEM'')')
	    WRITE(IOUT,'(4(ES22.14),I6)') ZT(M), AT(M), RHO(M), VMOL(M),
     1 NELEM(M)
	    WRITE(IOUT,'('' ***STF[30]'')')
	    WRITE(IOUT,'(30(ES22.14))') (STF(M,K),K=1,30)
	    WRITE(IOUT,'('' ***IZ[30]'')')
	    WRITE(IOUT,'(30(I6))') (IZ(M,K),K=1,30)
C	photon simulation data
	    WRITE(IOUT,'('' ***SGRA[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (SGRA(M,K),K=1,200)
	    WRITE(IOUT,'('' ***SGCO[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (SGCO(M,K),K=1,200)
	    WRITE(IOUT,'('' ***SGPH[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (SGPH(M,K),K=1,200)
	    WRITE(IOUT,'('' ***SGPP[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (SGPP(M,K),K=1,200)
C	Rayleigh scattering
	    WRITE(IOUT,'('' ***X2COH[241]'')')
	    WRITE(IOUT,'(241(ES22.14))') (X2COH(M,K),K=1,241)
	    WRITE(IOUT,'('' ***PDCOH[241]'')')
	    WRITE(IOUT,'(241(ES22.14))') (PDCOH(M,K),K=1,241)
	
C	Compton scattering
	    WRITE(IOUT,'('' ***FCO[64]'')')
	    WRITE(IOUT,'(64(ES22.14))') (FCO(M,K),K=1,64)
	    WRITE(IOUT,'('' ***UICO[64]'')')
	    WRITE(IOUT,'(64(ES22.14))') (UICO(M,K),K=1,64)
	    WRITE(IOUT,'('' ***FJ0[64]'')')
	    WRITE(IOUT,'(64(ES22.14))') (FJ0(M,K),K=1,64)
	    WRITE(IOUT,'('' ***KZCO[64]'')')
	    WRITE(IOUT,'(64(I6))') (KZCO(M,K),K=1,64)
	    WRITE(IOUT,'('' ***KSCO[64]'')')
	    WRITE(IOUT,'(64(I6))') (KSCO(M,K),K=1,64)
	    WRITE(IOUT,'('' ***NOSCCO'')')
	    WRITE(IOUT,'(I6)') NOSCCO(M)
C	pair production
	    WRITE(IOUT,'('' ***ZEQPP,Fpp,Bpp'')')
	    WRITE(IOUT,'(3(ES22.14))') ZEQPP(M),F0(M,2),BCB(M)
C	electron simulation table
	    WRITE(IOUT,'('' ***SEHEL[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (SEHEL(M,K),K=1,200)
	    WRITE(IOUT,'('' ***SEHIN[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (SEHIN(M,K),K=1,200)
	    WRITE(IOUT,'('' ***SEISI[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (SEISI(M,K),K=1,200)
	    WRITE(IOUT,'('' ***SEHBR[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (SEHBR(M,K),K=1,200)
	    WRITE(IOUT,'('' ***SETOT[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (SETOT(M,K),K=1,200)
	    WRITE(IOUT,'('' ***CSTPE[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (CSTPE(M,K),K=1,200)
	    WRITE(IOUT,'('' ***RSTPE[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (RSTPE(M,K),K=1,200)
	    WRITE(IOUT,'('' ***DEL[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (DEL(M,K),K=1,200)
	    WRITE(IOUT,'('' ***W1E[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (W1E(M,K),K=1,200)
	    WRITE(IOUT,'('' ***W2E[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (W2E(M,K),K=1,200)
	    WRITE(IOUT,'('' ***T1E[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (T1E(M,K),K=1,200)
	    WRITE(IOUT,'('' ***T2E[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (T2E(M,K),K=1,200)
	    WRITE(IOUT,'('' ***RNDCE[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (RNDCE(M,K),K=1,200)
	    WRITE(IOUT,'('' ***AE[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (AE(M,K),K=1,200)
	    WRITE(IOUT,'('' ***BE[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (BE(M,K),K=1,200)
C	positron simulation table
	    WRITE(IOUT,'('' ***SPHEL[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (SPHEL(M,K),K=1,200)
	    WRITE(IOUT,'('' ***SPHIN[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (SPHIN(M,K),K=1,200)
	    WRITE(IOUT,'('' ***SPISI[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (SPISI(M,K),K=1,200)
	    WRITE(IOUT,'('' ***SPHBR[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (SPHBR(M,K),K=1,200)
	    WRITE(IOUT,'('' ***SPAN[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (SPAN(M,K),K=1,200)
	    WRITE(IOUT,'('' ***SPTOT[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (SPTOT(M,K),K=1,200)
	    WRITE(IOUT,'('' ***CSTPP[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (CSTPP(M,K),K=1,200)
	    WRITE(IOUT,'('' ***RSTPP[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (RSTPP(M,K),K=1,200)
	    WRITE(IOUT,'('' ***W1P[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (W1P(M,K),K=1,200)
	    WRITE(IOUT,'('' ***W2P[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (W2P(M,K),K=1,200)
	    WRITE(IOUT,'('' ***T1P[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (T1P(M,K),K=1,200)
	    WRITE(IOUT,'('' ***T2P[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (T2P(M,K),K=1,200)
	    WRITE(IOUT,'('' ***RNDCP[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (RNDCP(M,K),K=1,200)
	    WRITE(IOUT,'('' ***AP[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (AP(M,K),K=1,200)
	    WRITE(IOUT,'('' ***BP[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (BP(M,K),K=1,200)
C	electron/position elastic collision
	    WRITE(IOUT,'('' ***EELMAX, PELMAX'')')
	    WRITE(IOUT,'(2(ES22.14))') EELMAX(M), PELMAX(M)
C	electron
	    WRITE(IOUT,'('' ***XSE[128][200]'')')
	    DO I=1,128
		    WRITE(IOUT,'(200(ES22.14))') (XSE(I,K,M),K=1,200)
	    ENDDO
	    WRITE(IOUT,'('' ***PSE[128][200]'')')
	    DO I=1,128
		    WRITE(IOUT,'(200(ES22.14))') (PSE(I,K,M),K=1,200)
	    ENDDO
	    WRITE(IOUT,'('' ***ASE[128][200]'')')
	    DO I=1,128
		    WRITE(IOUT,'(200(ES22.14))') (ASE(I,K,M),K=1,200)
	    ENDDO
	    WRITE(IOUT,'('' ***BSE[128][200]'')')
	    DO I=1,128
		    WRITE(IOUT,'(200(ES22.14))') (BSE(I,K,M),K=1,200)
	    ENDDO
	    WRITE(IOUT,'('' ***ITLE[128][200]'')')
	    DO I=1,128
		    WRITE(IOUT,'(200(I6))') (ITLE(I,K,M),K=1,200)
	    ENDDO
	    WRITE(IOUT,'('' ***ITUE[128][200]'')')
	    DO I=1,128
		    WRITE(IOUT,'(200(I6))') (ITUE(I,K,M),K=1,200)
	    ENDDO
C	positron
	    WRITE(IOUT,'('' ***XSP[128][200]'')')
	    DO I=1,128
		    WRITE(IOUT,'(200(ES22.14))') (XSP(I,K,M),K=1,200)
	    ENDDO
	    WRITE(IOUT,'('' ***PSP[128][200]'')')
	    DO I=1,128
		    WRITE(IOUT,'(200(ES22.14))') (PSP(I,K,M),K=1,200)
	    ENDDO
	    WRITE(IOUT,'('' ***ASP[128][200]'')')
	    DO I=1,128
		    WRITE(IOUT,'(200(ES22.14))') (ASP(I,K,M),K=1,200)
	    ENDDO
	    WRITE(IOUT,'('' ***BSP[128][200]'')')
	    DO I=1,128
		    WRITE(IOUT,'(200(ES22.14))') (BSP(I,K,M),K=1,200)
	    ENDDO
	    WRITE(IOUT,'('' ***ITLP[128][200]'')')
	    DO I=1,128
		    WRITE(IOUT,'(200(I6))') (ITLP(I,K,M),K=1,200)
	    ENDDO
	    WRITE(IOUT,'('' ***ITUP[128][200]'')')
	    DO I=1,128
		    WRITE(IOUT,'(200(I6))') (ITUP(I,K,M),K=1,200)
	    ENDDO
C	Electron/Positron inelastic collision
	    WRITE(IOUT,'('' ***EXPOT, OP2'')')
	    WRITE(IOUT,'(2(ES22.14))') EXPOT(M), OP2(M)
	    WRITE(IOUT,'('' ***F[64]'')')
	    WRITE(IOUT,'(64(ES22.14))') (F(M,K),K=1,64)
	    WRITE(IOUT,'('' ***UI[64]'')')
	    WRITE(IOUT,'(64(ES22.14))') (UI(M,K),K=1,64)
	    WRITE(IOUT,'('' ***WRI[64]'')')
	    WRITE(IOUT,'(64(ES22.14))') (WRI(M,K),K=1,64)
	    WRITE(IOUT,'('' ***KZ[64]'')')
	    WRITE(IOUT,'(64(I6))') (KZ(M,K),K=1,64)
	    WRITE(IOUT,'('' ***KS[64]'')')
	    WRITE(IOUT,'(64(I6))') (KS(M,K),K=1,64)
	    WRITE(IOUT,'('' ***NOSC'')')
	    WRITE(IOUT,'(I6)') NOSC(M)
	    WRITE(IOUT,'('' ***EINAC[200][64]'')')
	    DO I=1,64
		    WRITE(IOUT,'(200(ES22.14))') (EINAC(M,K,I),K=1,200)
	    ENDDO
	    WRITE(IOUT,'('' ***PINAC[200][64]'')')
	    DO I=1,64
		    WRITE(IOUT,'(200(ES22.14))') (PINAC(M,K,I),K=1,200)
	    ENDDO
C	Bremsstrahlung emission
	    WRITE(IOUT,'('' ***PBCUT[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (PBCUT(M,K),K=1,200)
	    WRITE(IOUT,'('' ***WBCUT[200]'')')
	    WRITE(IOUT,'(200(ES22.14))') (WBCUT(M,K),K=1,200)
	    WRITE(IOUT,'('' ***PDFB[200][32]'')')
	    DO I=1,32
		    WRITE(IOUT,'(200(ES22.14))') (PDFB(M,K,I),K=1,200)
	    ENDDO
	    WRITE(IOUT,'('' ***PACB[200][32]'')')
	    DO I=1,32
		    WRITE(IOUT,'(200(ES22.14))') (PACB(M,K,I),K=1,200)
	    ENDDO
	    WRITE(IOUT,'('' ***BP1[6][21][4]'')')
	    DO I=1,6
		    DO J=1,21
			    WRITE(IOUT,'(4(ES22.14))') (BP1(M,I,J,K),K=1,4)
		    ENDDO
	    ENDDO
	    WRITE(IOUT,'('' ***BP2[6][21][4]'')')
	    DO I=1,6
		    DO J=1,21
			    WRITE(IOUT,'(4(ES22.14))') (BP2(M,I,J,K),K=1,4)
		    ENDDO
	    ENDDO
      ENDIF
C
	CLOSE(IOUT) 
	
      RETURN
      END
