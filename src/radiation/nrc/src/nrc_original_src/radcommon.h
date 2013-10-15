C----------------------------------------------------------------------C
C
C                             radcommon.h
C                         FORTRAN PARAMETERS
C                          GCM2.0  Feb 2003
C
C----------------------------------------------------------------------C
C
C  "Include" grid.h and radinc.h before this file in code that uses
C  some or all of this common data set
C
C     WNOI       - Array of wavenumbers at the spectral interval
C                  centers for the infrared.  Array is NSPECTI
C                  elements long.
C     DWNI       - Array of "delta wavenumber", i.e., the width,
C                  in wavenumbers (cm^-1) of each IR spectral
C                  interval.  NSPECTI elements long.
C     WAVEI      - Array (NSPECTI elements long) of the wavelenght
C                  (in microns) at the center of each IR spectral
C                  interval.
C     WNOV       - Array of wavenumbers at the spectral interval
C                  center for the VISUAL.  Array is NSPECTV
C                  elements long.
C     DWNV       - Array of "delta wavenumber", i.e., the width,
C                  in wavenumbers (cm^-1) of each VISUAL spectral
C                  interval.  NSPECTV elements long.
C     WAVEV      - Array (NSPECTV elements long) of the wavelenght
C                  (in microns) at the center of each VISUAL spectral
C                  interval.
C     SOLARF     - Array (NSPECTV elements) of solar flux (W/M^2) in
C                  each spectral interval.  Values are for 1 AU, and
C                  are scaled to the Mars distance elsewhere.
C     TAURAY     - Array (NSPECTV elements) of the pressure-independent
C                  part of Rayleigh scattering optical depth.
C     PTOP       - Pressure at the top of the radiation code coordinate;
C                  = 0.5*Ptrop
C     FZEROI     - Fraction of zeros in the IR CO2 k-coefficients, for
C                  each temperature, pressure, and spectral interval
C     FZEROV     - Fraction of zeros in the VISUAL CO2 k-coefficients, for
C                  each temperature, pressure, and spectral interval
C
C     AEROSOL RADIATIVE OPTICAL CONSTANTS
C     Values are at the wavelenght interval center
C
C     MIE SCATTERING - Size distribution weighted
C     Qextv    - Extinction efficiency - in the visual.
C     QextREF  - Reference visual wavelength (.67 micron band)
C     Qscatv   - Scattering efficiency - in the visual.
C     WV       - Single scattering albedo - in the visual.
C     GV       - Asymmetry parameter - in the visual.
C
C     Qexti    - Extinction efficiency - in the infrared.
C     Qscati   - Scattering efficiency - in the infrared.
C     WI       - Single scattering albedo - in the infrared.
C     GI       - Asymmetry parameter - in the infrared.
C
C     VIS2IR   - VISIBLE (0.67 micron band) to IR (9 micron band) ratio.
C     
C  "Include" grid.h and radinc.h before this file in code that uses
C  some or all of this common data set

      REAL*8 WNOI(L_NSPECTI), DWNI(L_NSPECTI), WAVEI(L_NSPECTI)
      REAL*8 WNOV(L_NSPECTV), DWNV(L_NSPECTV), WAVEV(L_NSPECTV)
      REAL*8 SOLARF(L_NSPECTV), TAURAY(L_NSPECTV)

      real*8 CO2I(L_NTREF,L_PINT,L_REFH2O,L_NSPECTI,L_NGAUSS)
      real*8 CO2V(L_NTREF,L_PINT,L_REFH2O,L_NSPECTV,L_NGAUSS)
      real*8 FZEROI(L_NSPECTI)
      real*8 FZEROV(L_NSPECTV)
      real*8 PGASREF(L_NPREF), TGASREF(L_NTREF)

      real*8 qextv(L_NSPECTV), qscatv(L_NSPECTV), wv(L_NSPECTV)
      real*8 gv(L_NSPECTV)
      real*8 QextREF, VIS2IR

      real*8 qexti(L_NSPECTI), qscati(L_NSPECTI), wi(L_NSPECTI)
      real*8 gi(L_NSPECTI)

      real*8 planckir(L_NSPECTI,8501)

      real*8 PTOP, UBARI, TAUREF(L_LEVELS+1), GWEIGHT(L_NGAUSS)
      real*8 PFGASREF(L_PINT)

C  H2O and CO2 k-coefficients mixed

      real*8 WREFCO2(L_REFH2O), WREFH2O(L_REFH2O)

C  PLANCK defined variable

      common / plnr     / planckir

C  SETSPI and SETSPV defined variables

      common / radcom1 / WNOI, DWNI, WAVEI, WNOV, DWNV, WAVEV,
     *                   SOLARF, TAURAY, PTOP, TAUREF, GWEIGHT, UBARI,
     *                   PFGASREF

C  SETRAD defined variables

      common / radcom2 / CO2I, CO2V, PGASREF, TGASREF,
     *                   QEXTV, QSCATV, WV, GV,
     *                   QEXTI, QSCATI, WI, GI, QextREF, VIS2IR

      common / radcom3 / fzeroi, fzerov

      common / radcom4 / WREFCO2, WREFH2O

