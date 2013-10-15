      SUBROUTINE SETSPV(WNOV,DWNV,WAVEV,SOLARF,TAURAY)

C  GCM2.0  Feb 2003
C
C     PURPOSE:
C        Set up the spectral intervals in the Visual (solar).  Based
C     on Chris McKay's SETSPV code.
C
C     AUTHOR
C        Jim Schaeffer 
C
C     UPDATES FOR
C        Bob Haberle
C
C     ENVIRONMENT
C        Sun ULTRA-2       SOLARIS 2.5.1      FORTRAN
C
C     REVISION HISTORY
C        Original 9/17/1998
C     VERSION 2.0  OCT 2001
C
C     INPUT PARAMETERS
C     L_NSPECTV  - Number of spectral intervals in the Visual
C
C     OUTPUT PARAMETERS
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
C     TAURAY     - Array (NSPECTV elements) of the wavelength dependent
C                  part of Rayleigh Scattering.  The pressure dependent 
C                  part is computed elsewhere (OPTCV).
C     CALLED BY
C        RADIATION
C
C     SUBROUTINES CALLED
C        NONE 
C
C**********************************************************************C

      implicit none

      include "grid.h"
      include "radinc.h"

C     BWNV - Bin wavenumber of the edges of the VISUAL spectral bins
C     units are inverse centimeters.  Dimension needs to be changed
C     if the number of VISUAL bins changes.

      REAL*8 BWNV(L_NSPECTV+1)
      REAL*8 WNOV(L_NSPECTV), DWNV(L_NSPECTV), WAVEV(L_NSPECTV)
      REAL*8 SOLAR(L_NSPECTV), SOLARF(L_NSPECTV), TAURAY(L_NSPECTV)

      REAL*8  P0, GRAV, SCALEP, SUM, WL
      INTEGER N, M

C     P0      - Rayleigh scattering reference pressure in pascals.
C     GRAV    - Acceleration due to gravity (g) - MKS
C     SCALEP  - multiply by 100 to convert pressure from millibars
C               to pascals.

      DATA P0     / 9.423D+6 /
      DATA GRAV   / 3.72     /
      DATA SCALEP / 100.0    /

C     Bin wavenumber - wavenumber [cm^(-1)] at the edges of the VISUAL
C     spectral bins.  Go from smaller to larger wavenumbers, the same as
C     in the IR.

      DATA BWNV / 2222.22D0,  3087.37D0,  4030.63D0,  5370.57D0,
     *            7651.11D0, 12500.00D0, 25000.00D0, 41666.67D0 /

C     Solar flux within each spectral interval, at 1AU (W/M^2)
C     Sum equals 1356 W/m^2 (values from Allen, 4th edition)

      DATA SOLAR /  17.0, 29.0, 52.0, 148.0, 348.0, 643.0, 118.0 /

C======================================================================C

C     Set up mean wavenumbers and wavenumber deltas.  Units of 
C     wavenumbers is cm^(-1); units of wavelengths is microns.

      do M=1,L_NSPECTV
        WNOV(M)  = 0.5*(BWNV(M+1)+BWNV(M))
        DWNV(M)  = BWNV(M+1)-BWNV(M)
        WAVEV(M) = 1.0E+4/WNOV(M)
      end do

C     Sum the solar flux, and write out the result.  

      sum = 0.0
      do N=1,L_NSPECTV
        SOLARF(N) = SOLAR(N)
        sum       = sum+SOLARF(N)
      end do
      write(6,'("Solar flux at 1AU = ",f7.2," W/M^2")') sum

C     Set up the wavelength dependent part of Rayleigh Scattering.
C     The pressure dependent part will be computed elsewhere (OPTCV).
C     WAVEV is in microns.  There is no Rayleigh scattering in the IR.

      do N=1,L_NSPECTV
        WL        = WAVEV(N)
        TAURAY(N) = (8.7/grav)*(1.527*(1.0+0.013/wl**2)/wl**4)*
     *               scalep/P0
      end do

      RETURN
      END
