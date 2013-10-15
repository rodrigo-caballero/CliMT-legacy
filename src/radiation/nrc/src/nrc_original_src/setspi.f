      subroutine setspi(WNOI,DWNI,WAVEI)

C  GCM2.0  Feb 2003
C
C     PURPOSE:
C        Set up the spectral intervals in the infrared.  Based on
C     Chris McKay's SETSPI code.
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
C        VERSION 2  OCT 2001
C
C     INPUT PARAMETERS
C     L_NSPECTI  - Number of spectral intervals in the INFRARED
C
C     OUTPUT PARAMETERS
C     WNOI       - Array of wavenumbers at the spectral interval
C                  centers for the infrared.  Array is NSPECTI
C                  elements long.
C     DWNI       - Array of "delta wavenumber", i.e., the width,
C                  in wavenumbers (cm^-1) of each IR spectral
C                  interval.  NSPECTI elements long.
C     WAVEI      - Array (NSPECTI elements long) of the wavelenght
C                  (in microns) at the center of each IR spectral
C                  interval.
C     
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

      real*8 planckir(L_NSPECTI,8501)
      common / plnr / planckir

C     BWNI - Bin wavenumber of the edges of the IR spectral bins
C     units are inverse centimeters.  Dimension needs to be changed
C     if the number of IR bins changes.

      REAL*8 BWNI(L_NSPECTI+1)
      REAL*8 WNOI(L_NSPECTI), DWNI(L_NSPECTI), WAVEI(L_NSPECTI)

      real*8 a, b, x(12), w(12), ans, y, bpa, bma, T
      real*8 c1, c2, wn1, wn2, PI
      integer n, nw, nt, m

C  C1 and C2 values from Goody and Yung (2nd edition)  MKS units
C  These values lead to a "sigma" (sigma*T^4) of 5.67032E-8 W m^-2 K^-4

      data c1 / 3.741832D-16 /     ! W m^-2
      data c2 / 1.438786D-2  /     ! m K
      data PI / 3.14159265358979D0 /
      
      data x / -0.981560634246719D0,  -0.904117256370475D0,
     *         -0.769902674194305D0,  -0.587317954286617D0,
     *         -0.367831498998180D0,  -0.125233408511469D0,
     *          0.125233408511469D0,   0.367831498998180D0,
     *          0.587317954286617D0,   0.769902674194305D0,
     *          0.904117256370475D0,   0.981560634246719D0    /

      data w /  0.047175336386512D0,   0.106939325995318D0,
     *          0.160078328543346D0,   0.203167426723066D0,
     *          0.233492536538355D0,   0.249147045813403D0,
     *          0.249147045813403D0,   0.233492536538355D0,
     *          0.203167426723066D0,   0.160078328543346D0,
     *          0.106939325995318D0,   0.047175336386512D0   /

C======================================================================C

C     Bin wavenumber - wavenumber [cm^(-1)] at the edges of the IR
C     spectral bins.

      BWNI( 1) =   10.000D0
      BWNI( 2) =  166.667D0
      BWNI( 3) =  416.667D0
      BWNI( 4) =  833.333D0
      BWNI( 5) = 1250.000D0
      BWNI( 6) = 2500.000D0

C     Set up mean wavenumbers and wavenumber deltas.  Units of 
C     wavenumbers is cm^(-1); units of wavelengths is microns.

      do M=1,L_NSPECTI
        WNOI(M)  = 0.5*(BWNI(M+1)+BWNI(M))
        DWNI(M)  = BWNI(M+1)-BWNI(M)
        WAVEI(M) = 1.0E+4/WNOI(M)
      end do

C  For each IR wavelength interval, compute the integral of B(T), the
C  Planck function, divided by the wavelength interval, in cm-1.  The
C  integration is in MKS units, the final answer is the same as the
C  original planck.f; W m^-2 wavenumber^-1, where wavenumber is in CM^-1.

      DO NW=1,L_NSPECTI
        a = 1.0D-2/BWNI(NW+1)
        b = 1.0D-2/BWNI(NW)
        bpa = (b+a)/2.0
        bma = (b-a)/2.0
        do nt=500,9000
          T   = dble(NT)/1.0D+1
          ans = 0.0D0
          do m=1,12
            y    = bma*x(m)+bpa
            ans  = ans + w(m)*c1/(y**5*(exp(c2/(y*T))-1.0D0))
          end do
          planckir(NW,nt-499) = ans*bma/(PI*DWNI(NW))
        end do
      END DO

      return
      end
