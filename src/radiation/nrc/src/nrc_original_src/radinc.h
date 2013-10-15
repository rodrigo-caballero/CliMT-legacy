C======================================================================C
C
C     RADINC.H    RADiation INCludes
C
C     Includes for the radiation code; RADIATION LAYERS, LEVELS,
C     number of spectral intervals. . .
C
C     GCM2.0  Feb 2003
C 
C======================================================================C

C     RADIATION parameters

C     In radiation code, layer 1 corresponds to the stratosphere.  Level
C     1 is the top of the stratosphere.  The dummy layer is at the same
C     temperature as the (vertically isothermal) stratosphere, and
C     any time it is explicitly needed, the appropriate quantities will
C     be dealt with (aka "top". . .)

C     L_NLEVRAD corresponds to the surface - i.e., the GCM Level that
C     is at the surface.  PLEV(L_NLEVRAD) = P(J,I)+PTROP, 
C     PLEV(2) = PTROP, PLEV(1) = ptop

C     L_NLAYRAD is the number of radiation code layers
C     L_NLEVRAD is the number of radiation code levels.  Level N is the
C               top of layer N. 
C
C     L_NSPECTI is the number of IR spectral intervals
C     L_NSPECTV is the number of Visual(or Solar) spectral intervals
C     L_NGAUSS  is the number of Gauss points for K-coefficients
C               GAUSS POINT 9 (aka the last one) is the special case
C     L_NWNGI   is L_NSPECTI*L_NGAUSS;  the total number of "intervals"
C               in the IR
C     L_NWNGV   is L_NSPECTV*L_NGAUSS;  the total number of "intervals"
C               in the VISUAL
C
C     L_NPREF   is the number of reference pressures that the 
C               k-coefficients are calculated on
C     L_PINT    is the number of Lagrange interpolated reference
C               pressures for the CO2 k-coefficients.
C     L_NTREF   is the number of refernce temperatures for the
C               k-coefficients
C     L_TAUMAX  is the largest optical depth - larger ones are set
C               to this value.
C
C     L_REFH2O  The number of different water-mixing ratio values for
C               the k-coefficients that are now CO2+H2O. 
C
C     L_NREFV   The spectral interval number of the visible reference
C               wavelength (i.e. the 0.67 micron band) 
C
C----------------------------------------------------------------------C

      integer   L_NLAYRAD, L_NLEVRAD, L_NSPECTI, L_NSPECTV
      integer   L_NGAUSS,  L_NPREF, L_NTREF, L_TAUMAX
      integer   L_PINT, L_REFH2O, L_NREFV

      parameter (L_NLAYRAD  = L_LAYERS+1)   
      parameter (L_NLEVRAD  = L_LAYERS+2)
      
      parameter (L_NSPECTI =  5)
      parameter (L_NSPECTV =  7)
      parameter (L_NGAUSS  = 17)

      parameter (L_NPREF   = 11)
      parameter (L_NTREF   =  7)
      parameter (L_TAUMAX  = 35)

      parameter (L_PINT    = 51)

      parameter (L_REFH2O  = 10)

      parameter (L_NREFV   = 6)

