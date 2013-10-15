#include <misc.h>
#include <params.h>
      subroutine trcmix(pmid    , clat    , coslat  ,n2o     ,ch4     ,
     $                  cfc11   , cfc12   )
C-----------------------------------------------------------------------
C
c Specify zonal mean mass mixing ratios of CH4, N2O, CFC11 and
c CFC12
C
C Distributions assume constant mixing ratio in the troposphere
C and a decrease of mixing ratio in the stratosphere. Tropopause
C defined by ptrop. The scale height of the particular trace gas
C depends on latitude. This assumption produces a more realistic
C stratospheric distribution of the various trace gases.
C
C-------------------------Code History----------------------------------
C
C Original version:  J.T.Kiehl Nov 21 1994
C Standardized:      T. Acker, Feb 1996
C Reviewed:          J. Kiehl, April 1996        
C 
C-----------------------------------------------------------------------
c
c $Id: trcmix.F,v 1.1 2004/09/07 15:27:42 rca Exp $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C-----------------------------Parameters--------------------------------
#include <prgrid.h>
C------------------------------Commons----------------------------------
#include <comvmr.h>
C-----------------------------Arguments---------------------------------
C
C Input
C
      real pmid(plond,plev)        ! model pressures
      real clat                    ! current latitude in radians
      real coslat                  ! cosine of latitude
C
C Output
C
      real n2o(plond,plev)         ! nitrous oxide mass mixing ratio
      real ch4(plond,plev)         ! methane mass mixing ratio
      real cfc11(plond,plev)       ! cfc11 mass mixing ratio
      real cfc12(plond,plev)       ! cfc12 mass mixing ratio
C
C--------------------------Local Variables------------------------------
C
      integer i                ! longitude loop index
      integer k                ! level index
C
      real dlat                ! latitude in degrees
      real ptrop               ! pressure level of tropopause
      real pratio              ! pressure divided by ptrop
C
      real xn2o                ! pressure scale height for n2o
      real xch4                ! pressure scale height for ch4
      real xcfc11              ! pressure scale height for cfc11
      real xcfc12              ! pressure scale height for cfc12
C
      real ch40                ! tropospheric mass mixing ratio for ch4
      real n2o0                ! tropospheric mass mixing ratio for n2o
      real cfc110              ! tropospheric mass mixing ratio for cfc11
      real cfc120              ! tropospheric mass mixing ratio for cfc12
C
C
C-----------------------------------------------------------------------
C
C set tropospheric mass mixing ratios 
C
      ch40   = rmwch4 * ch4vmr
      n2o0   = rmwn2o * n2ovmr
      cfc110 = rmwf11 * f11vmr
      cfc120 = rmwf12 * f12vmr
C
C set stratospheric scale height factor for gases
C
      dlat = abs(57.2958 * clat)
      if(dlat.le.45.0) then
        xn2o = 0.3478 + 0.00116 * dlat
        xch4 = 0.2353
        xcfc11 = 0.7273 + 0.00606 * dlat
        xcfc12 = 0.4000 + 0.00222 * dlat
      else
        xn2o = 0.4000 + 0.013333 * (dlat - 45)
        xch4 = 0.2353 + 0.0225489 * (dlat - 45)
        xcfc11 = 1.00 + 0.013333 * (dlat - 45)
        xcfc12 = 0.50 + 0.024444 * (dlat - 45)
      end if
C
C pressure of tropopause
C
      ptrop = 250.0e2 - 150.0e2*coslat**2.0
C
C determine output mass mixing ratios
C
      do k = 1,plev
         do i = 1,plon
            if(pmid(i,k).ge.ptrop) then
              ch4(i,k) = ch40
              n2o(i,k) = n2o0
              cfc11(i,k) = cfc110
              cfc12(i,k) = cfc120
            else
              pratio = pmid(i,k)/ptrop
              ch4(i,k) = ch40 * (pratio)**xch4
              n2o(i,k) = n2o0 * (pratio)**xn2o
              cfc11(i,k) = cfc110 * (pratio)**xcfc11
              cfc12(i,k) = cfc120 * (pratio)**xcfc12
            end if
         end do
      end do
C
      return
C
      end
 
