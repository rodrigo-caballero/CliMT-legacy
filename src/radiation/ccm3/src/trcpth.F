#include <misc.h>
#include <params.h>
      subroutine trcpth(tnm     ,pnm     ,cfc11   ,cfc12   ,n2o     ,
     $                  ch4     ,qnm     ,ucfc11  ,ucfc12  ,un2o0   ,
     $                  un2o1   ,uch4    ,uco211  ,uco212  ,uco213  ,
     $                  uco221  ,uco222  ,uco223  ,bn2o0   ,bn2o1   ,
     $                  bch4    ,uptype  )
c----------------------------------------------------------------------
c
c Calculate path lengths and pressure factors for CH4, N2O, CFC11
c and CFC12. 
c
C See CCM3 description for details
C
C-------------------------Code History----------------------------------
C
C Original version:  J.T. Kiehl, Nov 21 1994
C Standardized:      T. Acker, Feb 1996
C Reviewed:          J. Kiehl, Apr 1996        
C 
c-----------------------------------------------------------------------
c
c $Id: trcpth.F,v 1.2 2004/09/07 02:47:46 rca Exp $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <prgrid.h>
C------------------------------Commons----------------------------------
#include <crdcon.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real tnm(plond,plev)      ! Model level temperatures
      real pnm(plond,plevp)     ! Pres. at model interfaces (dynes/cm2)
      real qnm(plond,plev)      ! h2o specific humidity
      real cfc11(plond,plev)    ! CFC11 mass mixing ratio
c
      real cfc12(plond,plev)    ! CFC12 mass mixing ratio
      real n2o(plond,plev)      ! N2O mass mixing ratio
      real ch4(plond,plev)      ! CH4 mass mixing ratio
C
C Output arguments
C
      real ucfc11(plond,plevp)  ! CFC11 path length
      real ucfc12(plond,plevp)  ! CFC12 path length
      real un2o0(plond,plevp)   ! N2O path length
      real un2o1(plond,plevp)   ! N2O path length (hot band)
      real uch4(plond,plevp)    ! CH4 path length
c
      real uco211(plond,plevp)  ! CO2 9.4 micron band path length
      real uco212(plond,plevp)  ! CO2 9.4 micron band path length
      real uco213(plond,plevp)  ! CO2 9.4 micron band path length
      real uco221(plond,plevp)  ! CO2 10.4 micron band path length
      real uco222(plond,plevp)  ! CO2 10.4 micron band path length
c
      real uco223(plond,plevp)  ! CO2 10.4 micron band path length
      real bn2o0(plond,plevp)   ! pressure factor for n2o
      real bn2o1(plond,plevp)   ! pressure factor for n2o
      real bch4(plond,plevp)    ! pressure factor for ch4
      real uptype(plond,plevp)  ! p-type continuum path length
C
C---------------------------Local variables-----------------------------
C
      integer   i               ! Longitude index
      integer   k               ! Level index
c
      real co2fac(plond,1)      ! co2 factor
      real alpha1(plond)        ! stimulated emission term
      real alpha2(plond)        ! stimulated emission term
      real rt(plond)            ! reciprocal of local temperature
      real rsqrt(plond)         ! reciprocal of sqrt of temp
c
      real pbar(plond)          ! mean pressure
      real dpnm(plond)          ! difference in pressure
      real diff                 ! diffusivity factor
C
C--------------------------Data Statements------------------------------
C
      data diff /1.66/
C
c-----------------------------------------------------------------------
c
c  Calculate path lengths for the trace gases at model top
c
      do i = 1,plon
         ucfc11(i,1) = 1.8 * cfc11(i,1) * pnm(i,1) * rga
         ucfc12(i,1) = 1.8 * cfc12(i,1) * pnm(i,1) * rga
         un2o0(i,1) = diff * 1.02346e5 * n2o(i,1) * pnm(i,1) * rga 
     $                       / sqrt(tnm(i,1))
         un2o1(i,1) = diff * 2.01909 * un2o0(i,1) * 
     $                    exp(-847.36/tnm(i,1))
         uch4(i,1) = diff * 8.60957e4 * ch4(i,1) * pnm(i,1) * rga 
     $                       / sqrt(tnm(i,1))
         co2fac(i,1) = diff * co2mmr * pnm(i,1) * rga
         alpha1(i) = (1.0 - exp(-1540.0/tnm(i,1)))**3.0/sqrt(tnm(i,1))
         alpha2(i) = (1.0 - exp(-1360.0/tnm(i,1)))**3.0/sqrt(tnm(i,1))
         uco211(i,1) = 3.42217e3 * co2fac(i,1) * alpha1(i) * 
     $                             exp(-1849.7/tnm(i,1))
         uco212(i,1) = 6.02454e3 * co2fac(i,1) * alpha1(i) * 
     $                             exp(-2782.1/tnm(i,1))
         uco213(i,1) = 5.53143e3 * co2fac(i,1) * alpha1(i) * 
     $                             exp(-3723.2/tnm(i,1))
         uco221(i,1) = 3.88984e3 * co2fac(i,1) * alpha2(i) * 
     $                             exp(-1997.6/tnm(i,1))
         uco222(i,1) = 3.67108e3 * co2fac(i,1) * alpha2(i) * 
     $                             exp(-3843.8/tnm(i,1))
         uco223(i,1) = 6.50642e3 * co2fac(i,1) * alpha2(i) * 
     $                             exp(-2989.7/tnm(i,1))
         bn2o0(i,1) = diff * 19.399 * pnm(i,1)**2.0 * n2o(i,1) * 
     $                  1.02346e5 * rga / (sslp*tnm(i,1))
         bn2o1(i,1) = bn2o0(i,1) * exp(-847.36/tnm(i,1)) *
     $                  2.06646e5
         bch4(i,1) = diff * 2.94449 * ch4(i,1) * pnm(i,1)**2.0 * rga *
     $                  8.60957e4 / (sslp*tnm(i,1))
         uptype(i,1) = diff * qnm(i,1) * pnm(i,1)**2.0 *
     $                   exp(1800.0*(1.0/tnm(i,1) - 1.0/296.0)) *
     $                   rga / sslp
      end do
C
C Calculate trace gas path lengths through model atmosphere
C
      do k = 1,plev
         do i = 1,plon
            rt(i) = 1./tnm(i,k)
            rsqrt(i) = sqrt(rt(i))
            pbar(i) = 0.5 * (pnm(i,k+1) + pnm(i,k)) / sslp
            dpnm(i) = (pnm(i,k+1) - pnm(i,k)) * rga
            alpha1(i) = diff * rsqrt(i) * 
     $                         (1.0 - exp(-1540.0/tnm(i,k)))**3.0
            alpha2(i) = diff * rsqrt(i) * 
     $                         (1.0 - exp(-1360.0/tnm(i,k)))**3.0
            ucfc11(i,k+1) = ucfc11(i,k) +  1.8 * cfc11(i,k) * dpnm(i)
            ucfc12(i,k+1) = ucfc12(i,k) +  1.8 * cfc12(i,k) * dpnm(i)
            un2o0(i,k+1) = un2o0(i,k) + diff * 1.02346e5 * 
     $                                  n2o(i,k) * rsqrt(i) * dpnm(i)
            un2o1(i,k+1) = un2o1(i,k) + diff * 2.06646e5 * n2o(i,k) *
     $           rsqrt(i) * exp(-847.36/tnm(i,k)) * dpnm(i)
            uch4(i,k+1) = uch4(i,k) + diff * 8.60957e4 * ch4(i,k) * 
     $           rsqrt(i) * dpnm(i)
            uco211(i,k+1) = uco211(i,k) + 1.15*3.42217e3 * alpha1(i) *
     $            co2mmr * exp(-1849.7/tnm(i,k)) * dpnm(i)
            uco212(i,k+1) = uco212(i,k) + 1.15*6.02454e3 * alpha1(i) *
     $            co2mmr * exp(-2782.1/tnm(i,k)) * dpnm(i)
            uco213(i,k+1) = uco213(i,k) + 1.15*5.53143e3 * alpha1(i) *
     $            co2mmr * exp(-3723.2/tnm(i,k)) * dpnm(i)
            uco221(i,k+1) = uco221(i,k) + 1.15*3.88984e3 * alpha2(i) *
     $            co2mmr * exp(-1997.6/tnm(i,k)) * dpnm(i)
            uco222(i,k+1) = uco222(i,k) + 1.15*3.67108e3 * alpha2(i) *
     $            co2mmr * exp(-3843.8/tnm(i,k)) * dpnm(i)
            uco223(i,k+1) = uco223(i,k) + 1.15*6.50642e3 * alpha2(i) *
     $            co2mmr * exp(-2989.7/tnm(i,k)) * dpnm(i)
            bn2o0(i,k+1) = bn2o0(i,k) + diff * 19.399 * pbar(i) * rt(i)
     $          * 1.02346e5 * n2o(i,k) * dpnm(i)
            bn2o1(i,k+1) = bn2o1(i,k) + diff * 19.399 * pbar(i) * rt(i) 
     $          * 2.06646e5 * exp(-847.36/tnm(i,k)) * n2o(i,k)*dpnm(i)
            bch4(i,k+1) = bch4(i,k) + diff * 2.94449 * rt(i) * pbar(i)
     $            * 8.60957e4 * ch4(i,k) * dpnm(i)
            uptype(i,k+1) = uptype(i,k) + diff *qnm(i,k) * 
     $                   exp(1800.0*(1.0/tnm(i,k) - 1.0/296.0)) *
     $                   pbar(i) * dpnm(i)
         end do
       end do
C
       return
C
       end
 
