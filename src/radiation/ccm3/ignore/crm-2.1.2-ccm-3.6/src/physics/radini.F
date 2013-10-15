#include <misc.h>
#include <params.h>
      subroutine radini(gravx   ,cpairx  ,epsilox ,stebolx )
C-----------------------------------------------------------------------
C
C Initialize various constants for radiation scheme; note that
C the radiation scheme uses cgs units.
C
C---------------------------Code history--------------------------------
C
C Original version:  CCM1
C Standardized:      J. Rosinski, June 1992
C Reviewed:          J. Kiehl, B. Briegleb, August 1992
C Reviewed:          J. Kiehl, April 1996
C Reviewed:          B. Briegleb, May 1996
C
C-----------------------------------------------------------------------
c
c $Id: radini.F,v 1.1 2004/09/07 15:27:42 rca Exp $
c
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <prgrid.h>
C------------------------------Commons----------------------------------
#include <comozp.h>
C-----------------------------------------------------------------------
#include <crdcae.h>
C-----------------------------------------------------------------------
#include <comvmr.h>
C-----------------------------------------------------------------------
#include <crdcon.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real gravx      ! Acceleration of gravity (MKS)
      real cpairx     ! Specific heat of dry air (MKS)
      real epsilox    ! Ratio of mol. wght of H2O to dry air
      real stebolx    ! Stefan-Boltzmann's constant (MKS)
C
C---------------------------Local variables-----------------------------
C
      integer iband   ! H2O band index

      real v0         ! Volume of a gas at stp (m**3/kmol)
      real p0         ! Standard pressure (pascals)
      real amd        ! Effective molecular weight of dry air (kg/kmol)
      real goz        ! Acceleration of gravity (m/s**2)
C
C-----------------------------------------------------------------------
C
C Set general radiation consts; convert to cgs units where appropriate:
C
      gravit  =  100.*gravx
      rga     =  1./gravit
      cpair   =  1.e4*cpairx
      epsilo  =  epsilox
      sslp    =  1.013250e6
      stebol  =  1.e3*stebolx
      rgsslp  =  0.5/(gravit*sslp)
      dpfo3   =  2.5e-3
      dpfco2  =  5.0e-3
      dayspy  =  365.
      pie     =  4.*atan(1.)
C
C Determine co2 global mass mixing ratio
C
      co2mmr = rmwco2 * co2vmr
C
C Coefficients for h2o emissivity and absorptivity.
C
      do iband=1,4
        c1(iband) = coefe(3,iband)/coefe(2,iband)
        c2(iband) = coefb(3,iband)/coefb(2,iband)
        c3(iband) = coefb(4,iband)/coefb(3,iband)
        c4(iband) = coefd(3,iband)/coefd(2,iband)
        c5(iband) = coefd(4,iband)/coefd(3,iband)
        c6(iband) = coefa(3,iband)/coefa(2,iband)
        c7(iband) = coefc(3,iband)/coefc(2,iband)
      end do
      c8   = coeff(3,1)/coeff(2,1)
      c9   = coeff(3,2)/coeff(2,2)
      c10  = coeff(4,1)/coeff(3,1)
      c11  = coeff(4,2)/coeff(3,2)
      c12  = coeff(5,1)/coeff(4,1)
      c13  = coeff(5,2)/coeff(4,2)
      c14  = coeff(6,1)/coeff(5,1)
      c15  = coeff(6,2)/coeff(5,2)
      c16  = coefj(3,1)/coefj(2,1)
      c17  = coefk(3,1)/coefk(2,1)
      c18  = coefi(3,1)/coefi(2,1)
      c19  = coefi(3,2)/coefi(2,2)
      c20  = coefi(4,1)/coefi(3,1)
      c21  = coefi(4,2)/coefi(3,2)
      c22  = coefi(5,1)/coefi(4,1)
      c23  = coefi(5,2)/coefi(4,2)
      c24  = coefi(6,1)/coefi(5,1)
      c25  = coefi(6,2)/coefi(5,2)
      c26  = coefj(3,2)/coefj(2,2)
      c27  = coefk(3,2)/coefk(2,2)
      c28  = .5
      c29  = .002053
      c30  = .1
      c31  = 3.0e-5
      cfa1 = .61
C
C Initialize further longwave constants referring to far wing
C correction; R&D refers to:
C
C            Ramanathan, V. and  P.Downey, 1986: A Nonisothermal
C            Emissivity and Absorptivity Formulation for Water Vapor
C            Journal of Geophysical Research, vol. 91., D8, pp 8649-8666
C
      fwcoef = .1           ! See eq(33) R&D
      fwc1   = .30          ! See eq(33) R&D
      fwc2   = 4.5          ! See eq(33) and eq(34) in R&D
      fc1    = 2.6          ! See eq(34) R&D
C
C Initialize ozone data.
C
      v0  = 22.4136         ! Volume of a gas at stp (m**3/kmol)
      p0  = 0.1*sslp        ! Standard pressure (pascals)
      amd = 28.9644         ! Molecular weight of dry air (kg/kmol)
      goz = gravx           ! Acceleration of gravity (m/s**2)
C
C Constants for ozone path integrals (multiplication by 100 for unit
C conversion to cgs from mks):
C
      cplos = v0/(amd*goz)       *100.0
      cplol = v0/(amd*goz*p0)*0.5*100.0
C
      return
      end
 
