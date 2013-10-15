#include <misc.h>
#include <params.h>
      subroutine radems(s2c     ,s2t     ,w       ,tplnke  ,plh2o   ,
     $                  pnm     ,plco2   ,tint    ,tint4   ,tlayr   ,
     $                  tlayr4  ,plol    ,plos    ,ucfc11  ,ucfc12  , 
     $                  un2o0   ,un2o1   ,uch4    ,uco211  ,uco212  ,
     $                  uco213  ,uco221  ,uco222  ,uco223  ,uptype  ,
     $                  bn2o0   ,bn2o1   ,bch4    ,co2em   ,co2eml  ,
     $                  co2t    ,h2otr   ,abplnk1 ,abplnk2 ,emstot  )
C-----------------------------------------------------------------------
C
C Compute emissivity for H2O, CO2, O3, CH4, N2O, CFC11 and CFC12
C
C H2O  ....  Uses nonisothermal emissivity for water vapor from
C            Ramanathan, V. and  P.Downey, 1986: A Nonisothermal
C            Emissivity and Absorptivity Formulation for Water Vapor
C            Jouranl of Geophysical Research, vol. 91., D8, pp 8649-8666
C
C
C CO2  ....  Uses absorptance parameterization of the 15 micro-meter
C            (500 - 800 cm-1) band system of Carbon Dioxide, from
C            Kiehl, J.T. and B.P.Briegleb, 1991: A New Parameterization
C            of the Absorptance Due to the 15 micro-meter Band System
C            of Carbon Dioxide Jouranl of Geophysical Research,
C            vol. 96., D5, pp 9013-9019. Also includes the effects
C            of the 9.4 and 10.4 micron bands of CO2.
C
C O3   ....  Uses absorptance parameterization of the 9.6 micro-meter
C            band system of ozone, from Ramanathan, V. and R. Dickinson,
C            1979: The Role of stratospheric ozone in the zonal and
C            seasonal radiative energy balance of the earth-troposphere
C            system. Journal of the Atmospheric Sciences, Vol. 36,
C            pp 1084-1104
C
C ch4  ....  Uses a broad band model for the 7.7 micron band of methane.
C  
C n20  ....  Uses a broad band model for the 7.8, 8.6 and 17.0 micron
C            bands of nitrous oxide
C
C cfc11 ...  Uses a quasi-linear model for the 9.2, 10.7, 11.8 and 12.5
C            micron bands of CFC11
C
C cfc12 ...  Uses a quasi-linear model for the 8.6, 9.1, 10.8 and 11.2
C            micron bands of CFC12
C
C
C Computes individual emissivities, accounting for band overlap, and
C sums to obtain the total.
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
c $Id: radems.F,v 1.1 2004/09/07 15:27:42 rca Exp $
c
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <prgrid.h>
C------------------------------Commons----------------------------------
#include <crdcae.h>
C-----------------------------------------------------------------------
#include <crdcon.h>
C-----------------------------------------------------------------------
#include <comvmr.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real s2c(plond,plevp)        ! H2o continuum path length
      real s2t(plond,plevp)        ! Tmp and prs wghted h2o path length
      real w(plond,plevp)          ! H2o path length
      real tplnke(plond)           ! Layer planck temperature
      real plh2o(plond,plevp)      ! H2o prs wghted path length
      real pnm(plond,plevp)        ! Model interface pressure
      real plco2(plond,plevp)      ! Prs wghted path of co2
      real tint(plond,plevp)       ! Model interface temperatures
      real tint4(plond,plevp)      ! Tint to the 4th power
      real tlayr(plond,plevp)      ! K-1 model layer temperature
      real tlayr4(plond,plevp)     ! Tlayr to the 4th power
      real plol(plond,plevp)       ! Pressure wghtd ozone path
      real plos(plond,plevp)       ! Ozone path
c
c Trace gas variables
c
      real ucfc11(plond,plevp)     ! CFC11 path length
      real ucfc12(plond,plevp)     ! CFC12 path length
      real un2o0(plond,plevp)      ! N2O path length
      real un2o1(plond,plevp)      ! N2O path length (hot band)
      real uch4(plond,plevp)       ! CH4 path length
      real uco211(plond,plevp)     ! CO2 9.4 micron band path length
      real uco212(plond,plevp)     ! CO2 9.4 micron band path length
      real uco213(plond,plevp)     ! CO2 9.4 micron band path length
      real uco221(plond,plevp)     ! CO2 10.4 micron band path length
      real uco222(plond,plevp)     ! CO2 10.4 micron band path length
      real uco223(plond,plevp)     ! CO2 10.4 micron band path length
      real bn2o0(plond,plevp)      ! pressure factor for n2o
      real bn2o1(plond,plevp)      ! pressure factor for n2o
      real bch4(plond,plevp)       ! pressure factor for ch4
      real uptype(plond,plevp)     ! p-type continuum path length
C
C Output arguments
C
      real emstot(plond,plevp)     ! Total emissivity
      real co2em(plond,plevp)      ! Layer co2 normalzd plnck funct drvtv
      real co2eml(plond,plev)      ! Intrfc co2 normalzd plnck func drvtv
      real co2t(plond,plevp)       ! Tmp and prs weighted path length
      real h2otr(plond,plevp)      ! H2o transmission over o3 band
      real emplnk(14,plond)        ! emissivity Planck factor
      real abplnk1(14,plond,plevp) ! non-nearest layer Plack factor
      real abplnk2(14,plond,plevp) ! nearest layer factor
      real emstrc(plond,plevp)     ! total trace gas emissivity
C
C---------------------------Local variables-----------------------------
C
      integer i                    ! Longitude index
      integer k                    ! Level index]
      integer k1                   ! Level index
      integer iband                ! H2o band index
C
C Local variables for H2O:
C
      real h2oems(plond,plevp)     ! H2o emissivity
      real tpathe(plond)           ! Used to compute h2o emissivity
      real a(plond)                ! Eq(2) in table A3a of R&D
      real corfac(plond)           ! Correction factors in table A3b
      real dtp(plond)              ! Path temperature minus 300 K used in 
C                                    h2o rotation band absorptivity
      real dtx(plond)              ! Planck temperature minus 250 K
      real dty(plond)              ! Path temperature minus 250 K
      real dtz(plond)              ! Planck temperature minus 300 K
      real emis(plond,4)           ! Total emissivity (h2o+co2+o3)
      real rsum(plond)             ! Eq(1) in table A2 of R&D
      real term1(plond,4)          ! Equation(5) in table A3a of R&D(1986)
      real term2(plond,4)          ! Delta a(Te) in table A3a of R&D(1986)
      real term3(plond,4)          ! B(T) function for rotation and
C                                    vibration-rotation band emissivity
      real term4(plond,4)          ! Equation(6) in table A3a of R&D(1986)
      real term5(plond,4)          ! Delta a(Tp) in table A3a of R&D(1986)
      real term6(plond,2)          ! B(T) function for window region
      real term7(plond,2)          ! Kl_inf(i) in eq(8) of table A3a of R&D
      real term8(plond,2)          ! Delta kl_inf(i) in eq(8)
      real term9(plond,2)          ! B(T) function for 500-800 cm-1 region
      real tr1(plond)              ! Equation(6) in table A2 for 650-800
      real tr2(plond)              ! Equation(6) in table A2 for 500-650
      real tr3(plond)              ! Equation(4) in table A2 for 650-800
      real tr4(plond)              ! Equation(4),table A2 of R&D for 500-650
      real tr7(plond)              ! Equation (6) times eq(4) in table A2
C                                      of R&D for 650-800 cm-1 region
      real tr8(plond)              ! Equation (6) times eq(4) in table A2
C                                      of R&D for 500-650 cm-1 region
      real uc(plond)               ! Y + 0.002U in eq(8) of table A2 of R&D
      real pnew(plond)             ! Effective pressure for h2o linewidth
      real trline(plond,2)         ! Transmission due to H2O lines in window
      real k21(plond)              ! Exponential coefficient used to calc
C                                     rot band transmissivity in the 650-800
C                                     cm-1 region (tr1)
      real k22(plond)              ! Exponential coefficient used to calc
C                                     rot band transmissivity in the 500-650
C                                     cm-1 region (tr2)
      real u(plond)                ! Pressure weighted H2O path length
      real uc1(plond)              ! H2o continuum pathlength 500-800 cm-1
      real r80257                  ! Conversion factor for h2o pathlength
      real a11                     ! A1 in table A3b for rotation band emiss
      real a31                     ! A3 in table A3b for rotation band emiss
      real a21                     ! First part in numerator of A2 table A3b
      real a22                     ! Second part in numertor of A2 table A3b
      real a23                     ! Denominator of A2 table A3b (rot band)
      real t1t4                    ! Eq(3) in table A3a of R&D
      real t2t5                    ! Eq(4) in table A3a of R&D
      real fwk                     ! Equation(33) in R&D far wing correction
      real a41                     ! Numerator in A2 in Vib-rot (table A3b)
      real a51                     ! Denominator in A2 in Vib-rot(table A3b)
      real a61                     ! A3 factor for Vib-rot band in table A3b
      real phi                     ! Eq(11) in table A3a of R&D
      real psi                     ! Eq(12) in table A3a of R&D
      real ubar                    ! H2o scaled path comment eq(10) table A2
      real g1                      ! Part of eq(10) table A2
      real pbar                    ! H2o scaled pres comment eq(10) table A2
      real g3                      ! Part of eq(10) table A2
      real g2                      ! Part of arguement in eq(10) in table A2
      real g4                      ! Arguement in exp() in eq(10) table A2
      real cf812                   ! Eq(11) in table A2 of R&D
      real troco2(plond,plevp)     ! H2o overlap factor for co2 absorption
C
C Local variables for CO2:
C
      real co2ems(plond,plevp)      ! Co2 emissivity
      real co2plk(plond)            ! Used to compute co2 emissivity
      real sum(plond)               ! Used to calculate path temperature
      real t1i                      ! Co2 hot band temperature factor
      real sqti                     ! Sqrt of temperature
      real pi                       ! Pressure used in co2 mean line width
      real et                       ! Co2 hot band factor
      real et2                      ! Co2 hot band factor
      real et4                      ! Co2 hot band factor
      real omet                     ! Co2 stimulated emission term
      real ex                       ! Part of co2 planck function
      real f1co2                    ! Co2 weak band factor
      real f2co2                    ! Co2 weak band factor
      real f3co2                    ! Co2 weak band factor
      real t1co2                    ! Overlap factor weak bands strong band
      real sqwp                     ! Sqrt of co2 pathlength
      real f1sqwp                   ! Main co2 band factor
      real oneme                    ! Co2 stimulated emission term
      real alphat                   ! Part of the co2 stimulated emiss term
      real wco2                     ! Consts used to define co2 pathlength
      real posqt                    ! Effective pressure for co2 line width
      real rbeta7                   ! Inverse of co2 hot band line width par
      real rbeta8                   ! Inverse of co2 hot band line width par
      real rbeta9                   ! Inverse of co2 hot band line width par
      real rbeta13                  ! Inverse of co2 hot band line width par
      real tpath                    ! Path temp used in co2 band model
      real tmp1                     ! Co2 band factor
      real tmp2                     ! Co2 band factor
      real tmp3                     ! Co2 band factor
      real tlayr5                   ! Temperature factor in co2 Planck func
      real rsqti                    ! Reciprocal of sqrt of temperature
      real exm1sq                   ! Part of co2 Planck function
      real u7                       ! Absorber amt for various co2 band systems
      real u8                       ! Absorber amt for various co2 band systems
      real u9                       ! Absorber amt for various co2 band systems
      real u13                      ! Absorber amt for various co2 band systems
      real r250                     ! Inverse 250K
      real r300                     ! Inverse 300K
      real rsslp                    ! Inverse standard sea-level pressure
C
C Local variables for O3:
C
      real o3ems(plond,plevp)       ! Ozone emissivity
      real dbvtt(plond)             ! Tmp drvtv of planck fctn for tplnke
      real te                       ! Temperature factor
      real u1                       ! Path length factor
      real u2                       ! Path length factor
      real phat                     ! Effecitive path length pressure
      real tlocal                   ! Local planck function temperature
      real tcrfac                   ! Scaled temperature factor
      real beta                     ! Absorption funct factor voigt effect
      real realnu                   ! Absorption function factor
      real o3bndi                   ! Band absorption factor
C
C Transmission terms for various spectral intervals:
C
      real trem4(plond)             ! H2o   800 - 1000 cm-1
      real trem6(plond)             ! H2o  1000 - 1200 cm-1
      real absbnd                   ! Proportional to co2 band absorptance
      real tco2(plond)              ! co2 overlap factor
      real th2o(plond)              ! h2o overlap factor
      real to3(plond)               ! o3 overlap factor
C
C---------------------------Statement functions-------------------------
C
C Derivative of planck function at 9.6 micro-meter wavelength, and
C an absorption function factor:
C
      real dbvt,fo3,t,ux,vx
C
      dbvt(t)=(-2.8911366682e-4+(2.3771251896e-6+1.1305188929e-10*t)*t)/
     $  (1.0+(-6.1364820707e-3+1.5550319767e-5*t)*t)
C
      fo3(ux,vx)=ux/sqrt(4.+ux*(1.+vx))
C
C-----------------------------------------------------------------------
C
C Initialize
C
      r80257  = 1./8.0257e-04
      r250  = 1./250.
      r300  = 1./300.
      rsslp = 1./sslp
C
C Planck function for co2
C
      do i=1,plon
        ex        = exp(960./tplnke(i))
        co2plk(i) = 5.e8/((tplnke(i)**4)*(ex - 1.))
        co2t(i,1) = tplnke(i)
        sum(i)    = co2t(i,1)*pnm(i,1)
      end do
      k = 1
      do k1=plevp,2,-1
        k = k + 1
        do i=1,plon
          sum(i)         = sum(i) + tlayr(i,k)*(pnm(i,k)-pnm(i,k-1))
          ex             = exp(960./tlayr(i,k1))
          tlayr5         = tlayr(i,k1)*tlayr4(i,k1)
          co2eml(i,k1-1) = 1.2e11*ex/(tlayr5*(ex - 1.)**2)
          co2t(i,k)      = sum(i)/pnm(i,k)
        end do
      end do
C
C Initialize planck function derivative for O3
C
      do i=1,plon
        dbvtt(i) = dbvt(tplnke(i))
      end do
c
c Calculate trace gas Planck functions
c
      call trcplk(tint    ,tlayr   ,tplnke  ,emplnk  ,abplnk1 ,
     $            abplnk2 )
C
C Interface loop
C
      do 200 k1=1,plevp
C
C H2O emissivity
C
C emis(i,1)     0 -  800 cm-1   rotation band
C emis(i,2)  1200 - 2200 cm-1   vibration-rotation band
C emis(i,3)   800 - 1200 cm-1   window
C emis(i,4)   500 -  800 cm-1   rotation band overlap with co2
C
C For the p type continuum
C
        do i=1,plon
          uc(i)     = s2c(i,k1) + 2.e-3*plh2o(i,k1)
          u(i)      = plh2o(i,k1)
          pnew(i)   = u(i)/w(i,k1)
C
C Apply scaling factor for 500-800 continuum
C
          uc1(i)    = (s2c(i,k1) + 1.7e-3*plh2o(i,k1))*
     $               (1. + 2.*s2c(i,k1))/(1. + 15.*s2c(i,k1))
          tpathe(i) = s2t(i,k1)/plh2o(i,k1)
        end do
        do i=1,plon
          dtx(i) = tplnke(i) - 250.
          dty(i) = tpathe(i) - 250.
          dtz(i) = dtx(i) - 50.
          dtp(i) = dty(i) - 50.
        end do
        do iband=1,3,2
          do i=1,plon
            term1(i,iband) = coefe(1,iband) + coefe(2,iband)*
     $                       dtx(i)*(1. + c1(iband)*dtx(i))
            term2(i,iband) = coefb(1,iband) + coefb(2,iband)*
     $                       dtx(i)*(1. + c2(iband)*dtx(i)*
     $                               (1. + c3(iband)*dtx(i)))
            term3(i,iband) = coefd(1,iband) + coefd(2,iband)*
     $                       dtx(i)*(1. +  c4(iband)*dtx(i)*
     $                               (1. + c5(iband)*dtx(i)))
            term4(i,iband) = coefa(1,iband) + coefa(2,iband)*
     $                       dty(i)*(1. + c6(iband)*dty(i))
            term5(i,iband) = coefc(1,iband) + coefc(2,iband)*
     $                       dty(i)*(1. + c7(iband)*dty(i))
          end do
        end do
C
C emis(i,1)     0 -  800 cm-1   rotation band
C
        do i=1,plon
          a11  = .37 - 3.33e-5*dtz(i) + 3.33e-6*dtz(i)*dtz(i)
          a31  = 1.07 - 1.00e-3*dtp(i) + 1.475e-5*dtp(i)*dtp(i)
          a21  = 1.3870 + 3.80e-3*dtz(i) - 7.8e-6*dtz(i)*dtz(i)
          a22  = 1.0 - 1.21e-3*dtp(i) - 5.33e-6*dtp(i)*dtp(i)
          a23  = 0.9 + 2.62*sqrt(u(i))
          corfac(i) = a31*(a11 + ((a21*a22)/a23))
          t1t4 = term1(i,1)*term4(i,1)
          t2t5 = term2(i,1)*term5(i,1)
          a(i) = t1t4 + t2t5/(1. + t2t5*sqrt(u(i))*corfac(i))
          fwk  = fwcoef + fwc1/(1. + fwc2*u(i))
          rsum(i)   = exp(-a(i)*(sqrt(u(i)) + fwk*u(i)))
          emis(i,1) = (1. - rsum(i))*term3(i,1)
C
C emis(i,2)  1200 - 2200 cm-1   vibration-rotation band
C
          a41      = 1.75 - 3.96e-3*dtz(i)
          a51      = 1.00 + 1.3*sqrt(u(i))
          a61      = 1.00 + 1.25e-3*dtp(i) + 6.25e-5*dtp(i)*dtp(i)
          corfac(i)= .3*(1. + (a41)/(a51))*a61
          t1t4     = term1(i,3)*term4(i,3)
          t2t5     = term2(i,3)*term5(i,3)
          a(i)     = t1t4 + t2t5/(1. + t2t5*sqrt(u(i))*corfac(i))
          fwk      = fwcoef + fwc1/(1. + fwc2*u(i))
          rsum(i)  = exp(-a(i)*(sqrt(u(i)) + fwk*u(i)))
          emis(i,2)= (1. - rsum(i))*term3(i,3)
        end do
C
C Line transmission in 800-1000 and 1000-1200 cm-1 intervals
C
        do k=1,2
          do i=1,plon
            phi  = a1(k)*(dty(i) + 15.) + a2(k)*(dty(i) + 15.)**2
            psi  = b1(k)*(dty(i) + 15.) + b2(k)*(dty(i) + 15.)**2
            phi  = exp(phi)
            psi  = exp(psi)
            ubar = w(i,k1)*phi
            ubar = (ubar*1.66)*r80257
            pbar = pnew(i)*(psi/phi)
            cf812 = cfa1 + ((1.-cfa1)/(1. + ubar*pbar*10.))
            g1   = (realk(k)*pbar)/(2.*st(k))
            g2   = 1. + (ubar*4.0*st(k)*cf812)/pbar
            g3   = sqrt(g2) - 1.
            g4   = g1*g3
            trline(i,k) = exp(-g4)
          end do
        end do
        do i=1,plon
          term7(i,1) = coefj(1,1) + coefj(2,1)*dty(i)*(1.+c16*dty(i))
          term8(i,1) = coefk(1,1) + coefk(2,1)*dty(i)*(1.+c17*dty(i))
          term7(i,2) = coefj(1,2) + coefj(2,2)*dty(i)*(1.+c26*dty(i))
          term8(i,2) = coefk(1,2) + coefk(2,2)*dty(i)*(1.+c27*dty(i))
        end do
C
C emis(i,3)   800 - 1200 cm-1   window
C
        do i=1,plon
          term6(i,1) = coeff(1,1) + coeff(2,1)*dtx(i)*
     $                (1. +  c8*dtx(i)*(1. + c10*dtx(i)*
     $                 (1. + c12*dtx(i)*(1. + c14*dtx(i)))))
          trem4(i)  = exp(-(coefg(1,1)+coefg(2,1)*dtx(i))*uc(i))
     $                *trline(i,2)
          trem6(i)  = exp(-(coefg(1,2)+coefg(2,2)*dtx(i))*uc(i))
     $                *trline(i,1)
          emis(i,3) = term6(i,1)*(1. - .5*trem4(i) -.5*trem6(i))
C
C emis(i,4)   500 -  800 cm-1   rotation band overlap with co2
C
          k21(i) = term7(i,1) + term8(i,1)/
     $           (1. + (c30 + c31*(dty(i)-10.)*(dty(i)-10.))*sqrt(u(i)))
          k22(i) = term7(i,2) + term8(i,2)/
     $           (1. + (c28 + c29*(dty(i)-10.))*sqrt(u(i)))
          term9(i,1) = coefi(1,1) + coefi(2,1)*dtx(i)*
     $                 (1. + c18*dtx(i)*(1. + c20*dtx(i)*
     $                  (1. + c22*dtx(i)*(1. + c24*dtx(i)))))
          fwk    = fwcoef + fwc1/(1.+fwc2*u(i))
          tr1(i) = exp(-(k21(i)*(sqrt(u(i)) + fc1*fwk*u(i))))
          tr2(i) = exp(-(k22(i)*(sqrt(u(i)) + fc1*fwk*u(i))))
          tr3(i) = exp(-((coefh(1,1) + coefh(2,1)*dtx(i))*uc1(i)))
          tr4(i) = exp(-((coefh(1,2) + coefh(2,2)*dtx(i))*uc1(i)))
          tr7(i) = tr1(i)*tr3(i)
          tr8(i) = tr2(i)*tr4(i)
          emis(i,4) = term9(i,1)*.5*(tr1(i)-tr7(i) + tr2(i)-tr8(i))
          h2oems(i,k1) = emis(i,1)+emis(i,2)+emis(i,3)+emis(i,4)
          troco2(i,k1) = 0.65*tr7(i) + 0.35*tr8(i)
          th2o(i) = tr8(i)
        end do
C
C CO2 emissivity for 15 micron band system
C
        do i=1,plon
          t1i    = exp(-480./co2t(i,k1))
          sqti   = sqrt(co2t(i,k1))
          rsqti  = 1./sqti
          et     = t1i
          et2    = et*et
          et4    = et2*et2
          omet   = 1. - 1.5*et2
          f1co2  = 899.70*omet*(1. + 1.94774*et + 4.73486*et2)*rsqti
          sqwp   = sqrt(plco2(i,k1))
          f1sqwp = f1co2*sqwp
          t1co2  = 1./(1. + 245.18*omet*sqwp*rsqti)
          oneme  = 1. - et2
          alphat = oneme**3*rsqti
          wco2   = 2.5221*co2vmr*pnm(i,k1)*rga
          u7     = 4.9411e4*alphat*et2*wco2
          u8     = 3.9744e4*alphat*et4*wco2
          u9     = 1.0447e5*alphat*et4*et2*wco2
          u13    = 2.8388e3*alphat*et4*wco2
C
          tpath  = co2t(i,k1)
          tlocal = tplnke(i)
          tcrfac = sqrt((tlocal*r250)*(tpath*r300))
          pi     = pnm(i,k1)*rsslp + 2.*dpfco2*tcrfac
          posqt  = pi/(2.*sqti)
          rbeta7 =  1./( 5.3288*posqt)
          rbeta8 = 1./ (10.6576*posqt)
          rbeta9 = rbeta7
          rbeta13= rbeta9
          f2co2  = (u7/sqrt(4. + u7*(1. + rbeta7))) +
     $             (u8/sqrt(4. + u8*(1. + rbeta8))) +
     $             (u9/sqrt(4. + u9*(1. + rbeta9)))
          f3co2  = u13/sqrt(4. + u13*(1. + rbeta13))
          tmp1   = log(1. + f1sqwp)
          tmp2   = log(1. +  f2co2)
          tmp3   = log(1. +  f3co2)
          absbnd = (tmp1 + 2.*t1co2*tmp2 + 2.*tmp3)*sqti
          tco2(i)=1.0/(1.0+10.0*(u7/sqrt(4. + u7*(1. + rbeta7))))
          co2ems(i,k1)  = troco2(i,k1)*absbnd*co2plk(i)
          ex     = exp(960./tint(i,k1))
          exm1sq = (ex - 1.)**2
          co2em(i,k1) = 1.2e11*ex/(tint(i,k1)*tint4(i,k1)*exm1sq)
        end do
C
C O3 emissivity
C
        do i=1,plon
          h2otr(i,k1) = exp(-12.*s2c(i,k1))
          te          = (co2t(i,k1)/293.)**.7
          u1          = 18.29*plos(i,k1)/te
          u2          = .5649*plos(i,k1)/te
          phat        = plos(i,k1)/plol(i,k1)
          tlocal      = tplnke(i)
          tcrfac      = sqrt(tlocal*r250)*te
          beta        = (1./.3205)*((1./phat) + (dpfo3*tcrfac))
          realnu      = (1./beta)*te
          o3bndi      = 74.*te*(tplnke(i)/375.)*
     $         log(1. + fo3(u1,realnu) + fo3(u2,realnu))
          o3ems(i,k1) = dbvtt(i)*h2otr(i,k1)*o3bndi
          to3(i)=1.0/(1. + 0.1*fo3(u1,realnu) + 0.1*fo3(u2,realnu))
        end do
c
c   Calculate trace gas emissivities
c
        call trcems(k1      ,co2t    ,pnm     ,ucfc11  ,ucfc12  ,
     $              un2o0   ,un2o1   ,bn2o0   ,bn2o1   ,uch4    ,
     $              bch4    ,uco211  ,uco212  ,uco213  ,uco221  ,
     $              uco222  ,uco223  ,uptype  ,w       ,s2c     ,
     $              u       ,emplnk  ,th2o    ,tco2    ,to3     ,
     $              emstrc  )
C
C Total emissivity:
C
        do i=1,plon
          emstot(i,k1) = h2oems(i,k1) + co2ems(i,k1) + o3ems(i,k1)
     $                   + emstrc(i,k1)
        end do
  200 continue                  ! End of interface loop
C
      return
      end
 
