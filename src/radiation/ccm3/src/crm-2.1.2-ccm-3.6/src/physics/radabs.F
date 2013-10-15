#include <misc.h>
#include <params.h>
      subroutine radabs(pbr    ,pnm     ,co2em    ,co2eml  ,tplnka  ,
     $                  s2c    ,s2t     ,w        ,h2otr   ,plco2   ,
     $                  plh2o  ,co2t    ,tint     ,tlayr   ,plol    ,
     $                  plos   ,pmln    ,piln     ,ucfc11  ,ucfc12  , 
     $                  un2o0  ,un2o1   ,uch4     ,uco211  ,uco212  ,
     $                  uco213 ,uco221  ,uco222   ,uco223  ,uptype  ,
     $                  bn2o0  ,bn2o1   ,bch4    ,abplnk1  ,abplnk2 ,
     $                  abstot ,absnxt  )
C-----------------------------------------------------------------------
C
C Compute absorptivities for h2o, co2, o3, ch4, n2o, cfc11 and cfc12
C
C h2o  ....  Uses nonisothermal emissivity for water vapor from
C            Ramanathan, V. and  P.Downey, 1986: A Nonisothermal
C            Emissivity and Absorptivity Formulation for Water Vapor
C            Journal of Geophysical Research, vol. 91., D8, pp 8649-8666
C
C co2  ....  Uses absorptance parameterization of the 15 micro-meter
C            (500 - 800 cm-1) band system of Carbon Dioxide, from
C            Kiehl, J.T. and B.P.Briegleb, 1991: A New Parameterization
C            of the Absorptance Due to the 15 micro-meter Band System
C            of Carbon Dioxide Jouranl of Geophysical Research,
C            vol. 96., D5, pp 9013-9019.
C            Parameterizations for the 9.4 and 10.4 mircon bands of CO2
C            are also included.
C
C o3   ....  Uses absorptance parameterization of the 9.6 micro-meter
C            band system of ozone, from Ramanathan, V. and R.Dickinson,
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
C Computes individual absorptivities for non-adjacent layers, accounting
C for band overlap, and sums to obtain the total; then, computes the
C nearest layer contribution.
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
c $Id: radabs.F,v 1.1 2004/09/07 15:27:41 rca Exp $
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
      real pbr(plond,plev)            ! Prssr at mid-levels (dynes/cm2)
      real pnm(plond,plevp)           ! Prssr at interfaces (dynes/cm2)
      real co2em(plond,plevp)         ! Co2 emissivity function
      real co2eml(plond,plev)         ! Co2 emissivity function
      real tplnka(plond,plevp)        ! Planck fnctn level temperature
      real s2c(plond,plevp)           ! H2o continuum path length
      real s2t(plond,plevp)           ! H2o tmp and prs wghted path
      real w(plond,plevp)             ! H2o prs wghted path
      real h2otr(plond,plevp)         ! H2o trnsmssn fnct for o3 overlap
      real plco2(plond,plevp)         ! Co2 prs wghted path length
      real plh2o(plond,plevp)         ! H2o prs wfhted path length
      real co2t(plond,plevp)          ! Tmp and prs wghted path length
      real tint(plond,plevp)          ! Interface temperatures
      real tlayr(plond,plevp)         ! K-1 level temperatures
      real plol(plond,plevp)          ! Ozone prs wghted path length
      real plos(plond,plevp)          ! Ozone path length
      real pmln(plond,plev)           ! Ln(pmidm1)
      real piln(plond,plevp)          ! Ln(pintm1)
c
c Trace gas variables
c
      real ucfc11(plond,plevp)        ! CFC11 path length
      real ucfc12(plond,plevp)        ! CFC12 path length
      real un2o0(plond,plevp)         ! N2O path length
      real un2o1(plond,plevp)         ! N2O path length (hot band)
      real uch4(plond,plevp)          ! CH4 path length
      real uco211(plond,plevp)        ! CO2 9.4 micron band path length
      real uco212(plond,plevp)        ! CO2 9.4 micron band path length
      real uco213(plond,plevp)        ! CO2 9.4 micron band path length
      real uco221(plond,plevp)        ! CO2 10.4 micron band path length
      real uco222(plond,plevp)        ! CO2 10.4 micron band path length
      real uco223(plond,plevp)        ! CO2 10.4 micron band path length
      real uptype(plond,plevp)        ! continuum path length
      real bn2o0(plond,plevp)         ! pressure factor for n2o
      real bn2o1(plond,plevp)         ! pressure factor for n2o
      real bch4(plond,plevp)          ! pressure factor for ch4
      real abplnk1(14,plond,plevp)    ! non-nearest layer Planck factor
      real abplnk2(14,plond,plevp)    ! nearest layer factor
      real abstrc(plond)              ! total trace gas absorptivity
      real bplnk(14,plond,4)          ! Planck functions for sub-divided layers
C
C Output arguments
C
      real abstot(plond,plevp,plevp)  ! Total absorptivity
      real absnxt(plond,plev,4)       ! Total nearest layer absorptivity
C
C---------------------------Local variables-----------------------------
C
      integer i               ! Longitude index
      integer k               ! Level index
      integer k1              ! Level index
      integer k2              ! Level index
      integer kn              ! Nearest level index
      integer iband           ! Band  index
      integer wvl             ! Wavelength index

      real pnew(plond)        ! Effective pressure for H2O vapor linewidth
      real trline(plond,2)    ! Transmission due to H2O lines in window
      real u(plond)           ! Pressure weighted H2O path length
      real tbar(plond,4)      ! Mean layer temperature
      real emm(plond,4)       ! Mean co2 emissivity
      real o3emm(plond,4)     ! Mean o3 emissivity
      real o3bndi             ! Ozone band parameter
      real temh2o(plond,4)    ! Mean layer temperature equivalent to tbar
      real k21                ! Exponential coefficient used to calculate
C                             !  rotation band transmissvty in the 650-800
C                             !  cm-1 region (tr1)
      real k22                ! Exponential coefficient used to calculate
C                             !  rotation band transmissvty in the 500-650
C                             !  cm-1 region (tr2)
      real uc1(plond)         ! H2o continuum pathlength in 500-800 cm-1
      real to3h2o(plond)      ! H2o trnsmsn for overlap with o3
      real pi                 ! For co2 absorptivity computation
      real sqti(plond)        ! Used to store sqrt of mean temperature
      real et                 ! Co2 hot band factor
      real et2                ! Co2 hot band factor squared
      real et4                ! Co2 hot band factor to fourth power
      real omet               ! Co2 stimulated emission term
      real f1co2              ! Co2 central band factor
      real f2co2(plond)       ! Co2 weak band factor
      real f3co2(plond)       ! Co2 weak band factor
      real t1co2(plond)       ! Overlap factr weak bands on strong band
      real sqwp               ! Sqrt of co2 pathlength
      real f1sqwp(plond)      ! Main co2 band factor
      real oneme              ! Co2 stimulated emission term
      real alphat             ! Part of the co2 stimulated emission term
      real wco2               ! Constants used to define co2 pathlength
      real posqt              ! Effective pressure for co2 line width
      real u7(plond)          ! Co2 hot band path length
      real u8                 ! Co2 hot band path length
      real u9                 ! Co2 hot band path length
      real u13                ! Co2 hot band path length
      real rbeta7(plond)      ! Inverse of co2 hot band line width par
      real rbeta8             ! Inverse of co2 hot band line width par
      real rbeta9             ! Inverse of co2 hot band line width par
      real rbeta13            ! Inverse of co2 hot band line width par
      real tpatha(plond)      ! For absorptivity computation
      real a                  ! Eq(2) in table A3a of R&D
      real abso(plond,6)      ! Absorptivity for various gases/bands
      real dtp(plond)         ! Path temp minus 300 K used in h2o
C                             !  rotation band absorptivity
      real dtx(plond)         ! Planck temperature minus 250 K
      real dty(plond)         ! Path temperature minus 250 K
      real dtz(plond)         ! Planck temperature minus 300 K
      real term1(plond,4)     ! Equation(5) in table A3a of R&D(1986)
      real term2(plond,4)     ! Delta a(Te) in table A3a of R&D(1986)
      real term3(plond,4)     ! DB/dT function for rotation and
C                             !  vibration-rotation band absorptivity
      real term4(plond,4)     ! Equation(6) in table A3a of R&D(1986)
      real term5(plond,4)     ! Delta a(Tp) in table A3a of R&D(1986)
      real term6(plond,plevp) ! DB/dT function for window region
      real term7(plond,2)     ! Kl_inf(i) in eq(8) of table A3a of R&D
      real term8(plond,2)     ! Delta kl_inf(i) in eq(8)
      real term9(plond,plevp) ! DB/dT function for 500-800 cm-1 region
      real tr1                ! Eqn(6) in table A2 of R&D for 650-800
      real tr10(plond)        ! Eqn (6) times eq(4) in table A2
C                             !  of R&D for 500-650 cm-1 region
      real tr2                ! Eqn(6) in table A2 of R&D for 500-650
      real tr5                ! Eqn(4) in table A2 of R&D for 650-800
      real tr6                ! Eqn(4) in table A2 of R&D for 500-650
      real tr9(plond)         ! Equation (6) times eq(4) in table A2
C                             !  of R&D for 650-800 cm-1 region
      real uc(plond)          ! Y + 0.002U in eq(8) of table A2 of R&D
      real sqrtu(plond)       ! Sqrt of pressure weighted h20 pathlength
      real fwk(plond)         ! Equation(33) in R&D far wing correction
      real fwku(plond)        ! GU term in eqs(1) and (6) in table A2
      real r2st(2)            ! 1/(2*beta) in eq(10) in table A2
      real dtyp15(plond)      ! DeltaTp in eqs(11) & (12) in table A3a
      real dtyp15sq(plond)    ! (DeltaTp)^2 in eqs(11) & (12) table A3a
      real to3co2(plond)      ! P weighted temp in ozone band model
      real dpnm(plond)        ! Pressure difference between two levels
      real pnmsq(plond,plevp) ! Pressure squared
      real dw(plond)          ! Amount of h2o between two levels
      real uinpl(plond,4)     ! Nearest layer subdivision factor
      real winpl(plond,4)     ! Nearest layer subdivision factor
      real zinpl(plond,4)     ! Nearest layer subdivision factor
      real pinpl(plond,4)     ! Nearest layer subdivision factor
      real dplh2o(plond)      ! Difference in press weighted h2o amount
      real r80257             ! Conversion factor for h2o pathlength
      real r293               ! 1/293
      real r250               ! 1/250
      real r3205              ! Line width factor for o3 (see R&Di)
      real r300               ! 1/300
      real rsslp              ! Reciprocal of sea level pressure
      real r2sslp             ! 1/2 of rsslp
      real ds2c               ! Y in eq(7) in table A2 of R&D
      real a11                ! A1 in table A3b for rotation band absorptivity
      real a31                ! A3 in table A3b for rotation band absorptivity
      real a21                ! First part in numerator of A2 in table A3b
      real a22                ! Second part in numerator of A2 in table A3b
      real a23                ! Denominator of A2 in table A3b (rotation band)
      real t1t4               ! Eq(3) in table A3a of R&D
      real t2t5               ! Eq(4) in table A3a of R&D
      real rsum               ! Eq(1) in table A2 of R&D
      real a41                ! Numerator in A2 in Vib-rot abstivity(table A3b)
      real a51                ! Denominator in A2 in Vib-rot (table A3b)
      real a61                ! A3 factor for Vib-rot band in table A3b
      real phi                ! Eq(11) in table A3a of R&D
      real psi                ! Eq(12) in table A3a of R&D
      real cf812              ! Eq(11) in table A2 of R&D
      real ubar               ! H2o scaled path see comment for eq(10) table A2
      real pbar               ! H2o scaled pres see comment for eq(10) table A2
      real g4                 ! Arguement in exp() in eq(10) table A2
      real  dplos             ! Ozone pathlength eq(A2) in R&Di
      real dplol              ! Presure weighted ozone pathlength
      real tlocal             ! Local interface temperature
      real beta               ! Ozone mean line parameter eq(A3) in R&Di
C                               (includes Voigt line correction factor)
      real rphat              ! Effective pressure for ozone beta
      real tcrfac             ! Ozone temperature factor table 1 R&Di
      real tmp1               ! Ozone band factor see eq(A1) in R&Di
      real u1                 ! Effective ozone pathlength eq(A2) in R&Di
      real realnu             ! 1/beta factor in ozone band model eq(A1)
      real tmp2               ! Ozone band factor see eq(A1) in R&Di
      real u2                 ! Effective ozone pathlength eq(A2) in R&Di
      real rsqti              ! Reciprocal of sqrt of path temperature
      real tpath              ! Path temperature used in co2 band model
      real tmp3               ! Weak band factor see K&B
      real rdpnmsq            ! Reciprocal of difference in press^2
      real rdpnm              ! Reciprocal of difference in press
      real p1                 ! Mean pressure factor
      real p2                 ! Mean pressure factor
      real dtym10             ! T - 260 used in eq(9) and (10) table A3a
      real dplco2             ! Co2 path length
      real corfac             ! Correction factors in table A3b
      real g2                 ! Part of arguement in eq(10) in table A2
      real te                 ! A_0 T factor in ozone model table 1 of R&Di
      real denom              ! Denominator in eq(8) of table A3a of R&D
      real th2o(plond)        ! transmission due to H2O
      real tco2(plond)        ! transmission due to CO2
      real to3(plond)         ! transmission due to O3
C
C Transmission terms for various spectral intervals:
C
      real trab2(plond)       ! H2o   500 -  800 cm-1
      real trab4(plond)       ! H2o   800 - 1000 cm-1
      real trab6(plond)       ! H2o  1000 - 1200 cm-1
      real absbnd             ! Proportional to co2 band absorptance
      real dbvtit(plond,plevp)! Intrfc drvtv plnck fnctn for o3
      real dbvtly(plond,plev) ! Level drvtv plnck fnctn for o3
C
C--------------------------Statement function---------------------------
C
      real dbvt,t             ! Planck fnctn tmp derivative for o3
C
      dbvt(t)=(-2.8911366682e-4+(2.3771251896e-6+1.1305188929e-10*t)*t)/
     $  (1.0+(-6.1364820707e-3+1.5550319767e-5*t)*t)
C
C-----------------------------------------------------------------------
C
C Initialize
C
      do k=1,plev
        do i=1,plon
          dbvtly(i,k) = dbvt(tlayr(i,k+1))
          dbvtit(i,k) = dbvt(tint(i,k))
        end do
      end do
      do i=1,plon
        dbvtit(i,plevp) = dbvt(tint(i,plevp))
      end do
C
      r80257  = 1./8.0257e-04
      r293    = 1./293.
      r250    = 1./250.
      r3205   = 1./.3205
      r300    = 1./300.
      rsslp   = 1./sslp
      r2sslp  = 1./(2.*sslp)
      r2st(1) = 1./(2.*st(1))
      r2st(2) = 1./(2.*st(2))
C
C Non-adjacent layer absorptivity:
C
C abso(i,1)     0 -  800 cm-1   h2o rotation band
C abso(i,2)  1200 - 2200 cm-1   h2o vibration-rotation band
C abso(i,3)   800 - 1200 cm-1   h2o window
C abso(i,4)   500 -  800 cm-1   h2o rotation band overlap with co2
C abso(i,5)   o3  9.6 micrometer band (nu3 and nu1 bands)
C abso(i,6)   co2 15  micrometer band system
C
      do k=1,plevp
        do i=1,plon
          pnmsq(i,k) = pnm(i,k)**2
          dtx(i) = tplnka(i,k) - 250.
          term6(i,k) = coeff(1,2) + coeff(2,2)*dtx(i)*
     $                 (1. +  c9*dtx(i)*(1. + c11*dtx(i)*
     $                 (1. + c13*dtx(i)*(1. + c15*dtx(i)))))
          term9(i,k) = coefi(1,2) + coefi(2,2)*dtx(i)*
     $                 (1. + c19*dtx(i)*(1. + c21*dtx(i)*
     $                 (1. + c23*dtx(i)*(1. + c25*dtx(i)))))
        end do
      end do
C
C Non-nearest layer level loops
C
      do 200 k1=plevp,1,-1
        do 100 k2=plevp,1,-1
          if(k1.eq.k2) go to 100
          do i=1,plon
            dplh2o(i) = plh2o(i,k1) - plh2o(i,k2)
            u(i)      = abs(dplh2o(i))
            sqrtu(i)  = sqrt(u(i))
            ds2c      = abs(s2c(i,k1) - s2c(i,k2))
            dw(i)     = abs(w(i,k1) - w(i,k2))
            uc1(i)    = (ds2c + 1.7e-3*u(i))*(1. +  2.*ds2c)/
     $                                       (1. + 15.*ds2c)
            uc(i)     = ds2c + 2.e-3*u(i)
          end do
          do i=1,plon
            pnew(i)   = u(i)/dw(i)
            tpatha(i) = (s2t(i,k1) - s2t(i,k2))/dplh2o(i)
            dtx(i)      = tplnka(i,k2) - 250.
            dty(i)      = tpatha(i)    - 250.
            dtyp15(i)   = dty(i) + 15.
            dtyp15sq(i) = dtyp15(i)**2
            dtz(i)      = dtx(i) - 50.
            dtp(i)      = dty(i) - 50.
          end do
          do iband=2,4,2
            do i=1,plon
              term1(i,iband) = coefe(1,iband) + coefe(2,iband)*
     $                         dtx(i)*(1. + c1(iband)*dtx(i))
              term2(i,iband) = coefb(1,iband) + coefb(2,iband)*
     $                         dtx(i)*(1. + c2(iband)*dtx(i)*
     $                                 (1. + c3(iband)*dtx(i)))
              term3(i,iband) = coefd(1,iband) + coefd(2,iband)*
     $                         dtx(i)*(1. + c4(iband)*dtx(i)*
     $                                 (1. + c5(iband)*dtx(i)))
              term4(i,iband) = coefa(1,iband) + coefa(2,iband)*
     $                         dty(i)*(1. + c6(iband)*dty(i))
              term5(i,iband) = coefc(1,iband) + coefc(2,iband)*
     $                         dty(i)*(1. + c7(iband)*dty(i))
            end do
          end do
C
C abso(i,1)     0 -  800 cm-1   h2o rotation band
C
          do i=1,plon
            a11 = 0.44 + 3.380e-4*dtz(i) - 1.520e-6*dtz(i)*dtz(i)
            a31 = 1.05 - 6.000e-3*dtp(i) + 3.000e-6*dtp(i)*dtp(i)
            a21 = 1.00 + 1.717e-3*dtz(i) - 1.133e-5*dtz(i)*dtz(i)
            a22 = 1.00 + 4.443e-3*dtp(i) + 2.750e-5*dtp(i)*dtp(i)
            a23 = 1.00 + 3.600*sqrtu(i)
            corfac  = a31*(a11 + ((2.*a21*a22)/a23))
            t1t4    = term1(i,2)*term4(i,2)
            t2t5    = term2(i,2)*term5(i,2)
            a       = t1t4 + t2t5/(1. + t2t5*sqrtu(i)*corfac)
            fwk(i)  = fwcoef + fwc1/(1. + fwc2*u(i))
            fwku(i) = fwk(i)*u(i)
            rsum    = exp(-a*(sqrtu(i) + fwku(i)))
            abso(i,1) = (1. - rsum)*term3(i,2)
          end do
C
C abso(i,2)  1200 - 2200 cm-1   h2o vibration-rotation band
C
          do i=1,plon
            a41   = 1.75 - 3.960e-03*dtz(i)
            a51   = 1.00 + 1.3*sqrtu(i)
            a61   = 1.00 + 1.250e-03*dtp(i) + 6.250e-05*dtp(i)*dtp(i)
            corfac = .29*(1. + a41/a51)*a61
            t1t4   = term1(i,4)*term4(i,4)
            t2t5   = term2(i,4)*term5(i,4)
            a      = t1t4 + t2t5/(1. + t2t5*sqrtu(i)*corfac)
            rsum   = exp(-a*(sqrtu(i) + fwku(i)))
            abso(i,2) = (1. - rsum)*term3(i,4)
          end do
C
C Line transmission in 800-1000 and 1000-1200 cm-1 intervals
C
          do k=1,2
            do i=1,plon
              phi   = exp(a1(k)*dtyp15(i) + a2(k)*dtyp15sq(i))
              psi   = exp(b1(k)*dtyp15(i) + b2(k)*dtyp15sq(i))
              ubar  = dw(i)*phi*1.66*r80257
              pbar  = pnew(i)*(psi/phi)
              cf812 = cfa1 + (1. - cfa1)/(1. + ubar*pbar*10.)
              g2    = 1. + ubar*4.0*st(k)*cf812/pbar
              g4    = realk(k)*pbar*r2st(k)*(sqrt(g2) - 1.)
              trline(i,k) = exp(-g4)
            end do
          end do
          do i=1,plon
            term7(i,1) = coefj(1,1) + coefj(2,1)*dty(i)*
     $                                (1. + c16*dty(i))
            term8(i,1) = coefk(1,1) + coefk(2,1)*dty(i)*
     $                                (1. + c17*dty(i))
            term7(i,2) = coefj(1,2) + coefj(2,2)*dty(i)*
     $                                (1. + c26*dty(i))
            term8(i,2) = coefk(1,2) + coefk(2,2)*dty(i)*
     $                                (1. + c27*dty(i))
          end do
C
C abso(i,3)   800 - 1200 cm-1   h2o window
C abso(i,4)   500 -  800 cm-1   h2o rotation band overlap with co2
C
          do i=1,plon
            k21    = term7(i,1) + term8(i,1)/
     $             (1. + (c30 + c31*(dty(i)-10.)*(dty(i)-10.))*sqrtu(i))
            k22    = term7(i,2) + term8(i,2)/
     $             (1. + (c28 + c29*(dty(i)-10.))*sqrtu(i))
            tr1    = exp(-(k21*(sqrtu(i) + fc1*fwku(i))))
            tr2    = exp(-(k22*(sqrtu(i) + fc1*fwku(i))))
            tr5    = exp(-((coefh(1,3) + coefh(2,3)*dtx(i))*uc1(i)))
            tr6    = exp(-((coefh(1,4) + coefh(2,4)*dtx(i))*uc1(i)))
            tr9(i)   = tr1*tr5
            tr10(i)  = tr2*tr6
            th2o(i) = tr10(i)
            trab2(i) = 0.65*tr9(i) + 0.35*tr10(i)
            trab4(i) = exp(-(coefg(1,3) + coefg(2,3)*dtx(i))*uc(i))
            trab6(i) = exp(-(coefg(1,4) + coefg(2,4)*dtx(i))*uc(i))
            abso(i,3) = term6(i,k2)*(1. - .5*trab4(i)*trline(i,2) -
     $                                    .5*trab6(i)*trline(i,1))
            abso(i,4) = term9(i,k2)*.5*(tr1 - tr9(i) + tr2 - tr10(i))
          end do
          if(k2.lt.k1) then
            do i=1,plon
              to3h2o(i) = h2otr(i,k1)/h2otr(i,k2)
            end do
          else
            do i=1,plon
              to3h2o(i) = h2otr(i,k2)/h2otr(i,k1)
            end do
          end if
C
C abso(i,5)   o3  9.6 micrometer band (nu3 and nu1 bands)
C
          do i=1,plon
            dpnm(i)  = pnm(i,k1) - pnm(i,k2)
            to3co2(i) = (pnm(i,k1)*co2t(i,k1) - pnm(i,k2)*co2t(i,k2))/
     $                  dpnm(i)
            te       = (to3co2(i)*r293)**.7
            dplos    = plos(i,k1) - plos(i,k2)
            dplol    = plol(i,k1) - plol(i,k2)
            u1       = 18.29*abs(dplos)/te
            u2       = .5649*abs(dplos)/te
            rphat    = dplol/dplos
            tlocal   = tint(i,k2)
            tcrfac   = sqrt(tlocal*r250)*te
            beta     = r3205*(rphat + dpfo3*tcrfac)
            realnu   = te/beta
            tmp1     = u1/sqrt(4. + u1*(1. + realnu))
            tmp2     = u2/sqrt(4. + u2*(1. + realnu))
            o3bndi    = 74.*te*log(1. + tmp1 + tmp2)
            abso(i,5) = o3bndi*to3h2o(i)*dbvtit(i,k2)
            to3(i)   = 1.0/(1. + 0.1*tmp1 + 0.1*tmp2)
          end do
C
C abso(i,6)      co2 15  micrometer band system
C
          do i=1,plon
            sqwp      = sqrt(abs(plco2(i,k1) - plco2(i,k2)))
            et        = exp(-480./to3co2(i))
            sqti(i)   = sqrt(to3co2(i))
            rsqti     = 1./sqti(i)
            et2       = et*et
            et4       = et2*et2
            omet      = 1. - 1.5*et2
            f1co2     = 899.70*omet*
     $                   (1. + 1.94774*et + 4.73486*et2)*rsqti
            f1sqwp(i) = f1co2*sqwp
            t1co2(i)  = 1./(1. + (245.18*omet*sqwp*rsqti))
            oneme     = 1. - et2
            alphat    = oneme**3*rsqti
            pi        = abs(dpnm(i))
            wco2      =  2.5221*co2vmr*pi*rga
            u7(i)     =  4.9411e4*alphat*et2*wco2
            u8        =  3.9744e4*alphat*et4*wco2
            u9        =  1.0447e5*alphat*et4*et2*wco2
            u13       = 2.8388e3*alphat*et4*wco2
            tpath     = to3co2(i)
            tlocal    = tint(i,k2)
            tcrfac    = sqrt(tlocal*r250*tpath*r300)
            posqt     = ((pnm(i,k2) + pnm(i,k1))*r2sslp +
     $                     dpfco2*tcrfac)*rsqti
            rbeta7(i) = 1./(5.3228*posqt)
            rbeta8    = 1./(10.6576*posqt)
            rbeta9    = rbeta7(i)
            rbeta13   = rbeta9
            f2co2(i)  = (u7(i)/sqrt(4. + u7(i)*(1. + rbeta7(i)))) +
     $                  (u8   /sqrt(4. + u8*(1. + rbeta8))) +
     $                  (u9   /sqrt(4. + u9*(1. + rbeta9)))
            f3co2(i)  = u13/sqrt(4. + u13*(1. + rbeta13))
          end do
          if (k2.ge.k1) then
            do i=1,plon
              sqti(i) = sqrt(tlayr(i,k2))
            end do
          end if
C
          do i=1,plon
            tmp1      = log(1. + f1sqwp(i))
            tmp2      = log(1. + f2co2(i))
            tmp3      = log(1. + f3co2(i))
            absbnd    = (tmp1 + 2.*t1co2(i)*tmp2 + 2.*tmp3)*sqti(i)
            abso(i,6) = trab2(i)*co2em(i,k2)*absbnd
            tco2(i)   = 1./(1.0+10.0*(u7(i)/sqrt(4. + u7(i)*
     $                                           (1. + rbeta7(i)))))
          end do
c
c Calculate absorptivity due to trace gases, abstrc
c
          call trcab(k1, k2, ucfc11, ucfc12, un2o0,  un2o1,
     $                       uch4,   uco211, uco212, uco213, 
     $                       uco221, uco222, uco223, bn2o0, 
     $                       bn2o1,  bch4,   to3co2, pnm,
     $                       dw,     pnew,   s2c,    uptype,
     $                       u,      abplnk1,tco2,   th2o,   
     $                       to3,    abstrc)
C
C Sum total absorptivity
C
          do i=1,plon
            abstot(i,k1,k2) = abso(i,1) + abso(i,2) + abso(i,3) +
     $                       abso(i,4) + abso(i,5) + abso(i,6)
     $                       + abstrc(i)
          end do
  100   continue
  200 continue                  ! End of non-nearest layer level loops
C
C Non-adjacent layer absorptivity:
C
C abso(i,1)     0 -  800 cm-1   h2o rotation band
C abso(i,2)  1200 - 2200 cm-1   h2o vibration-rotation band
C abso(i,3)   800 - 1200 cm-1   h2o window
C abso(i,4)   500 -  800 cm-1   h2o rotation band overlap with co2
C abso(i,5)   o3  9.6 micrometer band (nu3 and nu1 bands)
C abso(i,6)   co2 15  micrometer band system
C
C Nearest layer level loop
C
      do 500 k2=plev,1,-1
        do i=1,plon
          tbar(i,1)   = 0.5*(tint(i,k2+1) + tlayr(i,k2+1))
          emm(i,1)    = 0.5*(co2em(i,k2+1) + co2eml(i,k2))
          tbar(i,2)   = 0.5*(tlayr(i,k2+1) + tint(i,k2))
          emm(i,2)    = 0.5*(co2em(i,k2) + co2eml(i,k2))
          tbar(i,3)   = 0.5*(tbar(i,2) + tbar(i,1))
          emm(i,3)    = emm(i,1)
          tbar(i,4)   = tbar(i,3)
          emm(i,4)    = emm(i,2)
          o3emm(i,1)  = 0.5*(dbvtit(i,k2+1) + dbvtly(i,k2))
          o3emm(i,2)  = 0.5*(dbvtit(i,k2) + dbvtly(i,k2))
          o3emm(i,3)  = o3emm(i,1)
          o3emm(i,4)  = o3emm(i,2)
          temh2o(i,1) = tbar(i,1)
          temh2o(i,2) = tbar(i,2)
          temh2o(i,3) = tbar(i,1)
          temh2o(i,4) = tbar(i,2)
          dpnm(i)     = pnm(i,k2+1) - pnm(i,k2)
        end do
c
c  Weighted Planck functions for trace gases
c
        do wvl = 1,14
          do i = 1,plon
            bplnk(wvl,i,1) = 0.5*(abplnk1(wvl,i,k2+1) + 
     $                            abplnk2(wvl,i,k2))
            bplnk(wvl,i,2) = 0.5*(abplnk1(wvl,i,k2) + 
     $                          abplnk2(wvl,i,k2))
            bplnk(wvl,i,3) = bplnk(wvl,i,1)
            bplnk(wvl,i,4) = bplnk(wvl,i,2)
          end do
        end do

        do i=1,plon
          rdpnmsq    = 1./(pnmsq(i,k2+1) - pnmsq(i,k2))
          rdpnm      = 1./dpnm(i)
          p1         = .5*(pbr(i,k2) + pnm(i,k2+1))
          p2         = .5*(pbr(i,k2) + pnm(i,k2  ))
          uinpl(i,1) =  (pnmsq(i,k2+1) - p1**2)*rdpnmsq
          uinpl(i,2) = -(pnmsq(i,k2  ) - p2**2)*rdpnmsq
          uinpl(i,3) = -(pnmsq(i,k2  ) - p1**2)*rdpnmsq
          uinpl(i,4) =  (pnmsq(i,k2+1) - p2**2)*rdpnmsq
          winpl(i,1) = (.5*( pnm(i,k2+1) - pbr(i,k2)))*rdpnm
          winpl(i,2) = (.5*(-pnm(i,k2  ) + pbr(i,k2)))*rdpnm
          winpl(i,3) = (.5*( pnm(i,k2+1) + pbr(i,k2)) - pnm(i,k2  ))*
     $                   rdpnm
          winpl(i,4) = (.5*(-pnm(i,k2  ) - pbr(i,k2)) + pnm(i,k2+1))*
     $                   rdpnm
          tmp1       = 1./(piln(i,k2+1) - piln(i,k2))
          tmp2       = piln(i,k2+1) - pmln(i,k2)
          tmp3       = piln(i,k2  ) - pmln(i,k2)
          zinpl(i,1) = (.5*tmp2          )*tmp1
          zinpl(i,2) = (        - .5*tmp3)*tmp1
          zinpl(i,3) = (.5*tmp2 -    tmp3)*tmp1
          zinpl(i,4) = (   tmp2 - .5*tmp3)*tmp1
          pinpl(i,1) = 0.5*(p1 + pnm(i,k2+1))
          pinpl(i,2) = 0.5*(p2 + pnm(i,k2  ))
          pinpl(i,3) = 0.5*(p1 + pnm(i,k2  ))
          pinpl(i,4) = 0.5*(p2 + pnm(i,k2+1))
        end do
        do 400 kn=1,4
          do i=1,plon
            u(i)     = uinpl(i,kn)*abs(plh2o(i,k2) - plh2o(i,k2+1))
            sqrtu(i) = sqrt(u(i))
            dw(i)    = abs(w(i,k2) - w(i,k2+1))
            pnew(i)  = u(i)/(winpl(i,kn)*dw(i))
            ds2c     = abs(s2c(i,k2) - s2c(i,k2+1))
            uc1(i)   = uinpl(i,kn)*ds2c
            uc1(i)   = (uc1(i) + 1.7e-3*u(i))*(1. +  2.*uc1(i))/
     $                                        (1. + 15.*uc1(i))
            uc(i)    = uinpl(i,kn)*ds2c + 2.e-3*u(i)
          end do
          do i=1,plon
            dtx(i)      = temh2o(i,kn) - 250.
            dty(i)      = tbar(i,kn) - 250.
            dtyp15(i)   = dty(i) + 15.
            dtyp15sq(i) = dtyp15(i)**2
            dtz(i)      = dtx(i) - 50.
            dtp(i)      = dty(i) - 50.
          end do
          do iband=2,4,2
            do i=1,plon
              term1(i,iband) = coefe(1,iband) + coefe(2,iband)*
     $                             dtx(i)*(1. + c1(iband)*dtx(i))
              term2(i,iband) = coefb(1,iband) + coefb(2,iband)*
     $                             dtx(i)*(1. + c2(iband)*dtx(i)*
     $                                     (1. + c3(iband)*dtx(i)))
              term3(i,iband) = coefd(1,iband) + coefd(2,iband)*
     $                             dtx(i)*(1. + c4(iband)*dtx(i)*
     $                                     (1. + c5(iband)*dtx(i)))
              term4(i,iband) = coefa(1,iband) + coefa(2,iband)*
     $                             dty(i)*(1. + c6(iband)*dty(i))
              term5(i,iband) = coefc(1,iband) + coefc(2,iband)*
     $                             dty(i)*(1. + c7(iband)*dty(i))
            end do
          end do
C
C abso(i,1)     0 -  800 cm-1   h2o rotation band
C
          do i=1,plon
            a11 = 0.44 + 3.380e-4*dtz(i) - 1.520e-6*dtz(i)*dtz(i)
            a31 = 1.05 - 6.000e-3*dtp(i) + 3.000e-6*dtp(i)*dtp(i)
            a21 = 1.00 + 1.717e-3*dtz(i) - 1.133e-5*dtz(i)*dtz(i)
            a22 = 1.00 + 4.443e-3*dtp(i) + 2.750e-5*dtp(i)*dtp(i)
            a23 = 1.00 + 3.600*sqrtu(i)
            corfac    = a31*(a11 + ((2.*a21*a22)/a23))
            t1t4      = term1(i,2)*term4(i,2)
            t2t5      = term2(i,2)*term5(i,2)
            a         = t1t4 + t2t5/(1. + t2t5*sqrtu(i)*corfac)
            fwk(i)    = fwcoef + fwc1/(1. + fwc2*u(i))
            fwku(i)   = fwk(i)*u(i)
            rsum      = exp(-a*(sqrtu(i) + fwku(i)))
            abso(i,1) = (1. - rsum)*term3(i,2)
          end do
C
C abso(i,2)  1200 - 2200 cm-1   h2o vibration-rotation band
C
          do i=1,plon
            a41   = 1.75 - 3.960e-03*dtz(i)
            a51   = 1.00 + 1.3*sqrtu(i)
            a61   = 1.00 + 1.250e-03*dtp(i) + 6.250e-05*dtp(i)*dtp(i)
            corfac = .29*(1. + a41/a51)*a61
            t1t4   = term1(i,4)*term4(i,4)
            t2t5   = term2(i,4)*term5(i,4)
            a      = t1t4 + t2t5/(1. + t2t5*sqrtu(i)*corfac)
            rsum   = exp(-a*(sqrtu(i) + fwku(i)))
            abso(i,2) = (1. - rsum)*term3(i,4)
          end do
C
C Line transmission in 800-1000 and 1000-1200 cm-1 intervals
C
          do k=1,2
            do i=1,plon
              phi   = exp(a1(k)*dtyp15(i) + a2(k)*dtyp15sq(i))
              psi   = exp(b1(k)*dtyp15(i) + b2(k)*dtyp15sq(i))
              ubar  = dw(i)*phi*winpl(i,kn)*1.66*r80257
              pbar  = pnew(i)*(psi/phi)
              cf812 = cfa1 + (1. - cfa1)/(1. + ubar*pbar*10.)
              g2    = 1. + ubar*4.0*st(k)*cf812/pbar
              g4    = realk(k)*pbar*r2st(k)*(sqrt(g2) - 1.)
              trline(i,k) = exp(-g4)
            end do
          end do
          do i=1,plon
            term7(i,1) = coefj(1,1) + coefj(2,1)*dty(i)*
     $                                   (1. + c16*dty(i))
            term8(i,1) = coefk(1,1) + coefk(2,1)*dty(i)*
     $                                   (1. + c17*dty(i))
            term7(i,2) = coefj(1,2) + coefj(2,2)*dty(i)*
     $                                   (1. + c26*dty(i))
            term8(i,2) = coefk(1,2) + coefk(2,2)*dty(i)*
     $                                   (1. + c27*dty(i))
          end do
C
C abso(i,3)   800 - 1200 cm-1   h2o window
C abso(i,4)   500 -  800 cm-1   h2o rotation band overlap with co2
C
          do i=1,plon
            dtym10     = dty(i) - 10.
            denom      = 1. + (c30 + c31*dtym10*dtym10)*sqrtu(i)
            k21        = term7(i,1) + term8(i,1)/denom
            denom      = 1. + (c28 + c29*dtym10       )*sqrtu(i)
            k22        = term7(i,2) + term8(i,2)/denom
            term9(i,2) = coefi(1,2) + coefi(2,2)*dtx(i)*
     $                  (1. + c19*dtx(i)*(1. + c21*dtx(i)*
     $                   (1. + c23*dtx(i)*(1. + c25*dtx(i)))))
            tr1     = exp(-(k21*(sqrtu(i) + fc1*fwku(i))))
            tr2     = exp(-(k22*(sqrtu(i) + fc1*fwku(i))))
            tr5     = exp(-((coefh(1,3) + coefh(2,3)*dtx(i))*uc1(i)))
            tr6     = exp(-((coefh(1,4) + coefh(2,4)*dtx(i))*uc1(i)))
            tr9(i)  = tr1*tr5
            tr10(i) = tr2*tr6
            trab2(i)= 0.65*tr9(i) + 0.35*tr10(i)
            th2o(i) = tr10(i)
            trab4(i)= exp(-(coefg(1,3) + coefg(2,3)*dtx(i))*uc(i))
            trab6(i)= exp(-(coefg(1,4) + coefg(2,4)*dtx(i))*uc(i))
            term6(i,2) = coeff(1,2) + coeff(2,2)*dtx(i)*
     $                     (1. + c9*dtx(i)*(1. + c11*dtx(i)*
     $                     (1. + c13*dtx(i)*(1. + c15*dtx(i)))))
            abso(i,3)  = term6(i,2)*(1. - .5*trab4(i)*trline(i,2) -
     $                                       .5*trab6(i)*trline(i,1))
            abso(i,4)  = term9(i,2)*.5*(tr1 - tr9(i) + tr2 - tr10(i))
          end do
C
C abso(i,5)  o3  9.6 micrometer (nu3 and nu1 bands)
C
          do i=1,plon
            te        = (tbar(i,kn)*r293)**.7
            dplos     = abs(plos(i,k2+1) - plos(i,k2))
            u1        = zinpl(i,kn)*18.29*dplos/te
            u2        = zinpl(i,kn)*.5649*dplos/te
            tlocal    = tbar(i,kn)
            tcrfac    = sqrt(tlocal*r250)*te
            beta      = r3205*(pinpl(i,kn)*rsslp + dpfo3*tcrfac)
            realnu    = te/beta
            tmp1      = u1/sqrt(4. + u1*(1. + realnu))
            tmp2      = u2/sqrt(4. + u2*(1. + realnu))
            o3bndi    = 74.*te*log(1. + tmp1 + tmp2)
            abso(i,5) = o3bndi*o3emm(i,kn)*
     $                     (h2otr(i,k2+1)/h2otr(i,k2))
            to3(i)    = 1.0/(1. + 0.1*tmp1 + 0.1*tmp2)
          end do
C
C abso(i,6)   co2 15  micrometer band system
C
          do 300 i=1,plon
            dplco2   = plco2(i,k2+1) - plco2(i,k2)
            sqwp     = sqrt(uinpl(i,kn)*dplco2)
            et       = exp(-480./tbar(i,kn))
            sqti(i)  = sqrt(tbar(i,kn))
            rsqti    = 1./sqti(i)
            et2      = et*et
            et4      = et2*et2
            omet     = (1. - 1.5*et2)
            f1co2    = 899.70*omet*
     $                (1. + 1.94774*et + 4.73486*et2)*rsqti
            f1sqwp(i)= f1co2*sqwp
            t1co2(i) = 1./(1. + (245.18*omet*sqwp*rsqti))
            oneme    = 1. - et2
            alphat   = oneme**3*rsqti
            pi       = abs(dpnm(i))*winpl(i,kn)
            wco2     = 2.5221*co2vmr*pi*rga
            u7(i)    = 4.9411e4*alphat*et2*wco2
            u8       = 3.9744e4*alphat*et4*wco2
            u9       = 1.0447e5*alphat*et4*et2*wco2
            u13      = 2.8388e3*alphat*et4*wco2
            tpath    = tbar(i,kn)
            tlocal   = tbar(i,kn)
            tcrfac   = sqrt((tlocal*r250)*(tpath*r300))
            posqt    = (pinpl(i,kn)*rsslp + dpfco2*tcrfac)*rsqti
            rbeta7(i)= 1./(5.3228*posqt)
            rbeta8   = 1./(10.6576*posqt)
            rbeta9   = rbeta7(i)
            rbeta13  = rbeta9
            f2co2(i) = u7(i)/sqrt(4. + u7(i)*(1. + rbeta7(i))) +
     $                 u8   /sqrt(4. + u8*(1. + rbeta8)) +
     $                 u9   /sqrt(4. + u9*(1. + rbeta9))
            f3co2(i) = u13/sqrt(4. + u13*(1. + rbeta13))
            tmp1     = log(1. + f1sqwp(i))
            tmp2     = log(1. + f2co2(i))
            tmp3     = log(1. + f3co2(i))
            absbnd   = (tmp1 + 2.*t1co2(i)*tmp2 + 2.*tmp3)*sqti(i)
            abso(i,6)= trab2(i)*emm(i,kn)*absbnd
            tco2(i)  = 1.0/(1.0+ 10.0*u7(i)/sqrt(4. + u7(i)*
     $                                           (1. + rbeta7(i))))
  300     continue
c
c Calculate trace gas absorptivity for nearest layer, abstrc
c
          call trcabn(k2      ,kn      ,ucfc11  ,ucfc12  ,un2o0   ,
     $                un2o1   ,uch4    ,uco211  ,uco212  ,uco213  , 
     $                uco221  ,uco222  ,uco223  ,tbar    ,bplnk   , 
     $                winpl   ,pinpl   ,tco2    ,th2o    ,to3     ,
     $                uptype  ,dw      ,s2c     ,u       ,pnew    ,
     $                abstrc  ,uinpl)
C
C Total next layer absorptivity:
C
          do i=1,plon
            absnxt(i,k2,kn) = abso(i,1) + abso(i,2) + abso(i,3) +
     $                        abso(i,4) + abso(i,5) + abso(i,6) +
     $                        abstrc(i)
          end do
  400   continue
  500 continue                  !  end of nearest layer level loop
C
      return
      end
 
