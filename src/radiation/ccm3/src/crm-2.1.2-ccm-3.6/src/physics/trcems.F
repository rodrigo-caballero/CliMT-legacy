#include <misc.h>
#include <params.h>
      subroutine trcems(k       ,co2t    ,pnm     ,ucfc11  ,ucfc12  ,
     $                  un2o0   ,un2o1   ,bn2o0   ,bn2o1   ,uch4    ,
     $                  bch4    ,uco211  ,uco212  ,uco213  ,uco221  ,
     $                  uco222  ,uco223  ,uptype  ,w       ,s2c     ,
     $                  up2     ,emplnk  ,th2o    ,tco2    ,to3     ,
     $                  emstrc  )
C----------------------------------------------------------------------
C
C  Calculate emissivity for CH4, N2O, CFC11 and CFC12 bands.
C
C  See CCM3 Description for equations.
C
C-------------------------Code History----------------------------------
C
C Original version:  J.T. Kiehl, Nov 21 1994
C Standardized:      T. Acker, Feb 1996
C Reviewed:          J. Kiehl, April 1996         
C 
c-----------------------------------------------------------------------
c
c $Id: trcems.F,v 1.1 2004/09/07 15:27:42 rca Exp $
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
      real co2t(plond,plevp)    ! pressure weighted temperature
      real pnm(plond,plevp)     ! interface pressure
      real ucfc11(plond,plevp)  ! CFC11 path length
      real ucfc12(plond,plevp)  ! CFC12 path length
      real un2o0(plond,plevp)   ! N2O path length
c
      real un2o1(plond,plevp)   ! N2O path length (hot band)
      real uch4(plond,plevp)    ! CH4 path length
      real uco211(plond,plevp)  ! CO2 9.4 micron band path length
      real uco212(plond,plevp)  ! CO2 9.4 micron band path length
      real uco213(plond,plevp)  ! CO2 9.4 micron band path length
c
      real uco221(plond,plevp)  ! CO2 10.4 micron band path length
      real uco222(plond,plevp)  ! CO2 10.4 micron band path length
      real uco223(plond,plevp)  ! CO2 10.4 micron band path length
      real uptype(plond,plevp)  ! continuum path length
      real bn2o0(plond,plevp)   ! pressure factor for n2o
c
      real bn2o1(plond,plevp)   ! pressure factor for n2o
      real bch4(plond,plevp)    ! pressure factor for ch4
      real emplnk(14,plond)     ! emissivity Planck factor
      real th2o(plond)          ! water vapor overlap factor
      real tco2(plond)          ! co2 overlap factor
c
      real to3(plond)           ! o3 overlap factor
      real s2c(plond,plevp)     ! h2o continuum path length
      real w(plond,plevp)       ! h2o path length
      real up2(plond)           ! pressure squared h2o path length
c
      integer k                 ! level index
c
c  Output Arguments
c
      real emstrc(plond,plevp)  ! total trace gas emissivity
c
c--------------------------Local Variables------------------------------
c
      integer i,l               ! loop counters
c
      real sqti(plond)          ! square root of mean temp
      real ecfc1                ! emissivity of cfc11 798 cm-1 band
      real ecfc2                !     "      "    "   846 cm-1 band
      real ecfc3                !     "      "    "   933 cm-1 band
      real ecfc4                !     "      "    "   1085 cm-1 band
c
      real ecfc5                !     "      "  cfc12 889 cm-1 band
      real ecfc6                !     "      "    "   923 cm-1 band
      real ecfc7                !     "      "    "   1102 cm-1 band
      real ecfc8                !     "      "    "   1161 cm-1 band
      real u01                  ! n2o path length
c
      real u11                  ! n2o path length
      real beta01               ! n2o pressure factor
      real beta11               ! n2o pressure factor
      real en2o1                ! emissivity of the 1285 cm-1 N2O band
      real u02                  ! n2o path length
c
      real u12                  ! n2o path length
      real beta02               ! n2o pressure factor
      real en2o2                ! emissivity of the 589 cm-1 N2O band
      real u03                  ! n2o path length
      real beta03               ! n2o pressure factor
c
      real en2o3                ! emissivity of the 1168 cm-1 N2O band
      real betac                ! ch4 pressure factor
      real ech4                 ! emissivity of 1306 cm-1 CH4 band
      real betac1               ! co2 pressure factor
      real betac2               ! co2 pressure factor
c
      real eco21                ! emissivity of 1064 cm-1 CO2 band
      real eco22                ! emissivity of 961 cm-1 CO2 band
      real tt(plond)            ! temp. factor for h2o overlap factor
      real psi1                 ! narrow band h2o temp. factor
      real phi1                 !             "
c
      real p1                   ! h2o line overlap factor
      real w1                   !          "
      real tw(plond,6)          ! h2o transmission overlap
      real g1(6)                ! h2o overlap factor
      real g2(6)                !          "
c
      real g3(6)                !          "
      real g4(6)                !          "
      real ab(6)                !          "
      real bb(6)                !          "
      real abp(6)               !          "
c
      real bbp(6)               !          "
      real tcfc3                ! transmission for cfc11 band
      real tcfc4                !          "
      real tcfc6                ! transmission for cfc12 band
      real tcfc7                !          "
c
      real tcfc8                !          "
      real tlw                  ! h2o overlap factor
      real tch4                 ! ch4 overlap factor
C
C--------------------------Data Statements------------------------------
C
      data g1 /0.0468556,0.0397454,0.0407664,0.0304380,0.0540398,
     $         0.0321962/
      data g2 /14.4832,4.30242,5.23523,3.25342,0.698935,16.5599/
      data g3 /26.1898,18.4476,15.3633,12.1927,9.14992,8.07092/
      data g4 /0.0261782,0.0369516,0.0307266,0.0243854,0.0182932,
     $         0.0161418/
      data ab /3.0857e-2,2.3524e-2,1.7310e-2,2.6661e-2,2.8074e-2,
     $         2.2915e-2/
      data bb /-1.3512e-4,-6.8320e-5,-3.2609e-5,-1.0228e-5,
     $         -9.5743e-5,-1.0304e-4/
      data abp/2.9129e-2,2.4101e-2,1.9821e-2,2.6904e-2,2.9458e-2,
     $         1.9892e-2/
      data bbp/-1.3139e-4,-5.5688e-5,-4.6380e-5,-8.0362e-5,
     $         -1.0115e-4,-8.8061e-5/
C
C--------------------------Statement Functions--------------------------
C
      real func, u, b
      func(u,b) = u/sqrt(4.0 + u*(1.0 + 1.0 / b))
C
C-----------------------------------------------------------------------
C
      do i = 1,plon
         sqti(i) = sqrt(co2t(i,k))
c
c Transmission for h2o
c
         tt(i) = abs(co2t(i,k) - 250.0)
      end do
c
      do l = 1,6
         do i = 1,plon
            psi1 = exp(abp(l)*tt(i)+bbp(l)*tt(i)*tt(i))
            phi1 = exp(ab(l)*tt(i)+bb(l)*tt(i)*tt(i))
            p1 = pnm(i,k) * (psi1/phi1) / sslp
            w1 = w(i,k) * phi1
            tw(i,l) = exp(- g1(l)*p1*(sqrt(1.0+g2(l)*(w1/p1))-1.0)
     $                  - g3(l)*s2c(i,k)-g4(l)*uptype(i,k))
         end do
      end do 
c
      do i = 1,plon
c
c transmission due to cfc bands
c
            tcfc3 = exp(-175.005*ucfc11(i,k))
            tcfc4 = exp(-1202.18*ucfc11(i,k)) 
            tcfc6 = exp(-5786.73*ucfc12(i,k))
            tcfc7 = exp(-2873.51*ucfc12(i,k))
            tcfc8 = exp(-2085.59*ucfc12(i,k))
c
c Emissivity for CFC11 bands
c
            ecfc1 = 50.0*(1.0 - exp(-54.09*ucfc11(i,k))) * tw(i,1) * 
     $                                                  emplnk(7,i)
            ecfc2 = 60.0*(1.0 - exp(-5130.03*ucfc11(i,k)))* tw(i,2) *
     $                                                  emplnk(8,i)
            ecfc3 = 60.0*(1.0 - tcfc3)*tw(i,4)*tcfc6*emplnk(9,i)
            ecfc4 = 100.0*(1.0 - tcfc4)*tw(i,5)*emplnk(10,i)
c
c Emissivity for CFC12 bands
c
            ecfc5 = 45.0*(1.0 - exp(-1272.35*ucfc12(i,k)))*tw(i,3)*
     $                                                     emplnk(11,i)
            ecfc6 = 50.0*(1.0 - tcfc6)*tw(i,4)*emplnk(12,i)
            ecfc7 = 80.0*(1.0 - tcfc7)*tw(i,5)* tcfc4 * emplnk(13,i)
            ecfc8 = 70.0*(1.0 - tcfc8)*tw(i,6) * emplnk(14,i)
c
c Emissivity for CH4 band 1306 cm-1
c
            tlw = exp(-1.0*sqrt(up2(i)))
            betac = bch4(i,k)/uch4(i,k)
            ech4 = 6.00444*sqti(i)*log(1.0 + func(uch4(i,k),betac)) *
     $                 tlw * emplnk(3,i)
            tch4 = 1.0/(1.0 + 0.02*func(uch4(i,k),betac))
c
c Emissivity for N2O bands 
c
            u01 = un2o0(i,k)
            u11 = un2o1(i,k)
            beta01 = bn2o0(i,k)/un2o0(i,k)
            beta11 = bn2o1(i,k)/un2o1(i,k)
c
c 1285 cm-1 band
c
            en2o1 = 2.35558*sqti(i)*log(1.0 + func(u01,beta01) +
     $              func(u11,beta11))*tlw*tch4*emplnk(4,i)
            u02 = 0.100090*u01
            u12 = 0.0992746*u11
            beta02 = 0.964282*beta01
c
c 589 cm-1 band
c
            en2o2 = 2.65581*sqti(i)*log(1.0 + func(u02,beta02) +
     $              func(u12,beta02)) * tco2(i) * th2o(i) * 
     $              emplnk(5,i)
            u03 = 0.0333767*u01
            beta03 = 0.982143*beta01
c
c 1168 cm-1 band
c
            en2o3 = 2.54034*sqti(i)*log(1.0 + func(u03,beta03)) *
     $                 tw(i,6) * tcfc8 * emplnk(6,i)
c
c Emissivity for 1064 cm-1 band of CO2
c
            betac1 = 2.97558*pnm(i,k) / (sslp*sqti(i))
            betac2 = 2.0 * betac1
            eco21 = 3.7571*sqti(i)*log(1.0 + func(uco211(i,k),betac1)
     $         + func(uco212(i,k),betac2) + func(uco213(i,k),betac2))
     $         * to3(i) * tw(i,5) * tcfc4 * tcfc7 * emplnk(2,i)
c
c Emissivity for 961 cm-1 band
c
            eco22 = 3.8443*sqti(i)*log(1.0 + func(uco221(i,k),betac1)
     $         + func(uco222(i,k),betac1) + func(uco223(i,k),betac2))
     $         * tw(i,4) * tcfc3 * tcfc6 * emplnk(1,i)
c
c total trace gas emissivity
c
            emstrc(i,k) = ecfc1 + ecfc2 + ecfc3 + ecfc4 + ecfc5 +ecfc6
     $                + ecfc7 + ecfc8 + en2o1 + en2o2 + en2o3 + ech4
     $                + eco21 + eco22
      end do
C
      return
C
      end
 
