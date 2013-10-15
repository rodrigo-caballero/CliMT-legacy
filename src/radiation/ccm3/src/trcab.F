#include <misc.h>
#include <params.h>
      subroutine trcab(k1      ,k2      ,ucfc11  ,ucfc12  ,un2o0   ,
     $                 un2o1   ,uch4    ,uco211  ,uco212  ,uco213  , 
     $                 uco221  ,uco222  ,uco223  ,bn2o0   ,bn2o1   ,
     $                 bch4    ,to3co2  ,pnm     ,dw      ,pnew    ,
     $                 s2c     ,uptype  ,dplh2o  ,abplnk1 ,tco2    ,
     $                 th2o    ,to3     ,abstrc  )
c----------------------------------------------------------------------
c
c Calculate absorptivity for non nearest layers for CH4, N2O, CFC11 and
c CFC12.
c
c See CCM3 description for equations.
c
C-------------------------Code History----------------------------------
C
C Original version:  J.T. Kiehl, Nov 31, 1994
C Standardized:      T. Acker, Feb 1996
C Reviewed:          J. Kiehl, April 1996        
C 
c-----------------------------------------------------------------------
c
c $Id: trcab.F,v 1.2 2004/09/07 02:47:46 rca Exp $
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
      integer k1,k2                ! level indices
      integer i,l                  ! loop counters
C
      real to3co2(plond)           ! pressure weighted temperature
      real pnm(plond,plevp)        ! interface pressures
      real ucfc11(plond,plevp)     ! CFC11 path length
      real ucfc12(plond,plevp)     ! CFC12 path length
      real un2o0(plond,plevp)      ! N2O path length
C
      real un2o1(plond,plevp)      ! N2O path length (hot band)
      real uch4(plond,plevp)       ! CH4 path length
      real uco211(plond,plevp)     ! CO2 9.4 micron band path length
      real uco212(plond,plevp)     ! CO2 9.4 micron band path length
      real uco213(plond,plevp)     ! CO2 9.4 micron band path length
C
      real uco221(plond,plevp)     ! CO2 10.4 micron band path length
      real uco222(plond,plevp)     ! CO2 10.4 micron band path length
      real uco223(plond,plevp)     ! CO2 10.4 micron band path length
      real bn2o0(plond,plevp)      ! pressure factor for n2o
      real bn2o1(plond,plevp)      ! pressure factor for n2o
C
      real bch4(plond,plevp)       ! pressure factor for ch4
      real dw(plond)               ! h2o path length
      real pnew(plond)             ! pressure
      real s2c(plond,plevp)        ! continuum path length
      real uptype(plond,plevp)     ! p-type h2o path length
C
      real dplh2o(plond)           ! p squared h2o path length
      real abplnk1(14,plond,plevp) ! Planck factor
      real tco2(plond)             ! co2 transmission factor
      real th2o(plond)             ! h2o transmission factor
      real to3(plond)              ! o3 transmission factor
c
c  Output Arguments
c
      real abstrc(plond)           ! total trace gas absorptivity
c
C--------------------------Local Variables------------------------------
c
      real sqti(plond)             ! square root of mean temp
      real du1                     ! cfc11 path length
      real du2                     ! cfc12 path length
      real acfc1                   ! cfc11 absorptivity 798 cm-1
      real acfc2                   ! cfc11 absorptivity 846 cm-1
C
      real acfc3                   ! cfc11 absorptivity 933 cm-1
      real acfc4                   ! cfc11 absorptivity 1085 cm-1
      real acfc5                   ! cfc12 absorptivity 889 cm-1
      real acfc6                   ! cfc12 absorptivity 923 cm-1
      real acfc7                   ! cfc12 absorptivity 1102 cm-1
C
      real acfc8                   ! cfc12 absorptivity 1161 cm-1
      real du01                    ! n2o path length
      real dbeta01                 ! n2o pressure factor
      real dbeta11                 !         "
      real an2o1                   ! absorptivity of 1285 cm-1 n2o band
C
      real du02                    ! n2o path length
      real dbeta02                 ! n2o pressure factor
      real an2o2                   ! absorptivity of 589 cm-1 n2o band
      real du03                    ! n2o path length
      real dbeta03                 ! n2o pressure factor
C
      real an2o3                   ! absorptivity of 1168 cm-1 n2o band
      real duch4                   ! ch4 path length
      real dbetac                  ! ch4 pressure factor
      real ach4                    ! absorptivity of 1306 cm-1 ch4 band
      real du11                    ! co2 path length
C
      real du12                    !       "
      real du13                    !       "
      real dbetc1                  ! co2 pressure factor
      real dbetc2                  ! co2 pressure factor
      real aco21                   ! absorptivity of 1064 cm-1 band
C
      real du21                    ! co2 path length
      real du22                    !       "
      real du23                    !       "
      real aco22                   ! absorptivity of 961 cm-1 band
      real tt(plond)               ! temp. factor for h2o overlap factor
C
      real psi1                    !                 "
      real phi1                    !                 "
      real p1                      ! h2o overlap factor
      real w1                      !        "
      real ds2c(plond)             ! continuum path length
C
      real duptyp(plond)           ! p-type path length
      real tw(plond,6)             ! h2o transmission factor
      real g1(6)                   !         "
      real g2(6)                   !         "
      real g3(6)                   !         "
C
      real g4(6)                   !         "
      real ab(6)                   ! h2o temp. factor
      real bb(6)                   !         "
      real abp(6)                  !         "
      real bbp(6)                  !         "
C
      real tcfc3                   ! transmission for cfc11 band
      real tcfc4                   ! transmission for cfc11 band
      real tcfc6                   ! transmission for cfc12 band
      real tcfc7                   ! transmission for cfc12 band
      real tcfc8                   ! transmission for cfc12 band
C
      real tlw                     ! h2o transmission
      real tch4                    ! ch4 transmission
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
c
C--------------------------Statement Functions--------------------------
c
      real func, u, b
      func(u,b) = u/sqrt(4.0 + u*(1.0 + 1.0 / b))
c
c------------------------------------------------------------------------
c
      do i = 1,plon
        sqti(i) = sqrt(to3co2(i))
c
c h2o transmission 
c
        tt(i) = abs(to3co2(i) - 250.0)
        ds2c(i) = abs(s2c(i,k1) - s2c(i,k2))
        duptyp(i) = abs(uptype(i,k1) - uptype(i,k2))
      end do
c
      do l = 1,6
        do i = 1,plon
          psi1 = exp(abp(l)*tt(i) + bbp(l)*tt(i)*tt(i))
          phi1 = exp(ab(l)*tt(i) + bb(l)*tt(i)*tt(i))
          p1 = pnew(i)*(psi1/phi1)/sslp
          w1 = dw(i)*phi1
          tw(i,l) = exp(-g1(l)*p1*(sqrt(1.0 + g2(l)*(w1/p1)) - 1.0) -
     $                   g3(l)*ds2c(i)-g4(l)*duptyp(i))
        end do
      end do
c
      do i = 1,plon
        du1 = abs(ucfc11(i,k1) - ucfc11(i,k2))
        du2 = abs(ucfc12(i,k1) - ucfc12(i,k2))
c
c cfc transmissions
c
        tcfc3 = exp(-175.005*du1)
        tcfc4 = exp(-1202.18*du1)
        tcfc6 = exp(-5786.73*du2)
        tcfc7 = exp(-2873.51*du2)
        tcfc8 = exp(-2085.59*du2)  
c
c Absorptivity for CFC11 bands          
c
        acfc1 =  50.0*(1.0 - exp(-54.09*du1))*tw(i,1)*abplnk1(7,i,k2)
        acfc2 =  60.0*(1.0 - exp(-5130.03*du1))*tw(i,2)*abplnk1(8,i,k2)
        acfc3 =  60.0*(1.0 - tcfc3)*tw(i,4)*tcfc6*abplnk1(9,i,k2)
        acfc4 = 100.0*(1.0 - tcfc4)*tw(i,5)*abplnk1(10,i,k2)
c
c Absorptivity for CFC12 bands
c
        acfc5 = 45.0*(1.0 - exp(-1272.35*du2))*tw(i,3)*abplnk1(11,i,k2)
        acfc6 = 50.0*(1.0 - tcfc6)* tw(i,4) * abplnk1(12,i,k2)
        acfc7 = 80.0*(1.0 - tcfc7)* tw(i,5) * tcfc4*abplnk1(13,i,k2)
        acfc8 = 70.0*(1.0 - tcfc8)* tw(i,6) * abplnk1(14,i,k2)
c
c Emissivity for CH4 band 1306 cm-1
c
        tlw = exp(-1.0*sqrt(dplh2o(i)))
        duch4 = abs(uch4(i,k1) - uch4(i,k2))
        dbetac = abs(bch4(i,k1) - bch4(i,k2))/duch4
        ach4 = 6.00444*sqti(i)*log(1.0 + func(duch4,dbetac))*
     $         tlw*abplnk1(3,i,k2) 
        tch4 = 1.0/(1.0 + 0.02*func(duch4,dbetac))
c
c Absorptivity for N2O bands 
c
        du01 = abs(un2o0(i,k1) - un2o0(i,k2))
        du11 = abs(un2o1(i,k1) - un2o1(i,k2))
        dbeta01 = abs(bn2o0(i,k1) - bn2o0(i,k2))/du01
        dbeta11 = abs(bn2o1(i,k1) - bn2o1(i,k2))/du11
c
c 1285 cm-1 band
c
        an2o1 = 2.35558*sqti(i)*log(1.0 + func(du01,dbeta01)
     $          + func(du11,dbeta11))*tlw*tch4*abplnk1(4,i,k2)
        du02 = 0.100090*du01
        du12 = 0.0992746*du11
        dbeta02 = 0.964282*dbeta01
c
c 589 cm-1 band
c
        an2o2 = 2.65581*sqti(i)*log(1.0 + func(du02,dbeta02) +
     $          func(du12,dbeta02))*th2o(i)*tco2(i)*abplnk1(5,i,k2)
        du03 = 0.0333767*du01
        dbeta03 = 0.982143*dbeta01
c
c 1168 cm-1 band
c
        an2o3 = 2.54034*sqti(i)*log(1.0 + func(du03,dbeta03))*
     $          tw(i,6)*tcfc8*abplnk1(6,i,k2)
c
c Emissivity for 1064 cm-1 band of CO2
c
        du11 = abs(uco211(i,k1) - uco211(i,k2))
        du12 = abs(uco212(i,k1) - uco212(i,k2))
        du13 = abs(uco213(i,k1) - uco213(i,k2))            
        dbetc1 = 2.97558*abs(pnm(i,k1) + pnm(i,k2))/(2.0*sslp*sqti(i))
        dbetc2 = 2.0*dbetc1
        aco21 = 3.7571*sqti(i)*log(1.0 + func(du11,dbetc1)
     $         + func(du12,dbetc2) + func(du13,dbetc2))
     $         *to3(i)*tw(i,5)*tcfc4*tcfc7*abplnk1(2,i,k2)
c
c Emissivity for 961 cm-1 band
c
        du21 = abs(uco221(i,k1) - uco221(i,k2))
        du22 = abs(uco222(i,k1) - uco222(i,k2))
        du23 = abs(uco223(i,k1) - uco223(i,k2))
        aco22 = 3.8443*sqti(i)*log(1.0 + func(du21,dbetc1)
     $          + func(du22,dbetc1) + func(du23,dbetc2))
     $          *tw(i,4)*tcfc3*tcfc6*abplnk1(1,i,k2)
c
c total trace gas absorptivity
c
        abstrc(i) = acfc1 + acfc2 + acfc3 + acfc4 + acfc5 + acfc6
     $              +  acfc7 + acfc8 + an2o1 + an2o2 + an2o3 + ach4
     $              +  aco21 + aco22
      end do
C
      return
C
      end


 
