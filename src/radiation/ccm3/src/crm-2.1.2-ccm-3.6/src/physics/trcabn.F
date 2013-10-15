#include <misc.h>
#include <params.h>            
      subroutine trcabn(k2      ,kn      ,ucfc11  ,ucfc12  ,un2o0   ,
     $                  un2o1   ,uch4    ,uco211  ,uco212  ,uco213  , 
     $                  uco221  ,uco222  ,uco223  ,tbar    ,bplnk   , 
     $                  winpl   ,pinpl   ,tco2    ,th2o    ,to3     ,
     $                  uptype  ,dw      ,s2c     ,up2     ,pnew    ,
     $                  abstrc  ,uinpl)
c----------------------------------------------------------------------
c
c Calculate nearest layer absorptivity due to CH4, N2O, CFC11 and CFC12
c
c Equations in CCM3 description
C
C-------------------------Code History----------------------------------
C
C Original version:  J.T. Kiehl, Nov 21, 1994
C Standardized:      T. Acker, Feb 1996
C Reviewed:          J. Kiehl, April 1996         
C 
c-----------------------------------------------------------------------
c
c $Id: trcabn.F,v 1.1 2004/09/07 15:27:42 rca Exp $
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
      integer k2                ! level index
      integer kn                ! level index
C
      real tbar(plond,4)        ! pressure weighted temperature
      real ucfc11(plond,plevp)  ! CFC11 path length
      real ucfc12(plond,plevp)  ! CFC12 path length
      real un2o0(plond,plevp)   ! N2O path length
      real un2o1(plond,plevp)   ! N2O path length (hot band)
C
      real uch4(plond,plevp)    ! CH4 path length
      real uco211(plond,plevp)  ! CO2 9.4 micron band path length
      real uco212(plond,plevp)  ! CO2 9.4 micron band path length
      real uco213(plond,plevp)  ! CO2 9.4 micron band path length
      real uco221(plond,plevp)  ! CO2 10.4 micron band path length
C
      real uco222(plond,plevp)  ! CO2 10.4 micron band path length
      real uco223(plond,plevp)  ! CO2 10.4 micron band path length
      real bplnk(14,plond,4)    ! weighted Planck fnc. for absorptivity
      real winpl(plond,4)       ! fractional path length
      real pinpl(plond,4)       ! pressure factor for subdivided layer
C
      real tco2(plond)          ! co2 transmission 
      real th2o(plond)          ! h2o transmission
      real to3(plond)           ! o3 transmission
      real dw(plond)            ! h2o path length
      real pnew(plond)          ! pressure factor
C
      real s2c(plond,plevp)     ! h2o continuum factor
      real uptype(plond,plevp)  ! p-type path length
      real up2(plond)           ! p squared path length
      real uinpl(plond,4)       ! Nearest layer subdivision factor
c
c  Output Arguments
c
      real abstrc(plond)        ! total trace gas absorptivity
c
c--------------------------Local Variables------------------------------
c
      integer i,l               ! loop counters
C
      real sqti(plond)          ! square root of mean temp
      real rsqti(plond)         ! reciprocal of sqti
      real du1                  ! cfc11 path length
      real du2                  ! cfc12 path length
      real acfc1                ! absorptivity of cfc11 798 cm-1 band
C
      real acfc2                ! absorptivity of cfc11 846 cm-1 band
      real acfc3                ! absorptivity of cfc11 933 cm-1 band
      real acfc4                ! absorptivity of cfc11 1085 cm-1 band
      real acfc5                ! absorptivity of cfc11 889 cm-1 band
      real acfc6                ! absorptivity of cfc11 923 cm-1 band
C
      real acfc7                ! absorptivity of cfc11 1102 cm-1 band
      real acfc8                ! absorptivity of cfc11 1161 cm-1 band
      real du01                 ! n2o path length
      real dbeta01              ! n2o pressure factors
      real dbeta11              !        "
C
      real  an2o1               ! absorptivity of the 1285 cm-1 n2o band
      real du02                 ! n2o path length
      real dbeta02              ! n2o pressure factor
      real an2o2                ! absorptivity of the 589 cm-1 n2o band
      real du03                 ! n2o path length
C
      real dbeta03              ! n2o pressure factor
      real an2o3                ! absorptivity of the 1168 cm-1 n2o band
      real duch4                ! ch4 path length
      real dbetac               ! ch4 pressure factor
      real ach4                 ! absorptivity of the 1306 cm-1 ch4 band
C
      real du11                 ! co2 path length
      real du12                 !       "
      real du13                 !       "
      real dbetc1               ! co2 pressure factor
      real dbetc2               ! co2 pressure factor
C
      real aco21                ! absorptivity of the 1064 cm-1 co2 band
      real du21                 ! co2 path length
      real du22                 !       "
      real du23                 !       "
      real aco22                ! absorptivity of the 961 cm-1 co2 band
C
      real tt(plond)            ! temp. factor for h2o overlap
      real psi1                 !          "
      real phi1                 !          "
      real p1                   ! factor for h2o overlap
      real w1                   !          "
C
      real ds2c(plond)          ! continuum path length
      real duptyp(plond)        ! p-type path length
      real tw(plond,6)          ! h2o transmission overlap
      real g1(6)                ! h2o overlap factor
      real g2(6)                !         "
C
      real g3(6)                !         "
      real g4(6)                !         "
      real ab(6)                ! h2o temp. factor
      real bb(6)                !         "
      real abp(6)               !         "  
C
      real bbp(6)               !         "
      real tcfc3                ! transmission of cfc11 band
      real tcfc4                ! transmission of cfc11 band
      real tcfc6                ! transmission of cfc12 band
      real tcfc7                !         "
C
      real tcfc8                !         "
      real tlw                  ! h2o transmission
      real tch4                 ! ch4 transmission
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
C------------------------------------------------------------------
C
      do i = 1,plon
         sqti(i) = sqrt(tbar(i,kn))
         rsqti(i) = 1. / sqti(i)
c
c h2o transmission
c
         tt(i) = abs(tbar(i,kn) - 250.0)
         ds2c(i) = abs(s2c(i,k2+1) - s2c(i,k2))*uinpl(i,kn)
         duptyp(i) = abs(uptype(i,k2+1) - uptype(i,k2))*uinpl(i,kn)
      end do
c
      do l = 1,6
         do i = 1,plon
            psi1 = exp(abp(l)*tt(i)+bbp(l)*tt(i)*tt(i))
            phi1 = exp(ab(l)*tt(i)+bb(l)*tt(i)*tt(i))
            p1 = pnew(i) * (psi1/phi1) / sslp
            w1 = dw(i) * winpl(i,kn) * phi1
            tw(i,l) = exp(- g1(l)*p1*(sqrt(1.0+g2(l)*(w1/p1))-1.0)
     $                  - g3(l)*ds2c(i)-g4(l)*duptyp(i))
         end do
      end do
c
      do i = 1,plon
c
            du1 = abs(ucfc11(i,k2+1) - ucfc11(i,k2)) * winpl(i,kn)
            du2 = abs(ucfc12(i,k2+1) - ucfc12(i,k2)) * winpl(i,kn)
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
            acfc1 = 50.0*(1.0 - exp(-54.09*du1)) * tw(i,1)*bplnk(7,i,kn)
            acfc2 = 60.0*(1.0 - exp(-5130.03*du1))*tw(i,2)*bplnk(8,i,kn)
            acfc3 = 60.0*(1.0 - tcfc3)*tw(i,4)*tcfc6 * bplnk(9,i,kn)
            acfc4 = 100.0*(1.0 - tcfc4)* tw(i,5) * bplnk(10,i,kn)
c
c Absorptivity for CFC12 bands
c
            acfc5 = 45.0*(1.0 - exp(-1272.35*du2))*tw(i,3)
     $                                            *bplnk(11,i,kn)
            acfc6 = 50.0*(1.0 - tcfc6)*tw(i,4)*bplnk(12,i,kn)
            acfc7 = 80.0*(1.0 - tcfc7)* tw(i,5)*tcfc4 *bplnk(13,i,kn)
            acfc8 = 70.0*(1.0 - tcfc8)*tw(i,6)*bplnk(14,i,kn)
c
c Absorptivity for CH4 band 1306 cm-1
c
            tlw = exp(-1.0*sqrt(up2(i)))
            duch4 = abs(uch4(i,k2+1) - uch4(i,k2)) * winpl(i,kn)
            dbetac = 2.94449 * pinpl(i,kn) * rsqti(i) / sslp
            ach4 = 6.00444*sqti(i)*log(1.0 + func(duch4,dbetac)) *
     $           tlw * bplnk(3,i,kn) 
            tch4 = 1.0/(1.0 + 0.02*func(duch4,dbetac)) 
c
c Absorptivity for N2O bands 
c
            du01 = abs(un2o0(i,k2+1) - un2o0(i,k2)) * winpl(i,kn)
            du11 = abs(un2o1(i,k2+1) - un2o1(i,k2)) * winpl(i,kn)
            dbeta01 = 19.399 *  pinpl(i,kn) * rsqti(i) / sslp
            dbeta11 = dbeta01
c
c 1285 cm-1 band
c
            an2o1 = 2.35558*sqti(i)*log(1.0 + func(du01,dbeta01)
     $            + func(du11,dbeta11)) * tlw * tch4 * bplnk(4,i,kn)
            du02 = 0.100090*du01
            du12 = 0.0992746*du11
            dbeta02 = 0.964282*dbeta01
c
c 589 cm-1 band
c
            an2o2 = 2.65581*sqti(i)*log(1.0 + func(du02,dbeta02)
     $             +  func(du12,dbeta02)) * tco2(i) * th2o(i) *
     $                bplnk(5,i,kn)
            du03 = 0.0333767*du01
            dbeta03 = 0.982143*dbeta01
c
c 1168 cm-1 band
c
            an2o3 = 2.54034*sqti(i)*log(1.0 + func(du03,dbeta03)) *
     $           tw(i,6) * tcfc8 * bplnk(6,i,kn)
c
c Absorptivity for 1064 cm-1 band of CO2
c
            du11 = abs(uco211(i,k2+1) - uco211(i,k2)) * winpl(i,kn)
            du12 = abs(uco212(i,k2+1) - uco212(i,k2)) * winpl(i,kn)
            du13 = abs(uco213(i,k2+1) - uco213(i,k2)) * winpl(i,kn)
            dbetc1 = 2.97558 * pinpl(i,kn) * rsqti(i) / sslp
            dbetc2 = 2.0 * dbetc1
            aco21 = 3.7571*sqti(i)*log(1.0 + func(du11,dbetc1)
     $         + func(du12,dbetc2) + func(du13,dbetc2))
     $         * to3(i) * tw(i,5) * tcfc4 * tcfc7 * bplnk(2,i,kn)
c
c Absorptivity for 961 cm-1 band of co2
c
            du21 = abs(uco221(i,k2+1) - uco221(i,k2)) * winpl(i,kn)
            du22 = abs(uco222(i,k2+1) - uco222(i,k2)) * winpl(i,kn)
            du23 = abs(uco223(i,k2+1) - uco223(i,k2)) * winpl(i,kn)
            aco22 = 3.8443*sqti(i)*log(1.0 + func(du21,dbetc1)
     $         + func(du22,dbetc1) + func(du23,dbetc2))
     $         * tw(i,4) * tcfc3 * tcfc6 * bplnk(1,i,kn)
c
c total trace gas absorptivity
c
            abstrc(i) = acfc1 + acfc2 + acfc3 + acfc4 + acfc5 + acfc6
     $                + acfc7 + acfc8 + an2o1 + an2o2 + an2o3 + ach4
     $                + aco21 + aco22 
      end do
C
      return
C
      end




 
