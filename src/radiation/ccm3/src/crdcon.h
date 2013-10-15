c
c $Id: crdcon.h,v 1.2 2004/09/07 02:47:44 rca Exp $
c $Author: rca $
c
C
C Radiation constants
C
      common/crdcon/gravit  ,rga     ,cpair   ,epsilo  ,sslp    ,
     $              stebol  ,rgsslp  ,co2mmr  ,dpfo3   ,dpfco2  ,
     $              dayspy  ,pie
C
      real gravit     ! Acceleration of gravity
      real rga        ! 1./gravit
      real cpair      ! Specific heat of dry air
      real epsilo     ! Ratio of mol. wght of H2O to dry air
      real sslp       ! Standard sea-level pressure
      real stebol     ! Stefan-Boltzmann's constant
      real rgsslp     ! 0.5/(gravit*sslp)
      real co2mmr     ! CO2 mass mixing ratio
      real dpfo3      ! Voigt correction factor for O3
      real dpfco2     ! Voigt correction factor for CO2
      real dayspy     ! Number of days per 1 year
      real pie        ! 3.14.....
C
 
