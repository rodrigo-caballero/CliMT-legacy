c
c $Id: comadj.h,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c $Author: rca $
c
C
C Convective adjustment
C
      common/comadj/cappa   ,cpair   ,epsilo  ,gravit  ,latvap  ,
     $              rhoh2o  ,cldcp   ,clrh2o  ,nlvdry
C
      real cappa            ! R/cp
      real cpair            ! Specific heat of dry air
      real epsilo           ! Ratio of h2o to dry air molecular weights 
      real gravit           ! Gravitational acceleration 
      real latvap           ! Latent heat of vaporization
      real rhoh2o           ! Density of liquid water (STP)
      real cldcp            ! Latvap/cpair (L/cp)
      real clrh2o           ! Ratio of latvap to water vapor gas const
C
      integer nlvdry        ! Number of levels to apply dry adjustment
C

 
