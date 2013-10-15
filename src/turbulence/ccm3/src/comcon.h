c
c $Id: comcon.h,v 1.1 2005/01/24 19:21:44 rca Exp $
c $Author: rca $
c
C
C Physical constants. This is the master set. All parameterizations set
C their local values from these values.
C
      common/comcon/cappa   ,cpvir   ,cpwv    ,cpair   ,epsilo  ,
     $              ez      ,gravit  ,latvap  ,latice  ,omega   ,
     $              rair    ,rearth  ,rh2o    ,rhoh2o  ,ra      ,
     $              rga     ,stebol  ,tmelt   ,zvir    ,t0(plev)
C
      real cappa       ! R/Cp
      real cpvir       ! cpwv/cpair - 1
      real cpwv        ! Specific heat of water vapor
      real cpair       ! Specific heat of dry air
      real epsilo      ! Ratio of h2o to dry air molecular weights 
      real ez          ! Coriolis expansion coeff -> omega/sqrt(0.375)
      real gravit      ! Gravitational acceleration
      real latvap      ! Latent heat of vaporization
      real latice      ! Latent heat of fusion
      real omega       ! Angular velocity of Earth's rotation
      real rair        ! Gas constant for dry air
      real rearth      ! Radius of the earth
      real rh2o        ! Gas constant for water vapor
      real rhoh2o      ! Density of liquid water (STP)
      real ra          ! Reciprocal of earth radius
      real rga         ! Reciprocal of gravitational acceleration
      real stebol      ! Stefan-Boltzmann's constant
      real tmelt       ! Freezing point of water
      real zvir        ! rh2o/rair - 1
      real t0          ! Reference temperature for t-prime computations
C








 
