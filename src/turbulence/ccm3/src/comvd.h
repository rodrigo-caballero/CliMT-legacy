c
c $Id: comvd.h,v 1.1 2005/01/24 19:21:44 rca Exp $
c $Author: rca $
c
C
C Constants used in vertical diffusion and pbl
C
      common/comvd/cpair  ,cpvir ,gravit ,rair ,zkmin ,ml2(plevp),
     $             ntopfl ,npbl
C
      real cpair     ! Specific heat of dry air
      real cpvir     ! Derived constant for cp moist air
      real gravit    ! Acceleration due to gravity
      real rair      ! Gas const for dry air
      real zkmin     ! Minimum kneutral*f(ri)
      real ml2       ! Mixing lengths squared
      integer ntopfl ! Top level to which vertical diffusion is applied.
      integer npbl   ! Maximum number of levels in pbl from surface
C
 
