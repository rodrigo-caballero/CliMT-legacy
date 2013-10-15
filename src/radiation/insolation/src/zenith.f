      subroutine zenith(calday, dgr_lat, dgr_lon, delta, zen, coszrs)
c
c Computes solar zenith angle
c
c In
      real calday     ! Calender day
      real deg_lat    ! Latitude [dgr]
      real deg_lon    ! Longitude [dgr]
      real delta      ! Solar declination angle [rad]
c Out
      real zen        ! Zenith angle [dgr]
      real coszrs     ! Cosine of zenith angle
c Local
      real clat       ! Current centered latitude (radians)
      real clon       ! Centered longitude (radians)
      real orb_pi     ! pi, mathematical constant
      parameter( orb_pi = 3.14159265358979323846)
      real orb_jday   ! Julian calender day (1.xx to 365.xx)
      real orb_lat    ! Centered latitude (radians)
      real orb_lon    ! Centered longitude (radians)
      real orb_declin ! Solar declination (radians)
      real orb_cosz   ! Cosine of the solar zenith angle

c Statement function
      orb_cosz( orb_jday, orb_lat, orb_lon, orb_declin ) = 
     $   sin(orb_lat)*sin(orb_declin) - cos(orb_lat)*cos(orb_declin)
     $   *cos(orb_jday*2.0*orb_pi + orb_lon)

      deg2rad = orb_pi/180.
      clat = dgr_lat*deg2rad
      clon = dgr_lon*deg2rad

      coszrs = orb_cosz( calday, clat, clon, delta )
      zen = acos(coszrs)/deg2rad

      end
