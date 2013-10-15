      subroutine insolation(calday, dgr_lat, dgr_lon, scon, radius, daysperyear, zen, solin)
c
c Returns instantaneous insolation and zenith angle at specified time, lat and lon
c

c In
      real calday       ! Calendar day, including fraction
      real dgr_lat      ! Current centered latitude (degrees)
      real dgr_lon      ! Current centered longitude (degrees)
      real scon         ! Solar constant (W m-2)
      real radius       ! Mean orbital radius (au)
      real daysperyear  ! No. days in a year
c Out
      real zen          ! Solar zenith angle (degrees)
      real solin        ! Insolation (W m-2)
c Local
      real delta        ! Solar declination angle  in radians
      real eccf         ! Earth orbit eccentricity factor
      real coszrs       ! Cosine of zenith angle
 
c     Get declination angle and eccen factor
      call eccf_decl(calday, daysperyear, delta, eccf)

c     get zenith angle
      call zenith(calday, dgr_lat, dgr_lon, delta, zen, coszrs)

      if (zen .lt. 0. .or. zen .gt. 90.) then
         solin = 0.
      else
         solin = scon*eccf*coszrs/radius/radius
      endif

      end
c===============================================================================
      subroutine daily_avg_insolation(calday, dgr_lat, scon, radius, daysperyear, zen, solin)
c
c Returns daily-mean insolation and zenith angle at specified day and lat
c
c In
      real calday       ! Calendar day, including fraction
      real dgr_lat      ! Current centered latitude (degrees)
      real scon         ! Solar constant (W m-2)
      real radius       ! Mean orbital radius (au)
      real daysperyear  ! No. days in a year
c Out
      real zen          ! Solar zenith angle (degrees)
      real solin        ! Insolation (W m-2)
c Local
      real delta        ! Solar declination angle  in radians
      real eccf         ! Earth orbit eccentricity factor
      real coszrs       ! Cosine of zenith angle
 
c     Get declination angle and eccen factor
      calday1 = int(calday) + 0.5 ! compute at noon 
      call eccf_decl(calday1, daysperyear, delta, eccf)

c     Daily-avg zenith angle and insolation
      pi = abs(acos(-1.))
      deg2rad = pi/180.
      phi = dgr_lat*deg2rad
c     handle points poleward of polar circle
      polar_circle = pi/2. - abs(delta) 
      if ( abs(phi) .ge. polar_circle ) then
c       summer hemisph (permanent day)
        if (phi*delta .gt. 0.) h0 = pi
c       winter hemisph (permanent night)
        if (phi*delta .lt. 0.) h0 = 0.
      else
        h0 = acos( -tan(phi)*tan(delta) )
      endif
      coszrs = ( h0*sin(phi)*sin(delta) + cos(phi)*cos(delta)*sin(h0) )/pi
      zen = acos(coszrs)/deg2rad
      solin = scon*eccf*coszrs/radius/radius

      end
c===============================================================================
      subroutine annual_avg_insolation(dgr_lat, scon, radius, daysperyear, zen, solin)
c
c Returns annual-mean insolation and zenith angle at specified lat
c
c In
      real dgr_lat      ! Current centered latitude (degrees)
      real scon         ! Solar constant (W m-2)
      real radius       ! Mean orbital radius (au)
      real daysperyear  ! No. days in a year
c Out
      real zen          ! Solar zenith angle (degrees)
      real solin        ! Insolation (W m-2)


c     Do it the brute-force way by averaging daily insolation
      zen_avg = 0.
      zen_count = 0.
      solin_avg = 0.
      do i = 1,int(daysperyear)
        calday = float(i) 
        call daily_avg_insolation(calday, dgr_lat, scon, radius, daysperyear, zen, solin)
        solin_avg = solin_avg + solin
c       average zen only when sun is up
        if (solin .gt. 0.) then
          zen_avg = zen_avg + zen
          zen_count = zen_count + 1.
        endif
      enddo

      solin = solin_avg / int(daysperyear)
      zen = zen_avg / zen_count

      end
