c
c This file is used by f2py to generate the Python C wrapper. 
c 
      subroutine inst_driver(jm, im, calday, dgr_lat, dgr_lon, 
     & scon, radius, daysperyear, zen, solin)

c In
      integer jm,im
      real*8 calday
      real*8 dgr_lat(jm)
      real*8 dgr_lon(im)
      real*8 scon
      real*8 radius
      real*8 daysperyear
cf2py intent(in,hide) jm, im
cf2py intent(in) calday, dgr_lat, dgr_lon, scon, radius, daysperyear

c Out
      real*8 zen(jm,im)
      real*8 solin(jm,im)
cf2py intent(out) zen,solin

      do i=1,im
      do j=1,jm
      call insolation(calday, dgr_lat(j), dgr_lon(i), scon, 
     & radius, daysperyear, zen(j,i), solin(j,i) )
      enddo
      enddo

      end
c===============================================================================
      subroutine daily_driver(jm, im, calday, dgr_lat, dgr_lon,
     & scon, radius, daysperyear, zen, solin)
 
c In
      integer jm,im
      real*8 calday
      real*8 dgr_lat(jm)
      real*8 dgr_lon(im)
      real*8 scon
      real*8 radius
      real*8 daysperyear
cf2py intent(in,hide) jm, im
cf2py intent(in) calday, dgr_lat, dgr_lon, scon, radius, daysperyear

c Out
      real*8 zen(jm,im)
      real*8 solin(jm,im)
cf2py intent(out) zen,solin

      do j=1,jm
      call daily_avg_insolation(calday, dgr_lat(j), scon, 
     & radius, daysperyear, zen(j,1), solin(j,1) )
      do i=2,im
      zen(j,i) = zen(j,1)
      solin(j,i) = solin(j,1)
      enddo
      enddo

      end
c===============================================================================
      subroutine annual_driver(jm, im, calday, dgr_lat, dgr_lon,
     & scon, radius, daysperyear, zen, solin)

c In
      integer jm,im
      real*8 calday
      real*8 dgr_lat(jm)
      real*8 dgr_lon(im)
      real*8 scon
      real*8 radius
      real*8 daysperyear
cf2py intent(in,hide) jm, im
cf2py intent(in) calday, dgr_lat, dgr_lon, scon, radius, daysperyear

c Out
      real*8 zen(jm,im)
      real*8 solin(jm,im)
cf2py intent(out) zen,solin

      do j=1,jm
      call annual_avg_insolation(dgr_lat(j), scon, 
     & radius, daysperyear, zen(j,1), solin(j,1) )
      do i=2,im
      zen(j,i) = zen(j,1)
      solin(j,i) = solin(j,1)
      enddo
      enddo

      end
c===============================================================================
      subroutine berger78_driver(orb_year)

c Input
      integer orb_year
cf2py intent(in) orb_year

c Local
      real*8 eccen, obliq, mvelp
c     Orbital params 
      common/orbpar/eccen, obliq, mvelp

      call berger78(orb_year)

      end
