      subroutine driver(km,jm,im, p, o3)

c Routine wrapped by f2py

      real*8, dimension(km,jm,im) ::  p, o3
cf2py intent(in,hide) km,jm,im
cf2py intent(in) p
cf2py intent(out) o3

      call ozone(km,jm,im,p,o3)

      end
