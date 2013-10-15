subroutine eccf_decl(calday, daysperyear, delta, eccf)

! Computes declination angle (latitude of subsolar point) and
! eccentricity factor (which modulates insolation, accounting 
! for deviation of Sun-planet distance from its mean along orbit)
! for a given calendar day and orbital parameters 
! (accessed thru common/orbpar)

! Based on Pierrehumbert (2005), "A first course in climate"
! (http://geosci.uchicago.edu/~rtp1/geo232)

! Notes: 
! - Following (obscure) convention, the precession angle mvelp
!   ("moving vernal equinox longitude of perihelion") is
!              mvelp = pi - kappa1e 
!   where kappa1e is the angle from perihelion to vernal equinox
!
! - To match the calendar currently used on Earth, vernal equinox
!   is defined to occur 80/365 of the way through the year

! Rodrigo Caballero, University of Chicago, 2005.

implicit none

! In
real calday      ! calendar day
real daysperyear ! no. days in a year
real eccen, obliq, mvelp
common/orbpar/eccen, obliq, mvelp

! Out
real delta ! declination angle
real eccf  ! eccentricity factor

! Local
real kappa  ! season angle (0 at vernal equinox)
real kappa1 ! longitude of perihelion (angle from perihelion to sun-planet axis)
real pi 
real, external :: season_angle

if (mvelp .eq. 0.) mvelp = 1.e-4
pi     = abs(acos(-1.))
kappa  = season_angle(calday, daysperyear)
kappa1 = pi - mvelp*pi/180. + kappa 

delta = asin(sin(obliq*pi/180.)*sin(kappa))
eccf  = ( 1. + eccen*cos(kappa1) ) / (1. - eccen*eccen)
eccf  = eccf*eccf

end subroutine eccf_decl
!-------------------------------------------------------------------------
real function season_angle(calday, daysperyear)

! "Season angle" is defined as the angle from vernal equinox to
! position of sun-planet axis on day calday.

! Here, it is found by numerically inverting the relation between 
! season angle and calendar time using Ridder's root-finding  algorithm. 

! To match the calendar currently used on Earth, vernal equinox
! is defined to occur 80/365 of the way through the year

implicit none

! In
real calday       ! calendar day
real daysperyear  ! number of days in year
real eccen, obliq, mvelp
common/orbpar/eccen, obliq, mvelp

! Local
real period     ! adimensional period of orbit
real params(10) ! dummy vector to carray parameters
logical success
real pi
real, external :: ridder,diff,integral


pi = abs(acos(-1.))
period = integral(eccen,2.*pi) 

params(1) = period
params(2) = calday/daysperyear  - 80./365.
season_angle = ridder(success, diff, params, -2.*pi, 2.*pi, .0001)

if (.not.success) then
print*,'damm'
stop
endif

end function season_angle
!------------------------------------------------------------------------
real function diff(params, kappa)

! Returns the difference between time taken to reach  
! season angle kappa, and a specified target time

implicit none

! In
real kappa      ! season angle
real params(10) ! parameter vector
real eccen, obliq, mvelp
common/orbpar/eccen, obliq, mvelp

! Local
real period      ! orbital period
real time        ! time taken to reach season angle kappa 
real target_time ! target time
real kappa1e     ! angle between perihelion and vernal equinox
real pi
real, external :: integral

pi          = abs(acos(-1.))
period      = params(1)
target_time = params(2)

kappa1e = pi - mvelp*pi/180. 
time    = integral(eccen, kappa1e+kappa) - integral(eccen, kappa1e)

diff = time/period - target_time ! note time is adimensionalised by period

end function diff
!------------------------------------------------------------------------
real function integral(e,x)

! Returns indefinite integral of 1/(1+e*cos x)**2

implicit none
real e,x,oneme2,a,pi

oneme2   = 1.-e*e
a = atan( ((1.-e)/(1.+e))**0.5*tan(x/2.) )

pi = abs(acos(-1.))
if (x .gt.  pi)  a = a + pi
if (x .lt. -pi)  a = a - pi

integral = 2./oneme2**1.5*a - e*sin(x)/oneme2/(1.+e*cos(x))

end function integral
!------------------------------------------------------------------------
real function ridder(success, func, params, xmin, xmax, acc)

! Find zero crossing of func(x) in range xmin < x < xmax with accuracy acc
! Use Ridder's method (Numerical Recipes)

real params(10),xmin,xmax,acc,func
logical success
external func

maxit=60  ! max number of iterations
success=.true.

fl=func(params,xmin)
fh=func(params,xmax)

if((fl.gt.0..and.fh.lt.0.).or.(fl.lt.0..and.fh.gt.0.))then
   xl=xmin
   xh=xmax
   ridder=-1.11e30
   do j=1,maxit
      xm=0.5*(xl+xh)
      fm=func(params,xm)
      s=sqrt(fm**2-fl*fh)
      if(s.eq.0.)return
      xnew=xm+(xm-xl)*(sign(1.,fl-fh)*fm/s)
      if (abs(xnew-ridder).le.acc) return
      ridder=xnew
      fnew=func(params,ridder)
      if (fnew.eq.0.) return
      if(sign(fm,fnew).ne.fm) then
         xl=xm
         fl=fm
         xh=ridder
         fh=fnew
      else if(sign(fl,fnew).ne.fl) then
         xh=ridder
         fh=fnew
      else if(sign(fh,fnew).ne.fh) then
         xl=ridder
         fl=fnew
      else
         stop 'never get here in ridder'
      endif
      if(abs(xh-xl).le.acc) return
   enddo
   stop 'ridder exceed maximum iterations maxit'
else if (fl.eq.0.) then
   ridder=xmin
else if (fh.eq.0.) then
   ridder=xmax
else
   success=.false.
endif

end function ridder
