     
! A collection of functions to compute various moist thermodynamic quantities

! Rodrigo Caballero Augi 2000, 2005

! References: Bolton (1980), Mon. Wea. Rev. 108, 1046-1053 
!             Bohren and Albrecht (1998), "Atmospheric Thermodynamics"
!             Flatau et al (1992), J. App. Met . 31, 1507-1513.

!------------------------------------------------------------------------
subroutine tdew(km,jm,im,p,q,t)

! compute dew point temperature tdew  [K] 
! at pressure p [mb] and specific humidity q [g/kg]
! Use Bolton eqn (11)
!
  implicit none
  integer,intent(in)                       :: km,jm,im
!f2py intent(hide) km,jm,im
  real(8), dimension(km,jm,im), intent(in) :: p,q
  real(8), dimension(km,jm,im), intent(out) :: t

! Local
  real(8) :: Rv, Rd, eps
  real(8), dimension(km,jm,im) :: r,es,loges

  Rv  = 461.5
  Rd  = 287.04
  eps = Rd/Rv
  r = q/(1.e3 - q) * 1.e3 ! water vap. mixing ratio [g/kg]
  es = p*r/(r+eps*1.e3)   ! vapor pressure of water vap
  loges = log(es)

  t = ( 243.5*loges-440.8 )/( 19.48-loges ) + 273.15

end subroutine tdew
!------------------------------------------------------------------------
subroutine tstar(km,jm,im,T,p,q,Ts)

! compute saturation point temperature Ts [K] 
! (i.e. temperature at lifting condensation level) 
! given temperature T [K], pressure p [mb], specific humidity q [g/kg].
! Use Bolton eqn (12)

  implicit none
  integer,intent(in)                       :: km,jm,im
!f2py intent(hide) km,jm,im
  real(8), dimension(km,jm,im), intent(in) :: T,p,q
  real(8), dimension(km,jm,im), intent(out) :: Ts

! Local
  real(8) :: Rv, Rd, eps, oneoverk
  real(8), dimension(km,jm,im) :: r,th,c,e

  Rv  = 461.5
  Rd  = 287.04
  eps = Rd/Rv
  oneoverk = 3.504 ! Cp/Rd
  r = q/(1.e3 - q)  ! water vap. mixing ratio [kg/kg]

!  call theta(km,jm,im,t,p,q,th)
!  c=log( r/(r+eps*1.e3)/t**oneoverk / 1.7743e-8 )
!  ts=217.8*c/(12.992-c) + 273.15
  e = p*r/(r+eps)
  Ts = 2840./( 3.5*log(t) - log(e) - 4.805) + 55. 

end subroutine tstar
!------------------------------------------------------------------------
subroutine theta(km,jm,im, Rd, Rv, cpd, cpv, p0, p, T, q, th)

! compute potential temperature  theta [K]
! given temperature t [K], pressure p [mb], specific humidity q [g/kg].
 
  implicit none
  integer,intent(in)                       :: km,jm,im
!f2py intent(hide) km,jm,im
  real(8), intent(in)                       :: p0,Rd,Rv,cpd,cpv
  real(8), dimension(km,jm,im), intent(in)  :: T,p,q
  real(8), dimension(km,jm,im), intent(out) :: th

! Local
  real(8) :: eps
  real(8), dimension(km,jm,im) :: cp,R,kappa

  eps   = Rd/Rv
  R     = Rd * (1.+ (1.-eps)/eps*q*1.e-3)
  cp    = cpd * (1. + (cpv/cpd-1.)*q*1.e-3)
  kappa = R/cp 

  th = T*(p0/p)**kappa
      
end subroutine theta
!------------------------------------------------------------------------
subroutine theta_old(km,jm,im,t,p,q,th)

! compute potential temperature  theta [K]
! given temperature t [K], pressure p [mb], specific humidity q [g/kg].
! Use Bolton eqn (7) 
 
  implicit none
  integer,intent(in)                       :: km,jm,im
!f2py intent(hide) km,jm,im
  real(8), dimension(km,jm,im), intent(in) :: t,p,q
  real(8), dimension(km,jm,im), intent(out) :: th

! Local
  real(8) :: ak
  real(8), dimension(km,jm,im) :: r

  ak=0.2854 ! R_(dry air)/Cp
  r = q/(1.e3 - q) * 1.e3 ! water vap. mixing ratio [g/kg]

  th = t*(1000./p)**( ak*(1.-0.28e-3*r) )
      
end subroutine theta_old
!------------------------------------------------------------------------
subroutine thetae(km,jm,im,Rd,Rv,lv0,cpd,cpv,cl,p0,p,T,wt,the)

! compute equivalent potential temperature the [K]
! given temperature t [K], pressure p [mb], 
! total water mixing ratio [g/kg].
! Use Bohren+Albrecht (6.119) and (6.122), p.293

  implicit none
  integer, intent(in)                       :: km,jm,im
!f2py intent(hide) km,jm,im
  real(8), intent(in)                       :: p0,Rd,Rv,lv0,cpv,cpd,cl
  real(8), dimension(km,jm,im), intent(in)  :: p,T,wt
  real(8), dimension(km,jm,im), intent(out) :: the

! Local
  integer :: k,j,i
  real(8), dimension(km,jm,im)  :: esat,wsat,lv,cp,T1,p1,wt1
  real(8) :: eps,q,Tst,pst

  eps = Rd/Rv

  T1 = T
  p1 = p
  wt1 = wt
  call ws(km,jm,im,Rd,Rv,T,p,wsat)
  do i=1,im
     do j=1,jm
        do k=1,km 
           ! if wt was not specified, set it to wsat
           if (wt(k,j,i) < 0.)  then
              wt1(k,j,i) = wsat(k,j,i)
           ! if parcel unsaturated, bring to LCL
           else if (wt(k,j,i) < wsat(k,j,i))  then
              q = wt(k,j,i)/(1.+wt(k,j,i)*1.e-3) 
              call tstar(1,1,1,T(k,j,i),p(k,j,i),q,Tst) ! gives T at LCL
              pst = p(k,j,i)*(Tst/T(k,j,i))**(cpd/Rd)   ! gives p at LCL
              T1(k,j,i) = Tst
              p1(k,j,i) = pst
           endif
        enddo
     enddo
  enddo
  call es(km,jm,im,T1,esat)
  call ws(km,jm,im,Rd,Rv,T1,p1,wsat)
  lv  = lv0 + (cpv-cl)*(T1-273.15)           
  cp  = cpd+wt1*1.e-3*cl
  the = T1*((p1-esat)/p0)**(-Rd/cp)*exp(lv*wsat*1.e-3/cp/T1)
 
end subroutine thetae
!------------------------------------------------------------------------
subroutine thetaes(km,jm,im,Rd,Rv,lv0,cpd,cpv,cl,p0,p,T,thes)

! compute saturation equivalent potential temperature thes [K]
! given temperature T [K] and pressure p [mb]
! Use Bohren+Albrecht (6.123), p.293

  implicit none
  integer, intent(in)                       :: km,jm,im
!f2py intent(hide) km,jm,im
  real(8), intent(in)                       :: p0,Rd,Rv,lv0,cpv,cpd,cl
  real(8), dimension(km,jm,im), intent(in)  :: p,T
  real(8), dimension(km,jm,im), intent(out) :: thes

! Local
  integer :: k,j,i
  real(8), dimension(km,jm,im)  :: esat,wsat,lv,cp
  real(8) :: eps,q,Tst,pst

  eps = Rd/Rv

  call es(km,jm,im,T,esat)
  call ws(km,jm,im,Rd,Rv,T,p,wsat)
  wsat = wsat * 1.e-3 ! g/kg -> kg/kg
  lv  = lv0 + (cpv-cl)*(T-273.15)           
  cp  = cpd + wsat*cl
  thes = T*((p-esat)/p0)**(-Rd/cp)*exp(lv*wsat/cp/T)
 
end subroutine thetaes
!------------------------------------------------------------------------
subroutine thetae_old(km,jm,im,t,p,q,the)

! compute equivalent potential temperature thetae [K]
! given temperature t [K], pressure p [mb], specific humidity q [g/kg].
! Use Bolton eqn (38)

  implicit none
  integer,intent(in)                       :: km,jm,im
!f2py intent(hide) km,jm,im
  real(8), dimension(km,jm,im), intent(in) :: t,p,q
  real(8), dimension(km,jm,im), intent(out) :: the

! Local
  real(8), dimension(km,jm,im) :: r,th,ts

  call theta(km,jm,im,t,p,q,th)
  call tstar(km,jm,im,t,p,q,ts)
  r = q/(1.e3 - q) * 1.e3 ! water vap. mixing ratio [g/kg]

  the = th * exp( (3.376/ts-0.00254)*r*(1.+0.81e-3*r) )

end subroutine thetae_old
!------------------------------------------------------------------------
subroutine es(km,jm,im,T,e)

! Compute saturation partial pressure of water vapor es [mb]
! at temperature T [K].
! Use Bohren+Albrecht p. 198
     
  implicit none
  integer,intent(in)                       :: km,jm,im
!f2py intent(hide) km,jm,im
  real(8), dimension(km,jm,im), intent(in) :: T
  real(8), dimension(km,jm,im), intent(out) :: e

! Local
  real(8) :: T0

  T0 = 273.15
  e  = 6.11 * exp( 6808.*(1./T0 - 1./T) - 5.09*log(T/T0) )

end subroutine es
!------------------------------------------------------------------------
subroutine esflatau(km,jm,im,T,i,e)

! Compute saturation partial pressure of water vapor es [mb]
! at temperature T [K].
! Use 8th oRder polynomial fit of Flatau et al (1992), Table 4

! i=1 => vapour pressure over water (valid -85C < t < 70C)
! i=2 => vapour pressure over ice (valid -90C < t < 0C)
    
  implicit none
  integer,intent(in)                       :: km,jm,im,i
!f2py intent(hide) km,jm,im
  real(8), dimension(km,jm,im), intent(in) :: T
  real(8), dimension(km,jm,im), intent(out) :: e

! Local
  integer :: n
  real(8) :: t0
  real(8), dimension(9,2) :: a

  data a / 6.11239921,    0.443987641,  0.142986287e-1,  0.264847430e-3, &
       0.302950461e-5, 0.206739458e-7, 0.640689451e-10,-0.952447341e-13, &
       -0.976195544e-15,                                                 &
       6.11147274,    0.503160820, 0.188439774e-1,  0.420895665e-3,  &
   0.615021634e-5, 0.602588177e-7, 0.385852041e-9, 0.146898966e-11,  &
   0.252751365e-14 /

  t0=273.15

  e = a(9,i)
  do n=8,1,-1
     e = e*(t-t0) + a(n,i)
  enddo

end subroutine esflatau
!------------------------------------------------------------------------
subroutine qs(km,jm,im,Rd,Rv,T,p,q)

! Compute saturation specific humidity qs [g/kg]
! given temperature T [K] and total (air+vapor) pressure p [mb]
! use Bohren+Albrecht p. 186
     
  implicit none
  integer,intent(in)                        :: km,jm,im
!f2py intent(hide) km,jm,im
  real(8), intent(in)                       :: Rd,Rv
  real(8), dimension(km,jm,im), intent(in)  :: T,p
  real(8), dimension(km,jm,im), intent(out) :: q

! Local
  real(8) :: eps
  real(8), dimension(km,jm,im) :: e

  eps=Rd/Rv
  call es(km,jm,im,T,e)
  q = eps*e/(p+e*(eps-1.)) * 1.e3

end subroutine qs
!------------------------------------------------------------------------
subroutine qsflatau(km,jm,im,Rd,Rv,T,p,i,q)

! Compute saturation specific humidity qs [g/kg]
! given temperature T [K] and total (air+vapor) pressure p [mb]
! Use 8th oRder polynomial fit of Flatau et al (1992), Table 4

! i=1 => sat. mix. rat. over water (valid -85C < t < 70C)
! i=2 => sat. mix. rat. over ice (valid -90C < t < 0C)

  implicit none
  integer,intent(in)                        :: km,jm,im,i
!f2py intent(hide) km,jm,im
  real(8), intent(in)                       :: Rd,Rv
  real(8), dimension(km,jm,im), intent(in)  :: T,p
  real(8), dimension(km,jm,im), intent(out) :: q

! Local
  real(8) :: eps
  real(8), dimension(km,jm,im) :: e

  eps=Rd/Rv
  call esflatau(km,jm,im,T,i,e)
  q = eps*e/(p+e*(eps-1.)) * 1.e3
!  q = eps*e/p * 1.e3
  
end subroutine qsflatau
!------------------------------------------------------------------------
subroutine ws(km,jm,im,Rd,Rv,T,p,w)

! Compute saturation water vapor mass mixing ratio  ws [g/kg]
! given temperature T [K] and total (air+vapor) pressure p [mb]
! use Bohren+Albrecht p. 186
     
  implicit none
  integer,intent(in)                        :: km,jm,im
!f2py intent(hide) km,jm,im
  real(8), intent(in)                       :: Rd,Rv
  real(8), dimension(km,jm,im), intent(in)  :: T,p
  real(8), dimension(km,jm,im), intent(out) :: w

! Local
  real(8) :: eps
  real(8), dimension(km,jm,im) :: e

  eps=Rd/Rv
  call es(km,jm,im,T,e)
  w = eps*e/(p-e) * 1.e3

end subroutine ws
!------------------------------------------------------------------------
subroutine wsflatau(km,jm,im,Rd,Rv,T,p,i,w)

! Compute saturation water vapor mass mixing ratio ws [g/kg]
! given temperature T [K] and total (air+vapor) pressure p [mb]
! Use 8th oRder polynomial fit of Flatau et al (1992), Table 4

! i=1 => sat. mix. rat. over water (valid -85C < t < 70C)
! i=2 => sat. mix. rat. over ice (valid -90C < t < 0C)

  implicit none
  integer,intent(in)                        :: km,jm,im,i
!f2py intent(hide) km,jm,im
  real(8), intent(in)                       :: Rd,Rv
  real(8), dimension(km,jm,im), intent(in)  :: T,p
  real(8), dimension(km,jm,im), intent(out) :: w

! Local
  real(8) :: eps
  real(8), dimension(km,jm,im) :: e

  eps=Rd/Rv
  call esflatau(km,jm,im,t,i,e)
  w = eps*e/(p-e) * 1.e3

end subroutine wsflatau
!------------------------------------------------------------------------
subroutine pdryadiab(km,jm,im,t,theta,q,p)

! compute pressure level pdryadiab [mb] at which temperature is t [K]
! on the dry (i.e. unsaturated) adiabat identified by 
! potential temperature theta [K] and mixing ratio q [g/kg].
! Use Bolton eqn (7) 

  implicit none
  integer,intent(in)                       :: km,jm,im
!f2py intent(hide) km,jm,im
  real(8), dimension(km,jm,im), intent(in) :: t,theta,q
  real(8), dimension(km,jm,im), intent(out) :: p

! Local
  real(8) :: oneoverk
  real(8), dimension(km,jm,im) :: r

  oneoverk=3.504 ! Cp/R_(dry air)
  r = q/(1.e3 - q) * 1.e3 ! water vap. mixing ratio [g/kg]

  p = 1000.*(t/theta)**( oneoverk/(1.-0.28e-3*r) )

end subroutine pdryadiab
!------------------------------------------------------------------------
subroutine tdryadiab(km,jm,im,theta,p,q,T)

! Compute temperature T [K] at level p [mb] 
! on the dry (i.e. unsaturated) adiabat identified by 
! potential temperature theta [K] and mixing ratio q [g/kg].
! Use Bolton eqn (7) 

  implicit none
  integer,intent(in)                       :: km,jm,im
!f2py intent(hide) km,jm,im
  real(8), dimension(km,jm,im), intent(in) :: theta,p,q
  real(8), dimension(km,jm,im), intent(out) :: T

! Local
  real(8) :: ak
  real(8), dimension(km,jm,im) :: r

  ak=0.2854 ! R_(dry air)/Cp
  r = q/(1.e3 - q) * 1.e3 ! water vap. mixing ratio [g/kg]

  T = theta*(1000./p)**( -ak*(1.-0.28e-3*r) )
      
end subroutine tdryadiab
!------------------------------------------------------------------------
subroutine tmoistadiab(km,jm,im,thetaes,p,t)

! Compute temperature [K] at level p [mb] 
! on the moist (i.e. saturated) pseudo-adiabat identified 
! by saturation equivalent potential temperature thetaes [K].
! Do it by varying t until 
! delthetae := thetaes - thetae(t,p,qs(t,p)) = 0
! Use Bolton, eqn (43)

  implicit none
  integer,intent(in)                       :: km,jm,im
!f2py intent(hide) km,jm,im
  real(8), intent(in)                      :: thetaes
  real(8), dimension(km,jm,im), intent(in) :: p
  real(8), dimension(km,jm,im), intent(out) :: t

! Local
  integer :: i,j,k
  real(8) :: tmin,tmax,f1,f2,delthetae,ridder
  real(8), dimension(10) :: params
  external delthetae,ridder
  logical success

do i=1,im
   do j=1,jm
      do k=1,km
         params = 0.
         params(1) = thetaes
         params(2) = p(k,j,i)
         ! bracket zero crossing to within 2K
         tmin = 50.
         tmax = 51.
         f1 = delthetae(params,tmin)
         f2 = delthetae(params,tmax)
1        continue
         if(tmax.gt.500.) goto 2
         if(f1*f2.lt.0.) goto 3
         tmax = tmax+1.
         f2 = delthetae(params,tmax)
         goto 1
2        stop 'tmoistadiab(): cannot bracket root'
3        continue
         tmin = tmax-2.
         !  refine estimate using Ridder's method 
         t(k,j,i) = ridder(success,delthetae, params, tmin, tmax, 0.0001)
         if (.not.success) stop 'tmoistadiab(): root not bracketed'
      enddo
   enddo
enddo

end subroutine tmoistadiab
!-----------------------------------------------------------------------
real(8) function delthetae(params,t)

  implicit none
  real(8)                :: p,t,q,thetaes,the
  real(8), dimension(10) :: params
  external qs,thetae

  thetaes   = params(1)
  p         = params(2)
  call qs(1,1,1,t,p,q)
  call thetae_old(1,1,1,t,p,q,the)
  delthetae = thetaes - the

end function delthetae
!------------------------------------------------------------------------
real(8) function ridder(success,func,params,xmin,xmax,acc)

! Find zero crossing of func(x) in range xmin < x < xmax with accuracy acc
! Use Ridder's method (Numerical Recipes)

  implicit real*8 (a-h,o-z)
  real*8 params(10),xmin,xmax,acc
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
        xnew=xm+(xm-xl)*(sign(1.,real(fl-fh))*fm/s)
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


!------------------------------------------------------------------------
subroutine z(km,jm,im,R,g,p0,p,t,zz)

!
  implicit none
  integer, intent(in)                       :: km,jm,im
!f2py intent(hide) km,jm,im
  real(8), intent(in)                       :: R,g
  real(8), dimension(jm,im), intent(in)     :: p0
  real(8), dimension(km,jm,im), intent(in)  :: p,t
  real(8), dimension(km,jm,im), intent(out) :: zz

! Local
  integer :: k
  real(8), dimension(jm,im) :: dp,pm,Tm

! bottom level
  dp = (p0 - p(km,:,:))
  pm = (p0 + p(km,:,:))/2.
  zz(km,:,:) = R/g * T(km,:,:)/pm * dp

! build up from bottom
  do k=km-1,1,-1
     dp = p(k+1,:,:) - p(k,:,:)
     pm = (p(k+1,:,:) + p(k,:,:))/2.
     Tm = (T(k+1,:,:) + T(k,:,:))/2.
     zz(k,:,:) = zz(k+1,:,:) + R/g * Tm/pm * dp
  enddo

end subroutine z
!------------------------------------------------------------------------
subroutine pseudoadiab(km,jm,im,Rd,Rv,lv0,cpd,cpv,cl,p_in0,p00,T00,T_out)

  implicit none
  integer, parameter :: N=10000
  integer, intent(in)                       :: km,jm,im
!f2py intent(hide) km,jm,im
  real(8), intent(in)                       :: Rd,Rv,lv0,cpv,cpd,cl
  real(8), dimension(jm,im), intent(in)     :: p00,T00
  real(8), dimension(km,jm,im), intent(in)  :: p_in0
  real(8), dimension(km,jm,im), intent(out) :: T_out

! Local
  integer :: k,j,i,kmin
  real(8) :: eps,dp,e,w,R,lv,cp,T,p,pmax,pmin,delta
  real(8), dimension(jm,im)     :: p0,T0
  real(8), dimension(km,jm,im)  :: p_in

  eps = Rd/Rv
  p0 = log(p00*100.)
  T0 = log(T00)
  p_in = log(p_in0*100.)

  do i=1,im
     do j=1,jm
        pmax = max( p0(j,i), p_in(km,j,i) )
        pmin = min( p0(j,i), p_in(1,j,i) )
        dp = (pmax-pmin)/(N-1.)

        !--- up to top
        T = T0(j,i)
        p = p0(j,i)
        ! check if we're matching any of the required levels
        kmin = minloc(abs(p-p_in(:,j,i)),1)
        delta = abs(p-p_in(kmin,j,i))
        if (delta.le.dp) T_out(kmin,j,i) = T
        do k=1,N
           ! see if we've gone off the top
           p = p - dp
           if (p.lt.pmin) exit
           ! compute new T
           call es(1,1,1,exp(T),e)
           w = eps*e/(exp(p)/100.-e)
           cp = cpd+w*cl
           R = Rd*(1.+(1.-eps)/eps*w/(w+1.))
           lv = lv0 + (cpv-cl)*(exp(T)-273.15)
           T = T - Rd/cp* &
                (1.+lv*w/R/exp(T))/(1.+lv*lv*w/cp/Rv/exp(T)/exp(T)) * dp
           ! check if we're matching any of the required levels
           kmin = minloc(abs(p-p_in(:,j,i)),1)
           delta = abs(p-p_in(kmin,j,i))
           if (delta.le.dp) T_out(kmin,j,i) = T
        enddo

        ! down to bottom
        T = T0(j,i)
        p = p0(j,i)
        do k=1,N
           ! see if we've gone off the bottom
           p = p + dp
           if (p.gt.pmax) exit
           ! compute new value of T
           call es(1,1,1,exp(T),e)
           w = eps*e/(exp(p)/100.-e)
           cp = cpd+w*cl
           R = Rd*(1.+(1.-eps)/eps*w/(w+1.))
           lv = lv0 + (cpv-cl)*(exp(T)-273.15)
           T = T + Rd/cp* &
                (1.+lv*w/R/exp(T))/(1.+lv*lv*w/cp/Rv/exp(T)/exp(T)) * dp
           ! check if we're matching any of the required levels
           kmin = minloc(abs(p-p_in(:,j,i)),1)
           delta = abs(p-p_in(kmin,j,i))
           if (delta.le.dp) T_out(kmin,j,i) = T
        enddo

     enddo
  enddo

  T_out = exp(T_out)

end subroutine pseudoadiab
!------------------------------------------------------------------------
subroutine cape(km,jm,im,Rd,Rv,lv0,cpd,cpv,cl,virtual,ml_depth,p_in, T_in, q_in, c)

  implicit none
  integer, intent(in)                       :: km,jm,im
!f2py intent(hide) km,jm,im
  integer, intent(in)                       :: virtual
  real(8), intent(in)                       :: ml_depth
  real(8), intent(in)                       :: Rd,Rv,lv0,cpv,cpd,cl
  real(8), dimension(km,jm,im), intent(in)  :: p_in, T_in, q_in
  real(8), dimension(jm,im), intent(out)    :: c

! Local
  integer :: k,j,i
  real(8) :: eps,delT
  real(8), dimension(km,jm,im)  :: q,qp, Tp, Tvp, Tvs
  real(8), dimension(jm,im)  :: T0,q0, pst,Tst
  real(8), dimension(0:km)  :: pint
  real(8), dimension(km)  :: dp, dlnp

  eps = Rd/Rv
  q = max(q_in,q_in*0.) ! take out any negs in q

! get saturation point temp and press Tst, pst for mean parcel in mixed layer
  call mean_parcel(km,jm,im,Rd,cpd,ml_depth,p_in,T_in,q,T0,q0,Tst,pst)

! get the temperature profile of the corresponding moist adiabat 
  call pseudoadiab(km,jm,im,Rd,Rv,lv0,cpd,cpv,cl,p_in,pst,Tst,Tp)

! get the temperature profile of the corresponding dry adiabat 
  do k=1,km
     Tp(k,:,:) = max( Tp(k,:,:), Tst*(p_in(k,:,:)/pst)**(Rd/cpd) )
  end do

! apply virtual temp correction if required
  if (virtual > 0) then
     Tvs = T_in*(1.+ (1.-eps)/eps*q*1.e-3)
     call qs(km,jm,im,Rd,Rv,Tp,p_in,qp)
     do k=1,km
        qp(k,:,:) = min( qp(k,:,:), q0)
     end do
     Tvp = Tp*(1.+ (1.-eps)/eps*qp*1.e-3)
  endif 

! integrate to get CAPE
  c = 0.
  do i=1,im
     do j=1,jm
        pint(1:km-1) = (p_in(2:km,j,i)+p_in(1:km-1,j,i))/2.
        pint(0) = 0.
        pint(km) = p_in(km,j,i)+(p_in(km,j,i)-p_in(km-1,j,i))/2.
        dp = pint(1:km)-pint(0:km-1)
        dlnp = dp/p_in(:,j,i)
        do k=1,km
           if (virtual == 0) then 
              delT = Tp(k,j,i)-T_in(k,j,i)
           else
              delT = Tvp(k,j,i)-Tvs(k,j,i) 
           endif
           if ( delT > 0. ) then
              c(j,i) = c(j,i) + Rd*delT*dlnp(k)
           endif
        enddo
     enddo
  enddo

end subroutine cape
!------------------------------------------------------------------------
subroutine cine(km,jm,im,Rd,Rv,lv0,cpd,cpv,cl,virtual,ml_depth,p_in, T_in, q_in, c)

  implicit none
  integer, intent(in)                       :: km,jm,im
!f2py intent(hide) km,jm,im
  integer, intent(in)                       :: virtual
  real(8), intent(in)                       :: ml_depth
  real(8), intent(in)                       :: Rd,Rv,lv0,cpv,cpd,cl
  real(8), dimension(km,jm,im), intent(in)  :: p_in, T_in, q_in
  real(8), dimension(jm,im), intent(out)    :: c

! Local
  integer :: k,j,i
  real(8) :: eps,delT
  real(8), dimension(km,jm,im)  :: q,qp, Tp, Tvp, Tvs
  real(8), dimension(jm,im)  :: T0,q0, pst,Tst
  real(8), dimension(0:km)  :: pint
  real(8), dimension(km)  :: dp, dlnp

  eps = Rd/Rv
  q = max(q_in,q_in*0.) ! take out any negs in q

! get saturation point temp and press Tst, pst for mean parcel in mixed layer
  call mean_parcel(km,jm,im,Rd,cpd,ml_depth,p_in,T_in,q,T0,q0,Tst,pst)

! get the temperature profile of the corresponding moist adiabat 
  call pseudoadiab(km,jm,im,Rd,Rv,lv0,cpd,cpv,cl,p_in,pst,Tst,Tp)

! get the temperature profile of the corresponding dry adiabat 
  do k=1,km
     Tp(k,:,:) = max( Tp(k,:,:), Tst*(p_in(k,:,:)/pst)**(Rd/cpd) )
  end do

! apply virtual temp correction if required
  if (virtual > 0) then
     Tvs = T_in*(1.+ (1.-eps)/eps*q*1.e-3)
     call qs(km,jm,im,Rd,Rv,Tp,p_in,qp)
     do k=1,km
        qp(k,:,:) = min( qp(k,:,:), q0)
     end do
     Tvp = Tp*(1.+ (1.-eps)/eps*qp*1.e-3)
  endif 

  ! integrate to get CINE
  c = 0.
  do i=1,im
     do j=1,jm
        pint(1:km-1) = (p_in(2:km,j,i)+p_in(1:km-1,j,i))/2.
        pint(0) = 0.
        pint(km) = p_in(km,j,i)+(p_in(km,j,i)-p_in(km-1,j,i))/2.
        dp = pint(1:km)-pint(0:km-1)
        dlnp = dp/p_in(:,j,i)
        inner_loop: do k=km,1,-1
           if (virtual == 0) then 
              delT = Tp(k,j,i)-T_in(k,j,i)
           else
              delT = Tvp(k,j,i)-Tvs(k,j,i) 
           endif
           if ( delT < 0. ) then 
              c(j,i) = c(j,i) + Rd*delT*dlnp(k)
           else
              exit inner_loop
           endif
           enddo inner_loop
     enddo
  enddo
  
  c = -c

end subroutine cine
!------------------------------------------------------------------------
subroutine mean_parcel(km,jm,im,Rd,cpd,ml_depth,p,T,q,T0,q0,Tst,pst)

! compute mean thermodyn properties over layer of depth ml_depth above ground

  implicit none
  integer, intent(in)                       :: km,jm,im
  real(8), intent(in)                       :: ml_depth
  real(8), intent(in)                       :: Rd,cpd
  real(8), dimension(km,jm,im), intent(in)  :: p, T, q
  real(8), dimension(jm,im), intent(out)    :: Tst,pst,T0,q0

! Local
  integer :: k,j,i
  real(8) :: dp
  real(8), dimension(5,jm,im)  :: p5,T5,q5,theta5
  real(8), dimension(jm,im)  :: p0

! if mixed layer depth is 0, use surface values
  if (ml_depth == 0.) then
     p0 = p(km,:,:)
     T0 = T(km,:,:)
     q0 = q(km,:,:)

  else
! compute new pressure grid, equispaced over 5 layers from ground to top
     dp = ml_depth/5.
     do k=1,5
        p5(k,:,:) = p(km,:,:) - dp*(5-k)
     end do

! interpolate T, q onto new pressure grid
     do i=1,im
        do j=1,jm
           call interpol(p(:,j,i),T(:,j,i),km,p5(:,j,i),T5(:,j,i),5)
           call interpol(p(:,j,i),q(:,j,i),km,p5(:,j,i),q5(:,j,i),5)
        end do
     end do
  
! average conserved quantities over mixed layer
     p0 = p(km,:,:)
     do k=1,5
        theta5(k,:,:) = T5(k,:,:)*( p5(k,:,:)/p0 )**(Rd/cpd)
     end do
     do i=1,im
        do j=1,jm
           T0(j,i) = sum(theta5(:,j,i))/5.
           q0(j,i) = sum(q5(:,j,i))/5.
        end do
     end do

  endif

! compute saturation point pressure and temp
  call tstar(1,jm,im,T0,p0,q0,Tst)
  pst = p0*(Tst/T0)**(cpd/Rd)

end subroutine mean_parcel
!--------------------------------------------------------------------------
subroutine interpol(xin,yin,nin,xout,yout,nout)

  real xin(nin),yin(nin),xout(nout),yout(nout),spl(2000)

  yp1=1.e30
  ypn=1.e30
  call spline(xin,yin,nin,yp1,ypn,spl) 

  do n=1,nout
     call splint(xin,yin,spl,nin,xout(n),yout(n))
  enddo

end subroutine interpol
!------------------------------------------------------------------------
SUBROUTINE spline(x,y,n,yp1,ypn,y2)
  INTEGER n,NMAX
  REAL yp1,ypn,x(n),y(n),y2(n)
  PARAMETER (NMAX=500)
  INTEGER i,k
  REAL p,qn,sig,un,u(NMAX)
  if (yp1.gt..99e30) then
     y2(1)=0.
     u(1)=0.
  else
     y2(1)=-0.5
     u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
  endif
  do  i=2,n-1
     sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
     p=sig*y2(i-1)+2.
     y2(i)=(sig-1.)/p
     u(i)=(6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))&
          /(x(i+1)-x(i-1))-sig*u(i-1))/p
  enddo
     if (ypn.gt..99e30) then
        qn=0.
        un=0.
     else
        qn=0.5
        un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
     endif
     y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
     do  k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
     enddo
end SUBROUTINE spline
!  (C) Copr. 1986-92 Numerical Recipes Software 
!---------------------------------------------------------------------------
SUBROUTINE splint(xa,ya,y2a,n,x,y)
  INTEGER n
  REAL x,y,xa(n),y2a(n),ya(n)
  INTEGER k,khi,klo
  REAL a,b,h
  klo=1
  khi=n
1 if (khi-klo.gt.1) then
     k=(khi+klo)/2
     if(xa(k).gt.x)then
        khi=k
     else
        klo=k
     endif
     goto 1
  endif
  h=xa(khi)-xa(klo)
  if (h.eq.0.) stop 'bad xa input in splint'
  a=(xa(khi)-x)/h
  b=(x-xa(klo))/h
  y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
  
END SUBROUTINE splint
!  (C) Copr. 1986-92 Numerical Recipes Software 







