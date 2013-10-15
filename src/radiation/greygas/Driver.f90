subroutine driver(km,jm,im,              &
     beta, sigma, g, dt, Cpd, tau_inf, alpha,         &
     T, q, p, ps, solin, Ts,             &
     Tinc, Tdot, SrfRadFlx, swhr, swflx_out, lwhr, lwflx_out, up_out, down_out, tau_out)
!-----------------------------!
!  CliMT driver for grey gas  !
!  radiation scheme           !
!  J.L. Mitchell 10Mar05      !
!_____________________________!

!! Note: tau_inf here is the BROADBAND optical depth

! In
integer im,jm,km
!f2py intent(in,hide) im,jm,km
real(8),                     intent(in) :: sigma, g, dt, Cpd, tau_inf, alpha, beta
real(8),dimension(km,jm,im), intent(in) :: T, q, p
real(8),dimension(jm,im),    intent(in) :: Ts, ps, solin

! Out
real(8),dimension(km,jm,im), intent(out) :: Tinc,Tdot, lwflx_out, lwhr, up_out, down_out, tau_out
real(8),dimension(km,jm,im), intent(out) :: swflx_out, swhr
real(8),dimension(jm,im)   , intent(out) :: SrfRadFlx

! Local
real(8),dimension(0:km,jm,im) :: plev, tau, lwflx, up, down

! Input pressures are at midlevel, need p on level surfaces
plev(1:km-1,:,:) = ( p(1:km-1,:,:) + p(2:km,:,:) )/2.
plev(0     ,:,:) = 0.
plev(km    ,:,:) = ps

! Set profile of optical depth
do k=0,km
   tau(k,:,:) = tau_inf/beta*(1.- ( plev(k,:,:)/ps )**alpha )
enddo

! Loop over lat and lon
do i=1,im
   do j=1,jm
      call grey_gas(km,beta,sigma,T(:,j,i), tau(:,j,i), Ts(j,i),  &
                    lwflx(:,j,i), up(:,j,i), down(:,j,i))
   enddo
enddo

Tdot = g/Cpd*(lwflx(1:km,:,:)-lwflx(0:km-1,:,:))/        &
              (plev(1:km,:,:)-plev(0:km-1,:,:))/100.
lwflx_out = -lwflx(0:km-1,:,:) 
up_out    = -up(0:km-1,:,:)
down_out  = down(0:km-1,:,:) 
tau_out   = tau(0:km-1,:,:) * beta
! Surface radiative flux
SrfRadFlx = -lwflx(km,:,:) + solin

Tinc = Tdot*2.*dt
Tdot = Tdot*86400.
lwhr = Tdot

do k=1,km
swflx_out(k,:,:) = solin
enddo
swhr = 0.  

end subroutine driver

! - - - > midlevel
! ----- > surfaces

!- - - - - - - - - - - - kappa(0),T(0)                  TOP OF ATMOSPHERE
!----------------------- press(0),tau(0),up/down(0)
!- - - - - - - - - - - - kappa(1),T(1)
!----------------------- press(1)
!- - - - - - - - - - - -   ..
!-----------------------   ..
!- - - - - - - - - - - -   ..
!-----------------------   ..
!- - - - - - - - - - - - kappa(nlev)
!----------------------- press(nlev)
!- - - - - - - - - - - - kappa(nlev+1)                  SURFACE
