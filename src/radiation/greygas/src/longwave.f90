subroutine compute_longwave(nlev, beta, sigma, T, tau, Tsurf, up, down)

!----------------------------------!
!  Computes two-stream LW fluxes   !
!  using the Schwarzchild eqns.    !
!  J.L. Mitchell 10Mar05           !
!__________________________________!

integer nlev

! T field is defined at midlevel
real,dimension(nlev) :: T

! Flux up and down, lw flux, and tau are defined at level interfaces
real,dimension(0:nlev)   :: tau,up,down
real Tsurf,dtau,sigma,beta

! Upward stream -> d(up)/d(tau) = -up + sigma*T**4
up       = 0.
up(nlev) = sigma*Tsurf**4  ! Surface boundary condition
do k=nlev-1,0,-1
   dtau  = tau(k)-tau(k+1)
   up(k) = up(k+1)*exp(-dtau)                   &
         + sigma*T(k+1)**4.*(1. - exp(-dtau))
enddo

! Downward stream -> d(dn)/d(tau) = dn - sigma*T**4
down(0)    = 0.  ! TOA boundary condition
do k=1,nlev
   dtau    = tau(k) - tau(k-1)
   down(k) = down(k-1)*exp(dtau) - sigma*T(k)**4.*(exp(dtau) - 1.)
enddo

! semi grey
up   = beta*up + (1.-beta)*sigma*Tsurf**4
down = beta*down

end subroutine compute_longwave

! - - - > midlevel
! ----- > surface/interface

!- - - - - - - - - - - - kappa(0),T(0)                  tau big, p small
!----------------------- press(0),tau(0),up/down(0)
!- - - - - - - - - - - - kappa(1),T(1)
!----------------------- press(1)
!- - - - - - - - - - - -   ..
!-----------------------   ..                           
!- - - - - - - - - - - -   ..
!----------------------- press(nlev-1)
!- - - - - - - - - - - - kappa(nlev)
!----------------------- press(nlev)
!- - - - - - - - - - - - kappa(nlev+1)                  tau small, p big
