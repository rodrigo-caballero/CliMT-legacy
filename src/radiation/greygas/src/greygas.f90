subroutine grey_gas(nlev, beta, sigma, T, tau, Tsurf, lwflx, up, down)

!-------------------------------------!
!  Computes heating rate in a column  !
!  using grey gas approximation.      !
!  tau(nu,p) = tau(p)                 !
!  J.L. Mitchell 10Mar05              !
!_____________________________________!

integer nlev,k
! Heating rate and T are defined at midlevel
real, dimension(nlev)     :: T
! Tau and Up/Down LW fluxes are defined at level interfaces
real,dimension(0:nlev)   :: tau,lwflx,up,down
real Tsurf,sigma,beta

! Compute LW flux
call compute_longwave(nlev, beta, sigma, T, tau, Tsurf, up, down)

! Net LW flux at interfaces
lwflx = up - down

end subroutine grey_gas

! - - - > midlevel
! ----- > surface/interface

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
