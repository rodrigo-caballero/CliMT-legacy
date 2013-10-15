subroutine implicit_vertical_diffusion(km, jm,          & 
     dt, dp, g, Pr, eta, p, theta, U, V, q, srf_q_flx, srf_T_flx, srf_u_flx, srf_v_flx,  &
     Tinc, Uinc, Vinc, qinc)

implicit none

! In
integer, intent(in)                  :: km, jm
real, intent(in)                     :: dt, dp, g, Pr
real,             dimension(jm,km-1) :: eta
real, intent(in), dimension(jm,km)   :: p, theta, U, V, q
real, intent(in), dimension(jm)      :: srf_q_flx, srf_T_flx, srf_u_flx, srf_v_flx

! Out
real, intent(out), dimension(jm,km) :: Tinc, Uinc, Vinc, qinc

! Local
integer             :: j         
real                :: gamma     
real, dimension(km) :: sup,sub,dia,rhs,ans

integer :: k
real                :: smooth
real, dimension(jm) :: ysup,ysub,ydia,yrhs,yans


! Initialise
sup=0.
sub=0.
dia=0.
gamma = 2.*dt/dp/dp*g

! Momentum
do j=1,jm
   sup(2:km)   = -gamma*eta(j,:)
   sub(1:km-1) = -gamma*eta(j,:)
   dia(2:km-1) = 1. + gamma*(eta(j,1:km-2)+eta(j,2:km-1))
   dia(1)      = 1. + gamma*eta(j,1)     ! no flux through top
   dia(km)     = 1. + gamma*eta(j,km-1)  
   ! -- U
   rhs = U(j,:)
   rhs(km) = U(j,km) + srf_u_flx(j)*gamma*dp
   call trid(sup,dia,sub,rhs,ans,km)
   Uinc(j,:) = ans - U(j,:)
   ! -- V
   rhs = V(j,:) 
   rhs(km) = V(j,km) + srf_v_flx(j)*gamma*dp
   call trid(sup,dia,sub,rhs,ans,km)
   Vinc(j,:) = ans - V(j,:) 
enddo

! Heat 
eta = eta/Pr
do j=1,jm
   sup(2:km)   = -gamma*eta(j,:)
   sub(1:km-1) = -gamma*eta(j,:)
   dia(2:km-1) = 1. + gamma*(eta(j,1:km-2)+eta(j,2:km-1))
   dia(1)      = 1. + gamma*eta(j,1)     ! no flux through top
   dia(km)     = 1. + gamma*eta(j,km-1)  
   ! -- theta
   rhs=theta(j,:)
   rhs(km) = theta(j,km) + srf_T_flx(j)*gamma*dp
   call trid(sup,dia,sub,rhs,ans,km)
   Tinc(j,:) = ans - theta(j,:)
enddo

! Moisture
eta = eta/Pr
do j=1,jm
   sup(2:km)   = -gamma*eta(j,:)
   sub(1:km-1) = -gamma*eta(j,:)
   dia(2:km-1) = 1. + gamma*(eta(j,1:km-2)+eta(j,2:km-1))
   dia(1)      = 1. + gamma*eta(j,1)     ! no flux through top
   dia(km)     = 1. + gamma*eta(j,km-1)  
   ! -- q
   rhs=q(j,:) 
   rhs(km) = q(j,km) + srf_q_flx(j)*gamma*dp
   call trid(sup,dia,sub,rhs,ans,km)
   qinc(j,:) = ans - q(j,:)
enddo

! -- Jonathan's strong horizontal diffusion --
!+++rca ! Apply second-derivative smoother to damp 2*deltaY mode
!+++rca ! Smoothing coefficient, units of s-1
!+++rca smooth = 0.25
!+++rca ysup = -smooth
!+++rca ydia = 1.+2.*smooth
!+++rca ysub = -smooth
!+++rca do k=1,km
!+++rca    ! Zero flux at the boundaries
!+++rca    ydia(1) = 1.+3.*smooth
!+++rca    ydia(jm) = 1.+3.*smooth
!+++rca    yrhs = V(:,k)
!+++rca    call trid(ysup,ydia,ysub,yrhs,yans,jm)
!+++rca    Vinc(:,k) = Vinc(:,k) + (yans - V(:,k))
!+++rca enddo

end subroutine implicit_vertical_diffusion
!------------------------------------------------------------------------------
subroutine implicit_horizontal_diffusion(km, jm, dt, dphi, r, nuv, cosphi, tanphi, U, Uinc)

implicit none

! In
integer, intent(in)                  :: km, jm
real, intent(in)                     :: dt, dphi, nuv, r
real, intent(in), dimension(jm)      :: cosphi, tanphi
real, intent(in), dimension(jm,km)   :: U

! Out
real, intent(out), dimension(jm,km) :: Uinc

! Local
integer             :: j,k
real                :: gamma
real, dimension(jm) :: sup,sub,dia,rhs,ans

! Initialise
sup=0.
sub=0.
dia=0.
gamma = 2.*dt/r/r*nuv  

! Momentum
do k=1,km
   sup  =  gamma*tanphi/2./dphi - gamma/dphi/dphi
   sub  = -gamma*tanphi/2./dphi - gamma/dphi/dphi
   dia  = 1. + gamma/cosphi/cosphi + 2.*gamma/dphi/dphi
   rhs  = U(:,k)
   call trid(sup,dia,sub,rhs,ans,jm)
   Uinc(:,k) = ans - U(:,k)
enddo

end subroutine implicit_horizontal_diffusion
!------------------------------------------------------------------------------
subroutine trid(a,b,c,r,u,n)

real, dimension(n) :: a,b,c,r,u,gam

bet=b(1)
u(1)=r(1)/bet

do j=2,n
  gam(j)=c(j-1)/bet
   bet=b(j)-a(j)*gam(j)
   u(j)=(r(j)-a(j)*u(j-1))/bet
enddo

do j=n-1,1,-1
u(j)=u(j)-gam(j+1)*u(j+1)
enddo

end subroutine trid
