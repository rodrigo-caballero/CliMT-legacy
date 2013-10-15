subroutine tend(im,km,Rd,Rv,Cpd,Cpv,g,p,u,t,q,z0,udot,tdot,qdot,z0dot,w)   

! Compute tendencies of u, T, q

implicit none

! In 
integer :: im,km,k
real :: Rd,Rv,Cpd,Cpv,g
real, dimension(im,km) :: p,u,t,q
real, dimension(im) :: z0

! Out
real, dimension(im) :: z0dot
real, dimension(im,km) :: udot,tdot,qdot
real, dimension(im,km+1) :: w

! Local
integer :: ks
real :: dx,dp,e,timescale
real, dimension(im,km) :: z,alpha,udot_pgt,tdot_adiab,ut_x,wt_p,uq_x,wq_p,Cpeff,theta

! Column width (m)
dx = 1000.e3
dp = p(1,2)-p(1,1)

! Compute some diagnostic fields
alpha = Rd*t/p*(1.-q+Rv*q/Rd)
call get_z(im,km,dp,g,p,alpha,z0,z)   ! geopotential height
call get_w(im,km,dx,dp,u,w)      ! omega

!---------------- 1) Momentum equation
udot_pgt(1,:) = -g*( z(2,:)-z(1,:) )/dx ! Pressure gradient term
udot_pgt(2,:) = udot_pgt(1,:)
udot = udot_pgt         ! Total tendency

! Sponge
!ks=sum(minloc(p/100.-200.),1)-1
!do k=1,ks
!e=float(k-1)/ks
!udot(:,k) = udot(:,k)  -0.5e-3 * (1.-e)**2*u(:,k) 
!enddo

! Rayleigh friction
timescale=10.
!udot = udot -u/(timescale*86400.)

!----------------- 2) Heat equation
Cpeff = Cpd*(1.-q)+Cpv*q      ! moisture-adjusted heat capacity of air
call upwind(im,km,dx,dp,u,w,t,ut_x,wt_p)               ! Advection
tdot_adiab  = (w(:,1:km)+w(:,2:km+1))/2. * alpha/Cpeff    ! Adiabatic heating
tdot = -(ut_x + wt_p) + tdot_adiab 

!----------------- 3) Moisture equation
call upwind(im,km,dx,dp,u,w,q,uq_x,wq_p)          ! Advection
qdot = -(uq_x + wq_p) 

! surface height
z0dot = sum(alpha(:,km))/2./g*w(:,km+1)

end subroutine tend
!========================================================================
subroutine get_z(im,km,dp,g,p,alpha,z0,z)

! Compute geopotential height 

implicit none
integer :: im,km,k
real :: g,dp,z0(im)
real, dimension(im,km) :: p,alpha,z

! Integrate upwards from surface
z(:,km) = z0 + alpha(:,km)/g*dp/2.
do k=km-1,1,-1
z(:,k) = z(:,k+1) + (alpha(:,k)+alpha(:,k+1))/2./g*dp
enddo

end subroutine get_z
!========================================================================
subroutine get_w(im,km,dx,dp,u,w)

! Solve continuity equation for w
! Set w=0 at top and integrate downwards
!
implicit none
integer :: im,km,k,ix
real :: dx,dp,u(im,km),w(im,km+1)

w=0. ! top boundary condition

! interior
do k=2,km+1
w(1,k) = w(1,k-1) - dp * u(1,k-1)/ dx 
enddo

w(2,:) = -w(1,:) 

end subroutine get_w
!========================================================================
subroutine upwind(im,km,dx,dp,u,w,f,uf_x,wf_p)

! compute upwind-differenced flux convergences 
! of field f (defined on T grid) 

implicit none
integer :: im,km,k,i,iz,ix
real :: dx,dp
real, dimension(im,km) :: u,f,uf,uf_x,wf_p
real, dimension(im,km+1) :: w,wf

uf=0.
wf=0.

! Horizontal
do k=1,km
ix=( 1-nint(sign(1.,u(1,k))) )/2 ! u>=0 => ix=0; u<0 => ix=1
uf(1,k)=u(1,k)*f(1+ix,k)
enddo
uf_x(1,:) =  uf(1,:) / dx
uf_x(2,:) = -uf(1,:) / dx

! Vertical
do i=1,im
 do k=2,km
 iz=( 1-nint(sign(1.,w(i,k))) )/2 - 1 ! w>=0 => iz=-1; w<0 => iz=0
 wf(i,k)=w(i,k)*f(i,k+iz)
 enddo
 ! -- bottom
 wf(i,km+1) = w(i,km+1)*f(i,km)
enddo
wf_p(:,:) = ( wf(:,2:km+1)-wf(:,1:km) ) / dp

end subroutine upwind
