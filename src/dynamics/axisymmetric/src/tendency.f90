subroutine tendency(km, jm, a, Rd, kappa, omega, Newt, dphi, dp, dt, ps, p, T, U, V, q, &
     sinphi, cosphi, tanphi, theta_e, Tdot, Udot, Vdot, qdot, psi, theta, W)

! Prognostic variables are:
!
! theta pot temp
! U zonal wind
! V meridional wind
! q tracer (humidity)

implicit none

! In
integer :: km,jm
real :: a, Rd, kappa, omega, Newt, dphi, dp, dt, ps
real, dimension(jm,km) :: p,T,U,V,q,sinphi,cosphi,tanphi,theta_e

! Out
real, dimension(jm,km) :: Udot,Tdot,Vdot,qdot,psi,theta,W

! Local
integer :: k,j
real :: sponge_coeff,pi
real, dimension(0:jm+1,0:km+1) :: theta_bc, U_bc, V_bc, q_bc, psi_bc
real, dimension(jm,km) :: Jacobian,vq_y,wq_p,zy
real, dimension(0:jm,  km) :: Vcosphii
real, dimension(  jm,0:km) :: Wi

! Temp -> pot. temp.
theta = (ps/p)**kappa * T

! V and W on cell boundaries (needed for upwind advection)
call v2vcosphii(jm, km, cosphi, V, Vcosphii)
call vcosphii2wi(jm, km, a, dphi, dp, cosphi, Vcosphii, Wi)
W = ( Wi(:,0:km-1)+Wi(:,1:km) )/2.

! Variables with bound conds and streamfunction (needed for Jacobian)
call v2psi(jm, km, dp, cosphi, V, psi)
call bc(jm, km, theta, U, V, q, psi, theta_bc, U_bc, V_bc, q_bc, psi_bc)

! U tendency
!call upwind(jm, km, a, dphi, dp, cosphi, Vcosphii, Wi, U, vq_y, wq_p)
!Udot =   - (vq_y + wq_p) + U*V*tanphi/a  + 2.*omega*sinphi*V
!call arakawa_jacobian(jm, km, dphi, dp, psi_bc, U_bc, Jacobian)
!Udot =   -Jacobian/a/cosphi + U*V*tanphi/a  + 2.*omega*sinphi*V
call smolar(jm, km, a, dphi, dp, dt, cosphi, Vcosphii, Wi, U, Udot)
Udot =   Udot + U*V*tanphi/a  + 2.*omega*sinphi*V

! V tendency 
! Note -- The smolar routine does not work for V, because the 
! the constant added to the input field introduces north-south asymmetry
call presgrad(jm, km, a, Rd, dphi, dp, p, T, zy)
call upwind(jm, km, a, dphi, dp, cosphi, Vcosphii, Wi, V, vq_y, wq_p)
Vdot =  -(vq_y + wq_p) - U*U*tanphi/a - 2.*omega*sinphi*U - zy
!call arakawa_jacobian(jm, km, dphi, dp, psi_bc, V_bc, Jacobian)
!Vdot =  -Jacobian/cosphi/a - U*U*tanphi/a - 2.*omega*sinphi*U - zy
!call smolar(jm, km, a, dphi, dp, dt, cosphi, Vcosphii, Wi, V, Vdot)
!Vdot =  Vdot - U*U*tanphi/a - 2.*omega*sinphi*U - zy
! -- sponge layer
sponge_coeff = 0.0001 ! damping coeff
Vdot(:,1) = Vdot(:,1) - sponge_coeff*V(:,1)

! T tendency
!call arakawa_jacobian(jm, km, dphi, dp, psi_bc, theta_bc, Jacobian)
!Tdot = -Jacobian/cosphi/a  + 1./Newt/86400.*( theta_e - theta )
!call upwind(jm, km, a, dphi, dp, cosphi, Vcosphii, Wi, theta, vq_y, wq_p)
!Tdot = -(vq_y + wq_p)  + 1./Newt/86400.*( theta_e - theta )
call smolar(jm, km, a, dphi, dp, dt, cosphi, Vcosphii, Wi, theta, Tdot)
Tdot = Tdot +  1./Newt/86400.*( theta_e - theta )
Tdot = Tdot * (p/ps)**kappa ! theta -> T

! q tendency
!call upwind(jm, km, a, dphi, dp, cosphi, Vcosphii, Wi, q, vq_y, wq_p)
!qdot = - (vq_y + wq_p)
call smolar(jm, km, a, dphi, dp, dt, cosphi, Vcosphii, Wi, q, qdot)

end subroutine tendency
!--------------------------------------------------------------------
subroutine v2psi(jm, km, dp, cosphi, V, psi)

implicit none

! In
integer :: jm,km
real :: dp
real, intent(in), dimension(jm,km) :: V, cosphi

! Out
real, intent(out),  dimension(jm,km) :: psi

! Local
integer :: k
real, dimension(jm,0:km) :: psi1,psi2,psim

! Integrate top-down
psi1=0.
do k=1,km-1
   psi1(:,k) = psi1(:,k-1) - V(:,k)*cosphi(:,k)*dp
enddo

! Integrate bottom-up
psi2=0.
do k=km,1,-1
!!!!do k=km,2,-1
   psi2(:,k-1) = psi2(:,k) + V(:,k)*cosphi(:,k)*dp
enddo

! Average and interpolate  onto level midpoints
psi2 = ( psi1 + psi2 )/2.
psi  = ( psi2(:,0:km-1) + psi2(:,1:km) )/2.

end subroutine v2psi
!-------------------------------------------------------------------------------
subroutine presgrad(jm, km, a, Rd, dphi, dp, p, T, zy)

! Compute nondimensional pressure gradient term of V momentum equation

implicit none

! In
integer :: jm,km
real :: dphi,dp,a,Rd
real, intent(in), dimension(jm,km) :: p,T

! Out
real, intent(out),  dimension(jm,km) :: zy

! Local
integer :: k
real, dimension(jm,km) :: alpha,z
real, dimension(  jm,0:km) :: zi
real, dimension(0:jm,  km) :: zyi

! Specific volume
alpha = Rd*T/p

! Integrate to get z on level interfaces
zi(:,km) = 0. 
do k=km-1,0,-1
   zi(:,k) = zi(:,k+1) + alpha(:,k+1)*dp
enddo

! Interpolate z onto level midpoints
z = ( zi(:,0:km-1) + zi(:,1:km) )/2.

! Meridional gradient
zyi(1:jm-1,:) = ( z(2:jm,:) - z(1:jm-1, :) )/dphi/a
zyi(     0,:) = zyi(1,:)
zyi(    jm,:) = zyi(jm-1,:)
zy = ( zyi(0:jm-1,:) + zyi(1:jm,:) )/2.

end subroutine presgrad
