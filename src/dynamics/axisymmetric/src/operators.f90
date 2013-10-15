subroutine arakawa_jacobian(im, jm, dx, dy, a_bc, b_bc, Jacobian)

! Computes Jacobian J(a,b) in advective form
! See Arakawa (1966) J. Comp. Phys. vol 1, 119-143
! ** Note: Input fields must be "framed" with boundary conds.
!          Jacobian computed in internal points only.

implicit none

integer, intent(in)                           :: im, jm
real,    intent(in)                           :: dx, dy
real,    intent(in), dimension(0:im+1,0:jm+1) :: a_bc,b_bc

real, intent(out), dimension(im,jm) :: Jacobian

! Local variables:
integer :: i,j
real, dimension(im,jm) :: p1,p2,p3,p4,p5,p6,p7,p8
real, dimension(im,jm) :: q1,q2,q3,q4,q5,q6,q7,q8

!   6 --- 2 --- 5
!   |     |     |
!   |     |     |
!   3 --- 0 --- 1
!   |     |     |
!   |     |     |
!   7 --- 4 --- 8

i= 1 ; j= 0 ; p1 = a_bc(1+i:im+i,1+j:jm+j)
i= 0 ; j= 1 ; p2 = a_bc(1+i:im+i,1+j:jm+j)
i=-1 ; j= 0 ; p3 = a_bc(1+i:im+i,1+j:jm+j)
i= 0 ; j=-1 ; p4 = a_bc(1+i:im+i,1+j:jm+j)
i= 1 ; j= 1 ; p5 = a_bc(1+i:im+i,1+j:jm+j)
i=-1 ; j= 1 ; p6 = a_bc(1+i:im+i,1+j:jm+j)
i=-1 ; j=-1 ; p7 = a_bc(1+i:im+i,1+j:jm+j)
i= 1 ; j=-1 ; p8 = a_bc(1+i:im+i,1+j:jm+j)

i= 1 ; j= 0 ; q1 = b_bc(1+i:im+i,1+j:jm+j)
i= 0 ; j= 1 ; q2 = b_bc(1+i:im+i,1+j:jm+j)
i=-1 ; j= 0 ; q3 = b_bc(1+i:im+i,1+j:jm+j)
i= 0 ; j=-1 ; q4 = b_bc(1+i:im+i,1+j:jm+j)
i= 1 ; j= 1 ; q5 = b_bc(1+i:im+i,1+j:jm+j)
i=-1 ; j= 1 ; q6 = b_bc(1+i:im+i,1+j:jm+j)
i=-1 ; j=-1 ; q7 = b_bc(1+i:im+i,1+j:jm+j)
i= 1 ; j=-1 ; q8 = b_bc(1+i:im+i,1+j:jm+j)

Jacobian = (p1-p3)*(q2-q4) - (p2-p4)*(q1-q3)  +                & ! J++
           q2*(p5-p6) - q4*(p8-p7) - q1*(p5-p8) + q3*(p6-p7) + & ! Jx+
           p1*(q5-q8) - p3*(q6-q7) - p2*(q5-q6) + p4*(q8-q7)     ! J+x

Jacobian = Jacobian/12./dx/dy

end subroutine arakawa_jacobian
!------------------------------------------------------------------------
subroutine v2vcosphii(jm, km, cosphi, V, Vcosphii)

! 
implicit none

! In
integer, intent(in)                 :: jm,km
real, intent(in),  dimension(jm,km) :: cosphi,V

! Out
real, intent(out), dimension(0:jm,km) :: Vcosphii

! Local
integer :: j

! Interpolate Vcosphi onto cell boundaries 
Vcosphii=0.
do j=1,jm-1
  Vcosphii(j,:) = ( V(j,:)*cosphi(j,:) + V(j+1,:)*cosphi(j+1,:) )/2.
enddo

end subroutine v2vcosphii
!------------------------------------------------------------------------
subroutine vcosphii2wi(jm, km, a, dphi, dp, cosphi, Vcosphii, Wi)

! 
implicit none

! In
integer, intent(in)                  :: jm,km
real, intent(in)                     :: a,dphi,dp
real, intent(in), dimension(jm,km)   :: cosphi
real, intent(in), dimension(0:jm,km) :: Vcosphii

! Out
real, intent(out), dimension(jm,0:km) :: Wi

! Local
integer :: j,k
real :: rdy
real, dimension(jm,km)      :: Vcosphi_y

rdy=1./dphi/a

! Integrate continuity eqn from top down to get W
Vcosphi_y = ( Vcosphii(1:jm,:) - Vcosphii(0:jm-1,:) )/cosphi*rdy
Wi=0.
do k=1,km-1
  Wi(:,k) = Wi(:,k-1) - Vcosphi_y(:,k)*dp   
enddo

end subroutine vcosphii2wi
!------------------------------------------------------------------------
subroutine upwind(jm, km, a, dphi, dp, cosphi, Vcosphii, Wi, q, Vq_y, Wq_p)

! Compute upwind-differenced moisture flux divergence 

implicit none

! In
integer, intent(in)                      :: jm,km
real, intent(in)                         :: a,dphi,dp
real, intent(in),  dimension(0:jm,   km) :: Vcosphii
real, intent(in),  dimension(  jm, 0:km) :: Wi
real, intent(in),  dimension(  jm,   km) :: q, cosphi

! Out
real, intent(out), dimension(jm,km) :: Vq_y,Wq_p

! Local
integer :: j,k,iy,iz
real :: rdp, rdy
real, dimension(0:jm,  km)  :: Vq
real, dimension(  jm,0:km)  :: Wq

rdy=1./dphi/a
rdp=1./dp

! Horizontal flux divergence
Vq=0.
do k=1,km
 do j=1,jm-1
    iy = ( 1-nint(sign(1.,Vcosphii(j,k))) )/2 ! V >= 0 => iy=0; V < 0 => iy=1
    Vq(j,k) = Vcosphii(j,k)*q(j+iy,k)
 enddo
enddo
Vq_y = ( Vq(1:jm,:) - Vq(0:jm-1,:) )/cosphi*rdy

! Vertical flux divergence
Wq=0.
do j=1,jm
 do k=1,km-1
 iz = ( 1-nint(sign(1.,Wi(j,k))) )/2    ! W >= 0 => iz=0; W < 0 => iz=1
 Wq(j,k) = Wi(j,k)*q(j,k+iz)
 enddo
enddo
Wq_p = ( Wq(:,1:km) - Wq(:,0:km-1) )*rdp

end subroutine upwind
!------------------------------------------------------------------------
subroutine smolar(jm, km, a, dphi, dp, dt, cosphi, Vcosphii, Wi, q, qdot)

! Low-diffusivity upwind advection scheme 
! Ref: Smolarkiewicz, Mon Wea Rev (1983) p.479-486

implicit none

! In
integer, intent(in)                      :: jm,km
real, intent(in)                         :: a,dphi,dp,dt
real, intent(in),  dimension(0:jm,   km) :: Vcosphii
real, intent(in),  dimension(  jm, 0:km) :: Wi
real, intent(in),  dimension(  jm,   km) :: q, cosphi

! Out
real, intent(out), dimension(jm,km) :: qdot

! Local
integer :: j,k,iy,iz
real :: rdp, rdy,qmin,Sc,tdt
real, dimension(0:jm,  km) :: Vtilde
real, dimension(  jm,0:km) :: Wtilde
real, dimension(  jm,  km) :: qstar,vq_y,wq_p,q1

rdy = 1./dphi/a
rdp = 1./dp
tdt = 2.*dt
Sc  = 1.08

! Scheme works only for positive definite fields, 
! so add uniform background field to ensure q >= 0
q1 = q-minval(q)

! compute conventional upwind tendency
call upwind(jm, km, a, dphi, dp, cosphi, Vcosphii, Wi, q1, vq_y, wq_p)

! initial forecast
qstar = q1 - (vq_y + wq_p)*tdt

! compute "diffusive" velocities
Vtilde = 0.
do j=1,jm-1
   Vtilde(j,:) = Sc*( abs(Vcosphii(j,:))*a*dphi - tdt*Vcosphii(j,:)**2 )* &
           ( qstar(j+1,:)-qstar(j,:) )/( qstar(j+1,:)+qstar(j,:)+1.e-15 )*rdy
enddo
Wtilde = 0.
do k=1,km-1
   Wtilde(:,k) = Sc*( abs(Wi(:,k))*dp - tdt*Wi(:,k)**2 )* &
           ( qstar(:,k+1)-qstar(:,k) )/( qstar(:,k+1)+qstar(:,k)+1.e-15 )*rdp
enddo

! compute advection by diffusive velocities
call upwind(jm, km, a, dphi, dp, cosphi, Vtilde, Wtilde, qstar, vq_y, wq_p)

! add correction to tendency
qdot = (qstar - q1)/tdt - (vq_y + wq_p)

end subroutine smolar
