!---------------------------------------------------
!   Applies boundary conds to prognostic variables
!              Jonathan L. Mitchell
!             Rodrigo Caballero Augi
!---------------------------------------------------

subroutine bc(jm, km, theta, U, V, q, psi, &
             theta_bc, U_bc, V_bc, q_bc, psi_bc)

implicit none

! In
integer :: jm, km

! In
real,dimension(jm,km) :: theta, U, V, q, psi

! Out
real,dimension(0:jm+1,0:km+1) :: theta_bc, U_bc, V_bc, q_bc, psi_bc

! Boundary condition for theta
theta_bc(1:jm,1:km) = theta
theta_bc(   0,   :) = theta_bc( 2, :)
theta_bc(jm+1,   :) = theta_bc(jm-1, :)
theta_bc(   :,   0) = theta_bc( :, 1)
theta_bc(   :,km+1) = theta_bc( :,km)

! Boundary condition for q
q_bc(1:jm,1:km) = q
q_bc(   0,   :) = q_bc( 2, :)
q_bc(jm+1,   :) = q_bc(jm, :)
q_bc(   :,   0) = q_bc( :, 1)
q_bc(   :,km+1) = q_bc( :,km)

! Boundary condition for U
U_bc(  1:jm,1:  km) = U
U_bc(   0,       :) = U_bc( 1,   :)
U_bc(jm+1,       :) = U_bc(jm, :)
U_bc(   :,    km+1) = U_bc( :,  km) 
U_bc(   :,       0) = U_bc( :,   1)                      

! Boundary condition for V
V_bc(  1:jm,1:  km) = V
V_bc(   0,       :) = -V_bc( 1,   :)
V_bc(jm+1,       :) = -V_bc(jm, :)
V_bc(   :,    km+1) = -V_bc( :,  km) 
V_bc(   :,       0) = -V_bc( :,   1)                      

! Antisymmetric boundary conds (ensures psi=0. on boundary)
psi_bc(1:jm,1:km) = psi
psi_bc(0,   :) = 0.!-psi_bc(1, :)
psi_bc(jm+1,:) = 0.!-psi_bc(jm,:)
psi_bc(:,   0) = 0.!-psi_bc(:, 1)
psi_bc(:,km+1) = 0.!-psi_bc(:,km)


end subroutine bc
