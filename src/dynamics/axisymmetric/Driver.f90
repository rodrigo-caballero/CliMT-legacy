subroutine driver(km, jm, im,                                 &
     Rd, Cpd, a, omega, delh, delv, Newt, dt, lat, lev,       &
     T_, U_, V_, q_, Vold_,                                   &
     Tinc_, Uinc_, Vinc_, qinc_, psi_, theta_, theta_e_, W_,  &
     Tdot_, Udot_, Vdot_, qdot_)

implicit none

! In 
integer, intent(in)                       :: km,jm,im
!f2py intent(in,hide) km,jm,im
real(8), intent(in)                       :: Rd, Cpd, a, omega, delh
real(8), intent(in)                       :: delv, Newt, dt
real(8), intent(in),  dimension(jm)       :: lat
real(8), intent(in),  dimension(km)       :: lev
real(8), intent(in),  dimension(km,jm,im) :: T_, U_, q_, Vold_

real(8),  dimension(km,jm,im) :: V_
!f2py intent(in,out) V_

! Out
real(8), intent(out), dimension(km,jm,im) :: Tinc_, Uinc_, Vinc_, qinc_
real(8), intent(out), dimension(km,jm,im) :: psi_, theta_, theta_e_,  W_
real(8), intent(out), dimension(km,jm,im) :: Tdot_, Udot_, Vdot_, qdot_

! Local 
real(8), dimension(jm,km) :: T, U, V, q, Vold, psi, theta
real(8), dimension(jm,km) :: Tinc, Uinc, Vinc, qinc, theta_e, p, W
real(8), dimension(jm,km) :: Tdot, Udot, Vdot, qdot

! Reshape input field (legacy code uses (j,k) instead of (k,j))
T = transpose(T_(:,:,1))
U = transpose(U_(:,:,1))
V = transpose(V_(:,:,1))
q = transpose(q_(:,:,1))
Vold = transpose(Vold_(:,:,1))

call main(km, jm,                    &
          Rd, Cpd, a, omega,         &
          delh, delv, Newt,          &
          dt, lat, lev,  &
          T, U, V, q, Vold,          &
          Tinc, Uinc, Vinc, qinc, psi, theta, theta_e, p, W, &
          Tdot, Udot, Vdot, qdot)


! Reshape for output
Tinc_(:,:,1)    = transpose(Tinc)
Uinc_(:,:,1)    = transpose(Uinc)
Vinc_(:,:,1)    = transpose(Vinc)
qinc_(:,:,1)    = transpose(qinc)
Tdot_(:,:,1)    = transpose(Tdot)
Udot_(:,:,1)    = transpose(Udot)
Vdot_(:,:,1)    = transpose(Vdot)
qdot_(:,:,1)    = transpose(qdot)
psi_(:,:,1)     = transpose(psi)
theta_(:,:,1)   = transpose(theta)
theta_e_(:,:,1) = transpose(theta_e)
W_(:,:,1)       = transpose(W)

end subroutine driver
