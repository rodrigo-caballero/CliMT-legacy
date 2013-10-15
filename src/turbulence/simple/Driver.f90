
subroutine driver(km, jm, im,                             &
     do_srf_mom_flx, do_srf_sen_flx, do_srf_lat_flx,      &
     r, Pr, Rd, Cpd, Lv, eps, nuv, Cd, g, u0, dt, lat, lev,  &
     ps_in, Ts_in, T_in, U_in, V_in, q_in,                &
     Tinc_out, Uinc_out, Vinc_out, qinc_out,              &
     Tdot_out, Udot_out, Vdot_out, qdot_out,              &
     srf_lat_flx_out, srf_sen_flx_out)

! In
integer, intent(in)                       :: km, jm, im, do_srf_mom_flx
integer, intent(in)                       :: do_srf_sen_flx, do_srf_lat_flx
!f2py intent(in,hide) km,jm,im
real(8), intent(in)                       :: r, Pr, Rd, Cpd, Lv, eps, nuv, Cd, g, u0, dt
real(8), intent(in),  dimension(jm)       :: lat
real(8), intent(in),  dimension(km)       :: lev
real(8), intent(in),  dimension(jm,im)    :: Ts_in, ps_in
real(8), intent(in),  dimension(km,jm,im) :: T_in, U_in, V_in, q_in

! Out
real(8), intent(out), dimension(km,jm,im) :: Tinc_out, Uinc_out, Vinc_out, qinc_out
real(8), intent(out), dimension(km,jm,im) :: Tdot_out, Udot_out, Vdot_out, qdot_out
real(8), intent(out), dimension(jm,im)    :: srf_lat_flx_out, srf_sen_flx_out

! Local 
real(8), dimension(jm,km) :: T, U, V, q, Tinc, Uinc, Vinc, qinc, theta
real(8), dimension(jm)    :: ps, Ts, srf_lat_flx, srf_sen_flx

! Reshape input fields to local order
T = transpose(T_in(:,:,1))
U = transpose(U_in(:,:,1))
V = transpose(V_in(:,:,1))
q = transpose(q_in(:,:,1))
ps = ps_in(:,1)
Ts = Ts_in(:,1)

call main(km, jm, do_srf_mom_flx, do_srf_sen_flx, do_srf_lat_flx, &
     r, Pr, Rd, Cpd, Lv, eps, nuv, Cd, g, u0, dt, lat, lev, &
     ps, Ts, T, U, V, q,            &
     Tinc, Uinc, Vinc, qinc, theta, srf_lat_flx, srf_sen_flx)

! Reshape for output
Tinc_out(:,:,1) = transpose(Tinc)
Uinc_out(:,:,1) = transpose(Uinc)
Vinc_out(:,:,1) = transpose(Vinc)
qinc_out(:,:,1) = transpose(qinc)
srf_lat_flx_out(:,1) = srf_lat_flx
srf_sen_flx_out(:,1) = srf_sen_flx

Tdot_out = Tinc_out/2./dt*86400.
Udot_out = Uinc_out/2./dt*86400.
Vdot_out = Vinc_out/2./dt*86400.
qdot_out = qinc_out/2./dt*86400.

end subroutine driver
