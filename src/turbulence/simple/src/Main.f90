subroutine main(km, jm, do_srf_mom_flx, do_srf_sen_flx,  do_srf_lat_flx, &
     r, Pr, Rd, Cpd, Lv, eps, nuv, Cd, g, u0, dt, lat, lev, &
     ps, Ts, T, U, V, q,            &
     Tinc, Uinc, Vinc, qinc, theta, srf_lat_flx, srf_sen_flx)

implicit none

! In 
integer                             :: km, jm, do_srf_mom_flx
integer                             :: do_srf_sen_flx, do_srf_lat_flx
real, intent(in)                    :: r, Pr, Rd, Cpd, Lv, eps, nuv, Cd, g, u0, dt
real, intent(in), dimension(jm)     :: lat
real, intent(in), dimension(km)     :: lev
real, intent(in), dimension(jm)     :: ps, Ts
real, intent(in), dimension(jm,km)  :: T, U, V, q

! Out
real, intent(out), dimension(jm,km) :: Tinc, Uinc, Vinc, qinc, theta
real, intent(out), dimension(jm)    :: srf_lat_flx, srf_sen_flx

! Local
integer :: k
real                     :: dp, dphi
real, dimension(jm,km-1) :: eta
real, dimension(jm,km)   :: p, Uinc_vert, Uinc_horiz, Vinc_vert, Vinc_horiz
real, dimension(jm)      :: srf_q_flx, srf_T_flx, srf_u_flx, srf_v_flx, cosphi, tanphi

call init(km, jm, Rd, Cpd, g, nuv, lat, lev, T, cosphi, tanphi, dphi, theta, p, dp, eta)

call surface(km, jm, do_srf_mom_flx, do_srf_sen_flx, do_srf_lat_flx, &
     Rd, eps, Cd, u0, ps, Ts, p, T, U, V, q, &
     srf_q_flx, srf_T_flx, srf_u_flx, srf_v_flx)

Uinc_vert=0.
Vinc_vert=0.
call implicit_vertical_diffusion(km, jm,          & 
     dt, dp, g, Pr, eta, p, theta, U, V, q, srf_q_flx, srf_T_flx, srf_u_flx, srf_v_flx,  &
     Tinc, Uinc_vert, Vinc_vert, qinc) 

Uinc_horiz=0.
call implicit_horizontal_diffusion(km, jm, &
      dt, dphi, r, nuv, cosphi, tanphi, U, &
      Uinc_horiz)

Vinc_horiz=0.
call implicit_horizontal_diffusion(km, jm, &
      dt, dphi, r, nuv, cosphi, tanphi, V, &
      Vinc_horiz)

Uinc        = Uinc_vert + Uinc_horiz
Vinc        = Vinc_vert + Vinc_horiz
Tinc        = Tinc*(p/lev(km)/100.)**(Rd/Cpd)
srf_lat_flx = -Lv*srf_q_flx*1.e-3
srf_sen_flx = -Cpd*srf_T_flx

end subroutine main
