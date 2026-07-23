subroutine main(km, jm, do_srf_mom_flx, do_srf_sen_flx,  do_srf_lat_flx, &
     r, Pr, Rd, Cpd, Lv, eps, nuv, Cd, g, u0, dt, lat, &
     p, dp, ps, Ts, T, U, V, q,            &
     Tinc, Uinc, Vinc, qinc, theta, srf_lat_flx, srf_sen_flx, taux, tauy)

implicit none

! In 
integer                             :: km, jm, do_srf_mom_flx
integer                             :: do_srf_sen_flx, do_srf_lat_flx
real, intent(in)                    :: r, Pr, Rd, Cpd, Lv, eps, Cd, g, u0, dt
real, intent(in), dimension(jm)     :: lat
real, intent(in), dimension(jm)     :: Ts
real, intent(in), dimension(jm,km)  :: T, U, V, q, nuv

real, intent(inout), dimension(km)  :: p, dp
real, intent(inout)                 :: ps

! Out
real, intent(out), dimension(jm,km) :: Tinc, Uinc, Vinc, qinc, theta
real, intent(out), dimension(jm)    :: srf_lat_flx, srf_sen_flx, taux, tauy

! Local
integer :: j,k
real                     :: dphi, nuv0, deg2rad
real, dimension(jm,km)   :: Uinc_vert, Uinc_horiz, Vinc_vert, Vinc_horiz
real, dimension(jm)      :: srf_q_flx, srf_T_flx, srf_u_flx, srf_v_flx, cosphi, tanphi

!call init(km, jm, Rd, Cpd, g, lat, T, cosphi, tanphi, dphi, theta, p, dp, ps)

! initialize local fields
! Pressure field
p = p*100. ! mb -> Pa
ps = ps*100.
if (dp(1) == -99.) then
   dp = p*0. + p(2)-p(1)
else
   dp = dp*100.
endif
! geometry
deg2rad = abs(acos(-1.))/180.
dphi   = (lat(2)-lat(1))*deg2rad
cosphi = cos( lat*deg2rad )
tanphi = sin( lat*deg2rad )/cosphi
!  Pot. temp.
do j=1,jm
   theta(j,:) = (ps/p)**(Rd/Cpd) * T(j,:)
enddo

Tinc = 0.
Uinc_vert = 0.
Vinc_vert = 0.
Uinc_horiz = 0.
Vinc_horiz = 0.

call surface(km, jm, do_srf_mom_flx, do_srf_sen_flx, do_srf_lat_flx, &
     Rd, eps, Cd, u0, ps, Ts, p, T, U, V, q, &
     srf_q_flx, srf_T_flx, srf_u_flx, srf_v_flx)

if (sum(dp(2:km)-dp(1:km-1)) < 1.e-9) then ! grid is uniform
   call implicit_vertical_diffusion(km, jm,          & 
     dt, dp, g, Rd, Pr, nuv, p, theta, U, V, T, q, srf_q_flx, srf_T_flx, srf_u_flx, srf_v_flx,  &
     Tinc, Uinc_vert, Vinc_vert, qinc) 
else
   call implicit_vertical_diffusion_nonuniform_grid(km, jm,          & 
     dt, dp, g, Rd, Pr, nuv, p, theta, U, V, T, q, srf_q_flx, srf_T_flx, srf_u_flx, srf_v_flx,  &
     Tinc, Uinc_vert, Vinc_vert, qinc)
endif

!nuv0 = minval(nuv) ! use minimum value of nuv for horizontal diff
!call implicit_horizontal_diffusion(km, jm, &
!      dt, dphi, r, nuv0, cosphi, tanphi, U, &
!      Uinc_horiz)

!call implicit_horizontal_diffusion(km, jm, &
!      dt, dphi, r, nuv0, cosphi, tanphi, V, &
!      Vinc_horiz)

! tendencies
Uinc = Uinc_vert !+ Uinc_horiz
Vinc = Vinc_vert !+ Vinc_horiz
do j=1,jm
   Tinc(j,:) = Tinc(j,:)*(p/ps)**(Rd/Cpd)
enddo
srf_lat_flx = -Lv*srf_q_flx*1.e-3
srf_sen_flx = -Cpd*srf_T_flx
taux = -srf_u_flx
tauy = -srf_v_flx

end subroutine main
