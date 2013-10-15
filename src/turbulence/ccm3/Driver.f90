subroutine driver(km, jm, im, do_srf_mom_flx, do_srf_sen_flx, do_srf_lat_flx, &
     Cpd, g, Lv, Rd, Rv, Cpv, dt, p, ps, u, v, T, Ts, q, &
     Tinc, Uinc, Vinc, qinc, Tdot, Udot, Vdot, qdot, shflx, lhflx, taux, tauy)
implicit none

! In
integer, intent(in)                      :: km, jm, im, do_srf_mom_flx
integer, intent(in)                      :: do_srf_sen_flx, do_srf_lat_flx
!f2py intent(in,hide) km,jm,im
real(8), intent(in)                      :: Cpd, g, Lv, Rd, Rv, Cpv, dt
real(8), intent(in), dimension(jm,im)    :: ps, Ts
real(8), intent(in), dimension(km,jm,im) :: p, u, v, T, q

! Out
real(8), intent(out), dimension(jm,im)    :: shflx, lhflx, taux, tauy
real(8), intent(out), dimension(km,jm,im) :: Tinc, Uinc, Vinc, qinc
real(8), intent(out), dimension(km,jm,im) :: Tdot, Udot, Vdot, qdot

! Local
integer :: i,j
real(8), dimension(km,jm,im) :: theta

do i=1,im
   do j=1,jm
      call main(km, do_srf_mom_flx, do_srf_sen_flx, do_srf_lat_flx, &
           Cpd, g, Lv, Rd, Rv, Cpv, dt, &
           p(:,j,i), ps(j,i), u(:,j,i), v(:,j,i), T(:,j,i), Ts(j,i), q(:,j,i), &
           uinc(:,j,i), vinc(:,j,i), Tinc(:,j,i), qinc(:,j,i),                 &
           udot(:,j,i), vdot(:,j,i), Tdot(:,j,i), qdot(:,j,i), theta(:,j,i),   &
           shflx(j,i), lhflx(j,i), taux(j,i), tauy)
   enddo
enddo

Tdot = Tdot*86400.
Udot = Udot*86400.
Vdot = Vdot*86400.
qdot = qdot*86400.

end subroutine driver

!-------------------------------------------------------------
integer function get_nlev()
  
integer get_km
external get_km

get_nlev = get_km()

end function get_nlev
