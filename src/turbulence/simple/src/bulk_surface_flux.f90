subroutine surface(km, jm, do_srf_mom_flx, do_srf_sen_flx, do_srf_lat_flx, &
     Rd, eps, Cd, u0, ps, Ts, p, T, U, V, q, &
     srf_q_flx, srf_T_flx, srf_u_flx, srf_v_flx)

! Surface fluxes using bulk parameterizations

implicit none

! In
integer, intent(in)                :: jm, km, do_srf_mom_flx
integer, intent(in)                :: do_srf_sen_flx, do_srf_lat_flx
real, intent(in)                   :: Rd, eps, Cd, u0
real, intent(in), dimension(jm)    :: ps, Ts
real, intent(in), dimension(jm,km) :: p, T, U, V, q

! Out
real, intent(out), dimension(jm)   :: srf_q_flx, srf_T_flx, srf_u_flx, srf_v_flx

! Local
integer             :: j, k
real                :: qs 
real, dimension(jm) :: qsat,rho
external qs

! Near-surface density
rho = p(:,km)/Rd/T(:,km)

! Sat. spec hum. at surface
do j=1,jm
   qsat(j) = qs( Ts(j), ps(j), eps )
enddo

! Fluxes (+ve upward)
!  -- evaporation can only happen upward!
srf_q_flx =  rho * Cd * u0 * max(qsat - q(:,km), 0.) * do_srf_lat_flx
srf_T_flx =  rho * Cd * u0 * ( Ts   - T(:,km) )      * do_srf_sen_flx
srf_u_flx = -rho * Cd * u0 * u(:,km) * do_srf_mom_flx
srf_v_flx = -rho * Cd * u0 * v(:,km) * do_srf_mom_flx

end subroutine surface
!-------------------------------------------------------------------------
real function es(t)

! Compute saturation partial pressure of water vapor es [mb]
! at temperature t [K].
! Use Bohren+Albrecht p. 198
     
real :: t, t0m1

t0m1=1./273.15
es = 6.11 * exp( 6808.*(t0m1-1./t)-5.09*log(t*t0m1) )

end function es
!-------------------------------------------------------------------------
real function qs(t,p,eps)

! Compute saturation mass mixing ratio of water vapor qs [g/kg]
! at temperature t [K] and pressure p [mb]
! use Bohren+Albrecht p. 186
    
real :: t,p,eps
external es

ess = es(t)
qs  = eps*ess/p * 1.e3

end function qs
