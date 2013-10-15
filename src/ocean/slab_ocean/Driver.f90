subroutine driver(jm, im, srf_rad_flx, srf_lat_flx, srf_sen_flx, qflx, Hslab, rowl, Cl, dt, &
                             Tsinc, Tsdot )

implicit none

! Input
integer, intent(in)                   :: jm,im
real(8), intent(in), dimension(jm,im) :: srf_rad_flx, srf_lat_flx, srf_sen_flx, qflx
real(8), intent(in)                   :: Hslab, rowl, Cl, dt
!f2py intent(in,hide)  jm,im

! Output
real(8), intent(out), dimension(jm,im) :: Tsinc, Tsdot

! Local
real(8) :: invCslab

invCslab = 1. / (Hslab*rowl*Cl)
Tsdot = (srf_rad_flx + srf_lat_flx + srf_sen_flx + qflx) * invCslab
Tsinc = 2.*dt*Tsdot

end subroutine driver
