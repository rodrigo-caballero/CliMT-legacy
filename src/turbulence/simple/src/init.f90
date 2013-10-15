subroutine init(km, jm, Rd, Cpd, g, nuv, lat, lev, T, cosphi, tanphi, dphi, theta, p, dp, eta)

implicit none

! In
integer                            :: km, jm
real, intent(in)                   :: Rd, Cpd, g, nuv
real, intent(in), dimension(jm)    :: lat
real, intent(in), dimension(km)    :: lev
real, intent(in), dimension(jm,km) :: T

! Out
real, intent(out)                    :: dp, dphi
real, intent(out),dimension(jm)      :: cosphi, tanphi
real, intent(out),dimension(jm,km-1) :: eta
real, intent(out),dimension(jm,km)   :: p,theta

! Local
integer :: j
real :: deg2rad
real, dimension(jm,km)  :: eta_mid

! Pressure field 
p=1.
do j=1,jm
   p(j,:) = lev*100. ! mb -> Pa
enddo
dp =  p(1,km)-p(1,km-1) 

! geometry
deg2rad = abs(acos(-1.))/180.
dphi   = (lat(2)-lat(1))*deg2rad
cosphi = cos( lat*deg2rad )
tanphi = sin( lat*deg2rad )/cosphi

! Dynamical viscosity
eta_mid = nuv*g*(p/Rd/T)**2                     ! viscosity on level midpoints
eta = ( eta_mid(:,2:km)+eta_mid(:,1:km-1) )/2. ! viscosity on level interfaces

!  Temp -> pot. temp.
theta = (lev(km)*100./p)**(Rd/Cpd) * T

end subroutine init
