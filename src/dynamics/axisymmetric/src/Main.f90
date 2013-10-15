subroutine main(km, jm,             &
                Rd, Cpd, a, omega,  &
                delh, delv, Newt,   &
                dt, lat, lev,    &
                T, U, V, q, Vold,   &
                Tinc, Uinc, Vinc, qinc, psi, theta, theta_e, p, W, &
                Tdot, Udot, Vdot, qdot)

implicit none

! In 
integer                  :: km,jm
real                     :: Rd, Cpd, a, omega
real                     :: dt, delh, delv, Newt
real,  dimension(jm)     :: lat
real,  dimension(km)     :: lev
real,  dimension(jm,km)  :: T, U, V, q, Vold

! Out
real, intent(out), dimension(jm,km) :: psi, theta, theta_e, p, W
real, intent(out), dimension(jm,km) :: Tinc,  Uinc,  Vinc,  qinc
real, intent(out), dimension(jm,km) :: Tdot,  Udot,  Vdot,  qdot

! Local
integer :: j,k
real :: pi, phi, dphi, dp, kappa, ps
real, dimension(jm,km)  :: sinphi, cosphi, tanphi
real, dimension(jm)     :: Vm

!shapiro
!complex, allocatable, dimension(:) :: dat, smo, old, cof
complex, dimension(jm) :: dat, smo
complex, dimension(8) :: old, cof
integer, save :: count

! Scalar parameters
pi    = abs(acos(-1.))
dphi  = (lat(2)-lat(1))/180.*pi
dp    = (lev(2)-lev(1)) * 100. ! mb -> Pa
kappa = Rd/Cpd

! Initialise pressure field
do j=1,jm
   p(j,:) = lev * 100. ! mb -> Pa
enddo
ps = lev(km)*100. + dp/2.

! Initialise trig fields
do j=1,jm
   phi = lat(j)/180.*pi
   sinphi(j,:)  = sin(phi) !max(abs(sin(phi)), 1.e-8)*sign(1.,phi)
   cosphi(j,:)  = cos(phi)
   tanphi(j,:)  = sinphi(j,:)/cosphi(j,:)
enddo

! -- Initialise radiative equilibrium pot. temperature structure (at midlayer)
!    following Held and Suarez (1994)
do k=1,km
   theta_e(:,k) = max( 200./(p(:,k)/ps)**kappa,       &
                  315. - delh*sinphi(:,k)**2               &
                  - delv*log10(p(:,k)/ps)*cosphi(:,k)**2      )
enddo


! Apply Shapiro filter every 10 time steps
if (count > 10 .and. jm > 8) then
   do k=1,km
      dat = V(:,k)
      ! 8th order, 2 passes, zero boundary conds
      call shpiro(smo,dat,cof,old,jm,8,2,0,1)
      V(:,k) = real(smo)
      dat = T(:,k)
      ! 8th order, 2 passes, zero boundary conds
      call shpiro(smo,dat,cof,old,jm,8,2,0,1)
      T(:,k) = real(smo)
   enddo
   count = 0
endif
count=count+1
 
! Get tendencies
call tendency(km, jm, a, Rd, kappa, omega, Newt, dphi, dp, dt, ps, p, T, U, V, q, &
     sinphi, cosphi, tanphi, theta_e, Tdot, Udot, Vdot, qdot, psi, theta, W)

! Compute increments
Tinc = Tdot *2.*dt 
Uinc = Udot *2.*dt 
Vinc = Vdot *2.*dt
qinc = qdot *2.*dt 

! Impose non-divergent V (i.e. rigid boundary conditions)
! Do not need to solve an elliptic equation because it's a 1D problem here
! See http://mitgcm.org/pelican/online_documents/node32.html#eq:discrete-time-u
Vm = sum(Vold + Vinc, dim=2)/km
do k = 1,km
 Vinc(:,k) = Vinc(:,k) - Vm 
enddo
Vdot = Vinc/2./dt

! Convert tendencies to K/day (for output only)
Tdot = Tdot * 86400.
Udot = Udot * 86400.
Vdot = Vdot * 86400.
qdot = qdot * 86400.

end subroutine main
