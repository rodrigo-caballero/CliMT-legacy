subroutine driver(km, jm, im, lapse, g, Rd, Cpd, afc, UpdateFreq, p, ps, T, theta, Tdot)

! CliMT Driver for Emanuel hard adjustment 
!  with tunable lapse rate
! J. L. Mitchell 6June2005

implicit none

! In
integer                                   :: km,jm,im, i,j
!f2py intent(in,hide) km,jm,im
real(8),                      intent(in)  :: lapse, g, Rd, Cpd, afc, UpdateFreq
real(8), dimension(jm,im),    intent(in)  :: ps
real(8), dimension(km,jm,im), intent(in)  :: p

! In/out
real(8), dimension(km,jm,im)              :: T
!f2py intent(in,out) T

! Out
real(8), dimension(km,jm,im), intent(out) :: theta, Tdot

! Local
real(8), dimension(km,jm,im) :: Told

Told = T

do i=1,im
do j=1,jm
 call main(km, p(:,j,i), ps(j,i), Cpd, Rd, lapse, g, T(:,j,i), theta(:,j,i))
enddo
enddo

Tdot = ( (T-Told)*(1.-2.*afc)/2./UpdateFreq ) * 86400.

end subroutine driver
