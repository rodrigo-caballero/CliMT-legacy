subroutine driver(km, jm, im, &
     Lv, Cpd, g, Rd, Rv, Cpv, rowl, &
     dt, T, q, ps, p, &
     tau_bm, es0, T00, rhbm, Mv, Ma, &
     Tinc, qinc, precc, TdotConv, qdotConv, theta )

implicit none

integer km,jm,im
!f2py,intent(in,hide) km,jm,im
real(8),intent(in) :: Lv, Cpd, g, Rd, Rv, Cpv, rowl, dt
real(8),intent(in),dimension(km,jm,im) :: T,q,p
real(8),intent(in),dimension(   jm,im) :: ps
real(8),intent(in) :: tau_bm, es0, T00, rhbm, Mv, Ma
real(8),intent(out),dimension(   jm,im) :: precc
real(8),intent(out),dimension(km,jm,im) :: Tinc, qinc, TdotConv, qdotConv,theta

! Local
integer i,j,k,capeflag
integer do_shallower, do_changeqref
real(8) kappa
real(8) invtau_bm_t, invtau_bm_q
real(8),dimension(km,jm,im) :: phalf
integer,dimension(im,jm) :: klzbs1
real(8),dimension(im,jm,km) :: T1,q1,p1
real(8),dimension(im,jm,km) :: phalf1
real(8),dimension(im,jm   ) :: ps1
real(8),dimension(im,jm,km) :: t_ref1
real(8),dimension(im,jm,km) :: q_ref1
real(8),dimension(im,jm,km) :: es1
real(8),dimension(im,jm,km) :: Tinc1, qinc1, TdotConv1, qdotConv1
real(8),dimension(im,jm   ) :: cape1, cin1, bmflag
real(8),dimension(im,jm   ) :: rain1  ! [kg/m2]

! Make shallower scheme the default
do_shallower = 1
do_changeqref = 0

kappa = Rd/Cpd
theta = T*(1000./p)**kappa
! Interpolate pressure to level surfaces
phalf(1,:,:) = p(1,:,:)/2.
phalf = ( p(2:km,:,:) + p(1:km-1,:,:) )/2.

! Transpose fields
ps1 = transpose(ps) * 100.
do k=1,km
   T1(:,:,k) = transpose(T(k,:,:))
   q1(:,:,k) = transpose(q(k,:,:))/1.e3
   p1(:,:,k) = transpose(p(k,:,:))*100.
   phalf1(:,:,k) = transpose(phalf(k,:,:))*100.
enddo

! Set q_ref to saturation, t_ref to (single-component) moist adiabat
t_ref1 = T00/(1. - Rv*T00/Lv*log(p1/es0))
do k=1,km
do j=1,jm
do i=1,im
   call escomp(Lv, Rv, es0, T00, t_ref1(i,j,k), es1(i,j,k))
enddo
enddo
enddo
q_ref1 = es1/p1*(Mv/Ma)

call betts_miller( &
     im,jm,km, &           
     Lv, Cpd, g, Rd, Rv, &
     Cpv, kappa, es0, T00, &
     tau_bm, rhbm,  &
     do_shallower, do_changeqref, &
     dt, T1, q1, ps1, p1, phalf1, t_ref1, &
     q_ref1, &
     rain1, Tinc1, qinc1, bmflag, &
     klzbs1, cape1, cin1, invtau_bm_t, invtau_bm_q, &
     capeflag)

! un-Transpose
do k=1,km
   Tinc(k,:,:) = transpose(Tinc1(:,:,k))
   qinc(k,:,:) = transpose(qinc1(:,:,k))*1.e3
enddo

! Convert rain to precc: [kg/m2]->[mm/day]
precc = transpose(rain1)/rowl/dt * 1000.*86400.

! Derive tendencies in "per-day"
TdotConv = Tinc/dt * 86400.
qdotConv = qinc/dt * 86400.

end subroutine driver
