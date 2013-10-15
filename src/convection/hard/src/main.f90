subroutine main(km, p, ps, Cpd, Rd, lapse, g, T, theta)

! Emanuel hard adjustment scheme with tunable lapse rate
! J. L. Mitchell  6June2005

implicit none

integer               :: km, nl, k
real                  :: ps, Cpd, Rd, lapse, g, kap
real, dimension(km)   :: p, p1, T, T1, theta, theta1
real, dimension(km+1) :: ph, ph1

! Convert T ->  theta 
theta = T / (p/ps)**(Rd/Cpd)

! lapse rate in K/m
kap = Rd/(g/lapse)

! Set interface pressures
ph(1)=0.5*p(1)
do k=2,km
 ph(k)=0.5*( p(k-1)+p(k) )
enddo
ph(km+1)=ps

! Invert profiles, as required my Emanuel scheme
p1  = p(km:1:-1)
ph1 = ph(km+1:1:-1)
t1  = t(km:1:-1)
theta1  = theta(km:1:-1)

! Do adiabatic adjustment
! set max level to which convection can penetrate
nl = km - 1
call adjust(km,nl,Cpd,ps,ph1,p1,t1,theta1,kap)

! Invert profiles
t     = t1(km:1:-1)
theta = theta1(km:1:-1)

end subroutine main
