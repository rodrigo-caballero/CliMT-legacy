! Driver for Emanuel's convective scheme (version 4.3)

! Note: 
! In  Emanuel's convective scheme, k=1 is SURFACE

subroutine main(km, dt, t, q, p, ps,                                & 
     Cpdx, Cpvx, Clx, Rvx, Rdx, Lv0x, gx, rowlx,         & 
     elcritx,tlcritx,entpx,sigdx,sigsx,omtrainx,omtsnowx, &
     coeffrx,coeffsx,cux,betax,dtmaxx,alphax,dampx,      &
     tinc, qinc, tdot, qdot, precc, cbmf, theta)            ! Outputs

parameter(ntra=1)         ! number of tracers in convect

real                  :: Cpdx,Cpvx,Clx,Rvx,Rdx,Lv0x,gx,rowlx,precc,cbmf
real, dimension(km)   :: t,q,p,tdot,qdot,tinc,qinc,theta
! Local
real, dimension(km)   :: p1,t1,q1,qs1,u1,v1,ft1,fq1,fu1,fv1,theta1
real, dimension(km+1) :: ph,ph1
real, dimension(km,ntra) :: tra1,ftra1

! Initialise physical constants in emanuel's convection scheme
real Cpd,Cpv,Cl,Rv,Rd,Lv0,g,rowl,                 &
     elcrit,tlcrit,entp,sigd,sigs,omtrain,omtsnow, &
     coeffr,coeffs,cu,beta,dtmax,alpha,damp
common/emanuel_const/Cpd,Cpv,Cl,Rv,Rd,Lv0,g,rowl, &
                     elcrit,tlcrit,entp,sigd,sigs,omtrain,omtsnow, &
                     coeffr,coeffs,cu,beta,dtmax,alpha,damp

! -- Emanuel's recommended values:
Cpd      = 1005.7             ! specific heat of dry air [J kg-1 K-1] 
Cpv      = 1870.0             ! specific heat of water vapour [J kg-1 K-1]
Cl       = 4190.0             ! specific heat of liquid water
Rv       = 461.5              ! gas constant for water vapour [J kg-1 K-1]
Rd       = 287.04             ! gas constant for dry air [J kg-1 K-1] 
Lv0      = 2.501e6            ! latent heat of vaporisation AT SURFACE
g        = 9.8                ! gravitational acceleration [m s-2]
rowl     = 1000.0             ! density of liquid water [kg m-3]
elcrit   = 0.0011
tlcrit   = -55.0
entp     = 1.5
sigd     = 0.05
sigs     = 0.12
omtrain  = 50.0
omtsnow  = 5.5 
coeffr   = 1.0
coeffs   = 0.8
cu       = 0.7
beta     = 10.0
dtmax    = 0.9
alpha    = 0.2   
damp     = 0.1
! -- Externally specified values:
Cpd      = Cpdx
Cpv      = Cpvx
Cl       = Clx 
Rv       = Rvx
Rd       = Rdx
Lv0      = Lv0x
g        = gx
rowl     = rowlx
elcrit   = elcritx
tlcrit   = tlcritx
entp     = entpx
sigd     = sigdx
sigs     = sigsx
omtrain  = omtrainx
omtsnow  = omtsnowx
coeffr   = coeffrx
coeffs   = coeffsx
cu       = cux
beta     = betax
dtmax    = dtmax
alpha    = alphax
damp     = dampx


! Set values of some inputs
! interface pressures
ph(1)=0.5*p(1)
do k=2,km
 ph(k)=0.5*( p(k-1)+p(k) )
enddo
ph(km+1)=ps

! turn profiles upside-down
p1  = p(km:1:-1)
ph1 = ph(km+1:1:-1)
t1  = t(km:1:-1)
q1  = q(km:1:-1) * 1.e-3 ! g/kg -> g/g
call get_qs(km,p1,t1,qs1)

! call convective scheme
nl   = km-2  ! max level to which convection can penetrate
delt = dt    ! time interval between calls to convect 
u1=0.
v1=0.
tra1=0.

call convect( t1, q1, qs1, u1, v1, tra1, p1, ph1,            &
              km, nl, ntra, delt, iflag,  ft1, fq1, fu1,     &
              fv1, ftra1, precc, wd, tp, qp, cbmf, theta1)

! turn output upside-down
t     = t1(km:1:-1)
theta = theta1(km:1:-1)
q     = q1(km:1:-1) * 1.e3 ! g/g -> g/kg
tdot  = ft1(km:1:-1)
qdot  = fq1(km:1:-1) * 1.e3 ! g/g/s -> g/kg/s
tinc  = 2.*dt*tdot
qinc  = 2.*dt*qdot

end subroutine main
!========================================================================
subroutine get_qs(km,p,t,q)
!
! Compute saturation mixing ratio
!
external qsflatau,qs

real, dimension(km) :: t,q,p

do k=1,km
! if (t(k) < 273.15) then
 if (t(k) < 273.15-20.) then
  ice=2 
 else
  ice=1
 endif
! q(k)=qs(t(k),p(k))*1.e-3
 q(k)=qsflatau(t(k),p(k),ice)*1.e-3
 if (q(k) < 0.) q(k)=1.e-10
enddo

end subroutine get_qs
