subroutine main(km, do_srf_mom_flx, do_srf_sen_flx, do_srf_lat_flx, &
     Cpd, g, Lv, Rd, Rv, Cpv, dt, p_in, ps_in, u_in, v_in, T, Ts, q_in, &
     Uinc, Vinc, Tinc, qinc, udot, vdot, Tdot, qdot, th, shflx, lhflx, taux, tauy)
!    
! Driver for CCM3.6 vert diff, PBL and ocean surface flux schemes

implicit none

! In
integer, intent(in)                :: km, do_srf_mom_flx
integer, intent(in)                :: do_srf_sen_flx, do_srf_lat_flx
real,    intent(in)                :: Cpd, g, Lv, Rd, Rv, Cpv, dt, Ts, ps_in
real,    intent(in), dimension(km) :: p_in, u_in, v_in, T, q_in

! Out
real, intent(out)                :: shflx, lhflx, taux, tauy
real, intent(out), dimension(km) :: Uinc, Vinc, Tinc, qinc
real, intent(out), dimension(km) :: udot, vdot, Tdot, qdot,th

! Local
integer               :: lat, k
real                  :: ps, oro, qflx, ustar, lwup, twodt, tpert, qpert, pblh
real, dimension(km+1) :: pint, kvh, kvm, cgs
real, dimension(km)   :: rpdel, rpdeli, q, p, u, v, z
real, dimension(km)   :: up1, vp1, thp1, tp1, qp1

! Remove zeros for safety
u = u_in
v = v_in
where (u==0.) u = 1.e-12
where (v==0.) v = 1.e-12

! Initialise constants
call init( Cpd, g, Lv, Rd, Rv, Cpv) ! initialises common blocks
lat = 1
twodt = 2.*dt

! Convert units
q  = q_in*1.e-3 ! g/kg -> g/g
p  = p_in*100.0   ! mb --> Pa
ps = ps_in*100.0 ! mb --> Pa

! Compute interface pressures
do k = 1,km+1
   if(k.eq.1) then
      pint(k)=p(k)/2.0 ! Pa
   else if (k.gt.1.and.k.le.km) then
      pint(k)=0.5*(p(k-1)+p(k)) ! Pa
   else if (k.eq.km+1) then
      pint(k)=ps   ! Pa
   endif              
enddo                 

! Compute inverse pressure intervals
do k = 1,km-1
   rpdel(k) = 1./(p(k+1)-p(k))
   rpdeli(k) = 1./(pint(k+1)-pint(k))
enddo
rpdel(km) = rpdel(km-1)
rpdeli(km) = 1./(pint(km+1)-pint(km))

! Compute potential temp
!  (could improve by taking q into account)
th = (1000.*100./p)**(Rd/Cpd) * T 

! Compute geopot height
! ( should really give virtual temp on input)
call zmid(log(ps), log(p), Rd, g, T, z)

!=== Surface fluxes
oro = 0. !!!! assuming surface is water !!!!
call srfoce(oro    ,u(km)  ,v(km)      ,T(km) ,q(km) ,   &
            th(km) ,z(km)  ,p(km)      ,Ts    ,qflx  ,   &
            taux   ,tauy   ,shflx      ,lhflx ,lwup      )

taux  = taux  * do_srf_mom_flx
tauy  = tauy  * do_srf_mom_flx
shflx = shflx * do_srf_sen_flx
qflx  = qflx  * do_srf_lat_flx
lhflx  = lhflx  * do_srf_lat_flx

!=== Vertical diffusion and PBL
call vdiff(lat     ,u     ,v     ,T      ,q      ,   &
           p       ,pint  ,rpdel ,rpdeli ,twodt  ,   &
           th      ,z     ,taux  ,tauy   ,shflx  ,   &
           qflx    ,up1   ,vp1   ,thp1   ,qp1    ,   &
           pblh    ,ustar ,kvh   ,kvm    ,tpert  ,   &
           qpert   ,cgs                                    )

! Compute updated T from updated th
!  (could improve by taking q into account)
tp1 = (p/1000./100.)**(Rd/Cpd) * thp1 

! Compute increments
Uinc = up1 - u
Vinc = vp1 - v
Tinc = tp1 - T
qinc = (qp1 - q) * 1.e3 ! g/g -> g/kg

! Compute tendencies
udot = Uinc/twodt
vdot = Vinc/twodt
Tdot = Tinc/twodt
qdot = qinc/twodt

!print*,' --- '
!do k=1,km
!print'(10f10.5)', Vinc(k)*86400/dt,Tinc(k)*86400/dt,qinc(k)*86400/dt
!enddo
!print*,' --- '
!print'(10f10.5)', sum(Vinc)/km*86400/dt,sum(Tinc)/km*86400/dt,sum(qinc)/km*86400/dt

end subroutine main
