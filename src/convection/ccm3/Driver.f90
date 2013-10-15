subroutine driver(km, p, ps, T, Ts, q, z0, dt, pblh,   &
     Cpd, g, Lv, Lf, Rd, Rv, rhow, Cpv,                          & 
     tdot, qdot, precc, precsc, precl, precsl)

! Input
integer                :: km
real(8), dimension(km) :: p   
real(8), dimension(km) :: t    
real(8), dimension(km) :: q    
real(8)                :: ps   
real(8)                :: Ts   
real(8)                :: z0   
real(8)                :: dt   
real(8)                :: pblh 
real(8)                :: Rd   
real(8)                :: Rv   
real(8)                :: Cpd  
real(8)                :: Cpv  
real(8)                :: Lv   
real(8)                :: Lf   
real(8)                :: g    
real(8)                :: rhow 

! Output
real(8), dimension(km) :: tdot 
real(8), dimension(km) :: qdot 
real(8)                :: precc
real(8)                :: precl
real(8)                :: precsc
real(8)                :: precsl

!f2py intent(in,hide) km
!f2py intent(in) p, ps, Ts, z0, dt, pblh, Cpd, g, Lv, Lf, Rd, Rv, rhow, Cpv
!f2py intent(in,out)  t, q
!f2py intent(out)  tdot, qdot, precc, precsc, precl, precsl

call ccm3_convection(p, t, q, ps, Ts, z0, dt, pblh,       &
     Cpd, g, Lv, Lf, Rd, Rv, rhow, Cpv,                          & 
     tdot, qdot, precc, precsc, precl, precsl)

 
end subroutine ccm3_convection_driver

!-------------------------------------------------------------

integer function getlevels()

getlevels=levels()

end function getlevels 
