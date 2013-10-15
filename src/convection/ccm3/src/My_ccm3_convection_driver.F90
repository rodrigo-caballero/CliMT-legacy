subroutine ccm3_convection(p, t, q, ps, Ts, z0, dt, pblh,       &
     Cpd, g, Lv, Lf, Rd, Rv, rhow, Cpv,                          & 
     tdot, qdot, precc, precsc, precl, precsl)

integer, parameter :: km=KM

! Input
real, dimension(km) :: p      ! pressure at midpoints [mb]
real, dimension(km) :: t      ! temperature [K]
real, dimension(km) :: q      ! humidity [g/kg]
real                :: ps     ! surface pressure [mb]
real                :: Ts     ! surface temp [K]
real                :: z0     ! surface elevation [m]
real                :: dt     ! time step [s]
real                :: pblh   ! PBL height [m] (computed by CCM3 pbl scheme)
real                :: Rd     ! gas constant for dry air [J kg-1 K-1] 
real                :: Rv     ! gas constant for water vapour [J kg-1 K-1] 
real                :: Cpd    ! specific heat of dry air [J kg-1 K-1]
real                :: Cpv    ! specific heat of water vapour [J kg-1 K-1]
real                :: Lv     ! latent heat of vaporisation AT SURFACE 
real                :: Lf     ! latent heat of fusion
real                :: g      ! gravitational acceleration [m s-2]
real                :: rhow   ! density of water [kg m-3]

! Output
real, dimension(km) :: tdot   ! temperature tendency [K/s]
real, dimension(km) :: qdot   ! humidity tendency [g/kg/s]
real                :: precc  ! convective precip
real                :: precl  ! large-scale precip
real                :: precsc ! convective precip, snow
real                :: precsl ! large-scale precip, snow

! Local
real, dimension(km+1) :: pint,piln
real, dimension(km) :: pmid,pdel,rpdel,pmln,t1,q1
real, dimension(km) :: qc,cmfdt,cmfdq,zmdt,zmdq,cmfdqr,cmfmc,cmfsl,cmflq
real               :: phis,cnt,cnb,tpert,qpert
integer lat

! Initialise common blocks
call ccm36_convection_init(Cpd, g, Lv, Lf, Rd, Rv, rhow, Cpv)

! Initialise local quantities:
!  nstep used only in cmfmca for debugging; can safely set = 1
nstep=1
!  lat used only for screen output, set = 1
lat=1
! Convert pressures from mb to pascals
pmid=p*100.0 ! mb --> Pa
! Define interface pressures
pint(1)=0.5*pmid(1) ! Pa
do k=2,km
 pint(k)=0.5*(pmid(k-1)+pmid(k)) ! Pa
enddo               
pint(km+1)=ps*100.   ! mb->Pa
! Compute log pressures
pmln=log(pmid)
piln=log(pint)
! Compute pressure intervals
do k=1,km
 pdel(k)=pint(k+1)-pint(k) ! Pa
 rpdel(k)=1./pdel(k)
enddo               
! Use dummy variables, since t and q get changed 
t1=t
q1=q
! Constants
tdt=2.*dt
phis=g*z0

call aphys(      nstep     ,lat       ,tdt     ,phis    ,pblh    , &
                 tpert     ,pmid      ,pint    ,pdel    ,rpdel   , &
                 pmln      ,piln      ,t1      ,q1      ,ts      , &
                 qpert     ,precl     ,precc   ,precsl  ,precsc  , &
                 tdot      ,qdot      ,cnt     ,cnb     ,qc      , &
                 cmfdt     ,cmfdq     ,cmfdqr  ,cmfmc   ,cmfsl   , &
                 cmflq     ,zmdt      ,zmdq    )
 
end subroutine ccm3_convection
