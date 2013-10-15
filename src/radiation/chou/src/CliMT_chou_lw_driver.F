      subroutine chou_lw(
     & t, ts, q, 
     & p, ps, o3, co2, n2o, ch4, cfc11, cfc12, cfc22, 
     & clwp, ciwp, cldf, r_liq, r_ice,
     & g, Cp,
     & lwflx, lw_cf_toa, lw_cf_srf,  lwhr, tau)

      parameter(km=KM)

! Driver for Chou's LW radiation code. 

! In:
      real t(km)           ! atmos. temperature [K]
      real ts              ! surface temperature [K]
      real q(km)           ! water vapour mass mixing ratio [kg/kg]
      real p(km)           ! level pressure [mb] 
      real ps              ! surface pressure [mb] 
      real o3(km)          ! ozone mass mixing ratio [kg/kg]
      real co2             ! CO2   volume mixing ratio [ppmv] 
      real n2o             ! N2O   volume mixing ratio [ppmv] 
      real ch4             ! CH4   volume mixing ratio [ppmv] 
      real cfc11           ! CFC11 volume mixing ratio [ppmv] 
      real cfc12           ! CFC12 volume mixing ratio [ppmv] 
      real cfc22           ! CFC12 volume mixing ratio [ppmv] 
      real clwp(km)        ! cloud liquid water path [g/m2]
      real ciwp(km)        ! cloud liquid water path [g/m2]
      real r_liq(km)       ! effective radius liq drop
      real r_ice(km)       ! effective radius ice drop
      real cldf(km)        ! fractional cloud cover 
      real g               ! gravitational accn [m s-2]
      real Cp              ! specific heat capacity of air [J K-1 kg-1]
! Out:
      real lwflx(km+1)     ! NET downward longwave flux [W m-2] 
      real lw_cf_toa       ! LW TOA cloud forcing [W/m2]
      real lw_cf_srf       ! LW surface cloud forcing [W/m2]
      real lwhr(km)        ! LW heating rate [K s-1]
      real tau(km)         ! LW broadband optical depth [-]

! Local (input to Chou's routine):
      real ph(km+1)        ! interface pressure [mb] (ph(km+1) is SURFACE pressure)
      real cwc(km,3)       ! cloud water path [g/m2] (1->ice, 2->liquid, 3->rain)
      real reff(km,3)      ! effective cloud particle size [microns]
      real taucld(km,3)    ! cloud optical thickness 
      real emiss(10)       ! surface emissivity (in each of the 10 bands)
      real taual(km,10)    ! parameters for aerosols 
      real ssaal(km,10)    ! parameters for aerosols 
      real asyal(km,10)    ! parameters for aerosols 
      integer ict          ! top of middle clouds 
      integer icb          ! top of low clouds
      logical cldwater     ! true means taucld is computed from cwc and reff.
      logical high
      logical trace
! Local (output from Chou's routine):
      real dfdts(km+1)     ! sensitivity of LW flux to Ts [W m-2 K-1]
      real st4             ! emission by surface [W m-2]
! Local workspace
      real lwflxc(km+1)      ! NET downward longwave flux, clear sky [W m-2] 
      real fice(km)        ! fraction of cloud water that is ice
      real rel(km)         ! effective radius liquid drops [micron]
      real rei(km)         ! effective radius ice drops [micron]

! Set values of some inputs
      ! interface pressures
      ph(1)=1.e-9
      do k=2,km
       ph(k)=0.5*( p(k-1)+p(k) )
      enddo                     
      ph(km+1)=ps   
      ! ppmv -> ppv
      co2ppv   = co2   * 1.e-6 
      n2oppv   = n2o   * 1.e-6
      ch4ppv   = ch4   * 1.e-6
      cfc11ppv = cfc11 * 1.e-6
      cfc12ppv = cfc12 * 1.e-6
      cfc22ppv = cfc22 * 1.e-6
      ! treat surface as black body
      emiss = 1. 
      ! cloud water
      call cldefr(t, rel, rei, fice, ps, p)
      do k=1,km
         if (ciwp(k).eq.-99.) then
            cwc(k,1)=clwp(k)*fice(k)  ! ice
            cwc(k,2)=clwp(k)*(1.-fice(k)) ! liquid
         else
            cwc(k,1)=clwp(k)  ! ice
            cwc(k,2)=ciwp(k) ! liquid
         endif
      enddo
      cwc(:,3)=0.               ! rain
      ! drop radii
      reff(:,1) = r_ice
      reff(:,2) = r_liq
      reff(:,3) = r_liq
      ! no aerosols
      taual=0.
      ssaal=0.
      asyal=0.
      ! top of middle clouds 
      ict=km/3
      ! top of low clouds
      icb=km*2/3     
      ! we're passing cloud water cwc (not cloud optical depth taucl) to routine
      cldwater=.true.
      taucld=0.
      ! use high-accuracy algorithm
      high=.true.
      ! include trace gases in computation
      trace=.true.

! Compute radiative fluxes
      call irrad (ph, t, q*1.e-3, o3, ts, co2ppv,
     *            n2oppv, ch4ppv, cfc11ppv, cfc12ppv, cfc22ppv, emiss,
     *            cldwater, cwc, taucld, reff, cldf, ict, icb,
     *            taual, ssaal, asyal,
     *            high, trace, lwflx, lwflxc, dfdts, st4)
c+++climt
! disabled tau -- gives bus error with gfortran
!     *            high, trace, lwflx, lwflxc, dfdts, st4, tau)

! Compute cloud forc and heating rate

      lw_cf_toa = lwflx(1)    - lwflxc(1)
      lw_cf_srf = lwflx(km+1) - lwflxc(km+1)

      do k=1,km
        lwhr(k)  = -g/Cp*( lwflx(k+1)-lwflx(k) )/( ph(k+1)-  ph(k) ) / 100.
      enddo
c
      end subroutine chou_lw
