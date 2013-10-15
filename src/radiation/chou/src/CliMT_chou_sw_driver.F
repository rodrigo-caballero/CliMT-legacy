      subroutine chou_sw(
     & t, q, 
     & p, ps, o3, co2, 
     & clwp, ciwp, cldf, r_liq, r_ice,
     & aldif, aldir, asdif, asdir, zen, solin, g, Cp,
     & swflx, sw_cf_toa, sw_cf_srf, swhr)
      
      parameter(km=KM)

! Driver for Chou's SW radiation code. 

! In:
      real t(km)           ! atmos. temperature [K]
      real q(km)           ! water vapour mass mixing ratio [kg/kg]
      real p(km)           ! level pressure [mb] 
      real ps              ! surface pressure [mb] 
      real o3(km)          ! ozone mass mixing ratio [kg/kg]
      real co2             ! CO2 mixing ration by volume [ppmv]
      real clwp(km)        ! cloud liquid water path [g/m2]
      real ciwp(km)        ! cloud ice water path [g/m2]
      real cldf(km)        ! fractional cloud cover
      real r_liq(km)       ! liquid cloud drop radius [micron]
      real r_ice(km)       ! ice cloud drop radius [micron]
      real asdir           ! surface albedo, uv+vis, direct
      real asdif           ! surface albedo, uv+vis, diffuse
      real aldir           ! surface albedo, ir, direct
      real aldif           ! surface albedo, ir, diffuse
      real zen             ! zenith angle [dgr]
      real solin           ! insolation [W m-2]
      real g               ! gravitational accn [m s-2]
      real Cp              ! specific heat capacity of air [J K-1 kg-1]
! Out:
      real swflx(km+1)     ! SW radiative flux [W/m2]
      real sw_cf_toa       ! SW TOA cloud forcing [W/m2]
      real sw_cf_srf       ! SW surface cloud forcing [W/m2]
      real swhr(km)        ! SW heating rate [K/s]
! Local (input to Chou's routine):
      real ph(km+1)        ! interface pressure [mb] (ph(km+1) is SURFACE pressure)
      real cwc(km,3)       ! cloud water mass mixing ratio [kg/kg] (2->liquid, 1->ice, 3->rain)
      real taucld(km,3)    ! cloud optical thickness 
      real reff(km,3)      ! effective cloud particle size [microns]
      real cosz            ! cosine of solar zenith angle
      real taual(km,11)    ! parameters for aerosols 
      real ssaal(km,11)    ! parameters for aerosols 
      real asyal(km,11)    ! parameters for aerosols 
      integer ict          ! top of middle clouds 
      integer icb          ! top of low clouds
      logical overcast
      logical cldwater     ! true means taucld is computed from cwc and reff.
! Local (output from Chou's routine):
      real sflx(km+1)      ! NET downward solar flux [fraction] 
      real sflc(km+1)      ! NET downward solar flux, clear sky
      real fdiruv          ! surface flux
      real fdifuv          ! surface flux
      real fdirpar         ! surface flux
      real fdifpar         ! surface flux
      real fdirir          ! surface flux
      real fdifir          ! surface flux
! Local workspace
      real fice(km)        ! fraction of cloud water that is ice
      real rel(km)         ! effective radius liquid drops [micron]
      real rei(km)         ! effective radius ice drops [micron]
      real swflxc(km+1)     ! SW clear sky radiative flux [W/m2]

 ! Set values of some inputs
      ! interface pressures
      ph(1)=0.5*p(1)
      do k=2,km
       ph(k)=0.5*( p(k-1)+p(k) )
      enddo                     
      ph(km+1)=ps   
      ! cosine of zenith angle
      cosz  = cos(zen*abs(acos(-1.))/180.)
      ! protect against zen >= 90 dgr
      if (zen .ge. 89.) then
         swflx=0.
         swflxc=0.
         swhr=0.
         return
      endif
      ! ppmv -> ppv
      co2ppv   = co2*1.e-6 
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
      where (reff.eq.0.) reff=1.e-19 ! can't have zero drop size
      ! no aerosols
      taual=0.
      ssaal=0.
      asyal=0.
      ! top of middle clouds 
      ict=km/3
      ! top of low clouds
      icb=km*2/3     
      ! we're passing cloud water content cwc (not cloud optical depth taucl) to routine
      cldwater=.true.
      taucld=0.
      ! assume cloud cover is fractional
      overcast = .false.

! Compute radiative flux
      call sorad (ph, t, q*1.e-3, o3, co2ppv,
     &            overcast, cldwater, cwc, taucld, reff, cldf, ict, icb,
     &            taual,ssaal,asyal,
     &            cosz, asdir, asdif, aldir, aldif,
     &            sflx, sflc, fdiruv, fdifuv, fdirpar, fdifpar,
     &            fdirir, fdifir)

! Rescale fluxes and compute SW heating rate
      do k=1,km+1
       swflx(k) = sflx(k) * solin
       swflxc(k) = sflc(k) * solin
      enddo

      sw_cf_toa = swflx(1)    - swflxc(1)
      sw_cf_srf = swflx(km+1) - swflxc(km+1)

      do k=1,km
        swhr(k)  = -g/Cp*( swflx(k+1)-swflx(k) )/( ph(k+1)-  ph(k) ) / 100.
      enddo

      end subroutine chou_sw
