c     $Header: /Users/rca/cvsroot/CliMT/src/radiation/ccm3/src/crmsrb.h,v 1.2 2004/09/07 02:47:45 rca Exp $ -*-fortran-*- 

c     Purpose: Common block crmsrb stores surface radiation budget fields for output

c     Usage: crmsrb.h should be declared after params.h
c     The values of the variables in these common blocks are set with routine radcsw()
c     which is called by radctl() which is called by crm().

c     CCM does not declare nspint in prgrid.h (as it probably should) so redefine it here 
      integer bnd_nbr_SW           ! Number of spectral intervals
      parameter(bnd_nbr_SW=19)     ! Number of spectral intervals
      integer bnd_idx_vsb       ! Index of diagnostic visible band
      parameter(bnd_idx_vsb=8)  ! Index of diagnostic visible band
      integer lvl_idx_TOA       ! Index of interface level for TOA energy (0 or 1)
      parameter(lvl_idx_TOA=0)  ! Index of interface level for TOA energy (0 or 1)
      real wvl_vsb_NIR_bnd      ! Wavelength of the visible NIR boundary
      parameter(wvl_vsb_NIR_bnd=0.7e-6) ! Wavelength of the visible NIR boundary
c     Use 0.64 rather than 0.7 microns to mimic Nimbus 7 visible/NIR partitioning
c      parameter(wvl_vsb_NIR_bnd=0.64e-6) ! Wavelength of the visible NIR boundary

c     Physical constants (from /home/zender/f/parameter.com)
      real gas_cst_universal    ! [J mol-1 K-1] Universal gas constant
      real mmw_O3               ! [kg mol-1] mmw = mean molecular weight
      real prs_STP              ! [Pa] Ref. pres. for index of refraction
      real tpt_STP              ! [K] Ref. temp. for index of refraction
      parameter(
     $     gas_cst_universal=8.31441, ! [J mol-1 K-1] Universal gas constant
     $     mmw_O3=4.7997832e-02, ! [kg mol-1] Mean molecular weight of O3
     $     prs_STP=101325.0,    ! [Pa] Ref. pres. for index of refraction
     $     tpt_STP=273.16)      ! [K] Ref. temp. for index of refraction
c     Derived constants
      real gas_cst_O3           ! (173.2) [J kg-1 K-1] Mean molecular weight of O3
      parameter(
     $     gas_cst_O3=gas_cst_universal/mmw_O3) ! (173.2) [J kg-1 K-1] (a guess, because mmw(O3) is always changing)

c     Diagnostic column abundances
      real pdelm1(plond,plev)   ! [Pa] Layer pressure thickness
      real mpc_H2O(plond)       ! [kg m-2] Mass path column H2O
      real mpc_O3(plond)        ! [kg m-2] Mass path column O3
      real mpc_O3_DU(plond)     ! [DU] Mass path column O3 in Dobson Units
      common / crmmpc /         ! crmmpc is initialized in crm()
     $     pdelm1,              ! [Pa] Layer pressure thickness
     $     mpc_H2O,             ! [kg m-2] Mass path column H2O
     $     mpc_O3,              ! [kg m-2] Mass path column O3
     $     mpc_O3_DU            ! [DU] Mass path column O3 in Dobson Units

c     TOA radiation budget
      real alb_SW_TOA(plond)    ! [frc] SW albedo at TOA
      real alb_NIR_TOA(plond)   ! [frc] NIR albedo at TOA
      real alb_vsb_TOA(plond)   ! [frc] Visible albedo at TOA
      real alb_NIR_SW_TOA(plond) ! [frc] NIR to SW albedo ratio at TOA
      real alb_NIR_vsb_TOA(plond) ! [frc] NIR to visible albedo ratio at TOA
      real flx_SW_dwn_TOA(plond) ! [W m-2] SW flux downwelling at TOA
      real flx_NIR_dwn_TOA(plond) ! [W m-2] NIR flux downwelling at TOA
      real flx_vsb_dwn_TOA(plond) ! [W m-2] Visible flux downwelling at TOA
      real flx_SW_up_TOA(plond) ! [W m-2] SW flux upwelling at TOA
      real flx_NIR_up_TOA(plond) ! [W m-2] NIR flux upwelling at TOA
      real flx_vsb_up_TOA(plond) ! [W m-2] Visible flux upwelling at TOA
      common / crmtoa /         ! crmtoa is initialized in radcsw()
     $     alb_SW_TOA,          ! [frc] SW albedo at TOA
     $     alb_NIR_TOA,         ! [frc] NIR albedo at TOA
     $     alb_vsb_TOA,         ! [frc] Visible albedo at TOA
     $     alb_NIR_SW_TOA,      ! [frc] NIR to SW albedo ratio at TOA
     $     alb_NIR_vsb_TOA,     ! [frc] NIR to visible albedo ratio at TOA
     $     flx_SW_dwn_TOA,      ! [W m-2] SW flux downwelling at TOA
     $     flx_NIR_dwn_TOA,     ! [W m-2] NIR flux downwelling at TOA
     $     flx_vsb_dwn_TOA,     ! [W m-2] Visible flux downwelling at TOA
     $     flx_SW_up_TOA,       ! [W m-2] SW flux upwelling at TOA
     $     flx_NIR_up_TOA,      ! [W m-2] NIR flux upwelling at TOA
     $     flx_vsb_up_TOA       ! [W m-2] Visible flux upwelling at TOA
      
c     Surface radiation budget
      real alb_SW_sfc(plond)    ! [frc] SW albedo at surface
      real alb_NIR_sfc(plond)   ! [frc] NIR albedo at surface
      real alb_vsb_sfc(plond)   ! [frc] Visible albedo at surface
      real dff_drc_SW_sfc(plond) ! [frc] Diffuse/direct SW downwelling flux ratio at surface
      real dff_drc_vsb_sfc(plond) ! [frc] Diffuse/direct visible downwelling flux ratio at surface
      real dff_drc_NIR_sfc(plond) ! [frc] Diffuse/direct NIR downwelling flux ratio at surface
      real flx_SW_dwn_sfc(plond) ! [W m-2] SW flux downwelling at surface
      real flx_NIR_dwn_sfc(plond) ! [W m-2] NIR flux downwelling at surface
      real flx_vsb_dwn_sfc(plond) ! [W m-2] Visible flux downwelling at surface
      real flx_SW_up_sfc(plond) ! [W m-2] SW flux upwelling at surface
      real flx_NIR_up_sfc(plond) ! [W m-2] NIR flux upwelling at surface
      real flx_vsb_up_sfc(plond) ! [W m-2] Visible flux upwelling at surface
      real flx_SW_dwn_drc_sfc(plond) ! [W m-2] SW flux downwelling at surface in direct beam
      real flx_NIR_dwn_drc_sfc(plond) ! [W m-2] NIR flux downwelling at surface in direct beam
      real flx_vsb_dwn_drc_sfc(plond) ! [W m-2] Visible flux downwelling at surface in direct beam
      real flx_SW_dwn_dff_sfc(plond) ! [W m-2] SW flux downwelling at surface in diffuse beam
      real flx_NIR_dwn_dff_sfc(plond) ! [W m-2] NIR flux downwelling at surface in diffuse beam
      real flx_vsb_dwn_dff_sfc(plond) ! [W m-2] Visible flux downwelling at surface in diffuse beam
      common / crmsrb /         ! crmsrb is initialized in radcsw()
     $     alb_SW_sfc,          ! [frc] SW albedo at surface
     $     alb_NIR_sfc,         ! [frc] NIR albedo at surface
     $     alb_vsb_sfc,         ! [frc] Visible albedo at surface
     $     dff_drc_SW_sfc,      ! [frc] Diffuse/direct SW downwelling flux ratio at surface
     $     dff_drc_vsb_sfc,     ! [frc] Diffuse/direct visible downwelling flux ratio at surface
     $     dff_drc_NIR_sfc,     ! [frc] Diffuse/direct NIR downwelling flux ratio at surface
     $     flx_SW_dwn_sfc,      ! [W m-2] SW flux downwelling at surface
     $     flx_NIR_dwn_sfc,     ! [W m-2] NIR flux downwelling at surface
     $     flx_vsb_dwn_sfc,     ! [W m-2] Visible flux downwelling at surface
     $     flx_SW_up_sfc,       ! [W m-2] SW flux upwelling at surface
     $     flx_NIR_up_sfc,      ! [W m-2] NIR flux upwelling at surface
     $     flx_vsb_up_sfc,      ! [W m-2] Visible flux upwelling at surface
     $     flx_SW_dwn_drc_sfc,  ! [W m-2] SW flux downwelling at surface in direct beam
     $     flx_NIR_dwn_drc_sfc, ! [W m-2] NIR flux downwelling at surface in direct beam
     $     flx_vsb_dwn_drc_sfc, ! [W m-2] Visible flux downwelling at surface in direct beam
     $     flx_SW_dwn_dff_sfc,  ! [W m-2] SW flux downwelling at surface in diffuse beam
     $     flx_NIR_dwn_dff_sfc, ! [W m-2] NIR flux downwelling at surface in diffuse beam
     $     flx_vsb_dwn_dff_sfc  ! [W m-2] Visible flux downwelling at surface in diffuse beam

c     Wavelength grid
      real bnd(bnd_nbr_SW)         ! [m] Nominal coordinate
      real wvl(bnd_nbr_SW)         ! [m] Nominal wavelength coordinate
      real wvl_ctr(bnd_nbr_SW)     ! [m] Center of band in wavelength space
      real wvl_min(bnd_nbr_SW)     ! [m] Minimum wavelength in band
      real wvl_max(bnd_nbr_SW)     ! [m] Maximum wavelength in band
      real wvl_dlt(bnd_nbr_SW)     ! [m] Bandwidth
      real wvn(bnd_nbr_SW)         ! [cm-1] Nominal wavenumber coordinate
      real wvn_ctr(bnd_nbr_SW)     ! [cm-1] Center of band in wavenumber space
      real wvn_min(bnd_nbr_SW)     ! [cm-1] Minimum wavenumber in band
      real wvn_max(bnd_nbr_SW)     ! [cm-1] Maximum wavenumber in band
      real wvn_dlt(bnd_nbr_SW)     ! [cm-1] Bandwidth
      common / crmspc /         ! crmspc is initialized in radcsw()
     $     bnd,                 ! [m] Nominal coordinate
     $     wvl,                 ! [m] Nominal wavelength coordinate
     $     wvl_ctr,             ! [m] Center of band in wavelength space
     $     wvl_min,             ! [m] Minimum wavelength in band
     $     wvl_max,             ! [m] Maximum wavelength in band
     $     wvl_dlt,             ! [m] Bandwidth
     $     wvn,                 ! [cm-1] Nominal wavenumber coordinate
     $     wvn_ctr,             ! [cm-1] Center of band in wavenumber space
     $     wvn_min,             ! [cm-1] Minimum wavenumber in band
     $     wvn_max,             ! [cm-1] Maximum wavenumber in band
     $     wvn_dlt              ! [cm-1] Bandwidth

c     Surface and TOA band fluxes
      real flx_bnd_dwn_TOA(plond,bnd_nbr_SW) ! [W m-2] Downwelling flux at TOA
      real flx_bnd_dwn_dff_sfc(plond,bnd_nbr_SW) ! [W m-2] Downwelling diffuse field flux at surface
      real flx_bnd_dwn_drc_sfc(plond,bnd_nbr_SW) ! [W m-2] Downwelling direct beam flux at surface
      real flx_bnd_dwn_sfc(plond,bnd_nbr_SW) ! [W m-2] Downwelling flux at surface
      real flx_bnd_up_TOA(plond,bnd_nbr_SW) ! [W m-2] Upwelling flux at TOA
      real flx_bnd_up_sfc(plond,bnd_nbr_SW) ! [W m-2] Upwelling flux at surface
      common / crmflxspc /      ! crmflxspc is initialized in radcsw()
     $     flx_bnd_dwn_TOA,     ! [W m-2] Downwelling flux at TOA
     $     flx_bnd_dwn_dff_sfc, ! [W m-2] Downwelling diffuse field flux at surface
     $     flx_bnd_dwn_drc_sfc, ! [W m-2] Downwelling direct beam flux at surface
     $     flx_bnd_dwn_sfc,     ! [W m-2] Downwelling flux at surface
     $     flx_bnd_up_TOA,      ! [W m-2] Upwelling flux at TOA
     $     flx_bnd_up_sfc       ! [W m-2] Upwelling flux at surface

c     Vertical profiles of SW broadband fluxes
      real dff_drc_SW(plond,plevp) ! [frc] Diffuse/direct SW downwelling flux ratio
      real flx_SW_dwn(plond,plevp) ! [W m-2] Downwelling SW flux
      real flx_SW_dwn_dff(plond,plevp) ! [W m-2] Downwelling SW flux diffuse field
      real flx_SW_dwn_drc(plond,plevp) ! [W m-2] Downwelling SW flux direct beam
      real flx_SW_up(plond,plevp) ! [W m-2] Upwelling SW flux
      common / crmflxbbvrtSW /  ! crmflxbbvrtSW is initialized in radcsw()
     $     dff_drc_SW,          ! [frc] Diffuse/direct SW downwelling flux ratio
     $     flx_SW_dwn,          ! [W m-2] Downwelling SW flux
     $     flx_SW_dwn_dff,      ! [W m-2] Downwelling SW flux diffuse field
     $     flx_SW_dwn_drc,      ! [W m-2] Downwelling SW flux direct beam
     $     flx_SW_up            ! [W m-2] Upwelling SW flux
      
c     Vertical profiles of LW broadband fluxes
      real flx_LW_dwn(plond,plevp) ! [W m-2] Downwelling LW flux
      real flx_LW_up(plond,plevp) ! [W m-2] Upwelling LW flux
      common / crmflxbbvrtLW /  ! crmflxvrt is initialized in radclw()
     $     flx_LW_dwn,          ! [W m-2] Downwelling LW flux
     $     flx_LW_up            ! [W m-2] Upwelling LW flux

c     Vertical profiles of band fluxes
      real flx_bnd_dwn(plond,plevp,bnd_nbr_SW) ! [W m-2] Downwelling SW flux
      real flx_bnd_dwn_dff(plond,plevp,bnd_nbr_SW) ! [W m-2] Downwelling SW flux diffuse field
      real flx_bnd_dwn_drc(plond,plevp,bnd_nbr_SW) ! [W m-2] Downwelling SW flux direct beam
      real flx_bnd_up(plond,plevp,bnd_nbr_SW) ! [W m-2] Upwelling SW flux
      common / crmflxbndvrtSW / ! crmflxbndvrtSW is initialized in radcsw()
     $     flx_bnd_dwn,          ! [W m-2] Downwelling SW flux
     $     flx_bnd_dwn_dff,      ! [W m-2] Downwelling SW flux diffuse field
     $     flx_bnd_dwn_drc,      ! [W m-2] Downwelling SW flux direct beam
     $     flx_bnd_up            ! [W m-2] Upwelling SW flux
      
c     Spectral column optical depths
      real odxc_CO2(plond,bnd_nbr_SW) ! [frc] CO2 absorption
      real odxc_H2O(plond,bnd_nbr_SW) ! [frc] H2O absorption
      real odxc_O2(plond,bnd_nbr_SW) ! [frc] O2 absorption
      real odxc_O3(plond,bnd_nbr_SW) ! [frc] O3 absorption
      real odxc_Ray(plond,bnd_nbr_SW) ! [frc] Rayleigh scattering
      real odxc_aer(plond,bnd_nbr_SW) ! [frc] Aerosol extinction
      real odxc_ice(plond,bnd_nbr_SW) ! [frc] Ice cloud extinction
      real odxc_lqd(plond,bnd_nbr_SW) ! [frc] Liquid cloud extinction
      real odxc_ttl(plond,bnd_nbr_SW) ! [frc] Optical depth extinction column total
      common / crmodxc /        ! crmodxc is initialized in radcsw()
     $     odxc_CO2,            ! [frc] CO2 absorption
     $     odxc_H2O,            ! [frc] H2O absorption
     $     odxc_O2,             ! [frc] O2 absorption
     $     odxc_O3,             ! [frc] O3 absorption
     $     odxc_Ray,            ! [frc] Rayleigh scattering
     $     odxc_aer,            ! [frc] Aerosol extinction
     $     odxc_ice,            ! [frc] Ice cloud extinction
     $     odxc_lqd,            ! [frc] Liquid cloud extinction
     $     odxc_ttl             ! [frc] Optical depth extinction column total

c     Spectral layer optical depths
      real odxl_CO2(plond,plev,bnd_nbr_SW) ! [frc] CO2 absorption
      real odxl_H2O(plond,plev,bnd_nbr_SW) ! [frc] H2O absorption
      real odxl_O2(plond,plev,bnd_nbr_SW) ! [frc] O2 absorption
      real odxl_O3(plond,plev,bnd_nbr_SW) ! [frc] O3 absorption
      real odxl_Ray(plond,plev,bnd_nbr_SW) ! [frc] Rayleigh scattering
      real odxl_aer(plond,plev,bnd_nbr_SW) ! [frc] Aerosol extinction
      real odxl_ice(plond,plev,bnd_nbr_SW) ! [frc] Ice cloud extinction
      real odxl_lqd(plond,plev,bnd_nbr_SW) ! [frc] Liquid cloud extinction
      real odxl_ttl(plond,plev,bnd_nbr_SW) ! [frc] Optical depth extinction column total
      common / crmodxl /        ! crmodxl is initialized in radcsw()
     $     odxl_CO2,            ! [frc] CO2 absorption
     $     odxl_H2O,            ! [frc] H2O absorption
     $     odxl_O2,             ! [frc] O2 absorption
     $     odxl_O3,             ! [frc] O3 absorption
     $     odxl_Ray,            ! [frc] Rayleigh scattering
     $     odxl_aer,            ! [frc] Aerosol extinction
     $     odxl_ice,            ! [frc] Ice cloud extinction
     $     odxl_lqd,            ! [frc] Liquid cloud extinction
     $     odxl_ttl             ! [frc] Optical depth extinction column total

