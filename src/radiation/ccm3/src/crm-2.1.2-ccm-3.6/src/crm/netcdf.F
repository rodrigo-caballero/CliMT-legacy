c     $Header: /Users/rca/cvsroot/CliMT/src/radiation/ccm3/src/crm-2.1.2-ccm-3.6/src/crm/netcdf.F,v 1.1 2004/09/07 15:27:30 rca Exp $

c     Purpose: Provide netCDF output for CCM Column Radiation Model (CRM)

c     This routine assumes all floating point input variables are real*8,
c     i.e., use -r8 -i4 compiler switch on Suns. 
c     The NCPREC token defined in misc.h specifies the precision of the output netCDF dataset

#include <misc.h>
#ifndef NCPREC
Error, NCPREC must be either NF_FLOAT or NF_DOUBLE, recheck misc.h
#endif /* not NCPREC */
#include <params.h>
      subroutine netcdf(
     $     clat,
     $     cld,
     $     clon,
     $     clwp,
     $     coszrs,
     $     effcld,
     $     fice,
c     
     $     flns,
     $     flnsc,
     $     flnt,
     $     flntc,
     $     flwds,
c     
     $     fsnirt,                   ! [W m-2] Near-IR flux absorbed at TOA
     $     fsnirtsq,                 ! [W m-2] Near-IR flux absorbed at TOA >= 0.7 microns
     $     fsnrtc,                   ! [W m-2] Clear sky near-IR flux absorbed at TOA
c
     $     fsns,
     $     fsnsc,
     $     fsnt,
     $     fsntc,
     $     loctim,
c     
     $     lwup,
     $     pintm1,
     $     pmidm1,
     $     ps,
     $     o3mmr,
     $     o3vmr,
c     
     $     oro,
     $     qm1,
     $     qrl,
     $     qrs,
     $     rei,
c     
     $     rel,
     $     snowh,
     $     solin,
     $     srfrad,
     $     tg,
c     
     $     tm1,
     $     ts)

      implicit none
c     Parameters
#include <prgrid.h>
c     Commons
#include <netcdf.inc>
c     Input arguments

c     Sun-Earth Geometry
      real clat                 ! Current latitude (radians)
      real clon                 ! Centered longitude (radians)
      real coszrs               ! cosine solar zenith angle
      real loctim               ! local time of solar computation
      real oro                  ! Land/ocean/sea ice flag

c     Column thermodynamic profile
      real cld(plevp)           ! fractional cloud cover
      real clwp(plev)           ! cloud liquid water path
      real effcld(plevp)        ! effective fractional cloud cover
      real o3mmr(plev)          ! Ozone mass mixing ratio
      real o3vmr(plev)          ! Ozone volume mixing ratio
      real pintm1(plevp)        ! model interface pressures
      real pmidm1(plev)         ! model level pressures
      real ps                   ! surface pressure
      real qm1(plev)            ! model level specific humidity
      real snowh                ! snow depth (liquid water equivalent)
      real tg                   ! surface (skin) temperature
      real tm1(plev)            ! model level temperatures
      real ts                   ! surface air temperature

c     Cloud microphysics
      real fice(plev)           ! fractional amount of ice
      real rei(plev)            ! ice particle size
      real rel(plev)            ! liquid effective drop size (microns)

c     Shortwave
      real fsns                 ! surface absorbed solar flux
      real fsnsc                ! clr sky surface abs solar flux
      real fsnt                 ! total column absorbed solar flux
      real fsntc                ! clr sky total column abs solar flux
      real qrs(plev)            ! solar heating rate
      real solin                ! solar incident flux
      real srfrad               ! srf radiative heat flux
      real fsnirt               ! [W m-2] Near-IR flux absorbed at toa
      real fsnrtc               ! [W m-2] Clear sky near-IR flux absorbed at toa
      real fsnirtsq             ! [W m-2] Near-IR flux absorbed at toa >= 0.7 microns

c     Longwave
      real flns                 ! srf longwave cooling (up-dwn) flux
      real flnsc                ! clr sky lw flx at srf (up-dwn)
      real flnt                 ! net outgoing lw flx at model top
      real flntc                ! clr sky lw flx at model top
      real flwds                ! Surface down longwave flux
      real lwup                 ! Surface up longwave flux from coupler
      real qrl(plev)            ! longwave cooling rate

c     Local workspace
c     Externals

c     Local variables
      character fl_out*80
      integer nc_id             ! file handle
      integer rcd               ! return success code
      integer plev_dim_id       ! dimension id for plev
      integer plevp_dim_id      ! dimension id for plevp

#ifdef CRM_SRB
#include <crmsrb.h>
      integer bnd_dim_id        ! Dimension ID for bnd
      integer dim_plevp_bnd(2)  ! Dimension IDs
      integer dim_plev_bnd(2)  ! Dimension IDs

      integer dff_drc_SW_sfc_id
      integer dff_drc_NIR_sfc_id
      integer dff_drc_vsb_sfc_id
      integer flx_SW_dwn_sfc_id
      integer flx_NIR_dwn_sfc_id
      integer flx_vsb_dwn_sfc_id
      integer flx_SW_up_sfc_id
      integer flx_NIR_up_sfc_id
      integer flx_vsb_up_sfc_id
      integer flx_SW_dwn_drc_sfc_id
      integer flx_NIR_dwn_drc_sfc_id
      integer flx_vsb_dwn_drc_sfc_id
      integer flx_SW_dwn_dff_sfc_id
      integer flx_NIR_dwn_dff_sfc_id
      integer flx_vsb_dwn_dff_sfc_id
      integer odxc_CO2_id
      integer odxc_H2O_id
      integer odxc_O2_id
      integer odxc_O3_id
      integer odxc_Ray_id
      integer odxc_aer_id
      integer odxc_ice_id
      integer odxc_lqd_id
      integer odxc_ttl_id
      integer odxl_CO2_id
      integer odxl_H2O_id
      integer odxl_O2_id
      integer odxl_O3_id
      integer odxl_Ray_id
      integer odxl_aer_id
      integer odxl_ice_id
      integer odxl_lqd_id
      integer odxl_ttl_id
      integer bnd_id            ! Coordinate ID
      integer wvl_ctr_id        
      integer wvl_max_id
      integer wvl_min_id
      integer wvl_dlt_id
      integer wvn_id        
      integer wvn_ctr_id        
      integer wvn_max_id
      integer wvn_min_id
      integer wvn_dlt_id
      integer mpc_H2O_id        ! [kg m-2] Mass path column H2O
      integer mpc_O3_id         ! [kg m-2] Mass path column O3
      integer mpc_O3_DU_id      ! [DU] Mass path column O3 in Dobson Units
      integer alb_SW_TOA_id     ! [frc] SW albedo at TOA
      integer alb_NIR_TOA_id    ! [frc] NIR albedo at TOA
      integer alb_vsb_TOA_id    ! [frc] Visible albedo at TOA
      integer alb_SW_sfc_id     ! [frc] SW albedo at surface
      integer alb_NIR_sfc_id    ! [frc] NIR albedo at surface
      integer alb_vsb_sfc_id    ! [frc] Visible albedo at surface
      integer alb_NIR_SW_TOA_id ! [frc] NIR to SW albedo ratio at TOA
      integer alb_NIR_vsb_TOA_id ! [frc] NIR to visible albedo ratio at TOA
      integer flx_SW_dwn_TOA_id ! [W m-2] SW flux downwelling at TOA
      integer flx_NIR_dwn_TOA_id ! [W m-2] NIR flux downwelling at TOA
      integer flx_vsb_dwn_TOA_id ! [W m-2] Visible flux downwelling at TOA
      integer flx_SW_up_TOA_id  ! [W m-2] SW flux upwelling at TOA
      integer flx_NIR_up_TOA_id ! [W m-2] NIR flux upwelling at TOA
      integer flx_vsb_up_TOA_id ! [W m-2] Visible flux upwelling at TOA
      integer flx_bnd_dwn_TOA_id ! [W m-2] Downwelling band flux at TOA
      integer flx_bnd_dwn_dff_sfc_id ! [W m-2] Downwelling diffuse field band flux at surface
      integer flx_bnd_dwn_drc_sfc_id ! [W m-2] Downwelling direct beam band flux at surface
      integer flx_bnd_dwn_sfc_id ! [W m-2] Downwelling band flux at surface
      integer flx_bnd_up_TOA_id ! [W m-2] Upwelling band flux at TOA
      integer flx_bnd_up_sfc_id ! [W m-2] Upwelling band flux at surface
      integer dff_drc_SW_id ! [W m-2] Diffuse/direct SW downwelling flux ratio
      integer flx_SW_dwn_id ! [W m-2] Downwelling SW flux
      integer flx_SW_dwn_dff_id ! [W m-2] Downwelling SW flux diffuse field
      integer flx_SW_dwn_drc_id ! [W m-2] Downwelling SW flux direct beam
      integer flx_SW_up_id ! [W m-2] Upwelling SW flux
      integer flx_LW_dwn_id ! [W m-2] Downwelling LW flux
      integer flx_LW_up_id ! [W m-2] Upwelling LW flux
      integer flx_bnd_dwn_dff_id ! [W m-2] Downwelling diffuse field band flux
      integer flx_bnd_dwn_drc_id ! [W m-2] Downwelling direct beam band flux
      integer flx_bnd_dwn_id ! [W m-2] Downwelling band flux
      integer flx_bnd_up_id ! [W m-2] Upwelling band flux
#endif /* end CRM_SRB */

      integer clat_id   
      integer clon_id   
      integer cld_id   
      integer clwp_id
      integer coszrs_id
      integer effcld_id
      integer fice_id
      integer flns_id
      integer flnsc_id
      integer flnt_id
      integer flntc_id
      integer flwds_id
      integer fsnirt_id
      integer fsnrtc_id
      integer fsnirtsq_id
      integer fsns_id
      integer fsnsc_id
      integer fsnt_id
      integer fsntc_id
      integer oro_id
      integer loctim_id
      integer lwup_id
      integer o3mmr_id
      integer o3vmr_id
      integer pintm1_id
      integer plev_id
      integer plevp_id
      integer pmidm1_id
      integer ps_id
      integer qm1_id
      integer qrl_id
      integer qrs_id
      integer rei_id
      integer rel_id
      integer snowh_id
      integer solin_id
      integer srfrad_id
      integer tg_id
      integer tm1_id
      integer ts_id

c     Main Code
      rcd=NF_NOERR              ! NF_NOERR == 0
      fl_out='crm.nc'
      rcd=rcd+nf_create(fl_out,NF_CLOBBER,nc_id)

c     Define dimensions
      rcd=rcd+nf_def_dim(nc_id,'plev',plev,plev_dim_id)
      rcd=rcd+nf_def_dim(nc_id,'plevp',plevp,plevp_dim_id)

c     Define scalars
      rcd=rcd+nf_def_var(nc_id,'clat',NCPREC,0,0,clat_id)
      rcd=rcd+nf_def_var(nc_id,'clon',NCPREC,0,0,clon_id)
      rcd=rcd+nf_def_var(nc_id,'coszrs',NCPREC,0,0,coszrs_id)
      rcd=rcd+nf_def_var(nc_id,'flns',NCPREC,0,0,flns_id)
      rcd=rcd+nf_def_var(nc_id,'flnsc',NCPREC,0,0,flnsc_id)
      rcd=rcd+nf_def_var(nc_id,'flnt',NCPREC,0,0,flnt_id)
      rcd=rcd+nf_def_var(nc_id,'flntc',NCPREC,0,0,flntc_id)
      rcd=rcd+nf_def_var(nc_id,'flwds',NCPREC,0,0,flwds_id)
      rcd=rcd+nf_def_var(nc_id,'fsnirt',NCPREC,0,0,fsnirt_id)
      rcd=rcd+nf_def_var(nc_id,'fsnirtsq',NCPREC,0,0,fsnirtsq_id)
      rcd=rcd+nf_def_var(nc_id,'fsnrtc',NCPREC,0,0,fsnrtc_id)
      rcd=rcd+nf_def_var(nc_id,'fsns',NCPREC,0,0,fsns_id)
      rcd=rcd+nf_def_var(nc_id,'fsnsc',NCPREC,0,0,fsnsc_id)
      rcd=rcd+nf_def_var(nc_id,'fsnt',NCPREC,0,0,fsnt_id)
      rcd=rcd+nf_def_var(nc_id,'fsntc',NCPREC,0,0,fsntc_id)
      rcd=rcd+nf_def_var(nc_id,'oro',NCPREC,0,0,oro_id)
      rcd=rcd+nf_def_var(nc_id,'loctim',NCPREC,0,0,loctim_id)
      rcd=rcd+nf_def_var(nc_id,'lwup',NCPREC,0,0,lwup_id)
      rcd=rcd+nf_def_var(nc_id,'ps',NCPREC,0,0,ps_id)
      rcd=rcd+nf_def_var(nc_id,'snowh',NCPREC,0,0,snowh_id)
      rcd=rcd+nf_def_var(nc_id,'solin',NCPREC,0,0,solin_id)
      rcd=rcd+nf_def_var(nc_id,'srfrad',NCPREC,0,0,srfrad_id)
      rcd=rcd+nf_def_var(nc_id,'tg',NCPREC,0,0,tg_id)
      rcd=rcd+nf_def_var(nc_id,'ts',NCPREC,0,0,ts_id)

c     Define plev arrays
      rcd=rcd+nf_def_var(nc_id,'clwp',NCPREC,1,plev_dim_id,clwp_id)
      rcd=rcd+nf_def_var(nc_id,'fice',NCPREC,1,plev_dim_id,fice_id)
      rcd=rcd+nf_def_var(nc_id,'o3mmr',NCPREC,1,plev_dim_id,o3mmr_id)
      rcd=rcd+nf_def_var(nc_id,'o3vmr',NCPREC,1,plev_dim_id,o3vmr_id)
      rcd=rcd+nf_def_var(nc_id,'plev',NCPREC,1,plev_dim_id,plev_id)
      rcd=rcd+nf_def_var(nc_id,'pmid',NCPREC,1,plev_dim_id,pmidm1_id)
      rcd=rcd+nf_def_var(nc_id,'h2ommr',NCPREC,1,plev_dim_id,qm1_id)
      rcd=rcd+nf_def_var(nc_id,'qrl',NCPREC,1,plev_dim_id,qrl_id)
      rcd=rcd+nf_def_var(nc_id,'qrs',NCPREC,1,plev_dim_id,qrs_id)
      rcd=rcd+nf_def_var(nc_id,'rei',NCPREC,1,plev_dim_id,rei_id)
      rcd=rcd+nf_def_var(nc_id,'rel',NCPREC,1,plev_dim_id,rel_id)
      rcd=rcd+nf_def_var(nc_id,'t',NCPREC,1,plev_dim_id,tm1_id)

c     Define plevp arrays
      rcd=rcd+nf_def_var(nc_id,'cld',NCPREC,1,plevp_dim_id,cld_id)
      rcd=rcd+nf_def_var(nc_id,'effcld',NCPREC,1,plevp_dim_id,effcld_id)
      rcd=rcd+nf_def_var(nc_id,'pint',NCPREC,1,plevp_dim_id,pintm1_id)
      rcd=rcd+nf_def_var(nc_id,'plevp',NCPREC,1,plevp_dim_id,plevp_id)

c     Add units
      rcd=rcd+nf_put_att_text(nc_id,clat_id,'units',6,'radian')
      rcd=rcd+nf_put_att_text(nc_id,clon_id,'units',6,'radian')
      rcd=rcd+nf_put_att_text(nc_id,cld_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,clwp_id,'units',12,'gram meter-2')
      rcd=rcd+nf_put_att_text(nc_id,coszrs_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,effcld_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,fice_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,flns_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flnsc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flnt_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flntc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flwds_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,fsnirt_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,fsnirtsq_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,fsnrtc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,fsns_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,fsnsc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,fsnt_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,fsntc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,oro_id,'units',5,'index')
      rcd=rcd+nf_put_att_text(nc_id,loctim_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,lwup_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,o3mmr_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,o3vmr_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,pintm1_id,'units',6,'pascal')
      rcd=rcd+nf_put_att_text(nc_id,plev_id,'units',6,'pascal')
      rcd=rcd+nf_put_att_text(nc_id,plevp_id,'units',6,'pascal')
      rcd=rcd+nf_put_att_text(nc_id,pmidm1_id,'units',6,'pascal')
      rcd=rcd+nf_put_att_text(nc_id,ps_id,'units',6,'pascal')
      rcd=rcd+nf_put_att_text(nc_id,qm1_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,qrl_id,'units',15,'kelvin second-1')
      rcd=rcd+nf_put_att_text(nc_id,qrs_id,'units',15,'kelvin second-1')
      rcd=rcd+nf_put_att_text(nc_id,rei_id,'units',6,'micron')
      rcd=rcd+nf_put_att_text(nc_id,rel_id,'units',6,'micron')
      rcd=rcd+nf_put_att_text(nc_id,snowh_id,'units',5,'meter')
      rcd=rcd+nf_put_att_text(nc_id,solin_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,srfrad_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,tg_id,'units',6,'kelvin')
      rcd=rcd+nf_put_att_text(nc_id,tm1_id,'units',6,'kelvin')
      rcd=rcd+nf_put_att_text(nc_id,ts_id,'units',6,'kelvin')

c     Add English text descriptions
      rcd=rcd+nf_put_att_text(nc_id,clat_id,'long_name',8,'Latitude')
      rcd=rcd+nf_put_att_text(nc_id,clon_id,'long_name',8,'Longitude')
      rcd=rcd+nf_put_att_text(nc_id,cld_id,'long_name',14,'Cloud fraction')
      rcd=rcd+nf_put_att_text(nc_id,clwp_id,'long_name',24,'In-cloud condensate path')
      rcd=rcd+nf_put_att_text(nc_id,coszrs_id,'long_name',25,'Cosine solar zenith angle')
      rcd=rcd+nf_put_att_text(nc_id,effcld_id,'long_name',52,'Effective cloud amount (emissivity x cloud fraction)')
      rcd=rcd+nf_put_att_text(nc_id,fice_id,'long_name',12,'Ice fraction')
      rcd=rcd+nf_put_att_text(nc_id,flns_id,'long_name',37,'Net longwave flux at surface')
      rcd=rcd+nf_put_att_text(nc_id,flnsc_id,'long_name',40,'Net longwave flux at surface')
      rcd=rcd+nf_put_att_text(nc_id,flnt_id,'long_name',33,'Net longwave flux at TOA')
      rcd=rcd+nf_put_att_text(nc_id,flntc_id,'long_name',35,'Net longwave flux at TOA')
      rcd=rcd+nf_put_att_text(nc_id,flwds_id,'long_name',36,'Longwave downwelling flux at surface')
      rcd=rcd+nf_put_att_text(nc_id,fsnirt_id,'long_name',28,'Near-IR flux absorbed at TOA')
      rcd=rcd+nf_put_att_text(nc_id,fsnirtsq_id,'long_name',43,'Near-IR flux absorbed at TOA >= 0.7 microns')
      rcd=rcd+nf_put_att_text(nc_id,fsnrtc_id,'long_name',38,'Clear sky near-IR flux absorbed at TOA')
      rcd=rcd+nf_put_att_text(nc_id,fsns_id,'long_name',38,'Net shortwave flux at surface')
      rcd=rcd+nf_put_att_text(nc_id,fsnsc_id,'long_name',40,'Net shortwave flux at surface')
      rcd=rcd+nf_put_att_text(nc_id,fsnt_id,'long_name',34,'Net shortwave flux at TOA')
      rcd=rcd+nf_put_att_text(nc_id,fsntc_id,'long_name',30,'Net shortwave flux at TOA')
      rcd=rcd+nf_put_att_text(nc_id,oro_id,'long_name',14,'Orography flag')
      rcd=rcd+nf_put_att_text(nc_id,loctim_id,'long_name',10,'Local time')
      rcd=rcd+nf_put_att_text(nc_id,lwup_id,'long_name',38,'Longwave up flux from coupler (if any)')
      rcd=rcd+nf_put_att_text(nc_id,o3mmr_id,'long_name',27,'Ozone dry mass-mixing ratio')
      rcd=rcd+nf_put_att_text(nc_id,o3vmr_id,'long_name',29,'Ozone dry volume-mixing ratio')
      rcd=rcd+nf_put_att_text(nc_id,pintm1_id,'long_name',28,'Pressure at layer interfaces')
      rcd=rcd+nf_put_att_text(nc_id,plev_id,'long_name',16,'Number of levels')
      rcd=rcd+nf_put_att_text(nc_id,plevp_id,'long_name',26,'Number of level interfaces')
      rcd=rcd+nf_put_att_text(nc_id,pmidm1_id,'long_name',27,'Pressure at layer midpoints')
      rcd=rcd+nf_put_att_text(nc_id,ps_id,'long_name',16,'Surface pressure')
      rcd=rcd+nf_put_att_text(nc_id,qm1_id,'long_name',33,'Water vapor dry mass-mixing ratio')
      rcd=rcd+nf_put_att_text(nc_id,qrl_id,'long_name',21,'Longwave heating rate')
      rcd=rcd+nf_put_att_text(nc_id,qrs_id,'long_name',22,'Shortwave heating rate')
      rcd=rcd+nf_put_att_text(nc_id,rei_id,'long_name',32,'Effective radius of ice crystals')
      rcd=rcd+nf_put_att_text(nc_id,rel_id,'long_name',35,'Effective radius of liquid droplets')
      rcd=rcd+nf_put_att_text(nc_id,snowh_id,'long_name',10,'Snow depth')
      rcd=rcd+nf_put_att_text(nc_id,solin_id,'long_name',33,'Downwelling shortwave flux at TOA')
      rcd=rcd+nf_put_att_text(nc_id,srfrad_id,'long_name',33,'Total radiative flux into surface')
      rcd=rcd+nf_put_att_text(nc_id,tg_id,'long_name',18,'Ground temperature')
      rcd=rcd+nf_put_att_text(nc_id,tm1_id,'long_name',11,'Temperature')
      rcd=rcd+nf_put_att_text(nc_id,ts_id,'long_name',16,'Skin temperature')

c     End define mode
      rcd=rcd+nf_enddef(nc_id)

c     Write data
      rcd=rcd+nf_put_var_double(nc_id,clat_id,clat)
      rcd=rcd+nf_put_var_double(nc_id,clon_id,clon)
      rcd=rcd+nf_put_var_double(nc_id,cld_id,cld)
      rcd=rcd+nf_put_var_double(nc_id,clwp_id,clwp)
      rcd=rcd+nf_put_var_double(nc_id,coszrs_id,coszrs)
      rcd=rcd+nf_put_var_double(nc_id,effcld_id,effcld)
      rcd=rcd+nf_put_var_double(nc_id,fice_id,fice)
      rcd=rcd+nf_put_var_double(nc_id,flns_id,flns)
      rcd=rcd+nf_put_var_double(nc_id,flnsc_id,flnsc)
      rcd=rcd+nf_put_var_double(nc_id,flnt_id,flnt)
      rcd=rcd+nf_put_var_double(nc_id,flntc_id,flntc)
      rcd=rcd+nf_put_var_double(nc_id,flwds_id,flwds)
      rcd=rcd+nf_put_var_double(nc_id,fsnirt_id,fsnirt)
      rcd=rcd+nf_put_var_double(nc_id,fsnirtsq_id,fsnirtsq)
      rcd=rcd+nf_put_var_double(nc_id,fsnrtc_id,fsnrtc)
      rcd=rcd+nf_put_var_double(nc_id,fsns_id,fsns)
      rcd=rcd+nf_put_var_double(nc_id,fsnsc_id,fsnsc)
      rcd=rcd+nf_put_var_double(nc_id,fsnt_id,fsnt)
      rcd=rcd+nf_put_var_double(nc_id,fsntc_id,fsntc)
      rcd=rcd+nf_put_var_double(nc_id,loctim_id,loctim)
      rcd=rcd+nf_put_var_double(nc_id,lwup_id,lwup)
      rcd=rcd+nf_put_var_double(nc_id,o3mmr_id,o3mmr)
      rcd=rcd+nf_put_var_double(nc_id,o3vmr_id,o3vmr)
      rcd=rcd+nf_put_var_double(nc_id,oro_id,oro)
      rcd=rcd+nf_put_var_double(nc_id,pintm1_id,pintm1)
      rcd=rcd+nf_put_var_double(nc_id,plev_id,pmidm1)
      rcd=rcd+nf_put_var_double(nc_id,plevp_id,pintm1)
      rcd=rcd+nf_put_var_double(nc_id,pmidm1_id,pmidm1)
      rcd=rcd+nf_put_var_double(nc_id,ps_id,ps)
      rcd=rcd+nf_put_var_double(nc_id,qm1_id,qm1)
      rcd=rcd+nf_put_var_double(nc_id,qrl_id,qrl)
      rcd=rcd+nf_put_var_double(nc_id,qrs_id,qrs)
      rcd=rcd+nf_put_var_double(nc_id,rei_id,rei)
      rcd=rcd+nf_put_var_double(nc_id,rel_id,rel)
      rcd=rcd+nf_put_var_double(nc_id,snowh_id,snowh)
      rcd=rcd+nf_put_var_double(nc_id,solin_id,solin)
      rcd=rcd+nf_put_var_double(nc_id,srfrad_id,srfrad)
      rcd=rcd+nf_put_var_double(nc_id,tg_id,tg)
      rcd=rcd+nf_put_var_double(nc_id,tm1_id,tm1)
      rcd=rcd+nf_put_var_double(nc_id,ts_id,ts)

      rcd=rcd+nf_close(nc_id)
      write(6,'(2a)') 'Wrote results to ',fl_out

#ifdef CRM_SRB
      rcd=rcd+nf_open(fl_out,NF_WRITE,nc_id)
      if (rcd.ne.NF_NOERR) call nc_err_exit(rcd,fl_out)

c     Put output file in define mode 
      rcd=rcd+nf_redef(nc_id)
      if (rcd.ne.NF_NOERR) call nc_err_exit(rcd,fl_out)

c     Define dimension IDs
      rcd=rcd+nf_def_dim(nc_id,'bnd',bnd_nbr_SW,bnd_dim_id)
      if (rcd.ne.NF_NOERR) call nc_err_exit(rcd,fl_out)

c     Assemble each multidimensional combination of dimension IDs
      dim_plevp_bnd=(/plevp_dim_id,bnd_dim_id/)
      dim_plev_bnd=(/plev_dim_id,bnd_dim_id/)

c     Define scalars
      rcd=rcd+nf_def_var(nc_id,'alb_SW_sfc',NCPREC,0,0,alb_SW_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'alb_vsb_sfc',NCPREC,0,0,alb_vsb_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'alb_NIR_sfc',NCPREC,0,0,alb_NIR_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'alb_NIR_SW_TOA',NCPREC,0,0,alb_NIR_SW_TOA_id)
      rcd=rcd+nf_def_var(nc_id,'alb_NIR_TOA',NCPREC,0,0,alb_NIR_TOA_id)
      rcd=rcd+nf_def_var(nc_id,'alb_NIR_vsb_TOA',NCPREC,0,0,alb_NIR_vsb_TOA_id)
      rcd=rcd+nf_def_var(nc_id,'alb_SW_TOA',NCPREC,0,0,alb_SW_TOA_id)
      rcd=rcd+nf_def_var(nc_id,'alb_vsb_TOA',NCPREC,0,0,alb_vsb_TOA_id)
      rcd=rcd+nf_def_var(nc_id,'dff_drc_NIR_sfc',NCPREC,0,0,dff_drc_NIR_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'dff_drc_SW_sfc',NCPREC,0,0,dff_drc_SW_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'dff_drc_vsb_sfc',NCPREC,0,0,dff_drc_vsb_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'flx_NIR_dwn_TOA',NCPREC,0,0,flx_NIR_dwn_TOA_id)
      rcd=rcd+nf_def_var(nc_id,'flx_NIR_dwn_dff_sfc',NCPREC,0,0,flx_NIR_dwn_dff_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'flx_NIR_dwn_drc_sfc',NCPREC,0,0,flx_NIR_dwn_drc_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'flx_NIR_dwn_sfc',NCPREC,0,0,flx_NIR_dwn_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'flx_NIR_up_TOA',NCPREC,0,0,flx_NIR_up_TOA_id)
      rcd=rcd+nf_def_var(nc_id,'flx_SW_dwn_TOA',NCPREC,0,0,flx_SW_dwn_TOA_id)
      rcd=rcd+nf_def_var(nc_id,'flx_SW_dwn_dff_sfc',NCPREC,0,0,flx_SW_dwn_dff_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'flx_SW_dwn_drc_sfc',NCPREC,0,0,flx_SW_dwn_drc_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'flx_SW_dwn_sfc',NCPREC,0,0,flx_SW_dwn_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'flx_SW_up_sfc',NCPREC,0,0,flx_SW_up_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'flx_NIR_up_sfc',NCPREC,0,0,flx_NIR_up_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'flx_vsb_up_sfc',NCPREC,0,0,flx_vsb_up_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'flx_SW_up_TOA',NCPREC,0,0,flx_SW_up_TOA_id)
      rcd=rcd+nf_def_var(nc_id,'flx_vsb_dwn_TOA',NCPREC,0,0,flx_vsb_dwn_TOA_id)
      rcd=rcd+nf_def_var(nc_id,'flx_vsb_dwn_dff_sfc',NCPREC,0,0,flx_vsb_dwn_dff_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'flx_vsb_dwn_drc_sfc',NCPREC,0,0,flx_vsb_dwn_drc_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'flx_vsb_dwn_sfc',NCPREC,0,0,flx_vsb_dwn_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'flx_vsb_up_TOA',NCPREC,0,0,flx_vsb_up_TOA_id)
      rcd=rcd+nf_def_var(nc_id,'mpc_H2O',NCPREC,0,0,mpc_H2O_id)
      rcd=rcd+nf_def_var(nc_id,'mpc_O3',NCPREC,0,0,mpc_O3_id)
      rcd=rcd+nf_def_var(nc_id,'mpc_O3_DU',NCPREC,0,0,mpc_O3_DU_id)
c     Define plevp arrays
      rcd=rcd+nf_def_var(nc_id,'dff_drc_SW',NCPREC,1,plevp_dim_id,dff_drc_SW_id)
      rcd=rcd+nf_def_var(nc_id,'flx_SW_dwn',NCPREC,1,plevp_dim_id,flx_SW_dwn_id)
      rcd=rcd+nf_def_var(nc_id,'flx_SW_dwn_dff',NCPREC,1,plevp_dim_id,flx_SW_dwn_dff_id)
      rcd=rcd+nf_def_var(nc_id,'flx_SW_dwn_drc',NCPREC,1,plevp_dim_id,flx_SW_dwn_drc_id)
      rcd=rcd+nf_def_var(nc_id,'flx_SW_up',NCPREC,1,plevp_dim_id,flx_SW_up_id)
      rcd=rcd+nf_def_var(nc_id,'flx_LW_dwn',NCPREC,1,plevp_dim_id,flx_LW_dwn_id)
      rcd=rcd+nf_def_var(nc_id,'flx_LW_up',NCPREC,1,plevp_dim_id,flx_LW_up_id)

c     Define bnd arrays
      rcd=rcd+nf_def_var(nc_id,'bnd',NCPREC,1,bnd_dim_id,bnd_id)
      rcd=rcd+nf_def_var(nc_id,'flx_bnd_dwn_TOA',NCPREC,1,bnd_dim_id,flx_bnd_dwn_TOA_id)
      rcd=rcd+nf_def_var(nc_id,'flx_bnd_dwn_dff_sfc',NCPREC,1,bnd_dim_id,flx_bnd_dwn_dff_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'flx_bnd_dwn_drc_sfc',NCPREC,1,bnd_dim_id,flx_bnd_dwn_drc_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'flx_bnd_dwn_sfc',NCPREC,1,bnd_dim_id,flx_bnd_dwn_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'flx_bnd_up_TOA',NCPREC,1,bnd_dim_id,flx_bnd_up_TOA_id)
      rcd=rcd+nf_def_var(nc_id,'flx_bnd_up_sfc',NCPREC,1,bnd_dim_id,flx_bnd_up_sfc_id)
      rcd=rcd+nf_def_var(nc_id,'flx_bnd_dwn_dff',NCPREC,2,dim_plevp_bnd,flx_bnd_dwn_dff_id)
      rcd=rcd+nf_def_var(nc_id,'flx_bnd_dwn_drc',NCPREC,2,dim_plevp_bnd,flx_bnd_dwn_drc_id)
      rcd=rcd+nf_def_var(nc_id,'flx_bnd_dwn',NCPREC,2,dim_plevp_bnd,flx_bnd_dwn_id)
      rcd=rcd+nf_def_var(nc_id,'flx_bnd_up',NCPREC,2,dim_plevp_bnd,flx_bnd_up_id)
      rcd=rcd+nf_def_var(nc_id,'odxc_CO2',NCPREC,1,bnd_dim_id,odxc_CO2_id)
      rcd=rcd+nf_def_var(nc_id,'odxc_H2O',NCPREC,1,bnd_dim_id,odxc_H2O_id)
      rcd=rcd+nf_def_var(nc_id,'odxc_O2',NCPREC,1,bnd_dim_id,odxc_O2_id)
      rcd=rcd+nf_def_var(nc_id,'odxc_O3',NCPREC,1,bnd_dim_id,odxc_O3_id)
      rcd=rcd+nf_def_var(nc_id,'odxc_Ray',NCPREC,1,bnd_dim_id,odxc_Ray_id)
      rcd=rcd+nf_def_var(nc_id,'odxc_aer',NCPREC,1,bnd_dim_id,odxc_aer_id)
      rcd=rcd+nf_def_var(nc_id,'odxc_ice',NCPREC,1,bnd_dim_id,odxc_ice_id)
      rcd=rcd+nf_def_var(nc_id,'odxc_lqd',NCPREC,1,bnd_dim_id,odxc_lqd_id)
      rcd=rcd+nf_def_var(nc_id,'odxc_ttl',NCPREC,1,bnd_dim_id,odxc_ttl_id)
      rcd=rcd+nf_def_var(nc_id,'odxl_CO2',NCPREC,2,dim_plev_bnd,odxl_CO2_id)
      rcd=rcd+nf_def_var(nc_id,'odxl_H2O',NCPREC,2,dim_plev_bnd,odxl_H2O_id)
      rcd=rcd+nf_def_var(nc_id,'odxl_O2',NCPREC,2,dim_plev_bnd,odxl_O2_id)
      rcd=rcd+nf_def_var(nc_id,'odxl_O3',NCPREC,2,dim_plev_bnd,odxl_O3_id)
      rcd=rcd+nf_def_var(nc_id,'odxl_Ray',NCPREC,2,dim_plev_bnd,odxl_Ray_id)
      rcd=rcd+nf_def_var(nc_id,'odxl_aer',NCPREC,2,dim_plev_bnd,odxl_aer_id)
      rcd=rcd+nf_def_var(nc_id,'odxl_ice',NCPREC,2,dim_plev_bnd,odxl_ice_id)
      rcd=rcd+nf_def_var(nc_id,'odxl_lqd',NCPREC,2,dim_plev_bnd,odxl_lqd_id)
      rcd=rcd+nf_def_var(nc_id,'odxl_ttl',NCPREC,2,dim_plev_bnd,odxl_ttl_id)
      rcd=rcd+nf_def_var(nc_id,'wvl_ctr',NCPREC,1,bnd_dim_id,wvl_ctr_id)
      rcd=rcd+nf_def_var(nc_id,'wvl_dlt',NCPREC,1,bnd_dim_id,wvl_dlt_id)
      rcd=rcd+nf_def_var(nc_id,'wvl_max',NCPREC,1,bnd_dim_id,wvl_max_id)
      rcd=rcd+nf_def_var(nc_id,'wvl_min',NCPREC,1,bnd_dim_id,wvl_min_id)
      rcd=rcd+nf_def_var(nc_id,'wvn',NCPREC,1,bnd_dim_id,wvn_id)
      rcd=rcd+nf_def_var(nc_id,'wvn_ctr',NCPREC,1,bnd_dim_id,wvn_ctr_id)
      rcd=rcd+nf_def_var(nc_id,'wvn_dlt',NCPREC,1,bnd_dim_id,wvn_dlt_id)
      rcd=rcd+nf_def_var(nc_id,'wvn_max',NCPREC,1,bnd_dim_id,wvn_max_id)
      rcd=rcd+nf_def_var(nc_id,'wvn_min',NCPREC,1,bnd_dim_id,wvn_min_id)

c     Add units

      rcd=rcd+nf_put_att_text(nc_id,alb_NIR_SW_TOA_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,alb_NIR_TOA_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,alb_NIR_sfc_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,alb_NIR_vsb_TOA_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,alb_SW_TOA_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,alb_SW_sfc_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,alb_vsb_TOA_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,alb_vsb_sfc_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,bnd_id,'units',5,'meter')
      rcd=rcd+nf_put_att_text(nc_id,dff_drc_NIR_sfc_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,dff_drc_SW_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,dff_drc_SW_sfc_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,dff_drc_vsb_sfc_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,flx_LW_dwn_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_LW_up_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_NIR_dwn_TOA_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_NIR_dwn_dff_sfc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_NIR_dwn_drc_sfc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_NIR_dwn_sfc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_NIR_up_TOA_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_dwn_TOA_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_dwn_dff_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_dwn_dff_sfc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_dwn_drc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_dwn_drc_sfc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_dwn_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_dwn_sfc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_up_sfc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_NIR_up_sfc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_vsb_up_sfc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_up_TOA_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_up_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_dwn_TOA_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_dwn_dff_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_dwn_dff_sfc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_dwn_drc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_dwn_drc_sfc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_dwn_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_dwn_sfc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_up_TOA_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_up_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_up_sfc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_vsb_dwn_TOA_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_vsb_dwn_dff_sfc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_vsb_dwn_drc_sfc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_vsb_dwn_sfc_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,flx_vsb_up_TOA_id,'units',12,'watt meter-2')
      rcd=rcd+nf_put_att_text(nc_id,mpc_H2O_id,'units',16,'kilogram meter-2')
      rcd=rcd+nf_put_att_text(nc_id,mpc_O3_DU_id,'units',6,'Dobson')
      rcd=rcd+nf_put_att_text(nc_id,mpc_O3_id,'units',16,'kilogram meter-2')
      rcd=rcd+nf_put_att_text(nc_id,odxc_CO2_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,odxc_H2O_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,odxc_O2_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,odxc_O3_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,odxc_Ray_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,odxc_aer_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,odxc_ice_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,odxc_lqd_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,odxc_ttl_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,odxl_CO2_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,odxl_H2O_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,odxl_O2_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,odxl_O3_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,odxl_Ray_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,odxl_aer_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,odxl_ice_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,odxl_lqd_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,odxl_ttl_id,'units',8,'fraction')
      rcd=rcd+nf_put_att_text(nc_id,wvl_ctr_id,'units',5,'meter')
      rcd=rcd+nf_put_att_text(nc_id,wvl_ctr_id,'units',5,'meter')
      rcd=rcd+nf_put_att_text(nc_id,wvl_dlt_id,'units',5,'meter')
      rcd=rcd+nf_put_att_text(nc_id,wvl_max_id,'units',5,'meter')
      rcd=rcd+nf_put_att_text(nc_id,wvl_min_id,'units',5,'meter')
      rcd=rcd+nf_put_att_text(nc_id,wvn_ctr_id,'units',12,'centimeter-1')
      rcd=rcd+nf_put_att_text(nc_id,wvn_dlt_id,'units',12,'centimeter-1')
      rcd=rcd+nf_put_att_text(nc_id,wvn_id,'units',12,'centimeter-1')
      rcd=rcd+nf_put_att_text(nc_id,wvn_max_id,'units',12,'centimeter-1')
      rcd=rcd+nf_put_att_text(nc_id,wvn_min_id,'units',12,'centimeter-1')

c     Add English text descriptions
      rcd=rcd+nf_put_att_text(nc_id,alb_NIR_SW_TOA_id,'long_name',29,'Ratio of NIR to SW TOA albedo')
      rcd=rcd+nf_put_att_text(nc_id,alb_NIR_TOA_id,'long_name',14,'NIR TOA albedo')
      rcd=rcd+nf_put_att_text(nc_id,alb_NIR_sfc_id,'long_name',18,'NIR surface albedo')
      rcd=rcd+nf_put_att_text(nc_id,alb_NIR_vsb_TOA_id,'long_name',34,'Ratio of NIR to visible TOA albedo')
      rcd=rcd+nf_put_att_text(nc_id,alb_SW_TOA_id,'long_name',20,'Shortwave TOA albedo')
      rcd=rcd+nf_put_att_text(nc_id,alb_SW_sfc_id,'long_name',24,'Shortwave surface albedo')
      rcd=rcd+nf_put_att_text(nc_id,alb_vsb_TOA_id,'long_name',18,'Visible TOA albedo')
      rcd=rcd+nf_put_att_text(nc_id,alb_vsb_sfc_id,'long_name',22,'Visible surface albedo')
      rcd=rcd+nf_put_att_text(nc_id,bnd_id,'long_name',22,'Band nominal wavelength')
      rcd=rcd+nf_put_att_text(nc_id,dff_drc_NIR_sfc_id,'long_name',52,'Diffuse/direct NIR downwelling flux ratio at surface')
      rcd=rcd+nf_put_att_text(nc_id,dff_drc_SW_id,'long_name',40,'Diffuse/direct SW downwelling flux ratio')
      rcd=rcd+nf_put_att_text(nc_id,dff_drc_SW_sfc_id,'long_name',51,'Diffuse/direct SW downwelling flux ratio at surface')
      rcd=rcd+nf_put_att_text(nc_id,dff_drc_vsb_sfc_id,'long_name',56,'Diffuse/direct visible downwelling flux ratio at surface')
      rcd=rcd+nf_put_att_text(nc_id,flx_LW_dwn_id,'long_name',19,'Downwelling LW flux')
      rcd=rcd+nf_put_att_text(nc_id,flx_LW_up_id,'long_name',16,'Upwelling LW flux')
      rcd=rcd+nf_put_att_text(nc_id,flx_NIR_dwn_TOA_id,'long_name',27,'Downwelling NIR flux at TOA')
      rcd=rcd+nf_put_att_text(nc_id,flx_NIR_dwn_dff_sfc_id,'long_name',47,'NIR flux downwelling at surface in diffuse beam')
      rcd=rcd+nf_put_att_text(nc_id,flx_NIR_dwn_drc_sfc_id,'long_name',46,'NIR flux downwelling at surface in direct beam')
      rcd=rcd+nf_put_att_text(nc_id,flx_NIR_dwn_sfc_id,'long_name',31,'NIR flux downwelling at surface')
      rcd=rcd+nf_put_att_text(nc_id,flx_NIR_up_TOA_id,'long_name',25,'Upwelling NIR flux at TOA')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_dwn_TOA_id,'long_name',26,'Downwelling SW flux at TOA')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_dwn_dff_id,'long_name',32,'Downwelling SW flux diffuse field')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_dwn_dff_sfc_id,'long_name',46,'SW flux downwelling at surface in diffuse beam')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_dwn_drc_id,'long_name',31,'Downwelling SW flux direct beam')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_dwn_drc_sfc_id,'long_name',45,'SW flux downwelling at surface in direct beam')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_dwn_id,'long_name',19,'Downwelling SW flux')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_dwn_sfc_id,'long_name',30,'SW flux downwelling at surface')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_up_sfc_id,'long_name',27,'Upwelling SW flux at surface')
      rcd=rcd+nf_put_att_text(nc_id,flx_NIR_up_sfc_id,'long_name',28,'Upwelling NIR flux at surface')
      rcd=rcd+nf_put_att_text(nc_id,flx_vsb_up_sfc_id,'long_name',32,'Upwelling visible flux at surface')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_up_TOA_id,'long_name',24,'Upwelling SW flux at TOA')
      rcd=rcd+nf_put_att_text(nc_id,flx_SW_up_id,'long_name',16,'Upwelling SW flux')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_dwn_TOA_id,'long_name',23,'Downwelling flux at TOA')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_dwn_dff_id,'long_name',35,'Downwelling diffuse field band flux')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_dwn_dff_sfc_id,'long_name',39,'Downwelling diffuse field flux at surface')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_dwn_drc_id,'long_name',33,'Downwelling direct beam band flux')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_dwn_drc_sfc_id,'long_name',39,'Downwelling direct beam flux at surface')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_dwn_id,'long_name',21,'Downwelling band flux')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_dwn_sfc_id,'long_name',27,'Downwelling flux at surface')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_up_TOA_id,'long_name',21,'Upwelling flux at TOA')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_up_id,'long_name',19,'Upwelling band flux')
      rcd=rcd+nf_put_att_text(nc_id,flx_bnd_up_sfc_id,'long_name',25,'Upwelling flux at surface')
      rcd=rcd+nf_put_att_text(nc_id,flx_vsb_dwn_TOA_id,'long_name',31,'Downwelling visible flux at TOA')
      rcd=rcd+nf_put_att_text(nc_id,flx_vsb_dwn_dff_sfc_id,'long_name',51,'Visible flux downwelling at surface in diffuse beam')
      rcd=rcd+nf_put_att_text(nc_id,flx_vsb_dwn_drc_sfc_id,'long_name',50,'Visible flux downwelling at surface in direct beam')
      rcd=rcd+nf_put_att_text(nc_id,flx_vsb_dwn_sfc_id,'long_name',35,'Visible flux downwelling at surface')
      rcd=rcd+nf_put_att_text(nc_id,flx_vsb_up_TOA_id,'long_name',29,'Upwelling visible flux at TOA')
      rcd=rcd+nf_put_att_text(nc_id,mpc_H2O_id,'long_name',26,'Mass path of H2O in column')
      rcd=rcd+nf_put_att_text(nc_id,mpc_O3_DU_id,'long_name',25,'Mass path of O3 in column')
      rcd=rcd+nf_put_att_text(nc_id,mpc_O3_id,'long_name',25,'Mass path of O3 in column')
      rcd=rcd+nf_put_att_text(nc_id,odxc_CO2_id,'long_name',38,'Column extinction optical depth of CO2')
      rcd=rcd+nf_put_att_text(nc_id,odxc_H2O_id,'long_name',46,'Column extinction optical depth of gaseous H2O')
      rcd=rcd+nf_put_att_text(nc_id,odxc_O2_id,'long_name',37,'Column extinction optical depth of O2')
      rcd=rcd+nf_put_att_text(nc_id,odxc_O3_id,'long_name',37,'Column extinction optical depth of O3')
      rcd=rcd+nf_put_att_text(nc_id,odxc_Ray_id,'long_name',58,'Column extinction optical depth due to Rayleigh scattering')
      rcd=rcd+nf_put_att_text(nc_id,odxc_aer_id,'long_name',42,'Column extinction optical depth of aerosol')
      rcd=rcd+nf_put_att_text(nc_id,odxc_ice_id,'long_name',44,'Column extinction optical depth of solid H2O')
      rcd=rcd+nf_put_att_text(nc_id,odxc_lqd_id,'long_name',45,'Column extinction optical depth of liquid H2O')
      rcd=rcd+nf_put_att_text(nc_id,odxc_ttl_id,'long_name',37,'Total column extinction optical depth')
      rcd=rcd+nf_put_att_text(nc_id,odxl_CO2_id,'long_name',38,'Layer extinction optical depth of CO2')
      rcd=rcd+nf_put_att_text(nc_id,odxl_H2O_id,'long_name',46,'Layer extinction optical depth of gaseous H2O')
      rcd=rcd+nf_put_att_text(nc_id,odxl_O2_id,'long_name',37,'Layer extinction optical depth of O2')
      rcd=rcd+nf_put_att_text(nc_id,odxl_O3_id,'long_name',37,'Layer extinction optical depth of O3')
      rcd=rcd+nf_put_att_text(nc_id,odxl_Ray_id,'long_name',58,'Layer extinction optical depth due to Rayleigh scattering')
      rcd=rcd+nf_put_att_text(nc_id,odxl_aer_id,'long_name',42,'Layer extinction optical depth of aerosol')
      rcd=rcd+nf_put_att_text(nc_id,odxl_ice_id,'long_name',44,'Layer extinction optical depth of solid H2O')
      rcd=rcd+nf_put_att_text(nc_id,odxl_lqd_id,'long_name',45,'Layer extinction optical depth of liquid H2O')
      rcd=rcd+nf_put_att_text(nc_id,odxl_ttl_id,'long_name',37,'Total layer extinction optical depth')
      rcd=rcd+nf_put_att_text(nc_id,wvl_ctr_id,'long_name',22,'Band center wavelength')
      rcd=rcd+nf_put_att_text(nc_id,wvl_dlt_id,'long_name',9,'Bandwidth')
      rcd=rcd+nf_put_att_text(nc_id,wvl_max_id,'long_name',23,'Band maximum wavelength')
      rcd=rcd+nf_put_att_text(nc_id,wvl_min_id,'long_name',23,'Band minimum wavelength')
      rcd=rcd+nf_put_att_text(nc_id,wvn_ctr_id,'long_name',22,'Band center wavenumber')
      rcd=rcd+nf_put_att_text(nc_id,wvn_dlt_id,'long_name',9,'Bandwidth')
      rcd=rcd+nf_put_att_text(nc_id,wvn_id,'long_name',23,'Band nominal wavenumber')
      rcd=rcd+nf_put_att_text(nc_id,wvn_max_id,'long_name',23,'Band maximum wavenumber')
      rcd=rcd+nf_put_att_text(nc_id,wvn_min_id,'long_name',23,'Band minimum wavenumber')

c     End define mode
      rcd=rcd+nf_enddef(nc_id)

c     Write data
      rcd=rcd+nf_put_var_double(nc_id,alb_NIR_SW_TOA_id,alb_NIR_SW_TOA)
      rcd=rcd+nf_put_var_double(nc_id,alb_NIR_TOA_id,alb_NIR_TOA)
      rcd=rcd+nf_put_var_double(nc_id,alb_NIR_sfc_id,alb_NIR_sfc)
      rcd=rcd+nf_put_var_double(nc_id,alb_NIR_vsb_TOA_id,alb_NIR_vsb_TOA)
      rcd=rcd+nf_put_var_double(nc_id,alb_SW_TOA_id,alb_SW_TOA)
      rcd=rcd+nf_put_var_double(nc_id,alb_SW_sfc_id,alb_SW_sfc)
      rcd=rcd+nf_put_var_double(nc_id,alb_vsb_TOA_id,alb_vsb_TOA)
      rcd=rcd+nf_put_var_double(nc_id,alb_vsb_sfc_id,alb_vsb_sfc)
      rcd=rcd+nf_put_var_double(nc_id,bnd_id,wvl)
      rcd=rcd+nf_put_var_double(nc_id,dff_drc_NIR_sfc_id,dff_drc_NIR_sfc)
      rcd=rcd+nf_put_var_double(nc_id,dff_drc_SW_id,dff_drc_SW)
      rcd=rcd+nf_put_var_double(nc_id,dff_drc_SW_sfc_id,dff_drc_SW_sfc)
      rcd=rcd+nf_put_var_double(nc_id,dff_drc_vsb_sfc_id,dff_drc_vsb_sfc)
      rcd=rcd+nf_put_var_double(nc_id,flx_LW_dwn_id,flx_LW_dwn)
      rcd=rcd+nf_put_var_double(nc_id,flx_LW_up_id,flx_LW_up)
      rcd=rcd+nf_put_var_double(nc_id,flx_NIR_dwn_TOA_id,flx_NIR_dwn_TOA)
      rcd=rcd+nf_put_var_double(nc_id,flx_NIR_dwn_dff_sfc_id,flx_NIR_dwn_dff_sfc)
      rcd=rcd+nf_put_var_double(nc_id,flx_NIR_dwn_drc_sfc_id,flx_NIR_dwn_drc_sfc)
      rcd=rcd+nf_put_var_double(nc_id,flx_NIR_dwn_sfc_id,flx_NIR_dwn_sfc)
      rcd=rcd+nf_put_var_double(nc_id,flx_NIR_up_TOA_id,flx_NIR_up_TOA)
      rcd=rcd+nf_put_var_double(nc_id,flx_SW_dwn_TOA_id,flx_SW_dwn_TOA)
      rcd=rcd+nf_put_var_double(nc_id,flx_SW_dwn_dff_id,flx_SW_dwn_dff)
      rcd=rcd+nf_put_var_double(nc_id,flx_SW_dwn_dff_sfc_id,flx_SW_dwn_dff_sfc)
      rcd=rcd+nf_put_var_double(nc_id,flx_SW_dwn_drc_id,flx_SW_dwn_drc)
      rcd=rcd+nf_put_var_double(nc_id,flx_SW_dwn_drc_sfc_id,flx_SW_dwn_drc_sfc)
      rcd=rcd+nf_put_var_double(nc_id,flx_SW_dwn_id,flx_SW_dwn)
      rcd=rcd+nf_put_var_double(nc_id,flx_SW_dwn_sfc_id,flx_SW_dwn_sfc)
      rcd=rcd+nf_put_var_double(nc_id,flx_SW_up_sfc_id,flx_SW_up_sfc)
      rcd=rcd+nf_put_var_double(nc_id,flx_NIR_up_sfc_id,flx_NIR_up_sfc)
      rcd=rcd+nf_put_var_double(nc_id,flx_vsb_up_sfc_id,flx_vsb_up_sfc)
      rcd=rcd+nf_put_var_double(nc_id,flx_SW_up_TOA_id,flx_SW_up_TOA)
      rcd=rcd+nf_put_var_double(nc_id,flx_SW_up_id,flx_SW_up)
      rcd=rcd+nf_put_var_double(nc_id,flx_bnd_dwn_TOA_id,flx_bnd_dwn_TOA)
      rcd=rcd+nf_put_var_double(nc_id,flx_bnd_dwn_dff_id,flx_bnd_dwn_dff)
      rcd=rcd+nf_put_var_double(nc_id,flx_bnd_dwn_dff_sfc_id,flx_bnd_dwn_dff_sfc)
      rcd=rcd+nf_put_var_double(nc_id,flx_bnd_dwn_drc_id,flx_bnd_dwn_drc)
      rcd=rcd+nf_put_var_double(nc_id,flx_bnd_dwn_drc_sfc_id,flx_bnd_dwn_drc_sfc)
      rcd=rcd+nf_put_var_double(nc_id,flx_bnd_dwn_id,flx_bnd_dwn)
      rcd=rcd+nf_put_var_double(nc_id,flx_bnd_dwn_sfc_id,flx_bnd_dwn_sfc)
      rcd=rcd+nf_put_var_double(nc_id,flx_bnd_up_TOA_id,flx_bnd_up_TOA)
      rcd=rcd+nf_put_var_double(nc_id,flx_bnd_up_id,flx_bnd_up)
      rcd=rcd+nf_put_var_double(nc_id,flx_bnd_up_sfc_id,flx_bnd_up_sfc)
      rcd=rcd+nf_put_var_double(nc_id,flx_vsb_dwn_TOA_id,flx_vsb_dwn_TOA)
      rcd=rcd+nf_put_var_double(nc_id,flx_vsb_dwn_dff_sfc_id,flx_vsb_dwn_dff_sfc)
      rcd=rcd+nf_put_var_double(nc_id,flx_vsb_dwn_drc_sfc_id,flx_vsb_dwn_drc_sfc)
      rcd=rcd+nf_put_var_double(nc_id,flx_vsb_dwn_sfc_id,flx_vsb_dwn_sfc)
      rcd=rcd+nf_put_var_double(nc_id,flx_vsb_up_TOA_id,flx_vsb_up_TOA)
      rcd=rcd+nf_put_var_double(nc_id,mpc_H2O_id,mpc_H2O)
      rcd=rcd+nf_put_var_double(nc_id,mpc_O3_DU_id,mpc_O3_DU)
      rcd=rcd+nf_put_var_double(nc_id,mpc_O3_id,mpc_O3)
      rcd=rcd+nf_put_var_double(nc_id,odxc_CO2_id,odxc_CO2)
      rcd=rcd+nf_put_var_double(nc_id,odxc_H2O_id,odxc_H2O)
      rcd=rcd+nf_put_var_double(nc_id,odxc_O2_id,odxc_O2)
      rcd=rcd+nf_put_var_double(nc_id,odxc_O3_id,odxc_O3)
      rcd=rcd+nf_put_var_double(nc_id,odxc_Ray_id,odxc_Ray)
      rcd=rcd+nf_put_var_double(nc_id,odxc_aer_id,odxc_aer)
      rcd=rcd+nf_put_var_double(nc_id,odxc_ice_id,odxc_ice)
      rcd=rcd+nf_put_var_double(nc_id,odxc_lqd_id,odxc_lqd)
      rcd=rcd+nf_put_var_double(nc_id,odxc_ttl_id,odxc_ttl)
      rcd=rcd+nf_put_var_double(nc_id,odxl_CO2_id,odxl_CO2)
      rcd=rcd+nf_put_var_double(nc_id,odxl_H2O_id,odxl_H2O)
      rcd=rcd+nf_put_var_double(nc_id,odxl_O2_id,odxl_O2)
      rcd=rcd+nf_put_var_double(nc_id,odxl_O3_id,odxl_O3)
      rcd=rcd+nf_put_var_double(nc_id,odxl_Ray_id,odxl_Ray)
      rcd=rcd+nf_put_var_double(nc_id,odxl_aer_id,odxl_aer)
      rcd=rcd+nf_put_var_double(nc_id,odxl_ice_id,odxl_ice)
      rcd=rcd+nf_put_var_double(nc_id,odxl_lqd_id,odxl_lqd)
      rcd=rcd+nf_put_var_double(nc_id,odxl_ttl_id,odxl_ttl)
      rcd=rcd+nf_put_var_double(nc_id,wvl_ctr_id,wvl_ctr)
      rcd=rcd+nf_put_var_double(nc_id,wvl_dlt_id,wvl_dlt)
      rcd=rcd+nf_put_var_double(nc_id,wvl_max_id,wvl_max)
      rcd=rcd+nf_put_var_double(nc_id,wvl_min_id,wvl_min)
      rcd=rcd+nf_put_var_double(nc_id,wvn_ctr_id,wvn_ctr)
      rcd=rcd+nf_put_var_double(nc_id,wvn_dlt_id,wvn_dlt)
      rcd=rcd+nf_put_var_double(nc_id,wvn_id,wvn)
      rcd=rcd+nf_put_var_double(nc_id,wvn_max_id,wvn_max)
      rcd=rcd+nf_put_var_double(nc_id,wvn_min_id,wvn_min)

      rcd=rcd+nf_close(nc_id)
      write(6,'(2a)') 'Added SRB results to ',fl_out
      if (rcd.ne.NF_NOERR) write (6,'(a,a,i4,a)') 'netcdf()',': ERROR rcd = ',rcd,' on exit'
#endif /* not CRM_SRB */

      return
      end                       ! end netcdf()

      subroutine nc_err_exit(rcd,err_msg_sng)
c     Routine to print netCDF error message, routine name, and exit
      implicit none
c     Commons
#include <netcdf.inc>
c     Local Workspace
      character*(*) err_msg_sng ! Message to print before exiting
      integer rcd               ! Return code
      integer err_msg_sng_len   ! Length of custom message

c     Main Code
      err_msg_sng_len=len(err_msg_sng)
      write (6,'(a,a2,a)') err_msg_sng(1:err_msg_sng_len),', ',nf_strerror(rcd)
      stop 
      end                       ! end nc_err_exit()
