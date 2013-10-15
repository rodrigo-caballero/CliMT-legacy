#include <misc.h>
#include <params.h>

#ifdef SINGLE_SOURCE_FILE
#include <My_crm.F>
#include <aermix.F>
#include <albocean.F>
#include <blkdat.F>
#include <cldefr.F>
#include <cldems.F>
#include <endrun.F>
#include <freemem.F>
#include <getmem.F>
#include <fmrgrid.F>
#include <orb.F>
#include <radabs.F>
#include <radclr.F>
#include <radclw.F>
#include <radcsw.F>
#include <radctl.F>
#include <radded.F>
#include <radems.F>
#include <radini.F>
#include <radinp.F>
#include <radoz2.F>
#include <radtpl.F>
#include <resetr.F>
#include <torgrid.F>
#include <trcab.F>
#include <trcabn.F>
#include <trcems.F>
#include <trcmix.F>
#include <trcplk.F>
#include <trcpth.F>
#ifndef CRAY
#include <intmax.F>
#include <isrchfgt.F>
#include <isrchfle.F>
#include <wheneq.F>
#include <whenfgt.F>
#include <whenflt.F>
#include <whenne.F>
#endif /* CRAY */ 
#endif /* not SINGLE_SOURCE_FILE */

      program test

c     Purpose: Initialize column thermodynamic profile from external data 

c     Variables in getdat() have the same names they have in tphysbc(), where possible

c     O3 mass mixing ratios are read in, but the model also requires O3 path lengths; they are computed here
c     Cloud longwave emissivity is computed from cloud input (fraction and liquid water path), this is done here

#include <implicit.h>
c     Parameters
#include <prgrid.h>
c     Commons
#include <comtim.h> /* calday */ 
#include <crdcon.h> /* pie */ 
#include <comvmr.h> /* co2vmr, n2ovmr, ch4vmr, cfc11, cfc12 */ 
#include <comsol.h> /* eccen, obliq, mvelp, iyear_AD */
c     Output arguments
      real aldif(plond)         ! Albedo: longwave, diffuse
      real aldir(plond)         ! Albedo: longwave, direct
      real asdif(plond)         ! Albedo: shortwave, diffuse
      real asdir(plond)         ! Albedo: shortwave, direct
      real clat                 ! model latitude in radians
      real cld(plond,plevp)     ! cloud fraction
      real clon(plon)           ! Centered longitude (radians)
      real clwp(plond,plev)     ! cloud liquid water path (g/m**2)
      real coslat               ! cosine latitude
      real loctim(plond)        ! local time of solar computation
      real o3mmr(plond,plev)    ! o3 mass mixing ratio
      real o3vmr(plond,plev)    ! o3 volume mixing ratio
      real oro(plond)           ! Land surface flag
      real pilnm1(plond,plevp)  ! ln(pintm1)
      real pintm1(plond,plevp)  ! pressure at model interfaces 
      real pmidm1(plond,plev)   ! pressure at model mid-levels 
      real pmlnm1(plond,plev)   ! ln(pmidm1)
      real ps(plond)            ! model surface pressure field
      real qm1(plond,plev)      ! moisture field
      real snowh(plond)         ! snow depth (liquid water equivalent)
      real tg(plond)            ! surface (skin) temperature
      real tm1(plond,plev)      ! atmospheric temperature
      real ts(plond)            ! surface (air)  temperature
      real r_liq,r_ice,zen

      real qrs(plond,plev)      ! Solar heating rate      
      real qrl(plond,plev)      ! Longwave cooling rate
      real swflx(plond,plevp)   ! [W m-2] Net SW flux at interfaces  
      real lwflx(plond,plevp)   ! [W m-2] Net LW flux at interfaces  
      real sw_cf_toa(plond)     ! [W m-2] SW cloud forcing at top of atmos
      real sw_cf_srf(plond)     ! [W m-2] SW cloud forcing at surface
      real lw_cf_toa(plond)     ! [W m-2] LW cloud forcing at top of atmos
      real lw_cf_srf(plond)     ! [W m-2] LW cloud forcing at surface


c     Local workspace
      character*80 lbl          ! Temporary space for labels
      integer dbg_lvl           ! Debugging level
      integer i                 ! longitude index
      integer k                 ! level  index
      integer lev(plev)         ! [mb] Level index input
      logical log_print         ! Flag for status information in orb_params()
      real lat_dgr              ! [dgr] Latitude input
      real lon_dgr              ! [dgr] Longitude input
      real rghnss(plond)        ! surface roughness (obsolete)
      real frctst(plond)        ! fraction of surface with strong zenith dependent albedo (obsolete)
      integer idosw,idolw       ! switches (do/do not compute sw/lw)

c     CCM: physics/radinp()
      real amd                  ! effective molecular weight of dry air (g/mol)
      real amo                  ! molecular weight of ozone (g/mol)
      data amd   /  28.9644   /
      data amo   /  48.0000   /
      real vmmr                 ! Factor for ozone volume mixing ratio

c     Main Code
c     Initialize some variables
      dbg_lvl=0
      i=1                       ! Longitude index
      
c     Read input data
      read (5,'(a80)') lbl
      read (5,'(a80)') lbl
      read (5,'(a80)') lbl
      read (5,*) calday
      read (5,*) lat_dgr
      read (5,'(a80)') lbl      
      do k=1,plev
         read (5,*) lev(k),pmidm1(i,k),tm1(i,k),qm1(i,k),o3mmr(i,k),cld(i,k),clwp(i,k)
      enddo                     ! end loop over lev
      read (5,*) ps(i)
      read (5,*) ts(i)
      read (5,*) tg(i)
      read (5,*) oro(i)
      read (5,*) rghnss(i)
      read (5,*) snowh(i)
      read (5,*) asdir(i)
      read (5,*) asdif(i)
      read (5,*) aldir(i)
      read (5,*) aldif(i)
      read (5,*) frctst(i)
      read (5,*) co2vmr         ! CCM: physics/comvmr.h: co2vmr set in control/preset()
      read (5,*) n2ovmr         ! CCM: physics/comvmr.h: n2ovmr set in control/preset()
      read (5,*) ch4vmr         ! CCM: physics/comvmr.h: ch4vmr set in control/preset()
      read (5,*) f11vmr         ! CCM: physics/comvmr.h: co2vmr set in control/preset()
      read (5,*) f12vmr         ! CCM: physics/comvmr.h: f12vmr set in control/preset()
      read (5,*) tauvis         ! CCM: physics/comsol.h: tauvis set in control/preset()
      read (5,*) scon           ! CCM: physics/comsol.h: scon set in control/preset()
      read (5,*) iyear_AD       ! CCM: physics/comsol.h: iyear_AD set in control/preset()
      read (5,*) lon_dgr

      idosw=1
      idolw=1
      gravit =   9.80616
      cpair  =   1004.64
      epsilo =   0.622
      stebol =   5.67e-8
      r_liq = 10.
      r_ice = 20.
      zen = 60.00642693989500

      call crm(
     $     aldif,
     $     aldir,
     $     asdif,
     $     asdir,
     $     iyear_AD,
     $     zen,
     $     cld,
     $     clwp,
     $     o3mmr,
     $     oro,
     $     r_liq,
     $     r_ice,
     $     pmidm1,
     $     ps,
     $     qm1, 
     $     snowh,
     $     tg,
     $     tm1,
     $     co2vmr,
     $     n2ovmr,
     $     ch4vmr,
     $     f11vmr,
     $     f12vmr,
     $     scon,
     $     tauvis,
     $     idosw,
     $     idolw,
     $     gravit,
     $     cpair,
     $     epsilo,
     $     stebol,
     $     qrs,
     $     qrl,
     $     swflx,
     $     lwflx,
     $     sw_cf_toa,
     $     sw_cf_srf,
     $     lw_cf_toa,
     $     lw_cf_srf)
      
      do k=1,plev
      print*, k,swflx(k),lwflx(k)
      enddo

      end
