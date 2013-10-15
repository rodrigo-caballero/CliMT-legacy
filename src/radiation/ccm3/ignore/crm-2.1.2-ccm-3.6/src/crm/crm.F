c     $Header: /Users/rca/cvsroot/CliMT/src/radiation/ccm3/src/crm-2.1.2-ccm-3.6/src/crm/crm.F,v 1.1 2004/09/07 15:27:30 rca Exp $

c     CCM3 Column Radiation Model 
c     Jeffrey Kiehl, Bruce Briegleb, and Charlie Zender  
c     July 1998

c     All routines except crm(), getdat(), netcdf(), radctl(), radcsw() and 
c     four dummy routines (readric(), writeric(), outfld(), and radozn()) 
c     are #included straight from CCM code. Thus CRM code is identical to CCM code  
c     but does not invoke the Land Surface Model for land albedo computation. 
c     Routines radctl() and radcsw() are slightly modified to support the CRM.
c     The purpose of the (non-dummy) routines is:

c     crm(): The main() routine. This routine takes the place of tphysbc()
c     in the CCM3 calling tree. It places the required calls to the cloud
c     routines cldefr() and cldems() directly, rather than invoking cldint().

c     getdat(): Reads the stdin file to parse the user-specified column
c     profile. Overwrites the co2vmr previsouly set by radini() with the
c     user specified value. Computes o3vmr (instead of in radozn()).
c     Sets calday variable in comtim.h used in zenith() and radinp().

c     radctl(): Main radiation driving routine, same as ccm3 version except:
c     receives additional input variable o3vmr, and passes out
c     additional diagnostic radiation quantities and eccf usually local to radctl().
c     does cgs-->mks conversion for all fluxes.

c     netcdf(): Routine to output a netCDF file
c     named crm.nc containing most of the data.

c     radcsw(): Altered to compute SRB quantities and pass them into crmsrb.h

#include <misc.h>
#include <params.h>

c     The following block allows the entire CRM to compile with a simple command
c     Using this command also checks that all needed files are in the CRM
#ifdef SINGLE_SOURCE_FILE
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
#include <zenith.F>
#include <netcdf.F>
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

      program crm

#include <implicit.h>
c     Parameters
#include <prgrid.h>
c     Commons
#include <comtim.h> /* calday */ 
#include <comvmr.h> /* co2vmr, n2ovmr, ch4vmr, cfc11, cfc12 */ 
#include <comsol.h> /* scon, tauvis, eccen, obliq, mvelp, iyear_AD */
#include <comctl.h> /* anncyc,iradsw,iradlw,iradae */
#ifdef CRM_SRB
#include <crmsrb.h> 
#endif /* not CRM_SRB */
c     Arguments
c     Fields specified by the user in getdat()

      real clon(plon)           ! Centered longitude (radians)
      real clat                 ! Current centered latitude (radians)
      real cld(plond,plevp)     ! fractional cloud cover
      real clwp(plond,plev)     ! cloud liquid water path
      real coslat               ! cosine latitude

c     NB: o3mmr and o3vmr should be dimensioned (plond,plevr) if a different 
c     size radiation grid is used. Clashes between prgrid.h and ptrrgrid.h
c     (they both define plngbuf) prevent us from dimensioning anything by
c     plevr in this top level crm() routine.
      real o3mmr(plond,plev)    ! Ozone mass mixing ratio
      real o3vmr(plond,plev)    ! Ozone volume mixing ratio

      real aldif(plond)         ! Albedo: longwave, diffuse
      real aldir(plond)         ! Albedo: longwave, direct
      real asdif(plond)         ! Albedo: shortwave, diffuse
      real asdir(plond)         ! Albedo: shortwave, direct
      real oro(plond)           ! Land/ocean/sea ice flag
      real pilnm1(plond,plevp)  ! natural log of pintm1
      real pintm1(plond,plevp)  ! model interface pressures
      real pmidm1(plond,plev)   ! model level pressures
      real pmlnm1(plond,plev)   ! natural log of pmidm1
      real ps(plond)            ! surface pressure
      real qm1(plond,plev)      ! model level specific humidity
      real snowh(plond)         ! snow depth (liquid water equivalent)
      real tg(plond)            ! surface (skin) temperature
      real tm1(plond,plev)      ! model level temperatures
      real ts(plond)            ! surface air temperature

c     Fields computed from user input
      real coszrs(plond)        ! cosine solar zenith angle
      real eccf                 ! earth/sun distance factor 
      real effcld(plond,plevp)  ! effective cloud=cld*emis
      real emis(plond,plev)     ! cloud emissivity
      real fice(plond,plev)     ! fractional amount of ice
      real loctim(plond)        ! local time of solar computation
      real lwup(plond)          ! Longwave up flux at surface
      real rei(plond,plev)      ! ice particle size
      real rel(plond,plev)      ! liquid effective drop size (microns)
      real srfrad(plond)        ! srf radiative heat flux

c     Output longwave arguments from radctl()
      real flwds(plond)         ! Surface down longwave flux
      real qrl(plond,plev)      ! Longwave cooling rate

c     Output shortwave arguments from radctl()
      real fsns(plond)          ! Surface absorbed solar flux
      real qrs(plond,plev)      ! Solar heating rate
      real soll(plond)          ! Downward solar rad onto surface (lw direct)
      real solld(plond)         ! Downward solar rad onto surface (lw diffuse)
      real sols(plond)          ! Downward solar rad onto surface (sw direct)
      real solsd(plond)         ! Downward solar rad onto surface (sw diffuse)

c     Additional CRM diagnostic output from radctl()
      real flns(plond)          ! srf longwave cooling (up-dwn) flux
      real flnsc(plond)         ! clr sky lw flx at srf (up-dwn)
      real flnt(plond)          ! net outgoing lw flx at model top
      real flntc(plond)         ! clr sky lw flx at model top
      real fsnsc(plond)         ! clr sky surface abs solar flux
      real fsnt(plond)          ! total column absorbed solar flux
      real fsntc(plond)         ! clr sky total column abs solar flux
      real solin(plond)         ! solar incident flux
      real fsnirt(plond)        ! [W m-2] Near-IR flux absorbed at TOA
      real fsnirtsq(plond)      ! [W m-2] Near-IR flux absorbed at TOA>= 0.7 microns
      real fsnrtc(plond)        ! [W m-2] Clear sky near-IR flux absorbed at TOA

c     Local workspace: These variables are not saved
      real hbuf                 ! history buffer
      real pie                  ! 3.14159...
      integer i                 ! longitude index
      integer k                 ! level index
      integer lat               ! latitude row index 

c     Fundamental constants needed by radini()
      real cpair                ! heat capacity dry air at constant prs (J/kg/K)
      real epsilo               ! ratio mean mol weight h2o to dry air
      real gravit               ! gravitational acceleration (m/s**2)
      real stebol               ! Stefan-Boltzmann constant (W/m**2/K**4)

c     Externals
      external blkdat

c     Main Code
#ifdef SUN
c     19990907: IEEE code deprecated by default
c     CCM IEEE handling code from control/ccm3.F
c#include <f77_floatingpoint.h>
c      integer iexcept
c      integer ieee_handler
c      iexcept=ieee_handler('set','common',SIGFPE_ABORT)
c      if (iexcept.ne.0) write(6,*) 'IEEE trapping not supported here'
#endif /* not SUN */

      write(6,'(a)') 'Begin CCM3 Column Radiation Model'

c     Set latitude index to 1 (only used in calling radctl())
c     CCM: dynamics/eul/scan1bc()
      lat=1

c     Set control/comtim.h: nstep,dosw,dolw,doabsems
c     CCM: dynamics/eul/stepon()
      nstep=0
c     CCM: dynamics/advnce()
      dosw=.true.
c     CCM: dynamics/advnce()
      dolw=.true.
      doabsems=.true.

c     Set control/comctl.h: anncyc,iradsw,iradlw,iradae
c     CCM: control/data()
      anncyc=.true.
c     CCM: control/data()
      iradsw=1
c     CCM: control/data()
      iradlw=1
c     CCM: control/data()
      iradae=1

c     Set physics/comcon.h: gravit, cpair, epsilo, stebol
c     NB: physics/comcon.h is never actually used in the CRM
c     These SI parameters are used in physics/inti() to call physics/radini() to set CGS parameters of same name in physics/crdcon.h: gravit, cpair, epsilo, stebol
c     These constants must be set before computing lwup or calling radini()
c     CCM: dynamics/eul/initcom()
      gravit =   9.80616
      cpair  =   1004.64
      epsilo =   0.622
      stebol =   5.67e-8

c     Besides setting the variables passed as subroutines arguments, getdat() sets 
c     control/comtim.h: calday (needed in zenith() and radinp())
c     physics/comsol.h: scon, tauvis, iyear_AD, obliqr, lambm0 and mvelpp
c     physics/comvmr.h: co2vmr, n2ovmr, ch4vmr, f11vmr, f12vmr
c     control/crdcon.h: pie
      call getdat(
     $     aldif,
     $     aldir,
     $     asdif,
     $     asdir,
     $     clat,
     $     cld,
     $     clon,
     $     clwp,
     $     coslat,
     $     loctim,
     $     o3mmr,
     $     o3vmr,
     $     oro,
     $     pilnm1,
     $     pintm1,
     $     pmidm1,
     $     pmlnm1,
     $     ps,
     $     qm1, 
     $     snowh,
     $     tg,
     $     tm1,
     $     ts)     

c     Given these four physical constants in MKS, radini() defines CGS equivalents, 
c     and sets many radiation parameters stored in common blocks. 
c     radini() must be called after getdat(), because the CO2 mass mixing ratio 
c     set in radini() depends on co2vmr set by the user in getdat().
c     CCM: physics/inti()
      call radini(gravit,cpair,epsilo,stebol)

c     Get coszrs: needed as input to albocean() and radctl() 
c     CCM: dom/initext()
      call zenith (calday  ,clat    ,clon   ,coszrs  )

c     Adjust the user-specified albedo for ocean/sea-ice points only
c     Land points always use the user-specified albedos
c     CCM: dom/initext()
      call albocean(oro    ,snowh   ,coszrs  ,asdir  ,aldir   ,
     $     asdif   ,aldif   )

c     Cloud particle size and fraction of ice
c     CCM: physics/cldint()
      call cldefr(oro, tm1, rel, rei, fice, ps, pmidm1)

c     Cloud emissivity
c     CCM: physics/cldint()
      call cldems(clwp, fice, rei, emis)

c     The radiation scheme recomputes the skin temperature in physics/radtpl()
c     This skin temperature is based on the LW up flux at the surface
c     Thus, surface LW up flux is computed here, then passed down to radtpl() where,
c     hopefully, it is inverted to obtain the same skin temperature we started with.
c     CCM: control/initext() (Ocean and sea-ice only)
c     CCM: ? (Land)
      do i=1,plon
         lwup(i)=stebol*tg(i)**4 ! W m-2 LW up is skin temperature to fourth power
      end do

c     Effective cloud cover
c     CCM: physics/cldint()
      do k=1,plev
         do i=1,plon
            effcld(i,k) = cld(i,k)*emis(i,k)
         end do
      end do

c     Cloud cover at surface interface always zero (for safety's sake)
c     CCM: physics/cldint()
      do i=1,plon
         effcld(i,plevp) = 0.0
         cld(i,plevp)    = 0.0
      end do

c     Main radiation driving routine 
c     NB: All fluxes returned from radctl() have already been converted to MKS
c     CCM: physics/tphysbc()
      call radctl(hbuf    ,clat    ,coslat  ,lat     ,lwup    ,
     $     pmidm1  ,pintm1  ,pmlnm1  ,pilnm1  ,tm1     ,
     $     qm1     ,cld     ,effcld  ,clwp    ,coszrs  ,
     $     asdir   ,asdif   ,aldir   ,aldif   ,fsns    ,
     $     qrs     ,qrl     ,flwds   ,rel     ,rei     ,
     $     fice    ,sols    ,soll    ,solsd   ,solld   
c++csz
     $     ,fsnt,fsntc,fsnsc,flnt,flns,flntc,flnsc,solin, ! Output 
     $     fsnirt,fsnirtsq,fsnrtc, ! Output
     $     eccf,                ! Output (computed in radctl()/radinp()/orbdecl())
     $     o3vmr)               ! Input
c--csz
      
c     CCM: physics/tphysbc()
      do i=1,plon
         srfrad(i)=fsns(i)+flwds(i)
      end do

#ifdef CRM_SRB
c     Diagnostic column abundances
      do i=1,plon
         mpc_H2O(i)=0.0         ! kg m-2
         mpc_O3(i)=0.0          ! kg m-2
      end do
      do i=1,plon
         do k=1,plev
            pdelm1(i,k)=pintm1(i,k+1)-pintm1(i,k) ! Pa
            mpc_H2O(i)=mpc_H2O(i)+pdelm1(i,k)*qm1(i,k)/gravit ! kg m-2
            mpc_O3(i)=mpc_O3(i)+pdelm1(i,k)*o3mmr(i,k)/gravit ! kg m-2
         end do                 ! end loop over lev
         mpc_O3_DU(i)=mpc_O3(i)*gas_cst_O3*tpt_STP/prs_STP ! m
         mpc_O3_DU(i)=mpc_O3_DU(i)*1.0e5 ! m --> millicm = 1.0e-5 m
      end do                    ! end loop over lon
#endif /* not CRM_SRB */

c     Write out final results
      write (6,'(a)') 'CCM3 CRM Results:'
      write (6,'(a)') 'Conventions:'
      write (6,*) 'Shortwave fluxes are positive downward'
      write (6,*) 'Longwave fluxes are positive upward'
      write (6,*) 'Net Radiative fluxes are positive downward (into the system)'
      write (6,*) 'Fluxes defined to be zero are not reported (e.g., LW down flx TOA)'
      write (6,'(a)') 'Abbreviations, Acronyms and Definitions:'
      write (6,*) 'LW   = Longwave'
      write (6,*) 'LWCF = Longwave Cloud Forcing'
      write (6,*) 'NCF  = Net Cloud Forcing = SWCF+LWCF'
      write (6,*) 'NIR  = Near Infrared (0.7 < lambda < 5.0 microns)'
      write (6,*) 'N7   = NOAA7 satellite NIR instrument weighted flux'
      write (6,*) 'NRF  = Net Radiative Flux: sum of SW and LW fluxes'
      write (6,*) 'SW   = Shortwave'
      write (6,*) 'SWCF = Shortwave Cloud Forcing'
      write (6,*) 'TOA  = Top of Atmosphere'
      write (6,*) 'Vis  = Visible (0.2 < lambda < 0.7 microns)'
      write (6,*) 'atm  = Atmosphere'
      write (6,*) 'clr  = Clear sky (diagnostic computation with no clouds)'
      write (6,*) 'ctr  = Center'
      write (6,*) 'dff  = Diffuse flux'
      write (6,*) 'drc  = Direct flux'
      write (6,*) 'dwn  = Downwelling'
      write (6,*) 'frc  = Fraction'
      write (6,*) 'lqd  = Liquid'
      write (6,*) 'mpc  = Mass path column'
      write (6,*) 'net  = Net flux = downwelling minus upwelling flux'
      write (6,*) 'spc  = Spectral'
      write (6,*) 'sfc  = Surface level'
      write (6,*) 'vmr  = Volume mixing ratio'
      write (6,*) 'wvl  = Wavelength'
      write (6,*) 'um   = Microns'
      write (6,*) 'up   = Upwelling'

      write (6,*) ''
      write (6,'(a)') 'Sun-Earth Geometry:'
      pie    = 4.0*atan(1.0)    ! NB: This is not the pie in physics/crdcon.h
      do i=1,1
         write (6,*) 'Year AD                        = ',iyear_AD
         write (6,*) 'Day of year (Greenwich)        = ',calday
         write (6,*) 'Local solar hour               = ',loctim(i)
         write (6,*) 'Latitude                       = ',180.0*clat/pie,' degrees'
         write (6,*) 'Longitude                      = ',180.0*clon(i)/pie,' degrees'
         write (6,*) 'Solar zenith angle             = ',180.0*acos(coszrs(i))/pie, ' degrees'
         write (6,*) 'Cosine solar zenith angle      = ',coszrs(i)
         write (6,*) 'Earth-sun distance             = ',eccf,' AU'

         write (6,*) 'Solar constant                 = ',scon/1000.0,' W m-2' ! CGS --> SI
         write (6,*) ''

         write (6,'(a)') 'Shortwave (SW) results ( < 5.0 um):'
         if (solin(i).le.0.0) then
            write (6,*) 'SW albedo               = Ill-defined'
            write (6,*) 'SW albedo (clr)         = Ill-defined'
         else
            write (6,*) 'SW albedo               = ',(solin(i)-fsnt(i))/solin(i)
            write (6,*) 'SW albedo (clr)         = ',(solin(i)-fsntc(i))/solin(i)
         endif
         write (6,*) 'SW down flux TOA        = ',solin(i),' W m-2' 
         write (6,*) 'SW up flux TOA          = ',solin(i)-fsnt(i),' W m-2' 
         write (6,*) 'SW up flux TOA (clr)    = ',solin(i)-fsntc(i),' W m-2' 
         write (6,*) 'SW net flux TOA         = ',fsnt(i),' W m-2' 
         write (6,*) 'SW net flux TOA (clr)   = ',fsntc(i),' W m-2' 
         write (6,*) 'SW flux abs atm         = ',fsnt(i)-fsns(i),' W m-2' 
         write (6,*) 'SW flux abs atm (clr)   = ',fsntc(i)-fsnsc(i),' W m-2' 
         write (6,*) 'SW down flux sfc        = ',sols(i)+soll(i)+solsd(i)+solld(i),' W m-2' 
c         write (6,*) 'SW down flux sfc (clr)  = ',,' W m-2' 
         write (6,*) 'SW up flux sfc          = ',sols(i)+soll(i)+solsd(i)+solld(i)-fsns(i),' W m-2' 
c         write (6,*) 'SW up flux sfc (clr)    = ',,' W m-2' 
         write (6,*) 'SW net flux sfc         = ',fsns(i),' W m-2' 
         write (6,*) 'SW net flux sfc (clr)   = ',fsnsc(i),' W m-2' 
         write (6,*) 'SW cloud forcing TOA    = ',fsnt(i)-fsntc(i),' W m-2' 
         write (6,*) 'SW cloud forcing sfc    = ',fsns(i)-fsnsc(i),' W m-2'
         if (fsnt(i)-fsntc(i).eq.0.0) then
            write (6,*) 'SWCF(sfc)/SWCF(TOA)     = Ill-defined'
         else
            write (6,*) 'SWCF(sfc)/SWCF(TOA)     = ',(fsns(i)-fsnsc(i))/(fsnt(i)-fsntc(i))
         endif                  ! endif
         write (6,*) ''

         write (6,'(a)') 'Longwave (LW) results ( > 5.0 um):'
         write (6,*) 'LW up flux TOA          = ',flnt(i),' W m-2' 
         write (6,*) 'LW up flux TOA (clr)    = ',flntc(i),' W m-2' 
         write (6,*) 'LW up flux sfc          = ',flns(i)+flwds(i),' W m-2' 
         write (6,*) 'LW down flux sfc        = ',flwds(i),' W m-2' 
c         write (6,*) 'LW down flux sfc (clr)  = ',flwds(i),' W m-2' 
         write (6,*) 'LW net flux sfc         = ',flns(i),' W m-2' 
         write (6,*) 'LW net flux sfc (clr)   = ',flnsc(i),' W m-2' 
         write (6,*) 'LW cloud forcing TOA    = ',flntc(i)-flnt(i),' W m-2' 
         write (6,*) 'LW cloud forcing sfc    = ',flnsc(i)-flns(i),' W m-2' 
         write (6,*) ''

         write (6,'(a)') 'Net Radiative Flux results (NRF=SW+LW):'
         write (6,*) 'NRF up flux TOA         = ',solin(i)-fsnt(i)+flnt(i),' W m-2' 
         write (6,*) 'NRF down flux TOA       = ',solin(i),' W m-2' 
c         write (6,*) 'NRF up flux TOA (clr)  = ',flntc(i),' W m-2' 
         write (6,*) 'NRF net flux TOA        = ',fsnt(i)-flnt(i),' W m-2' 
         write (6,*) 'NRF net flux TOA (clr)  = ',fsntc(i)-flntc(i),' W m-2' 
         write (6,*) 'NRF up flux sfc         = ',sols(i)+soll(i)+solsd(i)+solld(i)-fsns(i)+flns(i)+flwds(i),' W m-2' 
         write (6,*) 'NRF down flux sfc       = ',sols(i)+soll(i)+solsd(i)+solld(i)+flwds(i),' W m-2' 
c         write (6,*) 'NRF down flux sfc (clr)  = ',flwds(i),' W m-2' 
         write (6,*) 'NRF net flux sfc        = ',fsns(i)-flns(i),' W m-2' 
         write (6,*) 'NRF net flux sfc (clr)  = ',fsnsc(i)-flnsc(i),' W m-2' 
         write (6,*) 'NRF cloud forcing TOA   = ',fsnt(i)-fsntc(i)+flntc(i)-flnt(i),' W m-2' 
         write (6,*) 'NRF cloud forcing sfc   = ',fsns(i)-fsnsc(i)+flnsc(i)-flns(i),' W m-2' 
         write (6,*) ''

#ifdef CRM_SRB
         write (6,'(a)') 'Specified atmospheric constituents:'
         write (6,*) 'Visible AOD = ',tauvis
         write (6,*) 'H2O mpc     = ',mpc_H2O(i),' kg m-2'
         write (6,*) 'O3  mpc     = ',mpc_O3(i),' kg m-2'
         write (6,*) 'O3  mpc     = ',mpc_O3_DU(i),' Dobson'
         write (6,*) 'CO2 vmr     = ',co2vmr
         write (6,*) 'N2O vmr     = ',n2ovmr
         write (6,*) 'CH4 vmr     = ',ch4vmr
         write (6,*) 'F11 vmr     = ',f11vmr
         write (6,*) 'F12 vmr     = ',f12vmr
         write (6,*) ''

         write (6,'(a)') 'Column extinction optical depths:'
         write (6,'(1x,a,f6.4,a,f6.4,a)') 
     $               'Visible band = ',wvl_min(bnd_idx_vsb)*1.0e6,'--',wvl_max(bnd_idx_vsb)*1.0e6,' um' 
         write (6,*) 'Tau total    = ',odxc_ttl(i,bnd_idx_vsb)
         write (6,*) 'Tau Ray      = ',odxc_Ray(i,bnd_idx_vsb)
         write (6,*) 'Tau aer      = ',odxc_aer(i,bnd_idx_vsb)
         write (6,*) 'Tau lqd      = ',odxc_lqd(i,bnd_idx_vsb)
         write (6,*) 'Tau ice      = ',odxc_ice(i,bnd_idx_vsb)
         write (6,*) 'Tau O3       = ',odxc_O3(i,bnd_idx_vsb)
         write (6,*) 'Tau H2O      = ',odxc_H2O(i,bnd_idx_vsb)
         write (6,*) 'Tau O2       = ',odxc_O2(i,bnd_idx_vsb)
         write (6,*) 'Tau CO2      = ',odxc_CO2(i,bnd_idx_vsb)
         write (6,*) ''

         write (6,'(a)') 'Visible spectral fluxes:'
         write (6,'(1x,a,f6.4,a,f6.4,a)') 
     $               'Visible band          = ',wvl_min(bnd_idx_vsb)*1.0e6,'--',wvl_max(bnd_idx_vsb)*1.0e6,' um' 
         write (6,*) 'Down spc flux TOA     = ',flx_bnd_dwn_TOA(i,bnd_idx_vsb)*1.0e-6/wvl_dlt(bnd_idx_vsb), ' W m-2 um-1'
         write (6,*) 'Up spc flux TOA       = ',flx_bnd_up_TOA(i,bnd_idx_vsb)*1.0e-6/wvl_dlt(bnd_idx_vsb), ' W m-2 um-1'
         write (6,*) 'Down spc flux sfc     = ',flx_bnd_dwn_sfc(i,bnd_idx_vsb)*1.0e-6/wvl_dlt(bnd_idx_vsb), ' W m-2 um-1'
         write (6,*) 'Down spc flux dff sfc = ',flx_bnd_dwn_dff_sfc(i,bnd_idx_vsb)*1.0e-6/wvl_dlt(bnd_idx_vsb), ' W m-2 um-1'
         write (6,*) 'Down spc flux drc sfc = ',flx_bnd_dwn_drc_sfc(i,bnd_idx_vsb)*1.0e-6/wvl_dlt(bnd_idx_vsb), ' W m-2 um-1'
         write (6,*) 'Up spc flux sfc       = ',flx_bnd_up_sfc(i,bnd_idx_vsb)*1.0e-6/wvl_dlt(bnd_idx_vsb), ' W m-2 um-1'
         write (6,*) ''

c     Output TOA Radiation Budget (TRB) 
         write (6,'(a)') 'Solar TOA radiation budget components:'
         if (solin(i).le.0.0) then
            write (6,*) 'SW alb TOA              = Ill-defined'
            write (6,*) 'Vis alb TOA             = Ill-defined'
            write (6,*) 'NIR alb TOA             = Ill-defined'
            write (6,*) 'alb(NIR)/alb(SW) TOA    = Ill-defined'
            write (6,*) 'alb(NIR)/alb(Vis) TOA   = Ill-defined'
         else
            write (6,*) 'SW alb TOA              = ',alb_SW_TOA(i)
            write (6,*) 'Vis alb TOA             = ',alb_vsb_TOA(i)
            write (6,*) 'NIR alb TOA             = ',alb_NIR_TOA(i)
            write (6,*) 'alb(NIR)/alb(SW) TOA    = ',alb_NIR_SW_TOA(i)
            write (6,*) 'alb(NIR)/alb(Vis) TOA   = ',alb_NIR_vsb_TOA(i)
         endif                  ! endif
         write (6,*) 'SW down flux TOA        = ',flx_SW_dwn_TOA(i),' W m-2'
         write (6,*) 'SW up flux TOA          = ',flx_SW_up_TOA(i),' W m-2'
         write (6,*) 'SW net flux TOA         = ',flx_SW_dwn_TOA(i)-flx_SW_up_TOA(i),' W m-2'
         write (6,*) 'Vis down flux TOA       = ',flx_vsb_dwn_TOA(i),' W m-2'
         write (6,*) 'Vis up flux TOA         = ',flx_vsb_up_TOA(i),' W m-2'
         write (6,*) 'Vis net flux TOA        = ',flx_vsb_dwn_TOA(i)-flx_vsb_up_TOA(i),' W m-2'
         write (6,*) 'NIR down flux TOA       = ',flx_NIR_dwn_TOA(i),' W m-2'
         write (6,*) 'NIR up flux TOA         = ',flx_NIR_up_TOA(i),' W m-2'
c        write (6,*) 'NIR net flux TOA        = ',fsnirtsq(i),' W m-2'
         write (6,*) 'NIR net flux TOA        = ',flx_NIR_dwn_TOA(i)-flx_NIR_up_TOA(i),' W m-2'
         write (6,*) 'NIR net flux TOA N7     = ',fsnirt(i),' W m-2'
         write (6,*) 'NIR net flux TOA N7 (clr) = ',fsnrtc(i),' W m-2'
         write (6,*) ''

c     Output SRB in terms of the "true" (unscaled) direct and diffuse fluxes
         write (6,'(a)') 'Solar surface radiation budget components:'
         if (flx_SW_dwn_sfc(i).le.0.0) then
            write (6,*) 'SW alb sfc              = Ill-defined'
         else
            write (6,*) 'SW alb sfc              = ',alb_SW_sfc(i)
         endif                  ! endif
         if (flx_vsb_dwn_sfc(i).le.0.0) then
            write (6,*) 'Vis alb sfc             = Ill-defined'
         else
            write (6,*) 'Vis alb sfc             = ',alb_vsb_sfc(i)
         endif                  ! endif
         if (flx_NIR_dwn_sfc(i).le.0.0) then
            write (6,*) 'NIR alb sfc             = Ill-defined'
         else
            write (6,*) 'NIR alb sfc             = ',alb_NIR_sfc(i)
         endif                  ! endif
         write (6,*) 'SW down flux sfc        = ',flx_SW_dwn_sfc(i),' W m-2'
         write (6,*) 'SW down flux drc sfc    = ',flx_SW_dwn_drc_sfc(i),' W m-2'
         write (6,*) 'SW down flux dff sfc    = ',flx_SW_dwn_dff_sfc(i),' W m-2'
         if (flx_SW_dwn_drc_sfc(i).le.0.0) then
            write (6,*) 'SW down flux dff/drc    = Ill-defined'
         else                   
            write (6,*) 'SW down flux dff/drc    = ',dff_drc_SW_sfc(i)
         endif                  ! endif
         write (6,*) 'Vis down flux sfc       = ',flx_vsb_dwn_sfc(i),' W m-2'
         write (6,*) 'Vis down flux drc sfc   = ',flx_vsb_dwn_drc_sfc(i),' W m-2'
         write (6,*) 'Vis down flux dff sfc   = ',flx_vsb_dwn_dff_sfc(i),' W m-2'
         if (flx_vsb_dwn_drc_sfc(i).le.0.0) then
            write (6,*) 'Vis down flux dff/drc   = Ill-defined'
         else
            write (6,*) 'Vis down flux dff/drc   = ',dff_drc_vsb_sfc(i)
         endif                  ! endif
         write (6,*) 'NIR down flux sfc       = ',flx_NIR_dwn_sfc(i),' W m-2'
         write (6,*) 'NIR down flux drc sfc   = ',flx_NIR_dwn_drc_sfc(i),' W m-2'
         write (6,*) 'NIR down flux dff sfc   = ',flx_NIR_dwn_dff_sfc(i),' W m-2'
         if (flx_NIR_dwn_drc_sfc(i).le.0.0) then
            write (6,*) 'NIR down flux dff/drc   = Ill-defined'
         else
            write (6,*) 'NIR down flux dff/drc   = ',dff_drc_NIR_sfc(i)
         endif                  ! endif
         write (6,*) ''

         write (6,*) ''
#endif /* not CRM_SRB */

c     The following output the SRB in terms of the scaled beam
c     These may be useful to some investigators so we leave them here
c     Do not use these numbers unless you are sure you know what they mean, 
c     i.e., unless you know how delta scaling repartitions downwelling flux.
c         write (6,*) 'SW down flux sfc        = ',sols(i)+soll(i)+solsd(i)+solld(i),' W m-2'
c         write (6,*) 'SW down flux drc sfc    = ',sols(i)+soll(i),' W m-2'
c         write (6,*) 'SW down flux dff sfc    = ',solsd(i)+solld(i),' W m-2'
c         if (sols(i)+soll(i).le.0.0) then
c            write (6,*) 'SW down flux dff/drc    = Ill-defined'
c         else
c            write (6,*) 'SW down flux dff/drc    = ',(solsd(i)+solld(i))/(sols(i)+soll(i))
c         endif
c         write (6,*) 'Vis down flux sfc       = ',sols(i)+solsd(i),' W m-2'
c         write (6,*) 'Vis down flux drc sfc   = ',sols(i),' W m-2'
c         write (6,*) 'Vis down flux dff sfc   = ',solsd(i),' W m-2'
c         if (sols(i).le.0.0) then
c            write (6,*) 'Vis down flux dff/drc   = Ill-defined'
c         else
c            write (6,*) 'Vis down flux dff/drc   = ',solsd(i)/sols(i)
c         endif
c         write (6,*) 'NIR down flux sfc       = ',soll(i)+solld(i),' W m-2'
c         write (6,*) 'NIR down flux drc sfc   = ',soll(i),' W m-2'
c         write (6,*) 'NIR down flux dff sfc   = ',solld(i),' W m-2'
c         if (soll(i).le.0.0) then
c            write (6,*) 'NIR down flux dff/drc   = Ill-defined'
c         else
c            write (6,*) 'NIR down flux dff/drc   = ',solld(i)/soll(i)
c         endif

         write (6,'(a)') 'Cloud microphysics:'
         write (6,*) 'Level  Pressure      r_e lqd      r_e ice      Ice frc'
         write (6,*) '    #        mb           um           um          frc'
         do k=1,plev
            write (6,'(i4,4x,f8.3,1x,3(f12.7,1x))') 
     $           k,pmidm1(i,k)/100.0,rel(i,k),rei(i,k),fice(i,k)
         enddo                  ! end loop over levels
         write (6,*) ''

#ifdef CRM_SRB
         write (6,'(a)') 'SW Spectral Fluxes:'
         write (6,*) 'Band Wvl Min  Wvl Max  Wvl Ctr      TOA Dwn       TOA Up      Srf Dwn       Srf Up'
         write (6,*) '   #      um       um       um   W m-2 um-1   W m-2 um-1   W m-2 um-1   W m-2 um-1'
         do k=1,bnd_nbr_SW      
            write (6,'(i4,1x,3(f8.4,1x),4(f12.7,1x))') 
     $           k,wvl_min(k)*1.0e6,wvl_max(k)*1.0e6,wvl_ctr(k)*1.0e6,
     $           flx_bnd_dwn_TOA(i,k)*1.0e-6/wvl_dlt(k),flx_bnd_up_TOA(i,k)*1.0e-6/wvl_dlt(k),
     $           flx_bnd_dwn_sfc(i,k)*1.0e-6/wvl_dlt(k),flx_bnd_up_sfc(i,k)*1.0e-6/wvl_dlt(k)
         enddo                  ! end loop over bands
         write (6,*) ''

         write (6,'(a)') 'SW Scattering:'
         write (6,*) 'Interface  Pressure      SW down    SW direct   SW diffuse   SW dff/drc'
         write (6,*) '        #        mb        W m-2        W m-2        W m-2          frc'
         do k=1,plevp           ! NB: plevp not plev
            write (6,'(i4,8x,f8.3,1x,4(f12.7,1x))') 
     $           k,pintm1(i,k)/100.0,flx_SW_dwn(i,k),flx_SW_dwn_drc(i,k),
     $           flx_SW_dwn_dff(i,k),dff_drc_SW(i,k)
         enddo                  ! end loop over levels
         write (6,*) ''

         write (6,'(a)') 'SW Fluxes:'
         write (6,*) 'Interface  Pressure      SW down        SW up       SW Net'
         write (6,*) '        #        mb        W m-2        W m-2        W m-2'
         do k=1,plevp           ! NB: plevp not plev
            write (6,'(i4,8x,f8.3,1x,3(f12.7,1x))') 
     $           k,pintm1(i,k)/100.0,flx_SW_dwn(i,k),flx_SW_up(i,k),
     $           flx_SW_dwn(i,k)-flx_SW_up(i,k)
         enddo                  ! end loop over levels
         write (6,*) ''

         write (6,'(a)') 'LW Fluxes:'
         write (6,*) 'Interface  Pressure      LW down        LW up       LW Net'
         write (6,*) '        #        mb        W m-2        W m-2        W m-2'
         do k=1,plevp           ! NB: plevp not plev
            write (6,'(i4,8x,f8.3,1x,3(f12.7,1x))') 
     $           k,pintm1(i,k)/100.0,flx_LW_dwn(i,k),flx_LW_up(i,k),
     $           flx_LW_up(i,k)-flx_LW_dwn(i,k)
         enddo                  ! end loop over levels
         write (6,*) ''

         write (6,'(a)') 'Total SW+LW Fluxes:'
         write (6,*) 'Interface  Pressure         Down           Up          Net'
         write (6,*) '        #        mb        W m-2        W m-2        W m-2'
         do k=1,plevp           ! NB: plevp not plev
            write (6,'(i4,8x,f8.3,1x,3(f12.7,1x))') 
     $           k,pintm1(i,k)/100.0,flx_SW_dwn(i,k)+flx_LW_dwn(i,k),
     $           flx_SW_up(i,k)+flx_LW_up(i,k),
     $           flx_SW_dwn(i,k)+flx_LW_dwn(i,k)-(flx_SW_up(i,k)+flx_LW_up(i,k))
         enddo                  ! end loop over levels
         write (6,*) ''
#endif /* not CRM_SRB */

         write (6,'(a)') 'Heating rates:'
         write (6,*) 'Level  Pressure           SW           LW          Net'
         write (6,*) '    #        mb      K day-1      K day-1      K day-1'
         do k=1,plev
            write (6,'(i4,4x,f8.3,1x,3(f12.7,1x))') 
     $           k,pmidm1(i,k)/100.0,qrs(i,k)*86400.0,qrl(i,k)*86400.0,
     $           86400.0*(qrs(i,k)+qrl(i,k))
         enddo                  ! end loop over levels

      enddo                     ! end loop over longitude

c     Write results to a netCDF file
      call netcdf(
     $     clat,
     $     cld(1,1),
     $     clon(1),
     $     clwp(1,1),
     $     coszrs(1),
     $     effcld(1,1),
     $     fice(1,1),
c     
     $     flns(1),
     $     flnsc(1),
     $     flnt(1),
     $     flntc(1),
     $     flwds(1),
c     
     $     fsnirt(1),           ! [W m-2] Near-IR flux absorbed at TOA
     $     fsnirtsq(1),         ! [W m-2] Near-IR flux absorbed at TOA >= 0.7 microns
     $     fsnrtc(1),           ! [W m-2] Clear sky near-IR flux absorbed at TOA
c
     $     fsns(1),
     $     fsnsc(1),
     $     fsnt(1),
     $     fsntc(1),
     $     loctim(1),
c
     $     lwup(1),
     $     pintm1(1,1),
     $     pmidm1(1,1),
     $     ps(1),
     $     o3mmr(1,1),
     $     o3vmr(1,1),
c
     $     oro(1),
     $     qm1(1,1),
     $     qrl(1,1),
     $     qrs(1,1),
     $     rei(1,1),
c
     $     rel(1,1),
     $     snowh(1),
     $     solin(1),
     $     srfrad(1),
     $     tg(1),
c
     $     tm1(1,1),
     $     ts(1))

      write (6,'(a)') 'End CCM3 CRM'
      stop
      end

      subroutine getdat(
     $     aldif,
     $     aldir,
     $     asdif,
     $     asdir,
     $     clat,
     $     cld,
     $     clon,
     $     clwp,
     $     coslat,
     $     loctim,
     $     o3mmr,
     $     o3vmr,
     $     oro,
     $     pilnm1,
     $     pintm1,
     $     pmidm1,
     $     pmlnm1,
     $     ps,
     $     qm1, 
     $     snowh,
     $     tg,
     $     tm1,
     $     ts)     
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
      if (dbg_lvl.gt.0) write (6,*) lbl
      read (5,'(a80)') lbl
      if (dbg_lvl.gt.0) write (6,*) lbl
      read (5,'(a80)') lbl
      if (dbg_lvl.gt.0) write (6,*) lbl
      read (5,*) calday
c     CCM: control/comtim.h: calday set in CCM: ccmlsm_share/calendr()
      if (dbg_lvl.gt.0) write (6,*) calday,' = Julian day of year (1.5 = Noon, Jan 1; from 1 to 365)'
      read (5,*) lat_dgr
      if (dbg_lvl.gt.0) write (6,*) lat_dgr,' = Latitude (degrees North, from -90.0 to +90.0)'
#ifdef NEW_FMT
      read (5,*) lon_dgr
      if (dbg_lvl.gt.0) write (6,*) lon_dgr,' = Longitude (degrees East, from 0.0 to 360.0)'
#endif /* not NEW_FMT */
      read (5,'(a80)') lbl
      if (dbg_lvl.gt.0) write (6,*) lbl
      
      do k=1,plev
         read (5,*) lev(k),pmidm1(i,k),tm1(i,k),qm1(i,k),o3mmr(i,k),cld(i,k),clwp(i,k)
         if (dbg_lvl.gt.0) write (6,'(1x,i3,1x,6(1pe10.3,1x))') k,pmidm1(i,k),tm1(i,k),qm1(i,k),o3mmr(i,k),cld(i,k),clwp(i,k)
c     CCM: physics/cldfrc()
         cld(i,k)=min(cld(i,k),0.999)
      enddo                     ! end loop over lev
      
      read (5,*) ps(i)
      if (dbg_lvl.gt.0) write (6,*) ps(i),' = Surface pressure [mb]'
      
c     Convert pressures from mb to pascals and define interface pressures:
      ps(i)=ps(i)*100.0         ! mb --> Pa
      do k=1,plev
         pmidm1(i,k)=pmidm1(i,k)*100.0 ! mb --> Pa
         pmlnm1(i,k)=log(pmidm1(i,k))
      enddo                     ! end loop over lev
      do k=1,plevp
         if(k.eq.1) then
            pintm1(i,k)=pmidm1(i,k)/2.0 ! Pa
         else if (k.gt.1.and.k.le.plev) then
            pintm1(i,k)=0.5*(pmidm1(i,k-1)+pmidm1(i,k)) ! Pa
         else if (k.eq.plevp) then
            pintm1(i,k)=ps(i)   ! Pa
         endif                  ! end if
         pilnm1(i,k)=log(pintm1(i,k))
      enddo                     ! end loop over levp
      
      read (5,*) ts(i)
      if (dbg_lvl.gt.0) write (6,*) ts(i),' = Surface air temperature [K]'
      read (5,*) tg(i)
      if (dbg_lvl.gt.0) write (6,*) tg(i),' = Surface skin temperature [K]'
      read (5,*) oro(i)
      if (dbg_lvl.gt.0) write (6,*) oro(i),' = Surface type (ORO) flag'
      read (5,*) rghnss(i)
      if (dbg_lvl.gt.0) write (6,*) rghnss(i),' = Surface aerodynamic roughness [m] (obsolete)'
      read (5,*) snowh(i)
      if (dbg_lvl.gt.0) write (6,*) snowh(i),' = Snow depth (liq. equiv.) [m]'
      read (5,*) asdir(i)
      if (dbg_lvl.gt.0) write (6,*) asdir(i),' = Albedo (Vis, direct)'
      read (5,*) asdif(i)
      if (dbg_lvl.gt.0) write (6,*) asdif(i),' = Albedo (Vis, diffuse)'
      read (5,*) aldir(i)
      if (dbg_lvl.gt.0) write (6,*) aldir(i),' = Albedo (NIR, direct)'
      read (5,*) aldif(i)
      if (dbg_lvl.gt.0) write (6,*) aldif(i),' = Albedo (NIR, diffuse)'
      read (5,*) frctst(i)
      if (dbg_lvl.gt.0) write (6,*) frctst(i),' =  Fraction strong zenith angle dep. sfc. (obsolete)'
      read (5,*) co2vmr         ! CCM: physics/comvmr.h: co2vmr set in control/preset()
      if (dbg_lvl.gt.0) write (6,*) co2vmr,' = CO2 volume mixing ratio [# #-1]'
      read (5,*) n2ovmr         ! CCM: physics/comvmr.h: n2ovmr set in control/preset()
      if (dbg_lvl.gt.0) write (6,*) n2ovmr,' = N2O volume mixing ratio [# #-1]'
      read (5,*) ch4vmr         ! CCM: physics/comvmr.h: ch4vmr set in control/preset()
      if (dbg_lvl.gt.0) write (6,*) ch4vmr,' = CH4 volume mixing ratio [# #-1]'
      read (5,*) f11vmr         ! CCM: physics/comvmr.h: co2vmr set in control/preset()
      if (dbg_lvl.gt.0) write (6,*) f11vmr,' = CFC11 volume mixing ratio [# #-1]'
      read (5,*) f12vmr         ! CCM: physics/comvmr.h: f12vmr set in control/preset()
      if (dbg_lvl.gt.0) write (6,*) f12vmr,' = CFC12 volume mixing ratio [# #-1]'
      read (5,*) tauvis         ! CCM: physics/comsol.h: tauvis set in control/preset()
      if (dbg_lvl.gt.0) write (6,*) tauvis,' = Aerosol visible opt. depth'
      read (5,*) scon           ! CCM: physics/comsol.h: scon set in control/preset()
      if (dbg_lvl.gt.0) write (6,*) scon,' = Solar constant [W m-2]'
      read (5,*) iyear_AD       ! CCM: physics/comsol.h: iyear_AD set in control/preset()
      if (dbg_lvl.gt.0) write (6,*) iyear_AD,' = Year AD'
      read (5,*) lon_dgr
      if (dbg_lvl.gt.0) write (6,*) lon_dgr,' = Longitude (degrees East, from 0.0 to 360.0)'
      
c     CCM: physics/comsol.h: scon set in control/preset()
      scon=scon*1000.0          ! SI W m-2 = J m-2 s-1 --> CGS dyne cm-2 s-1 
      loctim(i)=(calday-aint(calday))*24.0
c     CCM: control/crdcon.h: pie set in physics/radini()
      pie=4.0*atan(1.0)
c     CCM: control/commap.h: clat set in dynamics/eul/initcom()
      clat=lat_dgr*pie/180.0
c     CCM: control/commap.h: clon set in dynamics/eul/initcom()
      clon(i)=lon_dgr*pie/180.0
c     CCM: dynamics/eul/linemsbc()
      coslat=cos(clat)
      
c     Given iyear_AD, orb_params() computes physics/comsol.h: obliqr, lambm0 and mvelpp
c     CCM: control/initext()
      log_print=.false.
      call orb_params( iyear_AD  , eccen  , obliq,    mvelp,
     $     obliqr    , lambm0 , mvelpp, log_print  )
      
c     Convert ozone mass mixing ratio to ozone volume mixing ratio.
c     CCM: physics/radinp.F (inverted version)
      vmmr=amo/amd
      do k=1,plev
         do i=1,plon
c     o3mmr(i,k)=vmmr*o3vmr(i,k)
            o3vmr(i,k)=o3mmr(i,k)/vmmr
         end do
      end do

      if (dbg_lvl.gt.0) write (6,*) 'End of profile data input'

      return
      end                       ! end getdat()

      subroutine writeric(nabem,absems,lngbuf,nrow)
c     Dummy routine for write of abs/ems data
c     writeric() is called by radclw()
      return 
      end

      subroutine readric(nabem,absems,lngbuf,nrow)
c     Dummy routine for read of abs/ems data
c     readric() is called by radclw()
      return 
      end                       ! end readric()

      subroutine outfld(name,tmp ,plond,lat,hbuf)
c     Dummy routine for history tape write
c     outfld() is called by radctl()
      return 
      end                       ! end outfld()

      subroutine radozn(lat     ,pmid    ,o3vmr   )
c     Dummy routine for ozone read
c     radozn() is called from radctl() 
c     CRM gets o3vmr from getdat() instead
      return
      end                       ! end radozn()
