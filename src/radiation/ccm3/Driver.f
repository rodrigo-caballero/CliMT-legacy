c
c This file is used by f2py to generate the Python C wrapper. 
c 
      subroutine driver(
c Input
     $     km,
     $     jm,
     $     im,
     $     idosw,
     $     idolw,
     $     pmidm1,
     $     ps,
     $     tm1,
     $     tg,
     $     qm1, 
     $     o3mmr,
     $     cldf,
     $     clwp,
     $     ciwp,
     $     aldif,
     $     aldir,
     $     asdif,
     $     asdir,
     $     zen,
     $     solin,
     $     r_liq,
     $     r_ice,
     $     co2vmr,
     $     n2ovmr,
     $     ch4vmr,
     $     f11vmr,
     $     f12vmr,
     $     tauvis,
     $     gravit,
     $     cpair,
     $     epsilo,
     $     stebol,
     $     dt,
c Output
     $     tinc,
     $     tdot,
     $     srfflx,
     $     qrs,
     $     qrl,
     $     swflx_out,
     $     lwflx_out,
     $     sw_cf_toa,
     $     sw_cf_srf,
     $     lw_cf_toa,
     $     lw_cf_srf,
     $     sw_toa,
     $     sw_srf,
     $     lw_toa,
     $     lw_srf)

c     Input
      integer km,jm,im
      integer idosw,idolw
      real*8 aldif(jm,im)
      real*8 aldir(jm,im)
      real*8 asdif(jm,im)
      real*8 asdir(jm,im)
      real*8 zen(jm,im)
      real*8 solin(jm,im)
      real*8 cldf(km,jm,im)
      real*8 clwp(km,jm,im)
      real*8 ciwp(km,jm,im)
      real*8 o3mmr(km,jm,im)
      real*8 r_liq(km,jm,im)
      real*8 r_ice(km,jm,im)
      real*8 pmidm1(km,jm,im)
      real*8 ps(jm,im)
      real*8 qm1(km,jm,im)
      real*8 tg(jm,im)
      real*8 tm1(km,jm,im)
      real*8 co2vmr
      real*8 n2ovmr
      real*8 ch4vmr
      real*8 f11vmr
      real*8 f12vmr
      real*8 tauvis
      real*8 gravit
      real*8 cpair
      real*8 epsilo
      real*8 stebol
      real*8 dt
cf2py intent(in,hide)  km,jm,im
cf2py intent(in) aldif, aldir, asdif, asdir, zen, solin, cldf, clwp, ciwp, o3mmr, r_liq, r_ice
cf2py intent(in) pmidm1, ps, qm1, tg, tm1, co2vmr, n2ovmr, ch4vmr, f11vmr, f12vmr
cf2py intent(in) tauvis, idosw, idolw, gravit, cpair, epsilo, stebol, dt

c    Output
      real*8 tinc(km,jm,im)
      real*8 tdot(km,jm,im)
      real*8 qrs(km,jm,im)
      real*8 qrl(km,jm,im)
      real*8 swflx_out(km,jm,im)
      real*8 lwflx_out(km,jm,im)
      real*8 sw_toa(jm,im)
      real*8 sw_srf(jm,im)
      real*8 lw_toa(jm,im)
      real*8 lw_srf(jm,im)
      real*8 sw_cf_toa(jm,im)
      real*8 sw_cf_srf(jm,im)
      real*8 lw_cf_toa(jm,im)
      real*8 lw_cf_srf(jm,im)
      real*8 srfflx(jm,im)
cf2py intent(out) tinc, tdot, srfflx, qrs, qrl, swflx_out, lwflx_out, 
cf2py intent(out) sw_toa, sw_srf, lw_toa, lw_srf
cf2py intent(out) sw_cf_toa, sw_cf_srf, lw_cf_toa, lw_cf_srf

c Local 
      real*8 swflx(km+1),lwflx(km+1) 

      do i=1,im
      do j=1,jm
      do k=1,km
      if (qm1(k,j,i).lt.0.) then
         print*,'qneg!',i,j,k
         qm1(k,j,i)=1.e-9
      endif
      enddo
      enddo
      enddo

      do i=1,im
      do j=1,jm

      call crm(
     $     aldif(j,i),
     $     aldir(j,i),
     $     asdif(j,i),
     $     asdir(j,i),
     $     zen(j,i),
     $     solin(j,i),
     $     cldf(1,j,i),
     $     clwp(1,j,i),
     $     ciwp(1,j,i),
     $     o3mmr(1,j,i),
     $     r_liq(1,j,i),
     $     r_ice(1,j,i),
     $     pmidm1(1,j,i),
     $     ps(j,i),
     $     qm1(1,j,i), 
     $     tg(j,i),
     $     tm1(1,j,i),
     $     co2vmr,
     $     n2ovmr,
     $     ch4vmr,
     $     f11vmr,
     $     f12vmr,
     $     tauvis,
     $     idosw,
     $     idolw,
     $     gravit,
     $     cpair,
     $     epsilo,
     $     stebol,
     $     qrs(1,j,i),
     $     qrl(1,j,i),
     $     swflx,
     $     lwflx,
     $     sw_cf_toa(j,i),
     $     sw_cf_srf(j,i),
     $     lw_cf_toa(j,i),
     $     lw_cf_srf(j,i),
     $     sw_toa(j,i),
     $     sw_srf(j,i),
     $     lw_toa(j,i),
     $     lw_srf(j,i) )

      if (idosw .ne. 1) then
         swflx  = solin(j,i)*(1.-asdir(j,i))
         sw_toa = solin(j,i)*(1.-asdir(j,i))
         sw_srf = solin(j,i)*(1.-asdir(j,i))
         sw_cf_toa = 0.
         sw_cf_srf = 0.
         qrs(:,j,i) = 0.
      endif
      swflx_out(:,j,i) = (swflx(1:km)+swflx(2:km+1))/2.
      lwflx_out(:,j,i) = (lwflx(1:km)+lwflx(2:km+1))/2.
      srfflx(j,i) = swflx(km+1) + lwflx(km+1)
      enddo
      enddo

      tdot   = qrs + qrl 
      tinc   = 2.*dt*tdot
      qrs = qrs * 86400.
      qrl = qrl * 86400.
      tdot = tdot * 86400.

      end
c-------------------------------------------------------------
      integer function get_nlev()

      integer get_km
      external get_km

      get_nlev = get_km()

      end 
