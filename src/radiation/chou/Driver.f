      subroutine driver( km,jm,im, 
c In
     & isw, ilw, p, ps, t, ts, q, o3, cldf, clwp, ciwp, 
     & co2, n2o, ch4, cfc11, cfc12, cfc22, 
     & r_liq, r_ice,
     & aldif, aldir, asdif, asdir, zen, solin, g, Cp, dt,
c Out
     & tinc, tdot, srfflx, swhr, lwhr, swflx_out, lwflx_out, sw_cf_toa, 
     & sw_cf_srf, lw_cf_toa, lw_cf_srf, lwtau,
     & sw_toa, sw_srf, lw_toa, lw_srf)

c In:
      integer km,jm,im,isw,ilw
      real*8 t(km,jm,im)
      real*8 ts(jm,im)
      real*8 q(km,jm,im)
      real*8 p(km,jm,im)
      real*8 ps(jm,im)
      real*8 o3(km,jm,im)
      real*8 co2   
      real*8 n2o   
      real*8 ch4   
      real*8 cfc11 
      real*8 cfc12 
      real*8 cfc22 
      real*8 clwp(km,jm,im) 
      real*8 ciwp(km,jm,im) 
      real*8 cldf(km,jm,im)
      real*8 r_liq(km,jm,im)
      real*8 r_ice(km,jm,im)
      real*8 asdir(jm,im)  
      real*8 asdif(jm,im)  
      real*8 aldir(jm,im)  
      real*8 aldif(jm,im)  
      real*8 zen(jm,im)  
      real*8 solin(jm,im)  
      real*8 g    
      real*8 Cp   
      real*8 dt
cf2py intent(in,hide) km,jm,im
cf2py intent(in) t, q, p, ps, o3, co2, n2o, ch4, cfc11, cfc12, cfc22, clwp, ciwp
cf2py intent(in) cldf, r_liq, r_ice, aldif, aldir, asdif, asdir, zen, solin, g, Cp, dt

c Out:
      real*8 tinc(km,jm,im)   
      real*8 tdot(km,jm,im)   
      real*8 swhr(km,jm,im)   
      real*8 lwhr(km,jm,im)   
      real*8 lwtau(km,jm,im)   
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
cf2py intent(out) tinc, tdot, srfflx, swhr, swflx_out, sw_cf_toa, sw_cf_srf
cf2py intent(out) lwhr, lwflx_out, lw_cf_toa, lw_cf_srf, lwtau
cf2py intent(out) sw_toa, sw_srf, lw_toa, lw_srf
 
c Local 
      real*8 swflx(km+1),lwflx(km+1) 
      
      swhr=0.
      lwhr=0.
      swflx=0.
      lwflx=0.
      srfflx=0.

      do i=1,im
      do j=1,jm
      do k=1,km
      if (q(k,j,i).lt.0.) then
         print*,'qneg!',i,j,k
         q(k,j,i)=1.e-9
      endif
      enddo
      enddo
      enddo

      do i=1,im
      do j=1,jm

         if (isw .eq. 1) then
            call chou_sw(
     &           t(1,j,i), q(1,j,i), 
     &           p(1,j,i), ps(j,i), o3(1,j,i), co2, 
     &           clwp(1,j,i),ciwp(1,j,i), cldf(1,j,i), r_liq(1,j,i), r_ice(1,j,i),
     &           aldif(j,i), aldir(j,i), asdif(j,i), asdir(j,i), zen(j,i), solin(j,i), g, Cp,
     &           swflx, sw_cf_toa(j,i), sw_cf_srf(j,i), swhr(1,j,i))
         else
            swflx          = solin(j,i)*(1.-asdir(j,i))
            swhr(:,j,i)    = 0.
            sw_cf_toa(j,i) = 0.
            sw_cf_srf(j,i) = 0.
         endif

         if (ilw .eq. 1) then
            call chou_lw( 
     &           t(1,j,i), ts(j,i), q(1,j,i), 
     &           p(1,j,i), ps(j,i), o3, co2, n2o, ch4, cfc11, cfc12, cfc22, 
     &           clwp(1,j,i), clwp(1,j,i), cldf(1,j,i), r_liq(1,j,i), r_ice(1,j,i), g, Cp,
     &           lwflx, lw_cf_toa(j,i), lw_cf_srf(j,i), lwhr(1,j,i), lwtau(1,j,i))
         endif

      sw_toa = swflx(1)
      sw_srf = swflx(km+1)
      lw_toa = lwflx(1)
      lw_srf = lwflx(km+1)
      swflx_out(:,j,i) = (swflx(1:km)+swflx(2:km+1))/2.
      lwflx_out(:,j,i) = (lwflx(1:km)+lwflx(2:km+1))/2.
      srfflx(j,i) = swflx(km+1) + lwflx(km+1)

      enddo
      enddo

      tdot = swhr+lwhr
      tinc = 2.*dt*tdot
      swhr = swhr * 86400.
      lwhr = lwhr * 86400.
      tdot = tdot * 86400.

      end
c-------------------------------------------------------------
      integer function get_nlev()

      integer get_km
      external get_km

      get_nlev = get_km()

      end 
