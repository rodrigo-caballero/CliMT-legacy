      subroutine flxoce(indx    ,npts    ,pmidm1  ,ubot    ,vbot    ,
     $                  tbot    ,qbot    ,thbot   ,zbot    ,ts      ,
     $                  ltheat  ,shf     ,lhf     ,taux    ,tauy    ,
     $                  lwup    )
C
C Compute ocean to atmosphere surface fluxes of sensible, latent heat
C and stress components:
C
C Assume:
C   1) Neutral 10m drag coeff: 
C         cdn = .0027/U10N + .000142 + .0000764 U10N
C   2) Neutral 10m stanton number: 
C         ctn = .0327 sqrt(cdn), unstable
C         ctn = .0180 sqrt(cdn), stable
C   3) Neutral 10m dalton number:  
C         cen = .0346 sqrt(cdn)
C   4) The saturation humidity of air at T(K): 
C         qsat(T)  (kg/m^3)
C
C Note:
C   1) here, tstar = <WT>/U*, and qstar = <WQ>/U*.
C   2) wind speeds should all be above a minimum speed (umin)
C
C--------------------------Code History---------------------------------
C
C Original:      Bill Large/M.Vertenstein, Sep. 1995
C Standardized:  L. Buja,     Feb 1996
C Reviewed:      B. Briegleb, March 1996
C
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <params.h>
#include <pmgrid.h>
#include <parpbl.h>
C------------------------------Commons----------------------------------
#include <comcon.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      integer indx(plond) ! Longitude index array (land)
      integer npts        ! Number of land points
C                         
      real pmidm1(plond)  ! Bottom level pressure
      real ubot(plond)    ! Bottom level u wind
      real vbot(plond)    ! Bottom level v wind
      real tbot(plond)    ! Bottom level temperature
      real qbot(plond)    ! Bottom level specific humidity
      real thbot(plond)   ! Bottom level potential temperature
      real zbot(plond)    ! Bottom level height above surface
      real ts(plond)      ! Surface temperature
      real ltheat(plond)  ! Latent heat for given srf conditions
C                         
C Output arguments        
C                         
      real shf(plond)     ! Initial sensible heat flux (W/m2)
      real lhf(plond)     ! Initial latent heat flux (W/m2)
      real taux(plond)    ! X surface stress (N/m2)
      real tauy(plond)    ! Y surface stress (N/m2)
      real lwup(plond)    ! Longwave up flux at surface (W/m2)
C
C---------------------------Local variables-----------------------------
C
      integer i,ii        ! Longitude indices
      real ssq            ! Surface saturation specific humidity
      real ustar          ! ustar
      real tstar          ! tstar
      real qstar          ! qstar
      real u10n           ! neutral 10 m wind speed over ocean
      real vmag           ! Surface wind magnitude
      real thvbot         ! Bottom lev virtual potential temp
      real delt           ! potential T difference (K)
      real delq           ! specific humidity difference (kg/kg)
      real rdn            ! sqrt of neutral exchange coeff (momentum)
      real rhn            ! sqrt of neutral exchange coeff (heat)
      real ren            ! sqrt of neutral exchange coeff (tracers)          
      real rd             ! sqrt of exchange coeff (momentum)
      real rh             ! sqrt of exchange coeff (heat)
      real re             ! sqrt of exchange coeff (tracers)
      real hol            ! Ref hgt (10m) / monin-obukhov length
      real xsq            ! Temporary variable
      real xqq            ! Temporary variable
      real alz            ! ln(zbot/z10)
      real cp             ! Specific heat of moist air
      real tau            ! Reference height stress
      real psimh          ! Stability funct at ref lev (momentum)
      real psixh          ! Stability funct at ref lev (heat & tracers) 
      real stable         ! Stability factor
      real rbot(plond)    ! Density at bottom model level
C
C--------------------------Statement functions--------------------------
C
      real psimhu         ! Unstable part of psimh
      real psixhu         ! Unstable part of psixh
      real qsat           ! Saturation specific humidty of air
      real cdn            ! Neutral drag coeff at bottom model level
      real xd             ! Dummy argument
      real Tk             ! Temperature (K)
      real Umps           ! Wind velocity (m/sec)
C
      qsat(Tk)   = 640380. / exp(5107.4/Tk)
      cdn(Umps)  = 0.0027 / Umps + .000142 + .0000764 * Umps
      psimhu(xd) = log((1.+xd*(2.+xd))*(1.+xd*xd)/8.)
     $              - 2.*atan(xd) + 1.571
      psixhu(xd) = 2. * log((1. + xd*xd)/2.)
C
C-----------------------------------------------------------------------
C
      do ii=1,npts
         i = indx(ii)
C
C---------------------------------------------------------------
C Set up necessary variables
C---------------------------------------------------------------
C
         rbot(i)= pmidm1(i) / (rair*tbot(i))
         vmag   = max(umin, sqrt(ubot(i)**2+vbot(i)**2))
         thvbot = thbot(i) * (1.0 + zvir*qbot(i))
         ssq    = 0.98 * qsat(ts(i)) / rbot(i)
         delt   = thbot(i) - ts(i)
         delq   = qbot(i) - ssq  
         alz    = log(zbot(i)/zref) 
         cp     = cpair*(1. + cpvir*ssq) 
C
C---------------------------------------------------------------
C First iteration to converge on Z/L and hence the fluxes
C---------------------------------------------------------------
C
C Initial guess for roots of neutral exchange coefficients, 
C assume z/L=0. and u10n is approximated by vmag.
C Stable if (thbot > ts ).
C
         stable = 0.5 + sign(0.5 , delt)
         rdn  = sqrt(cdn(vmag))
         rhn  = (1.-stable) * 0.0327 + stable * 0.018 
         ren  = 0.0346 
C
C Initial guess of ustar,tstar and qstar
C
         ustar = rdn*vmag
         tstar = rhn*delt
         qstar = ren*delq
C
C Compute stability and evaluate all stability functions
C Stable if (thbot > ts or hol > 0 )
C
         hol = xkar *gravit *zbot(i)
     $         * (tstar/thvbot + qstar/(1./zvir+qbot(i)))
     $         / ustar**2
         hol = sign( min(abs(hol),10.), hol )
         stable = 0.5 + sign(0.5 , hol)
         xsq   = max(sqrt(abs(1. - 16.*hol)) , 1.)
         xqq   = sqrt(xsq)
         psimh = -5. * hol * stable + (1.-stable)*psimhu(xqq)
         psixh = -5. * hol * stable + (1.-stable)*psixhu(xqq)
C
C Shift 10m neutral wind speed using old rdn coefficient
C
         rd   = rdn / (1.+rdn/xkar*(alz-psimh))
         u10n = vmag * rd / rdn
C
C Update the neutral transfer coefficients at 10m and neutral stability
C
         rdn = sqrt(cdn(u10n))
         ren = 0.0346
         rhn = (1.-stable) * 0.0327 + stable * 0.018 
C
C Shift all coeffs to measurement height and stability
C
         rd = rdn / (1.+rdn/xkar*(alz-psimh)) 
         rh = rhn / (1.+rhn/xkar*(alz-psixh)) 
         re = ren / (1.+ren/xkar*(alz-psixh))
C
C Update ustar, tstar, qstar using updated, shifted coeffs 
C
         ustar = rd * vmag 
         tstar = rh * delt 
         qstar = re * delq 
C
C---------------------------------------------------------------
C Second iteration to converge on Z/L and hence the fluxes
C---------------------------------------------------------------
C
C Recompute stability & evaluate all stability functions  
C Stable if (thbot > ts or hol > 0 )
C 
         hol = xkar * gravit * zbot(i)
     $        * (tstar/thvbot + qstar/(1./zvir+qbot(i)))
     $        / ustar**2
         hol = sign( min(abs(hol),10.), hol )
         stable = 0.5 + sign(0.5 , hol)
         xsq   = max(sqrt(abs(1. - 16.*hol)) , 1.)
         xqq   = sqrt(xsq)
         psimh = -5. * hol * stable + (1.-stable)*psimhu(xqq)
         psixh = -5. * hol * stable + (1.-stable)*psixhu(xqq)
C
C Shift 10m neutral wind speed using old rdn coefficient
C
         rd   = rdn / (1.+rdn/xkar*(alz-psimh))
         u10n = vmag * rd / rdn
C
C Update the neutral transfer coefficients at 10m and neutral stability
C
        rdn = sqrt(cdn(u10n))
        ren = 0.0346
        rhn = (1.-stable) * 0.0327 + stable * 0.018 
C
C Shift all coeffs to measurement height and stability

        rd = rdn / (1.+rdn/xkar*(alz-psimh)) 
        rh = rhn / (1.+rhn/xkar*(alz-psixh)) 
        re = ren / (1.+ren/xkar*(alz-psixh))
C
C Update ustar, tstar, qstar using updated, shifted coeffs 
C
        ustar = rd * vmag 
        tstar = rh * delt 
        qstar = re * delq 
C
C---------------------------------------------------------------
C Done, compute the fluxes
C---------------------------------------------------------------
C
C Compute surface stress components
C
        tau     =  rbot(i) * ustar * ustar 
        taux(i) =  -tau * ubot(i) / vmag 
        tauy(i) =  -tau * vbot(i) / vmag 
C
C Compute heat flux components at current surface temperature
C (Define positive latent and sensible heat as upwards into the atm)
C
        shf(i) = -cp * tau * tstar / ustar 
        lhf(i) = -ltheat(i) * tau * qstar / ustar
        lwup(i) = stebol * ts(i)**4 
C
      end do
C
      end

