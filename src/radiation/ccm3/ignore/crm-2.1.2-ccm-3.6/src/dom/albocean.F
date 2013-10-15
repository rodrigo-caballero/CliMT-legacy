#include <misc.h>
#include <params.h>
      subroutine albocean(oro     ,snowh   ,coszrs  ,asdir  ,aldir   ,
     $                    asdif   ,aldif   )
C-----------------------------------------------------------------------
C
C Compute surface albedos
C
C Computes surface albedos for direct/diffuse incident radiation for
C two spectral intervals:
C   s = 0.2-0.7 micro-meters
C   l = 0.7-5.0 micro-meters
C
C Albedos specified as follows:
C
C Ocean           Uses solar zenith angle to compute albedo for direct
C                 radiation; diffuse radiation values constant; albedo
C                 independent of spectral interval and other physical
C                 factors such as ocean surface wind speed.
C
C Ocean with      Surface albs specified; combined with overlying snow
C   sea ice       
C
C For more details , see Briegleb, Bruce P., 1992: Delta-Eddington
C Approximation for Solar Radiation in the NCAR Community Climate Model,
C Journal of Geophysical Research, Vol 97, D7, pp7603-7612).
C
C---------------------------Code history--------------------------------
C
C Original version:        CCM1
C Standardized:            J. Rosinski, Jun 1992
C                          L. Buja,     Feb 1996
C Reviewed:                J. Kiehl, B. Briegleb, Aug 1992
C Reviewed:                J. Kiehl,    April 1996
C Rewritten for land only: J. Rosinski, May 1994
C Reviewed:                B. Briegleb, May 1996
C
C-----------------------------------------------------------------------
c
c $Id: albocean.F,v 1.1 2004/09/07 15:27:35 rca Exp $
c $Author: rca $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <prgrid.h>
C-----------------------------------------------------------------------
#if ( defined SPMD )
#include <comspmd.h>
#endif
C-----------------------------------------------------------------------
#include <albedo.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real oro(plond)       ! Land/ocean/seaice flag
      real snowh(plond)     ! Snow depth (liquid water equivalent)
      real coszrs(plond)    ! Cosine solar zenith angle
C
C Output arguments
C
      real asdir(plond)     ! Srf alb for direct rad   0.2-0.7 micro-ms
      real aldir(plond)     ! Srf alb for direct rad   0.7-5.0 micro-ms
      real asdif(plond)     ! Srf alb for diffuse rad  0.2-0.7 micro-ms
      real aldif(plond)     ! Srf alb for diffuse rad  0.7-5.0 micro-ms
C
C---------------------------Local variables-----------------------------
C
      integer i             ! Longitude index
      real frsnow           ! Horizontal fraction of snow cover
      real snwhgt           ! Physical snow height
      real rghsnw           ! Roughness for horizontal snow cover fractn
      real sasdir(plond)    ! Snow alb for direct rad  0.2-0.7 micro-ms
      real saldir(plond)    ! Snow alb for direct rad  0.7-5.0 micro-ms
      real sasdif(plond)    ! Snow alb for diffuse rad  0.2-0.7 micro-ms
      real saldif(plond)    ! Snow alb for diffuse rad  0.7-5.0 micro-ms
C
C--------------------------Statement functions--------------------------
C
      logical ocean,seaice
      ocean(i) = nint(oro(i)).eq.0
      seaice(i) = nint(oro(i)).eq.2
C
C-----------------------------------------------------------------------
C
C Initialize all ocean/sea ice surface albedos to zero
C
      do i=1,plon
        if (ocean(i) .or. seaice(i)) then
          asdir(i) = 0.
          aldir(i) = 0.
          asdif(i) = 0.
          aldif(i) = 0.
        end if
      end do
      do i=1,plon
        if (seaice(i) .and. coszrs(i).gt.0.0) then
          asdir(i)  = sices
          aldir(i)  = sicel
          asdif(i) = asdir(i)
          aldif(i) = aldir(i)
          sasdif(i) = snws
          saldif(i) = snwl
        end if
      end do
      do i=1,plon
        if (seaice(i)) then
          if (snowh(i).gt.0. .and. coszrs(i).gt.0.) then
            if (coszrs(i).lt.0.5) then
C
C Zenith angle regime 1 ( coszrs < 0.5 ).
C Set direct snow albedos (limit to 0.98 max)
C
              sasdir(i) = min(0.98,sasdif(i) + (1. - sasdif(i))*0.5*
     $                        ((3./(1. + 4.*coszrs(i))) - 1.))
              saldir(i) = min(0.98,saldif(i) + (1. - saldif(i))*0.5*
     $                        ((3./(1. + 4.*coszrs(i))) - 1.))
            else
C
C Zenith angle regime 2 ( coszrs >= 0.5 )
C
              sasdir(i) = snws
              saldir(i) = snwl
            end if
C
C Compute both diffuse and direct total albedos
C
            snwhgt = 20.*snowh(i)
            rghsnw = 0.25
            frsnow = snwhgt/(rghsnw + snwhgt)
            asdir(i)  = asdir(i) *(1. - frsnow) + sasdir(i) *frsnow
            aldir(i)  = aldir(i) *(1. - frsnow) + saldir(i) *frsnow
            asdif(i) = asdif(i)*(1. - frsnow) + sasdif(i)*frsnow
            aldif(i) = aldif(i)*(1. - frsnow) + saldif(i)*frsnow
          end if
        end if
      end do
C
C Ice-free ocean albedos function of solar zenith angle only, and
C independent of spectral interval:
C
      do i=1,plon
        if (ocean(i).and.coszrs(i).gt.0.) then
          aldir(i)  = (.026/(coszrs(i)**1.7 + .065)) +
     $                 (.15*(coszrs(i) - 0.10)*
     $                      (coszrs(i) - 0.50)*
     $                      (coszrs(i) - 1.00)  )
          asdir(i)  = aldir(i)
          aldif(i) = 0.06
          asdif(i) = 0.06
        end if
      end do
C   
      return
      end
 
