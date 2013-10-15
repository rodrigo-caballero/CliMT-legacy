#include <misc.h>
#include <params.h>
      subroutine dadadj(lat     ,pmid    ,pint    ,pdel    ,t       ,
     $                  q       )
C-----------------------------------------------------------------------
C
C GFDL style dry adiabatic adjustment 
C if stratification is unstable, adjustment to the dry adiabatic lapse
C rate is forced subject to the condition that enthalpy is conserved.
C
C---------------------------Code history--------------------------------
C
C Original version:  CCM1
C Standardized:      J. Rosinski, June 1992
C Reviewed:          J. Hack, G. Taylor, August 1992
C
C-----------------------------------------------------------------------
c
c $Id: dadadj.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <pmgrid.h>
C------------------------------Commons----------------------------------
      integer niter           ! number of iterations for convergence
      parameter (niter = 15)
#include <comadj.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      integer lat             ! latitude index (S->N)

      real pmid(plond,plev)   ! pressure at model levels
      real pint(plond,plevp)  ! pressure at model interfaces
      real pdel(plond,plev)   ! vertical delta-p
C
C Input/output arguments
C
      real t(plond,plev)      ! temperature (K)
      real q(plond,plev)      ! specific humidity
C
C---------------------------Local workspace-----------------------------
C
      integer i,k             ! longitude, level indices
      integer jiter           ! iteration index

      real c1dad(plev)        ! intermediate constant
      real c2dad(plev)        ! intermediate constant
      real c3dad(plev)        ! intermediate constant
      real c4dad(plev)        ! intermediate constant
      real gammad             ! dry adiabatic lapse rate (deg/Pa)
      real zeps               ! convergence criterion (deg/Pa)
      real rdenom             ! reciprocal of denominator of expression
      real dtdp               ! delta-t/delta-p
      real zepsdp             ! zeps*delta-p
      real zgamma             ! intermediate constant
      real qave               ! mean q between levels

      logical ilconv          ! .TRUE. ==> convergence was attained
      logical dodad(plond)    ! .TRUE. ==> do dry adjustment
C
C-----------------------------------------------------------------------
C
      zeps = 2.0e-5           ! set convergence criteria
C
C Find gridpoints with unstable stratification
C
      do i=1,plon
        gammad = cappa*0.5*(t(i,2) + t(i,1))/pint(i,2)
        dtdp = (t(i,2) - t(i,1))/(pmid(i,2) - pmid(i,1))
        dodad(i) = (dtdp + zeps) .gt. gammad
      end do
      do k=2,nlvdry
        do i=1,plon
          gammad = cappa*0.5*(t(i,k+1) + t(i,k))/pint(i,k+1)
          dtdp = (t(i,k+1) - t(i,k))/(pmid(i,k+1) - pmid(i,k))
          dodad(i) = dodad(i) .or. (dtdp + zeps).gt.gammad
        end do
      end do
C
C Make a dry adiabatic adjustment
C Note: nlvdry ****MUST**** be < plev
C
      do 80 i=1,plon
        if (dodad(i)) then
          zeps = 2.0e-5
          do k=1,nlvdry
            c1dad(k) = cappa*0.5*(pmid(i,k+1)-pmid(i,k))/pint(i,k+1)
            c2dad(k) = (1. - c1dad(k))/(1. + c1dad(k))
            rdenom = 1./(pdel(i,k)*c2dad(k) + pdel(i,k+1))
            c3dad(k) = rdenom*pdel(i,k)
            c4dad(k) = rdenom*pdel(i,k+1)
          end do
   50     do jiter=1,niter
            ilconv = .true.
            do k=1,nlvdry
              zepsdp = zeps*(pmid(i,k+1) - pmid(i,k))
              zgamma = c1dad(k)*(t(i,k) + t(i,k+1))
              if ((t(i,k+1)-t(i,k)) .ge. (zgamma+zepsdp)) then
                ilconv = .false.
                t(i,k+1) = t(i,k)*c3dad(k) + t(i,k+1)*c4dad(k)
                t(i,k) = c2dad(k)*t(i,k+1)
                qave = (pdel(i,k+1)*q(i,k+1) + pdel(i,k)*q(i,k))/
     $                 (pdel(i,k+1)          + pdel(i,k)       )
                q(i,k+1) = qave
                q(i,k) = qave
              end if
            end do
            if (ilconv) go to 80 ! convergence => next longitude
          end do
C
C Double convergence criterion if no convergence in niter iterations
C
          zeps = zeps + zeps
          if (zeps.gt.1.e-4) then
            write(6,*)'DADADJ: No convergence in dry adiabatic ',
     $                'adjustment'
            write(6,800)lat,i,zeps
            call blowup
          else
            write(6,810) zeps,lat,i
            go to 50
          end if
        end if
   80 continue
      return
C
C Formats
C
  800 format(' lat,lon = ',2i5,', zeps= ',e9.4)
  810 format(//,'DADADJ: Convergence criterion doubled to EPS=',E9.4,
     $          ' for'/'        DRY CONVECTIVE ADJUSTMENT at Lat,Lon=',
     $          2i5)
      end
 
