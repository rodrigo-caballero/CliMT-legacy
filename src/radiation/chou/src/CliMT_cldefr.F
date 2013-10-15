      subroutine cldefr(t, rel, rei, fice, ps, pmid)
C-----------------------------------------------------------------------
C
C Compute cloud drop size and cloud fraction.
c
c This routine adapted from CCM3.6 radiation code
c
c rca@geosci.uchicago.edu 2003
C------------------------------Arguments--------------------------------
C
      parameter(km=KM)

C Input arguments
C
      real t(km)        ! Temperature
      real ps           ! Surface pressure
      real pmid(km)     ! Midpoint pressures
C
C Output arguments
C
      real rel(km)      ! Liquid effective drop size (microns)
      real rei(km)      ! Ice effective drop size (microns)
      real fice(km)     ! Fractional ice content within cloud
      real pirnge       ! Nrmlzd pres range for ice particle changes
      real picemn       ! Normalized pressure below which rei=reimax
      real rirnge       ! Range of ice radii (reimax - 10 microns)
      real reimax       ! Maximum ice effective radius
      real pnrml        ! Normalized pressure
      real weight       ! Coef. for determining rei as fn of P/PS
C
C---------------------------Local workspace-----------------------------
C
      integer k         ! Lev index
      real rliq         ! Temporary liquid drop size
C
C-----------------------------------------------------------------------
C
      do k=1,km
C
C Set liquid drop radius to 10 micron
C
          rel(k) = 10.0
C
C Determine rei as function of normalized pressure
C
          reimax   = 30.0
          rirnge   = 20.0 
          pirnge   = 0.4
          picemn   = 0.4
C
          pnrml    = pmid(k)/ps
          weight   = max(min((pnrml-picemn)/pirnge,1.0),0.)
          rei(k)   = reimax - rirnge*weight
C
C Define fractional amount of cloud that is ice
C
C If warmer than -10 degrees C then water phase
C
          if(t(k).gt.263.16) fice(k) = 0.0
C
C If colder than -10 degrees C but warmer than -30 C mixed phase
C
          if (t(k).le.263.16.and.t(k).ge.243.16) then
            fice(k) =(263.16-t(k)) / 20.0
          end if
C
C If colder than -30 degrees C then ice phase
C
          if (t(k).lt.243.16) fice(k) = 1.0
C
C Turn off ice radiative properties by setting fice = 0.0
C
C+             fice(k) = 0.0
C
      end do
C
      end
 
