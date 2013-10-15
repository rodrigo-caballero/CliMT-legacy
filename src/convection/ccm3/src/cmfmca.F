#include <misc.h>
#include <params.h>
      subroutine cmfmca(lat     ,nstep   ,tdt     ,pmid    ,pdel    ,
     $                  rpdel   ,zm      ,tpert   ,qpert   ,phis    ,
     $                  pblht   ,t       ,q       ,cmfdt   ,cmfdq   ,
     $                  cmfmc   ,cmfdqr  ,cmfsl   ,cmflq   ,precc   ,
     $                  qc      ,cnt     ,cnb     )
C-----------------------------------------------------------------------
C
C Moist convective mass flux procedure:
C If stratification is unstable to nonentraining parcel ascent,
C complete an adjustment making successive use of a simple cloud model
C consisting of three layers (sometimes referred to as a triplet)
C 
C Code generalized to allow specification of parcel ("updraft")
C properties, as well as convective transport of an arbitrary
C number of passive constituents (see q array).  The code
C is written so the water vapor field is passed independently
C in the calling list from the block of other transported
C constituents, even though as currently designed, it is the
C first component in the constituents field. 
C
C----------------------------Code History-------------------------------
C
C Original version:  J. J. Hack, March 22, 1990
C Standardized:      J. Rosinski, June 1992
C Reviewed:          J. Hack, G. Taylor, August 1992
C
C#######################################################################
C#                                                                     #
C# Debugging blocks are marked this way for easy identification        #
C#                                                                     #
C#######################################################################
C
C-----------------------------------------------------------------------
c
c $Id: cmfmca.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <pmgrid.h>
      real ssfac               ! supersaturation bound (detrained air)
      parameter (ssfac = 1.001)
C------------------------------Commons----------------------------------
#include <comcmf.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      integer lat              ! latitude index (S->N)
      integer nstep            ! current time step index

      real tdt                 ! 2 delta-t (seconds)
      real pmid(plond,plev)    ! pressure
      real pdel(plond,plev)    ! delta-p
      real rpdel(plond,plev)   ! 1./pdel
      real zm(plond,plev)      ! height abv sfc at midpoints
      real tpert(plond)        ! PBL perturbation theta
      real qpert(plond,pcnst)  ! PBL perturbation specific humidity 
      real phis(plond)         ! surface geopotential
      real pblht(plond)        ! PBL height (provided by PBL routine)
C
C Input/output arguments
C
      real t(plond,plev)       ! temperature (t bar)
      real q(plond,plev,pcnst) ! specific humidity (sh bar)
C
C Output arguments
C
      real cmfdt(plond,plev)   ! dt/dt due to moist convection
      real cmfdq(plond,plev)   ! dq/dt due to moist convection
      real cmfmc(plond,plev )  ! moist convection cloud mass flux
      real cmfdqr(plond,plev)  ! dq/dt due to convective rainout 
      real cmfsl(plond,plev )  ! convective lw static energy flux
      real cmflq(plond,plev )  ! convective total water flux
      real precc(plond)        ! convective precipitation rate
      real qc(plond,plev)      ! dq/dt due to rainout terms
      real cnt(plond)          ! top level of convective activity   
      real cnb(plond)          ! bottom level of convective activity
C
C---------------------------Local workspace-----------------------------
C
      real gam(plond,plev)     ! 1/cp (d(qsat)/dT)
      real sb(plond,plev)      ! dry static energy (s bar)
      real hb(plond,plev)      ! moist static energy (h bar)
      real shbs(plond,plev)    ! sat. specific humidity (sh bar star)
      real hbs(plond,plev)     ! sat. moist static energy (h bar star)
      real shbh(plond,plevp)   ! specific humidity on interfaces
      real sbh(plond,plevp)    ! s bar on interfaces
      real hbh(plond,plevp)    ! h bar on interfaces
      real cmrh(plond,plevp)   ! interface constituent mixing ratio 
      real prec(plond)         ! instantaneous total precipitation
      real dzcld(plond)        ! depth of convective layer (m)
      real beta(plond)         ! overshoot parameter (fraction)
      real betamx(plond)       ! local maximum on overshoot
      real eta(plond)          ! convective mass flux (kg/m^2 s)
      real etagdt(plond)       ! eta*grav*dt
      real cldwtr(plond)       ! cloud water (mass)
      real rnwtr(plond)        ! rain water  (mass)
      real sc  (plond)         ! dry static energy   ("in-cloud")
      real shc (plond)         ! specific humidity   ("in-cloud")
      real hc  (plond)         ! moist static energy ("in-cloud")
      real cmrc(plond)         ! constituent mix rat ("in-cloud")
      real dq1(plond)          ! shb  convective change (lower lvl)
      real dq2(plond)          ! shb  convective change (mid level)
      real dq3(plond)          ! shb  convective change (upper lvl)
      real ds1(plond)          ! sb   convective change (lower lvl)
      real ds2(plond)          ! sb   convective change (mid level)
      real ds3(plond)          ! sb   convective change (upper lvl)
      real dcmr1(plond)        ! q convective change (lower lvl)
      real dcmr2(plond)        ! q convective change (mid level)
      real dcmr3(plond)        ! q convective change (upper lvl)
      real estemp(plond,plev)  ! saturation vapor pressure (scratch)
      real vtemp1(2*plond)     ! intermediate scratch vector
      real vtemp2(2*plond)     ! intermediate scratch vector
      real vtemp3(2*plond)     ! intermediate scratch vector
      real vtemp4(2*plond)     ! intermediate scratch vector
      integer indx1(plond)     ! longitude indices for condition true
      logical etagt0           ! true if eta > 0.0
      real sh1                 ! dummy arg in qhalf statement func.
      real sh2                 ! dummy arg in qhalf statement func.
      real shbs1               ! dummy arg in qhalf statement func.
      real shbs2               ! dummy arg in qhalf statement func.
      real cats                ! modified characteristic adj. time
      real rtdt                ! 1./tdt
      real qprime              ! modified specific humidity pert.
      real tprime              ! modified thermal perturbation
      real pblhgt              ! bounded pbl height (max[pblh,1m])
      real fac1                ! intermediate scratch variable
      real shprme              ! intermediate specific humidity pert.
      real qsattp              ! sat mix rat for thermally pert PBL parcels 
      real dz                  ! local layer depth
      real temp1               ! intermediate scratch variable
      real b1                  ! bouyancy measure in detrainment lvl
      real b2                  ! bouyancy measure in condensation lvl
      real temp2               ! intermediate scratch variable
      real temp3               ! intermediate scratch variable
      real g                   ! bounded vertical gradient of hb
      real tmass               ! total mass available for convective exch
      real denom               ! intermediate scratch variable
      real qtest1              ! used in negative q test (middle lvl) 
      real qtest2              ! used in negative q test (lower lvl) 
      real fslkp               ! flux lw static energy (bot interface)
      real fslkm               ! flux lw static energy (top interface)
      real fqlkp               ! flux total water (bottom interface)
      real fqlkm               ! flux total water (top interface)
      real botflx              ! bottom constituent mixing ratio flux
      real topflx              ! top constituent mixing ratio flux
      real efac1               ! ratio q to convectively induced chg (btm lvl)
      real efac2               ! ratio q to convectively induced chg (mid lvl)
      real efac3               ! ratio q to convectively induced chg (top lvl)
      real tb(plond,plev)      ! working storage for temp (t bar)
      real shb(plond,plev)     ! working storage for spec hum (sh bar)
      real adjfac              ! adjustment factor (relaxation related)
#if ( defined DIAGNS )
C
C  Following 7 real variables are used in diagnostics calculations   
C
      real rh                  ! relative humidity 
      real es                  ! sat vapor pressure 
      real hsum1               ! moist static energy integral
      real qsum1               ! total water integral 
      real hsum2               ! final moist static energy integral
      real qsum2               ! final total water integral
      real fac                 ! intermediate scratch variable
#endif
      integer i,k              ! longitude, level indices
      integer ii               ! index on "gathered" vectors
      integer len1             ! vector length of "gathered" vectors
      integer m                ! constituent index
      integer ktp              ! tmp indx used to track top of convective layer
#if ( defined DIAGNS )
      integer n                ! vertical index     (diagnostics)
      integer kp               ! vertical index     (diagnostics)
      integer kpp              ! index offset, kp+1 (diagnostics)
      integer kpm1             ! index offset, kp-1 (diagnostics)
#endif
C
C---------------------------Statement functions-------------------------
C
      real qhalf
      qhalf(sh1,sh2,shbs1,shbs2) = min(max(sh1,sh2),
     $                            (shbs2*sh1 + shbs1*sh2)/(shbs1+shbs2))
C
C-----------------------------------------------------------------------
C
C Ensure that characteristic adjustment time scale (cmftau) assumed
C in estimate of eta isn't smaller than model time scale (tdt)
C The time over which the convection is assumed to act (the adjustment
C time scale) can be applied with each application of the three-level
C cloud model, or applied to the column tendencies after a "hard"
C adjustment (i.e., on a 2-delta t time scale) is evaluated
C
      if (rlxclm) then
        cats   = tdt               ! relaxation applied to column
        adjfac = tdt/(max(tdt,cmftau))
      else
        cats   = max(tdt,cmftau) ! relaxation applied to triplet
        adjfac = 1.0
      endif
      rtdt = 1.0/tdt
C
C Move temperature and moisture into working storage
C
      do k=limcnv,plev
        do i=1,plon
          tb (i,k) = t(i,k)
          shb(i,k) = q(i,k,1)
        end do
      end do
C
C Compute sb,hb,shbs,hbs
C
      call aqsatd(tb      ,pmid    ,estemp ,shbs    ,gam     ,
     $            plond   ,plon    ,plev   ,limcnv  ,plev    )
C
      do k=limcnv,plev
        do i=1,plon
          sb (i,k) = cp*tb(i,k) + zm(i,k)*grav + phis(i)
          hb (i,k) = sb(i,k) + hlat*shb(i,k)
          hbs(i,k) = sb(i,k) + hlat*shbs(i,k)
        end do
      end do
C
C Compute sbh, shbh
C
      do k=limcnv+1,plev
        do i=1,plon
          sbh (i,k) = 0.5*(sb(i,k-1) + sb(i,k))
          shbh(i,k) = qhalf(shb(i,k-1),shb(i,k),shbs(i,k-1),shbs(i,k))
          hbh (i,k) = sbh(i,k) + hlat*shbh(i,k)
        end do
      end do
C
C Specify properties at top of model (not used, but filling anyway)
C
      do i=1,plon
        sbh (i,limcnv) = sb(i,limcnv)
        shbh(i,limcnv) = shb(i,limcnv)
        hbh (i,limcnv) = hb(i,limcnv)
      end do
C
C Zero vertically independent control, tendency & diagnostic arrays
C
      do i=1,plon
        prec(i)  = 0.0
        dzcld(i) = 0.0
        cnb(i)   = 0.0
        cnt(i)   = float(plev+1)
      end do
#if ( defined DIAGNS )
C#######################################################################
C#                                                                     #
C#    output initial thermodynamic profile if debug diagnostics        #
C#                                                                     #
      if (lat.eq.jloc .and. nstep.ge.nsloc) then
        i = iloc
C#                                                                     #
C#       approximate vertical integral of moist static energy          #
C#       and total preciptable water                                   #
C#                                                                     #
        hsum1 = 0.0
        qsum1 = 0.0
        do k=limcnv,plev
          hsum1 = hsum1 + pdel(i,k)*rgrav*hb(i,k)
          qsum1 = qsum1 + pdel(i,k)*rgrav*shb(i,k)
        end do
C#                                                                     #
        write (6,8010)
        fac = grav*864.
        do k=limcnv,plev
          rh = shb(i,k)/shbs(i,k)
          write(6,8020) shbh(i,k),sbh(i,k),hbh(i,k),fac*cmfmc(i,k),
     $         cmfsl(i,k), cmflq(i,k)
          write(6,8040) tb(i,k),shb(i,k),rh,sb(i,k),hb(i,k),hbs(i,k),
     $         tdt*cmfdt(i,k),tdt*cmfdq(i,k),tdt*cmfdqr(i,k)
        end do
        write(6, 8000) prec(i)
      end if
#endif
C#                                                                     #
C#                                                                     #
C#######################################################################
C
C Begin moist convective mass flux adjustment procedure.
C Formalism ensures that negative cloud liquid water can never occur
C
      do 70 k=plev-1,limcnv+1,-1
        do 10 i=1,plon
          etagdt(i) = 0.0
          eta   (i) = 0.0
          beta  (i) = 0.0
          ds1   (i) = 0.0
          ds2   (i) = 0.0
          ds3   (i) = 0.0
          dq1   (i) = 0.0
          dq2   (i) = 0.0
          dq3   (i) = 0.0
C
C Specification of "cloud base" conditions
C
          qprime    = 0.0
          tprime    = 0.0
C
C Assign tprime within the PBL to be proportional to the quantity
C tpert (which will be bounded by tpmax), passed to this routine by 
C the PBL routine.  Don't allow perturbation to produce a dry 
C adiabatically unstable parcel.  Assign qprime within the PBL to be 
C an appropriately modified value of the quantity qpert (which will be 
C bounded by shpmax) passed to this routine by the PBL routine.  The 
C quantity qprime should be less than the local saturation value 
C (qsattp=qsat[t+tprime,p]).  In both cases, tpert and qpert are
C linearly reduced toward zero as the PBL top is approached.
C
          pblhgt = max(pblht(i),1.0)
          if( (zm(i,k+1) .le. pblhgt) .and.
     $                            dzcld(i).eq.0.0 ) then
            fac1   = max(0.0,1.0-zm(i,k+1)/pblhgt)
            tprime = min(tpert(i),tpmax)*fac1
            qsattp = shbs(i,k+1) + cp*rhlat*gam(i,k+1)*tprime
            shprme = min(min(qpert(i,1),shpmax)*fac1,
     $                     max(qsattp-shb(i,k+1),0.0))
            qprime = max(qprime,shprme)
          else
            tprime = 0.0
            qprime = 0.0
          end if
C
C Specify "updraft" (in-cloud) thermodynamic properties
C
          sc (i)    = sb (i,k+1) + cp*tprime
          shc(i)    = shb(i,k+1) + qprime
          hc (i)    = sc (i    ) + hlat*shc(i)
          vtemp4(i) = hc(i) - hbs(i,k)
          dz        = pdel(i,k)*rgas*tb(i,k)*rgrav/pmid(i,k)
          if (vtemp4(i).gt.0.0) then
            dzcld(i) = dzcld(i) + dz
          else
            dzcld(i) = 0.0
          end if
   10   continue
#if ( defined DIAGNS )
C#######################################################################
C#                                                                     #
C#    output thermodynamic perturbation information                    #
C#                                                                     #
        if (lat.eq.jloc .and. nstep.ge.nsloc) then
          write (6,8090) k+1,sc(iloc),shc(iloc),hc(iloc)
        end if
C#                                                                     #
C#######################################################################
#endif
C
C Check on moist convective instability
C Build index vector of points where instability exists
C
        call whenfgt(plon,vtemp4,1,0.0,indx1,len1)
        if (len1.le.0) go to 70
C
C Current level just below top level => no overshoot
C
        if (k.le.limcnv+1) then
          do ii=1,len1
            i = indx1(ii)
            temp1     = vtemp4(i)/(1.0 + gam(i,k))
            cldwtr(i) = max(0.0,(sb(i,k) - sc(i) + temp1))
            beta(i)   = 0.0
            vtemp3(i) = (1.0 + gam(i,k))*(sc(i) - sbh(i,k))
          end do
        else
C
C First guess at overshoot parameter using crude buoyancy closure
C 10% overshoot assumed as a minimum and 1-c0*dz maximum to start
C If pre-existing supersaturation in detrainment layer, beta=0
C cldwtr is temporarily equal to hlat*l (l=> liquid water)
C
CDIR$ IVDEP
          do ii=1,len1
            i = indx1(ii)
            temp1     = vtemp4(i)/(1.0 + gam(i,k))
            cldwtr(i) = max(0.0,(sb(i,k)-sc(i)+temp1))
            betamx(i) = 1.0 - c0*max(0.0,(dzcld(i)-dzmin))
            b1        = (hc(i) - hbs(i,k-1))*pdel(i,k-1)
            b2        = (hc(i) - hbs(i,k  ))*pdel(i,k  )
            beta(i)   = max(betamn,min(betamx(i), 1.0 + b1/b2))
            if (hbs(i,k-1).le.hb(i,k-1)) beta(i) = 0.0
C
C Bound maximum beta to ensure physically realistic solutions
C
C First check constrains beta so that eta remains positive
C (assuming that eta is already positive for beta equal zero)
C
            vtemp1(i) = -(hbh(i,k+1) - hc(i))*pdel(i,k)*rpdel(i,k+1)+
     $                 (1.0 + gam(i,k))*(sc(i) - sbh(i,k+1) + cldwtr(i))
            vtemp2(i) = (1.0 + gam(i,k))*(sc(i) - sbh(i,k))
            vtemp3(i) = vtemp2(i)
            if ((beta(i)*vtemp2(i) - vtemp1(i)).gt.0.) then
              betamx(i) = 0.99*(vtemp1(i)/vtemp2(i))
              beta(i)   = max(0.0,min(betamx(i),beta(i)))
            end if
          end do
C
C Second check involves supersaturation of "detrainment layer"
C small amount of supersaturation acceptable (by ssfac factor)
C
CDIR$ IVDEP
          do ii=1,len1
            i = indx1(ii)
            if (hb(i,k-1).lt.hbs(i,k-1)) then
              vtemp1(i) = vtemp1(i)*rpdel(i,k)
              temp2 = gam(i,k-1)*(sbh(i,k) - sc(i) + cldwtr(i)) -
     $                   hbh(i,k) + hc(i) - sc(i) + sbh(i,k)
              temp3 = vtemp3(i)*rpdel(i,k)
              vtemp2(i) = (tdt/cats)*(hc(i) - hbs(i,k))*temp2/
     $                   (pdel(i,k-1)*(hbs(i,k-1) - hb(i,k-1))) + temp3
              if ((beta(i)*vtemp2(i) - vtemp1(i)).gt.0.) then
                betamx(i) = ssfac*(vtemp1(i)/vtemp2(i))
                beta(i)   = max(0.0,min(betamx(i),beta(i)))
              end if
            else 
              beta(i) = 0.0
            end if
          end do
C
C Third check to avoid introducing 2 delta x thermodynamic
C noise in the vertical ... constrain adjusted h (or theta e)
C so that the adjustment doesn't contribute to "kinks" in h
C
CDIR$ IVDEP
          do ii=1,len1
            i = indx1(ii)
            g = min(0.0,hb(i,k) - hb(i,k-1))
            temp1 = (hb(i,k) - hb(i,k-1) - g)*(cats/tdt)/
     $                 (hc(i) - hbs(i,k))
            vtemp1(i) = temp1*vtemp1(i) + (hc(i) - hbh(i,k+1))*
     $                     rpdel(i,k)
            vtemp2(i) = temp1*vtemp3(i)*rpdel(i,k) +
     $                     (hc(i) - hbh(i,k) - cldwtr(i))*
     $                     (rpdel(i,k) + rpdel(i,k+1))
            if ((beta(i)*vtemp2(i) - vtemp1(i)).gt.0.) then
              if (vtemp2(i).ne.0.0) then
                betamx(i) = vtemp1(i)/vtemp2(i)
              else
                betamx(i) = 0.0
              end if
              beta(i) = max(0.0,min(betamx(i),beta(i)))
            end if
          end do
        end if
C
C Calculate mass flux required for stabilization.
C
C Ensure that the convective mass flux, eta, is positive by
C setting negative values of eta to zero..
C Ensure that estimated mass flux cannot move more than the
C minimum of total mass contained in either layer k or layer k+1.
C Also test for other pathological cases that result in non-
C physical states and adjust eta accordingly.
C
CDIR$ IVDEP
        do ii=1,len1
          i = indx1(ii)
          beta(i) = max(0.0,beta(i))
          temp1 = hc(i) - hbs(i,k)
          temp2 = ((1.0 + gam(i,k))*(sc(i) - sbh(i,k+1) + cldwtr(i)) -
     $               beta(i)*vtemp3(i))*rpdel(i,k) -
     $              (hbh(i,k+1) - hc(i))*rpdel(i,k+1)
          eta(i) = temp1/(temp2*grav*cats)
          tmass = min(pdel(i,k),pdel(i,k+1))*rgrav
          if (eta(i).gt.tmass*rtdt .or. eta(i).le.0.0) eta(i) = 0.0
C
C Check on negative q in top layer (bound beta)
C
          if (shc(i)-shbh(i,k).lt.0.0 .and. beta(i)*eta(i).ne.0.0) then
            denom = eta(i)*grav*tdt*(shc(i) - shbh(i,k))*rpdel(i,k-1)
            beta(i) = max(0.0,min(-0.999*shb(i,k-1)/denom,
     $                                   beta(i)))
          end if
C
C Check on negative q in middle layer (zero eta)
C
          qtest1 = shb(i,k) + eta(i)*grav*tdt*((shc(i) - shbh(i,k+1)) -
     $               (1.0 - beta(i))*cldwtr(i)*rhlat -
     $               beta(i)*(shc(i) - shbh(i,k)))*rpdel(i,k)
          if (qtest1.le.0.0) eta(i) = 0.0
C
C Check on negative q in lower layer (bound eta)
C
          fac1 = -(shbh(i,k+1) - shc(i))*rpdel(i,k+1)
          qtest2 = shb(i,k+1) - eta(i)*grav*tdt*fac1
          if (qtest2 .lt. 0.0) then
            eta(i) = 0.99*shb(i,k+1)/(grav*tdt*fac1)
          end if
          etagdt(i) = eta(i)*grav*tdt
        end do
C
#if ( defined DIAGNS )
C#######################################################################
C#                                                                     #
        if (lat.eq.jloc .and. nstep.ge.nsloc) then
          write(6,8080) beta(iloc), eta(iloc)
        end if
C#                                                                     #
C#######################################################################
#endif
C
C Calculate cloud water, rain water, and thermodynamic changes
C
CDIR$ IVDEP
        do 30 ii=1,len1
          i = indx1(ii)
          cldwtr(i) = etagdt(i)*cldwtr(i)*rhlat*rgrav
          rnwtr(i) = (1.0 - beta(i))*cldwtr(i)
          ds1(i) = etagdt(i)*(sbh(i,k+1) - sc(i))*rpdel(i,k+1)
          dq1(i) = etagdt(i)*(shbh(i,k+1) - shc(i))*rpdel(i,k+1)
          ds2(i) = (etagdt(i)*(sc(i) - sbh(i,k+1)) +
     $                hlat*grav*cldwtr(i) - beta(i)*etagdt(i)*
     $                (sc(i) - sbh(i,k)))*rpdel(i,k)
          dq2(i) = (etagdt(i)*(shc(i) - shbh(i,k+1)) -
     $                grav*rnwtr(i) - beta(i)*etagdt(i)*
     $                (shc(i) - shbh(i,k)))*rpdel(i,k)
          ds3(i) = beta(i)*(etagdt(i)*(sc(i) - sbh(i,k)) -
     $               hlat*grav*cldwtr(i))*rpdel(i,k-1)
          dq3(i) = beta(i)*etagdt(i)*(shc(i) - shbh(i,k))*rpdel(i,k-1)
C
C Isolate convective fluxes for later diagnostics
C
          fslkp = eta(i)*(sc(i) - sbh(i,k+1))
          fslkm = beta(i)*(eta(i)*(sc(i) - sbh(i,k)) -
     $                       hlat*cldwtr(i)*rtdt)
          fqlkp = eta(i)*(shc(i) - shbh(i,k+1))
          fqlkm = beta(i)*eta(i)*(shc(i) - shbh(i,k))
C
C Update thermodynamic profile (update sb, hb, & hbs later)
C
          tb (i,k+1) = tb(i,k+1)  + ds1(i)*rcp
          tb (i,k  ) = tb(i,k  )  + ds2(i)*rcp
          tb (i,k-1) = tb(i,k-1)  + ds3(i)*rcp
          shb(i,k+1) = shb(i,k+1) + dq1(i)
          shb(i,k  ) = shb(i,k  ) + dq2(i)
          shb(i,k-1) = shb(i,k-1) + dq3(i)
C
C ** Update diagnostic information for final budget **
C Tracking precipitation, temperature & specific humidity tendencies,
C rainout term, convective mass flux, convective liquid
C water static energy flux, and convective total water flux
C The variable afac makes the necessary adjustment to the
C diagnostic fluxes to account for adjustment time scale based on
C how relaxation time scale is to be applied (column vs. triplet)
C
          prec(i)    = prec(i) + (rnwtr(i)/rhoh2o)*adjfac
C
C The following variables have units of "units"/second
C
          cmfdt (i,k+1) = cmfdt (i,k+1) + ds1(i)*rcp*rtdt*adjfac
          cmfdt (i,k  ) = cmfdt (i,k  ) + ds2(i)*rcp*rtdt*adjfac
          cmfdt (i,k-1) = cmfdt (i,k-1) + ds3(i)*rcp*rtdt*adjfac
          cmfdq (i,k+1) = cmfdq (i,k+1) + dq1(i)*rtdt*adjfac
          cmfdq (i,k  ) = cmfdq (i,k  ) + dq2(i)*rtdt*adjfac
          cmfdq (i,k-1) = cmfdq (i,k-1) + dq3(i)*rtdt*adjfac
          qc    (i,k  ) = (grav*rnwtr(i)*rpdel(i,k))*rtdt*adjfac
          cmfdqr(i,k  ) = cmfdqr(i,k  ) + qc(i,k)
          cmfmc (i,k+1) = cmfmc (i,k+1) + eta(i)*adjfac
          cmfmc (i,k  ) = cmfmc (i,k  ) + beta(i)*eta(i)*adjfac
C
C The following variables have units of w/m**2
C
          cmfsl (i,k+1) = cmfsl (i,k+1) + fslkp*adjfac
          cmfsl (i,k  ) = cmfsl (i,k  ) + fslkm*adjfac
          cmflq (i,k+1) = cmflq (i,k+1) + hlat*fqlkp*adjfac
          cmflq (i,k  ) = cmflq (i,k  ) + hlat*fqlkm*adjfac
   30   continue
C
C Next, convectively modify passive constituents
C For now, when applying relaxation time scale to thermal fields after 
C entire column has undergone convective overturning, constituents will 
C be mixed using a "relaxed" value of the mass flux determined above
C Although this will be inconsistant with the treatment of the thermal
C fields, it's computationally much cheaper, no more-or-less justifiable,
C and consistent with how the history tape mass fluxes would be used in
C an off-line mode (i.e., using an off-line transport model)
C
        do 50 m=2,pcnst         ! note: indexing assumes water is first field
CDIR$ IVDEP
          do 40 ii=1,len1
            i = indx1(ii)
C
C If any of the reported values of the constituent is negative in
C the three adjacent levels, nothing will be done to the profile
C
            if ((q(i,k+1,m).lt.0.0) .or. (q(i,k,m).lt.0.0) .or.
     $           (q(i,k-1,m).lt.0.0)) go to 40
C
C Specify constituent interface values (linear interpolation)
C
            cmrh(i,k  ) = 0.5*(q(i,k-1,m) + q(i,k  ,m))
            cmrh(i,k+1) = 0.5*(q(i,k  ,m) + q(i,k+1,m))
C
C Specify perturbation properties of constituents in PBL
C
            pblhgt = max(pblht(i),1.0)
            if( (zm(i,k+1) .le. pblhgt)
     $                                     .and.
     $                                dzcld(i).eq.0.0 ) then
              fac1 = max(0.0,1.0-zm(i,k+1)/pblhgt)
              cmrc(i) = q(i,k+1,m) + qpert(i,m)*fac1
            else
              cmrc(i) = q(i,k+1,m)
            end if
C
C Determine fluxes, flux divergence => changes due to convection
C Logic must be included to avoid producing negative values. A bit
C messy since there are no a priori assumptions about profiles.
C Tendency is modified (reduced) when pending disaster detected.
C
            botflx   = etagdt(i)*(cmrc(i) - cmrh(i,k+1))*adjfac
            topflx   = beta(i)*etagdt(i)*(cmrc(i)-cmrh(i,k))*adjfac
            dcmr1(i) = -botflx*rpdel(i,k+1)
            efac1    = 1.0
            efac2    = 1.0
            efac3    = 1.0
C
            if (q(i,k+1,m)+dcmr1(i) .lt. 0.0) then
              efac1 = max(tiny,abs(q(i,k+1,m)/dcmr1(i)) - eps)
            end if
C
            if (efac1.eq.tiny .or. efac1.gt.1.0) efac1 = 0.0
            dcmr1(i) = -efac1*botflx*rpdel(i,k+1)
            dcmr2(i) = (efac1*botflx - topflx)*rpdel(i,k)
C  
            if (q(i,k,m)+dcmr2(i) .lt. 0.0) then
              efac2 = max(tiny,abs(q(i,k  ,m)/dcmr2(i)) - eps)
            end if
C
            if (efac2.eq.tiny .or. efac2.gt.1.0) efac2 = 0.0
            dcmr2(i) = (efac1*botflx - efac2*topflx)*rpdel(i,k)
            dcmr3(i) = efac2*topflx*rpdel(i,k-1)
C
            if (q(i,k-1,m)+dcmr3(i) .lt. 0.0) then
              efac3 = max(tiny,abs(q(i,k-1,m)/dcmr3(i)) - eps)
            end if
C
            if (efac3.eq.tiny .or. efac3.gt.1.0) efac3 = 0.0
            efac3    = min(efac2,efac3)
            dcmr2(i) = (efac1*botflx - efac3*topflx)*rpdel(i,k)
            dcmr3(i) = efac3*topflx*rpdel(i,k-1)
C
            q(i,k+1,m) = q(i,k+1,m) + dcmr1(i)
            q(i,k  ,m) = q(i,k  ,m) + dcmr2(i)
            q(i,k-1,m) = q(i,k-1,m) + dcmr3(i)
   40     continue
   50   continue                ! end of m=2,pcnst loop
C
C Constituent modifications complete
C
        if (k.eq.limcnv+1) go to 60
C
C Complete update of thermodynamic structure at integer levels
C gather/scatter points that need new values of shbs and gamma
C
        do ii=1,len1
          i = indx1(ii)
          vtemp1(ii     ) = tb(i,k)
          vtemp1(ii+len1) = tb(i,k-1)
          vtemp2(ii     ) = pmid(i,k)
          vtemp2(ii+len1) = pmid(i,k-1)
        end do
        call vqsatd (vtemp1  ,vtemp2  ,estemp  ,vtemp3  , vtemp4  ,
     $               2*len1   )    ! using estemp as extra long vector
CDIR$ IVDEP
        do ii=1,len1
          i = indx1(ii)
          shbs(i,k  ) = vtemp3(ii     )
          shbs(i,k-1) = vtemp3(ii+len1)
          gam(i,k  ) = vtemp4(ii     )
          gam(i,k-1) = vtemp4(ii+len1)
          sb (i,k  ) = sb(i,k  ) + ds2(i)
          sb (i,k-1) = sb(i,k-1) + ds3(i)
          hb (i,k  ) = sb(i,k  ) + hlat*shb(i,k  )
          hb (i,k-1) = sb(i,k-1) + hlat*shb(i,k-1)
          hbs(i,k  ) = sb(i,k  ) + hlat*shbs(i,k  )
          hbs(i,k-1) = sb(i,k-1) + hlat*shbs(i,k-1)
        end do
C
C Update thermodynamic information at half (i.e., interface) levels
C
CDIR$ IVDEP
        do ii=1,len1
          i = indx1(ii)
          sbh (i,k) = 0.5*(sb(i,k) + sb(i,k-1))
          shbh(i,k) = qhalf(shb(i,k-1),shb(i,k),shbs(i,k-1),shbs(i,k))
          hbh (i,k) = sbh(i,k) + hlat*shbh(i,k)
          sbh (i,k-1) = 0.5*(sb(i,k-1) + sb(i,k-2))
          shbh(i,k-1) = qhalf(shb(i,k-2),shb(i,k-1),
     $                        shbs(i,k-2),shbs(i,k-1))
          hbh (i,k-1) = sbh(i,k-1) + hlat*shbh(i,k-1)
        end do
C
#if ( defined DIAGNS )
C#######################################################################
C#                                                                     #
C#    this update necessary, only if debugging diagnostics requested   #
C#                                                                     #
        if (lat.eq.jloc .and. nstep.ge.nsloc) then
          do i=1,plon
            call vqsatd(tb(i,k+1),pmid(i,k+1),es,shbs(i,k+1),gam(i,k+1),
     $                                                               1)
            sb (i,k+1) = sb(i,k+1) + ds1(i)
            hb (i,k+1) = sb(i,k+1) + hlat*shb(i,k+1)
            hbs(i,k+1) = sb(i,k+1) + hlat*shbs(i,k+1)
            kpp = k + 2
            if(k+1.eq.plev) kpp = k + 1
            do kp=k+1,kpp
              kpm1 = kp-1
              sbh(i,kp)  = 0.5*(sb(i,kpm1) + sb(i,kp))
              shbh(i,kp) = qhalf(shb(i,kpm1),shb(i,kp),shbs(i,kpm1),
     $                        shbs(i,kp))
              hbh(i,kp)  = sbh(i,kp) + hlat*shbh(i,kp)
            end do
          end do
C#                                                                     #
C#          diagnostic output                                          #
C#                                                                     #
          i = iloc
          write(6, 8060) k
          fac = grav*864.
          do n=limcnv,plev
            rh  = shb(i,n)/shbs(i,n)
            write(6,8020)shbh(i,n),sbh(i,n),hbh(i,n),fac*cmfmc(i,n),
     $              cmfsl(i,n), cmflq(i,n)
C--------------write(6, 8050)
C--------------write(6, 8030) fac*cmfmc(i,n),cmfsl(i,n), cmflq(i,n)
            write(6, 8040) tb(i,n),shb(i,n),rh,sb(i,n),hb(i,n),
     $              hbs(i,n), tdt*cmfdt(i,n),tdt*cmfdq(i,n),tdt*cmfdqr(i,n)
          end do
          write(6, 8000) prec(i)
        end if
C#                                                                     #
C#                                                                     #
C#######################################################################
#endif
C
C Ensure that dzcld is reset if convective mass flux zero
C specify the current vertical extent of the convective activity
C top of convective layer determined by size of overshoot param.
C
   60   do i=1,plon
          etagt0 = eta(i).gt.0.0
          if (.not.etagt0) dzcld(i) = 0.0
          if (etagt0 .and. beta(i).gt.betamn) then
            ktp = k-1
          else
            ktp = k
          end if
          if (etagt0) then
            cnt(i) = min(cnt(i),real(ktp))
            cnb(i) = max(cnb(i),real(k))
          end if
        end do
   70 continue                  ! end of k loop
C
C ** apply final thermodynamic tendencies **
C
      do k=limcnv,plev
        do i=1,plon
          t (i,k) = t (i,k) + cmfdt(i,k)*tdt
          q(i,k,1) = q(i,k,1) + cmfdq(i,k)*tdt
        end do
      end do
C
C Kludge to prevent cnb-cnt from being zero (in the event
C someone decides that they want to divide by this quantity)
C
      do i=1,plon
        if (cnb(i).ne.0.0 .and. cnb(i).eq.cnt(i)) then
          cnt(i) = cnt(i) - 1.0
        end if
      end do
C
      do i=1,plon
        precc(i) = prec(i)*rtdt
      end do
C
#if ( defined DIAGNS )
C#######################################################################
C#                                                                     #
C#    we're done ... show final result if debug diagnostics requested  #
C#                                                                     #
      if (lat.eq.jloc .and. nstep.ge.nsloc) then
        i=iloc
        fac = grav*864.
        write(6, 8010)
        do k=limcnv,plev
          rh = shb(i,k)/shbs(i,k)
          write(6, 8020) shbh(i,k),sbh(i,k),hbh(i,k),fac*cmfmc(i,k),
     $           cmfsl(i,k), cmflq(i,k)
          write(6, 8040) tb(i,k),shb(i,k),rh   ,sb(i,k),hb(i,k),
     $           hbs(i,k), tdt*cmfdt(i,k),tdt*cmfdq(i,k),tdt*cmfdqr(i,k)
        end do
        write(6, 8000) prec(i)
C#                                                                     #
C#       approximate vertical integral of moist static energy and      #
C#       total preciptable water after adjustment and output changes   #
C#                                                                     #
        hsum2 = 0.0
        qsum2 = 0.0
        do k=limcnv,plev
          hsum2 = hsum2 + pdel(i,k)*rgrav*hb(i,k)
          qsum2 = qsum2 + pdel(i,k)*rgrav*shb(i,k)
        end do
C#                                                                     #
        write (6,8070) hsum1, hsum2, abs(hsum2-hsum1)/hsum2,
     $        qsum1, qsum2, abs(qsum2-qsum1)/qsum2
      end if
C#                                                                     #
C#                                                                     #
C#######################################################################
#endif
      return                 ! we're all done ... return to calling procedure
#if ( defined DIAGNS )
C
C Formats
C
 8000 format(///,10x,'PREC = ',3pf12.6,/)
 8010 format('1**        TB      SHB      RH       SB',
     $     '       HB      HBS      CAH      CAM       PRECC ',
     $     '     ETA      FSL       FLQ     **', /)
 8020 format(' ----- ',     9x,3p,f7.3,2x,2p,     9x,-3p,f7.3,2x,
     $     f7.3, 37x, 0p,2x,f8.2,  0p,2x,f8.2,2x,f8.2, ' ----- ')
 8030 format(' ----- ',  0p,82x,f8.2,  0p,2x,f8.2,2x,f8.2,
     $       ' ----- ')
 8040 format(' - - - ',f7.3,2x,3p,f7.3,2x,2p,f7.3,2x,-3p,f7.3,2x,
     $     f7.3, 2x,f8.3,2x,0p,f7.3,3p,2x,f7.3,2x,f7.3,30x,
     $      ' - - - ')
 8050 format(' ----- ',110x,' ----- ')
 8060 format('1 K =>',  i4,/,
     $        '           TB      SHB      RH       SB',
     $        '       HB      HBS      CAH      CAM       PREC ',
     $        '     ETA      FSL       FLQ', /)
 8070 format(' VERTICALLY INTEGRATED MOIST STATIC ENERGY BEFORE, AFTER',
     $     ' AND PERCENTAGE DIFFERENCE => ',1p,2e15.7,2x,2p,f7.3,/,
     $     ' VERTICALLY INTEGRATED MOISTURE            BEFORE, AFTER',
     $     ' AND PERCENTAGE DIFFERENCE => ',1p,2e15.7,2x,2p,f7.3,/)
 8080       format(' BETA, ETA => ', 1p,2e12.3)
 8090 format (' k+1, sc, shc, hc => ', 1x, i2, 1p, 3e12.4)
#endif
C
      end
 
