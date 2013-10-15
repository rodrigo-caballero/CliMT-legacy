  subroutine betts_miller (  &
       ! Input
       im,jm,km, &           
       hlv, Cp_air, Grav, rdgas, rvgas, &
       Cpcond, kappa, es0, T00, &
       tau_bm, rhbm,  &
       do_shallower, do_changeqref, &
       dt, tin, qin, ps, pfull, phalf, t_ref, &
       ! In/Out
       q_ref, &
       ! Output
       rain, tdel, qdel, bmflag, &
       klzbs, cape, cin, invtau_bm_t, invtau_bm_q, &
       capeflag)

!-----------------------------------------------------------------------
!
!                     Betts-Miller Convection Scheme
!
!-----------------------------------------------------------------------
! INPUT
!
!  hlv      =  latent heat of vaporization for condensible
!  Cp_air    =  specific heat of air
!  Grav      =  surface gravity
!  rdgas     =  dry gas constant
!  rvgas     =  
!  Cpcond    =  specific heat of vapor condensibled
!  kappa     =  ratio R/Cp
!  es0       =  reference sat. vapor pressure
!  tau_bm    =  betts-miller relaxation timescale (seconds)
!  rhbm      = relative humidity that you're relaxing towards
!
!  do_shallower = do the shallow convection scheme where it chooses a smaller
!                 depth such that precipitation is zero MAKE THIS DEFAULT!!!
! 
!  do_changeqref = do the shallow convection scheme where if changes the 
!                  profile of both q and T in order make precip zero
!
!           dt       time step in seconds
!           tin      temperature at full model levels
!           qin      specific humidity of water vapor at full
!                      model levels
!           ps       surface pressure
!           pfull    pressure at full model levels
!           phalf    pressure at half (interface) model levels
!
! OUTPUT
!
!           rain     liquid precipitation (kg/m2)
!           tdel     temperature increment at full model levels
!           qdel     specific humidity increment (of condensible vapor) at
!                      full model levels
!           bmflag   flag for which routines you're calling
!           klzbs    stored klzb values
!           cape     convectively available potential energy 
!           cin      convective inhibition (this and the above are before the 
!                    adjustment)
!           invtau_bm_t inverse temperature relaxation timescale
!           invtau_bm_q inverse humidity relaxation timescale
!           capeflag    a flag that says why cape=0
!
!-----------------------------------------------------------------------
!--------------------- interface arguments -----------------------------

   real   , intent(in) , dimension(im,jm,km) :: tin, qin, pfull, phalf
   real   , intent(in)                    :: dt, ps, es0, T00, kappa
   real   , intent(out), dimension(im,jm)   :: rain, bmflag, klzbs, cape, &
       cin, invtau_bm_t, invtau_bm_q, capeflag
   real   , intent(out), dimension(im,jm,km) :: tdel, qdel, q_ref, t_ref
!-----------------------------------------------------------------------
!---------------------- local data -------------------------------------

logical,dimension(size(tin,1),size(tin,2),size(tin,3)) :: do_adjust
logical :: avgbl
   real,dimension(size(tin,1),size(tin,2),size(tin,3)) ::  &
             rin, esat, qsat, desat, dqsat, pmes, pmass
   real,dimension(size(tin,1),size(tin,2))             ::  &
                     hlcp, precip, precip_t
   real,dimension(size(tin,3))                         :: eref, rpc, tpc, &
                                                          tpc1, rpc1

   real                                                ::  & 
       cape1, cin1, tot, deltak, deltaq, qrefint, deltaqfrac, deltaqfrac2, &
       ptopfrac, es, capeflag1, plzb, plcl, cape2, small
integer  i, j, k, ix, jx, kx, klzb, ktop, klzb2
real    :: tau_bm   ! =7200.
real    :: rhbm     ! = .8
integer :: do_shallower, do_changeqref

!-----------------------------------------------------------------------
!     computation of precipitation by betts-miller scheme
!-----------------------------------------------------------------------
      ix=im
      jx=jm
      kx=km
!      ix=size(tin,1)
!      jx=size(tin,2)
!      kx=size(tin,3)
      avgbl = .false.
      small = 1.e-10

! calculate r
       rin = qin/(1.0 - qin)
       do i=1,ix
          do j=1,jx
             cape1 = 0.
             cin1 = 0.
             tot = 0.
             klzb=0
! the bmflag is written out to show what aspects of the bm scheme is called
! bmflag = 0 is no cape, no convection
! bmflag = 1 is shallow conv, the predicted precip is less than zero
! bmflag = 2 is deep convection
             bmflag(i,j) = 0.
             tpc = tin(i,j,:)
             rpc = rin(i,j,:)
! calculate cape, cin, level of zero buoyancy, and parcel properties 
! new code (second order in delta ln p and exact LCL calculation)
             call capecalcnew( kx,  pfull(i,j,:),  phalf(i,j,:),&
                            cp_air, rdgas, rvgas, ps, es0, T00, &
                            hlv, kappa, tin(i,j,:), &
                            rin(i,j,:), avgbl, cape1, cin1, tpc, &
                            rpc, klzb)

! set values for storage
             capeflag(i,j) = capeflag1
             cape(i,j) = cape1
             cin(i,j) = cin1
             klzbs(i,j) = klzb
             if(cape1.gt.0.) then 
!             if((tot.gt.0.).and.(cape1.gt.0.)) then 
                bmflag(i,j) = 1.
! reference temperature is just that of the parcel all the way up
                t_ref(i,j,:) = tpc
                do k=klzb,kx
                   rpc(k) = rhbm*rpc(k)
!                      eref(k) = rhbm*pfull(i,j,k)*rpc(k)/(d622 + d378*rpc(k))
!                      rpc(k) = d622*eref(k)/(pfull(i,j,k) - d378*eref(k))
                   q_ref(i,j,k) = rpc(k)/(1 + rpc(k))
                end do
! set the tendencies to zero where you don't adjust
! set the reference profiles to be the original profiles (for diagnostic 
! purposes only --  you can think of this as what you're relaxing to in
! areas above the actual convection
                do k=1,max(klzb-1,1)
                   qdel(i,j,k) = 0.0
                   tdel(i,j,k) = 0.0
                   q_ref(i,j,k) = qin(i,j,k)
                   t_ref(i,j,k) = tin(i,j,k)
                end do
! initialize p to zero for the loop
                precip(i,j) = 0.
                precip_t(i,j) = 0.
                do k=klzb, kx
! relax to reference profiles
                   tdel(i,j,k) = - (tin(i,j,k) - t_ref(i,j,k))/tau_bm*dt
                   qdel(i,j,k) = - (qin(i,j,k) - q_ref(i,j,k))/tau_bm*dt
! Precipitation can be calculated already, based on the change in q on the 
! way up (this isn't altered in the energy conservation scheme).  
                   precip(i,j) = precip(i,j) - qdel(i,j,k)*(phalf(i,j,k+1)- &
                                 phalf(i,j,k))/grav
                   precip_t(i,j)= precip_t(i,j) + cp_air/(hlv+small)*tdel(i,j,k)* &
                                 (phalf(i,j,k+1)-phalf(i,j,k))/grav
                end do
                if ((precip(i,j).gt.0.).and.(precip_t(i,j).gt.0.)) then 
! If precip > 0, then correct energy. 
                   bmflag(i,j) = 2.
                   if(precip(i,j).gt.precip_t(i,j)) then
! if the q precip is greater, then lengthen the relaxation timescale on q to
! conserve energy.  qdel is therefore changed.
                      invtau_bm_q(i,j) = precip_t(i,j)/precip(i,j)/tau_bm
                      qdel(i,j,klzb:kx) = tau_bm*invtau_bm_q(i,j)* &
                         qdel(i,j,klzb:kx)
                      precip(i,j) = precip_t(i,j)
                      invtau_bm_t(i,j) = 1./tau_bm
                   else
! not simple scheme: shift the reference profile of temperature
! deltak is the energy correction that you make to the temperature reference
! profile
                      deltak = 0.
                      do k=klzb, kx
! Calculate the integrated difference in energy change within each level.
                         deltak = deltak - (tdel(i,j,k) + hlv/cp_air*&
                                     qdel(i,j,k))* &
                                     (phalf(i,j,k+1) - phalf(i,j,k))
                      end do
! Divide by total pressure.
                      deltak = deltak/(phalf(i,j,kx+1) - phalf(i,j,klzb))
! Adjust the reference profile (uniformly with height), and correspondingly 
! the temperature change.
                      t_ref(i,j,klzb:kx) = t_ref(i,j,klzb:kx)+ &
                              deltak*tau_bm/dt
                      tdel(i,j,klzb:kx) = tdel(i,j,klzb:kx) + deltak
                   endif
                else if(precip_t(i,j).gt.0.) then
! If precip < 0, then do the shallow conv routine.
! First option: do_shallower = true
! This chooses the depth of convection based on choosing the height that 
! it can make precip zero, i.e., subtract off heights until that precip 
! becomes positive.  
                   if (do_shallower.eq.1) then
! ktop is the new top of convection.  set this initially to klzb.
                      ktop = klzb
! Work your way down until precip is positive again.
                      do while ((precip(i,j).lt.0.).and.(ktop.le.kx))
                         precip(i,j) = precip(i,j) - qdel(i,j,ktop)* &
                                  (phalf(i,j,ktop) - phalf(i,j,ktop+1))/grav
                         ktop = ktop + 1
                      end do
! since there will be an overshoot (precip is going to be greater than zero 
! once we finish this), the actual new top of convection is somewhere between
! the current ktop, and one level above this.  set ktop to the level above.
                      ktop = ktop - 1
! Adjust the tendencies in the places above back to zero, and the reference 
! profiles back to the original t,q.
                      if (ktop.gt.klzb) then
                         qdel(i,j,klzb:ktop-1) = 0.
                         q_ref(i,j,klzb:ktop-1) = qin(i,j,klzb:ktop-1)
                         tdel(i,j,klzb:ktop-1) = 0.
                         t_ref(i,j,klzb:ktop-1) = tin(i,j,klzb:ktop-1)
                      end if
! Then make the change only a fraction of the new top layer so the precip is 
! identically zero.
! Calculate the fractional penetration of convection through that top layer.  
! This is the amount necessary to make precip identically zero.  
                      if (precip(i,j).gt.0.) then 
                         ptopfrac = precip(i,j)/(qdel(i,j,ktop)* &
                            (phalf(i,j,ktop+1) - phalf(i,j,ktop)))*grav
! Reduce qdel in the top layer by this fraction. 
                         qdel(i,j,ktop) = ptopfrac*qdel(i,j,ktop)
! Set precip to zero
                         precip(i,j) = 0.
! Now change the reference temperature in such a way to make the net 
! heating zero.
!! Reduce tdel in the top layer
                         tdel(i,j,ktop) = ptopfrac*tdel(i,j,ktop)
                         deltak = 0.
                         if (ktop.lt.kx) then
! Integrate temperature tendency up to 1 level below top.
                            do k=ktop,kx
                               deltak = deltak + tdel(i,j,k)* &
                                   (phalf(i,j,k) - phalf(i,j,k+1))
                            end do
! Normalize by the pressure difference.
                            deltak = deltak/(phalf(i,j,kx+1) - phalf(i,j,ktop))
! Subtract this value uniformly from tdel, and make the according change to 
! t_ref.
                            do k=ktop,kx
                               tdel(i,j,k) = tdel(i,j,k) + deltak
                               t_ref(i,j,k) = t_ref(i,j,k) + deltak*tau_bm/dt
                            end do
                         end if
                      else
                         precip(i,j) = 0.
                         qdel(i,j,kx) = 0.
                         q_ref(i,j,kx) = qin(i,j,kx)
                         tdel(i,j,kx) = 0.
                         t_ref(i,j,kx) = tin(i,j,kx)
                         invtau_bm_t(i,j) = 0.
                         invtau_bm_q(i,j) = 0.
                      end if
                   else if(do_changeqref.eq.1) then
! Change the reference profile of q by a certain fraction so that precip is 
! zero.  This involves calculating the total integrated q_ref dp (this is the
! quantity intqref), as well as the necessary change in q_ref (this is the 
! quantity deltaq).  Then the fractional change in q_ref at each level (the 
! quantity deltaqfrac) is 1-deltaq/intqref.  (have to multiply q_ref by 
! 1-deltaq/intqref at every level)  Then the change in qdel is 
! -deltaq/intqref*q_ref*dt/tau_bm.
! Change the reference profile of T by a uniform amount so that precip is zero.
                      deltak = 0.
                      deltaq = 0.
                      qrefint = 0.
                      do k=klzb,kx
! deltaq = a positive quantity (since int qdel is positive).  It's how 
! much q_ref must be changed by, in an integrated sense.  The requisite 
! change in qdel is this without the factors of tau_bm and dt.
                         deltaq = deltaq - qdel(i,j,k)*tau_bm/dt* &
                                   (phalf(i,j,k) - phalf(i,j,k+1))
! deltak = the amount tdel needs to be changed
                         deltak  = deltak  + tdel(i,j,k)* &
                                   (phalf(i,j,k) - phalf(i,j,k+1))
! qrefint = integrated value of qref
                         qrefint = qrefint - q_ref(i,j,k)* &
                                   (phalf(i,j,k) - phalf(i,j,k+1))
                      end do
! Normalize deltak by total pressure.
                      deltak  = deltak /(phalf(i,j,kx+1) - phalf(i,j,klzb))
! multiplying factor for q_ref is 1 + the ratio
                      deltaqfrac = 1. - deltaq/qrefint
! multiplying factor for qdel adds dt/tau_bm
                      deltaqfrac2 = - deltaq/qrefint*dt/tau_bm
                      precip(i,j) = 0.0
                      do k=klzb,kx
                         qdel(i,j,k) = qdel(i,j,k) + deltaqfrac2*q_ref(i,j,k)
                         q_ref(i,j,k) = deltaqfrac*q_ref(i,j,k)
                         tdel(i,j,k) = tdel(i,j,k) + deltak
                         t_ref(i,j,k) = t_ref(i,j,k) + deltak*tau_bm/dt
                      end do
                   else
                      precip(i,j) = 0.
                      tdel(i,j,:) = 0.
                      qdel(i,j,:) = 0.
                      invtau_bm_t(i,j) = 0.
                      invtau_bm_q(i,j) = 0.
                   end if
! for cases where virtual temp predicts CAPE but precip_t < 0.
                else
!                   print *,'cape but no precip' 
                   tdel(i,j,:) = 0.0
                   qdel(i,j,:) = 0.0
                   precip(i,j) = 0.0
                   q_ref(i,j,:) = qin(i,j,:)
                   t_ref(i,j,:) = tin(i,j,:)
                   invtau_bm_t(i,j) = 0.
                   invtau_bm_q(i,j) = 0.
                end if
! if no CAPE, set tendencies to zero.
             else 
!                print *,'no cape' 
                tdel(i,j,:) = 0.0
                qdel(i,j,:) = 0.0
                precip(i,j) = 0.0
                q_ref(i,j,:) = qin(i,j,:)
                t_ref(i,j,:) = tin(i,j,:)
                invtau_bm_t(i,j) = 0.
                invtau_bm_q(i,j) = 0.
             end if
          end do
       end do

       rain = precip
   
   end subroutine betts_miller

!#######################################################################

!all new cape calculation.

      subroutine capecalcnew(kx,p,phalf,cp_air,rdgas,rvgas,ps,es0,T00,hlv,kappa,tin,rin,&
                             avgbl,cape,cin,tp,rp,klzb)

!
!    Input:
!
!    kx          number of levels
!    p           pressure (index 1 refers to TOA, index kx refers to surface)
!    phalf       pressure at half levels
!    cp_air      specific heat of dry air
!    rdgas       gas constant for dry air
!    rvgas       gas constant for water vapor (used in Clausius-Clapeyron, 
!                not for virtual temperature effects, which are not considered)
!    ps       surface pressure
!    es0         reference sat vap pressure
!    T00         reference temperature
!    hlv         latent heat of vaporization
!    kappa       the constant kappa
!    tin         temperature of the environment
!    rin         specific humidity of the environment
!    avgbl       if true, the parcel is averaged in theta and r up to its LCL
!
!    Output:
!    cape        Convective available potential energy
!    cin         Convective inhibition (if there's no LFC, then this is set 
!                to zero)
!    tp          Parcel temperature (set to the environmental temperature 
!                where no adjustment)
!    rp          Parcel specific humidity (set to the environmental humidity 
!                where no adjustment, and set to the saturation humidity at 
!                the parcel temperature below the LCL)
!    klzb        Level of zero buoyancy
!
!    Algorithm: 
!    Start with surface parcel. 
!    Calculate the lifting condensation level (uses an analytic formula and a 
!       lookup table).  
!    Average under the LCL if desired, if this is done, then a new LCL must
!       be calculated.  
!    Calculate parcel ascent up to LZB.
!    Calculate CAPE and CIN.  
      implicit none
      integer, intent(in)                    :: kx
      logical, intent(in)                    :: avgbl
      real, intent(in), dimension(kx)         :: p, phalf, tin, rin
      real, intent(in)                       :: rdgas, rvgas, hlv, kappa, cp_air, ps, es0, T00
      integer, intent(out)                   :: klzb
      real, intent(out), dimension(kx)        :: tp, rp
      real, intent(out)                      :: cape, cin

      integer            :: k, klcl, klfc, klcl2
      logical            :: nocape,success
      real, dimension(kx)   :: theta
      real                  :: t0, r0, es, rs, theta0, value, tlcl, &
                               a, b, dtdlnp, d2tdlnp2, thetam, rm, tlcl2, &
                               plcl2, plcl, plzb, small
      ! variables for root finder (LCL calculation)
      real                  :: Tmin,Tmax,f1,f2
      real,dimension(10)    :: args
      real                  :: ridder, lclfunc
      external ridder, lclfunc
      
! so we can run dry limit (one expression involves 1/hlv)
      small = 1.e-10

      nocape = .true.
      cape = 0.
      cin = 0.
      plcl = 0.
      plzb = 0.
      klfc = 0
      klcl = 0
      klzb = 0
      tp(1:kx) = tin(1:kx)
      rp(1:kx) = rin(1:kx)

! start with surface parcel
      t0 = tin(kx)
      r0 = rin(kx)
! calculate the lifting condensation level by the following:
! are you saturated to begin with?  
      call escomp(hlv, rvgas, es0, T00, t0,es)
      rs = rdgas/rvgas*es/p(kx)
      if (r0.ge.rs) then
! if you¹re already saturated, set lcl to be the surface value.
         plcl = p(kx)
! the first level where you¹re completely saturated.
         klcl = kx
! saturate out to get the parcel temp and humidity at this level
! first order (in delta T) accurate expression for change in temp
         tp(kx) = t0 + (r0 - rs)/(cp_air/(hlv+small) + hlv*rs/rvgas/t0**2.)
         call escomp(hlv, rvgas, es0, T00, tp(kx),es)
         rp(kx) = rdgas/rvgas*es/p(kx)
      else

! if not saturated to begin with, use the analytic expression to calculate the 
! exact pressure and temperature where you¹re saturated.  
         theta0 = tin(kx)*(ps/p(kx))**kappa
! the expression that we utilize is 
! log(r/theta**(1/kappa)*ps*rvgas/rdgas/es00) = log(es/T**(1/kappa))
! (the division by es00 is necessary because the RHS values are tabulated
! for control moisture content)
! The right hand side of this is only a function of temperature, therefore 
! this is put into a lookup table to solve for temperature.  
         if (r0.gt.0.) then
            ! Bracket root to within 2K
            args(1) = r0
            args(2) = theta0
            args(3) = kappa
            args(4) = ps
            args(5) = rvgas
            args(6) = rdgas
            args(7) = es0
            args(8) = hlv
            args(9) = T00
            Tmin = 10.
            Tmax = 12.
            f1   = lclfunc(args,Tmin)
            f2   = lclfunc(args,Tmax)
            do while(Tmax.lt.5000.and.f1*f2.ge.0.)
               Tmax = Tmax+1.
               f2   = lclfunc(args,Tmax)
            enddo
            Tmin = Tmax - 2.
            ! Refine estimate with Ridder's method
            tlcl = ridder(success,lclfunc,args,Tmin,Tmax,1.e-8)
            plcl = ps*(tlcl/theta0)**(1/kappa)
! just in case plcl is very high up
            if (plcl.lt.p(1)) then
               plcl = p(1)
               tlcl = theta0*(plcl/ps)**kappa
            end if
            k = kx
         else
! if the parcel sp hum is zero or negative, set lcl to top level
            plcl = p(1)
            tlcl = theta0*(plcl/ps)**kappa
!            write (*,*) 'zero r0', r0
            do k=1,kx
               tp(k) = theta0*(p(k)/ps)**kappa
               rp(k) = 0.
! this definition of CIN contains everything below the LCL
               cin = cin + rdgas*(tin(k) - tp(k))*log(phalf(k+1)/phalf(k))
            end do
            go to 11
         end if
! calculate the parcel temperature (adiabatic ascent) below the LCL.  
! the mixing ratio stays the same
         do while (p(k).gt.plcl)
            tp(k) = theta0*(p(k)/ps)**kappa
            call escomp(hlv, rvgas, es0, T00, tp(k),es)
            rp(k) = rdgas/rvgas*es/p(k)
! this definition of CIN contains everything below the LCL
            cin = cin + rdgas*(tin(k) - tp(k))*log(phalf(k+1)/phalf(k))
            k = k-1
         end do
! first level where you're saturated at the level
         klcl = k
	 if (klcl.eq.1) klcl = 2
! do a saturated ascent to get the parcel temp at the LCL.  
! use your 2nd order equation up to the pressure above.  
! moist adaibat derivatives: (use the lcl values for temp, humid, and 
! pressure)
         a = kappa*tlcl + hlv/cp_air*r0
         b = hlv**2.*r0/cp_air/rvgas/tlcl**2.
         dtdlnp = a/(1. + b)
! first order in p
!         tp(klcl) = tlcl + dtdlnp*log(p(klcl)/plcl)
! second order in p (RK2)
! first get temp halfway up 
         tp(klcl) = tlcl + dtdlnp*log(p(klcl)/plcl)/2.
         if ((tp(klcl).lt.173.16).and.nocape) go to 11
         call escomp(hlv, rvgas, es0, T00, tp(klcl),es)
         rp(klcl) = rdgas/rvgas*es/(p(klcl) + plcl)*2.
         a = kappa*tp(klcl) + hlv/cp_air*rp(klcl)
         b = hlv**2./cp_air/rvgas*rp(klcl)/tp(klcl)**2.
         dtdlnp = a/(1. + b)
! second half of RK2
         tp(klcl) = tlcl + dtdlnp*log(p(klcl)/plcl)
!         d2tdlnp2 = (kappa + b - 1. - b/tlcl*(hlv/rvgas/tlcl - &
!                   2.)*dtdlnp)/ (1. + b)*dtdlnp - hlv*r0/cp_air/ &
!                   (1. + b)
! second order in p
!         tp(klcl) = tlcl + dtdlnp*log(p(klcl)/plcl) + .5*d2tdlnp2*(log(&
!             p(klcl)/plcl))**2.
         if ((tp(klcl).lt.173.16).and.nocape) go to 11
         call escomp(hlv, rvgas, es0, T00, tp(klcl),es)
         rp(klcl) = rdgas/rvgas*es/p(klcl)
!         write (*,*) 'tp, rp klcl:kx, new', tp(klcl:kx), rp(klcl:kx)
! CAPE/CIN stuff
         if ((tp(klcl).lt.tin(klcl)).and.nocape) then
! if you¹re not yet buoyant, then add to the CIN and continue
            cin = cin + rdgas*(tin(klcl) - &
                 tp(klcl))*log(phalf(klcl+1)/phalf(klcl))
         else
! if you¹re buoyant, then add to cape
            cape = cape + rdgas*(tp(klcl) - &
                  tin(klcl))*log(phalf(klcl+1)/phalf(klcl))
! if it¹s the first time buoyant, then set the level of free convection to k
            if (nocape) then
               nocape = .false.
               klfc = klcl
            endif
         end if
      end if
! then average the properties over the boundary layer if so desired.  to give 
! a new "parcel".  this may not be saturated at the LCL, so make sure you get 
! to a level where it is before moist adiabatic ascent!
!!!! take out all the below (between the exclamation points) if no avgbl !!!!
      if (avgbl) then
         theta(klcl:kx) = tin(klcl:kx)*(ps/p(klcl:kx))**kappa
         thetam = 0.
         rm = 0.
         do k=klcl,kx
            thetam = thetam + theta(k)*(phalf(k+1) - phalf(k))
            rm = rm + rin(k)*(phalf(k+1) - phalf(k))
         end do
         thetam = thetam/(phalf(kx+1) - phalf(klcl))
         rm = rm/(phalf(kx+1) - phalf(klcl))
! check if you're saturated at the top level.  if not, then get a new LCL
         tp(klcl) = thetam*(p(klcl)/ps)**kappa
         call escomp(hlv, rvgas, es0, T00, tp(klcl),es)
         rs = rdgas/rvgas*es/p(klcl)

! if you're not saturated, get a new LCL
         if (rm.lt.rs) then
! reset CIN to zero.  
            cin = 0.
! again, use the analytic expression to calculate the exact pressure and 
! temperature where you¹re saturated.  
! the expression that we utilize is 
! log(r/theta**(1/kappa)*ps*rvgas/rdgas/es00)= log(es/T**(1/kappa))
! (the division by es00 is necessary because the RHS values are tabulated
! for control moisture content)
            ! Bracket root to within 2K
            args(1) = rm
            args(2) = thetam
            args(3) = kappa
            args(4) = ps
            args(5) = rvgas
            args(6) = rdgas
            args(7) = es0
            args(8) = hlv
            args(9) = T00
            Tmin = 10.
            Tmax = 12.
            f1   = lclfunc(args,Tmin)
            f2   = lclfunc(args,Tmax)
            do while(Tmax.lt.500.and.f1*f2.ge.0.)
               Tmax = Tmax+1.
               f2   = lclfunc(args,Tmax)
            enddo
            Tmin = Tmax - 2.
            ! Refine estimate with Ridder's method
            tlcl = ridder(success,lclfunc,args,Tmin,Tmax,1.e-8)
            print *,tmin,tmax
            plcl2 = ps*(tlcl2/thetam)**(1/kappa)
! just in case plcl is very high up
            if (plcl2.lt.p(1)) then
               plcl2 = p(1)
            end if
            k = kx
! calculate the parcel temperature (adiabatic ascent) below the LCL.  
! the mixing ratio stays the same
            do while (p(k).gt.plcl2) 
               tp(k) = thetam*(p(k)/ps)**kappa
               call escomp(hlv, rvgas, es0, T00, tp(k),es)
               rp(k) = rdgas/rvgas*es/p(k)
! this definition of CIN contains everything below the LCL
               cin = cin + rdgas*(tin(k) - tp(k))*log(phalf(k+1)/phalf(k))
               k = k-1
            end do
! first level where you¹re saturated at the level
            klcl2 = k
	    if (klcl2.eq.1) klcl2 = 2
! do a saturated ascent to get the parcel temp at the LCL.  
! use your 2nd order equation up to the pressure above.  
! moist adaibat derivatives: (use the lcl values for temp, humid, and 
! pressure)
            a = kappa*tlcl2 + hlv/cp_air*rm
            b = hlv**2.*rm/cp_air/rvgas/tlcl2**2.
            dtdlnp = a/(1. + b)
! first order in p
!            tp(klcl2) = tlcl2 + dtdlnp*log(p(klcl2)/plcl2)
! second order in p (RK2)
! first get temp halfway up 
         tp(klcl2) = tlcl2 + dtdlnp*log(p(klcl2)/plcl2)/2.
         if ((tp(klcl2).lt.173.16).and.nocape) go to 11
         call escomp(hlv, rvgas, es0, T00, tp(klcl2),es)
         rp(klcl2) = rdgas/rvgas*es/(p(klcl2) + plcl2)*2.
         a = kappa*tp(klcl2) + hlv/cp_air*rp(klcl2)
         b = hlv**2./cp_air/rvgas*rp(klcl2)/tp(klcl2)**2.
         dtdlnp = a/(1. + b)
! second half of RK2
         tp(klcl2) = tlcl2 + dtdlnp*log(p(klcl2)/plcl2)
!            d2tdlnp2 = (kappa + b - 1. - b/tlcl2*(hlv/rvgas/tlcl2 - &
!                          2.)*dtdlnp)/ (1. + b)*dtdlnp - hlv*rm/cp_air/ &
!                          (1. + b)
! second order in p
!            tp(klcl2) = tlcl2 + dtdlnp*log(p(klcl2)/plcl2) + &
!               .5*d2tdlnp2*(log(p(klcl2)/plcl2))**2.
            call escomp(hlv, rvgas, es0, T00, tp(klcl2),es)
            rp(klcl2) = rdgas/rvgas*es/p(klcl2)
! CAPE/CIN stuff
            if ((tp(klcl2).lt.tin(klcl2)).and.nocape) then
! if you¹re not yet buoyant, then add to the CIN and continue
               cin = cin + rdgas*(tin(klcl2) - &
                    tp(klcl2))*log(phalf(klcl2+1)/phalf(klcl2))
            else
! if you¹re buoyant, then add to cape
               cape = cape + rdgas*(tp(klcl) - &
                     tin(klcl))*log(phalf(klcl+1)/phalf(klcl))
! if it¹s the first time buoyant, then set the level of free convection to k
               if (nocape) then
                  nocape = .false.
                  klfc = klcl2
               endif
            end if
         end if
      end if
!!!! take out all of the above (within the exclamations) if no avgbl !!!!
! then, start at the LCL, and do moist adiabatic ascent by the first order 
! scheme -- 2nd order as well
      do k=klcl-1,1,-1
         a = kappa*tp(k+1) + hlv/cp_air*rp(k+1)
         b = hlv**2./cp_air/rvgas*rp(k+1)/tp(k+1)**2.
         dtdlnp = a/(1. + b)
! first order in p
!         tp(k) = tp(k+1) + dtdlnp*log(p(k)/p(k+1))
! second order in p (RK2)
! first get temp halfway up 
         tp(k) = tp(k+1) + dtdlnp*log(p(k)/p(k+1))/2.
         if ((tp(k).lt.173.16).and.nocape) go to 11
         call escomp(hlv, rvgas, es0, T00, tp(k),es)
         rp(k) = rdgas/rvgas*es/(p(k) + p(k+1))*2.
         a = kappa*tp(k) + hlv/cp_air*rp(k)
         b = hlv**2./cp_air/rvgas*rp(k)/tp(k)**2.
         dtdlnp = a/(1. + b)
! second half of RK2
         tp(k) = tp(k+1) + dtdlnp*log(p(k)/p(k+1))
!         d2tdlnp2 = (kappa + b - 1. - b/tp(k+1)*(hlv/rvgas/tp(k+1) - & 
!               2.)*dtdlnp)/(1. + b)*dtdlnp - hlv/cp_air*rp(k+1)/(1. + b)
! second order in p
!         tp(k) = tp(k+1) + dtdlnp*log(p(k)/p(k+1)) + .5*d2tdlnp2*(log( &
!             p(k)/p(k+1)))**2.
! if you're below the lookup table value, just presume that there's no way 
! you could have cape and call it quits
         if ((tp(k).lt.173.16).and.nocape) go to 11
         call escomp(hlv, rvgas, es0, T00, tp(k),es)
         rp(k) = rdgas/rvgas*es/p(k)
         if ((tp(k).lt.tin(k)).and.nocape) then
! if you¹re not yet buoyant, then add to the CIN and continue
            cin = cin + rdgas*(tin(k) - tp(k))*log(phalf(k+1)/phalf(k))
         elseif((tp(k).lt.tin(k)).and.(.not.nocape)) then
! if you have CAPE, and it¹s your first time being negatively buoyant, 
! then set the level of zero buoyancy to k+1, and stop the moist ascent
            klzb = k+1
            go to 11
         else
! if you¹re buoyant, then add to cape
            cape = cape + rdgas*(tp(k) - tin(k))*log(phalf(k+1)/phalf(k))
! if it¹s the first time buoyant, then set the level of free convection to k
            if (nocape) then
               nocape = .false.
               klfc = k
            endif
         end if
      end do
 11   if(nocape) then 
! this is if you made it through without having a LZB
! set LZB to be the top level.
         plzb = p(1)
         klzb = 0
         klfc = 0
         cin = 0.
         tp(1:kx) = tin(1:kx)
         rp(1:kx) = rin(1:kx)
      end if
!      write (*,*) 'plcl, klcl, tlcl, r0 new', plcl, klcl, tlcl, r0
!      write (*,*) 'tp, rp new', tp, rp
!       write (*,*) 'tp, new', tp
!       write (*,*) 'tin new', tin 
!       write (*,*) 'klcl, klfc, klzb new', klcl, klfc, klzb
      end subroutine capecalcnew

!************************************************************
subroutine escomp(hlv, Ra, es0, T00, T, es)

! Clausius-Clapeyron saturation vapor pressure
real hlv, Ra, es0, T00, T, es

! Methane (see http://www.airliquide.com/en/business/products/gases/gasdata/index.asp?GasID=41)
! hlv  = 510.e3  ! J/kg, latent heat of vap.
! Ra   = 499.9   ! J/kg/K
! es0  = 1.013e5 ! kPa, standard pressure
! T00  = 111.4   ! K, boiling point

es = es0 * exp(-hlv/Ra * (1./T - 1./T00))

end subroutine escomp
!************************************************************
real function lclfunc(args,T)

  real,dimension(10)  :: args
  real                :: T
  
  real                :: r, theta, kappa, ps, rvgas, rdgas, es0, hlv, es

! log(r/theta**(1/kappa)*ps*rvgas/rdgas/es00) = log(es/T**(1/kappa))
  r      = args(1)
  theta  = args(2)
  kappa  = args(3)
  ps     = args(4)
  rvgas  = args(5)
  rdgas  = args(6)
  es0    = args(7)
  hlv    = args(8)
  T00    = args(9)

  call escomp(hlv, rvgas, es0, T00, T, es)

  lclfunc = log(r/theta**(1/kappa)*ps*rvgas/rdgas/es0) - log(es*T**(-1./kappa))

end function lclfunc
!--------------------------------------------------------------------
real(8) function ridder(success,func,params,xmin,xmax,acc)

! Find zero crossing of func(x) in range xmin < x < xmax with accuracy acc
! Use Ridder's method (Numerical Recipes)

  implicit real (a-h,o-z)
!  integer maxit,j
!  real(8) fl,fh,xl,xh,xm,fm,s,xnew,fnew
!  real(8) func
  real,dimension(10) :: params
  real xmin,xmax,acc
  logical success
  external func

  maxit=60  ! max number of iterations
  success=.true.

  fl=func(params,xmin)
  fh=func(params,xmax)

  if((fl.gt.0..and.fh.lt.0.).or.(fl.lt.0..and.fh.gt.0.))then
     xl=xmin
     xh=xmax
     ridder=-1.11e30
     do j=1,maxit
        xm=0.5*(xl+xh)
        fm=func(params,xm)
        s=sqrt(fm**2-fl*fh)
        if(s.eq.0.)return
        xnew=xm+(xm-xl)*(sign(1.,fl-fh)*fm/s)
        if (abs(xnew-ridder).le.acc) return
        ridder=xnew
        fnew=func(params,ridder)
        if (fnew.eq.0.) return
        if(sign(fm,fnew).ne.fm) then
           xl=xm
           fl=fm
           xh=ridder
           fh=fnew
        else if(sign(fl,fnew).ne.fl) then
           xh=ridder
           fh=fnew
        else if(sign(fh,fnew).ne.fh) then
           xl=ridder
           fl=fnew
        else
           stop "never get here in ridder" 
        endif
        if(abs(xh-xl).le.acc) return
     enddo
     stop "ridder exceed maximum iterations maxit" 
  else if (fl.eq.0.) then
     ridder=xmin
  else if (fh.eq.0.) then
     ridder=xmax
  else
     success=.false.
  endif

end function ridder
