c     
c A collection of functions to compute various moist thermodynamic quantities
c
c Rodrigo Caballero Augi 2000 (rca@dcess.ku.dk)
c
c References: Bolton (1980), Mon. Wea. Rev. 108, 1046-1053 
c             Bohren and Albrecht (1998), "Atmospheric Thermodynamics"
c             Flatau et al (1992), J. App. Met . 31, 1507-1513.
c
c------------------------------------------------------------------------
      real*8 function tdew(p,q)
c
c compute dew point temperature tdew  [K] 
c at pressure p [mb] and specific humidity q [g/kg]
c Use Bolton eqn (11)
c
      real*8 p,q

      Rv  = 461.5
      Rd  = 287.04
      eps = Rd/Rv
      r = q/(1.e3 - q) * 1.e3 ! water vap. mixing ratio [g/kg]
      es = p*r/(r+eps*1.e3)   ! vapor pressure of water vap
c
      tdew = (243.5*log(es)-440.8)/(19.48-log(es))+273.15
c
      end
c------------------------------------------------------------------------
      real*8 function tstar(t,p,q)
c
c compute saturation point temperature tstar [K] 
c (i.e. temperature at lifting condensation level) 
c given temperature t [K], pressure p [mb], specific humidity q [g/kg].
c Use Bolton eqn (12)
c
      real*8 t,p,q
      external theta

      Rv  = 461.5
      Rd  = 287.04
      eps = Rd/Rv
      oneoverk = 3.504 ! Cp/Rd
      r = q/(1.e3 - q) * 1.e3 ! water vap. mixing ratio [g/kg]
c
      th=theta(t,p,q)
      c=log(r/(r+eps*1.e3)/th**oneoverk/1.7743e-8)
      tstar=217.8*c/(12.992-c) + 273.15
c
      end
c------------------------------------------------------------------------
      real*8 function theta(t,p,q)
c
c compute potential temperature  theta [K]
c given temperature t [K], pressure p [mb], specific humidity q [g/kg].
c Use Bolton eqn (7) 
c 
      ak=0.2854 ! R_(dry air)/Cp
      r = q/(1.e3 - q) * 1.e3 ! water vap. mixing ratio [g/kg]
c
      theta=t*(1000./p)**( ak*(1.-0.28e-3*r) )
c      
      end
c------------------------------------------------------------------------
      real*8 function thetae(t,p,q)
c
c compute equivalent potential temperature thetae [K]
c given temperature t [K], pressure p [mb], specific humidity q [g/kg].
c Use Bolton eqn (38)
c
      real*8 t,p,q
      external theta,tstar

      th=theta(t,p,q)
      ts=tstar(t,p,q)
      r = q/(1.e3 - q) * 1.e3 ! water vap. mixing ratio [g/kg]

      thetae = th * exp( (3.376/ts-0.00254)*r*(1.+0.81e-3*r) )
c
      end
c------------------------------------------------------------------------
      real*8 function es(t)
c
c Compute saturation partial pressure of water vapor es [mb]
c at temperature t [K].
c Use Bohren+Albrecht p. 198
c     
      real*8 t
      t0m1=1./273.15
c
      es = 6.11 * exp( 6808.*(t0m1-1./t)-5.09*log(t*t0m1) )
c
      end
c------------------------------------------------------------------------
      real*8 function esflatau(t,i)
c
c Compute saturation partial pressure of water vapor es [mb]
c at temperature t [K].
c Use 8th oRder polynomial fit of Flatau et al (1992), Table 4
c
c i=1 => vapour pressure over water (valid -85C < t < 70C)
c i=2 => vapour pressure over ice (valid -90C < t < 0C)
c    
      real*8 t,a(9,2)
      data a /
     &     6.11239921,    0.443987641,  0.142986287e-1,  0.264847430e-3,
     & 0.302950461e-5, 0.206739458e-7, 0.640689451e-10,-0.952447341e-13,
     & -0.976195544e-15,
     &     6.11147274,    0.503160820, 0.188439774e-1,  0.420895665e-3,
     & 0.615021634e-5, 0.602588177e-7, 0.385852041e-9, 0.146898966e-11,
     & 0.252751365e-14 /
c
      t0=273.15
c
      esflatau=a(9,i)
      do n=8,1,-1
      esflatau=esflatau*(t-t0) + a(n,i)
      enddo
c
      end
c------------------------------------------------------------------------
      real*8 function qs(t,p)
c
c Compute saturation specific humidity qs [g/kg]
c at temperature t [K] and pressure p [mb]
c use Bohren+Albrecht p. 186
c     
      real*8 t,p
      external es

      Rv=461.5
      Rd=287.04
      eps=Rd/Rv
c
      ess=es(t)
      qs=eps*ess/p *1.e3
c
      end
c------------------------------------------------------------------------
      real*8 function qsflatau(t,p,i)
c
c Compute saturation specific humidity qs [g/kg]
c at temperature t [K] and pressure p [mb]
c Use 8th oRder polynomial fit of Flatau et al (1992), Table 4
c
c i=1 => sat. mix. rat. over water (valid -85C < t < 70C)
c i=2 => sat. mix. rat. over ice (valid -90C < t < 0C)
c
      real*8 t,p
      Rv=461.5
      Rd=287.04
      eps=Rd/Rv*1.e3
c
      es=esflatau(t,i)
      qsflatau=eps*es/p
c
      end
c------------------------------------------------------------------------
      real*8 function ws(t,p)
c
c Compute saturation water vapor mass mixing ratio  ws [g/kg]
c at temperature t [K] and pressure p [mb]
c use Bohren+Albrecht p. 186
c     
      real*8 t,p
      external es

      Rv=461.5
      Rd=287.04
      eps=Rd/Rv
c
      ess=es(t)
      ws=eps*ess/(p-ess) *1.e3
c
      end
c------------------------------------------------------------------------
      real*8 function wsflatau(t,p,i)
c
c Compute saturation water vapor mass mixing ratio ws [g/kg]
c at temperature t [K] and pressure p [mb]
c Use 8th oRder polynomial fit of Flatau et al (1992), Table 4
c
c i=1 => sat. mix. rat. over water (valid -85C < t < 70C)
c i=2 => sat. mix. rat. over ice (valid -90C < t < 0C)
c
      real*8 t,p
      Rv=461.5
      Rd=287.04
      eps=Rd/Rv
c
      es=esflatau(t,i)
      wsflatau=eps*es/(p-es) * 1.e3
c
      end
c------------------------------------------------------------------------
      real*8 function pdryadiab(t,theta,q)
c
c compute pressure level pdryadiab [mb] at which temperature is t [K]
c on the dry (i.e. unsaturated) adiabat identified by 
c potential temperature theta [K] and mixing ratio q [g/kg].
c Use Bolton eqn (7) 
c
      oneoverk=3.504 ! Cp/R_(dry air)
      r = q/(1.e3 - q) * 1.e3 ! water vap. mixing ratio [g/kg]
c
      pdryadiab=1000.*(t/theta)**( oneoverk/(1.-0.28e-3*r) )
c
      end
c------------------------------------------------------------------------
      real*8 function tdryadiab(theta,p,q)
c
c Compute temperature [K] at level p [mb] 
c on the dry (i.e. unsaturated) adiabat identified by 
c potential temperature theta [K] and mixing ratio q [g/kg].
c Use Bolton eqn (7) 
c
      real*8 theta,p,q
      ak=0.2854 ! R_(dry air)/Cp
      r = q/(1.e3 - q) * 1.e3 ! water vap. mixing ratio [g/kg]
c
      tdryadiab=theta*(1000./p)**( -ak*(1.-0.28e-3*r) )
c      
      end
c------------------------------------------------------------------------
      real*8 function tmoistadiab(thetaes,p)
c
c Compute temperature [K] at level p [mb] 
c on the moist (i.e. saturated) pseudo-adiabat identified 
c by saturation equivalent potential temperature thetaes [K].
c Do it by varying t until 
c delthetae := thetaes - thetae(t,p,qs(t,p)) = 0
c Use Bolton, eqn (43)
c
      external delthetae,ridder
      real*8 thetaes,p,params(10)
      logical success
c
      params(1)=thetaes
      params(2)=p
c
c   bracket zero crossing to within 2K
      tmin=50.
      tmax=51.
      f1=delthetae(params,tmin)
      f2=delthetae(params,tmax)
 1    continue
      if(tmax.gt.500.) goto 2
      if(f1*f2.lt.0.) goto 3
      tmax=tmax+1.
      f2=delthetae(params,tmax)
      goto 1
 2    stop 'tmoistadiab(): cannot bracket root'
 3    continue
      tmin=tmax-2.
c
c  refine estimate using Ridder's method 
      tmoistadiab=ridder(success,delthetae,params,tmin,tmax,0.0001)
      if (.not.success) stop 'tmoistadiab(): root not bracketed'
c
      end
c-----------------------------------------------------------------------
      real*8 function delthetae(params,t)
      external qs,thetae
      real*8 t,params(10)

      thetaes   = params(1)
      p         = params(2)
      q         = qs(t,p)
      delthetae = thetaes - thetae(t,p,q)

      end
c------------------------------------------------------------------------
      real*8 function ridder(success,func,params,xmin,xmax,acc)
c
c Find zero crossing of func(x) in range xmin < x < xmax with accuracy acc
c Use Ridder's method (Numerical Recipes)
c
      external func
      real*8 params(10),xmin,xmax,acc
      logical success
c
      maxit=60  ! max number of iterations
      success=.true.
c
      fl=func(params,xmin)
      fh=func(params,xmax)
c
      if((fl.gt.0..and.fh.lt.0.).or.(fl.lt.0..and.fh.gt.0.))then
        xl=xmin
        xh=xmax
        ridder=-1.11e30
        do 11 j=1,maxit
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
            stop 'never get here in ridder'
          endif
          if(abs(xh-xl).le.acc) return
11      continue
        stop 'ridder exceed maximum iterations maxit'
      else if (fl.eq.0.) then
        ridder=xmin
      else if (fh.eq.0.) then
        ridder=xmax
      else
        success=.false.
      endif
c
      end


