      subroutine adjust(km, nl, Cpd, Ps, PH, P, T, Th, kap)

!     Hard adiabatic adjustment to lapse rate specified by kap

!     Follows Kerry Emanuel's convect43b.f but does not account for
!     virtual temperature effects and does not mix tracers

!     NOTE:  K=1 AT THE SURFACE!`
!     km  = vertical dimension
!     nl  = maximum level to which convection penetrates
!     Cpd = specific heat of dry air
!     Ps  = surface pressure
!     PH  = pressure at level interfaces
!     P   = pressure at midlevel
!     T   = temperature at midlevel
!     Th  = (effective) potential temperature at midlevel
!     kap = (effective) Rd/Cpd

      implicit none
      integer km,nl,jn,i,j,k,jc
      real Th(km),T(km),PH(km+1),P(km),TOLD(km)
      real sum,thbar,ahm,a2,RDCP,kap,X,Cpd,Ps
      
      do k=1,km
         told(k)=0.
      enddo

!     Set (effective) Rd/Cpd
      rdcp = kap

!     Perform adiabatic adjustment
      jc=0
      do 30 i=nl-1,1,-1
         jn=0
         sum=th(i)
         do j=i+1,nl
            sum=sum+th(j)
            thbar=sum/float(j+1-i)
            if(th(j).lt.thbar)jn=j
         enddo
!         if (i.eq.1) jn=max(jn,2)
         if (jn.eq.0) goto 30
 12      continue
         ahm=0.0
         do 15 j=i,jn
            ahm= ahm + cpd * t(j)*( ph(j)-ph(j+1) )
 15      continue
         a2=0.0
         do 20 j=i,jn
            x=(p(j)/ps)**rdcp
            told(j)=t(j)
            t(j)=x
            a2=a2+cpd*x*(ph(j)-ph(j+1))
 20      continue
         do 25 j=i,jn
            th(j)=ahm/a2
            t(j)=t(j)*th(j)
 25      continue
         if(th(jn+1).lt.th(jn).and.jn.le.nl)then
            jn=jn+1
            goto 12
         end if
         if(i.eq.1)jc=jn 
 30   continue
      
      end
