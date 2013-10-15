      subroutine ozone(km, jm, im, p, o3)

c Interpolates an observed, tropical ozone profile onto levels specified in p

      real, dimension(km,jm,im) ::  p, o3
      real, dimension(32)       ::  pdat, o3dat

      data pdat /
     & 5.8E-002,  
     & 0.854000000000000,
     & 1.59000000000000,
     & 3.05000000000000,
     & 6.00000000000000,
     & 12.2000000000000,
     & 25.7000000000000,
     & 30.0000000000000,
     & 35.0000000000000,
     & 40.9000000000000,
     & 48.0000000000000,
     & 56.5000000000000,
     & 66.6000000000000,
     & 78.9000000000000,
     & 93.7000000000000,
     & 111.000000000000,
     & 132.000000000000,
     & 156.000000000000,
     & 182.000000000000,
     & 213.000000000000,
     & 247.000000000000,
     & 286.000000000000,
     & 329.000000000000,
     & 378.000000000000,
     & 432.000000000000,
     & 492.000000000000,
     & 559.000000000000,
     & 633.000000000000,
     & 715.000000000000,
     & 805.000000000000,
     & 904.000000000000,
     & 1013.00000000000/

      data o3dat/
     & 6.195795690538510E-007,
     & 4.679844888796695E-006,
     & 7.688602631127004E-006,
     & 1.337892090780487E-005,
     & 1.805195910096790E-005,
     & 1.816895744985279E-005,
     & 1.104250541998903E-005,
     & 8.899986501627470E-006,
     & 7.100790146909205E-006,
     & 5.050033660844585E-006,
     & 3.848252873407772E-006,
     & 3.061263583490679E-006,
     & 2.120476522512967E-006,
     & 1.138644685053795E-006,
     & 5.806530579889549E-007,
     & 3.313083403725338E-007,
     & 2.786001864526371E-007,
     & 2.260507236737479E-007,
     & 1.937577593734446E-007,
     & 1.584629153240741E-007,
     & 1.298516863623691E-007,
     & 1.068603344149751E-007,
     & 9.289378536511751E-008,
     & 8.085199778587103E-008,
     & 7.424390151695265E-008,
     & 6.860284376810644E-008,
     & 6.308391668239037E-008,
     & 5.809671361569896E-008,
     & 5.589617320584910E-008,
     & 5.256731806461826E-008,
     & 4.848230235436268E-008,
     & 4.326554905269745E-008/

      call interpol(pdat, o3dat, 32, p(:,1,1), o3(:,1,1), km)

      do i=1,im
      do j=1,jm
      o3(:,j,i) = o3(:,1,1)
      enddo
      enddo

      end
c------------------------------------------------------------------------
      subroutine interpol(xin, yin, nin, xout, yout, nout)
c
      real xin(nin),yin(nin),xout(nout),yout(nout),spl(2000)
c
      yp1=1.e30
      ypn=1.e30
      call spline(xin,yin,nin,yp1,ypn,spl) 
c
      do n=1,nout
      call splint(xin,yin,spl,nin,xout(n),yout(n))
      enddo
c
      end
c------------------------------------------------------------------------
      SUBROUTINE spline(x,y,n,yp1,ypn,y2)
      INTEGER n,NMAX
      REAL yp1,ypn,x(n),y(n),y2(n)
      PARAMETER (NMAX=500)
      INTEGER i,k
      REAL p,qn,sig,un,u(NMAX)
      if (yp1.gt..99e30) then
        y2(1)=0.
        u(1)=0.
      else
        y2(1)=-0.5
        u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      endif
      do 11 i=2,n-1
        sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
        p=sig*y2(i-1)+2.
        y2(i)=(sig-1.)/p
        u(i)=(6.*((y(i+1)-y(i))/(x(i+
     *1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*
     *u(i-1))/p
11    continue
      if (ypn.gt..99e30) then
        qn=0.
        un=0.
      else
        qn=0.5
        un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      endif
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
      do 12 k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
12    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software Y"1i41k1'.
      SUBROUTINE splint(xa,ya,y2a,n,x,y)
      INTEGER n
      REAL x,y,xa(n),y2a(n),ya(n)
      INTEGER k,khi,klo
      REAL a,b,h
      klo=1
      khi=n
1     if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(xa(k).gt.x)then
          khi=k
        else
          klo=k
        endif
      goto 1
      endif
      h=xa(khi)-xa(klo)
      if (h.eq.0.) pause 'bad xa input in splint'
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**
     *2)/6.
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software Y"1i41k1'.







