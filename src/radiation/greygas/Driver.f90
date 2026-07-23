subroutine driver(km,jm,im,              &
     beta, sigma, g, dt, Cpd, tau_inf, alpha, lwtau,         &
     T, q, p, ps, solin, Ts,             &
     Tinc, Tdot, SrfRadFlx, swhr, swflx_out, lwhr, lwflx_out, up_out, down_out, tau_out)
!-----------------------------!
!  CliMT driver for grey gas  !
!  radiation scheme           !
!  J.L. Mitchell 10Mar05      !
!_____________________________!

!! Note: tau_inf here is the BROADBAND optical depth

! In
integer im,jm,km
!f2py intent(in,hide) im,jm,km
real(8),                     intent(in) :: sigma, g, dt, Cpd, tau_inf, alpha, beta
real(8),dimension(km,jm,im), intent(in) :: T, q, p, lwtau
real(8),dimension(jm,im),    intent(in) :: Ts, ps, solin

! Out
real(8),dimension(km,jm,im), intent(out) :: Tinc,Tdot, lwflx_out, lwhr, up_out, down_out, tau_out
real(8),dimension(km,jm,im), intent(out) :: swflx_out, swhr
real(8),dimension(jm,im)   , intent(out) :: SrfRadFlx

! Local
real(8),dimension(0:km,jm,im) :: plev, tau, lwflx, up, down

! Input pressures are at midlevel, need p on level surfaces
plev(1:km-1,:,:) = ( p(1:km-1,:,:) + p(2:km,:,:) )/2.
plev(0     ,:,:) = 0.
plev(km    ,:,:) = ps

! Set profile of optical depth
if (lwtau(1,1,1) == -999.) then ! tau profile not passed in, define here
   do k=1,km
      tau_out(k,:,:) = tau_inf * (1.- ( p(k,:,:)/ps )**alpha )
   enddo
else
   tau_out = lwtau 
endif

do k=0,km
   do j=1,jm
      do i=1,im
         call interp2(km,p(:,j,i),tau_out(:,j,i),plev(k,j,i),tau(k,j,i))
      enddo
   enddo
enddo

! Loop over lat and lon
do i=1,im
   do j=1,jm
      call grey_gas(km,beta,sigma,T(:,j,i), tau(:,j,i), Ts(j,i),  &
                    lwflx(:,j,i), up(:,j,i), down(:,j,i))
   enddo
enddo

Tdot = g/Cpd*(lwflx(1:km,:,:)-lwflx(0:km-1,:,:))/        &
              (plev(1:km,:,:)-plev(0:km-1,:,:))/100.
lwflx_out = -lwflx(0:km-1,:,:) 
up_out    = -up(0:km-1,:,:)
down_out  = down(0:km-1,:,:) 
! Surface radiative flux
SrfRadFlx = -lwflx(km,:,:) + solin

Tinc = Tdot*2.*dt
Tdot = Tdot*86400.
lwhr = Tdot

do k=1,km
swflx_out(k,:,:) = solin
enddo
swhr = 0.  

end subroutine driver

! - - - > midlevel
! ----- > surfaces

!- - - - - - - - - - - - kappa(0),T(0)                  TOP OF ATMOSPHERE
!----------------------- press(0),tau(0),up/down(0)
!- - - - - - - - - - - - kappa(1),T(1)
!----------------------- press(1)
!- - - - - - - - - - - -   ..
!-----------------------   ..
!- - - - - - - - - - - -   ..
!-----------------------   ..
!- - - - - - - - - - - - kappa(nlev)
!----------------------- press(nlev)
!- - - - - - - - - - - - kappa(nlev+1)                  SURFACE


!------------------ code below is for interpolation of tau profile

! Routines to do linear interpolation.
! They differ only in what the routine does if the point x1 is outside 
! the range of the array x(n).

! Subroutine to do linear INTERPOLATION in array, and STOP if x1
! if outside endpoints.
subroutine interp(n,x,y,x1,y1)
  implicit none
  !
  ! Array dimensions
  integer n
  !
  ! Integers
  integer i1
  !
  ! Floats
  real fx,x(n),x1,y(n),y1
  !
  ! Code
  !
  ! Find place in array.
  call locate(x,n,x1,i1)
  ! x1 bracketed by x(i1) and x(i1+1)
  !     
  ! In case that x1 is exactly equal to either endpoint, reset i1
  ! so interpolation will proceed as if x1 lies within range.
  if (x1.eq.x(1)) i1=1
  if (x1.eq.x(n)) i1=n-1
  !     
  if (i1.eq.0.or.i1.eq.n) then ! Outside range of array.
     write (0,*) 'interp(): FATAL -  x1 outside range of array x(n)'
     write (0,*) '          x1=',x1,' x(1)=',x(1),' x(n)=',x(n)
     stop
  else ! Interpolate.
     fx=(x1-x(i1))/(x(i1+1)-x(i1))
     y1=(1.0-fx)*y(i1)+fx*y(i1+1)
  end if
  return
end subroutine interp

! Subroutine to do linear INTERPOLATION in array, and set y1=0 if x1
! if outside endpoints.
subroutine interp0(n,x,y,x1,y1)
  implicit none
  !
  ! Array dimensions
  integer n
  !
  ! Integers
  integer i1
  !
  ! Floats
  real fx,x(n),x1,y(n),y1
  !
  ! Code
  !
  ! Find place in array.
  call locate(x,n,x1,i1)
  ! x1 bracketed by x(i1) and x(i1+1).
  if(i1.eq.0 .or. i1.eq.n) then ! Outside range of array.
     y1=0.0
  else ! Interpolate.
     fx=(x1-x(i1))/(x(i1+1)-x(i1))
     y1=(1.0-fx)*y(i1)+fx*y(i1+1)
  end if
  return
end subroutine interp0

! Subroutine to do linear INTERPOLATION in array, and linear EXTRAPOLATION
! if x1 outside endpoints.
subroutine interp2(n,x,y,x1,y1)
  implicit none
  !
  ! Array dimensions
  integer n
  !
  ! Integers
  integer i1
  !
  ! Floats
  real fx,x(n),x1,y(n),y1
  !
  ! Code
  !     
  ! Find place in array.
  call locate(x,n,x1,i1)
  ! x1 bracketed by x(i1) and x(i1+1).
  if(i1.eq.0) then ! Extrapolate.
     fx=(x1-x(1))/(x(2)-x(1))
     y1=(1.0-fx)*y(1)+fx*y(2)
  else if(i1.eq.n) then ! Extrapolate.
     fx=(x1-x(n-1))/(x(n)-x(n-1))
     y1=(1.0-fx)*y(n-1)+fx*y(n)
  else ! Interpolate.
     fx=(x1-x(i1))/(x(i1+1)-x(i1))
     y1=(1.0-fx)*y(i1)+fx*y(i1+1)
  end if
  return
end subroutine interp2

subroutine locate(XX,N,X,J)
  implicit none
  !
  ! Integers
  integer J,JL,JM,JU,N
  !
  ! Floats
  real X,XX(N)
  !
  ! Code
  JL=0
  JU=N+1
  do while (JU-JL.gt.1)
     JM=(JU+JL)/2
     if ((XX(N).gt.XX(1)).eqv.(X.gt.XX(JM))) then
        JL=JM
     else
        JU=JM
     endif
  end do
  J=JL
  return
end subroutine locate

module Linear_Interpolation

contains

  ! Function to do linear interpolation in array, and stop if beyond endpoints (?)
  real function Lin_Interp_Error_at_Endpoints(n,x,y,x1)
    implicit none
    !
    ! Array dimensions
    integer n
    !
    ! Integers
    integer i1
    !
    ! Floats
    real fx,x(n),x1,y(n)
    !
    ! Code
    call Lin_Interp_Err_at_Endpts_Fctrs(n,x,x1,i1,fx)
    Lin_Interp_Error_at_Endpoints=Lin_Interp_Do(n,y,i1,fx)
    return
  end function Lin_Interp_Error_at_Endpoints

  ! Subroutine to do compute linear interpolation factors. Beyond the endpoints of array x, return factors such that the answer
  ! will equal the value of the array y at the endpoint.
  subroutine Lin_Interp_Err_at_Endpts_Fctrs(n,x,x1,i1,fx,ierr)
    implicit none
    !
    ! * Arguments *
    !
    ! Integers
    integer, intent(out), optional :: ierr
    !
    ! Array dimensions
    integer n
    !
    ! Integers
    integer i1
    !
    ! Floats
    real fx,x(n),x1
    !
    ! Logicals
    logical Out_of_Range
    !
    ! Code
    if (present(ierr)) ierr=0
    call Lin_Interp_Find_Interpolation(n,x,x1,i1,fx,Out_of_Range)
    ! x1 bracketed by x(i1) and x(i1+1).
    if (Out_of_Range) then
       if (present(ierr)) then
          ierr=1
       else
          stop 'Lin_Interp_Err_at_Endpts_Fctrs(): FATAL - x is out of range'
       end if
    end if
    return
  end subroutine Lin_Interp_Err_at_Endpts_Fctrs

  ! Function to do linear interpolation in array, and return endpoint value of y
  ! if outside endpoints.
  real function Lin_Interp_Fixed_Endpoints(n,x,y,x1)
    implicit none
    !
    ! Array dimensions
    integer n
    !
    ! Integers
    integer i1
    !
    ! Floats
    real fx,x(n),x1,y(n)
    !
    ! Code
    call Lin_Interp_Fxd_Endpts_Fctrs(n,x,x1,i1,fx)
    Lin_Interp_Fixed_Endpoints=Lin_Interp_Do(n,y,i1,fx)
    return
  end function Lin_Interp_Fixed_Endpoints

  ! Subroutine to do compute linear interpolation factors. Beyond the endpoints of array x, return factors such that the answer
  ! will equal the value of the array y at the endpoint.
  subroutine Lin_Interp_Fxd_Endpts_Fctrs(n,x,x1,i1,fx)
    implicit none
    !
    ! Array dimensions
    integer n
    !
    ! Integers
    integer i1
    !
    ! Floats
    real fx,x(n),x1
    !
    ! Logicals
    logical Out_of_Range
    !
    ! Code
    call Lin_Interp_Find_Interpolation(n,x,x1,i1,fx,Out_of_Range)
    ! x1 bracketed by x(i1) and x(i1+1).
    if (Out_of_Range) then
       if (i1.eq.1) then
          fx=0.0
       else if (i1.eq.n-1) then
          fx=1.0
       else
          write (0,*) i1,n
          stop ' Lin_Interp_Fixed_Endpoints(): FATAL - point is not really out of range!'
       end if
    endif
    return
  end subroutine Lin_Interp_Fxd_Endpts_Fctrs

  !
  ! Find the interpolating factors for a linear interpolation. Use extrapolation beyond the ends of the x array, but flag such
  ! points as Out of Range
  subroutine Lin_Interp_Find_Interpolation(n,x,x1,j,h,Out_of_Range)
    implicit none
    !
    ! Array dimensions
    integer n
    !
    ! Integer
    integer j
    !
    ! Floats
    real h,x(n),x1
    !
    ! Logicals
    logical Out_of_Range
    !
    ! Code
    call locate(x,n,x1,j)
    !
    ! Deal with points exactly at the endpoint in a sensible way
    if (j.eq.0.and.x1.eq.x(1)) then
       j=1
    else if (j.eq.n.and.x1.eq.x(n)) then
       j=n-1
    end if
    !
    ! Compute interpolation factors and mark if out of range
    if (j.eq.0) then
       Out_of_Range=.true.
       j=1
    else if (j.ge.n) then
       Out_of_Range=.true.
       j=n-1
    else
       Out_of_Range=.false.
    end if
    h=(x1-x(j))/(x(j+1)-x(j))
    return
  end subroutine Lin_Interp_Find_Interpolation

  !
  ! Linear interpolation formula
  real function Lin_Interp_Do(n,y,i1,fx)
    implicit none
    !
    ! Array dimensions
    integer n
    !
    ! Integers
    integer i1
    !
    ! Floats
    real fx,y(n)
    !
    ! Code
    Lin_Interp_Do=(1.0-fx)*y(i1)+fx*y(i1+1)
    return
  end function Lin_Interp_Do
end module Linear_Interpolation
