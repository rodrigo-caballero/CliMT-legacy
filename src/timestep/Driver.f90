subroutine leapfrog(km, jm, im, afc, xinc, x, xold, x_out, xold_out)

! computes a leapfrog time step with Asselin filter of strength afc

implicit none

! In
integer, intent(in)   :: km, jm, im
!f2py intent(hide) km, jm, im
real(8)               :: afc
real(8), intent(in), dimension(km,jm,im) :: xinc, x, xold

!Out 
real(8), intent(out), dimension(km,jm,im) :: x_out, xold_out

! Local
real, dimension(km,jm,im) :: xnew

! time step
xnew = xold + xinc

! Asselin filter
xold_out = x + afc*(xold + xnew - 2.*x)

! update
x_out = xnew

end subroutine leapfrog

!--------------------------------------------------------------------------
subroutine asselin(km, jm, im, afc, xnew, x, xold, x_out, xold_out)

! Updates variables using Asselin filter of strength afc

integer, intent(in)                      :: km, jm, im
!f2py intent(in,hide) km,jm,im
real(8), intent(in)                      :: afc
real(8), intent(in), dimension(km,jm,im) :: xnew, x, xold
real(8), intent(out), dimension(km,jm,im) :: x_out, xold_out

! Asselin filter
xold_out = x + afc*(xold + xnew - 2.*x)

! update
x_out = xnew

end subroutine asselin

