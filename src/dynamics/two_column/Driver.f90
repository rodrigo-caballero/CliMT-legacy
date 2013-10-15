subroutine driver(im,jm,km,dt,Rd,Rv,Cpd,Cpv,g,p_,z0_,u_,t_,q_,udot_,tdot_,qdot_,z0dot_,w_)   

implicit none

! In
integer, intent(in) :: im,jm,km
!f2py intent(in,hide) im,jm,km
real(8), intent(in) :: dt,Rd,Rv,Cpd,Cpv,g
real(8), intent(in), dimension(km,jm,im) :: p_,u_,t_,q_
real(8), intent(in), dimension(jm,im) :: z0_

! Out
real(8), intent(out), dimension(km,jm,im) :: udot_,tdot_,qdot_,w_
real(8), intent(out), dimension(jm,im) :: z0dot_

! Local
real(8), dimension(jm) :: z0,z0dot
real(8), dimension(jm,km) :: u,t,q,p
real(8), dimension(jm,km) :: udot,tdot,qdot
real(8), dimension(jm,km+1) :: w

p = transpose(p_(:,:,1)) * 100. ! mb -> Pa
u = transpose(u_(:,:,1))
t = transpose(t_(:,:,1))
q = transpose(q_(:,:,1)) * 1.e-3 ! g/kg -> kg/kg
z0 = z0_(:,1)

call tend(jm,km,Rd,Rv,Cpd,Cpv,g,p,u,t,q,z0,udot,tdot,qdot,z0dot,w)

udot_(:,:,1) = transpose(udot)*2.*dt
tdot_(:,:,1) = transpose(tdot)*2.*dt
qdot_(:,:,1) = transpose(qdot)*2.*dt *1.e3
z0dot_(:,1) = z0dot *2.*dt
w_(:,:,1) = transpose(w(:,1:km)+w(:,2:km+1))/2.

end subroutine driver
