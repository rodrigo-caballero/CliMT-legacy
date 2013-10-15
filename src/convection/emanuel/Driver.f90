subroutine driver( km, jm, im,                                &
         afc, dt, UpdateFreq, p, ps, Cpd, Cpv, Cl, Rv, Rd, Lv0, g, rowl, & 
         elcrit, tlcrit, entp, sigd, sigs, omtrain, omtsnow,               &
         coeffr, coeffs, cu, beta, dtmax, alpha, damp,                    &
         t, q, theta, cbmf, tinc, qinc, tdot, qdot, precc )          

! In
integer, intent(in)                      :: km,jm,im
!f2py intent(in,hide) km,jm,im

real(8), intent(in)                      :: afc, Cpd, Cpv, Cl, Rv, Rd, Lv0, g, rowl,      &
                                            dt, UpdateFreq,                               &
                                            elcrit,tlcrit,entp,sigd,sigs,omtrain,omtsnow, &
                                            coeffr,coeffs,cu,beta,dtmax,alpha,damp
real(8), intent(in), dimension(jm,im)    :: ps
real(8), intent(in), dimension(km,jm,im) :: p

! Out
real(8), intent(out), dimension(jm,im)    :: precc
real(8), intent(out), dimension(km,jm,im) :: tdot, qdot, tinc, qinc, theta

! In/out
real(8), dimension(jm,im)    :: cbmf
real(8), dimension(km,jm,im) :: t, q
!f2py intent(in,out) t, q, cbmf

! Local
real(8), dimension(km,jm,im) :: told, qold

told=t
qold=q

do i=1,im
do j=1,jm
call main(km, dt, t(:,j,i), q(:,j,i), p(:,j,i), ps(j,i),    & 
     Cpd, Cpv, Cl, Rv, Rd, Lv0, g, rowl,              & 
     elcrit,tlcrit,entp,sigd,sigs,omtrain,omtsnow,     &
     coeffr,coeffs,cu,beta,dtmax,alpha,damp,          &
     tinc(:,j,i), qinc(:,j,i),                        & 
     tdot(:,j,i), qdot(:,j,i), precc(j,i), cbmf(j,i), theta(:,j,i))          
enddo
enddo

tdot = ( (t-told)*(1.-2.*afc)/2./UpdateFreq + tdot ) * 86400.
qdot = ( (q-qold)*(1.-2.*afc)/2./UpdateFreq + qdot ) * 86400.

end subroutine driver
