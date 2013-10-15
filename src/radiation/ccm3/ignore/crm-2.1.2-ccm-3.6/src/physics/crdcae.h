c
c $Id: crdcae.h,v 1.1 2004/09/07 15:27:41 rca Exp $
c $Author: rca $
c
C
C Water vapor narrow band constants for longwave radiation computations
C
      common/crdcae/realk(2), st(2), a1(2), a2(2), b1(2), b2(2),
     $              coefa(3,4),coefb(4,4),coefc(3,4),coefd(4,4),
     $              coefe(3,4),coeff(6,2),coefg(2,4),coefh(2,4),
     $              coefi(6,2),coefj(3,2),coefk(3,2),
     $              c1(4),c2(4),c3(4),c4(4),c5(4),c6(4),c7(4),c8,c9,
     $              c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,c21,
     $              c22,c23,c24,c25,c26,c27,c28,c29,c30,c31,
     $              fwcoef,fwc1,fwc2,fc1,cfa1
C
      real realk      ! H2O narrow band parameter
      real st         ! H2O narrow band parameter
      real a1,a2      ! Temperature correction terms for H2O path
      real b1,b2      ! Temperature correction terms for H2O path
C
C Constant coefficients for water vapor absorptivity and emissivity
C
      real coefa,coefb,coefc,coefd,coefe,coeff 
      real coefg,coefh,coefi,coefj,coefk 
      real c1, c2, c3, c4, c5, c6, c7,c8 ,c9 ,c10 
      real c11,c12,c13,c14,c15,c16,c17,c18,c19,c20 
      real c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,c31
C
C Farwing correction constants for narrow-band emissivity model,
C introduced to account for the deficiencies in narrow-band model
C used to derive the emissivity; tuned with Arking's line-by-line
C calculations.
C
      real fwcoef      ! Farwing correction constant
      real fwc1,fwc2   ! Farwing correction constants 
      real fc1         ! Farwing correction constant 
      real cfa1        ! Farwing correction constant 
C
 
