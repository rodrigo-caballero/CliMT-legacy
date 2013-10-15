c
c $Id: compbl.h,v 1.1 2005/01/24 19:21:44 rca Exp $
c $Author: rca $
c
C
C Pbl constants
C
      common /compbl/ betam   ,betas   ,betah   ,fak     ,g       ,
     $                onet    ,fakn    ,ricr    ,sffrac  ,vk      ,
     $                ccon    ,binm    ,binh
C
      real betam   ! Constant in wind gradient expression
      real betas   ! Constant in surface layer gradient expression
      real betah   ! Constant in temperature gradient expression 
      real fak     ! Constant in surface temperature excess         
      real g       ! Gravitational acceleration
      real onet    ! 1/3 power in wind gradient expression
      real fakn    ! Constant in turbulent prandtl number
      real ricr    ! Critical richardson number
      real sffrac  ! Surface layer fraction of boundary layer
      real vk      ! Von Karman's constant
      real ccon    ! fak * sffrac * vk
      real binm    ! betam * sffrac
      real binh    ! betah * sffrac
C
 
