c
c $Id: comcmf.h,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c $Author: rca $
c
C
C Common block for moist convective mass flux procedure
C
      common/comcmf/cp      ,hlat    ,grav    ,c0      ,betamn  ,
     $              rhlat   ,rcp     ,rgrav   ,cmftau  ,rhoh2o  ,
     $              rgas    ,dzmin   ,limcnv  ,iloc    ,jloc    ,
     $              nsloc   ,tiny    ,eps     ,tpmax   ,shpmax  ,
     $              rlxclm
C
      real cp          ! specific heat of dry air
      real hlat        ! latent heat of vaporization
      real grav        ! gravitational constant       
      real c0          ! rain water autoconversion coefficient
      real betamn      ! minimum overshoot parameter
      real rhlat       ! reciprocal of hlat
      real rcp         ! reciprocal of cp
      real rgrav       ! reciprocal of grav
      real cmftau      ! characteristic adjustment time scale
      real rhoh2o      ! density of liquid water (STP)
      real rgas        ! gas constant for dry air
      real dzmin       ! minimum convective depth for precipitation
      real tiny        ! arbitrary small num used in transport estimates
      real eps         ! convergence criteria (machine dependent)
      real tpmax       ! maximum acceptable t perturbation (degrees C)
      real shpmax      ! maximum acceptable q perturbation (g/g)           
C
      integer iloc        ! longitude location for diagnostics
      integer jloc        ! latitude  location for diagnostics
      integer nsloc       ! nstep for which to produce diagnostics
      integer limcnv      ! top interface level limit for convection
C
      logical rlxclm      ! logical to relax column versus cloud triplet
C
 
