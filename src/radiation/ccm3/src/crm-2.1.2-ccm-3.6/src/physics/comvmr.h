C
C Molecular weights
C
      real mwdry      ! molecular weight dry air
      real mwco2      ! molecular weight co2
      real mwh2o      ! molecular weight h2o
      real mwn2o      ! molecular weight n2o
      real mwch4      ! molecular weight ch4
      real mwf11      ! molecular weight cfc11
      real mwf12      ! molecular weight cfc12

      parameter (mwdry  = 29.)
      parameter (mwco2  = 44.)
      parameter (mwh2o =  18.)
      parameter (mwn2o =  44.)
      parameter (mwch4 =  16.)
      parameter (mwf11 = 136.)
      parameter (mwf12 = 120.)
C
C Ratios of molecular weights
C
      real rmwn2o   ! ratio of molecular weight n2o   to dry air
      real rmwch4   ! ratio of molecular weight ch4   to dry air
      real rmwf11   ! ratio of molecular weight cfc11 to dry air
      real rmwf12   ! ratio of molecular weight cfc12 to dry air
      real rmwco2   ! ratio of molecular weight co2   to dry air 
      real rh2och4  ! ratio of molecular weight h2o   to ch4

      parameter (rmwco2 = mwco2/mwdry)
      parameter (rmwn2o = mwn2o/mwdry)
      parameter (rmwch4 = mwch4/mwdry)
      parameter (rmwf11 = mwf11/mwdry)
      parameter (rmwf12 = mwf12/mwdry)
      parameter (rh2och4= mwh2o/mwch4)
C
C Volume mixing ratios
C
      real co2vmr   ! co2   volume mixing ratio 
      real n2ovmr   ! n2o   volume mixing ratio 
      real ch4vmr   ! ch4   volume mixing ratio 
      real f11vmr   ! cfc11 volume mixing ratio 
      real f12vmr   ! cfc12 volume mixing ratio 
      
      common /commrat/ co2vmr, n2ovmr, ch4vmr, f11vmr, f12vmr
