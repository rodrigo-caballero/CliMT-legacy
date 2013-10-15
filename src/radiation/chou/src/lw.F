c*********************    CLIRAD-LW    ****************************
c**********************   May, 1999   *****************************

      subroutine irrad (pl,ta,wa,oa,ts,co2,
     *                  n2o,ch4,cfc11,cfc12,cfc22,emiss,
     *                  cldwater,cwc,taucl,reff,fcld,ict,icb,
     *                  taual,ssaal,asyal,
     *                  high,trace,flx,flc,dfdts,st4)
c+++climt
! disabled tau -- gives bus error with gfortran
!     *            high, trace, flx, flc, dfdts, st4, tau)

c***********************************************************************
c
c                        Version IR-7 (June, 1999)
c
c  The water vapor continuum absortion is included in the specral 
c   region 1215-1380 /cm.  Clough's absorption data are used.
c
c***********************************************************************
c
c                        Version IR-6 (April, 1999)
c
c  The co2 transmission functions are parameterized so that this code
c   can be applied to the case with a large range of co2 concentration
c   (up to 100 times of present value).  
c
c***********************************************************************
c
c                        Version IR-5 (September, 1998)
c
c  New features of this version are:
c   (1) The effect of aerosol scattering on LW fluxes is included.
c   (2) Absorption and scattering of rain drops are included.
c
c***********************************************************************
cc
c                        Version IR-4 (October, 1997)
c
c  New features of this version are:
c   (1) The surface is treated as non-black.  The surface
c         emissivity, emiss, is an input parameter
c   (2) The effect of cloud scattering on LW fluxes is included
c
c*********************************************************************
c
c This routine computes ir fluxes due to water vapor, co2, o3,
c   trace gases (n2o, ch4, cfc11, cfc12, cfc22, co2-minor),
c   clouds, and aerosols.
c  
c This is a vectorized code.  It computes fluxes simultaneously for
c   m soundings.
c
c Some detailed descriptions of the radiation routine are given in
c   Chou and Suarez (1994).
c
c Ice and liquid cloud particles are allowed to co-exist in any of the
c  np layers. 
c
c If no information is available for the effective cloud particle size,
c  reff, default values of 10 micron for liquid water and 75 micron
c  for ice can be used.
c
c The maximum-random assumption is applied for cloud overlapping.
c  clouds are grouped into high, middle, and low clouds separated by the
c  level indices ict and icb.  Within each of the three groups, clouds
c  are assumed maximally overlapped, and the cloud cover of a group is
c  the maximum cloud cover of all the layers in the group.  clouds among
c  the three groups are assumed randomly overlapped. The indices ict and
c  icb correpond approximately to the 400 mb and 700 mb levels.
c
c Aerosols are allowed to be in any of the np layers. Aerosol optical
c  properties can be specified as functions of height and spectral band.
c
c There are options for computing fluxes:
c
c   If cldwater=.true., taucl is computed from cwc and reff as a
c   function of height and spectral band. 
c   If cldwater=.false., taucl must be given as input to the radiation
c   routine. It is independent of spectral band.
c
c   If high = .true., transmission functions in the co2, o3, and the
c   three water vapor bands with strong absorption are computed using
c   table look-up.  cooling rates are computed accurately from the
c   surface up to 0.01 mb.
c   If high = .false., transmission functions are computed using the
c   k-distribution method with linear pressure scaling for all spectral
c   bands and gases.  cooling rates are not calculated accurately for
c   pressures less than 20 mb. the computation is faster with
c   high=.false. than with high=.true.

c   If trace = .true., absorption due to n2o, ch4, cfcs, and the 
c   two minor co2 bands in the window region is included.
c   If trace = .false., absorption in minor bands is neglected.
c
c The IR spectrum is divided into nine bands:
c   
c   band     wavenumber (/cm)   absorber
c
c    1           0 - 340           h2o
c    2         340 - 540           h2o
c    3         540 - 800       h2o,cont,co2,n2o
c    4         800 - 980       h2o,cont
c                              co2,f11,f12,f22
c    5         980 - 1100      h2o,cont,o3
c                              co2,f11
c    6        1100 - 1215      h2o,cont
c                              n2o,ch4,f12,f22
c    7        1215 - 1380          h2o
c                              n2o,ch4
c    8        1380 - 1900          h2o
c    9        1900 - 3000          h2o
c
c In addition, a narrow band in the 15 micrometer region is added to
c    compute flux reduction due to n2o
c
c    10        540 - 620       h2o,cont,co2,n2o
c
c Band 3 (540-800/cm) is further divided into 3 sub-bands :
c
c   subband   wavenumber (/cm)
c
c    1          540 - 620
c    2          620 - 720
c    3          720 - 800
c
c---- Input parameters                               units    size
c
c   number of soundings (m)                           n/d      1
c   number of atmospheric layers (np)                 n/d      1
c   level pressure (pl)                               mb      m*(np+1)
c   layer temperature (ta)                            k       m*np
c   layer specific humidity (wa)                      g/g     m*np
c   layer ozone mixing ratio by mass (oa)             g/g     m*np
c   surface temperature (ts)                          k        m
c   co2 mixing ratio by volumn (co2)                  pppv     1
c   n2o mixing ratio by volumn (n2o)                  pppv     1
c   ch4 mixing ratio by volumn (ch4)                  pppv     1
c   cfc11 mixing ratio by volumn (cfc11)              pppv     1
c   cfc12 mixing ratio by volumn (cfc12)              pppv     1
c   cfc22 mixing ratio by volumn (cfc22)              pppv     1
c   surface emissivity (emiss)                      fraction   m*10
c   input option for cloud optical thickness          n/d      1
c           cldwater="true" if cwc is provided
c           cldwater="false" if taucl is provided
c   cloud water mixing ratio (cwc)                   gm/gm   m*np*3
c           index 1 for ice particles
c           index 2 for liquid drops
c           index 3 for rain drops
c   cloud optical thickness (taucl)                   n/d    m*np*3
c           index 1 for ice particles
c           index 2 for liquid drops
c           index 3 for rain drops
c   effective cloud-particle size (reff)          micrometer m*np*3
c           index 1 for ice particles
c           index 2 for liquid drops
c           index 3 for rain drops
c   cloud amount (fcld)                             fraction  m*np
c   level index separating high and middle            n/d      1
c           clouds (ict)
c   level index separating middle and low             n/d      1
c           clouds (icb)
c   aerosol optical thickness (taual)                 n/d   m*np*10
c   aerosol single-scattering albedo (ssaal)          n/d   m*np*10
c   aerosol asymmetry factor (asyal)                  n/d   m*np*10
c   high (see explanation above)                               1
c   trace (see explanation above)                              1
c
c Data used in table look-up for transmittance calculations:
c
c   c1 , c2, c3: for co2 (band 3)
c   o1 , o2, o3: for  o3 (band 5)
c   h11,h12,h13: for h2o (band 1)
c   h21,h22,h23: for h2o (band 2)
c   h81,h82,h83: for h2o (band 8)
c
c---- output parameters
c
c   net downward flux, all-sky   (flx)             w/m**2  m*(np+1)
c   net downward flux, clear-sky (flc)             w/m**2  m*(np+1)
c   sensitivity of net downward flux  
c       to surface temperature (dfdts)            w/m**2/k m*(np+1)
c   emission by the surface (st4)                 w/m**2     m
c 
c Notes: 
c
c   (1) Water vapor continuum absorption is included in 540-1380 /cm.
c   (2) Scattering is parameterized for clouds and aerosols.
c   (3) Diffuse cloud and aerosol transmissions are computed
c       from exp(-1.66*tau).
c   (4) If there are no clouds, flx=flc.
c   (5) plevel(1) is the pressure at the top of the model atmosphere,
c        and plevel(np+1) is the surface pressure.
c   (6) Downward flux is positive and upward flux is negative.
c   (7) dfdts is negative because upward flux is defined as negative.
c   (8) For questions and coding errors, contact with Ming-Dah Chou,
c       Code 913, NASA/Goddard Space Flight Center, Greenbelt, MD 20771.
c       Phone: 301-286-4012, Fax: 301-286-1759,
c       e-mail: chou@climate.gsfc.nasa.gov
c
c-----parameters defining the size of the pre-computed tables for transmittance
c     calculations using table look-up.
c
c     "nx" is the number of intervals in pressure
c     "no" is the number of intervals in o3 amount
c     "nc" is the number of intervals in co2 amount
c     "nh" is the number of intervals in h2o amount
c***************************************************************************

c
      implicit none
      integer np,m
      parameter(m=IM, np=KM) 
c
c+++climt 
      ! broadband transmissivity and optical depth
      real t_band(m,np,10), tau_out(np)
      ! width of spectral bands
      real bandwidth(10)
      data bandwidth/340.,200.,260.,180.,120.,115.,165.,520.,1100.,0./
c---climt    

      integer nx,no,nc,nh
      integer i,k,ip,iw,ib,ik,iq,isb,k1,k2
      parameter (nx=26,no=21,nc=30,nh=31)

c---- input parameters ------

      integer ict,icb
      real pl(m,np+1),ta(m,np),wa(m,np),oa(m,np),
     *     ts(m)
      real co2,n2o,ch4,cfc11,cfc12,cfc22,emiss(m,10)
      real cwc(m,np,3),taucl(m,np,3),reff(m,np,3),
     *     fcld(m,np)
      real taual(m,np,10),ssaal(m,np,10),asyal(m,np,10)
      logical cldwater,high,trace

c---- output parameters ------

      real flx(m,np+1),flc(m,np+1),dfdts(m,np+1),st4(m)

c---- static data -----

      real cb(5,10)
      real xkw(9),aw(9),bw(9),pm(9),fkw(6,9),gkw(6,3),xke(9)
      real aib(3,10),awb(4,10),aiw(4,10),aww(4,10),aig(4,10),awg(4,10)
      integer ne(9),mw(9)

c---- temporary arrays -----

      real pa(m,np),dt(m,np)
      real sh2o(m,np+1),swpre(m,np+1),swtem(m,np+1)
      real sco3(m,np+1),scopre(m,np+1),scotem(m,np+1)
      real dh2o(m,np),dcont(m,np),dco2(m,np),do3(m,np)
      real dn2o(m,np),dch4(m,np)
      real df11(m,np),df12(m,np),df22(m,np)
      real th2o(m,6),tcon(m,3),tco2(m,6,2)
      real tn2o(m,4),tch4(m,4),tcom(m,6)
      real tf11(m),tf12(m),tf22(m)
      real h2oexp(m,np,6),conexp(m,np,3),co2exp(m,np,6,2)
      real n2oexp(m,np,4),ch4exp(m,np,4),comexp(m,np,6)
      real f11exp(m,np),  f12exp(m,np),  f22exp(m,np)
      real clr(m,0:np+1),fclr(m)
      real blayer(m,0:np+1),dlayer(m,np+1),dbs(m)
      real clrlw(m),clrmd(m),clrhi(m)
      real cwp(m,np,3)
      real trant(m),tranal(m),transfc(m,np+1),trantcr(m,np+1)
      real flxu(m,np+1),flxd(m,np+1),flcu(m,np+1),flcd(m,np+1)
      real rflx(m,np+1),rflc(m,np+1)

      logical oznbnd,co2bnd,h2otbl,conbnd,n2obnd
      logical ch4bnd,combnd,f11bnd,f12bnd,f22bnd,b10bnd

      real c1 (nx,nc),c2 (nx,nc),c3 (nx,nc)
      real o1 (nx,no),o2 (nx,no),o3 (nx,no)
      real h11(nx,nh),h12(nx,nh),h13(nx,nh)
      real h21(nx,nh),h22(nx,nh),h23(nx,nh)
      real h81(nx,nh),h82(nx,nh),h83(nx,nh)

      real dp,xx,p1,dwe,dpe,a1,b1,fk1,a2,b2,fk2
      real w1,w2,w3,g1,g2,g3,ww,gg,ff,taux,reff1,reff2

c-----the following coefficients (equivalent to table 2 of
c     chou and suarez, 1995) are for computing spectrally 
c     integrated planck fluxes using eq. (22)

       data cb/
     1      -2.6844e-1,-8.8994e-2, 1.5676e-3,-2.9349e-6, 2.2233e-9,
     2       3.7315e+1,-7.4758e-1, 4.6151e-3,-6.3260e-6, 3.5647e-9,
     3       3.7187e+1,-3.9085e-1,-6.1072e-4, 1.4534e-5,-1.6863e-8,
     4      -4.1928e+1, 1.0027e+0,-8.5789e-3, 2.9199e-5,-2.5654e-8,
     5      -4.9163e+1, 9.8457e-1,-7.0968e-3, 2.0478e-5,-1.5514e-8,
     6      -4.7107e+1, 8.9130e-1,-5.9735e-3, 1.5596e-5,-9.5911e-9,
     7      -5.4041e+1, 9.5332e-1,-5.7784e-3, 1.2555e-5,-2.9377e-9,
     8      -6.9233e+0,-1.5878e-1, 3.9160e-3,-2.4496e-5, 4.9301e-8,
     9       1.1483e+2,-2.2376e+0, 1.6394e-2,-5.3672e-5, 6.6456e-8,
     *       1.9668e+1,-3.1161e-1, 1.2886e-3, 3.6835e-7,-1.6212e-9/

c-----xkw are the absorption coefficients for the first
c     k-distribution function due to water vapor line absorption
c     (tables 4 and 7).  units are cm**2/g    

      data xkw / 29.55  , 4.167e-1, 1.328e-2, 5.250e-4,
     *           5.25e-4, 9.369e-3, 4.719e-2, 1.320e-0, 5.250e-4/

c-----xke are the absorption coefficients for the first
c     k-distribution function due to water vapor continuum absorption
c     (table 6).  units are cm**2/g

      data xke /  0.00,   0.00,   27.40,   15.8,
     *            9.40,   7.75,    8.78,    0.0,    0.0/

c-----mw are the ratios between neighboring absorption coefficients
c     for water vapor line absorption (tables 4 and 7).

      data mw /6,6,8,6,6,8,9,6,16/

c-----aw and bw (table 3) are the coefficients for temperature scaling
c     in eq. (25).

      data aw/ 0.0021, 0.0140, 0.0167, 0.0302,
     *         0.0307, 0.0195, 0.0152, 0.0008, 0.0096/
      data bw/ -1.01e-5, 5.57e-5, 8.54e-5, 2.96e-4,
     *          2.86e-4, 1.108e-4, 7.608e-5, -3.52e-6, 1.64e-5/

      data pm/ 1.0, 1.0, 1.0, 1.0, 1.0, 0.77, 0.5, 1.0, 1.0/

c-----fkw is the planck-weighted k-distribution function due to h2o
c     line absorption given in table 4 of Chou and Suarez (1995).
c     the k-distribution function for the third band, fkw(*,3), is not used

      data fkw / 0.2747,0.2717,0.2752,0.1177,0.0352,0.0255,
     2           0.1521,0.3974,0.1778,0.1826,0.0374,0.0527,
     3           6*1.00,
     4           0.4654,0.2991,0.1343,0.0646,0.0226,0.0140,
     5           0.5543,0.2723,0.1131,0.0443,0.0160,0.0000,
     6           0.5955,0.2693,0.0953,0.0335,0.0064,0.0000,
     7           0.1958,0.3469,0.3147,0.1013,0.0365,0.0048,
     8           0.0740,0.1636,0.4174,0.1783,0.1101,0.0566,
     9           0.1437,0.2197,0.3185,0.2351,0.0647,0.0183/

c-----gkw is the planck-weighted k-distribution function due to h2o
c     line absorption in the 3 subbands (800-720,620-720,540-620 /cm)
c     of band 3 given in table 7.  Note that the order of the sub-bands
c     is reversed.

      data gkw/  0.1782,0.0593,0.0215,0.0068,0.0022,0.0000,
     2           0.0923,0.1675,0.0923,0.0187,0.0178,0.0000,
     3           0.0000,0.1083,0.1581,0.0455,0.0274,0.0041/

c-----ne is the number of terms used in each band to compute water vapor
c     continuum transmittance (table 6).

      data ne /0,0,3,1,1,1,1,0,0/

c
c-----coefficients for computing the extinction coefficient
c     for cloud ice particles
c     polynomial fit: y=a1+a2/x**a3; x is in m**2/gm
c
      data aib /  -0.44171,    0.62951,   0.06465,
     2            -0.13727,    0.61291,   0.28962,
     3            -0.01878,    1.67680,   0.79080,
     4            -0.01896,    1.06510,   0.69493,
     5            -0.04788,    0.88178,   0.54492,
     6            -0.02265,    1.57390,   0.76161,
     7            -0.01038,    2.15640,   0.89045, 
     8            -0.00450,    2.51370,   0.95989,
     9            -0.00044,    3.15050,   1.03750,
     *            -0.02956,    1.44680,   0.71283/
c
c-----coefficients for computing the extinction coefficient
c     for cloud liquid drops
c     polynomial fit: y=a1+a2*x+a3*x**2+a4*x**3; x is in m**2/gm
c
      data awb /   0.08641,    0.01769,    -1.5572e-3,   3.4896e-5,
     2             0.22027,    0.00997,    -1.8719e-3,   5.3112e-5,
     3             0.39252,   -0.02817,     7.2931e-4,  -3.8151e-6,
     4             0.39975,   -0.03426,     1.2884e-3,  -1.7930e-5,
     5             0.34021,   -0.02805,     1.0654e-3,  -1.5443e-5,
     6             0.15587,    0.00371,    -7.7705e-4,   2.0547e-5,
     7             0.05518,    0.04544,    -4.2067e-3,   1.0184e-4,
     8             0.12724,    0.04751,    -5.2037e-3,   1.3711e-4,
     9             0.30390,    0.01656,    -3.5271e-3,   1.0828e-4,
     *             0.63617,   -0.06287,     2.2350e-3,  -2.3177e-5/
c
c-----coefficients for computing the single-scattering albedo
c     for cloud ice particles
c     polynomial fit: y=a1+a2*x+a3*x**2+a4*x**3; x is in m**2/gm
c
      data aiw/    0.17201,    1.2229e-2,  -1.4837e-4,   5.8020e-7,
     2             0.81470,   -2.7293e-3,   9.7816e-8,   5.7650e-8,
     3             0.54859,   -4.8273e-4,   5.4353e-6,  -1.5679e-8,
     4             0.39218,    4.1717e-3, - 4.8869e-5,   1.9144e-7,
     5             0.71773,   -3.3640e-3,   1.9713e-5,  -3.3189e-8,
     6             0.77345,   -5.5228e-3,   4.8379e-5,  -1.5151e-7,
     7             0.74975,   -5.6604e-3,   5.6475e-5,  -1.9664e-7,
     8             0.69011,   -4.5348e-3,   4.9322e-5,  -1.8255e-7,
     9             0.83963,   -6.7253e-3,   6.1900e-5,  -2.0862e-7,
     *             0.64860,   -2.8692e-3,   2.7656e-5,  -8.9680e-8/
c
c-----coefficients for computing the single-scattering albedo
c     for cloud liquid drops
c     polynomial fit: y=a1+a2*x+a3*x**2+a4*x**3; x is in m**2/gm
c
      data aww/   -7.8566e-2,  8.0875e-2,  -4.3403e-3,   8.1341e-5,
     2            -1.3384e-2,  9.3134e-2,  -6.0491e-3,   1.3059e-4,
     3             3.7096e-2,  7.3211e-2,  -4.4211e-3,   9.2448e-5,
     4            -3.7600e-3,  9.3344e-2,  -5.6561e-3,   1.1387e-4,
     5             0.40212,    7.8083e-2,  -5.9583e-3,   1.2883e-4,
     6             0.57928,    5.9094e-2,  -5.4425e-3,   1.2725e-4,
     7             0.68974,    4.2334e-2,  -4.9469e-3,   1.2863e-4,
     8             0.80122,    9.4578e-3,  -2.8508e-3,   9.0078e-5,
     9             1.02340,   -2.6204e-2,   4.2552e-4,   3.2160e-6,
     *             0.05092,    7.5409e-2,  -4.7305e-3,   1.0121e-4/ 
c
c-----coefficients for computing the asymmetry factor for cloud ice particles
c     polynomial fit: y=a1+a2*x+a3*x**2+a4*x**3; x is in m**2/gm
c
      data aig /   0.57867,    1.0135e-2,  -1.1142e-4,   4.1537e-7,
     2             0.72259,    3.1149e-3,  -1.9927e-5,   5.6024e-8,
     3             0.76109,    4.5449e-3,  -4.6199e-5,   1.6446e-7,
     4             0.86934,    2.7474e-3,  -3.1301e-5,   1.1959e-7,
     5             0.89103,    1.8513e-3,  -1.6551e-5,   5.5193e-8,
     6             0.86325,    2.1408e-3,  -1.6846e-5,   4.9473e-8,
     7             0.85064,    2.5028e-3,  -2.0812e-5,   6.3427e-8,
     8             0.86945,    2.4615e-3,  -2.3882e-5,   8.2431e-8,
     9             0.80122,    3.1906e-3,  -2.4856e-5,   7.2411e-8,
     *             0.73290,    4.8034e-3,  -4.4425e-5,   1.4839e-7/
c
c-----coefficients for computing the asymmetry factor for cloud liquid drops
c     polynomial fit: y=a1+a2*x+a3*x**2+a4*x**3; x is in m**2/gm
c
      data awg /  -0.51930,    0.20290,    -1.1747e-2,   2.3868e-4,
     2            -0.22151,    0.19708,    -1.2462e-2,   2.6646e-4,
     3             0.14157,    0.14705,    -9.5802e-3,   2.0819e-4,
     4             0.41590,    0.10482,    -6.9118e-3,   1.5115e-4,
     5             0.55338,    7.7016e-2,  -5.2218e-3,   1.1587e-4,
     6             0.61384,    6.4402e-2,  -4.6241e-3,   1.0746e-4,
     7             0.67891,    4.8698e-2,  -3.7021e-3,   9.1966e-5,
     8             0.78169,    2.0803e-2,  -1.4749e-3,   3.9362e-5,
     9             0.93218,   -3.3425e-2,   2.9632e-3,  -6.9362e-5,
     *             0.01649,    0.16561,    -1.0723e-2,   2.3220e-4/ 
c
c-----include tables used in the table look-up for co2 (band 3), 
c     o3 (band 5), and h2o (bands 1, 2, and 7) transmission functions.
c     "co2.tran4" is the new co2 transmission table applicable to a large
c     range of co2 amount (up to 200 times of the present-time value).

      logical first
      data first /.true./

      include "h2o.tran3"
      include "co2.tran4"
      include "o3.tran3"

      if (first) then

c-----tables co2 and h2o are only used with 'high' option

       if (high) then

        do iw=1,nh
         do ip=1,nx
          h11(ip,iw)=1.0-h11(ip,iw)
          h21(ip,iw)=1.0-h21(ip,iw)
          h81(ip,iw)=1.0-h81(ip,iw)
         enddo
        enddo

        do iw=1,nc
         do ip=1,nx
          c1(ip,iw)=1.0-c1(ip,iw)
         enddo
        enddo

       endif

c-----always use table look-up for ozone transmittance

        do iw=1,no
         do ip=1,nx
          o1(ip,iw)=1.0-o1(ip,iw)
         enddo
        enddo

       first=.false.

      endif

c-----compute layer pressure (pa) and layer temperature minus 250K (dt)

      do k=1,np
       do i=1,m
         pa(i,k)=0.5*(pl(i,k)+pl(i,k+1))
         dt(i,k)=ta(i,k)-250.0
       enddo
      enddo

c-----compute layer absorber amount

c     dh2o : water vapor amount (g/cm**2)
c     dcont: scaled water vapor amount for continuum absorption
c            (g/cm**2)
c     dco2 : co2 amount (cm-atm)stp
c     do3  : o3 amount (cm-atm)stp
c     dn2o : n2o amount (cm-atm)stp
c     dch4 : ch4 amount (cm-atm)stp
c     df11 : cfc11 amount (cm-atm)stp
c     df12 : cfc12 amount (cm-atm)stp
c     df22 : cfc22 amount (cm-atm)stp
c     the factor 1.02 is equal to 1000/980
c     factors 789 and 476 are for unit conversion
c     the factor 0.001618 is equal to 1.02/(.622*1013.25) 
c     the factor 6.081 is equal to 1800/296


      do k=1,np
       do i=1,m

         dp           = pl(i,k+1)-pl(i,k)
         dh2o (i,k) = 1.02*wa(i,k)*dp+1.e-10
         do3  (i,k) = 476.*oa(i,k)*dp+1.e-10
         dco2 (i,k) = 789.*co2*dp+1.e-10
         dch4 (i,k) = 789.*ch4*dp+1.e-10
         dn2o (i,k) = 789.*n2o*dp+1.e-10
         df11 (i,k) = 789.*cfc11*dp+1.e-10
         df12 (i,k) = 789.*cfc12*dp+1.e-10
         df22 (i,k) = 789.*cfc22*dp+1.e-10

c-----compute scaled water vapor amount for h2o continuum absorption
c     following eq. (43).

         xx=pa(i,k)*0.001618*wa(i,k)*wa(i,k)*dp
         dcont(i,k) = xx*exp(1800./ta(i,k)-6.081)+1.e-10

       enddo
      enddo

c-----compute column-integrated h2o amoumt, h2o-weighted pressure
c     and temperature.  it follows eqs. (37) and (38).

       if (high) then
        call column(m,np,pa,dt,dh2o,sh2o,swpre,swtem)
       endif

c-----compute layer cloud water amount (gm/m**2)
c     index is 1 for ice, 2 for waterdrops and 3 for raindrops.

       if (cldwater) then
        do k=1,np
         do i=1,m
c+++rca: input is already cwp in g/m2
c             xx=1.02*10000.*(pl(i,k+1)-pl(i,k))
c             cwp(i,k,1)=xx*cwc(i,k,1)
c             cwp(i,k,2)=xx*cwc(i,k,2)
c             cwp(i,k,3)=xx*cwc(i,k,3)
             cwp(i,k,1)=cwc(i,k,1)
             cwp(i,k,2)=cwc(i,k,2)
             cwp(i,k,3)=cwc(i,k,3)
c---rca
         enddo
        enddo
       endif

c-----the surface (np+1) is treated as a layer filled with black clouds.
c     clr is the equivalent clear fraction of a layer
c     transfc is the transmttance between the surface and a pressure level.
c     trantcr is the clear-sky transmttance between the surface and a
c     pressure level.
 
      do i=1,m
        clr(i,0)    = 1.0
        clr(i,np+1) = 0.0
        st4(i)      = 0.0
        transfc(i,np+1)=1.0
        trantcr(i,np+1)=1.0
      enddo

c-----initialize fluxes

      do k=1,np+1
       do i=1,m
         flx(i,k)  = 0.0
         flc(i,k)  = 0.0
         dfdts(i,k)= 0.0
         rflx(i,k) = 0.0
         rflc(i,k) = 0.0
       enddo
      enddo

c-----integration over spectral bands

      do 1000 ib=1,10

c-----if h2otbl, compute h2o (line) transmittance using table look-up.
c     if conbnd, compute h2o (continuum) transmittance in bands 3-7.
c     if co2bnd, compute co2 transmittance in band 3.
c     if oznbnd, compute  o3 transmittance in band 5.
c     if n2obnd, compute n2o transmittance in bands 6 and 7.
c     if ch4bnd, compute ch4 transmittance in bands 6 and 7.
c     if combnd, compute co2-minor transmittance in bands 4 and 5.
c     if f11bnd, compute cfc11 transmittance in bands 4 and 5.
c     if f12bnd, compute cfc12 transmittance in bands 4 and 6.
c     if f22bnd, compute cfc22 transmittance in bands 4 and 6.
c     if b10bnd, compute flux reduction due to n2o in band 10.

       h2otbl=high.and.(ib.eq.1.or.ib.eq.2.or.ib.eq.8)
       conbnd=ib.ge.3.and.ib.le.7
       co2bnd=ib.eq.3
       oznbnd=ib.eq.5
       n2obnd=ib.eq.6.or.ib.eq.7
       ch4bnd=ib.eq.6.or.ib.eq.7
       combnd=ib.eq.4.or.ib.eq.5
       f11bnd=ib.eq.4.or.ib.eq.5
       f12bnd=ib.eq.4.or.ib.eq.6
       f22bnd=ib.eq.4.or.ib.eq.6
       b10bnd=ib.eq.10

c-----blayer is the spectrally integrated planck flux of the mean layer
c     temperature derived from eq. (22)
c     the fitting for the planck flux is valid in the range 160-345 K.

       do k=1,np
        do i=1,m
          blayer(i,k)=ta(i,k)*( ta(i,k)*
     *              ( ta(i,k)*(ta(i,k)*cb(5,ib)+cb(4,ib))+cb(3,ib) )
     *                 +cb(2,ib) ) + cb(1,ib)
        enddo
       enddo

c-----the earth's surface, with an index "np+1", is treated as a layer

       do i=1,m
         blayer(i,0)   = 0.0
         blayer(i,np+1)= (ts(i)*(ts(i)*(ts(i)
     *                    *(ts(i)*cb(5,ib)+cb(4,ib))+cb(3,ib))
     *                    +cb(2,ib))+cb(1,ib) )*emiss(i,ib)

c-----dbs is the derivative of the surface emission with respect to
c     surface temperature (eq. 59).

         dbs(i)=(ts(i)*(ts(i)*(ts(i)*4.*cb(5,ib)
     *           +3.*cb(4,ib))+2.*cb(3,ib))+cb(2,ib))*emiss(i,ib)

       enddo

       do k=1,np+1
        do i=1,m
           dlayer(i,k)=blayer(i,k-1)-blayer(i,k)
        enddo
       enddo

c-----compute column-integrated absorber amoumt, absorber-weighted
c     pressure and temperature for co2 (band 3) and o3 (band 5).
c     it follows eqs. (37) and (38).

c-----this is in the band loop to save storage

      if (high .and. co2bnd) then
        call column(m,np,pa,dt,dco2,sco3,scopre,scotem)
      endif

      if (oznbnd) then
        call column(m,np,pa,dt,do3,sco3,scopre,scotem)
      endif

c-----compute cloud optical thickness
c     Rain optical thickness is 0.00307 /(gm/m**2).
c     It is for a specific drop size distribution provided by Q. Fu.

      if (cldwater) then
       do k= 1, np
        do i= 1, m
          taucl(i,k,1)=cwp(i,k,1)*(aib(1,ib)+aib(2,ib)/
     *      reff(i,k,1)**aib(3,ib))
          taucl(i,k,2)=cwp(i,k,2)*(awb(1,ib)+(awb(2,ib)+(awb(3,ib)
     *      +awb(4,ib)*reff(i,k,2))*reff(i,k,2))*reff(i,k,2))
          taucl(i,k,3)=0.00307*cwp(i,k,3)
        enddo
       enddo
      endif

c-----compute cloud single-scattering albedo and asymmetry factor for
c     a mixture of ice particles, liquid drops, and rain drops
c     single-scattering albedo and asymmetry factor of rain are set
c     to 0.54 and 0.95, respectively, based on the information provided
c     by Prof. Qiang Fu.


       do k= 1, np
        do i= 1, m

           clr(i,k)    = 1.0
           taux=taucl(i,k,1)+taucl(i,k,2)+taucl(i,k,3)

          if (taux.gt.0.02 .and. fcld(i,k).gt.0.01) then

           reff1=min(reff(i,k,1),130.)
           reff2=min(reff(i,k,2),20.0)

           w1=taucl(i,k,1)*(aiw(1,ib)+(aiw(2,ib)+(aiw(3,ib)
     *       +aiw(4,ib)*reff1)*reff1)*reff1)
           w2=taucl(i,k,2)*(aww(1,ib)+(aww(2,ib)+(aww(3,ib)
     *       +aww(4,ib)*reff2)*reff2)*reff2)
           w3=taucl(i,k,3)*0.54
           ww=(w1+w2+w3)/taux

           g1=w1*(aig(1,ib)+(aig(2,ib)+(aig(3,ib)
     *      +aig(4,ib)*reff1)*reff1)*reff1)
           g2=w2*(awg(1,ib)+(awg(2,ib)+(awg(3,ib)
     *      +awg(4,ib)*reff2)*reff2)*reff2)
           g3=w3*0.95

           gg=(g1+g2+g3)/(w1+w2+w3)

c-----parameterization of LW scattering
c     compute forward-scattered fraction and scale optical thickness

           ff=0.5+(0.3739+(0.0076+0.1185*gg)*gg)*gg
           taux=taux*(1.-ww*ff)

c-----compute equivalent cloud-free fraction, clr, for each layer
c     the cloud diffuse transmittance is approximated by using 
c     a diffusivity factor of 1.66.

           clr(i,k)=1.0-(fcld(i,k)*(1.0-exp(-1.66*taux)))
          endif

        enddo
       enddo

c-----compute the exponential terms (eq. 32) at each layer due to
c     water vapor line absorption when k-distribution is used

      if (.not.h2otbl .and. .not.b10bnd) then
        call h2oexps(ib,m,np,dh2o,pa,dt,xkw,aw,bw,pm,mw,h2oexp)
      endif

c-----compute the exponential terms (eq. 46) at each layer due to
c     water vapor continuum absorption

       do k= 1, np
        do i= 1, m
          conexp(i,k,1)=1.0
          conexp(i,k,2)=1.0
          conexp(i,k,3)=1.0
        enddo
       enddo


      if (conbnd) then
        call conexps(ib,m,np,dcont,xke,conexp)
      endif

c-----compute the exponential terms (eq. 32) at each layer due to
c     co2 absorption

      if (.not.high .and. co2bnd) then
        call co2exps(m,np,dco2,pa,dt,co2exp)
      endif

c***** for trace gases *****

      if (trace) then

c-----compute the exponential terms at each layer due to n2o absorption

       if (n2obnd) then
        call n2oexps(ib,m,np,dn2o,pa,dt,n2oexp)
       endif

c-----compute the exponential terms at each layer due to ch4 absorption

       if (ch4bnd) then
        call ch4exps(ib,m,np,dch4,pa,dt,ch4exp)
       endif

c-----compute the exponential terms due to co2 minor absorption

       if (combnd) then
        call comexps(ib,m,np,dco2,dt,comexp)
       endif

c-----compute the exponential terms due to cfc11 absorption

       if (f11bnd) then
            a1  = 1.26610e-3
            b1  = 3.55940e-6
            fk1 = 1.89736e+1
            a2  = 8.19370e-4
            b2  = 4.67810e-6
            fk2 = 1.01487e+1
        call cfcexps(ib,m,np,a1,b1,fk1,a2,b2,fk2,df11,dt,f11exp)
       endif

c-----compute the exponential terms due to cfc12 absorption

       if (f12bnd) then
            a1  = 8.77370e-4
            b1  =-5.88440e-6
            fk1 = 1.58104e+1
            a2  = 8.62000e-4
            b2  =-4.22500e-6
            fk2 = 3.70107e+1
        call cfcexps(ib,m,np,a1,b1,fk1,a2,b2,fk2,df12,dt,f12exp)
       endif

c-----compute the exponential terms due to cfc22 absorption

       if (f22bnd) then
            a1  = 9.65130e-4
            b1  = 1.31280e-5
            fk1 = 6.18536e+0
            a2  =-3.00010e-5 
            b2  = 5.25010e-7
            fk2 = 3.27912e+1
        call cfcexps(ib,m,np,a1,b1,fk1,a2,b2,fk2,df22,dt,f22exp)
       endif

c-----compute the exponential terms at each layer in band 10 due to
c     h2o line and continuum, co2, and n2o absorption

       if (b10bnd) then
        call b10exps(m,np,dh2o,dcont,dco2,dn2o,pa,dt
     *              ,h2oexp,conexp,co2exp,n2oexp)
       endif

      endif

c-----compute transmittances for regions between levels k1 and k2
c     and update the fluxes at the two levels.


c-----initialize fluxes

      do k=1,np+1
       do i=1,m
         flxu(i,k) = 0.0
         flxd(i,k) = 0.0
         flcu(i,k) = 0.0
         flcd(i,k) = 0.0
       enddo
      enddo


      do 2000 k1=1,np

c-----initialize fclr, th2o, tcon, tco2, and tranal
c     fclr is the clear fraction of the line-of-sight
c     clrlw, clrmd, and clrhi are the clear-sky fractions of the 
c      low, middle, and high troposphere, respectively
c     tranal is the aerosol transmission function

        do i=1,m
          fclr(i)  = 1.0
          clrlw(i) = 1.0
          clrmd(i) = 1.0
          clrhi(i) = 1.0
          tranal(i)= 1.0
        enddo

c-----for h2o line transmission

      if (.not. h2otbl) then
        do ik=1,6
         do i=1,m
           th2o(i,ik)=1.0
         enddo
        enddo
      endif

c-----for h2o continuum transmission

         do iq=1,3
          do i=1,m
            tcon(i,iq)=1.0
          enddo
         enddo

c-----for co2 transmission using k-distribution method.
c     band 3 is divided into 3 sub-bands, but sub-bands 3a and 3c
c     are combined in computing the co2 transmittance.

       if (.not.high .and. co2bnd) then
         do isb=1,2
          do ik=1,6
           do i=1,m
             tco2(i,ik,isb)=1.0
           enddo
          enddo
         enddo
       endif

c***** for trace gases *****

      if (trace) then

c-----for n2o transmission using k-distribution method.

       if (n2obnd) then
          do ik=1,4
           do i=1,m
             tn2o(i,ik)=1.0
           enddo
          enddo
       endif

c-----for ch4 transmission using k-distribution method.

       if (ch4bnd) then
          do ik=1,4
           do i=1,m
             tch4(i,ik)=1.0
           enddo
          enddo
       endif

c-----for co2-minor transmission using k-distribution method.

       if (combnd) then
          do ik=1,6
           do i=1,m
             tcom(i,ik)=1.0
           enddo
          enddo
       endif

c-----for cfc-11 transmission using k-distribution method.

       if (f11bnd) then
           do i=1,m
             tf11(i)=1.0
           enddo
       endif

c-----for cfc-12 transmission using k-distribution method.

       if (f12bnd) then
           do i=1,m
             tf12(i)=1.0
           enddo
       endif

c-----for cfc-22 transmission when using k-distribution method.

       if (f22bnd) then
           do i=1,m
             tf22(i)=1.0
           enddo
       endif

c-----for the transmission in band 10 using k-distribution method.

       if (b10bnd) then
          do ik=1,6
           do i=1,m
              th2o(i,ik)=1.0
              tco2(i,ik,1)=1.0
           enddo
          enddo
          do iq=1,3
           do i=1,m
              tcon(i,iq)=1.0
           enddo
          enddo
          do ik=1,4
            do i=1,m
              tn2o(i,ik)=1.0
           enddo
          enddo
       endif

      endif

c-----loop over the bottom level of the region (k2)

      do 3000 k2=k1+1,np+1

c-----initialize total transmission function (trant)

          do i=1,m
            trant(i)=1.0
          enddo

      if (h2otbl) then
          w1=-8.0
          p1=-2.0
          dwe=0.3
          dpe=0.2

c-----compute water vapor transmittance using table look-up

          if (ib.eq.1) then
           call tablup(k1,k2,m,np,nx,nh,sh2o,swpre,swtem,
     *                 w1,p1,dwe,dpe,h11,h12,h13,trant)

          endif
          if (ib.eq.2) then
           call tablup(k1,k2,m,np,nx,nh,sh2o,swpre,swtem,
     *                 w1,p1,dwe,dpe,h21,h22,h23,trant)

          endif
          if (ib.eq.8) then
           call tablup(k1,k2,m,np,nx,nh,sh2o,swpre,swtem,
     *                 w1,p1,dwe,dpe,h81,h82,h83,trant)
          endif

      else

c-----compute water vapor transmittance using k-distribution

       if (.not.b10bnd) then
        call h2okdis(ib,m,np,k2-1,fkw,gkw,ne,h2oexp,conexp,
     *               th2o,tcon,trant)
       endif

      endif

      if (co2bnd) then

        if (high) then

c-----compute co2 transmittance using table look-up method

          w1=-4.0
          p1=-2.0
          dwe=0.3
          dpe=0.2
          call tablup(k1,k2,m,np,nx,nc,sco3,scopre,scotem,
     *                w1,p1,dwe,dpe,c1,c2,c3,trant)
       else

c-----compute co2 transmittance using k-distribution method
          call co2kdis(m,np,k2-1,co2exp,tco2,trant)

        endif

      endif

c-----All use table look-up to compute o3 transmittance.

      if (oznbnd) then
          w1=-6.0
          p1=-2.0
          dwe=0.3
          dpe=0.2
          call tablup(k1,k2,m,np,nx,no,sco3,scopre,scotem,
     *                w1,p1,dwe,dpe,o1,o2,o3,trant)
      endif

c***** for trace gases *****

      if (trace) then

c-----compute n2o transmittance using k-distribution method

       if (n2obnd) then
          call n2okdis(ib,m,np,k2-1,n2oexp,tn2o,trant)
       endif

c-----compute ch4 transmittance using k-distribution method

       if (ch4bnd) then
          call ch4kdis(ib,m,np,k2-1,ch4exp,tch4,trant)
       endif

c-----compute co2-minor transmittance using k-distribution method

       if (combnd) then

          call comkdis(ib,m,np,k2-1,comexp,tcom,trant)

       endif

c-----compute cfc11 transmittance using k-distribution method

       if (f11bnd) then
          call cfckdis(m,np,k2-1,f11exp,tf11,trant)
       endif

c-----compute cfc12 transmittance using k-distribution method

       if (f12bnd) then
          call cfckdis(m,np,k2-1,f12exp,tf12,trant)
       endif

c-----compute cfc22 transmittance using k-distribution method

       if (f22bnd) then
          call cfckdis(m,np,k2-1,f22exp,tf22,trant)
       endif

c-----compute transmittance in band 10 using k-distribution method
c     here, trant is the change in transmittance due to n2o absorption

       if (b10bnd) then
          call b10kdis(m,np,k2-1,h2oexp,conexp,co2exp,n2oexp
     *                ,th2o,tcon,tco2,tn2o,trant)
       endif

      endif
c
c-----include aerosol effect
c     the aerosol scattering effect is included by scaling the
c     optical thickness
c
      do i=1,m
         ff=0.5+(0.3739+(0.0076+0.1185*asyal(i,k2-1,ib))
     *      *asyal(i,k2-1,ib))*asyal(i,k2-1,ib)
         taux=taual(i,k2-1,ib)*(1.-ssaal(i,k2-1,ib)*ff)
         tranal(i)=tranal(i)*exp(-1.66*taux)
         trant (i)=trant(i) *tranal(i)
      enddo

c-----Identify the clear-sky fractions clrhi, clrmd and clrlw, in the
c     high, middle and low cloud groups.
c     fclr is the clear-sky fraction between levels k1 and k2 assuming
c     the three cloud groups are randomly overlapped.

      do i=1,m
        if( k2 .le. ict ) then
            clrhi(i)=min(clr(i,k2-1),clrhi(i))
        elseif( k2 .gt. ict .and. k2 .le. icb ) then
            clrmd(i)=min(clr(i,k2-1),clrmd(i))
        elseif( k2 .gt. icb ) then
            clrlw(i)=min(clr(i,k2-1),clrlw(i))
        endif
            fclr(i)=clrlw(i)*clrmd(i)*clrhi(i)

      enddo

c-----compute upward and downward fluxes (band 1-9).
c     add "boundary" terms to the net downward flux.
c     these are the first terms on the right-hand-side of
c     eqs. (56a) and (56b).  downward fluxes are positive.

      if (.not. b10bnd) then

       if (k2 .eq. k1+1) then

        do i=1,m

c-----compute upward and downward fluxes for clear-sky situation
c     contribution from the "boundary term"

          flcu(i,k1)=flcu(i,k1)-blayer(i,k1)
          flcd(i,k2)=flcd(i,k2)+blayer(i,k1)

c-----compute upward and downward fluxes for all-sky situation

          flxu(i,k1)=flxu(i,k1)-blayer(i,k1)
          flxd(i,k2)=flxd(i,k2)+blayer(i,k1)

        enddo

       endif

c-----add flux components involving the four layers above and below
c     the levels k1 and k2.  it follows eqs. (56a) and (56b).

       do i=1,m
         xx=trant(i)*dlayer(i,k2)
         flcu(i,k1) =flcu(i,k1)+xx
         flxu(i,k1) =flxu(i,k1)+xx*fclr(i)
         xx=trant(i)*dlayer(i,k1)
         flcd(i,k2) =flcd(i,k2)+xx
         flxd(i,k2) =flxd(i,k2)+xx*fclr(i)
       enddo

      else

c-----flux reduction due to n2o in band 10

       if (trace) then

        do i=1,m
           rflx(i,k1) = rflx(i,k1)+trant(i)*fclr(i)
     *                   *dlayer(i,k2)
           rflx(i,k2) = rflx(i,k2)+trant(i)*fclr(i)
     *                   *dlayer(i,k1)
           rflc(i,k1) = rflc(i,k1)+trant(i)
     *                   *dlayer(i,k2)
           rflc(i,k2) = rflc(i,k2)+trant(i)
     *                   *dlayer(i,k1)
        enddo

       endif

      endif

 3000 continue


c-----transmission between level k1 and the surface

         do i=1,m
           trantcr(i,k1) =trant(i)
           transfc(i,k1) =trant(i)*fclr(i)
c+++climt
           t_band(i,k1,ib) = trant(i)
c---climt
         enddo

c-----compute the partial derivative of fluxes with respect to
c     surface temperature (eq. 59)

      if (trace .or. (.not. b10bnd) ) then

       do i=1,m
         dfdts(i,k1) =dfdts(i,k1)-dbs(i)*transfc(i,k1)
       enddo

      endif

 2000 continue

      if (.not. b10bnd) then

c-----add contribution from the surface to the flux terms at the surface
c     the surface emissivity effect is included in blayer(i,np+1)

        do i=1,m
          flcu(i,np+1)=flcu(i,np+1)-blayer(i,np+1)
          flxu(i,np+1)=flxu(i,np+1)-blayer(i,np+1)
          st4(i)=st4(i)-blayer(i,np+1)
          dfdts(i,np+1)=dfdts(i,np+1)-dbs(i)
        enddo

c-----add the flux component reflected by the surface
c     note: upward flux is negative

        do k=1, np+1
         do i=1,m
           flcu(i,k)=flcu(i,k)-
     *           flcd(i,np+1)*trantcr(i,k)*(1.-emiss(i,ib))
           flxu(i,k)=flxu(i,k)-
     *           flxd(i,np+1)*transfc(i,k)*(1.-emiss(i,ib))
         enddo
        enddo

      endif

c-----summation of fluxes over spectral bands
      do k=1,np+1
       do i=1,m
         flc(i,k)=flc(i,k)+flcd(i,k)+flcu(i,k)
         flx(i,k)=flx(i,k)+flxd(i,k)+flxu(i,k)
       enddo
      enddo

 1000 continue

c-----adjust fluxes due to n2o absorption in band 10

      do k=1,np+1
       do i=1,m
         flc(i,k)=flc(i,k)+rflc(i,k)
         flx(i,k)=flx(i,k)+rflx(i,k)
       enddo
      enddo

c+++climt
C-------       do i=1,m
C-------          do k=1,np
C-------             do ib=1,9
C-------                tau(k) = tau(k) + t_band(i,k,ib)*bandwidth(ib)
C-------             enddo
C-------             tau(k) = -log(tau(k)/3000.)
C-------          enddo
C-------       enddo

      return
      end
c***********************************************************************
      subroutine column (m,np,pa,dt,sabs0,sabs,spre,stem)
c***********************************************************************
c-----compute column-integrated (from top of the model atmosphere)
c     absorber amount (sabs), absorber-weighted pressure (spre) and
c     temperature (stem).
c     computations of spre and stem follows eqs. (37) and (38).
c
c--- input parameters
c   number of soundings (m)
c   number of atmospheric layers (np)
c   layer pressure (pa)
c   layer temperature minus 250K (dt)
c   layer absorber amount (sabs0)
c
c--- output parameters
c   column-integrated absorber amount (sabs)
c   column absorber-weighted pressure (spre)
c   column absorber-weighted temperature (stem)
c
c--- units of pa and dt are mb and k, respectively.
c    units of sabs are g/cm**2 for water vapor and (cm-atm)stp
c    for co2 and o3
c***********************************************************************
      implicit none
      integer m,np,i,k

c---- input parameters -----

      real pa(m,np),dt(m,np),sabs0(m,np)

c---- output parameters -----

      real sabs(m,np+1),spre(m,np+1),stem(m,np+1)

c*********************************************************************
        do i=1,m
          sabs(i,1)=0.0
          spre(i,1)=0.0
          stem(i,1)=0.0
        enddo

        do k=1,np
         do i=1,m
           sabs(i,k+1)=sabs(i,k)+sabs0(i,k)
           spre(i,k+1)=spre(i,k)+pa(i,k)*sabs0(i,k)
           stem(i,k+1)=stem(i,k)+dt(i,k)*sabs0(i,k)
         enddo
        enddo

       return
       end
c**********************************************************************
      subroutine h2oexps(ib,m,np,dh2o,pa,dt,xkw,aw,bw,pm,mw,h2oexp)
c**********************************************************************
c   compute exponentials for water vapor line absorption
c   in individual layers.
c
c---- input parameters
c  spectral band (ib)
c  number of grid intervals (m)
c  number of layers (np)
c  layer water vapor amount for line absorption (dh2o) 
c  layer pressure (pa)
c  layer temperature minus 250K (dt)
c  absorption coefficients for the first k-distribution
c     function due to h2o line absorption (xkw)
c  coefficients for the temperature and pressure scaling (aw,bw,pm)
c  ratios between neighboring absorption coefficients for
c     h2o line absorption (mw)
c
c---- output parameters
c  6 exponentials for each layer  (h2oexp)
c**********************************************************************
      implicit none
      integer ib,m,np,i,k,ik

c---- input parameters ------

      real dh2o(m,np),pa(m,np),dt(m,np)

c---- output parameters -----

      real h2oexp(m,np,6)

c---- static data -----

      integer mw(9)
      real xkw(9),aw(9),bw(9),pm(9)

c---- temporary arrays -----

      real xh,xh1

c**********************************************************************
c    note that the 3 sub-bands in band 3 use the same set of xkw, aw,
c    and bw,  therefore, h2oexp for these sub-bands are identical.
c**********************************************************************

        do k=1,np
         do i=1,m

c-----xh is the scaled water vapor amount for line absorption
c     computed from (27)

           xh = dh2o(i,k)*(pa(i,k)/500.)**pm(ib)
     1        * ( 1.+(aw(ib)+bw(ib)* dt(i,k))*dt(i,k) )

c-----h2oexp is the water vapor transmittance of the layer k
c     due to line absorption

           h2oexp(i,k,1) = exp(-xh*xkw(ib))

         enddo
        enddo

        do ik=2,6

         if (mw(ib).eq.6) then

          do k=1,np
           do i=1,m
             xh = h2oexp(i,k,ik-1)*h2oexp(i,k,ik-1)
             h2oexp(i,k,ik) = xh*xh*xh
           enddo
          enddo

        elseif (mw(ib).eq.8) then

          do k=1,np
           do i=1,m
             xh = h2oexp(i,k,ik-1)*h2oexp(i,k,ik-1)
             xh = xh*xh
             h2oexp(i,k,ik) = xh*xh
           enddo
          enddo

        elseif (mw(ib).eq.9) then

          do k=1,np
           do i=1,m
             xh=h2oexp(i,k,ik-1)*h2oexp(i,k,ik-1)*h2oexp(i,k,ik-1)
             xh1 = xh*xh
             h2oexp(i,k,ik) = xh*xh1
           enddo
          enddo

        else

          do k=1,np
           do i=1,m
             xh = h2oexp(i,k,ik-1)*h2oexp(i,k,ik-1)
             xh = xh*xh
             xh = xh*xh
             h2oexp(i,k,ik) = xh*xh
           enddo
          enddo

        endif
       enddo

      return
      end
c**********************************************************************
      subroutine conexps(ib,m,np,dcont,xke,conexp)
c**********************************************************************
c   compute exponentials for continuum absorption in individual layers.
c
c---- input parameters
c  spectral band (ib)
c  number of grid intervals (m)
c  number of layers (np)
c  layer scaled water vapor amount for continuum absorption (dcont) 
c  absorption coefficients for the first k-distribution function
c     due to water vapor continuum absorption (xke)
c
c---- output parameters
c  1 or 3 exponentials for each layer (conexp)
c**********************************************************************
      implicit none
      integer ib,m,np,i,k

c---- input parameters ------

      real dcont(m,np)

c---- updated parameters -----

      real conexp(m,np,3)

c---- static data -----

      real xke(9)

c**********************************************************************

        do k=1,np
         do i=1,m
           conexp(i,k,1) = exp(-dcont(i,k)*xke(ib))
         enddo
        enddo

       if (ib .eq. 3) then

c-----the absorption coefficients for sub-bands 3b and 3a are, 
c     respectively, two and four times the absorption coefficient
c     for sub-band 3c (table 6).

         do k=1,np
          do i=1,m
            conexp(i,k,2) = conexp(i,k,1) *conexp(i,k,1)
            conexp(i,k,3) = conexp(i,k,2) *conexp(i,k,2)
          enddo
         enddo

       endif

      return
      end
c**********************************************************************
      subroutine co2exps(m,np,dco2,pa,dt,co2exp)
c**********************************************************************
c   compute co2 exponentials for individual layers.
c
c---- input parameters
c  number of grid intervals (m)
c  number of layers (np)
c  layer co2 amount (dco2)
c  layer pressure (pa)
c  layer temperature minus 250K (dt)
c
c---- output parameters
c  6 exponentials for each layer (co2exp)
c**********************************************************************
      implicit none
      integer m,np,i,k

c---- input parameters -----

      real dco2(m,np),pa(m,np),dt(m,np)

c---- output parameters -----

      real co2exp(m,np,6,2)

c---- temporary arrays -----

      real xc

c**********************************************************************

        do k=1,np
         do i=1,m

c-----compute the scaled co2 amount from eq. (27) for band-wings
c     (sub-bands 3a and 3c).

           xc = dco2(i,k)*(pa(i,k)/300.0)**0.5
     1             *(1.+(0.0182+1.07e-4*dt(i,k))*dt(i,k))

c-----six exponentials by powers of 8 (table 7).

           co2exp(i,k,1,1)=exp(-xc*2.656e-5)

           xc=co2exp(i,k,1,1)*co2exp(i,k,1,1)
           xc=xc*xc
           co2exp(i,k,2,1)=xc*xc

           xc=co2exp(i,k,2,1)*co2exp(i,k,2,1)
           xc=xc*xc
           co2exp(i,k,3,1)=xc*xc

           xc=co2exp(i,k,3,1)*co2exp(i,k,3,1)
           xc=xc*xc
           co2exp(i,k,4,1)=xc*xc

           xc=co2exp(i,k,4,1)*co2exp(i,k,4,1)
           xc=xc*xc
           co2exp(i,k,5,1)=xc*xc

           xc=co2exp(i,k,5,1)*co2exp(i,k,5,1)
           xc=xc*xc
           co2exp(i,k,6,1)=xc*xc

c-----compute the scaled co2 amount from eq. (27) for band-center
c     region (sub-band 3b).

           xc = dco2(i,k)*(pa(i,k)/30.0)**0.85
     1             *(1.+(0.0042+2.00e-5*dt(i,k))*dt(i,k))

           co2exp(i,k,1,2)=exp(-xc*2.656e-3)

           xc=co2exp(i,k,1,2)*co2exp(i,k,1,2)
           xc=xc*xc
           co2exp(i,k,2,2)=xc*xc

           xc=co2exp(i,k,2,2)*co2exp(i,k,2,2)
           xc=xc*xc
           co2exp(i,k,3,2)=xc*xc

           xc=co2exp(i,k,3,2)*co2exp(i,k,3,2)
           xc=xc*xc
           co2exp(i,k,4,2)=xc*xc

           xc=co2exp(i,k,4,2)*co2exp(i,k,4,2)
           xc=xc*xc
           co2exp(i,k,5,2)=xc*xc

           xc=co2exp(i,k,5,2)*co2exp(i,k,5,2)
           xc=xc*xc
           co2exp(i,k,6,2)=xc*xc

         enddo
        enddo

      return
      end
c**********************************************************************
      subroutine n2oexps(ib,m,np,dn2o,pa,dt,n2oexp)
c**********************************************************************
c   compute n2o exponentials for individual layers.
c
c---- input parameters
c  spectral band (ib)
c  number of grid intervals (m)
c  number of layers (np)
c  layer n2o amount (dn2o)
c  layer pressure (pa)
c  layer temperature minus 250K (dt)
c
c---- output parameters
c  2 or 4 exponentials for each layer (n2oexp)
c**********************************************************************
      implicit none
      integer ib,m,np,i,k

c---- input parameters -----

      real dn2o(m,np),pa(m,np),dt(m,np)

c---- output parameters -----

      real n2oexp(m,np,4)

c---- temporary arrays -----

      real xc,xc1,xc2

c**********************************************************************

       do k=1,np
        do i=1,m

c-----four exponential by powers of 21 for band 6

          if (ib.eq.6) then

           xc=dn2o(i,k)*(1.+(1.9297e-3+4.3750e-6*dt(i,k))*dt(i,k))
           n2oexp(i,k,1)=exp(-xc*6.31582e-2)

           xc=n2oexp(i,k,1)*n2oexp(i,k,1)*n2oexp(i,k,1)
           xc1=xc*xc
           xc2=xc1*xc1
           n2oexp(i,k,2)=xc*xc1*xc2

c-----four exponential by powers of 8 for band 7

          else

           xc=dn2o(i,k)*(pa(i,k)/500.0)**0.48
     *        *(1.+(1.3804e-3+7.4838e-6*dt(i,k))*dt(i,k))
           n2oexp(i,k,1)=exp(-xc*5.35779e-2)

           xc=n2oexp(i,k,1)*n2oexp(i,k,1)
           xc=xc*xc
           n2oexp(i,k,2)=xc*xc
           xc=n2oexp(i,k,2)*n2oexp(i,k,2)
           xc=xc*xc
           n2oexp(i,k,3)=xc*xc
           xc=n2oexp(i,k,3)*n2oexp(i,k,3)
           xc=xc*xc
           n2oexp(i,k,4)=xc*xc

          endif

        enddo
       enddo

      return
      end
c**********************************************************************
      subroutine ch4exps(ib,m,np,dch4,pa,dt,ch4exp)
c**********************************************************************
c   compute ch4 exponentials for individual layers.
c
c---- input parameters
c  spectral band (ib)
c  number of grid intervals (m)
c  number of layers (np)
c  layer ch4 amount (dch4)
c  layer pressure (pa)
c  layer temperature minus 250K (dt)
c
c---- output parameters
c  1 or 4 exponentials for each layer (ch4exp)
c**********************************************************************
      implicit none
      integer ib,m,np,i,k

c---- input parameters -----

      real dch4(m,np),pa(m,np),dt(m,np)

c---- output parameters -----

      real ch4exp(m,np,4)

c---- temporary arrays -----

      real xc

c**********************************************************************

       do k=1,np
        do i=1,m

c-----four exponentials for band 6

          if (ib.eq.6) then

           xc=dch4(i,k)*(1.+(1.7007e-2+1.5826e-4*dt(i,k))*dt(i,k))
           ch4exp(i,k,1)=exp(-xc*5.80708e-3)

c-----four exponentials by powers of 12 for band 7

          else

           xc=dch4(i,k)*(pa(i,k)/500.0)**0.65
     *       *(1.+(5.9590e-4-2.2931e-6*dt(i,k))*dt(i,k))
           ch4exp(i,k,1)=exp(-xc*6.29247e-2)

           xc=ch4exp(i,k,1)*ch4exp(i,k,1)*ch4exp(i,k,1)
           xc=xc*xc
           ch4exp(i,k,2)=xc*xc

           xc=ch4exp(i,k,2)*ch4exp(i,k,2)*ch4exp(i,k,2)
           xc=xc*xc
           ch4exp(i,k,3)=xc*xc

           xc=ch4exp(i,k,3)*ch4exp(i,k,3)*ch4exp(i,k,3)
           xc=xc*xc
           ch4exp(i,k,4)=xc*xc

          endif

        enddo
       enddo

      return
      end



c**********************************************************************
      subroutine comexps(ib,m,np,dcom,dt,comexp)
c**********************************************************************
c   compute co2-minor exponentials for individual layers.
c
c---- input parameters
c  spectral band (ib)
c  number of grid intervals (m)
c  number of layers (np)
c  layer co2 amount (dcom)
c  layer temperature minus 250K (dt)
c
c---- output parameters
c  6 exponentials for each layer (comexp)
c**********************************************************************
      implicit none
      integer ib,m,np,i,k,ik

c---- input parameters -----

      real dcom(m,np),dt(m,np)

c---- output parameters -----

      real comexp(m,np,6)

c---- temporary arrays -----

      real xc

c**********************************************************************

       do k=1,np
        do i=1,m

c-----

          if (ib.eq.4) then
           xc=dcom(i,k)*(1.+(3.5775e-2+4.0447e-4*dt(i,k))*dt(i,k))
          endif

          if (ib.eq.5) then
           xc=dcom(i,k)*(1.+(3.4268e-2+3.7401e-4*dt(i,k))*dt(i,k))
          endif

           comexp(i,k,1)=exp(-xc*1.9217e-7)

          do ik=2,6
           xc=comexp(i,k,ik-1)*comexp(i,k,ik-1)
           xc=xc*xc
           comexp(i,k,ik)=xc*comexp(i,k,ik-1)
          enddo

        enddo
       enddo

      return
      end

c**********************************************************************
      subroutine cfcexps(ib,m,np,a1,b1,fk1,a2,b2,fk2,dcfc,dt,cfcexp)
c**********************************************************************
c   compute cfc(-11, -12, -22) exponentials for individual layers.
c
c---- input parameters
c  spectral band (ib)
c  number of grid intervals (m)
c  number of layers (np)
c  parameters for computing the scaled cfc amounts
c             for temperature scaling (a1,b1,a2,b2)
c  the absorption coefficients for the
c     first k-distribution function due to cfcs (fk1,fk2)
c  layer cfc amounts (dcfc)
c  layer temperature minus 250K (dt)
c
c---- output parameters
c  1 exponential for each layer (cfcexp)
c**********************************************************************
      implicit none
      integer ib,m,np,i,k

c---- input parameters -----

      real dcfc(m,np),dt(m,np)

c---- output parameters -----

      real cfcexp(m,np)

c---- static data -----

      real a1,b1,fk1,a2,b2,fk2

c---- temporary arrays -----

      real xf

c**********************************************************************

       do k=1,np
        do i=1,m

c-----compute the scaled cfc amount (xf) and exponential (cfcexp)

          if (ib.eq.4) then
           xf=dcfc(i,k)*(1.+(a1+b1*dt(i,k))*dt(i,k))
           cfcexp(i,k)=exp(-xf*fk1)
          else
           xf=dcfc(i,k)*(1.+(a2+b2*dt(i,k))*dt(i,k))
           cfcexp(i,k)=exp(-xf*fk2)
          endif

        enddo
       enddo

      return
      end
c**********************************************************************
      subroutine b10exps(m,np,dh2o,dcont,dco2,dn2o,pa,dt
     *          ,h2oexp,conexp,co2exp,n2oexp)
c**********************************************************************
c   compute band3a exponentials for individual layers.
c
c---- input parameters
c  number of grid intervals (m)
c  number of layers (np)
c  layer h2o amount for line absorption (dh2o)
c  layer h2o amount for continuum absorption (dcont)
c  layer co2 amount (dco2)
c  layer n2o amount (dn2o)
c  layer pressure (pa)
c  layer temperature minus 250K (dt)
c
c---- output parameters
c
c  exponentials for each layer (h2oexp,conexp,co2exp,n2oexp)
c**********************************************************************
      implicit none
      integer m,np,i,k

c---- input parameters -----

      real dh2o(m,np),dcont(m,np),dn2o(m,np)
      real dco2(m,np),pa(m,np),dt(m,np)

c---- output parameters -----

      real h2oexp(m,np,6),conexp(m,np,3),co2exp(m,np,6,2)
     *    ,n2oexp(m,np,4)

c---- temporary arrays -----

      real xx,xx1,xx2,xx3

c**********************************************************************

        do k=1,np
         do i=1,m

c-----compute the scaled h2o-line amount for the sub-band 3a
c     table 3, Chou et al. (JAS, 1993)

           xx=dh2o(i,k)*(pa(i,k)/500.0)
     1           *(1.+(0.0149+6.20e-5*dt(i,k))*dt(i,k))

c-----six exponentials by powers of 8
c     the constant 0.10624 is equal to 1.66*0.064

           h2oexp(i,k,1)=exp(-xx*0.10624)

           xx=h2oexp(i,k,1)*h2oexp(i,k,1)
           xx=xx*xx
           h2oexp(i,k,2)=xx*xx

           xx=h2oexp(i,k,2)*h2oexp(i,k,2)
           xx=xx*xx
           h2oexp(i,k,3)=xx*xx

           xx=h2oexp(i,k,3)*h2oexp(i,k,3)
           xx=xx*xx
           h2oexp(i,k,4)=xx*xx

           xx=h2oexp(i,k,4)*h2oexp(i,k,4)
           xx=xx*xx
           h2oexp(i,k,5)=xx*xx

           xx=h2oexp(i,k,5)*h2oexp(i,k,5)
           xx=xx*xx
           h2oexp(i,k,6)=xx*xx

c-----compute the scaled co2 amount for the sub-band 3a
c     table 1, Chou et al. (JAS, 1993)

           xx=dco2(i,k)*(pa(i,k)/300.0)**0.5
     1           *(1.+(0.0179+1.02e-4*dt(i,k))*dt(i,k))

c-----six exponentials by powers of 8
c     the constant 2.656e-5 is equal to 1.66*1.60e-5

           co2exp(i,k,1,1)=exp(-xx*2.656e-5)

           xx=co2exp(i,k,1,1)*co2exp(i,k,1,1)
           xx=xx*xx
           co2exp(i,k,2,1)=xx*xx

           xx=co2exp(i,k,2,1)*co2exp(i,k,2,1)
           xx=xx*xx
           co2exp(i,k,3,1)=xx*xx

           xx=co2exp(i,k,3,1)*co2exp(i,k,3,1)
           xx=xx*xx
           co2exp(i,k,4,1)=xx*xx

           xx=co2exp(i,k,4,1)*co2exp(i,k,4,1)
           xx=xx*xx
           co2exp(i,k,5,1)=xx*xx

           xx=co2exp(i,k,5,1)*co2exp(i,k,5,1)
           xx=xx*xx
           co2exp(i,k,6,1)=xx*xx

c-----one exponential of h2o continuum for sub-band 3a
c     tabl 5 of Chou et. al. (JAS, 1993)
c     the constant 1.04995e+2 is equal to 1.66*63.25

           conexp(i,k,1)=exp(-dcont(i,k)*1.04995e+2)

c-----compute the scaled n2o amount for sub-band 3a

           xx=dn2o(i,k)*(1.+(1.4476e-3+3.6656e-6*dt(i,k))*dt(i,k))

c-----two exponential2 by powers of 58

           n2oexp(i,k,1)=exp(-xx*0.25238)

           xx=n2oexp(i,k,1)*n2oexp(i,k,1)
           xx1=xx*xx
           xx1=xx1*xx1
           xx2=xx1*xx1
           xx3=xx2*xx2
           n2oexp(i,k,2)=xx*xx1*xx2*xx3

         enddo
        enddo

      return
      end
c**********************************************************************
      subroutine tablup(k1,k2,m,np,nx,nh,sabs,spre,stem,w1,p1,
     *                  dwe,dpe,coef1,coef2,coef3,tran)
c**********************************************************************
c   compute water vapor, co2 and o3 transmittances between levels
c   k1 and k2 for m soundings, using table look-up.
c
c   calculations follow Eq. (40) of Chou and Suarez (1994)
c
c---- input ---------------------
c  indices for pressure levels (k1 and k2)
c  number of grid intervals (m)
c  number of atmospheric layers (np)
c  number of pressure intervals in the table (nx)
c  number of absorber amount intervals in the table (nh)
c  column-integrated absorber amount (sabs)
c  column absorber amount-weighted pressure (spre)
c  column absorber amount-weighted temperature (stem)
c  first value of absorber amount (log10) in the table (w1) 
c  first value of pressure (log10) in the table (p1) 
c  size of the interval of absorber amount (log10) in the table (dwe)
c  size of the interval of pressure (log10) in the table (dpe)
c  pre-computed coefficients (coef1, coef2, and coef3)
c
c---- updated ---------------------
c  transmittance (tran)
c
c  Note:
c   (1) units of sabs are g/cm**2 for water vapor and
c       (cm-atm)stp for co2 and o3.
c   (2) units of spre and stem are, respectively, mb and K.
c   (3) there are nt identical copies of the tables (coef1, coef2, and
c       coef3).  the prupose of using the multiple copies of tables is
c       to increase the speed in parallel (vectorized) computations.
C       if such advantage does not exist, nt can be set to 1.
c   
c**********************************************************************
      implicit none
      integer k1,k2,m,np,nx,nh,i

c---- input parameters -----

      real w1,p1,dwe,dpe
      real sabs(m,np+1),spre(m,np+1),stem(m,np+1)
      real coef1(nx,nh),coef2(nx,nh),coef3(nx,nh)

c---- update parameter -----

      real tran(m)

c---- temporary variables -----

      real x1,x2,x3,we,pe,fw,fp,pa,pb,pc,ax,ba,bb,t1,ca,cb,t2
      integer iw,ip

c**********************************************************************

      do i=1,m

        x1=sabs(i,k2)-sabs(i,k1)
        x2=(spre(i,k2)-spre(i,k1))/x1
        x3=(stem(i,k2)-stem(i,k1))/x1

        we=(log10(x1)-w1)/dwe
        pe=(log10(x2)-p1)/dpe

        we=max(we,w1-2.*dwe)
        pe=max(pe,p1)

        iw=int(we+1.5)
        ip=int(pe+1.5)

        iw=min(iw,nh-1)
        iw=max(iw, 2)

        ip=min(ip,nx-1)
        ip=max(ip, 1)

        fw=we-float(iw-1)
        fp=pe-float(ip-1)

c-----linear interpolation in pressure

        pa = coef1(ip,iw-1)*(1.-fp)+coef1(ip+1,iw-1)*fp
        pb = coef1(ip,  iw)*(1.-fp)+coef1(ip+1,  iw)*fp
        pc = coef1(ip,iw+1)*(1.-fp)+coef1(ip+1,iw+1)*fp

c-----quadratic interpolation in absorber amount for coef1

        ax = (-pa*(1.-fw)+pc*(1.+fw)) *fw*0.5 + pb*(1.-fw*fw)

c-----linear interpolation in absorber amount for coef2 and coef3

        ba = coef2(ip,  iw)*(1.-fp)+coef2(ip+1,  iw)*fp
        bb = coef2(ip,iw+1)*(1.-fp)+coef2(ip+1,iw+1)*fp
        t1 = ba*(1.-fw) + bb*fw

        ca = coef3(ip,  iw)*(1.-fp)+coef3(ip+1,  iw)*fp
        cb = coef3(ip,iw+1)*(1.-fp)+coef3(ip+1,iw+1)*fp
        t2 = ca*(1.-fw) + cb*fw

c-----update the total transmittance between levels k1 and k2

        tran(i)= (ax + (t1+t2*x3) * x3)*tran(i)

      enddo

      return
      end
c**********************************************************************
      subroutine h2okdis(ib,m,np,k,fkw,gkw,ne,h2oexp,conexp,
     *                   th2o,tcon,tran)
c**********************************************************************
c   compute water vapor transmittance between levels k1 and k2 for
c   m soundings, using the k-distribution method.
c
c   computations follow eqs. (34), (46), (50) and (52).
c
c---- input parameters
c  spectral band (ib)
c  number of grid intervals (m)
c  number of levels (np)
c  current level (k)
c  planck-weighted k-distribution function due to
c    h2o line absorption (fkw)
c  planck-weighted k-distribution function due to
c    h2o continuum absorption (gkw)
c  number of terms used in each band to compute water vapor
c     continuum transmittance (ne)
c  exponentials for line absorption (h2oexp) 
c  exponentials for continuum absorption (conexp) 
c
c---- updated parameters
c  transmittance between levels k1 and k2 due to
c    water vapor line absorption (th2o)
c  transmittance between levels k1 and k2 due to
c    water vapor continuum absorption (tcon)
c  total transmittance (tran)
c
c**********************************************************************
      implicit none
      integer ib,m,np,k,i

c---- input parameters ------

      real conexp(m,np,3),h2oexp(m,np,6)
      integer ne(9)
      real  fkw(6,9),gkw(6,3)

c---- updated parameters -----

      real th2o(m,6),tcon(m,3),tran(m)

c---- temporary arrays -----

      real trnth2o

c-----tco2 are the six exp factors between levels k1 and k2 
c     tran is the updated total transmittance between levels k1 and k2

c-----th2o is the 6 exp factors between levels k1 and k2 due to
c     h2o line absorption. 

c-----tcon is the 3 exp factors between levels k1 and k2 due to
c     h2o continuum absorption.

c-----trnth2o is the total transmittance between levels k1 and k2 due
c     to both line and continuum absorption computed from eq. (52).

         do i=1,m
           th2o(i,1) = th2o(i,1)*h2oexp(i,k,1)
           th2o(i,2) = th2o(i,2)*h2oexp(i,k,2)
           th2o(i,3) = th2o(i,3)*h2oexp(i,k,3)
           th2o(i,4) = th2o(i,4)*h2oexp(i,k,4)
           th2o(i,5) = th2o(i,5)*h2oexp(i,k,5)
           th2o(i,6) = th2o(i,6)*h2oexp(i,k,6)
         enddo

      if (ne(ib).eq.0) then

         do i=1,m

           trnth2o      =(fkw(1,ib)*th2o(i,1)
     *                  + fkw(2,ib)*th2o(i,2)
     *                  + fkw(3,ib)*th2o(i,3)
     *                  + fkw(4,ib)*th2o(i,4)
     *                  + fkw(5,ib)*th2o(i,5)
     *                  + fkw(6,ib)*th2o(i,6))

          tran(i)=tran(i)*trnth2o

         enddo

      elseif (ne(ib).eq.1) then

         do i=1,m

           tcon(i,1)= tcon(i,1)*conexp(i,k,1)

           trnth2o      =(fkw(1,ib)*th2o(i,1)
     *                  + fkw(2,ib)*th2o(i,2)
     *                  + fkw(3,ib)*th2o(i,3)
     *                  + fkw(4,ib)*th2o(i,4)
     *                  + fkw(5,ib)*th2o(i,5)
     *                  + fkw(6,ib)*th2o(i,6))*tcon(i,1)

          tran(i)=tran(i)*trnth2o

         enddo

      else

c-----Band 3 is divided into 3 subbands

         do i=1,m

           tcon(i,1)= tcon(i,1)*conexp(i,k,1)
           tcon(i,2)= tcon(i,2)*conexp(i,k,2)
           tcon(i,3)= tcon(i,3)*conexp(i,k,3)


           trnth2o      = (  gkw(1,1)*th2o(i,1)
     *                     + gkw(2,1)*th2o(i,2)
     *                     + gkw(3,1)*th2o(i,3)
     *                     + gkw(4,1)*th2o(i,4)
     *                     + gkw(5,1)*th2o(i,5)
     *                     + gkw(6,1)*th2o(i,6) ) * tcon(i,1)
     *                  + (  gkw(1,2)*th2o(i,1)
     *                     + gkw(2,2)*th2o(i,2)
     *                     + gkw(3,2)*th2o(i,3)
     *                     + gkw(4,2)*th2o(i,4)
     *                     + gkw(5,2)*th2o(i,5)
     *                     + gkw(6,2)*th2o(i,6) ) * tcon(i,2)
     *                  + (  gkw(1,3)*th2o(i,1)
     *                     + gkw(2,3)*th2o(i,2)
     *                     + gkw(3,3)*th2o(i,3)
     *                     + gkw(4,3)*th2o(i,4)
     *                     + gkw(5,3)*th2o(i,5)
     *                     + gkw(6,3)*th2o(i,6) ) * tcon(i,3)

          tran(i)=tran(i)*trnth2o

         enddo

      endif

      return
      end
c**********************************************************************
      subroutine co2kdis(m,np,k,co2exp,tco2,tran)
c**********************************************************************
c   compute co2 transmittances between levels k1 and k2 for
c    m soundings, using the k-distribution method with linear
c    pressure scaling. computations follow eq. (34).
c
c---- input parameters
c   number of grid intervals (m)
c   number of levels (np)
c   current level (k)
c   exponentials for co2 absorption (co2exp)
c
c---- updated parameters
c   transmittance between levels k1 and k2 due to co2 absorption
c     for the various values of the absorption coefficient (tco2)
c   total transmittance (tran)
c
c**********************************************************************
      implicit none
      integer m,np,k,i

c---- input parameters -----

      real co2exp(m,np,6,2)

c---- updated parameters -----

      real tco2(m,6,2),tran(m)

c---- temporary arrays -----

      real xc

c-----tco2 is the 6 exp factors between levels k1 and k2. 
c     xc is the total co2 transmittance given by eq. (53).

         do i=1,m

c-----band-wings

           tco2(i,1,1)=tco2(i,1,1)*co2exp(i,k,1,1)
           xc=   0.1395 *tco2(i,1,1)

           tco2(i,2,1)=tco2(i,2,1)*co2exp(i,k,2,1)
           xc=xc+0.1407 *tco2(i,2,1)

           tco2(i,3,1)=tco2(i,3,1)*co2exp(i,k,3,1)
           xc=xc+0.1549 *tco2(i,3,1)

           tco2(i,4,1)=tco2(i,4,1)*co2exp(i,k,4,1)
           xc=xc+0.1357 *tco2(i,4,1)

           tco2(i,5,1)=tco2(i,5,1)*co2exp(i,k,5,1)
           xc=xc+0.0182 *tco2(i,5,1)

           tco2(i,6,1)=tco2(i,6,1)*co2exp(i,k,6,1)
           xc=xc+0.0220 *tco2(i,6,1)

c-----band-center region

           tco2(i,1,2)=tco2(i,1,2)*co2exp(i,k,1,2)
           xc=xc+0.0766 *tco2(i,1,2)

           tco2(i,2,2)=tco2(i,2,2)*co2exp(i,k,2,2)
           xc=xc+0.1372 *tco2(i,2,2)

           tco2(i,3,2)=tco2(i,3,2)*co2exp(i,k,3,2)
           xc=xc+0.1189 *tco2(i,3,2)

           tco2(i,4,2)=tco2(i,4,2)*co2exp(i,k,4,2)
           xc=xc+0.0335 *tco2(i,4,2)

           tco2(i,5,2)=tco2(i,5,2)*co2exp(i,k,5,2)
           xc=xc+0.0169 *tco2(i,5,2)

           tco2(i,6,2)=tco2(i,6,2)*co2exp(i,k,6,2)
           xc=xc+0.0059 *tco2(i,6,2)

           tran(i)=tran(i)*xc

         enddo

      return
      end
c**********************************************************************
      subroutine n2okdis(ib,m,np,k,n2oexp,tn2o,tran)
c**********************************************************************
c   compute n2o transmittances between levels k1 and k2 for
c    m soundings, using the k-distribution method with linear
c    pressure scaling.
c
c---- input parameters
c   spectral band (ib)
c   number of grid intervals (m)
c   number of levels (np)
c   current level (k)
c   exponentials for n2o absorption (n2oexp)
c
c---- updated parameters
c   transmittance between levels k1 and k2 due to n2o absorption
c     for the various values of the absorption coefficient (tn2o)
c   total transmittance (tran)
c
c**********************************************************************
      implicit none
      integer ib,m,np,k,i

c---- input parameters -----

      real n2oexp(m,np,4)

c---- updated parameters -----

      real tn2o(m,4),tran(m)

c---- temporary arrays -----

      real xc

c-----tn2o is the 2 exp factors between levels k1 and k2. 
c     xc is the total n2o transmittance

        do i=1,m

c-----band 6

          if (ib.eq.6) then

           tn2o(i,1)=tn2o(i,1)*n2oexp(i,k,1)
           xc=   0.940414*tn2o(i,1)

           tn2o(i,2)=tn2o(i,2)*n2oexp(i,k,2)
           xc=xc+0.059586*tn2o(i,2)

c-----band 7

          else

           tn2o(i,1)=tn2o(i,1)*n2oexp(i,k,1)
           xc=   0.561961*tn2o(i,1)

           tn2o(i,2)=tn2o(i,2)*n2oexp(i,k,2)
           xc=xc+0.138707*tn2o(i,2)

           tn2o(i,3)=tn2o(i,3)*n2oexp(i,k,3)
           xc=xc+0.240670*tn2o(i,3)

           tn2o(i,4)=tn2o(i,4)*n2oexp(i,k,4)
           xc=xc+0.058662*tn2o(i,4)

          endif

           tran(i)=tran(i)*xc

        enddo

      return
      end
c**********************************************************************
      subroutine ch4kdis(ib,m,np,k,ch4exp,tch4,tran)
c**********************************************************************
c   compute ch4 transmittances between levels k1 and k2 for
c    m soundings, using the k-distribution method with
c    linear pressure scaling.
c
c---- input parameters
c   spectral band (ib)
c   number of grid intervals (m)
c   number of levels (np)
c   current level (k)
c   exponentials for ch4 absorption (ch4exp)
c
c---- updated parameters
c   transmittance between levels k1 and k2 due to ch4 absorption
c     for the various values of the absorption coefficient (tch4)
c   total transmittance (tran)
c
c**********************************************************************
      implicit none
      integer ib,m,np,k,i

c---- input parameters -----

      real ch4exp(m,np,4)

c---- updated parameters -----

      real tch4(m,4),tran(m)

c---- temporary arrays -----

      real xc

c-----tch4 is the 2 exp factors between levels k1 and k2. 
c     xc is the total ch4 transmittance

        do i=1,m

c-----band 6

          if (ib.eq.6) then

           tch4(i,1)=tch4(i,1)*ch4exp(i,k,1)
           xc= tch4(i,1)

c-----band 7

          else

           tch4(i,1)=tch4(i,1)*ch4exp(i,k,1)
           xc=   0.610650*tch4(i,1)

           tch4(i,2)=tch4(i,2)*ch4exp(i,k,2)
           xc=xc+0.280212*tch4(i,2)

           tch4(i,3)=tch4(i,3)*ch4exp(i,k,3)
           xc=xc+0.107349*tch4(i,3)

           tch4(i,4)=tch4(i,4)*ch4exp(i,k,4)
           xc=xc+0.001789*tch4(i,4)

          endif

           tran(i)=tran(i)*xc

        enddo

      return
      end
c**********************************************************************
      subroutine comkdis(ib,m,np,k,comexp,tcom,tran)
c**********************************************************************
c  compute co2-minor transmittances between levels k1 and k2
c   for m soundings, using the k-distribution method
c   with linear pressure scaling.
c
c---- input parameters
c   spectral band (ib)
c   number of grid intervals (m)
c   number of levels (np)
c   current level (k)
c   exponentials for co2-minor absorption (comexp)
c
c---- updated parameters
c   transmittance between levels k1 and k2 due to co2-minor absorption
c     for the various values of the absorption coefficient (tcom)
c   total transmittance (tran)
c
c**********************************************************************
      implicit none
      integer ib,m,np,k,i

c---- input parameters -----

      real comexp(m,np,6)

c---- updated parameters -----

      real tcom(m,6),tran(m)

c---- temporary arrays -----

      real xc

c-----tcom is the exp factors between levels k1 and k2. 
c     xc is the total co2-minor transmittance

         do i=1,m

c-----band 4

           if (ib.eq.4) then

            tcom(i,1)=tcom(i,1)*comexp(i,k,1)
            xc=   0.12159*tcom(i,1)
            tcom(i,2)=tcom(i,2)*comexp(i,k,2)
            xc=xc+0.24359*tcom(i,2)
            tcom(i,3)=tcom(i,3)*comexp(i,k,3)
            xc=xc+0.24981*tcom(i,3)
            tcom(i,4)=tcom(i,4)*comexp(i,k,4)
            xc=xc+0.26427*tcom(i,4)
            tcom(i,5)=tcom(i,5)*comexp(i,k,5)
            xc=xc+0.07807*tcom(i,5)
            tcom(i,6)=tcom(i,6)*comexp(i,k,6)
            xc=xc+0.04267*tcom(i,6)

c-----band 5

           else

            tcom(i,1)=tcom(i,1)*comexp(i,k,1)
            xc=   0.06869*tcom(i,1)
            tcom(i,2)=tcom(i,2)*comexp(i,k,2)
            xc=xc+0.14795*tcom(i,2)
            tcom(i,3)=tcom(i,3)*comexp(i,k,3)
            xc=xc+   0.19512*tcom(i,3)
            tcom(i,4)=tcom(i,4)*comexp(i,k,4)
            xc=xc+   0.33446*tcom(i,4)
            tcom(i,5)=tcom(i,5)*comexp(i,k,5)
            xc=xc+   0.17199*tcom(i,5)
            tcom(i,6)=tcom(i,6)*comexp(i,k,6)
            xc=xc+   0.08179*tcom(i,6)
           endif

            tran(i)=tran(i)*xc

         enddo

      return
      end
c**********************************************************************
      subroutine cfckdis(m,np,k,cfcexp,tcfc,tran)
c**********************************************************************
c  compute cfc-(11,12,22) transmittances between levels k1 and k2
c   for m soundings, using the k-distribution method with
c   linear pressure scaling.
c
c---- input parameters
c   number of grid intervals (m)
c   number of levels (np)
c   current level (k)
c   exponentials for cfc absorption (cfcexp)
c
c---- updated parameters
c   transmittance between levels k1 and k2 due to cfc absorption
c     for the various values of the absorption coefficient (tcfc)
c   total transmittance (tran)
c
c**********************************************************************
      implicit none
      integer m,np,k,i

c---- input parameters -----

      real cfcexp(m,np)

c---- updated parameters -----

      real tcfc(m),tran(m)

c-----tcfc is the exp factors between levels k1 and k2. 

         do i=1,m

            tcfc(i)=tcfc(i)*cfcexp(i,k)
            tran(i)=tran(i)*tcfc(i)

         enddo

      return
      end
c**********************************************************************
      subroutine b10kdis(m,np,k,h2oexp,conexp,co2exp,n2oexp
     *          ,th2o,tcon,tco2,tn2o,tran)
c**********************************************************************
c
c   compute h2o (line and continuum),co2,n2o transmittances between
c   levels k1 and k2 for m soundings, using the k-distribution
c   method with linear pressure scaling.
c
c---- input parameters
c   number of grid intervals (m)
c   number of levels (np)
c   current level (k)
c   exponentials for h2o line absorption (h2oexp)
c   exponentials for h2o continuum absorption (conexp)
c   exponentials for co2 absorption (co2exp)
c   exponentials for n2o absorption (n2oexp)
c
c---- updated parameters
c   transmittance between levels k1 and k2 due to h2o line absorption
c     for the various values of the absorption coefficient (th2o)
c   transmittance between levels k1 and k2 due to h2o continuum
c     absorption for the various values of the absorption
c     coefficient (tcon)
c   transmittance between levels k1 and k2 due to co2 absorption
c     for the various values of the absorption coefficient (tco2)
c   transmittance between levels k1 and k2 due to n2o absorption
c     for the various values of the absorption coefficient (tn2o)
c   total transmittance (tran)
c
c**********************************************************************
      implicit none
      integer m,np,k,i

c---- input parameters -----

      real h2oexp(m,np,6),conexp(m,np,3),co2exp(m,np,6,2)
     *    ,n2oexp(m,np,4)

c---- updated parameters -----

      real th2o(m,6),tcon(m,3),tco2(m,6,2),tn2o(m,4)
     *    ,tran(m)

c---- temporary arrays -----

      real xx

c-----initialize tran

       do i=1,m
           tran(i)=1.0
       enddo

c-----for h2o line

        do i=1,m

           th2o(i,1)=th2o(i,1)*h2oexp(i,k,1)
           xx=   0.3153*th2o(i,1)

           th2o(i,2)=th2o(i,2)*h2oexp(i,k,2)
           xx=xx+0.4604*th2o(i,2)

           th2o(i,3)=th2o(i,3)*h2oexp(i,k,3)
           xx=xx+0.1326*th2o(i,3)

           th2o(i,4)=th2o(i,4)*h2oexp(i,k,4)
           xx=xx+0.0798*th2o(i,4)

           th2o(i,5)=th2o(i,5)*h2oexp(i,k,5)
           xx=xx+0.0119*th2o(i,5)

           tran(i)=tran(i)*xx

        enddo

c-----for h2o continuum

        do i=1,m

           tcon(i,1)=tcon(i,1)*conexp(i,k,1)
           tran(i)=tran(i)*tcon(i,1)

        enddo
 
c-----for co2 

        do i=1,m

           tco2(i,1,1)=tco2(i,1,1)*co2exp(i,k,1,1)
           xx=    0.2673*tco2(i,1,1)

           tco2(i,2,1)=tco2(i,2,1)*co2exp(i,k,2,1)
           xx=xx+ 0.2201*tco2(i,2,1)

           tco2(i,3,1)=tco2(i,3,1)*co2exp(i,k,3,1)
           xx=xx+ 0.2106*tco2(i,3,1)

           tco2(i,4,1)=tco2(i,4,1)*co2exp(i,k,4,1)
           xx=xx+ 0.2409*tco2(i,4,1)

           tco2(i,5,1)=tco2(i,5,1)*co2exp(i,k,5,1)
           xx=xx+ 0.0196*tco2(i,5,1)

           tco2(i,6,1)=tco2(i,6,1)*co2exp(i,k,6,1)
           xx=xx+ 0.0415*tco2(i,6,1)

           tran(i)=tran(i)*xx

        enddo

c-----for n2o

        do i=1,m

           tn2o(i,1)=tn2o(i,1)*n2oexp(i,k,1)
           xx=   0.970831*tn2o(i,1)

           tn2o(i,2)=tn2o(i,2)*n2oexp(i,k,2)
           xx=xx+0.029169*tn2o(i,2)

           tran(i)=tran(i)*(xx-1.0)

        enddo

      return
      end
