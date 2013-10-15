c***********************       CLIRAD-SW      ************************
 
      subroutine sorad (pl, ta, wa, oa, co2,
     *                  overcast, cldwater, cwc, taucld, reff, fcld, ict,i cb,
     *                  taual, ssaal, asyal,
     *                  cosz, rsuvbm, rsuvdf, rsirbm, rsirdf,
     *                  flx, flc, fdiruv, fdifuv, fdirpar, fdifpar,
     *                  fdirir, fdifir)
c
c**********************************************************************
c  
c Following the NASA Technical Memorandum (NASA/TM-1999-104606, Vol. 15)
c  of Chou and Suarez (1999), this routine computes solar fluxes due to 
c  absorption by water vapor, ozone, co2, o2, clouds, and aerosols and 
c  due to scattering by clouds, aerosols, and gases.
c
c This code computes fluxes simultaneously for m soundings.
c
c Cloud ice, liquid, and rain particles are allowed to co-exist in a layer. 
c
c There is an option of providing either cloud ice/water mixing ratio 
c  (cwc) or optical thickness (taucld).  If the former is provided, set
c  cldwater=.true., and taucld is computed from cwc and reff as a
c  function of spectra band. Otherwise, set cldwater=.false., and
c  specify taucld, independent of spectral band.
c
c If no information is available for the effective particle size, reff, 
c  default values of 10 micron for liquid water and 75 micron for ice may be 
c  used. 
c  The size of raindrops, reff(3), is irrelevant in this code. It can be
c  set to any values.
c  For a clear layer, reff can be set to any values except zero.
c
c The maximum-random assumption is applied for treating cloud
c  overlapping. Clouds are grouped into high, middle, and low clouds
c  separated by the level indices ict and icb.  For detail, see
c  subroutine "cldscale".
c
c In a high spatial-resolution atmospheric model, fractional cloud cover
c  might be computed to be either 0 or 1.  In such a case, scaling of the
c  cloud optical thickness is not necessary, and the computation can be
c  made faster by setting overcast=.true.  Otherwise, set the option
c  overcast=.false.
c
c Aerosol optical thickness, single-scattering albedo, and asymmetry
c  factor can be specified as functions of height and spectral band.
c
c----- Input parameters:                           
c                                                   units      size
c  number of soundings (m)                          n/d         1
c  number of atmospheric layers (np)                n/d         1
c  level pressure (pl)                              mb        m*(np+1)
c  layer temperature (ta)                           k         m*np
c  layer specific humidity (wa)                     gm/gm     m*np
c  layer ozone concentration (oa)                   gm/gm     m*np
c  co2 mixing ratio by volume (co2)                 pppv        1
c  option for scaling cloud optical thickness       n/d         1
c        overcast="true" if scaling is NOT required
c        overcast="false" if scaling is required
c  option for cloud optical thickness               n/d         1
c        cldwater="true" if cwc is provided
c        cldwater="false" if taucld is provided
c+++rca directly passing cloud water path in g/m2 !!!
c  cloud water mixing ratio (cwc)                  gm/gm      m*np*3
c        index 1 for ice particles
c        index 2 for liquid drops
c        index 3 for rain drops
c  cloud optical thickness (taucld)                 n/d       m*np*3
c        index 1 for ice particles
c        index 2 for liquid drops
c        index 3 for rain drops
c  effective cloud-particle size (reff)          micrometer   m*np*3
c        index 1 for ice particles
c        index 2 for liquid drops
c        index 3 for rain drops
c  cloud amount (fcld)                            fraction    m*np
c  level index separating high and middle           n/d         1
c        clouds (ict)
c  level index separating middle and low            n/d         1
c        clouds (icb)
c  aerosol optical thickness (taual)                n/d       m*np*11
c  aerosol single-scattering albedo (ssaal)         n/d       m*np*11
c  aerosol asymmetry factor (asyal)                 n/d       m*np*11
c        in the uv region :
c           index  1 for the 0.175-0.225 micron band
c           index  2 for the 0.225-0.245; 0.260-0.280 micron band
c           index  3 for the 0.245-0.260 micron band
c           index  4 for the 0.280-0.295 micron band
c           index  5 for the 0.295-0.310 micron band
c           index  6 for the 0.310-0.320 micron band
c           index  7 for the 0.325-0.400 micron band
c        in the par region :
c           index  8 for the 0.400-0.700 micron band
c        in the infrared region :
c           index  9 for the 0.700-1.220 micron band
c           index 10 for the 1.220-2.270 micron band
c           index 11 for the 2.270-10.00 micron band
c   cosine of solar zenith angle (cosz)              n/d      m
c   uv+visible sfc albedo for beam radiation
c        for wavelengths<0.7 micron (rsuvbm)       fraction   m
c   uv+visible sfc albedo for diffuse radiation
c        for wavelengths<0.7 micron (rsuvdf)       fraction   m
c   ir sfc albedo for beam radiation
c        for wavelengths>0.7 micron  (rsirbm)      fraction   m
c   ir sfc albedo for diffuse radiation (rsirdf)   fraction   m
c
c----- Output parameters
c
c   all-sky flux (downward minus upward) (flx)     fraction   m*(np+1)
c   clear-sky flux (downward minus upward) (flc)   fraction   m*(np+1)
c   all-sky direct downward uv (0.175-0.4 micron)
c                flux at the surface (fdiruv)      fraction   m
c   all-sky diffuse downward uv flux at
c                the surface (fdifuv)              fraction   m
c   all-sky direct downward par (0.4-0.7 micron)
c                flux at the surface (fdirpar)     fraction   m
c   all-sky diffuse downward par flux at
c                the surface (fdifpar)             fraction   m
c   all-sky direct downward ir (0.7-10 micron)
c                flux at the surface (fdirir)      fraction   m
c   all-sky diffuse downward ir flux at
c                the surface (fdifir)              fraction   m
c
c----- Notes:
c
c    (1) The unit of output fluxes (flx,flc,etc.) is fraction of the
c        insolation at the top of the atmosphere.  Therefore, fluxes
c        are the output fluxes multiplied by the extra-terrestrial solar
c        flux and the cosine of the solar zenith angle.
c    (2) pl( ,1) is the pressure at the top of the model, and
c        pl( ,np+1) is the surface pressure.
c    (3) the pressure levels ict and icb correspond approximately
c        to 400 and 700 mb.
c
c-----If coding errors are found, please notify Ming-Dah Chou at
c     chou@climate.gsfc.nasa.gov
c        
c*************************************************************************

      implicit none
      integer np,m
      parameter(m=IM, np=KM) 
c
c-----input parameters

      integer ict,icb
      real pl(m,np+1),ta(m,np),wa(m,np),oa(m,np),co2
      real cwc(m,np,3),taucld(m,np,3),reff(m,np,3),fcld(m,np)
      real taual(m,np,11),ssaal(m,np,11),asyal(m,np,11)
      real cosz(m),rsuvbm(m),rsuvdf(m),rsirbm(m),rsirdf(m)
      logical overcast,cldwater

c-----output parameters

      real flx(m,np+1),flc(m,np+1)
      real fdiruv (m),fdifuv (m)
      real fdirpar(m),fdifpar(m)
      real fdirir (m),fdifir (m)

c-----temporary array
 
      integer i,j,k,ntop,nctop(m)
      real cwp(m,np,3)
      real dp(m,np),wh(m,np),oh(m,np),scal(m,np)
      real swu(m,np+1),swh(m,np+1),so2(m,np+1),df(m,np+1)
      real snt(m),cnt(m),x

c-----parameters for co2 transmission tables

      integer nu,nw,nx,ny
      parameter (nu=43,nw=37,nx=62,ny=101)
      real w1,dw,u1,du,coa(nx,ny),cah(nu,nw)

c-----cah is the co2 absorptance in band 10

       include "cah.data"

c-----coa is the co2 absorptance in strong absorption regions of band 11

       include "coa.data"

c-----

      do i=1,m 

         swh(i,1)=0. 
         so2(i,1)=0. 

c-----snt is the secant of the solar zenith angle
         snt(i)=1.0/cosz(i)

      enddo

      do k=1,np
       do i=1,m
c
c-----compute layer thickness. indices for the surface level and
c     surface layer are np+1 and np, respectively.
 
          dp(i,k)=pl(i,k+1)-pl(i,k)
c 
c-----compute scaled water vapor amount following Eqs. (3.3) and (3.5) 
c     unit is g/cm**2
c
          scal(i,k)=dp(i,k)*(.5*(pl(i,k)+pl(i,k+1))/300.)**.8
          wh(i,k)=1.02*wa(i,k)*scal(i,k)
     *              *(1.+0.00135*(ta(i,k)-240.)) +1.e-11
          swh(i,k+1)=swh(i,k)+wh(i,k)
c
c-----compute ozone amount, unit is (cm-atm)stp
c     the number 466.7 is the unit conversion factor
c     from g/cm**2 to (cm-atm)stp
 
          oh(i,k)=1.02*oa(i,k)*dp(i,k)*466.7 +1.e-11
c
c-----compute layer cloud water amount (gm/m**2)
c     the index is 1 for ice crystals, 2 for liquid drops, and
c     3 for rain drops

c+++rca: input is already cwp in g/m2
c          x=1.02*10000.*dp(i,k)   
c          cwp(i,k,1)=x*cwc(i,k,1)
c          cwp(i,k,2)=x*cwc(i,k,2)
c          cwp(i,k,3)=x*cwc(i,k,3)
          cwp(i,k,1)=cwc(i,k,1)
          cwp(i,k,2)=cwc(i,k,2)
          cwp(i,k,3)=cwc(i,k,3)
c---rca
       enddo
      enddo
c
c-----initialize fluxes for all-sky (flx), clear-sky (flc), and
c     flux reduction (df)
c
      do k=1,np+1
       do i=1,m
          flx(i,k)=0.
          flc(i,k)=0.
          df(i,k)=0.
       enddo
      enddo

c-----compute solar uv and par fluxes

      call soluv (wh,oh,dp,overcast,cldwater,
     *            cwp,taucld,reff,ict,icb,fcld,cosz,
     *            taual,ssaal,asyal,rsuvbm,rsuvdf,
     *            flx,flc,fdiruv,fdifuv,fdirpar,fdifpar)

c-----compute and update solar ir fluxes

      call solir (wh,dp,overcast,cldwater,
     *            cwp,taucld,reff,ict,icb,fcld,cosz,
     *            taual,ssaal,asyal,rsirbm,rsirdf,
     *            flx,flc,fdirir,fdifir)

c-----compute pressure-scaled o2 amount following Eq. (3.5) with
c     f=1. unit is (cm-atm)stp.
c     the constant 165.22 equals (1000/980)*23.14%*(22400/32)

       do i= 1, m
        cnt(i)=165.22*snt(i)
       enddo

      do k=1,np
       do i=1,m
          so2(i,k+1)=so2(i,k)+scal(i,k)*cnt(i)
       enddo
      enddo

c-----compute flux reduction due to oxygen following Eq. (3.18)
c     the constant 0.0633 is the fraction of insolation contained 
c     in the oxygen bands

       do k= 2, np+1
        do i= 1, m
          x=so2(i,k)
          df(i,k)=0.0633*(1.-exp(-0.000145*sqrt(x)))
        enddo
       enddo          

c-----for solar heating due to co2

       do i= 1, m
        cnt(i)=co2*snt(i)
       enddo

c-----scale co2 amounts following Eq. (3.5) with f=1.
c     unit is (cm-atm)stp.
c     the constant 789 equals (1000/980)*(44/28.97)*(22400/44)

      do k=1,np
       do i=1,m
         x=789.*cnt(i)
         so2(i,k+1)=so2(i,k)+x*scal(i,k)+1.e-11
       enddo
      enddo

c-----for co2 absorption in Band 10 where absorption due to
c     water vapor and co2 are both moderate

        u1=-3.0
        du=0.15

        w1=-4.0
        dw=0.15

c-----so2 and swh are the co2 and water vapor amounts integrated
c     from the top of the atmosphere

      do k= 2, np+1
       do i= 1, m
        swu(i,k)=log10(so2(i,k))
        swh(i,k)=log10(swh(i,k)*snt(i))
       enddo
      enddo

c-----df is the updated flux reduction given by the second term on the
c     right-hand-side of Eq. (3.24) divided by So

       call rflx(m,np,swu,u1,du,nu,swh,w1,dw,nw,cah,df)

c-----for co2 absorption in Band 11 where the co2 absorption has
c     a large impact on the heating of middle atmosphere.

        u1=0.000250
        du=0.000050

        w1=-2.0
        dw=0.05

        do i= 1, m
         swu(i,1)=co2*snt(i)
        enddo

c-----co2 mixing ratio is independent of space

        do k= 2, np+1
         do i= 1, m
          swu(i,k)=swu(i,1)
         enddo
        enddo

c-----swh is the logarithm of pressure

       do k= 2, np+1
        do i= 1, m
          swh(i,k)=log10(pl(i,k))
        enddo
       enddo

c-----df is the updated flux reduction derived from the table given by
c     Eq. (3.19)

      call rflx(m,np,swu,u1,du,nx,swh,w1,dw,ny,coa,df)

c-----adjustment for the effect of o2 cnd co2 on clear-sky fluxes.
c     both flc and df are positive quantities

       do k=1,np+1
         do i=1,m
          flc(i,k)=flc(i,k)-df(i,k)
         enddo
       enddo

c-----identify top cloud-layer

        do i= 1, m
         nctop(i)=np+1
        enddo

       do k=1,np
        do i= 1, m

         if (fcld(i,k).gt.0.02 .and. nctop(i).eq.np+1) then
          nctop(i)=k
         endif

        enddo
       enddo

       do i= 1,m
        ntop=nctop(m)

c-----adjust fluxes above clouds following Eq. (6.17)

        do k= 1,ntop
         flx(i,k)=flx(i,k)-df(i,k)
        enddo

c-----adjust fluxes below cloud top following Eq. (6.18)

        if (ntop.lt.np+1) then
         do k= ntop+1,np+1
          df(i,k)=df(i,k)*(flx(i,k)/flc(i,k))
          flx(i,k)=flx(i,k)-df(i,k)
         enddo
        endif

       enddo

c-----adjustment for the direct downward flux

       do i= 1,m
        fdirir(i)=fdirir(i)-df(i,np+1)
        if (fdirir(i) .lt. 0.0) fdirir(i)=0.0
       enddo
          
      return
      end  

c******************************************************************

      subroutine soluv (wh,oh,dp,overcast,cldwater,
     *          cwp,taucld,reff,ict,icb,fcld,cosz,
     *          taual,ssaal,asyal,rsuvbm,rsuvdf,
     *          flx,flc,fdiruv,fdifuv,fdirpar,fdifpar)

c******************************************************************
c  compute solar fluxes in the uv+par region. the spectrum is
c  grouped into 8 bands:
c  
c              Band     Micrometer
c
c       UV-C    1.     .175 - .225
c               2.     .225 - .245
c                      .260 - .280
c               3.     .245 - .260
c
c       UV-B    4.     .280 - .295
c               5.     .295 - .310
c               6.     .310 - .320
c      
c       UV-A    7.     .320 - .400
c      
c       PAR     8.     .400 - .700
c
c----- Input parameters:                            units      size
c
c  number of soundings (m)                          n/d         1
c  number of atmospheric layers (np)                n/d         1
c  layer scaled-water vapor content (wh)          gm/cm^2      m*np
c  layer ozone content (oh)                      (cm-atm)stp   m*np
c  layer pressure thickness (dp)                    mb         m*np
c  option for scaling cloud optical thickness       n/d         1
c        overcast="true" if scaling is NOT required
c        overcast="fasle" if scaling is required
c  input option for cloud optical thickness         n/d         1
c        cldwater="true" if taucld is provided
c        cldwater="false" if cwp is provided
c  cloud water amount (cwp)                        gm/m**2     m*np*3
c        index 1 for ice particles
c        index 2 for liquid drops
c        index 3 for rain drops
c  cloud optical thickness (taucld)                 n/d        m*np*3
c       index 1 for ice particles
c       index 2 for liquid drops
c       index 3 for rain drops
c  effective cloud-particle size (reff)          micrometer    m*np*3
c       index 1 for ice paticles
c       index 2 for liquid drops
c       index 3 for rain drops
c  level index separating high and                  n/d        m
c       middle clouds (ict)
c  level indiex separating middle and               n/d        m
c       low clouds (icb)
c  cloud amount (fcld)                            fraction     m*np
c  cosine of solar zenith angle (cosz)              n/d        m
c  aerosol optical thickness (taual)                n/d        m*np*11
c  aerosol single-scattering albedo (ssaal)         n/d        m*np*11
c  aerosol asymmetry factor (asyal)                 n/d        m*np*11
c  uv+par surface albedo for beam                 fraction     m
c       radiation (rsuvbm)
c  uv+par surface albedo for diffuse              fraction     m
c       radiation (rsuvdf)
c
c---- temporary array
c
c  scaled cloud optical thickness                   n/d        m*np
c       for beam radiation (tauclb)
c  scaled cloud optical thickness                   n/d        m*np
c       for diffuse radiation  (tauclf)     
c
c----- output (updated) parameters:
c
c  all-sky net downward flux (flx)               fraction      m*(np+1)
c  clear-sky net downward flux (flc)             fraction      m*(np+1)
c  all-sky direct downward uv flux at
c       the surface (fdiruv)                     fraction      m
c  all-sky diffuse downward uv flux at
c       the surface (fdifuv)                     fraction      m
c  all-sky direct downward par flux at
c       the surface (fdirpar)                    fraction      m
c  all-sky diffuse downward par flux at
c       the surface (fdifpar)                    fraction      m
c
c***********************************************************************

      implicit none
      integer np,m
      parameter(m=IM, np=KM) 
c
c-----input parameters

      integer ict,icb
      real taucld(m,np,3),reff(m,np,3),fcld(m,np)
      real cwp(m,np,3),wh(m,np),oh(m,np),dp(m,np)
      real taual(m,np,11),ssaal(m,np,11),asyal(m,np,11)
      real rsuvbm(m),rsuvdf(m),cosz(m)
      logical overcast,cldwater

c-----output (updated) parameter

      real flx(m,np+1),flc(m,np+1)
      real fdiruv (m),fdifuv (m)
      real fdirpar(m),fdifpar(m)

c-----static parameters

      integer nband
      parameter (nband=8)
      real hk(nband),wk(nband),zk(nband),ry(nband)
      real aig(3),awg(3),arg(3)
      real aib(2),awb(2),arb(2)

c-----temporary array

      integer i,k,ib
      integer ih1,ih2,im1,im2,is1,is2
      real dsm(m)
      real tauclb(m,np),tauclf(m,np),asycl(m,np)
      real taurs,tauoz,tauwv
      real tausto(m,np),ssatau(m,np),asysto(m,np)
      real tautob(m,np),ssatob(m,np),asytob(m,np)
      real tautof(m,np),ssatof(m,np),asytof(m,np)
      real taux,reff1,reff2,g1,g2,g3
      real rr(m,np+1,2),tt(m,np+1,2),td(m,np+1,2),
     *     rs(m,np+1,2),ts(m,np+1,2)
      real fall(m,np+1),fclr(m,np+1),fsdir(m),fsdif(m)
      real asyclt(m),cc(m,3)
      real rrt(m,np),ttt(m,np),tdt(m,np),rst(m,np),tst(m,np)
      real dum1(m,np+1),dum2(m),dum(m,np)

c-----hk is the fractional extra-terrestrial solar flux in each
c     of the 8 bands. the sum of hk is 0.47074. (Table 3)

      data hk/.00057, .00367, .00083, .00417,
     *        .00600, .00556, .05913, .39081/

c-----zk is the ozone absorption coefficient. unit: /(cm-atm)stp
c     (Table 3)

      data zk /30.47, 187.2,  301.9,   42.83,
     *         7.09,  1.25,   0.0345,  0.0572/

c-----wk is the water vapor absorption coefficient. unit: cm**2/g
c     (Table 3)

      data wk /7*0.0, 0.00075/

c-----ry is the extinction coefficient for Rayleigh scattering.
c     unit: /mb. (Table 3)

      data ry /.00604, .00170, .00222, .00132,
     *         .00107, .00091, .00055, .00012/ 

c-----coefficients for computing the extinction coefficients of ice, 
c     water, and rain particles, independent of spectral band. (Table 4)

      data aib/ 3.33e-4, 2.52/
c!!!!!!!!!!!!!!!!!!
      data awb/-6.59e-3, 1.65/
c      data awb/-6.59e-3, 1.5/
      data arb/ 3.07e-3, 0.00/

c-----coefficients for computing the asymmetry factor of ice, water,
c     and rain particles, independent of spectral band. (Table 6)

      data aig/.74625,.0010541,-.00000264/
      data awg/.82562,.0052900,-.00014866/
      data arg/.883,0.0,0.0/

c-----initialize fdiruv, fdifuv, surface reflectances and transmittances.
c     the reflectance and transmittance of the clear and cloudy portions
c     of a layer are denoted by 1 and 2, respectively.
c     cc is the maximum cloud cover in each of the high, middle, and low
c     cloud groups.
c     1/dsm=1/cos(53) = 1.66
            
      do i=1,m                    
         dsm(i)=0.602
         fdiruv(i)=0.0
         fdifuv(i)=0.0
         rr(i,np+1,1)=rsuvbm(i)
         rr(i,np+1,2)=rsuvbm(i)
         rs(i,np+1,1)=rsuvdf(i)
         rs(i,np+1,2)=rsuvdf(i)
         td(i,np+1,1)=0.0
         td(i,np+1,2)=0.0
         tt(i,np+1,1)=0.0
         tt(i,np+1,2)=0.0
         ts(i,np+1,1)=0.0
         ts(i,np+1,2)=0.0
         cc(i,1)=0.0
         cc(i,2)=0.0
         cc(i,3)=0.0
      enddo


c-----Compute cloud optical thickness.  Eqs. (4.6) and (4.11)

      if (cldwater) then

       do k=1,np
        do i=1,m
          taucld(i,k,1)=cwp(i,k,1)*(aib(1)+aib(2)/reff(i,k,1))
          taucld(i,k,2)=cwp(i,k,2)*(awb(1)+awb(2)/reff(i,k,2))
          taucld(i,k,3)=cwp(i,k,3)* arb(1)
        enddo
       enddo

      endif

c-----options for scaling cloud optical thickness

      if (overcast) then

       do k=1,np
        do i=1,m
          tauclb(i,k)=taucld(i,k,1)+taucld(i,k,2)+taucld(i,k,3)
          tauclf(i,k)=tauclb(i,k)
        enddo
       enddo

       do k=1,3
        do i=1,m
           cc(i,k)=1.0
        enddo
       enddo

      else

c-----scale cloud optical thickness in each layer from taucld (with
c     cloud amount fcld) to tauclb and tauclf (with cloud amount cc).
c     tauclb is the scaled optical thickness for beam radiation and
c     tauclf is for diffuse radiation (see section 7).

         call cldscale(m,np,cosz,fcld,taucld,ict,icb,
     *                 cc,tauclb,tauclf)

      endif

c-----cloud asymmetry factor for a mixture of liquid and ice particles.
c     unit of reff is micrometers. Eqs. (4.8) and (6.4)

      do k=1,np
       do i=1,m

           asyclt(i)=1.0
           taux=taucld(i,k,1)+taucld(i,k,2)+taucld(i,k,3)

         if (taux.gt.0.02 .and. fcld(i,k).gt.0.01) then

           reff1=min(reff(i,k,1),130.)
           reff2=min(reff(i,k,2),20.0)

           g1=(aig(1)+(aig(2)+aig(3)*reff1)*reff1)*taucld(i,k,1)
           g2=(awg(1)+(awg(2)+awg(3)*reff2)*reff2)*taucld(i,k,2)
           g3= arg(1)*taucld(i,k,3)
           asyclt(i)=(g1+g2+g3)/taux

         endif
       enddo

         do i=1,m
           asycl(i,k)=asyclt(i)
         enddo

      enddo

c-----integration over spectral bands

      do 100 ib=1,nband

       do k=1,np
        do i=1,m

c-----compute rayleigh, ozone and water vapor optical thicknesses

          taurs=ry(ib)*dp(i,k)
          tauoz=zk(ib)*oh(i,k)
          tauwv=wk(ib)*wh(i,k)

c-----compute clear-sky optical thickness, single scattering albedo,
c     and asymmetry factor (Eqs. 6.2-6.4)

          tausto(i,k)=taurs+tauoz+tauwv+taual(i,k,ib)+1.0e-8
          ssatau(i,k)=ssaal(i,k,ib)*taual(i,k,ib)+taurs
          asysto(i,k)=asyal(i,k,ib)*ssaal(i,k,ib)*taual(i,k,ib)

c-----compute reflectance and transmittance of the clear portion of a layer

          tautob(i,k)=tausto(i,k)
          ssatob(i,k)=ssatau(i,k)/tautob(i,k)+1.0e-8
          ssatob(i,k)=min(ssatob(i,k),0.999999)
          asytob(i,k)=asysto(i,k)/(ssatob(i,k)*tautob(i,k))

        enddo
       enddo

c-----for direct incident radiation

         call deledd (m,np,tautob,ssatob,asytob,cosz,rrt,ttt,tdt)

c-----diffuse incident radiation is approximated by beam radiation with
c     an incident angle of 53 degrees, Eqs. (6.5) and (6.6)

         call deledd (m,np,tautob,ssatob,asytob,dsm,rst,tst,dum)

       do k=1,np
        do i=1,m
           rr(i,k,1)=rrt(i,k)
           tt(i,k,1)=ttt(i,k)
           td(i,k,1)=tdt(i,k)
           rs(i,k,1)=rst(i,k)
           ts(i,k,1)=tst(i,k)
        enddo
       enddo

c-----compute reflectance and transmittance of the cloudy portion of a layer

       do k=1,np
        do i=1,m

c-----for direct incident radiation
c     The effective layer optical properties. Eqs. (6.2)-(6.4)

           tautob(i,k)=tausto(i,k)+tauclb(i,k)
           ssatob(i,k)=(ssatau(i,k)+tauclb(i,k))/tautob(i,k)+1.0e-8
           ssatob(i,k)=min(ssatob(i,k),0.999999)
           asytob(i,k)=(asysto(i,k)+asycl(i,k)*tauclb(i,k))
     *                /(ssatob(i,k)*tautob(i,k))

c-----for diffuse incident radiation

           tautof(i,k)=tausto(i,k)+tauclf(i,k)
           ssatof(i,k)=(ssatau(i,k)+tauclf(i,k))/tautof(i,k)+1.0e-8
           ssatof(i,k)=min(ssatof(i,k),0.999999)
           asytof(i,k)=(asysto(i,k)+asycl(i,k)*tauclf(i,k))
     *                /(ssatof(i,k)*tautof(i,k))

        enddo
       enddo

c-----for direct incident radiation
c     note that the cloud optical thickness is scaled differently for direct
c     and diffuse insolation, Eqs. (7.3) and (7.4).

         call deledd (m,np,tautob,ssatob,asytob,cosz,rrt,ttt,tdt)

c-----diffuse incident radiation is approximated by beam radiation with
c     an incident angle of 53 degrees, Eqs. (6.5) and (6.6)

         call deledd (m,np,tautof,ssatof,asytof,dsm,rst,tst,dum)

       do k=1,np
        do i=1,m

           rr(i,k,2)=rrt(i,k)
           tt(i,k,2)=ttt(i,k)
           td(i,k,2)=tdt(i,k)
           rs(i,k,2)=rst(i,k)
           ts(i,k,2)=tst(i,k)

        enddo
       enddo

c-----flux calculations

c     initialize clear-sky flux (fclr), all-sky flux (fall), 
c     and surface downward fluxes (fsdir and fsdif)

        do k=1,np+1
         do i=1,m
           fclr(i,k)=0.0
           fall(i,k)=0.0
         enddo
        enddo

        do i=1,m
           fsdir(i)=0.0
           fsdif(i)=0.0
        enddo

        if (overcast) then

              ih1=1
              ih2=1
              im1=1
              im2=1
              is1=1
              is2=1

c-----for clear-sky fluxes only

         call cldflx (ict,icb,ih1,ih2,im1,im2,is1,is2,
     *                cc,rr,tt,td,rs,ts,fclr,dum1,dum2,dum2)
             

           do k=1,np+1
            do i=1,m
              fall(i,k)=0.0
            enddo
           enddo

           do i=1,m
              fsdir(i)=0.0
              fsdif(i)=0.0
           enddo

              ih1=2
              ih2=2
              im1=2
              im2=2
              is1=2
              is2=2

c-----for cloudy-sky fluxes only

         call cldflx (ict,icb,ih1,ih2,im1,im2,is1,is2,
     *                cc,rr,tt,td,rs,ts,dum1,fall,fsdir,fsdif)

        else

              ih1=1
              ih2=2
              im1=1
              im2=2
              is1=1
              is2=2

c-----for clear- and all-sky fluxes
c     the all-sky flux, fall is the summation inside the brackets
c     of Eq. (7.11)

         call cldflx (ict,icb,ih1,ih2,im1,im2,is1,is2,
     *                cc,rr,tt,td,rs,ts,fclr,fall,fsdir,fsdif)

        endif

c-----flux integration, Eq. (6.1)

       do k=1,np+1
        do i=1,m
          flx(i,k)=flx(i,k)+fall(i,k)*hk(ib)
        enddo

        do i=1,m
          flc(i,k)=flc(i,k)+fclr(i,k)*hk(ib)
        enddo
       enddo

c-----compute direct and diffuse downward surface fluxes in the UV
c     and par regions

       if(ib.lt.8) then

        do i=1,m
          fdiruv(i)=fdiruv(i)+fsdir(i)*hk(ib)
          fdifuv(i)=fdifuv(i)+fsdif(i)*hk(ib)
        enddo

       else

        do i=1,m
          fdirpar(i)=fsdir(i)*hk(ib)
          fdifpar(i)=fsdif(i)*hk(ib)
        enddo

       endif

 100  continue

      return
      end

c***********************************************************************

      subroutine solir (wh,dp,overcast,cldwater,
     *                  cwp,taucld,reff,ict,icb,fcld,cosz,
     *                  taual,ssaal,asyal,
     *                  rsirbm,rsirdf,flx,flc,fdirir,fdifir)
 
c************************************************************************
c  compute solar flux in the infrared region. The spectrum is divided
c   into three bands:
c
c          band   wavenumber(/cm)  wavelength (micron)
c          1( 9)    14280-8200         0.70-1.22
c          2(10)     8200-4400         1.22-2.27
c          3(11)     4400-1000         2.27-10.0
c
c----- Input parameters:                            units      size
c
c  number of soundings (m)                          n/d         1
c  number of atmospheric layers (np)                n/d         1
c  layer scaled-water vapor content (wh)          gm/cm^2      m*np
c  option for scaling cloud optical thickness       n/d         1
c        overcast="true" if scaling is NOT required
c        overcast="fasle" if scaling is required
c  input option for cloud optical thickness         n/d         1
c        cldwater="true" if taucld is provided
c        cldwater="false" if cwp is provided
c  cloud water concentration (cwp)                gm/m**2      m*np*3
c        index 1 for ice particles
c        index 2 for liquid drops
c        index 3 for rain drops
c  cloud optical thickness (taucld)                 n/d        m*np*3
c        index 1 for ice paticles
c        index 2 for liquid drops
c        index 3 for rain drops
c  effective cloud-particle size (reff)           micrometer   m*np*3
c        index 1 for ice paticles
c        index 2 for liquid drops
c        index 3 for rain drops
c  level index separating high and                  n/d        m
c        middle clouds (ict)
c  level index separating middle and                n/d        m
c        low clouds (icb)
c  cloud amount (fcld)                            fraction     m*np
c  aerosol optical thickness (taual)                n/d        m*np*11
c  aerosol single-scattering albedo (ssaal)         n/d        m*np*11
c  aerosol asymmetry factor (asyal)                 n/d        m*np*11 
c  near ir surface albedo for beam                fraction     m
c        radiation (rsirbm)
c  near ir surface albedo for diffuse             fraction     m
c        radiation (rsirdf)
c
c---- temporary array
c
c  scaled cloud optical thickness                   n/d        m*np
c          for beam radiation (tauclb)
c  scaled cloud optical thickness                   n/d        m*np
c          for diffuse radiation  (tauclf)     
c
c----- output (updated) parameters:
c
c  all-sky flux (downward-upward) (flx)           fraction     m*(np+1)
c  clear-sky flux (downward-upward) (flc)         fraction     m*(np+1)
c  all-sky direct downward ir flux at
c          the surface (fdirir)                   fraction     m
c  all-sky diffuse downward ir flux at
c          the surface (fdifir)                   fraction     m
c
c**********************************************************************
      implicit none
      integer np,m
      parameter(m=IM, np=KM) 
c
c-----input parameters

      integer ict,icb
      integer ih1,ih2,im1,im2,is1,is2
      real cwp(m,np,3),taucld(m,np,3),reff(m,np,3)
      real fcld(m,np),cosz(m)
      real rsirbm(m),rsirdf(m)
      real taual(m,np,11),ssaal(m,np,11),asyal(m,np,11)
      real dp(m,np),wh(m,np)
      logical overcast,cldwater

c-----output (updated) parameters

      real flx(m,np+1),flc(m,np+1)
      real fdirir(m),fdifir(m)

c-----static parameters

      integer nk,nband
      parameter (nk=10,nband=3)
      real hk(nband,nk),xk(nk),ry(nband)
      real aib(nband,2),awb(nband,2),arb(nband,2)
      real aia(nband,3),awa(nband,3),ara(nband,3)
      real aig(nband,3),awg(nband,3),arg(nband,3)

c-----temporary array

      integer ib,iv,ik,i,k
      real dsm(m)
      real tauclb(m,np),tauclf(m,np),cc(m,3)
      real ssacl(m,np),asycl(m,np)
      real rr(m,np+1,2),tt(m,np+1,2),td(m,np+1,2),
     *     rs(m,np+1,2),ts(m,np+1,2)
      real fall(m,np+1),fclr(m,np+1),fsdir(m),fsdif(m)

      real taurs,tauwv
      real tausto(m,np),ssatau(m,np),asysto(m,np)
      real tautob(m,np),ssatob(m,np),asytob(m,np)
      real tautof(m,np),ssatof(m,np),asytof(m,np)
      real taux,reff1,reff2,w1,w2,w3,g1,g2,g3
      real ssaclt(m),asyclt(m)
      real rrt(m,np),ttt(m,np),tdt(m,np),rst(m,np),tst(m,np)
      real dum1(m,np+1),dum2(m),dum(m,np)

c-----water vapor absorption coefficient for 10 k-intervals.
c     unit: cm^2/gm (Table 2)

      data xk/            
     1  0.0010, 0.0133, 0.0422, 0.1334, 0.4217,            
     2  1.334,  5.623,  31.62,  177.8,  1000.0/  

c-----water vapor k-distribution function,
c     the sum of hk is 0.52926. unit: fraction (Table 2)

      data hk/
     1 .20673,.08236,.01074,  .03497,.01157,.00360,
     2 .03011,.01133,.00411,  .02260,.01143,.00421,
     3 .01336,.01240,.00389,  .00696,.01258,.00326,
     4 .00441,.01381,.00499,  .00115,.00650,.00465,
     5 .00026,.00244,.00245,  .00000,.00094,.00145/

c-----ry is the extinction coefficient for Rayleigh scattering.
c     unit: /mb (Table 3)

      data ry /.0000156, .0000018, .000000/

c-----coefficients for computing the extinction coefficients of
c     ice, water, and rain particles (Table 4)

      data aib/
     1  .000333, .000333, .000333,
     2     2.52,    2.52,    2.52/

      data awb/
     1  -0.0101, -0.0166, -0.0339,
     2     1.72,    1.85,    2.16/

      data arb/
     1   0.00307, 0.00307, 0.00307,
     2   0.0    , 0.0    , 0.0    /

c-----coefficients for computing the single-scattering co-albedo of
c     ice, water, and rain particles (Table 5)

      data aia/
     1 -.00000260, .00215346, .08938331,
     2  .00000746, .00073709, .00299387,
     3  .00000000,-.00000134,-.00001038/

      data awa/
     1  .00000007,-.00019934, .01209318,
     2  .00000845, .00088757, .01784739,
     3 -.00000004,-.00000650,-.00036910/

      data ara/
     1  .029,      .342,      .466,
     2  .0000,     .000,      .000,
     3  .0000,     .000,      .000/

c-----coefficients for computing the asymmetry factor of 
c     ice, water, and rain particles (Table 6)

      data aig/
     1  .74935228, .76098937, .84090400,
     2  .00119715, .00141864, .00126222,
     3 -.00000367,-.00000396,-.00000385/

      data awg/
     1  .79375035, .74513197, .83530748,
     2  .00832441, .01370071, .00257181,
     3 -.00023263,-.00038203, .00005519/

      data arg/
     1  .891,      .948,      .971,
     2  .0000,     .000,      .000,
     3  .0000,     .000,      .000/

c-----initialize surface fluxes, reflectances, and transmittances.
c     the reflectance and transmittance of the clear and cloudy portions
c     of a layer are denoted by 1 and 2, respectively.
c     cc is the maximum cloud cover in each of the high, middle, and low
c     cloud groups.
c     1/dsm=1/cos(53)=1.66

      do i=1,m
         dsm(i)=0.602
         fdirir(i)=0.0
         fdifir(i)=0.0
         rr(i,np+1,1)=rsirbm(i)
         rr(i,np+1,2)=rsirbm(i)
         rs(i,np+1,1)=rsirdf(i)
         rs(i,np+1,2)=rsirdf(i)
         td(i,np+1,1)=0.0
         td(i,np+1,2)=0.0
         tt(i,np+1,1)=0.0
         tt(i,np+1,2)=0.0
         ts(i,np+1,1)=0.0
         ts(i,np+1,2)=0.0
         cc(i,1)=0.0
         cc(i,2)=0.0
         cc(i,3)=0.0
      enddo

c-----integration over spectral bands

      do 100 ib=1,nband

       iv=ib+8

c-----Compute cloud optical thickness. Eqs. (4.6) and (4.11)

      if (cldwater) then

       do k=1,np
        do i=1,m
          taucld(i,k,1)=cwp(i,k,1)*(aib(ib,1)
     *                    +aib(ib,2)/reff(i,k,1))
          taucld(i,k,2)=cwp(i,k,2)*(awb(ib,1)
     *                    +awb(ib,2)/reff(i,k,2))
          taucld(i,k,3)=cwp(i,k,3)*arb(ib,1)
        enddo
       enddo

      endif

c-----options for scaling cloud optical thickness

      if (overcast) then

       do k=1,np
        do i=1,m
          tauclb(i,k)=taucld(i,k,1)+taucld(i,k,2)+taucld(i,k,3)
          tauclf(i,k)=tauclb(i,k)
        enddo
       enddo

       do k=1,3
        do i=1,m
          cc(i,k)=1.0
        enddo
       enddo

      else

c-----scale cloud optical thickness in each layer from taucld (with
c     cloud amount fcld) to tauclb and tauclf (with cloud amount cc).
c     tauclb is the scaled optical thickness for beam radiation and
c     tauclf is for diffuse radiation.

       call cldscale(m,np,cosz,fcld,taucld,ict,icb,
     *              cc,tauclb,tauclf)

      endif

c-----compute cloud single scattering albedo and asymmetry factor
c     for a mixture of ice and liquid particles.
c     Eqs.(4.6)-(4.8), (6.2)-(6.4)

       do k=1,np

        do i=1,m

           ssaclt(i)=0.99999
           asyclt(i)=1.0

           taux=taucld(i,k,1)+taucld(i,k,2)+taucld(i,k,3)
          if (taux.gt.0.02 .and. fcld(i,k).gt.0.01) then

           reff1=min(reff(i,k,1),130.)
           reff2=min(reff(i,k,2),20.0)

           w1=(1.-(aia(ib,1)+(aia(ib,2)+
     *         aia(ib,3)*reff1)*reff1))*taucld(i,k,1)
           w2=(1.-(awa(ib,1)+(awa(ib,2)+
     *         awa(ib,3)*reff2)*reff2))*taucld(i,k,2)
           w3=(1.- ara(ib,1))*taucld(i,k,3)
           ssaclt(i)=(w1+w2+w3)/taux

           g1=(aig(ib,1)+(aig(ib,2)+aig(ib,3)*reff1)*reff1)*w1
           g2=(awg(ib,1)+(awg(ib,2)+awg(ib,3)*reff2)*reff2)*w2
           g3= arg(ib,1)*w3
           asyclt(i)=(g1+g2+g3)/(w1+w2+w3)

          endif

        enddo

         do i=1,m
           ssacl(i,k)=ssaclt(i)
         enddo
         do i=1,m
           asycl(i,k)=asyclt(i)
         enddo

       enddo

c-----integration over the k-distribution function

       do 200 ik=1,nk

        do k=1,np
         do i=1,m

           taurs=ry(ib)*dp(i,k)
           tauwv=xk(ik)*wh(i,k)
 
c-----compute clear-sky optical thickness, single scattering albedo,
c     and asymmetry factor. Eqs.(6.2)-(6.4)
 
           tausto(i,k)=taurs+tauwv+taual(i,k,iv)+1.0e-8
           ssatau(i,k)=ssaal(i,k,iv)*taual(i,k,iv)+taurs
           asysto(i,k)=asyal(i,k,iv)*ssaal(i,k,iv)*taual(i,k,iv)
 
c-----compute reflectance and transmittance of the clear portion of a layer

           tautob(i,k)=tausto(i,k)
           ssatob(i,k)=ssatau(i,k)/tautob(i,k)+1.0e-8
           ssatob(i,k)=min(ssatob(i,k),0.999999)
           asytob(i,k)=asysto(i,k)/(ssatob(i,k)*tautob(i,k))

         enddo
        enddo

c-----for direct incident radiation

         call deledd (m,np,tautob,ssatob,asytob,cosz,rrt,ttt,tdt)

c-----diffuse incident radiation is approximated by beam radiation with
c     an incident angle of 53 degrees, Eqs. (6.5) and (6.6)

         call deledd (m,np,tautob,ssatob,asytob,dsm,rst,tst,dum)

        do k=1,np
         do i=1,m

            rr(i,k,1)=rrt(i,k)
            tt(i,k,1)=ttt(i,k)
            td(i,k,1)=tdt(i,k)
            rs(i,k,1)=rst(i,k)
            ts(i,k,1)=tst(i,k)

         enddo
        enddo

c-----compute reflectance and transmittance of the cloudy portion of a layer

        do k=1,np
         do i=1,m

c-----for direct incident radiation. Eqs.(6.2)-(6.4)

           tautob(i,k)=tausto(i,k)+tauclb(i,k)
           ssatob(i,k)=(ssatau(i,k)+ssacl(i,k)*tauclb(i,k))
     *                /tautob(i,k)+1.0e-8
           ssatob(i,k)=min(ssatob(i,k),0.999999)
           asytob(i,k)=(asysto(i,k)+asycl(i,k)*ssacl(i,k)*tauclb(i,k))
     *                /(ssatob(i,k)*tautob(i,k))

c-----for diffuse incident radiation

           tautof(i,k)=tausto(i,k)+tauclf(i,k)
           ssatof(i,k)=(ssatau(i,k)+ssacl(i,k)*tauclf(i,k))
     *                /tautof(i,k)+1.0e-8
           ssatof(i,k)=min(ssatof(i,k),0.999999)
           asytof(i,k)=(asysto(i,k)+asycl(i,k)*ssacl(i,k)*tauclf(i,k))
     *                /(ssatof(i,k)*tautof(i,k))

         enddo
        enddo

c-----for direct incident radiation

          call deledd (m,np,tautob,ssatob,asytob,cosz,rrt,ttt,tdt)

c-----diffuse incident radiation is approximated by beam radiation with
c     an incident angle of 53 degrees, Eqs.(6.5) and (6.6)

          call deledd (m,np,tautof,ssatof,asytof,dsm,rst,tst,dum)

        do k=1,np
         do i=1,m

            rr(i,k,2)=rrt(i,k)
            tt(i,k,2)=ttt(i,k)
            td(i,k,2)=tdt(i,k)
            rs(i,k,2)=rst(i,k)
            ts(i,k,2)=tst(i,k)

         enddo
        enddo

c-----flux calculations

c-----initialize clear-sky flux (fclr), all-sky flux (fall), 
c     and surface downward fluxes (fsdir and fsdif)

        do k=1,np+1
         do i=1,m
           fclr(i,k)=0.0
           fall(i,k)=0.0
         enddo
        enddo

        do i=1,m
           fsdir(i)=0.0
           fsdif(i)=0.0
        enddo

        if (overcast) then

              ih1=1
              ih2=1
              im1=1
              im2=1
              is1=1
              is2=1

c-----for clear-sky fluxes only

         call cldflx (ict,icb,ih1,ih2,im1,im2,is1,is2,
     *                cc,rr,tt,td,rs,ts,fclr,dum1,dum2,dum2)

           do k=1,np+1
            do i=1,m
              fall(i,k)=0.0
            enddo
           enddo

           do i=1,m
              fsdir(i)=0.0
              fsdif(i)=0.0
           enddo
              ih1=2
              ih2=2
              im1=2
              im2=2
              is1=2
              is2=2

c-----for cloudy-sky fluxes only

         call cldflx (ict,icb,ih1,ih2,im1,im2,is1,is2,
     *                cc,rr,tt,td,rs,ts,dum1,fall,fsdir,fsdif)

        else

              ih1=1
              ih2=2
              im1=1
              im2=2
              is1=1
              is2=2

c-----for clear- and all-sky fluxes
c     the all-sky flux, fall is the summation inside the brackets
c     of Eq. (7.11)

         call cldflx (ict,icb,ih1,ih2,im1,im2,is1,is2,
     *                cc,rr,tt,td,rs,ts,fclr,fall,fsdir,fsdif)

        endif

c-----flux integration following Eq. (6.1)

       do k=1,np+1
        do i=1,m
          flx(i,k) = flx(i,k)+fall(i,k)*hk(ib,ik)
        enddo

        do i=1,m
          flc(i,k) = flc(i,k)+fclr(i,k)*hk(ib,ik)
        enddo
       enddo

c-----compute downward surface fluxes in the ir region

       do i=1,m
          fdirir(i) = fdirir(i)+fsdir(i)*hk(ib,ik)
          fdifir(i) = fdifir(i)+fsdif(i)*hk(ib,ik)
       enddo

  200 continue
  100 continue
 
      return
      end

c********************************************************************

      subroutine cldscale (m,np,cosz,fcld,taucld,ict,icb,
     *                     cc,tauclb,tauclf)

c********************************************************************
c
c   This subroutine computes the high, middle, and low cloud
c    amounts and scales the cloud optical thickness (section 7)
c
c   To simplify calculations in a cloudy atmosphere, clouds are
c    grouped into high, middle and low clouds separated by the levels
c    ict and icb (level 1 is the top of the model atmosphere).
c
c   Within each of the three groups, clouds are assumed maximally
c    overlapped, and the cloud cover (cc) of a group is the maximum
c    cloud cover of all the layers in the group.  The optical thickness
c    (taucld) of a given layer is then scaled to new values (tauclb and
c    tauclf) so that the layer reflectance corresponding to the cloud
c    cover cc is the same as the original reflectance with optical
c    thickness taucld and cloud cover fcld.
c
c---input parameters
c
c    number of atmospheric soundings (m)
c    number of atmospheric layers (np)
c    cosine of the solar zenith angle (cosz)
c    fractional cloud cover (fcld)
c    cloud optical thickness (taucld)
c    index separating high and middle clouds (ict)
c    index separating middle and low clouds (icb)
c
c---output parameters
c
c    fractional cover of high, middle, and low cloud groups (cc)
c    scaled cloud optical thickness for direct  radiation (tauclb)
c    scaled cloud optical thickness for diffuse radiation (tauclf)
c
c********************************************************************

      implicit none

c-----input parameters

      integer m,np,ict,icb
      real cosz(m),fcld(m,np),taucld(m,np,3)

c-----output parameters

      real cc(m,3),tauclb(m,np),tauclf(m,np)

c-----temporary variables

      integer i,j,k,im,it,ia,kk
      real  fm,ft,fa,xai,taux

c-----pre-computed table
c     size of cosz-interval:         dm
c     size of taucld-interval:       dt
c     size of cloud amount-interval: da

      integer   nm,nt,na
      parameter (nm=11,nt=9,na=11) 
      real  dm,dt,da,t1,caib(nm,nt,na),caif(nt,na)
      parameter (dm=0.1,dt=0.30103,da=0.1,t1=-0.9031)

c-----include the pre-computed table of mcai for scaling the cloud optical
c     thickness under the assumption that clouds are maximally overlapped
c
c     caib is for scaling the cloud optical thickness for direct radiation
c     caif is for scaling the cloud optical thickness for diffuse radiation

      include "mcai.data"

c-----clouds within each of the high, middle, and low clouds are assumed
c     to be maximally overlapped, and the cloud cover (cc) for a group
c     (high, middle, or low) is the maximum cloud cover of all the layers
c     within a group

      do i=1,m
         cc(i,1)=0.0
         cc(i,2)=0.0
         cc(i,3)=0.0
      enddo

      do k=1,ict-1
       do i=1,m
          cc(i,1)=max(cc(i,1),fcld(i,k))
       enddo
      enddo

      do k=ict,icb-1
       do i=1,m
          cc(i,2)=max(cc(i,2),fcld(i,k))
       enddo
      enddo

      do k=icb,np
       do i=1,m
          cc(i,3)=max(cc(i,3),fcld(i,k))
       enddo
      enddo

c-----scale the cloud optical thickness.
c     taucld(i,k,1) is the optical thickness for ice particles
c     taucld(i,k,2) is the optical thickness for liquid particles
c     taucld(i,k,3) is the optical thickness for rain drops
      
      do k=1,np

         if(k.lt.ict) then
            kk=1
         elseif(k.ge.ict .and. k.lt.icb) then
            kk=2
         else
            kk=3
         endif

       do i=1,m

         tauclb(i,k) = 0.0
         tauclf(i,k) = 0.0
         taux=taucld(i,k,1)+taucld(i,k,2)+taucld(i,k,3)

         if (taux.gt.0.02 .and. fcld(i,k).gt.0.01) then

c-----normalize cloud cover following Eq. (7.8)

           fa=fcld(i,k)/cc(i,kk)

c-----table look-up

           taux=min(taux,32.)

           fm=cosz(i)/dm
           ft=(log10(taux)-t1)/dt
           fa=fa/da
 
           im=int(fm+1.5)
           it=int(ft+1.5)
           ia=int(fa+1.5)
  
           im=max(im,2)
           it=max(it,2)
           ia=max(ia,2)
     
           im=min(im,nm-1)
           it=min(it,nt-1)
           ia=min(ia,na-1)

           fm=fm-float(im-1)
           ft=ft-float(it-1)
           fa=fa-float(ia-1)

c-----scale cloud optical thickness for beam radiation following Eq. (7.3)
c     the scaling factor, xai, is a function of the solar zenith
c     angle, optical thickness, and cloud cover.
 
           xai=    (-caib(im-1,it,ia)*(1.-fm)+
     *      caib(im+1,it,ia)*(1.+fm))*fm*.5+caib(im,it,ia)*(1.-fm*fm)
         
           xai=xai+(-caib(im,it-1,ia)*(1.-ft)+
     *      caib(im,it+1,ia)*(1.+ft))*ft*.5+caib(im,it,ia)*(1.-ft*ft)

           xai=xai+(-caib(im,it,ia-1)*(1.-fa)+
     *     caib(im,it,ia+1)*(1.+fa))*fa*.5+caib(im,it,ia)*(1.-fa*fa)

           xai= xai-2.*caib(im,it,ia)
           xai=max(xai,0.0)
     
           tauclb(i,k) = taux*xai

c-----scale cloud optical thickness for diffuse radiation following Eq. (7.4)
c     the scaling factor, xai, is a function of the cloud optical
c     thickness and cover but not the solar zenith angle.

           xai=    (-caif(it-1,ia)*(1.-ft)+
     *      caif(it+1,ia)*(1.+ft))*ft*.5+caif(it,ia)*(1.-ft*ft)

           xai=xai+(-caif(it,ia-1)*(1.-fa)+
     *      caif(it,ia+1)*(1.+fa))*fa*.5+caif(it,ia)*(1.-fa*fa)

           xai= xai-caif(it,ia)
           xai=max(xai,0.0)
     
           tauclf(i,k) = taux*xai

         endif

       enddo
      enddo

      return
      end

c*********************************************************************

      subroutine deledd(m,np,tau,ssc,g0,cza,rr,tt,td)

c*********************************************************************
c
c-----uses the delta-eddington approximation to compute the
c     bulk scattering properties of a single layer
c     coded following King and Harshvardhan (JAS, 1986)
c
c  inputs:
c       m:  number of soundings
c      np:  number of atmospheric layers
c     tau:  optical thickness
c     ssc:  single scattering albedo
c     g0:   asymmetry factor
c     cza:  cosine of the zenith angle
c
c  outputs:
c
c     rr:  reflection of the direct beam
c     tt:  total diffuse transmission of the direct beam
c     td:  direct transmission of the direct beam
c
c*********************************************************************

      implicit none

      real zero,one,two,three,four,fourth,seven,thresh
      parameter (one =1., three=3.)
      parameter (two =2., seven=7.)
      parameter (four=4., fourth=.25)
      parameter (zero=0., thresh=1.e-8)

c-----input parameters
      integer m,np
      real tau(m,np),ssc(m,np),g0(m,np),cza(m)

c-----output parameters
      real rr(m,np+1),tt(m,np+1),td(m,np+1)

c-----temporary parameters

      integer i,k
      real zth,ff,xx,taup,sscp,gp,gm1,gm2,gm3,akk,alf1,alf2,
     *     all,bll,st7,st8,cll,dll,fll,ell,st1,st2,st3,st4
 
c---------------------------------------------------------------------

      do k=1,np
       do i=1,m

          zth = cza(i)
 
c  delta-eddington scaling of single scattering albedo,
c  optical thickness, and asymmetry factor,
c  K & H eqs(27-29)

           ff  = g0(i,k)*g0(i,k)
           xx  = one-ff *ssc(i,k)
           taup= tau(i,k)*xx
           sscp= ssc(i,k)*(one-ff)/xx
           gp  = g0(i,k) /(one+g0(i,k))
 
c  gamma1, gamma2, and gamma3. see table 2 and eq(26) K & H
c  ssc and gp are the d-s single scattering
c  albedo and asymmetry factor.

           xx  =  three*gp 
           gm1 =  (seven - sscp*(four+xx))*fourth
           gm2 = -(one   - sscp*(four-xx))*fourth

c  akk is k as defined in eq(25) of K & H
 
           akk = sqrt((gm1+gm2)*(gm1-gm2))
 
           xx  = akk * zth
           st7 = one - xx
           st8 = one + xx
           st3 = st7 * st8

           if (abs(st3) .lt. thresh) then
               zth = zth + 0.001
               xx  = akk * zth
               st7 = one - xx
               st8 = one + xx
               st3 = st7 * st8
           endif

c  extinction of the direct beam transmission
 
           td(i,k)  = exp(-taup/zth)

c  alf1 and alf2 are alpha1 and alpha2 from eqs (23) & (24) of K & H
 
           gm3  = (two - zth*three*gp)*fourth
           xx   = gm1 - gm2
           alf1 = gm1 - gm3 * xx
           alf2 = gm2 + gm3 * xx
 
c  all is last term in eq(21) of K & H
c  bll is last term in eq(22) of K & H
 
           xx  = akk * two
           all = (gm3 - alf2 * zth    )*xx*td(i,k)
           bll = (one - gm3 + alf1*zth)*xx
 
           xx  = akk * gm3
           cll = (alf2 + xx) * st7
           dll = (alf2 - xx) * st8
 
           xx  = akk * (one-gm3)
           fll = (alf1 + xx) * st8
           ell = (alf1 - xx) * st7
  
           st2 = exp(-akk*taup)
           st4 = st2 * st2
 
           st1 =  sscp / ((akk+gm1 + (akk-gm1)*st4) * st3)
 
c  rr is r-hat of eq(21) of K & H
c  tt is diffuse part of t-hat of eq(22) of K & H
 
           rr(i,k) =   ( cll-dll*st4         -all*st2)*st1
           tt(i,k) = - ((fll-ell*st4)*td(i,k)-bll*st2)*st1
 
           rr(i,k) = max(rr(i,k),zero)
           tt(i,k) = max(tt(i,k),zero)

           tt(i,k) = tt(i,k)+td(i,k)

        enddo
       enddo

      return
      end

c*******************************************************************

      subroutine cldflx (ict,icb,ih1,ih2,im1,im2,is1,is2,
     *           cc,rr,tt,td,rs,ts,fclr,fall,fsdir,fsdif)

c*******************************************************************
c  compute upward and downward fluxes using a two-stream adding method
c  following equations (6.9)-(6.16).
c
c  clouds are grouped into high, middle, and low clouds which are assumed
c  randomly overlapped. It involves a maximum of 8 sets of calculations.
c  In each set of calculations, each atmospheric layer is homogeneous,
c  either totally filled with clouds or without clouds.

c  input parameters:
c
c   m:   number of soundings
c   np:  number of atmospheric layers
c   ict: the level separating high and middle clouds
c   icb: the level separating middle and low clouds
c   ih1,ih2,im1,im2,is1,is2: indices for three group of clouds
c   cc:  effective cloud covers for high, middle and low clouds
c   rr:  reflection of a layer illuminated by beam radiation
c   tt:  total diffuse transmission of a layer illuminated by beam radiation
c   td:  direct beam transmission
c   rs:  reflection of a layer illuminated by diffuse radiation
c   ts:  transmission of a layer illuminated by diffuse radiation
c
c  output parameters:
c
c     fclr:  clear-sky flux (downward minus upward)
c     fall:  all-sky flux (downward minus upward)
c     fsdir: surface direct downward flux
c     fsdif: surface diffuse downward flux
c
c*********************************************************************c

      implicit none
      integer np,m
      parameter (m=IM, np=KM) 
c
c-----input parameters

      integer ict,icb,ih1,ih2,im1,im2,is1,is2

      real rr(m,np+1,2),tt(m,np+1,2),td(m,np+1,2)
      real rs(m,np+1,2),ts(m,np+1,2)
      real cc(m,3)

c-----temporary array

      integer i,k,ih,im,is
      real rra(m,np+1,2,2),tta(m,np+1,2,2),tda(m,np+1,2,2)
      real rsa(m,np+1,2,2),rxa(m,np+1,2,2)
      real ch(m),cm(m),ct(m),flxdn(m,np+1)
      real fdndir(m),fdndif(m),fupdif
      real denm,xx,yy

c-----output parameters

      real fclr(m,np+1),fall(m,np+1)
      real fsdir(m),fsdif(m)

c-----compute transmittances and reflectances for a composite of
c     layers. layers are added one at a time, going down from the top.
c     tda is the composite transmittance illuminated by beam radiation
c     tta is the composite total transmittance illuminated by
c         beam radiation
c     rsa is the composite reflectance illuminated from below
c         by diffuse radiation
c     tta and rsa are computed from Eqs. (6.10) and (6.12)

c-----for high clouds
c     ih=1 for clear-sky condition, ih=2 for cloudy-sky condition

      do ih=ih1,ih2

       do i=1,m
          tda(i,1,ih,1)=td(i,1,ih)
          tta(i,1,ih,1)=tt(i,1,ih)
          rsa(i,1,ih,1)=rs(i,1,ih)
          tda(i,1,ih,2)=td(i,1,ih)
          tta(i,1,ih,2)=tt(i,1,ih)
          rsa(i,1,ih,2)=rs(i,1,ih)
       enddo

       do k=2,ict-1
        do i=1,m
          denm = ts(i,k,ih)/( 1.-rsa(i,k-1,ih,1)*rs(i,k,ih))
          tda(i,k,ih,1)= tda(i,k-1,ih,1)*td(i,k,ih)
          tta(i,k,ih,1)= tda(i,k-1,ih,1)*tt(i,k,ih)
     *          +(tda(i,k-1,ih,1)*rsa(i,k-1,ih,1)*rr(i,k,ih)
     *          +tta(i,k-1,ih,1)-tda(i,k-1,ih,1))*denm
          rsa(i,k,ih,1)= rs(i,k,ih)+ts(i,k,ih)
     *                  *rsa(i,k-1,ih,1)*denm
          tda(i,k,ih,2)= tda(i,k,ih,1)
          tta(i,k,ih,2)= tta(i,k,ih,1)
          rsa(i,k,ih,2)= rsa(i,k,ih,1)
        enddo

       enddo

c-----for middle clouds
c     im=1 for clear-sky condition, im=2 for cloudy-sky condition
      do im=im1,im2

       do k=ict,icb-1
        do i=1,m
          denm = ts(i,k,im)/( 1.-rsa(i,k-1,ih,im)*rs(i,k,im))
          tda(i,k,ih,im)= tda(i,k-1,ih,im)*td(i,k,im)
          tta(i,k,ih,im)= tda(i,k-1,ih,im)*tt(i,k,im)
     *         +(tda(i,k-1,ih,im)*rsa(i,k-1,ih,im)*rr(i,k,im)
     *         +tta(i,k-1,ih,im)-tda(i,k-1,ih,im))*denm
          rsa(i,k,ih,im)= rs(i,k,im)+ts(i,k,im)
     *                  *rsa(i,k-1,ih,im)*denm
        enddo
       enddo

      enddo                 ! end im loop
      enddo                 ! end ih loop



c-----layers are added one at a time, going up from the surface.
c     rra is the composite reflectance illuminated by beam radiation
c     rxa is the composite reflectance illuminated from above
c         by diffuse radiation
c     rra and rxa are computed from Eqs. (6.9) and (6.11)
 
c-----for the low clouds
c     is=1 for clear-sky condition, is=2 for cloudy-sky condition

      do is=is1,is2

       do i=1,m
         rra(i,np+1,1,is)=rr(i,np+1,is)
         rxa(i,np+1,1,is)=rs(i,np+1,is)
         rra(i,np+1,2,is)=rr(i,np+1,is)
         rxa(i,np+1,2,is)=rs(i,np+1,is)
       enddo

       do k=np,icb,-1
        do i=1,m
          denm=ts(i,k,is)/( 1.-rs(i,k,is)*rxa(i,k+1,1,is) )
          rra(i,k,1,is)=rr(i,k,is)+(td(i,k,is)*rra(i,k+1,1,is)
     *        +(tt(i,k,is)-td(i,k,is))*rxa(i,k+1,1,is))*denm
          rxa(i,k,1,is)= rs(i,k,is)+ts(i,k,is)
     *        *rxa(i,k+1,1,is)*denm
          rra(i,k,2,is)=rra(i,k,1,is)
          rxa(i,k,2,is)=rxa(i,k,1,is)
        enddo
       enddo

c-----for middle clouds

      do im=im1,im2

       do k=icb-1,ict,-1
        do i=1,m
          denm=ts(i,k,im)/( 1.-rs(i,k,im)*rxa(i,k+1,im,is) )
          rra(i,k,im,is)= rr(i,k,im)+(td(i,k,im)*rra(i,k+1,im,is)
     *        +(tt(i,k,im)-td(i,k,im))*rxa(i,k+1,im,is))*denm
          rxa(i,k,im,is)= rs(i,k,im)+ts(i,k,im)
     *        *rxa(i,k+1,im,is)*denm
        enddo
       enddo

      enddo                 ! end im loop
      enddo                 ! end is loop

c-----integration over eight sky situations.
c     ih, im, is denotes high, middle and low cloud groups.

      do ih=ih1,ih2

c-----clear portion 

         if(ih.eq.1) then
           do i=1,m
             ch(i)=1.0-cc(i,1)
           enddo

          else

c-----cloudy portion

           do i=1,m
             ch(i)=cc(i,1)
           enddo

          endif

      do im=im1,im2

c-----clear portion

         if(im.eq.1) then

           do i=1,m
              cm(i)=ch(i)*(1.0-cc(i,2))
           enddo

         else

c-----cloudy portion

           do i=1,m
              cm(i)=ch(i)*cc(i,2) 
           enddo

         endif

      do is=is1,is2

c-----clear portion

         if(is.eq.1) then

           do i=1,m
             ct(i)=cm(i)*(1.0-cc(i,3)) 
           enddo

         else

c-----cloudy portion

           do i=1,m
             ct(i)=cm(i)*cc(i,3)
           enddo

         endif

c-----add one layer at a time, going down.

       do k=icb,np
        do i=1,m
          denm = ts(i,k,is)/( 1.-rsa(i,k-1,ih,im)*rs(i,k,is) )
          tda(i,k,ih,im)= tda(i,k-1,ih,im)*td(i,k,is)
          tta(i,k,ih,im)=  tda(i,k-1,ih,im)*tt(i,k,is)
     *         +(tda(i,k-1,ih,im)*rr(i,k,is)
     *         *rsa(i,k-1,ih,im)+tta(i,k-1,ih,im)-tda(i,k-1,ih,im))*denm
          rsa(i,k,ih,im)= rs(i,k,is)+ts(i,k,is)
     *         *rsa(i,k-1,ih,im)*denm
        enddo
       enddo

c-----add one layer at a time, going up.

       do k=ict-1,1,-1
        do i=1,m
          denm =ts(i,k,ih)/(1.-rs(i,k,ih)*rxa(i,k+1,im,is))
          rra(i,k,im,is)= rr(i,k,ih)+(td(i,k,ih)*rra(i,k+1,im,is)
     *        +(tt(i,k,ih)-td(i,k,ih))*rxa(i,k+1,im,is))*denm
          rxa(i,k,im,is)= rs(i,k,ih)+ts(i,k,ih)
     *        *rxa(i,k+1,im,is)*denm
        enddo
       enddo

c-----compute fluxes following Eq. (6.15) for fupdif and
c     Eq. (6.16) for (fdndir+fdndif)
 
c     fdndir is the direct  downward flux
c     fdndif is the diffuse downward flux
c     fupdif is the diffuse upward flux

      do k=2,np+1
       do i=1,m
         denm= 1./(1.-rsa(i,k-1,ih,im)*rxa(i,k,im,is))
         fdndir(i)= tda(i,k-1,ih,im)
         xx= tda(i,k-1,ih,im)*rra(i,k,im,is)
         yy= tta(i,k-1,ih,im)-tda(i,k-1,ih,im)
         fdndif(i)= (xx*rsa(i,k-1,ih,im)+yy)*denm
         fupdif= (xx+yy*rxa(i,k,im,is))*denm
         flxdn(i,k)= fdndir(i)+fdndif(i)-fupdif
       enddo
      enddo

       do i=1,m
         flxdn(i,1)=1.0-rra(i,1,im,is)
       enddo

c-----summation of fluxes over all sky situations;
c     the term in the brackets of Eq. (7.11)

       do k=1,np+1
        do i=1,m
           if(ih.eq.1 .and. im.eq.1 .and. is.eq.1) then
             fclr(i,k)=flxdn(i,k)
           endif
             fall(i,k)=fall(i,k)+flxdn(i,k)*ct(i)
        enddo
       enddo

        do i=1,m
            fsdir(i)=fsdir(i)+fdndir(i)*ct(i)
            fsdif(i)=fsdif(i)+fdndif(i)*ct(i)
        enddo

       enddo                 ! end is loop
       enddo                 ! end im loop
       enddo                 ! end ih loop

      return
      end

c*****************************************************************

      subroutine rflx(m,np,swc,u1,du,nu,swh,w1,dw,nw,tbl,df)

c*****************************************************************

c-----compute the reduction of clear-sky downward solar flux
c     due to co2 absorption.

      implicit none

c-----input parameters

      integer m,np,nu,nw
      real u1,du,w1,dw
      real swc(m,np+1),swh(m,np+1),tbl(nu,nw)

c-----output (undated) parameter

      real df(m,np+1)

c-----temporary array

      integer i,k,ic,iw 
      real clog,wlog,dc,dd,x1,x2,y1,y2

c-----table look-up for the reduction of clear-sky solar

         x1=u1-0.5*du
         y1=w1-0.5*dw

      do k= 2, np+1
       do i= 1, m
          clog=swc(i,k)
          wlog=swh(i,k)
          ic=int( (clog-x1)/du+1.)
          iw=int( (wlog-y1)/dw+1.)
          if(ic.lt.2)ic=2
          if(iw.lt.2)iw=2
          if(ic.gt.nu)ic=nu
          if(iw.gt.nw)iw=nw     
          dc=clog-float(ic-2)*du-u1
          dd=wlog-float(iw-2)*dw-w1   
          x2=tbl(ic-1,iw-1)+(tbl(ic-1,iw)-tbl(ic-1,iw-1))/dw*dd
          y2=x2+(tbl(ic,iw-1)-tbl(ic-1,iw-1))/du*dc
          df(i,k)=df(i,k)+y2
       enddo      
      enddo  

      return
      end

