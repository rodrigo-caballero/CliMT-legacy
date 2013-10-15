      subroutine berger78(iyear_AD)

C-----------------------------------------------------------------------
C
C	Calculate earth's orbital parameters using Dave Threshers
C	formula which came from Berger, Andre.  1978  
C      "A Simple Algorithm to Compute Long-Term Variations
C      of Daily Insolation".  Contribution 18, Institute of Astronomy and
C      Geophysics, Universite Catholique de Louvain, Louvain-la-Neuve, 
C	Belgium.
C
C---------------------------Code history--------------------------------
C
C Original Author: Erik Kluzek
C Date:            Oct/97
C
C-----------------------------------------------------------------------
      implicit none
C---------------------------- Input Arguments --------------------------
      integer iyear_AD ! Year to calculate orbit for..
C--------------------------- Output Arguments --------------------------
      real eccen,      ! Earth's orbital eccentricity
     $     obliq,      ! Earth's obliquity in degree's
     $     mvelp       ! Earth's moving vernal equinox longitude
      common/orbpar/eccen, obliq, mvelp

C------------------------------Parameters-------------------------------
C
C Parameters for calculating earth's orbital characteristics
C
      integer poblen,  ! number of elements in the series to calc obliquity
     $        pecclen, ! number of elements in the series to calc eccentricity
     $        pmvelen  ! number of elements in the series to calc vernal equinox
      parameter( poblen = 47, pecclen = 19, pmvelen = 78 )
      real
     $  psecdeg,         ! arc seconds to degrees conversion
     $  degrad,          ! degrees to radians conversion factor
     $  obamp(poblen),   ! amplitudes for obliquity cosine series
     $  obrate(poblen),  ! rates for obliquity cosine series
     $  obphas(poblen),  ! phases for obliquity cosine series
     $  ecamp(pecclen),  ! amplitudes for eccentricity/fvelp cosine/sine series
     $  ecrate(pecclen), ! rates for eccentricity/fvelp cosine/sine series
     $  ecphas(pecclen), ! phases for eccentricity/fvelp cosine/sine series
     $  mvamp(pmvelen),  ! amplitudes for mvelp sine series 
     $  mvrate(pmvelen), ! rates for mvelp sine series 
     $  mvphas(pmvelen), ! phases for mvelp sine series
     $  yb4_1950AD       ! number of years before 1950 AD
C
      parameter(psecdeg = 1./3600.)
C
C  Cosine series data for computation of obliquity:
C  amplitude (arc seconds), rate (arc seconds/year), phase (degrees).
C
      data obamp /-2462.2214466, -857.3232075, -629.3231835,
     $             -414.2804924, -311.7632587,  308.9408604,
     $             -162.5533601, -116.1077911,  101.1189923,
     $              -67.6856209,   24.9079067,   22.5811241,
     $              -21.1648355,  -15.6549876,   15.3936813,
     $               14.6660938,  -11.7273029,   10.2742696,
     $                6.4914588,    5.8539148,   -5.4872205,
     $               -5.4290191,    5.1609570,    5.0786314,
     $               -4.0735782,    3.7227167,    3.3971932,
     $               -2.8347004,   -2.6550721,   -2.5717867,
     $               -2.4712188,    2.4625410,    2.2464112,
     $               -2.0755511,   -1.9713669,   -1.8813061,
     $               -1.8468785,    1.8186742,    1.7601888,
     $               -1.5428851,    1.4738838,   -1.4593669,
     $                1.4192259,   -1.1818980,    1.1756474,
     $               -1.1316126,    1.0896928/
C
      data obrate /31.609974, 32.620504, 24.172203,
     $             31.983787, 44.828336, 30.973257,
     $             43.668246, 32.246691, 30.599444,
     $             42.681324, 43.836462, 47.439436,
     $             63.219948, 64.230478,  1.010530,
     $              7.437771, 55.782177,  0.373813,
     $             13.218362, 62.583231, 63.593761,
     $             76.438310, 45.815258,  8.448301,
     $             56.792707, 49.747842, 12.058272,
     $             75.278220, 65.241008, 64.604291,
     $              1.647247,  7.811584, 12.207832,
     $             63.856665, 56.155990, 77.448840,
     $              6.801054, 62.209418, 20.656133,
     $             48.344406, 55.145460, 69.000539,
     $             11.071350, 74.291298, 11.047742,
     $              0.636717, 12.844549/
C
      data obphas /251.9025, 280.8325, 128.3057,
     $             292.7252,  15.3747, 263.7951,
     $             308.4258, 240.0099, 222.9725,
     $             268.7809, 316.7998, 319.6024,
     $             143.8050, 172.7351,  28.9300,
     $             123.5968,  20.2082,  40.8226,
     $             123.4722, 155.6977, 184.6277,
     $             267.2772,  55.0196, 152.5268,
     $              49.1382, 204.6609,  56.5233,
     $             200.3284, 201.6651, 213.5577,
     $              17.0374, 164.4194,  94.5422,
     $             131.9124,  61.0309, 296.2073,
     $             135.4894, 114.8750, 247.0691,
     $             256.6114,  32.1008, 143.6804,
     $              16.8784, 160.6835,  27.5932,
     $             348.1074,  82.6496/
C
C  Cosine/sine series data for computation of eccentricity and
C  fixed vernal equinox longitude of perihelion (fvelp):
C  amplitude, rate (arc seconds/year), phase (degrees).
C
      data ecamp /0.01860798,  0.01627522, -0.01300660,
     $            0.00988829, -0.00336700,  0.00333077,
     $           -0.00235400,  0.00140015,  0.00100700,
     $            0.00085700,  0.00064990,  0.00059900,
     $            0.00037800, -0.00033700,  0.00027600,
     $            0.00018200, -0.00017400, -0.00012400,
     $            0.00001250/
C
      data ecrate /4.2072050,  7.3460910, 17.8572630,
     $            17.2205460, 16.8467330,  5.1990790,
     $            18.2310760, 26.2167580,  6.3591690,
     $            16.2100160,  3.0651810, 16.5838290,
     $            18.4939800,  6.1909530, 18.8677930,
     $            17.4255670,  6.1860010, 18.4174410,
     $             0.6678630/
C
      data ecphas /28.620089, 193.788772, 308.307024,
     $            320.199637, 279.376984,  87.195000,
     $            349.129677, 128.443387, 154.143880,
     $            291.269597, 114.860583, 332.092251,
     $            296.414411, 145.769910, 337.237063,
     $            152.092288, 126.839891, 210.667199,
     $             72.108838/
C
C  Sine series data for computation of moving vernal equinox
C  longitude of perihelion:
C  amplitude (arc seconds), rate (arc seconds/year), phase (degrees).      
C
      data mvamp /7391.0225890, 2555.1526947, 2022.7629188,
     $           -1973.6517951, 1240.2321818,  953.8679112,
     $            -931.7537108,  872.3795383,  606.3544732,
     $            -496.0274038,  456.9608039,  346.9462320,
     $            -305.8412902,  249.6173246, -199.1027200,
     $             191.0560889, -175.2936572,  165.9068833,
     $             161.1285917,  139.7878093, -133.5228399,
     $             117.0673811,  104.6907281,   95.3227476,
     $              86.7824524,   86.0857729,   70.5893698,
     $             -69.9719343,  -62.5817473,   61.5450059,
     $             -57.9364011,   57.1899832,  -57.0236109,
     $             -54.2119253,   53.2834147,   52.1223575,
     $             -49.0059908,  -48.3118757,  -45.4191685,
     $             -42.2357920,  -34.7971099,   34.4623613,
     $             -33.8356643,   33.6689362,  -31.2521586,
     $             -30.8798701,   28.4640769,  -27.1960802,
     $              27.0860736,  -26.3437456,   24.7253740,
     $              24.6732126,   24.4272733,   24.0127327,
     $              21.7150294,  -21.5375347,   18.1148363,
     $             -16.9603104,  -16.1765215,   15.5567653,
     $              15.4846529,   15.2150632,   14.5047426,
     $             -14.3873316,   13.1351419,   12.8776311,
     $              11.9867234,   11.9385578,   11.7030822,
     $              11.6018181,  -11.2617293,  -10.4664199,
     $              10.4333970,  -10.2377466,   10.1934446,
     $             -10.1280191,   10.0289441,  -10.0034259/
C
      data mvrate /31.609974, 32.620504, 24.172203,
     $              0.636717, 31.983787,  3.138886,
     $             30.973257, 44.828336,  0.991874,
     $              0.373813, 43.668246, 32.246691,
     $             30.599444,  2.147012, 10.511172,
     $             42.681324, 13.650058,  0.986922,
     $              9.874455, 13.013341,  0.262904,
     $              0.004952,  1.142024, 63.219948,
     $              0.205021,  2.151964, 64.230478,
     $             43.836462, 47.439436,  1.384343,
     $              7.437771, 18.829299,  9.500642,
     $              0.431696,  1.160090, 55.782177,
     $             12.639528,  1.155138,  0.168216,
     $              1.647247, 10.884985,  5.610937,
     $             12.658184,  1.010530,  1.983748,
     $             14.023871,  0.560178,  1.273434,
     $             12.021467, 62.583231, 63.593761,
     $             76.438310,  4.280910, 13.218362,
     $             17.818769,  8.359495, 56.792707,
     $              8.448301,  1.978796,  8.863925,
     $              0.186365,  8.996212,  6.771027,
     $             45.815258, 12.002811, 75.278220,
     $             65.241008, 18.870667, 22.009553,
     $             64.604291, 11.498094,  0.578834,
     $              9.237738, 49.747842,  2.147012,
     $              1.196895,  2.133898,  0.173168/
C
      data mvphas /251.9025, 280.8325, 128.3057,
     $             348.1074, 292.7252, 165.1686,
     $             263.7951,  15.3747,  58.5749,
     $              40.8226, 308.4258, 240.0099,
     $             222.9725, 106.5937, 114.5182,
     $             268.7809, 279.6869,  39.6448,
     $             126.4108, 291.5795, 307.2848,
     $              18.9300, 273.7596, 143.8050,
     $             191.8927, 125.5237, 172.7351,
     $             316.7998, 319.6024,  69.7526,
     $             123.5968, 217.6432,  85.5882,
     $             156.2147,  66.9489,  20.2082,
     $             250.7568,  48.0188,   8.3739,
     $              17.0374, 155.3409,  94.1709,
     $             221.1120,  28.9300, 117.1498,
     $             320.5095, 262.3602, 336.2148,
     $             233.0046, 155.6977, 184.6277,
     $             267.2772,  78.9281, 123.4722,
     $             188.7132, 180.1364,  49.1382,
     $             152.5268,  98.2198,  97.4808,
     $             221.5376, 168.2438, 161.1199,
     $              55.0196, 262.6495, 200.3284,
     $             201.6651, 294.6547,  99.8233,
     $             213.5577, 154.1631, 232.7153,
     $             138.3034, 204.6609, 106.5938,
     $             250.4676, 332.3345,  27.3039/
C
C---------------------------Local variables-----------------------------
C
      integer i       ! Index for series summations
      real obsum,     ! Obliquity series summation
     $     cossum,    ! Cosine series summation for eccentricity/fvelp
     $     sinsum,    ! Sine series summation for eccentricity/fvelp
     $     fvelp,     ! Fixed vernal equinox longitude of perihelion
     $     mvsum,     ! mvelp series summation
     $     beta,      ! Intermediate argument for lambm0
     $     years      ! Years to time of interest (negative = past;
C                     ! positive = future)
      real eccen2     ! eccentricity squared
      real pi        ! pi
C-----------------------------------------------------------------------
C
C radinp and algorithms below will need a degrees to radians conversion
C factor.
C
      pi     =  4.*atan(1.)
      degrad = pi/180.
C
C
C The following calculates the earth's obliquity, orbital eccentricity
C (and various powers of it) and vernal equinox mean longitude of
C perihelion for years in the past (future = negative of years past),
C using constants (see parameter section) given in the program of:
C
C Berger, Andre.  1978  A Simple Algorithm to Compute Long-Term Variations
C of Daily Insolation.  Contribution 18, Institute of Astronomy and
C Geophysics, Universite Catholique de Louvain, Louvain-la-Neuve, Belgium.
C
C and formulas given in the paper (where less precise constants are also
C given):
C
C Berger, Andre.  1978.  Long-Term Variations of Daily Insolation and
C Quaternary Climatic Changes.  J. of the Atmo. Sci. 35:2362-2367
C
C The algorithm is valid only to 1,000,000 years past or hence.
C For a solution valid to 5-10 million years past see the above author.
C Algorithm below is better for years closer to present than is the
C 5-10 million year solution.
C
C Years to time of interest must be negative of years before present
C (1950) in formulas that follow. 
C
        yb4_1950AD = 1950.0 - float(iyear_AD)
        years = - yb4_1950AD
C
C In the summations below, cosine or sine arguments, which end up in
C degrees, must be converted to radians via multiplication by degrad.
C
C Summation of cosine series for obliquity (epsilon in Berger 1978) in
C degrees. Convert the amplitudes and rates, which are in arc seconds, into
C degrees via multiplication by psecdeg (arc seconds to degrees conversion
C factor).  For obliq, first term is Berger 1978's epsilon star; second
C term is series summation in degrees.
C 
        obsum = 0.0
        do i = 1, poblen
          obsum = obsum +
     $           obamp(i)*psecdeg*cos((obrate(i)*psecdeg*years +
     $                                obphas(i))*degrad)
        end do
        obliq = 23.320556 + obsum
C
C Summation of cosine and sine series for computation of eccentricity 
C (eccen; e in Berger 1978) and fixed vernal equinox longitude of perihelion
C (fvelp; pi in Berger 1978), which is used for computation of moving vernal
C equinox longitude of perihelion.  Convert the rates, which are in arc
C seconds, into degrees via multiplication by psecdeg.
C
        cossum = 0.0
        do i = 1, pecclen
          cossum = cossum +
     $            ecamp(i)*cos((ecrate(i)*psecdeg*years +
     $                          ecphas(i))*degrad)
        end do
C
        sinsum = 0.0
        do i = 1, pecclen
          sinsum = sinsum +
     $            ecamp(i)*sin((ecrate(i)*psecdeg*years +
     $                          ecphas(i))*degrad)
        end do
C
C Use summations to calculate eccentricity
C
        eccen2 = cossum*cossum + sinsum*sinsum
        eccen = sqrt(eccen2)
C
C A series of cases for fvelp, which is in radians.
C           
        if (abs(cossum) .le. 1.0E-8) then
          if (sinsum .eq. 0.0) then
            fvelp = 0.0
          else if (sinsum .lt. 0.0) then
            fvelp = 1.5*pi
          else if (sinsum .gt. 0.0) then
            fvelp = .5*pi
          endif
        else if (cossum .lt. 0.0) then
          fvelp = atan(sinsum/cossum) + pi
        else if (cossum .gt. 0.0) then
          if (sinsum .lt. 0.0) then
            fvelp = atan(sinsum/cossum) + 2.0*pi
          else
            fvelp = atan(sinsum/cossum)
          endif
        endif
C
C Summation of sine series for computation of moving vernal equinox longitude
C of perihelion (mvelp; omega bar in Berger 1978) in degrees.  For mvelp,
C first term is fvelp in degrees; second term is Berger 1978's psi bar times
C years and in degrees; third term is Berger 1978's zeta; fourth term is
C series summation in degrees.  Convert the amplitudes and rates, which are
C in arc seconds, into degrees via multiplication by psecdeg.  Series summation
C plus second and third terms constitute Berger 1978's psi, which is the
C general precession.
C
        mvsum = 0.0
        do i = 1, pmvelen
          mvsum = mvsum +
     $           mvamp(i)*psecdeg*sin((mvrate(i)*psecdeg*years +
     $                                mvphas(i))*degrad)
        end do
        mvelp = fvelp/degrad + 50.439273*psecdeg*years + 3.392506
     $  + mvsum
C
C Cases to make sure mvelp is between 0 and 360.
C
        do while (mvelp .lt. 0.0)
          mvelp = mvelp + 360.0
        end do
        do while (mvelp .ge. 360.0)
          mvelp = mvelp - 360.0
        end do
C
      end
