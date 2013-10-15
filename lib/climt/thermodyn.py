#!/usr/bin/env python

# Moist thermodynamics module

from numpy import *
from utils import squeeze
from parameters import Parameters

try:
    import _thermodyn
except ImportError:
    print "Thermodynamics components are currently unavailable."


def es(T):
  '''
  Return saturation partial pressure of water vapour es [mb]
  at temperature T [K]
  Use Boren and Albrecht "Atmospheric Thermodynamics", p 198
  '''
  return squeeze(_thermodyn.es(T))

def qs(T, p, Rd=None, Rv=None):
  '''
  Return saturation specific humidity qs [g/kg]
  at temperature T [K] and pressure p [mb]
  Use Boren and Albrecht "Atmospheric Thermodynamics", p 186
  '''
  if Rd  is None: Rd  = Parameters()['Rd']
  if Rv  is None: Rv  = Parameters()['Rv']
  return squeeze(_thermodyn.qs(Rd,Rv,T,p))

def ws(T, p, Rd=None, Rv=None):
  '''
  Return saturation mass mixing ration of water vapour ws [g/kg]
  at temperature T [K] and pressure p [mb]
  Use Boren and Albrecht "Atmospheric Thermodynamics", p 186
  '''
  if Rd  is None: Rd  = Parameters()['Rd']
  if Rv  is None: Rv  = Parameters()['Rv']
  return squeeze(_thermodyn.ws(Rd,Rv,T,p))

def esflatau(T,i):
  '''
  Return saturation partial pressure of water vapor es [mb]
  at temperature T [K].
  Use 8th order polynomial fit of Flatau et al (1992),J. App. Met . 31, 1507-1513, Table 4

  i = 1 => return vapour pressure over water (valid -85C < t < 70C)
  i = 2 => return vapour pressure over ice (valid -90C < t < 0C)    
  '''
  return squeeze(_thermodyn.esflatau(T,i))

def qsflatau(T,p,i=None,Rd=None,Rv=None):
  '''
  Return saturation specific  humidity qs [g/kg]
  at temperature T [K] and pressure p [mb]
  Use 8th order polynomial fit of Flatau et al (1992),
  J. App. Met . 31, 1507-1513, Table 4

  i = 1 => return sat. mix. ratio over water (valid -85C < t < 70C)
  i = 2 => return sat. mix. ratio over ice (valid -90C < t < 0C)    
  '''
  if i   is None: i   = 1
  if Rd  is None: Rd  = Parameters()['Rd']
  if Rv  is None: Rv  = Parameters()['Rv']
  return squeeze(_thermodyn.qsflatau(Rd,Rv,T,p,i))

def wsflatau(T,p,i=None,Rd=None,Rv=None):
  '''
  Return saturation mass mixing ratio of water vapor ws [g/kg]
  at temperature T [K] and pressure p [mb]
  Use 8th order polynomial fit of Flatau et al (1992),
  J. App. Met . 31, 1507-1513, Table 4

  i = 1 => return sat. mix. ratio over water (valid -85C < t < 70C)
  i = 2 => return sat. mix. ratio over ice (valid -90C < t < 0C)    
  '''
  if i   is None: i   = 1
  if Rd  is None: Rd  = Parameters()['Rd']
  if Rv  is None: Rv  = Parameters()['Rv']
  return squeeze(_thermodyn.wsflatau(T,p,i))

def tdew(p,q):
  '''
  Return dew point temperature tdew  [K] 
  at pressure p [mb] and specific humidity q [g/kg]
  Use Eq. (11) in Bolton (1980), Mon. Wea. Rev. 108, 1046-1053
  '''
  return squeeze(_thermodyn.tdew(p,q))

def tstar(T,p,q):
  '''
  Return saturation point temperature tstar [K]
  (i.e. temperature at lifting condensation level) 
  given temperature T [K], pressure p [mb], specific humidity q [g/kg].
  Use Eq. (12) in Bolton (1980), Mon. Wea. Rev. 108, 1046-1053
  '''
  return squeeze(_thermodyn.tstar(T,p,q))

def theta(T, p, q=None, Rd=None, Rv=None, cpd=None, cpv=None, p0=None):
  '''
  Return potential temperature theta [K]
  given temperature T [K], pressure p [mb], specific humidity q [g/kg].
  '''
  if Rd  is None: Rd  = Parameters()['Rd']
  if Rv  is None: Rv  = Parameters()['Rv']
  if cpd is None: cpd = Parameters()['Cpd']
  if cpv is None: cpv = Parameters()['Cpv']
  if p0  is None: p0  = 1000.
  if q   is None: q   = zeros(shape(T))*0.
  return squeeze(_thermodyn.theta(Rd, Rv, cpd, cpv, p0, p, T, q))

def thetae(T, p, wt=None, \
           Rd=None, Rv=None, lv0=None, cpd=None, cpv=None, cl=None, p0=None):
  '''
  Return equivalent potential temperature thetae [K]
  given temperature T [K], pressure p [mb], and
  total water mass mixing ratio wt [g/kg].
  If wt is not specified, parcel is assumed to be just saturated (no condensate).
  If wt is specified and is less that saturation mixing ratio,
  thetae is computed at the lifting condensation level.
  Uses Eq. (6.119) in Bohren+Albrecht, p.293.
  '''
  if Rd  is None: Rd  = Parameters()['Rd']
  if Rv  is None: Rv  = Parameters()['Rv']
  if lv0 is None: lv0 = Parameters()['Lv']
  if cpd is None: cpd = Parameters()['Cpd']
  if cpv is None: cpv = Parameters()['Cpv']
  if cl  is None: cl  = Parameters()['Cl']
  if p0  is None: p0  = 1000.
  if wt  is None: wt  = -1.*ones(shape(T))
  return squeeze(_thermodyn.thetae(Rd, Rv, lv0, cpd, cpv, cl, p0, p, T, wt))

def thetaes(T, p,  \
           Rd=None, Rv=None, lv0=None, cpd=None, cpv=None, cl=None, p0=None):
  '''
  Return saturation equivalent potential temperature thetae [K]
  given temperature T [K] and pressure p [mb]
  Uses Eq. (6.123) in Bohren+Albrecht, p.293.
  '''
  if Rd  is None: Rd  = Parameters()['Rd']
  if Rv  is None: Rv  = Parameters()['Rv']
  if lv0 is None: lv0 = Parameters()['Lv']
  if cpd is None: cpd = Parameters()['Cpd']
  if cpv is None: cpv = Parameters()['Cpv']
  if cl  is None: cl  = Parameters()['Cl']
  if p0  is None: p0  = 1000.
  return squeeze(_thermodyn.thetaes(Rd, Rv, lv0, cpd, cpv, cl, p0, p, T))

def z(p, T, p0=None, R=None, g=None):
    '''
    Returns height z given temperature T [K] and pressure p.
    If gas constant R is not specified, then dry air value is used.
    If grav. accn. g is not specified, then earth value is used.
    If surface pressure p0 is not specified, then p0=p[-1].
    '''
    flip=False
    if alltrue(p[0]>p[-1]):
        p=p[::-1]
        flip=True
    if R is None: R=Parameters()['Rd']
    if g is None: g=Parameters()['g']
    if p0 is None: p0=p[-1]
    z=squeeze(_thermodyn.z(R,g,p0,p,T))
    if flip: z=z[::-1]
    return z

def pseudoadiab(p, p0, T0, thetae=None, \
                Rd=None, Rv=None, lv0=None, cpd=None, cpv=None, cl=None):
    '''
    Returns the temperature [K] at the pressure levels p [mb] along the
    psudoadiabat identified by pressure p0 [mb] and temperature T0 [K].
    If equivalent potential temp thetae is input, then it identifies the
    adiabat overriding p0,T0.
    '''
    if Rd  is None: Rd=Parameters()['Rd']
    if Rv  is None: Rv=Parameters()['Rv']
    if lv0 is None: lv0=Parameters()['Lv']
    if cpd is None: cpd=Parameters()['Cpd']
    if cpv is None: cpv=Parameters()['Cpv']
    if cl  is None: cl=Parameters()['Cl']

    if thetae is not None:
        p0 = zeros(shape(thetae)) + 1.
        T0 = thetae*(p0/1000.)**(Rd/cpd)
        
    flip=False
    if alltrue(p[0]>p[-1]):
        p=p[::-1]
        flip=True

    T=squeeze(_thermodyn.pseudoadiab(Rd,Rv,lv0,cpd,cpv,cl,p,p0,T0))

    if flip: T = T[::-1]
    return T

def CAPE(p, T, q, Virtual=True, MixedLayerDepth=100., \
         Rd=None, Rv=None, lv0=None, cpd=None, cpv=None, cl=None):
    '''
    Returns convective available potential energy [J/kg]
    given pressure p [mb], temperature T [K], spec. humidity q [g/kg].
    Use average parcel properties over layer of depth MixedLayerDepth [mb] above ground.
    If Virtual=True, apply virtual temp correction.
    '''
    if Rd  is None: Rd=Parameters()['Rd']
    if Rv  is None: Rv=Parameters()['Rv']
    if lv0 is None: lv0=Parameters()['Lv']
    if cpd is None: cpd=Parameters()['Cpd']
    if cpv is None: cpv=Parameters()['Cpv']
    if cl  is None: cl=Parameters()['Cl']

    if alltrue(p[0]>p[-1]):
        p=p[::-1]
        T=T[::-1]
        q=q[::-1]

    if Virtual:
        ivirt = 1
    else:
        ivirt = 0
        
    return squeeze(_thermodyn.cape(Rd,Rv,lv0,cpd,cpv,cl,ivirt,MixedLayerDepth,p,T,q))

def CINE(p, T, q, Virtual=True, MixedLayerDepth=100.,\
         Rd=None, Rv=None, lv0=None, cpd=None, cpv=None, cl=None):
    '''
    Returns convective inhibition energy [J/kg]
    given pressure p [mb], temperature T [K], spec. humidity q [g/kg]
    Use average parcel properties over layer of depth MixedLayerDepth [mb] above ground.
    If Virtual=True, apply virtual temp correction.
    '''
    if Rd  is None: Rd=Parameters()['Rd']
    if Rv  is None: Rv=Parameters()['Rv']
    if lv0 is None: lv0=Parameters()['Lv']
    if cpd is None: cpd=Parameters()['Cpd']
    if cpv is None: cpv=Parameters()['Cpv']
    if cl  is None: cl=Parameters()['Cl']

    if p[0]>p[-1]:
        p=p[::-1]
        T=T[::-1]
        q=q[::-1]

    if Virtual:
        ivirt = 1
    else:
        ivirt = 0
        
    return squeeze(_thermodyn.cine(Rd,Rv,lv0,cpd,cpv,cl,ivirt,MixedLayerDepth,p,T,q))

def skewT(p=None, T=None, Td=None, virtual=False):
    N    = 50
    p0   = 1050
    ptop = 10.
    p1   = p0 - (arange(N+1)) * (p0 - ptop)/N
    def getSkewOffset(p):
        Tmin =-40.
        Tmax = 40.
        pmin= 100.
        pmax = 1050.
        tanGamma = (Tmax-Tmin)/log(pmax/pmin)
        return  tanGamma*log(pmax/p)
    def plotSkewGrid():
        off = getSkewOffset(p1)
        for x in range(-90,40,10):
            pylab.semilogy(x+off,p1,'k',linestyle='-',linewidth=.5)
        for y in range(100,1050,100):
            #pylab.axhline(y,color='k',linestyle='-',linewidth=.5)
            pylab.semilogy([-100,100],[y,y],color='k',linestyle='-',linewidth=.5)
    def plotAdiabats():
        off = getSkewOffset(p1)
        for theta in range(240,480,10):
            # Dry
            kappa = Parameters()['Rd']/Parameters()['Cpd']
            T = theta*(p1/1000.)**kappa - 273.15 
            pylab.semilogy(T+off, p1, c='#996663', lw=1)
            # Moist
            if virtual:
                T = pseudoadiab(p1,1.,1.,thetae=theta+5.) 
                T = T*(1.+0.608*qs(T,p1)*1.e-3) - 273.15    
            else:
                T = pseudoadiab(p1,1.,1.,thetae=theta+5.) -273.15 
            pylab.semilogy(T+off, p1, c='#006600', lw=1)
    def plotHumidityIsopleths():
        off = getSkewOffset(p1)
        for q in [0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 0.4, 0.5, 0.6, 0.8, \
                  1., 1.5, 2., 2.5, 3., 4., 5., 6., 7., 8., 9., 10., 12., \
                  14., 16., 18., 20., 24., 28., 32.]:
            T=tdew(p1,p1*0.+q)-273.15
            pylab.semilogy(T+off, p1, c='y', linestyle='--', lw=1)
    try:
        try:    import matplotlib.pylab as pylab
        except: import matplotlib.matlab as pylab
        pylab.ion()
    except:
        print '\n ++++ CliMT: WARNING: matplotlib.pylab ' \
              +'could not be loaded, so cannot plot skew T !\n' 
    pylab.figure(figsize=(7,10))
    plotAdiabats()
    plotHumidityIsopleths()
    plotSkewGrid()
    if p is not None:
        if T is not None:
            pylab.semilogy(T+getSkewOffset(p),p,'k',linewidth=3)
        if Td is not None:
            # missing values must be negative !!
            kmax = argmin(Td)
            Td = Td[0:kmax]
            p = p[0:kmax]
            pylab.semilogy(Td+getSkewOffset(p),p, c='0.7',linewidth=3)
        if virtual and Td is not None and T is not None:
            T = T[0:kmax]
            Tv = (T+273.15)*(1.+0.608*qs(Td+273.15,p)*1.e-3) - 273.15    
            pylab.semilogy(Tv+getSkewOffset(p),p, c='k',linewidth=1)
    pylab.yticks(arange(100,1100,100),\
                 tuple([str(i) for i in arange(100,1100,100)]))
    pylab.ylabel('Pressure [hPa]')
    pylab.xlabel('Temperature [C]')
    pylab.xlim(-30,40)
    pylab.ylim(1050,100)
    manager = pylab.get_current_fig_manager()
    manager.canvas.draw()
    
def pdryadiab(T,theta,q):
  '''
  Return pressure level pdryadiab [mb] at which temperature is T [K]
  on the dry (i.e. unsaturated) adiabat identified by 
  potential temperature theta [K] and specific humidity q [g/kg].
  Use Eq. (7) in Bolton (1980), Mon. Wea. Rev. 108, 1046-1053
  '''
  return squeeze(_thermodyn.pdryadiab(T,theta,q))

def tdryadiab(theta,p,q):
  '''
  Return temperature tdryadiab [K] at level p [mb] 
  on the dry (i.e. unsaturated) adiabat identified by 
  potential temperature theta [K] and specific humidity q [g/kg].
  Use Eq. (7) in Bolton (1980), Mon. Wea. Rev. 108, 1046-1053
  '''
  return squeeze(_thermodyn.tdryadiab(theta,p,q))

def tmoistadiab(thetaes,p):
  '''
  Return temperature [K] at level p [mb] 
  on the moist (i.e. saturated) pseudo-adiabat identified 
  by saturation equivalent potential temperature thetaes [K].
  Do it by varying t until 
  delthetae := thetaes - thetae(T,p,qs(T,p)) = 0
  Use Eq. (43) in Bolton (1980), Mon. Wea. Rev. 108, 1046-1053
  '''
  return squeeze(_thermodyn.tmoistadiab(thetaes,p))

def relhum(p,T,q):
    '''
    Given pressure p [mb], temp T [K] and spec. hum. q [g/kg],
    return relative humidity w/ws [frac]
    '''
    w = q/(1000.-q) * 1000.
    return w/ws(T,p)
    
def moistadiabat(p, T0, Tstrat, rh):
    '''
    Given pressure levels p [mb],
    returns temperature profile T [K] along the moist pseudoadiabat
    assuming saturated conditions at pressure p[0] and temperature T0,
    and crossing over to isothermal stratosphere where T=Tstrat,
    and humidity profile q [g/kg] defined by constant relative humidity.
    Arguments:
    p[:]   Pressure profile [mb]
    T0     Temperature at lowest atmos level [K]
    Tstrat Stratospheric temperature [K]
    rh     Relative humidity [frac]
    '''
    p    = array(p)*1.
    T0   = array(T0)*1.
    # vertical and horizontal dimensions
    nlev = shape(p)[0]
    nhor = size(p[-1])
    # Work arrays
    InputShape = shape(p) # store for later use
    p = reshape(p,(nlev,nhor))
    T = zeros((nlev,nhor),'d') 
    q = zeros((nlev,nhor),'d') 
    # Bottom level temp needs special treatment
    if size(T0) == 1: # If a single bottom temp is given, spread it out horizontally
        T0 = ones(nhor,'d') * T0
    else:             # Else, make sure horiz dimensions are consistent
        assert shape(p[-1]) == shape(T0), \
           '\n\n +++ CliMT.thermodyn.moistadiabt: \
           inconsistent horizontal dimensions of p and T0 \n\n'
    # Build profiles
    for i in range(nhor):
        q0   = qs( T0[i], p[-1,i] )        # bottom level sat. specific humidity
        the0 = thetae( T0[i], p[-1,i], q0) # sat. equivalent pot. temp.
        # Build temp profile
        kpause=-1
        T[:,i] = pseudoadiab(p[:,i],p[-1,i],T0[i])
        for k in range(nlev):
            #T[k,i] = tmoistadiab( the0, p[k,i])   # moist adiabatic profile
            if T[k,i] < Tstrat: 
                T[k,i]=Tstrat   # isothermal stratosphere     
                kpause = k      # since we're building from the top down,
                                # kpause will be level just above tropopause
        # Build humidity profile
        wpause = ws(T[kpause,i],p[kpause,i])*rh  # tropopause mixing rat 
        for k in range(nlev):
            if k <= kpause:
                q[k,i] = 1.e-9 #wpause    # constant q in stratosphere
                continue
            q[k,i] = ws(T[k,i],p[k,i])*rh
    # Reshape results and return
    T = reshape(T,InputShape)
    q = reshape(q,InputShape)
    q = q/(q+1000.)*1000. # convert from mix ratio to spec hum
    return (T,q)

def thetae_old(T,p,q):
  '''
  Return equivalent potential temperature thetae [K]
  given temperature T [K], pressure p [mb], specific humidity q [g/kg].
  Use Eq. (38) in Bolton (1980), Mon. Wea. Rev. 108, 1046-1053
  '''
  return squeeze(_thermodyn.thetae_old(T,p,q))

def theta_old(T,p,q):
  '''
  Return potential temperature theta [K]
  given temperature T [K], pressure p [mb], specific humidity q [g/kg].
  Use Eq. (7) in Bolton (1980), Mon. Wea. Rev. 108, 1046-1053
  '''
  return squeeze(_thermodyn.theta_old(T,p,q))

