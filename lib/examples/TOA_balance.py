#!/usr/bin/env python
import climt
from pylab import *

# Replicates the behavior of the online radiation calculator
# see maths.ucd.ie/~rca

scheme         = 'ccm3'
Insolation     = 337. #* .75
Imbalance      = 30.
Albedo         = 30. /100.
CO2            = 350.
CH4            = 1.7 + 1.e-9
N2O            = 0. + 1.e-9

TropoHeight    = 16.
LapseRate      = -6.5
LapseRate_RH   = -6.5
BLRelHum       = 80. / 100.
LTRelHum       = 80. / 100.
UTRelHum       = 80. / 100.
STRelHum       = 1.e-16 / 100.
TropoHeight_RH = 16.
RH_control     = 0
T0_RH          = 20. + 273.15

Drop_size      = 10.
Cloud_frac_hi  = 0.
Cloud_frac_lo  = 0.
Cloud_water_hi = 0. 
Cloud_water_lo = 0.

zen = 60.

if Cloud_water_lo == 0.: Cloud_frac_lo  = 0.
if Cloud_water_hi == 0.: Cloud_frac_hi  = 0.

# instantiate radiation objects, get number of levels
r=climt.radiation(scheme=scheme)
nlev=r.nlev

# define some fixed profiles
SurfPres = 1000.
Pressure  = ( arange(nlev)+ 0.5 ) * SurfPres/nlev
cldf = zeros( nlev, 'd')  # Cloud frac
clwp = zeros( nlev, 'd')  # Cloud liquid water path
cloud_lev_hi = int(nlev*0.2)  # put high cloud roughly at 200 mb
cloud_lev_lo = int(nlev*0.8)  # put low cloud roughly at 800 mb
cldf[cloud_lev_lo] = Cloud_frac_lo
cldf[cloud_lev_hi] = Cloud_frac_hi
clwp[cloud_lev_lo] = Cloud_water_lo
clwp[cloud_lev_hi] = Cloud_water_hi

# dictionary for input into rad call
input={}
input['ps'] = SurfPres
input['lev'] = Pressure
input['cldf'] = cldf
input['clwp'] = clwp
input['solin'] = Insolation
input['r_liq'] = Drop_size + Pressure*0.
input['r_ice'] = Drop_size + Pressure*0.
input['aldir'] = Albedo
input['aldif'] = Albedo
input['asdir'] = Albedo
input['asdif'] = Albedo
input['co2'] = CO2
input['ch4'] = CH4
input['n2o'] = N2O

input['zen'] = zen
#input['o3'] = Pressure*0. + 1.e-16

# functions
def profiles(SurfTemp):
    """
    Compute temp and humidity profiles
    Stratosphere is isothermal
    """
    # parameters
    Rd = r['Rd']
    g  = r['g']
    # assume near-surface temp is 1 K less than surface
    T0 = SurfTemp - 1.
    # scale height (assume dry atmos) see Holton pg. 21
    Tmean =  (T0**2*TropoHeight + LapseRate*T0*TropoHeight**2
              + LapseRate**2*TropoHeight**3)/(T0*TropoHeight +
                                              LapseRate*TropoHeight**2/2)
    Tmean_RH =  (T0_RH**2*TropoHeight_RH + LapseRate_RH*T0_RH*TropoHeight_RH**2
              + LapseRate_RH**2*TropoHeight_RH**3)/(T0_RH*TropoHeight_RH +
                                                 LapseRate_RH*TropoHeight_RH**2/2)
    H = Rd*Tmean/g * 1.e-3 # [km]
    H_RH = Rd*Tmean_RH/g * 1.e-3 # [km]
    # now compute profiles
    z = -H*log(Pressure/SurfPres)
    z900 = -H*log(900./SurfPres)
    z700 = -H*log(700./SurfPres)
    T = T0 + LapseRate*z
    Tstrat = T0 + LapseRate*TropoHeight
    q  = zeros(nlev, 'd')
    for k in range(nlev-1,-1,-1): # compute from bottom up
      if z[k] <= z900:
        q[k] = climt.thermodyn.qs(T[k],Pressure[k])*BLRelHum
      elif z[k] > z900 and z[k] <= z700:
        q[k] = climt.thermodyn.qs(T[k],Pressure[k])*LTRelHum
      elif z[k] > z700 and z[k] <= TropoHeight:        
        q[k] = climt.thermodyn.qs(T[k],Pressure[k])*UTRelHum
      else:
        T[k] = Tstrat
	q[k] = 1.e-9 #climt.thermodyn.qsflatau(T[k],Pressure[k],2)*STRelHum
      if T[k] < 273.-80.: q[k] = 1.e-9
    # correct humidity if necessary
    if RH_control:
      z_RH = -H_RH*log(Pressure/SurfPres)
      T_RH = T0_RH + LapseRate_RH*z_RH
      z900 = -H_RH*log(900./SurfPres)
      z700 = -H_RH*log(700./SurfPres)
      q  = zeros(nlev, 'd')
      for k in range(nlev-1,-1,-1): # compute from bottom up
        if z_RH[k] <= z900:
          q[k] = climt.thermodyn.qs(T_RH[k],Pressure[k])*BLRelHum
        elif z_RH[k] > z900 and z_RH[k] <= z700:
          q[k] = climt.thermodyn.qs(T_RH[k],Pressure[k])*LTRelHum
        elif z_RH[k] > z700 and z_RH[k] <= TropoHeight_RH:        
          q[k] = climt.thermodyn.qs(T_RH[k],Pressure[k])*UTRelHum
        else:
	  T_RH[k] = T_RH[k+1]
          q[k] = climt.thermodyn.qsflatau(T_RH[k],Pressure[k],2)*STRelHum
      
    return T, q, z

def TOAFlux(SurfTemp):
    (T,q,z) = profiles(SurfTemp)
    input['T'] = T
    input['Ts'] = SurfTemp
    input['q'] = q
    r(**input)
    return r.swflx[0].item() + r.lwflx[0].item() + Imbalance


# Now compute equil surf temp assuming low albedo
try:
  Teq = climt.mathutil.ridder_root(TOAFlux, (173.15,353.15), accuracy=0.1)
except climt.mathutil.BracketingException, err:
  if str(err) == 'initial interval does not bracket a root: root probably to the right of interval':
    print '<P><font color="red"><b>Equilibrium surface temperature exceeds 80 <sup>o</sup>C.</font>'
  if str(err) == 'initial interval does not bracket a root: root probably to the left of interval':
      print '<P><font color="blue"><b>Equilibrium surface temperature less than -100 <sup>o</sup>C.</font>'
  sys.exit(1)

T,q,z = profiles(Teq)
input['T'] = T
input['Ts'] = Teq
input['q'] = q
r(**input)

# print results
print
print 'Equilibrium near-surface air temperature is %4.1f degC (%4.1f K)' % ((Teq-273.15-1.),Teq-1.)
print

print r['SwToaCf'],r['LwToaCf'],Teq
sys.exit()

print 'Profiles'
print("lev    p      z      T       q    LW flux LW heating SW flux SW heating cld frac cld water\n")
for i in range(r.nlev):
    print("%3i %6.1f %7.2f %6.1f %6.2f %10.2f %6.2f %10.2f %6.2f %6.1f %6.1f" % \
          (i, Pressure[i], z[i], T[i], q[i], r['lwflx'][i], r['lwhr'][i], r['swflx'][i], r['swhr'][i] , cldf[i], clwp[i])) 


# make plot

def setlims(x,y):
  dx = (max(x)-min(x))*.05
  if dx == 0.: dx=1.
  xmin = min(x) - dx
  xmax = max(x) + dx
#  set(gca(), 'xlim', [xmin,xmax],'ylim', [0,z[0]])
  xlim([xmin,xmax])
  ylim([0,z[0]])

subplot(231)
T = T-273.15
plot(T,z, 'b-o',linewidth=1,ms=3)
#title(r'$\rm{Temperature} (^__\rm{o}\rm{C})$')
title('Temperature (C)',fontsize=10)
ylabel('height (km)',fontsize=10)
setlims(T,z)

subplot(232)
plot(q,z, 'b-o',linewidth=1,ms=3)
title('Specific humidity (g/kg)',fontsize=10)
setlims(q,z)

subplot(233)
plot(clwp,z, 'b-o',linewidth=1,ms=3)
title('Cloud water path (g/m2)',fontsize=10)
setlims(clwp,z)

ax=subplot(234)
ax.xaxis.set_major_locator(MultipleLocator(50))
plot(r['lwflx'],z, 'b-o',linewidth=1,ms=3)
title('Longwave flux (W/m2)',fontsize=10)
ylabel('height (km)',fontsize=10)
setlims(r['lwflx'],z)

ax=subplot(235)
ax.xaxis.set_major_locator(MultipleLocator(50))
plot(r['swflx'],z, 'b-o',linewidth=1,ms=3)
title('Shortwave flux (W/m2)',fontsize=10)
setlims(r['swflx'],z)

subplot(236)
plot(r['lwhr'],z,'b-o', r['swhr'],z,'r-o', r['swhr']+r['lwhr'],z,'k-o',linewidth=1,ms=3)
title('Heating rates (K/day)',fontsize=10)
legend(('LW', 'SW', 'Total'), 'upper left')
x=r['lwhr'].tolist()
x.extend(r['swhr'].tolist())
x.extend((r['lwhr']+r['swhr']).tolist())
setlims(array(x),z)

#savefig(os.path.join(ImageDir,TimeStamp),dpi=100)
show() 
