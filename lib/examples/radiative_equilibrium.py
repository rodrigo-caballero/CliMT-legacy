#!/usr/bin/env python

from numpy import *
from climt import *

# Parameters
kwargs                   = {}
rh                       = 0.8    # relative humidity
kwargs['dt']             = 60.*60. 
kwargs['MonitorFields']  = ['T','q','TdotRad','TdotTurb']
kwargs['MonitorFreq']    = 60*60.*6
kwargs['OutputFile']     = 'radequil.nc'
kwargs['OutputFreq']     = 86400. * 10.
kwargs['do_srf_lat_flx'] = 0
kwargs['solin']          = 400.

# Helper functions
def getRelHum():
    q = thermodyn.qsflatau(fed.State['T'],fed.State['p'],1) * rh  
    q = maximum(q, q*0.+1.e-16)
    return reshape(q,shape(fed.State['p']))
def printout():
    time  = fed.State.ElapsedTime
    freq  = 86400.
    if int(time/freq) != int((time-fed['dt'])/freq): 
        print "\nlev    p     T         q"
        for i in range(fed.nlev):
            print "%3i %6.1f %6.1f %6.2f" % \
                  (i, fed['p'][i], fed['T'][i]-273.15, fed['q'][i])
        print 'Surface temp: %10.5f' % (fed['Ts']-273.15)

# Federation
rad = radiation(UpdateFreq=60.*60*4, scheme='gray')
dif = turbulence()
oce = ocean()
# -- Initial fields (temp is set to skin temp)
kwargs['q'] = zeros(rad.nlev) + 1.e-9
kwargs['T'] = zeros(rad.nlev) + (kwargs['solin']/2./rad['stebol'])**0.25 
fed = federation(dif, rad, oce, **kwargs)

# Main timestepping loop
RunLength   = 1000.   # Total length of run (days)
NSteps    = int( RunLength*86400./fed['dt'])
for i in range(NSteps):
      fed.State['q'] = getRelHum() # impose constant relative humidity
      fed.step()
      printout()

# If using graphics
try:
    from matplotlib.pylab import *
    show()
except:
    pass

