#!/usr/bin/env python

#
# Set up realistic tropical temperature and moisture profiles
# and compute radiative fluxes
#

from numpy import *
import pylab
import climt
r = climt.radiation(scheme='cam3')

def forcingDueToDoubling(p,ps,T,Ts,q,cldf,clwp,co2):
  r(p=p, ps=ps, T=T, Ts=Ts, q=q, cldf=cldf, clwp=clwp, co2=co2)
  lw0 = r['LwToa']
  r(p=p, ps=ps, T=T, Ts=Ts, q=q, cldf=cldf, clwp=clwp, co2=co2*2.)
  lw1 = r['LwToa']
  return lw1-lw0


def profiles(Ts):
  #--- initialise T,q
  # Surface temperature
  #Ts = 273.15 + 30.                         
  # Strospheric temp
  Tst = 273.15 - 80.                         
  # Surface pressure
  ps = 1000.
  # Equispaced pressure levels
  p = ( arange(r.nlev)+ 0.5 )/r.nlev * ps
  print r.nlev
  # Return moist adiabat with given rel hum
  rh=0.4
  (T,q) = climt.thermodyn.moistadiabat(p, Ts, Tst, rh)
  
  cldf = q*0. 
  clwp = q*0.
  kcld = argmin(abs(p-800.))
  #cldf[kcld] = 1.
  #clwp[kcld] = 100.
  return T,q,cldf,clwp,p,ps

co2 = 280.* 2**4

f = []
for i in range(10):
  Ts = 273.15 + i*3
  T,q,cldf,clwp,p,ps = profiles(Ts)
  f.append(forcingDueToDoubling(p,ps,T,Ts,q,cldf,clwp,co2))

pylab.plot(arange(10)*3,f)
#pylab.ylim([1000,0])
pylab.show()
