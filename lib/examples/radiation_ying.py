#!/usr/bin/env python

#
# Set up realistic tropical temperature and moisture profiles
# and compute radiative fluxes
#

from numpy import *
import climt

#--- instantiate radiation module
r = climt.radiation(scheme='ccm3')


#--- initialise T,q
# Surface temperature
Ts = 273.15 + 0                         
# Strospheric temp
Tst = 273.15 - 80.                         
# Surface pressure
ps = 1000.
# Equispaced pressure levels
p = ( arange(r.nlev)+ 0.5 )/r.nlev * ps
# Return moist adiabat with 70% rel hum

(T,q) = climt.thermodyn.moistadiabat(p, Ts, Tst, 1.)

r(p=p, ps=ps, T=T, Ts=Ts, q=q, co2=280.)
lwhr = r['lwhr']
q    = r['q']
T    = r['T']


T1=T.copy()
T1[15] = T1[15] +3
r(p=p, ps=ps, T=T1, Ts=Ts, q=q, co2=280.)
lwhr1 = r['lwhr']


print("lev     p     T       q     lwhr")
for i in range(r['nlev']):
    print("%3i %6.1f %6.1f %6.2f %10.2f %10.2f" % \
          (i, p[i], T[i]-273.15, q[i], lwhr[i], lwhr1[i]))



from matplotlib import pyplot as plt
plt.ioff()
plt.plot(T,p,'ko-')
plt.plot(T1,p,'bo-')
plt.plot(lwhr*100,p,'ro-')
plt.plot(lwhr1*100,p,'go-')
plt.ylim(1000,0)
plt.show()

