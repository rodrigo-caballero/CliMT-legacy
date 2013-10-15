#!/usr/bin/env python

#
# Set up realistic tropical temperature and moisture profiles
# and compute radiative fluxes
#

from numpy import *
import climt

#--- instantiate radiation module
r = climt.radiation(scheme='cam3')

#--- initialise T,q
# Surface temperature
Ts = 273.15 + 30.                         
# Strospheric temp
Tst = 273.15 - 80.                         
# Surface pressure
ps = 1000.
# Equispaced pressure levels
p = ( arange(r.nlev)+ 0.5 )/r.nlev * ps
# Return moist adiabat with 70% rel hum
(T,q) = climt.thermodyn.moistadiabat(p, Ts, Tst, 1.)

cldf = q*0. 
clwp = q*0. 
cldf[len(cldf)/3] = 0.5
clwp[len(cldf)/3] = 100.

#--- compute radiative fluxes and heating rates
r(p=p, ps=ps, T=T, Ts=Ts, q=q, cldf=cldf, clwp=clwp)

if 'SwToa' in r.State.keys(): print r['SwToa'],r['LwToa'],r['SwSrf'],r['LwSrf']
print r['SwToaCf'],r['LwToaCf'],(r['solin']-r['SwToa'])/r['solin']

#--- print out results
lwflx = r['lwflx']
swflx = r['swflx']
lwhr = r['lwhr']
swhr = r['swhr']
q    = r['q']
T    = r['T']
z=climt.thermodyn.z(p, T, p0=ps)/1000.

print "lev     z      p     T       q      lwflx   lwhr     swflx    swhr "
for i in range(r['nlev']):
    print "%3i %6.1f %6.1f %6.1f %6.2f %10.2f %6.2f %10.2f %6.2f" % \
          (i, z[i], p[i], T[i]-273.15, q[i], lwflx[i], lwhr[i], swflx[i], swhr[i])


