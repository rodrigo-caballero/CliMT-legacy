#!/usr/bin/env python

#
# Compute pseudoadiabat
#

from pylab import *
import climt

#--- initialise T,q
# Surface temperature
Ts = 273.15 + 20.                         
# Surface pressure
ps = 1000.
# Equispaced pressure levels
Nlev = 20
p = ( arange(Nlev)+ 1. )/Nlev * ps
# Return moist adiabats
Tpseudo = climt.thermodyn.pseudoadiab(p, ps, Ts)

#--- print out results
z=climt.thermodyn.z(p, Tpseudo, p0=ps)/1000.

print "lev     z        p      Tpseudo"
for i in range(len(p)):
    print ("%3i"+3*" %8.2f") % \
          (i, z[i], p[i], Tpseudo[i]-273.15)


