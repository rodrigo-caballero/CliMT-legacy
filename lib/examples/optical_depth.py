#!/usr/bin/env python

#
# Estimate total LW optical depth of atmosphere 
#

import climt
from numpy import *

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
p = ( arange(r.nlev)+ 0.5 ) * ps/r.nlev 
# Return moist adiabat with 70% rel hum
(T,q) = climt.thermodyn.moistadiabat(p, Ts, Tst, .7)


kwargs = {}
kwargs['p']   = p
kwargs['ps']  = ps
kwargs['T']   = T
kwargs['Ts']  = Ts
kwargs['co2'] = 330.
#kwargs['q']   = q*0. 
#kwargs['o3']  = q*0. 

r(**kwargs)
ToaLW0 = -r['lwflx'][0]

kwargs['Ts'] = 10.
r(**kwargs)
ToaLW1 = -r['lwflx'][0]

sigma = climt.Parameters()['stebol']
SurfLW = sigma*Ts**4
tau = -log( (ToaLW0-ToaLW1)/SurfLW )

print "Total optical depth: %f"%(tau)
