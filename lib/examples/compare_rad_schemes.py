#!/usr/bin/env python

from numpy import *
import climt

#--- initialise T,q
# Surface temperature
Ts = 273.15 + 30.                         
# Strospheric temp
Tst = 273.15 - 80.                         
# Surface pressure
ps = 1000.
# Equispaced pressure levels
nlev = climt.get_nlev()
p = ( arange(nlev)+ 0.5 )/nlev * ps
# Return moist adiabat with 70% rel hum
(T,q) = climt.thermodyn.moistadiabat(p, Ts, Tst, 1.)

cldf = q*0. 
cldf[len(cldf)/3] = 0.5

ciwp = q*0. 
ciwp[len(cldf)/3] = 10.

r_liq = q*0. + 15.

for scheme in ['ccm3','cam3','chou']:
    r = climt.radiation(scheme=scheme)
    r(p=p, ps=ps, T=T, Ts=Ts, q=q, cldf=cldf, ciwp=ciwp, r_liq=r_liq)
    print '\n%s' % scheme
    print r['SwToa'],r['LwToa'],r['SwSrf'],r['LwSrf']
    print r['SwToaCf'],r['LwToaCf'],(r['solin']-r['SwToa'])/r['solin'],r['asdir']

    print r['ciwp']
