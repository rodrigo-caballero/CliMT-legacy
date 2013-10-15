import climt
from Numeric import *

d=climt.turbulence(scheme='simple')

V =arange(d.nlev,typecode='d')+1
T =d['T']
Ts=d['Ts']+1
q =V/10.

d(Ts=Ts, V=V, q=q, do_srf_heat_flx=1, do_srf_mom_flx=1, nuv=.01, u0=10., Pr=1.)


print '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
print 'Testing simple turbulence module ...'

print d['SrfLatFlx']

print ' '
print 'Success!'
print '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
