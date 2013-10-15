## Automatically adapted for numpy.oldnumeric Feb 01, 2008 by -c

import climt,numpy.oldnumeric as Numeric

# instantiate radiation module
r=climt.radiation(scheme='chou')

# initialise T,q to moist adiabat with 70% RH
ps   = 1020. # surface pressure
q0   = 15.   # surface moisture
T0   = 300.  # surface temp
the0 = climt.thetae(T0,ps,q0)
p    = ( Numeric.arange(r.nlev,typecode='d')+ 0.5 ) * ps/r.nlev
T    = Numeric.zeros(r.nlev,typecode='d')
q    = Numeric.zeros(r.nlev,typecode='d')
for k in range(r.nlev):
    T[k] = climt.tmoistadiab(the0,p[k]) 
    if (T[k] < 273.15-80.): T[k]=273.15-80.
    q[k] = climt.qsflatau(T[k],p[k],1)*0.7
    if (p[k] < 100.): q[k]=1.e-9

# compute radiative fluxes and heating rates
r(p=p,ps=ps,T=T,Ts=T0,q=q)

# output
lw=r['lwflx']
ql=r['lwhr']
sw=r['swflx']
qs=r['swhr']
o3=r['o3']

print '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
print 'Testing ccm3 radiation module ...'

print "lev    p     T       q      O3          lwflx   lwhr     swflx      swhr"
for i in range(r.nlev):
    print "%3i %6.1f %6.1f %6.3f %10.3e %10.2f %6.2f %10.2f %6.2f" % (i, p[i], T[i], q[i], o3[i], lw[i], ql[i], sw[i], qs[i]) 

print ' '
print 'Success!'
print '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
