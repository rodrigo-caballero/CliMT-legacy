## Automatically adapted for numpy.oldnumeric Feb 01, 2008 by -c

import climt,numpy.oldnumeric as Numeric

# instantiate radiation module
r=climt.convection(scheme='ccm3')

# initialise T,q to moist adiabat with 70% RH
ps   = 1020. # surface pressure
q0   = 15.   # surface moisture
T0   = 300.  # surface temp
the0 = climt.thetae(T0,ps,q0)
p    = ( Numeric.arange(r.levs,typecode='d')+ 0.5 ) * ps/r.levs
T    = Numeric.zeros(r.levs,typecode='d')
q    = Numeric.zeros(r.levs,typecode='d')
for k in range(r.levs):
    T[k] = climt.tmoistadiab(the0,p[k]) 
    if (T[k] < 273.15-80.): T[k]=273.15-80.
    q[k] = climt.qsflatau(T[k],p[k],1)*0.7
    if (p[k] < 100.): q[k]=1.e-9

# compute radiative fluxes and heating rates
r(p,ps,T,q)

# output
td=r.tdot*86400.
qd=r.qdot*86400.
print "lev    p     T       q        tdot   qdot"
for i in range(r.levs):
    print "%3i %6.1f %6.1f %6.3f %10.2f %6.2f" % (i, p[i], T[i], q[i], td[i], qd[i]) 


