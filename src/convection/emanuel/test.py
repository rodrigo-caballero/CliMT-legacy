## Automatically adapted for numpy.oldnumeric Feb 01, 2008 by -c

#!/usr/bin/env python

import climt,numpy.oldnumeric as Numeric

# instantiate convection module
x=climt.convection(scheme='emanuel')

# initial condition taken from cold pool, year 80 of stp run Tc=14.05.nc
levs=30
ps   = 1020. # surface pressure
p    = ( Numeric.arange(levs,typecode='d')+ 0.5 ) * ps/levs
T    = Numeric.zeros(levs,typecode='d')
q    = Numeric.zeros(levs,typecode='d')

T[0], q[0] =  229.48622,   0.00974
T[1], q[1] =  208.02373,   0.00973
T[2], q[2] =  198.75464,   0.00973
T[3], q[3] =  201.19379,   0.01010
T[4], q[4] =  211.52014,   0.02950
T[5], q[5] =  221.65689,   0.08010
T[6], q[6] =  230.88527,   0.17872
T[7], q[7] =  238.23030,   0.38702
T[8], q[8] =  244.83342,   0.55836
T[9], q[9] =  250.61952,   0.71973
T[10], q[10] =  255.49783,   0.94264
T[11], q[11] =  259.61646,   1.23721
T[12], q[12] =  263.15134,   1.62335
T[13], q[13] =  266.24539,   2.10185
T[14], q[14] =  269.05978,   2.66333
T[15], q[15] =  271.70395,   3.28764
T[16], q[16] =  274.61026,   4.06313
T[17], q[17] =  277.23254,   4.63064
T[18], q[18] =  279.60522,   5.11627
T[19], q[19] =  281.82156,   5.64950
T[20], q[20] =  283.88925,   6.22998
T[21], q[21] =  285.85257,   6.84505
T[22], q[22] =  287.73657,   7.48173
T[23], q[23] =  289.52332,   8.12748
T[24], q[24] =  291.17166,   8.86209
T[25], q[25] =  292.67932,   9.90695
T[26], q[26] =  294.04276,  11.39125
T[27], q[27] =  295.40601,  13.59509
T[28], q[28] =  296.74643,  16.92211
T[29], q[29] =  299.66928,  16.92211

# compute convective tends
x(p=p, ps=ps, T=T, q=q, cbmf=0.007339)

# output
tdot=x['TdotConv']
qdot=x['qdotConv']

print '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
print 'Testing emanuel convection module ...'

print "precc=",x['precc']
print "cbmf=",x['cbmf']

print "lev    p     T       q        tdot   qdot"
for i in range(levs):
    print "%3i %6.1f %6.1f %6.3f %10.2f %6.2f" % (i, p[i], T[i], q[i], tdot[i], qdot[i]) 

print ' '
print 'Success!'
print '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'

