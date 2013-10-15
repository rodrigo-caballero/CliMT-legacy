#!/usr/bin/env python

#
# Set up realistic tropical temperature and moisture profiles
# and compute radiative fluxes
#

import _ozone
from Numeric import *
import climt


nlev = 18
ps   = 1000.                                # Surface pressure
p    = ( arange(nlev, typecode='d')+ 0.5 ) * ps/nlev
o3   = climt.utils.squeeze( _ozone.driver(p) )* 1.e8

#--- print out
print '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
print 'Testing ozone module ...'

print "lev    p     o3"
for i in range(nlev):
    print "%3i %6.1f %6.1f" % (i, p[i], o3[i])


print ' '
print 'Success!'
print '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'


