#! /usr/bin/env python

from _insolation import *
from Numeric import *
import climt

berger78_driver(1950)

scon = 1367.
calday = 3.
lat = 60.
lon = 180.
r = 1.
dpy = 3.

print '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
print 'Testing insolation module ...'

print ' zen = %f  solin = %f' % \
      tuple([i for i in climt.utils.squeeze( inst_driver(calday, lat, lon, scon, r, dpy))])
print ' zen = %f  solin = %f' % \
      tuple([i for i in climt.utils.squeeze(daily_driver(calday, lat, lon, scon, r, dpy))])
print ' zen = %f  solin = %f' %\
      tuple([i for i in  climt.utils.squeeze( annual_driver(calday, lat, lon, scon, r, dpy) )])

print ' '
print 'Success!'
print '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
