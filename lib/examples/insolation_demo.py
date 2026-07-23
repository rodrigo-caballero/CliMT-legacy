#!/usr/bin/env python

from matplotlib import pyplot as pl
from numpy import *
import climt
pl.ioff()

ins = climt.insolation(avg = 'daily')
def daily_solin(days,year,lat):
    solin = []
    for day in days:
        ins(lat=lat, orb_year=year, calday=day)
        solin.append(ins['solin'])
    return array(solin)

lat = 47
days = arange(366)
solin = daily_solin(days,2000,lat)
pl.plot(days,solin,'k-',lw=2)
solin = daily_solin(days,1950-15200,lat)
pl.plot(days,solin)
solin = daily_solin(days,1950-14400,lat)
pl.plot(days,solin)
solin = daily_solin(days,1950-12400,lat)
pl.plot(days,solin)
solin = daily_solin(days,1950-11800,lat)
pl.plot(days,solin)
pl.legend(['modern','15200','14400','12400','11800'])
pl.xlim([0,365])
pl.xlabel('day of year')
pl.ylabel('insolation at 47N (W/m2)')
pl.show()

# Make annual-average insolation instance
ins = climt.insolation(avg = 'annual')

# Iterate over years
# Note that instance is reinitialized each time it is called,
# so we need to pass lat each time
nlat = 20
lat = (arange(nlat)+0.5)*180./nlat -90.
solin=[]
for year in range(0, 40000, 1000):
    ins(lat=lat, orb_year=year)
    solin.append(ins['solin'])

# Remove mean
solin = solin - average(solin,axis=0)[None,:]

pl.clf()
pl.imshow(transpose(solin)[::-1,:],extent=(0,50000,-90,90))
pl.xlabel('Time (years)')
pl.ylabel('Latitude')
pl.title('Change in annual-mean insolation [W m-2]')
pl.ylim([-90,90])
pl.xlim([0,50000])
pl.colorbar()

pl.show()

