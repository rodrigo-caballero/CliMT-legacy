#!/usr/bin/env python

from numpy import *
import climt

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

# Plot
try:
    from matplotlib.pylab import *
    imshow(transpose(solin)[::-1,:],extent=(0,50000,-90,90))
    xlabel('Time (years)')
    ylabel('Latitude')
    title('Change in annual-mean insolation [W m-2]')
    ylim([-90,90])
    xlim([0,50000])
    colorbar()
    show()
except:
    pass
