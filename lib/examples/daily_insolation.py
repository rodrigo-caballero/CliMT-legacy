#!/usr/bin/env python

'''
Makes plot of daily insolation
This is the same as the CGI code at David Archers site
'''

import sys,os
from pylab import *
import climt

## Orbital parameters:
ecc = 0.0167
obl = 23.4
pre = 102.
##

ins = climt.insolation(avg = 'daily')
nlat = 180
lat = (arange(nlat)+0.5)*180./nlat -90.
solin=[]

for day in range(1,366):
  ins(lat=lat, calday=day, eccen=ecc, obliq=obl, prece=pre)
  solin.append(ins['solin'])

solin = climt.utils.squeeze(transpose(solin))
solin=solin[::-1,:]

imshow(solin,extent=(1,366,-89,89))
cset1 = contour(solin, arange(0,2000,100),origin='upper',linewidths=1,colors='k',extent=(1,365,-89,89))
clabel(cset1,inline=1,fmt='%1.0f',fontsize=10)

xlabel('Day of year')
ylabel('Latitude')
title('Daily-mean insolation [W/m2]')
ylim([-89,89])
xlim([1,365])

show()
