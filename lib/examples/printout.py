#!/usr/bin/env python

from numpy import *
import sys

# A simple helper function to print out
# values to screen during a column model run

# Helper function
def printout(fed,freq=86400.):
    time  = fed.State.ElapsedTime
    if int(time/freq) != int((time-fed['dt'])/freq): 
        print "\nlev    p     T       theta   q"
        for i in range(fed.nlev):
            print "%3i %6.1f %6.1f %6.1f %6.2f" % \
                  (i, fed['p'][i], fed['T'][i]-273.15, fed['theta'][i], fed['q'][i])
        print 'Surface temp: %10.5f' % (fed['Ts']-273.15)

