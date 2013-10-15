#!/usr/bin/env python

from numpy import *
import sys
import climt
from printout import printout


# This script sets up a radiative-convective column model and
# integrates it for 2000 days.

# It serves to illustrate the use of the federation class, and
# to document some of its features

# -- Set up a dictionary of input  parameters
kwargs = {}
# model time step (secs)
kwargs['dt']  = 60.*10. 

# Asselin filter coeffcient, controls the damping of the 2-timestep
# computational mode resulting from the leapfrog scheme used in CliMT.
# I find that values 0.1-0.2 are necessary with rad-conv models.
kwargs['afc'] = 0.2

# insolation (W/m2)
kwargs['solin'] = 320.

# The following 2 parameters control the real-time plotting of model fields
# as the run progresses. MonitorFields specifies the fields to be plotted
# (up to fields 4 allowed). If the MonitorFields parameter is not given,
# no real-time plotting is done.
# Fields are refreshed every MonitorFreq seconds.
kwargs['MonitorFields']  = ['T','theta','q','TdotRad']
kwargs['MonitorFreq']    = kwargs['dt']*50 #60*60*6.

# The following 2 parameters control output of model fields to file.
# OutputFile specifies the file to write to; if it already
# exists, it will be overwritten. If OutputFile is not given,
# no output will be written to file. Output will be written every
# OutputFreq seconds
kwargs['OutputFile']     = 'radconv.nc'
kwargs['OutputFreq']     = 86400.*10.

# Initial conditions can be specified in 2 ways:
# 1) by specifying a restart file, whose format is identical to
#    that of an output file. If the file contains a time series,
#    the model will initialize from the last time step in the file.
#    If RestartFile and OutputFile are the same, then output will be
#    appended to the restart file (ie. a continuation run).
#kwargs['RestartFile']     = 'restart.nc'
# 2) If RestartFile is not given, then initial values for prognostic
#    fields must be explicitly given, e.g.
nlev = climt.get_nlev()
stebol = climt.Parameters()['stebol']
kwargs['q'] = zeros(nlev) + 1.e-9
kwargs['T'] = zeros(nlev) + (kwargs['solin']/2./stebol)**0.25

# -- Instantiate components and federation
rad = climt.radiation(UpdateFreq=kwargs['dt']*50, scheme='cam3')
con = climt.convection(scheme='emanuel')
dif = climt.turbulence()
oce = climt.ocean()
fed = climt.federation(dif, rad, oce, con, **kwargs)

# Main timestepping loop
RunLength   = 2000.   # Total length of run (days)
NSteps    = int( RunLength*86400./fed['dt'] )
for i in range(NSteps):
    # With a line like the following, it is possible to manipulate
    # field values each time step (the example here resets humidity
    # to an almost dry value everywhere at each timestep)
    #fed.State['q']=zeros(rad.nlev)+1.e-9
    # The following code adds a uniform 1 K/day cooling rate to 
    # the internally-computed tendencies
    dT= array([[-1./86400.*kwargs['dt']*2.*ones(rad.nlev)]]).transpose()
    fed.step(Inc={'T':dT})
    printout(fed)

# If using graphics
try:
    from matplotlib.pylab import *
    show()
except:
    pass

