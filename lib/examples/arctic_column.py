#!/usr/bin/env python

from numpy import *
import sys
import climt


# Helper function 
def printout(fed,freq=86400.):
    time  = fed.State.ElapsedTime
    if int(time/freq) != int((time-fed['dt'])/freq): 
        print "\nlev    p     T       theta   q"
        for i in range(fed.nlev):
            print "%3i %6.1f %6.1f %6.1f %6.2f" % \
                  (i, fed['p'][i], fed['T'][i]-273.15, fed['theta'][i], fed['q'][i])
        print 'Surface temp: %10.5f' % (fed['Ts']-273.15)
        print fed.Params['calday'],float(fed.State['solin'])

# Script below sets up a radiative-convective column model and
# integrates it for 2000 days. It serves to illustrate the use of the federation class, and
# to document some of its features

# -- Set up a dictionary of input  parameters
kwargs = {}
# model time step (secs)
kwargs['dt']  = 60.*30. 
kwargs['lat'] = 85.
#kwargs['asdif'] = 0.#0.7
#kwargs['asdir'] = 0.#0.7

# Asselin filter coeffcient, controls the damping of the 2-timestep
# computational mode resulting from the leapfrog scheme used in CliMT.
# I find that values 0.1-0.2 are necessary with rad-conv models.
kwargs['afc'] = 0.2

# insolation (W/m2)
kwargs['solin'] = 0.

# The following 2 parameters control the real-time plotting of model fields
# as the run progresses. MonitorFields specifies the fields to be plotted
# (up to fields 4 allowed). If the MonitorFields parameter is not given,
# no real-time plotting is done.
# Fields are refreshed every MonitorFreq seconds.
kwargs['MonitorFields']  = ['T','theta','q','TdotRad']
kwargs['MonitorFreq']    = kwargs['dt']*20

# The following 2 parameters control output of model fields to file.
# OutputFile specifies the file to write to; if it already
# exists, it will be overwritten. If OutputFile is not given,
# no output will be written to file. Output will be written every
# OutputFreq seconds
kwargs['OutputFile']     = 'arctic_column.nc'
kwargs['OutputFreq']     = 86400.*1.

# Initial conditions can be specified in 2 ways:
# 1) by specifying a restart file, whose format is identical to
#    that of an output file. If the file contains a time series,
#    the model will initialize from the last time step in the file.
#    If RestartFile and OutputFile are the same, then output will be
#    appended to the restart file (ie. a continuation run).
#kwargs['RestartFile']     = 'radconv.nc'
# 2) If RestartFile is not given, then initial values for prognostic
#    fields must be explicitly given, e.g.
nlev = climt.get_nlev()
kwargs['q'] = zeros(nlev) + 1.e-9
kwargs['T'] = zeros(nlev) + 250. #(kwargs['solin']/2./climt.parameters.Parameters()['stebol'])**0.25
kwargs['Ts'] = 250.

# -- Instantiate components and federation
rad = climt.radiation(UpdateFreq=kwargs['dt']*50)
con = climt.convection(scheme='emanuel')
dif = climt.turbulence()
ice = climt.seaice()
ins = climt.insolation(lat=85.,avg='daily')

kwargs['Hslab']=50.
kwargs['Pr']=100.
#kwargs['do_srf_lat_flx']=0
#kwargs['do_srf_sen_flx']=0

T0,q0 = climt.thermodyn.moistadiabat(rad['p'],273.,273.-90,.1)
kwargs['T'],kwargs['q'] = T0,q0

fed = climt.federation(dif, rad, ice, con, ins, **kwargs)

# Main timestepping loop
RunLength   = 2000.   # Total length of run (days)
NSteps    = int( RunLength*86400./fed['dt'] )
TdotDyn =  2./86400.
for i in range(NSteps):
    # With a line like the following, it is possible to manipulate
    # field values each time step (the example here resets humidity
    # to an almost dry value everywhere at each timestep)
    #fed.State['q']= q0[:,None,None]
    # Inject heating here
    fed.State['T'][14:23] += TdotDyn*fed['dt']
    # Also put in a cloud; we could reset the moisture here, too
    #fed.State['cldf'][14:25] = 1.
    #fed.State['clwp'][14:25] = 30.
    
    printout(fed)
    fed.step()

# If using graphics
try:
    from matplotlib.pylab import *
    show()
except:
    pass

