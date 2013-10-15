#!/usr/bin/env python

import climt
from numpy import *

Ndays         = 5.  # Total length of run (days)

kwargs={}
kwargs['lat']            = ['1.','2.']
kwargs['solin']            = ['300.','310.']
kwargs['dt']              = 60.*10.
#kwargs['RestartFile']     = 'two_column.nc'
kwargs['OutputFile']      = 'two_column.nc'
kwargs['OutputFreq']      = 60*10.
#kwargs['MonitorFields']   = ['U','T','q']
#kwargs['MonitorFreq']     = 60 *60.*4.

# Set up federation
dyn  = climt.dynamics(scheme='two_column')
tur  = climt.turbulence()
rad  = climt.radiation(scheme='cam3')
con  = climt.convection(scheme='hard')
oce  = climt.ocean()
fed  = climt.federation(dyn,tur,rad,con,oce, **kwargs)

# Run
fed.step(Ndays*86400.)

try:
    from matplotlib.pylab import *
    show()
except:
    pass

