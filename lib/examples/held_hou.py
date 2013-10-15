#!/usr/bin/python2.3

#PBS -N held_hou
#PBS -l ncpus=1
#PBS -l nodes=1

try: 
    import os
    WorkDir= os.getenv('PBS_O_WORKDIR')
    os.chdir(WorkDir)
except:
    pass

import climt
from numpy import *

nlat          = 60
nlev          = 35
MinLat        = -90.
MaxLat        = 90.
MinLev        = 0.
MaxLev        = 1000.
Ndays         = 200.  # Total length of run (days)

kwargs={}
kwargs['Newt']            = 20.
kwargs['nuv']             = 0.05
kwargs['Pr']              = 1.
kwargs['dt']              = 60.*5.
#kwargs['RestartFile']     = 'held_hou.nc'
kwargs['OutputFile']      = 'held_hou.nc'
kwargs['OutputFreq']      = 86400. * 5.
kwargs['MonitorFields']   = ['psi','U','T','q']
kwargs['MonitorFreq']     = 60 *60.*4.
kwargs['do_srf_lat_flx']  = 0
kwargs['do_srf_sen_flx']  = 0

# Grid
kwargs['lat'] = (arange(nlat)+0.5)*(MaxLat-MinLat)/nlat + MinLat
kwargs['lev'] = (arange(nlev)+0.5)*(MaxLev-MinLev)/nlev + MinLev

kwargs['q'] = zeros((nlev,nlat,1)) + 1.e-9
kwargs['q'][nlev*4/5,:] =1.

# Set up federation
dyn  = climt.dynamics(scheme='axisymmetric')
tur  = climt.turbulence()
fed  = climt.federation(dyn,tur, **kwargs)

# Run
fed.step(Ndays*86400.)

try:
    from matplotlib.pylab import *
    show()
except:
    pass

