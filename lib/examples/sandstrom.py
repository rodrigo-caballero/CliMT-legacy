#!/usr/bin/env python

import climt
from numpy import *

#################### User adjustable:
# Grid
nlat        = 41
nlev        = 25
MinLat      = -90.
MaxLat      = 90.
MinLev      = 0.
MaxLev      = 1000.

# Parameters
kwargs={}
kwargs['nuv']             = 10.
kwargs['Pr']              = 100.
kwargs['dt']              = 60.*15.
#kwargs['RestartFile']     = 'sandstrom.nc'
#kwargs['OutputFile']      = 'sandstrom.nc'
#kwargs['OutputFreq']      = 86400. # 1 day
kwargs['MonitorFields']   = ['psi','SrfRadFlx','T','Ts']
kwargs['MonitorFreq']     = 60.*60.
kwargs['do_srf_heat_flx'] = 1
kwargs['Lv']              = 0.
kwargs['Fixed']           = ['q']

# Initial fields
kwargs['T'] = zeros((nlev,nlat)) + 100.
kwargs['q'] = zeros((nlev,nlat)) + 1.e-9

# Run length (days)
Ndays  = 3000  
########################## End user adjustble

kwargs['lat']  = (arange(nlat)+0.5)*(MaxLat-MinLat)/nlat + MinLat
kwargs['lev']  = (arange(nlev)+0.5)*(MaxLev-MinLev)/nlev + MinLev

# Set up federation
tur = climt.turbulence()
dyn = climt.dynamics()
ocn = climt.ocean()
con = climt.convection()
fed  = climt.federation(dyn, tur, ocn, con, **kwargs)

# Run
Nsteps = int(Ndays*86400./kwargs['dt'])
ins = climt.insolation(lat=kwargs['lat'], avg='annual')
CoolingRate = 0./86400.

for l in range(Nsteps):
    fed.State['SrfRadFlx'] = ins.State['solin']*0.5 - fed['stebol']*fed.State['Ts']**4
    fed.step()
    fed.State['T'][4:nlev,:,:] -= CoolingRate*kwargs['dt']*ones((nlev-4,nlat,1))

try:
    from matplotlib.pylab import *
    show()
except:
    pass

