#!/usr/bin/env python

#
# Sensitivity of radiative cloud forcing to changing cloud liquid water path (clwp)
# and cloud fraction (cldf) for a single-later cloud
#

import climt
try: from pylab import *
except: pass

# Instantiate radiation module
r = climt.radiation(scheme='ccm3')

# Initialise input data
nx=20
ny=20
nz=r.nlev
ps    = 1020.                                # Surface pressure
Ts    = 273.15 + 30.                         # Surface temperature
Tst   = 273.15 - 80.                         # 'Strospheric' temp
p     = ( arange(nz)+ 0.5 ) * ps/nz 
(T,q) = climt.thermodyn.moistadiabat(p, Ts, Tst, 0.7)
cloud_lev = int(nz*0.2) # put cloud roughly at 200 mb
cldfMax = 1.
cldf= (arange(ny)+0.5)*cldfMax/ny
cldf = cldf[::-1]
clwpMax = 200.
clwp = (arange(nx)+0.5)*clwpMax/nx

inputs = {}
inputs['Ts']    = zeros((ny,nx)) + Ts
inputs['ps']    = zeros((ny,nx)) + ps
inputs['solin'] = zeros((ny,nx)) + 400.
inputs['zen']   = zeros((ny,nx)) + 50.
inputs['p']     = zeros((nz,ny,nx)) + p[:,None,None]  
inputs['T']     = zeros((nz,ny,nx)) + T[:,None,None]  
inputs['q']     = zeros((nz,ny,nx)) + q[:,None,None]  
inputs['clwp']  = zeros((nz,ny,nx),'d')
inputs['cldf']  = zeros((nz,ny,nx),'d')
inputs['clwp'][cloud_lev] =  zeros((ny,nx)) + clwp[None,:]
inputs['cldf'][cloud_lev] =  zeros((ny,nx)) + cldf[:,None]

inputs['clwp'][cloud_lev+1] =  zeros((ny,nx)) + clwp[None,:]
inputs['cldf'][cloud_lev+1] =  zeros((ny,nx)) + cldf[:,None]
inputs['clwp'][cloud_lev-1] =  zeros((ny,nx)) + clwp[None,:]
inputs['cldf'][cloud_lev-1] =  zeros((ny,nx)) + cldf[:,None]



# Note that all profiles are computed at once 
r(**inputs) 

cldalb = - r['SwToaCf']/(r['swflx'][0]-r['SwToaCf'])
cldfrc = r['SwToaCf']+r['LwToaCf']

for i in range(ny):
    dum = [cldf[i]]
    dum.extend(cldfrc[:,i].tolist())
    print '%10.2f'*(nx+1) % tuple( dum )
dum = [0.]
dum.extend(clwp.tolist())
print '%10.2f'*(nx+1) % tuple( dum )

#try:
extent = (0,clwpMax,0,cldfMax)
imshow(cldfrc,extent=extent)
print cldfrc
colorbar()
xlim([0,clwpMax])
ylim([0,cldfMax])
xlabel('Cloud liquid water path [g m-2]')
ylabel('Cloud fraction')
title('TOA cloud radiative forcing [W/m2]')
show()
#except:
#    pass
