#!/usr/bin/env python

import climt
from numpy import *
try:
    from pylab import *
    gotPylab = True
except:
    gotPylab = False

def getProfile(nlayers, kcentral, kmin, kmax):
    # returns profile with N layers centered on layer k
    x = zeros(kmax,'d')
    k1 = max(kmin, kcentral-nlayers/2-1)
    k2 = min(kmax, kcentral+nlayers/2)
    x[k1:k2] = 1.
    return x

#--- initialise data
nx    = 7
ny    = 10
nz    = climt.get_nlev()
ps    = 1020.                                # Surface pressure
Ts    = 273.15 + 30.                         # Surface temperature
Tst   = 273.15 - 80.                         # 'Strospheric' temp
p     = ( arange(nz)+0.5 ) * ps/nz 
(T,q) = climt.thermodyn.moistadiabat(p, Ts, Tst, 0.7)
cldf = zeros((nz,nx),'d')
for i in range(nx):
    kcentral = 11
    nlayers = 2*i+3
    print getProfile(nlayers, kcentral, 3, nz)
    cldf[:,i] = getProfile(nlayers, kcentral, 3, nz)
zen = arange(ny)*5. + 5.
print zen

inputs = {}
inputs['Ts']    = zeros((ny,nx))    + Ts
inputs['ps']    = zeros((ny,nx))    + ps
inputs['solin'] = zeros((ny,nx))    + 400.
inputs['zen']   = zeros((ny,nx))    + zen[::-1,None]
inputs['p']     = zeros((nz,ny,nx)) + p[:,None,None]  
inputs['T']     = zeros((nz,ny,nx)) + T[:,None,None]  
inputs['q']     = zeros((nz,ny,nx)) + q[:,None,None]  
inputs['clwp']  = zeros((nz,ny,nx)) + 15.
inputs['cldf']  = zeros((nz,ny,nx)) + cldf[:,None,:]

r = climt.radiation(scheme='cam3', do_lw=0)

LoCldf = 0.01
HiCldf = 0.99
LoTime =1.
HiTime =1.
LoWeight = LoTime/(LoTime+HiTime)
HiWeight = HiTime/(LoTime+HiTime)

Mean = LoCldf*LoWeight + HiCldf*HiWeight

inputs['cldf'] *= LoCldf
r(**inputs)
Lo = r['SwToaCf']

inputs['cldf'] *= HiCldf/LoCldf
r(**inputs)
Hi = r['SwToaCf']

inputs['cldf'] *= Mean/HiCldf
r(**inputs)
M = r['SwToaCf']

V = Hi*HiWeight + Lo*LoWeight - M


if gotPylab:
    ticks=array([3,5,7,9,11,13,15]) #*ps/nz
    xmin=ticks[0] - 1 #ps/nz
    xmax=ticks[-1] + 1 #3ps/nz
    print xmin,xmax,ps/nz
    print ticks
    extent = (xmin,xmax,2.5,52.5)
    imshow(V,extent=extent,interpolation='nearest',cmap=cm.gray)
    colorbar()
#    clim(vmin=-10,vmax=50)
    setp(gca(),'xlim',[xmin,xmax],'ylim',[2.5,52.5])
    #xlabel('Depth of cloud deck [mb]')
    xlabel('Number of layers in cloud deck')
    xticks(ticks.tolist())
    ylabel('Zenith angle [deg]')
    yticks(zen)
    title('Variability contribution to SW CRF [W/m2]')
    savefig('sw_variability.ps')
    show()

