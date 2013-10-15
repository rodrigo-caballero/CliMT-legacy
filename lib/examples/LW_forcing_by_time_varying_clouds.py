#!/usr/bin/env python

from climt import *
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
nx    = 4
ny    = 8
nz    = get_nlev()
ps    = 1020. 
Ts    = 273.15 + 30.
Tst   = 273.15 - 80.
p     = ( arange(nz)+0.5 ) * ps/nz 
(T,q) = thermodyn.moistadiabat(p, Ts, Tst, 0.7)
cldf = zeros((nz,ny,nx))*0.
for i in range(nx):
    for j in range(ny):
        kcentral = j+7
        nlayers = 2*i+3
        print getProfile(nlayers, kcentral, 3, nz)
        cldf[:,ny-1-j,i] = getProfile(nlayers, kcentral, 3, nz)

inputs = {}
inputs['Ts']    = zeros((ny,nx))    + Ts
inputs['ps']    = zeros((ny,nx))    + ps
inputs['p']     = zeros((nz,ny,nx)) + p[:,None,None]  
inputs['T']     = zeros((nz,ny,nx)) + T[:,None,None]  
inputs['q']     = zeros((nz,ny,nx)) + q[:,None,None]  
inputs['clwp']  = zeros((nz,ny,nx)) + 15.
inputs['cldf']  = cldf

r = radiation(scheme='cam3', do_sw=0)

LoCldf = 0.01
HiCldf = 0.99
LoTime =1.
HiTime =1.
LoWeight = LoTime/(LoTime+HiTime)
HiWeight = HiTime/(LoTime+HiTime)

Mean = LoCldf*LoWeight + HiCldf*HiWeight

inputs['cldf'] *= LoCldf
print inputs['cldf']
r(**inputs)
Lo = r['LwToaCf']

inputs['cldf'] *= HiCldf/LoCldf
r(**inputs)
Hi = r['LwToaCf']

inputs['cldf'] *= Mean/HiCldf
r(**inputs)
M = r['LwToaCf']

V = Hi*HiWeight + Lo*LoWeight - M
print Hi,Lo

if gotPylab:
    dp = ps/nz
    ticks=array([3,5,7,9]) #*dp
    xmin=ticks[0] - 1 #dp
    xmax=ticks[-1] + 1 #dp
    ymin=p[6]-dp/2.
    ymax=p[13]+dp/2.
    extent = (xmin,xmax,ymin,ymax)
    print V
    imshow(V,extent=extent,interpolation='nearest',cmap=cm.gray)
    #clim(vmin=-20,vmax=50)
    colorbar()
    #xlabel('Depth of cloud deck [mb]')
    xlabel('Number of layers in cloud deck')
    xticks(ticks.tolist())
    ylabel('Height of cloud deck [mb]')
    yticks(p[14:5:-1])
    setp(gca(),'xlim',[xmin,xmax],'ylim',[ymax,ymin])
    title('Variability contribution to LW CRF [W/m2]')
    savefig('lw_variability.ps')
    show()

