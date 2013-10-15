#-------Define miscellaneous constants
hIceMin = .01   # Min thickness sea ice
Hi = .5         # Depth of layer used to compute ice-top temp
albOcean = .2   # Ocean albedo
albIce = .5     # sea ice albedo
kappa = 2.      # Thermal conductivity of ice, W/mK
rhoi  = 916.    # Density of ice kg/m**3
Lf    = 334000. # Latent heat of freezing
Tfreeze = 271.  # Temperature of freezing

# One layer ice model
def ice1L(*args):
    Ts, hIce, SrfRadFlx, SrfLatFlx, SrfSenFlx, Hslab, rowl, Csw, dt = args
    net = SrfRadFlx+SrfLatFlx+SrfSenFlx
    hIce = hIce[0,0]
    # Make sure ice thickness is never negative
    hIce = max(0.,hIce)
    if hIce < 1.e-6:
        # There is currently no ice
        alb = albOcean
        if Ts > Tfreeze+.01: 
            # Above freezing. Do mixed layer ocean
            Tsdot = net/(rowl*Csw*Hslab)
            Tsinc = Tsdot*dt
        else:
            # Below freezing. Create first ice.
            hIce = hIceMin
            Tsinc = Tsdot = 0.
    else:
        # There is ice. Is is melting or growing?
        # Set the albedo. Replace this switch with a continuous function
        if hIce < .5:
            alb = albOcean
        else:
            alb = albIce
        if Ts < Tfreeze:
            # Ice is growing
            Tsdot = net/(rowl*Csw*Hi)
            Tsinc = dt*Tsdot
            dhdt = kappa*(Tfreeze-Ts)/(max(hIceMin,hIce)*rhoi*Lf)
            hIce += dhdt*dt
        else:
            # Ice is melting
            Tsinc = Tfreeze - Ts
            Tsdot = Tsinc/dt
            dhdt = -net/(Lf*rhoi)
            print Ts,alb,net,hIce,dhdt*dt
            hIce += dhdt*dt
            if hIce < .1: # For debugging. Should be zero otherwise
                hIce = 0.
                Tsinc += .02 # To get open ocean started
    asdir = asdif = aldir = aldif = Ts*0.+alb
    hIce = Ts*0.+hIce
    return Tsinc,Tsdot,hIce,asdir,aldir,asdif,aldif

    
            
            
