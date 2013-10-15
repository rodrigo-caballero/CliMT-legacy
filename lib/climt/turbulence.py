#!/usr/bin/env python

from component  import Component
from numpy import *
import string

class turbulence(Component):
    """
    Interface to atmospheric vertical diffusion and PBL schemes.

    * Instantiation:
    
      x=climt.turbulence( <args> )
      
      where <args> are the following OPTIONAL arguments:
      Name           Dims  Meaning                  Units     Default         Notes
      scheme            0  Turbulence scheme        (string)  'simple'  Choices are: 'ccm3', 'simple' 
      nuv               0  Vertical viscosity        m2/s        1.     'simple' scheme only
      Pr                0  Prandtl number            -           1.      Pr = viscosity/conductance
      Cd                0  Surface flux coeff        -           1.3e-3  'simple' only
      u0                0  Surf. wind for flux        m/s        5.      'simple' only
      do_srf_sen_flx  0  Switch srf sens flux on/off            1       0=off, 1=on
      do_srf_lat_flx  0  Switch srf laten flux on/off           1       0=off, 1=on
      do_srf_mom_flx   0  Switch srf mom flux on/off            1       0=off, 1=on
      Ts              0-2  Surface temperature        K          283.15
      ps              0-2  Surface pressure           mb         1000.
      p               1-3  Atmospheric pressure       mb         Default is equispaced [0,ps].
      T               1-3  Temperature                K          283.15             
      q               1-3  Specific humidity          g/kg       1.e-5
      U               1-3  Zonal wind                 m/s        0.
      V               1-3  Meridional wind            m/s        0.
        
    * Usage:
      Call instance directly to compute radiative fluxes and heating rates:

      x( <args> )

      where <args> are as above.
        
    * Output (accessible as x.swflx etc.):
      Name       Meaning                         Units   Notes            
      Tdot       Turbulent heating rate          K s-1
      qdot       Turbulent humidification rate   g/kg s-1
      Udot       Turbulent drag                  m s-2
      Vdot       Turbulent drag                  m s-2
      SrfSenFlx  Surface sens. heat flux         W m-2
      SrfLatFlx  Surface lat. heat flux          W m-2
      taux       Surface stress                  Pa
      tauy       Surface stress                  Pa
    """
    def __init__(self, scheme = 'simple', **kwargs):

        # Initialize scheme-dependent attributes
        try: exec('self.__%s__init__()' % string.lower(scheme))
        except: raise ValueError,'\n \n ++++ CliMT.turbulence: Scheme %s unknown' % scheme

        # Initialize parameters, grid, fields etc
        Component.__init__(self, **kwargs)
        
    def __simple__init__(self):

        # Load extension
        try: import _simple_turbulence
        except: raise ImportError, '\n \n ++++ CliMT.turbulence: Could not load simple scheme'

        # Define some attributes
        self.Name           = 'simple_turbulence'
        self.LevType       = 'p'
        self.Extension      = _simple_turbulence
        self.driver         = _simple_turbulence.driver
        self.SteppingScheme = 'implicit'
        self.ToExtension    = ['do_srf_mom_flx','do_srf_sen_flx','do_srf_lat_flx',\
                               'r','Pr','Rd','Cpd','Lv','epsilon','nuv','Cd','g','u0','dt', \
                               'lat','lev','ps','Ts','T','U','V','q']
        self.FromExtension  = ['Tinc','Uinc','Vinc','qinc', \
                               'TdotTurb','UdotTurb','VdotTurb','qdotTurb', \
                               'SrfLatFlx','SrfSenFlx']
        self.Required       = ['ps','Ts','T','U','V','q']
        self.Prognostic     = ['T','U','V','q']
        self.Diagnostic     = ['TdotTurb','UdotTurb','VdotTurb','qdotTurb', \
                               'SrfLatFlx','SrfSenFlx']

    def __ccm3__init__(self):

        # Load extension
        try: import _ccm3_turbulence
        except: raise ImportError, '\n \n ++++ CliMT.turbulence: Could not load CCM3 scheme'

        # Define some attributes
        self.Name           = 'ccm3_turbulence'
        self.LevType        = 'p'
        self.Extension      = _ccm3_turbulence
        self.driver         = _ccm3_turbulence.driver
        self.SteppingScheme = 'implicit'
        self.ToExtension    = ['do_srf_mom_flx','do_srf_sen_flx','do_srf_lat_flx',\
                               'Cpd','g','Lv','Rd','Rv','Cpv','dt','p','ps','U','V','T','Ts','q']
        self.FromExtension  = ['Tinc','Uinc','Vinc','qinc', \
                               'TdotTurb','UdotTurb','VdotTurb','qdotTurb', \
                               'SrfSenFlx','SrfLatFlx','taux','tauy']
        self.Required       = ['p','ps','U','V','T','Ts','q']
        self.Prognostic     = ['U','V','T','q']
        self.Diagnostic     = ['TdotTurb','UdotTurb','VdotTurb','qdotTurb', \
                               'SrfSenFlx','SrfLatFlx','taux','tauy']

