#!/usr/bin/env python

from component  import Component
from numpy import *
import string

class dynamics(Component):
    """
    Interface to atmospheric dynamical cores. 

    * Instantiation:
    
      x=climt.dynamics( <args> )
      
      where <args> are the following OPTIONAL arguments:
      Name           Dims  Meaning                  Units     Default         Notes
      scheme            0  Dynamical core          (string) 'axisymmetric'  Choices are: 'axisymmetric'
      T               1-3  Temperature                K          283.15             
      q               1-3  Specific humidity          g/kg       1.e-5
      U               1-3  Zonal wind                 m/s        0.
      V               1-3  Meridional wind            m/s        0.
        
    * Usage:
      Call instance directly to compute dynamical tendencies.

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
    def __init__(self, scheme = 'axisymmetric', **kwargs):
        # Initialize scheme-dependent attributes
        if scheme not in ['axisymmetric','two_column']:
            raise ValueError,'\n \n ++++ CliMT.dynamics: Scheme %s unknown' % scheme
        exec('self.__%s_dynamics__init__()' % string.lower(scheme))
        # Initialize fields etc. 
        Component.__init__(self, **kwargs)
        
    def __axisymmetric_dynamics__init__(self):
        # Load extension
        try: import _axisymmetric_dynamics
        except: raise ImportError, \
          '\n \n ++++ CliMT.dynamics: Could not load axisymmetric scheme'
        # Define some attributes
        self.Name           = 'axisymmetric_dynamics'
        self.LevType        = 'p'
        self.Extension      = _axisymmetric_dynamics
        self.driver         = _axisymmetric_dynamics.driver
        self.SteppingScheme = 'semi-implicit'
        self.ToExtension    = ['Rd','Cpd','r','omega','delh','delv','Newt','dt',
                               'lat','lev','T','U','V','q','Vold']
        self.FromExtension  = ['V','Tinc','Uinc','Vinc','qinc','psi','theta','Te','W',
                               'TdotDyn','UdotDyn','VdotDyn','qdotDyn']
        self.Required       = ['T','U','V','q']
        self.Prognostic     = ['T','U','V','q']
        self.Diagnostic     = ['V','psi','theta','Te','W','TdotDyn','UdotDyn','VdotDyn','qdotDyn']

    def __two_column_dynamics__init__(self):
        # Load extension
        try: import _two_column_dynamics
        except: raise ImportError, \
          '\n \n ++++ CliMT.dynamics: Could not load two-column scheme'
        # Define some attributes
        self.Name           = 'two_column_dynamics'
        self.LevType       = 'p'
        self.Extension      = _two_column_dynamics
        self.driver         = _two_column_dynamics.driver
        self.SteppingScheme = 'explicit'
        self.ToExtension    = ['dt','Rd','Rv','Cpd','Cpv','g','p','z0','V','T','q']
        self.FromExtension  = ['Vinc','Tinc','qinc','z0inc','W']
        self.Required       = ['p','z0','V','T','q']
        self.Prognostic     = ['V','T','q','z0']
        self.Diagnostic     = ['W']
