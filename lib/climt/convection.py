#!/usr/bin/env python

from component  import Component
from numpy import *
import string

class convection(Component):
    '''
    Interface to atmospheric moist convection schemes.

    * Instantiation
      x=climt.convection( <args> )
      
      where <args> are the following OPTIONAL arguments:
      Name   Dims  Meaning              Units     Default   Notes
      scheme    0  Convective scheme    (string)  'hard'  Choices are: 'hard','emanuel','sbm'
    '''
    def __init__(self, scheme = 'hard', **kwargs):
        # Initialize scheme-dependent attributes
        try: exec('self.__%s__init__()' % string.lower(scheme))
        except: raise ValueError,'\n \n ++++ CliMT.convection: Scheme "%s" unknown' % scheme

        # Initialize fields etc. 
        Component.__init__(self, **kwargs)
        
    def __emanuel__init__(self):
        # Load extension
        try: import _emanuel_convection
        except: raise ImportError, '\n \n ++++ CliMT.convection: Could not load Emanuel scheme'
        # Define some attributes
        self.Name           = 'emanuel_convection'
        self.LevType        = 'p'
        self.Extension      = _emanuel_convection
        self.driver         = _emanuel_convection.driver
        self.SteppingScheme = 'explicit'
        self.ToExtension    = ['afc','dt', 'UpdateFreq','p','ps','Cpd','Cpv','Cl','Rv',\
                               'Rd','Lv','g','rowl','elcrit','tlcrit','entp','sigd','sigs',\
                               'omtrain','omtsnow','coeffr','coeffs','cu','beta','dtmax',\
                               'alpha','damp','T','q','cbmf']
        self.FromExtension  = ['T','q','theta','cbmf','Tinc','qinc','TdotConv','qdotConv','precc']
        self.Required       = ['p','ps','T','q','cbmf']
        self.Prognostic     = ['T','q']
        self.Diagnostic     = ['T','q','theta','TdotConv','qdotConv','cbmf','precc']
                              # adiab adjustment resets T,q profiles on output

    def __hard__init__(self):
        # Load extension
        try: import _hard_adjustment
        except: raise ImportError, '\n \n ++++ CliMT.convection: Could not load hard adjustment scheme'
        # Define some attributes
        self.Name           = 'hard_adjustment'
        self.LevType        = 'p'
        self.Extension      = _hard_adjustment
        self.driver         = _hard_adjustment.driver
        self.SteppingScheme = None
        self.ToExtension    = ['lapse','g','Rd','Cpd','afc','UpdateFreq','p','ps','T']
        self.FromExtension  = ['T','theta','TdotConv']
        self.Required       = ['p','ps','T']
        self.Prognostic     = []
        self.Diagnostic     = ['T','theta','TdotConv']

    def __sbm__init__(self):
        # Load extension
        try: import _sbm_convection
        except: raise ImportError, '\n \n ++++ CliMT.convection: Could not load SBM scheme'
        # Define some attributes
        self.Name           = 'sbm_convection'
        self.LevType        = 'p'
        self.Extension      = _sbm_convection
        self.driver         = _sbm_convection.driver
        self.SteppingScheme = 'explicit'
        self.ToExtension    = ['Lv','Cpd','g','Rd','Rv','Cpv','rowl','dt',\
                              'T','q','ps','p','tau_bm','es0','T00','rhbm',\
                              'Mv','Ma']
        self.FromExtension  = ['Tinc','qinc','precc','TdotConv','qdotConv','theta']
        self.Required       = ['T','q','ps','p']
        self.Prognostic     = ['T','q']
        self.Diagnostic     = ['precc','TdotConv','qdotConv']

    def __ccm3__init__(self):
        raise '\n\n ++++CliMT.convection.init: ccm3 convective scheme not yet implemented' 
