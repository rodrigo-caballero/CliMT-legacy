#!/usr/bin/env python

from component import Component
from numpy import *

class ocean(Component):
    """
    Interface to ocean models.

    * Instantiation:
    
      x = climt.ocean( <args> )

      where <args> are the following OPTIONAL arguments:
        Name     Dims  Meaning                 Units      Default 

    * Usage:
      Call instance directly to compute:

      x( <args> )

      where <args> are as above.
      
    * Output:
      Name    Meaning                Units            

    """
    def __init__(self, scheme='slab', **kwargs):
        # Initialize scheme-dependent attributes
        if   scheme == 'slab': self.__slab__init__()
        else: raise ValueError,'\n \n ++++ CliMT.ocean: Scheme %s unknown' % scheme

        # Initialize parameters, grid, fields etc
        Component.__init__(self, **kwargs)

    def __slab__init__(self):
        # Load extension
        try: import _slab_ocean
        except: raise ImportError, '\n \n ++++ CliMT.ocean: Could not load extension'
        # Define some attributes
        self.Name           = 'slab_ocean'
        self.LevType        = None
        self.Extension      = _slab_ocean
        self.driver         = _slab_ocean.driver
        self.SteppingScheme = 'explicit'
        self.ToExtension    = ['SrfRadFlx', 'SrfLatFlx', 'SrfSenFlx', 'Qflx', 'Hslab', 'rowl', 'Csw', 'dt']
        self.FromExtension  = ['Tsinc','Tsdot']
        self.Required       = ['Ts', 'SrfRadFlx', 'SrfLatFlx', 'SrfSenFlx', 'Qflx']
        self.Prognostic     = ['Ts']
        self.Diagnostic     = []

