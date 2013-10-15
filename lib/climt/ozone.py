#!/usr/bin/env python

from component import Component

class ozone(Component):
    """
    A module for computing atmospheric ozone concentration.

    * Instantiation:

      x = climt.ozone( <args> )

      where <args> are the following OPTIONAL arguments:
      Name  Dims Meaning               Units   Default  Notes
      p     1-3  Atmospheric pressure  mb               Default is equispaced 0-1000 mb. p[0] is top level (smallest pressure)

    * Usage:
      Call instance directly to compute ozone conc:

      x( <args> )

      where <args> are as above.
      
    * Output (available as x.o3):
      Name     Meaning      Units            
      o3       Ozone conc   kg/kg
    """
    def __init__(self, **kwargs):
        # Load extension
        try: import _ozone
        except: raise ImportError, '\n \n ++++ CliMT.ozone: Could not load ozone extension'
        # Define some attributes
        self.Name           = 'ozone'
        self.LevType        = 'p'
        self.Extension      = _ozone
        self.driver         = _ozone.driver
        self.SteppingScheme  = None
        self.ToExtension    = ['p']
        self.FromExtension  = ['o3']
        self.Required       = ['p']
        self.Prognostic     = []
        self.Diagnostic     = ['o3']
        # Initialize fields etc. 
        Component.__init__(self, **kwargs)
        
    # Return ozone concentrations
    def get_o3(self, **kwargs):
        self.__call__(**kwargs) # Re-initialize and compute
        return self.o3
