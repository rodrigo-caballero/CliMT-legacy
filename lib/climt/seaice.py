#!/usr/bin/env python
#Modified by rtp to include ice.
#I tried to save this under a separate name, IceOcean, but
#then for some reason climt didn't import it. Why? 

from component import Component
from numpy import *

class seaice(Component):
    """
    Interface to seaice models.

    * Instantiation:
    
      x = climt.seaice( <args> )

      where <args> are the following OPTIONAL arguments:
        Name     Dims  Meaning                 Units      Default 

    * Usage:
      Call instance directly to compute:

      x( <args> )

      where <args> are as above.
      
    * Output:
      Name    Meaning                Units            

    """
    def __init__(self, scheme='ice1L', **kwargs):
        # Initialize scheme-dependent attributes
        if   scheme == 'ice1L':
            self.__ice1L__init__()
        else:
            raise ValueError,'\n \n ++++ CliMT.seaice: Scheme %s unknown' % scheme

        # Initialize parameters, grid, fields etc
        Component.__init__(self, **kwargs)

    def __ice1L__init__(self):
        # Load extension
        try: import ice1L
        except: raise ImportError, '\n \n ++++ CliMT.seaice: Could not load extension'
        # Define some attributes
        self.Name           = 'ice1L_seaice'
        self.LevType        = None
        self.Extension      = ice1L
        self.driver         = ice1L.ice1L
        self.SteppingScheme = 'explicit'
        self.ToExtension    = ['Ts','hIce','SrfRadFlx','SrfLatFlx','SrfSenFlx','Hslab','rowl','Csw', 'dt']
        self.FromExtension  = ['Tsinc','Tsdot','hIce','asdir','aldir','asdif','aldif']
        self.Required       = ['Ts','hIce','SrfRadFlx','SrfLatFlx','SrfSenFlx']
        self.Prognostic     = ['Ts']
        self.Diagnostic     = ['hIce','asdir','aldir','asdif','aldif']

