#!/usr/bin/env python

from component import Component

class insolation(Component):
    """
    A module for computing Earth's orbital parameters, zenith angle and insolation.

    * Instantiation:
    
      x = climt.insolation( <args> )

      where <args> are the following OPTIONAL arguments:
        Name     Dims  Meaning              Units      Default  Notes
        scon        0  Solar constant       W m-2      1367.
        orb_year    0  Year AD for which    (integer)  1995   Only accurate within 1 million
                       orbital parmeters                      years of 1950. Taken from CCM3.6,
                       are computed                           based on Berger (1978)
        eccen       0  eccentricity          --       (1995)  | If specified, these
        obliq       0  obliquity             deg      (1995)  | will override values
        prece       0  longitude perihelion  deg      (1995)  | set by orb_year
        avg         0  Time averaging      (string)  'annual' 'inst'   => instantaneous insolation
                                                              'daily'  => daily-mean insolation
                                                              'annual' => annual-mean insolation
        calday      0  Julian calendar day  (float)    80.5    vernal equinox
        lat       0-1  Latitude              degree     0.
        lon       0-1  Longitude             degree     0.

    * Usage:
      Call instance directly to compute zenith angle and insolation:

      x( <args> )

      where <args> are as above.
      
    * Output (accessible as x.zen, etc):
      Name    Meaning                Units            
      zen     Solar zenith angle     degree
      solin   Insolation             W m-2    
    """
    def __init__(self, avg='annual', **kwargs):
        # Load extension
        try: import _insolation
        except: raise ImportError, '\n \n ++++ CliMT.insolation: Could not load insolation extension'
        # Define some attributes
        self.Name           = 'insolation'
        self.avg            = avg
        self.LevType        = None
        self.SteppingScheme = None
        self.Extension      = _insolation
        exec( 'self.driver = _insolation.%s_driver' % avg )
        self.ToExtension   = ['calday','lat','lon','scon','radius','daysperyear']
        self.FromExtension = ['zen','solin']
        self.Required      = []
        self.Prognostic    = []
        self.Diagnostic    = ['zen','solin']

        # Initialize fields etc.
        Component.__init__(self, **kwargs)
        self.setOrbParams(**kwargs)
        self.compute()
        # Check input
        errmsg='\n +++ CliMT.insolation: make sure 0 <= calday <= %s\n' % self.Params['daysperyear']
        assert 0 <= self.Params['calday'] <= self.Params['daysperyear'], errmsg

    # Sets orbital parameters
    def setOrbParams(self, **kwargs):

        # set orb params by orb_year
        if 'orb_year' in kwargs: orb_year = kwargs['orb_year']
        else: orb_year = self.Params['orb_year']
        self.setOrbParamsByYear(orb_year=orb_year)

        # if provided, set each orb param separately, overriding those set by orb_year
        if 'eccen' in kwargs:    
	    eccen = kwargs['eccen']
	    errmsg='\n\n +++ CliMT.insolation: eccen must be in range [0,1]'
	    assert 0. <= eccen <= 1., errmsg
	    self.eccen = self.Extension.orbpar.eccen = eccen

        if 'obliq' in kwargs:    
	    obliq = kwargs['obliq']
	    errmsg='\n\n +++ CliMT.insolation: obliq must be in range [-90,90] degrees'
	    assert -90. <= obliq <= 90., errmsg
	    self.obliq = self.Extension.orbpar.obliq = obliq

        if 'prece' in kwargs:    
	    prece = kwargs['prece']
	    errmsg='\n\n +++ CliMT.insolation: prece must be in range [0,360] degrees'
	    assert 0. <= prece <= 360., errmsg
	    self.prece = self.Extension.orbpar.mvelp = prece
                
    # Sets orbital parameters to those of orb_year
    def setOrbParamsByYear(self, orb_year=1995):
        # check input
        errmsg='\n\n +++ CliMT.insolation: orb_year must be in range 1950 +- 1million years'
        assert 1950-1000000 <= orb_year <= 1950+1000000, errmsg
        # compute orb params
        self.Extension.berger78_driver(orb_year)
        # f2py magic lets us get values from orbpar common block 
        self.eccen = self.Extension.orbpar.eccen
        self.obliq = self.Extension.orbpar.obliq
        self.prece = self.Extension.orbpar.mvelp

    # Returns orbital parameters (eccentriciy, obliquity, lon. vernal exquinox)
    def getOrbParams(self):
        return (self.eccen, self.obliq, self.prece)

    # Provides a simple interface to extension, useful e.g. for diagnostics.
    # Overrides __call__ inherited from Component
    def __call__(self,**kwargs):

        # Check input
        for key in ['calday','daysperyear']:
            if key in kwargs: self.Params[key] = kwargs[key]
        errmsg='\n +++ CliMT.insolation: make sure 0 <= calday <= %s\n'%self.Params['daysperyear']
        assert 0. <= self.Params['calday'] <= self.Params['daysperyear'], errmsg
        
        # Reset orb params
        self.setOrbParams(**kwargs)

        # Initialize grid and state
        Component.__call__(self,**kwargs)


