# Master dictionary containing all field identifiers known to CliMT
# with corresponding long names, units and nominal rank

KnownFields = {
    'ps':         ['surface pressure',              'mb', '2D'],
    'Ts':         ['surface temperature',            'K', '2D'],
    'z0':         ['geopotential at lowest p level', 'm', '2D'],
    'hIce':       ['sea ice thickness',              'm', '2D'],
    'zen':        ['solar zenith angle',           'deg', '2D'],
    'solin':      ['insolation',                 'W m-2', '2D'],
    'flus':       ['surface upwelling LW',       'W m-2', '2D'],
    'asdif':      ['diffuse SW albedo',              '-', '2D'],
    'asdir':      ['direct SW albedo',               '-', '2D'],
    'aldif':      ['diffuse LW albedo',              '-', '2D'],
    'aldir':      ['diffuse LW albedo',              '-', '2D'],
    'SrfSenFlx':  ['surface sensible heat flux', 'W m-2', '2D'],
    'SrfLatFlx':  ['surface latent heat flux',   'W m-2', '2D'],
    'taux':       ['surface zonal windstress',      'Pa', '2D'],
    'tauy':       ['surface merid windstress',      'Pa', '2D'],
    'SrfRadFlx':  ['net surface radiative flux', 'W m-2', '2D'],
    'Qflx':       ['Q flux (ocean heat convergence)', 'W m-2', '2D'],
    'SwToa':      ['net top-of-atmos SW flux', 'W m-2', '2D'],
    'LwToa':      ['net top-of-atmos LW flux', 'W m-2', '2D'],
    'SwSrf':      ['net surface SW flux',      'W m-2', '2D'],
    'LwSrf':      ['net surface LW flux',      'W m-2', '2D'],
    'SwToaCf':    ['top-of-atmos SW cloud forc', 'W m-2', '2D'],
    'LwToaCf':    ['top-of-atmos LW cloud forc', 'W m-2', '2D'],
    'SwSrfCf':    ['surface SW cloud forc',      'W m-2', '2D'],
    'LwSrfCf':    ['surface LW cloud forc',      'W m-2', '2D'],
    'cbmf':       ['cloud base mass flux',      'kg s-1', '2D'],
    'precc':      ['convective precip rate',  'mm day-1', '2D'],
    'Tsdot':      ['surface heating rate',       'K s-1', '2D'],
    'p':          ['atmospheric pressure',            'mb', '3D'],
    'dp':         ['level thickness',                 'mb', '3D'],
    'T':          ['atmospheric temperature',          'K', '3D'],
    'theta':      ['potential temperature',            'K', '3D'],
    'Te':         ['target temp, Newt. cooling',       'K', '3D'],
    'q':          ['specific humidity',           'g kg-1', '3D'],
    'U':          ['zonal wind',                   'm s-1', '3D'],
    'V':          ['meridional wind',              'm s-1', '3D'],
    'W':          ['pressure velocity',            'Pa s-1', '3D'],
    'psi':        ['streamfunction',                'm Pa s-1', '3D'],
    'cldf':       ['cloud fraction',                   '-', '3D'],
    'ciwp':       ['cloud ice water path',      'g m-2', '3D'],
    'clwp':       ['cloud liquid water path',      'g m-2', '3D'],
    'r_liq':      ['Effective radius, liquid drop','micron', '3D'],
    'r_ice':      ['Effective radius, ice drop','micron', '3D'],
    'o3':         ['ozone mass mix. ratio',      'kg kg-1', '3D'],
    'swflx':      ['SW radiative flux',            'W m-2', '3D'],
    'lwflx':      ['LW radiative flux',            'W m-2', '3D'],
    'lwuflx':     ['upward LW radiative flux',     'W m-2', '3D'],
    'lwdflx':     ['downward LW radiative flux',     'W m-2', '3D'],
    'swhr':       ['SW heating rate',            'K day-1', '3D'],
    'lwhr':       ['LW heating rate',            'K day-1', '3D'],
    'lwtau':      ['Broadband LW optical depth',       '-', '3D'],
    'TdotDyn':    ['dynamical heating rate',     'K day-1', '3D'],
    'TdotRad':    ['net radiative heating rate', 'K day-1', '3D'],
    'TdotTurb':   ['turbulent heating rate',     'K day-1', '3D'],
    'TdotConv':   ['convective heating rate',    'K day-1', '3D'],
    'qdotDyn':    ['dynamical moistening rate', 'g kg-1 day-1', '3D'],
    'qdotTurb':   ['turbulent moistening rate', 'g kg-1 day-1', '3D'],
    'qdotConv':   ['convective moistening rate','g kg-1 day-1', '3D'],
    'UdotDyn':    ['dynamical zonal accn',       'm s-1 day-1', '3D'],
    'UdotTurb':   ['turbulent zonal drag',       'm s-1 day-', '3D'],
    'VdotDyn':    ['dynamical merid accn',       'm s-1 day-1', '3D'],
    'VdotTurb':   ['turbulent merid drag',       'm s-1 day-1', '3D'],
    # SHOULD THESE BE PARAMETERS?
    'cloud_single_scattering_albedo': ['blah', 'blah', '3D'],
    'cloud_asymmetry_parameter': ['blah', 'blah', '3D'],
    'cloud_forward_scattering_fraction': ['blah', 'blah', '3D'],
    'tauc_lw': ['blah', 'blah', '3D'],
    'Tbound': ['temperature at upper edge of layer', 'K', '3D'],
    'lev': ['pressure at upper edge of layer', 'mb', '3D'],
    'h2o': ['alternative to specific humidity; H2O concentration','concentration','3D']
}

import os
from numpy import *
from grid      import Grid
from _timestep import leapfrog

class State:
    '''
    Contains all 2D and 3D fields used/produced by component.
    '''
    def __init__(self, Component, **kwargs):

        # Initialize dicts
        self.Now       = {} # Current time level (n)
        self.New       = {} # n+1
        self.Old       = {} # n-1

        # Initialize time counter (keeps track of 'age' of a time-marched State)
        if 'ElapsedTime' in kwargs:
            self.ElapsedTime = kwargs.pop('ElapsedTime')
        else:
            self.ElapsedTime = 0.

        # Initialize grid
        self.Grid = Grid(Component, **kwargs)
        
        # Initialize Required fields
        self._initializeFields(Component, **kwargs)

        # Initialize previous time level of prognostic fields
        for key in Component.Prognostic: self.Old[key] = self.Now[key].copy()

    def advance(self, Component):
        '''
        Advances State one timestep. 
        '''
        # If there's nothing to forecast, advance time counter and skip rest
        if len(Component.Inc) == 0:
            self.ElapsedTime += Component['dt']
            return

        # Invoke compiled leapfrog scheme
        afc = Component['afc']
        for key in Component.Inc:
            Shape = self.Now[key].shape
            self.Now[key], self.Old[key] = \
               leapfrog(afc, Component.Inc[key], self.Now[key], self.Old[key])
            # make sure array shape is unchanged 
            for dic in [self.Now, self.Old]: dic[key] = reshape(dic[key],Shape)

        # Advance time counter
        self.ElapsedTime += Component['dt']

    def _initializeFields(self, Component, **kwargs):
        """
        Sets initial values of requested fields
        """
        FieldNames = Component.Required
        LevType    = Component.LevType
        
        for Field in FieldNames:
            if Field not in KnownFields: raise KeyError, \
               '\n\n ++++ CliMT.State.init: Field %s unknown' % Field

        # Set fields' values to input or default
        Shape3D = self.Grid.Shape3D
        Shape2D = Shape3D[1:]
        
        for Field in FieldNames:
            exec('Shape = Shape%s' % KnownFields[Field][2])
            if Field in kwargs:
                try: self.Now[Field] = reshape( array(kwargs[Field]), Shape )
                except: raise \
                      '\n\n ++++ CliMT.State.init: Input %s incorrectly dimensioned' % Field
            else:
                self.Now[Field] = self._getDefault(Field, Shape, **kwargs)

        # Catch some anomalous cases
        if 'p' in FieldNames and 'p' not in kwargs and LevType == 'p':
            self.Now['p'] = transpose(resize(self.Grid['lev'],Shape3D[::-1]))

        if 'ps' in FieldNames and 'ps' not in kwargs:
            if 'p' in self.Now:
                dp = self.Now['p'][-1] - self.Now['p'][-2]
                self.Now['ps'] = self.Now['p'][-1] + dp/2.
            elif LevType == 'p':
                dp = self.Grid['lev'][-1] - self.Grid['lev'][-2]
                self.Now['ps'] = zeros(Shape2D,'d') + self.Grid['lev'][-1] + dp/2.

        #if 'Ts' in FieldNames and 'Ts' not in kwargs and 'T' in self.Now:
        #        self.Now['Ts'] = reshape( self.Now['T'][-1], Shape2D )

    def _getDefault(self, Field, Shape, **kwargs): 
        """
        Get default value of Field
        """
        # This routine instantiates ozone and insolation classes to get
        # default values. To do this correctly, we reset lat, lon in kwargs.
        # Also, we pop OutputFile and MonitorFields from kwargs to prevent 
        # those instances from creating output and monitor instances.
        for key in ['lat','lon']: kwargs[key]=self.Grid[key] 
        if 'OutputFile' in kwargs: kwargs.pop('OutputFile')
        if 'MonitorFields' in kwargs: kwargs.pop('MonitorFields')
        
        # Pressure
        if 'p' == Field:
            nlev = Shape[0]
            lev = (arange(nlev)+0.5) * 1000./nlev
            return transpose(resize(lev,Shape[::-1]))

        # Level thickness
        if 'dp' == Field: return zeros(Shape,'d') -99. # set as missing

        # Surface pressure
        if 'ps' == Field: return zeros(Shape,'d') + 1000.

        # Surface geopotential
        if 'z0' == Field: return zeros(Shape,'d')

        # Zonal wind
        if 'U' == Field: return zeros(Shape,'d')

        # Meridional wind
        if 'V' == Field: return zeros(Shape,'d')

        # Meridional streamfunc
        if 'psi' == Field: return zeros(Shape,'d')

        # Temperature
        if 'T' == Field: return zeros(Shape,'d') + 283.15

        # Potential temperature
        if 'theta' == Field: return zeros(Shape,'d') + 283.15

        # Surface temperature
        if 'Ts' == Field: return zeros(Shape,'d') + 283.15

        # Sea ice
        if 'hIce' == Field: return zeros(Shape,'d')

        # Moisture
        if 'q' == Field: return zeros(Shape,'d') + 1.e-5

        # Ozone
        if 'o3' == Field: 
            # create ozone instance to provide default ozone values
            # note that values will NOT evolve unless ozone explicitly
            # federated
            from ozone import ozone
            ozone = ozone(**kwargs) 
            return ozone.State['o3']

        # Cloud frac
        if 'cldf' == Field: return zeros(Shape,'d')

        # Cloud water path
        if 'clwp' == Field: return zeros(Shape,'d')
        if 'ciwp' == Field: return zeros(Shape,'d') -99. # set as missing by default

        # Effective radius cloud drops
        if 'r_liq' == Field: return zeros(Shape,'d') + 10.
        if 'r_ice' == Field: return zeros(Shape,'d') + 30.

        # Insolation
        if 'zen' == Field or 'solin' == Field:
            # create insolation instance to provide default zen and solin
            # values for radiation
            # note that values will NOT evolve unless insolation explicitly
            # federated with radiation 
            from insolation import insolation
            insolation = insolation(**kwargs)

        if 'zen'   == Field: return insolation.State['zen']
        if 'solin' == Field: return insolation.State['solin']

        # Surface upwelling LW
        if 'flus' == Field: return zeros(Shape,'d') -99. # set to missing as default

        # Albedos
        if 'asdir' == Field: return zeros(Shape,'d') + 0.07
        if 'asdif' == Field: return zeros(Shape,'d') + 0.07
        if 'aldir' == Field: return zeros(Shape,'d') + 0.07
        if 'aldif' == Field: return zeros(Shape,'d') + 0.07

        # Surface fluxes
        if 'SrfRadFlx' == Field: return zeros(Shape,'d')
        if 'SrfLatFlx' == Field: return zeros(Shape,'d')
        if 'SrfSenFlx' == Field: return zeros(Shape,'d')
        if 'Qflx'      == Field: return zeros(Shape,'d')

        # Cloud-base mass flux (required for Emanuel convection scheme)
        if 'cbmf' == Field: return zeros(Shape,'d')

    # The following methods allow a State instance to be treated as a dictionary
    def __getitem__(self,key):
        try: return self.Now[key]
        except: raise IndexError,'\n\n CliMT.State: %s not in State' % str(key)

    def __setitem__(self,key,value):
        self.Now[key] = value

    def __iter__(self):
        return self.Now.__iter__()

    def keys(self):
        return self.Now.keys()

    def update(self, dict):
        self.Now.update(dict)
