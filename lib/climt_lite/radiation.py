#!/usr/bin/env python

import string,os
from numpy import *
from component  import Component

class radiation(Component):
    """
    Interface to atmospheric radiation schemes.
    
    * Instantiation:
    
      x=climt.radiation( <args> )
      
      where <args> are the following OPTIONAL arguments:
      Name   Dims  Meaning              Units     Default   Notes
      scheme    0  Radiative scheme     (string)    'cam3'  Choices are: 'cam3', 'ccm3', 'chou', 'greygas', 'rrtm' 
      do_sw     0  Shortwave switch     (integer)       1   1 / 0 => do / do not compute SW        
      do_lw     0  Longwave switch      (integer)       1   1 / 0 => do / do not compute LW       
      scon      0  Solar constant       W m-2        1367.
      orb_yr    0  Orbital year         (integer)    1995   Year used to compute orbital params
      avg       0  Insolation average   (string)   'daily'  Choices are: 'inst', 'daily', 'annual'
      co2       0  CO2                  ppmv          330.
      n2o       0  N2O                  ppmv            0.
      ch4       0  CH4                  ppmv            0.
      cfc11     0  CFC11                ppmv            0.
      cfc12     0  CFC12                ppmv            0.
      cfc22     0  CFC22                ppmv            0.   Chou scheme only
      tauvis    0  Aerosol opt. depth   (float)         0.   CCM3 only
      tau_inf   0  Total opt. depth        -            1.   Greygas scheme only
      alpha_greygas 0  Tau shape parameter   -          1.   Greygas scheme only  
      calday    0  Calendar day         (float)      80.5    Insolation computed at specified
      lat     0-1  Latitude                 dgr      0.         day and lat/lon if  solin 
      lon     0-1  Longitude                dgr      0.         and zen are NOT specified
      solin   0-2  Insolation               W/m2     417.4   Daily-mean on equator at equinox
      zen     0-2  Solar zenith angle       dgr      72.2    Daily-mean on equator at equinox
      Ts      0-2  Surface temperature      K        283.15
      ps      0-2  Surface pressure         mb       1000.
      aldif   0-2  Diffuse IR albedo        (frac)   0.07
      aldir   0-2  Direct IR albedo         (frac)   0.07
      asdif   0-2  Diffuse UV+vis alb       (frac)   0.07
      asdir   0-2  Direct UV+vis alb        (frac)   0.07
      p       1-3  Atmospheric pressure     mb               Default is equispaced 0-ps. p[0] is top level
      T       1-3  Temperature              K        283.15  Isothermal
      q       1-3  Specific humidity        g/kg     1.e-5
      o3      1-3  Ozone mass mix. rat.     kg/kg            Default obtained by interpolating a tropical data profile
      cldf    1-3  Cloud fraction           frac     0.
      r_liq   1-3  Drop radius, liquid      micron   10.   
      r_ice   1-3  Drop radius, ice         micron   30.   
      clwp    1-3  Cloud liquid water path  g/m2     0.     
      ciwp    1-3  Cloud ice water path     g/m2     -99.   If not passed explicitly, ice frac computed internally (CAM3 only)
      in_cld    0  Cloud water path flag     -       0       0 / 1 => grid avg / in-cloud water paths (CAM3 only)
      flus    1-3  Upwelling LW at surface  W/m2     -99.   If not passed explicitly, computed from Ts using emiss=1 (CAM3 only)
                                                             
    * Usage:
      Call instance directly to compute radiative fluxes and heating rates:
      
      x( <args> )
      
      where <args> are as above.
        
    * Output (accessible as x['swhr'] etc.):
      Name       Meaning                         Units   Notes            
      - Heating rates:
      swhr       SW heating rate                 K day-1
      lwhr       LW heating rate                 K day-1
      - Radiative fluxes
      swflx      Net SW radiative flux           W m-2   At grid-cell midpoints
      lwflx      Net LW radiative flux           W m-2   At grid-cell midpoints
      SwToa      Top-of-atmos SW rad flux        W m-2
      LwToa      Top-of-atmos LW rad flux        W m-2
      SwSrf      Surface  SW rad flux            W m-2
      LwSrf      Surface  LW rad flux            W m-2
      - Cloud forcing
      SwToaCf    SW cloud forc, top of atmos     W m-2   
      SwSrfCf    SW cloud forc, surface          W m-2   
      LwToaCf    LW cloud forc, top of atmos     W m-2   
      LwSrfCf    LW cloud forc, surface          W m-2   
    """
    def __init__(self, scheme = 'ccm3', **kwargs):
        # Initialize scheme-dependent attributes
        if scheme in ['gray','grey','graygas']: scheme='greygas'
        if scheme not in ['greygas','chou','ccm3','cam3', 'rrtm']:
            raise ValueError,'\n \n ++++ CliMT.radiation: Scheme %s unknown' % scheme
        exec('self.__%s__init__()' % string.lower(scheme))

        # Initialize fields etc. 
        Component.__init__(self, **kwargs)
        
    def __ccm3__init__(self):
        # Load extension
        try: import _ccm3_radiation
        except: raise ImportError, '\n \n ++++ CliMT.radiation: Could not load CCM3 scheme'
        # Define some attributes
        self.Name           = 'ccm3_radiation'
        self.LevType        = 'p'
        self.Extension      = _ccm3_radiation
        self.driver         = _ccm3_radiation.driver
        self.SteppingScheme = 'explicit'
        self.ToExtension    = ['do_sw','do_lw','p','ps','T','Ts','q','o3','cldf','clwp','ciwp',
                               'aldif','aldir','asdif','asdir','zen','solin','r_liq','r_ice',
                               'co2','n2o','ch4','cfc11','cfc12','tauvis','g','Cpd',
                               'epsilon','stebol','dt']
        self.FromExtension  = ['Tinc','TdotRad','SrfRadFlx','swhr','lwhr','swflx','lwflx','SwToaCf',
                               'SwSrfCf','LwToaCf','LwSrfCf','SwToa','SwSrf','LwToa','LwSrf']
        self.Required       = ['p','ps','T','Ts','q','o3','cldf','clwp','ciwp','r_liq','r_ice',
                               'aldif','aldir','asdif','asdir','zen','solin']
        self.Prognostic     = ['T']
        self.Diagnostic     = ['TdotRad','SrfRadFlx','swhr','lwhr','swflx','lwflx','SwToaCf',
                               'SwSrfCf','LwToaCf','LwSrfCf','SwToa','SwSrf','LwToa','LwSrf']

    def __cam3__init__(self):
        # Load extension
        try: import _cam3_radiation
        except: raise ImportError, '\n \n ++++ CliMT.radiation: Could not load CAM3 scheme'
        # Initialise abs/ems 
        ClimtDir = os.path.dirname( __file__ )
        AbsEmsDataFile = os.path.join(ClimtDir,'data/cam3rad/abs_ems_factors_fastvx.c030508.nc')
        _cam3_radiation.init_absems(AbsEmsDataFile)
        # Define some attributes
        self.Name           = 'cam3_radiation'
        self.LevType        = 'p'
        self.Extension      = _cam3_radiation
        self.driver         = _cam3_radiation.driver
        self.SteppingScheme = 'explicit'
        self.ToExtension    = ['do_sw','do_lw','p','dp','ps','T','Ts','q','o3','cldf','clwp','ciwp',
                               'in_cld','aldif','aldir','asdif','asdir','zen','solin','flus','r_liq','r_ice',
                               'co2','n2o','ch4','cfc11','cfc12','g','Cpd',
                               'epsilon','stebol','dt']
        self.FromExtension  = ['Tinc','TdotRad','SrfRadFlx','swhr','lwhr','swflx','lwflx','SwToaCf',
                               'SwSrfCf','LwToaCf','LwSrfCf','LwToa','LwSrf','SwToa','SwSrf','lwuflx','lwdflx']
        self.Required       = ['p','dp','ps','T','Ts','q','o3','cldf','clwp','ciwp','r_liq','r_ice',
                               'aldif','aldir','asdif','asdir','zen','solin','flus']
        self.Prognostic     = ['T']
        self.Diagnostic     = ['TdotRad','SrfRadFlx','swhr','lwhr','swflx','lwflx','SwToaCf',
                               'SwSrfCf','LwToaCf','LwSrfCf','LwToa','LwSrf','SwToa','SwSrf','lwuflx','lwdflx']

    def __chou__init__(self):
        # Load extension
        try: import _chou_radiation
        except: raise ImportError, '\n \n ++++ CliMT.radiation: Could not load Chou scheme'
        # Define some attributes
        self.Name           = 'chou_radiation'
        self.LevType        = 'p'
        self.Extension      = _chou_radiation
        self.driver         = _chou_radiation.driver
        self.SteppingScheme = 'explicit'
        self.ToExtension    = ['do_sw','do_lw','p','ps','T','Ts','q','o3','cldf','clwp','ciwp',
                               'co2','n2o', 'ch4','cfc11', 'cfc12', 'cfc22','r_liq','r_ice',
                               'aldif', 'aldir','asdif', 'asdir','zen', 'solin', 'g', 'Cpd','dt']
        self.FromExtension  = ['Tinc','TdotRad','SrfRadFlx','swhr','lwhr','swflx','lwflx',
                               'SwToaCf','SwSrfCf','LwToaCf','LwSrfCf','lwtau','SwToa','SwSrf','LwToa','LwSrf']
        self.Required       = ['p','ps','T','Ts','q','o3','cldf','clwp','ciwp','r_liq','r_ice',
                               'aldif','aldir','asdif','asdir','zen','solin']
        self.Prognostic     = ['T']
        self.Diagnostic     = ['TdotRad','SrfRadFlx','swhr','lwhr','swflx','lwflx','SwToaCf',
                               'SwSrfCf','LwToaCf','LwSrfCf','lwtau','SwToa','SwSrf','LwToa','LwSrf']

    def __greygas__init__(self):
        # Load extension
        try: import _greygas_radiation
        except: raise ImportError, '\n \n ++++ CliMT.radiation: Could not load grey gas scheme'
        # Define some attributes
        self.Name           = 'greygas_radiation'
        self.LevType        = 'p'
        self.Extension      = _greygas_radiation
        self.driver         = _greygas_radiation.driver
        self.SteppingScheme = 'explicit'
        self.ToExtension    = ['beta_greygas','stebol','g','dt','Cpd','tau_inf','alpha_greygas','T','q','p','ps','solin','Ts']
        self.FromExtension  = ['Tinc','TdotRad','SrfRadFlx','swhr','swflx','lwhr','lwflx','lwuflx','lwdflx','lwtau']
        self.Required       = ['T','q','p','ps','solin','Ts']
        self.Prognostic     = ['T']
        self.Diagnostic     = ['TdotRad','SrfRadFlx','lwhr','lwflx','lwuflx','lwdflx','lwtau']
    
    def __rrtm__init__(self):
        # Load extension
        try: import _rrtm_radiation
        except: raise ImportError, '\n \n ++++ CliMT.radiation: Could not load rrtm scheme'
        # Define some attributes
        self.Name = 'rrtm_radiation'
        self.LevType = 'p'
        self.Extension = _rrtm_radiation
        self.driver = _rrtm_radiation.driver
        self.SteppingScheme = 'explicit'
        self.ToExtension = _rrtm_radiation.INPUTS
        self.FromExtension = _rrtm_radiation.OUTPUTS
        self.Required = [field for field in _rrtm_radiation.INPUTS if field not in \
                         ['co2', 'n2o', 'ch4', 'cfc11', 'cfc12', 'cfc22', 'scon', 'o2', 'ccl4',
                          'tauaer_sw', 'ssaaer_sw', 'asmaer_sw', 'tauaer_lw', 'lw_surface_emissivity','Cpd']]
        self.Prognostic = [] 
        self.Diagnostic = [] 
