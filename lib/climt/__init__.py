#from distutils.sysconfig import get_python_lib
#import sys
#sys.path.append(get_python_lib()+'/climt_classic/')

from .__version__ import __version__
 
from ._grid import get_nlev, get_nlat, get_nlon

from .thermodyn    import es,qs,ws,esflatau,qsflatau,wsflatau,tdew,\
                         tstar,theta,thetae,pdryadiab,tdryadiab, \
                         tmoistadiab,moistadiabat,pseudoadiab,thetaes, \
                         skewT
from .insolation   import insolation
from .ozone        import ozone
from .radiation    import radiation
from .grid         import Grid
from .state        import State
from .parameters   import Parameters
from . import mathutil,utils

if 'climt_lite' not in __file__:
  from .federation   import federation
  from .convection   import convection
  from .ocean        import ocean
  from .seaice        import seaice
  from .dynamics     import dynamics
  from .turbulence   import turbulence
