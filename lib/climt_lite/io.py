import os
from numpy import *
from __version__ import __version__
import time

if 'climt_lite' in __file__:
    Lite = True
else:
    Lite = False

try:
    from netCDF4 import Dataset as open_file
    gotNetCDF = True
    NetCDFInterface = 'netCDF4'
except:
    gotNetCDF = False

if not gotNetCDF:
    try:
        from Scientific.IO.NetCDF import NetCDFFile as open_file
        gotNetCDF = True
        NetCDFInterface = 'Scientific'
    except:
        gotNetCDF = False

if not gotNetCDF:
    try:
        from PyNGL.Nio import open_file 
        gotNetCDF = True
        NetCDFInterface = 'PyNGL'
    except:
        gotNetCDF = False

if gotNetCDF:
    print 'Using %s interface for IO' % NetCDFInterface
else:
    if not Lite: print '\n ++++ CliMT: WARNING: NetCDF interface ' \
          +'could not be loaded, so no file input or output !\n' 

from state import KnownFields

class IO:
    """
    """
    def __init__(self, Component, **kwargs):
        """
        """
        if not gotNetCDF:
            self.DoingOutput = False
            self.Appending = False
            return

        # Restart file name
        try:    self.RestartFileName = kwargs['RestartFile']
        except: self.RestartFileName = None

        # Output file name
        try:    self.OutputFileName = kwargs['OutputFile']
        except: self.OutputFileName = None

        # If no fields are specified, ALL fields in State will
        # be output (see writeOuput)
        try:    self.OutputFieldNames = kwargs['OutputFields']
        except: self.OutputFieldNames = None

        # If no output frequency specifed, output once daily
        try:    self.OutputFreq = kwargs['OutputFreq']
        except: self.OutputFreq = 86400.

        # Decide if we're doing output
        if  self.OutputFileName is not None:
            self.DoingOutput = True
        else:
            self.DoingOutput = False

        # Decide if we're appending output to restart file
        if self.OutputFileName is not None and \
           self.OutputFileName == self.RestartFileName:
            self.Appending = True
        else:
            self.Appending = False

        # Set of all fields in State
        self.AllFields = \
              list(set(Component.Required).union(Component.Diagnostic).union(Component.Prognostic))

        # Check that OutputFieldNames is a subset of AllFields
        if self.OutputFieldNames is None:
            OddFields = []
        else:
            OddFields = list(set(self.OutputFieldNames).difference(self.AllFields))
        if len(OddFields) > 0: raise \
           '\n +++ CliMT.IO.init: Output fields %s not recognized\n' % str(list(OddFields))
        
        # Inititalize output time index
        self.OutputTimeIndex = 0


    def readRestart(self, FieldNames, ParamNames, kwargs):
        """
        Reads required parameters and fields from restart file.
        """        
        if not gotNetCDF: return kwargs

        # Open file
        try:
            File = open_file(self.RestartFileName,'r')
        except IOError:  
            raise IOError, \
            '\n ++++ CliMT.IO.readRestart: Restart file %s not found or unreadable\n' \
            % self.RestartFileName 
            
        # Read elapsed time
        if 'ElapsedTime' not in kwargs:
            kwargs['ElapsedTime'] = File.variables['time'][-1]*86400.

       # Read parameters (they are stored as global attribs)
        for Name in ParamNames:
            try:
                exec('if Name not in kwargs: kwargs["%s"] = File.%s'%(Name,Name))
            except:
                print '\n ++++ CliMT.readRestart: Parameter %s ' % Name + \
                'not found in restart file, using default or supplied value\n'
            
        # Read grid
        if 'lev' not in kwargs: kwargs['lev'] = File.variables['lev'][:][::-1].astype('d')
        for Name in ['lat','lon']:
            if Name not in kwargs: kwargs[Name] = File.variables[Name][:].astype('d')
                        
        # Read variables 
        for Name in FieldNames:
            if Name not in kwargs:
                try:
                    kwargs[Name] = File.variables[Name][-1].astype('d')
                    if KnownFields[Name][2] == '3D': kwargs[Name] = kwargs[Name][::-1]
                except:
                    print '\n ++++ CliMT.readRestart: Field %s ' % Name + \
                          'not found in restart file, using default or supplied value\n'

         # If we're appending to restart file, shift time index forward
        if self.Appending: self.OutputTimeIndex = len(File.variables['time'][:])
        
        # Close file and return values
        File.close()
        print ' Read from restart file %s\n' % self.RestartFileName
        return kwargs

    def createOutputFile(self, State, Params):
        """
        Creates output file with all fields in State
        """
        # If we're not doing output or we're appending to restart file, skip creation
        if not gotNetCDF or not self.DoingOutput or self.Appending: return

        # Create file
        os.system('rm -f %s' % self.OutputFileName)
        if NetCDFInterface == 'netCDF4':
            File = open_file(self.OutputFileName,'w', format='NETCDF3_CLASSIC')
        else:
            File = open_file(self.OutputFileName,'w')            
        # rename methods
        if NetCDFInterface in  ['netCDF4', 'Scientific']:
            createDimension = File.createDimension
            createVariable  = File.createVariable
        elif NetCDFInterface == 'PyNGL':
            createDimension = File.create_dimension
            createVariable  = File.create_variable

        # Define some global attribs
        File.Conventions='COARDS'
        File.CliMTVersion = __version__
        File.RunStartDate = time.ctime()
        File.NetCDFInterface = NetCDFInterface
        if self.RestartFileName is not None:
            File.RestartFile = self.RestartFileName

        # Store parameters as global attribs
        for Name in Params:
            exec('File.%s = Params["%s"]'%(Name,Name))

        # Create dimensions and axes. Time is record dimension and gets special treatement
        createDimension('time',None)
        var = createVariable('time','d',('time',))
        var.long_name = 'time'
        var.units     = 'days'
        createDimension('lev',len(State.Grid['lev']))
        var = createVariable('lev','d',('lev',))
        var.long_name = 'level'
        var.units     = 'mb'
        var.depth     = 'true'
        var[:] = State.Grid['lev'][::-1]
        for key in ['lat','lon']:
            createDimension(key,len(State.Grid[key]))
            var = createVariable(key,'d',(key,))
            var.long_name = State.Grid.long_name[key]
            var.units     = State.Grid.units[key]
            var[:]        = State.Grid[key]
        # Create output fields
        axes2D = ('time','lat','lon')
        axes3D = ('time','lev','lat','lon')
        for key in self.AllFields:
            exec('axes = axes%s' % KnownFields[key][2])
            var = createVariable(key, 'f', axes)
            var.long_name = KnownFields[key][0]
            var.units     = KnownFields[key][1]
        # Close file
        File.close()

    def writeOutput(self, Params, State):
        """
        """            
        if not gotNetCDF or not self.DoingOutput: return

        # Open file
        File = open_file(self.OutputFileName,'a')

        # Decide what we're going to output
        if State.ElapsedTime == 0. or self.OutputFieldNames == None:
            OutputFieldNames = self.AllFields
        else:
            OutputFieldNames = self.OutputFieldNames

        # Write variables (as 'f' to reduce file size)
        File.variables['time'][self.OutputTimeIndex] = State.ElapsedTime/86400.
        nlev = State.Grid['nlev']
        for key in OutputFieldNames:
            if KnownFields[key][2] == '2D':
                out = State[key].copy()
                File.variables[key][self.OutputTimeIndex,:,:] = out[:,:].astype('f')
            if KnownFields[key][2] == '3D':
                out = State[key].copy()
                out = out[nlev-1::-1,:,:]
                File.variables[key][self.OutputTimeIndex,:,:,:] = out[:,:,:].astype('f')

        # Write time
        File.variables['time'][self.OutputTimeIndex] = State.ElapsedTime/86400.

        # Write calday
        File.calday = Params['calday']
        
        # Advance time index
        self.OutputTimeIndex += 1

        # Close file
        File.close()
        print ' Wrote to file %s, time=%10.5f days' % \
              (self.OutputFileName,State.ElapsedTime/86400.)
