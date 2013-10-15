#!/usr/bin/env python

from numpy import *
import os,sys,string,glob
import climt
from Scientific.IO.NetCDF import NetCDFFile
from datetime import datetime

class DataServer:

    def __init__(self,
                 Case1='uh0',
                 Case2='uh1',
                 FileNumber=0):
        # Identify files
        # NOTE: assumes directories contain full years of data !!
        Home = os.getenv('HOME')
        Dir = '%s/cam/runs' % Home
        Dir1 = '%s/%s' % (Dir,Case1)
        Dir2 = '%s/%s' % (Dir,Case2)
        FileNames1 = glob.glob('%s/*cam2*h1*.nc' % Dir1)
        FileNames2 = glob.glob('%s/*cam2*h1*.nc' % Dir2)
        FileNames1.sort()
        FileNames2.sort()
        N1 = len(FileNames1)
        N2 = len(FileNames2)
        N = min(N1,N2)
        if N1 > N:
            for i in range(N1-N): FileNames1.pop()
        if N2 > N:
            for i in range(N2-N): FileNames2.pop()
        #self.Files1 = [NetCDFFile(File,'r') for File in FileNames1]
        #self.Files2 = [NetCDFFile(File,'r') for File in FileNames2]
        self.N = N
        self.FileName1 = FileNames1[FileNumber]
        self.FileName2 = FileNames2[FileNumber]
        self.File1 = NetCDFFile(self.FileName1,'r')
        self.File2 = NetCDFFile(self.FileName2,'r')

        print 'Using input files:'
        print self.FileName1
        print self.FileName2

        #  Extract hybrid coord coefficients and ref press
        File = self.File1
        self.hyam = File.variables['hyam'][:]
        self.hybm = File.variables['hybm'][:]
        self.hyai = File.variables['hyai'][:]
        self.hybi = File.variables['hybi'][:]
        self.p0   = File.variables['P0'].getValue()
        # Extract lat, lon, lev, time
        self.lat = File.variables['lat'][:]
        self.lon = File.variables['lon'][:]
        self.lev = File.variables['lev'][:]
        self.time = File.variables['time'][:]
        
    def getData(self,Field,l):
        # retrieve field at time l
        #nt = len(self.time)
        #FileIndex = l/nt
        if Field == 'p':
            #ps1 = self.Files1[FileIndex].variables['PS'][l-nt*FileIndex]
            #ps2 = self.Files2[FileIndex].variables['PS'][l-nt*FileIndex]
            ps1 = self.File1.variables['PS'][l]
            ps2 = self.File2.variables['PS'][l]
            x1  = self.hyam[:,None,None]*self.p0 + \
                  self.hybm[:,None,None]*ps1[None,:,:]
            x2  = self.hyam[:,None,None]*self.p0 + \
                  self.hybm[:,None,None]*ps2[None,:,:]
        elif Field == 'dp':
            #ps1 = self.Files1[FileIndex].variables['PS'][l-nt*FileIndex]
            #ps2 = self.Files2[FileIndex].variables['PS'][l-nt*FileIndex]
            ps1 = self.File1.variables['PS'][l]
            ps2 = self.File2.variables['PS'][l]
            x1  = self.hyai[:,None,None]*self.p0 + \
                  self.hybi[:,None,None]*ps1[None,:,:]
            x2  = self.hyai[:,None,None]*self.p0 + \
                  self.hybi[:,None,None]*ps2[None,:,:]
            x1  = x1[1:]-x1[0:-1]
            x2  = x2[1:]-x2[0:-1]
        else:
            #x1  = self.Files1[FileIndex].variables[Field][l-nt*FileIndex]
            #x2  = self.Files2[FileIndex].variables[Field][l-nt*FileIndex]
            x1  = self.File1.variables[Field][l]
            x2  = self.File2.variables[Field][l]
        return x1,x2

class Feedback:

    def __init__(self,
                 Case1='uh0',
                 Case2='uh1',
                 FileNumber=0):

        self.Case1 = Case1
        self.Case2 = Case2
        self.FileNumber = FileNumber
        # init data server
        self.data = DataServer(Case1,Case2,FileNumber)
        # geometry
        self.cosphi = cos(self.data.lat*pi/180.)
        self.a   = 6.37122e6
        # radiation
        self.r = climt.radiation(scheme='cam3')
        # fixed params
        self.Fixed={}
        self.Fixed['ch4'] = 7.6e-7 * 1.e6
        self.Fixed['n2o'] = 2.6e-7 * 1.e6
        self.Fixed['cfc11'] = 1.e-32
        self.Fixed['cfc12'] = 1.e-32
        self.Fixed['scon'] = 1365.
        # compute
        self.compute()

    def compute(self):
        l = 0
        N = len(self.data.time)
        while l < N:
            # l is time index
            # note it's equal to NsnapsDone
            l, self.File = self.openOutputFile()
            print 'Doing snapshot %s of %s' % (l+1,N)
            Data = self.getFields(l)
            for i in range(2):
                Data[i].update(self.Fixed)
            # compute unperturbed TOA rad fluxes
            self.R,self.R_sw,self.R_lw = self.getToaRad(Data)
            # compute CO2 perturbation
            dR_co2 = self.getdR(Data,['co2'])
            # compute q perturbation
            dR_q = self.getdR(Data,['q'])
            # compute cloud perturbation
            dR_cld_sw,dR_cld_lw = \
                   self.getdR(Data,['cldf','ciwp','clwp','r_liq','r_ice'],swlw=True)
            # compute surface albedo perturbation
            dR_alb = self.getdR(Data,['aldir','aldif','asdir','asdif'])
            # compute temperature perturbation
            dR_Ts_tmp = self.getdR_Ts(Data)
            dR_T      = self.getdR(Data,['T','Ts','flus'])
            dR_Ts     = dR_Ts_tmp
            dR_lapse  = (dR_T - dR_Ts_tmp)            
            # write out
            self.File.NsnapsDone = l+1
            for Field in ['dR_co2','dR_q','dR_cld_sw','dR_cld_lw', \
                          'dR_alb','dR_lapse','dR_Ts']:
                exec("tmp = self.File.variables['%s'][:,:]" % Field)
                exec("self.File.variables['%s'][:,:] = (tmp + %s/N).astype('f')" %
                     (Field,Field))
            # close file
            self.File.close()

    def getdR(self,Data,Fields,swlw=False):
        # Compute radiative perturbation
        # use average of forward and backward substitutions
        # see Colman & McAvaney JGR 1997
        PertData = self.swap(Data,Fields)
        PertR,PertR_sw,PertR_lw = self.getToaRad(PertData)
        if swlw:
            dR_sw = ( (PertR_sw[0]-self.R_sw[0]) + (self.R_sw[1]-PertR_sw[1]) )/2. 
            dR_lw = ( (PertR_lw[0]-self.R_lw[0]) + (self.R_lw[1]-PertR_lw[1]) )/2. 
            return dR_sw,dR_lw
        else:
            dR = ( (PertR[0]-self.R[0]) + (self.R[1]-PertR[1]) )/2. 
            return dR
        
    def getdR_Ts(self,Data):
        # Compute radiative perturbation for the special
        # case of surface temperature
        PertData = self.swap(Data,['Ts','flus'])
        # add Ts perturbation throughout atmos
        for i in range(2):
            dT = PertData[i]['Ts'] - Data[i]['Ts']
            PertData[i]['T'] = PertData[i]['T'] + dT[None,:,:]
        PertR = self.getToaRad(PertData)[0]
        dR = ( (PertR[0]-self.R[0]) + (self.R[1]-PertR[1]) )/2. 
        return dR

    def swap(self,Data,Fields):
        # swaps the specified fields between the 2 datasets
        PertData = [d.copy() for d in Data]
        for Field in Fields:
            PertData[0][Field] =  Data[1][Field]
            PertData[1][Field] =  Data[0][Field]
        return PertData

    def getToaRad(self,Data):
        # returns net (SW+LW) rad flux at TOA
        R = []
        R_sw = []
        R_lw = []
        for i in range(2):
            self.r(**Data[i])
            R.append(self.r['SwToa']+self.r['LwToa'])
            R_sw.append(self.r['SwToa'])
            R_lw.append(self.r['LwToa'])
        return R,R_sw,R_lw
    
    def getFields(self,l):
        # retrieve snapshot of required fields,
        # and where necessary change units to suit CliMT
        CamFields = ['p','dp','PS','T','TS','Q','CLOUD','ICLDIWP',
                     'ICLDLWP','O3VMR','ALDIR','ALDIF','ASDIR',
                     'ASDIF','SOLIN','FLUS','REL','REI','co2vmr','COSZRS']
        ClimtFields = ['p','dp','ps','T','Ts','q','cldf','ciwp',
                       'clwp','o3','aldir','aldif','asdir',
                       'asdif','solin','flus','r_liq','r_ice','co2','zen']
        Conversion = dict(zip(CamFields,ClimtFields))
        Data=[{},{}]
        for Field in CamFields:
            x  = self.data.getData(Field,l)
            for i in range(2):
                Data[i][Conversion[Field]] = x[i] 
        for i in range(2):
            Data[i]['co2'] = Data[i]['co2']*1.e6 # vmr -> ppm
            Data[i]['q'] = Data[i]['q']*1.e3 # kg/kg -> g/kg            
            Data[i]['o3'] = Data[i]['o3'] *1.661 # vol mix rat -> mass mix rat            
            Data[i]['p'] = Data[i]['p']/100. 
            Data[i]['dp'] = Data[i]['dp']/100. 
            Data[i]['ps'] = Data[i]['ps']/100.
            Data[i]['zen'] = Data[i]['solin']/Data[i]['solin'].max()
            Data[i]['zen'] = arccos(Data[i]['zen'])*180./pi
        return Data
        
    def test(self):
        # Create file
        FileName = '%s_out.nc' % self.Case
        print 'creating %s ...' % FileName
        File = NetCDFFile(FileName,'w')
        File.createDimension('time',None)
        var = File.createVariable('time','f',('time',))
        var.long_name = 'time'
        var.units = ' '
        File.createDimension('lat',len(self.data.lat))
        var = File.createVariable('lat','f',('lat',))
        var.long_name = 'latitude'
        var.units = 'degrees_north'
        var[:] = self.data.lat.astype('f')
        File.createDimension('lon',len(self.data.lon))
        var = File.createVariable('lon','f',('lon',))
        var.long_name = 'longitude'
        var.units = 'degrees_east'
        var[:] = self.data.lon.astype('f')
        for Field in ['SwToa','LwToa','SwToaCf','LwToaCf']:
            var = File.createVariable(Field,'f',('time','lat','lon'))
            var.long_name = ''
            var.units = 'W m-2'
        lmax = 3
        for l in range(lmax):
            print 'doing %s of %s' % (l+1,lmax)
            # get data
            Data = self.getFields(l)[0]
            Data.update(self.Fixed)
            # compute
            self.r(**Data)
            File.variables['SwToa'][l] = self.r['SwToa'].astype('f')
            File.variables['LwToa'][l] = -self.r['LwToa'].astype('f')
            File.variables['SwToaCf'][l] = self.r['SwToaCf'].astype('f')
            File.variables['LwToaCf'][l] = self.r['LwToaCf'].astype('f')
        File.close()

    def openOutputFile(self):
        # Create file
        os.system('mkdir -p results/%s-%s' % (self.Case1,self.Case2))
        FileName = 'results/%s-%s/Cam3Feedbacks.%s-%s.%03i.nc' % \
                   (self.Case1,self.Case2,self.Case1,self.Case2,self.FileNumber)
        if not os.path.exists(FileName):
            print 'creating %s ...' % FileName
            File = NetCDFFile(FileName,'w')
            File.createDimension('lat',len(self.data.lat))
            var = File.createVariable('lat','f',('lat',))
            var.long_name = 'latitude'
            var.units = 'degrees_north'
            var[:] = self.data.lat.astype('f')
            File.createDimension('lon',len(self.data.lon))
            var = File.createVariable('lon','f',('lon',))
            var.long_name = 'longitude'
            var.units = 'degrees_east'
            var[:] = self.data.lon.astype('f')
            # create variables
            for Field in ['dR_Ts','dR_lapse','dR_q','dR_cld_sw','dR_cld_lw','dR_alb','dR_co2']:
                var = File.createVariable(Field,'f',('lat','lon'))
                var.long_name = 'TOA radiative perturbation'
                var.units = 'W m-2'
                var[:,:] = 0.
            File.NsnapsDone = 0
            return 0, File
        else:
            File = NetCDFFile(FileName,'a')
            NsnapsDone = int(File.NsnapsDone[0])
            if NsnapsDone < len(self.data.time):
                return NsnapsDone, File
            else:
                print 'No more snaps to be done'
                sys.exit(0)

if __name__ == '__main__':

    Feedback(Case1=sys.argv[1], Case2=sys.argv[2], FileNumber=int(sys.argv[3]))
