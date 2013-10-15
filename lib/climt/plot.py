#!/usr/bin/env python

if 'climt_lite' in __file__:
    Lite = True
else:
    Lite = False

try:
    try:    import matplotlib.pylab as pylab
    except: import matplotlib.matlab as pylab
    # catch old versions which use set instead of setp
    if not hasattr(pylab,'setp'): pylab.setp = pylab.set
    # gives right aspect ratio in subplots
    pylab.rcParams['image.aspect']='auto'
    # increase vertical gap between plots
    pylab.rcParams['figure.subplot.hspace']=0.3
    # turn on interactive mode
    pylab.ion() 
    gotMatplotlib = True
except:
    if not Lite: print '\n ++++ CliMT: WARNING: matplotlib.pylab ' \
       +'could not be loaded, so no runtime monitoring !\n' 
    gotMatplotlib = False

from numpy import *
from utils import squeeze
from state import KnownFields

def _figureSetUp(FieldKeys, Component):
    """
    Sets up a figure consisting of up to 4 panels (subplots).
    """
    if   len(FieldKeys) == 1: SubplotCodes = ['111']
    elif len(FieldKeys) == 2: SubplotCodes = ['211','212']
    elif len(FieldKeys) == 3: SubplotCodes = ['221','222','223']
    elif len(FieldKeys) == 4: SubplotCodes = ['221','222','223','224']
    else: raise '\n\n CliMT.plot: not more than 4 fields on 1 plot!'

    Subplots = dict(zip(FieldKeys,SubplotCodes))
    Figure = {}
    for key in FieldKeys:
        Subplot = Subplots[key]
        exec('pylab.subplot(%s)' % Subplot)
        Figure[key] = Panel(Component, key, Subplot)
        
    return Figure

class Plot:
    """
    Enables plotting of up to 4 variables.
    """    
    def __init__(self):
        pass

    def __call__(self, Component, *FieldKeys):
        """
        Plots fields indicated by FieldKeys
        """
        if not gotMatplotlib: return
        
        if len(FieldKeys) == 0: return
        # Clear figure
        pylab.clf()
        # FieldKeys was input either as ['a','b' ...] or as 'a','b', ...
        if type(FieldKeys[0]) is type([]):
            _figureSetUp(FieldKeys[0], Component)
        else:
            _figureSetUp(list(FieldKeys), Component)

    def setFigure(self, FigureNumber=None):
        if not gotMatplotlib: return
        if FigureNumber is None:
            FigureNumber = pylab.get_current_fig_manager().num
        pylab.figure(num=FigureNumber)

    def closeFigure(self, FigureNumber=None):
        if not gotMatplotlib: return
        if FigureNumber is None:
            FigureNumber = pylab.get_current_fig_manager().num
        pylab.close(FigureNumber)

class Monitor:
    """
    Enables runtime monitoring of up to 4 fields.
    """
    def __init__(self, Component, **kwargs):
        """
        """
        if not gotMatplotlib:
            self.Monitoring = False
            return
        
        # List of fields to monitor
        try:    self.FieldKeys = kwargs['MonitorFields']
        except: self.FieldKeys = []
        if type(self.FieldKeys) is not type([]):
            self.FieldKeys = []
        for key in self.FieldKeys:
            assert key in Component.State, \
                   '\n\n ++++ CliMT.monitor: %s not in component' % key
            if rank(Component[key]) == 0:
                print '\n\n ++++ CliMT.monitor: WARNING: cannot '\
                      +'monitor scalar variable %s' % key
                self.FieldKeys.pop(self.FieldKeys.index(key))

        # Decide if we're doing monitoring
        if len(self.FieldKeys) > 0:
            self.Monitoring = True
        else:
            self.Monitoring = False
            return

        # Frequency of monitor updates (default 6 hours)
        if 'MonitorFreq' in kwargs:
            self.MonitorFreq = kwargs['MonitorFreq']
        else:
            self.MonitorFreq = 6.*60.*60.

        # Clear monitor (if one already set up by a previous instance)
        pylab.clf()

        # Set up figure
        self.Figure = _figureSetUp(self.FieldKeys, Component)

        # Get figure manager
        self.manager = pylab.get_current_fig_manager()

    def refresh(self, Component):
        """
        Update monitor display.
        """
        if not gotMatplotlib or not self.Monitoring: return

        for key in self.Figure.keys():
            Field = Component[key]
            Panel = self.Figure[key]

            # If Field is 3D, show zonal average
            if rank(Field) == 3:
                Field = average(Field,axis=2)
                Field = squeeze(Field)

            # Reset data
            if rank(Field) == 1:
                if min(Field) == max(Field) == 0.: Field=Field+1.e-7
                MinVal = min(Field) - 0.01*abs(min(Field))
                MaxVal = max(Field) + 0.01*abs(max(Field))
                if Panel.orientation == 0:
                    Panel.handle.set_xdata(Field)
                    Panel.axes.set_xlim([MinVal, MaxVal])
                else:
                    Panel.handle.set_ydata(Field)
                    Panel.axes.set_ylim([MinVal, MaxVal])
            if rank(Field) == 2:
                if Panel.orientation == 0:
                    Panel.handle.set_data(Field[::-1])
                if Panel.orientation == 1:
                    Panel.handle.set_data(Field)
                # update normalization
                Panel.handle.set_norm(None)

            # Reset title
            day = Component.State.ElapsedTime/86400.
            try:
                TitleText = Panel.TitleTemplate % day
            except:
                TitleText = Panel.TitleTemplate % (day, min(ravel(Field)), max(ravel(Field)))
            Panel.title.set_text(TitleText)

        # Redraw figure
        self.manager.canvas.draw()

class Panel:
    """
    Configures single panel of monitor display.
    """
    def __init__(self, Component, FieldKey, Subplot):
        
        # get Field value
        Field = Component.State[FieldKey]
        Dims  = KnownFields[FieldKey][2]

        # Figure out axes
        AxisKey = []
        if Dims == '2D':
            for i in range(2):
                if shape(Field)[i] > 1: AxisKey.append(['lat','lon'][i])
        if Dims == '3D':
            for i in range(3):
                if shape(Field)[i] > 1: AxisKey.append(['lev','lat','lon'][i])

        # If Field is 3D, show zonal average
        Field = squeeze(Field)
        if rank(Field) == 3:
            Field = average(Field,axis=2)
            Field = squeeze(Field)
            AxisKey.pop(2)

        # Axes names and values
        AxisName = []
        AxisVal  = []
        for key in AxisKey:
            AxisName.append(Component.Grid.long_name[key])
            AxisVal.append(Component.Grid[key])
        
        # Get handles to figure and properties
        if len(AxisName) == 1:
            if min(Field) == max(Field) == 0.: Field=Field+1.e-7
            MinVal = min(Field) - 0.01*abs(min(Field))
            MaxVal = max(Field) + 0.01*abs(max(Field))
            if AxisKey[0] == 'lev':
                pylab.ylabel(AxisName[0])
                self.orientation = 0
                self.handle = pylab.plot(Field, AxisVal[0], 'bo-').pop(0)
                self.axes = pylab.gca()
                pylab.xlim(MinVal, MaxVal)
                pylab.ylim(AxisVal[0][-1], AxisVal[0][0] )
            else:
                if Subplot in ['111','212','223','224']: pylab.xlabel(AxisName[0])
                self.orientation = 1
                self.handle = pylab.plot(AxisVal[0], Field, 'bo-').pop(0)
                self.axes = pylab.gca()
                pylab.ylim([MinVal, MaxVal])
                pylab.xlim([ AxisVal[0][0], AxisVal[0][-1] ])

        elif len(AxisName) == 2:
            xmin = AxisVal[1][0]
            xmax = AxisVal[1][-1]
            ymin = AxisVal[0][0]
            ymax = AxisVal[0][-1]
            if AxisKey[0] == 'lev':
               self.orientation = 0
               self.handle = pylab.imshow(\
                    Field[::-1],\
                    extent=(xmin, xmax, ymin, ymax),\
                    interpolation='bilinear')
               pylab.setp(pylab.gca(), 'xlim',[xmin,xmax], 'ylim',[ymax,ymin])
            else:
               self.orientation = 1
               self.handle = pylab.imshow( \
                    Field, \
                    extent=(xmin, xmax, ymin, ymax),\
                    interpolation='bilinear')
               pylab.setp(pylab.gca(), 'xlim',[xmin,xmax], 'ylim',[ymin,ymax])
            # write x label only if subplot does not have other subplot underneath
            if Subplot in ['111','212','223','224']: pylab.xlabel(AxisName[1])
            pylab.ylabel(AxisName[0])
            self.handle.set_norm(None)

        # Title
        self.TitleTemplate = '%s  [%s]' % (FieldKey,KnownFields[FieldKey][1]) + ' %6.2f days' 
        if len(AxisName) == 2: self.TitleTemplate = self.TitleTemplate + '\n min=%g max=%g' 
        day = Component.State.ElapsedTime/86400.
        try:
            TitleText = self.TitleTemplate % day
        except:
            TitleText = self.TitleTemplate % (day, min(ravel(Field)), max(ravel(Field)))
        self.title = pylab.title(TitleText)
        self.title.set_fontsize(12)


if __name__=='__main__':

    import climt
    r=climt.radiation(lat=arange(0.,90.,10.))    
    m=Monitor(r, MonitorFields=['T','Ts'])
    for i in range(10):
        r.step()
        m.refresh(r)

    show()
