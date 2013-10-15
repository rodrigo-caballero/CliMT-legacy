from numpy import *

def squeeze(a):
    "squeeze(a) returns a with any ones from the shape of a removed"
    a = asarray(a)
    b = asarray(a.shape)
    return reshape (a, tuple (compress (not_equal (b, 1), b)))

def get_from_first(key,*dicts):
    """
    Returns value associated with key from the first dictionary in dicts which contains that key
    """
    for dict in dicts:
        if key in dict: return dict.get(key)
    raise KeyError, 'utils.get_from_multi(): Key %s not found' % str(key)

def demean(a,axis=0):
    """
    Returns a with mean subtracted along specified axis.
    """
    a=array(a)
    assert axis <= len(shape(a)), \
                  '\n\n ++++ CliMT.utils.demean: axis index out of range'

    if rank(a)==1: return a - average(a)
    
    x=[':']*rank(a)
    x[axis]='NewAxis'
    s='['+'%s,'*(rank(a)-1)+'%s]'
    sx = s % tuple(x)
    exec('a = a - average(a,axis=axis)'+sx)
    return a

