import _thermodyn
t=273.
q=10.
p=1000.

print '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
print 'Testing thermodynamics module ...'

print _thermodyn.tdew(p,q)-273.

print ' '
print 'Success!'
print '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
