from Numeric import *
from _NumPtr import *

a = array([1,2,3,4,5,6],'d')
print a
p=getpointer1(a)
print p
test1(p,6)

a = array([[1,2,3],[4,5,6]],'d')
print a
p=getpointer2(a)
print p
test2(p,2,3)

a = array([ [[1,2,3],[4,5,6]] , [[7,8,9],[10,11,12]] ],'d')
print a
print len(a.shape)
p=getpointer3(a)
print p
test3(p,2,2,3)

