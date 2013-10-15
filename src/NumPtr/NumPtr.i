%module NumPtr

%{
#include "Python.h"
#include "Numeric/arrayobject.h"
%}

extern int   getstride1(PyObject *array);

extern double *   getpointer1(PyObject *array);
extern double **  getpointer2(PyObject *array);
extern double *** getpointer3(PyObject *array);

extern void  test1(double *   a, int n);
extern void  test2(double **  a, int n, int m);
extern void  test3(double *** a, int n, int m, int l);
