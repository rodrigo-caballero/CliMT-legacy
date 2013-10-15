#include "Python.h"
#include "numpy/oldnumeric.h"

double * getpointer1(PyObject *A){
 PyArrayObject *array;
 array= (PyArrayObject *) A;
 return (double *) (array->data);
}

int getstride1(PyObject *A){
 PyArrayObject *array;
 array= (PyArrayObject *) A;
 return array->strides[0];
}

double ** getpointer2(PyObject *A){
 PyArrayObject * array;
 int imax,i;
 double ** p=NULL;
 array = (PyArrayObject *) A;
 imax = array->dimensions[0];
 p = (double **) calloc(imax,sizeof(double *));
   for (i=0; i<imax; i++){
     p[i]=(double *)(array->data + i*array->strides[0]);
     }
     return p;
}

double *** getpointer3(PyObject *A){
 PyArrayObject * array;
 int imax,jmax,i,j;
 double *** p=NULL;
 array = (PyArrayObject *) A;
 imax = array->dimensions[0];
 jmax = array->dimensions[1];
 p = (double ***) calloc(imax,sizeof(double **));
   for (i=0; i<imax; i++){
     p[i]=(double **) calloc(jmax,sizeof(double *));
     for (j=0; j<jmax; j++){
       p[i][j]=(double *)(array->data + i*array->strides[0] + j*array->strides[1]);
     }
   }
 return p;
}

