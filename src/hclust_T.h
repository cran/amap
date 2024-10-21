

#ifndef _AMAP_HCLUST_TEMPLATE
#define _AMAP_HCLUST_TEMPLATE 1


namespace hclust_T
{


  enum {WARD=1,SINGLE,COMPLETE,AVERAGE,MCQUITTY,MEDIAN,CENTROID,CENTROID2,WARDD2};


  template <class T> void hcluster(double *x, int *nr, int *nc, 
				   int *diag, int *method, int *iopt ,
				   int *ia , int *ib,int *iorder,
				   double *crit,double *membr,
				   int *nbprocess, int * result);

  template <class T> void hclust(int *nbprocess,double *mx,int nr, int nc,int *method,int *n,int *len, int *iopt ,int *ia ,
				 int *ib,int *iorder,double *crit,
				 double *membr,T *diss,int *result);
  

  /** \brief  Return indice 
   *
   *  The upper half diagonal distance matrix is stored as a vector...
   * so distance between individual i and j is stored at postion ioffst(i,j)
   *
   * \param n number of individuals (distance matrix is nxn)
   * \param i,j: indices in matrix
   */
  inline int ioffst(int n,int i,int j)
  {
    return j>i ? j+i*n-(i+1)*(i+2)/2 : ioffst(n,j,i);
  }

}

#endif


#ifndef _AMAP_HCLUST_TEMPLATE_CPP
#define _AMAP_HCLUST_TEMPLATE_CPP 1
#include "hclust_T.h.h"
#endif
