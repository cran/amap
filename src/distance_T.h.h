/*! \file distance.c
 * \brief all functions requiered for R dist function and C hcluster function.
 *
 *  \date Created: probably in 1995
 *  \date Last modified: Time-stamp: <2014-03-01 17:21:54 antoine>
 *
 *  \author R core members, and lately: Antoine Lucas 
 *
 *  
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998, 2001  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */


#define _AMAP_DISTANCE_TEMPLATE_CPP 1

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "distance_T.h"
#include "matriceTriangle.h"
#include "distance.h"

#include <float.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <R_ext/Arith.h>
#include <R_ext/Error.h>
#include <limits>

#ifndef WIN32
#include <pthread.h>
#endif

#define MAX( A , B )  ( ( A ) > ( B ) ? ( A ) : ( B ) )
#define MIN( A , B )  ( ( A ) < ( B ) ? ( A ) : ( B ) )

namespace amap {
// ---------------------------------------------------------
// Distance euclidean (i.e. sqrt(sum of square) )
//
// Euclidean distance between 2 vectors a,b is
//  d = sqrt[ sum_i (a_i - b_i)^2 ]
//
// This function compute distance between 2 vectors x[i1,] & y[i2,]
// x and y are matrix; we use here only line i1 from x and
// line i2 from y. Number of column (nc) is the same in x and y,
// number of column can differ (nr_x, nr_y).
//
// Flag will be set to 0 if NA value computed in distance
//
// When call by function distance or hclust, x and y are the same; it computes
// distance between vector x[i1,] and x[i2,]
//
// \param x matrix of size nr_x * nc; line i1 is of interest
// \param y matrix of size nr_y * nc; line i1 is of interest
// \param nr_x number of row in matrix x
// \param nr_y number of row in matrix y
// \param nc number of column in matrix x or y
// \param i1 row choosen in matrix x
// \param i2 row choosen in matrix y
// \param flag set to 0 if NA value computed in distance
// \param opt: unused
// 
//  Return: distance value
//
// ---------------------------------------------------------
template<class T> T  distance_T<T>::R_euclidean(vecteur<double> & x, vecteur<double> & y ,
						int * flag, T_tri & opt)
{
  T dev, dist;
  int count, i;

  count= 0;
  dist = 0;
  for(i = 0 ; i < x.size() && i < y.size() ; i++) {
    if(R_FINITE(x[i]) && R_FINITE(y[i])) {
      dev = (x[i] - y[i]);
      dist += dev * dev;
      count++;
    }
  }
  if(count == 0) // NA for all j: 
    { 
      *flag = 0;
      return NA_REAL;
    }

  if(count != x.size()) dist /= ((T)count/x.size());
  return sqrt(dist);
}

// ---------------------------------------------------------
//
// Distance maximum (supremum norm)
//
// Maximum distance between 2 vectors a,b is
// d = max |ai - bi |
//
// This function compute distance between 2 vectors x[i1,] & y[i2,]
// x and y are matrix; we use here only line i1 from x and
// line i2 from y. Number of column (nc) is the same in x and y,
// number of column can differ (nr_x, nr_y).
//
// Flag will be set to 0 if NA value computed in distance
//
// When call by function distance or hclust, x and y are the same; it computes
// distance between vector x[i1,] and x[i2,]
//
// \param x matrix of size nr_x * nc; line i1 is of interest
// \param y matrix of size nr_y * nc; line i1 is of interest
// \param nr_x number of row in matrix x
// \param nr_y number of row in matrix y
// \param nc number of column in matrix x or y
// \param i1 row choosen in matrix x
// \param i2 row choosen in matrix y
// \param flag set to 0 if NA value computed in distance
// \param opt: unused
//
//  Return: distance value
//
// ---------------------------------------------------------
template<class T> T  distance_T<T>::R_maximum(vecteur<double> & x, vecteur<double> & y ,
					      int * flag, T_tri & opt)
{
  T dev, dist;
  int count, j;

  count = 0;
  dist = std::numeric_limits<T>::min();
  for(j = 0 ; j < x.size() && j < y.size() ; j++) {
    if(R_FINITE(x[j]) && R_FINITE(y[j])) {
      dev = fabs(x[j] - y[j]);
      if(dev > dist)
	dist = dev;
      count++;
    }

  }
  if(count == 0)
    {
      *flag = 0;
      return NA_REAL;
    }
  return dist;
}


// ---------------------------------------------------------
// Distance manhattan (i.e. sum of abs difference )
//
// manhattan distance between 2 vectors a,b is
//  d = sum_i | a_i - b_i |
//
// This function compute distance between 2 vectors x[i1,] & y[i2,]
// x and y are matrix; we use here only line i1 from x and
// line i2 from y. Number of column (nc) is the same in x and y,
// number of column can differ (nr_x, nr_y).
//
// Flag will be set to 0 if NA value computed in distance
//
// When call by function distance or hclust, x and y are the same; it computes
// distance between vector x[i1,] and x[i2,]
//
// \param x matrix of size nr_x * nc; line i1 is of interest
// \param y matrix of size nr_y * nc; line i1 is of interest
// \param nr_x number of row in matrix x
// \param nr_y number of row in matrix y
// \param nc number of column in matrix x or y
// \param i1 row choosen in matrix x
// \param i2 row choosen in matrix y
// \param flag set to 0 if NA value computed in distance
// \param opt: unused
// 
//  Return: distance value
//
// ---------------------------------------------------------
template<class T> T  distance_T<T>::R_manhattan(vecteur<double> & x, vecteur<double> & y ,
						int * flag, T_tri & opt)
{
  T dist;
  int count, j;

  count = 0;
  dist = 0;
  for(j = 0 ; j < x.size() ; j++) {
    if(R_FINITE(x[j]) && R_FINITE(y[j])) {
      dist += fabs(x[j] - y[j]);
      count++;
    }

  }
  if(count == 0)
    {
      *flag = 0;
      return NA_REAL;
    }
  if(count != x.size()) dist /= ((T)count/x.size());
  return dist;
}

// ---------------------------------------------------------
// Distance Camberra
//
// Camberra distance between 2 vectors a,b is
//  d = sum_i | a_i - b_i | / | a_i + b_i |
//
// This function compute distance between 2 vectors x[i1,] & y[i2,]
// x and y are matrix; we use here only line i1 from x and
// line i2 from y. Number of column (nc) is the same in x and y,
// number of column can differ (nr_x, nr_y).
//
// Flag will be set to 0 if NA value computed in distance
//
// When call by function distance or hclust, x and y are the same; it computes
// distance between vector x[i1,] and x[i2,]
//
// \param x matrix of size nr_x * nc; line i1 is of interest
// \param y matrix of size nr_y * nc; line i1 is of interest
// \param nr_x number of row in matrix x
// \param nr_y number of row in matrix y
// \param nc number of column in matrix x or y
// \param i1 row choosen in matrix x
// \param i2 row choosen in matrix y
// \param flag set to 0 if NA value computed in distance
// \param opt: unused
// 
//  Return: distance value
//
// ---------------------------------------------------------
template<class T> T  distance_T<T>::R_canberra(vecteur<double> & x, vecteur<double> & y ,
					       int * flag, T_tri & opt)
{
  T dist, sum, diff;
  int count, j;

  count = 0;
  dist = 0;
  for(j = 0 ; j < x.size() && j < y.size() ; j++) {
    if(R_FINITE(x[j]) && R_FINITE(y[j])) {
      sum = fabs(x[j] + y[j]);
      diff = fabs(x[j] - y[j]);
      if (sum > DBL_MIN || diff > DBL_MIN) {
	dist += diff/sum;
	count++;
      }
    }

  }
  if(count == 0)
    {
      *flag = 0;
      return NA_REAL;
    }
  if(count != x.size()) dist /= ((T)count/x.size());
  return dist;
}

/** \brief Distance binary
 */
template<class T> T  distance_T<T>::R_dist_binary(vecteur<double> & x, vecteur<double> & y ,
						  int * flag, T_tri & opt)
{
  int total, count, dist;
  int j;

  total = 0;
  count = 0;
  dist = 0;

  for(j = 0 ; j < x.size() && j < y.size() ; j++) {
    if(R_FINITE(x[j]) && R_FINITE(y[j])) {
      if(x[j] || y[j]){
	count++;
	if( ! (x[j] && y[j]) ) dist++;
      }
      total++;
    }

  }

  if(total == 0)
    {
      *flag = 0;
      return NA_REAL;
    }
  if(count == 0) return 0;
  return (T) dist / count;
}

/** \brief Pearson / Pearson centered (correlation)
 *  \note Added by A. Lucas
 */
template<class T> T  distance_T<T>::R_pearson(vecteur<double> & x, vecteur<double> & y ,
					      int * flag, T_tri & opt)
{
  T num,sum1,sum2, dist;
  int count,j;

  count= 0;
  num = 0;
  sum1 = 0;
  sum2 = 0;
  for(j = 0 ; j < x.size() && j < y.size() ; j++) {
    if(R_FINITE(x[j]) && R_FINITE(y[j])) {
      num += (x[j] * y[j]);
      sum1 += (x[j] * x[j]);
      sum2 += (y[j] * y[j]);
      count++;
    }

  }
  if(count == 0) 
    {
      *flag = 0;
      return NA_REAL;
    }
  dist = 1 - ( num / sqrt(sum1 * sum2) );
  return dist;
}

/** \brief Absoulute Pearson / Pearson uncentered (correlation)
 *  \note Added by L. Cerulo
 */
template<class T> T  distance_T<T>::R_abspearson(vecteur<double> & x, vecteur<double> & y ,
						 int * flag, T_tri & opt)
{
  T num,sum1,sum2, dist;
  int count,j;
	
  count= 0;
  num = 0;
  sum1 = 0;
  sum2 = 0;
  for(j = 0 ; j < x.size() && j < y.size() ; j++) {
    if(R_FINITE(x[j]) && R_FINITE(y[j])) {
      num += (x[j] * y[j]);
      sum1 += (x[j] * x[j]);
      sum2 += (y[j] * y[j]);
      count++;
    }

  }
  if(count == 0) 
	{
		*flag = 0;
		return NA_REAL;
	}
  dist = ( num / sqrt(sum1 * sum2) );
	if (dist<0) {
		dist*=-1;
	}
  return (1-dist);	
}



/** \brief Distance correlation (Uncentered Pearson)
 *  \note Added by A. Lucas
 */
template<class T> T  distance_T<T>::R_correlation(vecteur<double> & x, vecteur<double> & y ,
						  int * flag, T_tri & opt)
{
  T num,denum2,sumx,sumy,sumxx,sumyy,sumxy;
  int count,j;

  count= 0;
  sumx=0;
  sumy=0;
  sumxx=0;
  sumyy=0;
  sumxy=0;


  for(j = 0 ; j < x.size() && j < y.size() ; j++) {
    if(R_FINITE(x[j]) && R_FINITE(y[j])) {
      sumxy += (x[j] * y[j]);
      sumx += x[j];
      sumy += y[j];
      sumxx += x[j] * x[j];
      sumyy += y[j] * y[j];
      count++;
    }

  }
  if(count == 0)
    {
      *flag = 0;
      return NA_REAL;
    }
  num = sumxy - ( sumx*sumy /count );
  denum2 =  (sumxx - (sumx*sumx /count ) )* (sumyy - (sumy*sumy /count ) ) ;
  if(denum2 <=0)
    {
      return 0;
    }
  return 1 - (num / sqrt(denum2));
}

/** \brief Absolute Distance correlation (Uncentered Pearson)
 *  \note Added by L. Cerulo
 */
template<class T> T  distance_T<T>::R_abscorrelation(vecteur<double> & x, vecteur<double> & y ,
						     int * flag, T_tri & opt)
{
  T num,denum,sumx,sumy,sumxx,sumyy,sumxy,dist,term;
  int count,j;
	
  count= 0;
  sumx=0;
  sumy=0;
  sumxx=0;
  sumyy=0;
  sumxy=0;
	
	
  for(j = 0 ; j < x.size() && j < y.size() ; j++) {
    if(R_FINITE(x[j]) && R_FINITE(y[j])) {
      sumxy += (x[j] * y[j]);
      sumx += x[j];
      sumy += y[j];
      sumxx += x[j] * x[j];
      sumyy += y[j] * y[j];
      count++;
    }

  }
  if(count == 0)
	{
		*flag = 0;
		return NA_REAL;
	}
  num = sumxy - ( sumx*sumy /count );

	term=(sumxx - (sumx*sumx /count ) )* (sumyy - (sumy*sumy /count ) );
	if (term<=0) return 1;
	denum = sqrt( term );
	dist=num/denum;
	if (dist<0) {
		dist*=-1;
	}
  return (1-dist);	
}

// ---------------------------------------------------------
// Distance Spearman
//
// Spearman distance between 2 vectors a,b is
//  d = sum_i (rank(a_i) - rank(b_i) )^2
//
// If one NA found: return NA
//
// This function compute distance between 2 vectors x[i1,] & y[i2,]
// x and y are matrix; we use here only line i1 from x and
// line i2 from y. Number of column (nc) is the same in x and y,
// number of column can differ (nr_x, nr_y).
//
// Flag will be set to 0 if NA value computed in distance
//
// When call by function distance or hclust, x and y are the same; it computes
// distance between vector x[i1,] and x[i2,]
//
// \param x matrix of size nr_x * nc; line i1 is of interest
// \param y matrix of size nr_y * nc; line i1 is of interest
// \param nr_x number of row in matrix x
// \param nr_y number of row in matrix y
// \param nc number of column in matrix x or y
// \param i1 row choosen in matrix x
// \param i2 row choosen in matrix y
// \param flag set to 0 if NA value computed in distance
// \param opt:  a set of 6 vectors of size nc, allocated but uninitialised. 
//         aim of this parameter is to avoid several vector allocation 
// 
//  Return: distance value
//
// ---------------------------------------------------------
template<class T> T  distance_T<T>::R_spearman(vecteur<double> & x, vecteur<double> & y ,
					       int * flag, T_tri & opt)
{
  int j;
  double * data_tri_x = opt.data_tri_x.get();
  int * order_tri_x = opt.order_tri_x.get();
  int * rank_tri_x = opt.rank_tri_x.get();
  double * data_tri_y = opt.data_tri_y.get();
  int * order_tri_y = opt.order_tri_y.get();
  int * rank_tri_y = opt.rank_tri_y.get();
  int n;
  T diffrang=0;

  for(j = 0 ; j < x.size() && j < y.size() ; j++) {
    if(!(R_FINITE(x[j]) && R_FINITE(y[j])))
      {
	*flag = 0;
	return NA_REAL;	
      }
    order_tri_x[j] = rank_tri_x[j] = 
      order_tri_y[j] = rank_tri_y[j] = j;
    data_tri_x[j] = x[j];
    data_tri_y[j] = y[j];

  }

  n  = x.size();
  /* sort and compute rank */
  /* First list */
  rsort_rank_order(data_tri_x, order_tri_x,rank_tri_x, &n);
  /* Second list */
  rsort_rank_order(data_tri_y, order_tri_y,rank_tri_y, &n);

  for(j=0;j< x.size();j++)
    {
      diffrang += pow((T) ( rank_tri_x[j] - rank_tri_y[j]),2);
    }

  return(  diffrang );

  /*
   * verification in R:
   * Dist(x,method='spearman') ; n =dim(x)[2]
   * l=c(x[3,],x[4,]); sum((rank(l[1:n])-rank(l[(n+1):(2*n)]))^2)
   * cor.test(x[3,],x[4,],method="spearm")$statistic
   */

}


/** \brief Kendall distance (rank base metric)
 * 1 - corr_kendall(x,y)
 *
 *  \note Added by A. Lucas
 *
template<class T> T  distance_T<T>::R_kendall_corr(double * x, double * y , int nr_x, int nr_y, int nc, 
						   int i1, int i2,
						   int * flag, T_tri & opt)
{
  int j,k;
  double * data_tri_x = opt.data_tri_x;
  int * order_tri_x = opt.order_tri_x;
  int * rank_tri_x = opt.rank_tri_x;
  double * data_tri_y = opt.data_tri_y;
  int * order_tri_y = opt.order_tri_y;
  int * rank_tri_y = opt.rank_tri_y;
  int n;
  T dist,P=0;

  for(j = 0 ; j < nc ; j++) {
    if(!(R_FINITE(x[i1]) && R_FINITE(y[i2])))
      {
	*flag = 0;
	return NA_REAL;	
      }
    order_tri_x[j] = rank_tri_x[j] = 
      order_tri_y[j] = rank_tri_y[j] = j;
    data_tri_x[j] = x[i1];
    data_tri_y[j] = y[i2];
    i1 += nr_x;
    i2 += nr_y;
  }

  n  = nc;
  // sort and compute rank 
  // First list 
  rsort_rank_order(data_tri_x, order_tri_x,rank_tri_x, &n);
  // Second list 
  rsort_rank_order(data_tri_y, order_tri_y,rank_tri_y, &n);

  for(j=0;j<nc;j++)
    {
     
      for(k=j+1; k < nc; ++k)
	if(rank_tri_y[order_tri_x[j]] < rank_tri_y[order_tri_x[k]])
	  ++P;
    }

  dist = 2 - ( 4*P / (n * (n-1) ) ) ;

  return( dist );

}
*/

// ---------------------------------------------------------
// Distance Kendall
//
// Kendall distance between 2 vectors a,b is
//  d = sum_i Kij (x,y)
//
// With Kij(x,y) is 0 if xi,xj in same order as yi,yj;
 //                 1 if not
//
// If one NA found: return NA
//
// This function compute distance between 2 vectors x[i1,] & y[i2,]
// x and y are matrix; we use here only line i1 from x and
// line i2 from y. Number of column (nc) is the same in x and y,
// number of column can differ (nr_x, nr_y).
//
// Flag will be set to 0 if NA value computed in distance
//
// When call by function distance or hclust, x and y are the same; it computes
// distance between vector x[i1,] and x[i2,]
//
// \param x matrix of size nr_x * nc; line i1 is of interest
// \param y matrix of size nr_y * nc; line i1 is of interest
// \param nr_x number of row in matrix x
// \param nr_y number of row in matrix y
// \param nc number of column in matrix x or y
// \param i1 row choosen in matrix x
// \param i2 row choosen in matrix y
// \param flag set to 0 if NA value computed in distance
// \param opt:  a set of 6 vectors of size nc, allocated but uninitialised. 
//         aim of this parameter is to avoid several vector allocation 
// 
//  Return: distance value
//
// ---------------------------------------------------------
template<class T> T  distance_T<T>::R_kendall(vecteur<double> & x, vecteur<double>& y ,
					      int * flag, T_tri & opt)
{
  int j,k;
  double * data_tri_x = opt.data_tri_x.get();
  int * order_tri_x = opt.order_tri_x.get();
  int * rank_tri_x = opt.rank_tri_x.get();
  double * data_tri_y = opt.data_tri_y.get();
  int * order_tri_y = opt.order_tri_y.get();
  int * rank_tri_y = opt.rank_tri_y.get();
  int n;
  T dist,P=0;
  bool ordre_x,ordre_y;

  for(j = 0 ; j < x.size() && j < y.size() ; j++) {
    if(!(R_FINITE(x[j]) && R_FINITE(y[j])))
      {
	*flag = 0;
	return NA_REAL;	
      }
    order_tri_x[j] = rank_tri_x[j] = 
      order_tri_y[j] = rank_tri_y[j] = j;
    data_tri_x[j] = x[j];
    data_tri_y[j] = y[j];

  }

  n  = x.size();
  /* sort and compute rank */
  /* First list */
  rsort_rank_order(data_tri_x, order_tri_x,rank_tri_x, &n);
  /* Second list */
  rsort_rank_order(data_tri_y, order_tri_y,rank_tri_y, &n);

  for(j=0;j<x.size();j++)
    {     
      for(k=j+1; k < x.size(); ++k)
	{
	  ordre_x = rank_tri_x[j] < rank_tri_x[k];
	  ordre_y = rank_tri_y[j] < rank_tri_y[k];
	  if(ordre_x != ordre_y)
	    ++P;
	}
    }

  dist = 2* P / (n * (n-1) )  ;

  return( dist );

}


// ---------------------------------------------------------
//
// R_distance: compute parallelized distance. Function called direclty by R
// \brief compute distance and call function thread_dist
// that call one of function R_euclidean or R_...
// \param x input matrix
// \param nr,nc number of row and columns
//        nr individuals with nc values.
// \param d distance half matrix.
// \param diag if we compute diagonal of dist matrix (usualy: no).
// \param method 1, 2,... method used
// \param nbprocess: number of threads to create
// \param ierr error return; 1 good; 0 missing values
//
// ---------------------------------------------------------
template<class T> void  distance_T<T>::distance(double *x, int *nr,
						int *nc, T *d, int *diag, 
						int *method,int *nbprocess, 
						int * ierr,int i2)
{



  int  i;
  T_argument * arguments;

  bool dc = (*diag) ? 0 : 1; /* diag=1:  we do the diagonal */ 
  
  /*
   * Arguments sent to thread (adress):
   * number of thread
   * nr
   * nc 
   * dc
   * *x
   * *d
   * *method
   * *ierr
   */ 

  arguments = (T_argument * ) malloc ((*nbprocess) * sizeof( T_argument ));


  //printf("nb processs %d\n",*nbprocess);

  for(i=0; i< *nbprocess; ++i)
    {
      arguments[i].id =i;
      arguments[i].x=x;
      arguments[i].nr = nr;
      arguments[i].nc = nc;
      arguments[i].dc = dc;
      arguments[i].d = d;
      arguments[i].method = method;
      arguments[i].nbprocess= *nbprocess;
      arguments[i].ierr=ierr;
      arguments[i].i2=i2;
    }
  *ierr = 1; /* res = 1 => no missing values
		res = 0 => missings values */


#ifndef WIN32
  pthread_t * th = (pthread_t *) malloc ( *nbprocess * sizeof(pthread_t));

  for (i=0; i < *nbprocess ; i++)
    {
      pthread_create(th+i,0,distance_T<T>::thread_dist,(void *)(arguments+i));
    }

  /* Attends la fin    */
  for (i=0; i < *nbprocess ; i++)
    {      
      pthread_join(*(th+i),NULL);
    }      
  free( th);

#else

  // p_thread not yet used on windows.

  arguments[0].nbprocess = 1;
	arguments[0].i2 = i2;
  thread_dist((void *)arguments);
#endif

  free( arguments );

}

//template<class T> T  distance_T<T>::distance(double * x, double * y , int nr_x, int nr_y, int nc, 


//
// get the distance function
//

template<class T>  void distance_T<T>::getDistfunction(int method,distfunction& distfun)
{
  //  T (*distfun)(double*,double*,int, int, int, int, int, int *, T_tri &) = NULL;
  
  switch(method) 
    {
    case EUCLIDEAN:
      distfun = R_euclidean;
      break;
    case MAXIMUM:
      distfun = R_maximum;
      break;
    case MANHATTAN:
      distfun = R_manhattan;
      break;
    case CANBERRA:
      distfun = R_canberra;
      break;
    case BINARY:
      distfun = R_dist_binary;
      break;
    case PEARSON:
      distfun = R_pearson;
      break;
    case CORRELATION:
      distfun = R_correlation;
      break;
    case SPEARMAN:
      distfun = R_spearman;
      break;
    case KENDALL:
      distfun = R_kendall;
      break;
    case ABSPEARSON:
      distfun = R_abspearson;
      break;
    case ABSCORRELATION:
      distfun = R_abscorrelation;
      break;			
    default:
      {
	error("distance(): invalid distance");
	distfun = R_euclidean;
      }
}


}

/** thread_dist function that compute distance.
 *
 */
template <class T> void* distance_T<T>::thread_dist(void* arguments_void)
{

  int nbprocess,nr,nc,i,j,dc,ij;
  T_argument * arguments = static_cast<T_argument*>(arguments_void); 
  T * d;
  double * x;
  int * method;
  int * ierr;
  /* for spearman dist */
  T_tri opt ;



  distfunction distfun;

  short int no = arguments[0].id;
  nr = *arguments[0].nr;
  nc = *arguments[0].nc;
  dc = arguments[0].dc;
  x  = arguments[0].x;
  d  = arguments[0].d;
  method =  arguments[0].method;
  nbprocess = arguments[0].nbprocess;
  ierr =  arguments[0].ierr;
  int i2 =  arguments[0].i2;


  matrice<double> myMatrice (x, nr, nc);
  matriceTriangle<T> distMatrice(d, nr, false);
  getDistfunction(*method,distfun);

  
  if( (*method == SPEARMAN) ||  (*method == KENDALL))
    {
      opt.reset(nc);
    }

  /*
    debut = ((nr+1) / nbprocess + 1 ) * no ;
    fin =  min ( ((nr+1) / nbprocess + 1) * ( no + 1 ) , (nr+1));
  */

  /* debut des boucles 0
     fin: nr+1 */

    
  int debut = (int) floor( ((nr+1.)*nbprocess - sqrt( (nr+1.)*(nr+1.) * nbprocess * nbprocess - (nr+1.)*(nr+1.) * nbprocess * no  ) )/nbprocess);
  int fin = (int) floor(((nr+1.)*nbprocess - sqrt( (nr+1.)*(nr+1.) * nbprocess * nbprocess - (nr+1.)*(nr+1.) * nbprocess * (no+1.)  ) )/nbprocess);

    
  if (fin > nr) {
    fin = nr;
  }
  //printf("Thread %d debut %d fin %d i2=%d met=%d\n",no,debut,fin,i2,*method);    


  // here: the computation !
  //    for(j = 0 ; j <= nr ; j++)
  if (i2==-1) /* compute all distance matrix*/
    {
      for(j = debut ; j < fin ; j++)
	{
	  vecteur<T> distRow = distMatrice.getRow(j);
	  vecteur<double> rowJ = myMatrice.getRow(j);

	  ij = (2 * (nr-dc) - j +1) * (j) /2 ;
	  for(i = j+dc ; i < nr ; i++)
	    {
	      vecteur<double> rowI = myMatrice.getRow(i);
	      distRow[i] = distfun(rowI, rowJ ,ierr,opt);
	    }
	}
  } 
  else { /* updates the distance only for i2*/

    // a row of distance matrix
    vecteur<T> distRow = distMatrice.getRow(i2);
    vecteur<double> rowI = myMatrice.getRow(i2);

    for(j = debut ; j < fin ; j++)
      {
	if (i2!=j) {

	  vecteur<double> rowJ = myMatrice.getRow(j);
	  distRow[j] = distfun(rowI, rowJ,ierr,opt);
	  //printf("updated dist %d %d %f\n",i2,j,d[ind1]);
	}
	
      }
  }

  return (void*)0;
    
}





// ---------------------------------------------------------
//
// R_distance_kms: compute distance between individual i1 and
// centroid i2
//
// compute distance and call one of function R_euclidean or R_...
// This function is called by kmeans_Lloyd2 
//
// \param x input matrix (individuals)
// \param y input matrix (centroids)
// \param nr1,nr2,nc number of row (nr1:x, nr2:y) and columns
//        nr individuals with nc values.
// \param i1, i2: indice of individuals (individual i1, centroid i2)
// \param method 1, 2,... method used
// \param ierr for NA 0 if no value can be comuted due to NA
// \param opt optional parameter send to spearman dist.
//
// ---------------------------------------------------------
template <class T> T distance_T<T>::distance_kms(vecteur<double> & x, vecteur<double> & y , int *method, 
						 int * ierr, T_tri & opt)
{
  /*
   * compute distance x[i1,*] - y[i2,*]
   * x matrix n x p
   * y matrix m x p
   * nr1 = n; nr2 = m; nc =p
   */
  
  T res;

  distfunction distfun;

  getDistfunction(*method,distfun);

  // here: distance computation
  res = distfun(x,y,ierr, opt);
  return( res);
}


};
