
#ifndef _AMAP_DISTANCE_TEMPLATE
#define _AMAP_DISTANCE_TEMPLATE 1

#include "smartPtr.h"
#include "matrice.h"

namespace amap {

  template<class T> class distance_T
    {

    public:
      /* == 1,2,..., defined by order in the r function dist */
      enum { EUCLIDEAN=1, MAXIMUM, MANHATTAN, CANBERRA, BINARY ,PEARSON, CORRELATION, SPEARMAN,  KENDALL, ABSPEARSON, ABSCORRELATION};

  
      class T_tri
      {
      public:
	SmartPtr<double> data_tri_x;
	SmartPtr<int> order_tri_x;
	SmartPtr<int> rank_tri_x;
	SmartPtr<double> data_tri_y;
	SmartPtr<int> order_tri_y;
	SmartPtr<int> rank_tri_y;

      T_tri() :
	data_tri_x(0),
	  order_tri_x(0),
	  rank_tri_x(0),
	  data_tri_y(0),
	  order_tri_y(0),
	  rank_tri_y(0) {};
    
	void reset(int size) {
	  data_tri_x.reset(size);
	  order_tri_x.reset(size);
	  rank_tri_x.reset(size);
	  data_tri_y.reset(size);
	  order_tri_y.reset(size);
	  rank_tri_y.reset(size);
	}
      };


      typedef T (* distfunction)(vecteur<double> & x, vecteur<double> & y , int *, T_tri &);

    private:

      /** \brief arguments sent to distance thread */
      struct T_argument
      {
	short int id;
	double * x;
	int * nr;
	int * nc;
	bool dc;
	T * d;
	int * method;
	int  nbprocess;
	int * ierr;
	int i2;
      };


  
      // only static functions; no attributes
      distance_T();
    

      ~distance_T();

    public:

      /** return a distance function, depending on method
       */
      static void getDistfunction(int method,distfunction & distfun);

      /** \brief R_distance compute parallelized distance. 
       *
       * compute distance and call function thread_dist
       * that call one of function R_euclidean or R_...
       *
       * \param x input matrix
       * \param nr,nc number of row and columns
       *        nr individuals with nc values.
       * \param d distance half matrix.
       * \param diag if we compute diagonal of dist matrix (usualy: no).
       * \param method 1, 2,... method used (correspond to the enum)
       * \param nbprocess: number of threads to create
       * \param ierr error return; 1 good; 0 missing values
       * \param i2: if -1: ignored, else, compute
       *            distance between individual i2 and others
       */
      static void distance(double *x, int *nr, int *nc, T *d, int *diag,
			   int *method,int *nbprocess, int * ierr,int i2);

	

      /** \brief R_distance_kms: compute distance between individual i1 and
       * centroid i2
       *
       * compute distance and call one of function R_euclidean or R_...
       * This function is called by kmeans_Lloyd2 
       *
       * \param x input matrix (individuals)
       * \param y input matrix (centroids)
       * \param nr1,nr2,nc number of row (nr1:x, nr2:y) and columns
       *        nr individuals with nc values.
       * \param i1, i2: indice of individuals (individual i1, centroid i2)
       * \param method 1, 2,... method used (correspond to the enum)
       * \param ierr for NA 0 if no value can be comuted due to NA
       * \param opt optional parameter required for spearman
       */
      static T distance_kms(vecteur<double> & x, vecteur<double> & y ,
			    int *method,
			    int * ierr, T_tri & opt);


    private:
  
      static void* thread_dist(void* arguments);

      /** \brief Distance euclidean (i.e. sqrt(sum of square) )
       *
       * Euclidean distance between 2 vectors a,b is
       * \f[ d = \sqrt{ \sum_i^n (a_i - b_i)^2}
       * \f]
       *
       * When NA values found for a_i or b_i, both a_i and b_i are 
       * skipped. Number of values skipped is couned (#NA in next formula) 
       *
       * \f[ d = \sqrt{\frac{n}{#NA} \sum_{i=1; a_i  \in \Re; b_i \in \Re}^n (a_i - b_i)^2}
       * \f]
       *
       * This function compute distance between 2 vectors x[i1,] & y[i2,]
       * x and y are matrix; we use here only line i1 from x and
       * line i2 from y. Number of column (nc) is the same in x and y,
       * number of column can differ (nr_x, nr_y).
       *
       * Flag will be set to 0 if too many NA to compute distance
       * 
       * When call by function distance or hclust, x and y are the same; it computes
       * distance between vector x[i1,] and x[i2,]
       *
       * \param x matrix of size nr_x * nc; line i1 is of interest
       * \param y matrix of size nr_y * nc; line i1 is of interest
       * \param nr_x number of row in matrix x
       * \param nr_y number of row in matrix y
       * \param nc number of column in matrix x or y
       * \param i1 row choosen in matrix x
       * \param i2 row choosen in matrix y
       * \param flag set to 0 if NA value computed in distance
       * \param opt unused
       *
       * \return distance value
       *
       */
      static T  R_euclidean(vecteur<double> & x, vecteur<double> & y ,
			    int * flag, T_tri & opt);

      /** \brief Distance maximum (supremum norm)
       *
       * Maximum distance between 2 vectors a,b is
       * \f[ d = \max_i |a_i - b_i|
       * \f]
       *
       * NA values are omitted.
       *
       * This function compute distance between 2 vectors x[i1,] & y[i2,]
       * x and y are matrix; we use here only line i1 from x and
       * line i2 from y. Number of column (nc) is the same in x and y,
       * number of column can differ (nr_x, nr_y).
       *
       * Flag will be set to 0 if too many NA to compute distance
       *
       * When call by function distance or hclust, x and y are the same; it computes
       * distance between vector x[i1,] and x[i2,]
       *
       * \param x matrix of size nr_x * nc; line i1 is of interest
       * \param y matrix of size nr_y * nc; line i1 is of interest
       * \param nr_x number of row in matrix x
       * \param nr_y number of row in matrix y
       * \param nc number of column in matrix x or y
       * \param i1 row choosen in matrix x
       * \param i2 row choosen in matrix y
       * \param flag set to 0 if NA value computed in distance
       * \param opt unused
       *
       * \return distance value
       *
       */
      static T R_maximum(vecteur<double> & x, vecteur<double> & y ,
			 int * flag, T_tri & opt);


      /** \brief manhattan (i.e. sum of abs difference )
       *
       * manhattan distance between 2 vectors a,b is
       * \f[ d = \sum_i^n |a_i - b_i|
       * \f]
       *
       * When NA values found for a_i or b_i, both a_i and b_i are 
       * skipped. Number of values skipped is couned (#NA in next formula) 
       *
       * \f[ d = \frac{n}{#NA} \sum_{i=1; a_i  \in \Re; b_i \in \Re}^n |a_i - b_i|}
       * \f]
       *
       * This function compute distance between 2 vectors x[i1,] & y[i2,]
       * x and y are matrix; we use here only line i1 from x and
       * line i2 from y. Number of column (nc) is the same in x and y,
       * number of column can differ (nr_x, nr_y).
       *
       * Flag will be set to 0 if too many NA to compute distance
       * 
       * When call by function distance or hclust, x and y are the same; it computes
       * distance between vector x[i1,] and x[i2,]
       *
       * \param x matrix of size nr_x * nc; line i1 is of interest
       * \param y matrix of size nr_y * nc; line i1 is of interest
       * \param nr_x number of row in matrix x
       * \param nr_y number of row in matrix y
       * \param nc number of column in matrix x or y
       * \param i1 row choosen in matrix x
       * \param i2 row choosen in matrix y
       * \param flag set to 0 if NA value computed in distance
       * \param opt unused
       *
       * \return distance value
       *
       */
      static T R_manhattan(vecteur<double> & x, vecteur<double> & y ,
			   int * flag, T_tri & opt);

      /** \brief Camberra distance
       *
       * Camberra distance between 2 vectors a,b is
       * \f[ d = \sum_i^n |a_i - b_i| / |a_i + b_i|
       * \f]
       *
       * When NA values found for a_i or b_i, both a_i and b_i are 
       * skipped. Number of values skipped is couned (#NA in next formula) 
       *
       *
       * This function compute distance between 2 vectors x[i1,] & y[i2,]
       * x and y are matrix; we use here only line i1 from x and
       * line i2 from y. Number of column (nc) is the same in x and y,
       * number of column can differ (nr_x, nr_y).
       *
       * Flag will be set to 0 if too many NA to compute distance
       * 
       * When call by function distance or hclust, x and y are the same; it computes
       * distance between vector x[i1,] and x[i2,]
       *
       * \param x matrix of size nr_x * nc; line i1 is of interest
       * \param y matrix of size nr_y * nc; line i1 is of interest
       * \param nr_x number of row in matrix x
       * \param nr_y number of row in matrix y
       * \param nc number of column in matrix x or y
       * \param i1 row choosen in matrix x
       * \param i2 row choosen in matrix y
       * \param flag set to 0 if NA value computed in distance
       * \param opt unused
       *
       * \return distance value
       *
       */
      static T R_canberra(vecteur<double> & x, vecteur<double> & y ,
			  int * flag, T_tri & opt);

      /** \brief Distance binary
       */
      static T R_dist_binary(vecteur<double> & x, vecteur<double> & y ,
			     int * flag, T_tri & opt);

      /** \brief Pearson / Pearson centered (correlation)
       *  \note Added by A. Lucas
       */
      static T R_pearson(vecteur<double> & x, vecteur<double> & y ,
			 int * flag, T_tri & opt);

      /** \brief Distance correlation (Uncentered Pearson)
       *  \note Added by A. Lucas
       */
      static T R_correlation(vecteur<double> & x, vecteur<double> & y ,
			     int * flag, T_tri & opt);

      /** \brief Pearson / Pearson centered (correlation)
       *  \note Added by L. Cerulo
       */
      static T R_abspearson(vecteur<double> & x, vecteur<double> & y ,
			    int * flag, T_tri & opt);
	
      /** \brief Distance correlation (Uncentered Pearson)
       *  \note Added by L. Cerulo
       */
      static T R_abscorrelation(vecteur<double> & x, vecteur<double> & y ,
				int * flag, T_tri & opt);

      /** \brief Spearman distance (rank base metric)
       *
       * Spearman distance between 2 vectors a,b is
       * \f[ d = \sum_i^n (rank(a_i) - rank(b_i)) ^ 2
       * \f]
       *
       * If a NA found: return NA, flag is set to 0.
       *
       *
       * This function compute distance between 2 vectors x[i1,] & y[i2,]
       * x and y are matrix; we use here only line i1 from x and
       * line i2 from y. Number of column (nc) is the same in x and y,
       * number of column can differ (nr_x, nr_y).
       *
       * 
       * When call by function distance or hclust, x and y are the same; it computes
       * distance between vector x[i1,] and x[i2,]
       *
       * \param x matrix of size nr_x * nc; line i1 is of interest
       * \param y matrix of size nr_y * nc; line i1 is of interest
       * \param nr_x number of row in matrix x
       * \param nr_y number of row in matrix y
       * \param nc number of column in matrix x or y
       * \param i1 row choosen in matrix x
       * \param i2 row choosen in matrix y
       * \param flag set to 0 if NA value computed in distance
       * \param opt a set of 6 vectors of size nc, allocated but uninitialised. 
       *        aim of this parameter is to avoid several vector allocation 
       *
       * \return distance value
       *
       */
      static T R_spearman(vecteur<double> & x, vecteur<double> & y ,
			  int * flag, T_tri & opt);

      /** \brief Kendall distance (rank base metric)
       *
       * Kendall distance between 2 vectors a,b is
       * \f[ d = \sum_{i,j} K_{i,j}(x,y)
       * \f]
       *
       * With \f$ K_{i,j}(x,y)\f$ is 0 if 
       * \f$ (x_i, x_j) \f$ in same order as \f$ ( y_i,y_j) \f$,
       *  1 if not. 
       *
       *
       * If a NA found: return NA, flag is set to 0.
       *
       *
       * This function compute distance between 2 vectors x[i1,] & y[i2,]
       * x and y are matrix; we use here only line i1 from x and
       * line i2 from y. Number of column (nc) is the same in x and y,
       * number of column can differ (nr_x, nr_y).
       *
       * 
       * When call by function distance or hclust, x and y are the same; it computes
       * distance between vector x[i1,] and x[i2,]
       *
       * \param x matrix of size nr_x * nc; line i1 is of interest
       * \param y matrix of size nr_y * nc; line i1 is of interest
       * \param nr_x number of row in matrix x
       * \param nr_y number of row in matrix y
       * \param nc number of column in matrix x or y
       * \param i1 row choosen in matrix x
       * \param i2 row choosen in matrix y
       * \param flag set to 0 if NA value computed in distance
       * \param opt a set of 6 vectors of size nc, allocated but uninitialised. 
       *        aim of this parameter is to avoid several vector allocation 
       *
       * \return distance value
       *
       */
      static T R_kendall(vecteur<double> & x, vecteur<double> & y ,
			 int * flag, T_tri & opt);


    };

};

#endif

#ifndef _AMAP_DISTANCE_TEMPLATE_CPP 
#define _AMAP_DISTANCE_TEMPLATE_CPP 1
#include "distance_T.h.h"
#endif


