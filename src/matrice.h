#ifndef AMAP_matrice
#define AMAP_matrice 1
#include <R_ext/Error.h>

namespace amap {
  
  template<class T> class array {
  public: 
    /** accessor to a data
     * \param index index in array
     * \return data
     */
    virtual T & operator[] (int index) = 0;
  };

  template<class T> class vecteur {

  private:
    array<T> & myMatrice;

    
    int indexFirstData;

    int stepByData;

    int vectorSize;
    
  public:
  vecteur(array<T> & values_p, int indexFirstData_p, int stepByData_p,  int size) 
    : myMatrice(values_p),
      indexFirstData(indexFirstData_p),
      stepByData(stepByData_p) ,
      vectorSize(size)
      {
      };
    
    /**
     * get data at index.
     * \param index index of data.
     * \return data.
     */
    virtual  T & operator[] (int index) {
      if (index >= vectorSize) {
	Rf_error("vecteur::operator[]: out of bound %d - %d", index, vectorSize);
      }

      return (myMatrice)[indexFirstData + (index * stepByData)];
    };
    
    /**
     * get size.
     * \return size
     */
    virtual int size() {
      return vectorSize;
    };
  };


  /**
   * Matrix data.
   *
   * a matrix with 3 row and 4 cols
   * +---+---+---+---+
   * | 0 | 3 | 6 | 9 |
   * +---+---+---+---+
   * | 1 | 4 | 7 | 10|
   * +---+---+---+---+
   * | 2 | 5 | 8 | 11|
   * +---+---+---+---+
   * 
   * 
   */
  template<class T> class matrice : public array<T> {
  private:
    /**
     * Array with values
     */
    T * values;

    /**
     * number of rows.
     */
    int nrow ;

    /**
     * number of cols.
     */
    int ncol;


  public:
    /**
     * Contructor.
     * \param values_p the data matrix
     * \param nrows_p the number of rows
     * \param ncols_p the number of cols.
     */
  matrice(T * values_p, int nrow_p, int ncol_p) : 
    values(values_p),
      nrow(nrow_p),
      ncol(ncol_p)
      {
    
      };
  

    /**
     * Accessor on data.
     */
    virtual T & operator[] (int index) {
      return values[index];
    };

  
    /**
     * get a row.
     * \return row i.
     */
    vecteur<T> getRow(int index) {
      if (index >= nrow) {
	Rf_error("matrice::getRow(): out of bound %d - %d", index, nrow);
      }
      vecteur<T> myRow (*this, index, nrow, ncol);
      return myRow;
    };

    /**
     * get a column.
     * \return col i.
     */
    vecteur<T> getCol(int index) {
      if (index >= ncol) {
	Rf_error("matrice::getCol(): out of bound %d - %d", index, ncol);
      }
      vecteur<T> myCol (*this, index*nrow, 1, nrow);
      return myCol;
    };

    /**
     * getSize.
     */
    int size() {
      return nrow * ncol;
    };
    
    /**
     * accessor.
     * \return number of rows.
     */
    int getNrow() {
      return nrow;
    }

    /**
     * accessor.
     * \return number of rows.
     */
    int getNcol() {
      return ncol;
    }


  };

 

};



#endif
