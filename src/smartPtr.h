#ifndef AMAP_MEMORY
#define AMAP_MEMORY 1

#include <new>
#include "R.h"

/**
 * This class is like std:auto_ptr - but with a new xxx[size] and delete [] ...
 */
template<class T> class SmartPtr {

 private:
  T* data;

 public:
  /**
   * Constructor. Create an array T*.
   *
   * \param size size of array.
   */
  SmartPtr(int size) 
    : data(0) {
    reset(size);
 }

  ~SmartPtr() {
    clear();
  }

  /** */
  void reset(int size) {
    clear();
    if (size > 0) {
      //data = new T[size];
      data = (T*) calloc(size, sizeof(T));
      if ( data == 0) {
	int sizeInMo = size * sizeof(T) / 1024 / 1024;
	Rf_error("AMAP: cannot allocate %d Mo", sizeInMo);
      }
    } else {
      data = 0;
    } 
  }

  /**
   * Clear data (free).
   */
  void clear() {
   if (data != 0 )
      {
	//delete [] data;
	free(data);
      }
    data = 0;
  }

  /** Accesssor.
   */
  T & operator[](int i) {
    return data[i];
  }

  /**
   * Get data.
   */
  T * get(){
    return data;
  }

 private:
  /**
   * This is to avoid a copy.
   */
  SmartPtr(const SmartPtr&){};
};


#endif
