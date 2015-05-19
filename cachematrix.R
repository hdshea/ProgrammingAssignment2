## These functions support an environment for efficiently calculating
## matrix inversions 

## makeCacheMatrix creates and returns an object which contains
## a reference environment and methods for efficiently handling
## matrix inversions.  It takes as its only argument 'x' - the 
## matrix to be inverted.

makeCacheMatrix <- function( x = matrix() )
{
  ## this is the cache storage for the already inverted matrix
  ## the cache will be NULL when no cached inversion is available
  matrix.inverse.cache <- NULL
    
  ## the 'set' method for this object clears the cache and resets 
  ## the matrix to be inverted but only if the new matrix to be
  ## inverted ('y') is not identical to the previous matrix ('x')
  set <- function( y )
  {
    if( !identical( x, y ) )
    {
      x <<- y
      matrix.inverse.cache <<- NULL
    }
  }
    
  ## the 'get' method for ths object returns the matrix to be inverted
  get <- function() { x }

  ## the 'set.inverse' method for this object caches the inverted matrix
  setinverse <- function( inverse ) { matrix.inverse.cache <<- inverse }

  ## the 'get.inverse' method for this object returns
  ## the inverted matrix from the cache
  getinverse <- function() { matrix.inverse.cache  }
    
  ## return the CacheMatrix object - which is implemented as a list of
  ## method functions
  list(
    set = set, 
    get = get, 
    setinverse = setinverse, 
    getinverse = getinverse
  )
}

## cacheSolve uses the reference object 'x' to implement an efficient
## matrix inversions:  if the matrix has previously been inverted and 
## is saved in the cache that value is used (and the case is reported),
## otherwise the original matrix is retrieved, inverted and saved in
## the cache for future reference

cacheSolve <- function( x, ... )
{
  ## Return a matrix that is the inverse of the reference matrix
  ## from the object 'x' - the getinverse method returns NULL if
  ## the inverted matrix has not been cached previously or the 
  ## reference matrix has been changed

  inv <- x$getinverse()
  
  if( !is.null( inv ) )
  {
    message("using cached data")
    return( inv )
  }
  
  ##  if the getinverse method does return NULL:
  ## recall the reference matrix using the get method
  data <- x$get()

  ## call solve to get the inverted matrix
  inv <- solve( data, ... )

  ## use the setinverse method to cache the inverted matrix
  x$setinverse(inv)

  ## return the inverted matrix
  inv
}
