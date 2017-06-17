## Since matrix is a costly computation, the following functions provide a way
## to cache the inverse of a matrix. Once computed and cached, this can be read 
## multiple times from the cache, without the need for any further computation.


## This function creates a cached matrix from the input argument,
## and returns a list of getter and setter funcions
## for the matrix and an inverse of the matrix as well.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Cache the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Return cached matrix
  get <- function() x
  
  ## Cache the inverse of the original matrix
  setinverse <- function(i) inv <<- i
  
  ## Reurn the cached inverse
  getinverse <- function() inv
  
  ## Return list of all functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function assumes the input argument is an inversible matrix 
## that has been cached. If an inverse of the matrix is available in cache,
## that is returned, else the inverse is computed using the solve() function,
## cached, and then returned. Other than the matrix argument, other arguments 
## accepted by the solve() function can be passed as well.
cacheSolve <- function(myMatrix, ...) {
  ## Retrieve the inverse matrix from cache
  cachedInverse <- myMatrix$getinverse()
  
  ## If inverse exists in cache, that is returned without any further computation
  if (!is.null(cachedInverse)) {
    message("getting cached data")
    return(cachedInverse)
  }
  
  ## If no cached inverse exists, compute one using solve(), cache it, and return the same.
  cachedMatrix <- myMatrix$get()
  myInverse <- solve(cachedMatrix, ...)
  myMatrix$setinverse(myInverse)
  return(myInverse)
}

