## This function creates a special "matrix" object 
## that can cache its inverse. This object contains 
## several functions that are used to set(cache) and 
## get(return) the matrix data and inverse matrix data.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

  # Function 1: Caches the matrix data for this object in 
  # the variable (x). 
  set <- function(y) {
    x <<- y
    # Setting the inverse matrix value (m) to NULL when new matrix
    # data is set means this will need to be recalculated later.
    m <<- NULL
  }

  # Function 2: Get the current cached matrix data (x) for the object.
  get <- function() x

  # Function 3: Caches the inverse matrix value (inv), calculated by
  # cacheSolve(), in the variable (m).
  setinverse <- function(inv) m <<- inv

  # Function 4: Gets the current cached inverse matrix value (m) stored
  # by setinverse()
  getinverse <- function() m
  list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## This function either calculates the inverse of the 
## "matrix" object created by makeCacheMatrix() by
## using the solve() function or, if this has been 
## previously calculated for the input matrix, retrieves 
## the inverse of the matrix object from the cache.
cacheSolve <- function(x, ...) {
  # Gets the current value of the inverted matrix data.
  m <- x$getinverse()
  # If there is a current inverse value, not NULL,
  # return this value and exit function.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Otherwise, get current matrix data.
  data <- x$get()
  # Use solve() to calculate the inverse of the matrix.
  ## The inverse of matrix 
  ## [a, b]
  ## [c, d]
  ## is
  ## 1/((a*d)-(b*c)) *
  ## [d, -b]
  ## [-c, a]
  m <- solve(data, ...)
  # Cache the inverse value for the next call to 
  # getinverse
  x$setinverse(m)
  # Return inverse value and exit function.
  return(m)
}
