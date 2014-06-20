## This function creates a special "matrix" object 
## that can cache its inverse. This object contains 
## several functions that are used to set(cache) and 
## get(return) the matrix data and inverse matrix data.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

  # Function 1: Stores the matrix data for this object in 
  # the variable (x). 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  # Function 2: Get the current matrix data (x) for the object.
  get <- function() x

  # Function 3: Stores inverse matrix value (inv) for this 
  # object in the cache (m).
  setinverse <- function(inv) m <<- inv

  # Function 4: Gets the cached inverse matrix value (m) stored
  # by setinverse()
  getinverse <- function() m
  list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## This function either calculates the inverse of the 
## "matrix" object created by makeCacheMatrix() or,
## if this has been previously calculated and the 
## matrix hasn't been altered, retrieves the inverse
## of the matrix object from the cache.
cacheSolve <- function(x, ...) {
  # Get the inverse of the current matrix data.
  m <- x$getinverse()
  # If there is a current inverse value, not NULL,
  # return this value and exit function.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Otherwise, get current matrix data.
  data <- x$get()
  # Use solve to calculate the inverse of the matrix.
  ## The inverse of matrix 
  ## [a, b]
  ## [c, d]
  ## is
  ## 1/((a*d)-(b*c)) *
  ## [d, -b]
  ## [-c, a]
  m <- solve(data, ...)
  # Set the inverse value for the next call to 
  # getinverse
  x$setinverse(m)
  # Return inverse value and exit function.
  return(m)
}
