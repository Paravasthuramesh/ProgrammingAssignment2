## cachematrix.R
## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly

## functions do

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

## Usage:
##  M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##  cacheMatrix <- makeCacheMatrix(M)
##  cacheSolve(cacheMatrix)
##  Output
##        [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5




## `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
  
  ## Initialize the inverse property
  cachedInverse <- NULL
  
  ## Method to set the matrix
  set <- function(matrix){
    m <<- matrix
    cachedInverse <<- NULL
  }
 
  
  ## Method the get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  setInverse <- function(inverse) cachedInverse <<- inverse
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    cachedInverse <<- inverse
  }

  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    cachedInverse
  }
  
  ## Return a list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  ## Just return the inverse if its already set
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  inverse <- solve(data, ...)
  
  
  ## Set (cache)  the inverse to the object
  x$setInverse(inverse)
  
  ## Return the matrix
  inverse  
}
