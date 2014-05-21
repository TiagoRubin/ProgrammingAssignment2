## makeCacheMatrix and cacheSolve
##
## makeCacheMatrix defines a list of functions 
##    allowing to define a "cacheMatrix" with 
##    cached inverse to speed up computations
##
## cacheSolve computes and caches the inverse 
##    of previously defined cacheMatrix
##
## written for the Coursera course on R programming

## makeCacheMatrix <- function(x = matrix(), ...)
##    creates the cacheMatrix list of functions needed to cache the inverse
##    it resets the "inverse" each time the cacheMatrix changes,
##    but keep it otherwise
makeCacheMatrix <- function(x = matrix(), ...) {
      inverse <- NULL
      
      ## set(y, ...) : set or replace the entire matrix. No subsetting.
      ##    if the input y is not a matrix, input is passed 
      ##    including optional input, to function matrix(y, ...)
      set <- function(y, ...) {
            if(is.matrix(y)) {
                  x <<- y
            } else {
                  x <<- matrix(y, ...)
            }
            inverse <<- NULL
      }
      
      ## subset(value, row position, column position) 
      ##    use to replace a subset of matrix components
      ##    many elements can be replaced at once, but (y, r, c) must respect the dim of x
      ##    example: x$subset(1:6, 1:3, 1:2)
      subset <- function(y, r, c) {
            x[r, c] <<- y
            inverse <<- NULL
      }
      get <- function() x
      setInverse <- function(inv) inverse <<- inv
      getInverse <- function() inverse
      
      list(set = set, get = get,
           subset = subset, 
           setInverse = setInverse,
           getInverse = getInverse)
}


## cacheSolve <- function(x, ...)
##    computes and caches the inverse of a cacheMatrix matrix
##    if the inverse is already cached, returns the cache
cacheSolve <- function(x, ...) {
      inverse <- x$getInverse()
      if(!is.null(inverse)) {
            message("getting cached inverse")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setInverse(inverse)
      inverse
}
