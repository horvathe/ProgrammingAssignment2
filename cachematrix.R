# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse.
# 1. set the value of the matrix set()
# 2. get the value of the matrix get()
# 3. set the value of inverse of the matrix setmatrix()
# 4. get the value of inverse of the matrix getmatrix()

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function()
            x
      setmatrix <- function(solve)
            m <<- solve
      getmatrix <- function()
            m
      list(
            set = set, get = get,
            setmatrix = setmatrix,
            getmatrix = getmatrix
      )
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#if the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
      z <- x$getmatrix()
      if (!is.null(z) ) {
            # Check if invert matrix was not changed since the last solve,
            # you can change the cached invert matrix with x$setmatrix(), 
            # this can invalidate the cached data 
            mat <- diag(1,nrow(z) ,ncol(z)) # Create identity matrix 
            if (!isTRUE(all.equal(z %*% x$get(),mat))){
                  message("invert matrix:  cache invalidated, invert matrix recalculated")
                  matrix <- x$get()
                  z <- solve(matrix, ...)
                  x$setmatrix(z)
                  return(z)
            }
            message("invert matrix : getting cached data")
            return(z)
      }
      # First call since matrix setup/ change, invert matrix has to be calculated
      matrix <- x$get()
      z <- solve(matrix, ...)
      x$setmatrix(z)
      message("invert matrix : invert matrix calculated")
      z
}