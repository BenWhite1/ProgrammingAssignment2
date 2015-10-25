## Matrix Inversion

# Computing the inverse of a matrix can be a computationally
# expensive operation, so the following code is useful for 
# loops and other repeating structures. The following functions 
# exploit R's lexical scoping rules to either calculate the 
# inverse of a matrix or, if cached, to load it from memory.
# To use it, supply makeCacheMatrix with an invertible matrix
# and store the result in a separate variable. Then, use
# cacheSolve on this. For example:
# > x = matrix(c(1,2,3,4),nrow = 2, ncol = 2)
# > x_2 = makeCacheMatrix(x)
# > cacheMatrix(x_2)
# This should work for any size of square matrix. Happy 
# computing!

# makeCacheMatrix sets up a list to:
# 1) Set the value of the matrix
# 2) Get the value of the matrix
# 3) Set the value of the matrix' inverse
# 4) Get the value of the matrix' inverse

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


# cacheSolve returns the inverse of the matrix supplied to it.
# If the inverse has already been computed, the function takes
# the result from the cache, else it computes it and stores
# the result in the cache using 'setinverse'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
