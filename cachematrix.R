# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix.

## makeCacheMatrix: This function creates a special "matrix" object that
## can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ## Set the value of the input matrix.
  set <- function(y) {
    x <<- y
  inverse <<- NULL
  }
  ## Get the value of the input matrix.
  get <- function() x
  
  ## Set the value of the inverse matrix.
  setinverse <- function(solve) inverse <<- solve
  
  ## Get the value of the matrix inverse.
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}


## Test Run
amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
> amatrix$get()         # Returns original matrix
[,1] [,2]
[1,]    1    3
[2,]    2    4
> cacheSolve(amatrix)   # Computes, caches, and returns matrix inverse
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> amatrix$getinverse()  # Returns matrix inverse
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed 
getting cached data
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
> cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
[,1] [,2]
[1,] -0.13333333  0.2
[2,]  0.01010101  0.0
> amatrix$get()         # Returns matrix
[,1] [,2]
[1,]    0   99
[2,]    5   66
> amatrix$getinverse()  # Returns matrix inverse
[,1] [,2]
[1,] -0.13333333  0.2
[2,]  0.01010101  0.0