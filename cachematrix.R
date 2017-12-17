## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## This function is able to cache potentially time-consuming computations


## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of matrix
## 4. get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  cache_inverse <- NULL
  set <- function(y) {
      x <<- y
      cache_inverse <<- NULL
    }
    get <- function() x
    set_inv <- function(inv) cache_inverse <<- inv
    get_inv <- function() cache_inverse
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ch_sol <- x$get_inv()
  if(!is.null(ch_sol)) {
    message("getting cached data")
    return(ch_sol)
  }
  data <- x$get()
  ch_sol <- solve(data)
  x$set_inv(ch_sol)
  ch_sol
}

## Return a matrix that is the inverse of 'x'
## > x = rbind(c(3/4, -32), c(-0.857,31))
## > a = makeCacheMatrix(x)
## > a$get()
##        [,1] [,2]
## [1,]  0.750  -32
## [2,] -0.857   31
## > cacheSolve(a)
##            [,1]       [,2]
## [1,] -7.4269286 -7.6665069
## [2,] -0.2053186 -0.1796838
## > cacheSolve(a)
## getting cached data
##            [,1]       [,2]
## [1,] -7.4269286 -7.6665069
## [2,] -0.2053186 -0.1796838
## > 
