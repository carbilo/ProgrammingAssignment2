## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This R function is able to cache potentially time-consuming computations


## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

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
## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated,
## then the cachesolve should retrieve the inverse from the cache.

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
## > makeCacheMatrix <- function(x = matrix()) {
##   +   cache_inverse <- NULL
##   +   set <- function(y) {
##     +       x <<- y
##     +       cache_inverse <<- NULL
##     +     }
##   +     get <- function() x
##   +     set_inv <- function(inv) cache_inverse <<- inv
##   +     get_inv <- function() cache_inverse
##   +     list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
##   + }
## > cacheSolve <- function(x, ...) {
##   +   ch_sol <- x$get_inv()
##   +   if(!is.null(ch_sol)) {
##     +     message("getting cached data")
##     +     return(ch_sol)
##     +   }
##   +   data <- x$get()
##   +   ch_sol <- solve(data)
##   +   x$set_inv(ch_sol)
##   +   ch_sol
##   + }
## > x <- rbind(c(-0.845, 84), c(-27, 4/33))
## > a = makeCacheMatrix(x)
## > a$get()
## [,1]       [,2]
## [1,]  -0.845 84.0000000
## [2,] -27.000  0.1212121
## > cacheSolve(a)
## [,1]          [,2]
## [1,] 5.344691e-05 -0.0370387097
## [2,] 1.190530e-02 -0.0003725918
## > cacheSolve(a)
## getting cached data
## [,1]          [,2]
## [1,] 5.344691e-05 -0.0370387097
## [2,] 1.190530e-02 -0.0003725918
## > 
