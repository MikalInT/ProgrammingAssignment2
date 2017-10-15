## Programming Assignment 2  Make a cache matrix
## 

## This Function creates a cache matrix

########makeVectorEXAMPLE##############################
##makeVector <- function(x = numeric()) {
##         m <- NULL
##         set <- function(y) {
##                 x <<- y
##                 m <<- NULL
##         }
##         get <- function() x
##         setmean <- function(mean) m <<- mean
##         getmean <- function() m
##         list(set = set, get = get,
##              setmean = setmean,
##              getmean = getmean)
## }
########################################################
makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
         set <- function(y) {
                 x <<- y
                 inv <<- NULL
         }
         get <- function() x
         setinv <- function(inverse) inv <<- inverse
         getinv <- function() inv
         list(set = set, get = get,
              setinv = setinv,
              getinv = getinv)
}

################EndOfMakeCacheMatrix

## The following function computes the inverse of the special "matrix" object returned by makeCacheMatrix
## If the inverse has been claculated and the matix has not changed then cacheSolve will retreive the inverse from cache

#######cachemeanEXAMPLE#################################
##cachemean <- function(x, ...) {
##         m <- x$getmean()
##         if(!is.null(m)) {
##                 message("getting cached data")
##                 return(m)
##         }
##         data <- x$get()
##         m <- mean(data, ...)
##         x$setmean(m)
##         m
## }
########################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getmean()
         if(!is.null(inv)) {
                 message("getting cached data")
                 return(inv)
         }
         data <- x$get()
         inv <- mean(data, ...)
         x$setmean(inv)
         inv
}
