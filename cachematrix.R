## Two functions makeCacheMatrix and cacheSolve work together to create a square matrix
## and inverse of the matrix. The inverse of the matrix is available in the cache
## the cached matrix is available cacheSolve returns it without recalculating
##

## makeCacheMatrix function creates a matrix

makeCacheMatrix <- function(x = matrix()) {
  cachedMatrix<-NULL
  set<-function(y){
  x<<-y
  cachedMatrix<<-NULL
}
get<-function() x
setmatrix<-function(inverse) cachedMatrix<<- inverse
getinverse<-function() cachedMatrix
list(set=set, get=get, setmatrix=setmatrix, getinverse=getinverse)
}


## cacheSolve function creates the invere of the created matrix by the makeCacheMatrix function.
## If the inverse is available in cache, cacheSolve retrieves and returns it,with re-calculating..

cacheSolve <- function(x=matrix(), ...) {
    cachedMatrix<-x$getinverse()
    if(!is.null(cachedMatrix)){
      message("getting cached data")
      return(cachedMatrix)
    }
    matrix <- x$get() 
 ## Some error handling
 tryCatch( {
                # set and return inverse of matrix
                cachedMatrix <- solve(matrix, ...)
        },
        error = function(e) {
                message("Error:")
                message(e)
                return(NA)
        },
        warning = function(w) {
                message("Warning:")
                message(w)
                return(NA)
        },
        finally = {
               x$setmatrix(cachedMatrix)
        } )
        return(cachedMatrix)
} 


##    Testing the functions
##    load the R program
## > source("cachematrix.R")
##    create the matrix function
## myMatrix <- makeCacheMatrix()
##    initialize a mttrix
## myMatrix$set(matrix(1:4, 2, 2))
##    returns the inverted matrix
## cacheSolve(myMatrix)
##    subsequent run uses the cached matrix 
## cacheSolve(myMatrix)
