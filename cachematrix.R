## These functions provide a way to cache the computed inversion of
## a matrix and thereby save computation time if the inversion for
## the same matrix in needed again. The used matrix must be invertible.



## Creates a list of functions for the provided matrix which make the
## caching of the inversion of the matrix possible.
## In an object oriented view, the matrix is wrapped in a new obejct
## with a property for the matrix and the cached inversion.
makeCacheMatrix <- function(x = matrix()) {
        ## Initilize the environement for the returned functions.
        currentMatrix <- x
        cachedInversion <- NULL
        ## Function to access the provided value
        getMatrix <- function(){
                currentMatrix
        }
        ## This function sets the parameter as the new value for the matrix. This also invalidates the cached
        ## value for the inversion.
        setMatrix <- function(newMatrix){
                currentMatrix <<- newMatrix
                cachedInversion <<- NULL
        }
        getCachedInversion <- function(){
                cachedInversion
        }
        setCachedInversion <- function(inversion){
                cachedInversion <<- inversion
        }
        list(getMatrix=getMatrix,setMatrix=setMatrix,getCachedInversion=getCachedInversion,setCachedInversion=setCachedInversion)
}


## Returns the cached in version of the provided cacheMatrix if the cached
## inversion is present. Otherwise it computes the the inversion of the
## current matrix by calling the solve function, puts the result in the
## cache of the cacheMatrix and returns the result.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversion <- x$getCachedInversion()
        if(is.null(inversion)){
                currentMatrix <- x$getMatrix()
                inversion <- solve(currentMatrix,...)
                x$setCachedInversion(inversion)
        }else{
                message("Returning cached data")
        }
        inversion
}
