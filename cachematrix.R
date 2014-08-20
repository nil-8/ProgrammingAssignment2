## This .R file consists of two functions:
## 1. makeCacheMatrix constructs an abstract matrix object that 
##    allows to cache the associated inverse matrix.
## 2. cacheSolve returns the inverse of the matrix represented by
##    the object created with makeCacheMatrix, and if was not
##    available yet, caches its answer for future use.

## makeCacheMatrix takes a single optional argument x, with as 
## default value the empty matrix. It initializes an 'internal 
## variable' x, and returns a list of functions 
## (set, get, setInverse, getInverse) that,respectively, 
## set the value of x, get the value of x, set a cached value 
## of the inverse of x, and retrieve cached value of the inverse 
## of x, if available.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve takes as argument a matrix object constructed by
## makeCacheMatrix, and optional arguments. If a cached matrix 
## inverse is present, it is returned along with a message. If
## a cached matrix inverse is not present, it is computed by the
## solve command along with the optional arguments, and cached 
## within the matrix object.


cacheSolve <- function(x, ...) {
    
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i    
}
