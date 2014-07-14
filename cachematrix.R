## This file contains R functions which implement Cache Matrix
## as per R Programming course Assignment 2 

## example usage:
## md <- c(4,3,3,2)
## mm <- matrix(data = md, ncol = 2)
## cachematrix <- makeCacheMatrix(mm)
## cacheSolve(cachematrix)

## makeCacheMatrix creates a list which contains functions
## used to access matrix and it's inverse. It also stores
## matrix values and caches inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve is a function which returns an inverse of CacheMatrix
## it returns cached values if they are available in CacheMatrix
## object passed as a parameter or calculates the inverse 
## of the passed object and stores in it 

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
