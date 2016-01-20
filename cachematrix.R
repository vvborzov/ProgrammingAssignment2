## Functions in this file provide caching functionality for inverting square 
## matrices. At first special object should be created by makeCacheMatrix 
## function. Thereafter cached version of solve() function may be used on 
## this object

## makeCacheMatrix
## Description: creates a special object, which contains square matrix and 
## cached version of matrix inversion
## to provide data integrity, access to underlying data of the returned object
## may be granted only via returned object interface

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    set.inv <- function(inv) invx <<- inv
    get.inv <- function() invx
    list(
        set = set,
        get = get,
        set.inv = set.inv,
        get.inv = get.inv
    )
}


## cacheSolve
## Description: returns inversion of matrix which is contained in object x,
## which should be created by function makeCacheMatrix

cacheSolve <- function(x, ...) {
        invx <- x$get.inv()
        if(!is.null(invx)){
            return(invx)
        }
        mat <- x$get()
        mat.inv <- solve(mat, ...) 
        x$set.inv(mat.inv)
        mat.inv
}
