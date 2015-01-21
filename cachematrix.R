## This file includes the functions makeCacheMatrix and cacheSolve, created as
## part of the Programming Assignment 2 for the R Programming course.

## makeCacheMatrix:
## Input:
##  - None
## Output:
##  - A list of functions to:
##      - $get: the matrix stored.
##      - $set: the matrix stored, given a matrix. Raises a warning if it's not squared.
##      - $get.inverse: of the matrix currently stored.
##      - $set.inverse: of the matrix currently stored, given the inversion of a matrix.

makeCacheMatrix <- function(x.matrix = matrix()) {
    inverse <- NULL
    set <- function(y.matrix) { 
            x.matrix <<- y.matrix
            inverse <<- NULL
            if (ncol(x.matrix) != nrow(x.matrix)) {
                warning("The stored matrix isn't squared, so it won't be invertible.")
            }
    }
    get <- function() x.matrix
    set.inverse <- function(inverted.matrix) inverse <<- inverted.matrix
    get.inverse <- function() inverse
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## cacheSolve:
## Input:
##  - A makeCacheMatrix list.
## Output:
##  - The inversion of the matrix stored in makeCacheMatrix.
## 
## Raises an error if the matrix stored in makeCacheMatrix isn't squared.
cacheSolve <- function(x, ...) {
    x.matrix <- x$get()
    if (nrow(x.matrix) != ncol(x.matrix)) {
        stop("The matrix currently stored it's not squared. Please, set a squared matrix
            using the $set() function of the makeCacheMatrix list.")
    }
    inverted.matrix <- x$get.inverse()
    if (!is.null(inverted.matrix)) {
        message("Invertion has already been computed. Getting cached invertion.")
        return(inverted.matrix)
    }
    inverted.matrix <- solve(x.matrix)
    x$set.inverse(inverted.matrix)
    inverted.matrix
}
