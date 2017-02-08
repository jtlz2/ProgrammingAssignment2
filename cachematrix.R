## cachematrix.R
## JZ @ 8 February 2017

## For a R matrix that is always invertible,
## compute the inverse,
## using the cached value if it exists.

## This is based on makeVector and cachemean by Roger Peng
## (https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping)

## Usage: run as e.g.
##
## source("cachematrix.R")
## A <- matrix(rnorm(4),2,2)
## a <- makeCacheMatrix(A)
## cacheSolve(a)
## cacheSolve(a)
## etc.

## Initialize the 'special' matrix based on A
## Set up inverse attribute, plus methods for
## getting and setting the matrix and its inverse
makeCacheMatrix <- function(A = matrix()) {
  I <- NULL
  set <- function(y) {
    A <<- y
    I <<- NULL
  }
  get <- function() A
  setinv <- function(solve) I <<- solve
  getinv <- function() I

  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## Calculate the matrix inverse
## using the cached value if it already exists (and say so)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of the special matrix 'x'
  I <- x$getinv()
  if(!is.null(I)) {
    message("getting cached inverse value")
    return(I)
  }
  a<-x$get()
  I <- solve(a)
  x$setinv(a)
  I
}

## --ENDS----