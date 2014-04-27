## This R code is for 'Programming Assignment 2'. 
## There are two functions defined here (1) makeCacheMatrix and (2) cacheSolve used to 
## get inverse of a matrix from cache or by calculating if not avaliable. 
## Please read function comments for details.


## This function create Cache Matrix object. This function took one argument
## as a matrix and return a list of function (set, get, setInverse and getInverse)
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inv) {
    inverse <<- inv
  }
  getInverse <- function(){
    inverse
  }
  list(set = set,get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function verify if the cached inverse matrix for a given object created by 
## makeCacheMatrix exists or not. If not exist then it will calculate the inverse 
## and cache it else return cached value.
## Parameter Matrix and return inverse of Matrix
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  ## Verifiying if cache return any value
  if(!is.null(inverse)){
    message("Found cached inverse returing cached value.")
    return (inverse)
  }
  ## Cache is empty calculate the inverse now.
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setInverse(inverse)
  inverse
}

