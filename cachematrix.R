## Put comments here that give an overall description of what your
## functions do
## This file contains two functions - makeCacheMatrix & cacheSolve
## that can cache the inverse of a square matrix.
## Individual descriptions of the functions are given below


## Write a short comment describing this function
## Given an invertible square matrix as an input, this function creates a special matrix objet
## that can cache the input's inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(matrix)
  {
    x <<- matrix
    inv <<- NULL
  }
  
  get <- function()
  {
    return(x)
  }
  
  setInverse <- function(inverse)
  {
    inv <<- inverse
  }
  
  getInverse <- function()
  {
    return(inv)
  }
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the matrix that is returned by the makeCacheMatrix
## Function. For an unchanged matrix, it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getInverse()
  ## Found cached inverse - return it
  if(!is.null(inv_matrix))
  {
    return(inv_matrix)
  }
  else
  {
    ## not found in cache - calculate and set inverse
    matrix <- x$get()
    inverse <- solve(matrix,...)
    x$setInverse(inverse)
    return(inverse)
  }
}
