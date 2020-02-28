## This is a pair of functions that caches the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(specialMat = matrix()){
  cache <- NULL
  
  #set value of matrix
  setValue <- function(y) {
    specialMat <<- y
    cache <<- NULL
  }
  
  #get value of matrix
  getValue <- function() specialMat
  
  #set value of inverse
  setInverse <- function(inverseMat) cache <<- inverseMat
  
  #get value of inverse
  getInverse <- function() cache
  
  #return list of functions
  list(setValue = setValue,
       getValue = getValue,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(specialMat){
  
  cache <- specialMat$getInverse()
  
  # if matrix already cached, return cache
  if (!is.null(cache)) {
    message("cacheSolve: retrieving cached special matrix.")
    return(cache)
  }
  # else, compute inverse and set cache
  else{
    message("cacheSolve: calculating inverse and caching special matrix.")
    data <- specialMat$getValue()
    cache <- solve(data)
    specialMat$setInverse(cache)
    return(cache)
  }
}
