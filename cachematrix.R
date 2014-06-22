## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##
## This function will create a "matrix" like object, which can
## cache it's own inverse
##
makeCacheMatrix <- function(x = matrix()) {
  ## create the set function
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  ## create the get function
  get <- function() x
  
  ## set the inverse matrix
  setinverse <- function(inverse) m_inv <<- inverse
  ## get the inverse matrix
  getinverse <- function() m_inv
  
  ## create the list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

##
## This function computes the inverse of the special "matrix"
## like object, returned by 'makeCacheMatrix' above. If the inverse
## has already been calculated (and the Matrix and the matrix has
## not changed), then 'cacheSolve' should retrieve the inverse from
## the cache.
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   
   ## get the inverse matrix       
   m_inv <- x$getinverse()
   
   ## test if the inverse matrix is valid   
   if(!is.null(m_inv)) {
     message("getting cached data")
     return(m_inv)
   }
   ## if it is not, get the inverse matrix   
   data <- x$get()
   m_inv <- solve(data, ...)
   ## set the inverse matrix 
   x$setinverse(m_inv)
   m_inv   

}
