## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  xinvt <- NULL                             # Array to store inverse result
  
  set <- function(y) {
    x <<- y
    xinv <<- NULL                           # Set initial value
  }
  
  get <- function() x                       # return the input matrix
  setInv <- function(inv) xinvt <<- inv     # set the inversed matrix
  getInv <- function() xinvt                # return the inversed matrix
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getInv() 					# retrive result from function x
      							# Only run functions as input from Function X is not NULL.
      if(!is.null(m)) { 				
	  message("Retrieving cached data")
	  return(m)						# return inversion calculation
      }
      data <- x$get() 					# if not, we do x$get to get the matrix object
      m <- solve(data)					# we solve it
      x$setInv(m) 					# we then set it to the object
      m 							# return the solved result
}
