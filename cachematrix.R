## Put comments here that give an overall description of what your
## functions do
##
## Functions create a user defined wrapper for system matrix type.
## Encapsulates the ability to store prior inverse computations for speed



## Write a short comment describing this function
## Create a list of member functions for a user defined vector wrapper
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function(inverse) m
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse= getInverse)
}


## Write a short comment describing this function
## Returns a matrix inverse or the cached value of the matrix inverse
cacheSolve <- function(x) {
  
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Return a matrix that is the inverse of 'x'
  cacheInv <- function(x) {
    return(solve(x))
  }
  
  print("Computing and storing matrix")
  inv <- cacheInv(x$get())
  x$setInverse(inv)
    
  inv
}



##TESTING CODE
##TEST MATRIX
mtrxExample<- matrix(c(.5,-.5,.5,.5),nrow=2,byrow=TRUE)

#SET MATRIX
testMatrix <- makeCacheMatrix()
testMatrix$set(mtrxExample)

#GET MATRIX - Should be example mtrxExample matrix
if (identical(testMatrix$get(), mtrxExample))
    print("Matrix Set works properly")

#GET INVERSE - Not yet set so this should compute it and store
#Inverse should be (.5,-.5,.5,.5)

cacheSolve(testMatrix)
if (identical(testMatrix$getInverse(),matrix(c(.5,-.5,.5,.5), nrow=2,byrow=TRUE)))
  print("Matrix store and Inversion calculation work properly")

#SHOULD BE ABLE TO RESET AND RECOMPUTE/STORE AND RETURN ORIGINAL MATRIX
mtrxExample<- matrix(c(.5,-.5,.5,.5),nrow=2,byrow=TRUE)
testMatrix$set(mtrxExample)
cacheSolve(testMatrix)
if (identical(testMatrix$get(), mtrxExample))
  print("Matrix reuse works properly")






  