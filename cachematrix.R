##find inverse of a square matrix using cache matrix
##makecachematrix is a function with argument as matrix

makeCacheMatrix <- function(x = matrix()) {
  
  
  inv <- NULL
  
  set <- function(y) {
    
    x <<- y
    
    inv <<- NULL
    
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}













##return the matrix that is inverse of x



cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    
    message("getting cached data.")
    
    return(inv)
    
  }
  
  data <- x$get()
  
  inv <- solve(data)
  
  x$setinverse(inv)
  
  inv
  
}
