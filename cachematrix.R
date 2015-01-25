## Using the idea behind makeVector, creating a makeCacheMatrix aimed 
## at the idea of setting inverse of a matrix.
## This creates a special matrix that will 
## 1) set and get the value of the matrix,
## 2) and set and get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  set <- function(y) {
      x <<- y
      inv_x <<- NULL
    }
  get <- function() x
  setinverse <- function(inv) sinv <<- inv
  getinverse <- function() sinv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

    i <- x$getinverse() 
  ##get the inversed matrix from object x
    
    if(!is.null(i)) { 
  ##if inverse result is there    
      message("getting cached data")
      return(i)
  ##shows pre-calculated/cached inversion
    
    data <- x$get()
  ##if no data available, will get matrix object using x$get
    i <- solve(data, ...)
  ##solve the data set
    x$setinverse(i)
  ## set it to the object
    i 
  ##return solved object that is the inverse of 'x'
  }
        
}
