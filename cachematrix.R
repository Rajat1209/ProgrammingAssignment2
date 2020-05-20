makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL #Initializing cache Matrix to NULL
  
  #setMatrix function
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #getMatrix function
  getMatrix <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  #Name of function that  will be available
   list(setmatrix=setMatrix, getmatrix=getMatrix, setinverse=setinverse, getinverse=getinverse)
}



#Function to calculate inverse of cache matrix
cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  
  # If inverse if already calculated return data
  if(!is.null(m)) {
    
    message("Cache data loading.")
    
    return(m)
  }
  
  # If not inversed, get the inverse
  d <- x$getmatrix()
  
  # Inverting the matrix via Solve
  m <- solve(d)
  
  # Making call to setinverse
  x$setinverse(m)
  
  # returning inverse of cache matrix
  m
}

