## Functions to provide a caching mechanism for the
## calculation of the inverse of a matrix

## A function to store (in a list) a source matrix 
## and the computed inverse
makeCacheMatrix <- function(x = matrix()) {

  # reset the inverse matrix
  m <- NULL
  
  # set the source matrix and reset the inverse (search envs) 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get the source matrix
  get <- function() x

  # set the inverse
  setInverse <- function(inverse) m <<- inverse

  # get the inverse
  getInverse <- function() m

  # store all the internal functions in a list
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##  This function accepts the "matrix (a list storing the source matrix and the associated functions)
cacheSolve <- function(x) {
  
  # return a matrix that is the inverse of 'x'
  # (if it was already calculated)
  m <- x$getInverse()
  if(!is.null(m)) {
    message(paste("getting cached inverse of matrix"))
    return(m)
  }

  # get the source matrix (via makeCacheMatrix)
  data <- x$get()
  
  # calculate and set
  m <- solve(data)
  x$setInverse(m)

  message(paste("calculated inverse of matrix"))
  
  # return the calculated matrix
  m
}
