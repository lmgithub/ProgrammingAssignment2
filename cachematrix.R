## Week 2 Assignment: Caching the Inverse of a Matrix
## Here are 2 functions which creates matrix, 
## computes the inverse of this matrix and caches computed data for future use.
## On the bottom there is a code which checks these functions

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() ## getting cached data
  if(!is.null(i)) {
    message("getting cached data")
    return(i) ## returning cached data
  } else {
    message("computing data")
  }
  ## calculating and caching data
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


## This part checks the functions
## Creating matrix
t <- makeCacheMatrix(matrix(c(2,0,0,0,1,0,2,2,2), 3, 3))
## First run - computing and caching inverse matrix (message "computing data")
cacheSolve(t)
## Second run - getting cached inverse matrix (message "getting cached data")
cacheSolve(t)
