## The functions makeCacheFunction and cacheSolve work together so that the inverse of a matrix
## is only calculated once. The first time the inverse matrix is calculated, it is cached.
## After the first time, whenever the inverse matrix is required the cached value is returned.



## makeCacheFunction returns a list to set the value of the matrix, get the value of the matrix, 
## set the value of the inverse matrix and get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverted <- function(solve) i <<- solve
  getinverted <- function() i
  list(set = set, get = get,
       setinverted = setinverted,
       getinverted = getinverted)
  
}


## The cacheSolve function  checks if the inverse matrix has already been calculated.
##  If the inverse matrix is already calculated it returns the cached value.  Otherwise
## it calculates the inverse matrix and sets the value in the cache via the setInverted function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverted()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverted(i)
  i
  
}