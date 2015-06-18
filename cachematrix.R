## This functions avoid the consumption of time calculation by checking if 
## a given matrix has been alread inversed. If then it put the inverse matrix already 
## calculated but cached in a variable. If no it will calculte the inverse

## return a list of function get/set the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## calculate the inverse of a mtrix. It first checks to see if the inverse has already 
## calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
