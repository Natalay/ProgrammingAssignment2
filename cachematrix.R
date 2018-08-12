##Matrix inversion is usually a costly computation and there are 
#fubctuonmay to caching the inverse of a matrix 

## Function return list of function set,get, setsolve, getsolve
##they set/get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- 
    function(y) {
      x <<- y
      m <<- NULL
    }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##this funtion calculate a inverse matrix, but firstly see if there is alreary chache 
##then it take cache (if not, calculate them)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}