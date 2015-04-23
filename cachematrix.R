## The two functions work together to 1) store and retrieve values for a 
## matrix and its inverse, then, 2) decide between retrieving a stored inverse
## value if it has been set and solving for the inverse if it has not been set.


## The first function, makeCacheMatrix, is a list of functions 
## that assign and store values for the matrix, x, and its inverse, s.
## The function also defaults the value of s to NULL

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The second function, cacheSolve, calls s using getsolve.
## Then, if s is not NULL, it returns a message "getting cached data"
## and returns the stored value in getsolve. Otherwise, it solves for the 
## inverse of x, returns that value, and stores the new value of s in setsolve.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}