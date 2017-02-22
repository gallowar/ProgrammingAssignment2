## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL               # Initially set m to null   
  
  set <- function(y) {    # Set function defined
    x <<- y
    m <<- NULL
  }

  get <- function() x     # Get Function defined 

  setinv <- function(inv) m <<- solve(x)   # setinv function defined
  
  getinv <- function() m  # getinv function defined
  
  list(set = set,         # gives the name 'set' to the set() function defined above   
       get = get,         # gives the name 'get' to the get() function defined above
       setinv = setinv,   # gives the name 'setminv' to the setinv() function defined above
       getinv = getinv)   # gives the name 'getinv' to the getinv() function defined above
}


## Write a short comment describing this function
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()           # Set value
  
    if(!is.null(m)) {         # If m is not null then cache has been created.  
        message("getting cached data")   # Message to show value is from cache
        return(m)               # Return cached value
    }

  data <- x$get()           #Since got here no cache exits
  m <- solve(data, ...)     # Calculate inverse on data
  x$setinv(m)               # Cache inverse
  m                         # Return inverse
}
