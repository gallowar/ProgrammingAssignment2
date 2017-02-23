## These functions implement lexical scoping to determine the
## inverse of a matrix that is passed as an argument.  There is
## no validation checking so as per the assignment we are passing
## a mtrix that has an inverse.  The inverse is computed only once
## when the set function is called.  When the get function is called
## the variable set is checked, if it is null the inverse is calculated
## and stored.  If the variable is not null then the associated value
## is returned from the previous execution.  The inverse is only
## calcualted once and then referenced after that.

## This routine accepts a matrix and defines the set
## and get functions used for lexical scoping in 
## finding the inverse of the matrix.
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


## This function checks the variable defined in the set function.   
## If that variable is null the inverse is calculated and the variable is set.
## if the variable is null it returns the value from the inital set function call that 
## that calculated the inverse and stored the result.
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
