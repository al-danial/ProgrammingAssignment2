
## Creates a matrix object to cache
makeCacheMatrix <- function( m = matrix() ) {
  
  mt <- NULL
  
  set <- function( matrix ) {
    m <<- matrix
    mt <<- NULL
  }
  
  get <- function() {
    m
  }
  
  setmt <- function(inverse) {
    mt <<- inverse
  }
  
  getmt <- function() {
    mt
  }
  
  list(set = set, get = get,
       setmt = setmt,
       getmt = getmt)
}


## Checks for a cached version of inversed matrix, or else creates new
cacheSolve <- function(x, ...) {
  
  m <- x$getmt()
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data) %*% data
  
  x$setmt(m)
  
  m
}