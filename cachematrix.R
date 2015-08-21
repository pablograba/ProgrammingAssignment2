

## makeCacheMatrix() provides set and get methods for the special cache matrix object (makeCacheMatrix)
makeCacheMatrix <- function(x = matrix()) {
  #cached matrix (the inverse)
    cm <- NULL
    set <- function(y = matrix()) {
      x <<- y
      cm <<- NULL
    }
    get <- function() x
    getCache <- function() cm
    setCache <- function(x) cm <<- x
    list(set = set, get = get, setCache = setCache, getCache = getCache)

}


## cacheSolve() gets a makeCacheMatrix as an argument and returns the inverse matrix (previously set in the makeCacheMatrix object)
## cacheSolve() computes the inverse of the given matrix if it has not been cached previously.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  data <- x$get()
  
  if (!is.null(x$getCache())) {
    message("getting cached data")
    return(x$getCache())
  }
  
  im <- solve(data)
  x$setCache(im)
  im
  
}
