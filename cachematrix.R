
# Returns a list of functions to store and retrieve a matrix and
# cache the inverse of that matrix 
makeCacheMatrix <- function(x = matrix()) {
  
  #initiates empty cache
  inv <- NULL
  
  #stores a new value of the matrix and empties the cache
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  #returns stored matrix
  get <- function() x
  
  #stores argument in cache
  setinv <- function(invrs) inv <<- invrs
  
  #returns cached value
  getinv <- function() inv
  
  #returns list of the functions 
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# Computes and returns the inverse of the matrix, as well as 
# requests for the inverse to be cached; or retrieves inverse
# from the cache if available
cacheSolve <- function(x, ...) {
  
  #retrieve cached value
  invrs <- x$getinv()
  if(!is.null(invrs)){
    message("getting cached data")
    return(invrs)
  }
  
  #if cache is empty, compute inverse and store in cache
  data <- x$get()
  invrs <- solve(data, ...)
  x$setinv(invrs)
  
  #return matrix inverse
  invrs
}
