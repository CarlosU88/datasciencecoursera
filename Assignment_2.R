makeCacheMatrix <- function(x) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  getInverse <- function() {
    inv
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" object returned by makeCacheMatrix
cacheSolve <- function(cacheMatrix) {
  inv <- cacheMatrix$getInverse()
  
  if (!is.null(inv)) {
    message("Retrieving cached inverse.")
    return(inv)
  }
  
  data <- cacheMatrix$get()
  inv <- solve(data)
  cacheMatrix$setInverse(inv)
  inv
}