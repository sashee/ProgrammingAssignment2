# These functions allows a creation of a caching matrix that
# can calculate and store it's inverse

# Constructor, makes a matrix that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function(){ x }
  setinverse <- function(new_inverse){
    inverse <<- new_inverse
  }
  getinverse <- function(){ inverse }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
   )
}


# Computes the inverse of a CacheMatrix, and memoizes the result
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}