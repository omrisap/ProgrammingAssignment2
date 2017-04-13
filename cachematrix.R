## My functions - makeCacheMatrix - create a new type of matrix "CacheMatrix", with a list of functions:
## set - set the the data of the matix with << that assign the value to environment that is different from the current environment
## get - get the matrix data
## setinverse - set the inverse of the matrix in the cache
## getinverse - get the inverse of the matrix from the cache

## Implement all the functionslisted above

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() invtype
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Checks if the inverse of the matrix is already in the cach and calculate if needed and returns the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
