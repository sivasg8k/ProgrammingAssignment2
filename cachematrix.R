##The objective of these functions is to calculate the inverse of a matrix.
##Once calculated, cache the result so that it can be used in further computations.

## takes input a matrix and returns a list of functions which set and get the matrix
## and also functions which set and get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## returns the inverse of a matrix. if cached, returns the cached inverse or else
## it calculates the inverse and sets it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setInverse(inv)
  inv
}
