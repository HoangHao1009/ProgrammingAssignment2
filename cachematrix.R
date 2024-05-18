makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) x <<- inverse
  getInverse <- function() inverse
  return(list(set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse))
}

## Take x as input and return 
##a obj contain 4 function: set, get, setInverse, getInverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cache data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
