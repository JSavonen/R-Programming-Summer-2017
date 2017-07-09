## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix sets up a matrix and saves it in a cache to be used later in future computions.
## Basicly, the x is equal to matric and then the function sets the matrix as a matrix value for the x
## Then the x can be used as a matrix to be used in different matrix computions. So the function saves basicly a matrix for future use by caching it into the memory.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## CacheSolve uses the saved matrix as the input and calculates the inverse for the saved or cached matrix if it is inversible matrix,
## Thus, there will warning message "getting cached data" if the matrix set by makeCacheMatrix has not been inversible.
## Thus, the function uses the cached matrix as input and returns the inverse of the cached matrix.

cacheSolve <- function(x, ...){
          ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
