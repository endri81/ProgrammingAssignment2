## Sometimes we want to cache the inverse of a matrix 
## rather than recompute it in the sense of time cost.
## Below we have created 2 functions to make possible the creation of
## a special object that stores a matrix and caches its inverse.

## Our function makeCacheMatrix creates 
## a special “matrix”, containing a function to cache the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function cacheSolve makes possible to cache the inverse of special "matrix"
## we created with function makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getInverse()
  if (!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  dat <- x$get()
  inver <- solve(dat, ...)
  x$setInverse(inver)
  inver
}


## Test on real data

## First create initial matrix
init_dat <- matrix(c(2,3,4,5),2,2)

#Test of function makeCacheMatrix

result_1 <- makeCacheMatrix(init_dat)

#our inverse result
cacheSolve(result_1) 







