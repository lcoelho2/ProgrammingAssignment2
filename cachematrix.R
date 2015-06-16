## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix, caculates its inverse and caches it

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function verifies if the inverse of the matrix returned by makeCacheMatrix 
## was already calculate, if it was, retrieves it from the cache, if it was not, computes it  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
## assume that the matrix supplied is always invertible  
  m <- solve(matrix, ...)
  x$setinv(m)
  m
}
