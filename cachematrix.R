## Use cacheSolve function together with makeCacheMatrix
## to evalutate the inverse of a given matrix.
## If the inverse exists in cache, cacheSolve will return it immediately
## For instance,
## mm <- makeCacheMatrix(x = rbind(c(1, 2,3), c(0,1,4),c(5,6,0)))
## cacheSolve(mm)
## mm$get() %*% cacheSolve(mm)

##  Given a matrix x, makeCacheMatrix function
##  Would return a list of functions which
##  1. set the matrix
##  2. get the matrix
##  3. set the inverse matrix
##  4. get the inverse matrix respectively.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inversem) inv <<- inversem
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Given a list of functions x, cacheSolve function
## would return the inverse of the matrix defined in
## the list x. It first check if the inverse has already
## been caculated. If so, it gets the inverse from the cache
## and skip the computation. Otherwise, it caculateds and sets 
## the inverse of the matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
