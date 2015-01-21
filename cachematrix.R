makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  y <-NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix<-function() x  
  setInverse<-function(solve) m<<- solve
  getInverse<-function() m
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  y <- x$getMatrix()
  x$setMatrix(y)
  m <- solve(y,...)
  x$setInverse(m)
  m
}

##a <-matrix(4:7,2,2)
##b<- makeCacheMatrix(a)
##cacheSolve(b)