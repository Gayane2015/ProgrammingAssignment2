makeCacheMatrix <- function(x = matrix()) {
        m <- NULL               ##sets the value of m to NULL
        y <-NULL                ##sets the value of y to NULL
        setMatrix <- function(y) { ##set the value of the matrix
                x <<- y                 ##caches the inputted matrix
                m <<- NULL              ##sets the value of y to NULL
        }
        getMatrix<-function() x ##get the value of the matrix
        setInverse<-function(solve) m<<- solve ##set the value of the inverse matrix
        getInverse<-function() m               ##get the value of the inverse matrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {      ##Returns a matrix that is the inverse of 'x' 
        m <- x$getInverse()           ##checks to see if the inverse has already been calculatedif
        if(!is.null(m)) {     
                message("getting cached data")
                return(m)
        }
        y <- x$getMatrix()            ##runs the getMatrix function to get the value of the input matrix
        x$setMatrix(y)                ##runs the setMatrix function on the input matrix to cache it
        
        m <- solve(y,...)             ##calculates the the inverse of the matrix
        x$setInverse(m)               ##runs the setInverse function to cache the inverse
        m                             ##returns the inverse
}

##example
##a <-matrix(4:7,2,2)
##b<- makeCacheMatrix(a)
##cacheSolve(b)