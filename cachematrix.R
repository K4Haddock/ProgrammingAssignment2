## Put comments here that give an overall description of what your
## functions do
##  makeCacheMatrix takes a matrix and put the inverse to an object
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
        ## The inverse property is initialized
        inv <- NULL
        ## assign a value to an object
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get the matrix
        get <- function() 
                ## matrix will return
                x
        ## invert the matrix
        setinverse <- function(inverse) 
                inv <<- inverse
        ## get the inverse of the matrix
        getinverse <- function() 
                inv
        ## returns a list of methods
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## takes a matrix from an object - computes an inverse if the matrix is not already inverted and returns the inverted matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        ## return the inverse if the inverse is set already
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## matrix form the defined object
        data <- x$get()
        ## function computes the inverse of the matrix 
        inv <- solve(data, ...)
        ## the inverse is set
        x$setinverse(inv)
        ## gives the matrix back
        inv
}
