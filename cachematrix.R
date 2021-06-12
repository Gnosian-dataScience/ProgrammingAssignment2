## Put comments here that give an overall description of what your
## functions do

###the function makeCacheMatrix creates a special "matrix", which is really a containing a function to
###set the value of the matrix
###get the value of the matrix
###set the value of the inverse
###get the value of the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
###the function is used to check if the inverse of the matrix is stored in the memory, 
###if so, the value that was previously computed will be used.
###if not, the inverse of the matrix will be computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
