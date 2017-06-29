## This function creates a special "matrix" object that can cache its inverse.

## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- inverse
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## The following function calculates the inverse of the special "matric" created with 
## the above function. It checks to see if the inverse is already calcuated, if so, it
## returns the cached values else computes it.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached iverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        message("printing newly computed inverse")
        inv ## Return a matrix that is the inverse of "x"
}
