# makeCacheMatrix is a function that returns a list of functions
# Its purpose is to create a special "matrix", which is really a list containing functions to
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   get the cahced value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)
#
# 
makeCacheMatrix <- function(x = matrix()) {
        ## Set cache to NULL initially
        m <- NULL
        ## setup new matrix and flush cache
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## returns cached matrix
        get <- function() x
        ## cache the solved matrix
        setinverse <- function(inv) m <<- inv
        ## get the cached value
        getinverse <- function() m
        ## return a list of the function  names
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## Returns a matrix that is the inverse of 'x'
## if the matrix is in cache and has not changed - print "getting cached data"
## otherwise calculate inverse

cacheSolve <- function(x, ...) {
        ## get the cached matrix
        m <- x$getinverse()
        ## if it exists return the cached matrix preceeded by a message
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if matrix not in cache get the passed matrix and solve it and 
        ## store it in the cache matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        ## return the inversed matrix
        m
}
##You can test the code with the command lines below
##       z <- matrix(c(-3, -4, 1, 1), 2,2)
##       p <- makeCacheMatrix(z)
##       cacheSolve(p)  - this run SHOULD NOT show "getting cached data
##       cacheSolve(p)  - this run SHOULD show "getting cached data
##
##