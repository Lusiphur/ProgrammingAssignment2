## makeCacheMatrix - initialise/set the matrix and function name
## cacheSolve - calculate inverse of matrix if value not cached

makeCacheMatrix <- function(x = matrix()) {
	     m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

cacheSolve <- function(vect, ...) {
         m <- vect$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- vect$get()
        m <- solve(data, ...)
        vect$setinverse(m)
        m

}
