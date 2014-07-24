## The functions herein cache the inverse of a matrix.
## Matrix inversion is usually a costly computation.
## Caching matrix inversions stores the data produced by the
## matrix inversion so that we can refer to that data repeatedly without
## having to compute the inversion over and over again.

makeCacheMatrix <- function(x = matrix(), ...) {
        ## This function creates a special "matrix" object that can
        ## cache its inverse:
        m <- NULL ## This initializes "m", the variable within which the function caches
        set <- function(y) { ## This enables user to reset m as NULL, as well as x.
                x <<- y
                m <<- NULL
        }
        get <- function() x ## This allows $get() to take on the value of argument x.
        setinverse <- function(solve) m <<- solve ## This allows the inverse to be reset.
        getinverse <- function() m ## This receives the inverse  matrix from cacheSolve.
        list(set = set, get = get, ## This coerces the functions above to list form.
             setinverse = setinverse, 
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## This function then computes the inverse of the special matrix
        ## which was created by the makeCacheMatrix function above. If the inverse
        ## has already been calculated (and the matrix has not changed), then
        ## the function below will just retrieve the data from the cache.
        ## Ultimately, it returns a matrix that is the inverse of 'x'.
        m <- x$getinverse() ## This makes the local "m" become getinverse()
        if(!is.null(m)) { ## If the local m is not NULL, then global m is returned.
                message("getting cached data...")
                return(m)
        }
        data <- x$get() ## Else, the original matrix from makeCacheMatrix is stored in "data".
        m <- solve(data, ...) ## And "data" is used to store the inverse value into local m
        x$setinverse(m) ## This sets global m as the local m here
        m ## This returns local m
}
