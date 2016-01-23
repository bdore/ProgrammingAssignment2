## The code below creates two functions to optimize the calculation of the inverse of a matrix. The first creates a cache and stores and retrieves data from it.
## The second calculates the inverse of the matrix. It also sets a new value to the cache if the matrix is changed
## or retrieves and displays the data in the cache if the matrix did not change.

## Function that creates a cache for matrix 'm'. The function receives a list of functions to be used for these purposes.
## The cache is read by 'getInverse'. 'setInverse' stores the inverse in the cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function that checks if the cache is populated or not.
## If the cache is populated it displays the message and returns the value stored.
## If the cache is not populated it calculates the inverse of the matrix and stores it using
## 'setInverse' defined in 'makeCacheMatrix'.

cacheSolve <- function(x, ...) {
        
        m <- x$getInverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}